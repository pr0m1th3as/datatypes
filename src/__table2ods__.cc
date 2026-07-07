/*
Copyright (C) 2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>

This file is part of the datatypes package for GNU Octave.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program; if not, see <http://www.gnu.org/licenses/>.
*/

#include <sstream>
#include <string>
#include <vector>
#include <fstream>
#include <cstring>
#include <stdint.h>
#include <octave/oct.h>
#include <octave/Cell.h>
#include "pugixml.hpp"
#include "miniz.h"

using namespace std;

static const char *ODS_MIMETYPE =
  "application/vnd.oasis.opendocument.spreadsheet";

// Write a numeric <table:table-cell>: integer scalars keep their exact digits
// (a double cannot hold the full 64-bit range; the reader restores the exact
// class from the metadata sheet), everything else prints with %.15g.  NaN and
// empty produce a bare, value-less cell (i.e. a missing value).
static void
write_float (pugi::xml_node &cell, const octave_value &ov)
{
  if (ov.isinteger () && ov.numel () == 1)
  {
    ostringstream oss;
    if (ov.is_uint8_type ()  || ov.is_uint16_type () ||
        ov.is_uint32_type () || ov.is_uint64_type ())
      oss << ov.uint64_scalar_value ().value ();
    else
      oss << ov.int64_scalar_value ().value ();
    string s = oss.str ();
    cell.append_attribute ("office:value-type") = "float";
    cell.append_attribute ("office:value") = s.c_str ();
    cell.append_child ("text:p").text ().set (s.c_str ());
  }
  else if (ov.is_real_scalar ())
  {
    double value = ov.double_value ();
    if (isnan (value))
      return;                           // NaN -> missing (bare cell)
    char tmp[32];
    snprintf (tmp, 32, "%.15g", value);
    cell.append_attribute ("office:value-type") = "float";
    cell.append_attribute ("office:value") = tmp;
    cell.append_child ("text:p").text ().set (tmp);
  }
}

// Emit one <table:table-cell> under 'row' for the value 'ov', encoded per the
// ODS value-type 'vt' ("float" | "boolean" | "date" | "time" | "string").
// datetime/duration arrive from the .m layer already formatted as ISO 8601
// strings (office:date-value / office:time-value); an empty string is a
// missing value.  The "string" branch inspects the value so that generic
// 'cell' columns still map numbers to float and text to string.
static void
write_cell (pugi::xml_node &row, const octave_value &ov, const string &vt)
{
  pugi::xml_node cell = row.append_child ("table:table-cell");

  // An empty value is a missing cell, EXCEPT an empty char, which is a real
  // empty string and must be preserved (as an empty string cell) below.
  if (ov.isempty () && ! ov.is_string ())
    return;

  if (vt == "float")
  {
    write_float (cell, ov);
  }
  else if (vt == "boolean")
  {
    if (ov.is_real_scalar ())
    {
      double value = ov.double_value ();
      if (isnan (value))
        return;
      bool b = (value != 0);
      cell.append_attribute ("office:value-type") = "boolean";
      cell.append_attribute ("office:boolean-value") = b ? "true" : "false";
      cell.append_child ("text:p").text ().set (b ? "TRUE" : "FALSE");
    }
  }
  else if (vt == "date")
  {
    if (ov.is_string ())
    {
      string str = ov.string_value ();
      if (str.empty ())
        return;                         // NaT -> missing (bare cell)
      // Cells at exactly midnight display as a plain date, the rest as a
      // date-and-time value.
      size_t tpos = str.find ('T');
      bool date_only = (tpos != string::npos
                        && str.substr (tpos + 1) == "00:00:00");
      cell.append_attribute ("table:style-name") =
        date_only ? "ce_date" : "ce_datetime";
      cell.append_attribute ("office:value-type") = "date";
      cell.append_attribute ("office:date-value") = str.c_str ();
      cell.append_child ("text:p").text ().set (str.c_str ());
    }
  }
  else if (vt == "time")
  {
    if (ov.is_string ())
    {
      string str = ov.string_value ();
      if (str.empty ())
        return;                         // missing -> bare cell
      cell.append_attribute ("table:style-name") = "ce_time";
      cell.append_attribute ("office:value-type") = "time";
      cell.append_attribute ("office:time-value") = str.c_str ();
      cell.append_child ("text:p").text ().set (str.c_str ());
    }
  }
  else                                  // "string" / "cell" (generic)
  {
    if (ov.is_string ())
    {
      string str = ov.string_value ();
      cell.append_attribute ("office:value-type") = "string";
      cell.append_child ("text:p").text ().set (str.c_str ());
    }
    else                                // numbers in a generic cell column
    {
      write_float (cell, ov);
    }
  }
}

// Estimate the number of characters a cell displays as, used to size columns.
// A missing cell is 0; date/time cells use their displayed (formatted) width
// rather than the longer ISO string.
static size_t
display_len (const octave_value &ov, const string &vt)
{
  if (ov.isempty () && ! ov.is_string ())
    return 0;
  if (vt == "boolean")
    return 5;                           // "FALSE"
  if (vt == "float")
  {
    if (ov.isinteger () && ov.numel () == 1)
    {
      ostringstream oss;
      if (ov.is_uint8_type ()  || ov.is_uint16_type () ||
          ov.is_uint32_type () || ov.is_uint64_type ())
        oss << ov.uint64_scalar_value ().value ();
      else
        oss << ov.int64_scalar_value ().value ();
      return oss.str ().size ();
    }
    if (ov.is_real_scalar ())
    {
      double v = ov.double_value ();
      if (isnan (v))
        return 0;
      char tmp[32];
      return snprintf (tmp, 32, "%.15g", v);
    }
    return 0;
  }
  if (vt == "date" && ov.is_string ())
  {
    string s = ov.string_value ();
    size_t tp = s.find ('T');
    if (tp != string::npos && s.substr (tp + 1) == "00:00:00")
      return 10;                        // date only: YYYY-MM-DD
    return 19;                          // date and time: YYYY-MM-DD HH:MM:SS
  }
  if (ov.is_string ())                  // time (ISO) / string
    return ov.string_value ().size ();
  return 0;
}

// Estimate a display width (in cm) for each data column from its widest cell.
// The metrics are approximate (we lack the viewer's font), erring wide so that
// dates and numbers are not clipped to '###'.
static vector<double>
compute_col_widths (const Cell &C, const Cell &vtype, const Cell &header)
{
  octave_idx_type rows = C.rows ();
  octave_idx_type cols = C.columns ();
  bool have_vt = (vtype.numel () == cols);
  bool have_hd = (header.numel () == cols);
  vector<double> widths (cols, 0.0);
  for (octave_idx_type c = 0; c < cols; c++)
  {
    size_t maxlen = 0;
    if (have_hd && header(c).is_string ())
      maxlen = header(c).string_value ().size ();
    for (octave_idx_type r = 0; r < rows; r++)
    {
      string vt = have_vt ? vtype(c).string_value () : string ("string");
      size_t l = display_len (C(r, c), vt);
      if (l > maxlen)
        maxlen = l;
    }
    double cm = maxlen * 0.21 + 0.35;   // ~0.21 cm/char plus padding
    if (cm < 0.75)
      cm = 0.75;
    if (cm > 12.0)
      cm = 12.0;
    widths[c] = cm;
  }
  return widths;
}

// Write a full <table:table> element named 'name' from the cell grid 'C',
// using per-column value-types from 'vtype' (a 1-by-cols cellstr).  When
// 'vtype' is empty every cell is treated as a "string" (used for the metadata
// sheet, which is text-only).  When 'emit_cols' is set, one <table:table-column>
// per column is written (referencing the "coN" width styles) ahead of the rows.
static void
write_sheet (pugi::xml_node &spreadsheet, const string &name,
             const Cell &C, const Cell &vtype, const char *style = 0,
             bool emit_cols = false, const Cell &header = Cell ())
{
  pugi::xml_node table = spreadsheet.append_child ("table:table");
  table.append_attribute ("table:name") = name.c_str ();
  if (style)
    table.append_attribute ("table:style-name") = style;

  octave_idx_type rows = C.rows ();
  octave_idx_type cols = C.columns ();
  bool have_vt = (vtype.numel () == cols);

  if (emit_cols)
  {
    for (octave_idx_type c = 0; c < cols; c++)
    {
      char nm[16];
      snprintf (nm, 16, "co%d", (int) (c + 1));
      table.append_child ("table:table-column")
           .append_attribute ("table:style-name") = nm;
    }
  }

  // Optional visible header row of variable names (text cells)
  if (header.numel () == cols && cols > 0)
  {
    pugi::xml_node row = table.append_child ("table:table-row");
    for (octave_idx_type c = 0; c < cols; c++)
    {
      pugi::xml_node cell = row.append_child ("table:table-cell");
      string h = header(c).string_value ();
      cell.append_attribute ("office:value-type") = "string";
      cell.append_child ("text:p").text ().set (h.c_str ());
    }
  }

  for (octave_idx_type r = 0; r < rows; r++)
  {
    pugi::xml_node row = table.append_child ("table:table-row");
    for (octave_idx_type c = 0; c < cols; c++)
    {
      string vt = have_vt ? vtype(c).string_value () : string ("string");
      write_cell (row, C(r, c), vt);
    }
  }
}

// Declare the OpenDocument namespaces and version common to the flat document
// and the packaged content.xml.
static void
add_office_namespaces (pugi::xml_node &root)
{
  root.append_attribute ("xmlns:office") =
    "urn:oasis:names:tc:opendocument:xmlns:office:1.0";
  root.append_attribute ("xmlns:table") =
    "urn:oasis:names:tc:opendocument:xmlns:table:1.0";
  root.append_attribute ("xmlns:text") =
    "urn:oasis:names:tc:opendocument:xmlns:text:1.0";
  root.append_attribute ("xmlns:style") =
    "urn:oasis:names:tc:opendocument:xmlns:style:1.0";
  root.append_attribute ("xmlns:number") =
    "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0";
  root.append_attribute ("office:version") = "1.3";
}

// Helpers for building the number-format elements of a data style.
static void
number_part (pugi::xml_node &st, const char *elem, const char *style_val)
{
  pugi::xml_node n = st.append_child (elem);
  if (style_val)
    n.append_attribute ("number:style") = style_val;
}
static void
number_text (pugi::xml_node &st, const char *txt)
{
  st.append_child ("number:text").text ().set (txt);
}

// Declare the automatic styles: a table style that hides the
// '__datatypes_meta__' sheet, plus number formats bound to cell styles so that
// date and time cells display as formatted dates/times rather than as the
// underlying serial numbers.
static void
add_automatic_styles (pugi::xml_node &root)
{
  pugi::xml_node styles = root.append_child ("office:automatic-styles");

  pugi::xml_node hid = styles.append_child ("style:style");
  hid.append_attribute ("style:name") = "hidden_tbl";
  hid.append_attribute ("style:family") = "table";
  hid.append_child ("style:table-properties")
     .append_attribute ("table:display") = "false";

  // Date format: YYYY-MM-DD
  pugi::xml_node nd = styles.append_child ("number:date-style");
  nd.append_attribute ("style:name") = "N_date";
  number_part (nd, "number:year", "long");
  number_text (nd, "-");
  number_part (nd, "number:month", "long");
  number_text (nd, "-");
  number_part (nd, "number:day", "long");

  // Date-time format: YYYY-MM-DD HH:MM:SS
  pugi::xml_node ndt = styles.append_child ("number:date-style");
  ndt.append_attribute ("style:name") = "N_datetime";
  number_part (ndt, "number:year", "long");
  number_text (ndt, "-");
  number_part (ndt, "number:month", "long");
  number_text (ndt, "-");
  number_part (ndt, "number:day", "long");
  number_text (ndt, " ");
  number_part (ndt, "number:hours", "long");
  number_text (ndt, ":");
  number_part (ndt, "number:minutes", "long");
  number_text (ndt, ":");
  number_part (ndt, "number:seconds", "long");

  // Duration format: [HH]:MM:SS, hours not wrapped at 24
  pugi::xml_node nt = styles.append_child ("number:time-style");
  nt.append_attribute ("style:name") = "N_time";
  nt.append_attribute ("number:truncate-on-overflow") = "false";
  number_part (nt, "number:hours", "long");
  number_text (nt, ":");
  number_part (nt, "number:minutes", "long");
  number_text (nt, ":");
  number_part (nt, "number:seconds", "long");

  // Cell styles binding each value type to its number format
  const char *ce[][2] = { {"ce_date", "N_date"}, {"ce_datetime", "N_datetime"},
                          {"ce_time", "N_time"} };
  for (int i = 0; i < 3; i++)
  {
    pugi::xml_node st = styles.append_child ("style:style");
    st.append_attribute ("style:name") = ce[i][0];
    st.append_attribute ("style:family") = "table-cell";
    st.append_attribute ("style:data-style-name") = ce[i][1];
  }
}

// Append a "coN" table-column style carrying an explicit width for each data
// column, so viewers show columns wide enough for their content (the optimal-
// width flag alone is not honoured on load).
static void
add_column_styles (pugi::xml_node &root, const vector<double> &widths)
{
  pugi::xml_node styles = root.child ("office:automatic-styles");
  for (size_t i = 0; i < widths.size (); i++)
  {
    pugi::xml_node st = styles.append_child ("style:style");
    char nm[16];
    snprintf (nm, 16, "co%d", (int) (i + 1));
    st.append_attribute ("style:name") = nm;
    st.append_attribute ("style:family") = "table-column";
    pugi::xml_node cp = st.append_child ("style:table-column-properties");
    char w[32];
    snprintf (w, 32, "%.3fcm", widths[i]);
    cp.append_attribute ("style:column-width") = w;
    cp.append_attribute ("style:use-optimal-column-width") = "true";
  }
}

// Append office:body -> office:spreadsheet with the data and metadata sheets.
// The data sheet emits per-column width styles; the metadata sheet is tagged
// with the hidden table style.
static void
build_spreadsheet (pugi::xml_node &root, const Cell &data, const Cell &vtype,
                   const Cell &meta, const Cell &header)
{
  pugi::xml_node body = root.append_child ("office:body");
  pugi::xml_node spreadsheet = body.append_child ("office:spreadsheet");
  write_sheet (spreadsheet, "Sheet1", data, vtype, 0, true, header);
  // The hidden metadata sheet is written only for the house format (non-empty
  // 'meta'); the MATLAB-compatible 'writetable' path passes an empty 'meta'.
  if (meta.numel () > 0)
    write_sheet (spreadsheet, "__datatypes_meta__", meta, Cell (), "hidden_tbl");
}

// Append a little-endian integer to a byte buffer.
static void put16 (string &b, mz_uint16 v)
{
  b.push_back ((char) (v & 0xff));
  b.push_back ((char) ((v >> 8) & 0xff));
}
static void put32 (string &b, mz_uint32 v)
{
  for (int i = 0; i < 4; i++)
    b.push_back ((char) ((v >> (8 * i)) & 0xff));
}

// One entry of the archive: name, raw payload, and whether to deflate it.
struct ods_entry { const char *name; string data; bool compress; };

// Package the parts as a compliant '.ods' ZIP.  miniz's own zip writer sets the
// data-descriptor flag on every entry, which makes the required stored
// 'mimetype' entry illegal (a stored entry has no discoverable end without its
// sizes in the local header) and strict readers reject the package.  We instead
// emit the archive by hand -- mimetype first, stored, no data descriptor and no
// extra field -- using miniz only for CRC-32 and raw deflate.
static string
write_ods_zip (const string &file, const string &content)
{
  string manifest =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<manifest:manifest"
    " xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\""
    " manifest:version=\"1.3\">\n"
    " <manifest:file-entry manifest:full-path=\"/\" manifest:version=\"1.3\""
    " manifest:media-type=\"" + string (ODS_MIMETYPE) + "\"/>\n"
    " <manifest:file-entry manifest:full-path=\"content.xml\""
    " manifest:media-type=\"text/xml\"/>\n"
    "</manifest:manifest>\n";

  vector<ods_entry> entries;
  entries.push_back ((ods_entry) {"mimetype", string (ODS_MIMETYPE), false});
  entries.push_back ((ods_entry) {"META-INF/manifest.xml", manifest, true});
  entries.push_back ((ods_entry) {"content.xml", content, true});

  int flags = tdefl_create_comp_flags_from_zip_params (MZ_DEFAULT_LEVEL, -15,
                                                       MZ_DEFAULT_STRATEGY);
  string local, central;
  mz_uint16 count = 0;

  for (size_t i = 0; i < entries.size (); i++)
  {
    const ods_entry &e = entries[i];
    mz_uint32 usize = (mz_uint32) e.data.size ();
    mz_uint32 crc = (mz_uint32) mz_crc32 (MZ_CRC32_INIT,
                    (const unsigned char *) e.data.data (), e.data.size ());
    string payload;
    mz_uint16 method = 0;
    if (e.compress && usize > 0)
    {
      size_t outlen = 0;
      void *cd = tdefl_compress_mem_to_heap (e.data.data (), e.data.size (),
                                             &outlen, flags);
      if (cd && outlen < e.data.size ())
      {
        payload.assign ((const char *) cd, outlen);
        method = 8;
      }
      else
        payload = e.data;             // incompressible: store it
      if (cd)
        mz_free (cd);
    }
    else
      payload = e.data;

    mz_uint32 csize = (mz_uint32) payload.size ();
    mz_uint32 offset = (mz_uint32) local.size ();
    mz_uint16 namelen = (mz_uint16) strlen (e.name);

    // Local file header (no general-purpose flags, no extra field)
    put32 (local, 0x04034b50);  put16 (local, 20);  put16 (local, 0);
    put16 (local, method);      put16 (local, 0);   put16 (local, 0);
    put32 (local, crc);         put32 (local, csize);  put32 (local, usize);
    put16 (local, namelen);     put16 (local, 0);
    local += e.name;
    local += payload;

    // Central directory header
    put32 (central, 0x02014b50);  put16 (central, 20);  put16 (central, 20);
    put16 (central, 0);           put16 (central, method);
    put16 (central, 0);           put16 (central, 0);
    put32 (central, crc);         put32 (central, csize);  put32 (central, usize);
    put16 (central, namelen);     put16 (central, 0);   put16 (central, 0);
    put16 (central, 0);           put16 (central, 0);   put32 (central, 0);
    put32 (central, offset);
    central += e.name;
    count++;
  }

  mz_uint32 cd_offset = (mz_uint32) local.size ();
  mz_uint32 cd_size = (mz_uint32) central.size ();
  string eocd;
  put32 (eocd, 0x06054b50);  put16 (eocd, 0);  put16 (eocd, 0);
  put16 (eocd, count);       put16 (eocd, count);
  put32 (eocd, cd_size);     put32 (eocd, cd_offset);  put16 (eocd, 0);

  ofstream f (file.c_str (), ios::binary);
  if (! f.is_open ())
    return "cannot open file '" + file + "' for writing.";
  f.write (local.data (), local.size ());
  f.write (central.data (), central.size ());
  f.write (eocd.data (), eocd.size ());
  f.close ();
  if (! f)
    return "failed to write ODS archive '" + file + "'.";
  return "";
}

DEFUN_DLD (__table2ods__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {} __table2ods__ (@var{file}, @var{data}, \
@var{vtype}, @var{meta}, @var{flat})\n\
\n\
\n\
Barebone function for saving a table to a flat (@qcode{.fods}) or compressed \
(@qcode{.ods}) OpenDocument spreadsheet file.\n\
\n\
This is a helper IO function for the @qcode{table2ods} method of the \
@qcode{table} class.  Do NOT call it directly. \n\
\n\
@end deftypefn")
{
  octave_value_list retval (nargout);

  if (args.length () != 5 && args.length () != 6)
    error ("__table2ods__: five or six input arguments are required.");

  string file  = args(0).string_value ();
  Cell   data  = args(1).cell_value ();
  Cell   vtype = args(2).cell_value ();
  Cell   meta  = args(3).cell_value ();
  bool   flat  = args(4).bool_value ();
  // Optional visible header row (variable names) for the data sheet; when
  // present, an empty 'meta' suppresses the hidden metadata sheet.
  Cell   header = (args.length () == 6) ? args(5).cell_value () : Cell ();

  pugi::xml_document doc;
  pugi::xml_node decl = doc.append_child (pugi::node_declaration);
  decl.append_attribute ("version") = "1.0";
  decl.append_attribute ("encoding") = "UTF-8";

  if (flat)
  {
    // A flat '.fods' is a single <office:document> carrying the mimetype
    pugi::xml_node root = doc.append_child ("office:document");
    add_office_namespaces (root);
    root.append_attribute ("office:mimetype") = ODS_MIMETYPE;
    add_automatic_styles (root);
    add_column_styles (root, compute_col_widths (data, vtype, header));
    build_spreadsheet (root, data, vtype, meta, header);
    if (! doc.save_file (file.c_str (), "  "))
    {
      retval(0) = "cannot open file '" + file + "' for writing.";
      return retval;
    }
  }
  else
  {
    // A compressed '.ods' packages a content.xml (<office:document-content>)
    pugi::xml_node root = doc.append_child ("office:document-content");
    add_office_namespaces (root);
    add_automatic_styles (root);
    add_column_styles (root, compute_col_widths (data, vtype, header));
    build_spreadsheet (root, data, vtype, meta, header);
    ostringstream oss;
    doc.save (oss, "  ");
    string msg = write_ods_zip (file, oss.str ());
    if (! msg.empty ())
    {
      retval(0) = msg;
      return retval;
    }
  }

  retval(0) = 0;
  return retval;
}
