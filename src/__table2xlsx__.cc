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
#include <cstdio>
#include <cstring>
#include <stdint.h>
#include <octave/oct.h>
#include <octave/Cell.h>
#include "miniz.h"

using namespace std;

// Days from 1970-01-01 to civil date y-m-d (Howard Hinnant's algorithm).
static long
days_from_civil (long y, unsigned m, unsigned d)
{
  y -= m <= 2;
  long era = (y >= 0 ? y : y - 399) / 400;
  unsigned yoe = (unsigned) (y - era * 400);
  unsigned doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
  unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
  return era * 146097 + (long) doe - 719468;
}

// Excel serial number (1900 date system) for an ISO datetime "Y-M-DThh:mm:ss".
// 25569 is the serial of 1970-01-01.  'date_only' reports whether the time part
// is exactly midnight, so the caller can pick a date vs date-and-time format.
static bool
iso_datetime_to_serial (const string &s, double &serial, bool &date_only)
{
  int Y, Mo, D, h, mi;
  double sec;
  if (sscanf (s.c_str (), "%d-%d-%dT%d:%d:%lf", &Y, &Mo, &D, &h, &mi, &sec) != 6)
    return false;
  double day = (double) days_from_civil (Y, (unsigned) Mo, (unsigned) D) + 25569.0;
  serial = day + (h * 3600.0 + mi * 60.0 + sec) / 86400.0;
  date_only = (h == 0 && mi == 0 && sec == 0.0);
  return true;
}

// Excel time serial (fraction of a day) for an ISO 8601 duration "PTnHnMnS",
// hours not wrapped at 24; a leading '-' negates it.
static bool
pt_duration_to_serial (const string &in, double &serial)
{
  string s = in;
  bool neg = (! s.empty () && s[0] == '-');
  if (neg)
    s.erase (0, 1);
  double H, M, S;
  if (sscanf (s.c_str (), "PT%lfH%lfM%lfS", &H, &M, &S) != 3)
    return false;
  serial = (H * 3600.0 + M * 60.0 + S) / 86400.0;
  if (neg)
    serial = -serial;
  return true;
}

// Spreadsheet column letters for a 1-based column index (1->A, 27->AA).
static string
col_letter (long n)
{
  string s;
  while (n > 0)
  {
    long r = (n - 1) % 26;
    s = string (1, (char) ('A' + r)) + s;
    n = (n - 1) / 26;
  }
  return s;
}

// XML-escape text content / attribute values.
static string
xml_escape (const string &in)
{
  string out;
  out.reserve (in.size ());
  for (size_t i = 0; i < in.size (); i++)
  {
    char c = in[i];
    switch (c)
    {
      case '&':  out += "&amp;";  break;
      case '<':  out += "&lt;";   break;
      case '>':  out += "&gt;";   break;
      case '"':  out += "&quot;"; break;
      default:   out += c;
    }
  }
  return out;
}

// Emit one worksheet cell <c> at (row, col) for value 'ov' of value-type 'vt'.
// A missing/empty value produces no cell (a blank).  Style indices match the
// cellXfs written in styles.xml: 1 = date, 2 = date-and-time, 3 = duration.
static void
emit_cell (ostringstream &oss, long row, long col, const octave_value &ov,
           const string &vt)
{
  if (ov.isempty () && ! ov.is_string ())
    return;
  string ref = col_letter (col) + std::to_string (row);

  if (vt == "float")
  {
    if (ov.isinteger () && ov.numel () == 1)
    {
      ostringstream v;
      if (ov.is_uint8_type () || ov.is_uint16_type () ||
          ov.is_uint32_type () || ov.is_uint64_type ())
        v << ov.uint64_scalar_value ().value ();
      else
        v << ov.int64_scalar_value ().value ();
      oss << "<c r=\"" << ref << "\"><v>" << v.str () << "</v></c>";
    }
    else if (ov.is_real_scalar ())
    {
      double val = ov.double_value ();
      if (isnan (val))
        return;
      char tmp[32];
      snprintf (tmp, 32, "%.15g", val);
      oss << "<c r=\"" << ref << "\"><v>" << tmp << "</v></c>";
    }
  }
  else if (vt == "boolean")
  {
    if (ov.is_real_scalar ())
    {
      double val = ov.double_value ();
      if (isnan (val))
        return;
      oss << "<c r=\"" << ref << "\" t=\"b\"><v>" << (val != 0 ? 1 : 0)
          << "</v></c>";
    }
  }
  else if (vt == "date")
  {
    if (ov.is_string ())
    {
      double serial;
      bool date_only;
      string str = ov.string_value ();
      if (str.empty () || ! iso_datetime_to_serial (str, serial, date_only))
        return;
      char tmp[32];
      snprintf (tmp, 32, "%.11g", serial);
      oss << "<c r=\"" << ref << "\" s=\"" << (date_only ? 1 : 2) << "\"><v>"
          << tmp << "</v></c>";
    }
  }
  else if (vt == "time")
  {
    if (ov.is_string ())
    {
      double serial;
      string str = ov.string_value ();
      if (str.empty () || ! pt_duration_to_serial (str, serial))
        return;
      char tmp[32];
      snprintf (tmp, 32, "%.11g", serial);
      oss << "<c r=\"" << ref << "\" s=\"3\"><v>" << tmp << "</v></c>";
    }
  }
  else                                  // string / generic
  {
    if (ov.is_string ())
    {
      oss << "<c r=\"" << ref << "\" t=\"inlineStr\"><is><t xml:space="
          << "\"preserve\">" << xml_escape (ov.string_value ())
          << "</t></is></c>";
    }
    else if (ov.is_real_scalar ())
    {
      double val = ov.double_value ();
      if (isnan (val))
        return;
      char tmp[32];
      snprintf (tmp, 32, "%.15g", val);
      oss << "<c r=\"" << ref << "\"><v>" << tmp << "</v></c>";
    }
  }
}

// Build the worksheet part from the data grid, per-column value types, an
// optional header row of variable names, and a 'Range' row/column offset.
static string
build_worksheet (const Cell &data, const Cell &vtype, const Cell &header,
                 long roff, long coff)
{
  octave_idx_type rows = data.rows ();
  octave_idx_type cols = data.columns ();
  bool have_vt = (vtype.numel () == cols);
  bool have_hd = (header.numel () == cols && cols > 0);

  ostringstream oss;
  oss << "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
      << "<worksheet xmlns=\"http://schemas.openxmlformats.org/"
         "spreadsheetml/2006/main\"><sheetData>";

  long row = roff;                      // 1-based row number of the next row - 1
  if (have_hd)
  {
    row++;
    oss << "<row r=\"" << row << "\">";
    for (octave_idx_type c = 0; c < cols; c++)
      emit_cell (oss, row, coff + c + 1, header(c), "string");
    oss << "</row>";
  }
  for (octave_idx_type r = 0; r < rows; r++)
  {
    row++;
    oss << "<row r=\"" << row << "\">";
    for (octave_idx_type c = 0; c < cols; c++)
    {
      string vt = have_vt ? vtype(c).string_value () : string ("string");
      emit_cell (oss, row, coff + c + 1, data(r, c), vt);
    }
    oss << "</row>";
  }
  oss << "</sheetData></worksheet>";
  return oss.str ();
}

// The fixed package parts.  'macro' selects the macro-enabled ('.xlsm')
// workbook content type.
static string
content_types_xml (bool macro)
{
  string wb = macro
    ? "application/vnd.ms-excel.sheet.macroEnabled.main+xml"
    : "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml";
  return string ("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
    + "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/"
      "content-types\">"
      "<Default Extension=\"rels\" ContentType=\"application/vnd."
      "openxmlformats-package.relationships+xml\"/>"
      "<Default Extension=\"xml\" ContentType=\"application/xml\"/>"
      "<Override PartName=\"/xl/workbook.xml\" ContentType=\"" + wb + "\"/>"
      "<Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\""
      "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet"
      "+xml\"/>"
      "<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd."
      "openxmlformats-officedocument.spreadsheetml.styles+xml\"/>"
      "</Types>";
}

static const char *ROOT_RELS =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/"
  "relationships\"><Relationship Id=\"rId1\" Type=\"http://schemas."
  "openxmlformats.org/officeDocument/2006/relationships/officeDocument\" "
  "Target=\"xl/workbook.xml\"/></Relationships>";

static const char *WORKBOOK_RELS =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/"
  "relationships\">"
  "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/"
  "officeDocument/2006/relationships/worksheet\" "
  "Target=\"worksheets/sheet1.xml\"/>"
  "<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/"
  "officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>"
  "</Relationships>";

// Styles: three custom number formats (date, date-and-time, duration) bound to
// cell formats s=1..3; s=0 is the default General format.
static const char *STYLES_XML =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  "<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/"
  "main\"><numFmts count=\"3\">"
  "<numFmt numFmtId=\"164\" formatCode=\"yyyy\\-mm\\-dd\"/>"
  "<numFmt numFmtId=\"165\" formatCode=\"yyyy\\-mm\\-dd\\ hh:mm:ss\"/>"
  "<numFmt numFmtId=\"166\" formatCode=\"[hh]:mm:ss\"/>"
  "</numFmts>"
  "<fonts count=\"1\"><font/></fonts>"
  "<fills count=\"1\"><fill><patternFill patternType=\"none\"/></fill></fills>"
  "<borders count=\"1\"><border/></borders>"
  "<cellStyleXfs count=\"1\"><xf/></cellStyleXfs>"
  "<cellXfs count=\"4\">"
  "<xf/>"
  "<xf numFmtId=\"164\" applyNumberFormat=\"1\"/>"
  "<xf numFmtId=\"165\" applyNumberFormat=\"1\"/>"
  "<xf numFmtId=\"166\" applyNumberFormat=\"1\"/>"
  "</cellXfs></styleSheet>";

static string
workbook_xml (const string &sheetname)
{
  return string ("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
    + "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/"
      "main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/"
      "relationships\"><sheets><sheet name=\"" + xml_escape (sheetname)
    + "\" sheetId=\"1\" r:id=\"rId1\"/></sheets></workbook>";
}

DEFUN_DLD (__table2xlsx__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {} __table2xlsx__ (@var{file}, @var{data}, \
@var{vtype}, @var{opts})\n\
\n\
\n\
Barebone function for writing a single-sheet Office Open XML (@qcode{.xlsx} / \
@qcode{.xlsm}) spreadsheet.\n\
\n\
This is a helper IO function for the @qcode{writetable} method of the \
@qcode{table} class.  Do NOT call it directly. \n\
\n\
@end deftypefn")
{
  octave_value_list retval (nargout);
  if (args.length () != 3 && args.length () != 4)
    error ("__table2xlsx__: three or four input arguments are required.");

  string file  = args(0).string_value ();
  Cell   data  = args(1).cell_value ();
  Cell   vtype = args(2).cell_value ();

  Cell   header;
  string sheetname = "Sheet1";
  long   roff = 0, coff = 0;
  bool   macro = false;
  if (args.length () == 4)
  {
    octave_scalar_map opts = args(3).scalar_map_value ();
    if (opts.isfield ("header"))
      header = opts.contents ("header").cell_value ();
    if (opts.isfield ("sheetname"))
      sheetname = opts.contents ("sheetname").string_value ();
    if (opts.isfield ("roff"))
      roff = (long) opts.contents ("roff").double_value ();
    if (opts.isfield ("coff"))
      coff = (long) opts.contents ("coff").double_value ();
    if (opts.isfield ("macro"))
      macro = opts.contents ("macro").bool_value ();
  }

  string sheet = build_worksheet (data, vtype, header, roff, coff);
  string ctypes = content_types_xml (macro);
  string wb = workbook_xml (sheetname);

  // Package the parts.  XLSX has no stored-first-entry rule, so miniz's own ZIP
  // writer is fine here.
  mz_zip_archive zip;
  memset (&zip, 0, sizeof (zip));
  remove (file.c_str ());
  if (! mz_zip_writer_init_file (&zip, file.c_str (), 0))
  {
    retval(0) = "cannot open file '" + file + "' for writing.";
    return retval;
  }
  struct part { const char *name; const string *data; };
  part parts[] = {
    {"[Content_Types].xml",         &ctypes},
    {"_rels/.rels",                 0},
    {"xl/workbook.xml",             &wb},
    {"xl/_rels/workbook.xml.rels",  0},
    {"xl/styles.xml",               0},
    {"xl/worksheets/sheet1.xml",    &sheet}
  };
  string root_rels = ROOT_RELS, wb_rels = WORKBOOK_RELS, styles = STYLES_XML;
  parts[1].data = &root_rels;
  parts[3].data = &wb_rels;
  parts[4].data = &styles;
  bool ok = true;
  for (size_t i = 0; i < sizeof (parts) / sizeof (parts[0]); i++)
    ok = ok && mz_zip_writer_add_mem (&zip, parts[i].name,
                                      parts[i].data->data (),
                                      parts[i].data->size (),
                                      MZ_DEFAULT_COMPRESSION);
  if (! ok || ! mz_zip_writer_finalize_archive (&zip))
  {
    mz_zip_writer_end (&zip);
    remove (file.c_str ());
    retval(0) = "failed to write Excel archive '" + file + "'.";
    return retval;
  }
  mz_zip_writer_end (&zip);

  retval(0) = 0;
  return retval;
}
