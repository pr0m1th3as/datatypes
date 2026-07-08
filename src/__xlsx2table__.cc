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

#include <string>
#include <vector>
#include <map>
#include <cmath>
#include <cctype>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <stdint.h>
#include <octave/oct.h>
#include <octave/Cell.h>
#include "pugixml.hpp"
#include "miniz.h"

using namespace std;

// Civil date from a day count since 1970-01-01 (inverse of days_from_civil).
static void
civil_from_days (long z, long &y, unsigned &m, unsigned &d)
{
  z += 719468;
  long era = (z >= 0 ? z : z - 146096) / 146097;
  unsigned doe = (unsigned) (z - era * 146097);
  unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
  long yy = (long) yoe + era * 400;
  unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
  unsigned mp = (5 * doy + 2) / 153;
  d = doy - (153 * mp + 2) / 5 + 1;
  m = mp < 10 ? mp + 3 : mp - 9;
  y = yy + (m <= 2);
}

// Excel serial -> ISO datetime "Y-M-DThh:mm:ss" (whole-second resolution), the
// form the .m layer's datetime parser expects.
static string
serial_to_iso_datetime (double serial)
{
  double days = serial - 25569.0;
  long intdays = (long) floor (days);
  long secs = (long) llround ((days - intdays) * 86400.0);
  if (secs >= 86400) { secs -= 86400; intdays++; }
  long y;  unsigned mo, d;
  civil_from_days (intdays, y, mo, d);
  long h = secs / 3600, mi = (secs % 3600) / 60, s = secs % 60;
  char buf[48];
  snprintf (buf, 48, "%04ld-%02u-%02uT%02ld:%02ld:%02ld", y, mo, d, h, mi, s);
  return buf;
}

// Excel time serial -> ISO 8601 duration "PTnHnMnS" (hours not wrapped).
static string
serial_to_pt_duration (double serial)
{
  bool neg = serial < 0;
  long isec = (long) llround (fabs (serial) * 86400.0);
  long H = isec / 3600, M = (isec % 3600) / 60, S = isec % 60;
  char buf[64];
  snprintf (buf, 64, "%sPT%ldH%ldM%ldS", neg ? "-" : "", H, M, S);
  return buf;
}

// Parse an A1 cell reference into 1-based (row, col).
static bool
ref_to_rowcol (const string &ref, long &row, long &col)
{
  size_t i = 0;
  col = 0;
  while (i < ref.size () && isalpha ((unsigned char) ref[i]))
  {
    col = col * 26 + (toupper ((unsigned char) ref[i]) - 'A' + 1);
    i++;
  }
  if (i == 0 || i >= ref.size ())
    return false;
  row = atol (ref.c_str () + i);
  return (col > 0 && row > 0);
}

// Extract a named archive member as a string; empty if absent.
static string
read_member (mz_zip_archive &zip, const char *name)
{
  size_t sz = 0;
  void *p = mz_zip_reader_extract_file_to_heap (&zip, name, &sz, 0);
  if (! p)
    return string ();
  string out ((const char *) p, sz);
  mz_free (p);
  return out;
}

// Concatenate the text of all <t> descendants of a node (shared-string item).
static string
all_text (const pugi::xml_node &node)
{
  string out;
  for (pugi::xml_node t = node.child ("t"); t; t = t.next_sibling ("t"))
    out += t.text ().as_string ();
  for (pugi::xml_node r = node.child ("r"); r; r = r.next_sibling ("r"))
    out += r.child ("t").text ().as_string ();
  return out;
}

// Classify a cell's style index into 'date', 'time', or "" (plain number),
// using the number-format bound to that cell format.
static string
style_kind (long s, const vector<int> &xf_fmt,
            const map<int, string> &custom_fmt)
{
  if (s < 0 || (size_t) s >= xf_fmt.size ())
    return string ();
  int id = xf_fmt[s];
  // Built-in date/time format ids.
  if ((id >= 14 && id <= 22))
    return "date";
  if (id == 45 || id == 46 || id == 47)
    return "time";
  if (id >= 164)
  {
    map<int, string>::const_iterator it = custom_fmt.find (id);
    if (it != custom_fmt.end ())
    {
      const string &f = it->second;
      if (f.find ('[') != string::npos)         // elapsed [h]/[hh]/[mm] -> time
        return "time";
      if (f.find ('y') != string::npos || f.find ('Y') != string::npos ||
          f.find ('d') != string::npos || f.find ('D') != string::npos)
        return "date";
      if (f.find ('h') != string::npos || f.find ('H') != string::npos ||
          f.find ('s') != string::npos || f.find ('S') != string::npos)
        return "date";                          // time-of-day -> datetime
    }
  }
  return string ();
}

DEFUN_DLD (__xlsx2table__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {[@var{data}, @var{vtype}, @var{meta}, @var{names}] =} \
__xlsx2table__ (@var{file})\n\
 @deftypefnx {datatypes} {[@var{data}, @var{vtype}, @var{meta}, @var{names}] =} \
__xlsx2table__ (@var{file}, @var{sheet})\n\
\n\
\n\
Barebone function for reading a sheet of an Office Open XML (@qcode{.xlsx} / \
@qcode{.xlsm}) spreadsheet.\n\
\n\
This is a helper IO function for the @qcode{readtable} function.  Do NOT call \
it directly. \n\
\n\
@end deftypefn")
{
  octave_value_list retval (4);
  retval(1) = Cell ();
  retval(2) = Cell ();                  // no metadata sheet in the interop format
  retval(3) = Cell ();

  if (args.length () < 1 || args.length () > 2)
    error ("__xlsx2table__: one or two input arguments are required.");
  string file = args(0).string_value ();

  string want_name;
  long   want_index = 0;
  bool   by_name = false, by_index = false;
  if (args.length () == 2 && ! args(1).isempty ())
  {
    if (args(1).is_string ())
    {
      want_name = args(1).string_value ();
      by_name = true;
    }
    else
    {
      want_index = (long) args(1).scalar_value ();
      by_index = true;
    }
  }

  mz_zip_archive zip;
  memset (&zip, 0, sizeof (zip));
  if (! mz_zip_reader_init_file (&zip, file.c_str (), 0))
  {
    retval(0) = string ("cannot read '") + file + "' as an Excel archive.";
    return retval;
  }

  // --- workbook.xml: sheet names in order, plus each sheet's relationship id ---
  pugi::xml_document wbdoc;
  string wbxml = read_member (zip, "xl/workbook.xml");
  if (wbxml.empty () || ! wbdoc.load_buffer (wbxml.data (), wbxml.size ()))
  {
    mz_zip_reader_end (&zip);
    retval(0) = string ("'") + file + "' is not a readable Excel workbook.";
    return retval;
  }
  vector<string> sheet_names, sheet_rid;
  for (pugi::xml_node sh =
         wbdoc.child ("workbook").child ("sheets").child ("sheet"); sh;
       sh = sh.next_sibling ("sheet"))
  {
    sheet_names.push_back (sh.attribute ("name").as_string ());
    sheet_rid.push_back (sh.attribute ("r:id").as_string ());
  }

  // Select the sheet.
  long sel = -1;
  if (by_name)
  {
    for (size_t i = 0; i < sheet_names.size (); i++)
      if (sheet_names[i] == want_name) { sel = (long) i; break; }
    if (sel < 0)
    {
      mz_zip_reader_end (&zip);
      retval(0) = string ("sheet '") + want_name + "' not found in '" + file
                  + "'.";
      return retval;
    }
  }
  else if (by_index)
  {
    if (want_index < 1 || (size_t) want_index > sheet_names.size ())
    {
      mz_zip_reader_end (&zip);
      retval(0) = string ("sheet index ") + std::to_string (want_index)
                  + " out of range in '" + file + "'.";
      return retval;
    }
    sel = want_index - 1;
  }
  else
    sel = (sheet_names.empty () ? -1 : 0);

  Cell names_out (1, sheet_names.size ());
  for (size_t i = 0; i < sheet_names.size (); i++)
    names_out(i) = sheet_names[i];
  retval(3) = names_out;

  if (sel < 0)
  {
    mz_zip_reader_end (&zip);
    retval(0) = Cell ();
    retval(1) = Cell ();
    return retval;
  }

  // --- resolve the worksheet part via workbook relationships ---
  string target = "worksheets/sheet1.xml";
  pugi::xml_document rdoc;
  string rxml = read_member (zip, "xl/_rels/workbook.xml.rels");
  if (! rxml.empty () && rdoc.load_buffer (rxml.data (), rxml.size ()))
  {
    for (pugi::xml_node rel =
           rdoc.child ("Relationships").child ("Relationship"); rel;
         rel = rel.next_sibling ("Relationship"))
      if (sheet_rid[sel] == rel.attribute ("Id").as_string ())
      {
        target = rel.attribute ("Target").as_string ();
        break;
      }
  }
  if (! target.empty () && target[0] == '/')
    target = target.substr (1);
  else
    target = "xl/" + target;

  // --- shared strings (optional) ---
  vector<string> shared;
  string sxml = read_member (zip, "xl/sharedStrings.xml");
  if (! sxml.empty ())
  {
    pugi::xml_document sdoc;
    if (sdoc.load_buffer (sxml.data (), sxml.size ()))
      for (pugi::xml_node si = sdoc.child ("sst").child ("si"); si;
           si = si.next_sibling ("si"))
        shared.push_back (all_text (si));
  }

  // --- styles: map each cell format (s index) to its number-format id ---
  vector<int> xf_fmt;
  map<int, string> custom_fmt;
  string stxml = read_member (zip, "xl/styles.xml");
  if (! stxml.empty ())
  {
    pugi::xml_document stdoc;
    if (stdoc.load_buffer (stxml.data (), stxml.size ()))
    {
      pugi::xml_node ss = stdoc.child ("styleSheet");
      for (pugi::xml_node nf = ss.child ("numFmts").child ("numFmt"); nf;
           nf = nf.next_sibling ("numFmt"))
        custom_fmt[nf.attribute ("numFmtId").as_int ()] =
          nf.attribute ("formatCode").as_string ();
      for (pugi::xml_node xf = ss.child ("cellXfs").child ("xf"); xf;
           xf = xf.next_sibling ("xf"))
        xf_fmt.push_back (xf.attribute ("numFmtId").as_int (0));
    }
  }

  // --- worksheet cells ---
  pugi::xml_document wsdoc;
  string wsxml = read_member (zip, target.c_str ());
  mz_zip_reader_end (&zip);
  if (wsxml.empty () || ! wsdoc.load_buffer (wsxml.data (), wsxml.size ()))
  {
    retval(0) = string ("cannot read sheet in '") + file + "'.";
    return retval;
  }

  struct xcell { long row, col; octave_value val; string vt; };
  vector<xcell> cells;
  long maxrow = 0, maxcol = 0;
  pugi::xml_node sheetData =
    wsdoc.child ("worksheet").child ("sheetData");
  for (pugi::xml_node row = sheetData.child ("row"); row;
       row = row.next_sibling ("row"))
  {
    for (pugi::xml_node c = row.child ("c"); c; c = c.next_sibling ("c"))
    {
      long r, col;
      if (! ref_to_rowcol (c.attribute ("r").as_string (), r, col))
        continue;
      string t = c.attribute ("t").as_string ();
      long s = c.attribute ("s").as_int (-1);

      octave_value val;
      string vt;
      if (t == "inlineStr")
      {
        val = octave_value (all_text (c.child ("is")));
        vt = "string";
      }
      else if (t == "s")                // shared string index
      {
        long idx = c.child ("v").text ().as_llong (-1);
        val = octave_value ((idx >= 0 && (size_t) idx < shared.size ())
                            ? shared[idx] : string ());
        vt = "string";
      }
      else if (t == "str")              // formula string result
      {
        val = octave_value (c.child ("v").text ().as_string ());
        vt = "string";
      }
      else if (t == "b")                // boolean
      {
        val = octave_value (c.child ("v").text ().as_int (0) != 0);
        vt = "boolean";
      }
      else                              // number (possibly a date/time)
      {
        pugi::xml_node v = c.child ("v");
        if (! v)
          continue;                     // empty cell
        double num = atof (v.text ().as_string ());
        string kind = style_kind (s, xf_fmt, custom_fmt);
        if (kind == "date")
        {
          val = octave_value (serial_to_iso_datetime (num));
          vt = "date";
        }
        else if (kind == "time")
        {
          val = octave_value (serial_to_pt_duration (num));
          vt = "time";
        }
        else
        {
          val = octave_value (num);
          vt = "float";
        }
      }
      xcell xc; xc.row = r; xc.col = col; xc.val = val; xc.vt = vt;
      cells.push_back (xc);
      if (r > maxrow) maxrow = r;
      if (col > maxcol) maxcol = col;
    }
  }

  Cell data (maxrow, maxcol), vtype (maxrow, maxcol);
  for (long r = 0; r < maxrow; r++)
    for (long col = 0; col < maxcol; col++)
    {
      data(r, col) = Matrix (0, 0);     // blank
      vtype(r, col) = string ();
    }
  for (size_t i = 0; i < cells.size (); i++)
  {
    data(cells[i].row - 1, cells[i].col - 1) = cells[i].val;
    vtype(cells[i].row - 1, cells[i].col - 1) = cells[i].vt;
  }

  retval(0) = data;
  retval(1) = vtype;
  return retval;
}
