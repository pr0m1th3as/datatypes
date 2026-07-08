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
#include <fstream>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <stdint.h>
#include <octave/oct.h>
#include <octave/Cell.h>
#include <octave/lo-ieee.h>
#include "pugixml.hpp"
#include "miniz.h"

using namespace std;

// A compressed '.ods' begins with the ZIP local-file-header magic "PK\x03\x04".
static bool
is_zip_file (const string &file)
{
  ifstream f (file.c_str (), ios::binary);
  char m[4] = {0};
  f.read (m, 4);
  return (m[0] == 'P' && m[1] == 'K' && m[2] == 3 && m[3] == 4);
}

// Parse a numeric string the same way __csv2table__ does: a plain integer of
// 16 or more digits is stored losslessly as a 64-bit integer, everything else
// becomes a double.
static octave_value
parse_float (const string &word)
{
  const char *word_str = word.c_str ();
  char *err;
  double val = strtod (word_str, &err);
  size_t start = (! word.empty () && (word[0] == '+' || word[0] == '-')) ? 1 : 0;
  bool is_int = (word.length () > start);
  size_t ndig = 0;
  for (size_t i = start; i < word.length (); i++)
  {
    if (word[i] < '0' || word[i] > '9') { is_int = false; break; }
    ndig++;
  }
  if (is_int && ndig >= 16)
  {
    errno = 0;
    if (word[0] == '-')
    {
      long long llv = strtoll (word_str, &err, 10);
      return (errno == 0) ? octave_value (octave_int64 (llv))
                          : octave_value (val);
    }
    else
    {
      unsigned long long ullv = strtoull (word_str, &err, 10);
      return (errno == 0) ? octave_value (octave_uint64 (ullv))
                          : octave_value (val);
    }
  }
  return octave_value (val);
}

// Concatenated text of a cell's <text:p> children.
static string
cell_text (const pugi::xml_node &cell)
{
  string out;
  for (pugi::xml_node p = cell.child ("text:p"); p;
       p = p.next_sibling ("text:p"))
  {
    if (! out.empty ())
      out += "\n";
    out += p.text ().as_string ();
  }
  return out;
}

// Parse one <table:table> into a value grid and a parallel per-cell value-type
// grid.  'typed' selects data-sheet decoding (float/boolean/date/time/string,
// with missing cells left empty); when false every cell is returned as text
// (used for the metadata sheet).  Row/column repeat counts are honoured so the
// grid stays rectangular.
static void
read_sheet (const pugi::xml_node &table, Cell &data, Cell &vtype, bool typed,
            bool trim)
{
  // First pass: collect rows as vectors of (value-type, raw-string) and find
  // the maximum column count.
  vector<vector<pair<string, string>>> grid;
  size_t maxcols = 0;
  // When 'trim' is set (foreign spreadsheets with no metadata sheet), trailing
  // empty cells -- blank cells padded out with large 'number-columns-repeated'
  // counts -- and trailing empty rows are dropped so the grid spans only the
  // sheet's used bounding box; leading and interior empties are preserved as
  // blank cells, keeping absolute A1 coordinates intact for the caller's
  // 'Range' handling.  When 'trim' is clear (our house files, whose metadata
  // sheet makes the grid authoritative), every cell is materialised verbatim --
  // a trailing row of all-missing values is real data and must be kept.
  long pending_rows = 0;              // empty rows not yet flushed (may be trailing)
  for (pugi::xml_node row = table.child ("table:table-row"); row;
       row = row.next_sibling ("table:table-row"))
  {
    long rrep = row.attribute ("table:number-rows-repeated").as_int (1);
    if (rrep < 1) rrep = 1;
    vector<pair<string, string>> cols;
    long pending_cols = 0;            // empty cells not yet flushed (may be trailing)
    for (pugi::xml_node cell = row.child ("table:table-cell"); cell;
         cell = cell.next_sibling ("table:table-cell"))
    {
      long crep = cell.attribute ("table:number-columns-repeated").as_int (1);
      if (crep < 1) crep = 1;
      string vt = cell.attribute ("office:value-type").as_string ();
      string raw;
      if (vt == "float" || vt == "percentage" || vt == "currency")
        raw = cell.attribute ("office:value").as_string ();
      else if (vt == "boolean")
        raw = cell.attribute ("office:boolean-value").as_string ();
      else if (vt == "date")
        raw = cell.attribute ("office:date-value").as_string ();
      else if (vt == "time")
        raw = cell.attribute ("office:time-value").as_string ();
      else                              // string / empty
        raw = cell_text (cell);
      if (trim && vt.empty () && raw.empty ())  // blank: defer (may be trailing)
      {
        pending_cols += crep;
      }
      else                              // content cell: flush deferred blanks first
      {
        for (long k = 0; k < pending_cols; k++)
          cols.push_back (make_pair (string (), string ()));
        pending_cols = 0;
        for (long k = 0; k < crep; k++)
          cols.push_back (make_pair (vt, raw));
      }
    }
    // A row with no content cells is empty; defer it in case it is trailing.
    if (trim && cols.empty ())
    {
      pending_rows += rrep;
    }
    else
    {
      for (long k = 0; k < pending_rows; k++)
        grid.push_back (vector<pair<string, string>> ());
      pending_rows = 0;
      if (cols.size () > maxcols)
        maxcols = cols.size ();
      for (long k = 0; k < rrep; k++)
        grid.push_back (cols);
    }
  }

  size_t nrows = grid.size ();
  data = Cell (nrows, maxcols);
  vtype = Cell (nrows, maxcols);
  for (size_t r = 0; r < nrows; r++)
  {
    for (size_t c = 0; c < maxcols; c++)
    {
      string vt = (c < grid[r].size ()) ? grid[r][c].first : string ();
      string raw = (c < grid[r].size ()) ? grid[r][c].second : string ();
      vtype(r, c) = vt;
      if (! typed)                      // metadata sheet: text only
      {
        data(r, c) = raw;
      }
      else if (vt == "float" || vt == "percentage" || vt == "currency")
      {
        data(r, c) = parse_float (raw);
      }
      else if (vt == "boolean")
      {
        data(r, c) = octave_value (raw == "true");
      }
      else if (vt.empty ())             // missing / blank cell
      {
        data(r, c) = Matrix (0, 0);
      }
      else                              // date / time / string -> raw text
      {
        data(r, c) = raw;
      }
    }
  }
}

DEFUN_DLD (__ods2table__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {[@var{data}, @var{vtype}, @var{meta}] =} \
__ods2table__ (@var{file})\n\
 @deftypefnx {datatypes} {[@var{data}, @var{vtype}, @var{meta}] =} \
__ods2table__ (@var{file}, @var{sheet})\n\
\n\
\n\
Barebone function for reading a flat ODS (@qcode{.fods}) file.\n\
\n\
This is a helper IO function for the @qcode{ods2table} function.  Do NOT call \
it directly. \n\
\n\
@end deftypefn")
{
  octave_value_list retval (3);
  // Keep the value-type and metadata outputs defined even on the error paths,
  // so the caller's three-output call never sees an undefined return element.
  retval(1) = Cell ();
  retval(2) = Cell ();

  if (args.length () < 1 || args.length () > 2)
    error ("__ods2table__: one or two input arguments are required.");

  string file = args(0).string_value ();

  // Optional sheet selector: a name (character vector) or a 1-based index over
  // the data sheets (the hidden '__datatypes_meta__' sheet is never counted).
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
      want_index = static_cast<long> (args(1).scalar_value ());
      by_index = true;
    }
  }

  // A compressed '.ods' is a ZIP whose content.xml holds the spreadsheet; a
  // flat '.fods' is the XML document itself.
  pugi::xml_document doc;
  pugi::xml_parse_result res;
  if (is_zip_file (file))
  {
    mz_zip_archive zip;
    memset (&zip, 0, sizeof (zip));
    if (! mz_zip_reader_init_file (&zip, file.c_str (), 0))
    {
      retval(0) = string ("cannot read '") + file + "' as a ZIP archive.";
      return retval;
    }
    size_t sz = 0;
    void *p = mz_zip_reader_extract_file_to_heap (&zip, "content.xml", &sz, 0);
    if (! p)
    {
      mz_zip_reader_end (&zip);
      retval(0) = string ("'") + file + "' has no 'content.xml' entry.";
      return retval;
    }
    res = doc.load_buffer (p, sz);
    mz_free (p);
    mz_zip_reader_end (&zip);
  }
  else
  {
    res = doc.load_file (file.c_str ());
  }
  if (! res)
  {
    retval(0) = string ("cannot read '") + file + "': " + res.description ()
                + ".";
    return retval;
  }

  // The spreadsheet lives under <office:document> (flat) or
  // <office:document-content> (packaged).
  pugi::xml_node root = doc.child ("office:document");
  if (! root)
    root = doc.child ("office:document-content");
  pugi::xml_node spreadsheet =
    root.child ("office:body").child ("office:spreadsheet");
  if (! spreadsheet)
  {
    retval(0) = string ("'") + file + "' is not an OpenDocument spreadsheet.";
    return retval;
  }

  pugi::xml_node data_tbl, meta_tbl;
  long data_seen = 0;
  for (pugi::xml_node t = spreadsheet.child ("table:table"); t;
       t = t.next_sibling ("table:table"))
  {
    string name = t.attribute ("table:name").as_string ();
    if (name == "__datatypes_meta__")
    {
      meta_tbl = t;
      continue;
    }
    data_seen++;
    if (by_name)
    {
      if (name == want_name && ! data_tbl)
        data_tbl = t;
    }
    else if (by_index)
    {
      if (data_seen == want_index && ! data_tbl)
        data_tbl = t;
    }
    else if (! data_tbl)
      data_tbl = t;
  }

  // A requested sheet that does not exist is an error the caller reports.
  if ((by_name || by_index) && ! data_tbl)
  {
    if (by_name)
      retval(0) = string ("sheet '") + want_name + "' not found in '" + file
                  + "'.";
    else
      retval(0) = string ("sheet index ") + std::to_string (want_index)
                  + " out of range in '" + file + "'.";
    return retval;
  }

  Cell data, vtype, meta, meta_vt;
  // Trim the data grid to the used block only for foreign spreadsheets (no
  // metadata sheet); our own house files are authoritative and kept verbatim.
  if (data_tbl)
    read_sheet (data_tbl, data, vtype, true, ! meta_tbl);
  if (meta_tbl)
    read_sheet (meta_tbl, meta, meta_vt, false, false);

  retval(0) = data;
  retval(1) = vtype;
  retval(2) = meta;
  return retval;
}
