/*
Copyright (C) 2025-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>

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
#include <fstream>
#include <iostream>
#include <stdint.h>
#include <cerrno>
#include <cstdlib>

#include <octave/ov.h>
#include <octave/oct.h>
#include <octave/Cell.h>
#include <octave/file-ops.h>
#include <octave/lo-ieee.h>

using namespace std;

DEFUN_DLD (__csv2table__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {@var{C} =} __csv2table__ (@var{file})\n\
\n\
\n\
Barebone function for loading a CSV file to a cell array.\n\
\n\
This is a helper IO function for the @qcode{csv2table} function.  Do NOT \
call it directly. \n\
\n\
@end deftypefn")
{
  octave_value_list retval(nargout);
  // Check input arguments
  if (args.length() != 1 && args.length() != 2)
  {
    error ("__csv2table__: one or two input arguments are required.");
  }

  // Get input arguments
  string file = args(0).string_value();

  // Optional field delimiter (a single character); defaults to a comma.
  string _sep = ",";
  if (args.length() == 2)
  {
    string d = args(1).string_value();
    if (d.length() != 1)
    {
      error ("__csv2table__: DELIMITER must be a single character.");
    }
    _sep = d;
  }

  // Open CSV file
  ifstream fd(file.c_str());
  if (! fd.is_open())
  {
    retval(0) = "cannot open file '" + file + "' for reading.";
    return retval;
  }

  // Initialize necessary variable;
  char sep = _sep[0];
  string _prot = "\"";
  char prot = _prot[0];
  string line, word;
  bool inside = false;
  int cols = 0;
  int rows = 1;

  // Get number of columns from parsing the first line
  getline (fd, line);
  for (int c = 0, len = line.length(); c <= len; c++)
  {
    if (c == len || ((line[c] == sep || line[c] == 10) && ! inside))
    {
      cols++;
    }
    else if ((inside) && line[c] == prot && (c + 1 < len && line[c + 1] == prot))
    {
      ++c;
    }
    else if (line[c] == prot)
    {
      inside = ! inside;
    }
  }
  // Get number of row from parsing the remaining file
  while (getline(fd, line))
  {
    rows++;
  }
  fd.clear();

  // Rewind
  fd.seekg (0, ios::beg);
  if (! fd.good())
  {
    retval(0) = "cannot read '" + file + "'.";
    return retval;
  }

  //Initialize cell array
  Cell C(rows, cols);
  for (int c = 0; c < cols; c++)
  {
    for (int r = 0; r < rows; r++)
    {
      C(r,c) = "";
    }
  }

  // Parse (again) each line of the file
  bool line_too_long = false;
  for (int row = 0; row < rows; row++)
  {
    getline (fd, line);
    word = "";
    inside = false;
    int col = 0;
    bool oinside = false;
    for (int k = 0, len = line.length(); k <= len; k++)
    {
      if ((k == len || line[k] == sep) && (! inside))
      {
        // Check number of columns
        if (! line_too_long && col == cols)
        {
          line_too_long = true;
          warning ("__csv2table__: line(s) found with more fields than in headerline");
          break;
        }
        // Check for last char to be 13 (CR) and remove if found
        if (word.length () && word[word.length () - 1] == char(13))
        {
          word.resize (word.size () - 1);
        }
        // Check if scalar
        const char *word_str = word.c_str ();
        char *err;
        double val = strtod (word_str, &err);
        // The "NA" token (Octave's missing value, as written by 'table2csv')
        // is not recognized by strtod; map it to NA explicitly.
        bool is_na = (! oinside) && (word == "NA");
        // A fully consumed, unquoted token is numeric.
        bool is_num = (word != "") && (! oinside) &&
                      (err == word_str + word.length ());
        // Store into the cell; check if it is in address argument range
        if (col < cols)
        {
          if (is_na)
          {
            C(row, col) = octave_value (octave::numeric_limits<double>::NA ());
          }
          else if (! is_num)
          {
            C(row, col) = octave_value (word);
          }
          else
          {
            // A plain integer of 16 or more digits cannot be held exactly by
            // a double, so parse it without loss into a 64-bit integer.
            size_t start = (word[0] == '+' || word[0] == '-') ? 1 : 0;
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
                C(row, col) = (errno == 0)
                              ? octave_value (octave_int64 (llv))
                              : octave_value (val);
              }
              else
              {
                unsigned long long ullv = strtoull (word_str, &err, 10);
                C(row, col) = (errno == 0)
                              ? octave_value (octave_uint64 (ullv))
                              : octave_value (val);
              }
            }
            else
            {
              C(row, col) = octave_value (val);
            }
          }
        }
        col++;
        word = "";
        oinside = false;
      }
      else if ((inside) && line[k] == prot && (k + 1 < len && line[k+1] == prot))
      {
        // Inside a string
        word += prot;
        ++k;
      }
      else if (line[k] == prot)
      {
        // Switch in/out of string
        oinside = inside;
        inside = ! inside;
      }
      else
      {
        word += line[k];
      }
    }
  }

  // Close file
  fd.close();

  retval(0) = C;
  return retval;
}

