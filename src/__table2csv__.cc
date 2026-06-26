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

#include <fstream>
#include <sstream>
#include <stdint.h>
#include <iostream>
#include <octave/oct.h>
#include <octave/parse.h>
#include <octave/Cell.h>

using namespace std;

DEFUN_DLD (__table2csv__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {} __table2csv__ (@var{file}, @var{C})\n\
\n\
\n\
Barebone function for saving a cell array to a CSV file.\n\
\n\
This is a helper IO function for the @qcode{table2csv} method of the \
@qcode{table} class.  Do NOT call it directly. \n\
\n\
@end deftypefn")
{
  octave_value_list retval(nargout);
  // Check input arguments
  if (args.length() != 2)
  {
    error ("__table2csv__: two input arguments are required.");
  }

  // Get input arguments
  string file = args(0).string_value ();
  Cell C = args(1).cell_value ();

  // Open CSV file
  ofstream fd(file.c_str ());
  if (! fd.is_open ())
  {
    retval(0) = "cannot open file '" + file + "' for writing.";
    return retval;
  }

  // Initialize necessary variable;
  string word;
  string sep = ",";
  string prot = "\"";
  int rows = C.rows ();
  int cols = C.columns ();

  // Process each row
  for (int row = 0; row < rows; row++)
  {
    // Process each element in row
    for (int col = 0; col < cols; col++)
    {
      word = "";

      // Add separator
      if (col != 0)
      {
        word += sep;
      }

      // Integer scalar values are written exactly: a double cannot hold the
      // full 64-bit integer range, so bypass the double conversion below.
      if (C(row, col).isinteger () && C(row, col).numel () == 1)
      {
        ostringstream oss;
        if (C(row, col).is_uint8_type ()  || C(row, col).is_uint16_type () ||
            C(row, col).is_uint32_type () || C(row, col).is_uint64_type ())
        {
          oss << C(row, col).uint64_scalar_value ().value ();
        }
        else
        {
          oss << C(row, col).int64_scalar_value ().value ();
        }
        word += oss.str ();
      }

      // Real numeric values
      else if (C(row, col).is_real_scalar ())
      {
        double value = C(row, col).double_value ();
        // Handle NaN first
        if (octave::math::isna (value))
        {
          word += "NA";
        }
        else if (isnan (value))
        {
          word += "NaN";
        }
        else
        {
          char tmp[32];
          int cx = snprintf(tmp, 32, "%.15g", value);
          word += tmp;
        }
      }

      // String values
      else if (C(row, col).is_string ())
      {
        string str = C(row, col).string_value ();
        // Escape embedded quotes by doubling them (RFC 4180) so that
        // __csv2table__ can restore them when reading the field back.
        size_t pos = 0;
        while ((pos = str.find (prot, pos)) != string::npos)
        {
          str.insert (pos, prot);
          pos += 2;
        }
        str = prot + str + prot;
        word += str;
      }

      // Everything else is forced to NaN
      else if (!C(row, col).isempty ())
      {
        word += "NaN";
      }

      fd << word;
    }

    // Add end of line
    fd << endl;
  }

  // Close file
  fd.close();

  retval(0) = 0;
  return retval;
}

