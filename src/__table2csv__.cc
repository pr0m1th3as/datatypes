/*
Copyright (C) 2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>

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
#include <stdint.h>

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
  string file = args(0).string_value();
  Cell C = args(1).cell_value();

  // Open CSV file
  ofstream fd(file.c_str());
  if (! fd.is_open())
  {
    retval(0) = "cannot open file '" + file + "' for writing.";
    return retval;
  }

  // Initialize necessary variable;
  string word;
  string sep = ",";
  string prot = "\"";
  int rows = C.rows();
  int cols = C.columns();

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

      // Real numeric values
      if (C(row, col).is_real_scalar())
      {
        char tmp[30];
        sprintf(tmp, "%.15g", C(row, col).double_value());
        word += tmp;
      }

      // String values
      else if (C(row, col).is_string())
      {
        string str = C(row, col).string_value();
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

