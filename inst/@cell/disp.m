## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
##
## This file is part of the datatypes package for GNU Octave.
##
## This program is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {datatypes} {} disp (@var{C})
##
## Customized display of cell arrays.
##
## This function overloads Octave core @code{disp} with respect to cell arrays
## and displays their contents in MATLAB like manner.
##
## @end deftypefn
function disp (C)
  __disp__ (C);
endfunction

## These overloads are what make a cell array display in the MATLAB form
## rather than Octave's own, which is the package's most visible side effect.
## The rendering itself is done by the private '__disp__'; what this file
## adds is that nothing names the variable.

## Test a scalar cell prints the header, then the element in braces
%!test
%! C = {1};
%! want = "  1x1 cell array\n\n    {[1]}    \n\n";
%! assert_equal (evalc ('disp (C)'), want);
## Test a row of two elements is laid out on one line
%!test
%! C = {1, 2};
%! want = "  1x2 cell array\n\n    {[1]}    {[2]}    \n\n";
%! assert_equal (evalc ('disp (C)'), want);
## Test a column of two elements takes a line each
%!test
%! C = {1; 2};
%! want = "  2x1 cell array\n\n    {[1]}    \n    {[2]}    \n\n";
%! assert_equal (evalc ('disp (C)'), want);
## Test text is quoted where a number is bracketed
%!test
%! C = {'ab'};
%! want = "  1x1 cell array\n\n    {'ab'}    \n\n";
%! assert_equal (evalc ('disp (C)'), want);
## Test an empty cell array says so
%!test
%! want = "\n  0x0 empty cell array\n\n";
%! assert_equal (evalc ('disp (cell (0, 0))'), want);

## Test 'disp' names nothing, so it prints what 'display' prints for a value
## that has no name of its own
%!test
%! C = {1, 2};
%! assert_equal (evalc ('disp (C)'), evalc ('display ({1, 2})'));
