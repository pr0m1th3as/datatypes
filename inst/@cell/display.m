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
## @deftypefn  {datatypes} {} display (@var{C})
##
## Customized display of cell arrays.
##
## This function overloads Octave core @code{display} with respect to cell
## arrays and displays their contents in MATLAB like manner.
##
## @end deftypefn
function display (this)
  in_name = inputname (1);
  if (! isempty (in_name))
    fprintf ('%s =\n', in_name);
  endif
  __disp__ (this, in_name);
endfunction

## This overload adds the name line to what 'disp' prints; the rendering
## itself is done by the private '__disp__'.

## Test a named variable is announced before its contents
%!test
%! x = {1, 2};
%! want = "x =\n  1x2 cell array\n\n    {[1]}    {[2]}    \n\n";
%! assert_equal (evalc ('display (x)'), want);
## Test the name is the variable's own, whatever it is
%!test
%! myCells = {1};
%! want = "myCells =\n  1x1 cell array\n\n    {[1]}    \n\n";
%! assert_equal (evalc ('display (myCells)'), want);
## Test a value with no name of its own is printed without a name line
%!test
%! want = "  1x2 cell array\n\n    {[1]}    {[2]}    \n\n";
%! assert_equal (evalc ('display ({1, 2})'), want);

## Test what follows the name line is exactly what 'disp' prints
%!test
%! x = {1, 2};
%! p = evalc ('display (x)');
%! assert_equal (p(5:end), evalc ('disp (x)'));
