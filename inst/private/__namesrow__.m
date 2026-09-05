## Copyright (C) 2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{row} =} __namesrow__ (@var{row}, @var{line}, @var{caller})
##
## Resolve the two spellings of the variable-names row into one value.
##
## @qcode{'VariableNamesRow'} is the spelling the package uses throughout, and
## @qcode{'VariableNamesLine'} is MATLAB's for a text file; they mean the same
## thing, so giving both is an error rather than a precedence puzzle.  An empty
## @var{row} and @var{line} mean neither was given and the default applies.
## @var{caller} names the function for the error message.
##
## @end deftypefn

function row = __namesrow__ (row, line, caller)

  name = 'VariableNamesRow';
  if (! isempty (row) && ! isempty (line))
    error (strcat ("%s: 'VariableNamesRow' and 'VariableNamesLine' name", ...
                   " the same thing; pass one of them."), caller);
  elseif (! isempty (line))
    row = line;
    name = 'VariableNamesLine';
  elseif (isempty (row))
    row = 1;
  endif
  if (! (isnumeric (row) && isscalar (row) && row == fix (row) && row >= 0))
    error ("%s: '%s' must be a non-negative integer.", caller, name);
  endif

endfunction
