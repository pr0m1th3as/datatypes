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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{M} =} __numcells__ (@var{C})
##
## Build a numeric matrix from the cells of a spreadsheet column.
##
## An empty cell is @code{NaN}.  A text cell reading @qcode{Inf}, @qcode{-Inf}
## or @qcode{NaN}, in any capitals, is that number: an infinity is written to
## ODS and XLSX as text, since a spreadsheet application loads a non-finite
## number as 0.  Any other text is @code{NaN}, as MATLAB reads text in a
## numeric column.
##
## @end deftypefn

function M = __numcells__ (C)

  M = nan (size (C));
  for i = 1:numel (C)
    x = C{i};
    if (ischar (x))
      if (strcmpi (x, 'Inf'))
        M(i) = Inf;
      elseif (strcmpi (x, '-Inf'))
        M(i) = -Inf;
      endif
    elseif (! isempty (x))
      M(i) = double (x);
    endif
  endfor

endfunction
