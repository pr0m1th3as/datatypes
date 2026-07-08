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
## @deftypefn {datatypes} {[@var{r1}, @var{c1}, @var{r2}, @var{c2}] =} __a1ref__ (@var{spec})
##
## Parse a spreadsheet A1-style range into 1-based row and column bounds.
##
## @var{spec} is a character vector or string scalar such as @qcode{'C5'} (a
## single-cell anchor) or @qcode{'C5:D8'} (a rectangle).  The return values are
## the top-left corner (@var{r1}, @var{c1}) and bottom-right corner (@var{r2},
## @var{c2}).  A single-cell spec yields an open-ended rectangle with
## @var{r2} = @var{c2} = @code{Inf}, meaning "from this cell to the end of the
## used range".  Reversed corners (e.g.@: @qcode{'D8:C5'}) are normalised.
##
## This is an internal helper shared by @code{readtable} and @code{writetable}.
## Do NOT call it directly.
##
## @end deftypefn

function [r1, c1, r2, c2] = __a1ref__ (spec)

  if (isa (spec, 'string'))
    spec = char (spec);
  endif
  if (! (ischar (spec) && isrow (spec)))
    error ("__a1ref__: RANGE must be a character vector or string scalar.");
  endif

  parts = ostrsplit (spec, ':');
  if (numel (parts) == 1)
    [r1, c1] = parse_cell (spec);
    r2 = Inf;
    c2 = Inf;
  elseif (numel (parts) == 2)
    [r1, c1] = parse_cell (parts{1});
    [r2, c2] = parse_cell (parts{2});
    ## Normalise so (r1,c1) is the top-left and (r2,c2) the bottom-right corner
    if (r2 < r1)
      [r1, r2] = deal (r2, r1);
    endif
    if (c2 < c1)
      [c1, c2] = deal (c2, c1);
    endif
  else
    error ("__a1ref__: invalid range '%s'.", spec);
  endif

endfunction

## Parse a single A1 cell reference ('C5') into 1-based (row, column).
function [r, c] = parse_cell (s)
  tok = regexp (s, '^([A-Za-z]+)([0-9]+)$', 'tokens');
  if (isempty (tok))
    error ("__a1ref__: invalid cell reference '%s'.", s);
  endif
  letters = toupper (tok{1}{1});
  c = 0;
  for i = 1:numel (letters)
    c = c * 26 + (double (letters(i)) - double ('A') + 1);
  endfor
  r = str2double (tok{1}{2});
  if (r < 1)
    error ("__a1ref__: invalid row number in cell reference '%s'.", s);
  endif
endfunction

## __a1ref__ is a private helper; it carries no inline BIST blocks (private
## functions are not reachable by 'pkg test').  Its behaviour is covered
## indirectly through the 'Range' tests of readtable and table.writetable.
