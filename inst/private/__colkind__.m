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
## @deftypefn  {datatypes} {@var{kind} =} __colkind__ (@var{C}, @var{VT})
##
## The kind of a spreadsheet column that declares no type of its own.
##
## @var{C} holds the column's cells and @var{VT} their value types.  A column
## whose typed cells are numbers, apart from text cells reading @qcode{Inf},
## @qcode{-Inf} or @qcode{NaN}, is @qcode{float}, so that an infinity written
## as text keeps its column numeric wherever it falls.  Otherwise the kind is
## that of the first typed cell, and a column with no typed cell is
## @qcode{string}.
##
## @end deftypefn

function kind = __colkind__ (C, VT)

  typed = ! cellfun (@isempty, VT);
  if (! any (typed))
    kind = 'string';
    return;
  endif
  vt = VT(typed);
  kind = vt{1};
  isnum = strcmp (vt, 'float');
  if (any (isnum))
    txt = C(typed);
    txt = txt(! isnum);
    special = @(x) ischar (x) && any (strcmpi (x, {'Inf', '-Inf', 'NaN'}));
    if (all (strcmp (vt(! isnum), 'string')) && all (cellfun (special, txt)))
      kind = 'float';
    endif
  endif

endfunction
