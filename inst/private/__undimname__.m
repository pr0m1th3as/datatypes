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
## @deftypefn  {datatypes} {@var{names} =} __undimname__ (@var{names}, @var{dims})
##
## Rename any variable name that collides with a dimension name.
##
## A table refuses a variable named after one of its dimensions, so a file of
## another program's whose column happens to be headed @qcode{Row} could not be
## read at all.  Such a name takes a numeric suffix, chosen so that it collides
## with neither the dimension names @var{dims} nor another variable name.
##
## @end deftypefn

function names = __undimname__ (names, dims)

  for i = 1:numel (names)
    if (any (strcmp (names{i}, dims)))
      k = 1;
      cand = sprintf ('%s_%d', names{i}, k);
      while (any (strcmp (cand, names)) || any (strcmp (cand, dims)))
        k += 1;
        cand = sprintf ('%s_%d', names{i}, k);
      endwhile
      names{i} = cand;
    endif
  endfor

endfunction
