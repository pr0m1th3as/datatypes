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
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {private} {@var{M} =} __varmissing__ (@var{v})
##
## Find missing data in one variable of a tabular object.
##
## @code{ismissing} is a method of the datatypes classes and reaches no plain
## numeric, logical or cellstr array, so the value's type chooses the route.
## A nested table and a character matrix have no missing value of their own.
##
## @end deftypefn

function M = __varmissing__ (v)

  if (isa (v, 'table') || isa (v, 'timetable'))
    M = false (size (v));
  elseif (any (isa (v, {'calendarDuration', 'categorical', 'datetime', ...
                        'duration', 'missing', 'string'})))
    M = ismissing (v);
  elseif (ischar (v))
    M = false (size (v));
  else  # numeric, logical, cellstr
    M = __ismissing__ (v);
  endif

endfunction
