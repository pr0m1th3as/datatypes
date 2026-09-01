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
## @deftypefn {datatypes} {@var{TF} =} istabular (@var{X})
##
## True if input is a @code{table} or a @code{timetable}, false otherwise.
##
## @code{@var{TF} = istabular (@var{X})} always returns a logical scalar,
## irrespective of the size of @var{X}.  It is true for every class of
## tabular data, whereas @code{istable} and @code{istimetable} each single
## one of them out.  A @code{timetable} is not a @code{table} and neither is
## a subclass of the other.
##
## @seealso{istable, istimetable, table, timetable}
## @end deftypefn
function TF = istabular (x)
  TF = isa (x, 'tabular');
endfunction

%!assert_equal (istabular (table ([1, 2, 3])), true);
%!assert_equal (istabular (timetable ([1; 2], 'TimeStep', hours (1))), true);
%!assert_equal (istabular ([0, 1, 2]), false);
%!assert_equal (istabular ({true, false}), false);
%!assert_equal (istabular ({table([1, 2, 3])}), false);
%!assert_equal (istabular (struct ('a', 1)), false);
