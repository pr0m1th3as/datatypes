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
## @deftypefn {datatypes} {@var{TF} =} istimetable (@var{X})
##
## True if input is a @code{timetable}, false otherwise.
##
## @code{@var{TF} = istimetable (@var{X})} always returns a logical scalar,
## irrespective of the size of @var{X}.  It is false for a @code{table},
## which is not a @code{timetable} and is not a superclass of one.
##
## @seealso{istable, istabular, timetable, table}
## @end deftypefn
function TF = istimetable (x)
  TF = isa (x, 'timetable');
endfunction

%!assert_equal (istimetable (timetable ([1; 2], 'TimeStep', hours (1))), true);
%!assert_equal (istimetable (table ([1, 2, 3])), false);
%!assert_equal (istimetable ([0, 1, 2]), false);
%!assert_equal (istimetable ({true, false}), false);
%!assert_equal (istimetable ({timetable([1;2], 'TimeStep', hours (1))}), false);
