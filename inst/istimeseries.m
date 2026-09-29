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
## @deftypefn {datatypes} {@var{TF} =} istimeseries (@var{X})
##
## True if input is a @code{timeseries} object, false otherwise.
##
## @code{@var{TF} = istimeseries (@var{X})} always returns a logical scalar,
## irrespective of the size of @var{X}, so it is true for an array of
## @code{timeseries} objects too.  MATLAB has no such function; it is provided
## as @code{isduration} and @code{istimetable} are for their classes.
##
## @seealso{timeseries, isa}
## @end deftypefn
function TF = istimeseries (x)
  TF = isa (x, 'timeseries');
endfunction

%!assert_equal (istimeseries (timeseries ([1; 2; 3])), true)
%!assert_equal (istimeseries (timeseries ()), true)
%!assert_equal (istimeseries ([timeseries(1), timeseries(2)]), true)
%!assert_equal (istimeseries ([1, 2, 3]), false)
%!assert_equal (istimeseries ({timeseries(1)}), false)
%!assert_equal (istimeseries (timetable (seconds ([1; 2]), [1; 2])), false)
%!assert_equal (istimeseries ('timeseries'), false)
