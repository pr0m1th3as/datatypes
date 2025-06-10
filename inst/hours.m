## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
##
## This file is part of the statistics package for GNU Octave.
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
## @deftypefn {datatypes} {@var{D} =} hours (@var{X})
##
## Fixed-time duration in hours.
##
## @code{@var{D} = hours (@var{X})} returns a @qcode{duration} array
## representing fixed-time duration hours equivalent to the values in @var{X},
## which must be a numeric array.
##
## @code{hours} is also available as a method for @qcode{duration} arrays, in
## which case it performs the opposite conversion.
##
## @seealso{duration, years, days, minutes, seconds, milliseconds,
## duration.hours}
## @end deftypefn
function out = hours (x)
  if (! isnumeric (x))
    error ("hours: input array must be numeric.");
  endif
  out = duration (double (x), 0, 0);
endfunction

%!test
%! X = magic (3);
%! D = hours (X);
%! assert (size (D), size (X));
%!test
%! D = hours ([1, 2, 3]);
%! assert (hours (D), [1, 2, 3]);
%!test
%! D = hours (int16 (1));
%! assert (hours (D), 1);

%!error<hours: input array must be numeric.> hours ("asd");
