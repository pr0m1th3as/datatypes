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
## @deftypefn {datatypes} {@var{D} =} milliseconds (@var{X})
##
## Fixed-time duration in milliseconds.
##
## @code{@var{D} = milliseconds (@var{X})} returns a @qcode{duration} array
## representing fixed-time duration milliseconds equivalent to the values in @var{X},
## which must be a numeric array.
##
## @code{milliseconds} is also available as a method for @qcode{duration} arrays, in
## which case it performs the opposite conversion.
##
## @seealso{duration, years, days, hours, minutes, seconds,
## duration.milliseconds}
## @end deftypefn
function out = milliseconds (x)
  if (! isnumeric (x))
    error ("milliseconds: input array must be numeric.");
  endif
  out = duration (0, 0, x / 1000);
endfunction

%!test
%! X = magic (3);
%! D = milliseconds (X);
%! assert (size (D), size (X));
%!test
%! D = milliseconds ([1, 2, 3]);
%! assert (milliseconds (D), [1, 2, 3]);

%!error<milliseconds: input array must be numeric.> milliseconds ("asd");
