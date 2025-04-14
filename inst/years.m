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
## @deftypefn {datatypes} {@var{D} =} years (@var{X})
##
## Fixed-time duration in years.
##
## @code{@var{D} = years (@var{X})} returns a @qcode{duration} array
## representing fixed-time duration years equivalent to the values in @var{X},
## which must be a numeric array.  A fixed-length year is equal to 365.2425
## days.
##
## @code{years} is also available as a method for @qcode{duration} arrays, in
## which case it performs the opposite conversion.
##
## @seealso{duration, days, hours, minutes, seconds, milliseconds,
## duration.years}
## @end deftypefn
function out = years (x)
  if (! isnumeric (x))
    error ("years: input array must be numeric.");
  endif
  out = duration (x * 24 * 365.2425, 0, 0);
endfunction

%!test
%! X = magic (3);
%! D = years (X);
%! assert (size (D), size (X));
%!test
%! D = years ([1, 2, 3]);
%! assert (years (D), [1, 2, 3]);

%!error<years: input array must be numeric.> years ("asd");
