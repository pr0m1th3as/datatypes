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
## @deftypefn {datatypes} {@var{D} =} seconds (@var{X})
##
## Fixed-time duration in seconds.
##
## @code{@var{D} = seconds (@var{X})} returns a @qcode{duration} array
## representing fixed-time duration seconds equivalent to the values in @var{X},
## which must be a numeric array.
##
## @code{seconds} is also available as a method for @qcode{duration} arrays, in
## which case it performs the opposite conversion.
##
## @seealso{duration, years, days, hours, minutes, milliseconds,
## duration.seconds}
## @end deftypefn
function out = seconds (x)
  if (! isnumeric (x))
    error ("seconds: input array must be numeric.");
  endif
  out = duration (0, 0, double (x));
endfunction

%!test
%! X = magic (3);
%! D = seconds (X);
%! assert (size (D), size (X));
%!test
%! D = seconds ([1, 2, 3]);
%! assert (seconds (D), [1, 2, 3]);
%!test
%! D = seconds (int16 (1));
%! assert (seconds (D), 1);

%!error<seconds: input array must be numeric.> seconds ("asd");
