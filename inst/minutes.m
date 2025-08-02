## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn {datatypes} {@var{D} =} minutes (@var{X})
##
## Fixed-time duration in minutes.
##
## @code{@var{D} = minutes (@var{X})} returns a @qcode{duration} array
## representing fixed-time duration minutes equivalent to the values in @var{X},
## which must be a numeric array.
##
## @code{minutes} is also available as a method for @qcode{duration} arrays, in
## which case it performs the opposite conversion.
##
## @seealso{duration, years, days, hours, seconds, milliseconds,
## duration.minutes}
## @end deftypefn
function out = minutes (x)
  if (nargin == 0)
    x = 1;
  elseif (! isnumeric (x))
    error ("minutes: input array must be numeric.");
  elseif (! isreal (x))
    error ("minutes: input array must be real.");
  endif
  out = duration (0, double (x), 0, 'Format', 'm');
endfunction

%!test
%! X = magic (3);
%! D = minutes (X);
%! assert (size (D), size (X));
%!test
%! D = minutes ([1, 2, 3]);
%! assert (minutes (D), [1, 2, 3]);
%!test
%! D = minutes (int16 (1));
%! assert (minutes (D), 1);
%!test
%! D = minutes ();
%! assert (minutes (D), 1);

%!error<minutes: input array must be numeric.> minutes ("asd");
%!error<minutes: input array must be real.> minutes (1+i);
