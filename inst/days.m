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
## @deftypefn {datatypes} {@var{D} =} days (@var{X})
##
## Fixed-time duration in days.
##
## @code{@var{D} = days (@var{X})} returns a @qcode{duration} array representing
## fixed-time duration days equivalent to the values in @var{X}, which must be a
## numeric array.
##
## @code{days} is also available as a method for @qcode{duration} arrays, in
## which case it performs the opposite conversion.
##
## @seealso{duration, years, hours, minutes, seconds, milliseconds,
## duration.days}
## @end deftypefn
function out = days (x)
  if (nargin == 0)
    x = 1;
  elseif (! isnumeric (x))
    error ("days: input array must be numeric.");
  elseif (! isreal (x))
    error ("days: input array must be real.");
  endif
  out = duration (double (x) * 24, 0, 0, 'Format', 'd');
endfunction

%!test
%! X = magic (3);
%! D = days (X);
%! assert (size (D), size (X));
%!test
%! D = days ([1, 2, 3]);
%! assert (days (D), [1, 2, 3]);
%!test
%! D = days (int16 (1));
%! assert (days (D), 1);
%!test
%! D = days ();
%! assert (days (D), 1);

%!error<days: input array must be numeric.> days ("asd");
%!error<days: input array must be real.> days (1+i);
