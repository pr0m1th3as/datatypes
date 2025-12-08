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
## @deftypefn {datatypes} {@var{calD} =} calweeks (@var{X})
##
## Calendar duration in weeks.
##
## @code{@var{calD} = calweeks (@var{X})} returns a @qcode{calendarDuration}
## array representing calendar weeks equivalent to the values in @var{X}, which
## must be a numeric array of integer values.
##
## @code{calweeks} is also available as a method for @qcode{calendarDuration}
## arrays, in which case it performs the opposite conversion.
##
## @seealso{calendarDuration, calyears, calquarters, calmonths, caldays,
## calendarDuration.calweeks}
## @end deftypefn
function out = calweeks (x)
  ## Check input
  if (nargin == 0)
    x = 1;
  elseif (! isnumeric (x))
    error ("calweeks: input array must be numeric.");
  elseif (! isreal (x))
    error ("calweeks: input array must be real.");
  endif
  xx = x;
  xx(isnan (x)) = 0;
  if (any (fix (xx(:)) != xx(:)))
    error ("calweeks: input array must contain only integer values.");
  endif
  out = calendarDuration (0, 0, double (x) * 7, 'Format', 'ymwdt');
endfunction

%!test
%! X = magic (3);
%! D = calweeks (X);
%! assert (size (D), size (X));
%!test
%! D = calweeks ([1, 2, 3]);
%! assert (calweeks (D), [1, 2, 3]);
%!test
%! D = calweeks ([1, 2, NaN, 4]);
%! assert (calweeks (D), [1, 2, NaN, 4]);
%!test
%! D = calweeks (int16 (1));
%! assert (calweeks (D), 1);
%!test
%! D = calweeks ();
%! assert (calweeks (D), 1);

%!error<calweeks: input array must be numeric.> calweeks ("asd");
%!error<calweeks: input array must be real.> calweeks (1+i);
%!error<calweeks: input array must contain only integer values.> ...
%! calweeks (1.2);

