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
## @deftypefn {datatypes} {@var{calD} =} calquarters (@var{X})
##
## Calendar duration in quarters.
##
## @code{@var{calD} = calquarters (@var{X})} returns a @qcode{calendarDuration}
## array representing calendar quarters equivalent to the values in @var{X},
## which must be a numeric array of integer values.
##
## @code{calquarters} is also available as a method for @qcode{calendarDuration}
## arrays, in which case it performs the opposite conversion.
##
## @seealso{calendarDuration, calyears, calmonths, calweeks, caldays,
## calendarDuration.calquarters}
## @end deftypefn
function out = calquarters (x)
  ## Check input
  if (! isnumeric (x))
    error ("calquarters: input array must be numeric.");
  elseif (any (fix (x(:)) != x(:)))
    error ("calquarters: input array must contain only integer values.");
  endif
  out = calendarDuration (0, double (x) * 3, 0);
endfunction

%!test
%! X = magic (3);
%! D = calquarters (X);
%! assert (size (D), size (X));
%!test
%! D = calquarters ([1, 2, 3]);
%! assert (calquarters (D), [1, 2, 3]);
%!test
%! D = calquarters (int16 (1));
%! assert (calquarters (D), 1);

%!error<calquarters: input array must be numeric.> calquarters ("asd");
%!error<calquarters: input array must contain only integer values.> ...
%! calquarters (1.2);

