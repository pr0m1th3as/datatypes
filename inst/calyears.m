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
## @deftypefn {datatypes} {@var{calD} =} calyears (@var{X})
##
## Calendar duration in years.
##
## @code{@var{calD} = calyears (@var{X})} returns a @qcode{calendarDuration}
## array representing calendar years equivalent to the values in @var{X}, which
## must be a numeric array of integer values.
##
## @code{calyears} is also available as a method for @qcode{calendarDuration}
## arrays, in which case it performs the opposite conversion.
##
## @seealso{calendarDuration, calquarters, calmonths, calweeks, caldays,
## calendarDuration.calyears}
## @end deftypefn
function out = calyears (x)
  ## Check input
  if (nargin == 0)
    x = 1;
  elseif (! isnumeric (x))
    error ("calyears: input array must be numeric.");
  elseif (any (fix (x(:)) != x(:)))
    error ("calyears: input array must contain only integer values.");
  endif
  out = calendarDuration (double (x), 0, 0);
endfunction

%!test
%! X = magic (3);
%! D = calyears (X);
%! assert (size (D), size (X));
%!test
%! D = calyears ([1, 2, 3]);
%! assert (calyears (D), [1, 2, 3]);
%!test
%! D = calyears (int16 (1));
%! assert (calyears (D), 1);
%!test
%! D = calyears ();
%! assert (calyears (D), 1);

%!error<calyears: input array must be numeric.> calyears ("asd");
%!error<calyears: input array must contain only integer values.> ...
%! calyears (1.2);

