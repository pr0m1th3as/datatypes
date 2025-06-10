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
## @deftypefn {datatypes} {@var{calD} =} caldays (@var{X})
##
## Calendar duration in days.
##
## @code{@var{calD} = caldays (@var{X})} returns a @qcode{calendarDuration}
## array representing calendar days equivalent to the values in @var{X}, which
## must be a numeric array of integer values.
##
## @code{caldays} is also available as a method for @qcode{calendarDuration}
## arrays, in which case it performs the opposite conversion.
##
## @seealso{calendarDuration, calyears, calquarters, calmonths, calweeks,
## calendarDuration.caldays}
## @end deftypefn
function out = caldays (x)
  ## Check input
  if (! isnumeric (x))
    error ("caldays: input array must be numeric.");
  elseif (any (fix (x(:)) != x(:)))
    error ("caldays: input array must contain only integer values.");
  endif
  out = calendarDuration (0, 0, double (x));
endfunction

%!test
%! X = magic (3);
%! D = caldays (X);
%! assert (size (D), size (X));
%!test
%! D = caldays ([1, 2, 3]);
%! assert (caldays (D), [1, 2, 3]);
%!test
%! D = caldays (int16 (1));
%! assert (caldays (D), 1);

%!error<caldays: input array must be numeric.> caldays ("asd");
%!error<caldays: input array must contain only integer values.> ...
%! caldays (1.2);

