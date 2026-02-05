## Copyright (C) 2025-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn {datatypes} {@var{TF} =} isnat (@var{T})
##
## Test for Not-A-Time elements in datetime array.
##
## @code{@var{TF} = isnat (@var{T})} returns a logical array @var{TF} of the
## same size as @var{T} containing @qcode{true} for each corresponding
## element of @var{T} that is Not-A-Time (@qcode{NaT}) and @qcode{false}
## otherwise.  @qcode{NaT} is the equivalent of @qcode{NaN} in numeric
## arrays.
##
## If @var{T} is not a datetime array, @code{isnat} returns an error.
##
## @end deftypefn
function TF = isnat (T)
  error ("isnat: input argument must be a datetime array.");
endfunction

%!error <isnat: input argument must be a datetime array.> isnat (1)
%!error <isnat: input argument must be a datetime array.> isnat ({'asd'})
%!error <isnat: input argument must be a datetime array.> isnat ('er')
%!error <isnat: input argument must be a datetime array.> isnat (ones (2, 3))
%!error <isnat: input argument must be a datetime array.> isnat (days (2))
