## Copyright (C) 2024-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn {datatypes} {@var{TF} =} isduration (@var{X})
##
## True if input is a @code{duration} array, false otherwise.
##
## @code{@var{TF} = isduration (@var{X})} always returns a logical scalar,
## irrespective of the size of @var{X}.
##
## @end deftypefn
function TF = isduration (x)
  TF = isa (x, 'duration');
endfunction

%!assert (isduration (duration ([1, 2, 3])), true);
%!assert (isduration (duration ([1, 2, 0; 1, 4, 0])), true);
%!assert (isduration ([0, 1, 2]), false);
%!assert (isduration ({true, false}), false);
%!assert (isduration ({duration([1, 2, 3])}), false);
