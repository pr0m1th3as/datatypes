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
## @deftypefn {datatypes} {@var{TF} =} iscalendarduration (@var{X})
##
## True if input is a @code{calendarDuration} array, false otherwise.
##
## @code{@var{TF} = iscalendarduration (@var{X})} always returns a logical
## scalar, irrespective of the size of @var{X}.
##
## @end deftypefn
function TF = iscalendarduration (x)
  TF = isa (x, 'calendarDuration');
endfunction

%!assert (iscalendarduration (calendarDuration (0, 0, 0)), true);
%!assert (iscalendarduration (calendarDuration ([1, 2], 0, 0)), true);
%!assert (iscalendarduration ([0, 0, 0]), false);
%!assert (iscalendarduration ({0, 0, 0}), false);
%!assert (iscalendarduration ({calendarDuration(0, 0, 0)}), false);
