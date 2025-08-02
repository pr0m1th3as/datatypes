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
## @deftypefn {datatypes} {@var{TF} =} istable (@var{X})
##
## True if input is a @code{table}, false otherwise.
##
## @code{@var{TF} = istable (@var{X})} always returns a logical scalar,
## irrespective of the size of @var{X}.
##
## @end deftypefn
function TF = istable (x)
  TF = isa (x, 'table');
endfunction

%!assert (istable (table ([1, 2, 3])), true);
%!assert (istable (table ([1, 2, 0; 1, 4, 0])), true);
%!assert (istable ([0, 1, 2]), false);
%!assert (istable ({true, false}), false);
%!assert (istable ({table([1, 2, 3])}), false);
