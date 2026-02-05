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
## @deftypefn {datatypes} {@var{TF} =} keyMatch (@var{A}, @var{B})
##
## Return true if both inputs have the same hash code.
##
## @code{@var{TF} = keyMatch (@var{A}, @var{B})} returns a logical scalar,
## which is @qcode{true}, if both inputs, @var{A} and @var{B}, have the same
## FNV-1a 64-bit hash code, and @qcode{false} otherwise.
##
## @end deftypefn
function TF = keyMatch (A, B)
  if (nargin != 2)
    print_usage;
  endif
  if (any (class (A) != class (B)))
    TF = false;
  elseif (isempty (A) || isempty (B))
    TF = false;
  else
    A_key = keyHash (A);
    B_key = keyHash (B);
    TF = A_key == B_key;
  endif
endfunction

%!assert (keyMatch (1, 2), false);
%!assert (keyMatch (1, 1), true);

%!error<Invalid call to keyMatch.  Correct usage is:> keyMatch (2);
