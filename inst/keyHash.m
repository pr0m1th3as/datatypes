## Copyright (C) 2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn  {datatypes} {@var{hey} =} keyHash (@var{X})
## @deftypefnx {datatypes} {@var{hey} =} keyHash (@var{X}, @var{base})
##
## Generate a hash code for a categorical array.
##
## @code{@var{h} = keyHash (@var{X})} generates a @qcode{uint64} scalar that
## represents the input array @var{X}.  @code{keyHash} utilizes the 64-bit
## FMV-1a variant of the Fowler-Noll-Vo non-cryptographic hash function.
##
## @code{@var{h} = keyHash (@var{X}), @var{base}} also generates a 64-bit
## hash code using @var{base} as the offset basis for the FNV-1a hash
## algorithm.  @var{base} must be a @qcode{uint64} integer type scalar.  Use
## this syntax to cascade @code{keyHash} on multiple objects for which a
## single hash code is required.
##
## Note that unlike MATLAB, this implementation does no use any random seed.
## As a result, @code{keyHash} will always generate the exact same hash key
## for any particular input across different workers and Octave sessions.
##
## @end deftypefn
function key = keyHash (x = [], base = [])
  ## Validate input
  if (isempty (x))
    error ("keyHash: X cannot not be empty.");
  endif
  ## Initialize string with size and class name
  size_str = sprintf ('%dx', size (x))(1:end-1);
  init_str = [size_str class(x)];
  if (base)
    if (! (isscalar (base) && isa (base, 'uint64')))
      error ("keyHash: BASE must be a UINT64 scalar.");
    endif
    key = __ckeyHash__(init_str, base);
  else
    key = __ckeyHash__(init_str);
  endif
  ## Select data type
  if (isnumeric (x) || islogical (x))
    key = __nkeyHash__(x(:), key);
  elseif (ischar (x))
    key = __ckeyHash__(x(:), key);
  elseif (iscellstr (x))
    key = __ckeyHash__([x{:}], key);
  else
    error ("keyHash: unsupported input type.");
  endif
endfunction

%!test
%! key = keyHash (1);
%! assert (isscalar (key), true);
%! key = keyHash ([1:5]);
%! assert (isscalar (key), true);
%!test
%! key1 = keyHash (1);
%! key2 = keyHash (1, 0xcbf29ce484222325); # default offset basis
%! assert (key1, key2);
%!test
%! key1 = keyHash (0);
%! assert (class (key1), 'uint64');
%!test
%! A = [1:5];
%! B = [1:5];
%! key1 = keyHash (A);
%! key2 = keyHash (B);
%! assert (key1, key2);
%!test
%! A = [1:5];
%! B = [1:5]';
%! key1 = keyHash (A);
%! key2 = keyHash (B);
%! assert (isequal (key1, key2), false);

%!error<keyHash: X cannot not be empty.> keyHash ();
%!error<keyHash: BASE must be a UINT64 scalar.> keyHash (1, 1);
%!error<keyHash: unsupported input type.> keyHash (@(x) x);
