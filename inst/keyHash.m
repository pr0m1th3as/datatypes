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
## @deftypefn  {datatypes} {@var{hey} =} keyHash (@var{X})
## @deftypefnx {datatypes} {@var{hey} =} keyHash (@var{X}, @var{base})
##
## Generate a hash code for an array.
##
## @code{@var{h} = keyHash (@var{X})} generates a @qcode{uint64} scalar that
## represents the input @var{X}, which may be numeric, logical, or character
## array or cell array of character vectors.  @code{keyHash} utilizes the 64-bit
## FNV-1a variant of the Fowler-Noll-Vo non-cryptographic hash function.
##
## @code{@var{h} = keyHash (@var{X}, @var{base})} also generates a 64-bit
## hash code using @var{base} as the offset basis for the FNV-1a hash
## algorithm.  @var{base} must be a @qcode{uint64} integer type scalar.  Use
## this syntax to cascade @code{keyHash} on multiple objects for which a
## single hash code is required.
##
## Note that unlike MATLAB, this implementation does not use any random seed.
## As a result, @code{keyHash} will always generate the exact same hash key
## for any particular input across different workers and Octave sessions.
##
## @end deftypefn
function key = keyHash (x = [], base = [])
  ## Validate input
  if (nargin < 1)
    print_usage;
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
%!test
%! A = '';
%! E = uint64(15921358368119480423);
%! key = keyHash (A);
%! assert (isequal(key, E), sprintf("k: %lx e: %lx d: %lx", key, E, key - E));
%! assert (key, E);
%!test
%! A = uint64(128);
%! E = uint64(8038837787959150693);
%! key = keyHash (A);
%! assert (isequal(key, E), sprintf("k: %lx e: %lx d: %lx", key, E, key - E));
%! assert (key, E);

## Add more tests on endianness (see GitHub issue 43)
%!assert_equal (keyHash ([1, 2]), uint64 (1291405323040189622));
%!assert_equal (keyHash ([1+2i]), uint64 (4391366703481493027));
%!assert_equal (keyHash (single ([1, 2])), uint64 (10607299016200876763));
%!assert_equal (keyHash (int64 ([1, 2])), uint64 (8387921810608003298));
%!assert_equal (keyHash (uint64 ([1, 2])), uint64 (15129359585364357711));
%!assert_equal (keyHash (int32 ([1, 2])), uint64 (7340059757986521701));
%!assert_equal (keyHash (uint32 ([1, 2])), uint64 (6705001899901839972));
%!assert_equal (keyHash (int16 ([1, 2])), uint64 (2451959940856797955));
%!assert_equal (keyHash (uint16 ([1, 2])), uint64 (10303904549507999050));
%!assert_equal (keyHash (int8 ([1, 2])), uint64 (3623669810820070662));
%!assert_equal (keyHash (uint8 ([1, 2])), uint64 (12877398120156278681));
%!assert_equal (keyHash (logical ([1, 2])), uint64 (16918259908176314871));
%!assert_equal (__nkeyHash__ ([1, 2]), uint64 (4062974330926783736));
%!assert_equal (__nkeyHash__ ([1+2i]), uint64 (4062974330926783736));
%!assert_equal (__nkeyHash__ (single ([1, 2])), uint64 (10375111465485032152));
%!assert_equal (__nkeyHash__ (int64 ([1, 2])), uint64 (8581494755304202342));
%!assert_equal (__nkeyHash__ (uint64 ([1, 2])), uint64 (8581494755304202342));
%!assert_equal (__nkeyHash__ (int32 ([1, 2])), uint64 (14538333428393601222));
%!assert_equal (__nkeyHash__ (uint32 ([1, 2])), uint64 (14538333428393601222));
%!assert_equal (__nkeyHash__ (int16 ([1, 2])), uint64 (12479921481467174326));
%!assert_equal (__nkeyHash__ (uint16 ([1, 2])), uint64 (12479921481467174326));
%!assert_equal (__nkeyHash__ (int8 ([1, 2])), uint64 (589729691727335466));
%!assert_equal (__nkeyHash__ (uint8 ([1, 2])), uint64 (589729691727335466));
%!assert_equal (__nkeyHash__ (logical ([1, 2])), uint64 (589728592215707255));

%!error<Invalid call to keyHash.  Correct usage is:> keyHash ();
%!error<keyHash: BASE must be a UINT64 scalar.> keyHash (1, 1);
%!error<keyHash: unsupported input type.> keyHash (@(x) x);
