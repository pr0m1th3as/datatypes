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
## @deftypefn  {datatypes} {@var{key} =} keyHash (@var{X})
## @deftypefnx {datatypes} {@var{key} =} keyHash (@var{X}, @var{base})
##
## Generate a hash code for an array.
##
## @code{@var{key} = keyHash (@var{X})} generates a @qcode{uint64} scalar that
## represents the input @var{X}, which may be a numeric, logical, character,
## cell, struct or function handle array.  @code{keyHash} utilizes the 64-bit
## FNV-1a variant of the Fowler-Noll-Vo non-cryptographic hash function.
##
## A @qcode{cell} array is keyed on its elements, each hashed in turn, so a
## cell may hold any type that @code{keyHash} accepts, nested to any depth.  A
## @qcode{struct} is keyed on its field names and their values; the order in
## which its fields were created is @emph{not} part of the key, so two structs
## differing only in field order are the same key.  A function handle is keyed
## on its text alone: two named handles for the same function are one key,
## while distinct anonymous handles never match even when written identically,
## so nothing a handle captures can be observed through @code{keyMatch}.
##
## @code{@var{key} = keyHash (@var{X}, @var{base})} also generates a 64-bit
## hash code using @var{base} as the offset basis for the FNV-1a hash
## algorithm.  @var{base} must be a @qcode{uint64} integer type scalar.  Use
## this syntax to cascade @code{keyHash} on multiple objects for which a
## single hash code is required.
##
## @code{keyMatch} decides key identity and @code{keyHash} agrees with it: two
## values that @code{keyMatch} reports as the same key always have the same
## hash code.  The converse does not hold, since distinct keys are permitted
## to share a hash code.  Values that differ only in a representation that
## @code{keyMatch} ignores therefore hash alike, so @code{-0} hashes as
## @code{0}, and every @qcode{NaN} hashes alike whatever produced it.
##
## Elements of a cell array of character vectors are hashed together with
## their lengths, so that the boundaries between them are part of the key and
## @code{@{'ab', 'c'@}} does not hash as @code{@{'a', 'bc'@}}.
##
## Note that unlike MATLAB, this implementation does not use any random seed.
## As a result, @code{keyHash} will always generate the exact same hash key
## for any particular input across different workers and Octave sessions.
## Hash codes are @emph{not} stable across package versions, however: those
## produced before version 1.3.1 are invalid and must be recomputed.
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
  if (! isempty (base))
    if (! (isscalar (base) && isa (base, 'uint64')))
      error ("keyHash: BASE must be a UINT64 scalar.");
    endif
    key = __ckeyHash__ (init_str, base);
  else
    key = __ckeyHash__ (init_str);
  endif
  ## Select data type
  if (isnumeric (x) || islogical (x))
    key = __nkeyHash__ (x(:), key);
  elseif (ischar (x))
    ## Passed whole: __ckeyHash__ reads the array in column-major order, so
    ## x(:) would only copy it to say what it already does.
    key = __ckeyHash__ (x, key);
  elseif (iscellstr (x))
    ## Passed whole, not flattened: __ckeyHash__ frames each element with its
    ## length so the boundaries between them are part of the key.  This branch
    ## stays ahead of the general cell one below so that a cellstr keeps the
    ## hash codes it has had since 1.3.1.
    key = __ckeyHash__ (x, key);
  elseif (iscell (x))
    ## Every element is hashed by a recursive call, in column-major order.
    ## Each one frames itself with its own size and class on the way in, so
    ## the boundaries between elements are part of the key for free and no
    ## two different arrangements of the same contents can collide.  An
    ## element of a class that has its own keyHash method reaches it, which
    ## is how a cell holding a datetime or categorical is keyed correctly
    ## without this function knowing about either.
    for i = 1:numel (x)
      key = keyHash (x{i}, key);
    endfor
  elseif (isstruct (x))
    ## Field names are hashed SORTED.  Two structs differing only in the
    ## order their fields were created are one key -- R2024a reports them as
    ## matching and hashes them alike, and isequaln agrees -- so storage
    ## order must not reach the hash.  Values follow in that same sorted
    ## order, element by element.
    fnames = sort (fieldnames (x));
    key = __ckeyHash__ (fnames, key);
    for i = 1:numel (x)
      for j = 1:numel (fnames)
        key = keyHash (x(i).(fnames{j}), key);
      endfor
    endfor
  elseif (is_function_handle (x))
    ## The function text alone.  A named handle is the same key as itself,
    ## while two anonymous handles never match however they were written, so
    ## nothing beyond the text can be observed through keyMatch.  The captured
    ## workspace is deliberately not hashed: R2024a hashes two handles that
    ## differ only in a captured value alike, and hashing it here would also
    ## make the key depend on values the handle merely closed over.
    key = __ckeyHash__ (func2str (x), key);
  else
    ## Name the class.  A cell or struct hashes its contents by recursion, so
    ## this is raised by the innermost frame and names the value that actually
    ## has no key -- not the container, which does.
    error ("keyHash: unsupported input type '%s'.", class (x));
  endif
endfunction

%!test
%! key = keyHash (1);
%! assert_equal (isscalar (key), true);
%! key = keyHash ([1:5]);
%! assert_equal (isscalar (key), true);
%!test
%! key1 = keyHash (1);
%! key2 = keyHash (1, 0xcbf29ce484222325); # default offset basis
%! assert_equal (key1, key2);
%!test
%! key1 = keyHash (0);
%! assert_equal (class (key1), 'uint64');
%!test
%! A = [1:5];
%! B = [1:5];
%! key1 = keyHash (A);
%! key2 = keyHash (B);
%! assert_equal (key1, key2);
%!test
%! A = [1:5];
%! B = [1:5]';
%! key1 = keyHash (A);
%! key2 = keyHash (B);
%! assert_equal (isequal (key1, key2), false);
%!test
%! A = '';
%! E = uint64 (15921358368119480423);
%! key = keyHash (A);
%! assert (isequal (key, E), sprintf ("k: %lx e: %lx d: %lx", key, E, key - E));
%! assert_equal (key, E);
%!test
%! A = uint64 (128);
%! E = uint64 (8038837787959150693);
%! key = keyHash (A);
%! assert (isequal (key, E), sprintf ("k: %lx e: %lx d: %lx", key, E, key - E));
%! assert_equal (key, E);

## Add more tests on endianness (see GitHub issue 43)
%!assert_equal (keyHash ([1, 2]), uint64 (1291405323040189622))
%!assert_equal (keyHash ([1+2i]), uint64 (4391366703481493027))
%!assert_equal (keyHash (single ([1, 2])), uint64 (10607299016200876763))
%!assert_equal (keyHash (int64 ([1, 2])), uint64 (8387921810608003298))
%!assert_equal (keyHash (uint64 ([1, 2])), uint64 (15129359585364357711))
%!assert_equal (keyHash (int32 ([1, 2])), uint64 (7340059757986521701))
%!assert_equal (keyHash (uint32 ([1, 2])), uint64 (6705001899901839972))
%!assert_equal (keyHash (int16 ([1, 2])), uint64 (2451959940856797955))
%!assert_equal (keyHash (uint16 ([1, 2])), uint64 (10303904549507999050))
%!assert_equal (keyHash (int8 ([1, 2])), uint64 (3623669810820070662))
%!assert_equal (keyHash (uint8 ([1, 2])), uint64 (12877398120156278681))
%!assert_equal (keyHash (logical ([1, 2])), uint64 (16918259908176314871))
%!assert_equal (__nkeyHash__ ([1, 2]), uint64 (4062974330926783736))
%!assert_equal (__nkeyHash__ ([1+2i]), uint64 (4062974330926783736))
%!assert_equal (__nkeyHash__ (single ([1, 2])), uint64 (10375111465485032152))
%!assert_equal (__nkeyHash__ (int64 ([1, 2])), uint64 (8581494755304202342))
%!assert_equal (__nkeyHash__ (uint64 ([1, 2])), uint64 (8581494755304202342))
%!assert_equal (__nkeyHash__ (int32 ([1, 2])), uint64 (14538333428393601222))
%!assert_equal (__nkeyHash__ (uint32 ([1, 2])), uint64 (14538333428393601222))
%!assert_equal (__nkeyHash__ (int16 ([1, 2])), uint64 (12479921481467174326))
%!assert_equal (__nkeyHash__ (uint16 ([1, 2])), uint64 (12479921481467174326))
%!assert_equal (__nkeyHash__ (int8 ([1, 2])), uint64 (589729691727335466))
%!assert_equal (__nkeyHash__ (uint8 ([1, 2])), uint64 (589729691727335466))
%!assert_equal (__nkeyHash__ (logical ([1, 2])), uint64 (589728592215707255))

## Add more tests on different architectures (see GitHub issues 38 and 40)
%!assert_equal (__nkeyHash__ (uint8 (128)), uint64 (5808531584386460767))
%!assert_equal (keyHash (uint8 (128)), uint64 (10759574069356082695))
%!assert_equal (__nkeyHash__ (uint32 (0xdeadbeef)), ...
%!              uint64 (12840711468051582507))
%!assert_equal (keyHash (uint32 (0xdeadbeef)), uint64 (3790509136731937468))

## Composite keys are hashed with their element boundaries framed, so a cell
## array is not confusable with any other splitting of the same characters.
%!test
%! assert_equal (isequal (keyHash ({'ab', 'c'}), keyHash ({'a', 'bc'})), false);
%! assert_equal (isequal (keyHash ({'abc'}), keyHash ({'ab', 'c'})), false);
%! assert_equal (isequal (keyHash ({'a', 'b', 'c'}), keyHash ({'ab', 'c'})), ...
%!               false);
%! assert_equal (keyHash ({'ab', 'c'}), keyHash ({'ab', 'c'}));

## An empty cell hashes rather than erroring, and stays distinct from a cell
## holding one empty character vector.
%!test
%! assert_equal (class (keyHash ({})), 'uint64');
%! assert_equal (keyHash ({}), keyHash ({}));
%! assert_equal (isequal (keyHash ({}), keyHash ({''})), false);
%! assert_equal (isequal (keyHash ({''}), keyHash ({'', ''})), false);
%! assert_equal (isequal (keyHash ({''}), keyHash ('')), false);

## Shape is carried by the size in the init string, so a row and a column of
## the same elements are different keys.
%!test
%! assert_equal (isequal (keyHash ({'a', 'b'}), keyHash ({'a'; 'b'})), false);
%! assert_equal (isequal (keyHash ({}), keyHash (cell (1, 0))), false);

## A character array is hashed whole.  Until 1.3.1 keyHash passed x(:), an
## N-by-1 char matrix, which was read with string_value () -- taking only the
## first row.  Every character vector was therefore hashed as its first
## character alone, so 'hello' and 'hxxxx' were one key.
%!test
%! assert_equal (isequal (keyHash ('hello'), keyHash ('hxxxx')), false);
%! assert_equal (isequal (keyHash ('abc'), keyHash ('axx')), false);
%! assert_equal (isequal (keyHash ('abc'), keyHash ('abd')), false);
%! assert_equal (keyHash ('abc'), keyHash ('abc'));
%! assert_equal (isequal (keyHash (['ab'; 'cd']), keyHash (['ab'; 'ce'])), ...
%!               false);
%! assert_equal (keyHash (['ab'; 'cd']), keyHash (['ab'; 'cd']));

## The class is part of the key, so a character vector and a cell holding it
## never collide.
%!test
%! assert_equal (isequal (keyHash ('ab'), keyHash ({'ab'})), false);
%! assert_equal (isequal (keyHash (''), keyHash ({})), false);

## Signed zero and NaN are canonicalised, so bit patterns that isequaln calls
## equal are one key.  Integer types have neither and are hashed untouched.
%!test
%! assert_equal (keyHash (0), keyHash (-0));
%! assert_equal (keyHash (0), keyHash (0 * -1));
%! assert_equal (keyHash (NaN), keyHash (0/0));
%! assert_equal (keyHash (NaN), keyHash (Inf - Inf));
%! assert_equal (keyHash ([0, NaN]), keyHash ([-0, 0/0]));
%! assert_equal (keyHash (single (0)), keyHash (single (-0)));
%! assert_equal (keyHash (single (NaN)), keyHash (single (0/0)));
%! assert_equal (isequal (keyHash (0), keyHash (NaN)), false);
%! assert_equal (isequal (keyHash (Inf), keyHash (-Inf)), false);

## A zero offset basis is a basis like any other and must not be read as no
## basis at all.  The guard tested truthiness, so uint64 (0) was silently
## replaced by the default basis, and every other falsy value -- 0, false, and
## any uint64 array containing a zero -- skipped validation entirely.
%!test
%! assert_equal (class (keyHash (1, uint64 (0))), 'uint64');
%! assert_equal (keyHash (1, uint64 (0)), keyHash (1, uint64 (0)));
%! assert_equal (isequal (keyHash (1), keyHash (1, uint64 (0))), false);
%! ## an empty base is the default, meaning no base at all
%! assert_equal (keyHash (1, []), keyHash (1));

%!error<Invalid call to keyHash.  Correct usage is:> keyHash ();
%!error<keyHash: BASE must be a UINT64 scalar.> keyHash (1, 1);
%!error<keyHash: BASE must be a UINT64 scalar.> keyHash (1, 0);
%!error<keyHash: BASE must be a UINT64 scalar.> keyHash (1, false);
%!error<keyHash: BASE must be a UINT64 scalar.> keyHash (1, uint64 ([0, 1]));
## The error names the value that has no key, not the container holding it:
## a cell and a struct are both hashable, so reporting either would send the
## caller after the wrong type.
%!error<keyHash: unsupported input type 'table'.> keyHash (table (1));
%!error<keyHash: unsupported input type 'table'.> keyHash ({table (1)});
%!error<keyHash: unsupported input type 'table'.> keyHash ({1, table (1), 3});
%!error<keyHash: unsupported input type 'table'.> keyHash ({{table (1)}});
%!error<keyHash: unsupported input type 'table'.> keyHash (struct ('a', table (1)));
%!error<keyHash: unsupported input type 'table'.> ...
%! keyHash (struct ('a', 1, 'b', table (1)));

## A struct is keyed on its field names and their values.  The order the
## fields were created in is NOT part of the key: R2024a reports two structs
## differing only in field order as one key and hashes them alike.
%!assert_equal (keyHash (struct ('a', 1)), keyHash (struct ('a', 1)))
%!assert_equal (keyHash (struct ('a', 1, 'b', 2)), keyHash (struct ('b', 2, 'a', 1)))
%!assert_equal (keyHash (struct ()), keyHash (struct ()))
%!assert_equal (isequal (keyHash (struct ('a', 1)), keyHash (struct ('a', 2))), false)
%!assert_equal (isequal (keyHash (struct ('a', 1)), keyHash (struct ('b', 1))), false)
%!assert_equal (isequal (keyHash (struct ('a', 1)), keyHash (struct ('a', 1, 'b', 2))), false)
%!assert_equal (isequal (keyHash (struct ('a', 1)), keyHash (struct ('a', int8 (1)))), false)
%!test  ## nesting, and a struct array against a scalar of the same field
%! assert_equal (keyHash (struct ('a', struct ('b', 1))), ...
%!               keyHash (struct ('a', struct ('b', 1))));
%! assert_equal (isequal (keyHash (struct ('a', struct ('b', 1))), ...
%!                        keyHash (struct ('a', struct ('b', 2)))), false);
%!test
%! A(1).a = 1; A(2).a = 2;
%! B(1).a = 1; B(2).a = 2;
%! assert_equal (keyHash (A), keyHash (B));
%! assert_equal (isequal (keyHash (A), keyHash (A')), false);

## A cell is keyed on its elements, each framed by its own size and class on
## the way in, so nesting and shape are part of the key and any type keyHash
## accepts may appear inside.
%!assert_equal (keyHash ({1, 2}), keyHash ({1, 2}))
%!assert_equal (keyHash ({}), keyHash ({}))
%!assert_equal (keyHash ({1, 'a'}), keyHash ({1, 'a'}))
%!assert_equal (keyHash ({{1}}), keyHash ({{1}}))
%!assert_equal (keyHash ({struct('a', 1)}), keyHash ({struct('a', 1)}))
%!assert_equal (keyHash ({@sin}), keyHash ({@sin}))
%!assert_equal (isequal (keyHash ({1, 2}), keyHash ({1, 3})), false)
%!assert_equal (isequal (keyHash ({1, 2}), keyHash ({1; 2})), false)
%!assert_equal (isequal (keyHash ({{1}}), keyHash ({1})), false)
%!assert_equal (isequal (keyHash ({1}), keyHash ({int8(1)})), false)

## A cell array of character vectors is hashed by the cellstr path, which sits
## ahead of the general cell one, so it keeps the codes it has had since 1.3.1
## and the element boundaries stay part of the key.
%!assert_equal (isequal (keyHash ({'ab', 'c'}), keyHash ({'a', 'bc'})), false)

## A function handle is keyed on its text alone.  Two named handles for the
## same function are one key.  What an anonymous handle captures is NOT part
## of the key -- R2024a hashes two handles differing only in a captured value
## alike, and distinct anonymous handles never match in any case.
%!assert_equal (keyHash (@sin), keyHash (@sin))
%!assert_equal (keyHash (@(x) x + 1), keyHash (@(x) x + 1))
%!assert_equal (isequal (keyHash (@sin), keyHash (@cos)), false)
%!assert_equal (isequal (keyHash (@(x) x + 1), keyHash (@(x) x + 2)), false)
%!assert_equal (isequal (keyHash (@sin), keyHash ('sin')), false)
%!test
%! n = 1; f = @(x) x + n;
%! n = 2; g = @(x) x + n;
%! assert_equal (keyHash (f), keyHash (g));

## The class is part of every key, so none of the new types collides with
## another or with a numeric holding the same contents.
%!assert_equal (isequal (keyHash (struct ('a', 1)), keyHash ({1})), false)
%!assert_equal (isequal (keyHash ({1}), keyHash (1)), false)
%!assert_equal (isequal (keyHash (@sin), keyHash (1)), false)

## Frozen hash codes, measured from the 1.3.1 release and baked in.  Hash codes
## are a published interface here -- this package offers codes that are stable
## across workers and sessions, which MATLAB's randomly seeded keyHash does not
## -- so a code changing is a breaking change and must be a deliberate one.
## 1.3.1 changed many of these on purpose and said so in NEWS; these literals
## exist so the next change cannot happen by accident.  A failure here is not a
## test to update: it means stored keys from 1.3.1 no longer match, and either
## the change is reverted or the release notes carry it.
%!assert_equal (keyHash (1), uint64 (10584184954000009987))
%!assert_equal (keyHash (0), uint64 (7866423903081946394))
%!assert_equal (keyHash (NaN), uint64 (10576514760883123851))
%!assert_equal (keyHash (Inf), uint64 (10584114585255804483))
%!assert_equal (keyHash (int8 (1)), uint64 (12242639213500019555))
%!assert_equal (keyHash (uint64 (7)), uint64 (7587370060813975170))
%!assert_equal (keyHash (single (1.5)), uint64 (5289544833140628974))
%!assert_equal (keyHash (true), uint64 (5335886618314126645))
%!assert_equal (keyHash ([1:5]), uint64 (1432333402695462731))
%!assert_equal (keyHash ([1:5]'), uint64 (9563671367466119291))
%!assert_equal (keyHash (zeros (2, 3)), uint64 (11685217659039319009))
%!assert_equal (keyHash ([]), uint64 (15028164940097692402))
%!assert_equal (keyHash ('a'), uint64 (9989574163509938906))
%!assert_equal (keyHash ('hello'), uint64 (10275689077389484113))
%!assert_equal (keyHash (''), uint64 (15921358368119480423))
%!assert_equal (keyHash (['ab'; 'cd']), uint64 (17517398146429816181))
%!assert_equal (keyHash ({}), uint64 (12134391156607149443))
%!assert_equal (keyHash ({''}), uint64 (17643749256637597450))
%!assert_equal (keyHash ({'a'}), uint64 (10819011992493875390))
%!assert_equal (keyHash ({'ab', 'c'}), uint64 (5911896368001593873))
%!assert_equal (keyHash ({'a', 'bc'}), uint64 (2035578884344404753))
%!assert_equal (keyHash (1, uint64 (12345)), uint64 (1427258369762746415))
