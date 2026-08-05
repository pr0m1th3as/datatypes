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
## Return true if both inputs are the same key.
##
## @code{@var{TF} = keyMatch (@var{A}, @var{B})} returns a logical scalar,
## which is @qcode{true}, if the inputs @var{A} and @var{B} are the same key,
## and @qcode{false} otherwise.  Two values are the same key when they have
## the same class, the same size, and equal contents, with missing values
## comparing equal to one another as in @code{isequaln}.  Neither class nor
## size is promoted, so @code{1} and @code{int8 (1)} are different keys, and
## so are a row and a column holding the same elements.
##
## @code{keyMatch} decides key identity and @code{keyHash} agrees with it:
## two values that @code{keyMatch} reports as the same key always have the
## same hash code.  The converse does not hold, since distinct keys are
## permitted to share a hash code, and so comparing hash codes is not a
## substitute for calling @code{keyMatch}.
##
## @end deftypefn
function TF = keyMatch (A, B)
  if (nargin != 2)
    print_usage;
  endif
  ## Identity is decided on the values, not on their hash codes: hash equality
  ## is not key equality, and any collision would silently report two distinct
  ## keys as one.  keyHash agrees with what is decided here, never the reverse.
  if (! strcmp (class (A), class (B)))
    TF = false;
  elseif (! isequal (size (A), size (B)))
    TF = false;
  else
    TF = isequaln (A, B);
  endif
endfunction

%!assert_equal (keyMatch (1, {'1'}), false);
%!assert_equal (keyMatch (ones (2), 2), false);
%!assert_equal (keyMatch (1, 2), false);
%!assert_equal (keyMatch (1, 1), true);

## The equality table.  Every row asserts BOTH directions of the contract:
## keyMatch decides identity, and any pair it calls equal must also hash
## equally, or a hash-bucketed container would never reach the candidate.
## Asserting only keyMatch would let the two drift apart again.

## No promotion across classes: the class is part of the key.
%!test
%! assert_equal (keyMatch (1, int8 (1)), false);
%! assert_equal (keyMatch (1, single (1)), false);
%! assert_equal (keyMatch (1, true), false);
%! assert_equal (keyMatch (string ('a'), 'a'), false);

## Shape is part of the key.
%!test
%! assert_equal (keyMatch ([1, 2], [1; 2]), false);
%! assert_equal (keyMatch (zeros (0, 0), zeros (1, 0)), false);
%! assert_equal (keyMatch ({}, {''}), false);

## Missing values compare equal -- isequaln semantics, not isequal.
%!test
%! assert_equal (keyMatch (NaN, NaN), true);
%! assert_equal (keyHash (NaN), keyHash (NaN));
%! assert_equal (keyMatch ([1, NaN], [1, NaN]), true);
%! assert_equal (keyHash ([1, NaN]), keyHash ([1, NaN]));

## A NaN is a NaN whatever bit pattern produced it.  On x86-64 the literal
## NaN is positive but 0/0 and Inf-Inf set the sign bit, so hashing raw bytes
## made two isequaln-equal arrays different keys.
%!test
%! assert_equal (keyMatch (NaN, 0/0), true);
%! assert_equal (keyHash (NaN), keyHash (0/0));
%! assert_equal (keyMatch (NaN, Inf - Inf), true);
%! assert_equal (keyHash (NaN), keyHash (Inf - Inf));
%! assert_equal (keyMatch ([1, NaN], [1, 0/0]), true);
%! assert_equal (keyHash ([1, NaN]), keyHash ([1, 0/0]));
%! assert_equal (keyMatch (single (NaN), single (0/0)), true);
%! assert_equal (keyHash (single (NaN)), keyHash (single (0/0)));

## Signed zero is one key, from the literal and from arithmetic alike.
%!test
%! assert_equal (keyMatch (0, -0), true);
%! assert_equal (keyHash (0), keyHash (-0));
%! assert_equal (keyMatch (0, 0 * -1), true);
%! assert_equal (keyHash (0), keyHash (0 * -1));
%! assert_equal (keyMatch (single (0), single (-0)), true);
%! assert_equal (keyHash (single (0)), keyHash (single (-0)));
%! assert_equal (keyMatch ([0, 1], [-0, 1]), true);
%! assert_equal (keyHash ([0, 1]), keyHash ([-0, 1]));

## Integers have neither signed zero nor NaN; they must be left alone.
%!test
%! assert_equal (keyMatch (int8 (0), int8 (0)), true);
%! assert_equal (keyMatch (int8 (0), int8 (1)), false);
%! assert_equal (keyMatch (uint8 (0), uint8 (0)), true);

## Complex: a zero imaginary part still compares equal to the real value.
%!test
%! assert_equal (keyMatch (complex (1, 0), 1), true);
%! assert_equal (keyHash (complex (1, 0)), keyHash (1));
%! assert_equal (keyMatch (1 + 2i, 1 + 3i), false);

## Character arrays are compared over their whole contents.  Until 1.3.1 this
## compared hash codes, and keyHash hashed only a character vector's first
## character, so any two equal-length vectors sharing it matched.
%!test
%! assert_equal (keyMatch ('hello', 'hxxxx'), false);
%! assert_equal (keyMatch ('hello', 'hello'), true);
%! assert_equal (keyHash ('hello'), keyHash ('hello'));
%! assert_equal (keyMatch ('abc', 'abd'), false);
%! assert_equal (keyMatch (['ab'; 'cd'], ['ab'; 'ce']), false);
%! assert_equal (keyMatch (['ab'; 'cd'], ['ab'; 'cd']), true);
%! assert_equal (keyHash (['ab'; 'cd']), keyHash (['ab'; 'cd']));

## Composite keys keep their element boundaries.  Flattening made {'ab','c'}
## and {'a','bc'} one key.
%!test
%! assert_equal (keyMatch ({'ab', 'c'}, {'a', 'bc'}), false);
%! assert_equal (isequal (keyHash ({'ab', 'c'}), keyHash ({'a', 'bc'})), false);
%! assert_equal (keyMatch ({'ab', 'c'}, {'ab', 'c'}), true);
%! assert_equal (keyHash ({'ab', 'c'}), keyHash ({'ab', 'c'}));
%! assert_equal (keyMatch ({'a', '', 'b'}, {'a', 'b', ''}), false);
%! assert_equal (keyMatch ({'', 'ab'}, {'ab', ''}), false);

## The same for string arrays.
%!test
%! assert_equal (keyMatch (string ({'ab', 'c'}), string ({'a', 'bc'})), false);
%! assert_equal (isequal (keyHash (string ({'ab', 'c'})), ...
%!                        keyHash (string ({'a', 'bc'}))), false);
%! assert_equal (keyMatch (string ({'ab', 'c'}), string ({'ab', 'c'})), true);
%! assert_equal (keyMatch (string (missing), string (missing)), true);
%! assert_equal (keyHash (string (missing)), keyHash (string (missing)));

## Empty and degenerate composites are distinguishable from one another.
%!test
%! assert_equal (keyMatch ({}, {}), true);
%! assert_equal (keyHash ({}), keyHash ({}));
%! assert_equal (keyMatch ({''}, {''}), true);
%! assert_equal (keyHash ({''}), keyHash ({''}));
%! assert_equal (isequal (keyHash ({}), keyHash ({''})), false);
%! assert_equal (isequal (keyHash ({''}), keyHash ({'', ''})), false);
%! assert_equal (keyMatch ('', ''), true);
%! assert_equal (keyMatch ([], []), true);

## duration is counted in milliseconds, so the unit it was written in is not
## part of the key -- but a NaN duration is still one key.
%!test
%! assert_equal (keyMatch (seconds (1), milliseconds (1000)), true);
%! assert_equal (keyHash (seconds (1)), keyHash (milliseconds (1000)));
%! assert_equal (keyMatch (seconds (NaN), seconds (0/0)), true);
%! assert_equal (keyHash (seconds (NaN)), keyHash (seconds (0/0)));
%! assert_equal (keyMatch (seconds (1), seconds (2)), false);

## calendarDuration keeps calendar units distinct where they are not
## interchangeable, and merges them where they are.
%!test
%! assert_equal (keyMatch (calmonths (12), calyears (1)), true);
%! assert_equal (keyHash (calmonths (12)), keyHash (calyears (1)));
%! assert_equal (keyMatch (calmonths (1), caldays (30)), false);

## datetime is keyed on the instant, not on the wall clock and zone name.
%!test
%! a = datetime (2024, 3, 5, 12, 0, 0, 'TimeZone', 'UTC');
%! b = datetime (2024, 3, 5, 7, 0, 0, 'TimeZone', 'America/New_York');
%! assert_equal (keyMatch (a, b), true);
%! assert_equal (keyHash (a), keyHash (b));
%! assert_equal (keyMatch (NaT, NaT), true);
%! assert_equal (keyHash (NaT), keyHash (NaT));

## An unzoned datetime names no instant, so it is never the same key as a
## zoned one that happens to share its wall clock.
%!test
%! a = datetime (2024, 3, 5, 12, 0, 0, 'TimeZone', 'UTC');
%! u = datetime (2024, 3, 5, 12, 0, 0);
%! assert_equal (keyMatch (u, a), false);
%! assert_equal (keyMatch (u, datetime (2024, 3, 5, 12, 0, 0)), true);
%! assert_equal (keyHash (u), keyHash (datetime (2024, 3, 5, 12, 0, 0)));

## categorical identity is the label, plus the ordering only when ordinal.
%!test
%! a = categorical ({'a'}, {'a', 'b'});
%! b = categorical ({'a'}, {'a', 'c'});
%! c = categorical ({'a'}, {'a', 'b', 'c'});
%! d = categorical ({'a'}, {'a', 'c', 'b'});
%! p = categorical ({'a'}, {'a', 'b'}, 'Protected', true);
%! assert_equal (keyMatch (a, b), true);
%! assert_equal (keyHash (a), keyHash (b));
%! assert_equal (keyMatch (c, d), true);
%! assert_equal (keyHash (c), keyHash (d));
%! assert_equal (keyMatch (a, p), true);
%! assert_equal (keyHash (a), keyHash (p));

## For ordinals the whole category ordering is part of identity, and an
## ordinal is never equal to a non-ordinal.
%!test
%! o = categorical ({'a'}, {'a', 'b'}, 'Ordinal', true);
%! o2 = categorical ({'a'}, {'b', 'a'}, 'Ordinal', true);
%! n = categorical ({'a'}, {'a', 'b'});
%! assert_equal (keyMatch (o, n), false);
%! assert_equal (keyMatch (o, o2), false);
%! assert_equal (keyMatch (o, categorical ({'a'}, {'a', 'b'}, 'Ordinal', true)), ...
%!               true);

## Undefined categorical elements compare equal to one another.
%!test
%! u = categorical ({''}, {'a'});
%! assert_equal (keyMatch (u, categorical ({''}, {'a'})), true);
%! assert_equal (keyHash (u), keyHash (categorical ({''}, {'a'})));
%! assert_equal (keyMatch (u, categorical ({'a'}, {'a'})), false);

%!error<Invalid call to keyMatch.  Correct usage is:> keyMatch (2);
