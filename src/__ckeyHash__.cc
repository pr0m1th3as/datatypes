/*
Copyright (C) 2025-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>

This file is part of the datatypes package for GNU Octave.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program; if not, see <http://www.gnu.org/licenses/>.
*/

#define FNV1A64_PRIME 0x00000100000001b3
#include <octave/oct.h>
#include <array>

using namespace std;

static inline constexpr uint64_t fnv1a64 (const char* buf, size_t len, uint64_t out)
{
  for (size_t i = 0; i < len; i++)
  {
    // default char signedness depends on architecture, for signed for
    // backwards compatibility
    const unsigned char b = buf[i];
    out = (out ^ b) * FNV1A64_PRIME;
  }
  return out;
}

namespace /* tests */ {
constexpr std::array<char, 4> testData{0, 1, char(0x80), 0};
static_assert(0 == fnv1a64(testData.data(), 0, 0));
static_assert(0x123 == fnv1a64(testData.data(), 0, 0x123));
static_assert(0 == fnv1a64(testData.data(), 1, 0));
static_assert(FNV1A64_PRIME == fnv1a64(testData.data(), 1, 1));
static_assert(FNV1A64_PRIME == fnv1a64(testData.data(), 2, 0));
}

// Hash a 64-bit count, least significant byte first so that the result does
// not depend on the architecture.
static inline uint64_t fnv1a64_count (uint64_t n, uint64_t out)
{
  for (size_t k = 0; k < 8; k++)
  {
    const unsigned char b = static_cast<unsigned char> ((n >> (8 * k)) & 0xff);
    out = (out ^ b) * FNV1A64_PRIME;
  }
  return out;
}


DEFUN_DLD (__ckeyHash__, args, nargout,
           "-*- texinfo -*-\n\
 @deftypefn  {} {@var{uint64} =} __ckeyHash__ (@var{str})\n\
 @deftypefnx {} {@var{uint64} =} __ckeyHash__ (@var{cstr})\n\
 @deftypefnx {} {@var{uint64} =} __ckeyHash__ (@dots{}, @var{FNV1A64_BASE})\n\
\n\
\n\
Fowler–Noll–Vo hash key for a character vector or a cell array of them. \n\
\n\
Given a cell array, the elements are hashed with their lengths so that the \
boundaries between them are part of the key and no two different splittings \
of the same characters can collide. \n\
\n\
This is a helper function for @qcode{keyHash} methods of `datatypes`' classes. \
Do NOT use this function directly. \n\
\n\
@end deftypefn")
{
  octave_uint64 base;
  // Validate input
  if (args.length () < 1)
  {
    error ("__ckeyHash__: too few input arguments.");
  }
  if (! (args(0).is_string () || args(0).iscell ()))
  {
    error ("__ckeyHash__: STR must be a character vector or a cell array of "
           "character vectors.");
  }
  // Get or assign a base value
  if (args.length() > 1)
  {
    base = args(1).uint64_scalar_value ();
  }
  else
  {
    base = 0xcbf29ce484222325;  // default FNV1A64_BASE
  }
  octave_uint64 out;
  if (args(0).iscell ())
  {
    // A composite is hashed as its element count followed by each element's
    // length and bytes.  Flattening the elements into one string instead --
    // which is what this did before -- loses the boundaries between them, so
    // {'ab', 'c'} and {'a', 'bc'} were one key.  Framing every element the
    // same way also makes later key types purely additive: a struct is a
    // field count followed by framed name/value pairs, one level deeper in
    // the same scheme, so no value fixed here has to change again.
    //
    // The count is hashed even when it is zero, which is what separates {}
    // from {''} and stops an empty composite from erroring.
    const Cell c = args(0).cell_value ();
    const octave_idx_type n = c.numel ();
    uint64_t key = fnv1a64_count (static_cast<uint64_t> (n), base);
    for (octave_idx_type i = 0; i < n; i++)
    {
      if (! c(i).is_string ())
      {
        error ("__ckeyHash__: cell elements must be character vectors.");
      }
      // Column-major order, matching what x(:) would give in m-code.  A char
      // matrix element is framed by its total length only, so it can collide
      // with a character vector holding the same characters; the two are
      // never equal keys, and the contract permits unequal keys to collide.
      const charNDArray el = c(i).char_array_value ();
      const octave_idx_type len = el.numel ();
      key = fnv1a64_count (static_cast<uint64_t> (len), key);
      key = fnv1a64 (el.data (), static_cast<size_t> (len), key);
    }
    out = key;
  }
  else
  {
    // A character vector is a single sequence, not a composite, so it is
    // hashed unframed and its hash code is unchanged by the framing above.
    //
    // char_array_value (), not string_value ().  The latter converts a
    // multi-row char matrix by taking its FIRST ROW and discarding the rest,
    // with only a warning -- and keyHash passed x(:), an N-by-1 matrix, so
    // every character vector was hashed as its first character alone.  In
    // 1.3.0 that made 'hello' and 'hxxxx' one key, and keyMatch compared hash
    // codes, so it called them equal.  char_array_value () reads the whole
    // array in column-major order, which is what x(:) meant to say.
    const charNDArray str = args(0).char_array_value ();
    out = fnv1a64 (str.data (), static_cast<size_t> (str.numel ()), base);
  }
  octave_value_list retval (nargout);
  retval(0) = out;
  return retval;
}
