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
#include <cstring>
#include <array>

using namespace std;

inline bool isLittleEndian()
{
  unsigned int value = 0x01020304;
  unsigned char bytes[4] = {};
  std::memcpy (bytes, &value, sizeof (value));
  return bytes[0] == 0x04;
}

// Canonical little-endian bit patterns for a quiet NaN.  Floating point has
// two families of values whose in-memory representation is not unique but
// which 'isequaln' -- and hence 'keyMatch' -- calls equal: the two signed
// zeros, and every NaN.  Hashing raw bytes therefore gave two equal keys
// different hash codes, which breaks the keyHash/keyMatch contract: a
// hash-bucketed container never reaches a candidate whose hash differs.  The
// sign bit is not academic here.  On x86-64 the literal NaN is positive but
// '0/0' and 'Inf - Inf' set it, so 'seconds (NaN)' and 'seconds (0/0)' were
// different duration keys.
//
// These are written as fixed bytes rather than taken from std::numeric_limits
// so that a NaN hashes to the same code on every platform, which is the
// cross-session, cross-worker determinism keyHash documents.  They are also
// the patterns the literal NaN already had, so canonicalising moves the stray
// representations onto the existing hash codes rather than changing them.
static const unsigned char CANON_NAN64_LE[8]
  = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x7f};
static const unsigned char CANON_NAN32_LE[4] = {0x00, 0x00, 0xc0, 0x7f};

// Hash a floating point buffer, canonicalising -0.0 to +0.0 and every NaN to
// CANON_NAN*_LE.  Bytes are consumed little-endian first, matching the two
// integer paths below, so a value that is neither a signed zero nor a NaN
// hashes exactly as it did before this canonicalisation existed.
template <typename T>
static inline uint64_t fnv1a64_float (const char *buf, size_t nelem,
                                      const unsigned char *canon_nan,
                                      uint64_t out)
{
  const bool little = isLittleEndian ();
  unsigned char b[sizeof (T)];
  for (size_t i = 0; i < nelem; i++)
  {
    T v;
    std::memcpy (&v, buf + i * sizeof (T), sizeof (T));
    if (v != v)
    {
      // NaN, whatever its sign and payload
      std::memcpy (b, canon_nan, sizeof (T));
    }
    else
    {
      // true for both +0.0 and -0.0, and assigning collapses them
      if (v == T (0))
      {
        v = T (0);
      }
      std::memcpy (b, &v, sizeof (T));
      if (! little)
      {
        for (size_t k = 0; k < sizeof (T) / 2; k++)
        {
          const unsigned char t = b[k];
          b[k] = b[sizeof (T) - 1 - k];
          b[sizeof (T) - 1 - k] = t;
        }
      }
    }
    for (size_t k = 0; k < sizeof (T); k++)
    {
      // signedness as in the byte paths below, for backwards compatibility
      const signed char sb = static_cast<signed char> (b[k]);
      out = (out ^ sb) * FNV1A64_PRIME;
    }
  }
  return out;
}

static inline constexpr uint64_t fnv1a64 (const char* buf, size_t len, uint64_t out)
{
  for (size_t i = 0; i < len; i++)
  {
    // default char signedness depends on architecture, for signed for
    // backwards compatibility
    const signed char b = buf[i];
    out = (out ^ b) * FNV1A64_PRIME;
  }
  return out;
}

static inline constexpr uint64_t fnv1a64 (const char* buf, size_t len, size_t nbytes, uint64_t out)
{
  for (size_t i = 0; i < len; i++)
  {
    // reverse bytes for big endian systems
    size_t dv = i / nbytes;
    size_t md = i % nbytes;
    size_t ii = dv * nbytes + nbytes - md - 1;
    const signed char b = buf[ii];
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


DEFUN_DLD (__nkeyHash__, args, nargout,
           "-*- texinfo -*-\n\
 @deftypefn  {} {@var{uint64} =} __nkeyHash__ (@var{x})\n\
 @deftypefnx {} {@var{uint64} =} __nkeyHash__ (@var{x}, @var{FNV1A64_BASE})\n\
\n\
\n\
Fowler–Noll–Vo hash key for a numeric vector. \n\
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
    error ("__nkeyHash__: too few input arguments.");
  }
  if (! (args(0).isnumeric () || args(0).islogical ()))
  {
    error ("__nkeyHash__: X must be either numeric or logical.");
  }
  if (args(0).is_range ())
  {
    error ("__nkeyHash__: X cannot be a range.");
  }
  // Get or assign a base value
  if (args.length () > 1)
  {
    base = args(1).uint64_scalar_value ();
  }
  else
  {
    base = 0xcbf29ce484222325;  // default FNV1A64_BASE
  }
  // Cast numeric input to const char.  size_t, not uint32_t: byte_size ()
  // exceeds 4 GiB for a large array and truncating it would silently hash a
  // prefix of the data.
  size_t len = args(0).byte_size ();
  const void *in = args(0).mex_get_data ();
  const char *buf = static_cast<const char *>(in);
  // Generate the hash key
  octave_value_list retval (nargout);
  // Floating point first, so that -0.0 and NaN are canonicalised.  Complex
  // arrays are stored as interleaved real/imaginary components of the same
  // type and so are covered by the same pass; integer and logical types have
  // neither a signed zero nor a NaN and are hashed untouched.
  if (args(0).is_double_type ())
  {
    octave_uint64 out = fnv1a64_float<double> (buf, len / sizeof (double),
                                               CANON_NAN64_LE, base);
    retval(0) = out;
  }
  else if (args(0).is_single_type ())
  {
    octave_uint64 out = fnv1a64_float<float> (buf, len / sizeof (float),
                                              CANON_NAN32_LE, base);
    retval(0) = out;
  }
  else if (isLittleEndian ())
  {
    octave_uint64 out = fnv1a64 (buf, len, base);
    retval(0) = out;
  }
  else
  {
    // Integer and logical types only; the floating point branches above
    // already emitted little-endian bytes on either architecture.
    uint32_t nbytes = 1;
    if (args(0).is_int16_type () || args(0).is_uint16_type ())
    {
      nbytes = 2;
    }
    else if (args(0).is_int32_type () || args(0).is_uint32_type ())
    {
      nbytes = 4;
    }
    else if (args(0).is_int64_type () || args(0).is_uint64_type ())
    {
      nbytes = 8;
    }
    octave_uint64 out = fnv1a64 (buf, len, nbytes, base);
    retval(0) = out;
  }
  return retval;
}
