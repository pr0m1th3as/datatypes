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
 @deftypefnx {} {@var{uint64} =} __nkeyHash__ (@var{x}, @var{FNV1A64_BASE)\n\
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
  // Cast numeric input to const char
  uint32_t len = args(0).byte_size ();
  const void *in = args(0).mex_get_data ();
  const char *buf = static_cast<const char *>(in);
  // Generate the hash key
  octave_value_list retval (nargout);
  if (isLittleEndian ())
  {
    octave_uint64 out = fnv1a64 (buf, len, base);
    retval(0) = out;
  }
  else
  {
    uint32_t nbytes = 1;
    if (args(0).is_int16_type () || args(0).is_uint16_type ())
    {
      nbytes = 2;
    }
    else if (args(0).is_int32_type () || args(0).is_uint32_type ()
                                      || args(0).is_single_type ())
    {
      nbytes = 4;
    }
    else if (args(0).is_int64_type () || args(0).is_uint64_type ()
                                      || args(0).is_double_type ())
    {
      nbytes = 8;
    }
    octave_uint64 out = fnv1a64 (buf, len, nbytes, base);
    retval(0) = out;
  }
  return retval;
}
