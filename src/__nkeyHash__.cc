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

using namespace std;

static inline constexpr uint64_t fnv1a64 (const char* buf, size_t len, uint64_t out)
{
	for (size_t i = 0; i < len; i++)
  {
		out = (out ^ buf[i]) * FNV1A64_PRIME;
  }
	return out;
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
  octave_uint64 out = fnv1a64 (buf, len, base);
  octave_value_list retval (nargout);
  retval(0) = out;
  return retval;
}
