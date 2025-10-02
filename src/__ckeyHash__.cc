/*
Copyright (C) 2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>

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
#include <iostream>
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


DEFUN_DLD (__ckeyHash__, args, nargout,
           "-*- texinfo -*-\n\
 @deftypefn {} {@var{uint64} =} __ckeyHash__ (@var{str})\n\
\n\
\n\
Fowler–Noll–Vo hash key for a string. \n\
\n\
This is a helper function for the @qcode{keyHash} method.  Do NOT \
call it directly. \n\
\n\
@end deftypefn")
{
  uint64_t base = 0xcbf29ce484222325;
  string str = args(0).string_value ();
  octave_uint64 out = fnv1a64 (str.c_str (), str.length (), base);
  octave_value_list retval (nargout);
  retval(0) = out;
  return retval;
}
