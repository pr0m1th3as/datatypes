## Copyright (C) 2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

## The exact decimal text of an integer-type scalar V.  A 64-bit value
## beyond flintmax is divided by ten in two 32-bit halves held as doubles,
## where every step is exact.
function s = __intstr__ (v)
  if (abs (double (v)) < flintmax ())
    s = sprintf ("%d", double (v));
    return;
  endif
  neg = v < 0;
  if (neg)
    m = uint64 (-(v + ones (class (v)))) + uint64 (1);
  else
    m = uint64 (v);
  endif
  hi = double (bitshift (m, -32));
  lo = double (bitand (m, uint64 (4294967295)));
  s = '';
  while (hi > 0 || lo > 0)
    qh = floor (hi / 10);
    t = (hi - 10 * qh) * 4294967296 + lo;
    ql = floor (t / 10);
    s = [char(48 + t - 10 * ql), s];
    hi = qh;
    lo = ql;
  endwhile
  if (neg)
    s = ['-', s];
  endif
endfunction
