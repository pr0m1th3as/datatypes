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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {datatypes} {@var{C} =} __dt2rfc9557__ (@var{dt})
##
## Format a zone-aware @code{datetime} column as RFC 9557 text.
##
## Each entry is written as the local time, the offset from UTC, and the zone
## name in square brackets, as in
## @qcode{2024-03-09T22:00:00-05:00[America/New_York]}.  That is the
## Internet Extended Date/Time Format of RFC 9557, which extends ISO 8601 with
## the zone name, and is what a Java @code{ZonedDateTime} prints.
##
## The offset alone does not name a zone, and the zone name alone does not say
## which side of a repeated hour an instant falls on; the two together say both,
## so a fold round-trips.  @code{NaT} yields an empty string, which a writer
## records as a missing field.
##
## @seealso{__rfc95572dt__}
## @end deftypefn

function C = __dt2rfc9557__ (dt)

  dt = dt(:);
  n = numel (dt);
  C = cell (n, 1);
  zone = dt.TimeZone;
  osec = seconds (tzoffset (dt));
  [Y, M, D] = ymd (dt);
  [h, m, s] = hms (dt);
  for i = 1:n
    if (isnan (Y(i)))
      C{i} = '';
      continue;
    endif
    o = osec(i);
    sgn = '+';
    if (o < 0)
      sgn = '-';
    endif
    a = abs (o);
    oh = floor (a / 3600);
    om = floor (mod (a, 3600) / 60);
    C{i} = sprintf ('%04d-%02d-%02dT%02d:%02d:%s%s%02d:%02d[%s]', ...
                    Y(i), M(i), D(i), h(i), m(i), sec_text (s(i)), ...
                    sgn, oh, om, zone);
  endfor

endfunction

## Seconds with a fractional part only where there is one, so that a whole
## second is written as two digits rather than with trailing zeros.
function out = sec_text (s)
  if (s == fix (s))
    out = sprintf ('%02d', s);
  else
    out = sprintf ('%09.6f', s);
    out = regexprep (out, '0+$', '');
  endif
endfunction
