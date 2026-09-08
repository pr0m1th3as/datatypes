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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {datatypes} {[@var{dt}, @var{ok}] =} __rfc95572dt__ (@var{C})
##
## Parse a cell block of RFC 9557 date-time strings into a zoned
## @code{datetime}.
##
## @var{ok} is false unless every non-empty entry carries the bracketed zone
## name and every one of them names the @strong{same} zone, a column being one
## variable; @var{dt} is empty then and the caller falls back to its own
## parsing.  An empty entry becomes @code{NaT}.
##
## The instant is taken from the local time @strong{less the offset the string
## carries}, and the zone is applied to that, so a time written in a repeated
## hour comes back on the side of the fold it was written on.  Reading the
## local components alone would resolve every fold the same way and silently
## move half of them.
##
## @seealso{__dt2rfc9557__}
## @end deftypefn

function [dt, ok] = __rfc95572dt__ (C)

  dt = [];
  ok = false;
  pat = strcat ('^\s*(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):', ...
                '(\d{2}(?:\.\d+)?)([+-])(\d{2}):(\d{2})', ...
                '\[([^\]]+)\]\s*$');
  sz = size (C);
  Y = nan (sz);  Mo = nan (sz);  D = nan (sz);
  h = nan (sz);  mi = nan (sz);  s = nan (sz);
  off = nan (sz);
  zone = '';
  any_value = false;
  for i = 1:numel (C)
    str = C{i};
    if (! ischar (str))
      str = '';
    endif
    if (isempty (str))
      continue;
    endif
    tok = regexp (str, pat, 'tokens', 'once');
    if (isempty (tok))
      return;
    endif
    if (isempty (zone))
      zone = tok{10};
    elseif (! strcmp (zone, tok{10}))
      return;
    endif
    Y(i) = str2double (tok{1});   Mo(i) = str2double (tok{2});
    D(i) = str2double (tok{3});   h(i)  = str2double (tok{4});
    mi(i) = str2double (tok{5});  s(i)  = str2double (tok{6});
    osec = str2double (tok{8}) * 3600 + str2double (tok{9}) * 60;
    if (strcmp (tok{7}, '-'))
      osec = -osec;
    endif
    off(i) = osec;
    any_value = true;
  endfor
  if (! any_value)
    return;
  endif

  ## The instant is the local time less its offset, read in UTC; assigning the
  ## zone then converts it, which is what carries a fold across.
  utc = datetime (Y, Mo, D, h, mi, s, 'TimeZone', 'UTC') - seconds (off);
  utc.TimeZone = zone;
  dt = utc;
  ok = true;

endfunction
