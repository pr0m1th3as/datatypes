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
## @deftypefn  {datatypes} {[@var{dt}, @var{ok}] =} __iso2dt__ (@var{C}, @var{tz})
##
## Parse a cell block of ISO 8601 date-time strings into a @code{datetime}.
##
## An empty cell becomes @code{NaT}.  A non-empty @var{tz} restores the
## @qcode{TimeZone}; the strings are the wall-clock time in that zone.  @var{ok}
## is false when a non-empty entry is not an ISO 8601 date-time, in which case
## @var{dt} is empty and the caller falls back to its own parsing.
##
## @end deftypefn

function [dt, ok] = __iso2dt__ (C, tz = '')

  sz = size (C);
  Y = nan (sz);  Mo = nan (sz);  D = nan (sz);
  h = nan (sz);  mi = nan (sz);  s = nan (sz);
  ok = true;
  for i = 1:numel (C)
    str = C{i};
    if (! ischar (str))
      str = '';
    endif
    if (! isempty (str))
      val = sscanf (str, "%d-%d-%dT%d:%d:%f");
      if (numel (val) != 6)
        dt = [];
        ok = false;
        return;
      endif
      Y(i) = val(1);  Mo(i) = val(2);  D(i) = val(3);
      h(i) = val(4);  mi(i) = val(5);  s(i) = val(6);
    endif
  endfor
  if (isempty (tz))
    dt = datetime (Y, Mo, D, h, mi, s);   # NaN components yield NaT
  else
    dt = datetime (Y, Mo, D, h, mi, s, 'TimeZone', tz);
  endif

endfunction
