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
## @deftypefn  {datatypes} {[@var{du}, @var{ok}] =} __iso2dur__ (@var{C})
##
## Parse a cell block of ISO 8601 duration strings into a @code{duration}.
##
## The strings are of the form @qcode{PTnHnMnS}, optionally signed, with hours
## unwrapped so that a duration of any magnitude is read back exactly.  An empty
## cell becomes a missing duration.  @var{ok} is false when a non-empty entry is
## not such a string, in which case @var{du} is empty and the caller falls back
## to its own parsing.
##
## @end deftypefn

function [du, ok] = __iso2dur__ (C)

  tot = nan (size (C));
  ok = true;
  for i = 1:numel (C)
    str = C{i};
    if (! ischar (str))
      str = '';
    endif
    if (! isempty (str))
      neg = (str(1) == '-');
      if (neg)
        str(1) = [];
      endif
      tk = regexp (str, '^PT([\d.]+)H([\d.]+)M([\d.]+)S$', 'tokens');
      if (isempty (tk))
        du = [];
        ok = false;
        return;
      endif
      H = str2double (tk{1}{1});
      M = str2double (tk{1}{2});
      S = str2double (tk{1}{3});
      val = H * 3600 + M * 60 + S;
      if (neg)
        val = -val;
      endif
      tot(i) = val;
    endif
  endfor
  du = seconds (tot);                   # NaN yields a missing duration

endfunction
