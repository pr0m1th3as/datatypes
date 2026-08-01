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

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{edges} =} __binedgesgrid__ (@var{xmin}, @var{xmax}, @var{nbins})
## @deftypefnx {datatypes} {@var{edges} =} __binedgesgrid__ (@dots{}, @var{tick})
##
## Bin edges for a requested bin count, snapped to whole time units.
##
## @code{@var{edges} = __binedgesgrid__ (@var{xmin}, @var{xmax}, @var{nbins})}
## returns @code{@var{nbins} + 1} uniformly spaced edges covering
## @code{[@var{xmin}, @var{xmax}]}.  All three arguments, and the result, count
## @strong{seconds}.
##
## Bins are placed on whole time units wherever that can be done without wasting
## a bin.  Taking @var{g} as a unit from @code{1 second}, @code{1 minute},
## @code{1 hour}, @code{1 day} and @code{1 year} (the fixed 365.2425-day year that
## @code{years} counts):
##
## @example
## @var{w} = @var{g} * ceil (@var{range} / (@var{nbins} * @var{g}))
## @var{L} = @var{g} * ceil ((@var{xmin} - (@var{nbins} * @var{w} - @var{range}) / 2) / @var{g} - 0.5)
## @end example
##
## the width is the raw width rounded up to a whole unit, and the bins are then
## centred on the data with the left edge snapped to that same unit.  The largest
## unit that still needs all @var{nbins} bins to cover the data is used, that is
## the largest @var{g} with @code{ceil (@var{range} / @var{w}) == @var{nbins}}.
## When no unit qualifies -- which is always the case for sub-second data -- the
## plain numeric rule in @code{__binedges__} takes over.
##
## @var{tick} is the half-width used when the data are constant, and defaults to
## half a second.  @code{duration} passes half a millisecond.
##
## @seealso{__binedges__, __binassign__}
## @end deftypefn

function edges = __binedgesgrid__ (xmin, xmax, nbins, tick = 0.5)

  ## Constant data: an interval of one tick centred on the value
  if (xmin == xmax)
    edges = (xmin - tick) + (0:nbins) * (2 * tick / nbins);
    return;
  endif

  ## Whole time units, largest first.  There is deliberately no week and no
  ## month: a week grid reproduces some spans and gets others wrong, and every
  ## span that looked like months is reproduced by days or years.
  units = [31556952, 86400, 3600, 60, 1];

  span = xmax - xmin;

  ## Take the largest unit whose width still needs every bin to cover the data.
  ## Rounding the width up to a unit that then leaves bins unused would both
  ## waste bins and make the width jump around as the data change.
  unit = [];
  for u = units
    w = u * ceil (span / (nbins * u));
    if (ceil (span / w) == nbins)
      unit = u;
      break;
    endif
  endfor

  ## Finer than a second: fall through to the plain numeric rule
  if (isempty (unit))
    edges = __binedges__ (xmin, xmax, nbins);
    return;
  endif

  width = unit * ceil (span / (nbins * unit));
  ## Half-way cases round down, which is what MATLAB does here and is not
  ## what round() gives -- it rounds half away from zero.
  left = unit * ceil ((xmin - (nbins * width - span) / 2) / unit - 0.5);
  edges = left + (0:nbins) * width;

  ## Guard against a rounding shortfall at either end
  if (edges(1) > xmin)
    edges = edges - (edges(1) - xmin);
  endif
  if (edges(end) < xmax)
    edges(end) = xmax;
  endif

endfunction
