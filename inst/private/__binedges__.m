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
## @deftypefn {datatypes} {@var{edges} =} __binedges__ (@var{xmin}, @var{xmax}, @var{nbins})
##
## Bin edges for a requested bin count.
##
## @code{@var{edges} = __binedges__ (@var{xmin}, @var{xmax}, @var{nbins})} returns
## a row vector of @code{@var{nbins} + 1} uniformly spaced edges covering the
## closed interval @code{[@var{xmin}, @var{xmax}]}, placed at "nice" decimal
## positions.  @var{xmin} and @var{xmax} must be finite real scalars with
## @code{@var{xmin} <= @var{xmax}}, and @var{nbins} a positive integer.
##
## This is the shared edge generator behind the bin-count syntax of both
## @code{discretize} and @code{histcounts}; the two must never disagree.
##
## @end deftypefn

function edges = __binedges__ (xmin, xmax, nbins)

  ## Constant data: a unit-wide interval centred on the value
  if (xmin == xmax)
    edges = (xmin - 0.5) + (0:nbins) * (1 / nbins);
    return;
  endif

  ## The left edge is anchored to a multiple of the raw bin width truncated to
  ## one significant digit.  Note this is NOT the {1,2,3,5,10} ladder used by
  ## the automatic bin-width rules -- every leading digit occurs here.
  rawWidth = (xmax - xmin) / nbins;
  powTen = 10 ^ floor (log10 (rawWidth));
  anchor = powTen * floor (rawWidth / powTen);
  left = anchor * floor (xmin / anchor);

  ## The width is then recomputed from the SHIFTED range, and rounded up onto a
  ## decimal grid whose fineness tracks NBINS.  Because the grid unit is about
  ## rawWidth / NBINS, the total over-coverage never exceeds one bin width.
  ##
  ## Known residual: 1 case in 426 measured against MATLAB R2024a lands on a
  ## ceil() boundary where MATLAB's internal scaling rounds the other way
  ## ([0, 0.0091] with 14 bins gives 0.00065 here, 0.00066 there).  Scaling by
  ## the reciprocal power of ten instead reproduces that case but breaks two
  ## others, so the division is kept.  Both binnings are valid -- ours is the
  ## tighter of the two and still covers the data.
  if (nbins == 1)
    width = xmax - left;
  else
    shifted = (xmax - left) / nbins;
    unit = 10 ^ floor (log10 (shifted / (nbins - 1)));
    width = ceil (shifted / unit) * unit;
  endif

  edges = left + (0:nbins) * width;

  ## Guard against a rounding shortfall at the top edge
  if (edges(end) < xmax)
    edges(end) = xmax;
  endif

endfunction
