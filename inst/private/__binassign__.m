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
## @deftypefn {datatypes} {@var{bin} =} __binassign__ (@var{xv}, @var{ev}, @var{leftClosed})
##
## Assign values to bins.
##
## @code{@var{bin} = __binassign__ (@var{xv}, @var{ev}, @var{leftClosed})} returns
## the index of the bin each element of the column vector @var{xv} falls into,
## given the non-decreasing row vector of edges @var{ev}.  @qcode{NaN} is
## returned for values that fall in no bin and for @qcode{NaN} input.
##
## When @var{leftClosed} is true each bin covers @code{[e(j), e(j+1))} with the
## last bin closed at both ends; otherwise each covers @code{(e(j), e(j+1)]} with
## the first bin closed at both ends.  Repeated edges define empty bins, which
## are never selected.
##
## This is shared by @code{discretize} and @code{histcounts} so that the two can
## never disagree about which bin a value belongs to.
##
## @end deftypefn

function bin = __binassign__ (xv, ev, leftClosed)

  nbins = numel (ev) - 1;
  if (leftClosed)
    bin = lookup (ev, xv);
    bin(bin == 0) = NaN;
    bin(xv > ev(end)) = NaN;
    bin(xv == ev(end)) = nbins;
  else
    bin = nbins + 1 - lookup (-fliplr (ev), -xv);
    bin(bin < 1 | bin > nbins) = NaN;
    bin(xv < ev(1) | xv > ev(end)) = NaN;
    bin(xv == ev(1)) = 1;
  endif
  bin(isnan (xv)) = NaN;

endfunction
