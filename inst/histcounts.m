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
## @deftypefn  {datatypes} {@var{N} =} histcounts (@var{X})
## @deftypefnx {datatypes} {@var{N} =} histcounts (@var{X}, @var{nbins})
## @deftypefnx {datatypes} {@var{N} =} histcounts (@var{X}, @var{edges})
## @deftypefnx {datatypes} {@var{N} =} histcounts (@dots{}, @var{Name}, @var{Value})
## @deftypefnx {datatypes} {[@var{N}, @var{edges}] =} histcounts (@dots{})
## @deftypefnx {datatypes} {[@var{N}, @var{edges}, @var{bin}] =} histcounts (@dots{})
##
## Histogram bin counts.
##
## @code{@var{N} = histcounts (@var{X})} partitions the values of @var{X} into
## bins chosen automatically and returns the number of elements in each bin as a
## row vector.  @var{X} is treated as @code{@var{X}(:)} regardless of its shape,
## and @qcode{NaN} and infinite values are excluded.
##
## @code{@var{N} = histcounts (@var{X}, @var{nbins})} uses @var{nbins} bins, and
## @code{@var{N} = histcounts (@var{X}, @var{edges})} uses the bin edges given in
## the monotonically non-decreasing vector @var{edges}.  Bin @math{j} covers
## @code{[@var{edges}(j), @var{edges}(j+1))}, except the last bin, which is
## closed at both ends.
##
## @code{[@var{N}, @var{edges}, @var{bin}] = histcounts (@dots{})} also returns
## the bin edges and, in @var{bin}, the index of the bin each element of @var{X}
## belongs to.  @var{bin} has the same size as @var{X} and holds @math{0} for
## elements that fall in no bin.  Note this differs from @code{discretize}, which
## returns @qcode{NaN} for such elements.
##
## The following @var{Name}, @var{Value} pairs are supported.  When more than one
## of @qcode{'NumBins'}, @qcode{'BinWidth'}, @qcode{'BinEdges'} and
## @qcode{'BinMethod'} is given, the last one specified takes effect.
##
## @multitable @columnfractions 0.2 0.8
## @headitem Name @tab Value
##
## @item @qcode{'NumBins'} @tab A positive integer scalar giving the number of
## bins, equivalent to the @var{nbins} syntax.
##
## @item @qcode{'BinEdges'} @tab A vector of bin edges, equivalent to the
## @var{edges} syntax.
##
## @item @qcode{'BinWidth'} @tab A positive finite scalar giving a uniform bin
## width.  The edges are placed at multiples of the width covering the data.
##
## @item @qcode{'BinLimits'} @tab A two-element vector @code{[@var{lo},
## @var{hi}]} restricting the binning to that range.  Values outside it are not
## counted, and the outermost edges are clamped to @var{lo} and @var{hi}.
##
## @item @qcode{'BinMethod'} @tab One of @qcode{'auto'} (default),
## @qcode{'scott'}, @qcode{'fd'}, @qcode{'integers'}, @qcode{'sturges'} or
## @qcode{'sqrt'}.  @qcode{'auto'} uses the @qcode{'integers'} rule when the data
## are integer-valued and span at most 50, and @qcode{'scott'} otherwise.
##
## @item @qcode{'Normalization'} @tab One of @qcode{'count'} (default),
## @qcode{'countdensity'}, @qcode{'cumcount'}, @qcode{'probability'},
## @qcode{'percentage'}, @qcode{'pdf'} or @qcode{'cdf'}.  The divisor for
## @qcode{'probability'}, @qcode{'percentage'}, @qcode{'pdf'} and @qcode{'cdf'}
## is @code{numel (@var{X})}, not the number of elements actually counted, so
## values excluded by being @qcode{NaN} or out of range still contribute to it.
## @end multitable
##
## MATLAB accepts @qcode{'percentage'} but omits it from the list of valid values
## in its own error message; it is documented here.
##
## @seealso{discretize}
## @end deftypefn
function [N, EDGES, BIN] = histcounts (X, varargin)

  ## Input validation
  if (nargin < 1)
    error ("histcounts: not enough input arguments.");
  endif
  if (! (isnumeric (X) || islogical (X)) || ! isreal (X))
    error ("histcounts: X must be a real numeric or logical array.");
  endif

  ## A leading positional argument is a bin count or a set of edges
  args = varargin;
  spec = 'auto';
  specVal = [];
  if (! isempty (args) && ! isTextScalar (args{1}))
    if (isscalar (args{1}) && ! islogical (args{1}))
      spec = 'numbins';
      specVal = checkNumBins (args{1});
    else
      spec = 'binedges';
      specVal = checkBinEdges (args{1});
    endif
    args(1) = [];
  endif

  ## Name/Value options.  The bin-determining options override one another in
  ## the order given, so they are applied as they are parsed.
  limits = [];
  normalization = 'count';
  if (mod (numel (args), 2) != 0)
    error ("histcounts: Name/Value arguments must appear in pairs.");
  endif
  for k = 1:2:numel (args)
    if (! isTextScalar (args{k}))
      error ("histcounts: option names must be character vectors.");
    endif
    name = charOf (args{k});
    value = args{k+1};
    switch (lower (name))
      case 'numbins'
        spec = 'numbins';
        specVal = checkNumBins (value);
      case 'binedges'
        spec = 'binedges';
        specVal = checkBinEdges (value);
      case 'binwidth'
        if (! isnumeric (value) || ! isscalar (value) || ! isreal (value)
            || ! isfinite (value) || value <= 0)
          error (strcat ("histcounts: 'BinWidth' must be a real, finite,", ...
                         " positive, numeric scalar."));
        endif
        spec = 'binwidth';
        specVal = double (value);
      case 'binmethod'
        if (! isTextScalar (value)
            || ! any (strcmpi (charOf (value), {'auto', 'scott', 'fd', ...
                                                'integers', 'sturges', 'sqrt'})))
          error (strcat ("histcounts: invalid value for 'BinMethod'.", ...
                         " Possible values are: 'auto', 'scott', 'fd',", ...
                         " 'integers', 'sturges', and 'sqrt'."));
        endif
        spec = lower (charOf (value));
        specVal = [];
      case 'binlimits'
        if (! isnumeric (value) || numel (value) != 2 || ! isreal (value)
            || any (! isfinite (value)))
          error (strcat ("histcounts: 'BinLimits' must be a two-element", ...
                         " vector of real, finite values."));
        endif
        if (value(1) > value(2))
          error ("histcounts: 'BinLimits' must be in ascending order.");
        endif
        limits = double (value(:)).';
      case 'normalization'
        if (! isTextScalar (value)
            || ! any (strcmpi (charOf (value), {'count', 'countdensity', ...
                                                'cumcount', 'probability', ...
                                                'percentage', 'pdf', 'cdf'})))
          error (strcat ("histcounts: invalid value for 'Normalization'.", ...
                         " Possible values are: 'count', 'countdensity',", ...
                         " 'cumcount', 'probability', 'percentage', 'pdf',", ...
                         " and 'cdf'."));
        endif
        normalization = lower (charOf (value));
      otherwise
        error ("histcounts: unknown option '%s'.", name);
    endswitch
  endfor

  ## Restrict the data to the finite values that take part in the binning
  xv = double (X(:));
  xf = xv(isfinite (xv));
  if (! isempty (limits))
    xf = xf(xf >= limits(1) & xf <= limits(2));
    lo = limits(1);
    hi = limits(2);
  elseif (isempty (xf))
    lo = [];
    hi = [];
  else
    lo = min (xf);
    hi = max (xf);
  endif

  ## Determine the bin edges
  if (strcmp (spec, 'binedges'))
    EDGES = specVal;
  elseif (isempty (lo))
    EDGES = [0, 1];
  elseif (strcmp (spec, 'numbins'))
    EDGES = __binedges__ (lo, hi, specVal);
    EDGES = clampToLimits (EDGES, limits);
  elseif (strcmp (spec, 'binwidth'))
    EDGES = clampToLimits (placeEdges (specVal, lo, hi), limits);
  else
    EDGES = clampToLimits (methodEdges (spec, xf, lo, hi), limits);
  endif
  if (isa (X, 'single') && ! strcmp (spec, 'binedges'))
    EDGES = single (EDGES);
  endif

  ## Count
  ev = double (EDGES(:)).';
  bin = __binassign__ (xv, ev, true);
  nbins = numel (ev) - 1;
  counted = ! isnan (bin);
  N = accumarray (bin(counted), 1, [nbins, 1]).';

  ## Apply the requested normalization.  Note the divisor below is numel (X),
  ## not sum (N) -- elements that were never counted still contribute to it.
  switch (normalization)
    case 'countdensity'
      N = N ./ diff (double (ev));
    case 'cumcount'
      N = cumsum (N);
    case 'probability'
      N = N / numel (X);
    case 'percentage'
      N = 100 * N / numel (X);
    case 'pdf'
      N = N ./ (numel (X) * diff (double (ev)));
    case 'cdf'
      N = cumsum (N) / numel (X);
  endswitch

  if (nargout > 2)
    bin(! counted) = 0;
    BIN = reshape (bin, size (X));
  endif

endfunction

## Bin edges for one of the automatic bin-selection rules
function edges = methodEdges (method, xf, lo, hi)
  n = numel (xf);
  if (lo == hi)
    edges = [lo - 0.5, lo + 0.5];
    return;
  endif
  if (strcmp (method, 'auto'))
    if (all (xf == fix (xf)) && (hi - lo) <= 50)
      method = 'integers';
    else
      method = 'scott';
    endif
  endif
  switch (method)
    case 'integers'
      centres = round (lo):round (hi);
      edges = [centres - 0.5, centres(end) + 0.5];
      return;
    case 'scott'
      rawWidth = 3.5 * std (xf) * n ^ (-1/3);
    case 'fd'
      ## Guard against a vanishing interquartile range, which would otherwise
      ## give a zero width for heavily tied data.
      q = quantile (xf, [0.25, 0.75], 1, 5);
      rawWidth = 2 * max (q(2) - q(1), (hi - lo) / 10) * n ^ (-1/3);
    case 'sturges'
      rawWidth = (hi - lo) / ceil (1 + log2 (n));
    case 'sqrt'
      rawWidth = (hi - lo) / ceil (sqrt (n));
  endswitch
  edges = placeEdges (niceWidth (rawWidth), lo, hi);
endfunction

## Round a raw bin width up onto the 1-2-3-5-10 ladder
function w = niceWidth (rawWidth)
  if (! (rawWidth > 0))
    w = 1;
    return;
  endif
  powTen = 10 ^ floor (log10 (rawWidth));
  relSize = rawWidth / powTen;
  if (relSize < 1.5)
    w = powTen;
  elseif (relSize < 2.5)
    w = 2 * powTen;
  elseif (relSize < 4)
    w = 3 * powTen;
  elseif (relSize < 7.5)
    w = 5 * powTen;
  else
    w = 10 * powTen;
  endif
endfunction

## Place edges of a given width at multiples of that width, covering the data
function edges = placeEdges (w, lo, hi)
  if (lo == hi)
    edges = [lo - w/2, lo + w/2];
    return;
  endif
  left = w * floor (lo / w);
  nbins = max (1, ceil ((hi - left) / w));
  edges = left + (0:nbins) * w;
endfunction

## Clamp the outermost edges into an explicit BinLimits range
function edges = clampToLimits (edges, limits)
  if (isempty (limits))
    return;
  endif
  edges = [limits(1), edges(edges > limits(1) & edges < limits(2)), limits(2)];
endfunction

## True for a character vector or a scalar string
function tf = isTextScalar (x)
  tf = (ischar (x) && isrow (x)) || (isa (x, 'string') && isscalar (x));
endfunction

## Character vector of a char or scalar string
function s = charOf (x)
  if (isa (x, 'string'))
    s = char (x);
  else
    s = x;
  endif
endfunction

function n = checkNumBins (value)
  if (! isnumeric (value) || ! isscalar (value) || ! isreal (value)
      || ! isfinite (value) || value < 1 || fix (value) != value)
    error (strcat ("histcounts: 'NumBins' must be a real, finite,", ...
                   " positive, integer value."));
  endif
  n = double (value);
endfunction

function e = checkBinEdges (value)
  if (! (isnumeric (value) || islogical (value)) || ! isreal (value)
      || ! isvector (value) || numel (value) < 2 || any (isnan (value(:))))
    error (strcat ("histcounts: 'BinEdges' must be a non-empty, real,", ...
                   " numeric vector with no missing values."));
  endif
  if (any (diff (double (value(:))) < 0))
    error ("histcounts: 'BinEdges' must be in ascending order.");
  endif
  e = value(:).';
endfunction

%!demo
%! ## Automatic binning.  Integer-valued data spanning at most 50 gets one bin
%! ## per integer, with edges at the half-integers.
%!
%! [N, edges] = histcounts ([1, 2, 3, 4, 5])

%!demo
%! ## Explicit edges, and the bin each value landed in.
%!
%! [N, edges, bin] = histcounts ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5])

%!demo
%! ## Normalization by relative frequency.  The divisor is numel (X), so the
%! ## NaN below still counts against the total.
%!
%! histcounts ([1, NaN, 2, 3], [1, 2, 3, 4], 'Normalization', 'probability')

## Explicit edges
%!assert_equal (histcounts ([1, 2, 3, 4, 5], [1, 3, 5]), [2, 3])
%!assert_equal (histcounts ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5]), [2, 3])
%!assert_equal (class (histcounts ([1, 2, 3], [1, 2, 3])), 'double')
%!test
%! [~, e, b] = histcounts ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5]);
%! assert_equal (e, [1, 3, 5]);
%! assert_equal (b, [0, 1, 1, 2, 2, 2, 0]);
%!test
%! [~, ~, b] = histcounts ([-Inf, 0, Inf, NaN, 1], [0, 1, 2]);
%! assert_equal (b, [0, 1, 0, 0, 2]);
%!assert_equal (histcounts ([-Inf, 0, Inf, NaN, 1], [-Inf, 1, Inf]), [2, 2])

## Automatic binning -- values verified against MATLAB R2024a
%!test
%! [N, e] = histcounts ([1, 2, 3, 4, 5]);
%! assert_equal (N, [1, 1, 1, 1, 1]);
%! assert_equal (e, [0.5, 1.5, 2.5, 3.5, 4.5, 5.5]);
%!test
%! [~, e] = histcounts ([0, 1]);
%! assert_equal (e, [-0.5, 0.5, 1.5]);
%!test
%! [~, e] = histcounts ([0, 100]);
%! assert_equal (e, [0, 200]);
%!test
%! [~, e] = histcounts (1:100);
%! assert_equal (e, [0, 20, 40, 60, 80, 100]);
%!test
%! [~, e] = histcounts (1:1000);
%! assert_equal (e, [0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]);
%!test
%! [~, e] = histcounts ([1, 2, 3, 4, 5, 100]);
%! assert_equal (e, [0, 100]);
%!test
%! [~, e] = histcounts ([-7.5, 2.5]);
%! assert_equal (e, [-20, 0, 20]);
%!test
%! [~, e] = histcounts (0.5 * (1:40));
%! assert_equal (e, [0, 5, 10, 15, 20]);
%!test
%! N = histcounts (0.5 * (1:40));
%! assert_equal (N, [9, 10, 10, 11]);

## The 'auto' integer switch turns over at a range of 50
%!test
%! [~, e] = histcounts (1:51);
%! assert_equal (e(1), 0.5);
%! assert_equal (numel (e) - 1, 51);
%!test
%! [~, e] = histcounts (1:52);
%! assert_equal (numel (e) - 1, 6);
%!test
%! [~, e] = histcounts ([0, 50]);
%! assert_equal (e(1), -0.5);
%!test
%! [~, e] = histcounts ([0, 51]);
%! assert_equal (e, [0, 100]);

## Constant and degenerate input
%!test
%! [~, e] = histcounts ([3, 3, 3]);
%! assert_equal (e, [2.5, 3.5]);
%!test
%! [~, e] = histcounts (7);
%! assert_equal (e, [6.5, 7.5]);
%!test
%! [N, e] = histcounts ([]);
%! assert_equal (N, 0);
%! assert_equal (e, [0, 1]);
%! assert_equal (size (N), [1, 1]);
%!test
%! [~, e] = histcounts ([3, 3, 3], 'BinMethod', 'scott');
%! assert_equal (e, [2.5, 3.5]);

## Bin methods
%!test
%! [~, e] = histcounts (1:100, 'BinMethod', 'scott');
%! assert_equal (e, [0, 20, 40, 60, 80, 100]);
%!test
%! [~, e] = histcounts (1:100, 'BinMethod', 'fd');
%! assert_equal (e, [0, 20, 40, 60, 80, 100]);
%!test
%! [~, e] = histcounts (1:100, 'BinMethod', 'sturges');
%! assert_equal (e, [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]);
%!test
%! [~, e] = histcounts (1:100, 'BinMethod', 'sqrt');
%! assert_equal (e, [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]);
%!test
%! [~, e] = histcounts ((1:100)/7, 'BinMethod', 'scott');
%! assert_equal (e, [0, 3, 6, 9, 12, 15]);
%!test
%! [~, e] = histcounts ((1:100)/7, 'BinMethod', 'fd');
%! assert_equal (e, [0, 3, 6, 9, 12, 15]);
%!test
%! [~, e] = histcounts ((1:100)/7, 'BinMethod', 'sturges');
%! assert_equal (e, [0, 2, 4, 6, 8, 10, 12, 14, 16]);
%!test
%! [~, e] = histcounts ((1:100)/7, 'BinMethod', 'sqrt');
%! assert_equal (e, 0:15);
%!test
%! [~, e] = histcounts ([1.5, 2.5], 'BinMethod', 'integers');
%! assert_equal (e, [1.5, 2.5, 3.5]);
%!test
%! [~, e] = histcounts ([-3, 0, 3], 'BinMethod', 'integers');
%! assert_equal (e, [-3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5]);

## Bin counts go through the same edge algorithm as discretize
%!test
%! [~, e] = histcounts (1:100, 7);
%! assert_equal (e, [0, 15, 30, 45, 60, 75, 90, 105]);
%!test
%! [~, e1] = histcounts ([1, 2, 3, 4, 5], 3);
%! [~, e2] = discretize ([1, 2, 3, 4, 5], 3);
%! assert_equal (e1, e2);

## NumBins, BinWidth, BinEdges, BinLimits
%!test
%! [~, e] = histcounts (1:100, 'NumBins', 7);
%! assert_equal (e, [0, 15, 30, 45, 60, 75, 90, 105]);
%!test
%! [~, e] = histcounts (1:100, 'BinWidth', 7);
%! assert_equal (e, 0:7:105);
%!test
%! [~, e] = histcounts (1:100, 'BinEdges', [0, 50, 100]);
%! assert_equal (e, [0, 50, 100]);
%!test
%! [N, e] = histcounts (1:100, 'BinLimits', [20, 40]);
%! assert_equal (e, [20, 20.5:1:39.5, 40]);
%! assert_equal (N, ones (1, 21));
%!test
%! [~, ~, b] = histcounts (1:10, 'BinLimits', [3, 6]);
%! assert_equal (b, [0, 0, 1, 2, 3, 4, 0, 0, 0, 0]);
%!test
%! [~, e] = histcounts (1:100, 'BinWidth', 7, 'BinLimits', [0, 70]);
%! assert_equal (e, 0:7:70);

## The last-specified bin option wins
%!test
%! [~, e] = histcounts (1:100, 'NumBins', 3, 'BinWidth', 7);
%! assert_equal (e, 0:7:105);

## Normalization
%!shared x, ed
%! x = [1, 1, 2, 3, 3, 3];
%! ed = [1, 2, 3, 4];
%!assert_equal (histcounts (x, ed), [2, 1, 3])
%!assert_equal (histcounts (x, ed, 'Normalization', 'count'), [2, 1, 3])
%!assert_equal (histcounts (x, ed, 'Normalization', 'probability'), ...
%!              [2, 1, 3] / 6)
%!assert_equal (histcounts (x, ed, 'Normalization', 'percentage'), ...
%!              100 * [2, 1, 3] / 6)
%!assert_equal (histcounts (x, ed, 'Normalization', 'countdensity'), [2, 1, 3])
%!assert_equal (histcounts (x, ed, 'Normalization', 'pdf'), [2, 1, 3] / 6)
%!assert_equal (histcounts (x, ed, 'Normalization', 'cumcount'), [2, 3, 6])
%!assert_equal (histcounts (x, ed, 'Normalization', 'cdf'), [2, 3, 6] / 6)
%!assert_equal (histcounts (x, [1, 3, 4], 'Normalization', 'pdf'), [0.25, 0.5])
%!assert_equal (histcounts (x, [1, 3, 4], 'Normalization', 'countdensity'), ...
%!              [1.5, 3])

## The divisor is numel (X), not the number of elements counted
%!assert_equal (histcounts ([1, NaN, 2, 3], [1, 2, 3, 4], ...
%!                          'Normalization', 'probability'), [0.25, 0.25, 0.25])
%!assert_equal (histcounts ([1, 5, 2, 3], [1, 2, 3, 4], ...
%!                          'Normalization', 'probability'), [0.25, 0.25, 0.25])
%!assert_equal (class (histcounts ([1, 2, 3], [1, 2, 3], ...
%!                                 'Normalization', 'pdf')), 'double')

## Types and shapes
%!assert_equal (class (histcounts (single ([1, 2, 3]))), 'double')
%!test
%! [~, e] = histcounts (single ([1, 2, 3]));
%! assert_equal (class (e), 'single');
%!test
%! [~, e] = histcounts (int8 ([1, 2, 3]));
%! assert_equal (class (e), 'double');
%! assert_equal (e, [0.5, 1.5, 2.5, 3.5]);
%!test
%! [~, e] = histcounts (uint8 ([1, 2, 3]));
%! assert_equal (e, [0.5, 1.5, 2.5, 3.5]);
%!test
%! [~, e] = histcounts (logical ([0, 1, 1]));
%! assert_equal (e, [-0.5, 0.5, 1.5]);
%!assert_equal (histcounts (logical ([0, 1, 1])), [1, 2])
%!assert_equal (histcounts ([1, 2; 3, 4], [1, 3, 5]), [2, 2])
%!assert_equal (size (histcounts (reshape (1:24, 2, 3, 4), [1, 10, 20, 30])), ...
%!              [1, 3])
%!test
%! [~, ~, b] = histcounts ([1, 2; 3, 4], [1, 3, 5]);
%! assert_equal (size (b), [2, 2]);
%!assert_equal (histcounts ([1, 2, 3]', [1, 2, 3]), [1, 2])
%!assert_equal (size (histcounts ([1, 2, 3]', [1, 2, 3])), [1, 2])
%!assert_equal (histcounts ([1, 2, 3], 1), 3)

## Error branches
%!error<histcounts: X must be a real numeric or logical array.> histcounts ('abc');
%!error<histcounts: X must be a real numeric or logical array.> histcounts ({1, 2});
%!error<histcounts: X must be a real numeric or logical array.> ...
%! histcounts (complex ([1, 2], [1, 1]));
%!error<histcounts: 'NumBins' must be a real, finite, positive, integer value.> ...
%! histcounts ([1, 2, 3], 0);
%!error<histcounts: 'NumBins' must be a real, finite, positive, integer value.> ...
%! histcounts ([1, 2, 3], -1);
%!error<histcounts: 'NumBins' must be a real, finite, positive, integer value.> ...
%! histcounts ([1, 2, 3], 2.5);
%!error<histcounts: 'NumBins' must be a real, finite, positive, integer value.> ...
%! histcounts ([1, 2, 3], 'NumBins', 0);
%!error<histcounts: 'BinEdges' must be in ascending order.> ...
%! histcounts ([1, 2, 3], [3, 1]);
%!error<histcounts: 'BinEdges' must be a non-empty, real, numeric vector with no missing values.> ...
%! histcounts ([1, 2, 3], [1, NaN, 3]);
%!error<histcounts: invalid value for 'BinMethod'. Possible values are: 'auto', 'scott', 'fd', 'integers', 'sturges', and 'sqrt'.> ...
%! histcounts ([1, 2, 3], 'BinMethod', 'bogus');
%!error<histcounts: invalid value for 'Normalization'. Possible values are: 'count', 'countdensity', 'cumcount', 'probability', 'percentage', 'pdf', and 'cdf'.> ...
%! histcounts ([1, 2, 3], 'Normalization', 'bogus');
%!error<histcounts: 'BinWidth' must be a real, finite, positive, numeric scalar.> ...
%! histcounts ([1, 2, 3], 'BinWidth', 0);
%!error<histcounts: 'BinWidth' must be a real, finite, positive, numeric scalar.> ...
%! histcounts ([1, 2, 3], 'BinWidth', -1);
%!error<histcounts: 'BinLimits' must be in ascending order.> ...
%! histcounts ([1, 2, 3], 'BinLimits', [3, 1]);
%!error<histcounts: 'BinLimits' must be a two-element vector of real, finite values.> ...
%! histcounts ([1, 2, 3], 'BinLimits', [1, 2, 3]);
%!error <histcounts: not enough input arguments.> ...
%! histcounts ()
%!error <histcounts: Name/Value arguments must appear in pairs.> ...
%! histcounts ([1, 2, 3], 'Normalization')
%!error <histcounts: option names must be character vectors.> ...
%! histcounts ([1, 2, 3], 5, 'Normalization', 'count', 7, 1)
%!error <histcounts: unknown option 'Nope'.> ...
%! histcounts ([1, 2, 3], 'Nope', 1)
