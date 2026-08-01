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
## @deftypefn  {datatypes} {@var{bin} =} discretize (@var{X}, @var{edges})
## @deftypefnx {datatypes} {@var{bin} =} discretize (@var{X}, @var{N})
## @deftypefnx {datatypes} {@var{Y} =} discretize (@dots{}, @var{values})
## @deftypefnx {datatypes} {@var{C} =} discretize (@dots{}, 'categorical')
## @deftypefnx {datatypes} {@var{C} =} discretize (@dots{}, 'categorical', @var{names})
## @deftypefnx {datatypes} {@var{Y} =} discretize (@dots{}, 'IncludedEdge', @var{side})
## @deftypefnx {datatypes} {[@var{bin}, @var{edges}] =} discretize (@dots{})
##
## Group data into bins or categories.
##
## @code{@var{bin} = discretize (@var{X}, @var{edges})} returns an array of the
## same size as @var{X} whose elements give the index of the bin that each value
## of @var{X} falls into.  @var{edges} must be a real numeric or logical vector
## of monotonically non-decreasing values, and defines
## @code{numel (@var{edges}) - 1} bins.  Bin @math{j} covers the half-open
## interval @code{[@var{edges}(j), @var{edges}(j+1))}, except the last bin, which
## is closed at both ends.  Values outside @code{[@var{edges}(1),
## @var{edges}(end)]}, and any @qcode{NaN}, return @qcode{NaN}.  @var{bin} is
## always of type @qcode{double}.
##
## Repeated edges are permitted and meaningful: they define empty bins, which are
## simply never selected.  @code{discretize ([1, 2, 3], [1, 2, 2, 3])} returns
## @code{[1, 3, 3]}.
##
## @code{@var{bin} = discretize (@var{X}, @var{N})} uses @var{N} bins of uniform
## width spanning the range of @var{X}, where @var{N} is a positive integer
## scalar.  The edges are placed at "nice" decimal positions rather than exactly
## at @code{min (@var{X})} and @code{max (@var{X})}, so the bins generally extend
## slightly beyond the data.  @qcode{NaN} and infinite values are ignored when
## determining the range.  This syntax is not available when @var{X} is of an
## integer type; supply explicit edges instead.
##
## @code{@var{Y} = discretize (@dots{}, @var{values})} returns the corresponding
## element of @var{values} in place of the bin index, so @var{values} must be a
## vector whose length equals the number of bins.  @var{Y} takes the type of
## @var{values}.  Elements of @var{X} that fall in no bin return @qcode{NaN} when
## @var{values} is a floating-point array, zero when it is of an integer type,
## and raise an error when it is a cell array.
##
## @code{@var{C} = discretize (@dots{}, 'categorical')} returns an ordinal
## @qcode{categorical} array whose categories are named after the bin intervals,
## for example @qcode{'[1, 3)'}.  @code{@var{C} = discretize (@dots{},
## 'categorical', @var{names})} names the categories explicitly; @var{names} must
## be a cell array of character vectors or a @qcode{string} vector whose length
## equals the number of bins.
##
## @code{@var{Y} = discretize (@dots{}, 'IncludedEdge', @var{side})} selects which
## end of each bin is closed.  @var{side} may be @qcode{'left'} (the default,
## giving @code{[@var{edges}(j), @var{edges}(j+1))} with the last bin closed at
## both ends) or @qcode{'right'} (giving @code{(@var{edges}(j),
## @var{edges}(j+1)]} with the first bin closed at both ends).
##
## @code{[@var{bin}, @var{edges}] = discretize (@dots{})} also returns the bin
## edges used.  When the edges were supplied they are returned unchanged; when a
## bin count was requested they are returned as a row vector.
##
## @seealso{histcounts, categorical}
## @end deftypefn
function [BIN, EDGES] = discretize (X, arg2, varargin)

  ## Input validation
  if (nargin < 2)
    error ("discretize: not enough input arguments.");
  endif
  if (! (isnumeric (X) || islogical (X)) || ! isreal (X))
    error ("discretize: X must be a real numeric or logical array.");
  endif

  ## Split off a trailing 'IncludedEdge', SIDE pair from the optional arguments
  args = varargin;
  side = 'left';
  for k = numel (args) - 1:-1:1
    if (isTextScalar (args{k}) && strcmpi (charOf (args{k}), 'IncludedEdge'))
      side = args{k+1};
      args(k:k+1) = [];
      break;
    endif
  endfor
  if (! isTextScalar (side) || ! any (strcmpi (charOf (side), {'left', 'right'})))
    error ("discretize: 'IncludedEdge' value must be 'left' or 'right'.");
  endif
  leftClosed = strcmpi (charOf (side), 'left');

  ## Determine the bin edges, either supplied or derived from a bin count
  if (isscalar (arg2) && ! islogical (arg2))
    if (! isnumeric (arg2) || ! isreal (arg2) || ! isfinite (arg2)
        || arg2 < 1 || fix (arg2) != arg2)
      error ("discretize: N must be a real positive integer.");
    endif
    if (isinteger (X))
      error (strcat ("discretize: when X is an integer data type,", ...
                     " specify bin edges instead of number of bins."));
    endif
    nbins = double (arg2);
    xf = double (X(isfinite (X)));
    if (isempty (xf))
      EDGES = 0:nbins;
    else
      EDGES = __binedges__ (min (xf), max (xf), nbins);
    endif
    if (isa (X, 'single'))
      EDGES = single (EDGES);
    endif
  else
    EDGES = arg2;
    if (! (isnumeric (EDGES) || islogical (EDGES)) || ! isreal (EDGES)
        || ! isvector (EDGES) || numel (EDGES) < 2 || any (isnan (EDGES(:)))
        || any (diff (double (EDGES(:))) < 0))
      error (strcat ("discretize: EDGES must be a vector that is real,", ...
                     " numeric or logical, and monotonically increasing."));
    endif
    nbins = numel (EDGES) - 1;
  endif

  ## Assign each element to a bin
  ev = double (EDGES(:)).';
  xv = double (X(:));
  BIN = reshape (__binassign__ (xv, ev, leftClosed), size (X));

  ## Map bin indices onto values or categories, if requested
  if (isempty (args))
    return;
  endif
  if (isTextScalar (args{1}) && strcmpi (charOf (args{1}), 'categorical'))
    if (numel (args) > 1)
      names = args{2};
      if (isa (names, 'string'))
        names = cellstr (names);
      endif
      if (! iscellstr (names) || numel (names) != nbins)
        error (strcat ("discretize: category names must be a cellstr or", ...
                       " string vector with length equal to the number", ...
                       " of bins."));
      endif
      names = names(:).';
    else
      names = defaultLabels (double (EDGES(:)).', leftClosed);
    endif
    BIN = categorical (BIN, 1:nbins, names, 'Ordinal', true);
  else
    values = args{1};
    if (! isvector (values) || numel (values) != nbins)
      error (strcat ("discretize: VALUES must be a vector with length", ...
                     " equal to the number of bins."));
    endif
    isbinned = ! isnan (BIN);
    if (iscell (values))
      if (! all (isbinned(:)))
        error (strcat ("discretize: all elements of X must belong to a bin", ...
                       " when VALUES is a cell array."));
      endif
      BIN = reshape (values(BIN(:)), size (X));
    else
      out = zeros (size (BIN), class (values));
      if (isfloat (values))
        out(:) = NaN;
      endif
      out(isbinned) = values(BIN(isbinned));
      BIN = out;
    endif
  endif

endfunction

## Default category labels naming each bin interval
function names = defaultLabels (ev, leftClosed)
  nbins = numel (ev) - 1;
  names = cell (1, nbins);
  for ii = 1:nbins
    lo = sprintf ("%g", ev(ii));
    hi = sprintf ("%g", ev(ii+1));
    if (leftClosed)
      if (ii == nbins)
        names{ii} = sprintf ("[%s, %s]", lo, hi);
      else
        names{ii} = sprintf ("[%s, %s)", lo, hi);
      endif
    else
      if (ii == 1)
        names{ii} = sprintf ("[%s, %s]", lo, hi);
      else
        names{ii} = sprintf ("(%s, %s]", lo, hi);
      endif
    endif
  endfor
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

%!demo
%! ## Bin data with explicit edges.  Bins are closed on the left, except the
%! ## last, which is closed at both ends.
%!
%! discretize ([1, 2, 3, 4, 5], [1, 3, 5])

%!demo
%! ## Ask for a bin count instead.  The edges land on round numbers, so they
%! ## generally extend a little beyond the data.
%!
%! [bin, edges] = discretize ([1, 2, 3, 4, 5], 3)

%!demo
%! ## Return a label per bin rather than an index.
%!
%! discretize ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5], {'low', 'high'})

## Explicit edges
%!assert_equal (discretize ([1, 2, 3, 4, 5], [1, 3, 5]), [1, 1, 2, 2, 2])
%!assert_equal (discretize ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5]), ...
%!              [NaN, 1, 1, 2, 2, 2, NaN])
%!assert_equal (class (discretize ([1, 2, 3], [1, 2, 3])), 'double')
%!assert_equal (class (discretize (single ([1, 2, 3]), [1, 2, 3])), 'double')
%!assert_equal (discretize (int8 ([1, 2, 3]), [1, 2, 3]), [1, 2, 2])
%!assert_equal (discretize (logical ([0, 1, 1]), [0, 1, 2]), [1, 2, 2])

## Repeated edges define empty bins
%!assert_equal (discretize ([1, 2, 3], [1, 1, 2, 3]), [2, 3, 3])
%!assert_equal (discretize ([1, 2, 3], [1, 2, 2, 3]), [1, 3, 3])
%!assert_equal (discretize (2, [1, 2, 2, 3]), 3)

## Missing and infinite values
%!assert_equal (discretize ([-Inf, 0, Inf, NaN], [0, 1, 2]), [NaN, 1, NaN, NaN])
%!assert_equal (discretize ([-Inf, 0, Inf, NaN], [-Inf, 1, Inf]), ...
%!              [1, 1, 2, NaN])

## Included edge
%!assert_equal (discretize ([1, 2, 3, 4, 5], [1, 3, 5], 'IncludedEdge', 'left'), ...
%!              [1, 1, 2, 2, 2])
%!assert_equal (discretize ([1, 2, 3, 4, 5], [1, 3, 5], 'IncludedEdge', 'right'), ...
%!              [1, 1, 1, 2, 2])
%!assert_equal (discretize ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5], ...
%!                          'IncludedEdge', 'right'), [NaN, 1, 1, 1, 2, 2, NaN])

## Shape is preserved, and edges keep the orientation they were given
%!assert_equal (discretize ([1, 2; 3, 4], [1, 3, 5]), [1, 1; 2, 2])
%!assert_equal (size (discretize (reshape (1:24, 2, 3, 4), [1, 10, 20, 30])), ...
%!              [2, 3, 4])
%!assert_equal (discretize ([1; 2; 3], [1, 2, 3]), [1; 2; 2])
%!test
%! [~, e] = discretize ([1, 2, 3], [1, 2, 3]');
%! assert_equal (size (e), [3, 1]);
%!test
%! [~, e] = discretize ([1, 5]', 2);
%! assert_equal (size (e), [1, 3]);

## Bin counts -- values verified against MATLAB R2024a
%!test
%! [~, e] = discretize ([1, 2, 3, 4, 5], 3);
%! assert_equal (e, [1, 2.4000000000000004, 3.8000000000000003, ...
%!                   5.2000000000000002]);
%!test
%! [~, e] = discretize ([0, 10], 3);
%! assert_equal (e, [0, 4, 8, 12]);
%!test
%! [~, e] = discretize ([0, 100], 1);
%! assert_equal (e, [0, 100]);
%!test
%! [~, e] = discretize ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 4);
%! assert_equal (e, [0, 2.5, 5, 7.5, 10]);
%!test
%! [~, e] = discretize ([-5, 5], 4);
%! assert_equal (e, [-6, -3.1999999999999997, -0.39999999999999947, ...
%!                   2.4000000000000004, 5.2000000000000011]);
%!test
%! [~, e] = discretize ([1.234, 9.876], 5);
%! assert_equal (e, [1, 2.7999999999999998, 4.5999999999999996, ...
%!                   6.4000000000000004, 8.1999999999999993, 10]);
%!test
%! [~, e] = discretize ([-7.5, 2.5], 6);
%! assert_equal (e, [-8, -6.2000000000000002, -4.4000000000000004, ...
%!                   -2.5999999999999996, -0.79999999999999982, 1, ...
%!                   2.8000000000000007]);
%!test
%! [~, e] = discretize ([1e6, 1e6+1], 4);
%! assert_equal (e, [1000000, 1000000.25, 1000000.5, 1000000.75, 1000001]);
%!test
%! [~, e] = discretize ([0, 2/3], 3);
%! assert_equal (e, [0, 0.30000000000000004, 0.60000000000000009, ...
%!                   0.90000000000000013]);

## NaN and Inf are ignored when the range is determined
%!test
%! [~, e] = discretize ([NaN, 1, 5, NaN], 2);
%! assert_equal (e, [0, 3, 6]);
%!test
%! [~, e] = discretize ([-Inf, 1, 5], 2);
%! assert_equal (e, [0, 3, 6]);

## Constant data spans a unit interval centred on the value
%!test
%! [~, e] = discretize ([3, 3, 3], 3);
%! assert_equal (e, [2.5, 2.8333333333333335, 3.1666666666666665, 3.5]);
%!test
%! [~, e] = discretize (7, 3);
%! assert_equal (e, [6.5, 6.833333333333333, 7.166666666666667, 7.5]);

## Empty input
%!assert_equal (size (discretize ([], [1, 2, 3])), [0, 0])
%!assert_equal (class (discretize ([], [1, 2, 3])), 'double')
%!test
%! [~, e] = discretize ([], 3);
%! assert_equal (e, [0, 1, 2, 3]);

## Edge type follows the input for floating-point data
%!test
%! [~, e] = discretize (single ([1, 5]), 2);
%! assert_equal (class (e), 'single');

## Values
%!assert_equal (discretize ([1, 2, 3, 4, 5], [1, 3, 5], [10, 20]), ...
%!              [10, 10, 20, 20, 20])
%!assert_equal (class (discretize ([1, 2, 3, 4, 5], [1, 3, 5], [10, 20])), ...
%!              'double')
%!assert_equal (discretize ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5], int8 ([10, 20])), ...
%!              int8 ([0, 10, 10, 20, 20, 20, 0]))
%!assert_equal (class (discretize ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5], ...
%!                                 int8 ([10, 20]))), 'int8')
%!assert_equal (discretize ([1, 2, 3, 4, 5], [1, 3, 5], {'lo', 'hi'}), ...
%!              {'lo', 'lo', 'hi', 'hi', 'hi'})

## Categorical
%!test
%! C = discretize ([1, 2, 3, 4, 5], [1, 3, 5], 'categorical');
%! assert_equal (categories (C), {'[1, 3)'; '[3, 5]'});
%! assert_equal (isordinal (C), true);
%!test
%! C = discretize ([1, 2, 3, 4, 5], [1, 3, 5], 'categorical', {'lo', 'hi'});
%! assert_equal (categories (C), {'lo'; 'hi'});

## Error branches
%!error<discretize: not enough input arguments.> discretize ([1, 2, 3]);
%!error<discretize: X must be a real numeric or logical array.> ...
%! discretize ('abc', [1, 2, 3]);
%!error<discretize: X must be a real numeric or logical array.> ...
%! discretize ({1, 2}, [1, 2, 3]);
%!error<discretize: X must be a real numeric or logical array.> ...
%! discretize (complex ([1, 2], [1, 1]), [1, 2, 3]);
%!error<discretize: N must be a real positive integer.> discretize ([1, 2, 3], 0);
%!error<discretize: N must be a real positive integer.> discretize ([1, 2, 3], -1);
%!error<discretize: N must be a real positive integer.> discretize ([1, 2, 3], 2.5);
%!error<discretize: when X is an integer data type, specify bin edges instead of number of bins.> ...
%! discretize (int8 ([1, 5]), 2);
%!error<discretize: EDGES must be a vector that is real, numeric or logical, and monotonically increasing.> ...
%! discretize ([1, 2, 3], [3, 1]);
%!error<discretize: EDGES must be a vector that is real, numeric or logical, and monotonically increasing.> ...
%! discretize ([1, 2, 3], [1, NaN, 3]);
%!error<discretize: VALUES must be a vector with length equal to the number of bins.> ...
%! discretize ([1, 2, 3], [1, 3, 5], [10, 20, 30]);
%!error<discretize: 'IncludedEdge' value must be 'left' or 'right'.> ...
%! discretize ([1, 2, 3], [1, 3, 5], 'IncludedEdge', 'middle');
%!error<discretize: category names must be a cellstr or string vector with length equal to the number of bins.> ...
%! discretize ([1, 2, 3], [1, 2, 3], 'categorical', {'a'});
%!error<discretize: all elements of X must belong to a bin when VALUES is a cell array.> ...
%! discretize ([0, 1, 2, 3, 4, 5, 6], [1, 3, 5], {'lo', 'hi'});
