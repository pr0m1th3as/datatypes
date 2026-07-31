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
## @deftypefn  {private} {[@var{I}, @var{lidx}] =} __minmaxk__ (@var{x}, @var{K}, @var{largest}, @var{args})
## @deftypefnx {private} {[@var{I}, @var{lidx}, @var{errmsg}] =} __minmaxk__ (@dots{})
##
## Rank the @var{K} largest or smallest elements of a numeric proxy array.
##
## This is the shared engine of the @code{maxk} and @code{mink} methods of the
## @code{datetime}, @code{duration}, and @code{categorical} classes.  It works
## on a real numeric proxy @var{x} of the calling class, in which missing
## elements are represented by @qcode{NaN}, and it returns indices only, so the
## caller can gather whatever component arrays it stores.
##
## @var{largest} must be @code{true} for @code{maxk} and @code{false} for
## @code{mink}.  @var{args} is the cell array of the caller's optional input
## arguments, which may contain the operating dimension.
##
## @var{I} contains the indices of the ranked elements along the operating
## dimension, which is the second output of @code{maxk} and @code{mink}, while
## @var{lidx} contains the equivalent linear indices into @var{x}, with which
## the caller can index its own arrays directly.  Both have the size of @var{x}
## with the operating dimension reduced to @code{min (@var{K}, n)}, where
## @code{n} is the size of @var{x} along that dimension.
##
## Each slice along the operating dimension is ranked independently.  The
## non-missing elements are ordered by value, with ties keeping their original
## relative order, and the missing ones are appended after them in their
## original order.  Hence, missing elements are never ranked and they only
## appear in the output when @var{K} exceeds the number of non-missing elements
## in that slice.  This differs from @code{sort}, which orders missing elements
## according to its @qcode{'MissingPlacement'} option.
##
## Input validation is done here so that it cannot drift between the calling
## classes, but no error is raised.  Instead, @var{errmsg} returns the message
## body, which is empty on success, and the caller is expected to raise the
## error under its own class and method name.
##
## @end deftypefn

function [I, lidx, errmsg] = __minmaxk__ (x, K, largest, args)

  I = [];
  lidx = [];
  errmsg = '';

  ## Check K
  if (! (isnumeric (K) && isscalar (K) && isreal (K) && isfinite (K) &&
         fix (K) == K && K >= 0))
    errmsg = "K must be a nonnegative integer scalar.";
    return;
  endif

  ## Check optional arguments.  Named options are reported by name, since
  ## otherwise 'MissingPlacement' and 'ComparisonMethod', which both belong to
  ## 'sort' but not here, would be reported as an invalid dimension.
  if (numel (args) > 0)
    [args{:}] = convertStringsToChars (args{:});
    cid = cellfun (@ischar, args);
    if (any (cid))
      errmsg = sprintf ("'%s' is not a supported option.", args{find (cid, 1)});
      return;
    endif
  endif
  if (numel (args) > 1)
    errmsg = "too many input arguments.";
    return;
  endif

  ## Get operating dimension
  if (numel (args) == 1)
    dim = args{1};
    if (! (isnumeric (dim) && isscalar (dim) && isreal (dim) &&
           isfinite (dim) && fix (dim) == dim && dim > 0))
      errmsg = "DIM must be a positive integer scalar.";
      return;
    endif
  else
    dim = find (size (x) != 1, 1);
    if (isempty (dim))  # scalar
      dim = 1;
    endif
  endif

  ## Elements are only ever taken from those available along the operating
  ## dimension, so K is clamped rather than padded.  A dimension beyond the
  ## dimensions of x is singleton, which leaves x untouched.
  nd = max (ndims (x), dim);
  sz = size (x);
  sz(end+1:nd) = 1;
  kk = min (K, sz(dim));

  ## Bring the operating dimension to the front and flatten everything after
  ## it, so that the loop below holds for an array of any number of dimensions.
  ## The result is put back the way it came at the end.
  perm = [dim, 1:dim-1, dim+1:nd];
  psz = sz(perm);
  X = reshape (permute (x, perm), psz(1), []);
  nc = columns (X);

  ## Rank each slice: the non-missing elements by value, ties broken by original
  ## position, followed by the missing elements in their original order.
  idx = zeros (kk, nc);
  for j = 1:nc
    col = X(:,j);
    mis = isnan (col);
    fin = find (! mis);
    if (largest)
      [~, ord] = sortrows ([-col(fin), fin]);
    else
      [~, ord] = sortrows ([col(fin), fin]);
    endif
    ranked = [fin(ord); find(mis)];
    idx(:,j) = ranked(1:kk);
  endfor

  ## Restore the original shape and dimension order.  The linear indices are
  ## mapped through the same permutation, so that they index x as it was given.
  osz = psz;
  osz(1) = kk;
  I = ipermute (reshape (idx, osz), perm);
  map = reshape (permute (reshape (1:prod (sz), sz), perm), psz(1), []);
  lin = idx + repmat ((0:nc-1) * psz(1), kk, 1);
  lidx = ipermute (reshape (map(lin), osz), perm);

endfunction
