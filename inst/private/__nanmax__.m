## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn  {private} {@var{v} =} __nanmax__ (@var{x})
## @deftypefnx {private} {@var{v} =} __nanmax__ (@var{x}, [], @var{dim})
## @deftypefnx {private} {[@var{v}, @var{idx}] =} __nanmax__ (@dots{})
## @deftypefnx {private} {@var{v} =} __nanmax__ (@var{x}, [], @qcode{'all'})
## @deftypefnx {private} {@var{v} =} __nanmax__ (@var{x}, [], @var{vecdim})
## @deftypefnx {private} {@var{v} =} __nanmax__ (@var{x}, @var{y})
##
## Find the maximum while ignoring NaN values.
##
## @end deftypefn

function [v, idx] = __nanmax__ (x, y, dim)
  if (nargin < 1 || nargin > 3)
    print_usage;
  elseif (nargin == 1 || (nargin == 2 && isempty (y)))
    nanvals = isnan (x);
    x(nanvals) = -Inf;
    [v, idx] = max (x);
    v(all (nanvals)) = NaN;
  elseif (nargin == 3 && strcmpi (dim, "all") && isempty (y))
    x = x(:);
    nanvals = isnan (x);
    x(nanvals) = -Inf;
    [v, idx] = max (x);
    v(all (nanvals)) = NaN;
  elseif (nargin == 3 && isempty (y))
    if (isscalar (dim))
      nanvals = isnan (x);
      x(nanvals) = -Inf;
      [v, idx] = max (x, [], dim);
      v(all (nanvals, dim)) = NaN;
    else
      vecdim = sort (dim);
      if (! all (diff (vecdim)))
         error ("nanmax: VECDIM must contain non-repeating positive integers.");
      endif
      ## Ignore dimensions in VECDIM larger than actual array
      vecdim(find (vecdim > ndims (x))) = [];

      if (isempty (vecdim))
        v = x;
        if (nargout > 1)
          idx = reshape ([1:numel(x)], size (x));
        endif
      else

        ## Calculate permutation vector
        szx = size (x);
        remdims = 1:ndims (x);      # All dimensions
        remdims(vecdim) = [];       # Delete dimensions specified by vecdim
        nremd = numel (remdims);

        ## If all dimensions are given, it is equivalent to 'all' flag
        if (nremd == 0)
          x = x(:);
          nanvals = isnan (x);
          x(nanvals) = -Inf;
          [v, idx] = max (x);
          v(all (nanvals)) = NaN;

        else
          ## Permute to push vecdims to back
          perm = [remdims, vecdim];
          x = permute (x, perm);

          ## Reshape to squash all vecdims in final dimension
          sznew = [szx(remdims), prod(szx(vecdim))];
          x = reshape (x, sznew);

          ## Calculate nanmax on final dimension
          dim = nremd + 1;
          nanvals = isnan (x);
          x(nanvals) = -Inf;
          [v, idx] = max (x, [], dim);
          v(all (nanvals, dim)) = NaN;

          ## Inverse permute back to correct dimensions
          v = ipermute (v, perm);
          idx = ipermute (idx, perm);
        endif
      endif
    endif
  else
    if (nargout > 1)
      error ("nanmax: a second output is not supported with this syntax.");
    endif
    Xnan = isnan (x);
    Ynan = isnan (y);
    x(Xnan) = -Inf;
    y(Ynan) = -Inf;
    v = max (x, y);
    v(Xnan & Ynan) = NaN;
  endif
endfunction
