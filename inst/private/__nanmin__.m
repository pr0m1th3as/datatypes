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
## @deftypefn  {private} {@var{v} =} __nanmin__ (@var{x})
## @deftypefnx {private} {@var{v} =} __nanmin__ (@var{x}, [], @var{dim})
## @deftypefnx {private} {[@var{v}, @var{idx}] =} __nanmin__ (@dots{})
## @deftypefnx {private} {@var{v} =} __nanmin__ (@var{x}, [], @qcode{'all'})
## @deftypefnx {private} {@var{v} =} __nanmin__ (@var{x}, [], @var{vecdim})
## @deftypefnx {private} {@var{v} =} __nanmin__ (@var{x}, [], @dots{}, @var{includenan})
## @deftypefnx {private} {@var{v} =} __nanmin__ (@var{x}, @var{y})
## @deftypefnx {private} {@var{v} =} __nanmin__ (@var{x}, @var{y}, @var{includenan})
##
## Find the minimum while ignoring NaN values.
##
## @end deftypefn

function [v, idx] = __nanmin__ (x, varargin)
  if (nargin < 1 || nargin > 4)
    print_usage;
  endif
  ## Get optional arguments
  nargs = numel (varargin);
  if (nargs == 0)
    y = [];
    dim = [];
    include = false;
  elseif (nargs == 1)
    y = varargin{1};
    dim = [];
    include = false;
  elseif (nargs == 2)
    y = varargin{1};
    if (isempty (y))
      dim = varargin{2};
      include = false;
    else
      dim = [];
      include = varargin{2};
    endif
  elseif (nargs == 3)
    y = varargin{1};
    if (isempty (y))
      dim = varargin{2};
      include = varargin{3};
    else
      dim = [];
      include = varargin{2};
    endif
  endif
  ## Process according to given input arguments
  if (isempty (y) && isempty (dim))
    nanvals = isnan (x);
    x(nanvals) = Inf;
    [v, idx] = min (x);
    if (include)
      v(any (nanvals)) = NaN;
    else
      v(all (nanvals)) = NaN;
    endif
  elseif (isempty (y) && strcmpi (dim, "all"))
    x = x(:);
    nanvals = isnan (x);
    x(nanvals) = Inf;
    [v, idx] = min (x);
    if (include)
      v(any (nanvals)) = NaN;
    else
      v(all (nanvals)) = NaN;
    endif
  elseif (isempty (y))
    if (isscalar (dim))
      nanvals = isnan (x);
      x(nanvals) = Inf;
      [v, idx] = min (x, [], dim);
      if (include)
        v(any (nanvals, dim)) = NaN;
      else
        v(all (nanvals, dim)) = NaN;
      endif
    else
      vecdim = sort (dim);
      if (! all (diff (vecdim)))
         error ("nanmin: VECDIM must contain non-repeating positive integers.");
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
          x(nanvals) = Inf;
          [v, idx] = min (x);
          if (include)
            v(any (nanvals)) = NaN;
          else
            v(all (nanvals)) = NaN;
          endif

        else
          ## Permute to push vecdims to back
          perm = [remdims, vecdim];
          x = permute (x, perm);

          ## Reshape to squash all vecdims in final dimension
          sznew = [szx(remdims), prod(szx(vecdim))];
          x = reshape (x, sznew);

          ## Calculate nanmin on final dimension
          dim = nremd + 1;
          nanvals = isnan (x);
          x(nanvals) = Inf;
          [v, idx] = min (x, [], dim);
          if (include)
            v(any (nanvals, dim)) = NaN;
          else
            v(all (nanvals, dim)) = NaN;
          endif

          ## Inverse permute back to correct dimensions
          v = ipermute (v, perm);
          idx = ipermute (idx, perm);
        endif
      endif
    endif
  else  # y is not empty
    if (nargout > 1)
      error ("nanmin: a second output is not supported with this syntax.");
    endif
    Xnan = isnan (x);
    Ynan = isnan (y);
    x(Xnan) = Inf;
    y(Ynan) = Inf;
    v = min (x, y);
    if (include)
      v(Xnan | Ynan) = NaN;
    else
      v(Xnan & Ynan) = NaN;
    endif
  endif
endfunction
