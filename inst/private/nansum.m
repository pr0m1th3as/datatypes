## Copyright (C) 2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn  {private} {@var{s} =} nansum (@var{x})
## @deftypefnx {private} {@var{s} =} nanmax (@var{x}, @qcode{'all'})
## @deftypefnx {private} {@var{s} =} nanmax (@var{x}, @var{dim})
## @deftypefnx {private} {@var{s} =} nanmax (@var{x}, @var{vecdim})
##
## Compute the sum while ignoring NaN values.
##
## @end deftypefn

function s = nansum (x, dim)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (! isnumeric (x))
    error ("nansum: X must be numeric.");
  elseif (isempty (x))
    s = 0;
  elseif (nargin == 1)
    nanvals = isnan (x);
    x(nanvals) = 0;
    s = sum (x);
    s(all (nanvals)) = 0;
  elseif (nargin == 2 && strcmpi (dim, "all"))
    x = x(:);
    nanvals = isnan (x);
    x(nanvals) = 0;
    s = sum (x);
    s(all (nanvals)) = 0;
  else  # DIM must be a numeric scalar or vector
    if (isscalar (dim))
      if (! isnumeric (dim) || fix (dim) != dim || dim <= 0)
        error ("nansum: DIM must be a positive integer.");
      endif
      nanvals = isnan (x);
      x(nanvals) = 0;
      s = sum (x, dim);
      s(all (nanvals, dim)) = 0;
    else
      if (! isvector (dim) || any (fix (dim) != dim) || any (dim <= 0))
        error ("nansum: VECDIM must be a vector of positive integer.");
      endif
      vecdim = sort (dim);
      if (! all (diff (vecdim)))
         error ("nansum: VECDIM must contain non-repeating positive integers.");
      endif
      ## Ignore dimensions in VECDIM larger than actual array
      vecdim(find (vecdim > ndims (x))) = [];

      if (isempty (vecdim))
        s = x;
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
          x(nanvals) = 0;
          s = sum (x);
          s(all (nanvals)) = 0;

        else
          ## Permute to push vecdims to back
          perm = [remdims, vecdim];
          x = permute (x, perm);

          ## Reshape to squash all vecdims in final dimension
          sznew = [szx(remdims), prod(szx(vecdim))];
          x = reshape (x, sznew);

          ## Calculate nansum on final dimension
          dim = nremd + 1;
          nanvals = isnan (x);
          x(nanvals) = 0;
          s = sum (x, dim);
          s(all (nanvals, dim)) = 0;

          ## Inverse permute back to correct dimensions
          s = ipermute (s, perm);
        endif
      endif
    endif
  endif
endfunction
