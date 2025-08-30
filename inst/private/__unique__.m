## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn  {private} {@var{y} =} __unique__ (@var{x})
## @deftypefnx {private} {@var{y} =} __unique__ (@var{x}, "rows")
## @deftypefnx {private} {@var{y} =} __unique__ (@dots{}, "sorted")
## @deftypefnx {private} {@var{y} =} __unique__ (@dots{}, "stable")
## @deftypefnx {private} {[@var{y}, @var{i}, @var{j}] =} __unique__ (@dots{})
## @deftypefnx {private} {[@var{y}, @var{i}, @var{j}] =} __unique__ (@dots{}, "first")
## @deftypefnx {private} {[@var{y}, @var{i}, @var{j}] =} __unique__ (@dots{}, "last")
## @deftypefnx {private} {[@var{y}, @var{i}, @var{j}] =} __unique__ (@dots{}, "legacy")
##
## Return the unique elements of @var{x}.
##
## @end deftypefn

function [y, i, j] = __unique__ (x, varargin)

  if (nargin < 1)
    print_usage ();
  elseif (! (isnumeric (x) || islogical (x) || ischar (x) || iscellstr (x)))
    error ("unique: X must be an array or cell array of strings.");
  endif

  if (nargin > 1)
    ## Parse options
    if (! iscellstr (varargin))
      error ("unique: options must be strings");
    endif

    optrows   = any (strcmp ("rows", varargin));
    optfirst  = any (strcmp ("first", varargin));
    optlast   = any (strcmp ("last", varargin));
    optsorted = any (strcmp ("sorted", varargin));
    optstable = any (strcmp ("stable", varargin));
    optlegacy = any (strcmp ("legacy", varargin));
    if (optfirst && optlast)
      error ('unique: cannot specify both "first" and "last"');
    elseif (optsorted && optstable)
      error ('unique: cannot specify both "sorted" and "stable"');
    elseif ((optfirst || optlast) && (optsorted || optstable))
      error ('unique: cannot specify "first"/"last" with "sorted"/"stable"');
    elseif (optlegacy && (optsorted || optstable))
      error ('unique: cannot specify "sorted" or "stable" with "legacy"');
    elseif (optrows + optfirst + optlast + optsorted + optstable + optlegacy
            != nargin-1)
      error ("unique: invalid option");
    endif

    ## Set defaults if not set earlier.
    if (! optfirst && ! optlast)
      optfirst = true;
    endif
    if (! optsorted && ! optstable)
      optsorted = true;
    endif
  else
    optrows = false;
    optfirst = true;
    optsorted = true;
    optlegacy = false;
  endif

  ## FIXME: The operations
  ##
  ##   match = (y(1:n-1) == y(2:n));
  ##   y(idx) = [];
  ##
  ## are very slow on sparse matrices.  Until they are fixed to be as
  ## fast as for full matrices, operate on the nonzero elements of the
  ## sparse array as long as we are not operating on rows.
  if (issparse (x) && ! optrows && nargout <= 1)
    if (nnz (x) < numel (x))
      y = unique ([0; nonzeros(x)], varargin{:});
    else
      ## Corner case where sparse matrix is actually full
      y = unique (full (x), varargin{:});
    endif
    return;
  endif

  if (optrows)
    n = rows (x);
    isrowvec = false;
  else
    n = numel (x);
    isrowvec = isrow (x);
  endif

  ## Special cases 0 and 1
  if (n == 0)
    y = x;
    if (! optrows && any (size (x)))
      if (iscellstr (x))
        y = cell (0, 1);
      else
        y = zeros (0, 1, class (x));
      endif
    endif
    i = j = [];
    return;
  elseif (n == 1)
    y = x;
    i = j = 1;
    return;
  endif

  ## Calculate y output
  if (optrows)
    if (nargout > 1 || ! optsorted)
      [y, j] = sortrows (x);
      j = j(:);
    else
      y = sortrows (x);
    endif
    if (iscellstr (x))
      match = all (cellfun (@isequal, y(1:n-1,:), y(2:n,:)), 2);
    else
      match = all (y(1:n-1,:) == y(2:n,:), 2);
    endif
    if (optsorted)
      y(match,:) = [];
    else
      y = x;
      y(j([false; match]), :) = [];
    endif
  else
    if (isvector (x))
      y = x;
    else
      y = x(:);
    endif
    if (nargout > 1 || ! optsorted)
      [y, j] = sort (y);
      j = j(:);
    else
      y = sort (y);
    endif
    if (iscellstr (y))
      match = strcmp (y(1:n-1), y(2:n));
    else
      match = (y(1:n-1) == y(2:n));
    endif
    if (optsorted)
      y(match) = [];
    else
      if (isvector (x))
        y = x;
      else
        y = x(:);
      endif
      y(j([false; match(:)])) = [];
    endif
  endif


  ## Calculate i and j outputs (2nd and 3rd outputs)
  if (nargout > 1)

    if (optsorted)

      idx = find (match);

      if (! optlegacy && optfirst)
        idx += 1;  # in-place is faster than other forms of increment
      endif

      i = j;
      i(idx) = [];

      if (nargout > 2)
        j(j) = cumsum (! [false; match(:)]);
      endif

    else

      ## Get inverse of sort index j so that sort(x)(k) = x(j)(k) = x.
      k = j;  # cheap way to copy dimensions
      k(j) = 1:n;

      ## Generate logical index of sorted unique value locations.
      uniquex = ! [false; match(:)];

      ## Remap unique locations to unsorted x, such that y = x(i).
      i = find (uniquex(k));

      if (nargout > 2)

        ni = numel (i);

        u = find (uniquex);      # Linear index of unique elements of sort(x)
        l = u(cumsum (uniquex)); # Expand u for all elements in sort(x)

        p = j;       # cheap way to copy dimensions
        p(i) = 1:ni; # set p to contain the vector positions of i.

        j = p(j(l(k))); # Replace j with 3rd output mapping y->x.

      endif
    endif

    if (optlegacy && isrowvec)
      i = i.';

      if (nargout > 2)
        j = j.';
      endif

    endif
  endif

endfunction
