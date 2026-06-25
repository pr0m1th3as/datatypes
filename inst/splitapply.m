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
## @deftypefn  {datatypes} {@var{Y} =} splitapply (@var{func}, @var{X}, @var{G})
## @deftypefnx {datatypes} {@var{Y} =} splitapply (@var{func}, @var{X1}, @dots{}, @var{XN}, @var{G})
## @deftypefnx {datatypes} {[@var{Y1}, @dots{}, @var{YM}] =} splitapply (@dots{})
##
## Split data into groups and apply a function to each group.
##
## @code{@var{Y} = splitapply (@var{func}, @var{X}, @var{G})} splits the data
## variable @var{X} into groups according to the group numbers @var{G}
## (typically produced by @code{findgroups}), applies the function handle
## @var{func} to each group, and concatenates the per-group results into the
## output @var{Y}.  @var{G} must be a vector of positive integers with one
## element per element of @var{X}; if it identifies @var{N} groups, every
## integer between 1 and @var{N} must occur at least once.  Elements for which
## @var{G} is @code{NaN} are omitted.
##
## @code{@var{Y} = splitapply (@var{func}, @var{X1}, @dots{}, @var{XN}, @var{G})}
## splits each of the data variables @var{X1}, @dots{}, @var{XN} and passes the
## corresponding group of each as a separate input argument to @var{func}.
##
## @code{[@var{Y1}, @dots{}, @var{YM}] = splitapply (@dots{})} returns the
## multiple outputs of @var{func}, each concatenated across groups.
##
## To split the variables of a @code{table}, call
## @code{splitapply (@var{func}, @var{T}, @var{G})}, which dispatches to the
## @code{table} method.
##
## @seealso{findgroups, table}
## @end deftypefn
function varargout = splitapply (func, varargin)

  if (nargin < 3)
    print_usage ();
  endif
  if (! is_function_handle (func))
    error ("splitapply: FUNC must be a function handle.");
  endif

  ## The last argument is the grouping vector; the rest are data variables.
  G = varargin{end};
  data = varargin(1:end-1);

  ## Force each data variable to a column (char matrices keep their rows).
  for k = 1:numel (data)
    v = data{k};
    if (! ischar (v) && isvector (v))
      data{k} = v(:);
    endif
  endfor

  ## All data variables must share the same number of rows.
  n = size (data{1}, 1);
  for k = 2:numel (data)
    if (size (data{k}, 1) != n)
      error (strcat ("splitapply: all data variables must have the same", ...
                     " number of rows."));
    endif
  endfor

  ## Validate the grouping vector.
  if (! (isnumeric (G) && isvector (G) && numel (G) == n))
    error (strcat ("splitapply: G must be a numeric vector with one element", ...
                   " per row of the data variables."));
  endif
  G = G(:);
  gv = G(! isnan (G));
  if (any (gv != fix (gv)) || any (gv < 1))
    error ("splitapply: G must contain positive integers.");
  endif
  if (isempty (gv))
    N = 0;
  else
    N = max (gv);
    if (! isequal (unique (gv), (1:N)'))
      error (strcat ("splitapply: G must contain every integer between 1", ...
                     " and the number of groups."));
    endif
  endif

  ## Apply FUNC to each group and concatenate the per-group results.
  nvar = numel (data);
  nout = max (nargout, 1);
  results = cell (N, nout);
  for g = 1:N
    rows = (G == g);
    args = cell (1, nvar);
    for j = 1:nvar
      v = data{j};
      args{j} = v(rows,:);
    endfor
    [results{g,:}] = func (args{:});
  endfor

  varargout = cell (1, nout);
  for k = 1:nout
    varargout{k} = vertcat (results{:,k});
  endfor

endfunction

%!assert_equal (splitapply (@(x) mean (x), [1; 2; 3; 4], [1; 1; 2; 2]), [1.5; 3.5])
%!assert_equal (splitapply (@(x) sum (x), [1; 2; 3], [2; 1; 2]), [2; 4])
%!test
%! ## Multiple data variables become separate arguments to FUNC
%! y = splitapply (@(a, b) sum (a) + sum (b), [1; 2; 3; 4], [10; 20; 30; 40], ...
%!                 [1; 1; 2; 2]);
%! assert_equal (y, [33; 77]);
%!test
%! ## Multiple outputs are concatenated separately
%! [lo, hi] = splitapply (@(x) deal (min (x), max (x)), [4; 1; 3; 2], ...
%!                        [1; 1; 2; 2]);
%! assert_equal (lo, [1; 2]);
%! assert_equal (hi, [4; 3]);
%!test
%! ## NaN group numbers omit the corresponding elements
%! y = splitapply (@(x) mean (x), [1; 2; 99; 4], [1; 1; NaN; 2]);
%! assert_equal (y, [1.5; 4]);

%!error <Invalid call> splitapply (@mean, [1; 2])
%!error <FUNC must be a function handle> splitapply (1, [1; 2], [1; 2])
%!error <G must contain every integer> splitapply (@mean, [1; 2], [1; 3])
