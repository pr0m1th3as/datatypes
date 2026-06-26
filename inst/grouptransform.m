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
## @deftypefn  {datatypes} {@var{B} =} grouptransform (@var{A}, @var{groupvars}, @var{method})
## @deftypefnx {datatypes} {[@var{B}, @var{BG}] =} grouptransform (@var{A}, @var{groupvars}, @var{method})
## @deftypefnx {datatypes} {[@dots{}] =} grouptransform (@dots{}, @var{Name}, @var{Value})
##
## Transform the columns of an array group by group.
##
## @code{@var{B} = grouptransform (@var{A}, @var{groupvars}, @var{method})}
## groups the rows of the array @var{A} by the grouping variables
## @var{groupvars}, applies @var{method} to each column of @var{A} within each
## group, and returns @var{B}, the transformed values, one row per row of @var{A}
## and in the original order.  @var{groupvars} is a grouping vector with one
## element per row of @var{A}, or a cell array of such vectors.  Rows holding a
## missing value in a grouping variable form their own groups, which are
## transformed like any other group.
##
## @var{method} is one of the names @qcode{'zscore'}, @qcode{'norm'},
## @qcode{'meancenter'}, @qcode{'rescale'}, @qcode{'meanfill'},
## @qcode{'linearfill'}, or a function handle.  @qcode{'zscore'} centers and
## scales each group to zero mean and unit standard deviation; @qcode{'norm'}
## divides by the group 2-norm; @qcode{'meancenter'} subtracts the group mean;
## @qcode{'rescale'} rescales to the range @code{[0, 1]}; @qcode{'meanfill'}
## replaces missing values with the group mean; and @qcode{'linearfill'} fills
## missing values by linear interpolation within the group (leaving leading and
## trailing missing values unchanged).  For the named methods @code{NaN} values
## are omitted when computing the group statistics.  A function handle is applied
## to each group's column slice and must return either a single row (broadcast)
## or one row per row of the group.
##
## @code{[@var{B}, @var{BG}] = grouptransform (@dots{})} also returns @var{BG},
## the grouping values of each row.  When @var{groupvars} is a single grouping
## vector, @var{BG} is that vector; when several grouping variables are given,
## @var{BG} is a cell array with one element per grouping variable.
##
## The @qcode{'ReplaceValues'} @var{Name}/@var{Value} pair (default @code{true})
## controls whether the transformed values replace the columns of @var{A} or, when
## @code{false}, are appended to them.  Binning the grouping variables (the
## @var{groupbins} argument) is not yet supported.
##
## To transform the variables of a @code{table}, call @code{grouptransform
## (@var{T}, @var{groupvars}, @dots{})}, which dispatches to the @code{table}
## method and returns the result as a table.
##
## @seealso{groupsummary, groupfilter, findgroups, splitapply, table}
## @end deftypefn
function [B, varargout] = grouptransform (A, groupvars, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  ## The transform method is the first trailing argument: a known method name or
  ## a function handle.  Binning (a GROUPBINS argument in its place) is not yet
  ## supported.
  method = varargin{1};
  knownMethods = {'zscore', 'norm', 'meancenter', 'rescale', ...
                  'meanfill', 'linearfill'};
  if (is_function_handle (method))
    ## ok
  elseif (((ischar (method) && isrow (method))
           || (isa (method, 'string') && isscalar (method)))
          && any (strcmpi (char (method), knownMethods)))
    method = lower (char (method));
  else
    error (strcat ("grouptransform: METHOD must be one of 'zscore', 'norm',", ...
                   " 'meancenter', 'rescale', 'meanfill', 'linearfill', or a", ...
                   " function handle; binning (the GROUPBINS argument) is not", ...
                   " yet supported."));
  endif

  ## Anything after METHOD must be the 'ReplaceValues' Name-Value pair (the array
  ## form has no DATAVARS argument; A's columns are the data).
  rest = varargin(2:end);
  optNames = {'ReplaceValues'};
  nvStart = numel (rest) + 1;
  for k = 1:numel (rest)
    a = rest{k};
    if (((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a)))
        && any (strcmpi (char (a), optNames)))
      nvStart = k;
      break;
    endif
  endfor
  if (nvStart > 1)
    error ("grouptransform: too many positional arguments.");
  endif
  dfValues = {true};
  replaceVals = parsePairedArguments (optNames, dfValues, rest(:));
  if (! (isscalar (replaceVals)
         && (islogical (replaceVals) || isnumeric (replaceVals))))
    error ("grouptransform: 'ReplaceValues' must be a logical scalar.");
  endif
  replaceVals = logical (replaceVals);

  ## Force the grouping variables to a cell array of column vectors.
  if (iscell (groupvars)
      && ! (iscellstr (groupvars) && isvector (groupvars)))
    gvs = groupvars(:)';
  else
    gvs = {groupvars};
  endif
  n = size (A, 1);
  for j = 1:numel (gvs)
    g = gvs{j};
    if (ischar (g))
      if (ndims (g) > 2)
        error ("grouptransform: grouping variables must be vectors.");
      endif
    elseif (isvector (g) || isempty (g))
      gvs{j} = g(:);
    else
      error ("grouptransform: grouping variables must be vectors.");
    endif
    if (size (gvs{j}, 1) != n)
      error (strcat ("grouptransform: each grouping variable must have one", ...
                     " element per row of A."));
    endif
  endfor

  ## Group the rows, treating missing grouping values as their own groups so
  ## that every row belongs to exactly one group.
  [Grp, ng, errmsg] = gt_group_rows (gvs);
  if (! isempty (errmsg))
    error ("grouptransform: %s", errmsg);
  endif

  ## Transform the columns of A, group by group.
  [trans, errmsg] = gt_transform_col (method, A, Grp, ng);
  if (! isempty (errmsg))
    error ("grouptransform: %s.", errmsg);
  endif

  if (replaceVals)
    B = trans;
  else
    B = [A, trans];
  endif

  ## Optional output: the grouping values of each row.
  if (nargout > 1)
    if (numel (gvs) == 1)
      BG = gvs{1};
    else
      BG = gvs;
    endif
    varargout{1} = BG;
  endif

endfunction

## Transform one data column COL (n-by-k) group by group, applying METHOD (a
## transform-name char vector or a function handle) to each group's slice and
## returning OUT the same size as COL.  G is the n-by-1 group-number vector
## (1..NG), every row assigned to a group.  A function handle must return a
## single row (broadcast) or one row per group row.  Returns an errmsg body
## emitted by the caller.  Self-contained: no table dependency.
function [out, errmsg] = gt_transform_col (method, col, G, ng)
  out = [];
  errmsg = '';
  if (! (isnumeric (col) || islogical (col)))
    errmsg = sprintf (strcat ("grouptransform requires numeric or logical", ...
                              " data; got '%s'"), class (col));
    return;
  endif
  x = double (col);
  out = x;
  for g = 1:ng
    rows = find (G == g);
    if (isempty (rows))
      continue;
    endif
    slice = x(rows,:);
    if (is_function_handle (method))
      r = method (slice);
      if (! (isnumeric (r) || islogical (r)))
        errmsg = "the transform function must return a numeric result";
        out = [];
        return;
      endif
      if (size (r, 1) == 1)
        r = repmat (r, numel (rows), 1);
      endif
      if (! isequal (size (r), size (slice)))
        errmsg = strcat ("the transform function must return a result the", ...
                         " same size as the group, or a single row");
        out = [];
        return;
      endif
      out(rows,:) = r;
    else
      for c = 1:columns (slice)
        out(rows,c) = gt_apply_named (method, slice(:,c));
      endfor
    endif
  endfor
endfunction

## Apply a single named transform METHOD to the column vector X, returning the
## transformed values V the same size as X.  NaN values are omitted when
## computing the group statistics; the centring and scaling methods leave NaN in
## place, while 'meanfill'/'linearfill' fill them.  Self-contained.
function v = gt_apply_named (method, x)
  nan = isnan (x);
  xo = x(! nan);
  switch (method)
    case 'meancenter'
      v = x - mean (xo);
    case 'zscore'
      v = (x - mean (xo)) / std (xo);
    case 'norm'
      v = x / norm (xo);
    case 'rescale'
      mn = min (xo);
      mx = max (xo);
      v = (x - mn) / (mx - mn);
    case 'meanfill'
      v = x;
      v(nan) = mean (xo);
    case 'linearfill'
      v = gt_linearfill (x);
  endswitch
endfunction

## Fill the missing values of the column vector X by linear interpolation over
## the non-missing positions, leaving leading and trailing missing values (and
## any group with fewer than two non-missing values) unchanged.  Self-contained.
function v = gt_linearfill (x)
  v = x;
  idx = find (! isnan (x));
  if (numel (idx) >= 2)
    pos = (1:numel (x))';
    vi = interp1 (idx, x(idx), pos, "linear");
    fill = isnan (x) & pos > idx(1) & pos < idx(end);
    v(fill) = vi(fill);
  endif
endfunction

## Group rows by the grouping-variable values GVS (a cell array of column
## vectors), treating each variable's missing values as a single group value so
## that every row is assigned to a group.  Returns G (1..NGROUPS), NGROUPS, and
## an errmsg body emitted by the caller.  Self-contained: no table dependency.
function [G, ngroups, errmsg] = gt_group_rows (gvs)
  errmsg = '';
  G = [];
  ngroups = 0;
  KEY = [];
  for j = 1:numel (gvs)
    [p, m, e] = gt_col_proxy (gvs{j});
    if (! isempty (e))
      errmsg = e;
      return;
    endif
    pc = p;
    pc(m,:) = 0;
    KEY = [KEY, pc, double(m)];
  endfor
  [~, ~, ic] = unique (KEY, "rows");
  G = ic;
  ngroups = max (ic);
endfunction

## Build a single-column grouping proxy for one grouping variable COL: a numeric
## matrix P whose value order matches COL's, together with a logical MISS mask
## flagging the missing elements.  Returns an errmsg body emitted by the caller.
## Self-contained: no table dependency.
function [p, miss, errmsg] = gt_col_proxy (col)
  p = [];
  miss = [];
  errmsg = '';
  if (isa (col, 'categorical'))
    p = double (col)(:);
    miss = isnan (p);
  elseif (isa (col, 'string') || iscellstr (col) || ischar (col))
    c = cellstr (col);
    c = c(:);
    miss = cellfun (@isempty, c);
    [~, ~, ic] = unique (c);
    p = ic(:);
  elseif (isa (col, 'datetime'))
    DV = datevec (col);
    nat = any (isnan (DV), 2);
    DV(nat,:) = 0;
    DV(nat,2:3) = 1;
    p = datenum (DV);
    p(nat) = NaN;
    p = p(:);
    miss = isnan (p);
  elseif (isa (col, 'duration'))
    p = days (col)(:);
    miss = isnan (p);
  elseif (isa (col, 'calendarDuration'))
    p = proxyArray (col);
    miss = any (isnan (p), 2);
  elseif (isnumeric (col) || islogical (col))
    p = double (col)(:);
    miss = isnan (p);
  else
    errmsg = sprintf ("unsupported grouping variable type '%s'.", class (col));
  endif
endfunction

%!test
%! ## 'meancenter' subtracts the group mean
%! g = [1; 1; 1; 2; 2];
%! x = [10; 20; 30; 40; 60];
%! assert_equal (grouptransform (x, g, 'meancenter'), [-10; 0; 10; -10; 10]);
%!test
%! ## 'zscore', 'norm', and 'rescale' spot checks
%! g = [1; 1; 1; 2; 2];
%! x = [10; 20; 30; 40; 60];
%! assert_equal (grouptransform (x, g, 'zscore'), ...
%!               [-1; 0; 1; -1/sqrt(2); 1/sqrt(2)], 8 * eps);
%! assert_equal (grouptransform (x, g, 'norm'), ...
%!               [[10; 20; 30] ./ norm([10; 20; 30]); ...
%!                [40; 60] ./ norm([40; 60])], 8 * eps);
%! assert_equal (grouptransform (x, g, 'rescale'), [0; 0.5; 1; 0; 1]);
%!test
%! ## 'meanfill' replaces missing values with the group mean
%! g = [1; 1; 2; 2];
%! x = [10; NaN; 30; 40];
%! assert_equal (grouptransform (x, g, 'meanfill'), [10; 10; 30; 40]);
%!test
%! ## 'linearfill' interpolates interior missing values, leaving the edges NaN
%! g = [1; 1; 1; 1; 2; 2; 2];
%! x = [1; NaN; NaN; 4; NaN; 10; NaN];
%! assert_equal (grouptransform (x, g, 'linearfill'), [1; 2; 3; 4; NaN; 10; NaN]);
%!test
%! ## A function handle is applied to each group's slice
%! g = [1; 1; 1; 2; 2];
%! x = [10; 20; 30; 40; 60];
%! assert_equal (grouptransform (x, g, @(v) v - mean (v)), [-10; 0; 10; -10; 10]);
%!test
%! ## The second output returns the grouping values of each row
%! g = [1; 1; 2; 2];
%! x = [10; 20; 30; 40];
%! [B, BG] = grouptransform (x, g, 'norm');
%! assert_equal (B, [[10; 20] ./ norm([10; 20]); [30; 40] ./ norm([30; 40])], ...
%!               8 * eps);
%! assert_equal (BG, [1; 1; 2; 2]);
%!test
%! ## A matrix transforms each column independently per group
%! A = [10 1; 20 2; 30 3; 40 4];
%! g = [1; 1; 2; 2];
%! assert_equal (grouptransform (A, g, 'meancenter'), ...
%!               [-5 -0.5; 5 0.5; -5 -0.5; 5 0.5]);
%!test
%! ## 'ReplaceValues' false appends the transformed columns to A
%! g = [1; 1; 1; 2; 2];
%! x = [10; 20; 30; 40; 60];
%! B = grouptransform (x, g, 'meancenter', 'ReplaceValues', false);
%! assert_equal (B, [10 -10; 20 0; 30 10; 40 -10; 60 10]);
%!test
%! ## Rows with a missing grouping value form their own group, in original order
%! g = [1; 1; NaN; 2; 2];
%! x = [10; 20; 30; 40; 60];
%! [B, BG] = grouptransform (x, g, 'meancenter');
%! assert_equal (B, [-5; 5; 0; -10; 10]);
%! assert_equal (BG, [1; 1; NaN; 2; 2]);
%!test
%! ## Several grouping vectors return BG as a cell array, one per variable
%! x = [10; 20; 30; 40; 60];
%! g1 = [1; 1; 2; 2; 1];
%! g2 = {'a'; 'a'; 'b'; 'b'; 'a'};
%! [B, BG] = grouptransform (x, {g1, g2}, 'meancenter');
%! assert_equal (B, [-20; -10; -5; 5; 30]);
%! assert_equal (BG, {[1; 1; 2; 2; 1], {'a'; 'a'; 'b'; 'b'; 'a'}});

%!error <Invalid call> grouptransform ([1; 2], [1; 2])
%!error <grouptransform: METHOD must be one of 'zscore', 'norm', 'meancenter', 'rescale', 'meanfill', 'linearfill', or a function handle; binning \(the GROUPBINS argument\) is not yet supported.> ...
%! grouptransform ([1; 2], [1; 2], 'bogus')
%!error <grouptransform: 'ReplaceValues' must be a logical scalar.> ...
%! grouptransform ([1; 2], [1; 2], 'zscore', 'ReplaceValues', 'yes')
%!error <grouptransform: too many positional arguments.> ...
%! grouptransform ([1; 2], [1; 2], 'zscore', 5)
%!error <grouptransform: each grouping variable must have one element per row of A.> ...
%! grouptransform ([1; 2; 3], [1; 2], 'zscore')
