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
## @deftypefn  {datatypes} {@var{B} =} groupsummary (@var{A}, @var{groupvars}, @var{method})
## @deftypefnx {datatypes} {[@var{B}, @var{BG}, @var{BC}] =} groupsummary (@var{A}, @var{groupvars}, @var{method})
##
## Compute summary statistics by group for an array.
##
## @code{@var{B} = groupsummary (@var{A}, @var{groupvars}, @var{method})} groups
## the rows of the array @var{A} by the grouping variables @var{groupvars} and
## applies @var{method} to each column of @var{A} within each group, returning
## the results in @var{B}, which has one row per group.  @var{groupvars} is a
## grouping vector with one element per row of @var{A}, or a cell array of such
## vectors.  Groups are the sorted unique combinations of grouping values; rows
## holding a missing value in a grouping variable form their own groups, sorted
## after the non-missing groups.
##
## @var{method} is one of the names @qcode{'sum'}, @qcode{'mean'},
## @qcode{'median'}, @qcode{'mode'}, @qcode{'var'}, @qcode{'std'}, @qcode{'min'},
## @qcode{'max'}, @qcode{'range'}, @qcode{'nnz'}, @qcode{'nummissing'}, or
## @qcode{'numunique'}, a function handle, or a cell
## array of method names and@/or function handles.  @code{NaN} values are omitted
## for every named method except @qcode{'nummissing'}; a function handle receives
## the values with @code{NaN} included and must return a single row.  When
## several methods are requested the columns of @var{B} are ordered by column of
## @var{A} first, then by method.
##
## @code{[@var{B}, @var{BG}, @var{BC}] = groupsummary (@dots{})} also returns
## @var{BG}, the grouping values that identify each group, and @var{BC}, a column
## vector with the number of rows in each group.  When @var{groupvars} is a
## single grouping vector, @var{BG} holds its representative value for each group;
## when several grouping variables are given, @var{BG} is a cell array with one
## element per grouping variable.
##
## The behaviour can be modified with the @qcode{'IncludeMissingGroups'} (default
## @code{true}) and @qcode{'IncludeEmptyGroups'} (default @code{false})
## @var{Name}/@var{Value} pairs, as for the @code{table} method.
##
## To summarise the variables of a @code{table}, call @code{groupsummary
## (@var{T}, @var{groupvars}, @dots{})}, which dispatches to the @code{table}
## method and returns the result as a table.
##
## @seealso{findgroups, splitapply, table}
## @end deftypefn
function [B, varargout] = groupsummary (A, groupvars, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  ## The first trailing argument is the method; the rest are Name-Value pairs.
  method = varargin{1};
  nvArgs = varargin(2:end);
  optNames = {'IncludeMissingGroups', 'IncludeEmptyGroups'};
  dfValues = {true, false};
  [incMiss, incEmpty] = parsePairedArguments (optNames, dfValues, nvArgs(:));
  if (! (isscalar (incMiss) && (islogical (incMiss) || isnumeric (incMiss))))
    error ("groupsummary: 'IncludeMissingGroups' must be a logical scalar.");
  endif
  incMiss = logical (incMiss);
  if (! (isscalar (incEmpty) && (islogical (incEmpty) || isnumeric (incEmpty))))
    error ("groupsummary: 'IncludeEmptyGroups' must be a logical scalar.");
  endif
  if (logical (incEmpty))
    error ("groupsummary: 'IncludeEmptyGroups' = true is not yet supported.");
  endif

  [methods, methNames, errmsg] = gs_normalise_methods (method);
  if (! isempty (errmsg))
    error ("groupsummary: %s", errmsg);
  endif
  if (isempty (methods))
    error ("groupsummary: a method is required for array input.");
  endif

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
        error ("groupsummary: grouping variables must be vectors.");
      endif
    elseif (isvector (g) || isempty (g))
      gvs{j} = g(:);
    else
      error ("groupsummary: grouping variables must be vectors.");
    endif
    if (size (gvs{j}, 1) != n)
      error (strcat ("groupsummary: each grouping variable must have one", ...
                     " element per row of A."));
    endif
  endfor

  ## Group the rows, treating missing grouping values as their own groups.
  [Grp, ng, repRows, errmsg] = gs_group_rows (gvs, incMiss);
  if (! isempty (errmsg))
    error ("groupsummary: %s", errmsg);
  endif

  ## Compute each method over each column of A (column first, then method).
  ncol = size (A, 2);
  B = [];
  for c = 1:ncol
    for mi = 1:numel (methods)
      vals = cell (ng, 1);
      for g = 1:ng
        rows = (Grp == g);
        [v, errmsg] = gs_apply_method (methods{mi}, A(rows,c));
        if (! isempty (errmsg))
          error ("groupsummary: %s for column %d of A.", errmsg, c);
        endif
        vals{g} = v;
      endfor
      B = [B, vertcat(vals{:})];
    endfor
  endfor

  ## Optional outputs: the grouping identifiers and the group counts.
  if (nargout > 1)
    if (numel (gvs) == 1)
      BG = gvs{1}(repRows,:);
    else
      BG = cell (1, numel (gvs));
      for j = 1:numel (gvs)
        BG{j} = gvs{j}(repRows,:);
      endfor
    endif
    varargout{1} = BG;
  endif
  if (nargout > 2)
    varargout{2} = accumarray (Grp(! isnan (Grp)), 1, [ng, 1]);
  endif

endfunction

## Normalise the METHOD argument into a cell array of method specs METHODS (each
## a method-name char vector or a function handle) and a parallel cell array of
## display names METHNAMES.  Returns an errmsg body emitted by the caller.
## Self-contained: no table dependency.
function [methods, methNames, errmsg] = gs_normalise_methods (method)
  methods = {};
  methNames = {};
  errmsg = '';
  if (isempty (method) && ! iscell (method) && ! ischar (method)
      && ! is_function_handle (method))
    return;
  endif
  if (is_function_handle (method) || (ischar (method) && isrow (method))
      || isa (method, 'string'))
    items = {method};
  elseif (iscell (method))
    items = method(:)';
  else
    errmsg = strcat ("METHOD must be a method name, a function handle, or a", ...
                     " cell array of method names and function handles.");
    return;
  endif
  known = {'sum', 'mean', 'median', 'mode', 'var', 'std', 'min', 'max', ...
           'range', 'nnz', 'nummissing', 'numunique'};
  nfun = 0;
  for k = 1:numel (items)
    it = items{k};
    if (is_function_handle (it))
      nfun++;
      methods{end+1} = it;
      methNames{end+1} = sprintf ("fun%d", nfun);
    elseif ((ischar (it) && isrow (it)) || (isa (it, 'string') && isscalar (it)))
      nm = lower (char (it));
      if (! any (strcmp (nm, known)))
        errmsg = sprintf ("'%s' is not a supported method name.", char (it));
        return;
      endif
      methods{end+1} = nm;
      methNames{end+1} = nm;
    else
      errmsg = strcat ("each method must be a method name or a function", ...
                       " handle.");
      return;
    endif
  endfor
endfunction

## Group rows by the grouping-variable values GVS (a cell array of column
## vectors), treating each variable's missing values as a single group value.
## Returns G (1..NGROUPS), NGROUPS, REPROWS (a representative row per group), and
## an errmsg body.  Groups are sorted by value with missing groups last; when
## INCMISS is false the rows with a missing grouping value are dropped.
function [G, ngroups, repRows, errmsg] = gs_group_rows (gvs, incMiss)
  errmsg = '';
  G = [];
  ngroups = 0;
  repRows = [];
  n = size (gvs{1}, 1);
  KEY = [];
  SORT = [];
  anyMiss = false (n, 1);
  for j = 1:numel (gvs)
    [p, m, e] = group_col_proxy (gvs{j});
    if (! isempty (e))
      errmsg = e;
      return;
    endif
    pc = p;
    pc(m,:) = 0;
    KEY = [KEY, pc, double(m)];
    sp = p;
    sp(m,:) = Inf;
    SORT = [SORT, sp];
    anyMiss = anyMiss | m;
  endfor

  [~, ia, ic] = unique (KEY, "rows");
  ng = numel (ia);
  grpMiss = anyMiss(ia);
  [~, ord] = sortrows (SORT(ia,:));
  reps = ia(ord);
  grpMiss = grpMiss(ord);
  pos = zeros (ng, 1);
  pos(ord) = 1:ng;
  G = pos(ic);

  if (! incMiss && any (grpMiss))
    keep = find (! grpMiss);
    newId = NaN (ng, 1);
    newId(keep) = 1:numel (keep);
    G = newId(G);
    reps = reps(keep);
    ng = numel (keep);
  endif
  ngroups = ng;
  repRows = reps;
endfunction

## Build a single-column grouping proxy for one grouping variable COL: a numeric
## matrix P whose sort order matches COL's value order, together with a logical
## MISS mask flagging the missing elements.  Returns an errmsg body emitted by
## the caller.  Self-contained: no table dependency.
function [p, miss, errmsg] = group_col_proxy (col)
  p = [];
  miss = [];
  errmsg = '';
  if (isa (col, 'categorical'))
    ## Categorical groups follow category order (ordinal or reordered), which the
    ## underlying category codes encode; <undefined> maps to NaN.
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

## Apply a single method M (a method-name char vector or a function handle) to
## the column slice X of one group, returning a row result V.  Named methods omit
## missing values (except 'nummissing'); a function handle receives X unchanged
## and must return a single row.  Returns an errmsg body emitted by the caller.
function [v, errmsg] = gs_apply_method (m, x)
  v = [];
  errmsg = '';
  if (is_function_handle (m))
    v = m (x);
    if (size (v, 1) != 1)
      errmsg = "a function handle method must return a single row";
    endif
    return;
  endif

  if (strcmp (m, 'nummissing'))
    v = sum (gs_missing_mask (x), 1);
    return;
  endif
  if (strcmp (m, 'numunique'))
    miss = gs_missing_mask (x);
    if (size (x, 2) == 1)
      v = numel (unique (x(! miss,:)));
    else
      v = zeros (1, size (x, 2));
      for c = 1:size (x, 2)
        col = x(:,c);
        v(c) = numel (unique (col(! miss(:,c))));
      endfor
    endif
    return;
  endif

  if (! (isnumeric (x) || islogical (x)))
    errmsg = sprintf (strcat ("named method '%s' is not supported for data", ...
                              " of type '%s'; use a function handle"), m, ...
                      class (x));
    return;
  endif
  x = double (x);
  nan = isnan (x);
  cnt = sum (! nan, 1);
  z = x;
  z(nan) = 0;
  switch (m)
    case 'sum'
      v = sum (z, 1);
    case 'mean'
      v = sum (z, 1) ./ cnt;
    case 'min'
      v = min (x, [], 1);
    case 'max'
      v = max (x, [], 1);
    case 'range'
      v = max (x, [], 1) - min (x, [], 1);
    case 'nnz'
      v = sum (x != 0 & ! nan, 1);
    case {'median', 'mode', 'var', 'std'}
      v = NaN (1, size (x, 2));
      for c = 1:size (x, 2)
        col = x(! nan(:,c), c);
        if (! isempty (col))
          switch (m)
            case 'median'
              v(c) = median (col);
            case 'mode'
              v(c) = mode (col);
            case 'var'
              v(c) = var (col);
            case 'std'
              v(c) = std (col);
          endswitch
        endif
      endfor
  endswitch
endfunction

## Return a logical mask the size of X flagging its missing elements.  Supports
## the numeric, logical, text, datetime, duration, calendarDuration, and
## categorical types.  Self-contained: no table dependency.
function mask = gs_missing_mask (x)
  if (isa (x, 'datetime'))
    DV = datevec (x);
    mask = any (isnan (DV), 2);
    mask = reshape (mask, size (x, 1), size (x, 2));
  elseif (isa (x, 'duration'))
    mask = isnan (days (x));
  elseif (isa (x, 'calendarDuration'))
    mask = any (isnan (proxyArray (x)), 2);
  elseif (isa (x, 'categorical') || isa (x, 'string'))
    mask = ismissing (x);
  elseif (iscellstr (x))
    mask = cellfun (@isempty, x);
  elseif (islogical (x))
    mask = false (size (x));
  elseif (isnumeric (x))
    mask = isnan (x);
  else
    mask = false (size (x));
  endif
endfunction

%!assert_equal (groupsummary ([10; 20; 30; 40], [1; 1; 2; 2], 'mean'), [15; 35])
%!assert_equal (groupsummary ([10; 20; 30; 40], [1; 1; 2; 2], 'sum'), [30; 70])
%!test
%! ## Multiple outputs return the group identifiers and the group counts
%! [B, BG, BC] = groupsummary ([10; 20; 30; 40], [1; 1; 2; 2], 'mean');
%! assert_equal (B, [15; 35]);
%! assert_equal (BG, [1; 2]);
%! assert_equal (BC, [2; 2]);
%!test
%! ## A matrix is summarised column by column
%! assert_equal (groupsummary ([10 1; 20 2; 30 3; 40 4], [1; 1; 2; 2], 'sum'), ...
%!               [30, 3; 70, 7]);
%!test
%! ## Several methods order the columns of B by data column first, then method
%! B = groupsummary ([10; 20; 30; 40], [1; 1; 2; 2], {'mean', 'sum'});
%! assert_equal (B, [15, 30; 35, 70]);
%!test
%! ## Named methods omit NaN in the data
%! assert_equal (groupsummary ([10; 20; NaN; 40], [1; 1; 2; 2], 'mean'), [15; 40]);
%!test
%! ## A missing value in a grouping variable forms its own group, sorted last
%! [B, BG, BC] = groupsummary ([10; 20; 30; 40], [1; 1; NaN; 2], 'mean');
%! assert_equal (B, [15; 40; 30]);
%! assert_equal (BG, [1; 2; NaN]);
%! assert_equal (BC, [2; 1; 1]);
%!test
%! ## 'IncludeMissingGroups' false drops the rows with a missing grouping value
%! B = groupsummary ([10; 20; 30; 40], [1; 1; NaN; 2], 'mean', ...
%!                   'IncludeMissingGroups', false);
%! assert_equal (B, [15; 40]);
%!test
%! ## A function handle is applied to each group's slice
%! B = groupsummary ([10; 20; 30; 40], [1; 1; 2; 2], @(v) max (v) - min (v));
%! assert_equal (B, [10; 10]);
%!test
%! ## Several grouping vectors return BG as a cell array, one per variable
%! [B, BG, BC] = groupsummary ([10; 20; 30; 40; 50], ...
%!                             {[1; 1; 2; 2; 1], {'b'; 'a'; 'a'; 'b'; 'b'}}, 'sum');
%! assert_equal (B, [20; 60; 30; 40]);
%! assert_equal (BG, {[1; 1; 2; 2], {'a'; 'b'; 'a'; 'b'}});
%! assert_equal (BC, [1; 2; 1; 1]);
%!test
%! ## A categorical grouping vector groups by category order, not alphabetically
%! c = categorical ({'medium'; 'low'; 'high'; 'low'}, ...
%!                  {'low', 'medium', 'high'}, 'Ordinal', true);
%! [B, BG, BC] = groupsummary ((1:4)', c, 'sum');
%! assert_equal (B, [6; 1; 3]);
%! assert_equal (cellstr (BG), {'low'; 'medium'; 'high'});
%! assert_equal (BC, [2; 1; 1]);

%!error <Invalid call> groupsummary ([1; 2], [1; 2])
%!error <groupsummary: 'bogus' is not a supported method name.> ...
%! groupsummary ([1; 2], [1; 2], 'bogus')
%!error <groupsummary: 'IncludeEmptyGroups' = true is not yet supported.> ...
%! groupsummary ([1; 2], [1; 2], 'sum', 'IncludeEmptyGroups', true)
%!error <groupsummary: each grouping variable must have one element per row of A.> ...
%! groupsummary ([1; 2; 3], [1; 2], 'sum')
