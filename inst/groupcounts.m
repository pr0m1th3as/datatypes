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
## @deftypefn  {datatypes} {@var{B} =} groupcounts (@var{A})
## @deftypefnx {datatypes} {@var{B} =} groupcounts (@var{A}, @var{groupbins})
## @deftypefnx {datatypes} {[@var{B}, @var{BG}, @var{BP}] =} groupcounts (@dots{})
## @deftypefnx {datatypes} {[@dots{}] =} groupcounts (@dots{}, @var{Name}, @var{Value})
##
## Count the number of elements in each group of an array.
##
## @code{@var{B} = groupcounts (@var{A})} groups the rows of @var{A} by their
## values and returns @var{B}, a column vector with the number of elements in
## each group.  @var{A} is a grouping vector, a matrix whose columns are grouping
## variables, or a cell array of grouping vectors.  Groups are the sorted unique
## combinations of grouping values; rows holding a missing value in a grouping
## variable form their own groups, sorted after the non-missing groups.
##
## @code{[@var{B}, @var{BG}, @var{BP}] = groupcounts (@dots{})} also returns
## @var{BG}, the grouping values that identify each group, and @var{BP}, a column
## vector giving each group's count as a percentage of the total.  When @var{A}
## is a single grouping vector, @var{BG} holds its representative value for each
## group; when several grouping variables are given, @var{BG} is a cell array
## with one element per grouping variable.
##
## The optional @var{groupbins} argument bins the grouping variables before
## grouping (a vector of bin edges or a positive integer number of bins, applied
## to a numeric, datetime, or duration grouping variable, or a cell array with
## one scheme per grouping variable); each binned variable becomes a categorical
## of bin interval labels.
##
## The behaviour can be modified with the @qcode{'IncludeMissingGroups'} (default
## @code{true}), @qcode{'IncludeEmptyGroups'} (default @code{false}), and
## @qcode{'IncludedEdge'} (default @qcode{'left'}, the inclusive bin edge)
## @var{Name}/@var{Value} pairs, as for the @code{table} method.  When
## @qcode{'IncludeEmptyGroups'} is @code{true}, the unused categories of a
## categorical or binned grouping variable contribute empty groups.
##
## To count the rows in each group of a @code{table}, call @code{groupcounts
## (@var{T}, @var{groupvars}, @dots{})}, which dispatches to the @code{table}
## method and returns the result as a table.
##
## @seealso{groupsummary, findgroups, splitapply, table}
## @end deftypefn
function [B, varargout] = groupcounts (A, varargin)

  if (nargin < 1)
    print_usage ();
  endif

  ## An optional GROUPBINS positional argument may precede the Name-Value
  ## options; anything else after A must be a recognised option.
  optNames = {'IncludeMissingGroups', 'IncludeEmptyGroups', 'IncludedEdge'};
  hasGB = false;
  groupbins = [];
  if (! isempty (varargin))
    a = varargin{1};
    isOpt = ((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a))) ...
            && any (strcmpi (char (a), optNames));
    if (! isOpt)
      if (__groupbins__ ('is_spec', a))
        hasGB = true;
        groupbins = a;
        varargin = varargin(2:end);
      else
        error (strcat ("groupcounts: invalid argument; expected a GROUPBINS", ...
                       " binning scheme or a Name-Value option."));
      endif
    endif
  endif

  ## Parse Name-Value options.
  dfValues = {true, false, 'left'};
  [incMiss, incEmpty, incEdge] = ...
              parsePairedArguments (optNames, dfValues, varargin(:));
  if (! (isscalar (incMiss) && (islogical (incMiss) || isnumeric (incMiss))))
    error ("groupcounts: 'IncludeMissingGroups' must be a logical scalar.");
  endif
  incMiss = logical (incMiss);
  if (! (isscalar (incEmpty) && (islogical (incEmpty) || isnumeric (incEmpty))))
    error ("groupcounts: 'IncludeEmptyGroups' must be a logical scalar.");
  endif
  incEmpty = logical (incEmpty);
  incEdge = check_included_edge ('groupcounts', incEdge);

  ## Resolve A into a cell array of grouping column vectors: a cell array holds
  ## one grouping variable per element, a numeric/logical matrix one per column,
  ## and anything else is a single grouping variable.
  if (iscell (A) && ! (iscellstr (A) && isvector (A)))
    gvs = A(:)';
  elseif (! ischar (A) && ismatrix (A) && size (A, 2) > 1)
    gvs = cell (1, size (A, 2));
    for j = 1:size (A, 2)
      gvs{j} = A(:,j);
    endfor
  else
    gvs = {A};
  endif

  for j = 1:numel (gvs)
    g = gvs{j};
    if (ischar (g))
      if (ndims (g) > 2)
        error ("groupcounts: grouping variables must be vectors.");
      endif
    elseif (isvector (g) || isempty (g))
      gvs{j} = g(:);
    else
      error ("groupcounts: grouping variables must be vectors.");
    endif
  endfor
  n = size (gvs{1}, 1);
  for j = 2:numel (gvs)
    if (size (gvs{j}, 1) != n)
      error (strcat ("groupcounts: each grouping variable must have the", ...
                     " same number of elements."));
    endif
  endfor

  ## Bin the grouping variables when a GROUPBINS argument was given.
  if (hasGB)
    names = cell (1, numel (gvs));
    for j = 1:numel (gvs)
      names{j} = sprintf ("%d", j);
    endfor
    [gvs, ~, errmsg] = __groupbins__ ('bin', gvs, names, groupbins, incEdge, ...
                                   'groupcounts');
    if (! isempty (errmsg))
      error ("groupcounts: %s", errmsg);
    endif
  endif

  ## Group the rows, treating missing grouping values as their own groups;
  ## IncludeEmptyGroups adds the unused categories of a categorical or binned
  ## grouping variable as empty groups.
  [Grp, ng, gvals, errmsg] = gb_grouping (gvs, incMiss, incEmpty);
  if (! isempty (errmsg))
    error ("groupcounts: %s", errmsg);
  endif

  B = accumarray (Grp(! isnan (Grp)), 1, [ng, 1]);

  ## Optional outputs: the grouping identifiers and the group percentages.
  if (nargout > 1)
    if (numel (gvs) == 1)
      BG = gvals{1};
    else
      BG = gvals;
    endif
    varargout{1} = BG;
  endif
  if (nargout > 2)
    varargout{2} = 100 * B / sum (B);
  endif

endfunction

## Group rows by the grouping-variable values GVS (a cell array of column
## vectors), treating each variable's missing values as a single group value.
## Returns G (1..NGROUPS), NGROUPS, REPROWS (a representative row per group), and
## an errmsg body.  Groups are sorted by value with missing groups last; when
## INCMISS is false the rows with a missing grouping value are dropped.
## Self-contained: no table dependency.
function [G, ngroups, repRows, errmsg] = gc_group_rows (gvs, incMiss)
  errmsg = '';
  G = [];
  ngroups = 0;
  repRows = [];
  n = size (gvs{1}, 1);
  KEY = [];
  SORT = [];
  anyMiss = false (n, 1);
  for j = 1:numel (gvs)
    [p, m, e] = gc_col_proxy (gvs{j});
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
function [p, miss, errmsg] = gc_col_proxy (col)
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

## Validate an 'IncludedEdge' binning option VAL for CALLER, returning it
## lowercased as 'left' or 'right'.  Self-contained: no table dependency.
function e = check_included_edge (caller, val)
  if (isa (val, 'string') && isscalar (val))
    val = char (val);
  endif
  if (! (ischar (val) && isrow (val) ...
         && any (strcmpi (val, {'left', 'right'}))))
    error ("%s: 'IncludedEdge' must be 'left' or 'right'.", caller);
  endif
  e = lower (val);
endfunction

## Group rows by the grouping-variable values GVS (already binned when a
## GROUPBINS argument was given).  Returns G (1..NG, NaN excluded), NG, GVALS (a
## 1-by-K cell of typed level-value columns, one per group), and an errmsg body.
## When INCEMPTY is true the unused categories of a categorical or binned grouping
## variable contribute empty groups, from the full Cartesian product of levels.
function [G, ng, gvals, errmsg] = gb_grouping (gvs, incMiss, incEmpty)
  errmsg = '';
  G = []; ng = 0; gvals = {};
  K = numel (gvs);
  n = size (gvs{1}, 1);
  if (! incEmpty)
    [G, ng, repRows, errmsg] = gc_group_rows (gvs, incMiss);
    if (! isempty (errmsg))
      return;
    endif
    gvals = cell (1, K);
    for j = 1:K
      gvals{j} = gvs{j}(repRows, :);
    endfor
    return;
  endif
  idxAll = NaN (n, K);
  levVals = cell (1, K);
  sizes = zeros (1, K);
  for j = 1:K
    [idx, lv, errmsg] = gb_levels (gvs{j}, incMiss);
    if (! isempty (errmsg))
      return;
    endif
    idxAll(:,j) = idx;
    levVals{j} = lv;
    sizes(j) = size (lv, 1);
  endfor
  ng = prod (sizes);
  lvlOf = ones (ng, K);
  period = 1;
  for j = K:-1:1
    lvlOf(:,j) = mod (floor ((0:ng-1)' / period), sizes(j)) + 1;
    period = period * sizes(j);
  endfor
  valid = all (! isnan (idxAll), 2);
  lin = zeros (n, 1);
  period = 1;
  for j = K:-1:1
    col = idxAll(:,j);
    col(isnan (col)) = 1;
    lin = lin + (col - 1) * period;
    period = period * sizes(j);
  endfor
  G = NaN (n, 1);
  G(valid) = lin(valid) + 1;
  gvals = cell (1, K);
  for j = 1:K
    gvals{j} = levVals{j}(lvlOf(:,j), :);
  endfor
endfunction

## Build the level structure of one grouping variable COL for empty-group-aware
## grouping: IDX (n-by-1 level index, NaN excluded), LEVVALS (one representative
## value per level), and an errmsg body.  A categorical uses its full category
## order; a missing value forms one extra level, sorted last, when INCMISS.
function [idx, levVals, errmsg] = gb_levels (col, incMiss)
  idx = []; levVals = []; errmsg = '';
  n = size (col, 1);
  [p, miss, errmsg] = gc_col_proxy (col);
  if (! isempty (errmsg))
    return;
  endif
  if (isa (col, 'categorical'))
    cats = categories (col);
    L = numel (cats);
    idx = double (col)(:);
    levVals = categorical (cats(:), cats, 'Ordinal', isordinal (col));
  else
    idx = NaN (n, 1);
    keep = find (! miss);
    if (isempty (keep))
      levVals = col([], :);
      L = 0;
    else
      [~, ia, ic] = unique (p(keep,:), "rows");
      idx(keep) = ic;
      levVals = col(keep(ia), :);
      L = numel (ia);
    endif
  endif
  if (any (miss))
    if (incMiss)
      L = L + 1;
      idx(miss) = L;
      mrow = find (miss, 1);
      levVals = [levVals; col(mrow, :)];
    else
      idx(miss) = NaN;
    endif
  endif
endfunction

%!assert_equal (groupcounts ([1; 1; 2; 2; 1]), [3; 2])
%!assert_equal (groupcounts ([30; 10; 10; 20]), [2; 1; 1])
%!test
%! ## The second and third outputs are the group values and the percentages
%! [B, BG, BP] = groupcounts ([1; 1; 2; 2; 1]);
%! assert_equal (B, [3; 2]);
%! assert_equal (BG, [1; 2]);
%! assert_equal (BP, [60; 40]);
%!test
%! ## Groups follow the sorted unique order of the values
%! [B, BG, BP] = groupcounts ([1; 1; 2; 2; 3; 5; 3; 3; 1; 4]);
%! assert_equal (B, [3; 2; 3; 1; 1]);
%! assert_equal (BG, [1; 2; 3; 4; 5]);
%! assert_equal (BP, [30; 20; 30; 10; 10]);
%!test
%! ## A matrix groups by the unique combinations of its columns
%! [B, BG, BP] = groupcounts ([1 0; 1 0; 1 1; 2 1]);
%! assert_equal (B, [2; 1; 1]);
%! assert_equal (BG, {[1; 1; 2], [0; 1; 1]});
%! assert_equal (BP, [50; 25; 25]);
%!test
%! ## A cell array of vectors groups by the unique combinations, BG a cell array
%! [B, BG, BP] = groupcounts ({[1; 1; 2; 2; 1], {'b'; 'a'; 'a'; 'b'; 'b'}});
%! assert_equal (B, [1; 2; 1; 1]);
%! assert_equal (BG, {[1; 1; 2; 2], {'a'; 'b'; 'a'; 'b'}});
%! assert_equal (BP, [20; 40; 20; 20]);
%!test
%! ## A missing value forms its own group, sorted last
%! [B, BG, BP] = groupcounts ([1; 1; 2; NaN; 2]);
%! assert_equal (B, [2; 2; 1]);
%! assert_equal (BG, [1; 2; NaN]);
%! assert_equal (BP, [40; 40; 20]);
%!test
%! ## 'IncludeMissingGroups' false drops the rows with a missing grouping value
%! [B, BG] = groupcounts ([1; 1; 2; NaN; 2], 'IncludeMissingGroups', false);
%! assert_equal (B, [2; 2]);
%! assert_equal (BG, [1; 2]);
%!test
%! ## A categorical vector groups by category order, not alphabetically
%! c = categorical ({'medium'; 'low'; 'high'; 'low'}, ...
%!                  {'low', 'medium', 'high'}, 'Ordinal', true);
%! [B, BG] = groupcounts (c);
%! assert_equal (B, [2; 1; 1]);
%! assert_equal (cellstr (BG), {'low'; 'medium'; 'high'});

%!test
%! ## A GROUPBINS edge vector groups a numeric variable by bin interval
%! [B, BG, BP] = groupcounts ([1; 3; 5; 7; 9; 11], [0 6 12]);
%! assert_equal (B, [3; 3]);
%! assert_equal (iscategorical (BG), true);
%! assert_equal (cellstr (BG), {'[0, 6)'; '[6, 12]'});
%! assert_equal (BP, [50; 50]);
%!test
%! ## A GROUPBINS number of bins makes equal-width bins over the data range
%! [B, BG] = groupcounts ([1; 2; 3; 4], 3);
%! assert_equal (cellstr (BG), {'[1, 2)'; '[2, 3)'; '[3, 4]'});
%! assert_equal (B, [1; 1; 2]);
%!test
%! ## 'IncludeEmptyGroups' adds unused categorical categories with zero count
%! c = categorical ({'a'; 'a'; 'b'}, {'a', 'b', 'c'});
%! [B, BG] = groupcounts (c, 'IncludeEmptyGroups', true);
%! assert_equal (cellstr (BG), {'a'; 'b'; 'c'});
%! assert_equal (B, [2; 1; 0]);

%!error <Invalid call> groupcounts ()
%!error <groupcounts: invalid argument; expected a GROUPBINS binning scheme or a Name-Value option.> ...
%! groupcounts ([1; 2], 'bogus')
%!error <groupcounts: 'IncludedEdge' must be 'left' or 'right'.> ...
%! groupcounts ([1; 2], 2, 'IncludedEdge', 'mid')
%!error <groupcounts: binning grouping variable '1' by time unit 'month' is only supported for datetime variables.> ...
%! groupcounts ([1; 2], 'month')
%!error <groupcounts: each grouping variable must have the same number of elements.> ...
%! groupcounts ({[1; 2; 3], [1; 2]})
