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
      if (is_groupbins_spec (a))
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
    [gvs, errmsg] = bin_groupvars (gvs, names, groupbins, incEdge, ...
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

## Recognised 'groupbins' time-unit keyword names (plus 'none').  Only 'none' is
## currently honoured; the rest are detected so a time-unit binning argument is
## not mistaken for an option, then reported as not yet supported.
function kw = gb_keywords ()
  kw = {'none', 'second', 'minute', 'hour', 'day', 'week', 'month', ...
        'quarter', 'year', 'decade', 'century', 'dayname', 'monthname', ...
        'dayofweek', 'dayofmonth', 'dayofyear', 'hourofday', 'weekofmonth', ...
        'weekofyear', 'monthofyear', 'quarterofyear', 'secondofminute', ...
        'minuteofhour'};
endfunction

## True if X is a single 'groupbins' binning-scheme element.
function tf = gb_is_scheme_elem (x)
  tf = false;
  if ((isnumeric (x) || islogical (x)) && ! isempty (x))
    tf = true;
  elseif (isa (x, 'datetime') || isa (x, 'duration') ...
          || isa (x, 'calendarDuration'))
    tf = true;
  elseif ((ischar (x) && isrow (x)) || (isa (x, 'string') && isscalar (x)))
    tf = any (strcmpi (char (x), gb_keywords ()));
  endif
endfunction

## True if X is a 'groupbins' binning specification: a single scheme element or a
## cell array whose every element is one.
function tf = is_groupbins_spec (x)
  tf = gb_is_scheme_elem (x);
  if (! tf && iscell (x) && ! isempty (x))
    tf = all (cellfun (@gb_is_scheme_elem, x(:)'));
  endif
endfunction

## Normalise a 'groupbins' SCHEME into a 1-by-K cell of per-variable schemes.
## Returns an errmsg body emitted by the caller.
function [schemes, errmsg] = gb_normalise_schemes (scheme, K)
  errmsg = '';
  schemes = {};
  if (iscell (scheme))
    if (numel (scheme) != K)
      errmsg = sprintf (strcat ("GROUPBINS as a cell array must hold one", ...
                                " binning scheme per grouping variable (%d)."), ...
                        K);
      return;
    endif
    schemes = scheme(:)';
  else
    schemes = repmat ({scheme}, 1, K);
  endif
endfunction

## Bin the grouping-variable columns COLS using the 'groupbins' SCHEME and the
## IncludedEdge INCEDGE.  NAMES holds the variable names for error messages.
## Returns the updated COLS and an errmsg body emitted by the caller.
function [cols, errmsg] = bin_groupvars (cols, names, scheme, incEdge, caller)
  errmsg = '';
  K = numel (cols);
  [schemes, errmsg] = gb_normalise_schemes (scheme, K);
  if (! isempty (errmsg))
    return;
  endif
  for j = 1:K
    sj = schemes{j};
    if (((ischar (sj) && isrow (sj)) || (isa (sj, 'string') && isscalar (sj))) ...
        && strcmpi (char (sj), 'none'))
      continue;
    endif
    [b, errmsg] = bin_group_col (cols{j}, sj, incEdge, names{j});
    if (! isempty (errmsg))
      return;
    endif
    cols{j} = b;
  endfor
endfunction

## Map a datetime COL to a column vector of datenum proxy values (NaT -> NaN).
function dn = gb_datetime_to_datenum (v)
  DV = datevec (v);
  nat = any (isnan (DV), 2);
  DV(nat,:) = 0;
  DV(nat,2:3) = 1;
  dn = datenum (DV);
  dn(nat) = NaN;
  dn = dn(:);
endfunction

## Bin one grouping-variable column COL into a categorical of bin interval
## labels, using the binning SCHEME and the IncludedEdge INCEDGE.  VARNAME names
## the variable for error messages.  Time-unit and bin-width schemes are not yet
## supported.  Returns an errmsg body emitted by the caller.
function [binned, errmsg] = bin_group_col (col, scheme, incEdge, varname)
  binned = [];
  errmsg = '';
  if (((ischar (scheme) && isrow (scheme)) ...
       || (isa (scheme, 'string') && isscalar (scheme))) ...
      && ! strcmpi (char (scheme), 'none'))
    errmsg = sprintf (strcat ("binning grouping variable '%s' by time unit", ...
                              " '%s' is not yet supported; use bin edges or a", ...
                              " number of bins."), varname, char (scheme));
    return;
  endif
  if (isa (scheme, 'duration') || isa (scheme, 'calendarDuration'))
    errmsg = sprintf (strcat ("binning grouping variable '%s' by a bin width", ...
                              " is not yet supported; use bin edges or a", ...
                              " number of bins."), varname);
    return;
  endif
  if (isa (col, 'datetime'))
    proxy = gb_datetime_to_datenum (col);
    ctype = 'datetime';
  elseif (isa (col, 'duration'))
    proxy = days (col)(:);
    ctype = 'duration';
  elseif (isnumeric (col) || islogical (col))
    proxy = double (col)(:);
    ctype = 'numeric';
  else
    errmsg = sprintf (strcat ("binning is not supported for grouping variable", ...
                              " '%s' of type '%s'."), varname, class (col));
    return;
  endif
  if (isscalar (scheme) && (isnumeric (scheme) || islogical (scheme)))
    nb = double (scheme);
    if (! (nb >= 1 && nb == fix (nb)))
      errmsg = sprintf (strcat ("the number of bins for grouping variable", ...
                                " '%s' must be a positive integer."), varname);
      return;
    endif
    good = proxy(! isnan (proxy));
    if (isempty (good))
      edgesP = [0, 1];
    elseif (min (good) == max (good))
      edgesP = [min(good), min(good) + 1];
    else
      edgesP = min (good) + (0:nb) * (max (good) - min (good)) / nb;
    endif
  elseif (isnumeric (scheme))
    if (! strcmp (ctype, 'numeric'))
      errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be", ...
                                " of type '%s'."), varname, ctype);
      return;
    endif
    edgesP = sort (double (scheme(:)'));
  elseif (isa (scheme, 'datetime'))
    if (! strcmp (ctype, 'datetime'))
      errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be", ...
                                " of type '%s'."), varname, ctype);
      return;
    endif
    edgesP = sort (gb_datetime_to_datenum (scheme)');
  else
    errmsg = sprintf ("invalid binning scheme for grouping variable '%s'.", ...
                      varname);
    return;
  endif
  if (numel (edgesP) < 2 || any (isnan (edgesP)) || any (diff (edgesP) <= 0))
    errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be at", ...
                              " least two finite, strictly increasing values."), ...
                      varname);
    return;
  endif
  nb = numel (edgesP) - 1;
  left = ! strcmpi (incEdge, 'right');
  idx = NaN (numel (proxy), 1);
  for k = 1:nb
    if (left)
      if (k < nb)
        in = proxy >= edgesP(k) & proxy < edgesP(k+1);
      else
        in = proxy >= edgesP(k) & proxy <= edgesP(k+1);
      endif
    else
      if (k == 1)
        in = proxy >= edgesP(k) & proxy <= edgesP(k+1);
      else
        in = proxy > edgesP(k) & proxy <= edgesP(k+1);
      endif
    endif
    idx(in) = k;
  endfor
  estr = bin_edge_labels (edgesP, ctype);
  labs = cell (1, nb);
  for k = 1:nb
    if (left)
      br = '[';  bl = ')';
      if (k == nb)
        bl = ']';
      endif
    else
      br = '(';  bl = ']';
      if (k == 1)
        br = '[';
      endif
    endif
    labs{k} = sprintf ("%s%s, %s%s", br, estr{k}, estr{k+1}, bl);
  endfor
  rowLab = repmat ({''}, numel (idx), 1);
  ok = ! isnan (idx);
  rowLab(ok) = labs(idx(ok));
  binned = categorical (rowLab, labs);
endfunction

## Format the bin edges EDGESP (numeric proxy values) as label strings of CTYPE.
function s = bin_edge_labels (edgesP, ctype)
  n = numel (edgesP);
  s = cell (1, n);
  switch (ctype)
    case 'datetime'
      dt = datetime (edgesP(:), 'ConvertFrom', 'datenum');
      for i = 1:n
        s{i} = char (dt(i));
      endfor
    case 'duration'
      for i = 1:n
        s{i} = char (days (edgesP(i)));
      endfor
    otherwise
      for i = 1:n
        s{i} = bin_num_str (edgesP(i));
      endfor
  endswitch
endfunction

## Format a numeric bin-edge value V for an interval label.
function s = bin_num_str (v)
  if (isfinite (v) && v == fix (v) && abs (v) < 1e15)
    s = sprintf ("%d", v);
  else
    s = num2str (v);
  endif
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
%!error <groupcounts: binning grouping variable '1' by time unit 'month' is not yet supported; use bin edges or a number of bins.> ...
%! groupcounts ([1; 2], 'month')
%!error <groupcounts: each grouping variable must have the same number of elements.> ...
%! groupcounts ({[1; 2; 3], [1; 2]})
