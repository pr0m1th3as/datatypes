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
## @deftypefnx {datatypes} {[@var{B}, @var{BG}, @var{BP}] =} groupcounts (@var{A})
## @deftypefnx {datatypes} {[@dots{}] =} groupcounts (@var{A}, @var{Name}, @var{Value})
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
## The behaviour can be modified with the @qcode{'IncludeMissingGroups'} (default
## @code{true}) and @qcode{'IncludeEmptyGroups'} (default @code{false})
## @var{Name}/@var{Value} pairs, as for the @code{table} method.  Binning the
## grouping variables (the @var{groupbins} argument) is not yet supported.
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

  ## Binning (the optional GROUPBINS argument) is not yet supported, so anything
  ## after A must be a recognised Name-Value option.
  optNames = {'IncludeMissingGroups', 'IncludeEmptyGroups'};
  if (! isempty (varargin))
    a = varargin{1};
    isOpt = ((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a))) ...
            && any (strcmpi (char (a), optNames));
    if (! isOpt)
      error (strcat ("groupcounts: binning (the GROUPBINS argument) is not", ...
                     " yet supported."));
    endif
  endif

  ## Parse Name-Value options.
  dfValues = {true, false};
  [incMiss, incEmpty] = parsePairedArguments (optNames, dfValues, varargin(:));
  if (! (isscalar (incMiss) && (islogical (incMiss) || isnumeric (incMiss))))
    error ("groupcounts: 'IncludeMissingGroups' must be a logical scalar.");
  endif
  incMiss = logical (incMiss);
  if (! (isscalar (incEmpty) && (islogical (incEmpty) || isnumeric (incEmpty))))
    error ("groupcounts: 'IncludeEmptyGroups' must be a logical scalar.");
  endif
  if (logical (incEmpty))
    error ("groupcounts: 'IncludeEmptyGroups' = true is not yet supported.");
  endif

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

  ## Group the rows, treating missing grouping values as their own groups.
  [Grp, ng, repRows, errmsg] = gc_group_rows (gvs, incMiss);
  if (! isempty (errmsg))
    error ("groupcounts: %s", errmsg);
  endif

  B = accumarray (Grp(! isnan (Grp)), 1, [ng, 1]);

  ## Optional outputs: the grouping identifiers and the group percentages.
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

%!error <Invalid call> groupcounts ()
%!error <groupcounts: 'IncludeEmptyGroups' = true is not yet supported.> ...
%! groupcounts ([1; 2], 'IncludeEmptyGroups', true)
%!error <groupcounts: binning \(the GROUPBINS argument\) is not yet supported.> ...
%! groupcounts ([1; 2], 3)
%!error <groupcounts: each grouping variable must have the same number of elements.> ...
%! groupcounts ({[1; 2; 3], [1; 2]})
