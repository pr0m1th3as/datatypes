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
## @deftypefn  {datatypes} {@var{B} =} groupfilter (@var{A}, @var{groupvars}, @var{method})
## @deftypefnx {datatypes} {@var{B} =} groupfilter (@var{A}, @var{groupvars}, @var{groupbins}, @var{method})
## @deftypefnx {datatypes} {[@var{B}, @var{BG}] =} groupfilter (@dots{})
##
## Filter the rows of an array by a per-group condition.
##
## @code{@var{B} = groupfilter (@var{A}, @var{groupvars}, @var{method})} groups
## the rows of the array @var{A} by the grouping variables @var{groupvars},
## applies the filter function @var{method} to each group, and returns @var{B},
## the rows of @var{A} that satisfy the condition, in their original order.
## @var{groupvars} is a grouping vector with one element per row of @var{A}, or a
## cell array of such vectors.  Rows holding a missing value in a grouping
## variable form their own groups, to which @var{method} is applied like any
## other group.
##
## @var{method} is a function handle applied to each group's slice of every
## column of @var{A}.  It must return either a logical scalar, which keeps or
## drops the whole group, or a logical vector with one element per row of the
## group, which keeps or drops the individual rows.  A row is kept only when the
## condition holds for it across all columns of @var{A}.
##
## @code{[@var{B}, @var{BG}] = groupfilter (@dots{})} also returns @var{BG}, the
## grouping values of the kept rows.  When @var{groupvars} is a single grouping
## vector, @var{BG} is a column vector with one element per kept row; when several
## grouping variables are given, @var{BG} is a cell array with one element per
## grouping variable.
##
## The optional @var{groupbins} argument bins the grouping variables before
## grouping (a vector of bin edges or a positive integer number of bins, or a
## cell array with one scheme per grouping variable); see @code{groupsummary} for
## details.  The @qcode{'IncludedEdge'} Name-Value pair (@qcode{'left'} by
## default, or @qcode{'right'}) selects which bin edge is inclusive.
##
## To filter the rows of a @code{table}, call @code{groupfilter (@var{T},
## @var{groupvars}, @dots{})}, which dispatches to the @code{table} method and
## returns the result as a table.
##
## @seealso{groupsummary, groupcounts, findgroups, splitapply, table}
## @end deftypefn
function [B, varargout] = groupfilter (A, groupvars, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  ## Split off a trailing 'IncludedEdge' Name-Value option, then an optional
  ## GROUPBINS positional argument that precedes the filter function METHOD.
  args = varargin;
  optNames = {'IncludedEdge'};
  nvStart = numel (args) + 1;
  for k = 1:numel (args)
    a = args{k};
    if (((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a)))
        && any (strcmpi (char (a), optNames)))
      nvStart = k;
      break;
    endif
  endfor
  nvArgs = args(nvStart:end);
  args = args(1:nvStart-1);
  incEdge = parsePairedArguments (optNames, {'left'}, nvArgs(:));
  incEdge = check_included_edge ('groupfilter', incEdge);

  hasGB = false;
  groupbins = [];
  if (! isempty (args) && __groupbins__ ('is_spec', args{1}))
    hasGB = true;
    groupbins = args{1};
    args = args(2:end);
  endif
  if (isempty (args))
    print_usage ();
  endif
  method = args{1};
  if (! is_function_handle (method))
    error ("groupfilter: METHOD must be a function handle.");
  endif
  if (numel (args) > 1)
    error ("groupfilter: too many positional arguments.");
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
        error ("groupfilter: grouping variables must be vectors.");
      endif
    elseif (isvector (g) || isempty (g))
      gvs{j} = g(:);
    else
      error ("groupfilter: grouping variables must be vectors.");
    endif
    if (size (gvs{j}, 1) != n)
      error (strcat ("groupfilter: each grouping variable must have one", ...
                     " element per row of A."));
    endif
  endfor

  ## Bin the grouping variables when a GROUPBINS argument was given.
  if (hasGB)
    names = cell (1, numel (gvs));
    for j = 1:numel (gvs)
      names{j} = sprintf ("%d", j);
    endfor
    [gvs, ~, errmsg] = __groupbins__ ('bin', gvs, names, groupbins, incEdge, ...
                                   'groupfilter');
    if (! isempty (errmsg))
      error ("groupfilter: %s", errmsg);
    endif
  endif

  ## Group the rows, treating missing grouping values as their own groups so
  ## that every row belongs to exactly one group.
  [Grp, ng, errmsg] = gf_group_rows (gvs);
  if (! isempty (errmsg))
    error ("groupfilter: %s", errmsg);
  endif

  ## Build the row keep-mask by applying METHOD to each column of A.
  dataCols = cell (1, size (A, 2));
  for c = 1:size (A, 2)
    dataCols{c} = A(:,c);
  endfor
  [keep, errmsg] = gf_keep_mask (method, dataCols, Grp, ng);
  if (! isempty (errmsg))
    error ("groupfilter: %s", errmsg);
  endif

  B = A(keep,:);

  ## Optional output: the grouping values of the kept rows.
  if (nargout > 1)
    if (numel (gvs) == 1)
      BG = gvs{1}(keep,:);
    else
      BG = cell (1, numel (gvs));
      for j = 1:numel (gvs)
        BG{j} = gvs{j}(keep,:);
      endfor
    endif
    varargout{1} = BG;
  endif

endfunction

## Build the row keep-mask by applying the filter function METHOD to each data
## column's per-group slice.  DATACOLS is a cell array of column values; G the
## n-by-1 group numbers (1..NG), every row assigned to a group.  For each group
## METHOD receives the slice and must return a logical scalar (keep/drop the
## whole group) or a logical vector with one element per group row.  The
## per-column masks are combined with logical AND.  Returns KEEP (n-by-1
## logical) and an errmsg body emitted by the caller.  Self-contained.
function [keep, errmsg] = gf_keep_mask (method, dataCols, G, ng)
  errmsg = '';
  n = numel (G);
  keep = true (n, 1);
  for d = 1:numel (dataCols)
    col = dataCols{d};
    for g = 1:ng
      rows = find (G == g);
      if (isempty (rows))
        continue;
      endif
      r = method (col(rows,:));
      if (! (islogical (r) || isnumeric (r)))
        errmsg = "the filter function must return a logical result.";
        return;
      endif
      r = logical (r(:));
      if (isscalar (r))
        m = repmat (r, numel (rows), 1);
      elseif (numel (r) == numel (rows))
        m = r;
      else
        errmsg = strcat ("the filter function must return a logical scalar", ...
                         " or a logical vector with one element per group", ...
                         " row.");
        return;
      endif
      keep(rows) = keep(rows) & m;
    endfor
  endfor
endfunction

## Group rows by the grouping-variable values GVS (a cell array of column
## vectors), treating each variable's missing values as a single group value so
## that every row is assigned to a group.  Returns G (1..NGROUPS), NGROUPS, and
## an errmsg body emitted by the caller.  Self-contained: no table dependency.
function [G, ngroups, errmsg] = gf_group_rows (gvs)
  errmsg = '';
  G = [];
  ngroups = 0;
  n = size (gvs{1}, 1);
  KEY = [];
  for j = 1:numel (gvs)
    [p, m, e] = gf_col_proxy (gvs{j});
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
function [p, miss, errmsg] = gf_col_proxy (col)
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

%!test
%! ## A scalar-returning method keeps or drops whole groups
%! g = [1; 1; 1; 2; 2; 3];
%! x = [3; 1; 2; 9; 8; 5];
%! assert_equal (groupfilter (x, g, @(v) numel (v) > 2), [3; 1; 2]);
%!test
%! ## A vector-returning method keeps individual rows within each group
%! d = [1; 1; 1; 1; 2; 2; 2; 2];
%! t = [67; 65; 71; 55; 61; 79; 58; 78];
%! assert_equal (groupfilter (t, d, @(x) x == max (x)), [71; 79]);
%!test
%! ## Output rows keep their original order, not group order
%! g = [2; 1; 2; 1];
%! x = [1; 2; 3; 4];
%! assert_equal (groupfilter (x, g, @(v) numel (v) >= 1), [1; 2; 3; 4]);
%!test
%! ## The second output returns the grouping values of the kept rows
%! d = [1; 1; 1; 1; 2; 2; 2; 2];
%! t = [67; 65; 71; 55; 61; 79; 58; 78];
%! [B, BG] = groupfilter (t, d, @(x) x == max (x));
%! assert_equal (B, [71; 79]);
%! assert_equal (BG, [1; 2]);
%!test
%! ## A matrix groups by its grouping vector; a row is kept only when the
%! ## condition holds across all columns of A
%! A = [5 1; 1 9; 9 1; 2 9];
%! g = [1; 1; 2; 2];
%! assert_equal (groupfilter (A, g, @(v) v > 3), zeros (0, 2));
%!test
%! ## Rows with a missing grouping value form their own group and are filtered
%! g = [1; 1; NaN; 2; 2];
%! x = [10; 20; 30; 40; 50];
%! [B, BG] = groupfilter (x, g, @(v) mean (v) > 25);
%! assert_equal (B, [30; 40; 50]);
%! assert_equal (BG, [NaN; 2; 2]);
%!test
%! ## Several grouping vectors return BG as a cell array, one per variable
%! x = [3; 1; 9; 2; 8];
%! g1 = [1; 1; 2; 2; 1];
%! g2 = {'a'; 'a'; 'b'; 'b'; 'a'};
%! [B, BG] = groupfilter (x, {g1, g2}, @(v) v == max (v));
%! assert_equal (B, [9; 8]);
%! assert_equal (BG, {[2; 1], {'b'; 'a'}});
%!test
%! ## A categorical grouping vector groups by category
%! c = categorical ({'a'; 'b'; 'a'; 'b'; 'a'});
%! x = [1; 5; 2; 6; 9];
%! [B, BG] = groupfilter (x, c, @(v) v == max (v));
%! assert_equal (B, [6; 9]);
%! assert_equal (cellstr (BG), {'b'; 'a'});

%!test
%! ## A GROUPBINS edge vector filters within bins of a numeric grouping variable
%! x = (1:6)';
%! g = [1; 3; 5; 7; 9; 11];
%! B = groupfilter (x, g, [0 6 12], @(v) numel (v) > 2);
%! assert_equal (B, (1:6)');
%! B2 = groupfilter (x, g, [0 6 12], @(v) mean (v) > 4, 'IncludedEdge', 'right');
%! assert_equal (B2, [4; 5; 6]);

%!error <Invalid call> groupfilter ([1; 2], [1; 2])
%!error <groupfilter: METHOD must be a function handle.> ...
%! groupfilter ([1; 2], [1; 2], 'sum')
%!error <groupfilter: binning grouping variable '1' by time unit 'month' is only supported for datetime variables.> ...
%! groupfilter ([1; 2], [1; 2], 'month', @(x) x > 0)
%!error <groupfilter: too many positional arguments.> ...
%! groupfilter ([1; 2], [1; 2], @(x) x > 0, 5)
%!error <groupfilter: each grouping variable must have one element per row of A.> ...
%! groupfilter ([1; 2; 3], [1; 2], @(x) x > 0)
%!error <groupfilter: the filter function must return a logical scalar or a logical vector with one element per group row.> ...
%! groupfilter ([1; 1], [1; 1], @(x) [true; true; true])
