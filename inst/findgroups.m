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
## @deftypefn  {datatypes} {@var{G} =} findgroups (@var{A})
## @deftypefnx {datatypes} {@var{G} =} findgroups (@var{A1}, @dots{}, @var{AN})
## @deftypefnx {datatypes} {[@var{G}, @var{ID}] =} findgroups (@var{A})
## @deftypefnx {datatypes} {[@var{G}, @var{ID1}, @dots{}, @var{IDN}] =} findgroups (@var{A1}, @dots{}, @var{AN})
##
## Find groups defined by one or more grouping variables.
##
## @code{@var{G} = findgroups (@var{A})} returns @var{G}, a column vector of
## positive integer group numbers with one element per element of the grouping
## variable @var{A}.  The groups are the sorted unique values of @var{A}: if
## @var{N} groups are found, every integer between 1 and @var{N} labels a group.
## Elements of @var{A} that are missing (@code{NaN}, @code{NaT},
## @code{<missing>}, @code{''}, or @code{<undefined>}) are labelled @code{NaN}
## in @var{G}.  @var{A} can be a numeric, logical, @code{string}, @code{cellstr},
## @code{char}, @code{datetime}, @code{duration}, @code{calendarDuration}, or
## @code{categorical} vector.
##
## @code{@var{G} = findgroups (@var{A1}, @dots{}, @var{AN})} defines groups as
## the sorted unique combinations of values across the grouping variables
## @var{A1}, @dots{}, @var{AN}, which must all have the same number of elements.
##
## @code{[@var{G}, @var{ID}] = findgroups (@var{A})} also returns @var{ID}, the
## sorted unique values of @var{A} that identify each group.  With multiple
## grouping variables, @code{[@var{G}, @var{ID1}, @dots{}, @var{IDN}] =
## findgroups (@var{A1}, @dots{}, @var{AN})} returns one identifier vector per
## grouping variable.
##
## To group the variables of a @code{table}, call @code{findgroups (@var{T})},
## which dispatches to the @code{table} method and returns the group identifiers
## as a table.
##
## @seealso{splitapply, table}
## @end deftypefn
function [G, varargout] = findgroups (varargin)

  if (nargin < 1)
    print_usage ();
  endif

  ## Force each grouping variable to a column (char matrices keep their rows as
  ## observations); reject anything that is not a vector or a char matrix.
  vars = varargin;
  for k = 1:numel (vars)
    v = vars{k};
    if (ischar (v))
      if (ndims (v) > 2)
        error ("findgroups: grouping variables must be vectors.");
      endif
    elseif (isvector (v) || isempty (v))
      vars{k} = v(:);
    else
      error ("findgroups: grouping variables must be vectors.");
    endif
  endfor

  ## Build the combined proxy matrix and the overall missing-row mask.
  nvar = numel (vars);
  n = size (vars{1}, 1);
  P = [];
  miss = false (n, 1);
  for k = 1:nvar
    if (size (vars{k}, 1) != n)
      error ("findgroups: grouping variables must have the same number of elements.");
    endif
    [p, m, errmsg] = group_col_proxy (vars{k});
    if (! isempty (errmsg))
      error ("findgroups: %s", errmsg);
    endif
    P = [P, p];
    miss = miss | m;
  endfor

  ## Label the non-missing rows by sorted unique combination.
  G = NaN (n, 1);
  keep = find (! miss);
  repRows = [];
  if (! isempty (keep))
    [~, ia, ic] = unique (P(keep,:), "rows");
    G(keep) = ic;
    repRows = keep(ia);
  endif

  if (nargout > 1)
    if (nargout - 1 > nvar)
      print_usage ();
    endif
    varargout = cell (1, nargout - 1);
    for k = 1:(nargout - 1)
      varargout{k} = vars{k}(repRows,:);
    endfor
  endif

endfunction

## Build a single-column grouping proxy for one grouping variable COL: a numeric
## matrix P (one row per element) whose sort order matches COL's value order, so
## that 'unique (P, "rows")' recovers the sorted unique groups, together with a
## logical MISS mask flagging the elements that findgroups treats as missing
## (NaN/NaT/<missing>/''/<undefined>).  Returns an errmsg body (empty on success)
## emitted by the caller under its own name.  Self-contained: no table dependency.
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
    DV = datevec (col);                 # (numel)-by-6 in column-major order
    nat = any (isnan (DV), 2);
    DV(nat,:) = 0;
    DV(nat,2:3) = 1;                     # valid month/day placeholder
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

%!assert_equal (findgroups ([1; 3; 1; 2]), [1; 3; 1; 2])
%!assert_equal (findgroups ([30, 10, 10, 20]), [3; 1; 1; 2])
%!assert_equal (findgroups ({'b'; 'a'; 'b'; 'c'}), [2; 1; 2; 3])
%!assert_equal (findgroups (logical ([1; 0; 1; 0])), [2; 1; 2; 1])
%!test
%! ## Missing values map to NaN in G
%! assert_equal (findgroups ([1; NaN; 2; 1]), [1; NaN; 2; 1]);
%! assert_equal (findgroups ({'b'; ''; 'a'}), [2; NaN; 1]);
%!test
%! ## Categorical groups follow category order, not alphabetical order
%! c = categorical ({'medium'; 'low'; 'high'; 'low'}, ...
%!                  {'low', 'medium', 'high'}, 'Ordinal', true);
%! assert_equal (findgroups (c), [2; 1; 3; 1]);
%! [g, id] = findgroups (c);
%! assert_equal (cellstr (id), {'low'; 'medium'; 'high'});
%!test
%! ## Reordered nominal categoricals also group by category order
%! c = reordercats (categorical ({'b'; 'a'; 'c'; 'a'}), {'c', 'a', 'b'});
%! assert_equal (findgroups (c), [3; 2; 1; 2]);
%!test
%! ## Multiple grouping variables group by sorted unique combinations
%! assert_equal (findgroups ([1; 1; 2; 1], {'b'; 'a'; 'a'; 'b'}), [2; 1; 3; 2]);
%!test
%! ## Second output holds the sorted unique identifiers of each input
%! [G, ID] = findgroups ([30; 10; 10; 20]);
%! assert_equal (G, [3; 1; 1; 2]);
%! assert_equal (ID, [10; 20; 30]);
%!test
%! [G, ID1, ID2] = findgroups ([1; 1; 2], {'b'; 'a'; 'a'});
%! assert_equal (G, [2; 1; 3]);
%! assert_equal (ID1, [1; 1; 2]);
%! assert_equal (ID2, {'a'; 'b'; 'a'});

%!error <Invalid call> findgroups ()
%!error <grouping variables must be vectors> findgroups ([1, 2; 3, 4])
