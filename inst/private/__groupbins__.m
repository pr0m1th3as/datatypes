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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{tf} =} __groupbins__ (@qcode{'is_spec'}, @var{x})
## @deftypefnx {datatypes} {[@var{cols}, @var{names}, @var{errmsg}] =} __groupbins__ (@qcode{'bin'}, @var{cols}, @var{names}, @var{scheme}, @var{incedge}, @var{caller})
##
## Shared @qcode{'groupbins'} engine for the grouping methods and functions.
##
## @code{__groupbins__ (@qcode{'is_spec'}, @var{x})} returns true when @var{x} is
## a @qcode{'groupbins'} binning specification (used to tell a positional binning
## argument apart from a method argument).
##
## @code{__groupbins__ (@qcode{'bin'}, @var{cols}, @var{names}, @var{scheme},
## @var{incedge}, @var{caller})} bins each grouping-variable column in @var{cols}
## per @var{scheme} (a number of bins, an edge vector, a @code{duration} /
## @code{calendarDuration} bin width, or a datetime time-unit keyword), returning
## the binned columns (categoricals) and the MATLAB-style output variable names
## (@qcode{@var{unit}_@var{var}} for a time unit, @qcode{disc_@var{var}}
## otherwise; unbinned variables keep their name).  @var{errmsg} is a message
## body the caller emits under its own name (empty on success).
##
## This is an internal helper; do NOT call it directly.
##
## @end deftypefn

function varargout = __groupbins__ (op, varargin)
  switch (op)
    case 'is_spec'
      varargout{1} = gb_is_spec (varargin{1});
    case 'bin'
      [varargout{1}, varargout{2}, varargout{3}] = gb_bin (varargin{:});
    otherwise
      error ("__groupbins__: unknown operation '%s'.", op);
  endswitch
endfunction

## Contiguous calendar-period time-unit keywords (datetime only).  The component
## keywords ('monthofyear', 'dayofweek', ...) are recognised as specs but not
## yet implemented (they raise a clean "not yet supported" error when binning).
function kw = gb_period_units ()
  kw = {'second', 'minute', 'hour', 'day', 'week', 'month', 'quarter', ...
        'year', 'decade', 'century'};
endfunction

function kw = gb_keywords ()
  kw = [{'none'}, gb_period_units(), {'dayname', 'monthname', 'dayofweek', ...
        'dayofmonth', 'dayofyear', 'hourofday', 'weekofmonth', 'weekofyear', ...
        'monthofyear', 'quarterofyear', 'secondofminute', 'minuteofhour'}];
endfunction

function tf = gb_is_charrow (x)
  tf = (ischar (x) && isrow (x)) || (isa (x, 'string') && isscalar (x));
endfunction

function tf = gb_is_none (x)
  tf = gb_is_charrow (x) && strcmpi (char (x), 'none');
endfunction

## True if X is a single binning-scheme element.
function tf = gb_is_scheme_elem (x)
  tf = false;
  if ((isnumeric (x) || islogical (x)) && ! isempty (x))
    tf = true;
  elseif (isa (x, 'datetime') || isa (x, 'duration') ...
          || isa (x, 'calendarDuration'))
    tf = true;
  elseif (gb_is_charrow (x))
    tf = any (strcmpi (char (x), gb_keywords ()));
  endif
endfunction

## True if X is a binning specification: a single element or a cell of elements.
function tf = gb_is_spec (x)
  tf = gb_is_scheme_elem (x);
  if (! tf && iscell (x) && ! isempty (x))
    tf = all (cellfun (@gb_is_scheme_elem, x(:)'));
  endif
endfunction

## Normalise SCHEME into a 1-by-K per-variable cell.
function [schemes, errmsg] = gb_normalise_schemes (scheme, K, caller)
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

## Bin each grouping column; return updated columns and their output names.
function [cols, names, errmsg] = gb_bin (cols, names, scheme, incEdge, caller)
  errmsg = '';
  K = numel (cols);
  [schemes, errmsg] = gb_normalise_schemes (scheme, K, caller);
  if (! isempty (errmsg))
    return;
  endif
  for j = 1:K
    if (gb_is_none (schemes{j}))
      continue;                         # no binning for this variable
    endif
    [b, nm, errmsg] = gb_bin_col (cols{j}, schemes{j}, incEdge, names{j});
    if (! isempty (errmsg))
      return;
    endif
    cols{j} = b;
    names{j} = nm;
  endfor
endfunction

## Bin one grouping column COL per SCHEME; return the categorical, its output
## name, and an errmsg body (empty on success).
function [binned, newname, errmsg] = gb_bin_col (col, scheme, incEdge, varname)
  binned = [];
  newname = varname;
  errmsg = '';
  n = numel (col);

  ## Time-unit keyword scheme.
  if (gb_is_charrow (scheme))
    unit = lower (char (scheme));
    if (any (strcmp (unit, gb_period_units ())))
      if (! isa (col, 'datetime'))
        errmsg = sprintf (strcat ("binning grouping variable '%s' by time unit", ...
                          " '%s' is only supported for datetime variables."), ...
                          varname, unit);
        return;
      endif
      [idx, labs] = gb_period_bins (col, unit, incEdge);
      binned = gb_make_categorical (idx, labs, n);
      newname = [unit, '_', varname];
      return;
    endif
    errmsg = sprintf (strcat ("binning grouping variable '%s' by '%s' is not", ...
                      " yet supported."), varname, unit);
    return;
  endif

  ## Duration / calendarDuration bin-width scalar.
  if ((isa (scheme, 'duration') || isa (scheme, 'calendarDuration')) ...
      && isscalar (scheme))
    [idx, labs, errmsg] = gb_width_bins (col, scheme, incEdge, varname);
    if (! isempty (errmsg))
      return;
    endif
    binned = gb_make_categorical (idx, labs, n);
    newname = ['disc_', varname];
    return;
  endif

  ## Number of bins / edge vector.
  [idx, labs, errmsg] = gb_edge_bins (col, scheme, incEdge, varname);
  if (! isempty (errmsg))
    return;
  endif
  binned = gb_make_categorical (idx, labs, n);
  newname = ['disc_', varname];
endfunction

## Build a categorical from per-row bin indices IDX (NaN -> <undefined>) and the
## ordered category labels LABS.
function c = gb_make_categorical (idx, labs, n)
  rowLab = repmat ({''}, n, 1);
  ok = ! isnan (idx);
  rowLab(ok) = labs(idx(ok));
  c = categorical (rowLab, labs);
endfunction

## ---- Number-of-bins / edge-vector binning (numeric, datetime, duration) ----
function [idx, labs, errmsg] = gb_edge_bins (col, scheme, incEdge, varname)
  idx = [];  labs = {};  errmsg = '';

  if (isa (col, 'datetime'))
    proxy = gb_dt2dn (col)(:);  ctype = 'datetime';
  elseif (isa (col, 'duration'))
    proxy = days (col)(:);      ctype = 'duration';
  elseif (isnumeric (col) || islogical (col))
    proxy = double (col)(:);    ctype = 'numeric';
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
    edgesP = sort (gb_dt2dn (scheme)(:)');
  elseif (isa (scheme, 'duration'))
    if (! strcmp (ctype, 'duration'))
      errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be", ...
                        " of type '%s'."), varname, ctype);
      return;
    endif
    edgesP = sort (days (scheme(:))');
  else
    errmsg = sprintf ("invalid binning scheme for grouping variable '%s'.", ...
                      varname);
    return;
  endif

  if (numel (edgesP) < 2 || any (isnan (edgesP)) || any (diff (edgesP) <= 0))
    errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be at", ...
                      " least two finite, strictly increasing values."), varname);
    return;
  endif
  [idx, labs] = gb_assign_intervals (proxy, edgesP, incEdge, ...
                                     gb_edge_labels (edgesP, ctype, gb_dur_fmt (col, ctype)));
endfunction

## ---- Duration / calendarDuration bin-width binning ----
function [idx, labs, errmsg] = gb_width_bins (col, width, incEdge, varname)
  idx = [];  labs = {};  errmsg = '';

  if (isa (col, 'datetime'))
    proxy = gb_dt2dn (col)(:);  ctype = 'datetime';
  elseif (isa (col, 'duration'))
    proxy = days (col)(:);      ctype = 'duration';
  else
    errmsg = sprintf (strcat ("bin-width binning is not supported for grouping", ...
                      " variable '%s' of type '%s'."), varname, class (col));
    return;
  endif
  good = proxy(! isnan (proxy));
  if (isempty (good))
    idx = NaN (numel (proxy), 1);  labs = {'[0, 0)'};
    return;
  endif

  if (! isa (width, 'duration'))        # calendarDuration width
    errmsg = sprintf (strcat ("calendarDuration bin widths for grouping", ...
             " variable '%s' are not yet supported; use a time-unit keyword", ...
             " such as 'month'."), varname);
    return;
  endif
  w = days (width);
  if (! (w > 0))
    errmsg = sprintf ("bin width for grouping variable '%s' must be positive.", ...
                      varname);
    return;
  endif
  ## Anchor width bins to multiples of the width from 0.
  lo = floor (min (good) / w) * w;
  hi = max (good);
  ne = max (1, ceil ((hi - lo) / w + eps (hi)));
  edgesP = lo + (0:ne) * w;
  [idx, labs] = gb_assign_intervals (proxy, edgesP, incEdge, ...
                                     gb_edge_labels (edgesP, ctype, gb_dur_fmt (col, ctype)));
endfunction

## ---- Calendar-period time-unit binning (datetime) ----
function [idx, labs] = gb_period_bins (col, unit, incEdge)
  DV = datevec (col(:));
  n = size (DV, 1);
  nat = any (isnan (DV), 2);
  Y = DV(:,1);  Mo = DV(:,2);  D = DV(:,3);
  h = DV(:,4);  mi = DV(:,5);  s = DV(:,6);
  dn = gb_dt2dn (col)(:);

  ## Per-row integer period key and grid step (in key units).
  right = strcmpi (incEdge, 'right');
  switch (unit)
    case 'year'
      key = Y;                       step = 1;
    case 'month'
      key = Y .* 12 + (Mo - 1);      step = 1;
    case 'quarter'
      key = Y .* 4 + floor ((Mo - 1) / 3);  step = 1;
    case 'decade'
      key = floor (Y / 10);          step = 1;
    case 'century'
      key = floor (Y / 100);         step = 1;
    case 'day'
      key = floor (dn);              step = 1;
    case 'week'
      key = floor (dn) - (weekday (floor (dn)) - 1);  step = 7;  # Sunday-anchored
    case 'hour'
      key = floor (dn .* 24);        step = 1;
    case 'minute'
      key = floor (dn .* 1440);      step = 1;
    case 'second'
      key = round (dn .* 86400);     step = 1;
  endswitch

  ## 'right' edge: a value exactly on a period boundary joins the previous bin.
  if (right)
    onEdge = gb_on_boundary (unit, Y, Mo, D, h, mi, s, dn);
    key(onEdge & ! nat) = key(onEdge & ! nat) - step;
  endif

  key(nat) = NaN;
  good = key(! isnan (key));
  if (isempty (good))
    idx = NaN (n, 1);  labs = {gb_period_label(unit, 0)};
    return;
  endif
  kmin = min (good);  kmax = max (good);
  gridKeys = kmin:step:kmax;
  nb = numel (gridKeys);
  labs = cell (1, nb);
  for b = 1:nb
    labs{b} = gb_period_label (unit, gridKeys(b));
  endfor
  idx = NaN (n, 1);
  ok = ! isnan (key);
  idx(ok) = (key(ok) - kmin) / step + 1;
  idx(idx < 1 | idx > nb) = NaN;
endfunction

## Whether each row sits exactly on the start boundary of its period (used for
## 'IncludedEdge','right').
function tf = gb_on_boundary (unit, Y, Mo, D, h, mi, s, dn)
  switch (unit)
    case {'year', 'decade', 'century'}
      tf = (Mo == 1 & D == 1 & h == 0 & mi == 0 & s == 0);
    case {'month', 'quarter'}
      tf = (D == 1 & h == 0 & mi == 0 & s == 0);
    case {'day', 'week'}
      tf = (h == 0 & mi == 0 & s == 0);
    case 'hour'
      tf = (mi == 0 & s == 0);
    case 'minute'
      tf = (s == 0);
    otherwise
      tf = (s == round (s));
  endswitch
endfunction

## Category label for period key K of the given time UNIT.
function lab = gb_period_label (unit, k)
  mon = {'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'};
  switch (unit)
    case 'year'
      lab = sprintf ("%d", k);
    case 'month'
      lab = sprintf ("%s-%04d", mon{mod(k,12)+1}, floor (k / 12));
    case 'quarter'
      lab = sprintf ("Q%d %d", mod (k, 4) + 1, floor (k / 4));
    case 'decade'
      lab = sprintf ("[%d, %d)", k * 10, k * 10 + 10);
    case 'century'
      lab = sprintf ("[%d, %d)", k * 100, k * 100 + 100);
    case 'day'
      lab = gb_date_str (k);
    case 'week'
      lab = sprintf ("[%s, %s)", gb_date_str (k), gb_date_str (k + 7));
    case 'hour'
      lab = gb_datetime_str (k / 24);
    case 'minute'
      lab = gb_datetime_str (k / 1440);
    case 'second'
      lab = gb_datetime_str (k / 86400);
  endswitch
endfunction

## 'dd-mmm-yyyy' for an integer datenum.
function s = gb_date_str (dn)
  mon = {'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'};
  DV = datevec (dn);
  s = sprintf ("%02d-%s-%04d", DV(3), mon{DV(2)}, DV(1));
endfunction

## 'dd-mmm-yyyy HH:MM:SS' for a datenum.
function s = gb_datetime_str (dn)
  mon = {'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'};
  DV = datevec (dn);
  s = sprintf ("%02d-%s-%04d %02d:%02d:%02d", DV(3), mon{DV(2)}, DV(1), ...
               DV(4), DV(5), round (DV(6)));
endfunction


## ---- Shared interval assignment + labels ----
function [idx, labs] = gb_assign_intervals (proxy, edgesP, incEdge, estr)
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
endfunction

## Format proxy bin edges as label strings of type CTYPE.  DFMT is the duration
## column's display format (e.g. 'm'), so duration edges label in the same unit.
function s = gb_edge_labels (edgesP, ctype, dfmt)
  n = numel (edgesP);
  s = cell (1, n);
  switch (ctype)
    case 'datetime'
      ## Use a uniform format across the edges: show the time component on every
      ## edge (including midnight) when any edge carries one, else date only.
      DVe = datevec (edgesP(:));
      hasTime = any (DVe(:,4) != 0 | DVe(:,5) != 0 | round (DVe(:,6)) != 0);
      for i = 1:n
        if (hasTime)
          s{i} = gb_datetime_str (edgesP(i));
        else
          s{i} = gb_date_str (edgesP(i));
        endif
      endfor
    case 'duration'
      for i = 1:n
        ed = days (edgesP(i));
        if (! isempty (dfmt))
          ed.Format = dfmt;
        endif
        s{i} = char (ed);
      endfor
    otherwise
      for i = 1:n
        s{i} = gb_num_str (edgesP(i));
      endfor
  endswitch
endfunction

function s = gb_num_str (v)
  if (isfinite (v) && v == fix (v) && abs (v) < 1e15)
    s = sprintf ("%d", v);
  else
    s = num2str (v);
  endif
endfunction

## The duration column's display format (for duration edge labels), else ''.
function f = gb_dur_fmt (col, ctype)
  f = '';
  if (strcmp (ctype, 'duration'))
    f = col.Format;
  endif
endfunction

## datetime -> datenum, NaT -> NaN.
function dn = gb_dt2dn (v)
  sz = size (v);
  DV = datevec (v);
  nat = any (isnan (DV), 2);
  DV(nat,:) = 0;
  DV(nat,2:3) = 1;
  dn = datenum (DV);
  dn(nat) = NaN;
  dn = reshape (dn, sz);
endfunction
