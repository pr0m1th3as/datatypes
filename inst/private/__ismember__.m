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
## @deftypefn  {private} {@var{TF} =} __ismember__ (@var{a}, @var{s})
## @deftypefnx {private} {@var{TF} =} __ismember__ (@var{a}, @var{s}, @qcode{'rows'})
## @deftypefnx {private} {[@var{TF}, @var{index}] =} __ismember__ (@dots{})
## @deftypefnx {private} {[@var{TF}, @var{index}] =} __ismember__ (@dots{}, @qcode{'legacy'})
##
## Find set memders of data.
##
## @end deftypefn

function [TF, index] = __ismember__ (a, s, varargin)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  by_rows = any (strcmpi ('rows', varargin));
  optlegacy = any (strcmpi ('legacy', varargin));

  ## lookup() uses absolute values for complex input so we handle the
  ## real and imaginary parts separately (bug #52437) unless 'rows', which
  ## does not use lookup() so it can be handled normally.
  if (! by_rows && (iscomplex (a) || iscomplex (s)))
    real_argout = cell (nargout, 1);
    imag_argout = cell (nargout, 1);
    [real_argout{:}] = ismember (real (a), real (s), varargin{:});
    [imag_argout{:}] = ismember (imag (a), imag (s), varargin{:});
    TF = real_argout{1} & imag_argout{1};
    if (nargout > 1)
      index = zeros (size (real_argout{2}));
      if (optlegacy)
        index(TF) = min (real_argout{2}(TF), imag_argout{2}(TF));
      else
        index(TF) = max (real_argout{2}(TF), imag_argout{2}(TF));
      endif
    endif
    return;
  endif

  ## lookup() does not handle logical values
  if (islogical (a))
    a = uint8 (a);
  endif
  if (islogical (s))
    s = uint8 (s);
  endif

  ## Matlab-compatible behavior (R2016b).  See bug #51187.
  if (ischar (a) && rows (a) == 1 && iscell (s))
    a = {a};
  endif

  ## Another Matlab-compatible behavior.  See bug #53924.
  if (isnumeric (a) && ischar (s))
    s = double (s);
  elseif (ischar (a) && isnumeric (s))
    a = double (a);
  endif

  if (! isempty (varargin))
    if (! cellfun ('ischar', varargin));
      error ("ismember: all options must be strings");
    elseif (! all (strcmpi (varargin, 'rows') | strcmpi (varargin, 'legacy')))
      error ('ismember: only "rows" and "legacy" are valid options');
    endif
  endif

  if (! by_rows)
    s = s(:);
    ## Check sort status, because we expect the array will often be sorted.
    if (issorted (s))
      is = [];
    else
      [s, is] = sort (s);
    endif

    ## Remove NaNs from table because lookup can't handle them
    if (isreal (s) && ! isempty (s) && isnan (s(end)))
      s = s(1:(end - sum (isnan (s))));
    endif

    if (nargout > 1)
      if (!optlegacy)
        s = s(end : -1: 1);
        if (! (isempty (s) || isempty (a)))
          if (isempty (is))
            is = [numel(s) : -1 : 1];
          else
            is = is(1 : numel (s))(end : -1 : 1);
          endif
        endif
      endif
      index = lookup (s, a, "m");
      TF = logical (index);
      if (! isempty (is))
        index(TF) = is(index(TF));
      endif
    else
      TF = lookup (s, a, "b");
    endif

  else  # "rows" argument
    if (isempty (a) || isempty (s))
      TF = false (rows (a), 1);
      if (nargout > 1)
        index = zeros (rows (a), 1);
      endif
    else
      if (rows (s) == 1)
        TF = all (a == s, 2);
        if (nargout > 1)
          index = double (TF);
        endif
      else
        ## FIXME: lookup does not support "rows", so we just use unique.
        na = rows (a);
        if (optlegacy)
          [~, ii, jj] = unique ([a; s], 'rows', 'last');
          jj = ii(jj(1:na));
          TF = jj > na;
          if (nargout > 1)
            index = max (0, jj - na);
          endif
        else
          [~, ii, jj] = unique ([s; a], 'rows', 'first');
          nj = numel(jj) - (na-1);
          jj = ii(jj(nj:end));
          TF = jj < nj;
          if (nargout > 1)
            index = jj;
            index (jj>nj) = 0;
          endif
        endif
      endif
    endif
  endif

endfunction
