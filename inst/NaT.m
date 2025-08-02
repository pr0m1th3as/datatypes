## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn  {Function} {} NaT
## @deftypefnx {Function} {@var{T} =} NaT (@var{n})
## @deftypefnx {Function} {@var{T} =} NaT (@var{sz1}, @dots{}, @var{szN})
## @deftypefnx {Function} {@var{T} =} NaT (@var{sz})
## @deftypefnx {Function} {@var{T} =} NaT (@dots{}, @qcode{'Format'}, @var{fmt})
## @deftypefnx {Function} {@var{T} =} NaT (@dots{}, @qcode{'TimeZone'}, @var{tz})
##
## “Not-a-Time”. Creates missing-valued datetime arrays.
##
## @code{@var{T} = NaT (@var{n})} creates an @math{NxN} datetime matrix with all
## values being Not-a-Time (@qcode{NaT}).  When called with no size input
## values, it returns a @qcode{NaT} datetime scalar.
##
## @code{@var{T} =} NaT (@var{sz1}, @dots{}, @var{szN}) returns a datetime array
## with @qcode{NaT} values sized according to the input arguments @var{sz1},
## @dots{}, @var{szN}.  Alternatively, individual input size arguments can be
## merged into a single size vector @var{sz}, as in the following syntax
## @code{@var{T} = NaT (@var{sz})}.
##
## @code{@var{T} = NaT (@dots{}, @qcode{'Format'}, @var{fmt})} returns a
## datetime array of @qcode{NaT} values with the specified display format.
##
## @code{@var{T} = NaT (@dots{}, @qcode{'TimeZone'}, @var{tz})} returns a
## datetime array of @qcode{NaT} values n the time zone specified by @var{tz}.
##
## @qcode{NaT} is the @qcode{datetime} equivalent of @qcode{NaN}.  It represents
## a missing or invalid value.  @qcode{NaT} values never compare equal to,
## greater than, or less than any value, including other @qcode{NaT}s.  Doing
## arithmetic with a @qcode{NaT} and any other value results in a @qcode{NaT}.
##
## @seealso{datetime}
## @end deftypefn
function T = NaT (varargin)

  ## Parse optional Name-Value paired arguments
  optNames = {'Format', 'TimeZone'};
  dfValues = {'default', ''};
  [Format, TimeZone, args] = pairedArgs (optNames, dfValues, varargin(:));
  ## Check optional Name-Value paired arguments
  if (! ((ischar (Format) && isvector (Format)) ||
         (isa (Format, "string") && isscalar (Format))))
    error (["NaT: 'Format' must be either a character vector or", ...
            " a string scalar."]);
  endif
  if (! ((ischar (TimeZone) && (isvector (TimeZone) || isempty (TimeZone))) ||
         (isa (TimeZone, "string") && isscalar (TimeZone))))
    error (["NaT: 'TimeZone' must be either a character vector or", ...
            " a string scalar."]);
  endif

  ## Parse and check SIZE arguments
  if (nargin == 0)
    sz = 1;
  elseif (nargin == 1)
    if (isscalar (args{1}) && args{1} >= 0 && args{1} == fix (args{1}))
      sz = [args{1}, args{1}];
    elseif (isrow (args{1}) && all (args{1} >= 0) ...
                            && all (args{1} == fix (args{1})))
      sz = args{1};
    else
      error (strcat (["NaT: N must be a scalar or a row vector"], ...
                     [" of non-negative integers."]));
    endif
  elseif (nargin > 1)
    posint = cellfun (@(x) (! isscalar (x) || x < 0 || x != fix (x)), args);
    if (any (posint))
      error ("NaT: dimensions must be non-negative integers.");
    endif
    sz = [args{:}];
  endif

  ## Construct datetime object with static method
  T = datetime (nan (sz), 'ConvertFrom', 'datenum', 'Format', Format, ...
                'TimeZone', TimeZone);

endfunction

%!assert (isscalar (NaT), true);
%!assert (isnat (NaT), true);
%!assert (size (NaT (3)), [3, 3]);
%!assert (size (NaT (2, 3, 4)), [2, 3, 4]);

%!error<NaT: 'Format' must be either a character vector or a string scalar.> ...
%! NaT (1, 'Format', 2);
%!error<NaT: 'TimeZone' must be either a character vector or a string scalar.> ...
%! NaT (1, 'TimeZone', 2);
