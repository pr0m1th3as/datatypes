## Copyright (C) 2024-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn  {datatypes} {@var{T} =} NaT (@var{n})
## @deftypefnx {datatypes} {@var{T} =} NaT (@var{sz1}, @dots{}, @var{szN})
## @deftypefnx {datatypes} {@var{T} =} NaT (@var{sz})
## @deftypefnx {datatypes} {@var{T} =} NaT (@dots{}, @qcode{'Format'}, @var{fmt})
## @deftypefnx {datatypes} {@var{T} =} NaT (@dots{}, @qcode{'TimeZone'}, @var{tz})
##
## “Not-a-Time”. Creates missing-valued datetime arrays.
##
## @code{@var{T} = NaT (@var{n})} creates an @math{N*N} datetime matrix with all
## values being Not-a-Time (@qcode{NaT}).  When called with no size input
## values, it returns a @qcode{NaT} datetime scalar.
##
## @code{@var{T} = NaT (@var{sz1}, @dots{}, @var{szN})} returns a datetime array
## with @qcode{NaT} values sized according to the input arguments @var{sz1},
## @dots{}, @var{szN}.  Alternatively, individual input size arguments can be
## merged into a single size vector @var{sz}, as in the following syntax
## @code{@var{T} = NaT (@var{sz})}.
##
## @code{@var{T} = NaT (@dots{}, @qcode{'Format'}, @var{fmt})} returns a
## datetime array of @qcode{NaT} values with the specified display format.
##
## @code{@var{T} = NaT (@dots{}, @qcode{'TimeZone'}, @var{tz})} returns a
## datetime array of @qcode{NaT} values in the time zone specified by @var{tz}.
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
  [Format, TimeZone, args] = parsePairedArguments (optNames, dfValues, ...
                                                   varargin(:));

  ## Check optional Name-Value paired arguments
  if (! ((ischar (Format) && isvector (Format)) ||
         (isa (Format, "string") && isscalar (Format))))
    error (strcat ("NaT: 'Format' must be either a character vector or", ...
                   " a string scalar."));
  endif
  if (! ((ischar (TimeZone) && (isvector (TimeZone) || isempty (TimeZone))) ||
         (isa (TimeZone, "string") && isscalar (TimeZone))))
    error (strcat ("NaT: 'TimeZone' must be either a character vector or", ...
                   " a string scalar."));
  endif

  ## Parse and check SIZE arguments.  Use the count of positional arguments
  ## (after the Name-Value pairs are stripped) so that, e.g.,
  ## NaT ('TimeZone', tz) is a scalar rather than falling through to the
  ## multi-dimension branch with an empty size.
  if (numel (args) == 0)
    sz = 1;
  elseif (numel (args) == 1)
    if (isscalar (args{1}) && args{1} >= 0 && args{1} == fix (args{1}))
      sz = [args{1}, args{1}];
    elseif (isrow (args{1}) && all (args{1} >= 0) ...
                            && all (args{1} == fix (args{1})))
      sz = args{1};
    else
      error (strcat ("NaT: N must be a scalar or a row vector", ...
                     " of non-negative integers."));
    endif
  else
    posint = cellfun (@(x) (! isscalar (x) || x < 0 || x != fix (x)), args);
    if (any (posint))
      error ("NaT: dimensions must be non-negative integers.");
    endif
    sz = [args{:}];
  endif

  ## Construct datetime object with static method.  The default 'Format' here is
  ## the same sentinel the datetime constructor falls back to on its own, so
  ## pass it on only when the caller actually named a format; that leaves a
  ## time zone which locks its own display format, 'UTCLeapSeconds', free to
  ## apply it.
  if (strcmp (Format, 'default'))
    T = datetime (nan (sz), 'ConvertFrom', 'datenum', 'TimeZone', TimeZone);
  else
    T = datetime (nan (sz), 'ConvertFrom', 'datenum', 'Format', Format, ...
                  'TimeZone', TimeZone);
  endif

endfunction

%!assert_equal (isscalar (NaT), true);
%!assert_equal (isnat (NaT), true);
%!assert_equal (size (NaT (3)), [3, 3]);
%!assert_equal (size (NaT (2, 3, 4)), [2, 3, 4]);

## A Name-Value pair with no size argument yields a scalar (not an empty array).
%!test
%! z = NaT ('TimeZone', 'America/New_York');
%! assert_equal (size (z), [1, 1]);
%! assert_equal (z.TimeZone, 'America/New_York');
%! assert_equal (isnat (z), true);
%!assert_equal (size (NaT ('Format', 'yyyy-MM-dd')), [1, 1]);
%!assert_equal (size (NaT (2, 3, 'TimeZone', 'UTC')), [2, 3]);

%!error<NaT: 'Format' must be either a character vector or a string scalar.> ...
%! NaT (1, 'Format', 2);
%!error<NaT: 'TimeZone' must be either a character vector or a string scalar.> ...
%! NaT (1, 'TimeZone', 2);
