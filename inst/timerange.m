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

classdef timerange
  ## -*- texinfo -*-
  ## @deftp {datatypes} timerange
  ##
  ## Subscript into a timetable by a range of times.
  ##
  ## A utility class that selects the rows of a timetable whose times fall in
  ## an interval.  The interval is half-open by default, including the time it
  ## starts at and excluding the one it ends at, so that ranges laid end to
  ## end select each row exactly once.
  ##
  ## @seealso{withtol, timetable}
  ## @end deftp

  properties (SetAccess = private, Hidden)
    ## Lower bound, or -Inf for none
    first
    ## Upper bound, or Inf for none
    last
    ## One of 'open', 'closed', 'openleft', 'openright'
    intervalType
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    ## Custom display
    function disp (this)
      fprintf ("  timerange subscript: %s to %s, %s\n\n", ...
               boundstr (this.first), boundstr (this.last), ...
               this.intervalType);
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timerange} {@var{tr} =} timerange (@var{startTime}, @var{endTime})
    ## @deftypefnx {timerange} {@var{tr} =} timerange (@var{startTime}, @var{endTime}, @var{intervalType})
    ## @deftypefnx {timerange} {@var{tr} =} timerange (@var{t}, @var{unit})
    ##
    ## Create a time range subscript.
    ##
    ## @code{@var{tr} = timerange (@var{startTime}, @var{endTime})} creates a
    ## subscript selecting the rows of a timetable whose times are at or after
    ## @var{startTime} and strictly before @var{endTime}.  The bounds may be
    ## @code{datetime} or @code{duration} scalars, text that reads as one, or
    ## @code{-Inf} and @code{Inf} for no bound at that end; they may not both
    ## be infinite, there being nothing to say what kind of time is meant.
    ##
    ## @code{@var{tr} = timerange (@var{startTime}, @var{endTime},
    ## @var{intervalType})} says which bounds belong to the interval:
    ## @qcode{'openright'} (the default) includes the start alone,
    ## @qcode{'openleft'} the end alone, @qcode{'closed'} both and
    ## @qcode{'open'} neither.
    ##
    ## @code{@var{tr} = timerange (@var{t}, @var{unit})} covers the whole
    ## calendar period containing @var{t}, one of @qcode{'seconds'},
    ## @qcode{'minutes'}, @qcode{'hours'}, @qcode{'days'}, @qcode{'weeks'},
    ## @qcode{'months'}, @qcode{'quarters'} or @qcode{'years'}, in the
    ## singular or the plural.  @var{t} must be a @code{datetime}: a
    ## @code{duration} is elapsed time and sits on no calendar.
    ##
    ## @seealso{withtol, timetable}
    ## @end deftypefn
    function this = timerange (varargin)

      if (nargin < 2 || nargin > 3)
        print_usage ();
      endif
      first = varargin{1};
      second = varargin{2};

      ## The two-argument form is a unit whenever the second argument names
      ## one; anything else there is the far bound of the interval.
      if (nargin == 2 && isUnitName (second))
        if (! isdatetime (first))
          error (strcat ("timerange: T must be a datetime when a unit of", ...
                         " time is given; a duration sits on no calendar."));
        endif
        [this.first, this.last] = unitPeriod (first, second);
        this.intervalType = 'openright';
        return
      endif

      this.first = checkBound (first, 'START');
      this.last = checkBound (second, 'END');
      if (isnumeric (this.first) && isnumeric (this.last))
        error (strcat ("timerange: the bounds cannot both be infinite;", ...
                       " at least one must say what kind of time is", ...
                       " meant."));
      endif
      if (nargin < 3)
        this.intervalType = 'openright';
      else
        this.intervalType = checkIntervalType (varargin{3});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timerange} {@var{ix} =} rowIndices (@var{tr}, @var{rowTimes})
    ##
    ## Return the positions in @var{rowTimes} the range selects.
    ##
    ## The positions come back in ascending order, being the rows of the
    ## timetable in the order it holds them.
    ##
    ## @end deftypefn
    function ix = rowIndices (this, rowTimes)
      lo = matchBound (this.first, rowTimes);
      hi = matchBound (this.last, rowTimes);
      closedLeft = any (strcmp (this.intervalType, {'closed', 'openright'}));
      closedRight = any (strcmp (this.intervalType, {'closed', 'openleft'}));
      keep = true (numel (rowTimes), 1);
      if (! isnumeric (lo))
        if (closedLeft)
          keep = keep & rowTimes >= lo;
        else
          keep = keep & rowTimes > lo;
        endif
      endif
      if (! isnumeric (hi))
        if (closedRight)
          keep = keep & rowTimes <= hi;
        else
          keep = keep & rowTimes < hi;
        endif
      endif
      ix = find (keep);
    endfunction

  endmethods

endclassdef

## Whether NAME is one of the calendar units the one-unit form takes.
function tf = isUnitName (name)
  if (isa (name, 'string') && isscalar (name))
    name = char (name);
  endif
  tf = ischar (name) && isrow (name) && any (strcmpi (name, ...
       {'second', 'seconds', 'minute', 'minutes', 'hour', 'hours', ...
        'day', 'days', 'week', 'weeks', 'month', 'months', ...
        'quarter', 'quarters', 'year', 'years'}));
endfunction

## The period of one UNIT containing T, as its first instant and the first
## instant of the period after it.
function [lo, hi] = unitPeriod (t, unit)
  if (isa (unit, 'string'))
    unit = char (unit);
  endif
  unit = lower (unit);
  if (unit(end) == 's')
    unit = unit(1:end-1);
  endif
  lo = dateshift (t, 'start', unit);
  switch (unit)
    case 'second'
      hi = lo + seconds (1);
    case 'minute'
      hi = lo + minutes (1);
    case 'hour'
      hi = lo + hours (1);
    case 'day'
      hi = lo + caldays (1);
    case 'week'
      hi = lo + calweeks (1);
    case 'month'
      hi = lo + calmonths (1);
    case 'quarter'
      hi = lo + calquarters (1);
    case 'year'
      hi = lo + calyears (1);
  endswitch
endfunction

## Validate one bound: a datetime or duration scalar, text that reads as one,
## or an infinity standing for no bound at all.  Text is kept as text and
## read once the row times say which kind of time it must be.
function b = checkBound (b, which)
  if (isnumeric (b) && isscalar (b) && isinf (b))
    return
  endif
  if (isdatetime (b) || isduration (b))
    if (! isscalar (b))
      error ("timerange: %s must be a scalar.", which);
    endif
    return
  endif
  if (isa (b, 'string') && isscalar (b))
    b = char (b);
  endif
  if (ischar (b) && isrow (b))
    return
  endif
  error (strcat ("timerange: %s must be a datetime or duration scalar,", ...
                 " text naming one, or -Inf or Inf."), which);
endfunction

## Validate the interval type.
function t = checkIntervalType (t)
  if (isa (t, 'string') && isscalar (t))
    t = char (t);
  endif
  types = {'open', 'closed', 'openleft', 'openright'};
  if (! (ischar (t) && isrow (t) && any (strcmpi (t, types))))
    error (strcat ("timerange: INTERVALTYPE must be 'openright',", ...
                   " 'openleft', 'open' or 'closed'."));
  endif
  t = lower (t);
endfunction

## One bound as a value comparable with ROWTIMES: an infinity stays numeric
## and marks the bound absent, text is read as the row times' own type, and
## a time of the other kind cannot be compared at all.
function b = matchBound (b, rowTimes)
  if (isnumeric (b))
    return
  endif
  if (ischar (b))
    if (isdatetime (rowTimes))
      b = datetime (b);
      b.TimeZone = rowTimes.TimeZone;
    else
      b = duration (b);
    endif
    return
  endif
  if (! strcmp (class (b), class (rowTimes)))
    error (strcat ("timerange: a timetable with %s row times cannot be", ...
                   " subscripted with %s bounds."), ...
           class (rowTimes), class (b));
  endif
endfunction

## The bound as text, for the display.
function s = boundstr (b)
  if (isnumeric (b))
    s = sprintf ("%g", b);
  elseif (ischar (b))
    s = b;
  else
    s = cellstr (b){1};
  endif
endfunction

## Test the default interval includes its start and excludes its end
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (timerange (tv(2), tv(4)), tv), [2; 3]);

## Test each interval type
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (timerange (tv(2), tv(4), 'openright'), tv), [2;3]);
%! assert_equal (rowIndices (timerange (tv(2), tv(4), 'openleft'), tv), [3;4]);
%! assert_equal (rowIndices (timerange (tv(2), tv(4), 'closed'), tv), [2;3;4]);
%! assert_equal (rowIndices (timerange (tv(2), tv(4), 'open'), tv), 3);

## Test an infinite bound leaves that end open
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (timerange (-Inf, tv(3)), tv), [1; 2]);
%! assert_equal (rowIndices (timerange (tv(3), Inf), tv), [3; 4; 5; 6]);

## Test text bounds are read as the row times' own type
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! tr = timerange ('01-Jan-2024 01:00:00', '01-Jan-2024 03:00:00');
%! assert_equal (rowIndices (tr, tv), [2; 3]);

## Test a range on duration row times
%!test
%! dv = hours (0:5)';
%! assert_equal (rowIndices (timerange (dv(2), dv(4)), dv), [2; 3]);

## Test the unit form covers the period containing the time
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (timerange (tv(3), 'hour'), tv), 3);
%! assert_equal (rowIndices (timerange (tv(1), 'day'), tv), (1:6)');
%! assert_equal (rowIndices (timerange (tv(1), 'year'), tv), (1:6)');
%! assert_equal (rowIndices (timerange (tv(1), 'hours'), tv), 1);

## Test a reversed or distant range selects nothing
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (timerange (tv(4), tv(2)), tv), zeros (0, 1));
%! assert_equal (rowIndices (timerange (tv(1) - hours (9), ...
%!                                      tv(1) - hours (5)), tv), zeros (0, 1));

%!error <timerange: INTERVALTYPE must be 'openright', 'openleft', 'open' or 'closed'.> ...
%! timerange (datetime (2024, 1, 1), datetime (2024, 1, 2), 'halfopen');
%!error <timerange: the bounds cannot both be infinite; at least one must say what kind of time is meant.> ...
%! timerange (-Inf, Inf);
%!error <timerange: START must be a datetime or duration scalar, text naming one, or -Inf or Inf.> ...
%! timerange ({1}, datetime (2024, 1, 1));
%!error <timerange: END must be a scalar.> ...
%! timerange (datetime (2024, 1, 1), datetime (2024, 1, 1) + hours (0:2)');
%!error <timerange: T must be a datetime when a unit of time is given; a duration sits on no calendar.> ...
%! timerange (hours (1), 'day');
%!error <timerange: a timetable with duration row times cannot be subscripted with datetime bounds.> ...
%! rowIndices (timerange (datetime (2024, 1, 1), datetime (2024, 1, 2)), ...
%!             hours (0:2)');
