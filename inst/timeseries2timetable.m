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
## @deftypefn  {datatypes} {@var{TT} =} timeseries2timetable (@var{ts})
## @deftypefnx {datatypes} {@var{TT} =} timeseries2timetable (@var{ts1}, @dots{}, @var{tsN})
## @deftypefnx {datatypes} {@var{TT} =} timeseries2timetable (@var{tc})
##
## Convert time series to a timetable.
##
## @code{@var{TT} = timeseries2timetable (@var{ts})} returns a timetable with
## a variable for each series in the @code{timeseries} array @var{ts}, and
## @code{@var{TT} = timeseries2timetable (@var{ts1}, @dots{}, @var{tsN})} one
## for each of the series @var{ts1} to @var{tsN}, which must then each be a
## single series.  A @code{tscollection} @var{tc} may stand for any of them
## and gives a variable for each of its members, in order; MATLAB accepts
## @code{timeseries} alone.
##
## Every series must have samples, and all the same times, in the same
## units, with the same @qcode{TimeInfo.StartDate}; convert series on other
## times one by one and combine the timetables with @code{synchronize}.
##
## The row times are the time vector: a @code{duration} array for series
## without a start date, displayed in the units of the series (seconds and
## finer units in seconds, weeks in days), and a @code{datetime} array from
## the start date otherwise, in the format @qcode{TimeInfo.Format} gives,
## or @qcode{'dd-MMM-uuuu HH:mm:ss'} when it is empty.  MATLAB copies
## @qcode{TimeInfo.Format}, a @code{datestr} format, as it is and fails;
## here it is translated to the @code{datetime} format that writes the same
## text.
##
## Each variable is named after its series, @qcode{Data} for a series with
## no name, made unique with the suffixes @qcode{_1}, @qcode{_2}, @dots{} in
## order.  A name reserved in a timetable, @qcode{Properties}, @qcode{Time}
## or @qcode{Variables}, takes a suffix too, with a warning; MATLAB raises
## for the last two.  The data are laid with time along the first
## dimension, so data whose time runs along its last dimension is permuted;
## a series whose samples then need more than two dimensions is refused,
## since a timetable here holds two-dimensional variables only, where
## MATLAB gives a variable of more.
##
## @qcode{DataInfo.Units} becomes @qcode{VariableUnits}, and the
## interpolation method @qcode{VariableContinuity}: @qcode{'linear'} is
## @qcode{'continuous'} and @qcode{'zoh'} is @qcode{'step'}; a series with an
## interpolation function is refused.  @qcode{Properties.UserData} is the
## @qcode{UserData} of the first series that has any.
##
## The events of the series become the @qcode{Properties.Events} of the
## timetable: an @code{eventtable} of their times, with their names as
## @qcode{EventLabels} and, when any event has some, their @qcode{EventData}
## as a variable; an event held by several series appears once.  MATLAB drops
## the events, with a warning.  Quality codes are dropped, with a warning, as
## in MATLAB.
##
## @seealso{timeseries, tscollection, timetable, eventtable}
## @end deftypefn
function TT = timeseries2timetable (varargin)

  if (nargin < 1)
    error ("timeseries2timetable: invalid number of input arguments.");
  endif

  ## The series, one per variable
  list = {};
  for i = 1:nargin
    x = varargin{i};
    if (isa (x, 'timeseries'))
      if (nargin > 1 && ! isscalar (x))
        error (strcat ("timeseries2timetable: with several inputs, each", ...
                       " timeseries must be a single series."));
      endif
      for k = 1:numel (x)
        list{end+1} = x(k);
      endfor
    elseif (isa (x, 'tscollection'))
      members = gettimeseriesnames (x);
      for k = 1:numel (members)
        list{end+1} = subsref (x, struct ('type', '.', 'subs', members{k}));
      endfor
    else
      error (strcat ("timeseries2timetable: every input must be a", ...
                     " timeseries or a tscollection."));
    endif
  endfor
  if (isempty (list))
    error ("timeseries2timetable: there is no series to convert.");
  endif

  ## One time vector for all
  first = list{1};
  if (any (cellfun (@(ts) ts.Length == 0, list)))
    error ("timeseries2timetable: every series must have samples.");
  endif
  if (! all (cellfun (@(ts) ts.Length == first.Length, list)))
    error (strcat ("timeseries2timetable: every series must have the same", ...
                   " number of samples; convert each to a timetable and", ...
                   " combine them with synchronize."));
  endif
  ti = first.TimeInfo;
  for i = 2:numel (list)
    tj = list{i}.TimeInfo;
    if (! (isequal (list{i}.Time, first.Time) && strcmp (tj.Units, ti.Units)
           && strcmp (tj.StartDate, ti.StartDate)))
      error (strcat ("timeseries2timetable: every series must have the", ...
                     " same times, time units and start date; convert", ...
                     " each to a timetable and combine them with", ...
                     " synchronize."));
    endif
  endfor

  ## The variables
  nv = numel (list);
  names = cell (1, nv);
  values = cell (1, nv);
  units = cell (1, nv);
  continuity = cell (1, nv);
  userData = [];
  hasQuality = false;
  for i = 1:nv
    ts = list{i};
    names{i} = ts.Name;
    if (isempty (names{i}) || strcmp (names{i}, 'unnamed'))
      names{i} = 'Data';
    endif
    data = ts.Data;
    if (! ts.IsTimeFirst)
      data = permute (data, [ndims(data), 1:ndims(data)-1]);
    endif
    if (ndims (data) > 2)
      error (strcat ("timeseries2timetable: the samples of '%s' are not", ...
                     " rows, and a timetable holds two-dimensional", ...
                     " variables only."), names{i});
    endif
    values{i} = data;
    units{i} = ts.DataInfo.Units;
    switch (getinterpmethod (ts))
      case 'linear'
        continuity{i} = 'continuous';
      case 'zoh'
        continuity{i} = 'step';
      otherwise
        error (strcat ("timeseries2timetable: the interpolation function", ...
                       " of '%s' has no timetable continuity; set its", ...
                       " method to 'linear' or 'zoh'."), names{i});
    endswitch
    if (isempty (userData))
      userData = ts.UserData;
    endif
    hasQuality = hasQuality || ! isempty (ts.Quality);
  endfor
  reserved = {'Properties', 'Time', 'Variables'};
  unique = matlab.lang.makeUniqueStrings (names, reserved);
  for i = find (ismember (names, reserved))
    warning (strcat ("timeseries2timetable: a series named '%s' is", ...
                     " renamed '%s', since the name is reserved in a", ...
                     " timetable."), names{i}, unique{i});
  endfor
  if (hasQuality)
    warning (strcat ("timeseries2timetable: quality codes cannot be held", ...
                     " by a timetable and are dropped."));
  endif

  rowTimes = timesOf (first.Time, ti);
  TT = timetable (values{:}, 'RowTimes', rowTimes, 'VariableNames', unique);
  if (! all (cellfun (@isempty, units)))
    TT.Properties.VariableUnits = units;
  endif
  TT.Properties.VariableContinuity = continuity;
  TT.Properties.UserData = userData;

  ## The events of every series, each once
  labels = {};
  eventData = {};
  eventTimes = [];
  for i = 1:nv
    ts = list{i};
    for k = 1:numel (ts.Events)
      e = ts.Events(k);
      eUnits = e.Units;
      if (isempty (eUnits))
        eUnits = ti.Units;
      endif
      start = e.StartDate;
      if (isempty (start))
        start = ti.StartDate;
      endif
      eti = ti;
      eti.Units = eUnits;
      eti.StartDate = start;
      t = timesOf (e.Time, eti);
      seen = false;
      for j = 1:numel (labels)
        if (strcmp (labels{j}, e.Name) && eventTimes(j) == t
            && isequal (eventData{j}, e.EventData))
          seen = true;
          break;
        endif
      endfor
      if (! seen)
        labels{end+1,1} = e.Name;
        eventData{end+1,1} = e.EventData;
        if (isempty (eventTimes))
          eventTimes = t;
        else
          eventTimes = [eventTimes; t];
        endif
      endif
    endfor
  endfor
  if (! isempty (labels))
    eventTimes.Format = rowTimes.Format;
    ET = eventtable (eventTimes, 'EventLabels', labels);
    if (! all (cellfun (@isempty, eventData)))
      ET.EventData = eventData;
    endif
    TT.Properties.Events = ET;
  endif

endfunction

## The times T, in the units of the time metadata TI, as row times: a
## duration shown in those units, or a datetime from TI.StartDate.
function rt = timesOf (t, ti)
  switch (ti.Units)
    case 'weeks'
      rt = days (7 * t);
      fmt = 'd';
    case 'days'
      rt = days (t);
      fmt = 'd';
    case 'hours'
      rt = hours (t);
      fmt = 'h';
    case 'minutes'
      rt = minutes (t);
      fmt = 'm';
    case 'seconds'
      rt = seconds (t);
      fmt = 's';
    case 'milliseconds'
      rt = seconds (t / 1e3);
      fmt = 's';
    case 'microseconds'
      rt = seconds (t / 1e6);
      fmt = 's';
    otherwise
      rt = seconds (t / 1e9);
      fmt = 's';
  endswitch
  if (isempty (ti.StartDate))
    rt.Format = fmt;
    return;
  endif
  rt = datetime (datevec (ti.StartDate)) + rt;
  if (isempty (ti.Format))
    rt.Format = 'dd-MMM-uuuu HH:mm:ss';
  else
    rt.Format = datetimeFormat (ti.Format);
  endif
endfunction

## The datetime format that writes what the datestr format FMT writes.
function out = datetimeFormat (fmt)
  hasAmPm = ! isempty (regexp (fmt, 'AM|PM', 'once'));
  tokens = {'yyyy', 'uuuu'; 'yy', 'uu'; 'mmmm', 'MMMM'; 'mmm', 'MMM'; ...
            'mm', 'MM'; 'm', 'MMMMM'; 'dddd', 'eeee'; 'ddd', 'eee'; ...
            'dd', 'dd'; 'd', 'eeeee'; 'HH', 'HH'; 'MM', 'mm'; ...
            'SS', 'ss'; 'FFF', 'SSS'; 'AM', 'a'; 'PM', 'a'};
  if (hasAmPm)
    tokens{strcmp (tokens(:,1), 'HH'), 2} = 'hh';
  endif
  out = '';
  i = 1;
  while (i <= numel (fmt))
    done = false;
    for k = 1:rows (tokens)
      tok = tokens{k,1};
      if (strncmp (fmt(i:end), tok, numel (tok)))
        out = [out, tokens{k,2}];
        i += numel (tok);
        done = true;
        break;
      endif
    endfor
    if (done)
      continue;
    endif
    ## Any other letter is literal text to a datetime format
    if (isletter (fmt(i)))
      out = [out, "'", fmt(i), "'"];
    else
      out = [out, fmt(i)];
    endif
    i += 1;
  endwhile
endfunction

%!demo
%! ## Each series becomes a variable of a timetable.  Without a start date
%! ## the row times are durations, in the units of the series.
%!
%! temp = timeseries ([12.1; 14.5; 13.2], [0; 30; 60], 'Name', 'temp');
%! temp.TimeInfo.Units = 'minutes';
%! temp.DataInfo.Units = 'degC';
%! wind = timeseries ([3; 5; 4], [0; 30; 60], 'Name', 'wind');
%! wind.TimeInfo.Units = 'minutes';
%! TT = timeseries2timetable (temp, wind)
%! TT.Properties.VariableUnits

%!demo
%! ## A dated series gives `datetime` row times, and its events become the
%! ## timetable's event table; MATLAB drops the events.
%!
%! ts = timeseries ([3; 5; 4], {'01-Mar-2024', '02-Mar-2024', ...
%!                              '03-Mar-2024'}, 'Name', 'level');
%! ts = addevent (ts, 'inspection', 1);
%! TT = timeseries2timetable (ts);
%! TT.Properties.RowTimes
%! TT.Properties.Events

%!demo
%! ## A collection gives a variable per member.
%!
%! tc = tscollection ({timeseries([1; 2], [0; 1], 'Name', 'a'), ...
%!                     timeseries([3; 4], [0; 1], 'Name', 'b')});
%! timeseries2timetable (tc)

%!shared A, B, U, DA
%! A = timeseries ([1; 2; 3; 4], [0; 1; 2; 3], [1; 2; 3; 4], 'Name', 'alpha');
%! A.DataInfo.Units = 'm';
%! A.UserData = 7;
%! B = setinterpmethod (timeseries ([10, 11; 20, 21; 30, 31; 40, 41], ...
%!                                  [0; 1; 2; 3], 'Name', 'beta'), 'zoh');
%! U = timeseries ([5; 6; 7; 8], [0; 1; 2; 3]);
%! DA = timeseries ([1; 2; 3], {'01-Jan-2024 06:00:00', ...
%!                  '02-Jan-2024 06:00:00', '03-Jan-2024 06:00:00'}, ...
%!                  'Name', 'da');

## Test one series
%!test
%! warning ('off', 'all', 'local');
%! tt = timeseries2timetable (A);
%! assert_equal (class (tt), 'timetable');
%! assert_equal (size (tt), [4, 1]);
%! assert_equal (tt.Properties.VariableNames, {'alpha'});
%! assert_equal (class (tt.Properties.RowTimes), 'duration');
%! assert_equal (tt.Properties.RowTimes.Format, 's');
%! assert_equal (seconds (tt.Properties.RowTimes), [0; 1; 2; 3]);
%! assert_equal (tt.alpha, [1; 2; 3; 4]);
%! assert_equal (tt.Properties.VariableUnits, {'m'});
%! assert_equal (tt.Properties.VariableContinuity, {'continuous'});
%! assert_equal (tt.Properties.DimensionNames, {'Time', 'Variables'});
%! assert_equal (tt.Properties.UserData, 7);
%! assert_equal (tt.Properties.Description, '');
%!test
%! tt = timeseries2timetable (B);
%! assert_equal (size (tt.beta), [4, 2]);
%! assert_equal (tt.beta, [10, 11; 20, 21; 30, 31; 40, 41]);
%! assert_equal (tt.Properties.VariableContinuity, {'step'});
%!assert_equal (timeseries2timetable (U).Properties.VariableNames, {'Data'})
%!assert_equal (timeseries2timetable (timeseries ([1; 2], [0; 1], ...
%!              'Name', '')).Properties.VariableNames, {'Data'})
## Test several series, in order
%!test
%! warning ('off', 'all', 'local');
%! tt = timeseries2timetable (A, B);
%! assert_equal (size (tt), [4, 2]);
%! assert_equal (tt.Properties.VariableNames, {'alpha', 'beta'});
%! assert_equal (tt.Properties.VariableUnits, {'m', ''});
%! assert_equal (tt.Properties.VariableContinuity, {'continuous', 'step'});
%!test
%! warning ('off', 'all', 'local');
%! tt = timeseries2timetable ([A, B]);
%! assert_equal (tt.Properties.VariableNames, {'alpha', 'beta'});
## Test names made unique in order, case-sensitive, kept verbatim
%!assert_equal (timeseries2timetable (U, U).Properties.VariableNames, ...
%!              {'Data', 'Data_1'})
%!test
%! warning ('off', 'all', 'local');
%! tt = timeseries2timetable (A, A, A);
%! assert_equal (tt.Properties.VariableNames, {'alpha', 'alpha_1', 'alpha_2'});
%!test
%! warning ('off', 'all', 'local');
%! assert_equal (timeseries2timetable (A, U).Properties.VariableNames, ...
%!               {'alpha', 'Data'});
%!assert_equal (timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', ...
%!              'a'), timeseries ([3; 4], [0; 1], 'Name', 'a_1'), ...
%!              timeseries ([5; 6], [0; 1], 'Name', 'a')) ...
%!              .Properties.VariableNames, {'a', 'a_1', 'a_2'})
%!assert_equal (timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', ...
%!              'alpha'), timeseries ([3; 4], [0; 1], 'Name', 'Alpha')) ...
%!              .Properties.VariableNames, {'alpha', 'Alpha'})
%!assert_equal (timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', ...
%!              'a b-c')).Properties.VariableNames, {'a b-c'})
%!assert_equal (timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', ...
%!              'Data'), timeseries ([3; 4], [0; 1])) ...
%!              .Properties.VariableNames, {'Data', 'Data_1'})
## Test reserved names take a suffix; MATLAB raises for 'Time', 'Variables'
%!test
%! warning ('off', 'all', 'local');
%! tt = timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', 'Time'));
%! assert_equal (tt.Properties.VariableNames, {'Time_1'});
%! tt = timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', ...
%!                                        'Variables'));
%! assert_equal (tt.Properties.VariableNames, {'Variables_1'});
%! tt = timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', ...
%!                                        'Properties'), ...
%!                            timeseries ([1; 2], [0; 1], 'Name', ...
%!                                        'Properties_1'));
%! assert_equal (tt.Properties.VariableNames, {'Properties_2', 'Properties_1'});
%!warning <timeseries2timetable: a series named 'Time' is renamed 'Time_1', since the name is reserved in a timetable.> ...
%! timeseries2timetable (timeseries ([1; 2], [0; 1], 'Name', 'Time'));
## Test the row times in the units of the series
%!test
%! m = timeseries ([1; 2; 3], [0; 1.5; 3]);
%! m.TimeInfo.Units = 'minutes';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 'm');
%! assert_equal (minutes (tt.Properties.RowTimes), [0; 1.5; 3]);
%!test
%! m = timeseries ([1; 2; 3], [0; 1; 2]);
%! m.TimeInfo.Units = 'hours';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 'h');
%! assert_equal (hours (tt.Properties.RowTimes), [0; 1; 2]);
%!test
%! m = timeseries ([1; 2; 3], [0; 1; 2]);
%! m.TimeInfo.Units = 'days';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 'd');
%! assert_equal (days (tt.Properties.RowTimes), [0; 1; 2]);
%!test
%! m = timeseries ([1; 2; 3], [0; 1; 2]);
%! m.TimeInfo.Units = 'weeks';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 'd');
%! assert_equal (days (tt.Properties.RowTimes), [0; 7; 14]);
%!test
%! m = timeseries ([1; 2; 3], [0; 1; 2]);
%! m.TimeInfo.Units = 'milliseconds';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 's');
%! assert_equal (milliseconds (tt.Properties.RowTimes), [0; 1; 2]);
%!test
%! m = timeseries ([1; 2], [0; 1]);
%! m.TimeInfo.Units = 'microseconds';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 's');
%! assert_equal (seconds (tt.Properties.RowTimes), [0; 1e-6]);
%!assert_equal (seconds (timeseries2timetable (timeseries ([1; 2; 3], ...
%!              [0; 1; 1])).Properties.RowTimes), [0; 1; 1])
## Test dated row times
%!test
%! tt = timeseries2timetable (DA);
%! assert_equal (class (tt.Properties.RowTimes), 'datetime');
%! assert_equal (tt.Properties.RowTimes.Format, 'dd-MMM-uuuu HH:mm:ss');
%! assert_equal (cellstr (char (tt.Properties.RowTimes)), ...
%!               {'01-Jan-2024 06:00:00'; '02-Jan-2024 06:00:00'; ...
%!                '03-Jan-2024 06:00:00'});
%! assert_equal (tt.Properties.RowTimes.TimeZone, '');
%!test
%! m = DA;
%! m.TimeInfo.Units = 'hours';
%! tt = timeseries2timetable (m);
%! assert_equal (cellstr (char (tt.Properties.RowTimes)), ...
%!               {'01-Jan-2024 06:00:00'; '01-Jan-2024 07:00:00'; ...
%!                '01-Jan-2024 08:00:00'});
%!test
%! p = timeseries ([1; 2; 3], [0; 0.25; 0.5], 'Name', 'p');
%! p.TimeInfo.Units = 'days';
%! p.TimeInfo.StartDate = '15-Mar-2024 10:30:00';
%! tt = timeseries2timetable (p);
%! assert_equal (cellstr (datestr (tt.Properties.RowTimes, ...
%!                                 'yyyy-mm-dd HH:MM:SS')), ...
%!               {'2024-03-15 10:30:00'; '2024-03-15 16:30:00'; ...
%!                '2024-03-15 22:30:00'});
## Test the time format is translated; MATLAB fails on it
%!test
%! m = DA;
%! m.TimeInfo.Format = 'yyyy-mm-dd';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 'uuuu-MM-dd');
%! assert_equal (char (tt.Properties.RowTimes(1)), '2024-01-01');
%!test
%! m = DA;
%! m.TimeInfo.Format = 'dd/mm/yyyy HH:MM PM';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 'dd/MM/uuuu hh:mm a');
%! assert_equal (char (tt.Properties.RowTimes(1)), '01/01/2024 06:00 AM');
%!test
%! m = DA;
%! m.TimeInfo.Format = 'mmm dd, yyyy';
%! tt = timeseries2timetable (m);
%! assert_equal (tt.Properties.RowTimes.Format, 'MMM dd, uuuu');
%! assert_equal (char (tt.Properties.RowTimes(2)), 'Jan 02, 2024');
## Test data laid time first
%!test
%! tt = timeseries2timetable (timeseries (reshape (1:8, 2, 1, 4), ...
%!                                        [0; 1; 2; 3], 'Name', 'r3'));
%! assert_equal (tt.r3, [1, 2; 3, 4; 5, 6; 7, 8]);
%!test
%! tt = timeseries2timetable (timeseries ([1, 2, 3, 4], [0, 1, 2, 3], ...
%!                                        'Name', 'rv'));
%! assert_equal (tt.rv, [1; 2; 3; 4]);
%!assert_equal (timeseries2timetable (timeseries ([1, 2, 3], 5, 'Name', ...
%!              'row')).row, [1, 2, 3])
%!assert_equal (size (timeseries2timetable (timeseries (7, 2))), [1, 1])
## Test the class, complex values and NaN are kept
%!test
%! tt = timeseries2timetable (timeseries (int8 ([1; 2; 3]), [0; 1; 2], ...
%!                                        'Name', 'i'), ...
%!                            timeseries (logical ([1; 0; 1]), [0; 1; 2], ...
%!                                        'Name', 'l'));
%! assert_equal (class (tt.i), 'int8');
%! assert_equal (class (tt.l), 'logical');
%!assert_equal (timeseries2timetable (timeseries ([1; NaN; 3], [0; 1; 2], ...
%!              'Name', 'n')).n, [1; NaN; 3])
%!assert_equal (timeseries2timetable (timeseries (complex ([1; 2], [3; 4]), ...
%!              [0; 1], 'Name', 'c')).c, complex ([1; 2], [3; 4]))
## Test UserData is the first series' that has any
%!test
%! p = timeseries ([1; 2], [0; 1], 'Name', 'p');
%! q = timeseries ([3; 4], [0; 1], 'Name', 'q');
%! q.UserData = 8;
%! assert_equal (timeseries2timetable (p, q).Properties.UserData, 8);
%! p.UserData = 7;
%! assert_equal (timeseries2timetable (p, q).Properties.UserData, 7);
%!assert_equal (timeseries2timetable (U).Properties.VariableUnits, {})
## Test a collection gives a variable per member
%!test
%! warning ('off', 'all', 'local');
%! tt = timeseries2timetable (tscollection ({A, B}));
%! assert_equal (tt.Properties.VariableNames, {'alpha', 'beta'});
%! assert_equal (tt.beta, [10, 11; 20, 21; 30, 31; 40, 41]);
%!test
%! warning ('off', 'all', 'local');
%! tt = timeseries2timetable (tscollection ({A, B}), U);
%! assert_equal (tt.Properties.VariableNames, {'alpha', 'beta', 'Data'});
## Test events become the timetable's events; MATLAB drops them
%!test
%! warning ('off', 'all', 'local');
%! a = addevent (A, 'ev', 1.5);
%! tt = timeseries2timetable (a);
%! ET = tt.Properties.Events;
%! assert_equal (class (ET), 'eventtable');
%! assert_equal (seconds (ET.Properties.RowTimes), 1.5);
%! assert_equal (ET.EventLabels, {'ev'});
%!test
%! e = tsdata.event ('dev', 1);
%! e.EventData = 'note';
%! m = addevent (DA, e);
%! tt = timeseries2timetable (m);
%! ET = tt.Properties.Events;
%! assert_equal (char (ET.Properties.RowTimes), '01-Jan-2024 06:00:01');
%! assert_equal (ET.EventData, {'note'});
## Test an event several series hold appears once
%!test
%! a = addevent (U, 'ev', 1.5);
%! a.Name = 'a';
%! b = addevent (U, 'ev', 2.5);
%! b.Name = 'b';
%! tt = timeseries2timetable (tscollection ({a, b}));
%! assert_equal (tt.Properties.Events.EventLabels, {'ev'; 'ev'});
%! tt = timeseries2timetable (a, a);
%! assert_equal (numel (tt.Properties.Events.EventLabels), 1);
%!assert_equal (timeseries2timetable (U).Properties.Events, [])
## Test quality codes are dropped with a warning, as in MATLAB
%!warning <timeseries2timetable: quality codes cannot be held by a timetable and are dropped.> ...
%! timeseries2timetable (A);

%!error <timeseries2timetable: invalid number of input arguments.> ...
%! timeseries2timetable ()
%!error <timeseries2timetable: every input must be a timeseries or a tscollection.> ...
%! timeseries2timetable (5)
%!error <timeseries2timetable: every input must be a timeseries or a tscollection.> ...
%! timeseries2timetable (U, 'Foo', 1)
%!error <timeseries2timetable: with several inputs, each timeseries must be a single series.> ...
%! timeseries2timetable ([U, U], U)
%!error <timeseries2timetable: there is no series to convert.> ...
%! timeseries2timetable (tscollection ([0; 1]))
%!error <timeseries2timetable: every series must have samples.> ...
%! timeseries2timetable (timeseries ())
%!error <timeseries2timetable: every series must have the same number of samples; convert each to a timetable and combine them with synchronize.> ...
%! timeseries2timetable (U, timeseries ([1; 2; 3], [0; 1; 2]))
%!error <timeseries2timetable: every series must have the same times, time units and start date; convert each to a timetable and combine them with synchronize.> ...
%! timeseries2timetable (U, timeseries ([1; 2; 3; 4], [0; 1; 2; 4]))
%!error <timeseries2timetable: every series must have the same times, time units and start date; convert each to a timetable and combine them with synchronize.> ...
%! m = U; m.TimeInfo.Units = 'minutes'; ...
%! timeseries2timetable (U, m)
%!error <timeseries2timetable: every series must have the same times, time units and start date; convert each to a timetable and combine them with synchronize.> ...
%! timeseries2timetable (DA, timeseries ([7; 8; 9], [0; 1; 2]))
%!error <timeseries2timetable: the samples of 'd3' are not rows, and a timetable holds two-dimensional variables only.> ...
%! timeseries2timetable (timeseries (reshape (1:24, 2, 3, 4), ...
%!                                   [0; 1; 2; 3], 'Name', 'd3'))
%!error <timeseries2timetable: the interpolation function of 'fh' has no timetable continuity; set its method to 'linear' or 'zoh'.> ...
%! m = setinterpmethod (U, @(nt, ot, od) interp1 (ot, od, nt, 'nearest')); ...
%! m.Name = 'fh'; ...
%! timeseries2timetable (m)
