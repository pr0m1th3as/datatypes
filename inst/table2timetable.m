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
## @deftypefn  {datatypes} {@var{tt} =} table2timetable (@var{tbl})
## @deftypefnx {datatypes} {@var{tt} =} table2timetable (@var{tbl}, @qcode{'RowTimes'}, @var{rowTimes})
## @deftypefnx {datatypes} {@var{tt} =} table2timetable (@var{tbl}, @qcode{'TimeStep'}, @var{dt})
## @deftypefnx {datatypes} {@var{tt} =} table2timetable (@var{tbl}, @qcode{'SampleRate'}, @var{fs})
## @deftypefnx {datatypes} {@var{tt} =} table2timetable (@dots{}, @qcode{'StartTime'}, @var{t0})
##
## Convert a table to a timetable.
##
## @code{@var{tt} = table2timetable (@var{tbl})} converts the table @var{tbl}
## to a timetable, taking its row times from the @strong{first} variable that
## is a @code{datetime} or a @code{duration}.  That variable stops being a
## variable and becomes the row times, and the row dimension is named after
## it.  A table with no such variable cannot be converted this way.
##
## @code{@var{tt} = table2timetable (@var{tbl}, @qcode{'RowTimes'},
## @var{rowTimes})} says which times to use.  @var{rowTimes} may be a
## @code{datetime} or @code{duration} vector with one element per row, in
## which case every variable of @var{tbl} is kept and the row dimension is
## named @qcode{'Time'}; or it may name one of the variables, by name or by
## index, which is then taken as the row times exactly as above.
##
## @code{@var{tt} = table2timetable (@var{tbl}, @qcode{'TimeStep'},
## @var{dt})} and @code{@var{tt} = table2timetable (@var{tbl},
## @qcode{'SampleRate'}, @var{fs})} generate the row times instead, keeping
## every variable.  @qcode{'StartTime'} sets the time of the first row for
## either, and the row dimension is named @qcode{'Time'}.
##
## The row names of @var{tbl}, if it has any, are not carried over: a
## timetable labels its rows by time and by nothing else.
##
## @seealso{timetable2table, array2timetable, timetable, table}
## @end deftypefn
function TT = table2timetable (tbl, varargin)

  if (nargin < 1)
    print_usage ();
  endif
  if (! istable (tbl))
    error ("table2timetable: TBL must be a table.");
  endif

  ## Parse optional Name-Value paired arguments
  optNames = {'RowTimes', 'TimeStep', 'SampleRate', 'StartTime'};
  dfValues = {missing, missing, missing, missing};
  [RowTimes, TimeStep, SampleRate, StartTime, args] = ...
                      parsePairedArguments (optNames, dfValues, varargin(:));
  if (! isempty (args))
    error ("table2timetable: unrecognized optional argument.");
  endif
  given = [! isa(RowTimes, 'missing'), ! isa(TimeStep, 'missing'), ...
           ! isa(SampleRate, 'missing')];
  if (sum (given) > 1)
    error (strcat ("table2timetable: only one of 'RowTimes', 'TimeStep'", ...
                   " and 'SampleRate' may be given."));
  endif

  props = tbl.Properties;
  varNames = props.VariableNames;

  ## Decide where the row times come from, and which variable if any stops
  ## being one.  A vector of times takes no variable and leaves the row
  ## dimension at its default; a variable, whether named outright or found
  ## by type, gives the dimension its own name.
  ixTime = 0;
  dimName = 'Time';
  timeArgs = {};
  if (given(1))
    if (isdatetime (RowTimes) || isduration (RowTimes))
      timeArgs = {'RowTimes', RowTimes};
    else
      ixTime = resolveTimeVar (RowTimes, varNames);
    endif
  elseif (given(2))
    timeArgs = {'TimeStep', TimeStep};
  elseif (given(3))
    timeArgs = {'SampleRate', SampleRate};
  else
    for i = 1:numel (varNames)
      v = tbl.(varNames{i});
      if (isdatetime (v) || isduration (v))
        ixTime = i;
        break;
      endif
    endfor
    if (ixTime == 0)
      error (strcat ("table2timetable: TBL must contain a datetime or a", ...
                     " duration variable to use as row times, or the row", ...
                     " times must be given."));
    endif
  endif
  if (ixTime > 0)
    rt = tbl.(varNames{ixTime});
    if (! (isdatetime (rt) || isduration (rt)))
      error (strcat ("table2timetable: the variable '%s' is a %s; row", ...
                     " times must be a datetime or a duration."), ...
             varNames{ixTime}, class (rt));
    endif
    timeArgs = {'RowTimes', rt};
    dimName = varNames{ixTime};
    varNames(ixTime) = [];
  endif
  if (! isa (StartTime, 'missing'))
    timeArgs = [timeArgs, {'StartTime', StartTime}];
  endif

  ## Build the timetable empty and fill it by name.  The variables cannot be
  ## handed to the constructor positionally: a surviving datetime variable
  ## would be read as the row times.
  TT = timetable ('Size', [height(tbl), 0], 'VariableTypes', {}, ...
                  timeArgs{:});
  for i = 1:numel (varNames)
    TT.(varNames{i}) = tbl.(varNames{i});
  endfor
  TT.Properties.DimensionNames = {dimName, props.DimensionNames{2}};

  ## Carry the metadata of the variables that survived, and of the table.
  keep = true (1, numel (props.VariableNames));
  if (ixTime > 0)
    keep(ixTime) = false;
  endif
  if (! isempty (props.VariableDescriptions))
    TT.Properties.VariableDescriptions = props.VariableDescriptions(keep);
  endif
  if (! isempty (props.VariableUnits))
    TT.Properties.VariableUnits = props.VariableUnits(keep);
  endif
  if (! isempty (props.VariableContinuity))
    TT.Properties.VariableContinuity = props.VariableContinuity(keep);
  endif
  if (! isempty (props.Description))
    TT.Properties.Description = props.Description;
  endif
  if (! isempty (props.UserData))
    TT.Properties.UserData = props.UserData;
  endif
  ## A custom property describing the variables loses the entry of the one
  ## that became the row times, which is no longer a variable.
  TT = tabular.carryCustomProps (TT, tbl, find (keep));

endfunction

## The index of the variable named by REF, which may be its name or its
## position.  Only one variable may be named: the row times are one vector.
function ix = resolveTimeVar (ref, varNames)
  if (isnumeric (ref) && isscalar (ref) && ref == fix (ref))
    ix = ref;
    if (ix < 1 || ix > numel (varNames))
      error ("table2timetable: 'RowTimes' index out of bound.");
    endif
    return
  endif
  if (isa (ref, 'string') && isscalar (ref))
    ref = char (ref);
  endif
  if (ischar (ref) && isrow (ref))
    ref = {ref};
  endif
  if (! (iscellstr (ref) && isscalar (ref)))
    error (strcat ("table2timetable: 'RowTimes' must be a datetime or", ...
                   " duration vector, or the name or index of a single", ...
                   " variable."));
  endif
  ix = find (strcmp (varNames, ref{1}));
  if (isempty (ix))
    error ("table2timetable: no such variable in table: '%s'", ref{1});
  endif
endfunction

%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! T = table (tv, (1:3)', 'VariableNames', {'when', 'A'});
%! TT = table2timetable (T);
%! assert_equal (class (TT), 'timetable');
%! assert_equal (TT.Properties.VariableNames, {'A'});
%! assert_equal (TT.Properties.DimensionNames, {'when', 'Variables'});
%! assert_equal (TT.Properties.RowTimes, tv);
%! assert_equal (TT.Properties.TimeStep, hours (1));

## Test the first time variable is the one taken
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! T = table (tv, tv + hours (1), (1:3)', ...
%!            'VariableNames', {'first', 'second', 'A'});
%! TT = table2timetable (T);
%! assert_equal (TT.Properties.VariableNames, {'second', 'A'});
%! assert_equal (TT.Properties.DimensionNames{1}, 'first');

## Test a duration variable serves as row times
%!test
%! dv = hours (0:2)';
%! T = table (dv, (1:3)', 'VariableNames', {'elapsed', 'A'});
%! TT = table2timetable (T);
%! assert_equal (class (TT.Properties.RowTimes), 'duration');
%! assert_equal (TT.Properties.DimensionNames{1}, 'elapsed');

## Test an explicit vector keeps every variable
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! T = table ((1:3)', (4:6)');
%! TT = table2timetable (T, 'RowTimes', tv);
%! assert_equal (TT.Properties.VariableNames, {'Var1', 'Var2'});
%! assert_equal (TT.Properties.DimensionNames, {'Time', 'Variables'});

## Test a variable named by index becomes the row times
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! T = table (tv, tv + hours (1), (1:3)', ...
%!            'VariableNames', {'first', 'second', 'A'});
%! TT = table2timetable (T, 'RowTimes', 2);
%! assert_equal (TT.Properties.VariableNames, {'first', 'A'});
%! assert_equal (TT.Properties.DimensionNames{1}, 'second');

## Test a variable named outright becomes the row times
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! T = table (tv, tv + hours (1), (1:3)', ...
%!            'VariableNames', {'first', 'second', 'A'});
%! TT = table2timetable (T, 'RowTimes', 'second');
%! assert_equal (TT.Properties.VariableNames, {'first', 'A'});

## Test a generated time step keeps every variable
%!test
%! T = table ((1:3)', (4:6)');
%! TT = table2timetable (T, 'TimeStep', hours (1));
%! assert_equal (TT.Properties.RowTimes, hours (0:2)');
%! assert_equal (TT.Properties.VariableNames, {'Var1', 'Var2'});

## Test a generated sample rate
%!test
%! T = table ((1:3)', (4:6)');
%! TT = table2timetable (T, 'SampleRate', 2);
%! assert_equal (TT.Properties.TimeStep, seconds (0.5));

## Test row names are not carried over
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! T = table (tv, (1:3)', 'VariableNames', {'when', 'A'}, ...
%!            'RowNames', {'a', 'b', 'c'});
%! TT = table2timetable (T);
%! assert_equal (TT.Properties.DimensionNames{1}, 'when');
%! assert_equal (any (strcmp (properties (TT.Properties), 'RowNames')), false);

## Test the variable metadata of the survivors is carried over
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! T = table (tv, (1:3)', 'VariableNames', {'when', 'A'});
%! T.Properties.VariableUnits = {'', 'm'};
%! T.Properties.Description = 'a description';
%! TT = table2timetable (T);
%! assert_equal (TT.Properties.VariableUnits, {'m'});
%! assert_equal (TT.Properties.Description, 'a description');

%!error <table2timetable: TBL must be a table.> table2timetable ((1:3)');
%!error <table2timetable: TBL must contain a datetime or a duration variable to use as row times, or the row times must be given.> ...
%! table2timetable (table ((1:3)', (4:6)'));
%!error <table2timetable: only one of 'RowTimes', 'TimeStep' and 'SampleRate' may be given.> ...
%! table2timetable (table ((1:3)'), 'RowTimes', hours (0:2)', ...
%!                  'TimeStep', hours (1));
%!error <table2timetable: no such variable in table: 'nope'> ...
%! table2timetable (table ((1:3)'), 'RowTimes', 'nope');
%!error <table2timetable: 'RowTimes' index out of bound.> ...
%! table2timetable (table ((1:3)'), 'RowTimes', 7);
%!error <table2timetable: the variable 'Var1' is a double; row times must be a datetime or a duration.> ...
%! table2timetable (table ((1:3)'), 'RowTimes', 1);

## Test a custom property describing the table survives the conversion
%!test
%! T = table ((1:3)', 'VariableNames', {'A'});
%! T = addprop (T, {'p'}, {'table'});
%! T.Properties.CustomProperties.p = 7;
%! tv = datetime (2024, 1, 1) + hours ((0:2)');
%! TT = table2timetable (T, 'RowTimes', tv);
%! assert_equal (TT.Properties.CustomProperties.p, 7);

## Test a custom property describing the variables survives the conversion
%!test
%! T = table ((1:3)', (11:13)', 'VariableNames', {'A', 'B'});
%! T = addprop (T, {'q'}, {'variable'});
%! T.Properties.CustomProperties.q = {'m', 'kg'};
%! tv = datetime (2024, 1, 1) + hours ((0:2)');
%! TT = table2timetable (T, 'RowTimes', tv);
%! assert_equal (TT.Properties.CustomProperties.q, {'m', 'kg'});

## Test the variable that became the row times loses its entry
%!test
%! tv = datetime (2024, 1, 1) + hours ((0:2)');
%! T = table (tv, (1:3)', 'VariableNames', {'when', 'A'});
%! T = addprop (T, {'q'}, {'variable'});
%! T.Properties.CustomProperties.q = {'x', 'm'};
%! TT = table2timetable (T, 'RowTimes', 'when');
%! assert_equal (TT.Properties.CustomProperties.q, {'m'});
