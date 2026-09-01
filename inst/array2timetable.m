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
## @deftypefn  {datatypes} {@var{tt} =} array2timetable (@var{A}, @qcode{'RowTimes'}, @var{rowTimes})
## @deftypefnx {datatypes} {@var{tt} =} array2timetable (@var{A}, @qcode{'TimeStep'}, @var{dt})
## @deftypefnx {datatypes} {@var{tt} =} array2timetable (@var{A}, @qcode{'SampleRate'}, @var{fs})
## @deftypefnx {datatypes} {@var{tt} =} array2timetable (@dots{}, @var{Name}, @var{Value})
##
## Convert an array to a timetable.
##
## @code{@var{tt} = array2timetable (@var{A}, @qcode{'RowTimes'},
## @var{rowTimes})} converts the 2-D array @var{A} to a timetable, each
## column becoming a variable and @var{rowTimes} labelling the rows.  The row
## times must be given, by one of @qcode{'RowTimes'}, @qcode{'TimeStep'} or
## @qcode{'SampleRate'}, since a timetable cannot be built without them.
##
## Variable names are taken from the name of @var{A} with the column number
## appended, so an array named @var{data} gives @qcode{'data1'},
## @qcode{'data2'} and so on; an unnamed array gives @qcode{'Var1'} onwards.
##
## The following Name-Value pairs are also accepted.
##
## @multitable @columnfractions 0.23 0.75
## @headitem @var{Name} @tab @var{Value}
##
## @item @qcode{'VariableNames'} @tab A cell array of character vectors or a
## string array defining the variable names, one per column of @var{A}.
##
## @item @qcode{'DimensionNames'} @tab A two-element cell array of character
## vectors or string array naming the rows and the variables.  By default the
## rows are named @qcode{'Time'}.
##
## @item @qcode{'StartTime'} @tab The time of the first row, for the
## @qcode{'TimeStep'} and @qcode{'SampleRate'} forms.
## @end multitable
##
## @seealso{table2timetable, timetable2table, timetable, array2table}
## @end deftypefn
function TT = array2timetable (A, varargin)

  if (nargin < 1)
    print_usage ();
  endif
  if (ndims (A) > 2)
    error ("array2timetable: input array must be a 2-D array.");
  endif

  ## Parse optional Name-Value paired arguments
  optNames = {'VariableNames', 'DimensionNames', 'RowTimes', 'TimeStep', ...
              'SampleRate', 'StartTime'};
  dfValues = {{}, {}, missing, missing, missing, missing};
  [varNames, dimNames, RowTimes, TimeStep, SampleRate, StartTime, args] = ...
                      parsePairedArguments (optNames, dfValues, varargin(:));
  if (! isempty (args))
    error ("array2timetable: unrecognized optional argument.");
  endif

  ## Handle variable names
  varN = size (A, 2);
  if (! isempty (varNames))
    if (numel (varNames) != varN)
      error (strcat ("array2timetable: 'VariableNames' must match the", ...
                     " columns in input array."));
    endif
    if (isa (varNames, 'string'))
      varNames = cellstr (varNames);
    endif
  else
    varName = inputname (1);
    if (isempty (varName))
      varName = 'Var';
    endif
    varNames = cell (1, varN);
    for ix = 1:varN
      varNames{ix} = sprintf ('%s%d', varName, ix);
    endfor
  endif

  ## Assemble the row time arguments the timetable constructor takes, which
  ## is the only place they are validated.
  timeArgs = {};
  if (! isa (RowTimes, 'missing'))
    timeArgs = [timeArgs, {'RowTimes', RowTimes}];
  endif
  if (! isa (TimeStep, 'missing'))
    timeArgs = [timeArgs, {'TimeStep', TimeStep}];
  endif
  if (! isa (SampleRate, 'missing'))
    timeArgs = [timeArgs, {'SampleRate', SampleRate}];
  endif
  if (! isa (StartTime, 'missing'))
    timeArgs = [timeArgs, {'StartTime', StartTime}];
  endif
  if (! isempty (dimNames))
    timeArgs = [timeArgs, {'DimensionNames', dimNames}];
  endif

  ## Build the timetable empty and fill it column by column.  The columns
  ## cannot be handed to the constructor positionally: a datetime array's
  ## first column would be read as the row times.
  TT = timetable ('Size', [size(A, 1), 0], 'VariableTypes', {}, ...
                  timeArgs{:});
  for ix = 1:varN
    TT.(varNames{ix}) = A(:,ix);
  endfor

endfunction

## Test conversion with explicit row times
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! TT = array2timetable ([(1:3)', (4:6)'], 'RowTimes', tv);
%! assert_equal (class (TT), 'timetable');
%! assert_equal (size (TT), [3, 2]);
%! assert_equal (TT.Properties.VariableNames, {'Var1', 'Var2'});
%! assert_equal (TT.Properties.DimensionNames, {'Time', 'Variables'});
%! assert_equal (TT.Properties.RowTimes, tv);

## Test variable names come from the name of the input array
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! data = [(1:3)', (4:6)'];
%! TT = array2timetable (data, 'RowTimes', tv);
%! assert_equal (TT.Properties.VariableNames, {'data1', 'data2'});

## Test a generated time step
%!test
%! TT = array2timetable ((1:3)', 'TimeStep', hours (1));
%! assert_equal (TT.Properties.RowTimes, hours (0:2)');
%! assert_equal (TT.Properties.DimensionNames{1}, 'Time');

## Test a generated sample rate
%!test
%! TT = array2timetable ((1:3)', 'SampleRate', 2);
%! assert_equal (TT.Properties.TimeStep, seconds (0.5));

## Test explicit variable and dimension names
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! TT = array2timetable ((1:3)', 'RowTimes', tv, 'VariableNames', {'z'}, ...
%!                       'DimensionNames', {'When', 'What'});
%! assert_equal (TT.Properties.VariableNames, {'z'});
%! assert_equal (TT.Properties.DimensionNames, {'When', 'What'});

## Test the columns keep their own type
%!test
%! TT = array2timetable (hours (0:2)' + hours ([0, 10]), ...
%!                       'TimeStep', hours (1));
%! assert_equal (class (TT.Var1), 'duration');

%!error <array2timetable: input array must be a 2-D array.> ...
%! array2timetable (ones (2, 2, 2), 'TimeStep', hours (1));
%!error <array2timetable: 'VariableNames' must match the columns in input array.> ...
%! array2timetable ((1:3)', 'TimeStep', hours (1), ...
%!                  'VariableNames', {'a', 'b'});
%!error <timetable: row times are required; give them as the first argument or with 'RowTimes', 'TimeStep' or 'SampleRate'.> ...
%! array2timetable ((1:3)');
