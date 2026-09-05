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
## @deftypefn  {datatypes} {@var{tbl} =} timetable2table (@var{tt})
## @deftypefnx {datatypes} {@var{tbl} =} timetable2table (@var{tt}, @qcode{'ConvertRowTimes'}, @var{tf})
##
## Convert a timetable to a table.
##
## @code{@var{tbl} = timetable2table (@var{tt})} converts the timetable
## @var{tt} to a table whose @strong{first} variable is the row times, named
## after the row dimension they came from.  The dimension names of the result
## are the defaults, @qcode{'Row'} and @qcode{'Variables'}, a table labelling
## its rows by name rather than by time.
##
## @code{@var{tbl} = timetable2table (@var{tt}, @qcode{'ConvertRowTimes'},
## @var{tf})} keeps the row times as that first variable when @var{tf} is
## true, which is the default, and @strong{discards them} when it is false:
## the result has only the variables, and the times are gone rather than
## kept elsewhere.
##
## @seealso{table2timetable, array2timetable, timetable, table}
## @end deftypefn
function tbl = timetable2table (tt, varargin)

  if (nargin < 1)
    print_usage ();
  endif
  if (! istimetable (tt))
    error ("timetable2table: TT must be a timetable.");
  endif

  ## Parse optional Name-Value paired arguments
  [ConvertRowTimes, args] = parsePairedArguments ({'ConvertRowTimes'}, ...
                                                  {true}, varargin(:));
  if (! isempty (args))
    error ("timetable2table: unrecognized optional argument.");
  endif
  if (! (isscalar (ConvertRowTimes) && (islogical (ConvertRowTimes)
                                        || isnumeric (ConvertRowTimes))))
    error ("timetable2table: 'ConvertRowTimes' must be a logical scalar.");
  endif

  props = tt.Properties;
  varNames = props.VariableNames;
  vals = cell (1, numel (varNames));
  for i = 1:numel (varNames)
    vals{i} = tt.(varNames{i});
  endfor

  if (ConvertRowTimes)
    vals = [{props.RowTimes}, vals];
    varNames = [{props.DimensionNames{1}}, varNames];
  endif

  tbl = table (vals{:}, 'VariableNames', varNames);
  if (! isempty (props.VariableDescriptions))
    tbl.Properties.VariableDescriptions = shiftMeta ( ...
                     props.VariableDescriptions, ConvertRowTimes, '');
  endif
  if (! isempty (props.VariableUnits))
    tbl.Properties.VariableUnits = shiftMeta (props.VariableUnits, ...
                                              ConvertRowTimes, '');
  endif
  if (! isempty (props.VariableContinuity))
    tbl.Properties.VariableContinuity = shiftMeta ( ...
                     props.VariableContinuity, ConvertRowTimes, 'unset');
  endif
  if (! isempty (props.Description))
    tbl.Properties.Description = props.Description;
  endif
  if (! isempty (props.UserData))
    tbl.Properties.UserData = props.UserData;
  endif
  ## The row times become the first variable when they are converted, and a
  ## custom property describing the variables has no entry for them.
  ixVars = 1:numel (varNames);
  if (ConvertRowTimes)
    ixVars = [0, 1:(numel (varNames) - 1)];
  endif
  tbl = tabular.carryCustomProps (tbl, tt, ixVars);

endfunction

## Per-variable metadata with an entry prepended for the row times when they
## have become a variable of their own, since they carried none as times.
function meta = shiftMeta (meta, prepend, blank)
  if (prepend)
    meta = [{blank}, meta];
  endif
endfunction

## Test the row times become the first variable, named after the dimension
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! V = timetable (tv, (1:3)', (4:6)', 'VariableNames', {'A', 'B'});
%! T = timetable2table (V);
%! assert_equal (class (T), 'table');
%! assert_equal (size (T), [3, 3]);
%! assert_equal (T.Properties.VariableNames, {'tv', 'A', 'B'});
%! assert_equal (T.tv, tv);
%! assert_equal (T.Properties.DimensionNames, {'Row', 'Variables'});

## Test the result carries no row names
%!test
%! V = timetable (hours (0:2)', (1:3)', 'VariableNames', {'A'});
%! assert_equal (timetable2table (V).Properties.RowNames, {});

## Test 'ConvertRowTimes' false discards the times
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! V = timetable (tv, (1:3)', (4:6)', 'VariableNames', {'A', 'B'});
%! T = timetable2table (V, 'ConvertRowTimes', false);
%! assert_equal (size (T), [3, 2]);
%! assert_equal (T.Properties.VariableNames, {'A', 'B'});
%! assert_equal (T.Properties.DimensionNames, {'Row', 'Variables'});

## Test duration row times become a duration variable
%!test
%! V = timetable (hours (0:2)', (1:3)', 'VariableNames', {'A'});
%! T = timetable2table (V);
%! assert_equal (class (T.Time), 'duration');

## Test the variable metadata shifts along with the variables
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! V = timetable (tv, (1:3)', (4:6)', 'VariableNames', {'A', 'B'});
%! V.Properties.VariableUnits = {'m', 's'};
%! V.Properties.Description = 'a description';
%! T = timetable2table (V);
%! assert_equal (T.Properties.VariableUnits, {'', 'm', 's'});
%! assert_equal (T.Properties.Description, 'a description');

## Test the metadata is not shifted when the times are discarded
%!test
%! tv = datetime (2024, 1, 1) + hours (0:2)';
%! V = timetable (tv, (1:3)', (4:6)', 'VariableNames', {'A', 'B'});
%! V.Properties.VariableUnits = {'m', 's'};
%! T = timetable2table (V, 'ConvertRowTimes', false);
%! assert_equal (T.Properties.VariableUnits, {'m', 's'});

%!error <timetable2table: TT must be a timetable.> timetable2table (table ());
%!error <timetable2table: 'ConvertRowTimes' must be a logical scalar.> ...
%! timetable2table (timetable (hours (0:2)', (1:3)'), ...
%!                  'ConvertRowTimes', 'yes');

## Test a custom property describing the timetable survives the conversion
%!test
%! tv = datetime (2024, 1, 1) + hours ((0:2)');
%! TT = timetable (tv, (1:3)', 'VariableNames', {'A'});
%! TT = addprop (TT, {'p'}, {'table'});
%! TT.Properties.CustomProperties.p = 7;
%! assert_equal (timetable2table (TT).Properties.CustomProperties.p, 7);

## Test the row times become a variable with no entry of their own
%!test
%! tv = datetime (2024, 1, 1) + hours ((0:2)');
%! TT = timetable (tv, (1:3)', (11:13)', 'VariableNames', {'A', 'B'});
%! TT = addprop (TT, {'q'}, {'variable'});
%! TT.Properties.CustomProperties.q = {'m', 'kg'};
%! q = timetable2table (TT).Properties.CustomProperties.q;
%! assert_equal (numel (q), 3);
%! assert_equal (q(2:3), {'m', 'kg'});
