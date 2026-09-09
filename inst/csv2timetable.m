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
## @deftypefn  {datatypes} {@var{tt} =} csv2timetable (@var{filename})
## @deftypefnx {datatypes} {@var{tt} =} csv2timetable (@var{filename}, @var{Name}, @var{Value})
##
## Read a comma-separated-value (CSV) file into a timetable.
##
## @code{@var{tt} = csv2timetable (@var{filename})} reads the CSV file named by
## @var{filename} into a @code{timetable}.  @var{filename} may be a character
## vector, a cellstr, or a string scalar.
##
## A file written by @code{timetable2csv} carries the package's own header
## block, which names and types every variable and tags the leading column of
## row times with their type, their @code{TimeZone} where they have one, and
## their @code{Format}.  Such a file comes back as the timetable it was
## written from: the row times keep their type, zone and format exactly, and
## the row dimension keeps its name.
##
## Any other CSV file is read as @code{csv2table} reads it, and its
## @strong{first} @code{datetime} or @code{duration} variable becomes the row
## times, the row dimension taking that variable's name.  A file with no such
## variable cannot be read as a timetable.
##
## Every @var{Name}-@var{Value} option of @code{csv2table} is accepted and
## behaves as it does there, with two exceptions.  @qcode{'ReadRowNames'} and
## @qcode{'RowNamesColumn'} are refused: a timetable labels its rows by time
## and by nothing else, so a file whose rows are named is read with
## @code{csv2table}.
##
## @code{TimeStep} and @code{SampleRate} are not stored in the file and are
## worked out again from the row times, so a regular timetable comes back
## regular.  A CSV file carries no event table, so @qcode{'Events'} of the
## result is always empty even when the timetable that was written had one;
## use @code{timetable2ods} and @code{ods2timetable} to keep events.
##
## @seealso{timetable2csv, csv2table, ods2timetable, readtimetable, timetable}
## @end deftypefn

function tt = csv2timetable (filename, varargin)

  if (nargin < 1)
    error ("csv2timetable: too few input arguments.");
  endif
  if (! ((ischar (filename) && isvector (filename)) || iscellstr (filename) ...
         || isa (filename, 'string')))
    error (strcat ("csv2timetable: FILENAME must be a character vector,", ...
                   " cellstr, or string."));
  endif

  ## A timetable has no row names, so an option asking for them would be
  ## honoured by the reader and then discarded by the conversion, which is
  ## worse than saying so.
  for i = 1:2:numel (varargin)
    opt = varargin{i};
    if (isa (opt, 'string') && isscalar (opt))
      opt = char (opt);
    endif
    if (ischar (opt) && any (strcmpi (opt, {'ReadRowNames', 'RowNamesColumn'})))
      error (strcat ("csv2timetable: '%s' is not supported; a timetable", ...
                     " labels its rows by time.  Read a file whose rows", ...
                     " are named with 'csv2table'."), opt);
    endif
  endfor

  tbl = csv2table (filename, varargin{:});
  types = tbl.Properties.VariableTypes;
  if (! any (ismember (types, {'datetime', 'duration'})))
    error (strcat ("csv2timetable: the file has no datetime or duration", ...
                   " column to use as row times."));
  endif
  tt = table2timetable (tbl);

endfunction

%!demo
%! ## A timetable written by `timetable2csv` comes back whole: the row times
%! ## keep their type, their time zone and their display format, and the row
%! ## dimension keeps its name.
%!
%! t = datetime (2024, 3, 9, 22, 0, 0, 'TimeZone', 'America/New_York') ...
%!     + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'reading'});
%! TT.Properties.DimensionNames{1} = 'Stamp';
%! fname = [tempname(), '.csv'];
%! timetable2csv (TT, fname);
%! csv2timetable (fname)
%! delete (fname);

## Test a zoned datetime round-trips whole
%!test
%! t = datetime (2024, 3, 9, 22, 0, 0, 'TimeZone', 'America/New_York') ...
%!     + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname);
%!   out = csv2timetable (fname);
%!   assert_equal (class (out), 'timetable');
%!   assert_equal (out.Properties.RowTimes.TimeZone, 'America/New_York');
%!   assert_equal (out.Properties.RowTimes, TT.Properties.RowTimes);
%!   assert_equal (out.v, TT.v);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the row times keep their display format
%!test
%! t = datetime (2024, 1, (1:3)');
%! t.Format = 'uuuu-MM-dd HH:mm:ss.SSS';
%! TT = timetable (t, (1:3)', 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname);
%!   assert_equal (csv2timetable (fname).Properties.RowTimes.Format, ...
%!                 'uuuu-MM-dd HH:mm:ss.SSS');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test duration row times round-trip whole, format included
%!test
%! d = hours ([0.5; 1.5; 2.5]);
%! d.Format = 'hh:mm:ss.SSS';
%! TT = timetable (d, (1:3)', 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname);
%!   out = csv2timetable (fname);
%!   assert_equal (class (out.Properties.RowTimes), 'duration');
%!   assert_equal (out.Properties.RowTimes, TT.Properties.RowTimes);
%!   assert_equal (out.Properties.RowTimes.Format, 'hh:mm:ss.SSS');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the row dimension name survives
%!test
%! t = datetime (2024, 1, (1:3)');
%! TT = timetable (t, (1:3)', 'VariableNames', {'v'});
%! TT.Properties.DimensionNames{1} = 'Stamp';
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname);
%!   assert_equal (csv2timetable (fname).Properties.DimensionNames, ...
%!                 {'Stamp', 'Variables'});
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the variable descriptions and units survive
%!test
%! t = datetime (2024, 1, (1:3)');
%! TT = timetable (t, (1:3)', {'a'; 'b'; 'c'}, 'VariableNames', {'v', 'w'});
%! TT.Properties.VariableDescriptions = {'a count', ''};
%! TT.Properties.VariableUnits = {'kg', ''};
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname);
%!   out = csv2timetable (fname);
%!   assert_equal (out.Properties.VariableDescriptions, {'a count', ''});
%!   assert_equal (out.Properties.VariableUnits, {'kg', ''});
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a regular timetable comes back regular
%!test
%! TT = timetable ((1:4)', 'TimeStep', hours (1), 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname);
%!   out = csv2timetable (fname);
%!   assert_equal (isregular (out), true);
%!   assert_equal (out.Properties.TimeStep, hours (1));
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test 'WriteVariableNames' false numbers the variables and defaults the name
%!test
%! t = datetime (2024, 1, (1:3)');
%! TT = timetable (t, (1:3)', 'VariableNames', {'v'});
%! TT.Properties.DimensionNames{1} = 'Stamp';
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname, 'WriteVariableNames', false);
%!   out = csv2timetable (fname);
%!   assert_equal (out.Properties.VariableNames, {'Var2'});
%!   assert_equal (out.Properties.DimensionNames{1}, 'Var1');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test an attached event table is not written and does not come back
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t([2 3]), 'EventLabels', {'on'; 'off'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   timetable2csv (TT, fname);
%!   assert_equal (isempty (csv2timetable (fname).Properties.Events), true);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a foreign file takes its first datetime column as the row times
%!test
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   fid = fopen (fname, 'w');
%!   fprintf (fid, "when,val\n2024-01-01,1\n2024-01-02,2\n");
%!   fclose (fid);
%!   out = csv2timetable (fname);
%!   assert_equal (class (out), 'timetable');
%!   assert_equal (out.Properties.DimensionNames{1}, 'when');
%!   assert_equal (out.val, [1; 2]);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a table written with row names is read by 'csv2table' unchanged
%!test
%! T = table ((1:3)', 'VariableNames', {'v'}, 'RowNames', {'a', 'b', 'c'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   table2csv (T, fname);
%!   assert_equal (csv2table (fname).Properties.RowNames, {'a'; 'b'; 'c'});
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test too few input arguments
%!error <csv2timetable: too few input arguments.> csv2timetable ()

## Test an invalid FILENAME
%!error <csv2timetable: FILENAME must be a character vector, cellstr, or string.> ...
%! csv2timetable (42)

## Test 'ReadRowNames' is refused
%!error <csv2timetable: 'ReadRowNames' is not supported; a timetable labels its rows by time.  Read a file whose rows are named with 'csv2table'.> ...
%! csv2timetable ('none.csv', 'ReadRowNames', true)

## Test 'RowNamesColumn' is refused
%!error <csv2timetable: 'RowNamesColumn' is not supported; a timetable labels its rows by time.  Read a file whose rows are named with 'csv2table'.> ...
%! csv2timetable ('none.csv', 'RowNamesColumn', 1)

## A file whose columns are all numeric, so nothing in it can label the rows.
%!shared fnum
%! fnum = [tempname(), '.csv'];
%! fid = fopen (fnum, 'w');
%! fputs (fid, "a,b\n1,2\n3,4\n");
%! fclose (fid);

## Test a file with no time column cannot become a timetable
%!error <csv2timetable: the file has no datetime or duration column to use as row times.> ...
%! csv2timetable (fnum)

## Test the fixture is removed again
%!test
%! delete (fnum);
%! assert_equal (exist (fnum, 'file'), 0);
