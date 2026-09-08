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
## @deftypefn  {datatypes} {@var{tt} =} ods2timetable (@var{filename})
## @deftypefnx {datatypes} {@var{tt} =} ods2timetable (@var{filename}, @var{Name}, @var{Value})
##
## Read an OpenDocument spreadsheet file into a timetable.
##
## @code{@var{tt} = ods2timetable (@var{filename})} reads a sheet of the
## OpenDocument spreadsheet named by @var{filename} into a @code{timetable}.
## @var{filename} may be a character vector, a cellstr, or a string scalar.
##
## A sheet written by @code{timetable2ods} carries the package's own metadata,
## which names and types every variable and tags the leading column of row
## times with their type, their @code{TimeZone} where they have one, and their
## @code{Format}.  Such a sheet comes back as the timetable it was written
## from: the row times keep their type, zone and format exactly, and the row
## dimension keeps its name.
##
## Any other sheet is read as @code{ods2table} reads it, and its @strong{first}
## @code{datetime} or @code{duration} variable becomes the row times, the row
## dimension taking that variable's name.  A sheet with no such variable cannot
## be read as a timetable.
##
## Every @var{Name}-@var{Value} option of @code{ods2table} is accepted and
## behaves as it does there, with two exceptions.  @qcode{'ReadRowNames'} and
## @qcode{'RowNamesColumn'} are refused: a timetable labels its rows by time
## and by nothing else, so a sheet whose rows are named is read with
## @code{ods2table}.
##
## @code{TimeStep} and @code{SampleRate} are not stored in the file and are
## worked out again from the row times, so a regular timetable comes back
## regular.
##
## @seealso{timetable2ods, ods2table, ods2struct, csv2timetable, timetable}
## @end deftypefn

function tt = ods2timetable (filename, varargin)

  if (nargin < 1)
    error ("ods2timetable: too few input arguments.");
  endif
  if (! ((ischar (filename) && isvector (filename)) || iscellstr (filename) ...
         || isa (filename, 'string')))
    error (strcat ("ods2timetable: FILENAME must be a character vector,", ...
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
      error (strcat ("ods2timetable: '%s' is not supported; a timetable", ...
                     " labels its rows by time.  Read a sheet whose rows", ...
                     " are named with 'ods2table'."), opt);
    endif
  endfor

  [tbl, rowTimesName] = ods2table (filename, varargin{:});
  if (! isempty (rowTimesName))
    tt = table2timetable (tbl, 'RowTimes', rowTimesName);
    return;
  endif
  types = tbl.Properties.VariableTypes;
  if (! any (ismember (types, {'datetime', 'duration'})))
    error (strcat ("ods2timetable: the sheet has no datetime or duration", ...
                   " column to use as row times."));
  endif
  tt = table2timetable (tbl);

endfunction

%!demo
%! ## A timetable written by `timetable2ods` comes back whole: the row times
%! ## keep their type, their time zone and their display format, the row
%! ## dimension keeps its name, and the spreadsheet shows the times as times
%! ## because they are written as native date cells.
%!
%! t = datetime (2024, 3, 9, 22, 0, 0, 'TimeZone', 'America/New_York') ...
%!     + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'reading'});
%! TT.Properties.DimensionNames{1} = 'Stamp';
%! fname = [tempname(), '.ods'];
%! timetable2ods (TT, fname);
%! ods2timetable (fname)
%! delete (fname);

## Test a zoned datetime round-trips whole
%!test
%! t = datetime (2024, 3, 9, 22, 0, 0, 'TimeZone', 'America/New_York') ...
%!     + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   out = ods2timetable (fname);
%!   assert_equal (class (out), 'timetable');
%!   assert_equal (out.Properties.RowTimes.TimeZone, 'America/New_York');
%!   assert_equal (out.Properties.RowTimes, TT.Properties.RowTimes);
%!   assert_equal (out.v, TT.v);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test duration row times round-trip whole, format included
%!test
%! d = hours ([0.5; 1.5; 2.5]);
%! d.Format = 'hh:mm:ss.SSS';
%! TT = timetable (d, (1:3)', 'VariableNames', {'v'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   out = ods2timetable (fname);
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
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   assert_equal (ods2timetable (fname).Properties.DimensionNames, ...
%!                 {'Stamp', 'Variables'});
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the row times are written as native date cells
%!test
%! t = datetime (2024, 1, (1:2)');
%! TT = timetable (t, (1:2)', 'VariableNames', {'v'});
%! fname = [tempname(), '.fods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   txt = fileread (fname);
%!   hit = strfind (txt, 'office:date-value="2024-01-01');
%!   assert_equal (! isempty (hit), true);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the duration row times are written as native time cells
%!test
%! TT = timetable (hours ([1; 2]), (1:2)', 'VariableNames', {'v'});
%! fname = [tempname(), '.fods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   txt = fileread (fname);
%!   assert_equal (! isempty (strfind (txt, 'office:time-value=')), true);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the variable descriptions and units survive
%!test
%! t = datetime (2024, 1, (1:3)');
%! TT = timetable (t, (1:3)', {'a'; 'b'; 'c'}, 'VariableNames', {'v', 'w'});
%! TT.Properties.VariableDescriptions = {'a count', ''};
%! TT.Properties.VariableUnits = {'kg', ''};
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   out = ods2timetable (fname);
%!   assert_equal (out.Properties.VariableDescriptions, {'a count', ''});
%!   assert_equal (out.Properties.VariableUnits, {'kg', ''});
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a regular timetable comes back regular
%!test
%! TT = timetable ((1:4)', 'TimeStep', hours (1), 'VariableNames', {'v'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   out = ods2timetable (fname);
%!   assert_equal (isregular (out), true);
%!   assert_equal (out.Properties.TimeStep, hours (1));
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a workbook of a table and a timetable round-trips as both
%!test
%! t = datetime (2024, 1, (1:3)');
%! wb.Plain = table ((1:2)', 'VariableNames', {'a'});
%! wb.Timed = timetable (t, (1:3)', 'VariableNames', {'v'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   struct2ods (fname, wb);
%!   s = ods2struct (fname);
%!   assert_equal (class (s.Plain), 'table');
%!   assert_equal (class (s.Timed), 'timetable');
%!   assert_equal (s.Timed.Properties.RowTimes, t);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a table whose first variable is a datetime stays a table
%!test
%! T = table (datetime (2024, 1, (1:3)'), (1:3)', ...
%!            'VariableNames', {'when', 'v'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   table2ods (T, fname);
%!   assert_equal (class (ods2struct (fname).Sheet1), 'table');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a second sheet is added to an existing workbook
%!test
%! t = datetime (2024, 1, (1:3)');
%! TT = timetable (t, (1:3)', 'VariableNames', {'v'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   table2ods (table ((1:2)', 'VariableNames', {'a'}), fname, 'Sheet', 'One');
%!   timetable2ods (TT, fname, 'Sheet', 'Two');
%!   s = ods2struct (fname);
%!   assert_equal (fieldnames (s), {'One'; 'Two'});
%!   assert_equal (class (s.Two), 'timetable');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test 'ods2table' reads the row times as a leading variable
%!test
%! t = datetime (2024, 1, (1:3)');
%! TT = timetable (t, (1:3)', 'VariableNames', {'v'});
%! TT.Properties.DimensionNames{1} = 'Stamp';
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   [T, rtn] = ods2table (fname);
%!   assert_equal (class (T), 'table');
%!   assert_equal (T.Properties.VariableNames, {'Stamp', 'v'});
%!   assert_equal (rtn, 'Stamp');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a sheet holding no row times reports none
%!test
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   table2ods (table ((1:2)', 'VariableNames', {'a'}), fname);
%!   [~, rtn] = ods2table (fname);
%!   assert_equal (rtn, '');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test too few input arguments
%!error <ods2timetable: too few input arguments.> ods2timetable ()

## Test an invalid FILENAME
%!error <ods2timetable: FILENAME must be a character vector, cellstr, or string.> ...
%! ods2timetable (42)

## Test 'ReadRowNames' is refused
%!error <ods2timetable: 'ReadRowNames' is not supported; a timetable labels its rows by time.  Read a sheet whose rows are named with 'ods2table'.> ...
%! ods2timetable ('none.ods', 'ReadRowNames', true)

## Test 'RowNamesColumn' is refused
%!error <ods2timetable: 'RowNamesColumn' is not supported; a timetable labels its rows by time.  Read a sheet whose rows are named with 'ods2table'.> ...
%! ods2timetable ('none.ods', 'RowNamesColumn', 1)
