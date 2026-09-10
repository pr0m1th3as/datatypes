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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{tt} =} readtimetable (@var{filename})
## @deftypefnx {datatypes} {@var{tt} =} readtimetable (@var{filename}, @var{Name}, @var{Value})
##
## Read a file into a timetable, in the MATLAB-compatible form.
##
## @code{@var{tt} = readtimetable (@var{filename})} reads @var{filename} as
## @code{readtable} reads it, taking the file to carry a header row of names and
## then one row per row, and turns the result into a @code{timetable}.  Text
## files (@qcode{.txt}, @qcode{.csv}, @qcode{.dat}) and spreadsheets
## (@qcode{.ods}, @qcode{.fods}, @qcode{.xlsx}, @qcode{.xlsm}) are supported,
## and @qcode{'FileType'} overrides what the extension says.
##
## The @strong{first} @code{datetime} or @code{duration} variable becomes the
## row times and the row dimension takes its name, which is what MATLAB does and
## what makes a file written by @code{writetimetable} come back as it went out.
## @qcode{'RowTimes'} names a different variable, by name or by index.  A file
## with no such variable cannot be read as a timetable.
##
## A column written in the @strong{RFC 9557} form,
## @qcode{2024-03-09T22:00:00-05:00[America/New_York]}, is recognised and comes
## back as a zone-aware @code{datetime} with its zone and its side of any
## daylight-saving fold intact.  That is what @code{writetimetable} writes for
## zoned row times; see deviation @strong{D7} in that method's help for why.
## A column MATLAB wrote is read as MATLAB reads it, so a bare wall clock comes
## back unzoned, exactly as it does there.
##
## Every @var{Name}-@var{Value} option of @code{readtable} is accepted and
## behaves as it does there, with two exceptions.  @qcode{'ReadRowNames'} and
## @qcode{'RowNamesColumn'} are refused: a timetable labels its rows by time and
## by nothing else, so a file whose rows are named is read with
## @code{readtable}.
##
## @code{TimeStep} and @code{SampleRate} are not stored in the file and are
## worked out again from the row times.  No file MATLAB can read carries an
## event table, so the result never has one; use @code{ods2timetable} for that.
##
## @seealso{writetimetable, readtable, ods2timetable, csv2timetable, timetable}
## @end deftypefn

function tt = readtimetable (filename, varargin)

  if (nargin < 1)
    error ("readtimetable: too few input arguments.");
  endif
  if (! ((ischar (filename) && isvector (filename)) || iscellstr (filename) ...
         || isa (filename, 'string')))
    error (strcat ("readtimetable: FILENAME must be a character vector,", ...
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
      error (strcat ("readtimetable: '%s' is not supported; a timetable", ...
                     " labels its rows by time.  Read a file whose rows", ...
                     " are named with 'readtable'."), opt);
    endif
  endfor

  ## 'RowTimes' is ours to act on and 'readtable' knows nothing of it.
  [rowTimes, args] = parsePairedArguments ({'RowTimes'}, {[]}, varargin(:));
  tbl = readtable (filename, args{:});

  ## A column written in RFC 9557 form arrives as text, no reader having a
  ## type for it; every entry of a column must parse, a column being one
  ## variable.
  names = tbl.Properties.VariableNames;
  for i = 1:numel (names)
    col = tbl.(names{i});
    if (ischar (col))
      col = cellstr (col);
    elseif (! iscellstr (col))
      continue;
    endif
    [dt, ok] = __rfc95572dt__ (col);
    if (ok)
      tbl.(names{i}) = dt;
    endif
  endfor

  if (isempty (rowTimes))
    types = tbl.Properties.VariableTypes;
    if (! any (ismember (types, {'datetime', 'duration'})))
      error (strcat ("readtimetable: the file has no datetime or duration", ...
                     " column to use as row times."));
    endif
    tt = table2timetable (tbl);
  else
    tt = table2timetable (tbl, 'RowTimes', rowTimes);
  endif

endfunction

%!demo
%! ## `writetimetable` writes a zoned datetime in the RFC 9557 form, which
%! ## carries both the offset and the zone name, so `readtimetable` returns
%! ## the timetable that was written rather than an unzoned shadow of it.
%!
%! t = datetime (2024, 3, 9, 22, 0, 0, 'TimeZone', 'America/New_York') ...
%!     + hours ((0:2)');
%! TT = timetable (t, (1:3)', 'VariableNames', {'reading'});
%! fname = [tempname(), '.csv'];
%! writetimetable (TT, fname);
%! type (fname);
%! readtimetable (fname)
%! delete (fname);

## Test a zoned datetime round-trips through the interop pair
%!test
%! t = datetime (2024, 3, 9, 22, 0, 0, 'TimeZone', 'America/New_York') ...
%!     + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   out = readtimetable (fname);
%!   assert_equal (out.Properties.RowTimes.TimeZone, 'America/New_York');
%!   assert_equal (out.Properties.RowTimes, TT.Properties.RowTimes);
%!   assert_equal (out.v, TT.v);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the written form is RFC 9557
%!test
%! t = datetime (2024, 3, 9, 22, 0, 0, 'TimeZone', 'America/New_York');
%! TT = timetable (t, 1, 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   txt = fileread (fname);
%!   hit = strfind (txt, '2024-03-09T22:00:00-05:00[America/New_York]');
%!   assert_equal (! isempty (hit), true);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a timestamp inside a daylight-saving fold keeps its side of it
%!test
%! a = datetime (2024, 11, 3, 5, 30, 0, 'TimeZone', 'UTC');
%! a.TimeZone = 'America/New_York';
%! b = datetime (2024, 11, 3, 6, 30, 0, 'TimeZone', 'UTC');
%! b.TimeZone = 'America/New_York';
%! TT = timetable ([a; b], (1:2)', 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   rt = readtimetable (fname).Properties.RowTimes;
%!   assert_equal (rt, TT.Properties.RowTimes);
%!   assert_equal (char (tzoffset (rt(1))), '-04:00');
%!   assert_equal (char (tzoffset (rt(2))), '-05:00');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test an unzoned datetime is written as MATLAB writes it
%!test
%! TT = timetable (datetime (2024, 1, (1:2)'), (1:2)', 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   txt = fileread (fname);
%!   assert_equal (! isempty (strfind (txt, '01-Jan-2024')), true);
%!   assert_equal (isempty (strfind (txt, '[')), true);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test duration row times round-trip
%!test
%! TT = timetable (hours ([1; 2; 3]), (1:3)', 'VariableNames', {'v'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   out = readtimetable (fname);
%!   assert_equal (class (out.Properties.RowTimes), 'duration');
%!   assert_equal (out.Properties.RowTimes, TT.Properties.RowTimes);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the row dimension name leads the file and comes back
%!test
%! TT = timetable (datetime (2024, 1, (1:2)'), (1:2)', 'VariableNames', {'v'});
%! TT.Properties.DimensionNames{1} = 'Stamp';
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   lines = strsplit (fileread (fname), "\n");
%!   assert_equal (lines{1}, 'Stamp,v');
%!   assert_equal (readtimetable (fname).Properties.DimensionNames{1}, 'Stamp');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a spreadsheet round-trips through the interop pair
%!test
%! TT = timetable (datetime (2024, 1, (1:3)'), (1:3)', 'VariableNames', {'v'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   out = readtimetable (fname);
%!   assert_equal (out.Properties.RowTimes, TT.Properties.RowTimes);
%!   assert_equal (out.v, TT.v);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test 'RowTimes' names another variable
%!test
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   fid = fopen (fname, 'w');
%!   fprintf (fid, "a,when\n1,2024-01-01\n2,2024-01-02\n");
%!   fclose (fid);
%!   out = readtimetable (fname, 'RowTimes', 'when');
%!   assert_equal (out.Properties.DimensionNames{1}, 'when');
%!   assert_equal (out.a, [1; 2]);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test an attached event table is not written
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t(2), 'EventLabels', {'on'});
%! fname = [tempname(), '.csv'];
%! unwind_protect
%!   writetimetable (TT, fname);
%!   assert_equal (isempty (readtimetable (fname).Properties.Events), true);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test too few input arguments
%!error <readtimetable: too few input arguments.> readtimetable ()

## Test an invalid FILENAME
%!error <readtimetable: FILENAME must be a character vector, cellstr, or string.> ...
%! readtimetable (42)

## Test 'ReadRowNames' is refused
%!error <readtimetable: 'ReadRowNames' is not supported; a timetable labels its rows by time.  Read a file whose rows are named with 'readtable'.> ...
%! readtimetable ('none.csv', 'ReadRowNames', true)

## Test 'RowNamesColumn' is refused
%!error <readtimetable: 'RowNamesColumn' is not supported; a timetable labels its rows by time.  Read a file whose rows are named with 'readtable'.> ...
%! readtimetable ('none.csv', 'RowNamesColumn', 1)

## Test 'WriteRowNames' is refused by the writer
%!error <timetable.writetimetable: 'WriteRowNames' is not supported; a timetable labels its rows by time.  Write a table with row names using 'writetable'.> ...
%! writetimetable (timetable ((1:3)', 'TimeStep', hours (1)), 'f.csv', ...
%!                 'WriteRowNames', true)

## A file whose columns are all numeric, so nothing in it can label the rows.
%!shared fnum
%! fnum = [tempname(), '.csv'];
%! fid = fopen (fnum, 'w');
%! fputs (fid, "a,b\n1,2\n3,4\n");
%! fclose (fid);

## Test a file with no time column cannot become a timetable
%!error <readtimetable: the file has no datetime or duration column to use as row times.> ...
%! readtimetable (fnum)

## Test the fixture is removed again
%!test
%! delete (fnum);
%! assert_equal (exist (fnum, 'file'), 0);
