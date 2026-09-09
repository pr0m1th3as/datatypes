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
## An event table is held on a sheet of its own, named by a
## @qcode{## Events crossref:} line in the hidden metadata sheet, and is
## attached to the timetable that names it.  With no @qcode{'Sheet'} given the
## first data sheet is read @strong{less the sheets that hold somebody's
## events}, so a file holding one timetable and its events reads with no
## argument at all.  A sheet asked for by name or by index is read whichever it
## is, the index running over every data sheet so that one number means one
## sheet here and in @code{ods2table} alike; an event sheet asked for by name
## comes back as the @code{eventtable} it is.
##
## A reference is refused when it names a sheet the file does not have, when
## the sheet it names holds no row times, when those row times are of a
## different type than the referring timetable's, or when the sheet it names
## carries a reference of its own, an event table not being something that can
## carry an event table.  All four are reachable only in a file edited by hand.
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

  file = char (cellstr (filename));
  [data, ~, ~, names, preamble] = __ods2table__ (file);
  if (ischar (data))
    error ("ods2timetable: %s", data);
  endif
  xrefs = __odscrossrefs__ (preamble);

  ## With no sheet asked for, the first data sheet is read, less the sheets
  ## that hold somebody's events: a file holding one timetable and its events
  ## therefore reads with no argument at all.  A sheet asked for by name or by
  ## index is read whichever it is, the index running over every data sheet so
  ## that one number means one sheet here and in 'ods2table' alike.
  args = varargin;
  if (! any (strcmpi (args(1:2:end), 'Sheet')))
    pick = names(! ismember (names, {xrefs.to}));
    if (isempty (pick))
      error (strcat ("ods2timetable: every sheet of '%s' holds the events", ...
                     " of another; name the one to read."), file);
    endif
    args = [{'Sheet', pick{1}}, args];
  endif

  [tbl, rowTimesName] = ods2table (file, args{:});
  if (isempty (rowTimesName))
    types = tbl.Properties.VariableTypes;
    if (! any (ismember (types, {'datetime', 'duration'})))
      error (strcat ("ods2timetable: the sheet has no datetime or duration", ...
                     " column to use as row times."));
    endif
    tt = table2timetable (tbl);
    return;
  endif
  tt = table2timetable (tbl, 'RowTimes', rowTimesName);

  ## A sheet asked for by name may be an event table, which is what it comes
  ## back as; otherwise its own events are attached where it has any.
  sheet = args{find (strcmpi (args(1:2:end), 'Sheet'), 1) * 2};
  if (isnumeric (sheet) && isscalar (sheet) && sheet >= 1 ...
      && sheet <= numel (names))
    sheet = names{sheet};
  endif
  jx = [];
  if (ischar (sheet))
    jx = find (strcmp (sheet, {xrefs.to}), 1);
  endif
  if (! isempty (jx))
    evargs = __odseventopts__ (xrefs(jx));
    tt = eventtable (tt, evargs{:});
    return;
  endif
  ix = find (strcmp (sheet, {xrefs.from}), 1);
  if (! isempty (ix))
    tt = __odsattach__ (tt, file, xrefs(ix), names, 'ods2timetable');
  endif

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

## Test an attached event table round-trips whole
%!test
%! t = datetime (2024, 1, 1) + hours ((0:5)');
%! TT = timetable (t, (1:6)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t([2 4]), 'EventLabels', ...
%!                                    {'on'; 'off'}, ...
%!                                    'EventLengths', hours ([2; 1]));
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   out = ods2timetable (fname);
%!   assert_equal (class (out.Properties.Events), 'eventtable');
%!   assert_equal (isequal (out.Properties.Events, TT.Properties.Events), true);
%!   assert_equal (isequal (out, TT), true);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the three event designations survive
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t([2 3]), 'EventLabels', ...
%!                                    {'on'; 'off'}, 'EventEnds', t([3 4]));
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname);
%!   P = ods2timetable (fname).Properties.Events.Properties;
%!   assert_equal (P.EventLabelsVariable, 'EventLabels');
%!   assert_equal (P.EventEndsVariable, 'EventEnds');
%!   assert_equal (P.EventLengthsVariable, []);
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test the event sheet is consumed and gets no field of its own
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t(2), 'EventLabels', {'on'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname, 'Sheet', 'Data');
%!   s = ods2struct (fname);
%!   assert_equal (fieldnames (s), {'Data'});
%!   assert_equal (class (s.Data.Properties.Events), 'eventtable');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test an event sheet asked for by name comes back as an event table
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t(2), 'EventLabels', {'on'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname, 'Sheet', 'Data');
%!   ev = ods2timetable (fname, 'Sheet', 'Data_Events');
%!   assert_equal (class (ev), 'eventtable');
%!   assert_equal (ev.Properties.EventLabelsVariable, 'EventLabels');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a numeric 'Sheet' indexes every data sheet, event sheets included
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t(2), 'EventLabels', {'on'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   timetable2ods (TT, fname, 'Sheet', 'Data');
%!   assert_equal (class (ods2timetable (fname, 'Sheet', 2)), 'eventtable');
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a workbook of two timetables keeps each one's events apart
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! A = timetable (t, (1:4)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t(2), 'EventLabels', {'up'});
%! B = timetable (t, (5:8)', 'VariableNames', {'w'});
%! B.Properties.Events = eventtable (t(3), 'EventLabels', {'down'});
%! fname = [tempname(), '.ods'];
%! unwind_protect
%!   struct2ods (fname, struct ('A', A, 'B', B));
%!   s = ods2struct (fname);
%!   assert_equal (fieldnames (s), {'A'; 'B'});
%!   assert_equal (cellstr (s.A.Properties.Events.EventLabels), {'up'});
%!   assert_equal (cellstr (s.B.Properties.Events.EventLabels), {'down'});
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test one event sheet referenced by two timetables is read as written
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! A = timetable (t, (1:4)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t(2), 'EventLabels', {'up'});
%! B = timetable (t, (5:8)', 'VariableNames', {'w'});
%! B.Properties.Events = eventtable (t(3), 'EventLabels', {'down'});
%! fname = [tempname(), '.fods'];
%! unwind_protect
%!   struct2ods (fname, struct ('A', A, 'B', B));
%!   txt = fileread (fname);
%!   txt = strrep (txt, '<text:p>B_Events</text:p>', ...
%!                 '<text:p>A_Events</text:p>');
%!   fid = fopen (fname, 'w');  fputs (fid, txt);  fclose (fid);
%!   s = ods2struct (fname);
%!   assert_equal (cellstr (s.A.Properties.Events.EventLabels), {'up'});
%!   assert_equal (cellstr (s.B.Properties.Events.EventLabels), {'up'});
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test a reference to a sheet the file does not have is refused
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! TT.Properties.Events = eventtable (t(2), 'EventLabels', {'on'});
%! fname = [tempname(), '.fods'];
%! unwind_protect
%!   timetable2ods (TT, fname, 'Sheet', 'Data');
%!   txt = fileread (fname);
%!   txt = strrep (txt, '<text:p>Data_Events</text:p>', ...
%!                 '<text:p>Nope</text:p>');
%!   fid = fopen (fname, 'w');  fputs (fid, txt);  fclose (fid);
%!   fail ("ods2timetable (fname)", ...
%!         "the event table of sheet 'Data' is said to be on sheet 'Nope'");
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test an event table said to carry an event table is refused
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! A = timetable (t, (1:4)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t(2), 'EventLabels', {'up'});
%! B = timetable (t, (5:8)', 'VariableNames', {'w'});
%! B.Properties.Events = eventtable (t(3), 'EventLabels', {'down'});
%! fname = [tempname(), '.fods'];
%! unwind_protect
%!   struct2ods (fname, struct ('A', A, 'B', B));
%!   txt = fileread (fname);
%!   txt = strrep (txt, '<text:p>B_Events</text:p>', '<text:p>A</text:p>');
%!   fid = fopen (fname, 'w');  fputs (fid, txt);  fclose (fid);
%!   fail ("ods2struct (fname)", "an event table cannot carry an event table");
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Test an event table whose row times are of another type is refused
%!test
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! A = timetable (t, (1:4)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t(2), 'EventLabels', {'up'});
%! B = timetable (hours ((0:3)'), (5:8)', 'VariableNames', {'w'});
%! fname = [tempname(), '.fods'];
%! unwind_protect
%!   struct2ods (fname, struct ('A', A, 'B', B));
%!   txt = fileread (fname);
%!   txt = strrep (txt, '<text:p>A_Events</text:p>', '<text:p>B</text:p>');
%!   fid = fopen (fname, 'w');  fputs (fid, txt);  fclose (fid);
%!   fail ("ods2struct (fname)", ...
%!         "has duration row times where sheet 'A' has datetime");
%! unwind_protect_cleanup
%!   delete (fname);
%! end_unwind_protect

## Two files nothing can be read out of: one whose columns are all numeric,
## so nothing in it can label the rows, and one in which a second cross
## reference points at the data sheet, leaving every sheet of the file the
## event table of another.
%!shared fnum, fev
%! fnum = [tempname(), '.fods'];
%! table2ods (table ([1; 2], [3; 4], 'VariableNames', {'a', 'b'}), fnum);
%! fev = [tempname(), '.fods'];
%! t = datetime (2024, 1, 1) + hours ((0:3)');
%! TT = timetable (t, (1:4)', 'VariableNames', {'v'});
%! ET = eventtable (timetable (t([2, 3]), {'a'; 'b'}, ...
%!                             'VariableNames', {'L'}));
%! ET.EventLabelsVariable = 'L';
%! TT.Properties.Events = ET;
%! timetable2ods (TT, fev);
%! txt = fileread (fev);
%! tc = '<table:table-cell office:value-type="string">';
%! row = ['<table:table-row>', tc, ...
%!        '<text:p>## Events crossref:</text:p></table:table-cell>', ...
%!        tc, '<text:p>Sheet1_Events</text:p></table:table-cell>', ...
%!        tc, '<text:p>Sheet1</text:p></table:table-cell>', ...
%!        '</table:table-row>'];
%! ix = strfind (txt, '<text:p>## Events crossref:</text:p>')(1);
%! ie = ix + strfind (txt(ix:end), '</table:table-row>')(1) + 17;
%! fid = fopen (fev, 'w');
%! fputs (fid, [txt(1:ie-1), row, txt(ie:end)]);
%! fclose (fid);

## Test a sheet with no time column cannot become a timetable
%!error <ods2timetable: the sheet has no datetime or duration column to use as row times.> ...
%! ods2timetable (fnum)

## Test a file whose every sheet is somebody's event table names none to read
%!error <ods2timetable: every sheet of '.*' holds the events of another; name the one to read.> ...
%! ods2timetable (fev)

## Test the fixtures are removed again
%!test
%! delete (fnum);
%! delete (fev);
%! assert_equal (exist (fnum, 'file'), 0);
%! assert_equal (exist (fev, 'file'), 0);
