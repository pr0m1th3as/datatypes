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
## @deftypefn  {datatypes} {@var{tbl} =} readtable (@var{filename})
## @deftypefnx {datatypes} {@var{tbl} =} readtable (@var{filename}, @var{Name}, @var{Value})
##
## Read a file into a table, detecting variable types automatically.
##
## @code{@var{tbl} = readtable (@var{filename})} reads the file named by
## @var{filename} (a character vector or string scalar) and returns it as a
## @code{table}.  The file type is inferred from the extension:
## @qcode{.txt}, @qcode{.csv}, and @qcode{.dat} are read as delimited text;
## @qcode{.ods} and @qcode{.fods} are read as OpenDocument spreadsheets.  Use
## the @qcode{'FileType'} option to override the inferred type.
##
## By default the first row supplies the variable names and each column's data
## type is detected automatically (numeric, @code{datetime}, @code{duration}, or
## text).  The following @var{Name}-@var{Value} options are supported:
##
## @multitable @columnfractions 0.28 0.72
## @item @qcode{'FileType'} @tab @qcode{'text'} or @qcode{'spreadsheet'}.
## @item @qcode{'ReadVariableNames'} @tab Logical; read the first row as variable
## names (default @qcode{true}).
## @item @qcode{'ReadRowNames'} @tab Logical; read the first column as row names
## (default @qcode{false}).
## @item @qcode{'Delimiter'} @tab Field delimiter for text files: a single
## character or one of @qcode{'comma'}, @qcode{'space'}, @qcode{'tab'},
## @qcode{'semi'}, @qcode{'bar'} (default @qcode{','}).
## @item @qcode{'NumHeaderLines'} @tab Number of lines to skip before the header
## (default @qcode{0}).
## @item @qcode{'TextType'} @tab @qcode{'char'} or @qcode{'string'} for text
## columns (default @qcode{'char'}).
## @item @qcode{'VariableNamingRule'} @tab @qcode{'modify'} or @qcode{'preserve'}
## (default @qcode{'modify'}).
## @item @qcode{'Sheet'} @tab Spreadsheet only: the sheet to read, selected by
## name or by a 1-based index over the data sheets (default: the first sheet).
## @item @qcode{'Range'} @tab Spreadsheet only: an A1-style range such as
## @qcode{'C5'} or @qcode{'C5:D8'} limiting the region read.
## @end multitable
##
## Office Open XML spreadsheets (@qcode{.xlsx}, @qcode{.xlsm}) are read via the
## same interface as ODS.  The legacy binary formats @qcode{.xls} and
## @qcode{.xlsb} are not supported; use @qcode{.xlsx}, @qcode{.ods}, or a text
## format.
##
## @seealso{writetable, csv2table, ods2table}
## @end deftypefn

function tbl = readtable (filename, varargin)

  if (nargin < 1)
    print_usage ();
  endif
  if (! ((ischar (filename) && isvector (filename)) ...
         || (isa (filename, 'string') && isscalar (filename))))
    error ("readtable: FILENAME must be a character vector or string scalar.");
  endif
  file = char (filename);

  optNames = {'FileType', 'ReadVariableNames', 'ReadRowNames', 'Delimiter', ...
              'NumHeaderLines', 'TextType', 'VariableNamingRule', 'Sheet', ...
              'Range'};
  dfValues = {'', true, false, ',', 0, 'char', 'modify', [], ''};
  [fileType, readVarNames, readRowNames, delim, numHeaderLines, textType, ...
   namingRule, sheet, range, args] = ...
                          parsePairedArguments (optNames, dfValues, varargin(:));
  if (! isempty (args))
    error ("readtable: unknown option '%s'.", args{1});
  endif
  if (isa (sheet, 'string'))
    sheet = char (sheet);
  endif
  if (isa (range, 'string'))
    range = char (range);
  endif

  ## Resolve the file type from the option or the extension
  [~, ~, ext] = fileparts (file);
  if (isempty (fileType))
    switch (lower (ext))
      case {'.txt', '.csv', '.dat'}
        fileType = 'text';
      case {'.ods', '.fods', '.xlsx', '.xlsm'}
        fileType = 'spreadsheet';
      case {'.xls', '.xlsb'}
        error (strcat ("readtable: '%s' Excel files are not supported; use", ...
                       " '.xlsx', '.ods', or a text format."), ext);
      otherwise
        error (strcat ("readtable: cannot infer the file type from '%s';", ...
                       " specify 'FileType'."), ext);
    endswitch
  endif
  ## Office Open XML (.xlsx/.xlsm) uses a separate reader from ODS.
  isXlsx = any (strcmpi (ext, {'.xlsx', '.xlsm'}));

  ## 'Sheet' and 'Range' apply only to spreadsheets; MATLAB rejects them on text
  ## files rather than silently ignoring them.
  if (strcmpi (fileType, 'text') && (! isempty (sheet) || ! isempty (range)))
    error (strcat ("readtable: 'Sheet' and 'Range' are not supported for", ...
                   " text files."));
  endif

  switch (lower (fileType))
    case 'text'
      d = resolve_delimiter (delim);
      ## Hexadecimal auto-detection is a csv2table extension that MATLAB's
      ## readtable does not perform, so keep hex-like strings as text.
      tbl = csv2table (file, 'ReadVariableNames', readVarNames, ...
                       'ReadRowNames', readRowNames, 'RowNamesColumn', 1, ...
                       'NumHeaderLines', numHeaderLines, 'TextType', textType, ...
                       'VariableNamingRule', namingRule, 'Delimiter', d, ...
                       'HexType', 'text');
    case 'spreadsheet'
      tbl = read_spreadsheet (file, readVarNames, readRowNames, textType, ...
                              namingRule, sheet, range, isXlsx);
    otherwise
      error ("readtable: 'FileType' must be 'text' or 'spreadsheet'.");
  endswitch

endfunction

## Translate a MATLAB delimiter (named or literal) into a single character.
function d = resolve_delimiter (delim)
  if (isa (delim, 'string'))
    delim = char (delim);
  endif
  if (! ischar (delim))
    error ("readtable: 'Delimiter' must be a character vector or string.");
  endif
  switch (lower (delim))
    case {'comma', ','}
      d = ',';
    case {'space', ' '}
      d = ' ';
    case {'tab', "\t"}
      d = "\t";
    case {'semi', ';'}
      d = ';';
    case {'bar', '|'}
      d = '|';
    otherwise
      if (isscalar (delim))
        d = delim;
      else
        error ("readtable: unsupported 'Delimiter' value '%s'.", delim);
      endif
  endswitch
endfunction

## Read an OpenDocument spreadsheet as a plain sheet: variable names from the
## first row (when requested), then one column per sheet column with its type
## taken from the native ODS cell value types.
function tbl = read_spreadsheet (file, readVarNames, readRowNames, textType, ...
                                 namingRule, sheet, range, isXlsx)
  if (isXlsx)
    [data, vtype] = __xlsx2table__ (file, sheet);
  else
    [data, vtype] = __ods2table__ (file, sheet);
  endif
  if (ischar (data))
    error ("readtable: %s", data);
  endif
  ## Apply an explicit 'Range' (absolute A1 coordinates), otherwise auto-trim
  ## the sheet to its used block so leading blank rows/columns are ignored.
  if (! isempty (range))
    [data, vtype] = clip_range (data, vtype, range);
  else
    [data, vtype] = trim_used_block (data, vtype);
  endif
  ncols = size (data, 2);
  if (ncols == 0)
    tbl = table ();
    return;
  endif

  ## Variable names from the first row
  if (readVarNames && size (data, 1) >= 1)
    names = cell (1, ncols);
    for c = 1:ncols
      x = data{1,c};
      if (ischar (x) && ! isempty (x))
        names{c} = x;
      elseif (isempty (x))
        names{c} = sprintf ("Var%d", c);
      else
        names{c} = num2str (x);
      endif
    endfor
    data(1,:) = [];  vtype(1,:) = [];
  else
    names = arrayfun (@(c) sprintf ("Var%d", c), 1:ncols, ...
                      'UniformOutput', false);
  endif
  if (strcmpi (namingRule, 'modify'))
    names = matlab.lang.makeValidName (names);
  endif

  ## A leading column becomes row names when requested
  rowNames = {};
  if (readRowNames)
    rowNames = ods_strings (data(:,1), vtype(:,1));
    data(:,1) = [];  vtype(:,1) = [];  names(1) = [];
  endif

  ## Reconstruct each column from its native value type
  varValues = cell (1, numel (names));
  for c = 1:numel (names)
    varValues{c} = reconstruct_column (data(:,c), vtype(:,c), textType);
  endfor

  if (isempty (rowNames))
    tbl = table (varValues{:}, 'VariableNames', names);
  else
    tbl = table (varValues{:}, 'VariableNames', names, 'RowNames', rowNames);
  endif
endfunction

## Clip the raw grid to an absolute A1 range; open-ended (Inf) bounds and
## bounds past the used area are capped to the grid, matching MATLAB, which
## reads whatever of the requested rectangle actually holds data.
function [data, vtype] = clip_range (data, vtype, range)
  [r1, c1, r2, c2] = __a1ref__ (range);
  nr = size (data, 1);
  nc = size (data, 2);
  r2 = min (r2, nr);
  c2 = min (c2, nc);
  if (r1 > r2 || c1 > c2)
    data = cell (0, 0);
    vtype = cell (0, 0);
    return;
  endif
  data = data(r1:r2, c1:c2);
  vtype = vtype(r1:r2, c1:c2);
endfunction

## Trim leading and trailing empty rows and columns to the used bounding box
## (interior blanks are preserved).  This mirrors readtable's automatic used-
## range detection for foreign spreadsheets whose data does not start at A1.
function [data, vtype] = trim_used_block (data, vtype)
  if (isempty (data))
    return;
  endif
  blank = cellfun (@isempty, data);
  keptRows = find (! all (blank, 2));
  keptCols = find (! all (blank, 1));
  if (isempty (keptRows) || isempty (keptCols))
    data = cell (0, 0);
    vtype = cell (0, 0);
    return;
  endif
  rr = keptRows(1):keptRows(end);
  cc = keptCols(1):keptCols(end);
  data = data(rr, cc);
  vtype = vtype(rr, cc);
endfunction

## Reconstruct one column from its data cells and their ODS value types.
function v = reconstruct_column (C, VT, textType)
  seen = VT(! cellfun (@isempty, VT));
  if (isempty (seen))
    kind = 'string';
  else
    kind = seen{1};
  endif
  switch (kind)
    case 'float'
      v = ods_numeric (C);
    case 'boolean'
      v = logical (ods_numeric (C));
    case 'date'
      v = ods_iso2datetime (C);
    case 'time'
      v = ods_iso2duration (C);
    otherwise
      s = ods_strings (C, VT);
      if (strcmpi (textType, 'string'))
        v = string (s);
      else
        v = s;
      endif
  endswitch
endfunction

## Numeric column from ODS cells; missing cells become NaN.
function M = ods_numeric (C)
  M = nan (size (C));
  for i = 1:numel (C)
    if (! isempty (C{i}))
      M(i) = double (C{i});
    endif
  endfor
endfunction

## Cellstr column from ODS cells; numbers are stringified, missing become ''.
function S = ods_strings (C, VT)
  S = cell (size (C));
  for i = 1:numel (C)
    x = C{i};
    if (ischar (x))
      S{i} = x;
    elseif (isempty (x))
      S{i} = '';
    else
      S{i} = num2str (x);
    endif
  endfor
endfunction

## Parse an ISO 8601 date block into a datetime array; empty cells become NaT.
function dt = ods_iso2datetime (C)
  sz = size (C);
  Y = nan (sz);  Mo = nan (sz);  D = nan (sz);
  h = nan (sz);  mi = nan (sz);  s = nan (sz);
  for i = 1:numel (C)
    str = C{i};
    if (ischar (str) && ! isempty (str))
      val = sscanf (str, "%d-%d-%dT%d:%d:%f");
      if (numel (val) == 6)
        Y(i) = val(1);  Mo(i) = val(2);  D(i) = val(3);
        h(i) = val(4);  mi(i) = val(5);  s(i) = val(6);
      endif
    endif
  endfor
  dt = datetime (Y, Mo, D, h, mi, s);
endfunction

## Parse an ISO 8601 duration block (PTnHnMnS) into a duration array; empty
## cells become NaN durations.
function du = ods_iso2duration (C)
  tot = nan (size (C));
  for i = 1:numel (C)
    str = C{i};
    if (ischar (str) && ! isempty (str))
      neg = (str(1) == '-');
      if (neg)
        str(1) = [];
      endif
      tk = regexp (str, '^PT([\d.]+)H([\d.]+)M([\d.]+)S$', 'tokens');
      if (! isempty (tk))
        val = str2double (tk{1}{1}) * 3600 + str2double (tk{1}{2}) * 60 ...
              + str2double (tk{1}{3});
        if (neg)
          val = -val;
        endif
        tot(i) = val;
      endif
    endif
  endfor
  du = seconds (tot);
endfunction

%!demo
%! ## `readtable` is the unified, MATLAB-compatible reader: it picks text vs
%! ## spreadsheet from the file extension.  Here it reads a comma-delimited file,
%! ## taking the first row as variable names and detecting each column's type.
%!
%! T = table ([38; 43], [71.5; 69.0], 'VariableNames', {'Age', 'Height'});
%! filename = fullfile (tempdir (), 'patients.csv');
%! writetable (T, filename);
%! readtable (filename)

%!demo
%! ## Options let you override the defaults — for example read a specific
%! ## spreadsheet `'Sheet'`, or turn the first column into row names.
%!
%! filename = fullfile (tempdir (), 'patients.ods');
%! T = table ([38; 43], 'VariableNames', {'Age'}, 'RowNames', {'Li', 'Diaz'});
%! writetable (T, filename, 'WriteRowNames', true);
%! readtable (filename, 'ReadRowNames', true)
%!
%! delete (filename);

## Read a comma-delimited file with a header row and automatic type detection
%!test
%! fn = [tempname() '.csv'];
%! fid = fopen (fn, 'w');
%! fputs (fid, sprintf ("id,val,name\n1,2.5,foo\n2,3.5,bar\n"));
%! fclose (fid);
%! unwind_protect
%!   t = readtable (fn);
%!   assert_equal (t.Properties.VariableNames, {'id', 'val', 'name'});
%!   assert_equal (t.id, [1; 2]);
%!   assert_equal (t.val, [2.5; 3.5]);
%!   assert_equal (t.name, {'foo'; 'bar'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Read row names from the first column
%!test
%! fn = [tempname() '.csv'];
%! fid = fopen (fn, 'w');
%! fputs (fid, sprintf ("rn,v\nr1,10\nr2,20\n"));
%! fclose (fid);
%! unwind_protect
%!   t = readtable (fn, 'ReadRowNames', true);
%!   assert_equal (t.Properties.RowNames, {'r1'; 'r2'});
%!   assert_equal (t.v, [10; 20]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## A named delimiter selects the field separator
%!test
%! fn = [tempname() '.csv'];
%! fid = fopen (fn, 'w');
%! fputs (fid, sprintf ("A;B\n1;x\n2;y\n"));
%! fclose (fid);
%! unwind_protect
%!   t = readtable (fn, 'Delimiter', 'semi');
%!   assert_equal (t.Properties.VariableNames, {'A', 'B'});
%!   assert_equal (t.A, [1; 2]);
%!   assert_equal (t.B, {'x'; 'y'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## 'TextType' string returns text columns as string arrays
%!test
%! fn = [tempname() '.csv'];
%! fid = fopen (fn, 'w');
%! fputs (fid, sprintf ("A,B\nfoo,bar\n"));
%! fclose (fid);
%! unwind_protect
%!   t = readtable (fn, 'TextType', 'string');
%!   assert_equal (class (t.A), 'string');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## A datetime column that is not the first column is detected as datetime
%!test
%! fn = [tempname() '.csv'];
%! fid = fopen (fn, 'w');
%! fputs (fid, sprintf ("id,d\n1,2024-01-15\n2,2024-02-20\n"));
%! fclose (fid);
%! unwind_protect
%!   t = readtable (fn);
%!   assert_equal (class (t.d), 'datetime');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Hex-like text is kept as text (readtable does not auto-detect hexadecimals)
%!test
%! fn = [tempname() '.csv'];
%! fid = fopen (fn, 'w');
%! fputs (fid, sprintf ("id,code\n1,a\n2,b\n"));
%! fclose (fid);
%! unwind_protect
%!   t = readtable (fn);
%!   assert_equal (t.code, {'a'; 'b'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip a spreadsheet written by writetable, restoring native types
%!test
%! fn = [tempname() '.ods'];
%! T = table ([1; 2], {'x'; 'y'}, datetime (2024, 1, [1; 2]), ...
%!            seconds ([30; 90]), 'VariableNames', {'a', 'b', 'c', 'd'});
%! unwind_protect
%!   writetable (T, fn);
%!   R = readtable (fn);
%!   assert_equal (R.Properties.VariableNames, {'a', 'b', 'c', 'd'});
%!   assert_equal (R.a, [1; 2]);
%!   assert_equal (R.b, {'x'; 'y'});
%!   assert_equal (class (R.c), 'datetime');
%!   assert_equal (isequaln (datevec (R.c), datevec (T.c)), true);
%!   assert_equal (class (R.d), 'duration');
%!   assert_equal (isequaln (seconds (R.d), [30; 90]), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Build a flat '.fods' fixture with three sheets: Alpha (x,y), Beta (p,q), and
## Offset, whose data starts at C2 and is padded with the large repeat counts a
## real spreadsheet emits -- exercising sheet selection, used-block trimming,
## and the trailing-repeat cap.
%!function fn = wr_multisheet ()
%!  fn = [tempname() '.fods'];
%!  sc = @(s) ['<table:table-cell office:value-type="string"><text:p>' s ...
%!             '</text:p></table:table-cell>'];
%!  fc = @(x) sprintf (['<table:table-cell office:value-type="float"' ...
%!             ' office:value="%g"><text:p>%g</text:p></table:table-cell>'], x, x);
%!  rw = @(c) ['<table:table-row>' c '</table:table-row>'];
%!  alpha = ['<table:table table:name="Alpha">' rw([sc('x') sc('y')]) ...
%!           rw([fc(1) fc(10)]) rw([fc(2) fc(20)]) rw([fc(3) fc(30)]) ...
%!           '</table:table>'];
%!  beta = ['<table:table table:name="Beta">' rw([sc('p') sc('q')]) ...
%!          rw([fc(4) fc(40)]) rw([fc(5) fc(50)]) '</table:table>'];
%!  pad = '<table:table-cell table:number-columns-repeated="2"/>';
%!  big = '<table:table-cell table:number-columns-repeated="1000"/>';
%!  offset = ['<table:table table:name="Offset">' ...
%!            '<table:table-row><table:table-cell' ...
%!            ' table:number-columns-repeated="1024"/></table:table-row>' ...
%!            rw([pad sc('a') sc('b') big]) rw([pad fc(7) fc(8)]) ...
%!            '<table:table-row table:number-rows-repeated="500">' ...
%!            '<table:table-cell table:number-columns-repeated="1024"/>' ...
%!            '</table:table-row></table:table>'];
%!  doc = ['<?xml version="1.0" encoding="UTF-8"?>' ...
%!         '<office:document' ...
%!         ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' ...
%!         ' xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"' ...
%!         ' xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"' ...
%!         ' office:mimetype="application/vnd.oasis.opendocument.spreadsheet">' ...
%!         '<office:body><office:spreadsheet>' alpha beta offset ...
%!         '</office:spreadsheet></office:body></office:document>'];
%!  fid = fopen (fn, 'w');
%!  fputs (fid, doc);
%!  fclose (fid);
%!endfunction

## Select a sheet by name
%!test
%! fn = wr_multisheet ();
%! unwind_protect
%!   R = readtable (fn, 'Sheet', 'Beta');
%!   assert_equal (R.Properties.VariableNames, {'p', 'q'});
%!   assert_equal (R.p, [4; 5]);
%!   assert_equal (R.q, [40; 50]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Select a sheet by 1-based index (2 -> Beta)
%!test
%! fn = wr_multisheet ();
%! unwind_protect
%!   R = readtable (fn, 'Sheet', 2);
%!   assert_equal (R.Properties.VariableNames, {'p', 'q'});
%!   assert_equal (R.p, [4; 5]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## A sheet whose data is offset and padded is trimmed to its used block
%!test
%! fn = wr_multisheet ();
%! unwind_protect
%!   R = readtable (fn, 'Sheet', 'Offset');
%!   assert_equal (size (R), [1, 2]);
%!   assert_equal (R.Properties.VariableNames, {'a', 'b'});
%!   assert_equal (R.a, 7);
%!   assert_equal (R.b, 8);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## An explicit 'Range' clips absolutely: with the header row, and without it
%!test
%! fn = wr_multisheet ();
%! unwind_protect
%!   R = readtable (fn, 'Sheet', 'Alpha', 'Range', 'A2:B3', ...
%!                  'ReadVariableNames', false);
%!   assert_equal (R.Var1, [1; 2]);
%!   assert_equal (R.Var2, [10; 20]);
%!   R2 = readtable (fn, 'Sheet', 'Alpha', 'Range', 'A1:B2');
%!   assert_equal (R2.Properties.VariableNames, {'x', 'y'});
%!   assert_equal (R2.x, 1);
%!   assert_equal (R2.y, 10);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Error: a requested sheet that does not exist
%!error <readtable: sheet 'Nope' not found in '.*'.> ...
%! fn = wr_multisheet (); ...
%! readtable (fn, 'Sheet', 'Nope');

## Error: a sheet index past the last data sheet
%!error <readtable: sheet index 9 out of range in '.*'.> ...
%! fn = wr_multisheet (); ...
%! readtable (fn, 'Sheet', 9);

## Error: 'Sheet' / 'Range' rejected on text files
%!error <readtable: 'Sheet' and 'Range' are not supported for text files.> ...
%! readtable ([tempname() '.csv'], 'Sheet', 'A');
%!error <readtable: 'Sheet' and 'Range' are not supported for text files.> ...
%! readtable ([tempname() '.csv'], 'Range', 'A1:B2');

## Round-trip through an '.xlsx' workbook written by writetable, including
## native datetime/duration/boolean columns and sheet/range selection
%!test
%! T = table ([1; 2; 3], {'a'; 'bb'; 'ccc'}, datetime (2024, 3, [1; 15; 31]), ...
%!            seconds ([30; 90; 3661]), [true; false; true], ...
%!            'VariableNames', {'n', 's', 'when', 'dur', 'flag'});
%! fn = [tempname() '.xlsx'];
%! unwind_protect
%!   writetable (T, fn, 'Sheet', 'Data');
%!   R = readtable (fn, 'Sheet', 'Data');
%!   assert_equal (R.Properties.VariableNames, {'n', 's', 'when', 'dur', 'flag'});
%!   assert_equal (R.n, [1; 2; 3]);
%!   assert_equal (R.s, {'a'; 'bb'; 'ccc'});
%!   assert_equal (class (R.when), 'datetime');
%!   assert_equal (isequaln (datevec (R.when), datevec (T.when)), true);
%!   assert_equal (class (R.dur), 'duration');
%!   assert_equal (isequaln (seconds (R.dur), [30; 90; 3661]), true);
%!   assert_equal (R.flag, [true; false; true]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## '.xlsx' 'Range' anchor on write, clipped on read
%!test
%! T = table ([1; 2; 3], {'a'; 'b'; 'c'}, 'VariableNames', {'n', 's'});
%! fn = [tempname() '.xlsx'];
%! unwind_protect
%!   writetable (T, fn, 'Range', 'C3');
%!   R = readtable (fn);
%!   assert_equal (R.n, [1; 2; 3]);
%!   R2 = readtable (fn, 'Range', 'C4:D6', 'ReadVariableNames', false);
%!   assert_equal (R2.Var1, [1; 2; 3]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## '.xlsm' round-trips too
%!test
%! T = table ([7; 8], 'VariableNames', {'v'});
%! fn = [tempname() '.xlsm'];
%! unwind_protect
%!   writetable (T, fn);
%!   assert_equal (readtable (fn).v, [7; 8]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Error: legacy binary Excel formats are not supported
%!error <readtable: '.xls' Excel files are not supported; use '.xlsx', '.ods', or a text format.> ...
%! readtable ('data.xls');
%!error <readtable: '.xlsb' Excel files are not supported; use '.xlsx', '.ods', or a text format.> ...
%! readtable ('data.xlsb');

## Error: an unknown option
%!error <readtable: unknown option 'Bogus'.> ...
%! readtable ([tempname() '.csv'], 'Bogus', 1);
