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
## @deftypefn  {datatypes} {@var{tbl} =} ods2table (@var{filename})
## @deftypefnx {datatypes} {@var{tbl} =} ods2table (@var{filename}, @qcode{'Sheet'}, @var{sheet})
##
## Read an OpenDocument spreadsheet file into a table.
##
## @code{@var{tbl} = ods2table (@var{filename})} reads the OpenDocument
## spreadsheet named by @var{filename}, which may be a character vector, a
## cellstr, or a string scalar, and returns it as a @code{table}.  Both the
## compressed @qcode{.ods} and the flat @qcode{.fods} formats are read; the
## format is detected from the file contents, not its extension.
##
## @code{@var{tbl} = ods2table (@dots{}, @qcode{'Sheet'}, @var{sheet})} reads a
## specific sheet, selected either by its name (a character vector or string
## scalar) or by a 1-based index over the data sheets.  Without this option the
## first data sheet is read.
##
## When the file carries the hidden @qcode{__datatypes_meta__} sheet written by
## the @code{table2ods} method, the variable types, names, descriptions, and
## units are restored from it, and @code{date} and @code{time} cells are
## reconstructed as @code{datetime} and @code{duration} arrays.  Integers are
## restored without loss of precision and missing cells become @code{NaN},
## @code{NaT}, or missing strings as appropriate.
##
## When the metadata sheet is absent (a spreadsheet written by another
## application) the variable types are inferred from the cell value types and
## the variables are named @qcode{Var1}, @qcode{Var2}, and so on.
##
## The following round-trip limitations apply, mirroring @code{csv2table}:
## @code{calendarDuration} and @code{categorical} variables are returned as cell
## arrays of character vectors (their values are not reconstructed), missing
## @code{string} values are read back as empty strings, and datetime and
## duration display formats are not preserved, although the values themselves
## are exact.
##
## @end deftypefn

function tbl = ods2table (filename, varargin)

  if (nargin < 1)
    print_usage ();
  endif
  if (! (ischar (filename) || iscellstr (filename) || isa (filename, 'string')))
    error ("ods2table: FILENAME must be a character vector, cellstr, or string.");
  endif
  file = char (cellstr (filename));

  optNames = {'Sheet'};
  dfValues = {[]};
  [sheet, args] = parsePairedArguments (optNames, dfValues, varargin(:));
  if (! isempty (args))
    error ("ods2table: unknown option '%s'.", args{1});
  endif
  if (! (isempty (sheet) || (ischar (sheet) && isrow (sheet)) ...
         || (isa (sheet, 'string') && isscalar (sheet)) ...
         || (isnumeric (sheet) && isscalar (sheet))))
    error (strcat ("ods2table: 'Sheet' must be a sheet name or a scalar", ...
                   " index."));
  endif
  if (isa (sheet, 'string'))
    sheet = char (sheet);
  endif

  ## Read the workbook into raw grids
  [data, vtype, meta] = __ods2table__ (file, sheet);
  if (ischar (data))
    error ("ods2table: %s", data);
  endif

  ## No metadata sheet -> infer everything from the data cell value types
  if (isempty (meta))
    tbl = ods_autodetect (data, vtype);
    return;
  endif

  ## Parse the descriptive comment on the metadata sheet
  hdr = sscanf (meta{1,1}, ...
        "# varTypes %d rows; varNames %d rows; varDescriptions %d rows; varUnits %d rows.");
  if (numel (hdr) != 4)
    error ("ods2table: malformed metadata header in '%s'.", file);
  endif
  Trows = hdr(1);  Nrows = hdr(2);  Drows = hdr(3);  Urows = hdr(4);

  ## A table with no variables round-trips to an empty table
  if (Nrows == 0)
    tbl = table ();
    return;
  endif

  ## Split the metadata rows into type, name, description, and unit blocks
  body = meta(2:end,:);
  T = body(1:Trows,:);          body(1:Trows,:) = [];
  N = body(1:Nrows,:);          body(1:Nrows,:) = [];
  if (Drows)
    D = body(1:Drows,:);        body(1:Drows,:) = [];
  else
    D = {};
  endif
  if (Urows)
    U = body(1:Urows,:);
  else
    U = {};
  endif

  ## When the table has no rows the data sheet is empty, so the data grid comes
  ## back without columns; restore the expected column count from the metadata.
  metacols = size (T, 2);
  if (size (data, 2) != metacols)
    data = cell (size (data, 1), metacols);
    vtype = cell (size (data, 1), metacols);
  endif

  ## A leading RowNames column is tagged in the type row with an empty name
  RowNames = {};
  if (strcmp (T{1,1}, 'RowNames') && isempty (N{1,1}))
    RowNames = ods_column_strings (data(:,1), vtype(:,1));
    data(:,1) = [];  vtype(:,1) = [];
    T(:,1) = [];     N(:,1) = [];
    if (! isempty (D)),  D(:,1) = [];  endif
    if (! isempty (U)),  U(:,1) = [];  endif
  endif

  ## Group consecutive columns that share a variable name (multicolumn
  ## variables), then reconstruct each variable from its declared type.
  names = N(1,:);
  ncol = numel (names);
  varNames = {};
  varValues = {};
  descr = {};
  units = {};
  c = 1;
  while (c <= ncol)
    c2 = c;
    while (c2 < ncol && strcmp (names{c2+1}, names{c}))
      c2 += 1;
    endwhile
    idx = c:c2;
    varNames{end+1} = names{c};
    varValues{end+1} = ods_cell2var (data(:,idx), vtype(:,idx), T{1,c});
    if (! isempty (D)),  descr{end+1} = D{1,c};  endif
    if (! isempty (U)),  units{end+1} = U{1,c};  endif
    c = c2 + 1;
  endwhile

  if (isempty (RowNames))
    tbl = table (varValues{:}, 'VariableNames', varNames);
  else
    tbl = table (varValues{:}, 'VariableNames', varNames, 'RowNames', RowNames);
  endif
  if (! isempty (descr))
    tbl.Properties.VariableDescriptions = descr;
  endif
  if (! isempty (units))
    tbl.Properties.VariableUnits = units;
  endif

endfunction

## Reconstruct one table variable (n-by-k) from its data and value-type columns
## and its declared type T.  Missing cells (empty value-type) become NaN, NaT,
## or missing strings according to the type.
function v = ods_cell2var (C, VT, T)
  numvartype = {'double', 'single', 'int8', 'uint8', 'int16', 'uint16', ...
                'int32', 'uint32', 'int64', 'uint64'};
  if (strcmp (T, 'cell'))
    v = C;
  elseif (strcmp (T, 'logical'))
    v = logical (cell2mat (C));
  elseif (ismember (T, numvartype))
    M = ods_column_numeric (C);
    if (any (cellfun (@(x) isinteger (x), C(:))))
      v = cellfun (@(x) cast (x, T), num2cell (M));  # element-wise, mixed types
    else
      v = cast (M, T);
    endif
  elseif (strncmp (T, 'datetime', 8))
    ## A zone-aware datetime carries its TimeZone after 'datetime '.
    tz = '';
    if (numel (T) > 9)
      tz = T(10:end);
    endif
    v = ods_iso2datetime (C, tz);
  elseif (strcmp (T, 'duration'))
    v = ods_iso2duration (C);
  elseif (strcmp (T, 'string'))
    v = string (ods_column_strings (C, VT));
  elseif (strcmp (T, 'calendarDuration'))
    warning ("ods2table: 'calendarDuration' strings are not converted.");
    v = ods_column_strings (C, VT);
  elseif (strcmp (T, 'categorical'))
    warning ("ods2table: 'categorical' strings are not converted.");
    v = ods_column_strings (C, VT);
  else
    v = ods_column_strings (C, VT);
  endif
endfunction

## Build a numeric matrix from a data cell block, mapping missing cells to NaN.
function M = ods_column_numeric (C)
  M = nan (size (C));
  for i = 1:numel (C)
    if (! isempty (C{i}))
      M(i) = double (C{i});
    endif
  endfor
endfunction

## Build a cellstr block from a data/value-type cell block: missing cells and
## numeric cells are coerced to text; genuine empty-string cells stay empty.
function S = ods_column_strings (C, VT)
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

## Parse an ISO 8601 data block into a datetime array; empty cells become NaT.
## A non-empty TZ restores the datetime's TimeZone (the ISO strings are the
## wall-clock time in that zone).
function dt = ods_iso2datetime (C, tz = '')
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
  if (isempty (tz))
    dt = datetime (Y, Mo, D, h, mi, s);   # NaN components yield NaT
  else
    dt = datetime (Y, Mo, D, h, mi, s, 'TimeZone', tz);
  endif
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
        H = str2double (tk{1}{1});
        M = str2double (tk{1}{2});
        S = str2double (tk{1}{3});
        val = H * 3600 + M * 60 + S;
        if (neg)
          val = -val;
        endif
        tot(i) = val;
      endif
    endif
  endfor
  du = seconds (tot);                   # NaN yields a missing duration
endfunction

## Foreign-file fallback: no metadata sheet, so infer each column's type from
## its cell value types and name the variables Var1, Var2, ...
function tbl = ods_autodetect (data, vtype)
  ncol = size (data, 2);
  varNames = arrayfun (@(x) sprintf ("Var%d", x), 1:ncol, ...
                       'UniformOutput', false);
  varValues = cell (1, ncol);
  for c = 1:ncol
    vt = vtype(:,c);
    seen = vt(! cellfun (@isempty, vt));
    if (isempty (seen))
      kind = 'string';
    else
      kind = seen{1};
    endif
    switch (kind)
      case 'float'
        varValues{c} = ods_column_numeric (data(:,c));
      case 'boolean'
        varValues{c} = logical (ods_column_numeric (data(:,c)));
      case 'date'
        varValues{c} = ods_iso2datetime (data(:,c));
      case 'time'
        varValues{c} = ods_iso2duration (data(:,c));
      otherwise
        varValues{c} = ods_column_strings (data(:,c), vtype(:,c));
    endswitch
  endfor
  tbl = table (varValues{:}, 'VariableNames', varNames);
endfunction

%!demo
%! ## `ods2table` reads an OpenDocument spreadsheet into a table.  With no options
%! ## it reads the first data sheet.
%!
%! T = table ([38; 43], [71; 69], 'VariableNames', {'Age', 'Height'});
%! filename = fullfile (tempdir (), 'patients.ods');
%! table2ods (T, filename);
%! ods2table (filename)

%!demo
%! ## Point `'Sheet'` at a specific sheet, by name or by 1-based index, to read
%! ## one page of a multi-sheet workbook.
%!
%! filename = fullfile (tempdir (), 'workbook.ods');
%! table2ods (table ([38; 43], 'VariableNames', {'Age'}), filename, 'Sheet', 'Patients');
%! table2ods (table ([1; 2; 3], 'VariableNames', {'Visit'}), filename, 'Sheet', 'Visits');
%! ods2table (filename, 'Sheet', 'Visits')
%!
%! delete (filename);

## Round-trip: numeric double and cellstr text
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2; 3], {'a'; 'b'; 'c'}, 'VariableNames', {'x', 'g'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.VariableNames, {'x', 'g'});
%!   assert_equal (R.x, [1; 2; 3]);
%!   assert_equal (R.g, {'a'; 'b'; 'c'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: full-range int64/uint64 preserved without precision loss
%!test
%! fn = [tempname() '.fods'];
%! T = table (int64 ([9223372036854775807; -5; 0]), ...
%!            uint64 ([18446744073709551615; 1; 0]), ...
%!            'VariableNames', {'i', 'u'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (class (R.i), 'int64');
%!   assert_equal (class (R.u), 'uint64');
%!   assert_equal (R.i, int64 ([9223372036854775807; -5; 0]));
%!   assert_equal (R.u, uint64 ([18446744073709551615; 1; 0]));
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: narrow integer types cast back from the declared metadata type
%!test
%! fn = [tempname() '.fods'];
%! T = table (int8 ([1; -2]), uint16 ([3; 4]), single ([1.5; 2.5]), ...
%!            'VariableNames', {'a', 'b', 'c'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (class (R.a), 'int8');
%!   assert_equal (class (R.b), 'uint16');
%!   assert_equal (class (R.c), 'single');
%!   assert_equal (R.a, int8 ([1; -2]));
%!   assert_equal (R.c, single ([1.5; 2.5]));
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: logical variable
%!test
%! fn = [tempname() '.fods'];
%! T = table ([true; false; true], 'VariableNames', {'flag'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (class (R.flag), 'logical');
%!   assert_equal (R.flag, [true; false; true]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: datetime with a NaT, restored to native datetime
%!test
%! fn = [tempname() '.fods'];
%! d = [datetime(2024, 1, 15, 10, 30, 15.5); NaT; datetime(1999, 12, 31, 23, 59, 59)];
%! T = table (d, 'VariableNames', {'when'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (class (R.when), 'datetime');
%!   assert_equal (isequaln (datevec (R.when), datevec (d)), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: duration with negative, sub-second, over-24h, and zero values
%!test
%! fn = [tempname() '.fods'];
%! du = [duration(25, 30, 15.5); duration(-1, -30, 0); duration(0, 0, 0)];
%! T = table (du, 'VariableNames', {'elapsed'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (class (R.elapsed), 'duration');
%!   assert_equal (isequaln (seconds (R.elapsed), seconds (du)), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: missing duration (NaN) restored as a missing duration
%!test
%! fn = [tempname() '.fods'];
%! du = [duration(1, 0, 0); duration(NaN, 0, 0)];
%! T = table (du, 'VariableNames', {'d'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isnan (seconds (R.d)), [false; true]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: string variable
%!test
%! fn = [tempname() '.fods'];
%! T = table (string ({'x'; 'y'; 'z'}), 'VariableNames', {'s'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (class (R.s), 'string');
%!   assert_equal (R.s, string ({'x'; 'y'; 'z'}));
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: missing numeric value restored as NaN
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; NaN; 3], 'VariableNames', {'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.v, [1; NaN; 3]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: row names
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2; 3], 'VariableNames', {'v'}, 'RowNames', {'r1', 'r2', 'r3'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.RowNames, {'r1'; 'r2'; 'r3'});
%!   assert_equal (R.v, [1; 2; 3]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: multicolumn variable kept as one matrix-valued variable
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1 2; 3 4; 5 6], {'p'; 'q'; 'r'}, 'VariableNames', {'mat', 'tag'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (size (R.mat), [3, 2]);
%!   assert_equal (R.mat, [1 2; 3 4; 5 6]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: descriptions and units restored when every variable carries one
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 'g'});
%! T.Properties.VariableDescriptions = {'count', 'group'};
%! T.Properties.VariableUnits = {'kg', '-'};
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.VariableDescriptions, {'count', 'group'});
%!   assert_equal (R.Properties.VariableUnits, {'kg', '-'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: descriptions/units written when only some variables carry one
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2], [3; 4], 'VariableNames', {'a', 'b'});
%! T.Properties.VariableDescriptions = {'first', ''};
%! T.Properties.VariableUnits = {'', 'kg'};
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.VariableDescriptions, {'first', ''});
%!   assert_equal (R.Properties.VariableUnits, {'', 'kg'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a zone-aware datetime keeps its TimeZone
%!test
%! fn = [tempname() '.ods'];
%! dt = datetime (2024, 6, [15; 16], 10, 30, 0, 'TimeZone', 'America/New_York');
%! T = table (dt, 'VariableNames', {'t'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.t.TimeZone, 'America/New_York');
%!   assert_equal (cellstr (char (R.t)), cellstr (char (dt)));
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Foreign spreadsheet with no metadata sheet: infer types, name Var1, Var2, ...
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2; 3], {'a'; 'b'; 'c'}, 'VariableNames', {'x', 'g'});
%! unwind_protect
%!   table2ods (T, fn);
%!   txt = fileread (fn);
%!   txt = regexprep (txt, ...
%!         '<table:table table:name="__datatypes_meta__".*?</table:table>', '');
%!   fid = fopen (fn, 'w');  fputs (fid, txt);  fclose (fid);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.VariableNames, {'Var1', 'Var2'});
%!   assert_equal (R.Var1, [1; 2; 3]);
%!   assert_equal (class (R.Var2), 'cell');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Select a specific sheet by name and by 1-based index from a multi-sheet file
%!test
%! fn = [tempname() '.fods'];
%! doc = ['<?xml version="1.0" encoding="UTF-8"?><office:document' ...
%!        ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' ...
%!        ' xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"' ...
%!        ' xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"' ...
%!        ' office:mimetype="application/vnd.oasis.opendocument.spreadsheet">' ...
%!        '<office:body><office:spreadsheet>' ...
%!        '<table:table table:name="One"><table:table-row>' ...
%!        '<table:table-cell office:value-type="float" office:value="11">' ...
%!        '<text:p>11</text:p></table:table-cell></table:table-row></table:table>' ...
%!        '<table:table table:name="Two"><table:table-row>' ...
%!        '<table:table-cell office:value-type="float" office:value="22">' ...
%!        '<text:p>22</text:p></table:table-cell></table:table-row></table:table>' ...
%!        '</office:spreadsheet></office:body></office:document>'];
%! fid = fopen (fn, 'w');  fputs (fid, doc);  fclose (fid);
%! unwind_protect
%!   Rdef = ods2table (fn);                    # default reads the first sheet
%!   assert_equal (Rdef.Var1, 11);
%!   Rname = ods2table (fn, 'Sheet', 'Two');
%!   assert_equal (Rname.Var1, 22);
%!   Ridx = ods2table (fn, 'Sheet', 2);
%!   assert_equal (Ridx.Var1, 22);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Error: a requested sheet that does not exist
%!error <ods2table: sheet 'X' not found in '.*'.> ...
%! fn = [tempname() '.fods']; ...
%! doc = ['<?xml version="1.0" encoding="UTF-8"?><office:document' ...
%!        ' xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0"' ...
%!        ' xmlns:table="urn:oasis:names:tc:opendocument:xmlns:table:1.0"' ...
%!        ' office:mimetype="application/vnd.oasis.opendocument.spreadsheet">' ...
%!        '<office:body><office:spreadsheet><table:table table:name="One"/>' ...
%!        '</office:spreadsheet></office:body></office:document>']; ...
%! fid = fopen (fn, 'w'); fputs (fid, doc); fclose (fid); ...
%! ods2table (fn, 'Sheet', 'X');

## Error: 'Sheet' of an invalid type
%!error <ods2table: 'Sheet' must be a sheet name or a scalar index.> ...
%! ods2table ([tempname() '.fods'], 'Sheet', {1, 2});

## Error: FILENAME of the wrong type
%!error <ods2table: FILENAME must be a character vector, cellstr, or string.> ...
%! ods2table (42);

## Error: a file that cannot be read as an OpenDocument spreadsheet
%!error <ods2table: cannot read> ...
%! ods2table ([tempname() '.fods']);

## Round-trip: a table with no rows preserves its variable types
%!test
%! fn = [tempname() '.fods'];
%! T = table (zeros (0, 1), int64 (zeros (0, 1)), datetime (zeros (0, 1), 1, 1), ...
%!            seconds (zeros (0, 1)), string (cell (0, 1)), ...
%!            'VariableNames', {'d', 'i', 'dt', 'du', 's'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (size (R), [0, 5]);
%!   assert_equal (class (R.i), 'int64');
%!   assert_equal (class (R.dt), 'datetime');
%!   assert_equal (class (R.du), 'duration');
%!   assert_equal (class (R.s), 'string');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a table with no variables comes back as an empty table
%!test
%! fn = [tempname() '.fods'];
%! unwind_protect
%!   table2ods (table (), fn);
%!   R = ods2table (fn);
%!   assert_equal (istable (R), true);
%!   assert_equal (size (R), [0, 0]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: XML metacharacters in a string are escaped and restored
%!test
%! fn = [tempname() '.fods'];
%! T = table ({'a<b>&"c"'; 'x&y'}, 'VariableNames', {'s'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.s, {'a<b>&"c"'; 'x&y'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: leading and trailing whitespace in a string is preserved
%!test
%! fn = [tempname() '.fods'];
%! T = table ({'  pad  '; ''}, 'VariableNames', {'s'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.s, {'  pad  '; ''});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: infinite numeric values are preserved
%!test
%! fn = [tempname() '.fods'];
%! T = table ([Inf; -Inf; 1.5], 'VariableNames', {'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.v, [Inf; -Inf; 1.5]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip through the compressed '.ods' (ZIP) container
%!test
%! fn = [tempname() '.ods'];
%! d = [datetime(2024, 1, 15, 10, 30, 15.5); NaT];
%! T = table ([1; 2], int64 ([9223372036854775807; -5]), d, ...
%!            seconds ([90; 3661]), {'a<b>&"c"'; 'y'}, ...
%!            'VariableNames', {'x', 'big', 'when', 'dur', 's'});
%! T.Properties.RowNames = {'r1', 'r2'};
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.RowNames, {'r1'; 'r2'});
%!   assert_equal (R.x, [1; 2]);
%!   assert_equal (R.big, int64 ([9223372036854775807; -5]));
%!   assert_equal (class (R.when), 'datetime');
%!   assert_equal (isequaln (datevec (R.when), datevec (d)), true);
%!   assert_equal (isequaln (seconds (R.dur), [90; 3661]), true);
%!   assert_equal (R.s, {'a<b>&"c"'; 'y'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect
