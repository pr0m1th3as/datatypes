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
## The following @var{Name}-@var{Value} options are supported:
##
## @multitable @columnfractions 0.30 0.70
## @headitem @var{Name} @tab @var{Value}
## @item @qcode{'Sheet'} @tab The sheet to read, selected by its name (a
## character vector or string scalar) or by a 1-based index over the data
## sheets.  The default is the first data sheet.
## @item @qcode{'ReadVariableNames'} @tab A logical scalar specifying whether
## the variable names are taken from the file (default @qcode{true}).  Setting
## it @qcode{false} numbers the variables @qcode{Var1}, @qcode{Var2}, and so on.
## @item @qcode{'ReadRowNames'} @tab A logical scalar specifying whether the
## table takes row names from the file (default @qcode{true}).  Setting it
## @qcode{false} leaves the table without row names.
## @item @qcode{'VariableNamesRow'} @tab A nonnegative integer scalar naming the
## row of the sheet that holds the variable names (default @qcode{1}).  Zero is
## equivalent to setting @qcode{'ReadVariableNames'} to @qcode{false}.  It
## applies only to a sheet with no metadata; a sheet written by
## @code{table2ods} records the names, so there is no row to name.
## @item @qcode{'RowNamesColumn'} @tab A nonnegative integer scalar naming the
## column of the sheet that holds the row names (default @qcode{0}).  Zero is
## equivalent to setting @qcode{'ReadRowNames'} to @qcode{false}.  It applies
## only to a sheet with no metadata, which says nothing about which column holds
## row names; a sheet written by @code{table2ods} records the column.  A leading
## column headed @qcode{Row}, which is what @code{writetable} writes for the row
## names, is taken as the row names without being named here.
## @end multitable
##
## When the file carries the hidden @qcode{__datatypes_meta__} sheet written by
## the @code{table2ods} method, the variable types, descriptions and units are
## restored from it and the variable names from the rows heading the data
## sheet, and @code{date} and @code{time} cells are
## reconstructed as @code{datetime} and @code{duration} arrays.  Integers are
## restored without loss of precision and missing cells become @code{NaN},
## @code{NaT}, or missing strings as appropriate.  A file written before the
## names moved to the data sheet carries them on the metadata sheet, and is
## read just as well.
##
## When the metadata sheet is absent (a spreadsheet written by another
## application) the variable types are inferred from the cell value types.
## Where no names are available the variables are numbered, and the columns can
## then no longer be grouped, so a multicolumn variable comes back as separate
## variables and a nested table as flat columns.
##
## A @code{datetime} or @code{duration} variable is restored exactly, along
## with its @qcode{Format} and, for a zone-aware @code{datetime}, its
## @qcode{TimeZone}.  A cell that carries no value at all is a missing entry,
## which is not the same as a cell holding an empty string.  The following
## round-trip limitation applies, mirroring @code{csv2table}:
## @code{calendarDuration} and @code{categorical} variables are returned as cell
## arrays of character vectors and their values are not reconstructed.
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

  optNames = {'Sheet', 'ReadVariableNames', 'ReadRowNames', ...
              'VariableNamesRow', 'RowNamesColumn'};
  ## A sheet with no metadata says nothing about which column holds row names,
  ## and taking the first one uninvited would leave a single-column sheet with
  ## no variables at all, so the column defaults to none.
  dfValues = {[], true, true, 1, 0};
  [sheet, readVarNames, readRowNames, varNamesRow, rowNamesCol, args] = ...
                        parsePairedArguments (optNames, dfValues, varargin(:));
  if (! (islogical (readVarNames) && isscalar (readVarNames)))
    error ("ods2table: 'ReadVariableNames' must be a logical scalar.");
  endif
  if (! (islogical (readRowNames) && isscalar (readRowNames)))
    error ("ods2table: 'ReadRowNames' must be a logical scalar.");
  endif
  if (! (isnumeric (varNamesRow) && isscalar (varNamesRow)
         && varNamesRow == fix (varNamesRow) && varNamesRow >= 0))
    error (strcat ("ods2table: 'VariableNamesRow' must be a non-negative", ...
                   " integer."));
  endif
  if (! (isnumeric (rowNamesCol) && isscalar (rowNamesCol)
         && rowNamesCol == fix (rowNamesCol) && rowNamesCol >= 0))
    error (strcat ("ods2table: 'RowNamesColumn' must be a non-negative", ...
                   " integer."));
  endif
  ## Either switch turns its position off; the position alone can too.
  if (! readVarNames)
    varNamesRow = 0;
  endif
  if (! readRowNames)
    rowNamesCol = 0;
  endif
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
    tbl = ods_autodetect (data, vtype, varNamesRow, rowNamesCol);
    return;
  endif

  ## Parse the descriptive comment on the metadata sheet
  hdr = sscanf (meta{1,1}, ...
        "# varTypes %d rows; varNames %d rows; varDescriptions %d rows; varUnits %d rows.");
  if (numel (hdr) != 4)
    error ("ods2table: malformed metadata header in '%s'.", file);
  endif
  Trows = hdr(1);  Nrows = hdr(2);  Drows = hdr(3);  Urows = hdr(4);

  ## A table with no variables round-trips to an empty table.  A written table
  ## always has a type row, so an absent one means there was nothing to write;
  ## an absent NAME row means only that the names were not written.
  if (Trows == 0)
    tbl = table ();
    return;
  endif

  ## Split the metadata rows into type, name, description, and unit blocks.
  ## The variable names sit on the data sheet, above the data; a file written
  ## before they moved carries them on the metadata sheet instead, which its
  ## extra block of rows gives away.
  body = meta(2:end,:);
  namesOnMeta = (size (body, 1) == Trows + Nrows + Drows + Urows && Nrows > 0);
  T = body(1:Trows,:);          body(1:Trows,:) = [];
  if (namesOnMeta)
    N = body(1:Nrows,:);        body(1:Nrows,:) = [];
  elseif (Nrows > 0)
    N = data(1:Nrows,:);
    data(1:Nrows,:) = [];       vtype(1:Nrows,:) = [];
  else
    N = {};
  endif
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

  ## A leading RowNames column is tagged in the type row; the column is
  ## consumed either way, and kept only when the caller asked for it.
  RowNames = {};
  if (strcmp (T{1,1}, 'RowNames'))
    if (readRowNames)
      RowNames = ods_column_strings (data(:,1), vtype(:,1));
    endif
    data(:,1) = [];  vtype(:,1) = [];
    T(:,1) = [];
    if (! isempty (N)),  N(:,1) = [];  endif
    if (! isempty (D)),  D(:,1) = [];  endif
    if (! isempty (U)),  U(:,1) = [];  endif
  endif

  ## Without a name block there is nothing to group the columns by, so each
  ## column is one variable, numbered, and carries the innermost type it was
  ## written with.  A nested table cannot be rebuilt without the names that
  ## tagged its columns.
  if (Nrows == 0 || ! readVarNames)
    ncol = size (T, 2);
    varNames = arrayfun (@(c) sprintf ("Var%d", c), 1:ncol, ...
                         'UniformOutput', false);
    varValues = cell (1, ncol);
    for c = 1:ncol
      col = T(:,c);
      col = col(! cellfun (@isempty, col));
      varValues{c} = ods_cell2var (data(:,c), vtype(:,c), col{end});
    endfor
    if (isempty (RowNames))
      tbl = table (varValues{:}, 'VariableNames', varNames);
    else
      tbl = table (varValues{:}, 'VariableNames', varNames, ...
                   'RowNames', RowNames);
    endif
    return;
  endif

  ## Group the columns that share a variable name into one variable, rebuild
  ## any nested table or structure from the deeper header rows, and restore the
  ## descriptions and units.  The value types travel alongside the data so that
  ## the leaf conversion still sees them.
  tbl = __cell2tbl__ (data, T, N, D, U, RowNames, ...
                      @(varC, varVT, typestr) ods_cell2var (varC, varVT, ...
                                                            typestr), vtype);

endfunction

## Reconstruct one table variable (n-by-k) from its data and value-type columns
## and its declared type T.  Missing cells (empty value-type) become NaN, NaT,
## or missing strings according to the type.
function v = ods_cell2var (C, VT, T)
  numvartype = {'double', 'single', 'int8', 'uint8', 'int16', 'uint16', ...
                'int32', 'uint32', 'int64', 'uint64'};
  if (strcmp (T, 'cell'))
    v = C;
  elseif (strcmp (T, 'char'))
    v = char (ods_column_strings (C, VT));
  elseif (strcmp (T, 'missing'))
    v = repmat (missing, size (C, 1), size (C, 2));
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
    ## A zone-aware datetime carries its TimeZone after 'datetime ' and its
    ## display format after a '|'.
    [T, dispfmt] = __typefmt__ (T);
    tz = '';
    if (numel (T) > 9)
      tz = T(10:end);
    endif
    v = __iso2dt__ (C, tz);
    if (! isempty (dispfmt))
      v.Format = dispfmt;
    endif
  elseif (strncmp (T, 'duration', 8))
    [~, dispfmt] = __typefmt__ (T);
    v = __iso2dur__ (C);
    if (! isempty (dispfmt))
      v.Format = dispfmt;
    endif
  elseif (strcmp (T, 'string'))
    ## A cell with no value type at all carries no value: the string is
    ## missing, which is not the same as an empty one.
    blank = cellfun (@isempty, VT);
    v = string (ods_column_strings (C, VT));
    v(blank) = string (missing);
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

## Foreign-file fallback: no metadata sheet, so infer each column's type from
## its cell value types and name the variables Var1, Var2, ...
function tbl = ods_autodetect (data, vtype, varNamesRow, rowNamesCol)
  ## A foreign sheet carries no metadata, so the caller's options say which row
  ## holds the names and which column the row names, a zero meaning neither.
  ## A leading column headed with the row-labels dimension name is what
  ## 'writetable' writes for the row names, so the header declares the column
  ## rather than the caller having to guess at it.
  if (! rowNamesCol && varNamesRow > 0 && size (data, 1) >= varNamesRow
      && ischar (data{varNamesRow,1}) && strcmp (data{varNamesRow,1}, 'Row'))
    rowNamesCol = 1;
  endif

  RowNames = {};
  if (rowNamesCol > 0 && rowNamesCol <= size (data, 2))
    RowNames = ods_column_strings (data(:,rowNamesCol), vtype(:,rowNamesCol));
    data(:,rowNamesCol) = [];  vtype(:,rowNamesCol) = [];
  endif
  ncol = size (data, 2);
  if (varNamesRow > 0 && varNamesRow <= size (data, 1))
    varNames = ods_column_strings (data(varNamesRow,:), vtype(varNamesRow,:));
    empt = cellfun (@isempty, varNames);
    varNames(empt) = arrayfun (@(x) sprintf ("Var%d", x), find (empt), ...
                               'UniformOutput', false);
    varNames = matlab.lang.makeValidName (varNames);
    data(varNamesRow,:) = [];  vtype(varNamesRow,:) = [];
    if (! isempty (RowNames))
      RowNames(varNamesRow) = [];
    endif
  else
    varNames = arrayfun (@(x) sprintf ("Var%d", x), 1:ncol, ...
                         'UniformOutput', false);
  endif
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
        varValues{c} = __iso2dt__ (data(:,c));
      case 'time'
        varValues{c} = __iso2dur__ (data(:,c));
      otherwise
        varValues{c} = ods_column_strings (data(:,c), vtype(:,c));
    endswitch
  endfor
  if (isempty (RowNames))
    tbl = table (varValues{:}, 'VariableNames', varNames);
  else
    tbl = table (varValues{:}, 'VariableNames', varNames, ...
                 'RowNames', RowNames);
  endif
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

## 'WriteVariableNames' false leaves no names in the file at all
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! unwind_protect
%!   table2ods (T, fn, 'WriteVariableNames', false);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.VariableNames, {'Var1', 'Var2'});
%!   assert_equal (R.Var1, [1; 2]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## 'WriteRowNames' false leaves the row labels out
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2], 'VariableNames', {'n'});
%! T.Properties.RowNames = {'r1', 'r2'};
%! unwind_protect
%!   table2ods (T, fn, 'WriteRowNames', false);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.RowNames, {});
%!   assert_equal (R.Properties.VariableNames, {'n'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## 'ReadVariableNames' false numbers the variables of a file that has names
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn, 'ReadVariableNames', false);
%!   assert_equal (R.Properties.VariableNames, {'Var1', 'Var2'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## 'ReadRowNames' false drops the row labels a file carries
%!test
%! fn = [tempname() '.fods'];
%! T = table ([1; 2], 'VariableNames', {'n'});
%! T.Properties.RowNames = {'r1', 'r2'};
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn, 'ReadRowNames', false);
%!   assert_equal (R.Properties.RowNames, {});
%!   assert_equal (R.n, [1; 2]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## A sheet with no metadata takes its names from the first row
%!test
%! fn = [tempname() '.ods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! unwind_protect
%!   writetable (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.Properties.VariableNames, {'n', 's'});
%!   assert_equal (height (R), 2);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## A file written by 'writetable' round-trips, names and values
%!test
%! fn = [tempname() '.ods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! unwind_protect
%!   writetable (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R, T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## A 'writetable' file keeps its row names, the header naming the column
%!test
%! fn = [tempname() '.ods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! T.Properties.RowNames = {'r1', 'r2'};
%! unwind_protect
%!   writetable (T, fn, 'WriteRowNames', true);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R, T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## 'VariableNamesRow' names the row of a sheet that carries the names
%!test
%! fn = [tempname() '.ods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! unwind_protect
%!   writetable (T, fn);
%!   R = ods2table (fn, 'VariableNamesRow', 2);
%!   assert_equal (R.Properties.VariableNames, {'x1', 'a'});
%!   assert_equal (height (R), 2);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## 'VariableNamesRow' zero is the same as 'ReadVariableNames' false
%!test
%! fn = [tempname() '.ods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! unwind_protect
%!   writetable (T, fn);
%!   R = ods2table (fn, 'VariableNamesRow', 0);
%!   assert_equal (R.Properties.VariableNames, {'Var1', 'Var2'});
%!   assert_equal (height (R), 3);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## A sheet with no metadata gives up a column as row names only when named
%!test
%! fn = [tempname() '.ods'];
%! T = table ([1; 2], {'a'; 'b'}, 'VariableNames', {'n', 's'});
%! unwind_protect
%!   writetable (T, fn);
%!   R = ods2table (fn, 'RowNamesColumn', 1);
%!   assert_equal (R.Properties.RowNames, {'1'; '2'});
%!   assert_equal (R.Properties.VariableNames, {'s'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## The variable names sit on the data sheet, where a reader of the file sees
## them, and a nested table's take one row per level
%!test
%! fn = [tempname() '.fods'];
%! inner = table ([3; 4], [5; 6], 'VariableNames', {'p', 'q'});
%! T = table ([1; 2], inner, 'VariableNames', {'a', 'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   txt = fileread (fn);
%!   ix = strfind (txt, 'table:name="__datatypes_meta__"');
%!   visible = txt(1:ix-1);
%!   assert_equal (! isempty (strfind (visible, '<text:p>a</text:p>')), true);
%!   assert_equal (! isempty (strfind (visible, '<text:p>p</text:p>')), true);
%!   hidden = txt(ix:end);
%!   assert_equal (isempty (strfind (hidden, '<text:p>p</text:p>')), true);
%!   assert_equal (isequaln (ods2table (fn), T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a nested table, values and metadata
%!test
%! fn = [tempname() '.fods'];
%! inner = table ([3; 4], [5; 6], 'VariableNames', {'p', 'q'});
%! inner.Properties.VariableUnits = {'m', 's'};
%! T = table ([1; 2], inner, 'VariableNames', {'a', 'v'});
%! T.Properties.VariableUnits = {'kg', ''};
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R, T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a nested table in a zipped '.ods' package
%!test
%! fn = [tempname() '.ods'];
%! inner = table ([3; 4], [5; 6], 'VariableNames', {'p', 'q'});
%! T = table ([1; 2], inner, 'VariableNames', {'a', 'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R, T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a struct variable becomes one column per field
%!test
%! fn = [tempname() '.fods'];
%! T = table ([9; 8], struct ('f', {1; 2}, 'g', {3; 4}), ...
%!            'VariableNames', {'a', 'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R, T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a nested table alongside a multicolumn variable and row names
%!test
%! fn = [tempname() '.fods'];
%! inner = table ([3; 4], [5; 6], 'VariableNames', {'p', 'q'});
%! T = table ([1; 2], inner, [7, 8; 9, 10], 'VariableNames', {'a', 'v', 'm'});
%! T.Properties.RowNames = {'r1', 'r2'};
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R, T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a nested table holding a datetime and a duration
%!test
%! fn = [tempname() '.fods'];
%! inner = table (datetime (2024, 1, [1; 2]), hours ([1; 2]), ...
%!                'VariableNames', {'d', 'u'});
%! T = table ([1; 2], inner, 'VariableNames', {'a', 'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R, T), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a char matrix keeps its padding
%!test
%! fn = [tempname() '.fods'];
%! T = table (['ab '; 'c  '], 'VariableNames', {'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.v, ['ab '; 'c  ']);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a missing string stays missing, an empty one stays empty
%!test
%! fn = [tempname() '.fods'];
%! T = table ([string('a'); string(missing); string('')], 'VariableNames', {'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (ismissing (R.v), [false; true; false]);
%!   assert_equal (char (R.v(3)), '');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a double is restored bit for bit
%!test
%! fn = [tempname() '.fods'];
%! x = [pi; 0.1; realmin; eps; 1e308; -1/3];
%! T = table (x, 'VariableNames', {'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (R.v, x);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a datetime keeps its display format
%!test
%! fn = [tempname() '.fods'];
%! d = datetime ({'2020-01-02 03:04:05'; '2021-03-04 05:06:07'});
%! d.Format = 'dd/MM/yyyy';
%! T = table (d, 'VariableNames', {'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R.v, d), true);
%!   assert_equal (R.v.Format, 'dd/MM/yyyy');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Round-trip: a duration keeps its display format
%!test
%! fn = [tempname() '.fods'];
%! u = hours ([1; 2]);
%! T = table (u, 'VariableNames', {'v'});
%! unwind_protect
%!   table2ods (T, fn);
%!   R = ods2table (fn);
%!   assert_equal (isequaln (R.v, u), true);
%!   assert_equal (R.v.Format, 'h');
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

## A sheet with no metadata sheet: types inferred, names from the header row
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
%!   assert_equal (R.Properties.VariableNames, {'x', 'g'});
%!   assert_equal (R.x, [1; 2; 3]);
%!   assert_equal (class (R.g), 'cell');
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
%!   Rdef = ods2table (fn, 'ReadVariableNames', false);
%!   assert_equal (Rdef.Var1, 11);      # the default reads the first sheet
%!   Rname = ods2table (fn, 'Sheet', 'Two', 'ReadVariableNames', false);
%!   assert_equal (Rname.Var1, 22);
%!   Ridx = ods2table (fn, 'Sheet', 2, 'ReadVariableNames', false);
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
%!error <ods2table: unknown option 'Nope'.> ...
%! ods2table ('x.ods', 'Nope', 1)
## a categorical or calendarDuration column is written as text and warns on
## the way back, the strings not being converted to the original type
%!warning <ods2table: 'categorical' strings are not converted.> ...
%! T = table (categorical ({'a'; 'b'}), 'VariableNames', {'c'});
%! fn = [tempname() '.ods'];
%! unwind_protect
%!   table2ods (T, fn);
%!   ods2table (fn);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect
%!warning <ods2table: 'calendarDuration' strings are not converted.> ...
%! T = table (calendarDuration ([1; 2], [0; 0], [0; 0]), 'VariableNames', {'c'});
%! fn = [tempname() '.ods'];
%! unwind_protect
%!   table2ods (T, fn);
%!   ods2table (fn);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect
