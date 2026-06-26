## Copyright (C) 2025-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn  {datatypes} {@var{tbl} =} csv2table (@var{filename})
## @deftypefnx {datatypes} {@var{tbl} =} csv2table (@var{filename}, @var{Name}, @var{Value})
##
## Load a CSV file into a table.
##
## @code{@var{tbl} = csv2table (@var{filename})} creates a table @var{tbl} by
## reading the data in CSV file specified by @var{filename}, which can be either
## character vector or a string scalar.  If the CSV file was saved by the
## @code{table2csv} method, then it reads the custom comment line in the first
## element of the CSV file and reconstructs the table as specified including
## variable types, variable names, and possibly any nested tables or structures
## it may contain.  If no such special comment is found, then it treats the CSV
## file as simple columnar data and loads with the following default options.
##
## @enumerate
## @item The first column is considered as @qcode{RowNames} and it is converted
## to cell array of character vectors and it is subsequently removed from the
## remaining data contained in the CSV file.  To change the default behavior,
## you need to specify the @qcode{ReadRowNames} and @qcode{RowNamesColumn}
## paired arguments accordingly.
## @item The first line is treated as a header containing the variable names of
## the table.  Any numeric values in the header line are converted to character
## vectors and all variable names are automatically modified to valid Octave
## variable names.  To change the default behavior, you need to specify the
## @qcode{ReadVariableNames}, @qcode{VariableNamingRule}, and
## @qcode{VariableNamesLine} paired arguments accordingly.
## @item The data type of each column in the remaining data is automatically
## detected according to its contents.  Consequently, text is converted to
## character vectors, datetime and duration strings are converted to datetime
## and duration arrays, respectively, and hexadecimal strings are converted to
## the smallest integer type that can represent all variable values.  To change
## the default behavior, you need to specify the @qcode{TextType},
## @qcode{DatetimeType}, @qcode{DurationType}, and @qcode{HexType} accordingly.
## @end enumerate
##
## @code{@var{tbl} = csv2table (@var{filename}, @var{Name}, @var{Value})}
## specifies optional parameters for creating the table @var{tbl} with the
## following Name-Value paired arguments.
##
## @multitable @columnfractions 0.33 0.02 0.65
## @headitem @var{Name} @tab @tab @var{Value}
##
## @item @qcode{'NumHeaderLines'} @tab @tab A positive integer scalar value
## specifying the number of rows to omit reading from the CSV file.  If the CSV
## contains the custom comment line in its first element, then the
## @qcode{'NumHeaderLines'} applies to the top number of rows to remove from the
## created table.  If CSV is a generic case, then @qcode{'NumHeaderLines'}
## specifies the number of lines to omit parsing from the CSV file itself.
##
## @item @qcode{'VariableNames'} @tab @tab A cell array of character vectors or
## a string array specifying the names of the variables of the created table.
## The names must be unique but not necessarily valid variable names.  If not
## empty, it overrides any variable names extracted from the CSV file.  This
## applies both to custom and generic CSV files.
##
## @item @qcode{'ReadVariableNames'} @tab @tab A logical scalar specifying
## whether to parse or not the CSV files for variable names.  If your CSV file
## only contains data, set @qcode{'ReadVariableNames'} to @qcode{false} so that
## @code{csv2table} parses all lines as data rows in the returned table.
## Variable names will be automatically generated to the default style as done
## by @code{table}, unless otherwise specified by the @qcode{'VariableNames'}
## paired argument.  The default value is @qcode{true}.
##
## @item @qcode{'VariableNamingRule'} @tab @tab A character vector or a string
## scalar specifying whether the variable names should be modified to valid
## Octave variable names or the original names should be preserved.  Valid
## options are @qcode{"modify"} and @qcode{"preserve"}.  By default,
## @code{csv2table} modifies the parsed variable names.
##
## @item @qcode{'VariableNamesLine'} @tab @tab A nonnegative integer scalar
## value specifying the line number in the CSV file, which should be parsed for
## variable names.  This only applies if the @qcode{'ReadVariableNames'} option
## is @qcode{true}.  The specified line is subsequently removed from the
## remaining data contained in the CSV file.  If @qcode{'VariableNamesLine'} is
## set to zero, then it is equivalent to setting @qcode{'ReadVariableNames'} to
## @qcode{false}.
##
## @item @qcode{'VariableTypes'} @tab @tab A cell array of character vectors or
## a string array specifying the data type of the variables of the created
## table.  The number of elements must much the number of variable in the table.
## This optional argument only has an effect on generic CSV files.  When
## specified, it overrides any other data type specification or automatic
## detection by the @code{csv2table} function.
##
## @item @qcode{'VariableUnitsLine'} @tab @tab A nonnegative integer scalar
## value specifying the line number in the CSV file, which should be parsed for
## variable units.  The specified line is subsequently removed from the
## remaining data contained in the CSV file.
##
## @item @qcode{'VariableDescriptionsLine'} @tab @tab A nonnegative integer
## scalar value specifying the line number in the CSV file, which should be
## parsed for variable descriptions.  The specified line is subsequently removed
## from the remaining data contained in the CSV file.
##
## @item @qcode{'ReadRowNames'} @tab @tab A logical scalar specifying whether to
## parse or not the CSV files for row names.  If your CSV file only contains
## data, set @qcode{'ReadRowNames'} to @qcode{false} so that @code{csv2table}
## parses all columns as data columms in the returned table.  The default value
## is @qcode{true}.
##
## @item @qcode{'RowNamesColumn'} @tab @tab A nonnegative integer scalar value
## specifying the column number in the CSV file, which should be parsed for
## row names.  This only applies if the @qcode{'ReadRowNames'} option is
## @qcode{true}.  The specified column is subsequently removed from the
## remaining data contained in the CSV file.  If @qcode{'RowNamesColumn'} is set
## to zero, then it is equivalent to setting @qcode{'ReadRowNames'} to
## @qcode{false}.  Note that the values in the column specified by
## @qcode{'RowNamesColumn'} must be unique, otherwise @code{csv2table} will
## return an error.
##
## @item @qcode{'TextType'} @tab @tab A character vector or a string scalar
## specifying whether the text data in the CSV file should be stored in the
## table as character vectors or string arrays.  Valid options are
## @qcode{"char"} and @qcode{"string"}.  By default, @code{csv2table} stores
## text data as character vectors.
##
## @item @qcode{'DatetimeType'} @tab @tab A character vector or a string scalar
## specifying whether the datetime strings found in the CSV file should be
## stored in the table as datetime arrays or as text data.  Valid options are
## @qcode{"datetime"} and @qcode{"text"}.  By default, @code{csv2table} stores
## datetime strings as datetime arrays.  If @qcode{"text"} is specified, then
## the data type depends on the @qcode{'TextType'} option.
##
## @item @qcode{'DurationType'} @tab @tab A character vector or a string scalar
## specifying whether the duration strings found in the CSV file should be
## stored in the table as duration arrays or as text data.  Valid options are
## @qcode{"duration"} and @qcode{"text"}.  By default, @code{csv2table} stores
## duration strings as duration arrays.  If @qcode{"text"} is specified, then
## the data type depends on the @qcode{'TextType'} option.
##
## @item @qcode{'HexType'} @tab @tab A character vector or a string scalar
## specifying whether the hexadecimal text found in the CSV file should be
## stored as a suitable integer type, @qcode{"auto"} (default), as unaltered
## input text, @qcode{"text"}, (in which case the data type depends on the
## @qcode{'TextType'} option), or as any of the integer types supported by
## Octave.  Valid options are @qcode{"auto"}, @qcode{"text"}, @qcode{"int8"},
## @qcode{"int16"}, @qcode{"int32"}, @qcode{"int64"}, @qcode{"uint8"},
## @qcode{"uint16"}, @qcode{"uint32"}, and @qcode{"uint64"}.
## @end multitable
##
## @seealso{array2table, struct2table, table}
## @end deftypefn
function tbl = csv2table (name, varargin)

  ## Check first input is a character vector or a string scalar
  if (ischar (name) && isvector (name))
    C = __csv2table__ (name);
  elseif (isa (name, 'string') && isscalar (name))
    C = __csv2table__ (char (name));
  else
    error ("csv2table: NAME must be a character vector or a string scalar.");
  endif
  if (ischar (C))
    error ("csv2table: %s.", C);
  endif

  ## Get first comment line (as saved by 'table2csv' method).  A generic CSV
  ## may carry a number in its first cell, so only attempt to split a string.
  if (ischar (C{1,1}))
    H_key = strsplit (C{1,1});
  else
    H_key = {};
  endif
  if (numel (H_key) == 13)
    H_txt = strjoin (H_key([1,2,4,5,7,8,10,11,13]));
  else
    H_txt = '';
  endif
  txt = '# varTypes rows; varNames rows; varDescriptions rows; varUnits rows.';
  if (strcmpi (H_txt, txt))
    Trows = str2num (H_key{3});
    Nrows = str2num (H_key{6});
    Drows = str2num (H_key{9});
    Urows = str2num (H_key{12});
    C(1,:) = [];  # remove comment line
  else
    Trows = 0;
    Nrows = 0;
    Drows = 0;
    Urows = 0;
  endif

  ## Handle CSV file with vartype header (as saved by table2csv method)
  if (Trows > 0)
    Hrows = Trows + Nrows + Drows + Urows;

    ## Check for RowNames
    if (strcmp (C{1,1}, 'RowNames') && isempty (C{Trows+1,1}))
      RowNames = C([Hrows+1:end],1);
      C(:,1) = [];        # remove row names
    else
      RowNames = {};
    endif

    ## Split cell into headers and data
    T = C(1:Trows,:);     # variable types
    C(1:Trows,:) = [];
    N = C(1:Nrows,:);     # variable names
    C(1:Nrows,:) = [];
    if (Drows)
      D = C(1:Drows,:);   # variable descriptions
      C(1:Drows,:) = [];
    else
      D = {};
    endif
    if (Urows)
      U = C(1:Urows,:);   # variable units
      C(1:Urows,:) = [];
    else
      U = {};
    endif
    ## After this point C contains only data

    ## Construct table
    tbl = cell2tbl (C, T, N, D, U, RowNames);
  endif

  ## Parse optional Name-Value paired arguments
  optNames = {'NumHeaderLines', 'VariableNames', 'ReadVariableNames', ...
              'VariableNamingRule', 'VariableNamesLine', 'VariableTypes', ...
              'VariableUnitsLine', 'VariableDescriptionsLine', ...
              'ReadRowNames', 'RowNamesColumn', 'TextType', ...
              'DatetimeType', 'DurationType', 'HexType'};
  dfValues = {0, {}, true, 'modify', 1, {}, 0, 0, true, 1, 'char', ...
              'datetime', 'duration', 'auto'};
  [numHeaderLines, varNames, readVarNames, varNamingRule, varNamesLine, ...
   varTypes, varUnitsLine, varDescrLine, readRowNames, rowNamesColumn, ...
   textType, datetimeType, durationTypes, hexType, args] = ...
             parsePairedArguments (optNames, dfValues, varargin(:));

  ## Apply optional arguments to CSV files with vartype header
  if (Trows > 0)
    if (numHeaderLines)
      tbl(1:numHeaderLines,:) = [];
    endif
    if (! isempty (varNames))
      tbl = tbl(:,varNames);
    endif
    if (! readRowNames)
      tbl.Properties.RowNames = [];
    endif
    return
  endif

  ## After this point handle generic CSV file

  ## Remove unnecessary header lines
  if (numHeaderLines)
    C(1:numHeaderLines,:) = [];
  endif

  ## Read row names
  has_RowNames = false;
  if (readRowNames && rowNamesColumn)
    has_RowNames = true;
    RowNames = C(:,rowNamesColumn);
    ## Force RowNames into cellstr
    if (! iscellstr (RowNames))
      RowNames = cellstr (string (RowNames));
    endif
    ## Remove RowNames column from data
    C(:,rowNamesColumn) = [];
  endif

  ## Read variable names, descriptions, and units from their respective lines.
  ## All line numbers refer to the same frame, so the values are read first and
  ## the consumed header rows are removed together below -- removing them one at
  ## a time would shift the indices of the lines still to be read.
  cols = size (C, 2);
  if (readVarNames && varNamesLine)
    N = C(varNamesLine,:);
    isnum = cellfun ('isnumeric', N);
    N(isnum) = cellfun ('num2str', N(isnum), "UniformOutput", false);
    ## Force to valid variable names
    if (strcmpi (varNamingRule, 'modify'))
      N = matlab.lang.makeValidName (N);
    endif
  else  # Generate default variable names
    N = arrayfun (@(x) sprintf ("Var%d", x), [1:cols], "UniformOutput", false);
  endif

  ## Override variable names with user supplied
  if (! isempty (varNames))
    N = varNames;
  endif

  ## Read variable descriptions and units
  if (varDescrLine)
    D = C(varDescrLine,:);
    ## Force variable descriptions into cellstr
    if (! iscellstr (D))
      D = cellstr (string (D));
    endif
  endif
  if (varUnitsLine)
    U = C(varUnitsLine,:);
    ## Force variable units into cellstr
    if (! iscellstr (U))
      U = cellstr (string (U));
    endif
  endif

  ## Remove all consumed header rows from data and RowNames at once
  droprows = [];
  if (readVarNames && varNamesLine)
    droprows = [droprows, varNamesLine];
  endif
  if (varDescrLine)
    droprows = [droprows, varDescrLine];
  endif
  if (varUnitsLine)
    droprows = [droprows, varUnitsLine];
  endif
  droprows = unique (droprows);
  if (! isempty (droprows))
    C(droprows,:) = [];
    if (has_RowNames)
      RowNames(droprows,:) = [];
    endif
  endif

  ## Check that RowNames are unique
  if (has_RowNames)
    if (numel (unique (RowNames)) != numel (RowNames))
      error ("csv2table: 'RowNames' must be unique.");
    endif
  endif

  ## Parse columns into variables with user defined variable types
  varValues = cell (1, cols);
  if (! isempty (varTypes))
    if (numel (varTypes) != cols)
      error ("csv2table: 'VariableTypes' do not match columns in CSV.");
    endif
    for ix = 1:cols
      varValues{ix} = cell2var (C(:,ix), varTypes{ix});
    endfor
  ## Parse columns into variables by identifying the variable type
  else
    for ix = 1:cols
      varValues{ix} = cell2auto (C(:,ix), textType, datetimeType, ...
                                 durationTypes, hexType);
    endfor
  endif

  ## Create table
  if (has_RowNames)
    tbl = table (varValues{:}, 'VariableNames', N, 'RowNames', RowNames);
  else
    tbl = table (varValues{:}, 'VariableNames', N);
  endif

  ## Add variable descriptions and units
  if (varDescrLine)
    tbl.Properties.VariableDescriptions = D;
  endif
  if (varUnitsLine)
    tbl.Properties.VariableUnits = U;
  endif

endfunction

function varValue = cell2var (C, T)
  ## Numeric vartypes
  numvartype = {'double', 'single', 'int8', 'uint8', 'int16', ...
                'uint16', 'int32', 'uint32', 'int64', 'uint64'};
  if (strcmp (T, "cell"))
    varValue = C;
  elseif (strcmp (T, "logical"))
    varValue = logical (cell2mat (C));
  elseif (ismember (T, numvartype))
    ## Cast each element to the declared type directly.  Large integers may
    ## have been read as int64/uint64 (to avoid double precision loss); casting
    ## the whole cell2mat result would let a mixed signed/unsigned column be
    ## promoted to a type that drops some values, so cast element-wise instead.
    if (all (cellfun (@(x) isa (x, 'double'), C(:))))
      varValue = cast (cell2mat (C), T);
    else
      varValue = cellfun (@(x) cast (x, T), C);
    endif
  elseif (strcmp (T, "calendarDuration"))
    warning ("csv2table: 'calendarDuration' strings are not converted.");
    varValue = C;
  elseif (strcmp (T, "categorical"))
    warning ("csv2table: 'categorical' strings are not converted.");
    varValue = C;
  elseif (strcmp (T, "datetime"))
    ## A 'NaT' token cannot be parsed alongside date strings (the format
    ## cannot be inferred), so reconstruct missing entries explicitly.  This
    ## mirrors how a 'NaN' token round-trips for a duration variable.
    isNaT = strcmp (strtrim (C), 'NaT');
    if (any (isNaT(:)))
      varValue = NaT (size (C, 1), size (C, 2));
      if (! all (isNaT(:)))
        varValue(! isNaT) = datetime (C(! isNaT));
      endif
    else
      varValue = datetime (C);
    endif
  elseif (strcmp (T, "duration"))
    varValue = duration (C);
  elseif (strcmp (T, "string"))
    varValue = string (C);
  endif
endfunction

function tbl = cell2tbl (C, T, N, D, U, RowNames);
  ## Get names, number, and positions of top level variables
  [varNames, ii, varIdx] = __unique__ (N(1,:), 'stable');
  varlen = numel (ii);
  varValues = cell (1, varlen);
  ## No nested table or structure
  if (size (T, 1) == 1)
    for ix = 1:varlen
      colIdx = varIdx == ix;
      varC = C(:,colIdx);
      varValues{ix} = cell2var (varC, T{ii(ix)});
    endfor
  ## Table contains nested tables or structures
  else
    ## For each top level variable search for nested tables or structures
    for ix = 1:varlen
      colIdx = varIdx == ix;
      varC = C(:,colIdx);
      varN = N(:,colIdx);
      varT = T(:,colIdx);
      ## No nested table or structure in this variable
      if (all (__ismissing__ (varT(2,:))))
        varValues{ix} = cell2var (varC, varT{1});
      ## Check for structure
      elseif (all (strcmp (varT(1,:), 'struct')))
        varValues{ix} = cell2struct (varC, varN(2,:), 2);
      ## Check for table
      elseif (all (strcmp (varT(1,:), 'table')))
        ## Pass the nested descriptions/units (rows below the top-level one)
        ## down so the recursive call restores them on the inner table.
        if (isempty (D))
          varD = [];
        else
          varD = D(2:end,colIdx);
        endif
        if (isempty (U))
          varU = [];
        else
          varU = U(2:end,colIdx);
        endif
        varValues{ix} = cell2tbl (varC, varT(2:end,:), varN(2:end,:), ...
                                  varD, varU, []);
      endif
    endfor
  endif
  ## Create table
  if (isempty (RowNames))
    tbl = table (varValues{:}, 'VariableNames', varNames);
  else
    tbl = table (varValues{:}, 'VariableNames', varNames, 'RowNames', RowNames);
  endif
  ## Restore variable descriptions and units, when present.  The first header
  ## row holds each top-level variable's metadata (deeper rows belong to nested
  ## tables and are restored by the recursive calls above).  Multicolumn
  ## variables repeat the entry across split columns, so take the first column
  ## belonging to each variable.
  if (! isempty (D))
    tbl.Properties.VariableDescriptions = D(1,ii);
  endif
  if (! isempty (U))
    tbl.Properties.VariableUnits = U(1,ii);
  endif
endfunction

function varValue = cell2auto (C, textType, datetimeType, durationTypes, ...
                               hexType)
  ## Index empty cells
  idx = cellfun ('isempty', C);
  ## Numeric columns are returned as doubles, unless they hold large integers
  ## that were read as int64/uint64 to avoid double precision loss.
  if (all (cellfun ('isnumeric', C)))
    if (all (cellfun (@(x) isa (x, 'double'), C)))
      varValue = cell2mat (C);
    elseif (any (cellfun (@(x) isa (x, 'int64'), C)))
      varValue = cellfun (@(x) int64 (x), C);   # a negative large integer present
    else
      varValue = cellfun (@(x) uint64 (x), C);
    endif
  elseif (all (cellfun ('isnumeric', C(! idx))))
    C(idx) = NaN;
    varValue = cell2mat (C);
  ## Mixed cells are forced to text
  elseif (! iscellstr (C))
    if (strcmpi (textType, 'char'))
      varValue = cellstr (C);
    else
      varValue = string (C);
    endif
  else  # cellstr
    ## Check for datetime strings
    if (strcmpi (datetimeType, 'datetime'))
      try
        varValue = datetime (C);
        is_datetime = true;
      catch
        varValue = C;
        is_datetime = false;
      end_try_catch
    endif
    ## Check for duration strings
    if (strcmpi (durationTypes, 'duration'))
      try
        varValue = duration (C);
        is_duration = true;
      catch
        varValue = C;
        is_duration = false;
      end_try_catch
    endif
    ## Check for hexadecimal strings (convert to integers)
    if (! strcmpi (hexType, 'text'))
      varValue = hex2dec (C);
      if (any (isnan (varValue)))
        is_hex = false;
      else
        is_hex = true;
        ## Detect smallest integer type or cast user defined
        if (strcmpi (hexType, 'auto'))
          minval = min (varValue);
          maxval = max (varValue);
          if (minval < 0) # signed integers
            if (minval >= intmin ('int8') && maxval <= intmax ('int8'))
              varValue = cast (varValue, 'int8');
            elseif (minval >= intmin ('int16') && maxval <= intmax ('int16'))
              varValue = cast (varValue, 'int16');
            elseif (minval >= intmin ('int32') && maxval <= intmax ('int32'))
              varValue = cast (varValue, 'int32');
            elseif (minval >= intmin ('int64') && maxval <= intmax ('int64'))
              varValue = cast (varValue, 'int64');
            endif
          else            # unsigned integers
            if (maxval <= intmax ('uint8'))
              varValue = cast (varValue, 'uint8');
            elseif (maxval <= intmax ('uint16'))
              varValue = cast (varValue, 'uint16');
            elseif (maxval <= intmax ('uint32'))
              varValue = cast (varValue, 'uint32');
            elseif (maxval <= intmax ('uint64'))
              varValue = cast (varValue, 'uint64');
            endif
          endif
        else
          varValue = cast (varValue, hexType);
        endif
      endif
    endif
    ## Leave it unaltered as cellstr or convert to strings
    if (! is_datetime && ! is_duration && ! is_hex)
      varValue = C;
      if (strcmpi (textType, 'string'))
        varValue = string (varValue);
      endif
    endif
  endif
endfunction

## Test user-defined 'VariableTypes' are applied per column
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "A,B,C\n1,2.5,10\n3,4.5,20\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadRowNames', false, ...
%!                  'VariableTypes', {'double', 'single', 'int32'});
%!   assert_equal (class (t{:, 1}), 'double');
%!   assert_equal (class (t{:, 2}), 'single');
%!   assert_equal (class (t{:, 3}), 'int32');
%!   assert_equal (t{:, 3}, int32 ([10; 20]));
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!error <csv2table: 'VariableTypes' do not match columns in CSV.> ...
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "A,B,C\n1,2,3\n");
%! fclose (fid);
%! unwind_protect
%!   csv2table (fn, 'ReadRowNames', false, 'VariableTypes', {'double', 'double'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test a generic CSV (no vartype header) reads names and auto-detects types
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "A,B,C\n1,2.5,foo\n3,4.5,bar\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadRowNames', false);
%!   assert_equal (t.Properties.VariableNames, {'A', 'B', 'C'});
%!   assert_equal (t.A, [1; 3]);
%!   assert_equal (t.B, [2.5; 4.5]);
%!   assert_equal (t.C, {'foo'; 'bar'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test 'ReadVariableNames' false generates default Var1, Var2, ... names
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "1,2\n3,4\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadVariableNames', false, 'ReadRowNames', false);
%!   assert_equal (t.Properties.VariableNames, {'Var1', 'Var2'});
%!   assert_equal (height (t), 2);
%!   assert_equal (t.Var1, [1; 3]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test 'VariableNames' overrides the names read from the header line
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "A,B\n1,2\n3,4\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'VariableNames', {'x', 'y'}, 'ReadRowNames', false);
%!   assert_equal (t.Properties.VariableNames, {'x', 'y'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test 'NumHeaderLines' skips leading lines before the header row
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "junk,more\nA,B\n1,2\n3,4\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'NumHeaderLines', 1, 'ReadRowNames', false);
%!   assert_equal (t.Properties.VariableNames, {'A', 'B'});
%!   assert_equal (t.A, [1; 3]);
%!   assert_equal (t.B, [2; 4]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test 'RowNamesColumn' reads a column as the table row names
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "id,v\nr1,10\nr2,20\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadRowNames', true, 'RowNamesColumn', 1);
%!   assert_equal (t.Properties.RowNames, {'r1'; 'r2'});
%!   assert_equal (t.v, [10; 20]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test 'TextType' string returns text columns as a string array
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "A,B\nfoo,bar\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'TextType', 'string', 'ReadRowNames', false);
%!   assert_equal (class (t.A), 'string');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test 'VariableUnitsLine' and 'VariableDescriptionsLine' read the right lines
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "A,B\nkg,m\nd1,d2\n1,2\n3,4\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'VariableUnitsLine', 2, 'VariableDescriptionsLine', 3, ...
%!                  'ReadRowNames', false);
%!   assert_equal (t.Properties.VariableUnits, {'kg', 'm'});
%!   assert_equal (t.Properties.VariableDescriptions, {'d1', 'd2'});
%!   assert_equal (t.A, [1; 3]);
%!   assert_equal (t.B, [2; 4]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test hexadecimal columns auto-detect to the smallest integer type
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "h\nFF\n0A\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadRowNames', false);
%!   assert_equal (class (t.h), 'uint8');
%!   assert_equal (t.h, uint8 ([255; 10]));
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test header names with spaces are made into valid variable names
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "my col,other one\n1,2\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadRowNames', false);
%!   assert_equal (t.Properties.VariableNames, {'myCol', 'otherOne'});
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test a generic CSV whose first cell is numeric does not error on read
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "10,20\n30,40\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadVariableNames', false, 'ReadRowNames', false);
%!   assert_equal (height (t), 2);
%!   assert_equal (t.Var1, [10; 30]);
%!   assert_equal (t.Var2, [20; 40]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test duplicate row names are rejected
%!error <csv2table: 'RowNames' must be unique.> ...
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "id,v\nr1,10\nr1,20\n");
%! fclose (fid);
%! unwind_protect
%!   csv2table (fn, 'ReadRowNames', true, 'RowNamesColumn', 1);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test a large integer column is read exactly as a 64-bit integer (no double loss)
%!test
%! fn = tempname ();
%! fid = fopen (fn, "w");
%! fputs (fid, "v\n18446744073709551615\n18446744073709551614\n");
%! fclose (fid);
%! unwind_protect
%!   t = csv2table (fn, 'ReadRowNames', false);
%!   assert_equal (class (t.v), 'uint64');
%!   assert_equal (t.v, [intmax('uint64'); intmax('uint64') - uint64(1)]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

## Test NAME input validation
%!error <csv2table: NAME must be a character vector or a string scalar.> ...
%! csv2table (42);

## Test reading a non-existent file reports a clear error
%!error <csv2table: cannot open file '.*' for reading.> ...
%! csv2table (fullfile (tempname (), 'no_such_file.csv'));
