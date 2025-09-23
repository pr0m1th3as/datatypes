## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

classdef table
  ## -*- texinfo -*-
  ## @deftp {Class} table
  ##
  ## Array of tabular data containing multiple columnar variables.
  ##
  ## A table is a 2-dimensional data structure that collects heterogeneous data
  ## and metadata into a singe container.  Tables are suitable for storing
  ## columnar data much like spreadsheets but they can also be used for storing
  ## more complex data including multicolumnar variables and nested tables.
  ##
  ## Tables can be subscripted using parentheses like ordinary numeric arrays,
  ## but in addition to indexing with numeric and logical vectors, you can also
  ## use the table's variable or row names much like indexing a structure field
  ## as well as using a @qcode{vartype} class object to make a selection of
  ## variable types.  While these methods will return a subset of the original
  ## table, you can also used curly brackets much like cell arrays to retrieve
  ## the contents of the table.  In this case, the original data type of the
  ## selected variables are returned.
  ##
  ## Besides the @code{table} constructor, you can also use @code{array2table},
  ## @code{cell2table}, and @code{struct2table} to create tables from the
  ## respective data types.
  ##
  ## Besides all numeric data types, other supported data types that can be
  ## stored in a table array are @qcode{logical}, @qcode{categorical},
  ## @qcode{cell}, (including @qcode{cellstr}), @qcode{calendarDuration},
  ## @qcode{duration}, @qcode{datetime}, @qcode{string}, and @qcode{struct}
  ## arrays, as well as @qcode{table} itself.
  ##
  ## @seealso{vartype, array2table, cell2table, struct2table}
  ## @end deftp

  properties
    ## -*- texinfo -*-
    ## @deftp {table} {property} Description
    ##
    ## Table description
    ##
    ## Table description specified as a character vector or a string scalar.
    ## If specified as a string scalar, it is converted and stored internally
    ## as a character vector.  You can access the @qcode{Description} property
    ## of a table @var{tbl} with @qcode{@var{tbl}.Properties.Description}.
    ##
    ## @end deftp
    Description = ""

    ## -*- texinfo -*-
    ## @deftp {table} {property} UserData
    ##
    ## Additional table information
    ##
    ## Additional table information, specified as an array.  Any type of data
    ## can be attached using this property.  You can access the @qcode{UserData}
    ## property of a table @var{tbl} with @qcode{@var{tbl}.Properties.UserData}.
    ##
    ## @end deftp
    UserData = []

    ## -*- texinfo -*-
    ## @deftp {table} {property} DimensionNames
    ##
    ## Dimension names
    ##
    ## Dimension names specified as a two-element cell array of character
    ## vectors or a two-element string array.  If specified as a string array,
    ## it is converted and stored internally as a cell array of character
    ## vectors.  You can access the @qcode{DimensionNames} property of a table
    ## @var{tbl} with @qcode{@var{tbl}.Properties.DimensionNames}.
    ##
    ## By default, @qcode{DimensionNames} is specified as
    ## @qcode{'Row', 'Variables'}.  You can access table data per rows or per
    ## columns by using either one of the two dimension names, respectively.
    ## However, if the table contains row names, then the first element of the
    ## @qcode{DimensionNames} corresponds to the row names.
    ##
    ## @end deftp
    DimensionNames = {"Row", "Variables"}

    ## -*- texinfo -*-
    ## @deftp {table} {property} VariableNames
    ##
    ## Variable names
    ##
    ## Variable names, specified as a cell array of character vectors or a
    ## string array.  If specified as a string array, it is converted and stored
    ## internally as a cell array of character vectors.  All elements must be
    ## nonempty and distinct, and their number must equal the number of
    ## variables.  You can access the data type of a specific variable by using
    ## dot name assignment, as in @qcode{@var{tbl}.@var{varname}}, where
    ## @var{varname} is the name of the variable in table @var{tbl}.  If the
    ## variable name does not a exist, a new one is created.
    ##
    ## @end deftp
    VariableNames = {}

    ## -*- texinfo -*-
    ## @deftp {table} {property} VariableTypes
    ##
    ## Variable data types
    ##
    ## The class of the data of each variable, defined as a cell array of
    ## character vectors or a string array with the same number of elements as
    ## the number of variables in the table.  If specified as a string array,
    ## it is converted and stored internally as a cell array of character
    ## vectors.  You can access the @qcode{VariableTypes} property of a table
    ## @var{tbl} with @qcode{@var{tbl}.Properties.VariableTypes}.  You can
    ## further index specific variables to access their data type.  Modifying
    ## the elements of the @qcode{VariableTypes} property automatically converts
    ## the underlying data of the corresponding variable into the specified
    ## data types provided that a valid conversion is requested.
    ##
    ## @end deftp
    VariableTypes = {}

    ## -*- texinfo -*-
    ## @deftp {table} {property} VariableDescriptions
    ##
    ## Variable descriptions
    ##
    ## Variable descriptions, specified as a cell array of character vectors or
    ## a string array.  If specified as a string array, it is converted and
    ## stored internally as a cell array of character vectors.  If not empty
    ## (default), it must contain the same number of elements as the number of
    ## variables.  If a specific variable does not have a description, this can
    ## be specified with an individual empty character vector or an empty
    ## string.  You can access the @qcode{VariableDescriptions} property of a
    ## table @var{tbl} with @qcode{@var{tbl}.Properties.VariableDescriptions}.
    ## You can further index specific variables to access their description.
    ##
    ## @end deftp
    VariableDescriptions = {}

    ## -*- texinfo -*-
    ## @deftp {table} {property} VariableUnits
    ##
    ## Variable units
    ##
    ## Variable units, specified as a cell array of character vectors or a
    ## string array.  If specified as a string array, it is converted and stored
    ## internally as a cell array of character vectors.  If not empty (default),
    ## it must contain the same number of elements as the number of variables.
    ## If a specific variable does not have a unit, this can be specified with
    ## an individual empty character vector or an empty string.  You can access
    ## the @qcode{VariableDescriptions} property of a table @var{tbl} with
    ## @qcode{@var{tbl}.Properties.VariableDescriptions}.   You can further
    ## index specific variables to access their description.
    ##
    ## @end deftp
    VariableUnits = {}

    ## -*- texinfo -*-
    ## @deftp {table} {property} RowNames
    ##
    ## Row names
    ##
    ## Row names, specified as a cell array of character vectors or a string
    ## array.  If specified as a string array, it is converted and stored
    ## internally as a cell array of character vectors.  If not empty (default),
    ## it must contain the same number of elements as the number of rows in the
    ## table.  All elements must be nonempty and distinct.  You can access the
    ## rows of the table @var{tbl} by specifying one or more row names within
    ## within parentheses or curly braces.  You can also set @qcode{RowNames} by
    ## dot name assignment to an existing variable.
    ##
    ## @end deftp
    RowNames = {}

    ## -*- texinfo -*-
    ## @deftp {table} {property} CustomProperties
    ##
    ## Customized metadata of table and its variables
    ##
    ## Custom properties that contain metadata of a table and its variables.
    ## By default, this is an empty container.  Each custom property can contain
    ## either table metadata or variable metadata.  Any custom property which is
    ## an array with the same number of elements as the number of variables is
    ## considered as variable metadata, otherwise is considered table metadata.
    ##
    ## You can add custom properties only by using the @code{addprop} method and
    ## you can only remove a custom property with the @code{rmprop} method.  To
    ## access existing custom properties use dot name structure assignment as in
    ## @qcode{@var{tbl}.Properties.CustomProperties.@var{PropertyName}}, where
    ## @var{PropertyName} is the name used with the @code{addprop} method.
    ##
    ## @end deftp
    CustomProperties = []

  endproperties

  properties (GetAccess = private, SetAccess = protected)
    CustomPropTypes = {}
    VariableValues = {}
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    ## Custom display
    function disp (this)
      if (isempty (this))
        fprintf ("  %dx%d empty table\n\n", size (this));
      else
        fprintf ("  %dx%d table\n\n", height (this), width (this));
        print_table (this);
      endif
    endfunction

  endmethods

################################################################################
##                    ** Create Table and Convert Type **                     ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'table'            'table2array'      'table2cell'       'table2struct'    ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} table (@var{var1}, @var{var2}, @dots{}, @var{varN})
    ## @deftypefnx {table} {@var{tbl} =} table (@qcode{'Size'}, @var{sz}, @qcode{'VariableTypes'}, @var{varTypes})
    ## @deftypefnx {table} {@var{tbl} =} table (@dots{}, @qcode{'VariableNames'}, @var{varNames})
    ## @deftypefnx {table} {@var{tbl} =} table (@dots{}, @qcode{'RowNames'}, @var{rowNames})
    ## @deftypefnx {table} {@var{tbl} =} table (@dots{}, @qcode{'DimensionNames'}, @var{dimNames})
    ##
    ## Create a new table.
    ##
    ## @code{@var{tbl} = table (@var{var1}, @var{var2}, @dots{}, @var{varN})}
    ## created a new table with the given variables.  The variables passed as
    ## input arguments become the variables of the table.  Their names are
    ## automatically detected from the input variable names that you used.
    ##
    ## @code{@var{tbl} = table (@qcode{'Size'}, @var{sz}, @qcode{'VariableTypes'},
    ## @var{varTypes})} creates a new table of the given size,  @var{sz}, and
    ## with the given variable types, @var{varTypes}.  @var{sz} must be a two-
    ## element numeric array, where @qcode{@var{sz}(1)} specifies the number of
    ## rows and @qcode{@var{sz}(2)} specifies the number of variables.  The
    ## variables will contain the default value for elements of that type.
    ##
    ## @code{@var{tbl} = table (@dots{}, @qcode{'VariableNames'}, @var{varNames})}
    ## specifies the variable names to use in the constructed table.
    ## @var{varNames} must be either a cell array of character vectors of string
    ## array with the same number of nonempty and unique elements as the number
    ## of table variables.
    ##
    ## @code{@var{tbl} = table (@dots{}, @qcode{'RowNames'}, @var{rowNames})}
    ## specifies the row names to use in the constructed table. @var{dimNames}
    ## must be either a cell array of character vectors of string array with the
    ## same number of nonempty and unique elements as the number of rows in the
    ## table.
    ##
    ## @code{@var{tbl} = table (@dots{}, @qcode{'DimensionNames'}, @var{dimNames})}
    ## specifies the dimension names to use in the constructed table.
    ## @var{dimNames} must be either a two-element cell array of character
    ## vectors or a two-element string array with nonempty and unique elements.
    ##
    ## @code{@var{tbl} = table ()} returns an empty table with 0 rows and 0
    ## variables.
    ##
    ## @seealso{array2table, cell2table, struct2table, table, istable}
    ## @end deftypefn
    function this = table (varargin)

      ## Return an empty table object
      if (nargin == 0)
        return
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {"VariableNames", "RowNames", "DimensionNames"};
      dfValues = {{}, {}, {"Row", "Variables"}};
      [VariableNames, RowNames, DimensionNames, args] = ...
             pairedArgs (optNames, dfValues, varargin(:));
      ## Check optional Name-Value paired arguments
      if (! isempty (VariableNames))
        if (! (iscellstr (VariableNames) || isa (VariableNames, "string")))
          error (["table: 'VariableNames' must be either a cell", ...
                  " array of character vectors or a string array."]);
        endif
        VariableNames = cellstr (VariableNames);
      endif
      if (! isempty (RowNames))
        if (! (iscellstr (RowNames) || isa (RowNames, "string")))
          error (["table: 'RowNames' must be either a cell", ...
                  " array of character vectors or a string array."]);
        endif
        RowNames = cellstr (RowNames);
      endif
      if (! (iscellstr (DimensionNames) || isa (DimensionNames, "string"))
          || numel (DimensionNames) != 2)
        error (["table: 'DimensionNames' must be either a two-element cell", ...
                " array of character vectors or a two-element string array."]);
      endif
      this.DimensionNames = cellstr (DimensionNames);
      ## Check for conflict between VariableNames and DimensionNames
      idx = ismember (this.DimensionNames, VariableNames);
      if (any (idx))
        error ("table: duplicate dimension and variable name: '%s'", ...
               this.DimensionNames{idx});
      endif

      ## Construct a preallocated table with default values
      if (numel (args) == 4 && strcmpi (args{1}, "Size") &&
                               strcmpi (args{3}, "VariableTypes"))
        ## Get number of rows and variables
        nr = args{2}(1);
        nv = args{2}(2);
        ## Get variable types
        varTypes = args{4};
        if (! iscellstr (varTypes) || numel (varTypes) != nv)
          error (["table: 'VariableTypes' must be a cellstring array of", ...
                  " the same number of elements as defined in SZ(2)."]);
        endif

        ## Check optional arguments
        if (! isempty (VariableNames) && numel (VariableNames) != nv)
          error (["table: inconsistent number of 'VariableNames' and", ...
                  " 'VariableTypes'."]);
        elseif (isempty (VariableNames))
          VariableNames = cell (1, nv);
          for i = 1:nv
            VariableNames{i} = sprintf ("Var%d", i);
          endfor
        endif
        if (! isempty (RowNames) && numel (RowNames) != nr)
          error (["table: inconsistent number of 'RowNames'", ...
                  " and rows defined in SZ(1)."]);
        endif

        ## Populate variables with defaults
        VariableTypes = cell (1, nv);
        VariableValues = cell (1, nv);
        for i = 1:nv
          VariableTypes{i} = varTypes{i};
          switch (varTypes{i})
            case {"double", "single", "int8", "uint8", "int16", "uint16", ...
                  "int32", "uint32", "int64", "uint64"}
              VariableValues{i} = zeros (nr, 1, varTypes{i});
            case {"doublenan", "doubleNaN"}
              VariableValues{i} = NaN (nr, 1, "double");
            case {"singlenan", "singleNaN"}
              VariableValues{i} = NaN (nr, 1, "single");
            case "logical"
              VariableValues{i} = logical (zeros (nr, 1));
            case "categorical"
              VariableValues{i} = categorical (NaN (nr, 1));
            case "datetime"
              VariableValues{i} = NaT (nr, 1);
            case "duration"
              VariableValues{i} = seconds (zeros (nr, 1));
            case "calendarDuration"
              VariableValues{i} = calendarDuration (zeros (nr, 3));
            case "string"
              VariableValues{i} = string (NaN (nr, 1));
            case "cellstr"
              VariableValues{i} = repmat (cellstr (""), nr, 1);
            case "cell"
              VariableValues{i} = cell (nr, 1);
            case "struct"
              VariableValues{i} = repmat (struct, nr, 1);
            case "table"
              VariableValues{i} = table([]);
            case "timetable"
              error ("table: 'timetable' variable type not supported yet.");
          endswitch
        endfor

      ## Construct a table with data from input arguments
      else
        ## Get variable names from input arguments
        if (isempty (VariableNames))
          VariableNames = cell (size (args));
          for i = 1:numel (args)
            VariableNames{i} = inputname (i);
            if (isempty (VariableNames{i}))
              VariableNames{i} = sprintf ("Var%d", i);
            endif
          endfor
        endif
        ## Check for unique names in input arguments
        [uqNames, ix] = __unique__ (VariableNames);
        if (numel (uqNames) < numel (VariableNames))
          ixBad = 1:numel (VariableNames);
          ixBad(ix) = [];
          error ("table: duplicate variable names: %s", ...
                 strjoin (VariableNames(ixBad), ", "));
        endif
        ## Check number of variable names and input arguments
        if (numel (VariableNames) != numel (args))
          error (["table: inconsistent number of variable names (%d) and",...
                  " variable values (%d)"], numel (VariableNames), numel (args));
        endif
        ## Check size of input variables
        if (! isempty (args))
          nrows = size (args{1}, 1);
          if (ndims (args{1}) > 2)
            error (["table: variable values must not have more than 2", ...
                    " dimensions: input 1 '%s' has %d."], ...
                    VariableNames{1}, ndims (args{1}));
          endif
          for i = 2:numel (args)
            if (ndims (args{i}) > 2)
              error (["table: variable values must not have more than 2", ...
                      " dimensions: input %d '%s' has %d."], ...
                      i, VariableNames{i}, ndims (args{i}));
            endif
            nrows2 = size (args{i}, 1);
            if (nrows != nrows2)
              error (["table: inconsistent sizes between variables:", ...
                      " var '%s' has %d rows; var '%s' has %d rows."], ...
                      VariableNames{1}, nrows, VariableNames{i}, nrows2);
            endif
          endfor
        endif
        VariableValues = args(:)';
      endif

      ## Construction
      this.VariableDescriptions = repmat ({""}, [1, numel(VariableNames)]);
      this.VariableUnits = repmat ({""}, [1, numel(VariableNames)]);
      this.VariableNames = VariableNames(:)';
      this.VariableValues = VariableValues;
      this.VariableTypes = cellfun ('class', VariableValues, ...
                                    'UniformOutput', false);
      if (! isempty (RowNames))
        if (numel (__unique__ (RowNames)) != size (VariableValues{1}, 1))
          error ("table: elements in 'RowNames' must be unique.");
        endif
        this.RowNames = RowNames(:);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{A} =} table2array (@var{tbl})
    ##
    ## Converts a table to a homogeneous array.
    ##
    ## @end deftypefn
    function A = table2array (this)
      ## Handle empty table
      if isempty (this)
        A = [];
        return
      endif
      ## Add a try...catch block instead of heuristics
      try
        A = cat (2, this.VariableValues{:});
      catch
        error (["table.table2array: table cannot be concatenated into", ...
                " a matrix due to incompatible variable types."]);
      end_try_catch
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{C} =} table2cell (@var{tbl})
    ##
    ## Converts a table to a cell array.
    ##
    ## Each variable in @var{tbl} becomes a column of cells in the output
    ## @var{C}.  Multicolumnar variables are returned in a single column with
    ## each cell element containing a row vector.
    ##
    ## The size of the returned cell array, @var{C}, is the same as the input
    ## table, @var{tbl}.  The output @var{C} does not include any of the table's
    ## properties.  This also applies to row names.
    ##
    ## Compatibity Notes:
    ##
    ## Variables of types @qcode{categorical}, @qcode{calendarDuration},
    ## @qcode{datetime}, @qcode{duration} and @qcode{string} are returned as
    ## in their printed representation as character vectors.  To revert them to
    ## their original class type you can parse the cell elements to the
    ## respective object constructor.
    ##
    ## Nested tables are handled as multicolumnar variables only if they contain
    ## data types, which can be converted to homogeneous array, i.e. numerical
    ## logical values. Other data types will result to a warning due to
    ## implicit conversion from numeric to char and the returned values will
    ## not contain all values from the nested table.
    ##
    ## @end deftypefn
    function C = table2cell (this, varargin)
      if (nargout > 1)
        error ("table.table2cell: too many output arguments.");
      endif
      C = cell (size (this));
      for i = 1:width (this)
        varVal = this.VariableValues{i};
        if (iscell (varVal))
          C(:,i) = varVal;
        elseif (isnumeric (varVal) || islogical (varVal))
          C(:,i) = num2cell (varVal, 2);
        elseif (any (isa (varVal, {"calendarDuration", "categorical"})))
          C(:,i) = dispstrs (varVal);
        elseif (any (isa (varVal, {"datetime", "duration"})))
          C(:,i) = dispstrs (varVal);
        elseif (isa (varVal, "string"))
          C(:,i) = cellstr (varVal);
        elseif (isa (varVal, "table"))
          tmpVal = table2cell (varVal);
          if (size (tmpVal, 2) > 1)
            C(:,i) = num2cell (cell2mat (tmpVal), 2);
          else
            C(:,i) = tmpVal;
          endif
        elseif (isa (varVal, "struct"))
          C(:,i) = struct2cell (varVal(:))';
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{S} =} table2struct (@var{tbl})
    ## @deftypefnx {table} {@var{S} =} table2struct (@var{tbl}, @qcode{"ToScalar"}, @qcode{true})
    ##
    ## Converts a table to a scalar structure or structure array.
    ##
    ## @code{@var{S} = table2struct (@var{tbl})} returns a structure array with
    ## the same fields as the variables in @var{tbl}.  The length of @var{S} is
    ## the same as the height of @var{tbl}.
    ##
    ## @code{@var{S} = table2struct (@var{tbl}, @qcode{"ToScalar"},
    ## @qcode{true})} returns a scalar structure with the same fields as the
    ## variables in @var{tbl}.  Each field has the same rows as the @var{tbl}.
    ##
    ## The output @var{S} does not include any of the table's properties.  This
    ## also applies to row names.
    ##
    ## @end deftypefn
    function S = table2struct (this, varargin)
      ## Add defaults
      toScalar = false;
      ## Check optional input arguments
      if (nargin > 1)
        if (nargin != 3)
          error ("table.table2struct: wrong number of input aguments.");
        endif
        if (strcmpi (varargin{1}, "ToScalar") && isequal (varargin{2}, 1))
          toScalar = true;
        elseif (strcmpi (varargin{1}, "ToScalar"))
          toScalar = false;
        else
          error ("table.table2struct: wrong optional input agument.");
        endif
      endif
      ## Do the conversion
      if (toScalar)
        S = struct;
        for i = 1:width (this)
          S.(this.VariableNames{i}) = this.VariableValues{i};
        endfor
      else
        C = table2cell (this);
        F = this.VariableNames(:);
        S = cell2struct (C, F, 2);
      endif
    endfunction

  endmethods

################################################################################
##                            ** Save to Files **                             ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'table2csv'                                                                ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {table} {} table2csv (@var{tbl}, @var{file})
    ##
    ## Save a table to a CSV file.
    ##
    ## Each variable in @var{tbl} becomes a column of cells in the output
    ## @var{C}.  Multicolumnar variables are split into separate columns with
    ## each column sharing the same variable name.  Nested tables are also split
    ## into separate columns with each column sharing the same variable name but
    ## also the corresponding nested table's variable name.  Structures are
    ## converted to cell arrays with each field becoming a separate column and
    ## the fieldnames becoming nested column names in the same way as in nested
    ## tables.
    ##
    ## The first cell of the CSV file contains a comment mentioning the number
    ## of consecutive rows that contain info about the variable types. The
    ## number of vartype rows may differ accordign to the nested tables and
    ## structures contained in the table. The following rows contain the header
    ## with the column names.
    ##
    ## @end deftypefn
    function table2csv (this, file)
      file = char (cellstr (file));
      [V, N, T, D, U] = table2cellarrays (this);
      ## Get columns for final cell array
      Ccols = size (V, 2);
      ## Get rows for variable types, names, descriptions, and units
      Trows = cellfun (@(x) size (x, 1), T);
      Tmaxr = max (Trows);
      Nrows = cellfun (@(x) size (x, 1), N);
      Nmaxr = max (Nrows);
      isvar = cellfun (@(x) ! isempty (x), N(1,:));
      Drows = cellfun (@(x) ! isempty (x), D);
      Dmaxr = sum (all (Drows, 2));
      Urows = cellfun (@(x) ! isempty (x), U);
      Umaxr = sum (all (Urows, 2));
      ## Initialize header
      Header = repmat ({''}, Nmaxr + Tmaxr + Dmaxr + Umaxr, Ccols);
      ## Populate header
      for c = 1:Ccols
        if (isvar(c))   # variable
          if (Trows(c) == 1)
            Header{1,c} = T{c};
          else
            for tr = 1:Trows(c)
              Header{tr,c} = T{c}{tr};
            endfor
          endif
          if (Nrows(c) == 1)
            Header{1 + Tmaxr,c} = N{c};
          else
            for nr = 1:Nrows(c)
              Header{nr + Tmaxr,c} = N{c}{nr};
            endfor
          endif
          if (Dmaxr)
            if (Dmaxr == 1)
              Header{1 + Tmaxr + Nmaxr,c} = D{c};
            else
              for dr = 1:Dmaxr
                Header{dr + Tmaxr + Nmaxr,c} = D{c}{dr};
              endfor
            endif
          endif
          if (Umaxr)
            if (Umaxr == 1)
              Header{1 + Tmaxr + Nmaxr + Dmaxr,c} = U{c};
            else
              for ur = 1:Umaxr
                Header{ur + Tmaxr + Nmaxr + Dmaxr,c} = U{c}{ur};
              endfor
            endif
          endif
        else            # RowNames
          Header{1,c} = 'RowNames';
        endif
      endfor
      ## Generate descriptive comment for header contents
      cmt = cell (1, Ccols);
      txt = strcat ("# varTypes %d rows; varNames %d rows;", ...
                    " varDescriptions %d rows; varUnits %d rows.");
      cmt{1} = sprintf (txt, Tmaxr, Nmaxr, Dmaxr, Umaxr);
      ## Merge cell arrays into a single cell array for saving to csv file
      csv = [cmt; Header; V];
      ## Write to file
      msg = __table2csv__ (file, csv);
      if (msg)
        error ("table.table2csv: %s", msg);
      endif
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'summary'          'height'           'width'            'head'            ##
## 'tail'                                                                     ##
##                                                                            ##
################################################################################
##                           Unimplemented Methods                            ##
##                                                                            ##
## 'stackedplot'                                                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {} summary (@var{tbl})
    ## @deftypefnx {table} {@var{s} =} summary (@var{tbl})
    ##
    ## Print a summary of a table.
    ##
    ## @code{summary (@var{tbl}} prints the description from
    ## @qcode{@var{tbl}.Properties.Description} (not implemented yet) followed
    ## by a summary of each table variable's values and their properties as
    ## defined in @qcode{@var{tbl}.Properties.VariableUnits} and
    ## @qcode{@var{tbl}.Properties.VariableDescriptions} (not implemented yet).
    ##
    ## @code{@var{s} = summary (@var{tbl}} returns a structure, @var{s}, that
    ## contains a summary of the input table, @var{tbl}.  Each field of @var{s}
    ## is a structure that summarizes the values in the corresponding variable
    ## of @var{tbl}.
    ##
    ## @itemize
    ## @item For numerical variables of @qcode{double}, @qcode{single} or any
    ## @qcode{int} type, it prints the minimum, median, and maximum values.  For
    ## multicolumnar numerical variables it prints the minimum, median, and
    ## maximum values for each column separately.
    ##
    ## @item For variables of @qcode{logical} type, it prints the occurences
    ## of @qcode{True} and @qcode{False}.
    ##
    ## @item For variables of type @qcode{datetime}, @qcode{duration}, and
    ## @qcode{calendarDuration} it prints the minimum, median, and maximum
    ## values along with the TimeStep, which is only calculated for fixed
    ## intervals present in the data, otherwise @qcode{NaN} is returned.
    ##
    ## @item For variables of type @qcode{cellstr}, @qcode{cell}, @qcode{string}
    ## and @qcode{struct} it prints the size and the type of variable.
    ## @end itemize
    ##
    ## @end deftypefn
    function [varargout] = summary (this)
      ## Get summary for each variable into the returning structure
      s = summary_for_variables (this);
      ## Print summary if no output is requested
      if (nargout == 0)
        ## Print table description
        if (! isempty (this.Description))
          fprintf ("Description:  %s\n\n", this.Description);
        endif
        ## Print summary for each variable
        fprintf ("Variables:\n\n");
        tab = "    ";
        varNames = fieldnames (s);
        for i = 1:numel (varNames)
          var = s.(varNames{i});
          ## Print variable name
          fprintf ("%s%s: %dx%d %s\n\n", tab, varNames{i}, var.Size(1), ...
                                         var.Size(2), var.Type);
          ## Print variable properties (if available)
          if (! isempty (var.Units) || ! isempty (var.Description))
            fprintf ("%s    Properties:\n", tab);
            if (! isempty (var.Units))
              fprintf ("%s        Units: %s\n", tab, var.Units);
            endif
            if (! isempty (var.Description))
              fprintf ("%s        Description: %s\n", tab, var.Description);
            endif
          endif
          ## Print custom properties (if available)
          if (isfield (var, "CustomProperties"))
            if (! isempty (var.CustomProperties))
              fprintf ("%s    Custom Properties:\n", tab);
              cpNames = fieldnames (var.CustomProperties);
              for p = 1:numel (cpNames)
                fprintf ("%s        %s: %s\n", tab, cpNames{p}, ...
                         var.CustomProperties.(cpNames{p}){:});
              endfor
            endif
          endif
          ## Print values (either {Min Median Max TimeStep} or {True False})
          if (isfield (var, "Min") || isfield (var, "True"))
            fprintf ("%s    Values:\n", tab);
            ## Check for multicolumnar variable
            if (var.Size(2) > 1)
              ## Check for numeric, time, or logical and find max length for
              ## properly aligning the numerical columns
              if (isfield (var, "Min") && ! isfield (var, "TimeStep"))
                ## numeric
                minLen = max (cell2mat (arrayfun (@(x)length(num2str(x)), ...
                               var.Min, "UniformOutput", false)));
                medLen = max (cell2mat (arrayfun (@(x)length(num2str(x)), ...
                               var.Median, "UniformOutput", false)));
                maxLen = max (cell2mat (arrayfun (@(x)length(num2str(x)), ...
                               var.Max, "UniformOutput", false)));
                mLen = max ([minLen, medLen, maxLen]);
              elseif (isfield (var, "TimeStep"))
                ## datetime, duration, calendarDuration
                minLen = max (cellfun (@length, dispstrs (var.Min)));
                medLen = max (cellfun (@length, dispstrs (var.Median)));
                maxLen = max (cellfun (@length, dispstrs (var.Max)));
                tspLen = max (cellfun (@length, dispstrs (var.TimeStep)));
                mLen = max ([minLen, medLen, maxLen, tspLen]);
              else
                ## logical
                tLen = max (cell2mat (arrayfun (@(x)length(num2str(x)), ...
                            var.True, "UniformOutput", false)));
                fLen = max (cell2mat (arrayfun (@(x)length(num2str(x)), ...
                            var.False, "UniformOutput", false)));
                mLen = max ([tLen, fLen]);
              endif
              ## Create padding character vectors
              mLen = max ([8 mLen]);
              pad = "";
              for t = 1:mLen - 8
                pad = [b_pad, " "];
              endfor
              strhead = "";
              strline = "";
              for c = 1:var.Size(2)
                strhead = [strhead, sprintf("%sColumn %d    ", pad, c)];
                strline = [strline, sprintf("%s________    ", pad)];
              endfor
              ## Print multicolumnar variable header
              fprintf ("%s                  %s\n", tab, strhead);
              fprintf ("%s                  %s\n", tab, strline);
              ## Print multicolumnar variable statistics
              if (isfield (var, "Min") && ! isfield (var, "TimeStep"))
                ## numeric
                strMin = "";
                strMed = "";
                strMax = "";
                template = ["%s%", sprintf("%d", mLen), "g    "];
                for c = 1:numel (var.Min)
                  strMin = [strMin, sprintf(template, pad, var.Min(c))];
                  strMed = [strMed, sprintf(template, pad, var.Median(c))];
                  strMax = [strMax, sprintf(template, pad, var.Max(c))];
                endfor
                fprintf ("%s        Min       %s\n", tab, strMin);
                fprintf ("%s        Median    %s\n", tab, strMed);
                fprintf ("%s        Max       %s\n\n", tab, strMax);
              elseif (isfield (var, "TimeStep"))
                ## datetime, duration, calendarDuration
                strMin = "";
                strMed = "";
                strMax = "";
                strTSp = "";
                template = ["%s%", sprintf("%s", mLen), "g    "];
                for c = 1:numel (var.Min)
                  strMin = [strMin, sprintf(template, pad, ...
                                            dispstrs(var.Min(c)){:})];
                  strMed = [strMed, sprintf(template, pad, ...
                                            dispstrs(var.Median(c)){:})];
                  strMax = [strMax, sprintf(template, pad, ...
                                            dispstrs(var.Max(c)){:})];
                  strTSp = [strTSp, sprintf(template, pad, ...
                                            dispstrs(var.TimeStep(c)){:})];
                endfor
                fprintf ("%s        Min       %s\n", tab, strMin);
                fprintf ("%s        Median    %s\n", tab, strMed);
                fprintf ("%s        Max       %s\n", tab, strMax);
                fprintf ("%s        TimeStep  %s\n\n", tab, strTSp);
              else
                ## logical
                strTrue = "";
                strFalse = "";
                for c = 1:numel (var.True)
                  strTrue = [strTrue, sprintf(template, pad, var.True(c))];
                  strFalse = [strFalse, sprintf(template, pad, var.False(c))];
                endfor
                fprintf ("%s        True      %s\n", tab, strTrue);
                fprintf ("%s        False     %s\n\n", tab, strFalse);
              endif

            ## Print single column variable
            else
              ## Check for numeric, time, or logical
              if (isfield (var, "Min") && ! isfield (var, "TimeStep"))
                ## numeric
                fprintf ("%s        Min       %g\n", tab, var.Min);
                fprintf ("%s        Median    %g\n", tab, var.Median);
                fprintf ("%s        Max       %g\n", tab, var.Max);
              elseif (isfield (var, "TimeStep"))
                ## datetime, duration, calendarDuration
                fprintf ("%s        Min       %s\n", ...
                         tab, dispstrs (var.Min){:});
                fprintf ("%s        Median    %s\n", ...
                         tab, dispstrs (var.Median){:});
                fprintf ("%s        Max       %s\n", ...
                         tab, dispstrs (var.Max){:});
                fprintf ("%s        TimeStep  %s\n\n", ...
                         tab, dispstrs (var.TimeStep){:});
              else
                ## logical
                fprintf ("%s        True      %d\n", tab, var.True);
                fprintf ("%s        False     %d\n\n", tab, var.False);
              endif
            endif
          endif
        endfor
      endif
      ## Return structure if requested
      if (nargout > 0)
        varargout{1} = s;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{H} =} height (@var{tbl})
    ##
    ## Number of rows in table.
    ##
    ## @code{@var{H} = height (@var{tbl})} returns the number of rows in the
    ## table, @var{tbl}.  It is the equivalent of @qcode{size (@var{tbl}, 1)}.
    ##
    ## @end deftypefn
    function out = height (this)
      if (isempty (this.VariableValues))
        out = 0;
      else
        out = size (this.VariableValues{1}, 1);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{W} =} width (@var{tbl})
    ##
    ## Number of variables in table.
    ##
    ## @code{@var{W} = width (@var{tbl})} returns the number of variables in the
    ## table, @var{tbl}.  It is the equivalent of @qcode{size (@var{tbl}, 2)}.
    ##
    ## Note that this is not the sum of the number of columns in each variable.
    ## It is just the number of variables.
    ##
    ## @end deftypefn
    function out = width (this)
      out = numel (this.VariableNames);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {} head (@var{tbl})
    ## @deftypefnx {table} {} head (@var{tbl}, @var{k})
    ## @deftypefnx {table} {@var{out} =} head (@var{tbl}, @var{k})
    ##
    ## Display or return the first K rows of table.
    ##
    ## @code{head (@var{tbl})} displays the first eight rows of the table
    ## @var{tbl}.  If there are less rows in @var{tbl}, @code{head} displays all
    ## rows available. @var{k} must be a positive integer scalar value.
    ##
    ## @code{head (@var{tbl}, @var{k})} displays the first @var{k} rows of the
    ## table @var{tbl}.  If there are less than @var{k} rows in @var{tbl},
    ## @code{head} displays all rows available.
    ##
    ## @code{@var{out} = head (@var{tbl}, @var{k})} returns the first @var{k}
    ## rows in a new table @var{out}. If @var{k} is omitted or empty, then it
    ## defaults to eight.  If there are less than @var{k} rows in @var{tbl},
    ## all rows available are returned.
    ##
    ## @end deftypefn
    function [varargout] = head (this, k)
      if (nargin < 2 || isempty (k))
        k = 8;
      endif
      if (! isscalar (k) || fix (k) != k || k <= 0)
        error ("table.head: K must be a positive integer scalar value.");
      endif
      nRows = height (this);
      if (nRows < k)
        out = this;
      else
        out = subsetrows (this, 1:k);
      endif
      if (nargout == 0)
        print_table (out);
      elseif (nargout == 1)
        varargout{1} = out;
      else
        error ("table.head: invalid number of output arguments.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {} tail (@var{tbl})
    ## @deftypefnx {table} {} tail (@var{tbl}, @var{k})
    ## @deftypefnx {table} {@var{out} =} tail (@var{tbl}, @var{k})
    ##
    ## Display or return the last K rows of table.
    ##
    ## @code{tail (@var{tbl})} displays the last eight rows of the table
    ## @var{tbl}.  If there are less rows in @var{tbl}, @code{tail} displays all
    ## rows available. @var{k} must be a positive integer scalar value.
    ##
    ## @code{tail (@var{tbl}, @var{k})} displays the last @var{k} rows of the
    ## table @var{tbl}.  If there are less than @var{k} rows in @var{tbl},
    ## @code{tail} displays all rows available.
    ##
    ## @code{@var{out} = tail (@var{tbl}, @var{k})} returns the last @var{k}
    ## rows in a new table @var{out}. If @var{k} is omitted or empty, then it
    ## defaults to eight.  If there are less than @var{k} rows in @var{tbl},
    ## all rows available are returned.
    ##
    ## @end deftypefn
    function [varargout] = tail (this, k)
      if (nargin < 2 || isempty (k))
        k = 8;
      endif
      if (! isscalar (k) || fix (k) != k || k <= 0)
        error ("table.tail: K must be a positive integer scalar value.");
      endif
      nRows = height (this);
      if (nRows < k)
        out = this;
      else
        out = subsetrows (this, [(nRows - (k - 1)):nRows]);
      endif
      if (nargout == 0)
        print_table (out);
      elseif (nargout == 1)
        varargout{1} = out;
      else
        error ("table.tail: invalid number of output arguments.");
      endif
    endfunction

  endmethods

################################################################################
##                     ** Sort, Filter, and Rearrange **                      ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'sortrows'         'unique'           'issortedrows'     'topkrows'        ##
## 'addvars'          'renamevars'       'movevars'         'removevars'      ##
## 'splitvars'        'mergevars'        'convertvars'      'rows2vars'       ##
## 'stack'            'unstack'          'inner2outer'      'addprop'         ##
## 'rmprop'                                                                   ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} sortrows (@var{tblA})
    ## @deftypefnx {table} {@var{tblB} =} sortrows (@var{tblA}, @qcode{'RowNames'})
    ## @deftypefnx {table} {@var{tblB} =} sortrows (@var{tblA}, @var{rowDimName})
    ## @deftypefnx {table} {@var{tblB} =} sortrows (@var{tblA}, @var{vars})
    ## @deftypefnx {table} {@var{tblB} =} sortrows (@var{tblA}, @dots{}, @var{direction})
    ## @deftypefnx {table} {@var{tblB} =} sortrows (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tblB}, @var{index}] =} sortrows (@dots{})
    ##
    ## Sort the rows of a table.
    ##
    ## @code{@var{tblB} = sortrows (@var{tblA})} sorts the rows in @var{tblA} in
    ## asceding order based on the values in the first variable.   If elements
    ## in the first variable are repeated, then @code{sortrows} sorts by the
    ## elements in the second variable, and so on.
    ##
    ## @code{@var{tblB} = sortrows (@var{tblA}, 'RowNames')} sorts the
    ## table @var{tblA} according to its row names.  If @var{tblA} does not
    ## have row names, i.e. @qcode{tblA.Properties.RowNames} is empty, then it
    ## returns @var{tblA}.
    ##
    ## @code{@var{tblB} = sortrows (@var{tblA}, @var{rowDimName})} also sorts
    ## the table @var{tblA} along the first dimension, @var{rowDimName}, which
    ## is the equivalent to the previous syntax, i.e. according to its row
    ## names.  If @var{tblA} does not have row names, that is
    ## @qcode{tblA.Properties.RowNames} is empty, then it returns @var{tblA}.
    ## For this syntax to work, @var{rowDimName} must match the first element in
    ## @qcode{tblA.Properties.DimensionNames}, otherwise @var{rowDimName} is
    ## considered a variable name, as in the following syntax.
    ##
    ## @code{@var{tblB} = sortrows (@var{tblA}, @var{vars})} sorts the rows in
    ## table @var{tblA} by the elements in the variables specified by
    ## @var{vars}, which can be a character vector (for a single variable) or a
    ## cell array of character vectors or a string array (specifying a single or
    ## multiple variables).  If @var{tblA} has row names, then @var{vars} can
    ## include the row names.  Alternatively, @var{vars} can be a logical vector
    ## or a numeric vector of real integers indexing the desired variables.
    ## Positive integers specify an ascending order, whereas negative integers
    ## specify a descending order for the referenced variables.  You can also
    ## index all available variables in @var{tblA} by passing a semicolon
    ## character argument.  This Octave specific syntax, facilitates the use of
    ## @var{direction} input argument, when no particular variable needs to be
    ## selected for sorting upon.  Additionally, @var{vars} can be a
    ## @qcode{vartype} object used to create a subscript that selects variables
    ## of a specified type.
    ##
    ## @code{@var{tblB} = sortrows (@var{tblA}, @dots{}, @var{direction})} sorts
    ## the rows in table @var{tblA} in the order specified by @var{direction}
    ## for any of the previous syntaxes.  @var{direction} can be
    ## @qcode{'ascend'} or @qcode{'descend'}, which is applied to all specified
    ## variables or row names that @code{sortrows} operates on.  @var{direction}
    ## can also be a cell array of character vectors, whose elements are
    ## @qcode{'ascend'} and @qcode{'descend'}, where each element corresponds to
    ## the specified variables and/or, row names used for sorting the table.
    ## The order specified by @var{direction} always takes precedence over the
    ## order defined by a numerical vector of integers in @var{vars}.
    ## @var{direction} must always be the 3rd inpnut argument.  If you want to
    ## omit passing selected variables and allow @code{sortrows} to work on
    ## consequtive variables until all ties are resolved, then you can leave the
    ## second input argument empty, as in
    ## @code{sortrows (@var{tblA}, @{[]@}, @var{direction})} or pass a
    ## colon argument for @var{vars} as in
    ## @code{sortrows (@var{tblA}, @{':'@}, @var{direction})}.
    ##
    ## @code{@var{tblB} = sortrows (@dots{}, @var{Name}, @var{Value})} specifies
    ## additional parameters for sorting rows of a table with the following
    ## Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{'MissingPlacement'} specifies the placement of missing
    ## values with one of the following options: @qcode{'auto'} places the
    ## missing elements at the bottom for ascending order and at the top for
    ## descending order; @qcode{'first'} places missing elements at the top;
    ## @qcode{'last'} places missing elements at the bottom.
    ## @item @qcode{'ComparisonMethod'} specifies the element comparison method
    ## with one of the following options: @qcode{'auto'} sorts rows using the
    ## real part for real numbers and the magnitude for complex numbers;
    ## @qcode{'real'} sorts rows using the real part for both real and complex
    ## numbers; @qcode{'abs'} sorts rows using the magnitude for both real and
    ## complex numbers.  For complex numbers with equal magnitude, the phase
    ## angle in the interval @math{(-π, π]} is further used to break ties.
    ## @end itemize
    ##
    ## @code{[@var{tblB}, @var{index}] = sortrows (@dots{})} also returns an
    ## index vector such that @qcode{@var{tblB} = @var{tblA}(@var{index},:)}.
    ##
    ## @end deftypefn
    function [tbl, index] = sortrows (this, varargin)

      ## Add defaults
      varRef = ':';
      doRowNames = false;
      inRowNames = 0;
      direction = {'ascend'};
      dir_given = false;

      ## Parse optional Name-Value paired arguments
      optNames = {"MissingPlacement", "ComparisonMethod"};
      dfValues = {"auto", "auto"};
      [MP, CM, args] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! ismember (MP, {"auto", "first", "last"}))
        error (["table.sortrows: 'MissingPlacement' parameter can be", ...
                " either 'auto', 'first', or 'last'."]);
      endif
      if (! ismember (CM, {"auto", "real", "abs"}))
        error (["table.sortrows: 'ComparisonMethod' parameter can be", ...
                " either 'auto', 'real', or 'abs'."]);
      endif

      ## Parse extra arguments
      nargs = numel (args);
      if (nargs > 2)
        error ("table.sortrows: invalid number of input arguments.");
      endif
      if (nargs > 1)
        direction = cellstr (args{2});
        dir_given = true;
        if (! all (ismember (direction, {"ascend", "descend"})))
          error ("table.sortrows: invalid value for DIRECTION argument.");
        endif
      endif
      if (nargs > 0)
        ## RowNames and rowDimName take precedence over variable names
        arg1 = args{1};
        if (ischar (arg1) && isvector (arg1) &&
            ismember (arg1, {"RowNames", this.DimensionNames{1}}))
          ## Check user's direction is scalar
          if (dir_given && numel (direction) != 1)
            error (["table.sortrows: DIRECTION must be a scalar input", ...
                    " when 'RowNames' or 'rowDimNames' are selected."]);
          endif
          ## Handle special case here
          if (isempty (this.RowNames))
            tbl = this;
            index = [1:height(this)]';
            return
          else
            [~, index] = sort (this.RowNames, direction{:});
            tbl = subsetrows (this, index);
            return
          endif
        endif

        ## At this point, VARS must be variable name(s)
        if (islogical (arg1))
          varRef = arg1;
          if (! (isvector (varRef) && numel (varRef) == width (this)))
            error (["table.sortrows: logical indexing vector", ...
                    " does not match table width."]);
          endif
          ## Check user's direction matches selected variables
          if (! isscalar (direction))
            if (dir_given && sum (varRef) != numel (direction))
              error ("table.sortrows: invalid size for DIRECTION argument.");
            endif
          endif
        elseif (isnumeric (arg1))
          if (isempty (arg1))
            arg1 = [1:width(this)];
          endif
          if (! isvector (arg1) || any (fix (arg1) != arg1) || any (arg1 == 0))
            error (["table.sortrows: numerical indexing must", ...
                    " be a vector of nonzero integers."]);
          endif
          if (any (abs (arg1) > width (this)))
            error ("table.sortrows: numerical index exceeds table dimensions.");
          endif
          varRef = arg1;
          ## If direction was given, ignore sign of numerical indexing
          if (dir_given)
            varRef = abs (varRef);
            ## Check user's direction matches selected variables
            if (! isscalar (direction))
              if (! isequal (size (varRef), size (direction)))
                error ("table.sortrows: invalid size for DIRECTION argument.");
              endif
            endif
          else
            direction = cell (1, numel (varRef));
            direction(sign (varRef) > 0) = "ascend";
            direction(sign (varRef) < 0) = "descend";
            varRef = abs (varRef);
          endif
        elseif (ischar (arg1) || iscellstr (arg1) || isa (arg1, "string"))
          varRef = cellstr (arg1);
          if (isscalar (varRef) && strcmp (varRef, ":"))
            varRef = ':';
          elseif (! all (ismember (varRef, [this.VariableNames, {"RowNames"}])))
            error ("table.sortrows: VARS indexes non-existing variable names.");
          endif
          ## Check user's direction matches selected variables
          if (! isscalar (direction))
            if (strcmp (varRef, ':') && numel (direction) != width (this))
              error ("table.sortrows: invalid size for DIRECTION argument.");
            elseif (! isequal (size (varRef), size (direction)))
              error ("table.sortrows: invalid size for DIRECTION argument.");
            endif
          endif
          ## Check whether 'RowNames' are included in the indexed variables
          if (any (ismember (varRef, "RowNames")))
            inRowNames = find (strcmp (varRef, "RowNames"));
            varRef(inRowNames) = [];
          endif
        elseif (isa (arg1, "vartype"))
          varRef = arg1;
          ## Check user's direction is scalar
          if (dir_given && numel (direction) != 1)
            error (["table.sortrows: DIRECTION must be a scalar input", ...
                    " when variables are indexed with a 'vartype' object."]);
          endif
        endif
      endif

      ## Resolve varRef to variables' indices
      ixVars = resolveVarRef (this, varRef);

      ## Build a cell array for the selected variables to be used in sorting
      if (inRowNames == 0)
        varVal = cell (1, numel (ixVars));
      else
        varVal = cell (1, numel (ixVars) + 1);
      endif

      ## Expand direction if it is a scalar
      if (isscalar (direction))
        direction = repmat (direction, 1, numel (varVal));
      endif

      ## Populate cell array for sorting
      offset = 0;
      for ix = 1:numel (varVal)
        if (inRowNames == ix)
          varVal(ix) = {this.RowNames};
          offset = 1;
        else
          varVal(ix) = this.VariableValues(ixVars(ix - offset));
        endif
      endfor

      ## Prepare a proxy array by converting all variable to numeric proxies
      varValIdx = [];
      varValDir = [];
      for ix = 1:numel (varVal)

        tmpVal = varVal{ix};
        if (strcmpi (direction{ix}, "ascend"))
          tmpDir = 1;
        else
          tmpDir = -1;
        endif

        if (isa (tmpVal, "categorical"))
          tmpVal = double (tmpVal);
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, "calendarDuration"))
          tmpVal = tmpVal.proxyArray;
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, "datetime"))
          tmpVal = datenum (tmpVal);
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, "duration"))
          tmpVal = tmpVal.days;
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, "string"))
          tmpVal = cellstr (tmpVal);
          [~, ~, idx] = __unique__ (tmpVal, 'rows');
          varValIdx = [varValIdx, idx];

        elseif (iscellstr (tmpVal))
          [~, ~, idx] = __unique__ (tmpVal, 'rows');
          varValIdx = [varValIdx, idx];

        elseif (iscell (tmpVal))
          ## Sorting mixed cell data is not supported
          error ("table.sortrows: cannot sort variables of 'cell' type.");

        elseif (isnumeric (tmpVal))
          if (strcmpi (CM, "real") && iscomplex (tmpVal))
            tmpVal = real (tmpVal);
          elseif (strcmpi (CM, "abs") && isreal (tmpVal))
            tmpVal = abs (tmpVal);
          endif
          varValIdx = [varValIdx, tmpVal];

        elseif (isstruct (tmpVal))
          ## Sorting structure data is not supported
          error ("table.sortrows: cannot sort variables of 'struct' type.");

        elseif (isa (tmpVal, "table"))
          try
            tmpVal = table2array (varVal{ix});
            varValIdx = [varValIdx, tmpVal];
          catch
            error (["table.sortrows: cannot sort nested tables", ...
                    " with mixed data types."]);
          end_try_catch
        endif
        tmpDir = repmat (tmpDir, 1, size (tmpVal, 2));
        varValDir = [varValDir, tmpDir];
      endfor

      ## Fix direction vector
      varValDir = [1:numel(varValDir)] .* varValDir;

      ## Do the actual sorting here
      [~, index] = sortrows (varValIdx, varValDir);
      tbl = subsetrows (this, index);
      index = index(:);

      ## Fix missing placement
      TF = ismissing (tbl);
      TFvec = TF(:,ixVars(1));
      if (any (TFvec) && ! all (TFvec))
        is_nan = index(TFvec);
        no_nan = index(! TFvec);
        if (any (find (TFvec) == 1) && strcmpi (MP, "last"))
          index = [no_nan; is_nan];
        elseif (any (find (TFvec) == numel (index)) && strcmpi (MP, "first"))
          index = [is_nan; no_nan];
        endif
        tbl = subsetrows (this, index);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} unique (@var{tblA})
    ## @deftypefnx {table} {@var{tblB} =} unique (@var{tblA}, @var{setOrder})
    ## @deftypefnx {table} {@var{tblB} =} unique (@var{tblA}, @var{occurence})
    ## @deftypefnx {table} {[@var{tblB}, @var{ixA}, @var{ixB}] =} unique (@dots{})
    ##
    ## Unique rows in a table.
    ##
    ## @code{@var{tblB} = unique (@var{tblA})} returns the unique rows of table
    ## @var{tblA} in sorted order.
    ##
    ## @code{@var{tblB} = unique (@var{tblA}, @var{setOrder})} returns the
    ## unique rows of table @var{tblA} in a specified order.  @var{setOrder} can
    ## be either @qcode{"sorted"} (default) or @qcode{"stable"}.
    ##
    ## @itemize
    ## @item @qcode{'sorted'} returns the unique rows sorted in ascending order.
    ## @item @qcode{'stable'} returns the unique rows according to their order
    ## of occurence.
    ## @end itemize
    ##
    ## @code{@var{tblB} = unique (@var{tblA}, @var{occurence})} returns the
    ## unique rows of table @var{tblA} according to their order of occurence.
    ## @var{occurence} cen be either @qcode{'first'} (default) or @qcode{'last'}.
    ##
    ## @itemize
    ## @item @qcode{'first'} returns the first occurence of each unique row,
    ## i.e. the lowest possible indices are returned.
    ## @item @qcode{'last'} returns the last occurence of each unique row, i.e.
    ## the highest possible indices are returned.
    ## @end itemize
    ##
    ## @code{[@var{tblB}, @var{ixA}, @var{ixB}] = unique (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} using any of the previous syntaxes.
    ## @var{ixA} and @var{ixB} map the tables @var{tblA} and @var{tblB} to one
    ## another such that @qcode{@var{tblB} = @var{tblA}(@var{ixA},:)} and
    ## @qcode{@var{tblA} = @var{tblB}(@var{ixB},:)}.
    ##
    ## @end deftypefn
    function [tbl, ia, ic] = unique (this, varargin)

      ## Check max number of input arguments
      if (nargin > 2)
        error ("table.unique: too many input arguments.");
      endif

      ## Handle 'setOrder' and 'occurence' options
      opt = "sorted";
      if (! isempty (varargin))
        if (any (strcmp (varargin{1}, {"sorted", "stable", "first", "last"})))
          opt = varargin{1};
        else
          error ("table.unique: invalid option '%s'.", varargin{1});
        endif
      endif

      ## Prepare a proxy array by converting all variables to numeric proxies
      varProxy = [];
      for ix = 1:width (this)

        varVal = this.VariableValues{ix};
        if (isa (varVal, "categorical"))
          varVal = double (varVal);
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, "calendarDuration"))
          varVal = varVal.proxyArray;
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, "datetime"))
          varVal = datenum (varVal);
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, "duration"))
          varVal = varVal.days;
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, "string"))
          varVal = cellstr (varVal);
          [~, ~, idx] = __unique__ (varVal, "stable", "rows");
          varProxy = [varProxy, idx];

        elseif (iscellstr (varVal))
          [~, ~, idx] = __unique__ (varVal, "stable", "rows");
          varProxy = [varProxy, idx];

        elseif (iscell (varVal))
          ## Sorting mixed cell data is not supported
          error ("table.sortrows: cannot sort variables of 'cell' type.");

        elseif (isnumeric (varVal))
          varProxy = [varProxy, varVal];

        elseif (isstruct (varVal))
          ## Sorting structure data is not supported
          error ("table.sortrows: cannot sort variables of 'struct' type.");

        elseif (isa (varVal, "table"))
          try
            varVal = table2array (varVal{ix});
            varProxy = [varProxy, varVal];
          catch
            error (["table.sortrows: cannot sort nested tables", ...
                    " with mixed data types."]);
          end_try_catch
        endif
      endfor

      ## Find unique rows in proxy table
      [~, ia, ic] = __unique__ (varProxy, opt, "rows");
      ## Return unique table
      tbl = subsetrows (this, ia);

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{TF} =} issortedrows (@var{tblA})
    ## @deftypefnx {table} {@var{TF} =} issortedrows (@var{tblA}, @qcode{'RowNames'})
    ## @deftypefnx {table} {@var{TF} =} issortedrows (@var{tblA}, @var{rowDimName})
    ## @deftypefnx {table} {@var{TF} =} issortedrows (@var{tblA}, @var{vars})
    ## @deftypefnx {table} {@var{TF} =} issortedrows (@var{tblA}, @dots{}, @var{direction})
    ## @deftypefnx {table} {@var{TF} =} issortedrows (@dots{}, @var{Name}, @var{Value})
    ##
    ## Check if table rows are sorted accordingly.
    ##
    ## @code{@var{TF} = issortedrows (@var{tblA})} determines if the rows in
    ## @var{tblA} are sorted in asceding order based on the values in the first
    ## variable or subsequent variables if elements of the former are repeated.
    ## @var{TF} is a logical scalar and it is @qcode{true} when
    ## @code{@var{tblA} == sortrows (@var{tblA})} or @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = issortedrows (@var{tblA}, 'RowNames')}
    ## determines if the rows in @var{tblA} are sorted according to its row
    ## names.  @var{TF} is @qcode{true} when @code{@var{tblA} == sortrows
    ## (@var{tblA}, 'RowNames')} or @qcode{false} otherwise.  If
    ## @var{tblA} does not have row names, i.e. @qcode{tblA.Properties.RowNames}
    ## is empty, then @var{TF} is @qcode{true}.
    ##
    ## @code{@var{TF} = issortedrows (@var{tblA}, @var{rowDimName})} determines
    ## if the rows in table @var{tblA} are sorted along the first dimension,
    ## @var{rowDimName}, which is the equivalent to the previous syntax, i.e.
    ## according to its row names. For this syntax to work, @var{rowDimName}
    ## must match the first element in @qcode{tblA.Properties.DimensionNames},
    ## otherwise @var{rowDimName} is considered a variable name, as in the
    ## following syntax.  @var{TF} is @qcode{true} when @code{@var{tblA} ==
    ## sortrows (@var{tblA}, @var{rowDimName})} or @qcode{false} otherwise.  If
    ## @var{tblA} does not have row names, i.e. @qcode{tblA.Properties.RowNames}
    ## is empty, then @var{TF} is @qcode{true}.
    ##
    ## @code{@var{TF} = issortedrows (@var{tblA}, @var{vars})} determines if the
    ## rowns in @var{tblA} are sorted by the elements in the variables specified
    ## by @var{vars}, which can be a character vector (for a single variable) or
    ## a cell array of character vectors or a string array (specifying a single
    ## or multiple variables).  If @var{tblA} has row names, then @var{vars} can
    ## include the row names.  Alternatively, @var{vars} can be a logical vector
    ## or a numeric vector of real integers indexing the desired variables.
    ## Positive integers specify an ascending order, whereas negative integers
    ## specify a descending order for the referenced variables.  You can also
    ## index all available variables in @var{tblA} by passing a semicolon
    ## character argument.  This Octave specific syntax, facilitates the use of
    ## @var{direction} input argument, when no particular variable needs to be
    ## selected for sorting upon.  Additionally, @var{vars} can be a
    ## @qcode{vartype} object used to create a subscript that selects variables
    ## of a specified type.
    ##
    ## @code{@var{TF} = issortedrows (@var{tblA}, @dots{}, @var{direction})}
    ## determines if the rows in @var{tblA} are sorted in the order specified by
    ## @var{direction} for any of the previous syntaxes.  @var{direction} can be
    ## @qcode{'ascend'} or @qcode{'descend'}, which is applied to all specified
    ## variables or row names that @code{sortrows} operates on.  @var{direction}
    ## can also be a cell array of character vectors, whose elements are
    ## @qcode{'ascend'} and @qcode{'descend'}, where each element corresponds to
    ## the specified variables and/or, row names used for sorting the table.
    ## The order specified by @var{direction} always takes precedence over the
    ## order defined by a numerical vector of integers in @var{vars}.
    ## @var{direction} must always be the 3rd inpnut argument.  If you want to
    ## omit passing selected variables and allow @code{sortrows} to work on
    ## consequtive variables until all ties are resolved, then you can leave the
    ## second input argument empty, as in
    ## @code{sortrows (@var{tblA}, @{[]@}, @var{direction})} or pass a
    ## colon argument for @var{vars} as in
    ## @code{sortrows (@var{tblA}, @{':'@}, @var{direction})}.
    ##
    ## @code{@var{TF} = issortedrows (@dots{}, @var{Name}, @var{Value})}
    ## determines if the rows in @var{tblA} are sorted accoring the additional
    ## parameters specifying the sorting of rows of a table with the following
    ## Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{'MissingPlacement'} specifies the placement of missing
    ## values with one of the following options: @qcode{'auto'} places the
    ## missing elements at the bottom for ascending order and at the top for
    ## descending order; @qcode{'first'} places missing elements at the top;
    ## @qcode{'last'} places missing elements at the bottom.
    ## @item @qcode{'ComparisonMethod'} specifies the element comparison method
    ## with one of the following options: @qcode{'auto'} sorts rows using the
    ## real part for real numbers and the magnitude for complex numbers;
    ## @qcode{'real'} sorts rows using the real part for both real and complex
    ## numbers; @qcode{'abs'} sorts rows using the magnitude for both real and
    ## complex numbers.  For complex numbers with equal magnitude, the phase
    ## angle in the interval @math{(-π, π]} is further used to break ties.
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = issortedrows (this, varargin)
      ## Get indices of sorted table according to the user's options
      [~, ix] = sortrows (this, varargin{:});
      ## Check that indices match the current order
      TF = isequal (ix', 1:height (this));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} topkrows (@var{tblA}, @var{k})
    ## @deftypefnx {table} {@var{tblB} =} topkrows (@var{tblA}, @var{k}, @qcode{'RowNames'})
    ## @deftypefnx {table} {@var{tblB} =} topkrows (@var{tblA}, @var{k}, @var{rowDimName})
    ## @deftypefnx {table} {@var{tblB} =} topkrows (@var{tblA}, @var{k}, @var{vars})
    ## @deftypefnx {table} {@var{tblB} =} topkrows (@var{tblA}, @var{k}, @dots{}, @var{direction})
    ## @deftypefnx {table} {@var{tblB} =} topkrows (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tblB}, @var{index}] =} topkrows (@dots{})
    ##
    ## Sort the rows of a table.
    ##
    ## @code{@var{tblB} = topkrows (@var{tblA}, @var{k})} returns the top
    ## @var{k} rows from table @var{tblA} sorted in asceding order based on the
    ## values in the first variable.   If elements in the first variable are
    ## repeated, then @code{topkrows} sorts by the elements in the second
    ## variable, and so on.
    ##
    ## @code{@var{tblB} = topkrows (@var{tblA}, @var{k}, 'RowNames')} returns
    ## the top @var{k} rows from table @var{tblA} sorted according to its row
    ## names.  If @var{tblA} does not have row names, i.e.
    ## @qcode{tblA.Properties.RowNames} is empty, then it returns @var{tblA}.
    ##
    ## @code{@var{tblB} = topkrows (@var{tblA}, @var{k}, @var{rowDimName})} also
    ## returns the top @var{k} rows from table @var{tblA} sorted along its first
    ## dimension, @var{rowDimName}, which is the equivalent to the previous
    ## syntax, i.e. according to its row names.  If @var{tblA} does not have row
    ## names, i.e. @qcode{tblA.Properties.RowNames} is empty, then it returns
    ## @var{tblA}.  For this syntax to work, @var{rowDimName} must match the
    ## first element in @qcode{tblA.Properties.DimensionNames}, otherwise
    ## @var{rowDimName} is considered a variable name, as in the following
    ## syntax.
    ##
    ## @code{@var{tblB} = topkrows (@var{tblA}, @var{k}, @var{vars})} returns
    ## the top @var{k} rows from table @var{tblA} sorted by the elements in the
    ## variables specified by @var{vars}, which can be a character vector (for a
    ## single variable) or a cell array of character vectors or a string array
    ## (specifying a single or multiple variables).  If @var{tblA} has row
    ## names, then @var{vars} can include the row names.  Alternatively,
    ## @var{vars} can be a logical vector or a numeric vector of real integers
    ## indexing the desired variables.  Positive integers specify an ascending
    ## order, whereas negative integers specify a descending order for the
    ## referenced variables.  You can also index all available variables in
    ## @var{tblA} by passing a semicolon character argument.  This Octave
    ## specific syntax, facilitates the use of @var{direction} input argument,
    ## when no particular variable needs to be selected for sorting upon.
    ## Additionally, @var{vars} can be a @qcode{vartype} object used to create a
    ## subscript that selects variables of a specified type.
    ##
    ## @code{@var{tblB} = topkrows (@var{tblA}, @var{k}, @dots{},
    ## @var{direction})} returns the top @var{k} rows from table @var{tblA}
    ## sorted in the order specified by @var{direction} for any of the previous
    ## syntaxes.  @var{direction} can be @qcode{'ascend'} or @qcode{'descend'},
    ## which is applied to all specified variables or row names that
    ## @code{sortrows} operates on.  @var{direction} can also be a cell array of
    ## character vectors, whose elements are @qcode{'ascend'} and
    ## @qcode{'descend'}, where each element corresponds to the specified
    ## variables and/or, row names used for sorting the table.  The order
    ## specified by @var{direction} always takes precedence over the order
    ## defined by a numerical vector of integers in @var{vars}.  @var{direction}
    ## must always be the 3rd inpnut argument.  If you want to omit passing
    ## selected variables and allow @code{sortrows} to work on consequtive
    ## variables until all ties are resolved, then you can leave the second
    ## input argument empty, as in
    ## @code{sortrows (@var{tblA}, @{[]@}, @var{direction})} or pass a
    ## colon argument for @var{vars} as in
    ## @code{sortrows (@var{tblA}, @{':'@}, @var{direction})} or pass a
    ##
    ## @code{@var{tblB} = topkrows (@dots{}, @var{k}, @var{Name}, @var{Value})}
    ## returns the top @var{k} rows from table @var{tblA} sorted with any of the
    ## previous syntaxes and further specified by additional parameters for
    ## sorting rows of a table with the following Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{'MissingPlacement'} specifies the placement of missing
    ## values with one of the following options: @qcode{'auto'} places the
    ## missing elements at the bottom for ascending order and at the top for
    ## descending order; @qcode{'first'} places missing elements at the top;
    ## @qcode{'last'} places missing elements at the bottom.
    ## @item @qcode{'ComparisonMethod'} specifies the element comparison method
    ## with one of the following options: @qcode{'auto'} sorts rows using the
    ## real part for real numbers and the magnitude for complex numbers;
    ## @qcode{'real'} sorts rows using the real part for both real and complex
    ## numbers; @qcode{'abs'} sorts rows using the magnitude for both real and
    ## complex numbers.  For complex numbers with equal magnitude, the phase
    ## angle in the interval @math{(-π, π]} is further used to break ties.
    ## @end itemize
    ##
    ## @code{[@var{tblB}, @var{index}] = topkrows (@dots{})} also returns an
    ## index vector such that @qcode{@var{tblB} = @var{tblA}(@var{index},:)}.
    ##
    ## @end deftypefn
    function [tbl, ix] = topkrows (this, k, varargin)
      ## Check for valid k
      if (k < 0 || fix (k) != k || ! isscalar (k))
        error ("table.topkrows: K must be a nonnegative integer scalar.");
      endif
      ## Sort the table and retain the indices
      [tbl, ix] = sortrows (this, varargin{:});
      if (k < height (tbl))
        tbl = subsetrows (tbl, 1:k);
        ix = ix(1:k);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} addvars (@var{tblA}, @var{var1}, @dots{}, @var{varN})
    ## @deftypefnx {table} {@var{tblB} =} addvars (@dots{}, @qcode{'After'}, @var{location})
    ## @deftypefnx {table} {@var{tblB} =} addvars (@dots{}, @qcode{'Before'}, @var{location})
    ## @deftypefnx {table} {@var{tblB} =} addvars (@dots{}, @qcode{'NewVariableNames'}, @var{newNames})
    ##
    ## Add new variables to a table.
    ##
    ## @code{@var{tblB} = addvars (@var{tblA}, @var{var1}, @dots{}, @var{varN})}
    ## adds new variables to the right of the last variable in table @var{tblA}.
    ## Each of the arrays specified by the input arguments @qcode{@var{var1},
    ## @dots{}, @var{varN}} becomes a new variable and its name is derived from
    ## the input argument's variable name or a default is created if the input
    ## argument is not a variable itself.  The input arrays can be of any data
    ## type including a table as long as they have the same number of rows as
    ## @var{tblA}.
    ##
    ## @code{@var{tblB} = addvars (@dots{}, @code{'After'}, @var{location})}
    ## adds the new variables after, i.e. to the right, the table variable
    ## specified in @var{location}, which can be a character vector, a scalar
    ## integer value or even a logical vector of the same size as
    ## @qcode{width (@var{tblA})}, as long as it indexes a single variable in
    ## @var{tblA}.
    ##
    ## @code{@var{tblB} = addvars (@dots{}, @code{'Before'}, @var{location})}
    ## adds the new variables before, i.e. to the left, the table variable
    ## specified in @var{location}, which can be a character vector, a scalar
    ## integer value or even a logical vector of the same size as
    ## @qcode{width (@var{tblA})}, as long as it indexes a single variable in
    ## @var{tblA}.
    ##
    ## @code{@var{tblB} = addvars (@dots{}, @code{'NewVariableNames'},
    ## @var{newNames})} renames the new variables added from the previous
    ## syntaxes according to the names specified by @var{newNames}, which can be
    ## a character vector, a cell array of character vectors or a string array.
    ## The number of names in @var{newNames} must be the same as the number of
    ## added variables.
    ##
    ## @end deftypefn
    function tbl = addvars (this, varargin)

      ## Add defaults
      tbl_width = width (this);
      ix_insert = tbl_width;
      AB_insert = true;   # after by default

      ## Parse optional Name-Value paired arguments
      optNames = {"After", "Before", "NewVariableNames"};
      dfValues = {[], [], []};
      [After, Before, newVarNames, args] = pairedArgs ...
                                           (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! isempty (After) && ! isempty (Before))
        error ("table.addvars: cannot use both 'After' and 'Before' options.");
      endif
      ## All other errors will be handled by 'resolveVarRef' for invalid input
      msg_error1 = ["table.addvars: LOCATION must index a single variable."];
      msg_error2 = ["table.addvars: LOCATION must be either a scalar", ...
                    " integer, a character vector, or a logical vector", ...
                    " indexing a single table variable."];
      if (! isempty (After))
        if ((isnumeric (After) && isscalar (After)) || ischar (After))
          ix_insert = resolveVarRef (this, After);
        elseif (isvector (After) && islogical (After))
          ix_insert = resolveVarRef (this, After);
          if (numel (ix_insert) > 1)
            error (msg_error1);
          endif
        else
          error (msg_error2);
        endif
      elseif (! isempty (Before))
        if ((isnumeric (Before) && isscalar (Before)) || ischar (Before))
          ix_insert = resolveVarRef (this, Before);
          AB_insert = false;
        elseif (isvector (After) && islogical (After))
          ix_insert = resolveVarRef (this, After);
          if (numel (ix_insert) > 1)
            error (msg_error1);
          endif
        else
          error (msg_error2);
        endif
      endif
      if (isempty (newVarNames))
        ## Create names for new variables
        offset = width (this);   # for incrementing automatic variable naming
        newVarNames = cell (size (args));
        for i = 1:numel (args)
          newVarNames{i} = inputname (i+1);
          if (isempty (newVarNames{i}))
            newVarNames{i} = sprintf ("Var%d", i + offset);
            ## Catch case that Var1 ... already exists
            while (ismember (newVarNames{i}, this.VariableNames))
              newVarNames{i} = sprintf ("Var%d", i + offset);
              offset++;
            endwhile
          endif
        endfor
      else
        ## Force to cellstr (in case of string array)
        newVarNames = cellstr (newVarNames);
        if (numel (args) != numel (newVarNames))
          error (["table.addvars: NEWNAMES does not match the number", ...
                  " of new variables."]);
        endif
        if (numel (__unique__ (newVarNames)) != numel (newVarNames))
          error ("table.addvars: NEWNAMES contains duplicate names.");
        endif
        idx = ismember (newVarNames, this.VariableNames);
        if (any (idx))
          if (sum (idx) == 1)
            error ("table.addvars: new variable name '%s' already exists.", ...
                   newVarNames{idx});
          else
            msg_error3 = sprintf ("'%s', ", newVarNames{idx});
            msg_error3(end-1:end) = [];
            error ("table.addvars: new variable names %s already exist.", ...
                   msg_error3);
          endif
        endif
      endif

      ## Append the new variables
      tbl = this;
      for i = 1:numel (args)
        tbl = setvar (tbl, newVarNames{i}, args{i});
      endfor

      ## Relocate new variables (if necessary)
      if (AB_insert)  # after
        if (ix_insert < tbl_width)
          ix_L = [1:ix_insert];
          ix_M = [tbl_width+1:tbl_width+numel(args)];
          ix_R = [ix_insert+1:tbl_width];
          ixVars = [ix_L, ix_M, ix_R];
          tbl = subsetvars (tbl, ixVars);
        endif
      else            # before
        if (ix_insert > 1)
          ix_L = [1:ix_insert-1];
          ix_M = [tbl_width+1:tbl_width+numel(args)];
          ix_R = [ix_insert:tbl_width];
          ixVars = [ix_L, ix_M, ix_R];
        else
          ixVars = [tbl_width+1:tbl_width+numel(args), 1:tbl_width];
        endif
        tbl = subsetvars (tbl, ixVars);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} renamevars (@var{tblA}, @var{vars}, @var{newNames})
    ##
    ## Rename variables in a table.
    ##
    ## @code{@var{tblB} = renamevars (@var{tblA}, @var{vars}, @var{newNames})}
    ## renames the selected variables in the table @var{tblA} specified by
    ## @var{vars} using the names in @var{newNames}.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables).
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @var{newNames} can either be a character vector (when renaming a single
    ## variable) or a cell array of character vectors or a string array.  The
    ## number of names specified by @var{newNames} must match the number of
    ## variables specified by @var{vars}.
    ##
    ## @end deftypefn
    function tbl = renamevars (this, vars, newNames)

      ## Check input arguments
      if (nargin < 3 || isempty (vars) || isempty (newNames))
        error ("table.renamevars: too few input arguments.");
      endif
      if (! iscellstr (newNames) && ! isa (newNames, "string") &&
          ! (ischar (newNames) && isvector (newNames)))
        error (["table.renamevars: NEWNAMES must be either a character", ...
                " vector, a cell array of character vectors, or a string", ...
                " array."]);
      endif

      ## Force to cellstring and get indices
      newNames = cellstr (newNames);
      if (numel (__unique__ (newNames)) != numel (newNames))
        error ("table.renamevars: NEWNAMES contains duplicate names.");
      endif
      ixVars = resolveVarRef (this, vars, "lenient");

      ## Check selected variables
      if (any (ixVars == 0))
        error ("table.renamevars: cannot index non-existing variable: '%s'.",...
               vars{find (ixVars == 0)});
      elseif (numel (ixVars) != numel (newNames))
        error (["table.renamevars: number of names in NEWNAMES do not", ...
                " match the selected variables specified by VARS."]);
      endif

      ## Rename the indexed variables
      tbl = this;
      tbl.VariableNames(ixVars) = newNames;

      ## Check for duplicate names
      if (numel (__unique__ (tbl.VariableNames)) != numel (tbl.VariableNames))
        error ("table.renamevars: newly assigned variable name already exists.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} movevars (@var{tblA}, @var{vars})
    ## @deftypefnx {table} {@var{tblB} =} movevars (@dots{}, @qcode{'After'}, @var{location})
    ## @deftypefnx {table} {@var{tblB} =} movevars (@dots{}, @qcode{'Before'}, @var{location})
    ##
    ## Move variables in a table.
    ##
    ## @code{@var{tblB} = movevars (@var{tblA}, @var{vars})} moves the variables
    ## specified by @var{vars} to the end of the input table @var{tblA}.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @code{@var{tblB} = movevars (@dots{}, @code{'After'}, @var{location})}
    ## moves the selected variables after, i.e. to the right, the table variable
    ## specified in @var{location}, which can be a character vector, a scalar
    ## integer value or even a logical vector of the same size as
    ## @qcode{width (@var{tblA})}, as long as it indexes a single variable in
    ## @var{tblA} which is not selected by @var{vars}.
    ##
    ## @code{@var{tblB} = movevars (@dots{}, @code{'Before'}, @var{location})}
    ## moves the selected variables before, i.e. to the left, the table variable
    ## specified in @var{location}, which can be a character vector, a scalar
    ## integer value or even a logical vector of the same size as
    ## @qcode{width (@var{tblA})}, as long as it indexes a single variable in
    ## @var{tblA} which is not selected by @var{vars}.
    ##
    ## @end deftypefn
    function tbl = movevars (this, vars, varargin)

      ## Check input argument
      if (nargin < 2 || isempty (vars))
        error ("table.movevars: too few input arguments.");
      endif

      ## Add defaults
      tbl_width = width (this);
      ix_insert = tbl_width;
      AB_insert = true;   # after by default

      ## Parse optional Name-Value paired arguments
      optNames = {"After", "Before"};
      dfValues = {[], []};
      [After, Before] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! isempty (After) && ! isempty (Before))
        error ("table.movevars: cannot use both 'After' and 'Before' options.");
      endif

      ## All other errors will be handled by 'resolveVarRef' for invalid input
      msg_error1 = "table.movevars: LOCATION must index a single variable.";
      msg_error2 = ["table.movevars: LOCATION must be either a scalar", ...
                    " integer, a character vector, or a logical vector", ...
                    " indexing a single table variable."];
      msg_error3 = ["table.movevars: LOCATION does not index an", ...
                    " existing variable."];

      if (! isempty (After) || ! isempty (Before))
        if (! isempty (Before))
          AB_insert = false;
          After = Before;
        endif
        if ((isnumeric (After) && isscalar (After)) || ischar (After))
          ix_insert = resolveVarRef (this, After, "lenient");
        elseif (isvector (After) && islogical (After))
          ix_insert = resolveVarRef (this, After, "lenient");
          if (numel (ix_insert) > 1)
            error (msg_error1);
          endif
        else
          error (msg_error2);
        endif
        ## Grab silent errors returned by 'resolveVarRef'
        if (any (ix_insert == 0))
          error (msg_error3);
        endif
      endif

      ## Get variables to be moved
      mvVar = resolveVarRef (this, vars, "lenient");
      if (any (mvVar == 0))
        vars = cellstr (vars);
        error ("table.movevars: cannot index non-existing variable: '%s'.", ...
               vars{find (mvVar == 0)});
      endif

      ## Get variables that remain static
      stVar = 1:tbl_width;
      stVar(mvVar) = [];

      ## Construct remapping vector
      if (AB_insert)  # after
        if (ix_insert < tbl_width)
          ## Check LOCATION variable is a static one
          if (ismember (ix_insert, mvVar))
            error ("table.movevars: LOCATION variable cannot be moved.");
          endif
          ix_L = stVar(stVar <= ix_insert);
          ix_R = stVar(stVar > ix_insert);
          ixVars = [ix_L, mvVar, ix_R];
        else
          ixVars = [stVar, mvVar];
        endif
      else            # before
        if (ix_insert > 1)
          ## Check LOCATION variable is a static one
          if (ismember (ix_insert, mvVar))
            error ("table.movevars: LOCATION variable cannot be moved.");
          endif
          ix_L = stVar(stVar < ix_insert);
          ix_R = stVar(stVar >= ix_insert);
          ixVars = [ix_L, mvVar, ix_R];
        else
          ixVars = [mvVar, stVar];
        endif
      endif

      ## Return remapped table
      tbl = subsetvars (this, ixVars);

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} removevars (@var{tblA}, @var{vars})
    ##
    ## Remove variables from a table.
    ##
    ## @code{@var{tblB} = removevars (@var{tblA}, @var{vars})} removes the
    ## variables specified by @var{vars} from the input table @var{tblA}.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables).
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @end deftypefn
    function tbl = removevars (this, vars)

      ## Check input argument
      if (nargin < 2 || isempty (vars))
        error ("table.removevars: too few input arguments.");
      endif

      ## Resolve variables to be removed
      ixVar = resolveVarRef (this, vars);

      ## Remove selected variables
      tbl = this;
      tbl.VariableTypes(ixVar) = [];
      tbl.VariableNames(ixVar) = [];
      tbl.VariableValues(ixVar) = [];
      tbl.VariableDescriptions(ixVar) = [];
      tbl.VariableUnits(ixVar) = [];

      ## Check for custom variable properties and remove accordingly
      if (! isempty (this.CustomProperties))
        cpIdx = strcmp (this.CustomPropTypes, "variable");
        if (any (cpIdx))
          ## Get the fieldnames of custom variable properties
          cpNames = fieldnames (this.CustomProperties);
          cpNames = cpNames(cpIdx);
          ## Remove referenced variable values from custom variable properties
          for i = 1:numel (cpNames)
            tmp = this.CustomProperties.(cpNames{i});
            if (! isempty (tmp))
              tmp(ixVar) = [];
              tbl.CustomProperties.(cpNames{i}) = tmp;
            endif
          endfor
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} splitvars (@var{tblA})
    ## @deftypefnx {table} {@var{tblB} =} splitvars (@var{tblA}, @var{vars})
    ## @deftypefnx {table} {@var{tblB} =} splitvars (@dots{}, @qcode{'NewVariableNames'}, @var{NewNames})
    ##
    ## Split multicolumn variables in a table.
    ##
    ## @code{@var{tblB} = splitvars (@var{tblA})} splits mutlicolumn variables
    ## in @var{tblA} so that they are single-column variables in @var{tblB},
    ## while all single-column variables in @var{tblA} are copied to @var{tblB}
    ## unaltered. Each newly created single-column variable in @var{tblB} is
    ## uniquely named by joining the name of its parent multicolumn variable in
    ## @var{tblA} and the corresponding column number.  If a variable in
    ## @var{tblA} contains a table, then each variable of this nested table is
    ## returned as a newly created variable in @var{tblB}. By default, these
    ## variables retain their original name in the nested table, unless there
    ## are duplicate names, in which case the name of the nested table is also
    ## used.  If the nested table in @var{tblA} contains a multicolumn variable,
    ## then the newly created variable in @var{tblB} is also multicolumnar.
    ##
    ## @code{@var{tblB} = splitvars (@var{tblA}, @var{vars})} splits only the
    ## variables in @var{tblA} specified by @var{vars}.  If left empty, it
    ## defaults to all variables that can be split.  Single-column variables
    ## specified in @var{vars} are coppied unaltered.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @code{@var{tblB} = splitvars (@dots{}, @qcode{'NewVariableNames'},
    ## @var{NewNames})} assigns new names to the variables that are split out of
    ## @var{tblA} and copied to @var{tblB}.  @var{NewNames} can be specified as
    ## a cell array of character vectors and/or string arrays.
    ##
    ## @end deftypefn
    function tbl = splitvars (this, varargin)

      ## Check max number of input arguments
      if (nargin > 4)
        error ("table.splitvars: too many input arguments.");
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {"NewVariableNames"};
      dfValues = {[]};
      [newNames, vars] = pairedArgs (optNames, dfValues, varargin(:));

      ## Get vars to actually split
      if (isempty (vars))
        vars_to_split = [];
        for ix = 1:width (this)
          if (size (this.VariableValues{ix}, 2) > 1)  # multicolumn variable
            vars_to_split(end+1) = ix;
          elseif (istable (this.VariableValues{ix}))  # nested table
            vars_to_split(end+1) = ix;
          endif
        endfor
        [ixVars, oldNames] = resolveVarRef (this, vars_to_split);
      else
        [ixVars, oldNames] = resolveVarRef (this, vars{1});
        [ixVars, ixSorted] = sort (ixVars);
        oldNames = oldNames(ixSorted);
        ## Ignore referenced variables that cannot be split
        for ix = numel (ixVars):-1:1
          if (size (this.VariableValues{ixVars(ix)}, 2) == 1)
            ixVars(ix) = [];
            oldNames(ix) = [];
          endif
        endfor
      endif

      ## Return input table if there's nothing to split
      if (isempty (ixVars))
        tbl = this;
        return;
      endif

      ## Create a remapping vector along with the corresponding variable names
      ixCols = [];
      ix_remap = [];
      ix_names = {};
      for ix = 1:width (this)
        if (ismember (ix, ixVars))
          tmp = this.VariableValues{ix};
          col = size (tmp, 2);
          ix_remap = [ix_remap, repmat(ix, 1, col)];
          if (istable (tmp))
            ix_names = [ix_names, tmp.VariableNames];
          else
            fcn = @(x) sprintf ("%s_%d", this.VariableNames{ix}, x);
            newnames = arrayfun (fcn, 1:col, "UniformOutput", false);
            ix_names = [ix_names, newnames];
          endif
          ixCols(end+1) = col;
        else
          ix_remap(end+1) = ix;
          ix_names{end+1} = this.VariableNames{ix};
        endif
      endfor

      ## If there are duplicate variable names, this means that there are
      ## nested tables with identical variable names. Switch to optional
      ## 'nestedTableName_varName' naming convention applied only on tables
      ## with duplicated variable names.
      if (numel (__unique__ (ix_names)) != numel (ix_names))
        dup_N = arrayfun (@(k) sum (arrayfun (@(j) isequal (ix_names{k}, ...
                          ix_names{j}), 1:numel(ix_names))), 1:numel(ix_names));
        dup_names = ix_names (dup_N > 1);
        ixCols = 0;
        ix_remap = [];
        ix_names = {};
        for ix = 1:width (this)
          if (ismember (ix, ixVars))
            tmp = this.VariableValues{ix};
            col = size (tmp, 2);
            ix_remap = [ix_remap, repmat(ix, 1, col)];
            if (istable (tmp))
              if (any (ismember (dup_names, tmp.VariableNames)))
                fcn = @(x) sprintf ("%s_%s", this.VariableNames{ix}, x);
                newnames = cellfun (fcn, tmp.VariableNames, ...
                                    "UniformOutput", false);
                ix_names = [ix_names, newnames];
              else
                ix_names = [ix_names, tmp.VariableNames];
              endif
            else
              fcn = @(x) sprintf ("%s_%d", this.VariableNames{ix}, x);
              newnames = arrayfun (fcn, 1:col, "UniformOutput", false);
              ix_names = [ix_names, newnames];
            endif
            ixCols(end+1) = col;
          else
            ix_remap(end+1) = ix;
            ix_names{end+1} = this.VariableNames{ix};
          endif
        endfor
      endif

      ## Create the new table by duplicating splitable variables
      ## and set new variable names
      tbl = subsetvars (this, ix_remap);
      tbl.VariableNames = ix_names;

      ## Split the multicolumn data into separate variables
      idx = 1;  # variable index
      idc = 1;  # new name index
      for ix = 1:width (this)
        if (ismember (ix, ixVars))
          tmp = this.VariableValues{ix};
          col = size (tmp, 2);
          ## Check for user defined new variable names
          if (! isempty (newNames))
            if (! iscellstr (newNames) && iscell (newNames))
              if (iscellstr (newNames{idc}))
                varNames = newNames{idc};
              elseif (isa (newNames{idc}, "string"))
                varNames = cellstr (newNames{idc});
              else
                error ("table.splitvars: invalid input for 'NewVariableNames'.");
              endif
              idc += 1;
            elseif (iscellstr (newNames) && idc == 1)
              varNames = newNames;
            elseif
              error ("table.splitvars: invalid input for 'NewVariableNames'.");
            endif
            if (numel (varNames) != col)
              error ("table.splitvars: wrong number of 'NewVariableNames'.");
            endif
            change_newNames = true;
          else
            change_newNames = false;
          endif
          ## Change variable data here
          if (istable (tmp))
            for i = 1:col
              ## Copy new variable name if given
              if (change_newNames)
                tbl.VariableNames{idx} = varNames{i};
              endif
              ## Copy data from each separate column
              tbl.VariableValues{idx} = tmp.VariableValues{i};
              ## Copy variable properties from nested table
              tbl.VariableTypes{idx} = tmp.VariableTypes{i};
              if (! isempty (tmp.VariableDescriptions{i}))
                tbl.VariableDescriptions{idx} = tmp.VariableDescriptions{i};
              endif
              if (! isempty (tmp.VariableUnits{i}))
                tbl.VariableUnits{idx} = tmp.VariableUnits{i};
              endif
              ## FIX ME! Handle custom properties here (more tricky)
              idx += 1;
            endfor
          else
            for i = 1:col
              ## Copy new variable name if given
              if (change_newNames)
                tbl.VariableNames{idx} = varNames{i};
              endif
              ## Copy data from each separate column
              tbl.VariableValues{idx} = tmp(:,i);
              tbl.VariableTypes{idx} = class (tmp(:,1));
              idx += 1;
            endfor
          endif
        else
          idx += 1;
        endif
      endfor

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} mergevars (@var{tblA}, @var{vars})
    ## @deftypefnx {table} {@var{tblB} =} mergevars (@var{tblA}, @var{vars}, @var{Name}, @var{Value})
    ##
    ## Merge table variables into a single multicolumn variable.
    ##
    ## @code{@var{tblB} = mergevars (@var{tblA}, @var{vars})} combines the table
    ## variables in @var{tblA} specified by @var{vars} to create a new
    ## multicolumn variable in @var{tblB}.  All other variables in @var{tblA}
    ## are copied to @var{tblB} unaltered.  By default, the name of the merged
    ## variable in @var{tblB} takes the form @math{VarN}, where @math{N} is the
    ## position of the first variable in @var{tblA} among those to be merged,
    ## which is also the location of the merged variable in @var{tblB}.
    ##
    ## Note that merging variables with a @qcode{'string'} data type variable
    ## will result to a multicolumn variable of @qcode{'string'} data type, by
    ## initially converting all other to-be-merged variables into
    ## @qcode{'string'} data type.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @code{@var{tblB} = mergevars (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for merging table variables with the
    ## following Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{"NewVariableName"} specifies the name of the merged variable
    ## in @var{tblB}, which must be unique.  @qcode{"NewVariableName"} must be
    ## either a cellstr or string scalar or a character vector.
    ## @item @qcode{"MergeAsTable"} specifies whether the selected variables
    ## should be merged into a multicolumn variable (default) or into a table
    ## nested into a variable, which is usefull for variables that cannot be
    ## concatenated due to incompatible variable types.  @qcode{"MergeAsTable"}
    ## must be either a boolean scalar or a numeric scalar value of @qcode{1}
    ## (@qcode{true}) or @qcode{0} (@qcode{false}).
    ## @end itemize
    ##
    ## @end deftypefn
    function tbl = mergevars (this, vars, varargin)

      ## Check input argument
      if (nargin < 2 || isempty (vars))
        error ("table.mergevars: too few input arguments.");
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {"NewVariableName", "MergeAsTable"};
      dfValues = {[], false};
      [newVarName, mergeAsTable] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check user input for 'MergeAsTable'
      if (! isscalar (mergeAsTable))
        error ("table.mergevars: invalid input for 'MergeAsTable'.");
      endif
      if (! (isbool (mergeAsTable) || ismember (mergeAsTable, [0, 1])))
        error ("table.mergevars: invalid input for 'MergeAsTable'.");
      endif

      ## Resolve variables to be removed
      [ixVars, varNames] = resolveVarRef (this, vars);
      if (isscalar (ixVars))
        tbl = this;
        return;
      endif

      ## Get name and location for new variable
      [ixVars, ixSorted] = sort (ixVars);
      varNames = varNames(ixSorted);
      location = ixVars(1);
      if (isempty (newVarName))
        newVarName = cellstr (sprintf ("Var%d", location));
      else
        ## Check user input for 'NewVariableName'
        if (isa (newVarName, "string") && isscalar (newVarName))
          newVarName = cellstr (newVarName);
        elseif (ischar (newVarName) && isvector (newVarName))
          newVarName = cellstr (newVarName);
        elseif (! (iscellstr (newVarName) && isscalar (newVarName)))
          error ("table.mergevars: invalid input for 'NewVariableName'.");
        endif
      endif

      ## Gather remaining variables to be copied unaltered
      ixRem = 1:width (this);
      ixRem(ixVars) = [];
      tbl = subsetvars (this, ixRem);

      ## Check that new variable name does not conflict any existing variable
      if (ismember (newVarName, tbl.VariableNames))
        error ("table.mergevars: assigned 'NewVariableName' already exists.");
      endif

      ## Merge as a table (easy, custom properties are handled by 'subsetvars')
      if (mergeAsTable)
        newVarTable = subsetvars (this, ixVars);
        tbl = addvars (tbl, newVarTable, 'Before', location, ...
                       'NewVariableNames', newVarName);
        return;
      endif

      ## Merge into multicolumn variable.  (keep the custom properties of the
      ## first variable that is to be merged)
      if (! mergeAsTable)
        ## Use the first to-be-merged variable for copying custom properties
        ixRem = 1:width (this);
        ixRem(ixVars(2:end)) = [];
        tbl = subsetvars (this, ixRem);
        ## Add a try...catch block instead of heuristics to check how
        ## selected variables can be merged
        try
          newVarValue = cat (2, this.VariableValues{ixVars});
        catch
          error (["table.mergevars: selected variables cannot", ...
                  " be merged into a multicolumn variable due", ...
                  " to incompatible variable types."]);
        end_try_catch
        tbl.VariableTypes{location} = class (newVarValue);
        tbl.VariableValues{location} = newVarValue;
        tbl.VariableNames(location) = newVarName;
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{tblB} =} convertvars (@var{tblA}, @var{vars}, @var{dataType})
    ##
    ## Convert table variables to specified data type.
    ##
    ## @code{@var{tblB} = mergevars (@var{tblA}, @var{vars})} converts the
    ## variables in @var{tblA} specified by @var{vars} to the specified data
    ## type.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @var{dataType} specifies the data type to convert those variables to.  It
    ## can either be a character vector defining the name of the data type to
    ## convert to or a function handle, which will perform the conversion.
    ## a char holding the name of the data type, or a function handle which will
    ## perform the conversion.  When specifying a name for data type conversion,
    ## it can either be a one-argument constructor for the specified data type,
    ## which must accept the selected variables' current data types as input, or
    ## an available method, which can be applied on selected variables' current
    ## data types.  When specifying a function handle for applying a conversion
    ## on selected variables, this function handle must accept a single input
    ## argument and return in its output the same rows as the input argument.
    ##
    ## Either way, each resulting variable must have the same number of rows as
    ## the respective variable selected for conversion.  However, depending on
    ## the chosen type of conversion, the columns of the converted variable(s)
    ## might differ.  It is up to the user to ensure that the appropriate type
    ## of conversion is performed.  @code{convertvars} only checks the custom
    ## function handles for returning the correct number of rows, which must
    ## equal the number of rows of the input table, @var{tblA}.
    ##
    ## @end deftypefn
    function tbl = convertvars (this, vars, dataType)

      ## Check input arguments
      if (nargin < 3 || isempty (vars) || isempty (dataType))
        error ("table.mergevars: too few input arguments.");
      endif

      if (ischar (dataType))
        if (! isvector (dataType))
          error ("table.convertvars: DATATYPE must be a character vector.");
        endif
      elseif (! isa (dataType, "function_handle"))
        error (["table.convertvars: DATATYPE must be either a character", ...
                " vector or a function handle; got a '%s'."], class (dataType));
      endif

      ## Get variables to convert (input validation is done by 'resolveVarRef')
      [ixVars, varNames] = resolveVarRef (this, vars);
      tbl = this;

      ## Apply conversion
      for i = 1:numel (ixVars)
        try
          newVarValue = feval (dataType, this.VariableValues{ixVars(i)});
        catch
          error (["table.convertvars: specified DATATYPE conversion cannot", ...
                  " be applied on selected variable '%s'."], varNames{i});
        end_try_catch
        if (size (newVarValue, 1) != height (this))
          error (["table.convertvars: specified DATATYPE conversion", ...
                  " on '%s' does not return the appropriate amount", ...
                  " of rows."], varNames{i});
        endif

        ## Write output
        tbl.VariableTypes{ixVars(i)} = class (newVarValue);
        tbl.VariableValues{ixVars(i)} = newVarValue;
      endfor

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} rows2vars (@var{tblA})
    ## @deftypefnx {table} {@var{tblB} =} rows2vars (@var{tblA}, @var{Name}, @var{Value})
    ##
    ## Reorient table by swapping rows into variables.
    ##
    ## @code{@var{tblB} = rows2vars (@var{tblA})} reorients the input table
    ## @var{tblA} so that its rows become variables in the output table
    ## @var{tblB} and the variables are swapped into rows and their names
    ## are stored into a new variable at the beginning of the output table.  If
    ## the contents of @var{tblA} can be concatenated, then the corresponding
    ## variables of @var{tblB} arrays, otherwise they are cell arrays.  If the
    ## input table @var{tblA} contains @qcode{RowNames}, then those names become
    ## the variable names of the output table @var{tblB}, otherwise the variable
    ## names of @var{tblB} are generated automatically.  @code{rows2vars} cannot
    ## handle multicolumn variables or nested tables.
    ##
    ## variables in @var{tblA} specified by @var{vars} to create a new
    ## multicolumn variable in @var{tblB}.  All other variables in @var{tblA}
    ## are copied to @var{tblB} unaltered.  By default, the name of the merged
    ## variable in @var{tblB} takes the form @math{VarN}, where @math{N} is the
    ## position of the first variable in @var{tblA} among those to be merged,
    ## which is also the location of the merged variable in @var{tblB}.
    ##
    ## @code{@var{tblB} = rows2vars (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for merging table variables with the
    ## following Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{"DataVariables"} specifies the variables from input table
    ## @var{tblA} which will be reoriented.  @qcode{"DataVariables"} can be any
    ## of the following types: a character vector specifying a single variable;
    ## a cell array of character vectors or a string array specifying a single
    ## or multiple variables; a numeric array of integer values specifying a
    ## single or multiple variables; a logical vector of the same length as the
    ## width of the input table specifying a single or multiple variables.
    ## @item @qcode{"VariableNamesSource"} specifies a single variable that
    ## contains the variable names for the output table.  The values of the
    ## selected variable must have a data type which can be converted to strings
    ## and the number of unique names in the selected variable must match the
    ## number of rows of the input table.  @qcode{"VariableNamesSource"} accepts
    ## the same data types supported by @qcode{"DataVariables"} as long as they
    ## index a single variable, which, however, must not be specified by the
    ## @qcode{"DataVariables"} ame-Value paired argument.
    ## @item @qcode{"VariableNamingRule"} must be a character vector specifying
    ## the rule for naming variables in the output table @var{tblB}.  When set
    ## to @qcode{'modify'} (default), the variable names are modified so that
    ## they are valid variable identifiers.  When set to @qcode{'preserve'}, the
    ## original names are preserved.
    ## @end itemize
    ##
    ## @end deftypefn
    function tbl = rows2vars (this, varargin)

      ## Parse optional Name-Value paired arguments
      optNames = {"DataVariables", "VariableNamesSource", "VariableNamingRule"};
      dfValues = {[], [], 'modify'};
      [varRef, source, rule] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check user input for 'DataVariables'
      if (! isempty (varRef))
        ixVar = resolveVarRef (this, varRef, "lenient");
        if (any (ixVar == 0))
          varRef = cellstr (varRef);
          error (["table.rows2vars: 'DataVariables' index a", ...
                  " non-existing variable: '%s'."], varRef{find (ixVar == 0)});
        endif
        tbl = subsetvars (this, ixVar);
      else
        tbl = this;
      endif

      ## Check user input for 'VariableNamesSource'
      if (! isempty (source))
        srcVar = resolveVarRef (this, source, "lenient");
        if (! isscalar (srcVar))
          error (["table.rows2vars: 'VariableNamesSource' must", ...
                  " index a single variable."]);
        elseif (any (srcVar == 0))
          source = cellstr (source);
          error (["table.rows2vars: 'VariableNamesSource' indexes a", ...
                  " non-existing variable: '%s'."], source{find (srcVar == 0)});
        endif
        ## The number of names taken from the specified table variable
        ## must match the number of rows of the input table.
        newVarNames = this.VariableValues{srcVar};
        if (! iscellstr (newVarNames))
          newVarNames = cellstr (string (newVarNames));
        endif
        if (numel (__unique__ (newVarNames)) != height (this))
          error (["table.rows2vars: the number of names taken from the", ...
                  " variable specified in 'VariableNamesSource' does", ...
                  " not match the number of rows in input table."]);
        endif
        ## Check that 'VariableNamesSource' does not specify a variable
        ## that is specified by 'DataVariables', otherwise remove it from
        ## returning table
        if (! isempty (varRef))
          if (ismember (srcVar, ixVar))
            error (["table.rows2vars: 'VariableNamesSource' cannot specify", ...
                    " a variable that is also specified by 'DataVariables'."]);
          endif
        else
          tbl = removevars (tbl, srcVar);
        endif
        ## If input table has RowNames remove them
        if (! isempty (tbl.RowNames))
          tbl.RowNames = {};
        endif
      elseif (! isempty (tbl.RowNames))
        newVarNames = tbl.RowNames;
        tbl.RowNames = {};
      else
        rows = height (tbl);
        newVarNames = cell (1, rows);
        for i = 1:rows
          newVarNames{i} = sprintf ("Var%d", i);
        endfor
      endif

      ## Handle variable naming rule
      if (strcmpi (rule, "modify"))
        for i = 1:numel (newVarNames)
          if (! isvarname (newVarNames{i}))
            newVarNames{i} = matlab.lang.makeValidName (newVarNames{i});
          endif
        endfor
      elseif (! strcmpi (rule, "preserve"))
        error ("table.rows2vars: invalid input for 'VariableNamingRule'.");
      endif

      ## Check for multicolumn variables and nested tables
      for i = 1:width (tbl)
        if (isa (tbl.VariableValues{i}, "table"))
          error ("table.rows2vars: input table must not contain nested tables.");
        elseif (size (tbl.VariableValues{i}, 2) > 1)
          error (["table.rows2vars: input table must not contain", ...
                  " multicolumn variables."]);
        endif
      endfor

      ## Check column types to decide whether to return arrays or cell arrays
      col_types = cellfun (@(x) class(x), tbl.VariableValues, ...
                           "UniformOutput", false);
      if (isscalar (__unique__ (col_types)))
        matrix = cat (2, tbl.VariableValues{:})';
        new_var_values = num2cell (matrix, 1);
        out = table (new_var_values{:}, "VariableNames", newVarNames);
      else
        cols_as_cells = cell (1, width (tbl));
        for i = 1:width (tbl)
          if (iscellstr (tbl.VariableValues{i}))
            cols_as_cells{i} = tbl.VariableValues{i};
          elseif (iscell (tbl.VariableValues{i}))
            cols_as_cells{i} = tbl.VariableValues{i};
          else
            cols_as_cells{i} = num2cell (tbl.VariableValues{i});
          endif
        endfor
        matrix = cat (2, cols_as_cells{:})';
        out = table ();
        for i = 1:height (tbl)
          tmp = table (matrix(:,i), "VariableNames", newVarNames(i));
          out = [out tmp];
        endfor
      endif

      ## Merge original variable names into the table
      OriginalVariableNames = tbl.VariableNames(:);
      OriginalVariableNames = table (OriginalVariableNames);
      tbl = [OriginalVariableNames, out];

      ## Fix lengths of VariableDescriptions and VariableUnits
      tbl.VariableDescriptions = repmat ({""}, 1, size (tbl, 2));
      tbl.VariableUnits = repmat ({""}, 1, size (tbl, 2));

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, "UniformOutput", false);
      tbl.VariableTypes = new_types;

      ## Remove any custom variable properties
      if (! isempty (tbl.CustomProperties))
        cp_names = fieldnames (this.CustomProperties);
        cp_types = this.CustomPropTypes;
        idx = find (strcmpi (cp_types, "variable"));
        ## Remove custom variable properties only
        if (! isempty (idx))
          for i = idx
            tbl = rmprop (tbl, cp_names{i});
          endfor
        endif
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} stack (@var{tblA}, @var{vars})
    ## @deftypefnx {table} {@var{tblB} =} stack (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tblB}, @var{idxA}] =} stack (@dots{})
    ##
    ## Stack multiple table variables into a single table variable.
    ##
    ## @code{@var{tblB} = stack (@var{tblA}, @var{vars})} stacks the values from
    ## the variables @var{vars} in input @var{tblA} into a single variable in
    ## output table @var{tblB}.  By default, the stacked variable in @var{tblB}
    ## is named by joining the names of the variables in @var{tblA} as defined
    ## by @var{vars}.  Additionally, a new categorical variable is included in
    ## @var{tblB} that indicates which variable in @var{tblA} the stacked data
    ## in each row of @var{tblB} comes from.  By default, this categorical
    ## variable is named by appending @qcode{"_Indicator"} to the name of the
    ## stacked variable.  Variables in @var{tblA} that are not defined in
    ## @var{vars} for stacking are replicated in @var{tblB}.  If @var{tblA}
    ## contains @qcode{RowNames}, these are not stacked.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @code{@var{tblB} = stack (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for merging table variables with the
    ## following Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{"ConstantVariables"} specifies the variables other than
    ## @var{vars} to include in the output table.  By default, all remaining
    ## variables not specified by @var{vars} are included in the output table.
    ## Specifying @qcode{"ConstantVariables"} allows you to select specific
    ## variables to replicate in @var{tblB}.  Row names in @var{tblA} are always
    ## replicated in @var{tblB}.  You can specify @qcode{"ConstantVariables"} in
    ## the same manner as with @var{vars}.
    ## @item @qcode{"NewDataVariableName"} specifies the name for the new data
    ## variable in the output table @var{tblB}.  It can be a character vector,
    ## a string scalar, or a cellstring scalar.
    ## @item @qcode{"IndexVariableName"} specifies the name for the new
    ## indicator variable in the output table @var{tblB}.  It can be a character
    ## vector, a string scalar, or a cellstring scalar.
    ## @end itemize
    ##
    ## @code{[@var{tblB}, @var{idxA}] = stack (@dots{})} also returns an index
    ## vector, @var{idxA}, indicating the correspondence between the rows in
    ## @var{tblB} and the rows in @var{tblA}.
    ##
    ## @end deftypefn
    function [tbl, idxA] = stack (this, vars, varargin)

      ## Check input argument
      if (nargin < 2 || isempty (vars))
        error ("table.stack: too few input arguments.");
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {"ConstantVariables", "NewDataVariableName", ...
                  "IndexVariableName"};
      dfValues = {[], [], []};
      [constVars, newVarName, idxVarName] = pairedArgs (optNames, dfValues, ...
                                                        varargin(:));

      ## Get variables to stack
      [ixVars, varNames] = resolveVarRef (this, vars, "lenient");
      if (any (ixVars == 0))
        vars = cellstr (vars);
        error ("table.stack: VARS index a non-existing variable: '%s'.", ...
               vars{find (ixVars == 0)});
      endif

      ## Get constant variables to include
      if (isempty (constVars))
        cIxVars = setdiff (1:width (this), ixVars);
      else
        cIxVars = resolveVarRef (this, constVars, "lenient");
        if (any (cIxVars == 0))
          constVars = cellstr (constVars);
          error (["table.stack: 'ConstantVariables' index a non-existing", ...
                  "  variable: '%s'."], constVars{find (cIxVars == 0)});
        endif
        if (any (ismember (cIxVars, ixVars)))
          error (["table.stack: 'ConstantVariables' cannot contain any", ...
                  " variables to be stacked as specified by VARS."]);
        endif
      endif

      ## Get new data and index variable names
      if (isempty (newVarName))
        newVarName = strjoin (varNames, '_');
      else
        if (! (iscellstr (newVarName) && isscalar (newVarName)) &&
            ! (isa (newVarName, "string") && isscalar (newVarName)) &&
            ! (ischar (newVarName) && isvector (newVarName)))
          error (["table.stack: 'NewDataVariableName' must be either a", ...
                  " character vector, or a cellstring or string scalar."]);
        endif
      endif
      if (isempty (idxVarName))
        idxVarName = strcat (newVarName, '_Indicator');
      else
        if (! (iscellstr (idxVarName) && isscalar (idxVarName)) &&
            ! (isa (idxVarName, "string") && isscalar (idxVarName)) &&
            ! (ischar (idxVarName) && isvector (idxVarName)))
          error (["table.stack: 'IndexVariableName' must be either a", ...
                  " character vector, or a cellstring or string scalar."]);
        endif
      endif

      ## Handle constant variables first (and RowNames if present)
      constTable = subsetvars (this, cIxVars);
      if (! isempty (this.RowNames))
        constTable.RowNames = this.RowNames;
      endif
      constTable = repelem (constTable, numel (ixVars), 1);

      ## Handle stacked variables
      idVarValues = categorical (varNames)';
      idVarValues = repmat (idVarValues, height (this), 1);
      ndVarValues = this.VariableValues(ixVars);
      ndVarValues = vec (cat (2, ndVarValues{:})');
      stackedTable = table (idVarValues, ndVarValues, 'VariableNames', ...
                                                      {idxVarName, newVarName});

      ## Merge tables
      tbl = [constTable, stackedTable];

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, "UniformOutput", false);
      tbl.VariableTypes = new_types;

      ## Return index vector (if requested)
      if (nargout > 1)
        nRow = height (this);
        nVar = numel (ixVars);
        idxA = repelem ([1:nRow]', nVar, 1);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} unstack (@var{tblA}, @var{vars}, @var{ivar})
    ## @deftypefnx {table} {@var{tblB} =} unstack (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tblB}, @var{idxA}] =} unstack (@dots{})
    ##
    ## Unstack a single table variable into multiple table variables.
    ##
    ## @code{@var{tblB} = stack (@var{tblA}, @var{vars}, @var{ivar})} unstacks
    ## the values from the variables @var{vars} in input @var{tblA} into
    ## multiple variables in output table @var{tblB}.  By default, the stacked
    ## variable in @var{tblB} is named by joining the names of the variables in
    ## @var{tblA} as defined by @var{vars}.  Additionally, a new categorical
    ## variable is included in @var{tblB} that indicates which variable in
    ## @var{tblA} the stacked data in each row of @var{tblB} comes from.  By
    ## default, this categorical variable is named by appending
    ## @qcode{"_Indicator"} to the name of the stacked variable.  Variables in
    ## @var{tblA} that are not defined in @var{vars} for stacking are replicated
    ## in @var{tblB}.  If @var{tblA} contains @qcode{RowNames}, these are not
    ## stacked.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## renamed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be renamed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @code{@var{tblB} = stack (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for merging table variables with the
    ## following Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{"ConstantVariables"} specifies the variables other than
    ## @var{vars} to include in the output table.  By default, all remaining
    ## variables not specified by @var{vars} are included in the output table.
    ## Specifying @qcode{"ConstantVariables"} allows you to select specific
    ## variables to replicate in @var{tblB}.  Row names in @var{tblA} are always
    ## replicated in @var{tblB}.  You can specify @qcode{"ConstantVariables"} in
    ## the same manner as with @var{vars}.
    ## @item @qcode{"NewDataVariableName"} specifies the name for the new data
    ## variable in the output table @var{tblB}.  It can be a character vector,
    ## a string scalar, or a cellstring scalar.
    ## @item @qcode{"IndexVariableName"} specifies the name for the new
    ## indicator variable in the output table @var{tblB}.  It can be a character
    ## vector, a string scalar, or a cellstring scalar.
    ## @end itemize
    ##
    ## @code{[@var{tblB}, @var{idxA}] = stack (@dots{})} also returns an index
    ## vector, @var{idxA}, indicating the correspondence between the rows in
    ## @var{tblB} and the rows in @var{tblA}.
    ##
    ## @end deftypefn
    function [tbl, idxA] = unstack (this, vars, ivar, varargin)

      ## Check input argument
      if (nargin < 3 || isempty (vars))
        error ("table.unstack: too few input arguments.");
      endif

      ## Define allowed vartypes (cellstr + numeric are checked in place)
      allowed = {"logical", "string", "categorical"};

      ## Parse optional Name-Value paired arguments
      optNames = {"GroupingVariables", "ConstantVariables", ...
                  "NewDataVariableNames", "AggregationFunction", ...
                  "VariableNamingRule"};
      dfValues = {[], [], [], [], "modify"};
      [groupVars, constVars, newVarNames, aggrFcn, rule] = pairedArgs ...
                                             (optNames, dfValues, varargin(:));

      ## Get variables to unstack
      [ixVars, ~] = resolveVarRef (this, vars, "lenient");
      if (any (ixVars == 0))
        vars = cellstr (vars);
        error ("table.unstack: VARS index a non-existing variable: '%s'.", ...
               vars{find (ixVars == 0)});
      endif
      ## Check that variables to unstack do not contain nested tables
      for i = ixVars
        if (isa (this.VariableValues{i}, "table"))
          error ("table.unstack: VARS must not index nested tables.");
        endif
      endfor
      ## Move variables to unstack into a new table
      VarsTable = subsetvars (this, ixVars);

      ## Get indicator variable
      [ixIvar, ~] = resolveVarRef (this, ivar, "lenient");
      if (! isscalar (ixIvar))
        error ("table.unstack: IVAR must index a single variable.");
      elseif (ixIvar == 0)
        ivar = cellstr (ivar);
        error ("table.unstack: IVAR indexes a non-existing variable: '%s'.", ...
               ivar{find (ixIvar == 0)});
      endif
      ## Check indicator variable is not a multicolumn variable
      ## or member of the variables to be unstacked
      IvarValues = this.VariableValues{ixIvar};
      if (! isvector (IvarValues))
        error ("table.unstack: IVAR must index a single column variable.");
      endif
      if (ismember (ixIvar, ixVars))
        error (["table.unstack: IVAR cannot be any of the variables to", ...
                " be unstacked as specified by VARS."]);
      endif
      ## Check indicator variable is of a valid type
      if (! (iscellstr (IvarValues) || isnumeric (IvarValues)))
        if (! ismember (class (IvarValues), allowed))
          error (["table.unstack: IVAR indexes a variable of invalid", ...
                  " type: '%s'."], class (IvarValues));
        endif
        IvarValues = cellstr (string (IvarValues));
      endif

      ## Get default names for new unstacked variables
      IvarNames = __unique__ (IvarValues);
      ## Force both names and values to cellstr
      if (! iscellstr (IvarNames))
        IvarNames = cellstr (string (IvarNames));
        IvarValues = cellstr (string (IvarValues));
      endif

      ## Get constant variables
      if (! isempty (constVars))
        cIxVars = resolveVarRef (this, constVars, "lenient");
        if (any (cIxVars == 0))
          constVars = cellstr (constVars);
          error (["table.unstack: 'ConstantVariables' index a non-existing", ...
                  "  variable: '%s'."], constVars{find (cIxVars == 0)});
        endif
        if (any (ismember (cIxVars, ixVars)))
          error (["table.unstack: 'ConstantVariables' cannot contain any", ...
                  " variables to be unstacked as specified by VARS."]);
        endif
        if (any (ismember (cIxVars, ixIvar)))
          error (["table.unstack: 'ConstantVariables' cannot contain the", ...
                  " indicator variable as specified by IVAR."]);
        endif
      else
        cIxVars = [];
      endif

      ## Get grouping variables
      if (isempty (groupVars))
        gIxVars = setdiff (1:width (this), [ixVars, ixIvar, cIxVars]);
      else
        gIxVars = resolveVarRef (this, groupVars, "lenient");
        if (any (gIxVars == 0))
          groupVars = cellstr (groupVars);
          error (["table.unstack: 'GroupingVariables' index a non-existing", ...
                  " variable: '%s'."], groupVars{find (gIxVars == 0)});
        endif
        if (any (ismember (gIxVars, ixVars)))
          error (["table.unstack: 'GroupingVariables' cannot contain any", ...
                  " variables to be unstacked as specified by VARS."]);
        endif
      endif
      ## Exclude variables of invalid type as grouping variables (emit warning)
      for i = numel (gIxVars):-1:1
        GvarValues = this.VariableValues{gIxVars(i)};
        if (! (iscellstr (GvarValues) || isnumeric (GvarValues)))
          if (! ismember (class (GvarValues), allowed))
            invalid = this.VariableNames{gIxVars(i)};
            gIxVars(i) = [];
            warning (["table.unstack: 'GroupingVariables' index a variable", ...
                      " of invalid type: '%s', which is ingored."], invalid);
          endif
        endif
      endfor

      ## Move grouping variables into a new table
      removeVar = setdiff (1:width (this), gIxVars);
      GvarTable = removevars (this, removeVar);

      ## Move constant variables int a new table
      if (! isempty (cIxVars))
        if (any (ismember (cIxVars, gIxVars)) && ! isempty (groupVars))
          error (["table.unstack: 'ConstantVariables' cannot contain any", ...
                  " grouping variables as specified by 'GroupingVariables'."]);
        endif
        CvarTable = subsetvars (this, cIxVars);
      else
        CvarTable = table;
      endif

      ## Get new data variable names
      if (isempty (newVarNames))
        newVarNames = IvarNames';
      else
        if (! (iscellstr (newVarNames) && ! (isa (newVarNames, "string"))))
          error (["table.unstack: 'NewDataVariableNames' must be either a", ...
                  " cell array of character vectors, or a string array."]);
        endif
        if (numel (newVarNames) != numel (IvarNames))
          error (["table.unstack: 'NewDataVariableNames' do not match the", ...
                  " number of unique values in the indicator variable."]);
        endif
      endif

      ## Check user-defined aggregation function

      ## Create table containing unique instances of grouping variables,
      ## otherwise use unique instances of the indicator variable
      if (! isempty (GvarTable))
        [GvarTable, I, J] = unique (GvarTable, "stable");
        nrows = numel (I);
      else
        [~, I, J] = __unique__ (IvarValues, "stable", "rows");
        nrows = 1;
      endif

      ## Start unstacking here
      if (isscalar (ixVars))  # single variable to unstack
        ## Handle variable naming rule
        ncols = numel (newVarNames);
        if (strcmpi (rule, "modify"))
          for i = 1:ncols
            if (! isvarname (newVarNames{i}))
              newVarNames{i} = matlab.lang.makeValidName (newVarNames{i});
            endif
          endfor
        elseif (! strcmpi (rule, "preserve"))
          error ("table.unstack: invalid input for 'VariableNamingRule'.");
        endif

        ## Create table with unstacked variables
        vvals = VarsTable.VariableValues{:,:};
        if (iscellstr (vvals))
          vtype = 'cellstr';
        else
          vtype = class (vvals);
        endif
        vtype = repmat ({vtype}, 1, ncols);
        UvarTable = table ('Size', [nrows, ncols], 'VariableTypes', vtype, ...
                                                   'VariableNames', newVarNames);
        ## Copy descriptions and units to unstacked variables
        vd = this.VariableDescriptions{ixVars};
        UvarTable.VariableDescriptions = repmat ({vd}, 1, ncols);
        vu = this.VariableUnits{ixVars};
        UvarTable.VariableUnits = repmat ({vu}, 1, ncols);

        ## FIX ME: copy custom variable properties to unstacked variables

        ## Add type-specific NaN values and handle multicolumn variables
        vcols = size (vvals, 2);
        if (isnumeric (vvals))
          mcvec =  repmat (NaN, nrows, vcols);
          if (isempty (aggrFcn))  # add default aggrevation function
            aggrFcn = @sum;
          endif
        elseif (isa (vvals, "duration"))
          mcvec =  repmat (duration ([NaN, NaN, NaN]), nrows, vcols);
          if (isempty (aggrFcn))  # add default aggrevation function
            aggrFcn = @sum;
          endif
        else
          mcvec = repmat (UvarTable.VariableValues{1}, 1, vcols);
          if (isempty (aggrFcn))  # add default aggrevation function
            aggrFcn = @(x) __unique__ (x)(1);
          endif
        endif

        ## Process each unstacked variable
        for i = 1:ncols
          UvarTable.VariableValues{i} = mcvec;
          ix = strcmp (IvarNames{i}, IvarValues);
          if (nrows == 1)
            aggrVal = aggrFcn (vvals(ix, :));
            UvarTable.VariableValues{i} = aggrVal;
            CixRows = 1;
          else
            CixRows = [];
            for j = 1:nrows
              tmpIvarNames = IvarValues(J == j);
              ix = strcmp (IvarNames{i}, tmpIvarNames);
              if (any (ix))
                aggrVec = ismember (tmpIvarNames, IvarNames{i});
                aggrVal = aggrFcn (vvals(J == j, :)(aggrVec,:));
                UvarTable.VariableValues{i}(j,:) = aggrVal;
              endif
              CixRows = [CixRows, find(J == j, 1)];
            endfor
          endif
        endfor

        ## Keep corresponding rows from ConstantVariables
        if (! isempty (CvarTable))
          CvarTable = subsetrows (CvarTable, CixRows);
        endif

      else # multiple variables to unstack
        nvars = numel (ixVars);
        ncols = numel (newVarNames);
        expVarNames = cell (1, nvars * ncols);
        expVarTypes = expVarNames;

        ## Create composite variable names and get vartypes
        ij = 1;
        for i = 1:nvars
          vvals = VarsTable.VariableValues{i};
          if (iscellstr (vvals))
            vtype = 'cellstr';
          else
            vtype = class (vvals);
          endif
          for j = 1:ncols
            expVarNames{ij} = sprintf ('%s_%s', VarsTable.VariableNames{i}, ...
                                                newVarNames{j});
            expVarTypes{ij} = vtype;
            ij++;
          endfor
        endfor

        ## Handle variable naming rule
        if (strcmpi (rule, "modify"))
          for i = 1:numel (expVarNames)
            if (! isvarname (expVarNames{i}))
              expVarNames{i} = matlab.lang.makeValidName (expVarNames{i});
            endif
          endfor
        elseif (! strcmpi (rule, "preserve"))
          error ("table.unstack: invalid input for 'VariableNamingRule'.");
        endif

        ## Create table for each unstacked variable
        UvarTable = table ('Size', [nrows, ncols*nvars], ...
                           'VariableTypes', expVarTypes, ...
                           'VariableNames', expVarNames);

        ## Copy descriptions and units to unstacked variables
        VD = {};
        VU = {};
        for i = 1:nvars
          vd = this.VariableDescriptions{ixVars(i)};
          VD = [VD, repmat({vd}, 1, ncols)];
          vu = this.VariableUnits{ixVars(i)};
          VU = [VU, repmat({vu}, 1, ncols)];
        endfor
        UvarTable.VariableDescriptions = VD;
        UvarTable.VariableUnits = VU;

        ## FIX ME: copy custom variable properties to unstacked variables

        ## Process each separate variable to be unstacked
        vi = 1;
        for v = 1:nvars
          ## Get values of selected variable
          vvals = VarsTable.VariableValues{v};

          ## Add type-specific NaN values and handle multicolumn variables
          vcols = size (vvals, 2);
          if (isnumeric (vvals))
            mcvec =  repmat (NaN, nrows, vcols);
            if (isempty (aggrFcn))  # add default aggrevation function
              aggrFcn = @sum;
            endif
          elseif (isa (vvals, "duration"))
            mcvec =  repmat (duration ([NaN, NaN, NaN]), nrows, vcols);
            if (isempty (aggrFcn))  # add default aggrevation function
              aggrFcn = @sum;
            endif
          else
            mcvec = repmat (UvarTable.VariableValues{1}, 1, vcols);
            if (isempty (aggrFcn))  # add default aggrevation function
              aggrFcn = @(x) __unique__ (x)(1);
            endif
          endif

          ## Process each unstacked variable
          for i = 1:ncols
            UvarTable.VariableValues{vi} = mcvec;
            ix = strcmp (IvarNames{i}, IvarValues);
            if (nrows == 1)
              aggrVal = aggrFcn (vvals(ix, :));
              UvarTable.VariableValues{vi} = aggrVal;
              CixRows = 1;
            else
              CixRows = [];
              for j = 1:nrows
                tmpIvarNames = IvarValues(J == j);
                ix = strcmp (IvarNames{i}, tmpIvarNames);
                if (any (ix))
                  aggrVec = ismember (tmpIvarNames, IvarNames{i});
                  aggrVal = aggrFcn (vvals(J == j, :)(aggrVec,:));
                  UvarTable.VariableValues{vi}(j,:) = aggrVal;
                endif
                if (v == 1)
                  CixRows = [CixRows, find(J == j, 1)];
                endif
              endfor
            endif
            vi++;
          endfor
        endfor

        ## Keep corresponding rows from ConstantVariables
        if (! isempty (CvarTable))
          CvarTable = subsetrows (CvarTable, CixRows);
        endif
      endif

      idxA = I;
      tbl = [GvarTable, CvarTable, UvarTable];

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, "UniformOutput", false);
      tbl.VariableTypes = new_types;

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{T} =} addprop (@var{T}, @var{propertyNames}, @var{propertyTypes})
    ##
    ## Add custom properties to a table.
    ##
    ## @code{@var{T} = addprop (@var{T}, @var{propertyNames}, @var{propertyTypes})}
    ## adds properties that contain custom metadata to the table @var{T}.  The
    ## input argument @var{propertyNames} specifies the names of the custom
    ## properties to be added and @var{propertyTypes} the type of each
    ## corresponding custom property, that is whether the metadata values
    ## contained in the property apply to table @var{T} as a whole, or to the
    ## variables of @var{T}.  Both @var{propertyNames} and @var{propertyTypes}
    ## can be character vectors, cell arrays of character vectors, or stings.
    ## When defined as cell arrays of character vectors or stings, they must
    ## have the same number of elements.
    ##
    ## Valid @var{propertyTypes} are either @qcode{'table'} or
    ## @qcode{'variable'}.  When defined as @qcode{'table'}, the custom property
    ## can contain a scalar value of arbitrary type, which applies as metadata
    ## to the table as a whole.  When defined as @qcode{'variable'}, the custom
    ## property contains a vector, whose elements correspond to the number of
    ## variables in the table.
    ##
    ## After adding custom properties using @code{addprop}, metadata values can
    ## be assigned to the properties using dot syntax.
    ##
    ## @end deftypefn
    function tbl = addprop (this, Names, Types)

      ## Check input arguments
      if (nargin < 3)
        error ("table.addprop: too few input arguments.");
      elseif (! (any (isa (Names, {"string", "char"})) || iscellstr (Names)))
        error ("table.addprop: invalid input type for 'propertyNames'.");
      elseif (! (any (isa (Types, {"string", "char"})) || iscellstr (Types)))
        error ("table.addprop: invalid input type for 'propertyTypes'.");
      endif

      ## Force to cellstr
      Names = cellstr (Names);
      Types = cellstr (Types);
      if (numel (Names) != numel (Types))
        error (["table.addprop: the number of 'propertyTypes'", ...
                " must equal the number of 'propertyNames'."]);
      endif

      ## Check for duplicate property names
      if (! isempty (this.CustomProperties))
        existingNames = fieldnames (this.CustomProperties);
        idx = ismember (Names, existingNames);
        if (any (idx))
          error ("table.addprop: custom property '%s' already exists.", ...
                  Names{idx(find (idx)(1))});
        endif
        offset = numel (this.CustomPropTypes);
      else
        offset = 0;
      endif

      ## Add each custom property
      for idx = 1:numel (Names)
        ## Check for valid custom property name
        if (! isvarname (Names{idx}))
          error (["table.addprop: custom property '%s'", ...
                  " does not have a valid name."], Names{idx});
        endif
        ## Check for valid custom property type
        if (! any (strcmp (Types{idx}, {"table", "variable"})))
          error ("table.addprop: invalid value for 'propertyTypes'.");
        endif
        this.CustomProperties.(Names{idx}) = [];
        this.CustomPropTypes(idx + offset) = Types{idx};
      endfor
      tbl = this;

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{T} =} rmprop (@var{T}, @var{propertyNames})
    ##
    ## Remove custom properties from a table.
    ##
    ## @code{@var{T} = rmprop (@var{T}, @var{propertyNames})} removes properties
    ## that contain custom metadata from the table @var{T}.  The input argument
    ## @var{propertyNames} specifies the names of the custom properties to be
    ## removed and it can either be a character vector, a cell array of
    ## character vectors, or a sting array.
    ##
    ## @end deftypefn
    function tbl = rmprop (this, Names)

      ## Check input arguments
      if (nargin < 2)
        error ("table.rmprop: too few input arguments.");
      elseif (! (any (isa (Names, {"string", "char"})) || iscellstr (Names)))
        error ("table.rmprop: invalid input type for 'propertyNames'.");
      endif

      ## Force to cellstr
      Names = cellstr (Names);

      ## Check that referenced property names exist
      if (! isempty (this.CustomProperties))
        existingNames = fieldnames (this.CustomProperties);
        idx = ismember (Names, existingNames);
        idx = ! idx;
        if (any (idx))
          error ("table.rmprop: custom property '%s' does not exist.", ...
                  Names{idx(find (idx)(1))});
        endif
      else
        error ("table.rmprop: table does not contain any custom properties.");
      endif

      ## Remove each custom property
      idx = [];
      for i = 1:numel (Names)
        ## Get index number of field to be removed
        idx = [idx; find(strcmp (Names{i}, existingNames))];
        ## Remove field and its corresponding type reference
        this.CustomProperties = rmfield (this.CustomProperties, Names{i});
      endfor
      this.CustomPropTypes(idx) = [];
      tbl = this;

    endfunction

  endmethods

################################################################################
##                       ** Join and Set Operations **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'join'             'innerjoin'        'outerjoin'        'union'           ##
## 'intersect'        'ismember'         'setdiff'          'setxor'          ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} join (@var{tblL}, @var{tblR})
    ## @deftypefnx {table} {@var{tbl} =} join (@var{tblL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixR}] =} join (@dots{})
    ##
    ## Combine two tables by rows using key variables.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, ixR] = join (tblL, tblR, varargin)
      error ("table.join: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} innerjoin (@var{tblL}, @var{tblR})
    ## @deftypefnx {table} {@var{tbl} =} innerjoin (@var{tblL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixL}, @var{ixR}] =} innerjoin (@dots{})
    ##
    ## Inner join between two tables by rows using key variables.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, ixL, ixR] = innerjoin (tblL, tblR, varargin)
      error ("table.innerjoin: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} outerjoin (@var{tblL}, @var{tblR})
    ## @deftypefnx {table} {@var{tbl} =} outerjoin (@var{tblL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixL}, @var{ixR}] =} outerjoin (@dots{})
    ##
    ## Outer join between two tables by rows using key variables.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, ixL, ixR] = outerjoin (tblL, tblR, varargin)
      error ("table.outerjoin: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} union (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} union (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}, @var{ixB}] =} union (@dots{})
    ##
    ## Union of two tables by rows.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, ixA, ixB] = union (tblA, tblB, varargin)
      error ("table.union: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} intersect (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} intersect (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}, @var{ixB}] =} intersect (@dots{})
    ##
    ## Intersection of two tables by rows.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, ixA, ixB] = intersect (tblA, tblB, varargin)
      error ("table.intersect: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{TF} =} ismember (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {[@var{TF}, @var{ixB}] =} ismember (@var{tblA}, @var{tblB})
    ##
    ## Find set members between two tables by rows.
    ##
    ##
    ##
    ## @end deftypefn
    function [TF, ixB] = ismember (tblA, tblB)
      error ("table.ismember: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} setdiff (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}] =} setdiff (@var{tblA}, @var{tblB})
    ##
    ## Difference between two tables by rows.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, ixA] = setdiff (tblA, tblB)
      error ("table.setdiff: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} setxor (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} setxor (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}, @var{ixB}] =} setxor (@dots{})
    ##
    ## Exclusive OR of two tables by rows.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, ixA, ixB] = setxor (tblA, tblB, varargin)
      error ("table.setxor: not implemented yet.");
    endfunction

  endmethods

################################################################################
##                           ** Missing Values **                             ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'anymissing'       'ismissing'        'rmmissing'        'fillmissing'     ##
## 'standardizeMissing'                                                       ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{TF} =} anymissing (@var{tblA})
    ##
    ## Determine if any table element is missing.
    ##
    ## @code{@var{TF} = anymissing (@var{tblA})} returns @qcode{true} if at
    ## least one element in table var{tblA} is missing, otherwise tt returns
    ## @qcode{false}.  @var{TF} is a logical scalar value.
    ##
    ## Missing values are defined according to the data type of each variable in
    ## @var{tblA}:
    ##
    ## @itemize
    ## @item @qcode{NaN} - double, single, duration and calendarDuration
    ## @item @qcode{NaT} - datetime
    ## @item @qcode{<missing>} - string
    ## @item @qcode{<undefined>} - categorical
    ## @item @qcode{''} - cell arrays of character vectors
    ## @item @qcode{[]} - cell arrays
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = anymissing (this)
      TF = any (ismissing (this));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {Method} {@var{TF} =} ismissing (@var{tblA})
    ## @deftypefnx {Method} {@var{TF} =} ismissing (@var{tblA}, @var{indicator})
    ##
    ## Find missing values in table.
    ##
    ## Finds missing values in @var{obj}’s variables.
    ##
    ## If indicator is not supplied, uses the standard missing values for each
    ## variable’s data type. If indicator is supplied, the same indicator list is
    ## applied across all variables.
    ##
    ## All variables in this must be vectors. (This is due to the requirement
    ## that @code{size(out) == size(obj)}.)
    ##
    ## Returns a logical array the same size as @var{obj}.
    ##
    ## @end deftypefn
    function TF = ismissing (this, indicator = [], varargin)

      ## Parse optional Name-Value paired arguments
      optNames = {'OutputFormat'};
      dfValues = {'logical'};
      [outFmt] = pairedArgs (optNames, dfValues, varargin(:));

      if (! any (strcmpi (outFmt, {'logical', 'tabular'})))
        error ("table.ismissing: invalid value for 'OutputFormat'.");
      endif

      ## Process each table variable
      for i = 1:width (this)
        tmpVar = this.VariableValues{i};
        if (any (isa (tmpVar, {'categorical', 'calendarDuration'})))
          varTF = ismissing (tmpVar);
          varTF = any (varTF, 2);
          this.VariableValues{i} = varTF;
        elseif (any (isa (tmpVar, {'duration', 'datetime', 'string'})))
          varTF = ismissing (tmpVar, indicator);
          varTF = any (varTF, 2);
          this.VariableValues{i} = varTF;
        elseif (isa (tmpVar, 'table'))
          varTF = ismissing (tmpVar, indicator, outFmt, ...
                             'OutputFormat', 'logical');
          varTF = any (varTF, 2);
          this.VariableValues{i} = varTF;
        else
          varTF = __ismissing__ (tmpVar, indicator);
          varTF = any (varTF, 2);
          this.VariableValues{i} = varTF;
        endif
      endfor

      ## Return appropriate OutputFormat
      if (strcmpi (outFmt, 'logical'))
        TF = table2array (this);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} rmmissing (@var{tblA})
    ## @deftypefnx {table} {@var{tbl} =} rmmissing (@var{tblA}, @var{dim})
    ## @deftypefnx {table} {@var{tbl} =} rmmissing (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{TF}] =} rmmissing (@dots{})
    ##
    ## Remove missing table elements by rows.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, TF] = rmmissing (tblA, varargin)
      error ("table.rmmissing: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} fillmissing (@var{tblA}, @qcode{'constant'}, @var{val})
    ## @deftypefnx {table} {@var{tbl} =} fillmissing (@var{tblA}, @var{method})
    ## @deftypefnx {table} {@var{tbl} =} fillmissing (@var{tblA}, @var{movmethod}, @var{window})
    ## @deftypefnx {table} {@var{tbl} =} fillmissing (@var{tblA}, @qcode{'knn'})
    ## @deftypefnx {table} {@var{tbl} =} fillmissing (@var{tblA}, @qcode{'knn'}, @var{k})
    ## @deftypefnx {table} {@var{tbl} =} fillmissing (@var{tblA}, @var{fillfun}, @var{gapwindow})
    ## @deftypefnx {table} {@var{tbl} =} fillmissing (@dots{}, @var{dim})
    ## @deftypefnx {table} {@var{tbl} =} fillmissing (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{TF}] =} fillmissing (@dots{})
    ##
    ## Fill missing table elements.
    ##
    ##
    ##
    ## @end deftypefn
    function [tbl, TF] = fillmissing (tblA, varargin)
      error ("table.fillmissing: not implemented yet.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} standardizeMissing (@var{tblA}, @var{indicator})
    ## @deftypefnx {table} {@var{tbl} =} rmmissing (@dots{}, @var{Name}, @var{Value})
    ##
    ## Remove missing table elements by rows.
    ##
    ##
    ##
    ## @end deftypefn
    function tbl = standardizeMissing (tblA, indicator)
      error ("table.standardizeMissing: not implemented yet.");
    endfunction

  endmethods

################################################################################
##                  ** Apply Functions to Table Contents **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'pivot'            'groupcounts'      'groupfilter'      'groupsummary'    ##
## 'grouptransform'   'findgroups'       'splitapply'       'rowfun'          ##
## 'varfun'                                                                   ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function out = pivot (this)
      error ("table.pivot: not implemented yet.");
    endfunction

    function out = groupcounts (this)
      error ("table.groupcounts: not implemented yet.");
    endfunction

    function out = groupfilter (this)
      error ("table.groupfilter: not implemented yet.");
    endfunction

    function out = groupsummary (this)
      error ("table.groupsummary: not implemented yet.");
    endfunction

    function out = grouptransform (this)
      error ("table.grouptransform: not implemented yet.");
    endfunction

    function out = findgroups (this)
      error ("table.findgroups: not implemented yet.");
    endfunction

    function out = splitapply (this)
      error ("table.splitapply: not implemented yet.");
    endfunction

    function out = rowfun (this)
      error ("table.rowfun: not implemented yet.");
    endfunction

    function out = varfun (this)
      error ("table.varfun: not implemented yet.");
    endfunction

  endmethods

################################################################################
##                       **    Auxiliary Methods    **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'horzcat'          'iscolumn'         'isempty'          'ismatrix'        ##
## 'isrow'            'isscalar'         'istable'          'isvector'        ##
## 'length'           'ndims'            'numel'            'repelem'         ##
## 'repmat'           'size'             'squeeze'          'vertcat'         ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{tbl} =} horzcat (@var{tbl1}, @var{tbl2}, @dots{})
    ##
    ## Horizontal concatenation for tables.
    ##
    ## @code{@var{tbl} = horzcat (@var{tbl1}, @var{tbl2}, @dots{})} merges
    ## tables by horizontally concatenating them, provided that all input tables
    ## have collectively unique variable names and the same number of rows.
    ##
    ## Input tables that have row names must share the same unique set of row
    ## names but not necessarily in the same order.  When row names are present
    ## in multiple input tables, their position is matched to the row names of
    ## the first input table.  Input tables without row names are concatenated
    ## by position without re-indexing.  Output table's @qcode{Description} and
    ## @qcode{UserData} properties are assigned using the first non-empty value.
    ##
    ## @end deftypefn
    function tbl = horzcat (varargin)
      ## All inputs must be tables
      are_tables = cellfun (@istable, varargin);
      if (! all (are_tables))
        error ("table.horzcat: all inputs must be tables.");
      endif
      ## All tables must have unique variable names
      varNames = cellfun (@(obj) obj.VariableNames, varargin, ...
                          "UniformOutput", false);
      is_empty = cellfun (@isempty, varNames);
      varNames = [varNames{:}];
      if (numel (varNames) != numel (unique (varNames)))
        error (strcat ("table.horzcat: all input tables must", ...
                       " have unique variable names."));
      endif
      ## All tables must have the same rows (height)
      numRows = cellfun (@height, varargin);
      if (numel (unique (numRows(! is_empty))) != 1)
        error ("table.horzcat: all input tables must have the same height.");
      endif
      numRows = numRows(1);
      ## Check for RowNames
      has_RowNames = ! cellfun (@(obj) isempty (obj.RowNames), varargin);
      if (! any (has_RowNames)) # no RowNames in any table (easy)
        tbl = varargin{1};
        tbl.VariableNames = varNames;
        for i = 2:numel (varargin)
          in = varargin{i};
          tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
          tbl.VariableDescriptions = [tbl.VariableDescriptions, ...
                                      in.VariableDescriptions];
          tbl.VariableUnits = [tbl.VariableUnits, in.VariableUnits];
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
          ## FIX ME: Deal with custom properties here
        endfor
      elseif (sum (has_RowNames) == 1) # only one input table has RowNames (ok)
        tbl = varargin{1};
        tbl.VariableNames = varNames;
        for i = 2:numel (varargin)
          in = varargin{i};
          tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
          tbl.VariableDescriptions = [tbl.VariableDescriptions, ...
                                      in.VariableDescriptions];
          tbl.VariableUnits = [tbl.VariableUnits, in.VariableUnits];
          if (! isempty (in.RowNames))
            tbl.RowNames = in.RowNames;
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
          ## FIX ME: Deal with custom properties here
        endfor
      else  # multiple tables has rowNames (we are screwed)
        ## First we need to ensure that all tables with RowNames share the
        ## same unique RowNames (in any order)
        rowNames = cellfun (@(obj) obj.RowNames, varargin(has_RowNames), ...
                            "UniformOutput", false);
        sortedRowNames = cellfun (@sort, rowNames, "UniformOutput", false);
        if (! isequal (sortedRowNames{:}))
          error ("table.horzcat: input tables must have identical RowNames.");
        endif
        ## We need to figure out some indexing for every other table with
        ## RowNames so that we now how to merge them with the first table
        ## with RowNames
        tbl_withRowNames = find (has_RowNames);
        index = [1:numRows]'; # first table is reindexed to itself
        for i = 2:numel (rowNames)
          fcn = @(x) find (ismember (rowNames{1}, x));
          index(:,i) = cellfun (fcn, rowNames{i});
        endfor
        ## Start merging tables and re-index every other table with RowNames
        ## before merging
        tbl = varargin{1};
        tbl.VariableNames = varNames;
        tbl.VariableValues = {};
        tbl.VariableDescriptions = {};
        tbl.VariableUnits = {};
        tbl.RowNames = {};
        add_row_names = true; # only once
        for i = 1:numel (varargin)
          in = varargin{i};
          if (ismember (i, tbl_withRowNames)) # this table has RowNames
            ixRows = index(:,1);
            index(:,1) = [];
            in = subsetrows (in, ixRows);
            if (add_row_names)
              tbl.RowNames = in.RowNames(ixRows);
              add_row_names = false;
            endif
            tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
            tbl.VariableDescriptions = [tbl.VariableDescriptions, ...
                                        in.VariableDescriptions];
            tbl.VariableUnits = [tbl.VariableUnits, in.VariableUnits];
          else
            tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
            tbl.VariableDescriptions = [tbl.VariableDescriptions, ...
                                        in.VariableDescriptions];
            tbl.VariableUnits = [tbl.VariableUnits, in.VariableUnits];
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
          ## FIX ME: Deal with custom properties here
        endfor
      endif

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, "UniformOutput", false);
      tbl.VariableTypes = new_types;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{TF} =} iscolumn (@var{tbl})
    ##
    ## Test input table for being a column vector.
    ##
    ## @qcode{@var{TF} = iscolumn (@var{tbl})} returns @qcode{true} if the input
    ## table @var{tbl} has a single variable.  The number of columns within that
    ## variable does not matter.
    ##
    ## @end deftypefn
    function TF = iscolumn (this)
      TF = width (this) == 1;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{TF} =} isempty (@var{tbl})
    ##
    ## Test input table for being empty.
    ##
    ## For tables, @code{isempty} is true if the number of rows is 0 or the
    ## number of variables is 0.
    ##
    ## @end deftypefn
    function TF = isempty (this)
      TF = prod (size (this)) == 0;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{TF} =} ismatrix (@var{tbl})
    ##
    ## Test input table for being a matrix.
    ##
    ## For tables, @code{ismatrix} is always true, by definition.
    ##
    ## @end deftypefn
    function TF = ismatrix (this)
      TF = true;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{TF} =} isrow (@var{tbl})
    ##
    ## Test input table for being a row vector.
    ##
    ## @qcode{@var{TF} = isrow (@var{tbl})} returns @qcode{true} if the input
    ## table @var{tbl} has a single row.
    ##
    ## @end deftypefn
    function TF = isrow (this)
      TF = height (this) == 1;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {Method} {@var{TF} =} isscalar (@var{tbl})
    ##
    ## Test input table for being a scalar.
    ##
    ## @qcode{@var{TF} = isscalar (@var{tbl})} returns @qcode{true} if the input
    ## table @var{tbl} has a single row and a single variable.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = height (this) == 1 && width (this) == 1;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{tf} =} istable (@var{tbl})
    ##
    ## Return @qcode{True} if input is a table.
    ##
    ## @end deftypefn
    function TF = istable (this)
      TF = true;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{TF} =} isvector (@var{tbl})
    ##
    ## Test input table for being a vector.
    ##
    ## @qcode{@var{TF} = isvector (@var{tbl})} returns @qcode{true} if the input
    ## table @var{tbl} has a single row or a single column.
    ##
    ## @end deftypefn
    function TF = isvector (this)
      TF = isrow (this) || iscol (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{out} =} length (@var{tbl})
    ##
    ## Length along longest dimension.
    ##
    ## @end deftypefn
    function out = length (this, varargin)
      out = max (size (this));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{out} =} ndims (@var{tbl})
    ##
    ## Number of table dimensions.
    ##
    ## For tables, @code{ndims(tbl)} is always 2.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = 2;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{out} =} numel (@var{tbl})
    ##
    ## Total number of elements in table.
    ##
    ## For compatibility reasons with Octave's OOP interface and @code{subsasgn}
    ## behavior, table's numel is defined to always return 1.  This is an
    ## incompatibility with Matlab.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = 1;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} repelem (@var{tblA}, @var{sz})
    ## @deftypefnx {table} {@var{tblB} =} repelem (@var{tblA}, @var{rows}, @var{columns})
    ##
    ## Replicate elements of a table.
    ##
    ## Replicates elements of the input table @var{tblA} in a similar fashion
    ## to how @code{repelem} applies to a matrix.  Only two dimensions are
    ## supported for tables.
    ##
    ## @end deftypefn
    function tbl = repelem (this, varargin);

      ## Check input arguments
      nargs = numel (varargin);
      if (nargs < 1)
        error ("table.repelem: too few input arguments for table input.");
      endif
      if (nargs > 2)
        error ("table.repelem: only 2 dimensions are supported for tables.");
      endif
      if (nargs == 1)
        rows = cols = varargin{1};
      else
        rows = varargin{1};
        cols = varargin{2};
      endif
      if (rows < 1 || fix (rows) != rows || ! isnumeric (rows))
        if (nargs == 1)
          error ("table.repelem: SZ must be a positive integer.");
        else
          error ("table.repelem: ROWS must be a positive integer.");
        endif
      endif
      if (cols < 1 || fix (rows) != rows || ! isnumeric (rows))
        error ("table.repelem: COLUMNS must be a positive integer.");
      endif

      tbl = this;
      ## Replicate elements per rows (apply on each variable)
      if (rows > 1)
        for i = 1:width (this)
          tbl.VariableValues{i} = repelem (this.VariableValues{i}, rows, 1);
        endfor
        ## Handle RowNames (if not empty)
        if (! isempty (this.RowNames))
          tbl.RowNames = repelem (this.RowNames, rows, 1);
          ## Fix row name repetitions
          for i = 1:rows - 1
            vec = i + 1:rows:height (tbl);
            fcn = eval (["@(x) sprintf (""%s_", sprintf("%d", i), """, x)"]);
            tbl.RowNames(vec) = cellfun (fcn, tbl.RowNames(vec), ...
                                         "UniformOutput", false);
          endfor
        endif
      endif

      ## Replicate variables accordingly
      if (cols > 1)
        ## Replicate variables
        tbl.VariableTypes = repelem (tbl.VariableTypes, 1, cols);
        tbl.VariableValues = repelem (tbl.VariableValues, 1, cols);
        tbl.VariableDescriptions = repelem (tbl.VariableDescriptions, 1, cols);
        tbl.VariableUnits = repelem (tbl.VariableUnits, 1, cols);
        ## Fix variable name repetitions
        idx = num2cell (1:cols - 1)
        newNames = {};
        for i = 1:width (this)
          newNames = [newNames, this.VariableNames{i}];
          fnc = eval (["@(x) sprintf (""", this.VariableNames{i}, "_%d"", x)"]);
          addNames = cellfun (fnc, idx, "UniformOutput", false);
          newNames = [newNames, addNames];
        endfor
        tbl.VariableNames = newNames;
        ## Handle custom variable properties
        if (! isempty (this.CustomProperties))
          cp_names = fieldnames (this.CustomProperties);
          cp_types = this.CustomPropTypes;
          idx = find (strcmpi (cp_types, "variable"));
          ## Replicate custom variable properties only
          if (! isempty (idx))
            for i = idx
              cvp_name = cp_names{i};
              tbl.CustomProperties.(cvp_name) = ...
                             repelem (tbl.CustomProperties.(cvp_name), 1, cols);
            endfor
          endif
        endif
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} repmat (@var{tblA}, @var{sz})
    ## @deftypefnx {table} {@var{tblB} =} repmat (@var{tblA}, @var{rows}, @var{columns})
    ##
    ## Repeat copies of a table.
    ##
    ## Repeats cp[ies of the input table @var{tblA} in a similar fashion
    ## to how @code{repmat} applies to a matrix.  Only two dimensions are
    ## supported for tables.
    ##
    ## @end deftypefn
    function tbl = repmat (this, varargin)

      ## Check input arguments
      nargs = numel (varargin);
      if (nargs < 1)
        error ("table.repmat: too few input arguments for table input.");
      endif
      if (nargs > 2)
        error ("table.repmat: only 2 dimensions are supported for tables.");
      endif
      if (nargs == 1)
        rows = cols = varargin{1};
      else
        rows = varargin{1};
        cols = varargin{2};
      endif
      if (rows < 1 || fix (rows) != rows || ! isnumeric (rows))
        if (nargs == 1)
          error ("table.repmat: SZ must be a positive integer.");
        else
          error ("table.repmat: ROWS must be a positive integer.");
        endif
      endif
      if (cols < 1 || fix (rows) != rows || ! isnumeric (rows))
        error ("table.repmat: COLUMNS must be a positive integer.");
      endif

      tbl = this;
      ## Replicate elements per rows (apply on each variable)
      if (rows > 1)
        for i = 1:width (this)
          tbl.VariableValues{i} = repmat (this.VariableValues{i}, rows, 1);
        endfor
        ## Handle RowNames (if not empty)
        if (! isempty (this.RowNames))
          tbl.RowNames = repmat (this.RowNames, rows, 1);
          ## Fix row name repetitions
          for i = 1:rows - 1
            rep = height (this);
            vec = i * rep + 1:rep * (i + 1);
            fcn = eval (["@(x) sprintf (""%s_", sprintf("%d", i), """, x)"]);
            tbl.RowNames(vec) = cellfun (fcn, tbl.RowNames(vec), ...
                                         "UniformOutput", false);
          endfor
        endif
      endif

      ## Replicate variables accordingly
      if (cols > 1)
        ## Replicate variables
        tbl.VariableTypes = repmat (tbl.VariableTypes, 1, cols);
        tbl.VariableValues = repmat (tbl.VariableValues, 1, cols);
        tbl.VariableDescriptions = repmat (tbl.VariableDescriptions, 1, cols);
        tbl.VariableUnits = repmat (tbl.VariableUnits, 1, cols);
        ## Fix variable name repetitions
        newNames = this.VariableNames;
        for i = 1:cols - 1
          fnc = eval (["@(x) sprintf (""%s_", sprintf("%d", i), """, x)"]);
          addNames = cellfun (fnc, this.VariableNames, "UniformOutput", false);
          newNames = [newNames, addNames];
        endfor
        tbl.VariableNames = newNames;
        ## Handle custom variable properties
        if (! isempty (this.CustomProperties))
          cp_names = fieldnames (this.CustomProperties);
          cp_types = this.CustomPropTypes;
          idx = find (strcmpi (cp_types, "variable"));
          ## Replicate custom variable properties only
          if (! isempty (idx))
            for i = idx
              cvp_name = cp_names{i};
              tbl.CustomProperties.(cvp_name) = ...
                             repmat (tbl.CustomProperties.(cvp_name), 1, cols);
            endfor
          endif
        endif
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{sz} =} size (@var{tbl})
    ## @deftypefnx {table} {[@var{rows}, @var{columns}] =} size (@var{tbl})
    ## @deftypefnx {table} {[@var{rows}, @var{columns}, @dots{}] =} size (@var{tbl})
    ##
    ## Return the size of a table.
    ##
    ## For tables, the size is [number-of-rows x number-of-variables].
    ## This is the same as @code{[height(obj), width(obj)]}.
    ##
    ## @end deftypefn
    function varargout = size (this, dim)
      h = height (this);
      w = width (this);
      varargout = cell (1, nargout);
      if (nargin == 2)
        if dim == 1
          varargout{1} = height (this);
        elseif dim == 2
          varargout{1} = width (this);
        else
          varargout{1} = 1;
        endif
      elseif nargout == 0 || nargout == 1
        varargout{1} = [height(this), width(this)];
      else
        varargout{1} = height (this);
        varargout{2} = width (this);
        [varargout{3:end}] = deal(1);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{tblB} =} squeeze (@var{tblA})
    ##
    ## Remove singleton dimensions.
    ##
    ## For tables, this is always a no-op that returns the input table
    ## unmodified, because tables always have exactly 2 dimensions.
    ##
    ## @end deftypefn
    function tbl = squeeze (this)
      tbl = this;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{tbl} =} vertcat (@var{tbl1}, @var{tbl2}, @dots{})
    ##
    ## Vertical concatenation for tables.
    ##
    ## @code{@var{tbl} = vertcat (@var{tbl1}, @var{tbl2}, @dots{})} merges
    ## tables by vertically concatenating them, provided that all input tables
    ## have the same variable names but not necessarily in the same order.  The
    ## position of the variable names are matched to those of the first input
    ## table.
    ##
    ## When input tables have row names, they must be unique across tables.  In
    ## such case, concatenated rows from input tables without row names are
    ## automatically assigned default input names using the input table's name
    ## as a prefix. Output table's @qcode{Description} and @qcode{UserData}
    ## properties are assigned using the first non-empty value.
    ##
    ## @end deftypefn
    function tbl = vertcat (varargin)
      ## All inputs must be tables
      are_tables = cellfun (@istable, varargin);
      if (! all (are_tables))
        error ("table.vertcat: all inputs must be tables.");
      endif
      ## All tables must have the same variable names
      varNames = cellfun (@(obj) obj.VariableNames, varargin, ...
                          "UniformOutput", false);
      is_empty = cellfun (@isempty, varNames);
      sortedVarNames = cellfun (@sort, varNames(! is_empty), ...
                                "UniformOutput", false);
      if (! isequal (sortedVarNames{:}))
        error ("table.vertcat: input tables must have identical variable names.");
      endif
      ## All tables must have the same columns (width)
      numCols = cellfun (@width, varargin);
      if (numel (unique (numCols)) != 1)
        error ("table.vertcat: all input tables must have the same width.");
      endif
      numCols = numCols(1);
      ## We need to figure out some indexing for the variables of every other
      ## table so we can re-index to the variables of the first table.
      index = [1:numCols]; # first table is reindexed to itself
      for i = 2:numel (varNames)
        fcn = @(x) find (ismember (varNames{1}, x));
        index(i,:) = cellfun (fcn, varNames{i});
      endfor
      ## Check for RowNames
      has_RowNames = ! cellfun (@(obj) isempty (obj.RowNames), varargin);
      ## Check that all RowNames are unique across tables
      rowNames = cellfun (@(obj) obj.RowNames, varargin(has_RowNames), ...
                          "UniformOutput", false);
      rowNames = [rowNames{:}];
      if (numel (rowNames) != numel (unique (rowNames)))
        error (strcat ("table.vertcat: all input tables must", ...
                       " have unique row names."));
      endif
      ## Start vertical concatenation
      if (! any (has_RowNames)) # no RowNames in any table (easy)
        tbl = varargin{1};
        for i = 2:numel (varargin)
          in = varargin{i};
          ixVars = index(i,:);
          in = subsetvars (in, ixVars);
          for v = 1:numCols
            tbl.VariableValues{v} = [tbl.VariableValues{v}; in.VariableValues{v}];
          endfor
          if (isempty (tbl.VariableDescriptions))
            tbl.VariableDescriptions = in.VariableDescriptions;
          endif
          if (isempty (tbl.VariableUnits))
            tbl.VariableUnits = in.VariableUnits;
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
          ## FIX ME: Deal with custom properties here
        endfor
      elseif (sum (has_RowNames) == 1) # some input tables have RowNames (oh!)
        tbl = varargin{1};
        ## If first input table does not have row names, add them here
        if (isempty (tbl.RowNames))
          prefx = inputname(1);
          nRows = height (tbl);
          fcn = @(x) {sprintf("%s_%d", prefx, nRows)};
          tbl.RowNames = arrayfun (fcn, 1:nRows)';
        endif
        for i = 2:numel (varargin)
          in = varargin{i};
          ixVars = index(i,:);
          in = subsetvars (in, ixVars);
          for v = 1:numCols
            tbl.VariableValues{v} = [tbl.VariableValues{v}; in.VariableValues{v}];
          endfor
          ## Handle row names here
          if (isempty (in.RowNames))
            prefx = inputname(i);
            nRows = height (in);
            fcn = @(x) {sprintf("%s_%d", prefx, nRows)};
            in.RowNames = arrayfun (fcn, 1:nRows)';
          endif
          tbl.RowNames = [tbl.RowNames; in.RowNames];
          ## Handle remaining stuff
          if (isempty (tbl.VariableDescriptions))
            tbl.VariableDescriptions = in.VariableDescriptions;
          endif
          if (isempty (tbl.VariableUnits))
            tbl.VariableUnits = in.VariableUnits;
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
          ## FIX ME: Deal with custom properties here
        endfor
      endif
    endfunction

  endmethods

################################################################################
##                       **    Forbidden Methods    **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'reshape'          'resize'           'shiftdims'        'vec'             ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function out = shiftdims (this, varargin)
      error ('Function shiftdims is not supported for tables');
    endfunction

    function out = reshape (this, varargin)
      error ('Function reshape is not supported for tables');
    endfunction

    function out = resize (this, varargin)
      error ('Function resize is not supported for tables');
    endfunction

    function out = vec (this, varargin)
      error ('Function vec is not supported for tables');
    endfunction

  endmethods

################################################################################
##                  ** Reference and Assignment Operations **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'subsref'          'subsasgn'                                              ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Class specific subscripted reference
    function varargout = subsref (this, s)
      chain_s = s(2:end);
      s = s(1);
      switch (s.type)
        case "()"
          if (numel (s.subs) != 2)
            error (["table.subsref: '()' indexing of table", ...
                    " requires exactly two arguments."]);
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          tbl = this;
          tbl = subsetrows (tbl, ixRow);
          tbl = subsetvars (tbl, ixVar);

        case "{}"
          if (numel (s.subs) != 2)
            error (["table.subsref: '{}' indexing of table", ...
                    " requires exactly two arguments."]);
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          tbl = this;
          tbl = subsetrows (tbl, ixRow);
          tbl = subsetvars (tbl, ixVar);
          try
            tbl = table2array (tbl);
          catch
            error (["table.subsref: table cannot be", ...
                    " concatenated into a matrix"]);
          end_try_catch

        case "."
          if (! ischar (s.subs))
            error (["table.subsref: '.' index argument must be", ...
                    " a character vector."]);
          endif
          ## Handle special cases: "Properties" and "DimensionNames"
          if (isequal (s.subs, "Properties"))
            if (nargout == 0 && isempty (chain_s))
              print_properties (this);
              return;
            else
              tbl = getProperties (this);
            endif
          elseif (isequal (s.subs, this.DimensionNames{1}))
            tbl = this.RowNames;
          elseif (isequal (s.subs, this.DimensionNames{2}))
            tbl = table2array (this);
          ## Everything else is indexing an existing variable name
          else
            tbl = getvar (this, s.subs);
          endif
      endswitch

      ## Chained references
      if (! isempty (chain_s))
        tbl = subsref (tbl, chain_s);
      endif
      varargout{1} = tbl;
    endfunction

    ## Class specific subscripted assignment
    function tbl = subsasgn (this, s, val)

      ## Chained subscripts
      chain_s = s(2:end);
      s = s(1);
      if (! isempty (chain_s) && ! isequal (s.subs, "Properties"))
        rhs_in = single_subref (this, s);
        rhs = subsasgn (rhs_in, chain_s, val);
      else
        rhs = val;
      endif

      tbl = this;
      switch (s.type)
        case "()"
          if (numel (s.subs) != 2)
            error (["table.subsasgn: '()' indexing of table", ...
                    " requires exactly two arguments."]);
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          ## Check input data matches referenced elements
          if (! isequal (size (rhs), [numel(ixRow), numel(ixVar)]))
            error ("table.subsasgn: input data mismatch indexed dimensions.");
          endif
          ## Handle different cases of input data
          if (isa (rhs, "table"))     # MATLAB compatible
            rhs = table2cell (rhs);
          endif
          if (isa (rhs, "cell"))      # MATLAB compatible
            for i = 1:numel(ixVar)
              varData = this.VariableValues{ixVar(i)};
              try
                varData(ixRow) = rhs{:,i};
              catch
                error (["table.subsasgn: input data type mismatch", ...
                        " indexed variable type."]);
              end_try_catch
              tbl.VariableValues{ixVar(i)} = varData;
            endfor
          else                        # Octave specific
            for i = 1:numel(ixVar)
              varData = this.VariableValues{ixVar(i)};
              try
                varData(ixRow) = rhs(:,i);
              catch
                error (["table.subsasgn: input data type mismatch", ...
                        " indexed variable type."]);
              end_try_catch
              tbl.VariableValues{ixVar(i)} = varData;
            endfor
          endif

        ## {} not used in Octave for assigning values
        case "{}"
          error (["table.subsasgn: '{}' invalid indexing for assigning", ...
                  " values. Use '()' instead."]);

        case "."
          if (! ischar (s.subs))
            error (["table.subsasgn: '.' index argument must be", ...
                    " a character vector."]);
          endif
          ## Grab Properties
          if (isequal (s.subs, "Properties"))
            ## no further recursion, everything is handled here
            if (isempty (chain_s))
              error ("table.subsasgn: cannot assign new properties.");
            endif
            s = chain_s(1);

            ## Handle table properties
            if (isequal (s.subs, "Description"))
              ## Check for valid input: character vector of string
              if (isa (val, "string"))
                if (numel (val) > 1)
                  error (["table.subsasgn: Table description must be a", ...
                          " character vector or a string scalar."]);
                endif
                val = cellstr (val){1};
              endif
              if (! ischar (val))
                error (["table.subsasgn: Table description must be a", ...
                        " character vector or string scalar."]);
              endif
              this.Description = val;
              tbl = this;

            elseif (isequal (s.subs, "UserData"))
              ## Any kind !!
              this.UserData = val;
              tbl = this;

            elseif (isequal (s.subs, "DimensionNames"))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (["table.subsasgn: cannot index DimensionNames", ...
                          " with more than one dimension. Use a vector", ...
                          " to index multiple DimensionNames at once."]);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ":"))
                  idx = [1:2];
                endif
                if (! all (ismember (idx, [1:2])))
                  error (["table.subsasgn: out of bound index", ...
                          " for DimensionNames."]);
                endif
                if (ischar (val) || isa (val, "string"))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (["table.subsasgn: DimensionNames must be a cell", ...
                          " array of character vectors or a string array", ...
                          " matching the number of indexed variables."]);
                endif
                this.DimensionNames(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: two-element cellstring or string array
              if (ischar (val) || isa (val, "string"))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == 2))
                error (["table.subsasgn: DimensionNames must be a two-element", ...
                        " cell array of character vectors or string array."]);
              endif
              this.DimensionNames = val;
              tbl = this;

            elseif (isequal (s.subs, "VariableNames"))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (["table.subsasgn: cannot index VariableNames with", ...
                          " more than one dimension. Use a vector to", ...
                          " index multiple VariableNames at once."]);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ":"))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error ("table.subsasgn: out of bound index for VariableNames");
                endif
                if (ischar (val) || isa (val, "string"))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (["table.subsasgn: VariableNames must be a cell", ...
                          " array of character vectors or a string array", ...
                          " matching the number of indexed variables."]);
                endif
                this.VariableNames(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, "string"))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (["table.subsasgn: VariableNames must be a cell", ...
                        " array of character vectors or a string array", ...
                        " matching the number of variables."]);
              endif
              this.VariableNames = val;
              tbl = this;

            elseif (isequal (s.subs, "VariableTypes"))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (["table.subsasgn: cannot index VariableTypes", ...
                          " with more than one dimension. Use a vector to", ...
                          " index multiple VariableTypes at once."]);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ":"))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (["table.subsasgn: out of bound index for", ...
                          " VariableTypes"]);
                endif
                if (ischar (val) || isa (val, "string"))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (["table.subsasgn: VariableTypes must be", ...
                          " a cell array of character vectors or a string", ...
                          " array matching the number of indexed variables."]);
                endif
                ## Convert selected variable(s) to new data type(s)
                tbl = convertvars (this, idx, val)
                ## Save new datatypes to VariableTypes property
                this.VariableTypes(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, "string"))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (["table.subsasgn: VariableTypes must be a", ...
                        " cell array of character vectors or a string", ...
                        " array matching the number of variables."]);
              endif
              ## Covnert variables to new data types
              tbl = convertvars (this, ":", val)
              ## Save new datatypes to VariableTypes property
              this.VariableDescriptions = val;
              tbl = this;

            elseif (isequal (s.subs, "VariableDescriptions"))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (["table.subsasgn: cannot index VariableDescriptions", ...
                          " with more than one dimension. Use a vector to", ...
                          " index multiple VariableDescriptions at once."]);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ":"))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (["table.subsasgn: out of bound index for", ...
                          " VariableDescriptions"]);
                endif
                if (ischar (val) || isa (val, "string"))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (["table.subsasgn: VariableDescriptions must be", ...
                          " a cell array of character vectors or a string", ...
                          " array matching the number of indexed variables."]);
                endif
                this.VariableDescriptions(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, "string"))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (["table.subsasgn: VariableDescriptions must be a", ...
                        " cell array of character vectors or a string", ...
                        " array matching the number of variables."]);
              endif
              this.VariableDescriptions = val;
              tbl = this;

            elseif (isequal (s.subs, "VariableUnits"))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (["table.subsasgn: cannot index VariableUnits", ...
                          " with more than one dimension. Use a vector", ...
                          " to index multiple VariableUnits at once."]);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ":"))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error ("table.subsasgn: out of bound index for VariableUnits");
                endif
                if (ischar (val) || isa (val, "string"))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (["table.subsasgn: VariableUnits must be a cell", ...
                          " array of character vectors or a string array", ...
                          " matching the number of indexed variables."]);
                endif
                this.VariableUnits(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, "string"))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (["table.subsasgn: VariableUnits must be a cell", ...
                        " array of character vectors or a string array", ...
                        " matching the number of variables."]);
              endif
              this.VariableUnits = val;
              tbl = this;

            elseif (isequal (s.subs, "RowNames"))
              ## Check for empty input to remove RowNames from table.
              if (isempty (val))
                this.RowNames = {};
                tbl = this;
                return;
              endif
              ## Check for valid input: cellstring scalar, char row vector,or
              ## string scalar matching an existing VariableName of appropriate
              ## type, or a numeric scalar referencing an existing VariableName
              ## of appropriate type.
              if ((ischar (val) && size (val, 1) == 1) ||
                 ((iscellstr (val) || isa (val, "string") || isnumeric (val)) &&
                  numel (val) == 1))
                if (ischar (val) || isa (val, "string"))
                  val = cellstr (val);
                endif
                [ixVar, ~] = resolveVarRef (this, val, "lenient");
                ##
                ## If variable name exists check for appropriate varTypes
                if (ixVar != 0)
                  selvar = this.VariableValues{ixVar};
                  if (iscellstr (selvar) || ischar (selvar)
                                         || isa (selvar, "string"))
                    if (ischar (selvar) || isa (selvar, "string"))
                      selvar = cellstr (selvar);
                    endif
                    ## When RowNames as set this way, the referenced Variable
                    ## is removed. Octave specific behavior. MATLAB does not
                    ## support this feature.
                    this.RowNames = selvar;
                    tbl = removevars (this, ixVar);
                    return
                  endif
                endif
              endif
              ## Check for valid input: cellstring, char, or string array
              ## matching the number of rows in the table
              if (ischar (val) || isa (val, "string"))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == height (this)))
                if (numel (__unique__ (val)) != height (this))
                error (["table.subsasgn: RowNames must be a cell array", ...
                        " of character vectors or a string array with", ...
                        " nonempty and distinct elements that are equal", ...
                        " to the number of variables."]);
                endif
              endif
              this.RowNames = val;
              tbl = this;

            elseif (isequal (s.subs, "CustomProperties"))
              ## Check that a custom property name is indexed
              if (numel (chain_s) < 2)
                if (isempty (val))
                  error (["table.subsasgn: use 'rmprop' to remove", ...
                          " an existing custom property."]);
                else
                  error (["table.subsasgn: use 'addprop' to add", ...
                          " a new custom property."]);
                endif
              endif
              ## Check for valid indexing a custom property
              if (! strcmp (chain_s(2).type, '.'))
                error (["table.subsasgn: use '.' notation to index", ...
                        " a custom property."]);
              endif
              cpName = chain_s(2).subs;
              if (! ischar (cpName))
                error (["table.subsasgn: indexing a custom property", ...
                        " requires a character vector."]);
              endif
              ## Check that referenced custom property exists
              if (isempty (this.CustomProperties))
                error (["table.subsasgn: custom property '%s' does not", ...
                        " exist, use 'addprop' to add it."], cpName);
              endif
              existingNames = fieldnames (this.CustomProperties);
              if (! ismember (cpName, existingNames))
                error (["table.subsasgn: custom property '%s' does not", ...
                        " exist, use 'addprop' to add it."], cpName);
              endif
              ## Get type of custom property
              cpType = this.CustomPropTypes{strcmp (cpName, existingNames)};
              if (strcmp (cpType, "table"))
                if (! ischar (val) && numel (val) > 1)
                  error (["table.subsasgn: custom property '%s' is a table", ...
                          " property and only a scalar value can be", ...
                          " assigned to it."], cpName);
                endif
                if (numel (chain_s) > 2)
                  error (["table.subsasgn: custom property '%s' is", ...
                          " a scalar table property and cannot be", ...
                          " indexed any further."], cpName);
                endif
                this.CustomProperties.(cpName) = val;
              else
                maxIdx = width (this);
                ## Check input is a vector
                if (! isvector (val))
                  error (["table.subsasgn: assigned value to a custom", ...
                          " variable property must be a vector."]);
                endif
                ## Get further indexing (if available)
                if (numel (chain_s) > 2)
                  if (strcmp (chain_s(3).type, '.'))
                    error (["table.subsasgn: custom property '%s' is", ...
                            " a variable property and cannot be indexed", ...
                            " any further with '.' notation."], cpName);
                  endif
                  cpIdx = chain_s(3).subs;
                  if (numel (cpIdx) > 1)
                    error (["table.subsasgn: cannot index a custom", ...
                            " variable property in more than one dimension."]);
                  endif
                  cpIdx = cell2mat (cpIdx);
                  if (isequal (cpIdx, ":"))
                    cpIdx = [1:maxIdx];
                  endif
                  if (! all (ismember (cpIdx, [1:maxIdx])))
                    error (["table.subsasgn: out of bound index for", ...
                            " custom variable property '%s'."], cpName);
                  endif
                  if (numel (val) != numel (cpIdx))
                    error (["table.subsasgn: input vector does not", ...
                            " match the number of indexed variables", ...
                            " in the custom variable property '%s'."], cpName);
                  endif
                  this.CustomProperties.(cpName)(cpIdx) = val;
                else
                  ## Check that input vector matches the number of variables
                  if (numel (val) != maxIdx)
                    error (["table.subsasgn: input vector does not", ...
                            " match the number of variables in table."]);
                  endif
                  this.CustomProperties.(cpName) = val;
                endif
              endif
              tbl = this;
            endif

          else
            ## Everything else is indexing a variable name (existing of new)
            tbl = setvar (this, s.subs, rhs);
          endif
      endswitch
    endfunction

  endmethods

  ## Private methods for accessing Tables and Properties
  methods (Access = private)

    ## Resolve variable references to indices and variable names.
    ## Returns:
    ##   @var{ixVar} - numeric indices of the variables in @var{tbl}
    ##   @var{varNames} - a cellstr of the names of the indexed variables
    ##
    ## Raises an error if any of the specified variables could not be resolved,
    ## unless strictness is 'lenient', in which case it will return 0 for the
    ## index and '' for the name for each variable which could not be resolved.
    function [ixVar, varNames] = resolveVarRef (this, varRef, strictness)
      if (nargin < 3 || isempty (strictness))
        strictness = "strict";
      endif
      if (! isvector (varRef))
        error ("table: variable index must be a vector.");
      endif
      nvars = width (this);
      if (islogical (varRef))
        vec = numel (varRef);
        if (nvars != vec)
          error ("table: variable logical index does not match table width.");
        endif
        ixVar = 1:nvars;
        ixVar(! varRef) = [];
      elseif (isnumeric (varRef))
        ixVar = varRef;
        ix_bad = find(ixVar > nvars | ixVar < 1);
        if (! isempty (ix_bad))
          error (["table: variable index out of bounds: requested index", ...
                  " %d; table has %d variables."], ...
                  ixVar(ix_bad(1)), nvars);
        endif
      elseif (isequal (varRef, ":"))
        ixVar = 1:nvars;
      elseif (ischar (varRef) || iscellstr (varRef) || isa (varRef, "string"))
        varRef = cellstr (varRef);
        [tf, ixVar] = ismember (varRef, this.VariableNames);
        if (isequal (strictness, "strict"))
          if (! all (tf))
            if (sum (! tf) == 1)
              error ("table: no such variable in table: '%s'.", varRef{! tf});
            else
              missing_vars = sprintf ("'%s', ", varRef{! tf});
              missing_vars(end-1:end) = [];
              error ("table: no such variables in table: %s.", missing_vars);
            endif
          endif
        else
          ixVar(! tf) = 0;
        endif
      elseif (isa (varRef, "vartype"))
        ixVar = [];
        for i = 1:nvars
          if (varRef.varMatch (this.VariableValues{i}))
            ixVar(end+1) = i;
          endif
        endfor
      else
        error ("table: unsupported variable indexing operand type: '%s'.", ...
               class (varRef));
      endif
      if (nargout > 1)
        varNames = repmat ({""}, size (ixVar));
        varNames(ixVar != 0) = this.VariableNames(ixVar(ixVar != 0));
      endif
    endfunction

    ## Resolve both row and variable references to indices.
    function [ixRow, ixVar] = resolveRowVarRefs (this, rowRef, varRef)
      if (isnumeric (rowRef) || islogical (rowRef))
        ixRow = rowRef;
      elseif (isequal (rowRef, ":"))
        ixRow = 1:height (this);
      elseif (ischar (rowRef) || iscellstr (rowRef) || isa (rowRef, "string"))
        rowRef = cellstr (rowRef);
        if (isempty (this.RowNames))
          error ("table: this table has no RowNames.");
        end
        [tf, ixRow] = ismember (rowRef, this.RowNames);
        if (! all (tf))
          error ("table: no such named row in table: '%s'.", ...
                 strjoin (rowRef(! tf), ", "));
        end
      else
        error ("table: unsupported row indexing operand type: '%s'.", ...
               class (rowRef));
      endif
      ixVar = resolveVarRef (this, varRef);
    endfunction

    ## Return a subset of rows defined by the numerical or logical vector ixRows
    function tbl = subsetrows (this, ixRows)
      tbl = this;
      s = struct ("type", "()", "subs", {{ixRows,":"}});
      for i = 1:width (this)
        tbl.VariableValues{i} = subsref (tbl.VariableValues{i}, s);
      endfor
      if (! isempty (this.RowNames))
        tbl.RowNames = tbl.RowNames(ixRows);
      endif
    endfunction

    ## Return a subset of variables defined by the numerical vector ixVars
    function tbl = subsetvars (this, ixVars)
      tbl = this;
      ## Copy selected variables
      tbl.VariableTypes = this.VariableTypes(ixVars);
      tbl.VariableNames = this.VariableNames(ixVars);
      tbl.VariableValues = this.VariableValues(ixVars);
      tbl.VariableDescriptions = this.VariableDescriptions(ixVars);
      tbl.VariableUnits = this.VariableUnits(ixVars);
      ## Check for custom variable properties
      if (! isempty (this.CustomProperties))
        cpIdx = strcmp (this.CustomPropTypes, "variable");
        if (any (cpIdx))
          ## Get the fieldnames of custom variable properties
          cpNames = fieldnames (this.CustomProperties);
          cpNames = cpNames(cpIdx);
          ## Copy custom variable properties from selected variables
          for i = 1:numel (cpNames)
            tmp = this.CustomProperties.(cpNames{i});
            if (isempty (tmp))
              tbl.CustomProperties.(cpNames{i}) = tmp;
            else
              tbl.CustomProperties.(cpNames{i}) = tmp(ixVars);
            endif
          endfor
        endif
      endif
    endfunction

    ## Get table properties as a struct for internal use called by subsasgn
    function out = getProperties (this)
      out = struct;
      out.Description = this.Description;
      out.UserData = this.UserData;
      out.DimensionNames = this.DimensionNames;
      out.VariableTypes = this.VariableTypes;
      out.VariableNames = this.VariableNames;
      out.VariableDescriptions = this.VariableDescriptions;
      out.VariableUnits = this.VariableUnits;
      out.VariableValues = this.VariableValues;
      out.RowNames = this.RowNames;
      out.CustomProperties = this.CustomProperties;
    endfunction

    ## Get values from a single referenced variable
    function out = getvar (this, var_ref)
      [ix_var, ~] = resolveVarRef (this, var_ref);
      out = this.VariableValues{ix_var};
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{out} =} setvar (@var{tbl}, @var{varRef}, @var{value})
    ##
    ## Set values to an existing or a new variable in table.
    ##
    ## This sets (adds or replaces) the value for a variable in @var{tbl}. It
    ## may be used to change the value of an existing variable, or add a new
    ## variable.
    ##
    ## @var{varRef} is a variable reference, either its index or its name.
    ## If you are adding a new variable, it must be a name, and not an index.
    ##
    ## @var{value} is the value to set the variable to.  If it is a scalar, it
    ## is scalar-expanded to match the number of rows in @var{tbl}.
    ##
    ## @end deftypefn
    function tbl = setvar (this, varRef, value)
      ## Do scalar expansion if necessary
      n_rows = height (this);
      val_is_scalar = (isscalar(value) || (ischar(value) && ...
        (size (value, 1) == 1 || isequal (size (value), [0 0]))));
      if (n_rows != 1 && (isscalar(value) || (ischar(value) &&
          (size (value, 1) == 1 || isequal (size (value), [0 0])))))
        if (ischar (value))
          value = {value};
        endif
        value = repmat (value, [n_rows, 1]);
      endif
      ## Check input matches table height
      if (size (value, 1) != n_rows)
        error ("table.subsasgn: input value and table height mismatch.");
      endif
      ## Resolve variable index
      ixVar = resolveVarRef (this, varRef, 'lenient');
      tbl = this;
      if (ixVar == 0)
        ## Add new variable
        ix_new_var = width (this) + 1;
        tbl.VariableNames{ix_new_var} = varRef;
        tbl.VariableTypes{ix_new_var} = class (value);
        tbl.VariableValues{ix_new_var} = value;
        tbl.VariableDescriptions{ix_new_var} = "";
        tbl.VariableUnits{ix_new_var} = "";
        ## Check for custom variable properties
        if (! isempty (this.CustomProperties))
          cpIdx = strcmp (this.CustomPropTypes, "variable");
          if (any (cpIdx))
            ## Get the fieldnames of custom variable properties
            cpNames = fieldnames (this.CustomProperties);
            cpNames = cpNames(cpIdx);
            ## Add default values to custom variable properties for new variable
            for i = 1:numel (cpNames)
              tmp = this.CustomProperties.(cpNames{i});
              if (! isempty (tmp))
                if (isnumeric (tmp))
                  tmp(end+1) = NaN;
                elseif (islogical (tmp))
                  tmp(end+1) = false;
                elseif (isa (tmp, "string"))
                  tmp(end+1) = string (NaN);
                elseif (iscell (tmp))
                  tmp{end+1} = [];
                endif
                tbl.CustomProperties.(cpNames{i}) = tmp;
              endif
            endfor
          endif
        endif
      else
        ## Set existing variable
        tbl.VariableTypes{ixVar} = class (value);
        tbl.VariableValues{ixVar} = value;
      endif
    endfunction

    ## Resolve subscripted reference for internal use called by subsasgn
    function out = single_subref (this, s)
      switch s.type
        case "()"
          if (numel (s.subs) != 2)
            error (["table.subsasgn: ()-indexing of table", ...
                    " requires exactly two arguments."]);
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          out = this;
          out = subsetrows (out, ixRow);
          out = subsetvars (out, ixVar);

        case '.'
          if (! ischar (s.subs))
            error (["table.subsasgn: .-index argument must be", ...
                    " a character vector."]);
          endif
          ## Handle special cases: "Properties" and "DimensionNames"
          if isequal (s.subs, "Properties")
            out = getProperties (this);
          elseif isequal (s.subs, this.DimensionNames{1})
            out = this.RowNames;
          elseif isequal (s.subs, this.DimensionNames{2})
            out = this.VariableNames;
          ## Everything else is indexing an existing variable name
          else
            out = getvar (this, s.subs);
          endif
      endswitch
    endfunction

  endmethods

  ## Private methods for displaying (printing) Tables and Properties
  methods (Access = private)

    ## Print Table Properties
    function print_properties (this)
      ## Gather info
      D = this.Description;
      if (isempty (this.UserData))
        UD = "[]";
      else
        sz = size (this.UserData);
        strs = cell (sz);
        for i = 1:numel (strs)
          strs{i} = sprintf ("%d", sz(i));
        endfor
        UD = strjoin (strs, "-by-");
        UD = [UD, " of type ", class(this.UserData)];
        if (iscellstr (this.UserData))
          UD = strrep (UD, "cell", "cellstr");
        endif
      endif
      DN = sprintf ("{'%s'  '%s'}", this.DimensionNames{:});
      VN = strtrim (sprintf ("'%s'  ", this.VariableNames{:}));
      VN = ["{", VN, "}"];
      if (all (cellfun (@isempty, this.VariableDescriptions)))
        VD = "{}";
      else
        VD = strtrim (sprintf ("'%s'  ", this.VariableDescriptions{:}));
        VD = ["{", VD, "}"];
      endif
      if (all (cellfun (@isempty, this.VariableUnits)))
        VU = "{}";
      else
        VU = strtrim (sprintf ("'%s'  ", this.VariableUnits{:}));
        VU = ["{", VU, "}"];
      endif
      VC = "[]";
      if (isempty (this.RowNames))
        RN = "{}";
      else
        RN = sprintf ("{%dx%d cell}", height (this), width (this));
      endif
      if (isempty (this.CustomProperties))
        CP = ["        CustomProperties: No custom properties are set.\n", ...
              "      Use 'addprop' and 'rmprop' methods to modify", ...
              " CustomProperties."];
      else
        CP = ["\n   Custom Properties (access using t.Properties.", ...
              "CustomProperties.<name>):"];
        cpNames = fieldnames (this.CustomProperties);
        for i = 1:numel (cpNames)
          cpValue = this.CustomProperties.(cpNames{i});
          if (isempty (cpValue))
            CP = [CP, sprintf("\n%+24s: []", cpNames{i})];
          elseif (islogical (cpValue) || isnumeric (cpValue))
            strValue = strtrim (disp (this.CustomProperties.(cpNames{i})));
            CP = [CP, sprintf("\n%+24s: [%s]", cpNames{i}, strValue(1:end))];
          elseif (iscellstr (cpValue))
            strValue = strtrim (sprintf ("'%s'  ", cpValue{:}));
            CP = [CP, sprintf("\n%+24s: {%s}", cpNames{i}, strValue)];
          elseif (isa (cpValue, "string"))
            cpValue = cellstr (cpValue);
            strValue = strtrim (sprintf ("""%s""  ", cpValue{:}));
            CP = [CP, sprintf("\n%+24s: [%s]", cpNames{i}, strValue)];
          elseif (ischar (cpValue))
            CP = [CP, sprintf("\n%+24s: '%s'", cpNames{i}, cpValue)];
          elseif (iscell (cpValue))
            strValue = '';
            for idx = 1:numel (cpValue)
              if (isempty (cpValue{idx}))
                strValue = [strValue, "  []"];
              elseif (islogical (cpValue{idx}) || isnumeric (cpValue{idx}))
                tmp = strtrim (disp (cpValue{idx}));
                strValue = [strValue, sprintf("  %s", tmp)];
              elseif (iscellstr (cpValue{idx}))
                tmp = sprintf ("{'%s'}", cpValue{idx})
                strValue = [strValue, sprintf("  %s", tmp)];
              elseif (isa (cpValue{idx}, "string"))
                tmp = cellstr (cpValue{idx});
                tmp = sprintf ("""%s""", tmp{:})
                strValue = [strValue, sprintf("  %s", tmp)];
              elseif (ischar (cpValue{idx}))
                strValue = [strValue, sprintf("  '%s'", cpValue{idx})];
              endif
            endfor
            CP = [CP, sprintf("\n%+24s: {%s}", cpNames{i}, strtrim (strValue))];
          endif
        endfor
      endif
      ## Print info
      fprintf ("\n  TableProperties with properties:\n\n");
      fprintf ("%+24s: '%s'\n", "Description", D);
      fprintf ("%+24s: %s\n", "UserData", UD);
      fprintf ("%+24s: %s\n", "DimensionNames", DN);
      fprintf ("%+24s: %s\n", "VariableNames", VN);
      fprintf ("%+24s: %s\n", "VariableDescriptions", VD);
      fprintf ("%+24s: %s\n", "VariableUnits", VU);
      fprintf ("%+24s: %s\n", "VariableContinuity", VC);
      fprintf ("%+24s: %s\n", "RowNames", RN);
      fprintf ("%s\n", CP);
    endfunction

    ## Display table internal function
    function print_table (this)
      ## Get VariableNames and VariableNames for optimal length of each column
      var_num = width (this);
      colData = {};
      rowSpat = "";
      T.parentV = [];
      T.nestedV = [];
      T.varName = {};
      T.varNLen = [];
      T.optLen = [];
      colgap = "    ";
      [colData, rowSpat, T] = resolve_table_for_printing ...
             (this, colData, rowSpat, T);
      ## Check for nested tables
      if (numel (T.nestedV) > 0)
        nested = true;
        ## Prepare nested table header
        varL_idx = 1;
        varN_idx = 1;
        varT_idx = 1;
        strhead1 = "";
        strline1 = "";
        strhead2 = "";
        strline2 = "";
        for v = 1:var_num
          ## Check for nested table in each variable
          if (ismember (v, T.parentV))
            ## Get name length of variable containing the table and remove it
            ## from T.varNLen so it is aligned with data columns' T.optLen
            parVarNLen = T.varNLen(varL_idx);
            parVarName = T.varName{varN_idx};
            T.varNLen(varL_idx) = [];
            ## Go through variables of nested table
            sum_optLen = 0;
            for nv = 1:T.nestedV(varT_idx)
              varN_idx += 1;
              pad_nB = floor ((T.optLen(varL_idx) - T.varNLen(varL_idx)) / 2);
              pad_nA = T.optLen(varL_idx) - (T.varNLen(varL_idx) + pad_nB);
              srtVarName = sprintf ("%s", T.varName{varN_idx});
              strhead2 = [strhead2, repmat(" ", [1, pad_nB]), ...
                          srtVarName, repmat(" ", [1, pad_nA]), colgap];
              strline2 = [strline2, repmat("_", [1, T.optLen(varL_idx)]), colgap];
              sum_optLen += T.optLen(varL_idx) + 4;
              varL_idx += 1;
            endfor
            ## Keep track of indexing
            sum_optLen -= 4;
            varL_idx -= 1;
            ## Fix continuous line for all nested table variables
            strline1 = [strline1, repmat("_", [1, sum_optLen]), colgap];
            ## Position parent variable in the middle of the top header
            pad_nB = floor ((sum_optLen - parVarNLen) / 2);
            pad_nA = sum_optLen - (parVarNLen + pad_nB);
            srtVarName = sprintf ("%s", parVarName);
            strhead1 = [strhead1, repmat(" ", [1, pad_nB]), ...
                        srtVarName, repmat(" ", [1, pad_nA]), colgap];
            ## Increment index for nested table variables
            varT_idx += 1;
          else
            ## Oridinary variable (no nested table)
            pad_nB = floor ((T.optLen(varL_idx) - T.varNLen(varL_idx)) / 2);
            pad_nA = T.optLen(varL_idx) - (T.varNLen(varL_idx) + pad_nB);
            srtVarName = sprintf ("%s", T.varName{varN_idx});
            strhead1 = [strhead1, repmat(" ", [1, pad_nB]), ...
                        srtVarName, repmat(" ", [1, pad_nA]), colgap];
            strhead2 = [strhead2, repmat(" ", [1, T.optLen(varL_idx)]), colgap];
            strline1 = [strline1, repmat("_", [1, T.optLen(varL_idx)]), colgap];
            strline2 = [strline2, repmat(" ", [1, T.optLen(varL_idx)]), colgap];
          endif
          varL_idx += 1;
          varN_idx += 1;
        endfor
      else
        ## No nested table
        nested = false;
        ## Prepare table header
        strhead1 = "";
        strline1 = "";
        for v = 1:var_num
          pad_nB = floor ((T.optLen(v) - T.varNLen(v)) / 2);
          pad_nA = T.optLen(v) - (T.varNLen(v) + pad_nB);
          srtVarName = sprintf ("%s", T.varName{v});
          strhead1 = [strhead1, repmat(" ", [1, pad_nB]), ...
                      srtVarName, repmat(" ", [1, pad_nA]), colgap];
          strline1 = [strline1, repmat("_", [1, T.optLen(v)]), colgap];
        endfor
      endif
      ## Check whether RowNames are available in table and construct
      ## cell array of strings of size [height(table), 1] to prepad
      ## the displayed table with
      if (! isempty (this.RowNames))
        rnLen = max (cellfun (@length, this.RowNames)) + 4;
        padPT = sprintf("%%-%ds", rnLen);
        padfn = @(x) sprintf (padPT, x);
        rowNM = cellfun (padfn, this.RowNames, "UniformOutput", false);
        ## Print table header
        fprintf ("    %s%s\n", repmat(" ", [1, rnLen]), strhead1);
        fprintf ("    %s%s\n\n", repmat(" ", [1, rnLen]), strline1);
        if (nested)
          fprintf ("    %s%s\n", repmat(" ", [1, rnLen]), strhead2);
          fprintf ("    %s%s\n\n", repmat(" ", [1, rnLen]), strline2);
        endif
        ## Print table rows
        for iRow = 1:height (this)
          strrow = sprintf (rowSpat, colData{iRow,:});
          fprintf ("    %s%s\n", rowNM{iRow}, strrow);
        endfor
        fprintf ("\n");
      else
        ## Print table header
        fprintf ("    %s\n", strhead1);
        fprintf ("    %s\n\n", strline1);
        if (nested)
          fprintf ("    %s\n", strhead2);
          fprintf ("    %s\n\n", strline2);
        endif
        ## Print table rows
        for iRow = 1:height (this)
          strrow = sprintf (rowSpat, colData{iRow,:});
          fprintf ("    %s\n", strrow);
        endfor
        fprintf ("\n");
      endif
    endfunction

    ## Prepare table for printing
    function [colData, rowSpat, T] = resolve_table_for_printing ...
                                     (this, colData, rowSpat, T)
      ## Get recursion for nested tables
      #n = numel (T);
      if (numel (T.nestedV) > 0)
        nested = true;
        minLen = T.varNLen(end);
      else
        nested = false;
        minLen = 1;
      endif
      colgap = "    ";
      ## Start parsing table variables
      for v = 1:width (this)
        ## Get variable name
        T.varName = [T.varName, this.VariableNames(v)];
        ## Get length of variable name
        varNLen = length (this.VariableNames{v});
        T.varNLen = [T.varNLen, varNLen];
        ## Get max length from data
        data = this.VariableValues{v};
        cols = size (data)(2);
        ## Numeric
        if (isnumeric (data))
          numfun = @(x) sprintf ("%g", x);
          if (cols > 1)
            colLen = zeros (1, cols);
            rowSpat_c = "";
            for c = 1:cols
              ## Prepare data values to char vector
              tmpData = arrayfun (numfun, data(:,c), "UniformOutput", false);
              colData = [colData, tmpData];
              ## Get max length and append row string pattern
              colLen(c) = max (cellfun (@length, tmpData));
              rowSpat_c = [rowSpat_c, sprintf("%%+%ds", colLen(c)), colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            ## Prepare data values to char vector
            tmpData = arrayfun (numfun, data, "UniformOutput", false);
            colData = [colData, tmpData];
            ## Get max length and append row string pattern
            dataLen = max (cellfun (@length, tmpData));
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%+%ds", optLen), colgap];
          endif
        ## Logical
        elseif (islogical (data))
          if (cols > 1)
            rowSpat_c = "";
            for c = 1:cols
              tmpData = repmat ({"false"}, size (data(:,c)));
              tmpData(data(:,c)) = "true";
              colData = [colData, tmpData];
              colLen(c) = 5;
              rowSpat_c = [rowSpat_c, "%-5s", colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            tmpData = repmat ({"false"}, size (data));
            tmpData(data) = "true";
            colData = [colData, tmpData];
            dataLen = 5;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
          endif
        ## Categorical
        elseif (isa (data, {"categorical"}))
          if (cols > 1)
            colLen = zeros (1, cols);
            rowSpat_c = "";
            for c = 1:cols
              tmpData = dispstrings (data(:,c));
              colData = [colData, tmpData];
              colLen(c) = max (cellfun (@length, tmpData));
              rowSpat_c = [rowSpat_c, sprintf("%%+%ds", colLen(c)), colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            tmpData = dispstrings (data);
            colData = [colData, tmpData];
            dataLen = max (cellfun (@length, tmpData));
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%+%ds", optLen), colgap];
          endif
        ## Datetime, duration, calendarDuration
        elseif (any (isa (data, {"datetime", "duration", "calendarDuration"})))
          if (cols > 1)
            colLen = zeros (1, cols);
            rowSpat_c = "";
            for c = 1:cols
              tmpData = dispstrings (data(:,c));
              colData = [colData, tmpData];
              colLen(c) = max (cellfun (@length, tmpData));
              rowSpat_c = [rowSpat_c, sprintf("%%+%ds", colLen(c)), colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;
            optLen = max ([varNLen, dataLen]);
            T.optLen = [T.optLen, optLen, minLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            tmpData = dispstrings (data);
            colData = [colData, tmpData];
            dataLen = max (cellfun (@length, tmpData));
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%+%ds", optLen), colgap];
          endif
        ## String
        elseif (isa (data, "string"))
          if (cols > 1)
            colLen = zeros (1, cols);
            rowSpat_c = "";
            for c = 1:cols
              tmpData = dispstrings (data(:,c));
              colData = [colData, tmpData];
              colLen(c) = max (cellfun (@length, tmpData));
              rowSpat_c = [rowSpat_c, sprintf("%%-%ds", colLen(c)), colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            tmpData = dispstrings (data);
            colData = [colData, tmpData];
            dataLen = max (cellfun (@length, tmpData));
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen optLen];
            rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
          endif
        ## Character vectors
        elseif (ischar (data))
          fcn = @(x) sprintf ("'%s'", x); ## add '' unlike MATLAB display
          tmpData = cell (rows (data), 1);
          for r = 1:rows (data)
            tmpData(r) = fcn (data(r,:));
          endfor
          colData = [colData, tmpData];
          dataLen = max (cellfun (@length, tmpData));
          optLen = max ([varNLen, dataLen, minLen]);
          T.optLen = [T.optLen, optLen];
          rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
        ## Cell array of character vectors
        elseif (iscellstr (data))
          fcn = @(x) sprintf ("'%s'", x); ## add '' for MATLAB like display
          if (cols > 1)
            colLen = zeros (1, cols);
            rowSpat_c = "";
            for c = 1:cols
              tmpData = cellfun (fcn, data(:,c), "UniformOutput", false);
              colData = [colData, tmpData];
              colLen(c) = max (cellfun (@length, tmpData)) + 2;
              rowSpat_c = [rowSpat_c, sprintf("{%%-%ds}", ...
                                      colLen(c) - 4), colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            tmpData = cellfun (fcn, data, "UniformOutput", false);
            colData = [colData, tmpData];
            dataLen = max (cellfun (@length, tmpData)) + 2;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("{%%-%ds}", optLen - 2), colgap];
          endif
        ## Cell array of mixed values
        elseif (iscell (data))
          if (cols > 1)
            colLen = zeros (1, cols);
            rowSpat_c = "";
            for c = 1:cols
              [tmpData, colLen(c)]  = mixedcell2str (data(:,c), varNLen);
              colData = [colData, tmpData];
              rowSpat_c = [rowSpat_c, sprintf("%%-%ds", colLen(c)), colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;  # +2 due to extra {}
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            [tmpData, optLen]  = mixedcell2str (data, varNLen);
            T.optLen = [T.optLen, max([optLen, minLen])];
            colData = [colData, tmpData];
            rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
          endif
        ## Structures
        elseif (isa (data, "struct"))
          if (cols > 1)
            rowSpat_c = "";
            for c = 1:cols
              tmpData = repmat ({"<struct>"}, size (data(:,c)));
              colData = [colData, tmpData];
              colLen(c) = 8;
              rowSpat_c = [rowSpat_c, "%-8s", colgap];
            endfor
            dataLen = sum (colLen + 4) - 4;
            optLen = max ([TvarNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            prePad = repmat (" ", [1, optLen-dataLen]);
            rowSpat = [rowSpat, prePad, rowSpat_c];
          else
            tmpData = repmat ({"<struct>"}, size (data));
            colData = [colData, tmpData];
            optLen = max ([varNLen, 8, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
          endif
        ## Tables (nested)
        elseif (isa (data, "table"))
          if (nested)
            tmpData = repmat ({"<table>"}, [height(data), 1]);
            colData = [colData, tmpData];
            optLen = max ([varNLen, 7, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
          else
            ## Increment structure array, add referenced variable, and
            ## recurse with nested table
            T.nestedV = [T.nestedV width(data)];
            T.parentV = [T.parentV v];
            [colData, rowSpat, T] = resolve_table_for_printing ...
                                    (data, colData, rowSpat, T);
          endif
        endif
      endfor
    endfunction

    ## Summary internal function
    function s = summary_for_variables (this);
      for v = 1:width (this)
        varName = this.VariableNames{v};
        s.(varName).Size = size (this.VariableValues{v});
        s.(varName).Type = class (this.VariableValues{v});
        if (! isempty (this.VariableDescriptions{v}))
          s.(varName).Description = this.VariableDescriptions{v};
        else
          s.(varName).Description = "";
        endif
        if (! isempty (this.VariableUnits{v}))
          s.(varName).Units = this.VariableUnits{v};
        else
          s.(varName).Units = "";
        endif
        s.(varName).Continuity = [];
        if (isa (this.VariableValues{v}, "logical"))
          s.(varName).True = sum (this.VariableValues{v});
          s.(varName).False = sum (! this.VariableValues{v});
        elseif (isa (this.VariableValues{v}, "duration"))
          sec = seconds (this.VariableValues{v});
          nat = isnan (sec);
          s.(varName).Min = min (sec(! nat));
          s.(varName).Median = median (sec(! nat));
          s.(varName).Max = max (sec(! nat));
          s.(varName).NumMissing = sum (nat);
          ## Check for TimeStep
          TimeStep = __unique__ (diff (sort (sec)))
          if (numel (TimeStep) == 1)
            s.(varName).TimeStep = TimeStep;
          else
            s.(varName).TimeStep = NaN;
          endif
        elseif (isa (this.VariableValues{v}, "datetime"))
          day = this.VariableValues{v}.dnums;
          nat = isnan (day);
          s.(varName).Min = min (day(! nat));
          s.(varName).Median = median (day(! nat));
          s.(varName).Max = max (day(! nat));
          s.(varName).NumMissing = sum (nat);
          ## Check for TimeStep
          TimeStep = __unique__ (diff (sort (day)))
          if (numel (TimeStep) == 1)
            s.(varName).TimeStep = TimeStep;
          else
            s.(varName).TimeStep = NaN;
          endif
        elseif (isa (this.VariableValues{v}, "calendarDuration"))
          day = datetime ([0, 0, 0]) + this.VariableValues{v};
          day = day.dnums;
          nat = isnan (day);
          s.(varName).Min = min (day(! nat));
          s.(varName).Median = median (day(! nat));
          s.(varName).Max = max (day(! nat));
          s.(varName).NumMissing = sum (nat);
          ## Check for TimeStep
          TimeStep = __unique__ (diff (sort (day)))
          if (numel (TimeStep) == 1)
            s.(varName).TimeStep = TimeStep;
          else
            s.(varName).TimeStep = NaN;
          endif
        elseif (isnumeric (this.VariableValues{v}))
          ## Cannot avoid dependency on statistics package here for median
          ## until Octave >= 9.1, when median with omitnan option gets into
          ## core Octave.  Hence no need to workaround nanmin and nanmax.
          s.(varName).Min = nanmin (this.VariableValues{v});
          s.(varName).Median = median (this.VariableValues{v}, "omitnan");
          s.(varName).Max = nanmax (this.VariableValues{v});
          s.(varName).NumMissing = sum (isnan (this.VariableValues{v}));
        endif
        ## No need to summarize values in 'cell', 'cellstr', 'string',
        ## and 'struct' variable types .

        ## Fix me: as soon as CustomProperties are introduced in table class
      endfor
    endfunction

  endmethods

  ## Private methods for exporting (saving) Tables and Properties to files
  methods (Access = private)

    ## Export table to cell arrays
    function [V, N, T, D, U] = table2cellarrays (this)
      V = {};  # variable values
      N = {};  # variable names
      T = {};  # variable types
      D = {};  # variable descriptions
      U = {};  # variable units
      ## Process RowNames
      if (! isempty (this.RowNames))
        V = [V, this.RowNames];
        N = [N, {''}];
        T = [T, 'cellstr'];
        D = [D, {''}];
        U = [U, {''}];
      endif
      ## Process variables
      for ix = 1:width (this)
        var_V = this.VariableValues{ix};
        ncols = size (var_V, 2);
        ## Handle each variable type
        if (iscell (var_V))
          for col = 1:ncols
            V = [V, var_V(:,col)];
            N = [N, this.VariableNames{ix}];
            T = [T, 'cell'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (islogical (var_V))
          for col = 1:ncols
            V = [V, num2cell(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'logical'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isnumeric (var_V))
          for col = 1:ncols
            V = [V, num2cell(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, class(var_V(:,col))];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'calendarDuration'))
          for col = 1:ncols
            V = [V, dispstrs(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'calendarDuration'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'categorical'))
          for col = 1:ncols
            V = [V, dispstrs(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'categorical'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'datetime'))
          for col = 1:ncols
            V = [V, dispstrs(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'datetime'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'duration'))
          for col = 1:ncols
            V = [V, dispstrs(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'duration'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'string'))
          for col = 1:ncols
            V = [V, cellstr(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'string'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'table'))
          [tmpV, tmpN, tmpT tmpD, tmpU] = table2cellarrays (var_V);
          V = [V, tmpV];
          nestedN = {};
          nestedT = {};
          nestedD = {};
          nestedU = {};
          for col = 1:size (tmpV, 2)
            nestedN = [nestedN, {{this.VariableNames{ix}; tmpN{col}}}];
            nestedT = [nestedT, {{'table'; tmpT{col}}}];
            if (isempty (tmpD{col}))
              nestedD = [nestedD, {{this.VariableDescriptions(ix); {''}}}];
            else
              nestedD = [nestedD, {{this.VariableDescriptions(ix); tmpD(col)}}];
            endif
            if (isempty (tmpU{col}))
              nestedU = [nestedU, {{this.VariableUnits(ix); {''}}}];
            else
              nestedU = [nestedU, {{this.VariableUnits(ix); tmpU(col)}}];
            endif
          endfor
          N = [N, nestedN];
          T = [T, nestedT];
          D = [D, nestedD];
          U = [U, nestedU];
        elseif (isa (var_V, 'struct'))
          tmpV = squeeze (struct2cell (var_V(:)))';
          tmpN = fieldnames (var_V(:))';
          tmpT = cellfun ('class', tmpV(1,:), 'UniformOutput', false);
          V = [V, tmpV];
          nestedN = {};
          nestedT = {};
          nestedD = {};
          nestedU = {};
          for col = 1:size (tmpV, 2)
            nestedN = [nestedN, {{this.VariableNames{ix}; tnpH{col}}}];
            nestedT = [nestedT, {{'struct'; tmpT{col}}}];
            nestedD = [nestedD, {{this.VariableDescriptions(ix); {''}}}];
            nestedU = [nestedU, {{this.VariableUnits(ix); {''}}}];
          endfor
          N = [N, nestedN];
          T = [T, nestedT];
          D = [D, nestedD];
          U = [U, nestedU];
        endif
      endfor
    endfunction

  endmethods

endclassdef

## Special function to convert a mixed cell array to cellstr array
## that keeps MATLAB like formatting for each type of element
function [outData, optLen]  = mixedcell2str (data, varLen)
  ## Preallocate indexes to avoid truncation when last elements are 0
  idx_cell = logical (zeros (size (data)));
  idx_charvec = idx_cell;
  idx_logical = idx_cell;
  idx_numeric = idx_cell;
  idx_object = idx_cell;
  idx_string = idx_cell;
  idx_struct = idx_cell;

  ## Find scalars or row vectors
  se = cell2mat (cellfun (@(x) numel (x), data, "UniformOutput", false)) == 1;
  ve = cell2mat (cellfun (@(x) size (x,1), data, "UniformOutput", false)) == 1;

  ## Catch 'cell' scalars
  tmp = cell2mat (cellfun (@iscell, data(se), "UniformOutput", false)) == 1;
  idx_cell(se) = tmp;
  sf = @(x) sprintf ("1x1 cell");
  out_str(idx_cell) = (cellfun (sf, data(idx_cell), ...
                       "UniformOutput", false));
  ## Catch 'char' scalars or row vectors
  tmp = cell2mat (cellfun (@ischar, data(ve), "UniformOutput", false));
  idx_charvec(ve) = tmp;
  sf = @(x) sprintf ("'%s'", x);
  out_str(idx_charvec) = (cellfun (sf, data(idx_charvec), ...
                          "UniformOutput", false));
  ## Catch 'logical' scalars or row vectors
  tmp = cell2mat (cellfun (@islogical, data(ve), "UniformOutput", false)) == 1;
  idx_logical(ve) = tmp;
  sf = @(x) sprintf ("[%s]", strtrim (sprintf ("%d ", x)));
  out_str(idx_logical) = (cellfun (sf, data(idx_logical), ...
                          "UniformOutput", false));
  ## Catch 'numeric' scalars or row vectors
  tmp = cell2mat (cellfun (@isnumeric, data(ve), "UniformOutput", false)) == 1;
  idx_numeric(ve) = tmp;
  sf = @(x) sprintf ("[%s]", strtrim (sprintf ("%g ", x)));
  out_str(idx_numeric) = (cellfun (sf, data(idx_numeric), ...
                          "UniformOutput", false));
  ## Catch 'object' scalars
  tmp = cell2mat (cellfun (@isobject, data(se), "UniformOutput", false)) == 1;
  idx_struct(se) = tmp;
  sf = @(x) sprintf ("1x1 %s", class (x));
  out_str(idx_struct) = (cellfun (sf, data(idx_struct), ...
                         "UniformOutput", false));
  ## Catch 'string' scalars or row vectors
  tmp = cell2mat (cellfun (@isstring, data(ve), "UniformOutput", false)) == 1;
  idx_string(ve) = tmp;
  sf = @(x) sprintf ("[%s]", strtrim (sprintf ("%s    ", dispstrings(x){:})));
  out_str(idx_string) = (cellfun (sf, data(idx_string), ...
                         "UniformOutput", false));
  ## Catch scalar elements of struct type
  tmp = cell2mat (cellfun (@isstruct, data(se), "UniformOutput", false)) == 1;
  idx_struct(se) = tmp;
  sf = @(x) sprintf ("1x1 struct");
  out_str(idx_struct) = (cellfun (sf, data(idx_struct), ...
                         "UniformOutput", false));

  ## Keep indexes for numerical and logical values to right alignment
  pad_B = idx_numeric | idx_logical;  # pad before: sprintf("{%%-%ds}"
  pad_A = ! pad_B;                    # pad after:  sprintf("{%%+%ds}"

  ## Catch remaining elements
  me = ! (idx_cell | idx_charvec | idx_logical | idx_numeric | ...
          idx_object | idx_string | idx_struct);

  ## Preallocate indexes to avoid truncation when last elements are 0
  idx_cell = logical (zeros (size (data)));
  idx_charvec = idx_cell;
  idx_logical = idx_cell;
  idx_numeric = idx_cell;
  idx_object = idx_cell;
  idx_string = idx_cell;
  idx_struct = idx_cell;

  if (any (me))
    ## 'cell' arrays
    tmp = cell2mat (cellfun (@iscell, data(me), "UniformOutput", false)) == 1;
    idx_cell(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' cell']), size (x));
    out_str(idx_cell) = (cellfun (sf, data(idx_cell), ...
                         "UniformOutput", false));
    ## 'char' arrays
    tmp = cell2mat (cellfun (@ischar, data(me), "UniformOutput", false));
    idx_charvec(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' char']), size (x));
    out_str(idx_charvec) = (cellfun (sf, data(idx_charvec), ...
                            "UniformOutput", false));
    ## 'logical' arrays
    tmp = cell2mat (cellfun (@islogical, data(me), "UniformOutput", false)) == 1;
    idx_logical(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' logical']), size (x));
    out_str(idx_logical) = (cellfun (sf, data(idx_logical), ...
                            "UniformOutput", false));
    ## 'numeric' arrays
    tmp = cell2mat (cellfun (@isnumeric, data(me), "UniformOutput", false)) == 1;
    idx_numeric(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' array']), size (x));
    out_str(idx_numeric) = (cellfun (sf, data(idx_numeric), ...
                            "UniformOutput", false));
    ## 'object' arrays
    tmp = cell2mat (cellfun (@isstring, data(me), "UniformOutput", false)) == 1;
    idx_string(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' %s']), size (x), class (x));
    out_str(idx_string) = (cellfun (sf, data(idx_string), ...
                           "UniformOutput", false));
    ## 'string' arrays
    tmp = cell2mat (cellfun (@isstring, data(me), "UniformOutput", false)) == 1;
    idx_string(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' string']), size (x));
    out_str(idx_string) = (cellfun (sf, data(idx_string), ...
                           "UniformOutput", false));
    ## 'struct' arrays
    tmp = cell2mat (cellfun (@isstruct, data(me), "UniformOutput", false)) == 1;
    idx_struct(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' struct']), size (x));
    out_str(idx_struct) = (cellfun (sf, data(idx_struct), ...
                           "UniformOutput", false));
  endif

  ## Get optimal length
  strLen = max (cellfun (@length, out_str)) + 2;
  optLen = max ([varLen, strLen]);

  ## Pad data according to optimal length
  ## numeric and logical is right aligned, everything else is left aligned
  Ra = sprintf("{%%+%ds}", optLen - 2);
  La = sprintf("{%%-%ds}", optLen - 2);
  fcn = @(x) sprintf (Ra, x);
  outData(pad_B) = cellfun (fcn, out_str(pad_B), "UniformOutput", false);
  fcn = @(x) sprintf (La, x);
  outData(pad_A) = cellfun (fcn, out_str(pad_A), "UniformOutput", false);
  outData = outData(:);
endfunction



## Test 'subref' and 'subsasgn' methods
%!shared LastName, Age, Smoker, Height, Weight, BloodPressure, T, tblA
%! LastName = {"Sanchez"; "Johnson"; "Li"; "Diaz"; "Brown"};
%! Age = [38; 43; 38; 40; 49];
%! Smoker = logical ([1; 0; 1; 0; 1]);
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 133; 119];
%! BloodPressure = [124, 93; 109, 77; 125, 83; 117, 75; 122, 80];
%! T = table (Age, Smoker, Height, Weight, BloodPressure);
%!assert (size (T(1,:)), [1, 5]);
%!assert (class (T(1,:)), "table");
%!assert (size (T{1,:}), [1, 6]);
%!assert (T{1,:}, [38, 1, 71, 176, 124, 93]);
%!assert (T{:,2}, logical([1; 0; 1; 0; 1]));
%!assert (T{:,"Smoker"}, logical([1; 0; 1; 0; 1]));
%!assert (T{:,"Height"}, [71; 69; 64; 67; 64]);
%!assert (size(T{:,"Smoker"}), [5, 1]);
%!assert (size(T{:,"Height"}), [5, 1]);
%!assert (T.Variables, [Age, Smoker, Height, Weight, BloodPressure]);
%!assert (isempty (T.Properties.RowNames), true);
%!assert (numel (T.Properties.VariableNames), 5);
%!test
%! T.Properties.Description = "text";
%! assert (T.Properties.Description, "text");
%!assert (size (T(:,{"Age", "Smoker"})), [5, 2]);
%!assert (T{:,{"Age", "Smoker"}}, [Age, Smoker]);
%!test
%! T = table (Age, Smoker);
%! T.("29-May-2019 Blood Pressure Reading") = BloodPressure;
%! assert (T.("29-May-2019 Blood Pressure Reading"), BloodPressure);
%!test
%! T = table(Age,Smoker);
%! T.Height = Height;
%! T.Weight = Weight;
%! assert (size (T), [5, 4]);
%! assert (T.Weight, Weight);
%! assert (T.Weight, T{:,"Weight"});
%! assert (T.Weight, T{:,4});
%!test
%! T.Weight(1) = 25;
%! assert (T.Weight, [25;Weight(2:end)]);
%!test
%! T.Weight([1,3]) = 25;
%! assert (T.Weight, [25;Weight(2);25;Weight(4:end)]);
%!test
%! T.Weight([1,3]) = 25;
%! assert (T.Weight, [25;Weight(2);25;Weight(4:end)]);
%!test
%! T.Weight([1:3,5]) = 25;
%! assert (T.Weight, [25;25;25;Weight(4);25]);
%!test
%! T = table (LastName, Age, Smoker, Height, Weight);
%! T.Properties.RowNames = string (LastName);
%!test
%! T = table (LastName, Age, Smoker, Height, Weight);
%! T.Properties.RowNames = 'LastName';
%!test
%! T = table (LastName, Age, Smoker, Height, Weight);
%! T.Properties.RowNames = {'LastName'};
%!test
%! T = table (LastName, Age, Smoker, Height, Weight);
%! T.Properties.RowNames = ['LastName'];
%!test
%! T = table (LastName, Age, Smoker, Height, Weight);
%! assert (isempty (T.Row), true);
%! T.Properties.DimensionNames(1) = 'Patients';
%! assert (isempty (T.Patients), true);
%!test
%! T.Properties.RowNames = "LastName";
%! assert (isempty (T.Patients), false);
%! T.Properties.DimensionNames(2) = 'Data';
%! assert (T.Data, [Age,Smoker,Height,Weight]);

## Test **summary information** methods
%!test
%! T = table (Age, Smoker, Height, Weight, BloodPressure);
%! assert (table2array (T), [Age, Smoker, Height, Weight, BloodPressure]);
%!test
%! C = table2cell (T);
%! assert (cell2mat (C(:,[1:4])), [Age, Smoker, Height, Weight]);
%!test
%! S = table2struct (T);
%! assert ([S.Age], Age');
%! assert (reshape ([S.BloodPressure], 2, 5)', BloodPressure);
%! assert (numel (S), height (T));
%!test
%! S = table2struct (T, "ToScalar", true);
%! assert (S.Age, Age);
%! assert (S.BloodPressure, BloodPressure);
%! assert (numel (S), 1);
%!test
%! t = summary (T);
%! assert (isstruct (t), true);
%! assert ([t.Age.Min, t.Age.Median, t.Age.Max], [38, 40, 49]);
%! assert (t.Smoker.Size, [5, 1]);
%! assert (t.BloodPressure.Size, [5, 2]);
%!assert (height (T), 5);
%!assert (width (T), 5);
%!assert (height (head (T, 3)), 3);
%!assert (height (head (T, 8)), 5);
%!assert (height (head (T)), 5);
%!assert (height (tail (T, 3)), 3);
%!assert (height (tail (T, 8)), 5);
%!assert (height (tail (T)), 5);

## Test 'sortrows' and 'issortedrows' methods
%!test
%! tblA = table (Age, Height, Weight, BloodPressure, 'RowNames', LastName);
%! assert (issortedrows (tblA, "RowNames"), false);
%! [sorted_tblA, index] = sortrows (tblA, "RowNames");
%! assert (index, [5; 4; 2; 3; 1]);
%! assert (issortedrows (sorted_tblA, "RowNames"), true);
%! [sorted_tblA, index] = sortrows (tblA, "RowNames", "descend");
%! assert (index, [1; 3; 2; 4; 5]);
%! assert (issortedrows (sorted_tblA, "RowNames", "descend"), true);
%!test
%! tblA.Properties.DimensionNames(1) = "Patients";
%! assert (issortedrows (tblA, "Patients"), false);
%! [sorted_tblA, index] = sortrows (tblA, "Patients");
%! assert (index, [5; 4; 2; 3; 1]);
%! assert (issortedrows (sorted_tblA, "Patients"), true);
%! [sorted_tblA, index] = sortrows (tblA, "Patients", "descend");
%! assert (index, [1; 3; 2; 4; 5]);
%! assert (issortedrows (sorted_tblA, "Patients", "descend"), true);
%!test
%! [sorted_tblA, index] = sortrows (tblA);
%! assert (index, [3; 1; 4; 2; 5]);
%! assert (issortedrows (sorted_tblA), true);
%! [sorted_tblA, index] = sortrows (tblA, [], "ascend");
%! assert (index, [3; 1; 4; 2; 5]);
%! assert (issortedrows (sorted_tblA, [], "ascend"), true);
%! [sorted_tblA, index] = sortrows (tblA, [], "descend");
%! assert (index, [5; 2; 4; 1; 3]);
%! assert (issortedrows (sorted_tblA, [], "descend"), true);
%!test
%! [tblB, index] = sortrows (tblA, "Height");
%! assert (index, [3; 5; 4; 2; 1]);
%! assert (tblB.Properties.RowNames, ...
%!         {"Li"; "Brown"; "Diaz"; "Johnson"; "Sanchez"})
%! assert (issortedrows (tblB, "Height"), true);
%!test
%! LastName = {"Sweet"; "Jacobson"; "Wang"; "Joiner"; "Berger"};
%! tblA = table (Age, Height, Weight, BloodPressure, 'RowNames', LastName);
%! [tblB, index] = sortrows (tblA, {"Height", "Weight"}, {"ascend", "descend"});
%! assert (index, [3; 5; 4; 2; 1]);
%! assert (tblB.Properties.RowNames, ...
%!         {"Wang"; "Berger"; "Joiner"; "Jacobson"; "Sweet"})
%! assert (issortedrows (tblB, {"Height", "Weight"}, ...
%!                             {"ascend", "descend"}), true);
%!test
%! [tblB, index] = sortrows (tblA, {"BloodPressure"}, {"ascend"});
%! assert (index, [2; 4; 5; 1; 3]);
%! assert (tblB.BloodPressure, sortrows (BloodPressure));
%! assert (issortedrows (tblB, {"BloodPressure"}, {"ascend"}), true);
%! [tblB, index] = sortrows (tblA, {"BloodPressure"}, {"descend"});
%! assert (index, flip ([2; 4; 5; 1; 3]));
%! assert (tblB.BloodPressure, sortrows (BloodPressure, -1));
%! assert (issortedrows (tblB, "BloodPressure", "descend"), true);
%!test
%! [tblB, index] = sortrows (tblA, {"Height", "RowNames"}, {"ascend"});
%! assert (index, [5; 3; 4; 2; 1]);
%! assert (tblB.Properties.RowNames, ...
%!         {"Berger"; "Wang"; "Joiner"; "Jacobson"; "Sweet"})
%! assert (tblB.Height, sortrows (Height));
%! assert (issortedrows (tblB, {"Height", "RowNames"}, {"ascend"}), true);
%! [tblB, index] = sortrows (tblA, {"Height", "RowNames"}, ...
%!                                 {"ascend", "descend"});
%! assert (index, [3; 5; 4; 2; 1]);
%! assert (tblB.Properties.RowNames, ...
%!         {"Wang"; "Berger"; "Joiner"; "Jacobson"; "Sweet"})
%! assert (issortedrows (tblB, {"Height", "RowNames"}, ...
%!                             {"ascend", "descend"}), true);
%!test
%! [tblB, index] = sortrows (tblA, [true, true, false, false]);
%! assert (index, [3; 1; 4; 2; 5]);
%! assert (issortedrows (tblB, [true, true, false, false]), true);
%! [tblB, index] = sortrows (tblA, [true, true, false, false], "ascend");
%! assert (index, [3; 1; 4; 2; 5]);
%! assert (issortedrows (tblB, [true, true, false, false], "ascend"), true);
%! [tblB, index] = sortrows (tblA, [true, true, false, false], "descend");
%! assert (index, flip ([3; 1; 4; 2; 5]));
%! assert (issortedrows (tblB, [true, true, false, false], "descend"), true);
%! [tblB, index] = sortrows (tblA, [true, true, false, false], ...
%!                                 {"ascend", "descend"});
%! assert (index, [1; 3; 4; 2; 5]);
%! assert (issortedrows (tblB, [true, true, false, false], ...
%!                             {"ascend", "descend"}), true);
%!test
%! [sorted_tblA, indexN] = sortrows (tblA, 1);
%! assert (issortedrows (sorted_tblA, 1), true);
%! [sorted_tblA, indexV] = sortrows (tblA, "Age");
%! assert (issortedrows (sorted_tblA, "Age"), true);
%! assert (indexN, indexV);
%! [sorted_tblA, indexN] = sortrows (tblA, -1);
%! assert (issortedrows (sorted_tblA, -1), true);
%! [sorted_tblA, indexV] = sortrows (tblA, "Age", "descend");
%! assert (issortedrows (sorted_tblA, "Age", "descend"), true);
%! assert (indexN, indexV);
%! [sorted_tblA, indexN] = sortrows (tblA, -1, "ascend");
%! assert (issortedrows (sorted_tblA, -1, "ascend"), true);
%! [~, indexV] = sortrows (tblA, "Age");
%! assert (indexN, indexV);
%! [sorted_tblA, indexN] = sortrows (tblA, [-1, 2]);
%! assert (issortedrows (sorted_tblA, [-1, 2]), true);
%! [~, indexV] = sortrows (tblA, {"Age", "Height"}, {"descend", "ascend"});
%! assert (indexN, indexV);
%!test
%! TT = table (Age, table (Age, Height), BloodPressure, 'RowNames', LastName);
%! [sorted_TT, index] = sortrows (TT);
%! assert (index, [3; 1; 4; 2; 5]);
%! assert (issortedrows (sorted_TT), true);
%! [sorted_TT, index] = sortrows (TT, [1, 2]);
%! assert (index, [3; 1; 4; 2; 5]);
%! assert (issortedrows (sorted_TT, [1, 2]), true);
%! [sorted_TT, index] = sortrows (TT, [1, -2]);
%! assert (index, [1; 3; 4; 2; 5]);
%! assert (issortedrows (sorted_TT, [1, -2]), true);
%!test
%! Weights = [176;NaN;131;133;NaN];
%! tblN = table (Age, Height, Weights, BloodPressure, 'RowNames', LastName);
%! [tblB, index] = sortrows (tblN, "Weights", "MissingPlacement", "first");
%! assert (index, [2; 5; 3; 4; 1]);
%! assert (issortedrows (tblB, "Weights", "MissingPlacement", "first"), true);
%! [tblB, index] = sortrows (tblN, "Weights");
%! assert (index, [3; 4; 1; 2; 5]);
%! assert (issortedrows (tblB, "Weights"), true);
%!test
%! tblN = table (Age, Height, Weight, BloodPressure, LastName);
%! [tblB, index] = sortrows (tblN, vartype ("numeric"));
%! assert (index, [3; 1; 4; 2; 5]);
%! assert (issortedrows (tblB, vartype ("numeric")), true);
%! [tblB, index] = sortrows (tblN, vartype ("cellstr"));
%! assert (index, [5; 2; 4; 1; 3]);
%! assert (issortedrows (tblB, vartype ("cellstr")), true);

## Test 'unique' method
%!test
%! Name = {'Fred'; 'Betty'; 'Bob'; 'George'; 'Jane'};
%! Age = [38; 43; 38; 40; 38];
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 185; 131];
%! A = table (Age, Height, Weight, 'RowNames', Name);
%! [C, ia, ic] = unique (A);
%! assert (ia, [3; 1; 4; 2]);
%! assert (ic, [2; 4; 1; 3; 1]);
%! assert (C.Age, [38; 38; 40; 43]);
%! assert (C.Height, [64; 71; 67; 69]);
%! assert (C.Weight, [131; 176; 185; 163]);
%! assert (C.Properties.RowNames, {'Bob'; 'Fred'; 'George'; 'Betty'});
%!test
%! Name = {'Fred'; 'Betty'; 'Bob'; 'George'; 'Jane'};
%! Age = [38; 43; 38; 40; 38];
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 185; 131];
%! A = table (Age, Height, Weight, 'RowNames', Name);
%! [C, ia, ic] = unique (A, "sorted");
%! assert (ia, [3; 1; 4; 2]);
%! assert (ic, [2; 4; 1; 3; 1]);
%! assert (C.Age, [38; 38; 40; 43]);
%! assert (C.Height, [64; 71; 67; 69]);
%! assert (C.Weight, [131; 176; 185; 163]);
%! assert (C.Properties.RowNames, {'Bob'; 'Fred'; 'George'; 'Betty'});
%!test
%! Name = {'Fred'; 'Betty'; 'Bob'; 'George'; 'Jane'};
%! Age = [38; 43; 38; 40; 38];
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 185; 131];
%! A = table (Age, Height, Weight, 'RowNames', Name);
%! [C, ia, ic] = unique (A, "stable");
%! assert (ia, [1; 2; 3; 4]);
%! assert (ic, [1; 2; 3; 4; 3]);
%! assert (C.Age, [38; 43; 38; 40]);
%! assert (C.Height, [71; 69; 64; 67]);
%! assert (C.Weight, [176; 163; 131; 185]);
%! assert (C.Properties.RowNames, {'Fred'; 'Betty'; 'Bob'; 'George'});
%!test
%! Name = {'Fred'; 'Betty'; 'Bob'; 'George'; 'Jane'};
%! Age = [38; 43; 38; 40; 38];
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 185; 131];
%! A = table (Age, Height, Weight, 'RowNames', Name);
%! [C, ia, ic] = unique (A, "first");
%! assert (ia, [3; 1; 4; 2]);
%! assert (ic, [2; 4; 1; 3; 1]);
%! assert (C.Age, [38; 38; 40; 43]);
%! assert (C.Height, [64; 71; 67; 69]);
%! assert (C.Weight, [131; 176; 185; 163]);
%! assert (C.Properties.RowNames, {'Bob'; 'Fred'; 'George'; 'Betty'});
%!test
%! Name = {'Fred'; 'Betty'; 'Bob'; 'George'; 'Jane'};
%! Age = [38; 43; 38; 40; 38];
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 185; 131];
%! A = table (Age, Height, Weight, 'RowNames', Name);
%! [C, ia, ic] = unique (A, "last");
%! assert (ia, [5; 1; 4; 2]);
%! assert (ic, [2; 4; 1; 3; 1]);
%! assert (C.Age, [38; 38; 40; 43]);
%! assert (C.Height, [64; 71; 67; 69]);
%! assert (C.Weight, [131; 176; 185; 163]);
%! assert (C.Properties.RowNames, {'Jane'; 'Fred'; 'George'; 'Betty'});

## Test 'topkrows' method
%!test
%! LastName = {"Sanchez"; "Johnson"; "Li"; "Diaz"; "Brown"};
%! Age = [38; 43; 38; 40; 49];
%! Smoker = logical ([1; 0; 1; 0; 1]);
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 133; 119];
%! BloodPressure = [124, 93; 109, 77; 125, 83; 117, 75; 122, 80];
%! tblA = table (Age, Height, Weight, BloodPressure, 'RowNames', LastName);
%! [sorted_tblA, index] = topkrows (tblA, 3, "RowNames");
%! assert (index, [5; 4; 2]);
%! assert (issortedrows (sorted_tblA, "RowNames"), true);
%! [sorted_tblA, index] = topkrows (tblA, 4, "RowNames", "descend");
%! assert (index, [1; 3; 2; 4]);
%! assert (issortedrows (sorted_tblA, "RowNames", "descend"), true);
%!test
%! tblA.Properties.DimensionNames(1) = "Patients";
%! assert (issortedrows (tblA, "Patients"), false);
%! [sorted_tblA, index] = topkrows (tblA, 2, "Patients");
%! assert (index, [5; 4]);
%! assert (issortedrows (sorted_tblA, "Patients"), true);
%! [sorted_tblA, index] = topkrows (tblA, 5, "Patients", "descend");
%! assert (index, [1; 3; 2; 4; 5]);
%! assert (issortedrows (sorted_tblA, "Patients", "descend"), true);
%!test
%! [sorted_tblA, index] = topkrows (tblA, 3);
%! assert (index, [3; 1; 4]);
%! assert (issortedrows (sorted_tblA), true);
%! [sorted_tblA, index] = topkrows (tblA, 3, ":", "ascend");
%! assert (index, [3; 1; 4]);
%! assert (issortedrows (sorted_tblA, [], "ascend"), true);
%! [sorted_tblA, index] = topkrows (tblA, 3, [], "descend");
%! assert (index, [5; 2; 4]);
%! assert (issortedrows (sorted_tblA, [], "descend"), true);
%!test
%! [tblB, index] = topkrows (tblA, 3, "Height");
%! assert (index, [3; 5; 4]);
%! assert (tblB.Properties.RowNames, {"Li"; "Brown"; "Diaz"});
%! assert (issortedrows (tblB, "Height"), true);
%!test
%! LastName = {"Sweet"; "Jacobson"; "Wang"; "Joiner"; "Berger"};
%! tblA = table (Age, Height, Weight, BloodPressure, 'RowNames', LastName);
%! [tblB, index] = topkrows (tblA, 2, {"Height", "Weight"}, ...
%!                                    {"ascend", "descend"});
%! assert (index, [3; 5]);
%! assert (tblB.Properties.RowNames, {"Wang"; "Berger"});
%! assert (issortedrows (tblB, {"Height", "Weight"}, ...
%!                             {"ascend", "descend"}), true);
%!test
%! [tblB, index] = topkrows (tblA, 4, {"BloodPressure"}, {"ascend"});
%! assert (index, [2; 4; 5; 1]);
%! assert (issortedrows (tblB, {"BloodPressure"}, {"ascend"}), true);
%! [tblB, index] = topkrows (tblA, 3, {"BloodPressure"}, {"descend"});
%! assert (index, [3; 1; 5]);
%! assert (issortedrows (tblB, "BloodPressure", "descend"), true);
%!test
%! [tblB, index] = topkrows (tblA, 3, {"Height", "RowNames"}, {"ascend"});
%! assert (index, [5; 3; 4]);
%! assert (tblB.Properties.RowNames, {"Berger"; "Wang"; "Joiner"})
%! assert (issortedrows (tblB, {"Height", "RowNames"}, {"ascend"}), true);
%! [tblB, index] = topkrows (tblA, 4, {"Height", "RowNames"}, ...
%!                                    {"ascend", "descend"});
%! assert (index, [3; 5; 4; 2]);
%! assert (tblB.Properties.RowNames, {"Wang"; "Berger"; "Joiner"; "Jacobson"});
%! assert (issortedrows (tblB, {"Height", "RowNames"}, ...
%!                             {"ascend", "descend"}), true);
%!test
%! [tblB, index] = topkrows (tblA, 3, [true, true, false, false]);
%! assert (index, [3; 1; 4]);
%! assert (issortedrows (tblB, [true, true, false, false]), true);
%! [tblB, index] = topkrows (tblA, 2, [true, true, false, false], "ascend");
%! assert (index, [3; 1]);
%! assert (issortedrows (tblB, [true, true, false, false], "ascend"), true);
%! [tblB, index] = topkrows (tblA, 5, [true, true, false, false], "descend");
%! assert (index, flip ([3; 1; 4; 2; 5]));
%! assert (issortedrows (tblB, [true, true, false, false], "descend"), true);
%! [tblB, index] = topkrows (tblA, 6, [true, true, false, false], ...
%!                                    {"ascend", "descend"});
%! assert (index, [1; 3; 4; 2; 5]);
%! assert (issortedrows (tblB, [true, true, false, false], ...
%!                             {"ascend", "descend"}), true);
%!test
%! [sorted_tblA, indexN] = topkrows (tblA, 2, 1);
%! assert (issortedrows (sorted_tblA, 1), true);
%! [sorted_tblA, indexV] = topkrows (tblA, 2, "Age");
%! assert (issortedrows (sorted_tblA, "Age"), true);
%! assert (indexN, indexV);
%! [sorted_tblA, indexN] = topkrows (tblA, 3, -1);
%! assert (issortedrows (sorted_tblA, -1), true);
%! [sorted_tblA, indexV] = topkrows (tblA, 3, "Age", "descend");
%! assert (issortedrows (sorted_tblA, "Age", "descend"), true);
%! assert (indexN, indexV);
%! [sorted_tblA, indexN] = topkrows (tblA, 5, -1, "ascend");
%! assert (issortedrows (sorted_tblA, -1, "ascend"), true);
%! [~, indexV] = topkrows (tblA, 5, "Age");
%! assert (indexN, indexV);
%! [sorted_tblA, indexN] = topkrows (tblA, 3, [-1, 2]);
%! assert (issortedrows (sorted_tblA, [-1, 2]), true);
%! [~, indexV] = topkrows (tblA, 3, {"Age", "Height"}, {"descend", "ascend"});
%! assert (indexN, indexV);
%!test
%! TT = table (Age, table (Age, Height), BloodPressure, 'RowNames', LastName);
%! [sorted_TT, index] = topkrows (TT, 4);
%! assert (index, [3; 1; 4; 2]);
%! assert (issortedrows (sorted_TT), true);
%! [sorted_TT, index] = topkrows (TT, 3, [1, 2]);
%! assert (index, [3; 1; 4]);
%! assert (issortedrows (sorted_TT, [1, 2]), true);
%! [sorted_TT, index] = topkrows (TT, 2, [1, -2]);
%! assert (index, [1; 3]);
%! assert (issortedrows (sorted_TT, [1, -2]), true);
%!test
%! Weights = [176;NaN;131;133;NaN];
%! tblN = table (Age, Height, Weights, BloodPressure, 'RowNames', LastName);
%! [tblB, index] = topkrows (tblN, 4, "Weights", "MissingPlacement", "first");
%! assert (index, [2; 5; 3; 4]);
%! assert (issortedrows (tblB, "Weights", "MissingPlacement", "first"), true);
%! [tblB, index] = topkrows (tblN, 3, "Weights");
%! assert (index, [3; 4; 1]);
%! assert (issortedrows (tblB, "Weights"), true);
%!test
%! tblN = table (Age, Height, Weight, BloodPressure, LastName);
%! [tblB, index] = topkrows (tblN, 3, vartype ("numeric"));
%! assert (index, [3; 1; 4]);
%! assert (issortedrows (tblB, vartype ("numeric")), true);
%! [tblB, index] = topkrows (tblN, 3, vartype ("cellstr"));
%! assert (index, [5; 2; 4]);
%! assert (issortedrows (tblB, vartype ("cellstr")), true);

## Test 'addvars' method
%!test
%! load patients
%! T1 = table (Age, Height, Weight);
%! T2 = addvars (T1, ones (size (Age)));
%! assert (size (head (T2, 3)), [3, 4]);
%! assert (T2.Properties.VariableNames, {'Age', 'Height', 'Weight', 'Var4'});
%!test
%! load patients
%! T1 = table (LastName, Age, Height, Weight);
%! assert (size (head (T1, 3)), [3, 4]);
%! assert (T1.Properties.VariableNames, {'LastName', 'Age', 'Height', 'Weight'});
%! T2 = addvars (T1, Gender, Smoker);
%! assert (size (head (T2, 3)), [3, 6]);
%! assert (T2.Properties.VariableNames, ...
%!         {'LastName', 'Age', 'Height', 'Weight', 'Gender', 'Smoker'});
%!test
%! load patients
%! T1 = table (LastName, Gender);
%! assert (size (head (T1, 3)), [3, 2]);
%! assert (T1.Properties.VariableNames, {'LastName', 'Gender'});
%! T2 = addvars (T1, Age, 'Before', 'Gender');
%! assert (size (head (T2, 5)), [5, 3]);
%! assert (T2.Properties.VariableNames, {'LastName', 'Age', 'Gender'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender);
%! assert (size (head (T1, 4)), [4, 3]);
%! assert (T1.Properties.VariableNames, {'LastName', 'Age', 'Gender'});
%! T2 = addvars (T1, Height, Weight, 'After', 'Age');
%! assert (size (head (T2, 4)), [4, 5]);
%! assert (T2.Properties.VariableNames, ...
%!         {'LastName', 'Age', 'Height', 'Weight', 'Gender'});
%!test
%! load patients
%! T1 = table (LastName, Age, Height, Weight, Gender);
%! assert (size (head (T1, 3)), [3, 5]);
%! assert (T1.Properties.VariableNames, ...
%!         {'LastName', 'Age', 'Height', 'Weight', 'Gender'});
%! T2 = addvars (T1, Smoker, 'After', 1);
%! assert (size (head (T2, 3)), [3, 6]);
%! assert (T2.Properties.VariableNames, ...
%!         {'LastName', 'Smoker', 'Age', 'Height', 'Weight', 'Gender'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! assert (size (head (T1, 3)), [3, 4]);
%! assert (T1.Properties.VariableNames, {'LastName', 'Age', 'Gender', 'Smoker'});
%! T2 = addvars (T1, [Diastolic Systolic], 'NewVariableNames', 'BloodPressure');
%! assert (size (head (T2, 3)), [3, 5]);
%! assert (T2.Properties.VariableNames, ...
%!         {'LastName', 'Age', 'Gender', 'Smoker', 'BloodPressure'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! assert (size (head (T1, 3)), [3, 4]);
%! assert (T1.Properties.VariableNames, {'LastName', 'Age', 'Gender', 'Smoker'});
%! T2 = addvars (T1, Height, Weight, 'Before', 'Smoker', ...
%!                                   'NewVariableNames', {'Inches', 'Pounds'});
%! assert (size (head (T2, 3)), [3, 6]);
%! assert (T2.Properties.VariableNames, ...
%!         {'LastName', 'Age', 'Gender', 'Inches', 'Pounds', 'Smoker'});

## Test 'rename' method
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! assert (T1.Properties.VariableNames, {'LastName', 'Age', 'Gender', 'Smoker'});
%! T2 = renamevars (T1, {'LastName', 'Gender'}, {'Name', 'Sex'});
%! assert (T2.Properties.VariableNames, {'Name', 'Age', 'Sex', 'Smoker'});
%!test
%! T1 = array2table (rand (5, 3));
%! assert (T1.Properties.VariableNames, {'Var1', 'Var2', 'Var3'});
%! allVars = 1:width (T1);
%! newNames = cellfun (@(x) sprintf ("Rand%i",x), num2cell (allVars), ...
%!                     "UniformOutput", false);
%! T2 = renamevars (T1, allVars, newNames);
%! assert (T2.Properties.VariableNames, {'Rand1', 'Rand2', 'Rand3'});

## Test 'movevars' method
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! assert (T1.Properties.VariableNames, {'LastName', 'Age', 'Gender', 'Smoker'});
%! T2 = movevars (T1, {'LastName', 'Gender'});
%! assert (T2.Properties.VariableNames, {'Age', 'Smoker', 'LastName', 'Gender'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = movevars (T1, vartype ("cellstr"));
%! assert (T2.Properties.VariableNames, {'Age', 'Smoker', 'LastName', 'Gender'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = movevars (T1, vartype ("numeric"));
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', 'Smoker', 'Age'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = movevars (T1, {'LastName', 'Gender'}, 'After', 'Age');
%! assert (T2.Properties.VariableNames, {'Age', 'LastName', 'Gender', 'Smoker'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = movevars (T1, [true, false, true, false], 'After', 'Age');
%! assert (T2.Properties.VariableNames, {'Age', 'LastName', 'Gender', 'Smoker'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = movevars (T1, [3, 4], 'Before', 'Age');
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', 'Smoker', 'Age'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = movevars (T1, [3, 4], 'Before', 2);
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', 'Smoker', 'Age'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = movevars (T1, [3, 4], 'Before', [false, true, false, false]);
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', 'Smoker', 'Age'});

## Test 'removevars' method
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = removevars (T1, {'LastName', 'Gender'});
%! assert (T2.Properties.VariableNames, {'Age', 'Smoker'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = removevars (T1, [true, false, true, false]);
%! assert (T2.Properties.VariableNames, {'Age', 'Smoker'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = removevars (T1, [1, 3]);
%! assert (T2.Properties.VariableNames, {'Age', 'Smoker'});
%!test
%! load patients
%! T1 = table (LastName, Age, Gender, Smoker);
%! T2 = removevars (T1, vartype ("numeric"));
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', 'Smoker'});

## Test 'splitvars' method
%!test
%! A = [1:3]';
%! B = rand (3);
%! C = {'a', 'XX'; 'b', 'YY'; 'c', 'ZZ'};
%! D = {"A"; "B"; "C"};
%! T1 = table (A, B, C, D);
%! T2 = splitvars (T1);
%! assert (T2.Properties.VariableNames, ...
%!         {'A', 'B_1', 'B_2', 'B_3', 'C_1', 'C_2', 'D'});
%!test
%! A = [1:3]';
%! B = rand (3);
%! C = {'a', 'XX'; 'b', 'YY'; 'c', 'ZZ'};
%! D = {"A"; "B"; "C"};
%! T1 = table (A, B, C, D);
%! T2 = addvars (T1, table ([5; 5; 5]));
%! assert (size (T2.Var5), [3, 1]);
%! T3 = splitvars (T2);
%! assert (T3.Properties.VariableNames, ...
%!         {'A', 'B_1', 'B_2', 'B_3', 'C_1', 'C_2', 'D', 'Var1'});
%! assert (size (T3.Var1), [3, 1]);
%!test
%! A = [1:3]';
%! B = rand (3);
%! C = {'a', 'XX'; 'b', 'YY'; 'c', 'ZZ'};
%! D = {"A"; "B"; "C"};
%! T1 = table (A, B, C, D);
%! T2 = addvars (T1, table ([5, 6; 5, 6; 5, 6]));
%! assert (size (T2.Var5), [3, 1]);
%! T3 = splitvars (T2);
%! assert (T3.Properties.VariableNames, ...
%!         {'A', 'B_1', 'B_2', 'B_3', 'C_1', 'C_2', 'D', 'Var1'});
%! assert (size (T3.Var1), [3, 2]);
%!test
%! A = [1:3]';
%! B = rand (3);
%! C = {'a', 'XX'; 'b', 'YY'; 'c', 'ZZ'};
%! D = {"A"; "B"; "C"};
%! T1 = table (A, B, C, D);
%! A = [5; 5; 5];
%! T2 = addvars (T1, table (A));
%! T3 = splitvars (T2);
%! assert (T3.Properties.VariableNames, ...
%!         {'A', 'B_1', 'B_2', 'B_3', 'C_1', 'C_2', 'D', 'Var5_A'});
%!test
%! A = [1:3]';
%! B = rand (3);
%! C = {'a', 'XX'; 'b', 'YY'; 'c', 'ZZ'};
%! D = {"A"; "B"; "C"};
%! T1 = table (A, B, C, D);
%! A = [5; 5; 5];
%! T2 = addvars (T1, table ([5, 6; 5, 6; 5, 6]), table (A));
%! T3 = splitvars (T2);
%! assert (T3.Properties.VariableNames, ...
%!         {'A', 'B_1', 'B_2', 'B_3', 'C_1', 'C_2', 'D', 'Var1', 'Var6_A'});
%!test
%! load patients
%! Personal_Data = table (Gender, Age);
%! BMI_Data = table (Height, Weight);
%! BloodPressure = table (Systolic, Diastolic);
%! T1 = table (LastName, Personal_Data, BMI_Data, BloodPressure);
%! T2 = splitvars (T1, 'BloodPressure');
%! assert (T2.Properties.VariableNames, ...
%!         {'LastName', 'Personal_Data', 'BMI_Data', 'Systolic', 'Diastolic'});
%!test
%! load patients
%! Personal_Data = table (Gender, Age);
%! BMI_Data = table (Height, Weight);
%! BloodPressure = table (Systolic, Diastolic);
%! T1 = table (LastName, Personal_Data, BMI_Data, BloodPressure);
%! T2 = splitvars (T1, {'BMI_Data', 'BloodPressure'});
%! assert (T2.Properties.VariableNames, {'LastName', 'Personal_Data', ...
%!         'Height', 'Weight', 'Systolic', 'Diastolic'});
%!test
%! load patients
%! Personal_Data = table (Gender, Age);
%! BMI_Data = table (Height, Weight);
%! BloodPressure = table (Systolic, Diastolic);
%! T1 = table (LastName, Personal_Data, BMI_Data, BloodPressure);
%! T2 = splitvars (T1, [2, 4]);
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', 'Age', ...
%!                                       'BMI_Data', 'Systolic', 'Diastolic'});
%!test
%! load patients
%! Personal_Data = table (Gender, Age);
%! BMI_Data = table (Height, Weight);
%! BloodPressure = table (Systolic, Diastolic);
%! T1 = table (LastName, Personal_Data, BMI_Data, BloodPressure);
%! T2 = splitvars (T1, [false, true, false, true]);
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', 'Age', ...
%!                                       'BMI_Data', 'Systolic', 'Diastolic'});
%!test
%! load patients
%! Personal_Data = [Age, Height, Weight];
%! BloodPressure = [Systolic, Diastolic];
%! T1 = table (LastName, Gender, Personal_Data, BloodPressure);
%! T2 = splitvars (T1, 'BloodPressure', ...
%!                 'NewVariableNames', {'Systolic','Diastolic'});
%! assert (T2.Properties.VariableNames, ...
%!         {'LastName', 'Gender', 'Personal_Data', 'Systolic', 'Diastolic'});
%!test
%! load patients
%! Personal_Data = [Age, Height, Weight];
%! BloodPressure = [Systolic, Diastolic];
%! T1 = table (LastName, Gender, Personal_Data, BloodPressure);
%! T2 = splitvars (T1, {'Personal_Data', 'BloodPressure'}, ...
%!                 'NewVariableNames', {{'Age', 'Height', 'Weight'}, ...
%!                                      {'Systolic', 'Diastolic'}});
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', ...
%!         'Age', 'Height', 'Weight', 'Systolic', 'Diastolic'});
%!test
%! load patients
%! Personal_Data = [Age, Height, Weight];
%! BloodPressure = [Systolic, Diastolic];
%! T1 = table (LastName, Gender, Personal_Data, BloodPressure);
%! new_PD = string ({'Age', 'Height', 'Weight'});
%! T2 = splitvars (T1, {'Personal_Data', 'BloodPressure'}, ...
%!                 'NewVariableNames', {new_PD, {'Systolic', 'Diastolic'}});
%! assert (T2.Properties.VariableNames, {'LastName', 'Gender', ...
%!         'Age', 'Height', 'Weight', 'Systolic', 'Diastolic'});

## Test 'mergevars' method
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, [2, 3]);
%! assert (T2.Properties.VariableNames, {'A', 'Var2', 'D'});
%! assert (isequal (T2.Var2, cat (2, B, C)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, [2, 3], 'NewVariableName', 'Merged');
%! assert (T2.Properties.VariableNames, {'A', 'Merged', 'D'});
%! assert (isequal (T2.Merged, cat (2, B, C)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, [2, 3], 'MergeAsTable', true);
%! assert (T2.Properties.VariableNames, {'A', 'Var2', 'D'});
%! assert (isequal (T2.Var2, table (B, C)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, [2, 3], 'NewVariableName', 'Merged', 'MergeAsTable', 1);
%! assert (T2.Properties.VariableNames, {'A', 'Merged', 'D'});
%! assert (isequal (T2.Merged, table (B, C)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, {"A", "B"});
%! assert (T2.Properties.VariableNames, {'Var1', 'C', 'D'});
%! assert (isequal (T2.Var1, cat (2, A, B)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, {"A", "B"}, 'NewVariableName', 'Merged');
%! assert (T2.Properties.VariableNames, {'Merged', 'C', 'D'});
%! assert (isequal (T2.Merged, cat (2, A, B)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, {"A", "B"}, 'MergeAsTable', true);
%! assert (T2.Properties.VariableNames, {'Var1', 'C', 'D'});
%! assert (isequal (T2.Var1, table (A, B)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! T2 = mergevars (T1, {"A", "B"}, 'NewVariableName', 'Merged', ...
%!                                 'MergeAsTable', true);
%! assert (T2.Properties.VariableNames, {'Merged', 'C', 'D'});
%! assert (isequal (T2.Merged, table (A, B)), true);

## Test 'convertvars' method
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! assert (isa (T1.A, "numeric"), true);
%! assert (isa (T1.B, "numeric"), true);
%! T2 = convertvars (T1, {"A", "B"}, "string");
%! assert (isa (T2.A, "string"), true);
%! assert (isa (T2.B, "string"), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! assert (isa (T1.D, "string"), true);
%! T2 = convertvars (T1, 4, "cellstr");
%! assert (iscellstr (T2.D), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! assert (isa (T1.D, "string"), true);
%! T2 = convertvars (T1, [1:3], @(x) sqrt (x));
%! assert (isequal (T2.A, sqrt (A)), true);
%! assert (isequal (T2.B, sqrt (B)), true);
%! assert (isequal (T2.C, sqrt (C)), true);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = string ({"a"; "b"; "c"});
%! T1 = table (A, B, C, D);
%! assert (isa (T1.D, "string"), true);
%! T2 = convertvars (T1, [1:3], @(x) [x, x, x]);
%! assert (size (T2.A, 2), 3);
%! assert (size (T2.B, 2), 3);
%! assert (size (T2.C, 2), 3);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = {"a"; "b"; "c"};
%! T1 = table (A, B, C, D);
%! T2 = convertvars (T1, "D", @(x) cellstr (upper (char (x))));
%! assert (T2.D, {"A"; "B"; "C"});

## Test 'rows2vars' method
%!test
%! load patients
%! T1 = table (LastName, Gender, Age, Height, Weight);
%! T2 = rows2vars (T1);
%! assert (size (T1, 2), size (T2, 1));
%! assert (size (T1, 1) + 1, size (T2, 2));
%! assert (T2.OriginalVariableNames, ...
%!         {"LastName"; "Gender"; "Age"; "Height"; "Weight"});
%! assert (T2.Var1, {"Smith"; "Male"; 38; 71; 176});
%!test
%! load patients
%! T1 = table (Gender, Age, Height, Weight, "RowNames", LastName);
%! T2 = rows2vars (T1);
%! assert (T2.OriginalVariableNames, {"Gender"; "Age"; "Height"; "Weight"});
%! assert (T2.Smith, {"Male"; 38; 71; 176});
%! assert (T2.Johnson, {"Male"; 43; 69; 163});
%! assert (T2.Williams, {"Female"; 38; 64; 131});
%!test
%! load patients
%! T1 = table (LastName, Gender, Age, Height, Weight);
%! T2 = rows2vars (T1, "VariableNamesSource", "LastName");
%! assert (T2.OriginalVariableNames, {"Gender"; "Age"; "Height"; "Weight"});
%! assert (T2.Smith, {"Male"; 38; 71; 176});
%! assert (T2.Johnson, {"Male"; 43; 69; 163});
%! assert (T2.Williams, {"Female"; 38; 64; 131})
%!test
%! load patients
%! T1 = table (LastName, Gender, Age, Height, Weight);
%! T2 = rows2vars (T1, "DataVariables", {"LastName", "Gender", "Age"});
%! assert (size (T2, 1), 3);
%! assert (size (T1, 1) + 1, size (T2, 2));
%! assert (T2.OriginalVariableNames, {"LastName"; "Gender"; "Age"});
%! assert (T2.Var1, {"Smith"; "Male"; 38});
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! T1 = table (A, B, C);
%! T2 = rows2vars (T1);
%! assert (class (T2.Var1), "double");
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! D = {"a"; "b"; "c"};
%! T1 = table (A, B, C, D);
%! T2 = rows2vars (T1);
%! assert (class (T2.Var1), "cell");
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! T1 = table (A, B, C);
%! T2 = rows2vars (T1, "VariableNamesSource", "A");
%! assert (size (T2), [2, 4]);
%! assert (T2.Properties.VariableNames(1), {"OriginalVariableNames"});
%! assert (T2.Properties.VariableNames(2:4), {"x1", "x2", "x3"});
%! assert (T2.x1, [5; 3.14]);
%!test
%! A = [1:3]';
%! B = [5; 11; 12];
%! C = [3.14; 2.72; 1.37];
%! T1 = table (A, B, C);
%! T2 = rows2vars (T1, "VariableNamesSource", "A", ...
%!                     "VariableNamingRule", "preserve");
%! assert (size (T2), [2, 4]);
%! assert (T2.Properties.VariableNames(1), {"OriginalVariableNames"});
%! assert (T2.Properties.VariableNames(2:4), {"1", "2", "3"});
%! assert (T2.('1'), [5; 3.14]);

## Test 'stack' method
%!test
%! Test1 = [93; 57; 87; 89];
%! Test2 = [89; 77; 92; 86];
%! Test3 = [95; 62; 89; 91];
%! U = table (Test1, Test2, Test3);
%! S = stack (U, 1:3);
%! assert (size (S), [12, 2]);
%! assert (class (S.Test1_Test2_Test3_Indicator), "categorical");
%! assert (cellstr (S.Test1_Test2_Test3_Indicator), ...
%!         repmat ({"Test1"; "Test2"; "Test3"}, 4, 1));
%! assert (S.Test1_Test2_Test3, [Test1';Test2';Test3'](:));
%!test
%! Test1 = [93; 57; 87; 89];
%! Test2 = [89; 77; 92; 86];
%! Test3 = [95; 62; 89; 91];
%! U = table (Test1, Test2, Test3);
%! [S, idx] = stack (U, 1:2);
%! assert (idx, [1; 1; 2; 2; 3; 3; 4; 4]);
%! assert (cellstr (S.Test1_Test2_Indicator), ...
%!         repmat ({"Test1"; "Test2"}, 4, 1));
%! assert (S.Test3, repelem (Test3, 2, 1));
%!test
%! Test1 = [93; 57; 87; 89];
%! Test2 = [89; 77; 92; 86];
%! Test3 = [95; 62; 89; 91];
%! U = table (Test1, Test2, Test3);
%! S = stack (U, 1:3, 'NewDataVariableName', 'NewData', ...
%!                    'IndexVariableName', 'Index');
%! assert (size (S), [12, 2]);
%! assert (class (S.Index), "categorical");
%! assert (cellstr (S.Index), repmat ({"Test1"; "Test2"; "Test3"}, 4, 1));
%! assert (S.NewData, [Test1';Test2';Test3'](:));
%!test
%! Test1 = [93; 57; 87; 89];
%! Test2 = [89; 77; 92; 86];
%! Test3 = [95; 62; 89; 91];
%! Test4 = [95; 62; 89; 91];
%! U = table (Test1, Test2, Test3, Test4);
%! S = stack (U, 1:2, 'ConstantVariables', "Test4");
%! assert (S.Properties.VariableNames, ...
%!         {"Test4", "Test1_Test2_Indicator", "Test1_Test2"});
%!test
%! Test1 = [93; 57; 87; 89];
%! Test2 = [89; 77; 92; 86];
%! Test3 = [95; 62; 89; 91];
%! Test4 = [95; 62; 89; 91];
%! U = table (Test1, Test2, Test3, Test4);
%! S = stack (U, {"Test1", "Test4"}, 'ConstantVariables', "Test2");
%! assert (S.Properties.VariableNames, ...
%!         {"Test2", "Test1_Test4_Indicator", "Test1_Test4"});
%!test
%! Test1 = [93; 57; 87; 89];
%! Test2 = [89; 77; 92; 86];
%! Test3 = [95; 62; 89; 91];
%! Test4 = [95; 62; 89; 91];
%! U = table (Test1, Test2, Test3, Test4);
%! S = stack (U, {"Test1", "Test4"}, 'ConstantVariables', "Test2", ...
%!            'NewDataVariableName', 'NewData', 'IndexVariableName', 'Index');
%! assert (S.Properties.VariableNames, ...
%!         {"Test2", "Index", "NewData"});

## Test 'unstack' method
%! Storm = [3; 3; 1; 3; 1; 1; 4; 2; 4; 2; 4; 2];
%! Town = categorical ({'Natick'; 'Worcester'; 'Natick'; 'Boston'; ...
%!                      'Boston'; 'Worcester'; 'Boston'; 'Natick'; ...
%!                      'Worcester'; 'Worcester'; 'Natick'; 'Boston'});
%! Snowfall = [0; 3; 5; 5; 9; 10; 12; 13; 15; 16; 17; 21];
%! S = table (Storm, Town, Snowfall);
%! U = unstack (S, 'Snowfall', 'Town');
%! assert (U.Storm, [3; 1; 4; 2]);
%! assert (U.Properties.VariableNames, {'Storm', 'Boston', 'Natick', 'Worcester'});





## Reinitialize variables before testing input validation
%! LastName = {"Sanchez"; "Johnson"; "Li"; "Diaz"; "Brown"};
%! Age = [38; 43; 38; 40; 49];
%! Smoker = logical ([1; 0; 1; 0; 1]);
%! Height = [71; 69; 64; 67; 64];
%! Weight = [176; 163; 131; 133; 119];
%! BloodPressure = [124, 93; 109, 77; 125, 83; 117, 75; 122, 80];
%! tblA = table (Age, Height, Weight, BloodPressure, 'RowNames', LastName);
%! tblB = table (LastName, Age, Height, Weight, BloodPressure);

## Test input validation for constructor
%!error <table: 'VariableNames' must be either a cell array of character vectors or a string array.> ...
%! table (Age, Smoker, "VariableNames", {2, 3});
%!error <table: 'VariableNames' must be either a cell array of character vectors or a string array.> ...
%! table (Age, Smoker, "VariableNames", [2, 3]);
%!error <table: 'VariableNames' must be either a cell array of character vectors or a string array.> ...
%! table (Age, Smoker, "VariableNames", [true, true]);
%!error <table: 'RowNames' must be either a cell array of character vectors or a string array.> ...
%! table (Age, Smoker, "RowNames", {2, 3});
%!error <table: 'RowNames' must be either a cell array of character vectors or a string array.> ...
%! table (Age, Smoker, "RowNames", [2, 3]);
%!error <table: 'RowNames' must be either a cell array of character vectors or a string array.> ...
%! table (Age, Smoker, "RowNames", [true, false]);
%!error <table: 'DimensionNames' must be either a two-element cell array of character vectors or a two-element string array.> ...
%! table (Age, Smoker, "DimensionNames", {2, 3});
%!error <table: 'DimensionNames' must be either a two-element cell array of character vectors or a two-element string array.> ...
%! table (Age, Smoker, "DimensionNames", [2, 3]);
%!error <table: 'DimensionNames' must be either a two-element cell array of character vectors or a two-element string array.> ...
%! table (Age, Smoker, "DimensionNames", [true, false]);
%!error <table: 'DimensionNames' must be either a two-element cell array of character vectors or a two-element string array.> ...
%! table (Age, Smoker, "DimensionNames", {"A", "B", "C"});
%!error <table: duplicate dimension and variable name: 'Variables'> ...
%! table (1, 'VariableNames', {'Variables'});
%!error <table: 'VariableTypes' must be a cellstring array of the same number of elements as defined in SZ> ...
%! table ("Size", [4, 2], "VariableTypes", {"A", "B", "C"});
%!error <table: 'VariableTypes' must be a cellstring array of the same number of elements as defined in SZ> ...
%! table ("Size", [4, 3], "VariableTypes", ["A", "B", "C"]);
%!error <table: 'VariableTypes' must be a cellstring array of the same number of elements as defined in SZ> ...
%! table ("Size", [4, 3], "VariableTypes", {1, 2, 3});
%!error <table: inconsistent number of 'VariableNames' and 'VariableTypes'.> ...
%! table ("Size", [4, 3], "VariableTypes", {"double", "double", "logical"}, ...
%!        "VariableNames", {"A", "B"});
%!error <table: inconsistent number of 'RowNames' and rows defined in SZ.> ...
%! table ("Size", [4, 3], "VariableTypes", {"double", "double", "logical"}, ...
%!        "RowNames", {"A", "B", "C"});
%!error <table: 'timetable' variable type not supported yet.> ...
%! table ("Size", [4, 3], "VariableTypes", {"double", "double", "timetable"}, ...
%!        "VariableNames", {"A", "B", "C"});
%!error <table: 'timetable' variable type not supported yet.> ...
%! table ("Size", [4, 3], "VariableTypes", {"double", "double", "timetable"}, ...
%!        "RowNames", {"A", "B", "C", "D"});
%!error <table: 'timetable' variable type not supported yet.> ...
%! table ("Size", [4, 3], "VariableTypes", {"double", "double", "timetable"});
%!error <table: duplicate variable names: Age> table (Age, Age);
%!error <table: duplicate variable names: Age, Height> ...
%! table (Age, Age, Height, Height);
%!error <table: inconsistent number of variable names> ...
%! table (Age, Height, "VariableNames", {"A"})
%!error <table: variable values must not have more than 2 dimensions: input 1 'A' has 3.> ...
%! table (ones (5, 5, 5), Height, "VariableNames", {"A", "B"});
%!error <table: variable values must not have more than 2 dimensions: input 2 'B' has 3.> ...
%! table (Height, ones (5, 5, 5), "VariableNames", {"A", "B"});
%!error <table: inconsistent sizes between variables: var 'A' has 1 rows; var 'B' has 5 rows.> ...
%! table (struct ("age", Age), Height, "VariableNames", {"A", "B"});
%!error <table: inconsistent sizes between variables: var 'A' has 5 rows; var 'B' has 1 rows.> ...
%! table (Height, struct ("age", Age), "VariableNames", {"A", "B"});
%!error <table.table2array: table cannot be concatenated into a matrix due to incompatible variable types.> ...
%! table2array (table (Height, {1; 2; 3; 4; 5}, "VariableNames", {"A", "B"}));
%!error <table.table2struct: wrong number of input aguments.> ...
%! table2struct (T, "ToScalar");
%!error <table.table2struct: wrong optional input agument.> ...
%! table2struct (T, "Scalar", 1);
%!error <table: elements in 'RowNames' must be unique.> ...
%! table ("Size", [4, 2], "VariableTypes", {"double", "double"}, ...
%!        "RowNames", {"A", "B", "B", "D"});

## Test input validation for **summary information** methods
%!error <table.head: K must be a positive integer scalar value.> ...
%! head (T, 1.5);
%!error <table.head: K must be a positive integer scalar value.> ...
%! head (T, -2);
%!error <table.head: invalid number of output arguments.> ...
%! [out1, out2] = head (T, 2);
%!error <table.tail: K must be a positive integer scalar value.> ...
%! tail (T, 1.5);
%!error <table.tail: K must be a positive integer scalar value.> ...
%! tail (T, -2);
%!error <table.tail: invalid number of output arguments.> ...
%! [out1, out2] = tail (T, 2);

## Test input validation for 'sortrows' method
%!error <table.sortrows: 'MissingPlacement' parameter can be either 'auto', 'first', or 'last'.> ...
%! sortrows (tblA, "MissingPlacement", "param");
%!error <table.sortrows: 'ComparisonMethod' parameter can be either 'auto', 'real', or 'abs'.> ...
%! sortrows (tblA, "ComparisonMethod", "param");
%!error <table.sortrows: invalid number of input arguments.> ...
%! sortrows (tblA, [], "ascend", "param");
%!error <table.sortrows: invalid value for DIRECTION argument.> ...
%! sortrows (tblA, [], "aaascend");
%!error <table.sortrows: DIRECTION must be a scalar input when 'RowNames' or 'rowDimNames' are selected.> ...
%! sortrows (tblA, "RowNames", {"ascend", "ascend"});
%!error <table.sortrows: DIRECTION must be a scalar input when 'RowNames' or 'rowDimNames' are selected.> ...
%! sortrows (tblA, "Row", {"ascend", "ascend"});
%!error <table.sortrows: logical indexing vector does not match table width.> ...
%! sortrows (tblA, [true, true, false]);
%!error <table.sortrows: invalid size for DIRECTION argument.> ...
%! sortrows (tblA, "Age", {"ascend", "ascend"});
%!error <table.sortrows: numerical indexing must be a vector of nonzero integers.> ...
%! sortrows (tblA, [1, 0, 3]);
%!error <table.sortrows: numerical index exceeds table dimensions.> ...
%! sortrows (tblA, [1, 6, 3]);
%!error <table.sortrows: invalid size for DIRECTION argument.> ...
%! sortrows (tblA, [1, 3, 2], {"ascend", "ascend"});
%!error <table.sortrows: VARS indexes non-existing variable names.> ...
%! sortrows (tblA, {"Age", "whatever"});
%!error <table.sortrows: invalid size for DIRECTION argument.> ...
%! sortrows (tblA, {"Age", "Height", "Weight"}, {"ascend", "ascend"});
%!error <table.sortrows: DIRECTION must be a scalar input when variables are indexed with a 'vartype' object.> ...
%! sortrows (tblA, vartype ("numeric"), {"ascend", "ascend"});
%!error <table.sortrows: cannot sort variables of 'cell' type.> ...
%! sortrows (table ({5; 4; 3}));
%!error <table.sortrows: cannot sort variables of 'struct' type.> ...
%! sortrows (table ([struct("s", 4); struct("s", 5); struct("s", 6)]));
%!error <table.sortrows: cannot sort nested tables with mixed data types.> ...
%! sortrows (table (Age, table (Age, LastName), "RowNames", LastName))

## Test input validation for 'unique' method
%!error <table.unique: too many input arguments.> unique (tblA, 1, 2);
%!error <table.unique: invalid option 'some'.> unique (tblA, "some");
%!error <table.sortrows: cannot sort variables of 'cell' type.> ...
%! unique (table ({2; 2; 3}), "sorted");
%!error <table.sortrows: cannot sort variables of 'struct' type.> ...
%! unique (table (struct ("a", {2; 2; 3})), "sorted");
%!error <table.sortrows: cannot sort nested tables with mixed data types.> ...
%! unique (table (table (Age, LastName)), "sorted");

## Test input validation for 'topkrows' method
%!error <table.topkrows: K must be a nonnegative integer scalar.> ...
%! topkrows (tblA, -1)
%!error <table.topkrows: K must be a nonnegative integer scalar.> ...
%! topkrows (tblA, 2.5)

## Test input validation for 'addvars' method
%!error <table.addvars: cannot use both 'After' and 'Before' options.> ...
%! addvars (tblA, Smoker, 'After', 1, 'Before', 2);
%!error <table.addvars: LOCATION must index a single variable.> ...
%! addvars (tblA, Smoker, 'After', [true, false, false, true]);
%!error <table.addvars: LOCATION must be either a scalar integer, a character vector, or a logical vector indexing a single table variable.> ...
%! addvars (tblA, Smoker, 'After', [1, 2]);
%!error <table.addvars: NEWNAMES does not match the number of new variables.> ...
%! addvars (tblA, Smoker, 'NewVariableNames', {'A', 'B'});
%!error <table.addvars: NEWNAMES contains duplicate names.> ...
%! addvars (tblA, Smoker, Smoker, 'NewVariableNames', {'A', 'A'});
%!error <table.addvars: new variable name 'Height' already exists.> ...
%! addvars (tblA, Smoker, 'NewVariableNames', {'Height'});
%!error <table.addvars: new variable names 'Height', 'Weight' already exist.> ...
%! addvars (tblA, Smoker, Smoker, 'NewVariableNames', {'Height', 'Weight'});

## Test input validation for 'renamevars' method
%!error <table.renamevars: too few input arguments.> ...
%! renamevars (tblA, {"Age", "Smoker"});
%!error <table.renamevars: too few input arguments.> ...
%! renamevars (tblA, {"Age"}, {});
%!error <table.renamevars: too few input arguments.> ...
%! renamevars (tblA, {}, {"NewName"});
%!error <table.renamevars: NEWNAMES must be either a character vector, a cell array of character vectors, or a string array.> ...
%! renamevars (tblA, {"Age", "Smoker"}, {"Age", 4});
%!error <table.renamevars: NEWNAMES contains duplicate names.> ...
%! renamevars (tblA, {"Age", "Smoker"}, {"Age", "Age"});
%!error <table.renamevars: cannot index non-existing variable: 'Smoker'.> ...
%! renamevars (tblA, {"Age", "Smoker"}, {"Age", "User"});
%!error <table.renamevars: number of names in NEWNAMES do not match the selected variables specified by VARS.> ...
%! renamevars (tblA, {"Age", "Height"}, {"Age", "Smoker", "Height"});
%!error <table.renamevars: newly assigned variable name already exists.> ...
%! renamevars (tblA, {"Age", "Height"}, {"Age", "Weight"});

## Test input validation for 'movevars' method
%!error <table.movevars: too few input arguments.> movevars (tblA);
%!error <table.movevars: too few input arguments.> movevars (tblA, {});
%!error <table.movevars: cannot use both 'After' and 'Before' options.> ...
%! movevars (tblA, 1, "After", "Height", "Before", "Weight");
%!error <table.movevars: LOCATION must index a single variable.> ...
%! movevars (tblA, 1, "After", [true, true, false, false]);
%!error <table.movevars: LOCATION must be either a scalar integer, a character vector, or a logical vector indexing a single table variable.> ...
%! movevars (tblA, 1, "After", [1, 2]);
%!error <table.movevars: LOCATION must be either a scalar integer, a character vector, or a logical vector indexing a single table variable.> ...
%! movevars (tblA, 1, "After", {"Height"});
%!error <table.movevars: LOCATION does not index an existing variable.> ...
%! movevars (tblA, 1, "After", "Smoker");
%!error <table.movevars: cannot index non-existing variable: 'Smoker'.> ...
%! movevars (tblA, "Smoker", "After", 2);
%!error <table.movevars: LOCATION variable cannot be moved.> ...
%! movevars (tblA, {"Age", "Height"}, "After", 2);
%!error <table.movevars: LOCATION variable cannot be moved.> ...
%! movevars (tblA, {"Height", "Weight"}, "Before", "Height");

## Test input validation for 'removevars' method
%!error <table.removevars: too few input arguments.> removevars (tblA);
%!error <table: variable index must be a vector.> removevars (tblA, ones (2));
%!error <table: variable logical index does not match table width.> ...
%! removevars (tblA, [true, true, false]);
%!error <table: variable index out of bounds: requested index 5; table has 4 variables.> ...
%! removevars (tblA, [1, 5]);
%!error <table: variable index out of bounds: requested index 7; table has 4 variables.> ...
%! removevars (tblA, [1, 2, 7]);
%!error <table: no such variable in table: 'Smoker'.> ...
%! removevars (tblA, "Smoker");
%!error <table: no such variables in table: 'Smoker', 'Health'.> ...
%! removevars (tblA, {"Smoker", "Health"});
%!error <table: unsupported variable indexing operand type: 'cell'.> ...
%! removevars (tblA, {1});
%!error <table: unsupported variable indexing operand type: 'struct'.> ...
%! removevars (tblA, struct ("A", 1));

## Test input validation for 'splitvars' method
%!error <table.splitvars: too many input arguments.> splitvars (tblA, 1, 2, 3, 4);
%!error <table.splitvars: invalid input for 'NewVariableNames'.> ...
%! splitvars (table ([5, 6; 5, 6; 5, 6]), 1, 'NewVariableNames', {1})
%!error <table.splitvars: invalid input for 'NewVariableNames'.> ...
%! splitvars (table ([5, 6; 5, 6; 5, 6]), 1, 'NewVariableNames', 1)
%!error <table.splitvars: invalid input for 'NewVariableNames'.> ...
%! splitvars (table ([5, 6; 5, 6; 5, 6]), 1, 'NewVariableNames', "new_name")
%!error <table.splitvars: invalid input for 'NewVariableNames'.> ...
%! splitvars (table ([5, 6; 5, 6; 5, 6], ones (3, 2)), ":", ...
%!            'NewVariableNames', {{"A", "B"}, {"new_name", 1}})
%!error <table.splitvars: wrong number of 'NewVariableNames'.> ...
%! splitvars (table ([5, 6; 5, 6; 5, 6], ones (3, 2)), ":", ...
%!            'NewVariableNames', {{"A", "B"}, {"C", "D", "E"}})

## Test input validation for 'mergevars' method
%!error <table.mergevars: too few input arguments.> mergevars (tblA);
%!error <table.mergevars: invalid input for 'MergeAsTable'.> ...
%! mergevars (tblA, {'Height', 'Weight'}, 'MergeAsTable', 'on');
%!error <table.mergevars: invalid input for 'MergeAsTable'.> ...
%! mergevars (tblA, {'Height', 'Weight'}, 'MergeAsTable', 2);
%!error <table.mergevars: invalid input for 'MergeAsTable'.> ...
%! mergevars (tblA, {'Height', 'Weight'}, 'MergeAsTable', [true, true]);
%!error <table.mergevars: invalid input for 'NewVariableName'.> ...
%! mergevars (tblA, {'Height', 'Weight'}, 'NewVariableName', 2)
%!error <table.mergevars: assigned 'NewVariableName' already exists.> ...
%! mergevars (tblA, {'Height', 'Weight'}, 'NewVariableName', 'Age');
%!error <table.mergevars: selected variables cannot be merged into a multicolumn variable due to incompatible variable types.> ...
%! mergevars (addvars (tblA, LastName), {'Height', 'LastName'});

## Test input validation for 'convertvars' method
%!error <table.mergevars: too few input arguments.> convertvars (tblA);
%!error <table.mergevars: too few input arguments.> ...
%! convertvars (tblA, "Height");
%!error <table.convertvars: DATATYPE must be a character vector.> ...
%! convertvars (tblA, "Height", ["c","h";"a","r"]);
%!error <table.convertvars: DATATYPE must be either a character vector or a function handle; got a 'cell'.> ...
%! convertvars (tblA, "Height", {"char"});
%!error <table.convertvars: DATATYPE must be either a character vector or a function handle; got a 'double'.> ...
%! convertvars (tblA, "Height", 25);
%!error <table.convertvars: specified DATATYPE conversion cannot be applied on selected variable 'Height'.> ...
%! convertvars (tblA, "Height", "struct");
%!error <table.convertvars: specified DATATYPE conversion on 'Height' does not return the appropriate amount of rows.> ...
%! convertvars (tblA, "Height", @(x) sum (x));

## Test 'rows2vars' method
%!error <table.rows2vars: 'DataVariables' index a non-existing variable: 'Some'.> ...
%! rows2vars (tblA, "DataVariables", "Some");
%!error <table.rows2vars: 'VariableNamesSource' must index a single variable.> ...
%! rows2vars (tblA, "VariableNamesSource", {"LastName", "Age"});
%!error <table.rows2vars: 'VariableNamesSource' indexes a non-existing variable: 'Some'.> ...
%! rows2vars (tblA, "VariableNamesSource", {"Some"});
%!error <table.rows2vars: the number of names taken from the variable specified in 'VariableNamesSource' does not match the number of rows in input table.> ...
%! rows2vars (tblA, "VariableNamesSource", {"Age"});
%!error <table.rows2vars: 'VariableNamesSource' cannot specify a variable that is also specified by 'DataVariables'.> ...
%! rows2vars (table (LastName, Age), "DataVariables", "LastName", "VariableNamesSource", "LastName");
%!error <table.rows2vars: invalid input for 'VariableNamingRule'.> ...
%! rows2vars (tblA, "VariableNamingRule", "somerule");
%!error <table.rows2vars: input table must not contain nested tables.> ...
%! rows2vars (table (tblA, LastName));
%!error <table.rows2vars: input table must not contain multicolumn variables.> ...
%! rows2vars (tblA);

## Test 'stack' method
%!error <table.stack: too few input arguments.> stack (tblA);
%!error <table.stack: VARS index a non-existing variable: 'Some'.> ...
%! stack (tblA, "Some");
%!error <table.stack: 'ConstantVariables' index a non-existing  variable: 'Some'.> ...
%! stack (tblA, 1:3, "ConstantVariables", "Some");
%!error <table.stack: 'ConstantVariables' cannot contain any variables to be stacked as specified by VARS.> ...
%! stack (tblA, 1:3, "ConstantVariables", "Weight");
%!error <table.stack: 'NewDataVariableName' must be either a character vector, or a cellstring or string scalar.> ...
%! stack (tblA, 1:3, "NewDataVariableName", 3);
%!error <table.stack: 'IndexVariableName' must be either a character vector, or a cellstring or string scalar.> ...
%! stack (tblA, 1:3, "IndexVariableName", 3);
