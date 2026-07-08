## Copyright (C) 2024-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
  ## @deftp {datatypes} table
  ##
  ## Array of tabular data containing multiple columnar variables.
  ##
  ## A table is a 2-dimensional data structure that collects heterogeneous data
  ## and metadata into a single container.  Tables are suitable for storing
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
    Description = ''

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
    DimensionNames = {'Row', 'Variables'}

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
    ## parentheses or curly braces.  You can also set @qcode{RowNames} by
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
    ## @code{@var{tbl} = table (@qcode{'Size'}, @var{sz},
    ## @qcode{'VariableTypes'}, @var{varTypes})} creates a new table of the
    ## given size, @var{sz}, and with the given variable types, @var{varTypes}.
    ## @var{sz} must be a two-element numeric array, where @qcode{@var{sz}(1)}
    ## specifies the number of rows and @qcode{@var{sz}(2)} specifies the
    ## number of variables.  The variables will contain the default value for
    ## elements of that type.
    ##
    ## @code{@var{tbl} = table (@dots{}, @qcode{'VariableNames'},
    ## @var{varNames})} specifies the variable names to use in the constructed
    ## table.
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
    ## @code{@var{tbl} = table (@dots{}, @qcode{'DimensionNames'},
    ## @var{dimNames})} specifies the dimension names to use in the constructed
    ## table.
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
      optNames = {'VariableNames', 'RowNames', 'DimensionNames'};
      dfValues = {{}, {}, {'Row', 'Variables'}};
      [VariableNames, RowNames, DimensionNames, args] = ...
                      parsePairedArguments (optNames, dfValues, varargin(:));
      ## Check optional Name-Value paired arguments
      if (! isempty (VariableNames))
        if (! (iscellstr (VariableNames) || isa (VariableNames, 'string')))
          error (strcat ("table: 'VariableNames' must be either a cell", ...
                         " array of character vectors or a string array."));
        endif
        VariableNames = cellstr (VariableNames);
        if (any (cellfun (@isempty, VariableNames)))
          error ("table: 'VariableNames' must contain nonempty names.");
        endif
      endif
      if (! isempty (RowNames))
        if (! (iscellstr (RowNames) || isa (RowNames, 'string')))
          error (strcat ("table: 'RowNames' must be either a cell array", ...
                         " of character vectors or a string array."));
        endif
        RowNames = cellstr (RowNames);
      endif
      if (! (iscellstr (DimensionNames) || isa (DimensionNames, 'string'))
          || numel (DimensionNames) != 2)
        error (strcat ("table: 'DimensionNames' must be either a", ...
                       " two-element cell array of character vectors or", ...
                       " a two-element string array."));
      endif
      this.DimensionNames = cellstr (DimensionNames);
      ## Dimension names cannot match reserved table identifiers
      reserved = {'Properties', 'RowNames', 'VariableNames', ':'};
      idr = ismember (this.DimensionNames, reserved);
      if (any (idr))
        error (strcat ("table: 'DimensionNames' cannot include the", ...
                       " reserved name: '%s'."), this.DimensionNames{idr});
      endif
      ## Check for conflict between VariableNames and DimensionNames
      idx = ismember (this.DimensionNames, VariableNames);
      if (any (idx))
        error ("table: duplicate dimension and variable name: '%s'", ...
               this.DimensionNames{idx});
      endif

      ## Construct a preallocated table with default values
      if (numel (args) == 4 && strcmpi (args{1}, 'Size') &&
                               strcmpi (args{3}, 'VariableTypes'))
        ## Validate the size specifier
        if (! isnumeric (args{2}) || numel (args{2}) != 2)
          error ("table: 'Size' must be a two-element numeric vector.");
        endif
        ## Get number of rows and variables
        nr = args{2}(1);
        nv = args{2}(2);
        ## Get variable types
        varTypes = args{4};
        if (! iscellstr (varTypes) || numel (varTypes) != nv)
          error (strcat ("table: 'VariableTypes' must be a cellstring", ...
                         " array of the same number of elements as", ...
                         " defined in SZ(2)."));
        endif

        ## Check optional arguments
        if (! isempty (VariableNames) && numel (VariableNames) != nv)
          error (strcat ("table: inconsistent number of 'VariableNames'", ...
                         " and 'VariableTypes'."));
        elseif (isempty (VariableNames))
          VariableNames = cell (1, nv);
          for i = 1:nv
            VariableNames{i} = sprintf ("Var%d", i);
          endfor
        endif
        if (! isempty (RowNames) && numel (RowNames) != nr)
          error (strcat ("table: inconsistent number of 'RowNames' and", ...
                         " rows defined in SZ(1)."));
        endif

        ## Populate variables with defaults
        VariableTypes = cell (1, nv);
        VariableValues = cell (1, nv);
        for i = 1:nv
          VariableTypes{i} = varTypes{i};
          switch (varTypes{i})
            case {'double', 'single', 'int8', 'uint8', 'int16', 'uint16', ...
                  'int32', 'uint32', 'int64', 'uint64'}
              VariableValues{i} = zeros (nr, 1, varTypes{i});
            case {'doublenan', 'doubleNaN'}
              VariableValues{i} = NaN (nr, 1, 'double');
            case {'singlenan', 'singleNaN'}
              VariableValues{i} = NaN (nr, 1, 'single');
            case 'logical'
              VariableValues{i} = logical (zeros (nr, 1));
            case 'categorical'
              VariableValues{i} = categorical (NaN (nr, 1));
            case 'datetime'
              VariableValues{i} = NaT (nr, 1);
            case 'duration'
              VariableValues{i} = seconds (zeros (nr, 1));
            case 'calendarDuration'
              VariableValues{i} = calendarDuration (zeros (nr, 3));
            case 'string'
              VariableValues{i} = string (NaN (nr, 1));
            case {'cellstr', 'char'}
              VariableValues{i} = repmat (cellstr (""), nr, 1);
            case 'cell'
              VariableValues{i} = cell (nr, 1);
            case 'struct'
              VariableValues{i} = repmat (struct, nr, 1);
            case 'table'
              VariableValues{i} = table([]);
            case 'timetable'
              error ("table: 'timetable' variable type not supported yet.");
            otherwise
              error ("table: unsupported variable type: '%s'.", varTypes{i});
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
          error (strcat ("table: inconsistent number of variable names", ...
                         " (%d) and variable values (%d)"), ...
                 numel (VariableNames), numel (args));
        endif
        ## Check size of input variables
        if (! isempty (args))
          nrows = size (args{1}, 1);
          if (ndims (args{1}) > 2)
            error (strcat ("table: variable values must not have more", ...
                           " than 2 dimensions: input 1 '%s' has %d."), ...
                   VariableNames{1}, ndims (args{1}));
          endif
          for i = 2:numel (args)
            if (ndims (args{i}) > 2)
              error (strcat ("table: variable values must not have more", ...
                             " than 2 dimensions: input %d '%s' has %d."), ...
                     i, VariableNames{i}, ndims (args{i}));
            endif
            nrows2 = size (args{i}, 1);
            if (nrows != nrows2)
              error (strcat ("table: inconsistent sizes between", ...
                             " variables: var '%s' has %d rows; var '%s'", ...
                             " has %d rows."), ...
                     VariableNames{1}, nrows, VariableNames{i}, nrows2);
            endif
          endfor
        endif
        VariableValues = args(:)';
      endif

      ## Construction
      this.VariableDescriptions = repmat ({''}, [1, numel(VariableNames)]);
      this.VariableUnits = repmat ({''}, [1, numel(VariableNames)]);
      this.VariableNames = VariableNames(:)';
      this.VariableValues = VariableValues;
      this.VariableTypes = cellfun ('class', VariableValues, ...
                                    'UniformOutput', false);
      if (! isempty (RowNames))
        if (isempty (VariableValues))
          nrows = 0;
        else
          nrows = size (VariableValues{1}, 1);
        endif
        if (numel (RowNames) != nrows)
          error (strcat ("table: the number of 'RowNames' (%d) must", ...
                         " equal the number of rows (%d)."), ...
                 numel (RowNames), nrows);
        elseif (numel (__unique__ (RowNames)) != numel (RowNames))
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
      ## A mix of cell and non-cell variables cannot form a homogeneous array.
      ## Octave would silently promote single-row pieces to a cell (MATLAB
      ## errors), so guard explicitly and report the first incompatible pair.
      pair = mixed_cell_pair (this.VariableValues);
      if (! isempty (pair))
        error (strcat ("table.table2array: cannot concatenate the table", ...
                       " variables '%s' and '%s', because their types are", ...
                       " %s and %s."), this.VariableNames{pair(1)}, ...
               this.VariableNames{pair(2)}, ...
               class (this.VariableValues{pair(1)}), ...
               class (this.VariableValues{pair(2)}));
      endif
      ## Add a try...catch block instead of heuristics
      try
        A = cat (2, this.VariableValues{:});
      catch
        error (strcat ("table.table2array: table cannot be concatenated", ...
                       " into a matrix due to incompatible variable", ...
                       " types."));
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
    ## Compatibility Notes:
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
      C = cell (size (this));
      for i = 1:width (this)
        varVal = this.VariableValues{i};
        if (iscell (varVal))
          C(:,i) = varVal;
        elseif (isnumeric (varVal) || islogical (varVal))
          C(:,i) = num2cell (varVal, 2);
        elseif (any (isa (varVal, {'calendarDuration', 'categorical'})))
          C(:,i) = dispstrings (varVal);
        elseif (any (isa (varVal, {'datetime', 'duration'})))
          C(:,i) = dispstrings (varVal);
        elseif (isa (varVal, 'string'))
          C(:,i) = cellstr (varVal);
        elseif (isa (varVal, 'table'))
          tmpVal = table2cell (varVal);
          if (size (tmpVal, 2) > 1)
            C(:,i) = num2cell (cell2mat (tmpVal), 2);
          else
            C(:,i) = tmpVal;
          endif
        elseif (isa (varVal, 'struct'))
          C(:,i) = num2cell (varVal(:));
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{S} =} table2struct (@var{tbl})
    ## @deftypefnx {table} {@var{S} =} table2struct (@var{tbl}, @qcode{'ToScalar'}, @qcode{true})
    ##
    ## Converts a table to a scalar structure or structure array.
    ##
    ## @code{@var{S} = table2struct (@var{tbl})} returns a structure array with
    ## the same fields as the variables in @var{tbl}.  The length of @var{S} is
    ## the same as the height of @var{tbl}.
    ##
    ## @code{@var{S} = table2struct (@var{tbl}, @qcode{'ToScalar'},
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
          error ("table.table2struct: wrong number of input arguments.");
        endif
        if (strcmpi (varargin{1}, 'ToScalar') && isequal (varargin{2}, 1))
          toScalar = true;
        elseif (strcmpi (varargin{1}, 'ToScalar'))
          toScalar = false;
        else
          error ("table.table2struct: wrong optional input argument.");
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
        ## 'table2cell' renders categorical, datetime, duration,
        ## calendarDuration, and string variables as character vectors; restore
        ## the original typed values so the structure array preserves the
        ## variable types, consistent with the 'ToScalar' output and MATLAB.
        for i = 1:width (this)
          vv = this.VariableValues{i};
          if (any (isa (vv, {'categorical', 'datetime', 'duration', ...
                             'calendarDuration', 'string'})))
            for r = 1:size (vv, 1)
              C{r,i} = vv(r,:);
            endfor
          endif
        endfor
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
    ## Write a table to a comma-separated-value (CSV) file.
    ##
    ## @code{table2csv (@var{tbl}, @var{file})} writes the table @var{tbl} to
    ## @var{file}, which may be a character vector, a cellstr, or a string
    ## scalar.  The resulting file can be read back with @code{csv2table}.
    ##
    ## The file begins with a comment line reporting how many consecutive rows
    ## hold the variable types, names, descriptions, and units, in that order.
    ## Those header rows are followed by one row of data per table row.
    ##
    ## Variables are serialized as follows:
    ##
    ## @itemize
    ## @item
    ## Numeric and logical variables are written as numbers (logicals as
    ## @code{0}/@code{1}).  Missing and infinite values are written as the
    ## tokens @qcode{NaN}, @qcode{NA}, @qcode{inf}, and @qcode{-inf}.
    ##
    ## @item
    ## Character, cellstr, and @code{string} variables are written as quoted
    ## text.
    ##
    ## @item
    ## @code{datetime}, @code{duration}, @code{calendarDuration}, and
    ## @code{categorical} variables are written as their display strings.
    ##
    ## @item
    ## A multicolumn variable is split into consecutive columns that share the
    ## same variable name.
    ##
    ## @item
    ## A nested table is split into columns tagged with both the outer and the
    ## nested variable name.  A structure is split into one column per field,
    ## tagged with the variable name and the field name.
    ## @end itemize
    ##
    ## When @var{tbl} has row names they are written under a leading
    ## @qcode{RowNames} column.  Variable descriptions and units are written
    ## only when @emph{every} variable has a non-empty description or unit,
    ## respectively.
    ##
    ## Note the following round-trip limitations when reading the file back
    ## with @code{csv2table}: @code{calendarDuration} and @code{categorical}
    ## variables are returned as cell arrays of character vectors (their values
    ## are not reconstructed), and missing @code{string} values are read back
    ## as empty strings.
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
      ## Descriptions and units are written only when every variable carries
      ## one; nested variables expand them to as many rows as varNames/varTypes.
      Drows = cellfun (@(x) size (x, 1), D);
      if (all (cellfun (@(x) ! isempty (x), D(isvar))))
        Dmaxr = max (Drows(isvar));
      else
        Dmaxr = 0;
      endif
      Urows = cellfun (@(x) size (x, 1), U);
      if (all (cellfun (@(x) ! isempty (x), U(isvar))))
        Umaxr = max (Urows(isvar));
      else
        Umaxr = 0;
      endif
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
            if (Drows(c) == 1)
              Header{1 + Tmaxr + Nmaxr,c} = D{c};
            else
              for dr = 1:Drows(c)
                Header{dr + Tmaxr + Nmaxr,c} = D{c}{dr};
              endfor
            endif
          endif
          if (Umaxr)
            if (Urows(c) == 1)
              Header{1 + Tmaxr + Nmaxr + Dmaxr,c} = U{c};
            else
              for ur = 1:Urows(c)
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

    ## -*- texinfo -*-
    ## @deftypefn  {table} {} table2ods (@var{tbl}, @var{file})
    ## @deftypefnx {table} {} table2ods (@var{tbl}, @var{file}, @var{Name}, @var{Value})
    ##
    ## Write a table to an OpenDocument spreadsheet file.
    ##
    ## @code{table2ods (@var{tbl}, @var{file})} writes the table @var{tbl} to
    ## @var{file}, which may be a character vector, a cellstr, or a string
    ## scalar.  When @var{file} ends in @qcode{.ods} a compressed (ZIP-packaged)
    ## OpenDocument spreadsheet is written; when it ends in @qcode{.fods} a flat
    ## (single-XML) OpenDocument spreadsheet is written instead.  The resulting
    ## file can be read back with @code{ods2table}.
    ##
    ## The data sheet (named @qcode{Sheet1} by default) carries one natively typed
    ## cell per value, and a hidden @qcode{__datatypes_meta__} sheet carries the
    ## variable types, names, descriptions, and units needed to restore the exact
    ## Octave types on read-back.  Variables map to ODS cell types as follows:
    ##
    ## @itemize
    ## @item
    ## Numeric variables become @code{float} cells and logical variables become
    ## @code{boolean} cells.  Integers are written with their exact digits.
    ##
    ## @item
    ## @code{datetime} variables become native @code{date} cells and
    ## @code{duration} variables become native @code{time} cells, both encoded
    ## as ISO 8601 strings.
    ##
    ## @item
    ## Character, cellstr, @code{string}, @code{categorical}, and
    ## @code{calendarDuration} variables become @code{string} cells.
    ##
    ## @item
    ## A multicolumn variable is split into consecutive columns that share the
    ## same variable name.
    ## @end itemize
    ##
    ## Missing values (@code{NaN}, @code{NaT}, and missing strings) are written
    ## as empty cells.  When @var{tbl} has row names they are written under a
    ## leading @qcode{RowNames} column.  Variable descriptions and units are
    ## written only when @emph{every} variable has a non-empty description or
    ## unit, respectively.
    ##
    ## @code{table2ods (@dots{}, @qcode{'Sheet'}, @var{name})} writes to a sheet
    ## named @var{name} (default @qcode{'Sheet1'}).  When @var{file} already
    ## exists the named sheet is added or replaced while every other sheet is
    ## preserved, so a workbook can be built up one table at a time.
    ## @code{table2ods (@dots{}, @qcode{'WriteMode'}, @var{mode})} selects the
    ## behaviour: @qcode{'overwritesheet'} / @qcode{'inplace'} replace the sheet
    ## (the default when the sheet exists), @qcode{'append'} appends the table's
    ## rows to it, and @qcode{'replacefile'} discards any existing file.
    ##
    ## Nested tables and structures are not supported and raise an error.  Note
    ## the following round-trip limitations when reading the file back with
    ## @code{ods2table}: @code{calendarDuration} and @code{categorical}
    ## variables are returned as cell arrays of character vectors (their values
    ## are not reconstructed).
    ##
    ## @seealso{struct2ods, ods2table, ods2struct, writetable}
    ## @end deftypefn
    function table2ods (this, file, varargin)
      file = char (cellstr (file));
      ## A '.fods' file is written as flat XML, a '.ods' file as a ZIP package.
      [~, ~, ext] = fileparts (file);
      if (strcmpi (ext, '.fods'))
        is_flat = true;
      elseif (strcmpi (ext, '.ods'))
        is_flat = false;
      else
        error (strcat ("table.table2ods: FILE must have a '.ods' or", ...
                       " '.fods' extension."));
      endif

      optNames = {'Sheet', 'WriteMode'};
      dfValues = {'Sheet1', ''};
      [sheet, writeMode, args] = ...
              parsePairedArguments (optNames, dfValues, varargin(:));
      if (! isempty (args))
        error ("table.table2ods: unknown option '%s'.", args{1});
      endif
      if (isa (sheet, 'string'))
        sheet = char (sheet);
      endif
      if (! (ischar (sheet) && isrow (sheet)))
        error ("table.table2ods: 'Sheet' must be a sheet name.");
      endif
      writeMode = lower (char (writeMode));
      switch (writeMode)
        case {'', 'replacefile', 'overwritesheet', 'inplace', 'append'}
          ## supported write modes
        otherwise
          error ("table.table2ods: 'WriteMode' '%s' is not valid.", writeMode);
      endswitch

      ## Merge into an existing workbook (preserving other sheets) by reading it
      ## back, modifying the struct of tables, and rewriting the whole file.
      if (exist (file, 'file') && ! strcmp (writeMode, 'replacefile'))
        s = ods2struct (file);
        s = merge_table_into_struct (s, this, sheet, writeMode);
        struct2ods (file, s);
        return;
      endif

      ## Fresh single-sheet write.
      [V, vtype, meta] = __ods_parts__ (this, 'table.table2ods');
      msg = __table2ods__ (file, V, vtype, meta, is_flat, ...
                           struct ('sheetname', sheet));
      if (! isequal (msg, 0))
        error ("table.table2ods: %s", msg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {} writetable (@var{tbl}, @var{filename})
    ## @deftypefnx {table} {} writetable (@var{tbl}, @var{filename}, @var{Name}, @var{Value})
    ##
    ## Write a table to a file in a MATLAB-compatible format.
    ##
    ## @code{writetable (@var{tbl}, @var{filename})} writes the table @var{tbl}
    ## to @var{filename}.  The file type is inferred from the extension:
    ## @qcode{.txt}, @qcode{.csv}, and @qcode{.dat} are written as delimited
    ## text; @qcode{.ods} is written as an OpenDocument spreadsheet.  Use the
    ## @qcode{'FileType'} option to override the inferred type.
    ##
    ## Unlike @code{table2csv}/@code{table2ods}, no type metadata is written: the
    ## file holds only an optional variable-name header row followed by the data,
    ## so it can be read by other applications.  Type information is recovered by
    ## @code{readtable} through automatic detection (text) or the native cell
    ## types (spreadsheet).  The following options are supported:
    ##
    ## @multitable @columnfractions 0.28 0.72
    ## @item @qcode{'FileType'} @tab @qcode{'text'} or @qcode{'spreadsheet'}.
    ## @item @qcode{'WriteVariableNames'} @tab Logical; write the variable names
    ## as the first row (default @qcode{true}).
    ## @item @qcode{'WriteRowNames'} @tab Logical; write the row names as the
    ## first column (default @qcode{false}).
    ## @item @qcode{'Delimiter'} @tab Field delimiter for text files: a single
    ## character or one of @qcode{'comma'}, @qcode{'space'}, @qcode{'tab'},
    ## @qcode{'semi'}, @qcode{'bar'} (default @qcode{','}).
    ## @item @qcode{'QuoteStrings'} @tab @qcode{'minimal'}, @qcode{'all'}, or
    ## @qcode{'none'} for text files (default @qcode{'minimal'}).
    ## @item @qcode{'Sheet'} @tab Spreadsheet only: the name of the sheet to
    ## write (default @qcode{'Sheet1'}).
    ## @item @qcode{'Range'} @tab Spreadsheet only: an A1-style anchor such as
    ## @qcode{'C5'} at which to place the top-left corner of the data (fresh
    ## writes only).
    ## @item @qcode{'WriteMode'} @tab For text: @qcode{'overwrite'} (default) or
    ## @qcode{'append'}.  For spreadsheets: @qcode{'overwritesheet'} /
    ## @qcode{'inplace'} (replace the target sheet), @qcode{'append'} (append
    ## rows to it), or @qcode{'replacefile'} (overwrite the whole file).
    ## @end multitable
    ##
    ## When the target spreadsheet already exists, the sheet named by
    ## @qcode{'Sheet'} (default @qcode{'Sheet1'}) is added or replaced while every
    ## other sheet is preserved, unless @qcode{'WriteMode'} is
    ## @qcode{'replacefile'}.  Existing foreign spreadsheets (for example those
    ## written by LibreOffice) are updated in place, keeping their other parts.
    ##
    ## Nested tables and structures are not supported.  Microsoft Excel formats
    ## (@qcode{.xls}, @qcode{.xlsx}, @qcode{.xlsb}) are not supported either; use
    ## @qcode{.ods} or a text format.
    ##
    ## @end deftypefn
    function writetable (this, filename, varargin)
      if (! ((ischar (filename) && isvector (filename)) ...
             || (isa (filename, 'string') && isscalar (filename))))
        error (strcat ("table.writetable: FILENAME must be a character", ...
                       " vector or string scalar."));
      endif
      file = char (filename);

      optNames = {'FileType', 'WriteVariableNames', 'WriteRowNames', ...
                  'Delimiter', 'QuoteStrings', 'Sheet', 'Range', 'WriteMode'};
      dfValues = {'', true, false, ',', 'minimal', '', '', ''};
      [fileType, writeVarNames, writeRowNames, delim, quoteStrings, sheet, ...
       range, writeMode, args] = ...
              parsePairedArguments (optNames, dfValues, varargin(:));
      if (! isempty (args))
        error ("table.writetable: unknown option '%s'.", args{1});
      endif
      if (isa (sheet, 'string'))
        sheet = char (sheet);
      endif
      if (isa (range, 'string'))
        range = char (range);
      endif
      writeMode = lower (char (writeMode));

      ## Resolve the file type from the option or the extension
      [~, ~, ext] = fileparts (file);
      if (isempty (fileType))
        switch (lower (ext))
          case {'.txt', '.csv', '.dat'}
            fileType = 'text';
          case {'.ods', '.fods'}
            fileType = 'spreadsheet';
          case {'.xls', '.xlsx', '.xlsb', '.xlsm'}
            error (strcat ("table.writetable: Microsoft Excel formats are", ...
                           " not supported; use '.ods' or a text format."));
          otherwise
            error (strcat ("table.writetable: cannot infer the file type", ...
                           " from '%s'; specify 'FileType'."), ext);
        endswitch
      endif

      switch (lower (fileType))
        case 'text'
          fmt = 'display';
        case 'spreadsheet'
          fmt = 'iso';
        otherwise
          error ("table.writetable: 'FileType' must be 'text' or 'spreadsheet'.");
      endswitch

      ## Validate 'Sheet', 'Range', and 'WriteMode' against the resolved type.
      appendMode = false;
      if (strcmp (fmt, 'display'))          # text
        if (! isempty (sheet) || ! isempty (range))
          error (strcat ("table.writetable: 'Sheet' and 'Range' are not", ...
                         " supported for text files."));
        endif
        switch (writeMode)
          case {'', 'overwrite'}
            appendMode = false;
          case 'append'
            appendMode = true;
          otherwise
            error (strcat ("table.writetable: 'WriteMode' '%s' is not valid", ...
                           " for text files; use 'overwrite' or 'append'."), ...
                   writeMode);
        endswitch
      else                                  # spreadsheet
        if (! isempty (sheet) && ! (ischar (sheet) && isrow (sheet)))
          error ("table.writetable: 'Sheet' must be a sheet name.");
        endif
        switch (writeMode)
          case {'', 'replacefile', 'overwritesheet', 'inplace', 'append'}
            ## supported spreadsheet write modes
          otherwise
            error (strcat ("table.writetable: 'WriteMode' '%s' is not valid", ...
                           " for spreadsheet files."), writeMode);
        endswitch
        ## A 'Range' anchors a fresh write; it has no meaning when merging into
        ## an existing workbook.
        if (! isempty (range))
          if (strcmp (writeMode, 'append'))
            error (strcat ("table.writetable: 'Range' is not supported with", ...
                           " 'WriteMode' 'append'."));
          endif
          if (exist (file, 'file') && ! strcmp (writeMode, 'replacefile'))
            error (strcat ("table.writetable: 'Range' is not supported when", ...
                           " writing into an existing file."));
          endif
        endif
      endif

      ## Flatten the table; nested tables and structs (multi-row type entries)
      ## are refused, as MATLAB does.
      [V, N, T] = table2cellarrays (this, fmt);
      if (any (cellfun (@iscell, T)))
        error (strcat ("table.writetable: writetable does not support", ...
                       " writing nested tables.  Use splitvars to split", ...
                       " multicolumn variables into single-column variables", ...
                       " before writing."));
      endif
      [names, V, T] = writetable_prep (V, N, T, writeRowNames);

      if (strcmp (fmt, 'display'))
        ## In append mode MATLAB writes the data rows only, never a header.
        if (writeVarNames && ! appendMode)
          grid = [names; V];
        else
          grid = V;
        endif
        d = wt_resolve_delimiter (delim);
        msg = __table2csv__ (file, grid, d, lower (quoteStrings), appendMode);
        if (msg)
          error ("table.writetable: %s", msg);
        endif
      else
        vtype = cell (1, numel (T));
        for c = 1:numel (T)
          vtype{c} = ods_value_type (T{c});
        endfor
        opts = struct ();
        ## Append mode writes data rows only, never a header.
        if (writeVarNames && ! strcmp (writeMode, 'append'))
          opts.header = names;
        else
          opts.header = {};
        endif
        if (! isempty (sheet))
          opts.sheetname = sheet;
        endif
        is_flat = strcmpi (ext, '.fods');
        ## Merge into an existing workbook (preserving other sheets) unless the
        ## file is new or 'replacefile' asks to overwrite it outright.
        if (exist (file, 'file') && ! strcmp (writeMode, 'replacefile'))
          opts.merge = true;
          opts.writemode = writeMode;
        elseif (! isempty (range))
          [r1, c1] = __a1ref__ (range);
          opts.roff = r1 - 1;
          opts.coff = c1 - 1;
        endif
        msg = __table2ods__ (file, V, vtype, {}, is_flat, opts);
        if (! isequal (msg, 0))
          error ("table.writetable: %s", msg);
        endif
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

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {} summary (@var{tbl})
    ## @deftypefnx {table} {@var{s} =} summary (@var{tbl})
    ##
    ## Print a summary of a table.
    ##
    ## @code{summary (@var{tbl})} prints the description from
    ## @qcode{@var{tbl}.Properties.Description} followed by a summary of each
    ## table variable's values and their properties as defined in
    ## @qcode{@var{tbl}.Properties.VariableUnits} and
    ## @qcode{@var{tbl}.Properties.VariableDescriptions}.
    ##
    ## @code{@var{s} = summary (@var{tbl})} returns a structure, @var{s}, that
    ## contains a summary of the input table, @var{tbl}.  Each field of @var{s}
    ## is a structure that summarizes the values in the corresponding variable
    ## of @var{tbl}.  Where applicable, the number of missing values is reported
    ## in a @qcode{NumMissing} field and printed when it is greater than zero.
    ##
    ## @itemize
    ## @item For numerical variables of @qcode{double}, @qcode{single} or any
    ## @qcode{int} type, it prints the minimum, median, and maximum values.  For
    ## multicolumnar numerical variables it prints the minimum, median, and
    ## maximum values for each column separately.
    ##
    ## @item For variables of @qcode{logical} type, it prints the occurrences
    ## of @qcode{True} and @qcode{False}.
    ##
    ## @item For variables of type @qcode{datetime} and @qcode{duration} it
    ## prints the minimum, median, and maximum values, computed after excluding
    ## any missing (@qcode{NaT} or @qcode{NaN}) elements.
    ##
    ## @item For variables of type @qcode{calendarDuration}, which are not
    ## totally ordered, only the size, the type, and the number of missing
    ## values are reported.
    ##
    ## @item For variables of type @qcode{cellstr}, @qcode{cell},
    ## @qcode{string}, @qcode{categorical}, and @qcode{struct} it prints the
    ## size and the type of variable.
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
          if (isfield (var, 'CustomProperties'))
            if (! isempty (var.CustomProperties))
              fprintf ("%s    Custom Properties:\n", tab);
              cpNames = fieldnames (var.CustomProperties);
              for p = 1:numel (cpNames)
                fprintf ("%s        %s: %s\n", tab, cpNames{p}, ...
                         var.CustomProperties.(cpNames{p}){:});
              endfor
            endif
          endif
          ## Print values (numeric/datetime/duration Min Median Max, logical
          ## True False) followed by the count of missing values, if any.
          if (isfield (var, 'Min') || isfield (var, 'True'))
            fprintf ("%s    Values:\n", tab);
            isLogical = isfield (var, 'True');
            isNumeric = isfield (var, 'Min') && isnumeric (var.Min);
            ## Check for multicolumnar variable
            if (var.Size(2) > 1)
              ## Find max element length for aligning the columns
              if (isNumeric)
                mLen = max (arrayfun (@(x) length (num2str (x)), ...
                            [var.Min, var.Median, var.Max]));
              elseif (isLogical)
                mLen = max (arrayfun (@(x) length (num2str (x)), ...
                            [var.True, var.False]));
              else
                strs = [dispstrings(var.Min), dispstrings(var.Median), ...
                        dispstrings(var.Max)];
                mLen = max (cellfun (@length, strs));
              endif
              ## Create padding character vectors
              mLen = max (8, mLen);
              pad = repmat (' ', 1, mLen - 8);
              strhead = '';
              strline = '';
              for c = 1:var.Size(2)
                strhead = [strhead, sprintf("%sColumn %d    ", pad, c)];
                strline = [strline, sprintf("%s________    ", pad)];
              endfor
              ## Print multicolumnar variable header
              fprintf ("%s                  %s\n", tab, strhead);
              fprintf ("%s                  %s\n", tab, strline);
              ## Print multicolumnar variable statistics
              if (isNumeric)
                template = ["%s%", sprintf("%d", mLen), "g    "];
                strMin = '';
                strMed = '';
                strMax = '';
                for c = 1:numel (var.Min)
                  strMin = [strMin, sprintf(template, pad, var.Min(c))];
                  strMed = [strMed, sprintf(template, pad, var.Median(c))];
                  strMax = [strMax, sprintf(template, pad, var.Max(c))];
                endfor
                fprintf ("%s        Min       %s\n", tab, strMin);
                fprintf ("%s        Median    %s\n", tab, strMed);
                fprintf ("%s        Max       %s\n", tab, strMax);
              elseif (isLogical)
                template = ["%s%", sprintf("%d", mLen), "g    "];
                strTrue = '';
                strFalse = '';
                for c = 1:numel (var.True)
                  strTrue = [strTrue, sprintf(template, pad, var.True(c))];
                  strFalse = [strFalse, sprintf(template, pad, var.False(c))];
                endfor
                fprintf ("%s        True      %s\n", tab, strTrue);
                fprintf ("%s        False     %s\n", tab, strFalse);
              else
                ## datetime or duration
                template = ["%s%", sprintf("%d", mLen), "s    "];
                minStr = dispstrings (var.Min);
                medStr = dispstrings (var.Median);
                maxStr = dispstrings (var.Max);
                strMin = '';
                strMed = '';
                strMax = '';
                for c = 1:numel (minStr)
                  strMin = [strMin, sprintf(template, pad, minStr{c})];
                  strMed = [strMed, sprintf(template, pad, medStr{c})];
                  strMax = [strMax, sprintf(template, pad, maxStr{c})];
                endfor
                fprintf ("%s        Min       %s\n", tab, strMin);
                fprintf ("%s        Median    %s\n", tab, strMed);
                fprintf ("%s        Max       %s\n", tab, strMax);
              endif
            ## Print single column variable
            else
              if (isNumeric)
                fprintf ("%s        Min       %g\n", tab, var.Min);
                fprintf ("%s        Median    %g\n", tab, var.Median);
                fprintf ("%s        Max       %g\n", tab, var.Max);
              elseif (isLogical)
                fprintf ("%s        True      %d\n", tab, var.True);
                fprintf ("%s        False     %d\n", tab, var.False);
              else
                ## datetime or duration
                fprintf ("%s        Min       %s\n", tab, ...
                         dispstrings (var.Min){:});
                fprintf ("%s        Median    %s\n", tab, ...
                         dispstrings (var.Median){:});
                fprintf ("%s        Max       %s\n", tab, ...
                         dispstrings (var.Max){:});
              endif
            endif
            ## Print number of missing values, if any
            if (isfield (var, 'NumMissing') && any (var.NumMissing(:) > 0))
              fprintf ("%s        NumMissing%s\n", tab, ...
                       sprintf (" %d", var.NumMissing));
            endif
            fprintf ("\n");
          ## Variables carrying only a missing-value count (calendarDuration)
          elseif (isfield (var, 'NumMissing') && any (var.NumMissing(:) > 0))
            fprintf ("%s    Values:\n", tab);
            fprintf ("%s        NumMissing%s\n\n", tab, ...
                     sprintf (" %d", var.NumMissing));
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
    ## table @var{tbl} as a scalar.  It is the equivalent of
    ## @qcode{size (@var{tbl}, 1)}.
    ##
    ## For an empty table, or a table created with zero rows, @code{height}
    ## returns 0.  The presence of row names does not affect the result.
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
    ## table @var{tbl} as a scalar.  It is the equivalent of
    ## @qcode{size (@var{tbl}, 2)}.
    ##
    ## Note that this is the number of table variables, not the total number of
    ## columns.  A single variable may itself contain several columns (for
    ## example, a matrix-valued variable), but it still counts as one towards
    ## the table width.
    ##
    ## For a table with no variables, @code{width} returns 0.
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
    ## Display or return the first @var{k} rows of a table.
    ##
    ## @code{head (@var{tbl})} displays the first eight rows of the table
    ## @var{tbl}.  If @var{tbl} has fewer than eight rows, then all rows are
    ## displayed.
    ##
    ## @code{head (@var{tbl}, @var{k})} displays the first @var{k} rows of the
    ## table @var{tbl}.  @var{k} must be a positive integer scalar value.  If
    ## @var{tbl} has fewer than @var{k} rows, then all rows are displayed.
    ##
    ## @code{@var{out} = head (@var{tbl}, @var{k})} returns the first @var{k}
    ## rows in a new table @var{out} instead of displaying them.  If @var{k} is
    ## omitted or empty, then it defaults to eight.  If @var{tbl} has fewer than
    ## @var{k} rows, then all available rows are returned.
    ##
    ## The returned table preserves the variable names, row names, and all other
    ## properties of @var{tbl}.
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
    ## Display or return the last @var{k} rows of a table.
    ##
    ## @code{tail (@var{tbl})} displays the last eight rows of the table
    ## @var{tbl}.  If @var{tbl} has fewer than eight rows, then all rows are
    ## displayed.
    ##
    ## @code{tail (@var{tbl}, @var{k})} displays the last @var{k} rows of the
    ## table @var{tbl}.  @var{k} must be a positive integer scalar value.  If
    ## @var{tbl} has fewer than @var{k} rows, then all rows are displayed.
    ##
    ## @code{@var{out} = tail (@var{tbl}, @var{k})} returns the last @var{k}
    ## rows in a new table @var{out} instead of displaying them.  If @var{k} is
    ## omitted or empty, then it defaults to eight.  If @var{tbl} has fewer than
    ## @var{k} rows, then all available rows are returned.
    ##
    ## The returned table preserves the variable names, row names, and all other
    ## properties of @var{tbl}.
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
    ## ascending order based on the values in the first variable.   If elements
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
    ## @var{direction} must always be the 3rd input argument.  If you want to
    ## omit passing selected variables and allow @code{sortrows} to work on
    ## consecutive variables until all ties are resolved, then you can leave the
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
      optNames = {'MissingPlacement', 'ComparisonMethod'};
      dfValues = {'auto', 'auto'};
      [MP, CM, args] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! ismember (MP, {'auto', 'first', 'last'}))
        error (strcat ("table.sortrows: 'MissingPlacement' parameter can", ...
                       " be either 'auto', 'first', or 'last'."));
      endif
      if (! ismember (CM, {'auto', 'real', 'abs'}))
        error (strcat ("table.sortrows: 'ComparisonMethod' parameter can", ...
                       " be either 'auto', 'real', or 'abs'."));
      endif

      ## Parse extra arguments
      nargs = numel (args);
      if (nargs > 2)
        error ("table.sortrows: invalid number of input arguments.");
      endif
      if (nargs > 1)
        direction = cellstr (args{2});
        dir_given = true;
        if (! all (ismember (direction, {'ascend', 'descend'})))
          error ("table.sortrows: invalid value for DIRECTION argument.");
        endif
      endif
      if (nargs > 0)
        ## RowNames and rowDimName take precedence over variable names
        arg1 = args{1};
        if (ischar (arg1) && isvector (arg1) &&
            ismember (arg1, {'RowNames', this.DimensionNames{1}}))
          ## Check user's direction is scalar
          if (dir_given && numel (direction) != 1)
            error (strcat ("table.sortrows: DIRECTION must be a scalar", ...
                           " input when 'RowNames' or 'rowDimNames' are", ...
                           " selected."));
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
            error (strcat ("table.sortrows: logical indexing vector does", ...
                           " not match table width."));
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
            error (strcat ("table.sortrows: numerical indexing must be a", ...
                           " vector of nonzero integers."));
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
            direction(sign (varRef) > 0) = 'ascend';
            direction(sign (varRef) < 0) = 'descend';
            varRef = abs (varRef);
          endif
        elseif (ischar (arg1) || iscellstr (arg1) || isa (arg1, 'string'))
          varRef = cellstr (arg1);
          if (isscalar (varRef) && strcmp (varRef, ':'))
            varRef = ':';
          elseif (! all (ismember (varRef, [this.VariableNames, {'RowNames'}])))
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
          if (any (ismember (varRef, 'RowNames')))
            inRowNames = find (strcmp (varRef, 'RowNames'));
            varRef(inRowNames) = [];
          endif
        elseif (isa (arg1, 'vartype'))
          varRef = arg1;
          ## Check user's direction is scalar
          if (dir_given && numel (direction) != 1)
            error (strcat ("table.sortrows: DIRECTION must be a scalar", ...
                           " input when variables are indexed with a", ...
                           " 'vartype' object."));
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
        if (strcmpi (direction{ix}, 'ascend'))
          tmpDir = 1;
        else
          tmpDir = -1;
        endif

        if (isa (tmpVal, 'categorical'))
          tmpVal = double (tmpVal);
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, 'calendarDuration'))
          tmpVal = tmpVal.proxyArray;
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, 'datetime'))
          tmpVal = datetime_to_datenum (tmpVal);
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, 'duration'))
          tmpVal = days (tmpVal);
          varValIdx = [varValIdx, tmpVal];

        elseif (isa (tmpVal, 'string'))
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
          if (strcmpi (CM, 'real') && iscomplex (tmpVal))
            tmpVal = real (tmpVal);
          elseif (strcmpi (CM, 'abs') && isreal (tmpVal))
            tmpVal = abs (tmpVal);
          endif
          varValIdx = [varValIdx, tmpVal];

        elseif (isstruct (tmpVal))
          ## Sorting structure data is not supported
          error ("table.sortrows: cannot sort variables of 'struct' type.");

        elseif (isa (tmpVal, 'table'))
          try
            tmpVal = table2array (varVal{ix});
            varValIdx = [varValIdx, tmpVal];
          catch
            error (strcat ("table.sortrows: cannot sort nested tables", ...
                           " with mixed data types."));
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
        if (any (find (TFvec) == 1) && strcmpi (MP, 'last'))
          index = [no_nan; is_nan];
        elseif (any (find (TFvec) == numel (index)) && strcmpi (MP, 'first'))
          index = [is_nan; no_nan];
        endif
        tbl = subsetrows (this, index);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} unique (@var{tblA})
    ## @deftypefnx {table} {@var{tblB} =} unique (@var{tblA}, @var{setOrder})
    ## @deftypefnx {table} {@var{tblB} =} unique (@var{tblA}, @var{occurrence})
    ## @deftypefnx {table} {[@var{tblB}, @var{ixA}, @var{ixB}] =} unique (@dots{})
    ##
    ## Unique rows in a table.
    ##
    ## @code{@var{tblB} = unique (@var{tblA})} returns the unique rows of table
    ## @var{tblA} in sorted order.
    ##
    ## @code{@var{tblB} = unique (@var{tblA}, @var{setOrder})} returns the
    ## unique rows of table @var{tblA} in a specified order.  @var{setOrder} can
    ## be either @qcode{'sorted'} (default) or @qcode{'stable'}.
    ##
    ## @itemize
    ## @item @qcode{'sorted'} returns the unique rows sorted in ascending order.
    ## @item @qcode{'stable'} returns the unique rows according to their order
    ## of occurrence.
    ## @end itemize
    ##
    ## @code{@var{tblB} = unique (@var{tblA}, @var{occurrence})} returns the
    ## unique rows of table @var{tblA} according to their order of occurrence.
    ## @var{occurrence} can be either @qcode{'first'} (default) or
    ## @qcode{'last'}.
    ##
    ## @itemize
    ## @item @qcode{'first'} returns the first occurrence of each unique row,
    ## i.e. the lowest possible indices are returned.
    ## @item @qcode{'last'} returns the last occurrence of each unique row, i.e.
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

      ## Handle 'setOrder' and 'occurrence' options
      opt = 'sorted';
      if (! isempty (varargin))
        if (any (strcmp (varargin{1}, {'sorted', 'stable', 'first', 'last'})))
          opt = varargin{1};
        else
          error ("table.unique: invalid option '%s'.", varargin{1});
        endif
      endif

      ## Prepare a proxy array by converting all variables to numeric proxies
      varProxy = [];
      for ix = 1:width (this)

        varVal = this.VariableValues{ix};
        if (isa (varVal, 'categorical'))
          varVal = double (varVal);
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, 'calendarDuration'))
          varVal = varVal.proxyArray;
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, 'datetime'))
          varVal = datetime_to_datenum (varVal);
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, 'duration'))
          varVal = days (varVal);
          varProxy = [varProxy, varVal];

        elseif (isa (varVal, 'string'))
          varVal = cellstr (varVal);
          [~, ~, idx] = __unique__ (varVal, 'rows');
          varProxy = [varProxy, idx];

        elseif (iscellstr (varVal))
          [~, ~, idx] = __unique__ (varVal, 'rows');
          varProxy = [varProxy, idx];

        elseif (iscell (varVal))
          ## Mixed cell data is not supported
          error (strcat ("table.unique: cannot find unique rows for", ...
                         " variables of 'cell' type."));

        elseif (isnumeric (varVal) || islogical (varVal))
          varProxy = [varProxy, varVal];

        elseif (isstruct (varVal))
          ## Structure data is not supported
          error (strcat ("table.unique: cannot find unique rows for", ...
                         " variables of 'struct' type."));

        elseif (isa (varVal, 'table'))
          try
            varVal = table2array (varVal);
            varProxy = [varProxy, varVal];
          catch
            error (strcat ("table.unique: cannot find unique rows for", ...
                           " nested tables with mixed data types."));
          end_try_catch
        endif
      endfor

      ## Find unique rows in proxy table
      [~, ia, ic] = __unique__ (varProxy, opt, 'rows');
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
    ## @var{tblA} are sorted in ascending order based on the values in the first
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
    ## rows in @var{tblA} are sorted by the elements in the variables specified
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
    ## @var{direction} must always be the 3rd input argument.  If you want to
    ## omit passing selected variables and allow @code{sortrows} to work on
    ## consecutive variables until all ties are resolved, then you can leave the
    ## second input argument empty, as in
    ## @code{sortrows (@var{tblA}, @{[]@}, @var{direction})} or pass a
    ## colon argument for @var{vars} as in
    ## @code{sortrows (@var{tblA}, @{':'@}, @var{direction})}.
    ##
    ## @code{@var{TF} = issortedrows (@dots{}, @var{Name}, @var{Value})}
    ## determines if the rows in @var{tblA} are sorted according the additional
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
    ## @var{k} rows from table @var{tblA} sorted in descending order based on
    ## all of its variables.  If elements in the first variable are repeated,
    ## then @code{topkrows} sorts by the elements in the second variable, and so
    ## on.
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
    ## indexing the desired variables.  Unlike @code{sortrows}, positive
    ## integers specify a descending order, whereas negative integers specify an
    ## ascending order for the referenced variables, consistent with the
    ## descending default of @code{topkrows}.  You can also index all available
    ## variables in @var{tblA} by passing a semicolon character argument.  This
    ## Octave specific syntax, facilitates the use of @var{direction} input
    ## argument, when no particular variable needs to be selected for sorting
    ## upon.  Additionally, @var{vars} can be a @qcode{vartype} object used to
    ## create a subscript that selects variables of a specified type.
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
    ## must always be the 3rd input argument.  If you want to omit passing
    ## selected variables and allow @code{sortrows} to work on consecutive
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
      if (! isscalar (k) || k < 0 || fix (k) != k)
        error ("table.topkrows: K must be a nonnegative integer scalar.");
      endif

      ## Unlike 'sortrows', 'topkrows' sorts in descending order by default
      ## (MATLAB compatibility).  Split off any trailing Name-Value pairs, then
      ## adjust the positional (VARS, DIRECTION) arguments so that the delegated
      ## 'sortrows' call yields descending order whenever the caller did not
      ## specify an explicit DIRECTION.
      optNames = {'MissingPlacement', 'ComparisonMethod'};
      nvStart = numel (varargin) + 1;
      for ii = 1:numel (varargin)
        if (ischar (varargin{ii}) && isrow (varargin{ii}) && ...
            any (strcmp (varargin{ii}, optNames)))
          nvStart = ii;
          break;
        endif
      endfor
      pos = varargin(1:nvStart-1);
      nv = varargin(nvStart:end);

      ## With no explicit DIRECTION (i.e. fewer than two positional arguments)
      ## enforce the descending default.
      if (numel (pos) < 2)
        if (numel (pos) == 0)
          ## No VARS: sort by all variables in descending order.
          pos = {':', 'descend'};
        elseif (isnumeric (pos{1}) && ! isempty (pos{1}))
          ## Signed numeric index: flip the sign convention relative to
          ## 'sortrows' so that a positive index sorts descending and a
          ## negative index ascending.
          pos = {-pos{1}};
        else
          ## Named / logical / vartype / ':' / [] selection: descending default.
          pos = [pos, {'descend'}];
        endif
      endif

      ## Sort the table and retain the indices
      [tbl, ix] = sortrows (this, pos{:}, nv{:});
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
    ## specified in @var{location}, which can be a character vector, a string
    ## scalar, a scalar integer value or even a logical vector of the same size
    ## as @qcode{width (@var{tblA})}, as long as it indexes a single variable in
    ## @var{tblA}.
    ##
    ## @code{@var{tblB} = addvars (@dots{}, @code{'Before'}, @var{location})}
    ## adds the new variables before, i.e. to the left, the table variable
    ## specified in @var{location}, which can be a character vector, a string
    ## scalar, a scalar integer value or even a logical vector of the same size
    ## as @qcode{width (@var{tblA})}, as long as it indexes a single variable in
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
      optNames = {'After', 'Before', 'NewVariableNames'};
      dfValues = {[], [], []};
      [After, Before, newVarNames, args] = ...
                      parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! isempty (After) && ! isempty (Before))
        error ("table.addvars: cannot use both 'After' and 'Before' options.");
      endif
      ## All other errors will be handled by 'resolveVarRef' for invalid input
      msg_error1 = "table.addvars: LOCATION must index a single variable.";
      msg_error2 = strcat ("table.addvars: LOCATION must be either a", ...
                           " scalar integer, a character vector, or a", ...
                           " logical vector indexing a single table variable.");
      if (! isempty (After))
        if ((isnumeric (After) && isscalar (After)) || ischar (After) || ...
            (isa (After, 'string') && isscalar (After)))
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
        if ((isnumeric (Before) && isscalar (Before)) || ischar (Before) || ...
            (isa (Before, 'string') && isscalar (Before)))
          ix_insert = resolveVarRef (this, Before);
          AB_insert = false;
        elseif (isvector (Before) && islogical (Before))
          ix_insert = resolveVarRef (this, Before);
          AB_insert = false;
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
          error (strcat ("table.addvars: NEWNAMES does not match the", ...
                         " number of new variables."));
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
      if (! iscellstr (newNames) && ! isa (newNames, 'string') &&
          ! (ischar (newNames) && isvector (newNames)))
        error (strcat ("table.renamevars: NEWNAMES must be either a", ...
                       " character vector, a cell array of character", ...
                       " vectors, or a string array."));
      endif

      ## Force to cellstring and get indices
      newNames = cellstr (newNames);
      if (numel (__unique__ (newNames)) != numel (newNames))
        error ("table.renamevars: NEWNAMES contains duplicate names.");
      endif
      ixVars = resolveVarRef (this, vars, 'lenient');

      ## Check selected variables
      if (any (ixVars == 0))
        error ("table.renamevars: cannot index non-existing variable: '%s'.",...
               vars{find (ixVars == 0)});
      elseif (numel (ixVars) != numel (newNames))
        error (strcat ("table.renamevars: number of names in NEWNAMES do", ...
                       " not match the selected variables specified by", ...
                       " VARS."));
      endif

      ## Rename the indexed variables
      tbl = this;
      tbl.VariableNames(ixVars) = newNames;

      ## Check for duplicate names
      if (numel (__unique__ (tbl.VariableNames)) != numel (tbl.VariableNames))
        error (strcat ("table.renamevars: newly assigned variable name", ...
                       " already exists."));
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
    ## specified in @var{location}, which can be a character vector, a string
    ## scalar, a scalar integer value or even a logical vector of the same size
    ## as @qcode{width (@var{tblA})}, as long as it indexes a single variable in
    ## @var{tblA} which is not selected by @var{vars}.
    ##
    ## @code{@var{tblB} = movevars (@dots{}, @code{'Before'}, @var{location})}
    ## moves the selected variables before, i.e. to the left, the table variable
    ## specified in @var{location}, which can be a character vector, a string
    ## scalar, a scalar integer value or even a logical vector of the same size
    ## as @qcode{width (@var{tblA})}, as long as it indexes a single variable in
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
      optNames = {'After', 'Before'};
      dfValues = {[], []};
      [After, Before] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! isempty (After) && ! isempty (Before))
        error ("table.movevars: cannot use both 'After' and 'Before' options.");
      endif

      ## All other errors will be handled by 'resolveVarRef' for invalid input
      msg_error1 = "table.movevars: LOCATION must index a single variable.";
      msg_error2 = strcat ("table.movevars: LOCATION must be either a", ...
                           " scalar integer, a character vector, or a", ...
                           " logical vector indexing a single table variable.");
      msg_error3 = strcat ("table.movevars: LOCATION does not index an", ...
                           " existing variable.");

      if (! isempty (After) || ! isempty (Before))
        if (! isempty (Before))
          AB_insert = false;
          After = Before;
        endif
        if ((isnumeric (After) && isscalar (After)) || ischar (After) || ...
            (isa (After, 'string') && isscalar (After)))
          ix_insert = resolveVarRef (this, After, 'lenient');
        elseif (isvector (After) && islogical (After))
          ix_insert = resolveVarRef (this, After, 'lenient');
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
      mvVar = resolveVarRef (this, vars, 'lenient');
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
    ## specified in @var{vars} are copied unaltered.
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
      optNames = {'NewVariableNames'};
      dfValues = {[]};
      [newNames, vars] = parsePairedArguments (optNames, dfValues, varargin(:));

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
            newnames = arrayfun (fcn, 1:col, 'UniformOutput', false);
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
                          ix_names{j}), 1:numel (ix_names))), ...
                          1:numel (ix_names));
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
                                    'UniformOutput', false);
                ix_names = [ix_names, newnames];
              else
                ix_names = [ix_names, tmp.VariableNames];
              endif
            else
              fcn = @(x) sprintf ("%s_%d", this.VariableNames{ix}, x);
              newnames = arrayfun (fcn, 1:col, 'UniformOutput', false);
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
              elseif (isa (newNames{idc}, 'string'))
                varNames = cellstr (newNames{idc});
              else
                error (strcat ("table.splitvars: invalid input for", ...
                               " 'NewVariableNames'."));
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
    ## @item @qcode{'NewVariableName'} specifies the name of the merged variable
    ## in @var{tblB}, which must be unique.  @qcode{'NewVariableName'} must be
    ## either a cellstr or string scalar or a character vector.
    ## @item @qcode{'MergeAsTable'} specifies whether the selected variables
    ## should be merged into a multicolumn variable (default) or into a table
    ## nested into a variable, which is useful for variables that cannot be
    ## concatenated due to incompatible variable types.  @qcode{'MergeAsTable'}
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
      optNames = {'NewVariableName', 'MergeAsTable'};
      dfValues = {[], false};
      [newVarName, mergeAsTable] = parsePairedArguments (optNames, dfValues, ...
                                                         varargin(:));

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
        if (isa (newVarName, 'string') && isscalar (newVarName))
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
          error (strcat ("table.mergevars: selected variables cannot be", ...
                         " merged into a multicolumn variable due to", ...
                         " incompatible variable types."));
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
    ## @code{@var{tblB} = convertvars (@var{tblA}, @var{vars}, @var{dataType})}
    ## converts the variables in @var{tblA} specified by @var{vars} to the
    ## specified data type.
    ##
    ## @var{vars} can be any of the following types.
    ## @itemize
    ## @item a character vector specifying a single variable.
    ## @item a cell array of character vectors specifying a single or multiple
    ## variables.
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## converted.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be converted.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @var{dataType} specifies the data type to convert those variables to.  It
    ## can either be a character vector defining the name of the data type to
    ## convert to or a function handle, which will perform the conversion.
    ## When specifying a name for data type conversion,
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
        error ("table.convertvars: too few input arguments.");
      endif

      if (ischar (dataType))
        if (! isvector (dataType))
          error ("table.convertvars: DATATYPE must be a character vector.");
        endif
      elseif (! isa (dataType, 'function_handle'))
        error (strcat ("table.convertvars: DATATYPE must be either a", ...
                       " character vector or a function handle; got a", ...
                       " '%s'."), ...
               class (dataType));
      endif

      ## Get variables to convert (input validation is done by 'resolveVarRef')
      [ixVars, varNames] = resolveVarRef (this, vars);
      tbl = this;

      ## Apply conversion
      for i = 1:numel (ixVars)
        try
          newVarValue = feval (dataType, this.VariableValues{ixVars(i)});
        catch
          error (strcat ("table.convertvars: specified DATATYPE", ...
                         " conversion cannot be applied on selected", ...
                         " variable '%s'."), ...
                 varNames{i});
        end_try_catch
        if (size (newVarValue, 1) != height (this))
          error (strcat ("table.convertvars: specified DATATYPE", ...
                         " conversion on '%s' does not return the", ...
                         " appropriate amount of rows."), ...
                 varNames{i});
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
    ## @item @qcode{'DataVariables'} specifies the variables from input table
    ## @var{tblA} which will be reoriented.  @qcode{'DataVariables'} can be any
    ## of the following types: a character vector specifying a single variable;
    ## a cell array of character vectors or a string array specifying a single
    ## or multiple variables; a numeric array of integer values specifying a
    ## single or multiple variables; a logical vector of the same length as the
    ## width of the input table specifying a single or multiple variables.
    ## @item @qcode{'VariableNamesSource'} specifies a single variable that
    ## contains the variable names for the output table.  The values of the
    ## selected variable must have a data type which can be converted to strings
    ## and the number of unique names in the selected variable must match the
    ## number of rows of the input table.  @qcode{'VariableNamesSource'} accepts
    ## the same data types supported by @qcode{'DataVariables'} as long as they
    ## index a single variable, which, however, must not be specified by the
    ## @qcode{'DataVariables'} ame-Value paired argument.
    ## @item @qcode{'VariableNamingRule'} must be a character vector specifying
    ## the rule for naming variables in the output table @var{tblB}.  When set
    ## to @qcode{'modify'} (default), the variable names are modified so that
    ## they are valid variable identifiers.  When set to @qcode{'preserve'}, the
    ## original names are preserved.
    ## @end itemize
    ##
    ## @end deftypefn
    function tbl = rows2vars (this, varargin)

      ## Parse optional Name-Value paired arguments
      optNames = {'DataVariables', 'VariableNamesSource', 'VariableNamingRule'};
      dfValues = {[], [], 'modify'};
      [varRef, source, rule] = parsePairedArguments (optNames, dfValues, ...
                                                     varargin(:));

      ## Check user input for 'DataVariables'
      if (! isempty (varRef))
        ixVar = resolveVarRef (this, varRef, 'lenient');
        if (any (ixVar == 0))
          varRef = cellstr (varRef);
          error (strcat ("table.rows2vars: 'DataVariables' index a", ...
                         " non-existing variable: '%s'."), ...
                 varRef{find (ixVar == 0)});
        endif
        tbl = subsetvars (this, ixVar);
      else
        tbl = this;
      endif

      ## Check user input for 'VariableNamesSource'
      if (! isempty (source))
        srcVar = resolveVarRef (this, source, 'lenient');
        if (! isscalar (srcVar))
          error (strcat ("table.rows2vars: 'VariableNamesSource' must", ...
                         " index a single variable."));
        elseif (any (srcVar == 0))
          source = cellstr (source);
          error (strcat ("table.rows2vars: 'VariableNamesSource' indexes", ...
                         " a non-existing variable: '%s'."), ...
                 source{find (srcVar == 0)});
        endif
        ## The number of names taken from the specified table variable
        ## must match the number of rows of the input table.
        newVarNames = this.VariableValues{srcVar};
        if (! iscellstr (newVarNames))
          newVarNames = cellstr (string (newVarNames));
        endif
        if (numel (__unique__ (newVarNames)) != height (this))
          error (strcat ("table.rows2vars: the number of names taken", ...
                         " from the variable specified in", ...
                         " 'VariableNamesSource' does not match the", ...
                         " number of rows in input table."));
        endif
        ## Check that 'VariableNamesSource' does not specify a variable
        ## that is specified by 'DataVariables', otherwise remove it from
        ## returning table
        if (! isempty (varRef))
          if (ismember (srcVar, ixVar))
            error (strcat ("table.rows2vars: 'VariableNamesSource'", ...
                           " cannot specify a variable that is also", ...
                           " specified by 'DataVariables'."));
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
      if (strcmpi (rule, 'modify'))
        for i = 1:numel (newVarNames)
          if (! isvarname (newVarNames{i}))
            newVarNames{i} = matlab.lang.makeValidName (newVarNames{i});
          endif
        endfor
      elseif (! strcmpi (rule, 'preserve'))
        error ("table.rows2vars: invalid input for 'VariableNamingRule'.");
      endif

      ## Check for multicolumn variables and nested tables
      for i = 1:width (tbl)
        if (isa (tbl.VariableValues{i}, 'table'))
          error (strcat ("table.rows2vars: input table must not contain", ...
                         " nested tables."));
        elseif (size (tbl.VariableValues{i}, 2) > 1)
          error (strcat ("table.rows2vars: input table must not contain", ...
                         " multicolumn variables."));
        endif
      endfor

      ## Check column types to decide whether to return arrays or cell arrays
      col_types = cellfun (@(x) class (x), tbl.VariableValues, ...
                           'UniformOutput', false);
      if (isscalar (__unique__ (col_types)))
        matrix = cat (2, tbl.VariableValues{:})';
        new_var_values = num2cell (matrix, 1);
        out = table (new_var_values{:}, 'VariableNames', newVarNames);
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
          tmp = table (matrix(:,i), 'VariableNames', newVarNames(i));
          out = [out tmp];
        endfor
      endif

      ## Merge original variable names into the table
      OriginalVariableNames = tbl.VariableNames(:);
      OriginalVariableNames = table (OriginalVariableNames);
      tbl = [OriginalVariableNames, out];

      ## Fix lengths of VariableDescriptions and VariableUnits
      tbl.VariableDescriptions = repmat ({''}, 1, size (tbl, 2));
      tbl.VariableUnits = repmat ({''}, 1, size (tbl, 2));

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, 'UniformOutput', false);
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
    ## @deftypefnx {table} {@var{tblB} =} stack (@var{tblA}, @{@var{vars1}, @dots{}, @var{varsN}@})
    ## @deftypefnx {table} {@var{tblB} =} stack (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tblB}, @var{idxA}] =} stack (@dots{})
    ##
    ## Stack multiple table variables into a single table variable.
    ##
    ## @code{@var{tblB} = stack (@var{tblA}, @var{vars})} stacks the values from
    ## the variables @var{vars} in input @var{tblA} into a single variable in
    ## output table @var{tblB}.  By default, the stacked variable in @var{tblB}
    ## is named by joining the names of the variables in @var{tblA} as defined
    ## by @var{vars}, and it inherits the units and description of the first
    ## variable in @var{vars}.  Additionally, a new categorical variable is
    ## included in @var{tblB} that indicates which variable in @var{tblA} the
    ## stacked data in each row of @var{tblB} comes from.  By default, this
    ## categorical variable is named by appending @qcode{'_Indicator'} to the
    ## name of the stacked variable.  Variables in @var{tblA} that are not
    ## defined in @var{vars} for stacking are replicated in @var{tblB}.  If
    ## @var{tblA} contains @qcode{RowNames}, these are not stacked.
    ##
    ## @code{@var{tblB} = stack (@var{tblA}, @{@var{vars1}, @dots{},
    ## @var{varsN}@})} stacks multiple groups of variables, given as a cell
    ## array of variable references, producing one stacked data variable in
    ## @var{tblB} per group (each named and metadata-inherited from its own
    ## group).  All groups must contain the same number of variables.  In this
    ## case a single indicator variable, named @qcode{'Indicator'} by default,
    ## holds the numeric position within each group of the source variable for
    ## each stacked value.
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
    ## @item @qcode{'ConstantVariables'} specifies the variables other than
    ## @var{vars} to include in the output table.  By default, all remaining
    ## variables not specified by @var{vars} are included in the output table.
    ## Specifying @qcode{'ConstantVariables'} allows you to select specific
    ## variables to replicate in @var{tblB}.  Row names in @var{tblA} are always
    ## replicated in @var{tblB}.  You can specify @qcode{'ConstantVariables'} in
    ## the same manner as with @var{vars}.
    ## @item @qcode{'NewDataVariableName'} specifies the name for the new data
    ## variable in the output table @var{tblB}.  It can be a character vector,
    ## a string scalar, or a cellstring scalar.
    ## @item @qcode{'IndexVariableName'} specifies the name for the new
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
      optNames = {'ConstantVariables', 'NewDataVariableName', ...
                  'IndexVariableName'};
      dfValues = {[], [], []};
      [constVars, newVarName, idxVarName] = ...
                  parsePairedArguments (optNames, dfValues, varargin(:));

      ## Determine single- vs multi-group stacking.  Multiple groups of
      ## variables to stack are passed as a cell array of variable references
      ## (each a cellstr, string, numeric, or logical index), producing one
      ## stacked data variable per group; a single group is any other valid
      ## variable reference.
      isMulti = iscell (vars) && ! iscellstr (vars);
      if (isMulti)
        groups = vars;
      else
        groups = {vars};
      endif
      nGroup = numel (groups);

      ## Resolve each group of variables to stack
      grpIx = cell (1, nGroup);
      grpNames = cell (1, nGroup);
      for g = 1:nGroup
        [ix, nm] = resolveVarRef (this, groups{g}, 'lenient');
        if (any (ix == 0))
          gv = cellstr (groups{g});
          error ("table.stack: VARS index a non-existing variable: '%s'.", ...
                 gv{find (ix == 0)(1)});
        endif
        grpIx{g} = ix(:)';
        grpNames{g} = nm;
      endfor

      ## All groups must contain the same number of variables
      grpSize = numel (grpIx{1});
      if (any (cellfun (@numel, grpIx) != grpSize))
        error (strcat ("table.stack: all groups of variables to stack", ...
                       " must be the same size."));
      endif
      allStackIx = [grpIx{:}];

      ## Get constant variables to include
      if (isempty (constVars))
        cIxVars = setdiff (1:width (this), allStackIx);
      else
        cIxVars = resolveVarRef (this, constVars, 'lenient');
        if (any (cIxVars == 0))
          constVars = cellstr (constVars);
          error (strcat ("table.stack: 'ConstantVariables' index a", ...
                         " non-existing  variable: '%s'."), ...
                 constVars{find (cIxVars == 0)(1)});
        endif
        if (any (ismember (cIxVars, allStackIx)))
          error (strcat ("table.stack: 'ConstantVariables' cannot", ...
                         " contain any variables to be stacked as", ...
                         " specified by VARS."));
        endif
      endif

      ## Get new data variable name(s), one per group
      if (isempty (newVarName))
        newVarName = cellfun (@(nm) strjoin (nm, '_'), grpNames, ...
                              'UniformOutput', false);
      else
        if (! ((ischar (newVarName) && isvector (newVarName)) ||
               ((iscellstr (newVarName) || isa (newVarName, 'string')) &&
                ! isempty (newVarName))))
          error (strcat ("table.stack: 'NewDataVariableName' must be a", ...
                         " character vector, or a cellstring or string", ...
                         " array."));
        endif
        newVarName = cellstr (newVarName);
        if (numel (newVarName) != nGroup)
          error (strcat ("table.stack: the number of 'NewDataVariableName'", ...
                         " names must equal the number of variable groups", ...
                         " to stack."));
        endif
      endif

      ## Get index (indicator) variable name
      if (isempty (idxVarName))
        if (isMulti)
          idxVarName = 'Indicator';
        else
          idxVarName = strcat (newVarName{1}, '_Indicator');
        endif
      else
        if (! ((ischar (idxVarName) && isvector (idxVarName)) ||
               ((iscellstr (idxVarName) || isa (idxVarName, 'string')) &&
                isscalar (idxVarName))))
          error (strcat ("table.stack: 'IndexVariableName' must be", ...
                         " either a character vector, or a cellstring or", ...
                         " string scalar."));
        endif
        idxVarName = char (idxVarName);
      endif

      ## Handle constant variables first (and RowNames if present)
      constTable = subsetvars (this, cIxVars);
      if (! isempty (this.RowNames))
        constTable.RowNames = this.RowNames;
      endif
      constTable = repelem (constTable, grpSize, 1);

      ## Build the indicator variable values.  For a single group these are the
      ## categorical names of the stacked variables; for multiple groups they
      ## are the numeric position within each group, since the variable names
      ## differ between groups.
      nRow = height (this);
      if (isMulti)
        idVarValues = repmat ((1:grpSize)', nRow, 1);
      else
        idVarValues = repmat (categorical (grpNames{1})', nRow, 1);
      endif

      ## Build one stacked data column per group
      ndCols = cell (1, nGroup);
      for g = 1:nGroup
        gvals = this.VariableValues(grpIx{g});
        ndCols{g} = vec (cat (2, gvals{:})');
      endfor

      ## Assemble the stacked table (indicator followed by the data columns)
      stackVals = [{idVarValues}, ndCols];
      stackNames = [{idxVarName}, newVarName];
      stackedTable = table (stackVals{:}, 'VariableNames', stackNames);

      ## Inherit units and descriptions for the new data variables from the
      ## first variable of each group; the indicator carries a fixed
      ## description and no units.
      ndUnits = cell (1, nGroup);
      ndDescr = cell (1, nGroup);
      for g = 1:nGroup
        ndUnits{g} = this.VariableUnits{grpIx{g}(1)};
        ndDescr{g} = this.VariableDescriptions{grpIx{g}(1)};
      endfor
      stackedTable.VariableUnits = [{''}, ndUnits];
      stackedTable.VariableDescriptions = [{'Data indicator'}, ndDescr];

      ## Merge tables
      tbl = [constTable, stackedTable];

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, 'UniformOutput', false);
      tbl.VariableTypes = new_types;

      ## Return index vector (if requested)
      if (nargout > 1)
        idxA = repelem ((1:nRow)', grpSize, 1);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} unstack (@var{tblA}, @var{vars}, @var{ivar})
    ## @deftypefnx {table} {@var{tblB} =} unstack (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tblB}, @var{idxA}] =} unstack (@dots{})
    ##
    ## Unstack a single table variable into multiple table variables.
    ##
    ## @code{@var{tblB} = unstack (@var{tblA}, @var{vars}, @var{ivar})} unstacks
    ## the values from the variables @var{vars} according to the indicator
    ## variable @var{ivar} in input @var{tblA} into multiple variables in output
    ## table @var{tblB}.  The new (unstacked) variables in @var{tblB} are named
    ## according to the unique values of the indicator variable and the rows
    ## with matching indicator values are aggregated into the new (unstacked)
    ## variables.  By default, @qcode{numeric} and @qcode{duration} data types
    ## are aggregated by summation, whereas from other data types the first
    ## unique element of each group is returned.
    ##
    ## @var{vars} may specify one or more variables of any data type supported
    ## by the @qcode{table} class except for nested tables, whereas @var{ivar}
    ## must only specify a single variable, which must be numeric, logical,
    ## categorical, string, or cellstring.  Both @var{vars} and @var{ivar} can
    ## be specified as follows:
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
    ## By default, all remaining variables in @var{tblA} which are not specified
    ## by @var{vars} and @var{ivar} are treated as grouping variables, in which
    ## case each unique combination of values in the grouping variables
    ## identifies a group of rows in @var{tblA} that is unstacked into one row
    ## of @var{tblB}.
    ##
    ## @code{@var{tblB} = unstack (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for merging table variables with the
    ## following Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{'GroupingVariables'} specifies the variables that should be
    ## used as grouping variables.  All valid schemes for indexing a table
    ## variable can be used.  If grouping variables have missing values, the
    ## data from corresponding rows are not aggregated in the output table.
    ## variables not specified by @var{vars} are included in the output table.
    ## Table row names cannot be assigned as a grouping variable, since these
    ## must be unique for each row and it kind of misses the point of unstacking
    ## a table to itself.
    ## @item @qcode{'ConstantVariables'} specifies the variables that are
    ## constant within each group.  All valid schemes for indexing a table
    ## variable can be used.  The values for these variables in the output are
    ## taken from the first row in each group in the input.  By default, no
    ## variable is treated as constant unless specified.  However, if the input
    ## table has row names, these effectively are treated as constant variable.
    ## @item @qcode{'NewDataVariableNames'} specifies the names for the new data
    ## variables in the output table @var{tblB}.  It can be a character vector,
    ## a string scalar, or a cellstring scalar.  By default, the names of the
    ## new unstacked data variables are based on the string representation of
    ## the unique values in the indicator variable @var{ivar}.  If multiple
    ## variables are unstacked, then @code{unstack} generates composite names
    ## using both the values from the indicator variable and the name of the
    ## variable being unstacked.  The number of names must match the number of
    ## unique values in the indicator variable.
    ## @item @qcode{'AggregationFunction'} specifies a function handle used to
    ## aggregate each group's data into a single value.  By default,
    ## @code{@@sum} is applied on numeric data, whereas @code{@@unique} is
    ## applied on all other supported data types, including @code{duration} and
    ## @code{calendarDuration}.  In the latter case, if a group contains more
    ## than one distinct value for the same indicator value, the default
    ## aggregation errors, and an explicit @qcode{'AggregationFunction'} that
    ## returns a scalar must be specified.
    ## @item @qcode{'VariableNamingRule'}, specified as either @qcode{'modify'}
    ## or @qcode{'preserve'}, defines the rule for naming the new unstacked
    ## variables in the output table @var{tblB}.  @qcode{'modify'} (default)
    ## forces all variable names to be a valid Octave variable names.
    ## @qcode{'preserve'} allows to preserve original names taken from the input
    ## table and can have any Unicode characters, including spaces and non-ASCII
    ## characters.
    ## @end itemize
    ##
    ## @code{[@var{tblB}, @var{idxA}] = unstack (@dots{})} also returns an index
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
      allowed = {'logical', 'string', 'categorical'};

      ## Parse optional Name-Value paired arguments
      optNames = {'GroupingVariables', 'ConstantVariables', ...
                  'NewDataVariableNames', 'AggregationFunction', ...
                  'VariableNamingRule'};
      dfValues = {[], [], [], [], 'modify'};
      [groupVars, constVars, newVarNames, aggrFcn, rule] = ...
                  parsePairedArguments (optNames, dfValues, varargin(:));

      ## Get variables to unstack
      [ixVars, ~] = resolveVarRef (this, vars, 'lenient');
      if (any (ixVars == 0))
        vars = cellstr (vars);
        error ("table.unstack: VARS index a non-existing variable: '%s'.", ...
               vars{find (ixVars == 0)});
      endif
      ## Check that variables to unstack do not contain nested tables
      for i = ixVars
        if (isa (this.VariableValues{i}, 'table'))
          error ("table.unstack: VARS must not index nested tables.");
        endif
      endfor
      ## Move variables to unstack into a new table
      VarsTable = subsetvars (this, ixVars);

      ## Get indicator variable
      [ixIvar, ~] = resolveVarRef (this, ivar, 'lenient');
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
        error (strcat ("table.unstack: IVAR cannot be any of the", ...
                       " variables to be unstacked as specified by VARS."));
      endif
      ## Check indicator variable is of a valid type
      if (! (iscellstr (IvarValues) || isnumeric (IvarValues)))
        if (! ismember (class (IvarValues), allowed))
          error (strcat ("table.unstack: IVAR indexes a variable of", ...
                         " invalid type: '%s'."), ...
                 class (IvarValues));
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
        cIxVars = resolveVarRef (this, constVars, 'lenient');
        if (any (cIxVars == 0))
          constVars = cellstr (constVars);
          error (strcat ("table.unstack: 'ConstantVariables' index a", ...
                         " non-existing variable: '%s'."), ...
                 constVars{find (cIxVars == 0)});
        endif
        if (any (ismember (cIxVars, ixVars)))
          error (strcat ("table.unstack: 'ConstantVariables' cannot", ...
                         " contain any variables to be unstacked as", ...
                         " specified by VARS."));
        endif
        if (any (ismember (cIxVars, ixIvar)))
          error (strcat ("table.unstack: 'ConstantVariables' cannot", ...
                         " contain the indicator variable as specified", ...
                         " by IVAR."));
        endif
      else
        cIxVars = [];
      endif

      ## Get grouping variables
      if (isempty (groupVars))
        gIxVars = setdiff (1:width (this), [ixVars, ixIvar, cIxVars]);
      else
        gIxVars = resolveVarRef (this, groupVars, 'lenient');
        if (any (gIxVars == 0))
          groupVars = cellstr (groupVars);
          error (strcat ("table.unstack: 'GroupingVariables' index a", ...
                         " non-existing variable: '%s'."), ...
                 groupVars{find (gIxVars == 0)});
        endif
        if (any (ismember (gIxVars, ixVars)))
          error (strcat ("table.unstack: 'GroupingVariables' cannot", ...
                         " contain any variables to be unstacked as", ...
                         " specified by VARS."));
        endif
        if (any (ismember (gIxVars, ixIvar)))
          error (strcat ("table.unstack: 'GroupingVariables' cannot", ...
                         " contain the indicator variable as specified", ...
                         " by IVAR."));
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
                      " of invalid type: '%s', which is ignored."], invalid);
          endif
        endif
      endfor

      ## Move grouping variables into a new table
      removeVar = setdiff (1:width (this), gIxVars);
      GvarTable = removevars (this, removeVar);

      ## Move constant variables into a new table
      if (! isempty (cIxVars))
        if (any (ismember (cIxVars, gIxVars)) && ! isempty (groupVars))
          error (strcat ("table.unstack: 'ConstantVariables' cannot", ...
                         " contain any grouping variables as specified", ...
                         " by 'GroupingVariables'."));
        endif
        CvarTable = subsetvars (this, cIxVars);
      else
        CvarTable = table;
      endif

      ## Get new data variable names
      if (isempty (newVarNames))
        newVarNames = IvarNames';
      else
        if (! (iscellstr (newVarNames) && ! (isa (newVarNames, 'string'))))
          error (strcat ("table.unstack: 'NewDataVariableNames' must be", ...
                         " either a cell array of character vectors, or", ...
                         " a string array."));
        endif
        if (numel (newVarNames) != numel (IvarNames))
          error (strcat ("table.unstack: 'NewDataVariableNames' do not", ...
                         " match the number of unique values in the", ...
                         " indicator variable."));
        endif
      endif

      ## Check user-defined aggregation function
      if (! isempty (aggrFcn))
        if (! is_function_handle (aggrFcn))
          error (strcat ("table.unstack: 'AggregationFunction' must be a", ...
                         " function handle."));
        endif
      endif

      ## Create table containing unique instances of grouping variables,
      ## otherwise use unique instances of the indicator variable.  Rows whose
      ## grouping variables contain missing values are excluded from unstacking,
      ## together with the corresponding indicator, data, and constant values,
      ## while the original row indices are retained for the returned index.
      if (! isempty (GvarTable))
        [GvarTable, rmRows] = rmmissing (GvarTable);
        validRows = ! rmRows;
        origIdx = find (validRows);
        IvarValues = IvarValues(validRows);
        VarsTable = subsetrows (VarsTable, origIdx);
        if (! isempty (CvarTable))
          CvarTable = subsetrows (CvarTable, origIdx);
        endif
        [GvarTable, I, J] = unique (GvarTable, 'stable');
        nrows = numel (I);
        rowIdx = origIdx(I);
      else
        [~, I, J] = __unique__ (IvarValues, 'stable', 'rows');
        nrows = 1;
        rowIdx = 1;
      endif

      ## Start unstacking here
      if (isscalar (ixVars))  # single variable to unstack
        ## Handle variable naming rule
        ncols = numel (newVarNames);
        if (strcmpi (rule, 'modify'))
          for i = 1:ncols
            if (! isvarname (newVarNames{i}))
              newVarNames{i} = matlab.lang.makeValidName (newVarNames{i});
            endif
          endfor
        elseif (! strcmpi (rule, 'preserve'))
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
        ## Check that aggregation function returns suitable output
        [mcvec, aggrFcn] = get_default_aggrFcn (vvals, nrows, aggrFcn);
        if (ischar (aggrFcn))
          error (aggrFcn);
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
        if (strcmpi (rule, 'modify'))
          for i = 1:numel (expVarNames)
            if (! isvarname (expVarNames{i}))
              expVarNames{i} = matlab.lang.makeValidName (expVarNames{i});
            endif
          endfor
        elseif (! strcmpi (rule, 'preserve'))
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

          ## Add type-specific NaN values and handle multicolumn variables.
          ## Resolve the aggregation per variable into THISAGGR so that the
          ## original AGGRFCN (or its default placeholder) is not overwritten
          ## between variables of different types.
          [mcvec, thisAggr] = get_default_aggrFcn (vvals, nrows, aggrFcn);
          if (ischar (thisAggr))
            error (thisAggr);
          endif

          ## Process each unstacked variable
          for i = 1:ncols
            UvarTable.VariableValues{vi} = mcvec;
            ix = strcmp (IvarNames{i}, IvarValues);
            if (nrows == 1)
              aggrVal = thisAggr (vvals(ix, :));
              UvarTable.VariableValues{vi} = aggrVal;
              CixRows = 1;
            else
              CixRows = [];
              for j = 1:nrows
                tmpIvarNames = IvarValues(J == j);
                ix = strcmp (IvarNames{i}, tmpIvarNames);
                if (any (ix))
                  aggrVec = ismember (tmpIvarNames, IvarNames{i});
                  aggrVal = thisAggr (vvals(J == j, :)(aggrVec,:));
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

      ## Merge output table and return index
      tbl = [GvarTable, CvarTable, UvarTable];
      idxA = rowIdx;

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} inner2outer (@var{tblA})
    ##
    ## Invert the nested hierarchy of nested tables in a table.
    ##
    ## @code{@var{tblB} = inner2outer (@var{tblA})} finds the variables in
    ## @var{tblA} that are themselves tables (nested tables) and returns a table
    ## @var{tblB} in which the inner and outer levels of nesting are transposed.
    ## The variables of the nested tables in @var{tblA} become the variables of
    ## @var{tblB}, and the variables of @var{tblA} that contain the nested
    ## tables become the variables of the nested tables in @var{tblB}.  Any
    ## variable in @var{tblA} that is not a nested table is copied unaltered
    ## into @var{tblB}.
    ##
    ## For example, if @var{tblA} has two variables @var{A} and @var{B} that
    ## both contain nested tables with the variables @var{X} and @var{Y}, then
    ## @var{tblB} has two variables @var{X} and @var{Y}, each containing a
    ## nested table with the variables @var{A} and @var{B}.  As a result, the
    ## table variables @code{tblA.A.X} and @code{tblA.B.X} are regrouped into
    ## @code{tblB.X.A} and @code{tblB.X.B}, while @code{tblA.A.Y} and
    ## @code{tblA.B.Y} are regrouped into @code{tblB.Y.A} and @code{tblB.Y.B}.
    ##
    ## The new variables of @var{tblB} are the union of the variable names of
    ## the nested tables in @var{tblA}, placed at the position of the first
    ## nested table.  An inner variable name shared by more than one nested
    ## table becomes a nested table in @var{tblB} grouping the corresponding
    ## variables; an inner variable name held by a single nested table becomes
    ## a plain variable carrying that column.
    ##
    ## @end deftypefn
    function tbl = inner2outer (this)

      ## Identify the variables that are themselves tables (nested tables)
      isNested = cellfun (@istable, this.VariableValues);
      ixNest = find (isNested);
      if (isempty (ixNest))
        error (strcat ("table.inner2outer: TBLA must have at least one", ...
                       " variable that contains a table."));
      endif

      ## The names of the nested-table variables become the variable names of
      ## the nested tables in the output.
      nestNames = this.VariableNames(ixNest);

      ## The union of the inner variable names (ordered by first appearance
      ## across the nested tables) becomes the outer variable names of the
      ## output.  Nested tables need not share the same set of names.
      allNames = {};
      for j = 1:numel (ixNest)
        allNames = [allNames, this.VariableValues{ixNest(j)}.VariableNames];
      endfor
      innerNames = __unique__ (allNames, 'stable');

      ## Build one outer variable per inner variable name.  An inner name held
      ## by more than one nested table becomes a nested table grouping those
      ## source variables (named by the source nested-table variable names),
      ## inheriting each source variable's description and units.  An inner name
      ## held by a single nested table becomes a plain variable carrying that
      ## column and its metadata.
      newVals = cell (1, numel (innerNames));
      newTypes = cell (1, numel (innerNames));
      newDesc = cell (1, numel (innerNames));
      newUnits = cell (1, numel (innerNames));
      for k = 1:numel (innerNames)
        srcJ = [];
        srcP = [];
        for j = 1:numel (ixNest)
          nt = this.VariableValues{ixNest(j)};
          p = find (strcmp (nt.VariableNames, innerNames{k}), 1);
          if (! isempty (p))
            srcJ(end+1) = j;
            srcP(end+1) = p;
          endif
        endfor
        if (numel (srcJ) == 1)
          nt = this.VariableValues{ixNest(srcJ)};
          newVals{k} = nt.VariableValues{srcP};
          newTypes{k} = nt.VariableTypes{srcP};
          newDesc{k} = nt.VariableDescriptions{srcP};
          newUnits{k} = nt.VariableUnits{srcP};
        else
          cols = cell (1, numel (srcJ));
          descs = cell (1, numel (srcJ));
          units = cell (1, numel (srcJ));
          for m = 1:numel (srcJ)
            nt = this.VariableValues{ixNest(srcJ(m))};
            cols{m} = nt.VariableValues{srcP(m)};
            descs{m} = nt.VariableDescriptions{srcP(m)};
            units{m} = nt.VariableUnits{srcP(m)};
          endfor
          nt2 = table (cols{:}, 'VariableNames', nestNames(srcJ));
          nt2.VariableDescriptions = descs;
          nt2.VariableUnits = units;
          newVals{k} = nt2;
          newTypes{k} = 'table';
          newDesc{k} = '';
          newUnits{k} = '';
        endif
      endfor

      ## Assemble the output variable order: the new outer block sits at the
      ## position of the first nested variable, the other nested variables drop
      ## out, and the non-nested variables keep their relative position.
      outNames = {};
      outVals = {};
      outTypes = {};
      outDesc = {};
      outUnits = {};
      emitted = false;
      for ix = 1:width (this)
        if (ismember (ix, ixNest))
          if (! emitted)
            for k = 1:numel (innerNames)
              outNames{end+1} = innerNames{k};
              outVals{end+1} = newVals{k};
              outTypes{end+1} = newTypes{k};
              outDesc{end+1} = newDesc{k};
              outUnits{end+1} = newUnits{k};
            endfor
            emitted = true;
          endif
        else
          outNames{end+1} = this.VariableNames{ix};
          outVals{end+1} = this.VariableValues{ix};
          outTypes{end+1} = this.VariableTypes{ix};
          outDesc{end+1} = this.VariableDescriptions{ix};
          outUnits{end+1} = this.VariableUnits{ix};
        endif
      endfor

      ## A new outer variable name must not clash with a kept non-nested one.
      if (numel (__unique__ (outNames)) != numel (outNames))
        error (strcat ("table.inner2outer: an inner variable name clashes", ...
                       " with an existing variable name in TBLA."));
      endif

      ## Build the output: preserve table-level metadata and row names; drop
      ## variable-scoped custom properties since the variable identities change.
      tbl = this;
      tbl.VariableNames = outNames;
      tbl.VariableValues = outVals;
      tbl.VariableTypes = outTypes;
      tbl.VariableDescriptions = outDesc;
      tbl.VariableUnits = outUnits;
      if (! isempty (this.CustomProperties))
        cpIdx = strcmp (this.CustomPropTypes, "variable");
        if (any (cpIdx))
          cpNames = fieldnames (this.CustomProperties);
          cpNames = cpNames(cpIdx);
          for i = 1:numel (cpNames)
            tbl.CustomProperties = rmfield (tbl.CustomProperties, cpNames{i});
          endfor
          tbl.CustomPropTypes(cpIdx) = [];
        endif
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{T} =} addprop (@var{T}, @var{propertyNames}, @var{propertyTypes})
    ##
    ## Add custom properties to a table.
    ##
    ## @code{@var{T} = addprop (@var{T}, @var{propertyNames},
    ## @var{propertyTypes})} adds properties that contain custom metadata to
    ## the table @var{T}.  The
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
      elseif (! (any (isa (Names, {'string', 'char'})) || iscellstr (Names)))
        error ("table.addprop: invalid input type for 'propertyNames'.");
      elseif (! (any (isa (Types, {'string', 'char'})) || iscellstr (Types)))
        error ("table.addprop: invalid input type for 'propertyTypes'.");
      endif

      ## Force to cellstr
      Names = cellstr (Names);
      Types = cellstr (Types);
      if (numel (Names) != numel (Types))
        error (strcat ("table.addprop: the number of 'propertyTypes'", ...
                       " must equal the number of 'propertyNames'."));
      endif

      ## Check for duplicate property names within the input
      if (numel (unique (Names)) != numel (Names))
        error (strcat ("table.addprop: 'propertyNames' cannot contain", ...
                       " duplicate names."));
      endif

      ## Check for property names that already exist
      if (! isempty (this.CustomProperties))
        existingNames = fieldnames (this.CustomProperties);
        idx = ismember (Names, existingNames);
        if (any (idx))
          error ("table.addprop: custom property '%s' already exists.", ...
                  Names{find (idx)(1)});
        endif
        offset = numel (this.CustomPropTypes);
      else
        offset = 0;
      endif

      ## Add each custom property
      for idx = 1:numel (Names)
        ## Check for valid custom property name
        if (! isvarname (Names{idx}))
          error (strcat ("table.addprop: custom property '%s' does not", ...
                         " have a valid name."), ...
                 Names{idx});
        endif
        ## Check for valid custom property type
        if (! any (strcmp (Types{idx}, {'table', 'variable'})))
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
    ## character vectors, or a string array.  Names that do not match any
    ## existing custom property are silently ignored.
    ##
    ## @end deftypefn
    function tbl = rmprop (this, Names)

      ## Check input arguments
      if (nargin < 2)
        error ("table.rmprop: too few input arguments.");
      elseif (! (any (isa (Names, {'string', 'char'})) || iscellstr (Names)))
        error ("table.rmprop: invalid input type for 'propertyNames'.");
      endif

      ## Force to cellstr
      Names = cellstr (Names);

      ## Remove the referenced custom properties that exist; names that do not
      ## match any existing custom property (including repeated names) are
      ## silently ignored, matching MATLAB.
      if (! isempty (this.CustomProperties))
        existingNames = fieldnames (this.CustomProperties);
        tf = ismember (existingNames, Names);
        if (any (tf))
          this.CustomProperties = rmfield (this.CustomProperties, ...
                                           existingNames(tf));
          this.CustomPropTypes(tf) = [];
        endif
      endif
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

  methods (Hidden)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} join (@var{tblL}, @var{tblR})
    ## @deftypefnx {table} {@var{tbl} =} join (@var{tblL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixR}] =} join (@dots{})
    ##
    ## Combine two tables by rows using key variables.
    ##
    ## @code{@var{tbl} = join (@var{tblL}, @var{tblR})} combines @var{tblL} and
    ## @var{tblR} by matching the values of their @emph{key variables}, which by
    ## default are the variables that share the same name in both tables.
    ## @var{tbl} contains one row for each row of @var{tblL}, in the same order;
    ## each is completed with the single row of @var{tblR} whose key variables
    ## match.  The key variables of @var{tblR} must contain unique combinations of
    ## values, and every key combination in @var{tblL} must be present in
    ## @var{tblR}.
    ##
    ## By default @var{tbl} contains all the variables of @var{tblL} followed by
    ## the non-key variables of @var{tblR}.  Whenever a non-key variable name
    ## appears in both tables, a suffix derived from each input's argument name is
    ## appended to the conflicting names (for inputs named @var{tblL} and
    ## @var{tblR}, the suffixes @qcode{'_tblL'} and @qcode{'_tblR'}; when an input
    ## has no name, @qcode{'_left'} and @qcode{'_right'} are used).  The row names
    ## of @var{tblL}, if any, are preserved.
    ##
    ## @code{@var{tbl} = join (@var{tblL}, @var{tblR}, @var{Name}, @var{Value})}
    ## customizes the join with the following options:
    ##
    ## @table @asis
    ## @item @qcode{'Keys'}
    ## Variables to use as keys in both tables, given as variable names or
    ## indices.  It cannot be combined with @qcode{'LeftKeys'} or
    ## @qcode{'RightKeys'}.
    ##
    ## @item @qcode{'LeftKeys'}, @qcode{'RightKeys'}
    ## Variables to use as keys in @var{tblL} and @var{tblR}, respectively, when
    ## the key variables have different names.  They must be specified together
    ## and reference the same number of variables.
    ##
    ## @item @qcode{'LeftVariables'}, @qcode{'RightVariables'}
    ## Variables of @var{tblL} and @var{tblR} to include in @var{tbl}.  By default
    ## @qcode{'LeftVariables'} is all the variables of @var{tblL} and
    ## @qcode{'RightVariables'} is the non-key variables of @var{tblR}.
    ##
    ## @item @qcode{'KeepOneCopy'}
    ## Names of non-key variables that occur in both tables for which only the
    ## copy from @var{tblL} is kept (no suffix is added and the @var{tblR} copy is
    ## dropped).
    ## @end table
    ##
    ## @code{[@var{tbl}, @var{ixR}] = join (@dots{})} also returns the index vector
    ## @var{ixR} that identifies, for each row of @var{tbl}, the matching row of
    ## @var{tblR}.
    ##
    ## @end deftypefn
    function [tbl, ixR] = join (tblL, tblR, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("table.join: too few input arguments.");
      endif
      if (! istable (tblL) || ! istable (tblR))
        error ("table.join: both inputs must be tables.");
      endif

      ## Parse Name/Value options
      optNames = {'Keys', 'LeftKeys', 'RightKeys', 'LeftVariables', ...
                  'RightVariables', 'KeepOneCopy'};
      dfValues = {[], [], [], [], [], []};
      [Keys, LeftKeys, RightKeys, LeftVariables, RightVariables, KeepOneCopy, ...
       rem] = parsePairedArguments (optNames, dfValues, varargin(:));
      if (! isempty (rem))
        error ("table.join: invalid optional input argument.");
      endif

      ## Resolve key variables on each side
      if (! isempty (Keys))
        if (! isempty (LeftKeys) || ! isempty (RightKeys))
          error (strcat ("table.join: 'Keys' cannot be combined with", ...
                         " 'LeftKeys' or 'RightKeys'."));
        endif
        lKeyIdx = resolveVarRef (tblL, Keys);
        rKeyIdx = resolveVarRef (tblR, Keys);
      elseif (! isempty (LeftKeys) || ! isempty (RightKeys))
        if (isempty (LeftKeys) || isempty (RightKeys))
          error (strcat ("table.join: 'LeftKeys' and 'RightKeys' must be", ...
                         " specified together."));
        endif
        lKeyIdx = resolveVarRef (tblL, LeftKeys);
        rKeyIdx = resolveVarRef (tblR, RightKeys);
        if (numel (lKeyIdx) != numel (rKeyIdx))
          error (strcat ("table.join: 'LeftKeys' and 'RightKeys' must", ...
                         " reference the same number of variables."));
        endif
      else
        ## Default keys are the variables common to both tables (left order)
        isCommon = ismember (tblL.VariableNames, tblR.VariableNames);
        lKeyIdx = find (isCommon);
        if (isempty (lKeyIdx))
          error (strcat ("table.join: cannot find any common key variables", ...
                         " between the two tables."));
        endif
        [~, rKeyIdx] = ismember (tblL.VariableNames(lKeyIdx), ...
                                 tblR.VariableNames);
      endif

      ## Resolve output variables on each side
      if (isempty (LeftVariables))
        lVarIdx = 1:width (tblL);
      else
        lVarIdx = resolveVarRef (tblL, LeftVariables);
      endif
      if (isempty (RightVariables))
        rVarIdx = setdiff (1:width (tblR), rKeyIdx);
      else
        rVarIdx = resolveVarRef (tblR, RightVariables);
      endif

      ## Drop the right copy of any 'KeepOneCopy' variable shared with the left
      if (! isempty (KeepOneCopy))
        keepNames = cellstr (KeepOneCopy);
        rNames = tblR.VariableNames(rVarIdx);
        lNames = tblL.VariableNames(lVarIdx);
        dropMask = ismember (rNames, keepNames) & ismember (rNames, lNames);
        rVarIdx(dropMask) = [];
      endif

      ## Build consistent numeric key proxies for both tables
      leftProxy = [];
      rightProxy = [];
      for k = 1:numel (lKeyIdx)
        lcol = tblL.VariableValues{lKeyIdx(k)};
        rcol = tblR.VariableValues{rKeyIdx(k)};
        [lp, rp, errmsg] = key_col_proxy (lcol, rcol);
        if (! isempty (errmsg))
          error ("table.join: %s", errmsg);
        endif
        leftProxy = [leftProxy, lp];
        rightProxy = [rightProxy, rp];
      endfor

      ## The right key combinations must be unique
      if (rows (unique (rightProxy, 'rows')) != rows (rightProxy))
        error (strcat ("table.join: the key variables of TBLR must contain", ...
                       " unique combinations of values."));
      endif

      ## Match each left row to its unique right row
      [tf, ixR] = ismember (leftProxy, rightProxy, 'rows');
      if (! all (tf))
        error (strcat ("table.join: the key variables of TBLR must contain", ...
                       " all values of the key variables of TBLL."));
      endif

      ## Assemble the output: all selected left rows + matched right rows
      Lpart = subsetvars (tblL, lVarIdx);
      Rpart = subsetrows (subsetvars (tblR, rVarIdx), ixR);
      Rpart.RowNames = {};
      ## Suffix any non-key variable names shared by both sides
      shared = intersect (Lpart.VariableNames, Rpart.VariableNames);
      if (! isempty (shared))
        [lsuf, rsuf] = join_suffixes (inputname (1), inputname (2));
        lNames = Lpart.VariableNames;
        rNames = Rpart.VariableNames;
        for i = find (ismember (lNames, shared))
          lNames{i} = [lNames{i}, lsuf];
        endfor
        for i = find (ismember (rNames, shared))
          rNames{i} = [rNames{i}, rsuf];
        endfor
        Lpart.VariableNames = lNames;
        Rpart.VariableNames = rNames;
      endif
      tbl = horzcat (Lpart, Rpart);

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} innerjoin (@var{tblL}, @var{tblR})
    ## @deftypefnx {table} {@var{tbl} =} innerjoin (@var{tblL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixL}, @var{ixR}] =} innerjoin (@dots{})
    ##
    ## Inner join between two tables by rows using key variables.
    ##
    ## @code{@var{tbl} = innerjoin (@var{tblL}, @var{tblR})} combines the tables
    ## @var{tblL} and @var{tblR} by matching the values of their @emph{key
    ## variables}, which by default are the variables that share the same name in
    ## both tables.  Each row of @var{tbl} is formed by horizontally
    ## concatenating a row of @var{tblL} with a row of @var{tblR} whose key
    ## variables share the same combination of values.  If @math{m} rows in
    ## @var{tblL} and @math{n} rows in @var{tblR} share the same key combination,
    ## then @var{tbl} contains all @math{m*n} pairings for that combination.  The
    ## rows of @var{tbl} are sorted by the values of the key variables, and any
    ## row names are dropped.
    ##
    ## By default @var{tbl} contains all the variables of @var{tblL} followed by
    ## the non-key variables of @var{tblR}.  Whenever a non-key variable name
    ## appears in both tables, a suffix derived from each input's argument name
    ## is appended to the conflicting names in @var{tbl} (for inputs named
    ## @var{tblL} and @var{tblR}, the suffixes @qcode{'_tblL'} and
    ## @qcode{'_tblR'}; when an input has no name, @qcode{'_left'} and
    ## @qcode{'_right'} are used).
    ##
    ## @code{@var{tbl} = innerjoin (@var{tblL}, @var{tblR}, @var{Name},
    ## @var{Value})} customizes the join with the following options:
    ##
    ## @table @asis
    ## @item @qcode{'Keys'}
    ## Variables to use as keys in both tables, given as variable names or
    ## indices.  It cannot be combined with @qcode{'LeftKeys'} or
    ## @qcode{'RightKeys'}.
    ##
    ## @item @qcode{'LeftKeys'}, @qcode{'RightKeys'}
    ## Variables to use as keys in @var{tblL} and @var{tblR}, respectively, when
    ## the key variables have different names.  They must be specified together
    ## and reference the same number of variables.
    ##
    ## @item @qcode{'LeftVariables'}, @qcode{'RightVariables'}
    ## Variables of @var{tblL} and @var{tblR} to include in @var{tbl}.  They may
    ## include or exclude key variables.  By default @qcode{'LeftVariables'} is
    ## all the variables of @var{tblL} and @qcode{'RightVariables'} is the
    ## non-key variables of @var{tblR}.
    ## @end table
    ##
    ## @code{[@var{tbl}, @var{ixL}, @var{ixR}] = innerjoin (@dots{})} also returns
    ## the row-index vectors @var{ixL} and @var{ixR} such that @var{tbl} is the
    ## horizontal concatenation of @code{@var{tblL}(@var{ixL}, leftVars)} and
    ## @code{@var{tblR}(@var{ixR}, rightVars)}.
    ##
    ## @end deftypefn
    function [tbl, ixL, ixR] = innerjoin (tblL, tblR, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("table.innerjoin: too few input arguments.");
      endif
      if (! istable (tblL) || ! istable (tblR))
        error ("table.innerjoin: both inputs must be tables.");
      endif

      ## Parse Name/Value options
      optNames = {'Keys', 'LeftKeys', 'RightKeys', 'LeftVariables', ...
                  'RightVariables'};
      dfValues = {[], [], [], [], []};
      [Keys, LeftKeys, RightKeys, LeftVariables, RightVariables, rem] = ...
        parsePairedArguments (optNames, dfValues, varargin(:));
      if (! isempty (rem))
        error ("table.innerjoin: invalid optional input argument.");
      endif

      ## Resolve key variables on each side
      if (! isempty (Keys))
        if (! isempty (LeftKeys) || ! isempty (RightKeys))
          error (strcat ("table.innerjoin: 'Keys' cannot be combined with", ...
                         " 'LeftKeys' or 'RightKeys'."));
        endif
        lKeyIdx = resolveVarRef (tblL, Keys);
        rKeyIdx = resolveVarRef (tblR, Keys);
      elseif (! isempty (LeftKeys) || ! isempty (RightKeys))
        if (isempty (LeftKeys) || isempty (RightKeys))
          error (strcat ("table.innerjoin: 'LeftKeys' and 'RightKeys' must", ...
                         " be specified together."));
        endif
        lKeyIdx = resolveVarRef (tblL, LeftKeys);
        rKeyIdx = resolveVarRef (tblR, RightKeys);
        if (numel (lKeyIdx) != numel (rKeyIdx))
          error (strcat ("table.innerjoin: 'LeftKeys' and 'RightKeys' must", ...
                         " reference the same number of variables."));
        endif
      else
        ## Default keys are the variables common to both tables (left order)
        isCommon = ismember (tblL.VariableNames, tblR.VariableNames);
        lKeyIdx = find (isCommon);
        if (isempty (lKeyIdx))
          error (strcat ("table.innerjoin: cannot find any common key", ...
                         " variables between the two tables."));
        endif
        [~, rKeyIdx] = ismember (tblL.VariableNames(lKeyIdx), ...
                                 tblR.VariableNames);
      endif

      ## Resolve output variables on each side
      if (isempty (LeftVariables))
        lVarIdx = 1:width (tblL);
      else
        lVarIdx = resolveVarRef (tblL, LeftVariables);
      endif
      if (isempty (RightVariables))
        rVarIdx = setdiff (1:width (tblR), rKeyIdx);
      else
        rVarIdx = resolveVarRef (tblR, RightVariables);
      endif

      ## Build consistent numeric key proxies for both tables
      leftProxy = [];
      rightProxy = [];
      for k = 1:numel (lKeyIdx)
        lcol = tblL.VariableValues{lKeyIdx(k)};
        rcol = tblR.VariableValues{rKeyIdx(k)};
        [lp, rp, errmsg] = key_col_proxy (lcol, rcol);
        if (! isempty (errmsg))
          error ("table.innerjoin: %s", errmsg);
        endif
        leftProxy = [leftProxy, lp];
        rightProxy = [rightProxy, rp];
      endfor

      ## Match key rows and lay out the Cartesian product, key-sorted
      Nl = height (tblL);
      [uKeys, ~, ic] = unique ([leftProxy; rightProxy], 'rows');
      icL = ic(1:Nl);
      icR = ic(Nl+1:end);
      ixL = [];
      ixR = [];
      for g = 1:rows (uKeys)
        lr = find (icL == g);
        rr = find (icR == g);
        if (! isempty (lr) && ! isempty (rr))
          ixL = [ixL; repelem(lr(:), numel (rr))];
          ixR = [ixR; repmat(rr(:), numel (lr), 1)];
        endif
      endfor

      ## Assemble the output table
      Lpart = subsetrows (subsetvars (tblL, lVarIdx), ixL);
      Rpart = subsetrows (subsetvars (tblR, rVarIdx), ixR);
      Lpart.RowNames = {};
      Rpart.RowNames = {};
      ## Suffix any non-key variable names shared by both sides
      shared = intersect (Lpart.VariableNames, Rpart.VariableNames);
      if (! isempty (shared))
        [lsuf, rsuf] = join_suffixes (inputname (1), inputname (2));
        lNames = Lpart.VariableNames;
        rNames = Rpart.VariableNames;
        for i = find (ismember (lNames, shared))
          lNames{i} = [lNames{i}, lsuf];
        endfor
        for i = find (ismember (rNames, shared))
          rNames{i} = [rNames{i}, rsuf];
        endfor
        Lpart.VariableNames = lNames;
        Rpart.VariableNames = rNames;
      endif
      tbl = horzcat (Lpart, Rpart);

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} outerjoin (@var{tblL}, @var{tblR})
    ## @deftypefnx {table} {@var{tbl} =} outerjoin (@var{tblL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixL}, @var{ixR}] =} outerjoin (@dots{})
    ##
    ## Outer join between two tables by rows using key variables.
    ##
    ## @code{@var{tbl} = outerjoin (@var{tblL}, @var{tblR})} combines the tables
    ## @var{tblL} and @var{tblR} by matching the values of their @emph{key
    ## variables}, which by default are the variables that share the same name in
    ## both tables.  Unlike @code{innerjoin}, an outer join also keeps the rows of
    ## each table that have no match in the other table, filling the variables
    ## taken from the non-matching table with missing values (@qcode{NaN},
    ## @qcode{NaT}, @qcode{<undefined>}, empty string, etc., as appropriate).  If
    ## @math{m} rows in @var{tblL} and @math{n} rows in @var{tblR} share the same
    ## key combination, then @var{tbl} contains all @math{m*n} pairings for that
    ## combination.  The rows of @var{tbl} are sorted by the values of the key
    ## variables and any row names are dropped.
    ##
    ## By default @var{tbl} contains all the variables of @var{tblL} followed by
    ## all the variables of @var{tblR}.  Because the key variables are kept from
    ## both tables, conflicting names receive a suffix derived from each input's
    ## argument name (for inputs named @var{tblL} and @var{tblR}, the suffixes
    ## @qcode{'_tblL'} and @qcode{'_tblR'}; when an input has no name,
    ## @qcode{'_left'} and @qcode{'_right'} are used).  See @qcode{'MergeKeys'} to
    ## combine the keys into single columns instead.
    ##
    ## @code{@var{tbl} = outerjoin (@var{tblL}, @var{tblR}, @var{Name},
    ## @var{Value})} customizes the join with the following options:
    ##
    ## @table @asis
    ## @item @qcode{'Type'}
    ## The type of outer join: @qcode{'full'} (default) keeps unmatched rows from
    ## both tables, @qcode{'left'} keeps all rows of @var{tblL} and only matching
    ## rows of @var{tblR}, and @qcode{'right'} keeps all rows of @var{tblR} and
    ## only matching rows of @var{tblL}.
    ##
    ## @item @qcode{'MergeKeys'}
    ## A logical scalar (default @qcode{false}).  When @qcode{true}, each pair of
    ## key variables is merged into a single variable that takes the value from
    ## @var{tblL} where a matching left row exists and from @var{tblR} otherwise.
    ## The merged variable is named after the left key when both keys share the
    ## same name, or @qcode{'leftName_rightName'} when their names differ.
    ##
    ## @item @qcode{'Keys'}
    ## Variables to use as keys in both tables, given as variable names or
    ## indices.  It cannot be combined with @qcode{'LeftKeys'} or
    ## @qcode{'RightKeys'}.
    ##
    ## @item @qcode{'LeftKeys'}, @qcode{'RightKeys'}
    ## Variables to use as keys in @var{tblL} and @var{tblR}, respectively, when
    ## the key variables have different names.  They must be specified together
    ## and reference the same number of variables.
    ##
    ## @item @qcode{'LeftVariables'}, @qcode{'RightVariables'}
    ## Variables of @var{tblL} and @var{tblR} to include in @var{tbl}.  By default
    ## all the variables of each table are included.
    ## @end table
    ##
    ## @code{[@var{tbl}, @var{ixL}, @var{ixR}] = outerjoin (@dots{})} also returns
    ## the row-index vectors @var{ixL} and @var{ixR} that identify the row of
    ## @var{tblL} and @var{tblR}, respectively, corresponding to each row of
    ## @var{tbl}.  A zero indicates a row of @var{tbl} that has no corresponding
    ## row in that table.
    ##
    ## @end deftypefn
    function [tbl, ixL, ixR] = outerjoin (tblL, tblR, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("table.outerjoin: too few input arguments.");
      endif
      if (! istable (tblL) || ! istable (tblR))
        error ("table.outerjoin: both inputs must be tables.");
      endif

      ## Parse Name/Value options
      optNames = {'Keys', 'LeftKeys', 'RightKeys', 'LeftVariables', ...
                  'RightVariables', 'Type', 'MergeKeys'};
      dfValues = {[], [], [], [], [], 'full', false};
      [Keys, LeftKeys, RightKeys, LeftVariables, RightVariables, Type, ...
       MergeKeys, rem] = parsePairedArguments (optNames, dfValues, varargin(:));
      if (! isempty (rem))
        error ("table.outerjoin: invalid optional input argument.");
      endif

      ## Validate 'Type' and 'MergeKeys'
      if (! (ischar (Type) && isrow (Type))
          || ! any (strcmpi (Type, {'full', 'left', 'right'})))
        error (strcat ("table.outerjoin: 'Type' must be 'full', 'left', or", ...
                       " 'right'."));
      endif
      Type = lower (Type);
      if (! (islogical (MergeKeys) && isscalar (MergeKeys)))
        error ("table.outerjoin: 'MergeKeys' must be a logical scalar.");
      endif

      ## Resolve key variables on each side
      if (! isempty (Keys))
        if (! isempty (LeftKeys) || ! isempty (RightKeys))
          error (strcat ("table.outerjoin: 'Keys' cannot be combined with", ...
                         " 'LeftKeys' or 'RightKeys'."));
        endif
        lKeyIdx = resolveVarRef (tblL, Keys);
        rKeyIdx = resolveVarRef (tblR, Keys);
      elseif (! isempty (LeftKeys) || ! isempty (RightKeys))
        if (isempty (LeftKeys) || isempty (RightKeys))
          error (strcat ("table.outerjoin: 'LeftKeys' and 'RightKeys' must", ...
                         " be specified together."));
        endif
        lKeyIdx = resolveVarRef (tblL, LeftKeys);
        rKeyIdx = resolveVarRef (tblR, RightKeys);
        if (numel (lKeyIdx) != numel (rKeyIdx))
          error (strcat ("table.outerjoin: 'LeftKeys' and 'RightKeys' must", ...
                         " reference the same number of variables."));
        endif
      else
        ## Default keys are the variables common to both tables (left order)
        isCommon = ismember (tblL.VariableNames, tblR.VariableNames);
        lKeyIdx = find (isCommon);
        if (isempty (lKeyIdx))
          error (strcat ("table.outerjoin: cannot find any common key", ...
                         " variables between the two tables."));
        endif
        [~, rKeyIdx] = ismember (tblL.VariableNames(lKeyIdx), ...
                                 tblR.VariableNames);
      endif

      ## Resolve output variables (defaults: all variables of each table)
      if (isempty (LeftVariables))
        lVarIdx = 1:width (tblL);
      else
        lVarIdx = resolveVarRef (tblL, LeftVariables);
      endif
      if (isempty (RightVariables))
        rVarIdx = 1:width (tblR);
      else
        rVarIdx = resolveVarRef (tblR, RightVariables);
      endif

      ## Build consistent numeric key proxies for both tables
      leftProxy = [];
      rightProxy = [];
      for k = 1:numel (lKeyIdx)
        lcol = tblL.VariableValues{lKeyIdx(k)};
        rcol = tblR.VariableValues{rKeyIdx(k)};
        [lp, rp, errmsg] = key_col_proxy (lcol, rcol);
        if (! isempty (errmsg))
          error ("table.outerjoin: %s", errmsg);
        endif
        leftProxy = [leftProxy, lp];
        rightProxy = [rightProxy, rp];
      endfor

      ## Match key rows, producing zero-filled index vectors per join type
      Nl = height (tblL);
      [uKeys, ~, ic] = unique ([leftProxy; rightProxy], 'rows');
      icL = ic(1:Nl);
      icR = ic(Nl+1:end);
      keepL = any (strcmp (Type, {'full', 'left'}));
      keepR = any (strcmp (Type, {'full', 'right'}));
      ixL = [];
      ixR = [];
      for g = 1:rows (uKeys)
        lr = find (icL == g);
        rr = find (icR == g);
        nl = numel (lr);
        nr = numel (rr);
        if (nl > 0 && nr > 0)
          ixL = [ixL; repelem(lr(:), nr)];
          ixR = [ixR; repmat(rr(:), nl, 1)];
        elseif (nl > 0 && keepL)
          ixL = [ixL; lr(:)];
          ixR = [ixR; zeros(nl, 1)];
        elseif (nr > 0 && keepR)
          ixL = [ixL; zeros(nr, 1)];
          ixR = [ixR; rr(:)];
        endif
      endfor

      ## Assemble each side, filling unmatched rows with missing values
      [Lout, emsg] = joinBuildSide (subsetvars (tblL, lVarIdx), ixL);
      if (! isempty (emsg))
        error ("table.outerjoin: %s", emsg);
      endif
      [Rout, emsg] = joinBuildSide (subsetvars (tblR, rVarIdx), ixR);
      if (! isempty (emsg))
        error ("table.outerjoin: %s", emsg);
      endif

      ## Optionally merge each key pair into a single variable.  A merged key
      ## keeps the left position; its name is the left key name when both keys
      ## share it, or 'leftName_rightName' when they differ.
      if (MergeKeys)
        [tfL, posL] = ismember (lKeyIdx, lVarIdx);
        [tfR, posR] = ismember (rKeyIdx, rVarIdx);
        dropR = [];
        fillRows = (ixL == 0);
        lNames = Lout.VariableNames;
        for k = 1:numel (lKeyIdx)
          if (tfL(k) && tfR(k))
            mcol = Lout.VariableValues{posL(k)};
            rcol = Rout.VariableValues{posR(k)};
            mcol(fillRows,:) = rcol(fillRows,:);
            Lout.VariableValues{posL(k)} = mcol;
            lkn = tblL.VariableNames{lKeyIdx(k)};
            rkn = tblR.VariableNames{rKeyIdx(k)};
            if (! strcmp (lkn, rkn))
              lNames{posL(k)} = [lkn, '_', rkn];
            endif
            dropR = [dropR, posR(k)];
          endif
        endfor
        Lout.VariableNames = lNames;
        if (! isempty (dropR))
          Rout = subsetvars (Rout, setdiff (1:width (Rout), dropR));
        endif
      endif

      ## Suffix any variable names shared by both sides
      shared = intersect (Lout.VariableNames, Rout.VariableNames);
      if (! isempty (shared))
        [lsuf, rsuf] = join_suffixes (inputname (1), inputname (2));
        lNames = Lout.VariableNames;
        rNames = Rout.VariableNames;
        for i = find (ismember (lNames, shared))
          lNames{i} = [lNames{i}, lsuf];
        endfor
        for i = find (ismember (rNames, shared))
          rNames{i} = [rNames{i}, rsuf];
        endfor
        Lout.VariableNames = lNames;
        Rout.VariableNames = rNames;
      endif
      tbl = horzcat (Lout, Rout);

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} union (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} union (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}, @var{ixB}] =} union (@dots{})
    ##
    ## Union of two tables by rows.
    ##
    ## @code{@var{tbl} = union (@var{tblA}, @var{tblB})} returns the combined set
    ## of rows of @var{tblA} and @var{tblB}, with duplicate rows removed.  Both
    ## tables must have the same variable names, although not necessarily in the
    ## same order; @var{tbl} keeps the variable order of @var{tblA}.  Rows are
    ## compared by their variable values only (row names are ignored), and by
    ## default @var{tbl} is sorted by those values.
    ##
    ## @code{@var{tbl} = union (@var{tblA}, @var{tblB}, @var{setOrder})} controls
    ## the ordering of @var{tbl}.  @var{setOrder} is either @qcode{'sorted'}
    ## (default) for ascending order, or @qcode{'stable'} to keep the order in
    ## which the rows appear in @var{tblA} and @var{tblB}.
    ##
    ## @code{[@var{tbl}, @var{ixA}, @var{ixB}] = union (@dots{})} also returns the
    ## index vectors @var{ixA} and @var{ixB} such that @var{tbl} is the vertical
    ## concatenation of @code{@var{tblA}(@var{ixA},:)} and
    ## @code{@var{tblB}(@var{ixB},:)}.
    ##
    ## @end deftypefn
    function [tbl, ixA, ixB] = union (tblA, tblB, varargin)
      if (nargin < 2)
        error ("table.union: too few input arguments.");
      endif
      if (! istable (tblA) || ! istable (tblB))
        error ("table.union: both inputs must be tables.");
      endif
      [order, emsg] = parse_set_order (varargin);
      if (! isempty (emsg))
        error ("table.union: %s", emsg);
      endif
      [proxyA, proxyB, emsg] = rowProxies (tblA, tblB);
      if (! isempty (emsg))
        error ("table.union: %s", emsg);
      endif
      [keyU, ixA, ixB] = union (proxyA, proxyB, 'rows', order);
      ## ixA, ixB list A's then B's contributions, but the result row order
      ## interleaves them per SETORDER, so reorder the assembled rows to the
      ## result's own order.  Row names are dropped: rows are drawn from both
      ## tables and cannot be attributed to a single input (like MATLAB).
      sA = subsetrows (tblA, ixA);
      sB = subsetrows (tblB, ixB);
      sA.RowNames = {};
      sB.RowNames = {};
      sel = vertcat (sA, sB);
      [~, perm] = ismember (keyU, [proxyA(ixA,:); proxyB(ixB,:)], 'rows');
      tbl = subsetrows (sel, perm);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} intersect (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} intersect (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}, @var{ixB}] =} intersect (@dots{})
    ##
    ## Intersection of two tables by rows.
    ##
    ## @code{@var{tbl} = intersect (@var{tblA}, @var{tblB})} returns the set of
    ## rows common to both @var{tblA} and @var{tblB}, with duplicate rows removed.
    ## Both tables must have the same variable names, although not necessarily in
    ## the same order; @var{tbl} keeps the variable order of @var{tblA}.  Rows are
    ## compared by their variable values only (row names are ignored), and by
    ## default @var{tbl} is sorted by those values.
    ##
    ## @code{@var{tbl} = intersect (@var{tblA}, @var{tblB}, @var{setOrder})}
    ## controls the ordering of @var{tbl}, either @qcode{'sorted'} (default) or
    ## @qcode{'stable'}.
    ##
    ## @code{[@var{tbl}, @var{ixA}, @var{ixB}] = intersect (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that @var{tbl} equals
    ## @code{@var{tblA}(@var{ixA},:)} and @code{@var{tblB}(@var{ixB},:)}.
    ##
    ## @end deftypefn
    function [tbl, ixA, ixB] = intersect (tblA, tblB, varargin)
      if (nargin < 2)
        error ("table.intersect: too few input arguments.");
      endif
      if (! istable (tblA) || ! istable (tblB))
        error ("table.intersect: both inputs must be tables.");
      endif
      [order, emsg] = parse_set_order (varargin);
      if (! isempty (emsg))
        error ("table.intersect: %s", emsg);
      endif
      [proxyA, proxyB, emsg] = rowProxies (tblA, tblB);
      if (! isempty (emsg))
        error ("table.intersect: %s", emsg);
      endif
      [~, ixA, ixB] = intersect (proxyA, proxyB, 'rows', order);
      tbl = subsetrows (tblA, ixA);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{TF} =} ismember (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {[@var{TF}, @var{ixB}] =} ismember (@var{tblA}, @var{tblB})
    ##
    ## Find set members between two tables by rows.
    ##
    ## @code{@var{TF} = ismember (@var{tblA}, @var{tblB})} returns a logical
    ## column vector @var{TF} with one element per row of @var{tblA}, where
    ## @code{@var{TF}(i)} is @qcode{true} when the @math{i}-th row of @var{tblA}
    ## also appears as a row of @var{tblB}.  Both tables must have the same
    ## variable names, although not necessarily in the same order, and rows are
    ## compared by their variable values only (row names are ignored).
    ##
    ## @code{[@var{TF}, @var{ixB}] = ismember (@var{tblA}, @var{tblB})} also
    ## returns a column vector @var{ixB} containing, for each row of @var{tblA},
    ## the index of the lowest matching row in @var{tblB}, or @qcode{0} if there
    ## is no match.
    ##
    ## @end deftypefn
    function [TF, ixB] = ismember (tblA, tblB)
      if (nargin < 2)
        error ("table.ismember: too few input arguments.");
      endif
      if (! istable (tblA) || ! istable (tblB))
        error ("table.ismember: both inputs must be tables.");
      endif
      [proxyA, proxyB, emsg] = rowProxies (tblA, tblB);
      if (! isempty (emsg))
        error ("table.ismember: %s", emsg);
      endif
      [TF, ixB] = ismember (proxyA, proxyB, 'rows');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} setdiff (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} setdiff (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}] =} setdiff (@dots{})
    ##
    ## Difference between two tables by rows.
    ##
    ## @code{@var{tbl} = setdiff (@var{tblA}, @var{tblB})} returns the set of rows
    ## that are present in @var{tblA} but not in @var{tblB}, with duplicate rows
    ## removed.  Both tables must have the same variable names, although not
    ## necessarily in the same order; @var{tbl} keeps the variable order of
    ## @var{tblA}.  Rows are compared by their variable values only (row names are
    ## ignored), and by default @var{tbl} is sorted by those values.
    ##
    ## @code{@var{tbl} = setdiff (@var{tblA}, @var{tblB}, @var{setOrder})}
    ## controls the ordering of @var{tbl}, either @qcode{'sorted'} (default) or
    ## @qcode{'stable'}.
    ##
    ## @code{[@var{tbl}, @var{ixA}] = setdiff (@dots{})} also returns the index
    ## vector @var{ixA} such that @var{tbl} equals @code{@var{tblA}(@var{ixA},:)}.
    ##
    ## @end deftypefn
    function [tbl, ixA] = setdiff (tblA, tblB, varargin)
      if (nargin < 2)
        error ("table.setdiff: too few input arguments.");
      endif
      if (! istable (tblA) || ! istable (tblB))
        error ("table.setdiff: both inputs must be tables.");
      endif
      [order, emsg] = parse_set_order (varargin);
      if (! isempty (emsg))
        error ("table.setdiff: %s", emsg);
      endif
      [proxyA, proxyB, emsg] = rowProxies (tblA, tblB);
      if (! isempty (emsg))
        error ("table.setdiff: %s", emsg);
      endif
      [~, ixA] = setdiff (proxyA, proxyB, 'rows', order);
      tbl = subsetrows (tblA, ixA);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} setxor (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} setxor (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}, @var{ixB}] =} setxor (@dots{})
    ##
    ## Exclusive OR of two tables by rows.
    ##
    ## @code{@var{tbl} = setxor (@var{tblA}, @var{tblB})} returns the set of rows
    ## that are present in either @var{tblA} or @var{tblB} but not in both, with
    ## duplicate rows removed.  Both tables must have the same variable names,
    ## although not necessarily in the same order; @var{tbl} keeps the variable
    ## order of @var{tblA}.  Rows are compared by their variable values only (row
    ## names are ignored), and by default @var{tbl} is sorted by those values.
    ##
    ## @code{@var{tbl} = setxor (@var{tblA}, @var{tblB}, @var{setOrder})} controls
    ## the ordering of @var{tbl}, either @qcode{'sorted'} (default) or
    ## @qcode{'stable'}.
    ##
    ## @code{[@var{tbl}, @var{ixA}, @var{ixB}] = setxor (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that @var{tbl} is the vertical
    ## concatenation of @code{@var{tblA}(@var{ixA},:)} and
    ## @code{@var{tblB}(@var{ixB},:)}.
    ##
    ## @end deftypefn
    function [tbl, ixA, ixB] = setxor (tblA, tblB, varargin)
      if (nargin < 2)
        error ("table.setxor: too few input arguments.");
      endif
      if (! istable (tblA) || ! istable (tblB))
        error ("table.setxor: both inputs must be tables.");
      endif
      [order, emsg] = parse_set_order (varargin);
      if (! isempty (emsg))
        error ("table.setxor: %s", emsg);
      endif
      [proxyA, proxyB, emsg] = rowProxies (tblA, tblB);
      if (! isempty (emsg))
        error ("table.setxor: %s", emsg);
      endif
      [keyX, ixA, ixB] = setxor (proxyA, proxyB, 'rows', order);
      ## ixA, ixB list A's then B's contributions, but the result row order
      ## interleaves them per SETORDER, so reorder the assembled rows to the
      ## result's own order.  Row names are dropped: rows are drawn from both
      ## tables and cannot be attributed to a single input (like MATLAB).
      sA = subsetrows (tblA, ixA);
      sB = subsetrows (tblB, ixB);
      sA.RowNames = {};
      sB.RowNames = {};
      sel = vertcat (sA, sB);
      [~, perm] = ismember (keyX, [proxyA(ixA,:); proxyB(ixB,:)], 'rows');
      tbl = subsetrows (sel, perm);
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
    ## least one element in table @var{tblA} is missing, otherwise it returns
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
    ## @item @qcode{@{''@}} - cell arrays of character vectors
    ## @item @qcode{''} - character arrays
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = anymissing (this)
      TF = any (any (ismissing (this)));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {Method} {@var{TF} =} ismissing (@var{tbl})
    ## @deftypefnx {Method} {@var{TF} =} ismissing (@var{tbl}, @var{indicator})
    ## @deftypefnx {Method} {@var{TF} =} ismissing (@dots{}, @qcode{'OutputFormat'}, @var{outFmt})
    ##
    ## Find missing values in table.
    ##
    ## @code{@var{TF} = ismissing (@var{tbl})} returns a logical array,
    ## @var{TF}, with any @qcode{true} values corresponding to missing elements
    ## in the input table @var{tbl}.
    ##
    ## Missing values are defined according to the data type of each variable in
    ## @var{tblA}:
    ##
    ## @itemize
    ## @item @qcode{NaN} - double, single, duration and calendarDuration
    ## @item @qcode{NaT} - datetime
    ## @item @qcode{<missing>} - string
    ## @item @qcode{<undefined>} - categorical
    ## @item @qcode{@{''@}} - cell arrays of character vectors
    ## @item @qcode{''} - character arrays
    ## @end itemize
    ##
    ## @code{@var{TF} = ismissing (@var{tbl}, @var{indicator})} also returns a
    ## logical array, @var{TF}, with any @qcode{true} values corresponding to
    ## elements in the input table @var{tbl}, which are equal to the values in
    ## @var{indicator}.  When specifying an @var{indicator}, all default missing
    ## values are ignored.  If you want to keep them, you need to define them in
    ## @var{indicator}.
    ##
    ## @var{indicator} can be either a vector of specific data type, in which
    ## case all other data types in table @var{tbl} are ignored, or a cell array
    ## containing mixed types of data types, in which case they match the data
    ## types of the variables in table @var{tbl}.  Missing values specified by
    ## @var{indicator} also apply to nested tables.
    ##
    ## Besides the explicit data type match between @var{indicator} and
    ## @var{tbl}, the following additional data types matches apply.
    ## @itemize
    ## @item @qcode{double} indicators match numeric and logical variables.
    ## @item @qcode{logical} indicators match numeric and logical variables.
    ## @item @qcode{char} and @qcode{cellstr} indicators match string variables.
    ## @item @qcode{char} and @var{string} indicators match categorical
    ## variables.
    ## @end itemize
    ##
    ## The output array @var{FT} has the same size as the input table @var{tbl}.
    ##
    ## @code{@var{TF} = ismissing (@dots{}, @qcode{'OutputFormat'},
    ## @var{outFmt})} specifies whether @var{TF} is returned as a logical array
    ## or as a table,
    ## which maintains the variable names and all other information of the input
    ## table @var{tbl}.  Specifying @var{outFmt} as @qcode{'logical'} (default)
    ## returns a logical array.  Specifying @var{outFmt} as @qcode{'tabular'}
    ## returns a table.
    ##
    ## @end deftypefn
    function TF = ismissing (this, varargin)

      ## Parse optional Name-Value paired arguments
      optNames = {'OutputFormat'};
      dfValues = {'logical'};
      [outFmt, indicator] = parsePairedArguments (optNames, dfValues, ...
                                                  varargin(:));

      if (! any (strcmpi (outFmt, {'logical', 'tabular'})))
        error ("table.ismissing: invalid value for 'OutputFormat'.");
      endif

      ## Process each table variable with default missing values
      if (isempty (indicator))
        for i = 1:width (this)
          tmpVar = this.VariableValues{i};
          if (isa (tmpVar, 'table'))
            varTF = ismissing (tmpVar, 'OutputFormat', 'logical');
            varTF = any (varTF, 2);
            this.VariableValues{i} = varTF;
          elseif (any (isa (tmpVar, {'calendarDuration', 'categorical', ...
                                     'datetime', 'duration', 'string'})))
            varTF = ismissing (tmpVar);
            varTF = any (varTF, 2);
            this.VariableValues{i} = varTF;
          elseif (ischar (tmpVar))
            varTF = __ismissing__ (tmpVar);
            varTF = all (varTF, 2);
            this.VariableValues{i} = varTF;
          else  # numeric, logical, and cellstr arrays
            varTF = __ismissing__ (tmpVar);
            varTF = any (varTF, 2);
            this.VariableValues{i} = varTF;
          endif
        endfor
      else
        ## Remove nested cell caused by parsing with paredArgs
        indicator = indicator{1};
        ## Indicator must be a vector in any case
        if (! isvector (indicator))
          error ("table.ismissing: INDICATOR must be a vector.");
        endif
        ## NaN values for calendarDuration and duration
        nan_calendarDuration = nan_duration = false;
        ## Preprocess indicator if it is a cell array
        if (iscell (indicator) && ! iscellstr (indicator))
          ## Elements in indicator vector must be scalars (except char vectors)
          fcn = @(x) isscalar (x) | isempty (x) | (ischar (x) & isvector (x));
          all_scalar = all (cellfun (fcn, indicator));
          if (! all_scalar)
            error (strcat ("table.ismissing: INDICATOR must explicitly", ...
                           " contain scalar elements or character", ...
                           " vectors."));
          endif
          ## categorical arrays
          idx_categorical = false;
          categorical_indicator = [];
          fcn = @(x) isa (x, 'categorical');
          ids_categorical = cellfun (fcn, indicator);
          if (any (ids_categorical))
            new_categories = [indicator{ids_categorical}];
            categorical_indicator = [categorical_indicator, new_categories];
            idx_categorical = true;
          endif
          fcn = @(x) isa (x, 'string');
          ids_categorical = cellfun (fcn, indicator);
          if (any (ids_categorical))
            new_categories = categorical ([indicator{ids_categorical}]);
            categorical_indicator = [categorical_indicator, new_categories];
            idx_categorical = true;
          endif
          ids_categorical = cellfun ('ischar', indicator);
          if (any (ids_categorical))
            new_categories = ...
                categorical (string ([indicator{ids_categorical}]));
            categorical_indicator = [categorical_indicator, new_categories];
            idx_categorical = true;
          endif
          ## datetime arrays
          fcn = @(x) isa (x, 'datetime');
          idx_datetime = cellfun (fcn, indicator);
          if (any (idx_datetime))
            datetime_indicator = [indicator{idx_datetime}];
            idx_datetime = true;
          endif
          ## duration arrays
          fcn = @(x) isa (x, 'duration');
          idx_duration = cellfun (fcn, indicator);
          if (any (idx_duration))
            duration_indicator = [indicator{idx_duration}];
            idx_duration = true;
          endif
          ## string arrays
          fcn = @(x) isa (x, 'string') || ischar (x) || iscellstr (x);
          idx_string = cellfun (fcn, indicator);
          if (any (idx_string))
            string_indicator = string (indicator(idx_string));
            idx_string = true;
          endif
          ## cell arrays of character vectors
          fcn = @(x) iscellstr (x);
          idx_iscstr = cellfun (fcn, indicator);
          if (any (idx_iscstr))
            iscstr_indicator = indicator{idx_iscstr};
            idx_iscstr = true;
          endif
          ## char arrays
          idx_ischar = cellfun ('ischar', indicator);
          if (any (idx_ischar))
            ischar_indicator = [indicator{idx_ischar}];
            idx_ischar = true;
          endif
          ## numeric and logical arrays
          fcn = @(x) isnumeric (x) || islogical (x);
          idx_numlog = cellfun (fcn, indicator);
          if (any (idx_numlog))
            numlog_indicator = [indicator{idx_numlog}];
            idx_numlog = true;
            ## Check for NaN and apply to duration and calendarDuration arrays
            if (any (isnan (numlog_indicator)))
              nan_calendarDuration = nan_duration = true;
            endif
          endif
        elseif (iscellstr (indicator))
          ## cell arrays of character vectors and string arrays are searched
          idx_iscstr = true;
          iscstr_indicator = indicator;
          idx_string = true;
          string_indicator = indicator;
          ## all other arrays are ignored
          idx_categorical = false;
          idx_datetime = false;
          idx_duration = false;
          idx_ischar = false;
          idx_numlog = false;
        else  # single data type indicator
          idx_categorical = false;
          idx_datetime = false;
          idx_duration = false;
          idx_string = false;
          idx_iscstr = false;
          idx_ischar = false;
          idx_numlog = false;
          if (isa (indicator, 'categorical'))
            idx_categorical = true;
            categorical_indicator = indicator;
          elseif (isa (indicator, 'datetime'))
            idx_datetime = true;
            datetime_indicator = indicator;
          elseif (isa (indicator, 'duration'))
            idx_duration = true;
            duration_indicator = indicator;
          elseif (isa (indicator, 'string'))
            idx_string = true;
            string_indicator = indicator;
            idx_categorical = true;
            categorical_indicator = categorical (indicator);
          elseif (iscellstr (indicator))
            idx_iscstr = true;
            iscstr_indicator = indicator;
            idx_string = true;
            string_indicator = string (indicator);
          elseif (ischar (indicator))
            idx_ischar = true;
            ischar_indicator = cellstr (indicator);
            idx_string = true;
            string_indicator = string (indicator);
            idx_categorical = true;
            categorical_indicator = categorical (string_indicator);
          else  # numeric and logical arrays
            idx_numlog = true;
            numlog_indicator = indicator;
            ## Check for NaN and apply to duration and calendarDuration arrays
            if (any (isnan (numlog_indicator)))
              nan_calendarDuration = nan_duration = true;
            endif
          endif
        endif
        ## Return false TF vector for any datatypes that are not
        ## represented in the indicator and should be ignored
        TF_false = false (rows (this), 1);
        for i = 1:width (this)
          tmpVar = this.VariableValues{i};
          if (isa (tmpVar, 'table'))
            varTF = ismissing (tmpVar, indicator, 'OutputFormat', 'logical');
          elseif (isa (tmpVar, 'calendarDuration'))
            if (nan_calendarDuration)
              varTF = ismissing (tmpVar);
            else
              varTF = TF_false;
            endif
          elseif (isa (tmpVar, 'categorical'))
            if (idx_categorical)
              varTF = ismissing (tmpVar, categorical_indicator);
            else
              varTF = TF_false;
            endif
          elseif (isa (tmpVar, 'datetime'))
            if (idx_datetime)
              varTF = ismissing (tmpVar, datetime_indicator);
            else
              varTF = TF_false;
            endif
          elseif (isa (tmpVar, 'duration'))
            varTF = TF_false;
            if (nan_duration)
              varTF = varTF | ismissing (tmpVar);
            endif
            if (idx_duration)
              varTF = varTF | ismissing (tmpVar, duration_indicator);
            endif
          elseif (isa (tmpVar, 'string'))
            if (idx_string)
              varTF = ismissing (tmpVar, string_indicator);
            else
              varTF = TF_false;
            endif
          elseif (iscellstr (tmpVar))
            if (idx_iscstr)
              varTF = __ismissing__ (tmpVar, iscstr_indicator);
            else
              varTF = TF_false;
            endif
          elseif (ischar (tmpVar))
            if (idx_ischar)
              varTF = __ismissing__ (cellstr (tmpVar), ischar_indicator);
            else
              varTF = TF_false;
            endif
          else  # numeric and logical arrays
            if (idx_numlog)
              varTF = __ismissing__ (tmpVar, numlog_indicator);
            else
              varTF = TF_false;
            endif
          endif
          varTF = any (varTF, 2);
          this.VariableValues{i} = varTF;
        endfor
      endif

      ## Return appropriate OutputFormat
      if (strcmpi (outFmt, 'logical'))
        TF = table2array (this);
      else
        TF = this;
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} rmmissing (@var{tblA})
    ## @deftypefnx {table} {@var{tbl} =} rmmissing (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{TF}] =} rmmissing (@dots{})
    ##
    ## Remove missing table elements by rows.
    ##
    ## @code{@var{tbl} = rmmissing (@var{tblA})} returns a table with the rows of
    ## @var{tblA} that contain at least one missing value removed.  Missing values
    ## are determined per variable according to its data type (@code{NaN} for
    ## numeric, @code{NaT} for @code{datetime}, @code{<missing>} for @code{string},
    ## @code{<undefined>} for @code{categorical}, @code{@{''@}} for cellstr, etc.),
    ## as reported by @code{ismissing}.
    ##
    ## @code{@var{tbl} = rmmissing (@dots{}, @var{Name}, @var{Value})} customizes
    ## the operation with the following options:
    ##
    ## @table @asis
    ## @item @qcode{'MinNumMissing'}
    ## A positive integer @var{n} (default @code{1}).  A row is removed only when
    ## it has at least @var{n} variables with a missing value.
    ##
    ## @item @qcode{'DataVariables'}
    ## Restrict the search for missing values to the indicated subset of table
    ## variables, using the same variable referencing as the other @code{table}
    ## methods.  Variables outside the subset are not inspected, but all variables
    ## are kept in the output.
    ##
    ## @item @qcode{'MissingLocations'}
    ## Supply the missing-value locations explicitly instead of deriving them with
    ## @code{ismissing}.  The value is either a logical matrix with one row per
    ## row of the input and one column per inspected variable, or a @code{table}
    ## of logical variables whose names and sizes match the inspected variables.
    ## @end table
    ##
    ## @code{[@var{tbl}, @var{TF}] = rmmissing (@dots{})} also returns a logical
    ## column vector @var{TF}, with one element per row of @var{tblA}, that is
    ## @qcode{true} for each removed row.
    ##
    ## @end deftypefn
    function [tbl, TF] = rmmissing (this, varargin)
      ## Handle simple input argument first
      if (numel (varargin) == 0)
        TF = any (ismissing (this), 2);
        tbl = subsetrows (this, ! TF);
        return;
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'MinNumMissing', 'DataVariables', 'MissingLocations'};
      dfValues = {1, [], []};
      [minNum, dVars, mLocs] = parsePairedArguments (optNames, dfValues, ...
                                                     varargin(:));

      ## Check optional Name-Value paired arguments and operate accordingly
      if (! isscalar (minNum) || fix (minNum) != minNum || minNum <= 0)
        error ("table.rmmissing: 'MinNumMissing' must be a positive integer.");
      endif
      if (! isempty (dVars))
        dIxVars = resolveVarRef (this, dVars, 'lenient');
        if (any (dIxVars == 0))
          badpos = find (dIxVars == 0)(1);
          dv = dVars;
          if (isa (dv, 'string'))
            dv = cellstr (dv);
          endif
          if (ischar (dv))
            badname = dv;
          elseif (iscellstr (dv))
            badname = dv{badpos};
          else
            badname = "<unknown>";
          endif
          error (strcat ("table.rmmissing: 'DataVariables' index a", ...
                         " non-existing variable: '%s'."), badname);
        endif
        tmpT = subsetvars (this, dIxVars);
      else
        tmpT = this;
      endif
      if (! isempty (mLocs))
        if (islogical (mLocs))
          if (! isequal (size (mLocs), size (tmpT)))
            error (strcat ("table.rmmissing: 'MissingLocations' must be", ...
                           " a logical matrix of the same size as the", ...
                           " input table or the part of it referenced by", ...
                           " 'DataVariables'."));
          endif
          TF = sum (mLocs, 2) >= minNum;
          tbl = subsetrows (this, ! TF);
        elseif (isa (mLocs, 'table'))
          if (! all (ismember (tmpT.VariableNames, mLocs.VariableNames)))
            error (strcat ("table.rmmissing: 'MissingLocations' must be", ...
                           " a table with the same variable names as the", ...
                           " input table or the part of it referenced by", ...
                           " 'DataVariables'."));
          endif
          TF = false (rows (this), 0);
          for jx = 1:width (tmpT)
            kx = find (strcmp (tmpT.VariableNames{jx}, mLocs.VariableNames), 1);
            varTF = mLocs.VariableValues{kx};
            if (! islogical (varTF))
              error (strcat ("table.rmmissing: 'MissingLocations' must", ...
                             " be a table with logical variables."));
            endif
            if (! isequal (size (varTF), size (tmpT.VariableValues{jx})))
              error (strcat ("table.rmmissing: 'MissingLocations' must", ...
                             " be a table with the same variable sizes", ...
                             " as the input table or the part of it", ...
                             " referenced by 'DataVariables'."));
            endif
            TF = [TF, any(varTF, 2)];
          endfor
          TF = sum (TF, 2) >= minNum;
          tbl = subsetrows (this, ! TF);
        else
          error ("table.rmmissing: invalid data type for 'MissingLocations'.");
        endif
      else
        TF = sum (ismissing (tmpT), 2) >= minNum;
        tbl = subsetrows (this, ! TF);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} fillmissing (@var{tblA}, @qcode{'constant'}, @var{val})
    ## @deftypefnx {table} {@var{tblB} =} fillmissing (@var{tblA}, @var{method})
    ## @deftypefnx {table} {@var{tblB} =} fillmissing (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tblB}, @var{TF}] =} fillmissing (@dots{})
    ##
    ## Fill missing entries of a table, variable by variable.
    ##
    ## @code{@var{tblB} = fillmissing (@var{tblA}, @qcode{'constant'},
    ## @var{val})} replaces the missing entries of each table variable with the
    ## fill value @var{val}.  @var{val} can be a scalar that is broadcast to
    ## every targeted variable, a vector with one element per targeted
    ## variable, or a cell array with one fill value per targeted variable.
    ## The fill value of each variable must be compatible with that variable's
    ## data type.
    ##
    ## @code{@var{tblB} = fillmissing (@var{tblA}, @var{method})} fills missing
    ## entries using the gap-filling method @var{method}, which can be one of:
    ##
    ## @table @asis
    ## @item @qcode{'previous'}
    ## Fill with the previous non-missing entry along each column.
    ##
    ## @item @qcode{'next'}
    ## Fill with the next non-missing entry along each column.
    ##
    ## @item @qcode{'nearest'}
    ## Fill with the nearest non-missing entry along each column.  When two
    ## non-missing entries are equidistant, the later (next) one is used.
    ##
    ## @item @qcode{'linear'}
    ## Fill numeric variables by linear interpolation of neighboring
    ## non-missing entries.  Non-numeric variables are left unchanged.
    ## @end table
    ##
    ## The @qcode{'previous'}, @qcode{'next'}, and @qcode{'nearest'} methods
    ## operate on variables of any data type.  Leading or trailing missing
    ## entries that cannot be reached by the method are left missing.
    ##
    ## The following @var{Name}/@var{Value} pairs are supported:
    ##
    ## @table @asis
    ## @item @qcode{'DataVariables'}
    ## Restrict the operation to the indicated subset of table variables.  The
    ## value uses the same variable referencing as the rest of the @code{table}
    ## methods.  By default, every variable is targeted.
    ##
    ## @item @qcode{'EndValues'}
    ## Control how the @qcode{'linear'} method treats leading and trailing
    ## missing entries.  Valid values are @qcode{'extrap'} (default, linear
    ## extrapolation), @qcode{'none'} (leave them missing), or a numeric scalar
    ## used as a constant for the end gaps.
    ## @end table
    ##
    ## @code{[@var{tblB}, @var{TF}] = fillmissing (@dots{})} also returns a
    ## logical array @var{TF} with @code{height (@var{tblA})} rows and one
    ## column per table variable.  @code{@var{TF}(i,j)} is @qcode{true} when an
    ## entry of the j-th variable in the i-th row was missing and has been
    ## filled.
    ##
    ## Not yet supported: the @qcode{'spline'}, @qcode{'pchip'},
    ## @qcode{'makima'}, @qcode{'movmean'}, @qcode{'movmedian'},
    ## @qcode{'mean'}, @qcode{'median'}, @qcode{'mode'}, and @qcode{'knn'}
    ## methods, as well as the @qcode{'ReplaceValues'}, @qcode{'MaxGap'},
    ## @qcode{'SamplePoints'}, and @qcode{'MissingLocations'} options.
    ##
    ## @end deftypefn
    function [tbl, TF] = fillmissing (tblA, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("table.fillmissing: too few input arguments.");
      endif

      ## Resolve the fill method (and the value for the 'constant' method)
      method = varargin{1};
      if (isa (method, 'string') && isscalar (method))
        method = char (method);
      endif
      if (! (ischar (method) && isrow (method)))
        error ("table.fillmissing: METHOD must be a character vector.");
      endif
      method = lower (method);
      rest = varargin(2:end);
      constVal = [];
      switch (method)
        case 'constant'
          if (isempty (rest))
            error (strcat ("table.fillmissing: the 'constant' method", ...
                           " requires a fill value."));
          endif
          constVal = rest{1};
          rest = rest(2:end);
        case {'previous', 'next', 'nearest', 'linear'}
          ## supported; no extra positional argument
        case {'movmean', 'movmedian', 'spline', 'pchip', 'makima', 'knn', ...
              'mean', 'median', 'mode'}
          error (strcat ("table.fillmissing: method '%s' is not supported", ...
                         " yet."), method);
        otherwise
          error ("table.fillmissing: unknown method '%s'.", method);
      endswitch

      ## Parse optional Name-Value paired arguments
      optNames = {'DataVariables', 'EndValues', 'ReplaceValues'};
      dfValues = {[], 'extrap', true};
      [dVars, endVals, replace] = parsePairedArguments (optNames, dfValues, ...
                                                        rest(:));
      if (! (islogical (replace) && isscalar (replace)))
        error ("table.fillmissing: 'ReplaceValues' must be a logical scalar.");
      endif
      if (! replace)
        error (strcat ("table.fillmissing: 'ReplaceValues' set to false is", ...
                       " not supported yet."));
      endif

      ## Resolve targeted variables
      if (isempty (dVars))
        ixVars = 1:width (tblA);
      else
        ixVars = resolveVarRef (tblA, dVars, 'lenient');
        if (any (ixVars == 0))
          badpos = find (ixVars == 0)(1);
          dv = dVars;
          if (isa (dv, 'string'))
            dv = cellstr (dv);
          endif
          if (ischar (dv))
            badname = dv;
          elseif (iscellstr (dv))
            badname = dv{badpos};
          else
            badname = "<unknown>";
          endif
          error (strcat ("table.fillmissing: 'DataVariables' index a", ...
                         " non-existing variable: '%s'."), badname);
        endif
      endif

      ## Resolve per-variable fill values for the 'constant' method
      if (strcmp (method, 'constant'))
        fillVals = resolve_const_values (constVal, numel (ixVars));
      endif

      ## Initialize outputs (TF has one column per table variable)
      tbl = tblA;
      TF = false (height (tblA), width (tblA));

      ## Fill each targeted variable
      for k = 1:numel (ixVars)
        iv = ixVars(k);
        v = tbl.VariableValues{iv};
        M = var_missing_mask (v);
        if (! any (M(:)))
          continue;
        endif
        filled = false (size (M));
        if (strcmp (method, 'constant'))
          [v, filled] = fill_constant (v, M, fillVals{k}, ...
                                       tbl.VariableNames{iv});
        elseif (strcmp (method, 'linear') && ! (isnumeric (v) || islogical (v)))
          ## 'linear' applies only to numeric variables; skip others
          continue;
        else
          for c = 1:columns (M)
            m = M(:,c);
            if (! any (m))
              continue;
            endif
            if (strcmp (method, 'linear'))
              [v(:,c), filled(:,c)] = fill_linear (v(:,c), m, endVals);
            else
              si = fill_neighbor_idx (m, method);
              rows = m & si > 0;
              v(rows,c) = v(si(rows),c);
              filled(:,c) = rows;
            endif
          endfor
        endif
        tbl.VariableValues{iv} = v;
        TF(:,iv) = any (filled, 2);
      endfor

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} standardizeMissing (@var{tblA}, @var{indicator})
    ## @deftypefnx {table} {@var{tblB} =} standardizeMissing (@dots{}, @var{Name}, @var{Value})
    ##
    ## Insert standard missing values into a table.
    ##
    ## @code{@var{tblB} = standardizeMissing (@var{tblA}, @var{indicator})}
    ## replaces every entry of @var{tblA} that matches a value in
    ## @var{indicator} with the standard missing value of that variable's data
    ## type (@code{NaN}
    ## for @code{double}/@code{single}, @code{@qcode{''}} for cell arrays of
    ## character vectors, @code{<missing>} for @code{string}, and
    ## @code{<undefined>} for @code{categorical}).
    ##
    ## @var{indicator} may be a numeric scalar or vector, a character vector, a
    ## @code{string} array, a cell array of character vectors, or a cell array
    ## mixing numeric and text indicators.  Each indicator is applied only to
    ## the variables whose type is compatible with it: numeric indicators match
    ## @code{double} and @code{single} variables, while text indicators (char,
    ## @code{string}, or cellstr) match cell-array-of-character-vector,
    ## @code{string}, and @code{categorical} variables.
    ##
    ## The @qcode{'DataVariables'} @var{Name}/@var{Value} pair restricts the
    ## operation to a subset of variables, using the same variable referencing
    ## as the other @code{table} methods.  Variables not selected pass through
    ## unchanged.
    ##
    ## Logical and integer variables (which have no standard missing value) and
    ## @code{duration}, @code{datetime}, and @code{calendarDuration} variables
    ## pass through unchanged.
    ##
    ## @end deftypefn
    function tbl = standardizeMissing (tblA, indicator, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("table.standardizeMissing: too few input arguments.");
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'DataVariables'};
      dfValues = {[]};
      dVars = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Resolve targeted variables
      if (isempty (dVars))
        ixVars = 1:width (tblA);
      else
        ixVars = resolveVarRef (tblA, dVars, 'lenient');
        if (any (ixVars == 0))
          badpos = find (ixVars == 0)(1);
          dv = dVars;
          if (isa (dv, 'string'))
            dv = cellstr (dv);
          endif
          if (ischar (dv))
            badname = dv;
          elseif (iscellstr (dv))
            badname = dv{badpos};
          else
            badname = "<unknown>";
          endif
          error (strcat ("table.standardizeMissing: 'DataVariables' index", ...
                         " a non-existing variable: '%s'."), badname);
        endif
      endif

      ## Split the indicator into numeric and text indicator values
      [numInd, txtInd] = std_normalize_indicator (indicator);

      ## Standardize each targeted variable
      tbl = tblA;
      for k = 1:numel (ixVars)
        iv = ixVars(k);
        v = std_apply_indicator (tbl.VariableValues{iv}, numInd, txtInd);
        tbl.VariableValues{iv} = v;
      endfor

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

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{G} =} findgroups (@var{T})
    ## @deftypefnx {table} {[@var{G}, @var{TID}] =} findgroups (@var{T})
    ##
    ## Find groups defined by the variables of a table.
    ##
    ## @code{@var{G} = findgroups (@var{T})} returns @var{G}, a column vector of
    ## positive integer group numbers, with one element for each row of the table
    ## @var{T}.  Each variable of @var{T} acts as a grouping variable, and the
    ## groups are the unique combinations of values across those variables, sorted
    ## in ascending order.  If @var{N} groups are found, every integer between 1
    ## and @var{N} labels a group.  Rows holding a missing value (@code{NaN},
    ## @code{NaT}, @code{<missing>}, @code{''}, or @code{<undefined>}) in any
    ## grouping variable are labelled @code{NaN} in @var{G}.
    ##
    ## @code{[@var{G}, @var{TID}] = findgroups (@var{T})} also returns @var{TID},
    ## a table whose rows are the sorted unique combinations identifying each
    ## group, with the same variables as @var{T}.
    ##
    ## @end deftypefn
    function [G, TID] = findgroups (this)
      if (nargin != 1)
        print_usage ();
      endif
      nvar = width (this);
      n = height (this);
      if (nvar == 0)
        error ("table.findgroups: T must have at least one variable.");
      endif
      ## Build the combined proxy matrix and the overall missing-row mask.
      P = [];
      miss = false (n, 1);
      for j = 1:nvar
        [p, m, errmsg] = group_col_proxy (this.VariableValues{j});
        if (! isempty (errmsg))
          error ("table.findgroups: %s", errmsg);
        endif
        P = [P, p];
        miss = miss | m;
      endfor
      ## Label the non-missing rows by sorted unique combination.
      G = NaN (n, 1);
      keep = find (! miss);
      if (! isempty (keep))
        [~, ia, ic] = unique (P(keep,:), "rows");
        G(keep) = ic;
      endif
      if (nargout > 1)
        if (isempty (keep))
          TID = this([], :);
        else
          repRows = keep(ia);
          idcols = cell (1, nvar);
          for j = 1:nvar
            col = this.VariableValues{j};
            idcols{j} = col(repRows,:);
          endfor
          TID = table (idcols{:}, "VariableNames", this.VariableNames);
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{Y} =} splitapply (@var{func}, @var{T}, @var{G})
    ## @deftypefnx {table} {[@var{Y1}, @dots{}, @var{YM}] =} splitapply (@var{func}, @var{T}, @var{G})
    ##
    ## Split table data into groups and apply a function to each group.
    ##
    ## @code{@var{Y} = splitapply (@var{func}, @var{T}, @var{G})} splits the rows
    ## of the table @var{T} into groups according to the group numbers @var{G}
    ## (typically produced by @code{findgroups}), applies the function handle
    ## @var{func} to each group, and concatenates the per-group results into the
    ## output @var{Y}.  @var{G} must be a column vector of positive integers with
    ## one element per row of @var{T}; if it identifies @var{N} groups, every
    ## integer between 1 and @var{N} must occur at least once.  Rows for which
    ## @var{G} is @code{NaN} are omitted.  Each variable of @var{T} is passed to
    ## @var{func} as a separate input argument, so @var{func} must accept as many
    ## arguments as @var{T} has variables.
    ##
    ## @code{[@var{Y1}, @dots{}, @var{YM}] = splitapply (@dots{})} returns the
    ## multiple outputs of @var{func}, each concatenated across groups.
    ##
    ## @end deftypefn
    function varargout = splitapply (func, this, G)
      if (nargin != 3)
        print_usage ();
      endif
      if (! is_function_handle (func))
        error ("table.splitapply: FUNC must be a function handle.");
      endif
      n = height (this);
      if (! (isnumeric (G) && isvector (G) && numel (G) == n))
        error (strcat ("table.splitapply: G must be a numeric vector with", ...
                       " one element per row of T."));
      endif
      G = G(:);
      gv = G(! isnan (G));
      if (any (gv != fix (gv)) || any (gv < 1))
        error ("table.splitapply: G must contain positive integers.");
      endif
      if (isempty (gv))
        N = 0;
      else
        N = max (gv);
        if (! isequal (unique (gv), (1:N)'))
          error (strcat ("table.splitapply: G must contain every integer", ...
                         " between 1 and the number of groups."));
        endif
      endif
      nvar = width (this);
      nout = max (nargout, 1);
      results = cell (N, nout);
      for g = 1:N
        rows = (G == g);
        args = cell (1, nvar);
        for j = 1:nvar
          col = this.VariableValues{j};
          args{j} = col(rows,:);
        endfor
        [results{g,:}] = func (args{:});
      endfor
      varargout = cell (1, nout);
      for k = 1:nout
        varargout{k} = vertcat (results{:,k});
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{B} =} varfun (@var{func}, @var{A})
    ## @deftypefnx {table} {@var{B} =} varfun (@var{func}, @var{A}, @var{Name}, @var{Value}, @dots{})
    ##
    ## Apply a function to each variable of a table.
    ##
    ## @code{@var{B} = varfun (@var{func}, @var{A})} applies the function
    ## handle @var{func} separately to each variable of the table @var{A} and
    ## returns the results in the table @var{B}.  @var{func} is called once per
    ## variable with that variable as its single input argument.  By default
    ## each output variable of @var{B} is named @qcode{@var{f}_@var{v}}, where
    ## @var{f} is the name of @var{func} (or @qcode{Fun} when @var{func} is
    ## anonymous) and @var{v} is the name of the corresponding variable of
    ## @var{A}.
    ##
    ## @code{@var{B} = varfun (@var{func}, @var{A}, @var{Name}, @var{Value},
    ## @dots{})} modifies the operation through the following
    ## @var{Name}/@var{Value} pairs:
    ##
    ## @table @asis
    ## @item @qcode{'InputVariables'}
    ## The variables of @var{A} to which @var{func} is applied, given as
    ## variable names, indices, a logical vector, or a function handle that
    ## returns @code{true} for the variables to include.  By default @var{func}
    ## is applied to every variable of @var{A} that is not a grouping variable.
    ##
    ## @item @qcode{'GroupingVariables'}
    ## One or more variables of @var{A} that define groups of rows.  When
    ## grouping variables are given, @var{func} is applied to the values of each
    ## input variable within each group, @var{B} has one row per group, and
    ## @var{B} also includes the grouping variables and a @qcode{GroupCount}
    ## variable holding the number of rows in each group.  Rows with a missing
    ## value in any grouping variable are omitted.
    ##
    ## @item @qcode{'OutputFormat'}
    ## The format of @var{B}, one of @qcode{'auto'} (the default, equivalent to
    ## @qcode{'table'}), @qcode{'table'}, @qcode{'uniform'}, or @qcode{'cell'}.
    ## For @qcode{'uniform'}, @var{func} must return a scalar on each call and
    ## the results are concatenated into an array.  For @qcode{'cell'} the
    ## results are returned in a cell array.  The @qcode{'uniform'} and
    ## @qcode{'cell'} formats return only the results of @var{func}, without the
    ## grouping variables or @qcode{GroupCount}.
    ##
    ## @item @qcode{'ErrorHandler'}
    ## A function handle that is called when @var{func} throws an error.  It
    ## receives a structure with fields @qcode{identifier}, @qcode{message}, and
    ## @qcode{index}, followed by the same inputs that were passed to
    ## @var{func}, and its outputs are used in place of the outputs of
    ## @var{func}.
    ## @end table
    ##
    ## @end deftypefn
    function B = varfun (func, A, varargin)
      if (nargin < 2)
        print_usage ();
      endif
      if (! is_function_handle (func))
        error ("table.varfun: FUNC must be a function handle.");
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'InputVariables', 'GroupingVariables', 'OutputFormat', ...
                  'ErrorHandler'};
      dfValues = {[], [], 'auto', []};
      [inVars, grpVars, outFmt, errHandler] = ...
                  parsePairedArguments (optNames, dfValues, varargin(:));
      outFmt = check_output_format ('varfun', outFmt);
      if (! isempty (errHandler) && ! is_function_handle (errHandler))
        error ("table.varfun: 'ErrorHandler' must be a function handle.");
      endif

      ## Resolve grouping variables and input variables (default input is every
      ## variable that is not a grouping variable).
      if (isempty (grpVars))
        gIx = [];
      else
        gIx = resolveVarRef (A, grpVars)(:)';
      endif
      if (isempty (inVars))
        iIx = 1:width (A);
        iIx(ismember (iIx, gIx)) = [];
      else
        iIx = resolveVarRef (A, inVars)(:)';
      endif
      if (isempty (iIx))
        error ("table.varfun: there are no variables to which to apply FUNC.");
      endif

      ## Build the output variable names from the function and variable names.
      inNames = A.VariableNames(iIx);
      fname = apply_func_name (func);
      outNames = strcat (fname, '_', inNames);

      if (isempty (gIx))
        ## Ungrouped: apply FUNC to each whole variable.
        res = cell (1, numel (iIx));
        for k = 1:numel (iIx)
          out = apply_func (func, errHandler, k, 1, {A.VariableValues{iIx(k)}});
          res{1,k} = out{1};
        endfor
        B = build_apply_result ('varfun', outFmt, res, outNames, {}, {}, []);
      else
        ## Grouped: apply FUNC to each group's slice of each variable.
        inCols = A.VariableValues(iIx);
        [G, ng, repRows, errmsg] = group_table_rows (A.VariableValues(gIx));
        if (! isempty (errmsg))
          error ("table.varfun: %s", errmsg);
        endif
        res = cell (ng, numel (iIx));
        for g = 1:ng
          rows = (G == g);
          for k = 1:numel (iIx)
            col = inCols{k};
            out = apply_func (func, errHandler, g, 1, {col(rows,:)});
            res{g,k} = out{1};
          endfor
        endfor
        [gcols, gcount] = group_output_cols (A.VariableValues(gIx), G, repRows);
        B = build_grouped_apply_result ('varfun', outFmt, res, outNames, ...
                                        gcols, A.VariableNames(gIx), gcount);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{B} =} rowfun (@var{func}, @var{A})
    ## @deftypefnx {table} {@var{B} =} rowfun (@var{func}, @var{A}, @var{Name}, @var{Value}, @dots{})
    ##
    ## Apply a function to each row of a table.
    ##
    ## @code{@var{B} = rowfun (@var{func}, @var{A})} applies the function
    ## handle @var{func} to each row of the table @var{A} and returns the
    ## results in the table @var{B}, which has one row for each row of @var{A}.
    ## By default the value of each variable in the row is passed to @var{func}
    ## as a separate input argument, and the output variables of @var{B} are
    ## named @qcode{Var1}, @qcode{Var2}, and so on.
    ##
    ## @code{@var{B} = rowfun (@var{func}, @var{A}, @var{Name}, @var{Value},
    ## @dots{})} modifies the operation through the following
    ## @var{Name}/@var{Value} pairs:
    ##
    ## @table @asis
    ## @item @qcode{'InputVariables'}
    ## The variables of @var{A} that are passed to @var{func}, given as variable
    ## names, indices, a logical vector, or a function handle.  By default every
    ## variable of @var{A} that is not a grouping variable is used.
    ##
    ## @item @qcode{'GroupingVariables'}
    ## One or more variables of @var{A} that define groups of rows.  When
    ## grouping variables are given, @var{func} is applied once to each group,
    ## receiving the values of each input variable across the rows of the group;
    ## @var{B} has one row per group and also includes the grouping variables
    ## and a @qcode{GroupCount} variable.  Rows with a missing value in any
    ## grouping variable are omitted.
    ##
    ## @item @qcode{'OutputVariableNames'}
    ## The names of the output variables of @var{B}, one per output of
    ## @var{func}.
    ##
    ## @item @qcode{'NumOutputs'}
    ## The number of output arguments to request from @var{func}.  It defaults
    ## to the number of @qcode{'OutputVariableNames'} if those are given,
    ## otherwise to @code{1}.
    ##
    ## @item @qcode{'SeparateInputs'}
    ## A logical scalar.  When @code{true} (the default), the value of each
    ## input variable is passed to @var{func} as a separate argument.  When
    ## @code{false}, the values of the row are horizontally concatenated and
    ## passed as a single argument.
    ##
    ## @item @qcode{'ExtractCellContents'}
    ## A logical scalar.  When @code{true}, the contents of cell-valued
    ## variables are extracted before being passed to @var{func}.  It defaults
    ## to @code{false}.
    ##
    ## @item @qcode{'OutputFormat'}
    ## The format of @var{B}, one of @qcode{'auto'} (the default, equivalent to
    ## @qcode{'table'}), @qcode{'table'}, @qcode{'uniform'}, or @qcode{'cell'}.
    ## For @qcode{'uniform'}, every call to @var{func} must return scalars of
    ## the same type, which are concatenated into an array.  For @qcode{'cell'}
    ## the results are returned in a cell array.  The @qcode{'uniform'} and
    ## @qcode{'cell'} formats return only the results of @var{func}.
    ##
    ## @item @qcode{'ErrorHandler'}
    ## A function handle that is called when @var{func} throws an error,
    ## receiving a structure with fields @qcode{identifier}, @qcode{message},
    ## and @qcode{index} followed by the inputs passed to @var{func}.
    ## @end table
    ##
    ## @end deftypefn
    function B = rowfun (func, A, varargin)
      if (nargin < 2)
        print_usage ();
      endif
      if (! is_function_handle (func))
        error ("table.rowfun: FUNC must be a function handle.");
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'InputVariables', 'GroupingVariables', ...
                  'OutputVariableNames', 'NumOutputs', 'SeparateInputs', ...
                  'ExtractCellContents', 'OutputFormat', 'ErrorHandler'};
      dfValues = {[], [], [], [], true, false, 'auto', []};
      [inVars, grpVars, outNames, numOut, sepIn, extractCell, outFmt, ...
       errHandler] = parsePairedArguments (optNames, dfValues, varargin(:));
      outFmt = check_output_format ('rowfun', outFmt);
      if (! (isscalar (sepIn) && (islogical (sepIn) || isnumeric (sepIn))))
        error ("table.rowfun: 'SeparateInputs' must be a logical scalar.");
      endif
      sepIn = logical (sepIn);
      if (! (isscalar (extractCell)
             && (islogical (extractCell) || isnumeric (extractCell))))
        error ("table.rowfun: 'ExtractCellContents' must be a logical scalar.");
      endif
      extractCell = logical (extractCell);
      if (! isempty (errHandler) && ! is_function_handle (errHandler))
        error ("table.rowfun: 'ErrorHandler' must be a function handle.");
      endif

      ## Resolve grouping variables and input variables (default input is every
      ## variable that is not a grouping variable).
      if (isempty (grpVars))
        gIx = [];
      else
        gIx = resolveVarRef (A, grpVars)(:)';
      endif
      if (isempty (inVars))
        iIx = 1:width (A);
        iIx(ismember (iIx, gIx)) = [];
      else
        iIx = resolveVarRef (A, inVars)(:)';
      endif
      if (isempty (iIx))
        error ("table.rowfun: there are no variables to which to apply FUNC.");
      endif

      ## Determine the number of outputs requested from FUNC.
      if (! isempty (numOut))
        if (! (isnumeric (numOut) && isscalar (numOut) && numOut >= 0
               && numOut == fix (numOut)))
          error ("table.rowfun: 'NumOutputs' must be a nonnegative integer.");
        endif
        nout = numOut;
        if (! isempty (outNames) && numel (cellstr (outNames)) != nout)
          error (strcat ("table.rowfun: the number of", ...
                         " 'OutputVariableNames' must equal 'NumOutputs'."));
        endif
      elseif (! isempty (outNames))
        nout = numel (cellstr (outNames));
      else
        nout = 1;
      endif

      ## Build the output variable names.  Default names are 'Var<k>'; for
      ## grouped output the numbering continues past the grouping variables and
      ## the GroupCount column (so the first result is 'Var<ngroup+2>').
      if (isempty (outNames))
        if (isempty (gIx))
          base = 0;
        else
          base = numel (gIx) + 1;
        endif
        resNames = arrayfun (@(k) sprintf ("Var%d", base + k), 1:nout, ...
                             "UniformOutput", false);
      else
        resNames = cellstr (outNames)(:)';
      endif

      inCols = A.VariableValues(iIx);
      if (isempty (gIx))
        ## Ungrouped: apply FUNC to each row.
        n = height (A);
        res = cell (n, max (nout, 1));
        for r = 1:n
          rows = false (n, 1);
          rows(r) = true;
          args = build_row_args (inCols, rows, sepIn, extractCell);
          res(r,:) = apply_func (func, errHandler, r, nout, args);
        endfor
        B = build_apply_result ('rowfun', outFmt, res(:,1:nout), resNames, ...
                                {}, {}, [], A.RowNames);
      else
        ## Grouped: apply FUNC to the rows of each group.
        [G, ng, repRows, errmsg] = group_table_rows (A.VariableValues(gIx));
        if (! isempty (errmsg))
          error ("table.rowfun: %s", errmsg);
        endif
        res = cell (ng, max (nout, 1));
        for g = 1:ng
          rows = (G == g);
          args = build_row_args (inCols, rows, sepIn, extractCell);
          res(g,:) = apply_func (func, errHandler, g, nout, args);
        endfor
        [gcols, gcount] = group_output_cols (A.VariableValues(gIx), G, repRows);
        B = build_grouped_apply_result ('rowfun', outFmt, res(:,1:nout), ...
                                        resNames, gcols, ...
                                        A.VariableNames(gIx), gcount);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{G} =} groupsummary (@var{T}, @var{groupvars})
    ## @deftypefnx {table} {@var{G} =} groupsummary (@var{T}, @var{groupvars}, @var{groupbins})
    ## @deftypefnx {table} {@var{G} =} groupsummary (@var{T}, @var{groupvars}, @var{groupbins}, @var{method})
    ## @deftypefnx {table} {@var{G} =} groupsummary (@var{T}, @var{groupvars}, @var{groupbins}, @var{method}, @var{datavars})
    ## @deftypefnx {table} {@var{G} =} groupsummary (@dots{}, @var{Name}, @var{Value})
    ##
    ## Compute summary statistics by group for the variables of a table.
    ##
    ## @code{@var{G} = groupsummary (@var{T}, @var{groupvars})} groups the rows of
    ## the table @var{T} by the grouping variables @var{groupvars} and returns the
    ## table @var{G} with one row per group, holding the grouping variables and a
    ## @qcode{GroupCount} variable counting the rows in each group.  @var{groupvars}
    ## selects the grouping variables by name, index, logical vector, function
    ## handle, or @code{vartype} subscript.
    ##
    ## @code{@var{G} = groupsummary (@var{T}, @var{groupvars}, @var{method})} also
    ## applies @var{method} to each data variable within each group and appends the
    ## results to @var{G}.  @var{method} is one of the method names below, a
    ## function handle, or a cell array of method names and@/or function handles:
    ##
    ## @table @asis
    ## @item @qcode{'sum'}, @qcode{'mean'}, @qcode{'median'}, @qcode{'mode'}
    ## @itemx @qcode{'var'}, @qcode{'std'}, @qcode{'min'}, @qcode{'max'}
    ## @itemx @qcode{'range'}, @qcode{'nnz'}
    ## Standard statistics, computed over numeric or logical data variables.
    ## @code{NaN} values are omitted (as in MATLAB) for every named method except
    ## @qcode{'nummissing'}.
    ##
    ## @item @qcode{'nummissing'}
    ## The number of missing values in the group, supported for data variables of
    ## any type.
    ##
    ## @item @qcode{'numunique'}
    ## The number of unique non-missing values in the group, supported for data
    ## variables of any type.
    ## @end table
    ##
    ## A function handle is applied to each group's slice of each data variable and
    ## must return a single row (its first dimension must be @code{1}); it receives
    ## the values with @code{NaN} included.
    ##
    ## @code{@var{G} = groupsummary (@var{T}, @var{groupvars}, @var{method},
    ## @var{datavars})} applies @var{method} only to the data variables selected by
    ## @var{datavars} (named, indexed, logical, function handle, or @code{vartype}
    ## subscript).  By default every variable that is not a grouping variable is a
    ## data variable.
    ##
    ## The computed variables of @var{G} are named @code{<method>_<datavar>}, e.g.
    ## @qcode{mean_X}; results from a function handle are named
    ## @code{fun<n>_<datavar>}, where @var{n} is the position of the handle among
    ## the requested methods.  When several methods are requested the computed
    ## variables are ordered method first, then data variable.
    ##
    ## The optional @var{groupbins} argument bins the grouping variables before
    ## grouping: a vector of bin edges or a positive integer number of
    ## equal-width bins spanning the data range, applied to a numeric, datetime,
    ## or duration grouping variable.  Each binned variable becomes a categorical
    ## whose categories are the bin interval labels, e.g.@: @qcode{'[0, 10)'}.
    ## Pass a cell array with one scheme per grouping variable to bin them
    ## differently, or @qcode{'none'} to leave a variable unbinned.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'IncludeMissingGroups'}
    ## A logical scalar.  When @code{true} (the default), rows holding a missing
    ## value in a grouping variable form their own groups, sorted after the
    ## non-missing groups.  When @code{false}, such rows are excluded.
    ##
    ## @item @qcode{'IncludeEmptyGroups'}
    ## A logical scalar, @code{false} by default.  When @code{true}, the unused
    ## categories of a categorical or binned grouping variable contribute empty
    ## groups (@qcode{GroupCount} @code{0}, @code{0} for @qcode{'sum'} and
    ## @qcode{'nnz'}, @code{NaN} otherwise).
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Either @qcode{'left'} (the default) or @qcode{'right'}, selecting which
    ## edge of each bin is inclusive when @var{groupbins} is given.
    ## @end table
    ##
    ## @end deftypefn
    function G = groupsummary (T, groupvars, varargin)
      if (nargin < 2)
        print_usage ();
      endif

      ## Split the trailing arguments into the optional positional METHOD and
      ## DATAVARS arguments and any Name-Value pairs.  A Name-Value region starts
      ## at the first char-vector/string that names a known option.
      optNames = {'IncludeMissingGroups', 'IncludeEmptyGroups', 'IncludedEdge'};
      nvStart = numel (varargin) + 1;
      for k = 1:numel (varargin)
        a = varargin{k};
        if ((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a)))
          if (any (strcmpi (char (a), optNames)))
            nvStart = k;
            break;
          endif
        endif
      endfor
      posArgs = varargin(1:nvStart-1);
      nvArgs = varargin(nvStart:end);
      ## An optional GROUPBINS positional argument precedes METHOD.
      hasGroupbins = false;
      groupbins = [];
      if (! isempty (posArgs) && is_groupbins_spec (posArgs{1}))
        hasGroupbins = true;
        groupbins = posArgs{1};
        posArgs = posArgs(2:end);
      endif
      if (numel (posArgs) > 2)
        error ("table.groupsummary: too many positional arguments.");
      endif
      if (numel (posArgs) >= 1)
        method = posArgs{1};
      else
        method = {};
      endif
      if (numel (posArgs) >= 2)
        datavars = posArgs{2};
        hasDataVars = true;
      else
        datavars = [];
        hasDataVars = false;
      endif

      ## Parse Name-Value options.
      dfValues = {true, false, 'left'};
      [incMiss, incEmpty, incEdge] = ...
                  parsePairedArguments (optNames, dfValues, nvArgs(:));
      if (! (isscalar (incMiss) && (islogical (incMiss) || isnumeric (incMiss))))
        error (strcat ("table.groupsummary: 'IncludeMissingGroups' must be", ...
                       " a logical scalar."));
      endif
      incMiss = logical (incMiss);
      if (! (isscalar (incEmpty)
             && (islogical (incEmpty) || isnumeric (incEmpty))))
        error (strcat ("table.groupsummary: 'IncludeEmptyGroups' must be", ...
                       " a logical scalar."));
      endif
      incEmpty = logical (incEmpty);
      incEdge = check_included_edge ('groupsummary', incEdge);

      ## Normalise METHOD into parallel cell arrays of method specs and the
      ## display names used to build output variable names.
      [methods, methNames, errmsg] = gs_normalise_methods (method);
      if (! isempty (errmsg))
        error ("table.groupsummary: %s", errmsg);
      endif

      ## Resolve grouping and data variables.  The default data variables are all
      ## variables that are not grouping variables.
      gIx = resolveVarRef (T, groupvars)(:)';
      if (isempty (gIx))
        error ("table.groupsummary: at least one grouping variable is required.");
      endif
      if (hasDataVars)
        dIx = resolveVarRef (T, datavars)(:)';
      else
        dIx = 1:width (T);
        dIx(ismember (dIx, gIx)) = [];
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given.
      grpCols = T.VariableValues(gIx);
      if (hasGroupbins)
        [grpCols, errmsg] = bin_groupvars (grpCols, T.VariableNames(gIx), ...
                                           groupbins, incEdge, 'groupsummary');
        if (! isempty (errmsg))
          error ("table.groupsummary: %s", errmsg);
        endif
      endif

      ## Group the rows, treating missing grouping values as their own groups
      ## (sorted last) when IncludeMissingGroups is true; IncludeEmptyGroups adds
      ## the unused categories of a categorical or binned grouping variable as
      ## empty groups.
      [Grp, ng, gcols, errmsg] = gs_grouping (grpCols, incMiss, incEmpty);
      if (! isempty (errmsg))
        error ("table.groupsummary: %s", errmsg);
      endif
      gcount = accumarray (Grp(! isnan (Grp)), 1, [ng, 1]);

      ## Compute each method over each data variable.  Output columns are ordered
      ## data variable first, then method, to match MATLAB's column order.
      datNames = T.VariableNames(dIx);
      rescols = {};
      resNames = {};
      for di = 1:numel (dIx)
        for mi = 1:numel (methods)
          col = T.VariableValues{dIx(di)};
          vals = cell (ng, 1);
          for g = 1:ng
            rows = (Grp == g);
            [v, errmsg] = gs_apply_method (methods{mi}, col(rows,:));
            if (! isempty (errmsg))
              error ("table.groupsummary: %s (variable '%s').", errmsg, ...
                     datNames{di});
            endif
            vals{g} = v;
          endfor
          try
            rescols{end+1} = vertcat (vals{:});
          catch
            error (strcat ("table.groupsummary: the '%s' results for", ...
                           " variable '%s' cannot be concatenated into a", ...
                           " column."), methNames{mi}, datNames{di});
          end_try_catch
          resNames{end+1} = sprintf ("%s_%s", methNames{mi}, datNames{di});
        endfor
      endfor

      vars = [gcols, {gcount}, rescols];
      names = [T.VariableNames(gIx), {'GroupCount'}, resNames];
      G = table (vars{:}, 'VariableNames', names);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{G} =} groupcounts (@var{T}, @var{groupvars})
    ## @deftypefnx {table} {@var{G} =} groupcounts (@var{T}, @var{groupvars}, @var{groupbins})
    ## @deftypefnx {table} {@var{G} =} groupcounts (@dots{}, @var{Name}, @var{Value})
    ##
    ## Count the number of rows in each group of a table.
    ##
    ## @code{@var{G} = groupcounts (@var{T}, @var{groupvars})} groups the rows of
    ## the table @var{T} by the grouping variables @var{groupvars} and returns the
    ## table @var{G} with one row per group, holding the grouping variables, a
    ## @qcode{GroupCount} variable counting the rows in each group, and a
    ## @qcode{Percent} variable giving each group's count as a percentage of the
    ## total.  @var{groupvars} selects the grouping variables by name, index,
    ## logical vector, function handle, or @code{vartype} subscript.
    ##
    ## The optional @var{groupbins} argument bins the grouping variables before
    ## grouping (a vector of bin edges or a positive integer number of bins,
    ## applied to a numeric, datetime, or duration grouping variable, or a cell
    ## array with one scheme per grouping variable); each binned variable becomes
    ## a categorical of bin interval labels.  See @code{groupsummary} for details.
    ##
    ## Groups are the sorted unique combinations of grouping values.  The following
    ## @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'IncludeMissingGroups'}
    ## A logical scalar.  When @code{true} (the default), rows holding a missing
    ## value in a grouping variable form their own groups, sorted after the
    ## non-missing groups.  When @code{false}, such rows are excluded.
    ##
    ## @item @qcode{'IncludeEmptyGroups'}
    ## A logical scalar, @code{false} by default.  When @code{true}, the unused
    ## categories of a categorical or binned grouping variable contribute empty
    ## groups with a @qcode{GroupCount} of @code{0}.
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Either @qcode{'left'} (the default) or @qcode{'right'}, selecting which
    ## edge of each bin is inclusive when @var{groupbins} is given.
    ## @end table
    ##
    ## @end deftypefn
    function G = groupcounts (T, groupvars, varargin)
      if (nargin < 2)
        print_usage ();
      endif

      ## An optional GROUPBINS positional argument may precede the Name-Value
      ## options; anything else after GROUPVARS must be a recognised option.
      optNames = {'IncludeMissingGroups', 'IncludeEmptyGroups', 'IncludedEdge'};
      hasGroupbins = false;
      groupbins = [];
      if (! isempty (varargin))
        a = varargin{1};
        isOpt = ((ischar (a) && isrow (a)) ...
                 || (isa (a, 'string') && isscalar (a))) ...
                && any (strcmpi (char (a), optNames));
        if (! isOpt)
          if (is_groupbins_spec (a))
            hasGroupbins = true;
            groupbins = a;
            varargin = varargin(2:end);
          else
            error (strcat ("table.groupcounts: invalid argument; expected a", ...
                           " GROUPBINS binning scheme or a Name-Value option."));
          endif
        endif
      endif

      ## Parse Name-Value options.
      dfValues = {true, false, 'left'};
      [incMiss, incEmpty, incEdge] = ...
                  parsePairedArguments (optNames, dfValues, varargin(:));
      if (! (isscalar (incMiss) && (islogical (incMiss) || isnumeric (incMiss))))
        error (strcat ("table.groupcounts: 'IncludeMissingGroups' must be", ...
                       " a logical scalar."));
      endif
      incMiss = logical (incMiss);
      if (! (isscalar (incEmpty)
             && (islogical (incEmpty) || isnumeric (incEmpty))))
        error (strcat ("table.groupcounts: 'IncludeEmptyGroups' must be", ...
                       " a logical scalar."));
      endif
      incEmpty = logical (incEmpty);
      incEdge = check_included_edge ('groupcounts', incEdge);

      ## Resolve grouping variables.
      gIx = resolveVarRef (T, groupvars)(:)';
      if (isempty (gIx))
        error (strcat ("table.groupcounts: at least one grouping variable", ...
                       " is required."));
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given.
      grpCols = T.VariableValues(gIx);
      if (hasGroupbins)
        [grpCols, errmsg] = bin_groupvars (grpCols, T.VariableNames(gIx), ...
                                           groupbins, incEdge, 'groupcounts');
        if (! isempty (errmsg))
          error ("table.groupcounts: %s", errmsg);
        endif
      endif

      ## Group the rows, treating missing grouping values as their own groups
      ## (sorted last) when IncludeMissingGroups is true; IncludeEmptyGroups adds
      ## the unused categories of a categorical or binned grouping variable as
      ## empty groups.
      [Grp, ng, gcols, errmsg] = gs_grouping (grpCols, incMiss, incEmpty);
      if (! isempty (errmsg))
        error ("table.groupcounts: %s", errmsg);
      endif
      gcount = accumarray (Grp(! isnan (Grp)), 1, [ng, 1]);
      pcent = 100 * gcount / sum (gcount);

      vars = [gcols, {gcount, pcent}];
      names = [T.VariableNames(gIx), {'GroupCount', 'Percent'}];
      G = table (vars{:}, 'VariableNames', names);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{G} =} groupfilter (@var{T}, @var{groupvars}, @var{method})
    ## @deftypefnx {table} {@var{G} =} groupfilter (@var{T}, @var{groupvars}, @var{groupbins}, @var{method})
    ## @deftypefnx {table} {@var{G} =} groupfilter (@dots{}, @var{method}, @var{datavars})
    ##
    ## Filter the rows of a table by a per-group condition.
    ##
    ## @code{@var{G} = groupfilter (@var{T}, @var{groupvars}, @var{method})} groups
    ## the rows of the table @var{T} by the grouping variables @var{groupvars},
    ## applies the filter function @var{method} to each group, and returns the
    ## table @var{G} holding the rows that satisfy the condition, in their original
    ## order and with all the variables of @var{T}.  @var{groupvars} selects the
    ## grouping variables by name, index, logical vector, function handle, or
    ## @code{vartype} subscript.
    ##
    ## @var{method} is a function handle applied to each group's slice of every
    ## data variable.  It must return either a logical scalar, which keeps or drops
    ## the whole group, or a logical vector with one element per row of the group,
    ## which keeps or drops the individual rows.  A row is kept only when the
    ## condition holds for it across all data variables.
    ##
    ## @code{@var{G} = groupfilter (@var{T}, @var{groupvars}, @var{method},
    ## @var{datavars})} applies @var{method} only to the data variables selected by
    ## @var{datavars} (named, indexed, logical, function handle, or @code{vartype}
    ## subscript).  By default every variable that is not a grouping variable is a
    ## data variable.
    ##
    ## Rows holding a missing value in a grouping variable form their own groups,
    ## to which @var{method} is applied like any other group.
    ##
    ## The optional @var{groupbins} argument bins the grouping variables before
    ## grouping (a vector of bin edges or a positive integer number of bins, or a
    ## cell array with one scheme per grouping variable); see @code{groupsummary}
    ## for details.  The @qcode{'IncludedEdge'} Name-Value pair (@qcode{'left'} by
    ## default, or @qcode{'right'}) selects which bin edge is inclusive.
    ##
    ## @end deftypefn
    function G = groupfilter (T, groupvars, varargin)
      if (nargin < 3)
        print_usage ();
      endif

      ## Split off a trailing 'IncludedEdge' Name-Value option, then an optional
      ## GROUPBINS positional argument that precedes the filter function METHOD.
      optNames = {'IncludedEdge'};
      args = varargin;
      nvStart = numel (args) + 1;
      for k = 1:numel (args)
        a = args{k};
        if (((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a)))
            && any (strcmpi (char (a), optNames)))
          nvStart = k;
          break;
        endif
      endfor
      nvArgs = args(nvStart:end);
      args = args(1:nvStart-1);
      incEdge = parsePairedArguments (optNames, {'left'}, nvArgs(:));
      incEdge = check_included_edge ('groupfilter', incEdge);

      hasGroupbins = false;
      groupbins = [];
      if (! isempty (args) && is_groupbins_spec (args{1}))
        hasGroupbins = true;
        groupbins = args{1};
        args = args(2:end);
      endif

      ## The filter function METHOD is the first remaining argument.
      if (isempty (args))
        print_usage ();
      endif
      method = args{1};
      if (! is_function_handle (method))
        error ("table.groupfilter: METHOD must be a function handle.");
      endif

      ## An optional DATAVARS argument may follow the filter function.
      rest = args(2:end);
      if (numel (rest) > 1)
        error ("table.groupfilter: too many positional arguments.");
      endif
      if (numel (rest) == 1)
        datavars = rest{1};
        hasDataVars = true;
      else
        datavars = [];
        hasDataVars = false;
      endif

      ## Resolve grouping and data variables.  The default data variables are all
      ## variables that are not grouping variables.
      gIx = resolveVarRef (T, groupvars)(:)';
      if (isempty (gIx))
        error (strcat ("table.groupfilter: at least one grouping variable", ...
                       " is required."));
      endif
      if (hasDataVars)
        dIx = resolveVarRef (T, datavars)(:)';
      else
        dIx = 1:width (T);
        dIx(ismember (dIx, gIx)) = [];
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given, then
      ## group the rows, treating missing grouping values as their own groups so
      ## that every row belongs to exactly one group.
      grpCols = T.VariableValues(gIx);
      if (hasGroupbins)
        [grpCols, errmsg] = bin_groupvars (grpCols, T.VariableNames(gIx), ...
                                           groupbins, incEdge, 'groupfilter');
        if (! isempty (errmsg))
          error ("table.groupfilter: %s", errmsg);
        endif
      endif
      [Grp, ng, ~, errmsg] = gs_group_rows (grpCols, true);
      if (! isempty (errmsg))
        error ("table.groupfilter: %s", errmsg);
      endif

      ## Build the row keep-mask by applying METHOD to each data variable.
      [keep, errmsg] = gf_keep_mask (method, T.VariableValues(dIx), Grp, ng);
      if (! isempty (errmsg))
        error ("table.groupfilter: %s", errmsg);
      endif

      G = subsetrows (T, find (keep));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{G} =} grouptransform (@var{T}, @var{groupvars}, @var{method})
    ## @deftypefnx {table} {@var{G} =} grouptransform (@var{T}, @var{groupvars}, @var{groupbins}, @var{method})
    ## @deftypefnx {table} {@var{G} =} grouptransform (@dots{}, @var{method}, @var{datavars})
    ## @deftypefnx {table} {@var{G} =} grouptransform (@dots{}, @var{Name}, @var{Value})
    ##
    ## Transform the data variables of a table group by group.
    ##
    ## @code{@var{G} = grouptransform (@var{T}, @var{groupvars}, @var{method})}
    ## groups the rows of the table @var{T} by the grouping variables
    ## @var{groupvars}, applies @var{method} to each data variable within each
    ## group, and returns the table @var{G} with the transformed values, one row
    ## per row of @var{T} and in the original order.  @var{groupvars} selects the
    ## grouping variables by name, index, logical vector, function handle, or
    ## @code{vartype} subscript.
    ##
    ## @var{method} is one of the transform names below or a function handle:
    ##
    ## @table @asis
    ## @item @qcode{'zscore'}
    ## Center and scale each group to zero mean and unit standard deviation.
    ##
    ## @item @qcode{'norm'}
    ## Divide each group by its 2-norm.
    ##
    ## @item @qcode{'meancenter'}
    ## Subtract the group mean.
    ##
    ## @item @qcode{'rescale'}
    ## Rescale each group to the range @code{[0, 1]}.
    ##
    ## @item @qcode{'meanfill'}
    ## Replace missing values with the group mean.
    ##
    ## @item @qcode{'linearfill'}
    ## Fill missing values by linear interpolation within the group; leading and
    ## trailing missing values are left unchanged.
    ## @end table
    ##
    ## For the named methods @code{NaN} values are omitted when computing the
    ## group statistics.  A function handle is applied to each group's slice of
    ## each data variable and must return either a single row (broadcast to all
    ## the group's rows) or a result with one row per row of the group.
    ##
    ## @code{@var{G} = grouptransform (@var{T}, @var{groupvars}, @var{method},
    ## @var{datavars})} transforms only the data variables selected by
    ## @var{datavars} (named, indexed, logical, function handle, or @code{vartype}
    ## subscript).  By default every variable that is not a grouping variable is a
    ## data variable.
    ##
    ## The following @var{Name}/@var{Value} pair is accepted:
    ##
    ## @table @asis
    ## @item @qcode{'ReplaceValues'}
    ## A logical scalar.  When @code{true} (the default), each data variable is
    ## replaced by its transformed values.  When @code{false}, the transformed
    ## values are appended as new variables named @code{<method>_<datavar>}
    ## (@code{fun1_<datavar>} for a function handle), leaving the originals in
    ## place.
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Either @qcode{'left'} (the default) or @qcode{'right'}, selecting which
    ## edge of each bin is inclusive when @var{groupbins} is given.
    ## @end table
    ##
    ## Rows holding a missing value in a grouping variable form their own groups,
    ## which are transformed like any other group.  The optional @var{groupbins}
    ## argument bins the grouping variables before grouping (a vector of bin edges
    ## or a positive integer number of bins, or a cell array with one scheme per
    ## grouping variable); see @code{groupsummary} for details.
    ##
    ## @end deftypefn
    function G = grouptransform (T, groupvars, varargin)
      if (nargin < 3)
        print_usage ();
      endif

      ## An optional GROUPBINS positional argument precedes the transform METHOD
      ## (a known method name or a function handle).
      args = varargin;
      hasGroupbins = false;
      groupbins = [];
      if (! isempty (args) && is_groupbins_spec (args{1}))
        hasGroupbins = true;
        groupbins = args{1};
        args = args(2:end);
      endif
      if (isempty (args))
        print_usage ();
      endif
      method = args{1};
      knownMethods = {'zscore', 'norm', 'meancenter', 'rescale', ...
                      'meanfill', 'linearfill'};
      if (is_function_handle (method))
        methDisp = 'fun1';
      elseif (((ischar (method) && isrow (method))
               || (isa (method, 'string') && isscalar (method)))
              && any (strcmpi (char (method), knownMethods)))
        method = lower (char (method));
        methDisp = method;
      else
        error (strcat ("table.grouptransform: METHOD must be one of 'zscore',", ...
                       " 'norm', 'meancenter', 'rescale', 'meanfill',", ...
                       " 'linearfill', or a function handle."));
      endif

      ## Split the remaining arguments into the optional positional DATAVARS and
      ## any Name-Value pairs (a Name-Value region starts at the first option).
      rest = args(2:end);
      optNames = {'ReplaceValues', 'IncludedEdge'};
      nvStart = numel (rest) + 1;
      for k = 1:numel (rest)
        a = rest{k};
        if (((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a)))
            && any (strcmpi (char (a), optNames)))
          nvStart = k;
          break;
        endif
      endfor
      posArgs = rest(1:nvStart-1);
      nvArgs = rest(nvStart:end);
      if (numel (posArgs) > 1)
        error ("table.grouptransform: too many positional arguments.");
      endif
      if (numel (posArgs) == 1)
        datavars = posArgs{1};
        hasDataVars = true;
      else
        datavars = [];
        hasDataVars = false;
      endif

      dfValues = {true, 'left'};
      [replaceVals, incEdge] = ...
                  parsePairedArguments (optNames, dfValues, nvArgs(:));
      if (! (isscalar (replaceVals)
             && (islogical (replaceVals) || isnumeric (replaceVals))))
        error (strcat ("table.grouptransform: 'ReplaceValues' must be a", ...
                       " logical scalar."));
      endif
      replaceVals = logical (replaceVals);
      incEdge = check_included_edge ('grouptransform', incEdge);

      ## Resolve grouping and data variables.  The default data variables are all
      ## variables that are not grouping variables.
      gIx = resolveVarRef (T, groupvars)(:)';
      if (isempty (gIx))
        error (strcat ("table.grouptransform: at least one grouping variable", ...
                       " is required."));
      endif
      if (hasDataVars)
        dIx = resolveVarRef (T, datavars)(:)';
      else
        dIx = 1:width (T);
        dIx(ismember (dIx, gIx)) = [];
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given, then
      ## group the rows, treating missing grouping values as their own groups so
      ## that every row belongs to exactly one group.
      grpCols = T.VariableValues(gIx);
      if (hasGroupbins)
        [grpCols, errmsg] = bin_groupvars (grpCols, T.VariableNames(gIx), ...
                                           groupbins, incEdge, 'grouptransform');
        if (! isempty (errmsg))
          error ("table.grouptransform: %s", errmsg);
        endif
      endif
      [Grp, ng, ~, errmsg] = gs_group_rows (grpCols, true);
      if (! isempty (errmsg))
        error ("table.grouptransform: %s", errmsg);
      endif

      ## Transform each data variable, group by group.
      transCols = cell (1, numel (dIx));
      for i = 1:numel (dIx)
        [tc, errmsg] = gt_transform_col (method, T.VariableValues{dIx(i)}, ...
                                         Grp, ng);
        if (! isempty (errmsg))
          error ("table.grouptransform: %s (variable '%s').", errmsg, ...
                 T.VariableNames{dIx(i)});
        endif
        transCols{i} = tc;
      endfor

      if (replaceVals)
        G = T;
        for i = 1:numel (dIx)
          G.VariableValues{dIx(i)} = transCols{i};
        endfor
      else
        newNames = cell (1, numel (dIx));
        for i = 1:numel (dIx)
          newNames{i} = sprintf ("%s_%s", methDisp, T.VariableNames{dIx(i)});
        endfor
        G = addvars (T, transCols{:}, 'NewVariableNames', newNames);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{P} =} pivot (@var{T}, @qcode{'Columns'}, @var{colvars})
    ## @deftypefnx {table} {@var{P} =} pivot (@var{T}, @qcode{'Rows'}, @var{rowvars})
    ## @deftypefnx {table} {@var{P} =} pivot (@dots{}, @var{Name}, @var{Value})
    ##
    ## Summarize tabular data in a pivoted table.
    ##
    ## @code{@var{P} = pivot (@var{T}, 'Columns', @var{colvars}, 'Rows',
    ## @var{rowvars})} reshapes the table @var{T} into the pivoted table @var{P}.
    ## The unique combinations of the grouping variables @var{colvars} become the
    ## variables (columns) of @var{P}, the unique combinations of the grouping
    ## variables @var{rowvars} become its rows, and each cell holds one statistic
    ## computed over the rows of @var{T} that fall into that row-and-column group.
    ## At least one of @qcode{'Columns'} or @qcode{'Rows'} is required; an omitted
    ## dimension collapses to a single group.  Each of @var{colvars} and
    ## @var{rowvars} selects variables by name, index, or logical vector, and may
    ## name several variables.
    ##
    ## Groups are the sorted unique combinations of the grouping values, with the
    ## first variable varying slowest; a categorical variable groups by its
    ## category order.  Column variable names are taken from the grouping values
    ## (e.g.@: @qcode{'true'}/@qcode{'false'} for a logical variable), joined
    ## with @qcode{'_'} when several variables define the columns.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'DataVariable'}
    ## The single variable whose values are aggregated.  When omitted, the cells
    ## hold group counts.
    ##
    ## @item @qcode{'Method'}
    ## The aggregation applied to @qcode{'DataVariable'}: one of
    ## @qcode{'count'}, @qcode{'sum'}, @qcode{'mean'}, @qcode{'median'},
    ## @qcode{'mode'}, @qcode{'std'}, @qcode{'var'}, @qcode{'min'},
    ## @qcode{'max'}, @qcode{'range'}, @qcode{'nummissing'},
    ## @qcode{'numunique'}, @qcode{'nnz'}, @qcode{'percentage'},
    ## @qcode{'none'}, or a function handle.  Named methods omit missing
    ## values.  The default is @qcode{'count'} when no data variable is given
    ## or the data variable is non-numeric, and @qcode{'sum'} when it is
    ## numeric.  @qcode{'none'} rearranges the data without aggregating and
    ## requires at most one value per cell.
    ##
    ## @item @qcode{'IncludeMissingGroups'}
    ## A logical scalar, @code{true} by default.  When @code{true}, rows
    ## holding a missing value in a grouping variable form their own group,
    ## sorted last; when @code{false}, such rows are excluded.
    ##
    ## @item @qcode{'IncludeEmptyGroups'}
    ## A logical scalar, @code{false} by default.  When @code{true}, every
    ## category of a categorical grouping variable contributes a group even
    ## if it is unused in the data, so unused combinations appear as empty
    ## cells.
    ##
    ## @item @qcode{'IncludeTotals'}
    ## A logical scalar, @code{false} by default.  When @code{true}, a
    ## @qcode{'Total'} marginal row and/or column holding the same statistic
    ## computed over each margin is appended.  Row labels are then placed in the
    ## row names.
    ##
    ## @item @qcode{'RowLabelPlacement'}
    ## Either @qcode{'variable'} (the default), which keeps the row grouping
    ## variables as the leftmost variables of @var{P}, or @qcode{'rownames'},
    ## which places the row group labels in the @code{RowNames} property.
    ##
    ## @item @qcode{'ColumnsBinMethod'}, @qcode{'RowsBinMethod'}
    ## A binning scheme applied to the @qcode{'Columns'} or @qcode{'Rows'}
    ## grouping variables before pivoting: a vector of bin edges or a positive
    ## integer number of equal-width bins, applied to a numeric, datetime, or
    ## duration grouping variable, or a cell array with one scheme per variable.
    ## Each binned variable becomes a categorical of bin interval labels.  The
    ## default @qcode{'none'} applies no binning.
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Either @qcode{'left'} (the default) or @qcode{'right'}, selecting which
    ## edge of each bin is inclusive when a binning scheme is given.
    ##
    ## @item @qcode{'OutputFormat'}
    ## Only @qcode{'flat'} (the default) is supported; @qcode{'nested'}
    ## raises an error.
    ## @end table
    ##
    ## @end deftypefn
    function P = pivot (T, varargin)
      if (nargin < 1)
        print_usage ();
      endif

      ## Parse Name-Value options; unrecognised names land in REST.
      optNames = {'Columns', 'Rows', 'DataVariable', 'Method', ...
                  'IncludeMissingGroups', 'IncludeEmptyGroups', ...
                  'IncludeTotals', 'RowLabelPlacement', 'OutputFormat', ...
                  'ColumnsBinMethod', 'RowsBinMethod', 'IncludedEdge'};
      dfValues = {[], [], [], [], true, false, false, 'variable', 'flat', ...
                  'none', 'none', 'left'};
      [colvars, rowvars, datavar, method, incMiss, incEmpty, incTot, ...
       rowPlace, outFmt, colBin, rowBin, incEdge, rest] = ...
                  parsePairedArguments (optNames, dfValues, varargin(:));
      if (! isempty (rest))
        bad = rest{1};
        if (isa (bad, 'string'))
          bad = char (bad);
        endif
        if (ischar (bad) && isrow (bad))
          error ("table.pivot: unrecognised option '%s'.", bad);
        else
          error ("table.pivot: invalid optional arguments.");
        endif
      endif

      ## Validate the IncludedEdge binning option; the 'nested' output format is
      ## not yet supported.
      incEdge = check_included_edge ('pivot', incEdge);
      if (isa (outFmt, 'string'))
        outFmt = char (outFmt);
      endif
      if (! (ischar (outFmt) && isrow (outFmt)))
        error ("table.pivot: 'OutputFormat' must be 'flat' or 'nested'.");
      endif
      if (strcmpi (outFmt, 'nested'))
        error (strcat ("table.pivot: OutputFormat 'nested' is not yet", ...
                       " supported; use 'flat'."));
      elseif (! strcmpi (outFmt, 'flat'))
        error ("table.pivot: 'OutputFormat' must be 'flat' or 'nested'.");
      endif

      ## Validate the logical-scalar and RowLabelPlacement options.
      incMiss = pivot_logical_opt ('IncludeMissingGroups', incMiss);
      incEmpty = pivot_logical_opt ('IncludeEmptyGroups', incEmpty);
      incTot = pivot_logical_opt ('IncludeTotals', incTot);
      if (isa (rowPlace, 'string'))
        rowPlace = char (rowPlace);
      endif
      if (! (ischar (rowPlace) && isrow (rowPlace) ...
             && any (strcmpi (rowPlace, {'variable', 'rownames'}))))
        error (strcat ("table.pivot: 'RowLabelPlacement' must be 'variable'", ...
                       " or 'rownames'."));
      endif
      rowPlace = lower (rowPlace);

      ## At least one grouping dimension is required.
      if (isempty (colvars) && isempty (rowvars))
        error (strcat ("table.pivot: specify at least one of 'Columns' or", ...
                       " 'Rows'."));
      endif

      ## Resolve grouping and data variables.
      if (isempty (colvars))
        colIx = [];
      else
        colIx = resolveVarRef (T, colvars)(:)';
      endif
      if (isempty (rowvars))
        rowIx = [];
      else
        rowIx = resolveVarRef (T, rowvars)(:)';
      endif
      hasDV = ! isempty (datavar);
      if (hasDV)
        dvIx = resolveVarRef (T, datavar)(:)';
        if (! isscalar (dvIx))
          error (strcat ("table.pivot: 'DataVariable' must specify a single", ...
                         " variable."));
        endif
        dataVals = T.VariableValues{dvIx};
      else
        dvIx = [];
        dataVals = [];
      endif

      ## Resolve the aggregation method and its default.
      reqDataMethods = {'sum', 'mean', 'median', 'mode', 'std', 'var', ...
                        'min', 'max', 'range', 'nnz'};
      knownMethods = [{'count', 'percentage', 'nummissing', 'numunique', ...
                       'none'}, reqDataMethods];
      if (isempty (method))
        if (! hasDV || ! (isnumeric (dataVals) || islogical (dataVals)))
          method = 'count';
        else
          method = 'sum';
        endif
      elseif (isa (method, 'string') && isscalar (method))
        method = char (method);
      endif
      if (ischar (method))
        method = lower (method);
        if (! (isrow (method) && any (strcmp (method, knownMethods))))
          error ("table.pivot: unknown Method '%s'.", method);
        endif
        if (! hasDV && (any (strcmp (method, reqDataMethods)) ...
                        || strcmp (method, 'none')))
          error ("table.pivot: Method '%s' requires a 'DataVariable'.", method);
        endif
      elseif (! is_function_handle (method))
        error (strcat ("table.pivot: 'Method' must be a method name or a", ...
                       " function handle."));
      endif
      isNone = ischar (method) && strcmp (method, 'none');
      if (isNone && incTot)
        error (strcat ("table.pivot: 'IncludeTotals' is not supported with", ...
                       " Method 'none'."));
      endif
      ## The display name of the method labels the single output variable when
      ## 'Columns' is omitted and the 'Overall_<method>' totals row and column.
      if (ischar (method))
        methodName = method;
      else
        methodName = apply_func_name (method);
      endif
      totalLabel = ['Overall_', methodName];

      ## Bin the row/column grouping variables when a 'RowsBinMethod' or
      ## 'ColumnsBinMethod' was given (a per-variable cell or a single scheme).
      rowGcols = T.VariableValues(rowIx);
      colGcols = T.VariableValues(colIx);
      rowNone = ((ischar (rowBin) && isrow (rowBin)) ...
                 || (isa (rowBin, 'string') && isscalar (rowBin))) ...
                && strcmpi (char (rowBin), 'none');
      colNone = ((ischar (colBin) && isrow (colBin)) ...
                 || (isa (colBin, 'string') && isscalar (colBin))) ...
                && strcmpi (char (colBin), 'none');
      if (! rowNone && ! isempty (rowIx))
        [rowGcols, emsg] = bin_groupvars (rowGcols, T.VariableNames(rowIx), ...
                                          rowBin, incEdge, 'pivot');
        if (! isempty (emsg))
          error ("table.pivot: %s", emsg);
        endif
      endif
      if (! colNone && ! isempty (colIx))
        [colGcols, emsg] = bin_groupvars (colGcols, T.VariableNames(colIx), ...
                                          colBin, incEdge, 'pivot');
        if (! isempty (emsg))
          error ("table.pivot: %s", emsg);
        endif
      endif

      ## Group the rows along each dimension.
      n = height (T);
      [rGid, nR, rLvlOf, rLevVals, ~, emsg] = ...
              pivot_dimension (rowGcols, n, incMiss, incEmpty);
      if (! isempty (emsg))
        error ("table.pivot: %s.", emsg);
      endif
      [cGid, nC, cLvlOf, cLevVals, cMissLvls, emsg] = ...
              pivot_dimension (colGcols, n, incMiss, incEmpty);
      if (! isempty (emsg))
        error ("table.pivot: %s.", emsg);
      endif
      assigned = ! isnan (rGid) & ! isnan (cGid);
      totalAssigned = sum (assigned);

      ## Build one output data column per column group.
      dataCols = cell (1, nC);
      for c = 1:nC
        if (isNone)
          [col, emsg] = pivot_none_column (dataVals, rGid, cGid, c, nR);
          if (! isempty (emsg))
            error ("table.pivot: %s.", emsg);
          endif
        else
          col = NaN (nR, 1);
          for r = 1:nR
            rows = find (rGid == r & cGid == c);
            [v, emsg] = ...
                pivot_cell_value (method, hasDV, dataVals, rows, totalAssigned);
            if (! isempty (emsg))
              error ("table.pivot: %s.", emsg);
            endif
            col(r) = v;
          endfor
        endif
        dataCols{c} = col;
      endfor

      ## Column variable names.  With no 'Columns', a single variable named for
      ## the method ('<method>_<datavar>', or just 'count' with no data variable);
      ## otherwise one per column group, from the grouping values (a missing group
      ## becomes '<missing_<varname>>').
      if (isempty (colIx))
        if (hasDV)
          colNames = {[methodName, '_', T.VariableNames{dvIx}]};
        else
          colNames = {methodName};
        endif
      else
        colNames = cell (1, nC);
        for c = 1:nC
          parts = cell (1, numel (colIx));
          for j = 1:numel (colIx)
            lvl = cLvlOf(c,j);
            if (cMissLvls{j}(lvl))
              parts{j} = sprintf ("<missing_%s>", T.VariableNames{colIx(j)});
            else
              parts{j} = pivot_value_name (cLevVals{j}(lvl, :));
            endif
          endfor
          colNames{c} = strjoin (parts, '_');
        endfor
      endif

      ## Row label columns (one per row grouping variable).
      if (isempty (rowIx))
        rowLabelCols = {};
        rowLabelNames = {};
      else
        rowLabelCols = cell (1, numel (rowIx));
        for j = 1:numel (rowIx)
          rowLabelCols{j} = rLevVals{j}(rLvlOf(:,j), :);
        endfor
        rowLabelNames = T.VariableNames(rowIx);
      endif

      ## Marginal totals, recomputed over each margin from the raw data.
      addTotalCol = incTot && ! isempty (colIx);
      addTotalRow = incTot && ! isempty (rowIx);
      if (incTot)
        colMargin = NaN (nR, 1);
        for r = 1:nR
          rows = find (rGid == r & assigned);
          [colMargin(r), emsg] = ...
              pivot_cell_value (method, hasDV, dataVals, rows, totalAssigned);
          if (! isempty (emsg))
            error ("table.pivot: %s.", emsg);
          endif
        endfor
        rowMargin = NaN (1, nC);
        for c = 1:nC
          rows = find (cGid == c & assigned);
          [rowMargin(c), emsg] = ...
              pivot_cell_value (method, hasDV, dataVals, rows, totalAssigned);
          if (! isempty (emsg))
            error ("table.pivot: %s.", emsg);
          endif
        endfor
        rows = find (assigned);
        [grand, emsg] = ...
            pivot_cell_value (method, hasDV, dataVals, rows, totalAssigned);
        if (! isempty (emsg))
          error ("table.pivot: %s.", emsg);
        endif
        if (addTotalRow)
          for c = 1:nC
            dataCols{c} = [dataCols{c}; rowMargin(c)];
          endfor
        endif
        if (addTotalCol)
          tcol = colMargin;
          if (addTotalRow)
            tcol = [tcol; grand];
          endif
          dataCols{end+1} = tcol;
          colNames{end+1} = totalLabel;
        endif
      endif

      ## Assemble the output table.  Totals keep the row grouping variables and
      ## label the marginal row with 'Overall_<method>', appended to the single
      ## row-label variable; a multi-variable or non-text row label falls back to
      ## row-name labelling instead.
      useRowNames = strcmp (rowPlace, 'rownames');
      if (incTot && ! useRowNames && addTotalRow)
        if (numel (rowIx) == 1)
          [lc, ok] = pivot_append_label (rowLabelCols{1}, totalLabel);
          if (ok)
            rowLabelCols{1} = lc;
          else
            useRowNames = true;
          endif
        else
          useRowNames = true;
        endif
      endif

      if (useRowNames)
        if (isempty (rowIx))
          P = table (dataCols{:}, 'VariableNames', colNames);
        else
          rn = pivot_row_names (rowLabelCols);
          if (addTotalRow)
            rn = [rn; {totalLabel}];
          endif
          P = table (dataCols{:}, 'VariableNames', colNames, 'RowNames', rn);
        endif
      else
        allVars = [rowLabelCols, dataCols];
        allNames = [rowLabelNames, colNames];
        P = table (allVars{:}, 'VariableNames', allNames);
      endif
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
                          'UniformOutput', false);
      is_empty = cellfun (@isempty, varNames);
      varNames = [varNames{:}];
      if (numel (varNames) != numel (unique (varNames)))
        error (strcat ("table.horzcat: all input tables must have unique", ...
                       " variable names."));
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
                            'UniformOutput', false);
        sortedRowNames = cellfun (@sort, rowNames, 'UniformOutput', false);
        if (! isequal (sortedRowNames{:}))
          error ("table.horzcat: input tables must have identical RowNames.");
        endif
        ## We need to figure out some indexing for every other table with
        ## RowNames so that we now how to merge them with the first table
        ## with RowNames
        tbl_withRowNames = find (has_RowNames);
        index = [1:numRows]'; # first table is reindexed to itself
        for i = 2:numel (rowNames)
          ## For each row of the first table, find the matching row in the
          ## i-th table (the inverse map), so subsetrows aligns it to the first.
          fcn = @(x) find (ismember (rowNames{i}, x));
          index(:,i) = cellfun (fcn, rowNames{1});
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
      new_types = cellfun ('class', tbl.VariableValues, 'UniformOutput', false);
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
      TF = isrow (this) || iscolumn (this);
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
    ## For tables, @code{ndims (tbl)} is always 2.
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
    ## @code{@var{out} = numel (@var{tbl})} returns the number of elements in
    ## the table, @var{tbl}, equivalent to @qcode{prod (size (@var{tbl}))}.  A
    ## table is treated as a two-dimensional container, so this is the number of
    ## rows times the number of variables.  Variables may themselves span
    ## multiple columns, but @code{numel} only accounts for the number of rows
    ## and the number of variables, not the underlying columns.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = prod (size (this));
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
    function tbl = repelem (this, varargin)

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
      if (cols < 1 || fix (cols) != cols || ! isnumeric (cols))
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
                                         'UniformOutput', false);
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
        idx = num2cell (1:cols - 1);
        newNames = {};
        for i = 1:width (this)
          newNames = [newNames, this.VariableNames{i}];
          fnc = eval (["@(x) sprintf (""", this.VariableNames{i}, "_%d"", x)"]);
          addNames = cellfun (fnc, idx, 'UniformOutput', false);
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
    ## Repeats copies of the input table @var{tblA} in a similar fashion
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
      if (cols < 1 || fix (cols) != cols || ! isnumeric (cols))
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
                                         'UniformOutput', false);
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
          addNames = cellfun (fnc, this.VariableNames, 'UniformOutput', false);
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
    ## @deftypefnx {table} {@var{dim_sz} =} size (@var{tbl}, @var{dim})
    ## @deftypefnx {table} {@var{dim_sz} =} size (@var{tbl}, @var{vecdim})
    ## @deftypefnx {table} {[@var{rows}, @var{columns}] =} size (@var{tbl})
    ## @deftypefnx {table} {[@var{rows}, @var{columns}, @dots{}] =} size (@var{tbl})
    ##
    ## Return the size of a table.
    ##
    ## For tables, the size is [number-of-rows x number-of-variables].
    ## This is the same as @code{[height(obj), width(obj)]}.
    ##
    ## @code{size (@var{tbl}, @var{dim})} returns the size along dimension
    ## @var{dim}; dimensions greater than 2 have size 1.  @var{dim} may be a
    ## vector @var{vecdim}, in which case a row vector of the corresponding
    ## sizes is returned.
    ##
    ## @end deftypefn
    function varargout = size (this, dim)
      sz = [height(this), width(this)];
      if (nargin == 2)
        ## Sizes along the requested dimension(s); dimensions above 2 are 1.
        dim_sz = ones (1, numel (dim));
        valid = dim <= 2;
        dim_sz(valid) = sz(dim(valid));
        if (nargout > 1)
          varargout = num2cell (dim_sz);
        else
          varargout{1} = dim_sz;
        endif
      elseif (nargout <= 1)
        varargout{1} = sz;
      else
        varargout{1} = sz(1);
        varargout{2} = sz(2);
        [varargout{3:nargout}] = deal (1);
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
    ## When any input table has row names, they must be unique across all input
    ## tables.  In such case, rows coming from input tables without row names are
    ## assigned default @qcode{Row@var{N}} names, where @var{N} is the row's
    ## position in the output table.  Output table's @qcode{Description} and
    ## @qcode{UserData} properties are assigned using the first non-empty value.
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
                          'UniformOutput', false);
      is_empty = cellfun (@isempty, varNames);
      sortedVarNames = cellfun (@sort, varNames(! is_empty), ...
                                'UniformOutput', false);
      if (! isequal (sortedVarNames{:}))
        error (strcat ("table.vertcat: input tables must have identical", ...
                       " variable names."));
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
                          'UniformOutput', false);
      rowNames = [rowNames{:}];
      if (numel (rowNames) != numel (unique (rowNames)))
        error (strcat ("table.vertcat: all input tables must have unique", ...
                       " row names."));
      endif
      ## Start vertical concatenation
      if (! any (has_RowNames)) # no RowNames in any table (easy)
        tbl = varargin{1};
        for i = 2:numel (varargin)
          in = varargin{i};
          ixVars = index(i,:);
          in = subsetvars (in, ixVars);
          for v = 1:numCols
            tbl.VariableValues{v} = [tbl.VariableValues{v}; ...
                                     in.VariableValues{v}];
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
      else # at least one input table has RowNames
        ## Input tables without row names get default 'Row<N>' names, where N
        ## is the row's position in the output table (MATLAB-compatible).
        tbl = varargin{1};
        fcn = @(x) {sprintf("Row%d", x)};
        ## If first input table does not have row names, add them here
        if (isempty (tbl.RowNames))
          tbl.RowNames = arrayfun (fcn, 1:height (tbl))';
        endif
        pos = height (tbl);
        for i = 2:numel (varargin)
          in = varargin{i};
          ixVars = index(i,:);
          in = subsetvars (in, ixVars);
          for v = 1:numCols
            tbl.VariableValues{v} = [tbl.VariableValues{v}; ...
                                     in.VariableValues{v}];
          endfor
          ## Handle row names here
          if (isempty (in.RowNames))
            in.RowNames = arrayfun (fcn, pos + (1:height (in)))';
          endif
          tbl.RowNames = [tbl.RowNames; in.RowNames];
          pos += height (in);
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
## 'repelems'         'reshape'          'resize'           'shiftdims'       ##
## 'vec'                                                                      ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function out = repelems (this, varargin)
      error ("Function 'repelems' is not supported for tables");
    endfunction

    function out = reshape (this, varargin)
      error ("Function 'reshape' is not supported for tables");
    endfunction

    function out = resize (this, varargin)
      error ("Function 'resize' is not supported for tables");
    endfunction

    function out = shiftdims (this, varargin)
      error ("Function 'shiftdims' is not supported for tables");
    endfunction

    function out = vec (this, varargin)
      error ("Function 'vec' is not supported for tables");
    endfunction

  endmethods

################################################################################
##                  ** Reference and Assignment Operations **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'end'              'subsref'          'subsasgn'                           ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Overload 'end' keyword
    function last_index = end (this, end_dim, ndim_obj)
      lastdim = ndims (this);
      if (end_dim == ndim_obj && ndim_obj == 1)
        last_index = prod (size (this));
      elseif (end_dim == ndim_obj && end_dim < lastdim)
        last_index = prod (size (this)(end_dim:lastdim));
      else
        last_index = size (this, end_dim);
      endif
    endfunction

    ## Class specific subscripted reference
    function varargout = subsref (this, s)
      chain_s = s(2:end);
      s = s(1);
      switch (s.type)
        case '()'
          if (numel (s.subs) != 2)
            error (strcat ("table.subsref: '()' indexing of table", ...
                           " requires exactly two arguments."));
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          tbl = this;
          tbl = subsetrows (tbl, ixRow);
          tbl = subsetvars (tbl, ixVar);

        case '{}'
          if (numel (s.subs) != 2)
            error (strcat ("table.subsref: '{}' indexing of table", ...
                           " requires exactly two arguments."));
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          tbl = this;
          tbl = subsetrows (tbl, ixRow);
          tbl = subsetvars (tbl, ixVar);
          pair = mixed_cell_pair (tbl.VariableValues);
          if (! isempty (pair))
            error (strcat ("table.subsref: cannot concatenate the table", ...
                           " variables '%s' and '%s', because their types", ...
                           " are %s and %s."), tbl.VariableNames{pair(1)}, ...
                   tbl.VariableNames{pair(2)}, ...
                   class (tbl.VariableValues{pair(1)}), ...
                   class (tbl.VariableValues{pair(2)}));
          endif
          try
            tbl = table2array (tbl);
          catch
            error (strcat ("table.subsref: table cannot be concatenated", ...
                           " into a matrix"));
          end_try_catch

        case '.'
          if (! ischar (s.subs))
            error (strcat ("table.subsref: '.' index argument must be a", ...
                           " character vector."));
          endif
          ## Handle special cases: "Properties" and "DimensionNames"
          if (isequal (s.subs, 'Properties'))
            if (nargout == 0 && isempty (chain_s))
              print_properties (this);
              return;
            else
              tbl = getProperties (this);
            endif
          elseif (isequal (s.subs, this.DimensionNames{1}))
            tbl = this.RowNames;
          elseif (isequal (s.subs, this.DimensionNames{2}))
            try
              tbl = table2array (this);
            catch
              tbl = table2cell (this);
            end_try_catch
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
      if (! isempty (chain_s) && ! isequal (s.subs, 'Properties'))
        rhs_in = single_subref (this, s);
        rhs = subsasgn (rhs_in, chain_s, val);
      else
        rhs = val;
      endif

      tbl = this;
      switch (s.type)
        case '()'
          if (numel (s.subs) != 2)
            error (strcat ("table.subsasgn: '()' indexing of table", ...
                           " requires exactly two arguments."));
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          ## Check input data matches referenced elements
          if (! isequal (size (rhs), [numel(ixRow), numel(ixVar)]))
            error ("table.subsasgn: input data mismatch indexed dimensions.");
          endif
          ## Handle different cases of input data
          if (isa (rhs, 'table'))     # MATLAB compatible
            rhs = table2cell (rhs);
          endif
          if (isa (rhs, 'cell'))      # MATLAB compatible
            for i = 1:numel (ixVar)
              varData = this.VariableValues{ixVar(i)};
              col = rhs(:,i);
              try
                if (iscell (varData))
                  varData(ixRow) = col;
                else
                  varData(ixRow) = vertcat (col{:});
                endif
              catch
                error (strcat ("table.subsasgn: input data type mismatch", ...
                               " indexed variable type."));
              end_try_catch
              tbl.VariableValues{ixVar(i)} = varData;
            endfor
          else                        # Octave specific
            for i = 1:numel (ixVar)
              varData = this.VariableValues{ixVar(i)};
              try
                varData(ixRow) = rhs(:,i);
              catch
                error (strcat ("table.subsasgn: input data type mismatch", ...
                               " indexed variable type."));
              end_try_catch
              tbl.VariableValues{ixVar(i)} = varData;
            endfor
          endif

        ## {} not used in Octave for assigning values
        case '{}'
          error (strcat ("table.subsasgn: '{}' invalid indexing for", ...
                         " assigning values. Use '()' instead."));

        case '.'
          if (! ischar (s.subs))
            error (strcat ("table.subsasgn: '.' index argument must be a", ...
                           " character vector."));
          endif
          ## Grab Properties
          if (isequal (s.subs, 'Properties'))
            ## no further recursion, everything is handled here
            if (isempty (chain_s))
              error ("table.subsasgn: cannot assign new properties.");
            endif
            s = chain_s(1);

            ## Handle table properties
            if (isequal (s.subs, 'Description'))
              ## Check for valid input: character vector of string
              if (isa (val, 'string'))
                if (numel (val) > 1)
                  error (strcat ("table.subsasgn: Table description must", ...
                                 " be a character vector or a string", ...
                                 " scalar."));
                endif
                val = cellstr (val){1};
              endif
              if (! ischar (val))
                error (strcat ("table.subsasgn: Table description must", ...
                               " be a character vector or string scalar."));
              endif
              this.Description = val;
              tbl = this;

            elseif (isequal (s.subs, 'UserData'))
              ## Any kind !!
              this.UserData = val;
              tbl = this;

            elseif (isequal (s.subs, 'DimensionNames'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("table.subsasgn: cannot index", ...
                                 " DimensionNames with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple DimensionNames at once."));
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:2];
                endif
                if (! all (ismember (idx, [1:2])))
                  error (strcat ("table.subsasgn: out of bound index for", ...
                                 " DimensionNames."));
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("table.subsasgn: DimensionNames must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."));
                endif
                this.DimensionNames(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: two-element cellstring or string array
              if (ischar (val) || isa (val, 'string'))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == 2))
                error (strcat ("table.subsasgn: DimensionNames must be a", ...
                               " two-element cell array of character", ...
                               " vectors or string array."));
              endif
              this.DimensionNames = val;
              tbl = this;

            elseif (isequal (s.subs, 'VariableNames'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("table.subsasgn: cannot index", ...
                                 " VariableNames with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple VariableNames at once."));
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("table.subsasgn: out of bound index for", ...
                                 " VariableNames."));
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("table.subsasgn: VariableNames must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."));
                endif
                this.VariableNames(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, 'string'))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (strcat ("table.subsasgn: VariableNames must be a", ...
                               " cell array of character vectors or a", ...
                               " string array matching the number of", ...
                               " variables."));
              endif
              this.VariableNames = val;
              tbl = this;

            elseif (isequal (s.subs, 'VariableTypes'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("table.subsasgn: cannot index", ...
                                 " VariableTypes with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple VariableTypes at once."));
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("table.subsasgn: out of bound index for", ...
                                 " VariableTypes"));
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("table.subsasgn: VariableTypes must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."));
                endif
                ## Convert each selected variable to its new data type;
                ## convertvars updates both the data and the VariableTypes
                ## entry for the corresponding variable.
                tbl = this;
                for k = 1:numel (idx)
                  tbl = convertvars (tbl, idx(k), val{k});
                endfor
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, 'string'))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (strcat ("table.subsasgn: VariableTypes must be a", ...
                               " cell array of character vectors or a", ...
                               " string array matching the number of", ...
                               " variables."));
              endif
              ## Convert each variable to its new data type; convertvars
              ## updates both the data and the VariableTypes entry for the
              ## corresponding variable.
              tbl = this;
              for k = 1:width (this)
                tbl = convertvars (tbl, k, val{k});
              endfor

            elseif (isequal (s.subs, 'VariableDescriptions'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("table.subsasgn: cannot index", ...
                                 " VariableDescriptions with more than", ...
                                 " one dimension. Use a vector to index", ...
                                 " multiple VariableDescriptions at", ...
                                 " once."));
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("table.subsasgn: out of bound index for", ...
                                 " VariableDescriptions"));
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("table.subsasgn: VariableDescriptions", ...
                                 " must be a cell array of character", ...
                                 " vectors or a string array matching", ...
                                 " the number of indexed variables."));
                endif
                this.VariableDescriptions(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, 'string'))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (strcat ("table.subsasgn: VariableDescriptions", ...
                               " must be a cell array of character", ...
                               " vectors or a string array matching the", ...
                               " number of variables."));
              endif
              this.VariableDescriptions = val;
              tbl = this;

            elseif (isequal (s.subs, 'VariableUnits'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("table.subsasgn: cannot index", ...
                                 " VariableUnits with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple VariableUnits at once."));
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("table.subsasgn: out of bound index for", ...
                                 " VariableUnits."));
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("table.subsasgn: VariableUnits must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."));
                endif
                this.VariableUnits(idx) = val;
                tbl = this;
                return
              endif
              ## Check for valid input: cellstring or string array matching
              ## the number of variables in the table
              if (ischar (val) || isa (val, 'string'))
                val = cellstr (val);
              endif
              if (! (iscellstr (val) && numel (val) == width (this)))
                error (strcat ("table.subsasgn: VariableUnits must be a", ...
                               " cell array of character vectors or a", ...
                               " string array matching the number of", ...
                               " variables."));
              endif
              this.VariableUnits = val;
              tbl = this;

            elseif (isequal (s.subs, 'RowNames'))
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
                 ((iscellstr (val) || isa (val, 'string') || isnumeric (val)) &&
                  numel (val) == 1))
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                [ixVar, ~] = resolveVarRef (this, val, 'lenient');
                ##
                ## If variable name exists check for appropriate varTypes
                if (ixVar != 0)
                  selvar = this.VariableValues{ixVar};
                  if (iscellstr (selvar) || ischar (selvar)
                                         || isa (selvar, 'string'))
                    if (ischar (selvar) || isa (selvar, 'string'))
                      selvar = cellstr (selvar);
                    endif
                    ## RowNames must be unique, just as for the array-form
                    ## assignment and the constructor.  The referenced
                    ## variable may contain duplicates, so guard here.
                    if (numel (__unique__ (selvar)) != numel (selvar))
                      error (strcat ("table.subsasgn: elements in", ...
                                     " 'RowNames' must be unique."));
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
              ## with as many distinct elements as the table has rows.
              if (ischar (val) || isa (val, 'string'))
                val = cellstr (val);
              endif
              if (! iscellstr (val) || numel (val) != height (this))
                error (strcat ("table.subsasgn: the number of 'RowNames'", ...
                               " must equal the number of rows."));
              elseif (numel (__unique__ (val)) != numel (val))
                error (strcat ("table.subsasgn: elements in 'RowNames'", ...
                               " must be unique."));
              endif
              this.RowNames = val(:);
              tbl = this;

            elseif (isequal (s.subs, 'CustomProperties'))
              ## Check that a custom property name is indexed
              if (numel (chain_s) < 2)
                if (isempty (val))
                  error (strcat ("table.subsasgn: use 'rmprop' to remove", ...
                                 " an existing custom property."));
                else
                  error (strcat ("table.subsasgn: use 'addprop' to add a", ...
                                 " new custom property."));
                endif
              endif
              ## Check for valid indexing a custom property
              if (! strcmp (chain_s(2).type, '.'))
                error (strcat ("table.subsasgn: use '.' notation to", ...
                               " index a custom property."));
              endif
              cpName = chain_s(2).subs;
              if (! ischar (cpName))
                error (strcat ("table.subsasgn: indexing a custom", ...
                               " property requires a character vector."));
              endif
              ## Check that referenced custom property exists
              if (isempty (this.CustomProperties))
                error (strcat ("table.subsasgn: custom property '%s'", ...
                               " does not exist, use 'addprop' to add", ...
                               " it."), ...
                       cpName);
              endif
              existingNames = fieldnames (this.CustomProperties);
              if (! ismember (cpName, existingNames))
                error (strcat ("table.subsasgn: custom property '%s'", ...
                               " does not exist, use 'addprop' to add", ...
                               " it."), ...
                       cpName);
              endif
              ## Get type of custom property
              cpType = this.CustomPropTypes{strcmp (cpName, existingNames)};
              if (strcmp (cpType, 'table'))
                if (! ischar (val) && numel (val) > 1)
                  error (strcat ("table.subsasgn: custom property '%s'", ...
                                 " is a table property and only a scalar", ...
                                 " value can be assigned to it."), ...
                         cpName);
                endif
                if (numel (chain_s) > 2)
                  error (strcat ("table.subsasgn: custom property '%s'", ...
                                 " is a scalar table property and cannot", ...
                                 " be indexed any further."), ...
                         cpName);
                endif
                this.CustomProperties.(cpName) = val;
              else
                maxIdx = width (this);
                ## Check input is a vector
                if (! isvector (val))
                  error (strcat ("table.subsasgn: assigned value to a", ...
                                 " custom variable property must be a", ...
                                 " vector."));
                endif
                ## Get further indexing (if available)
                if (numel (chain_s) > 2)
                  if (strcmp (chain_s(3).type, '.'))
                    error (strcat ("table.subsasgn: custom property '%s'", ...
                                   " is a variable property and cannot", ...
                                   " be indexed any further with '.'", ...
                                   " notation."), ...
                           cpName);
                  endif
                  cpIdx = chain_s(3).subs;
                  if (numel (cpIdx) > 1)
                    error (strcat ("table.subsasgn: cannot index a", ...
                                   " custom variable property in more", ...
                                   " than one dimension."));
                  endif
                  cpIdx = cell2mat (cpIdx);
                  if (isequal (cpIdx, ':'))
                    cpIdx = [1:maxIdx];
                  endif
                  if (! all (ismember (cpIdx, [1:maxIdx])))
                    error (strcat ("table.subsasgn: out of bound index", ...
                                   " for custom variable property '%s'."), ...
                           cpName);
                  endif
                  if (numel (val) != numel (cpIdx))
                    error (strcat ("table.subsasgn: input vector does", ...
                                   " not match the number of indexed", ...
                                   " variables in the custom variable", ...
                                   " property '%s'."), ...
                           cpName);
                  endif
                  this.CustomProperties.(cpName)(cpIdx) = val;
                else
                  ## Check that input vector matches the number of variables
                  if (numel (val) != maxIdx)
                    error (strcat ("table.subsasgn: input vector does", ...
                                   " not match the number of variables", ...
                                   " in table."));
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
        strictness = 'strict';
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
        ix_bad = find (ixVar > nvars | ixVar < 1);
        if (! isempty (ix_bad))
          error (strcat ("table: variable index out of bounds: requested", ...
                         " index %d; table has %d variables."), ...
                 ixVar(ix_bad(1)), nvars);
        endif
      elseif (ischar (varRef) && isequal (varRef, ':'))
        ixVar = 1:nvars;
      elseif (ischar (varRef) || iscellstr (varRef) || isa (varRef, 'string'))
        varRef = cellstr (varRef);
        [tf, ixVar] = ismember (varRef, this.VariableNames);
        if (isequal (strictness, 'strict'))
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
      elseif (isa (varRef, 'vartype'))
        ixVar = [];
        for i = 1:nvars
          if (varRef.varMatch (this.VariableValues{i}))
            ixVar(end+1) = i;
          endif
        endfor
      elseif (is_function_handle (varRef))
        ixVar = [];
        for i = 1:nvars
          if (varRef (this.VariableValues{i}))
            ixVar(end+1) = i;
          endif
        endfor
      else
        error ("table: unsupported variable indexing operand type: '%s'.", ...
               class (varRef));
      endif
      if (nargout > 1)
        varNames = repmat ({''}, size (ixVar));
        varNames(ixVar != 0) = this.VariableNames(ixVar(ixVar != 0));
      endif
    endfunction

    ## Resolve both row and variable references to indices.
    function [ixRow, ixVar] = resolveRowVarRefs (this, rowRef, varRef)
      if (isnumeric (rowRef) || islogical (rowRef))
        ixRow = rowRef;
      elseif (isequal (rowRef, ':'))
        ixRow = 1:height (this);
      elseif (ischar (rowRef) || iscellstr (rowRef) || isa (rowRef, 'string'))
        rowRef = cellstr (rowRef);
        if (isempty (this.RowNames))
          error ("table: this table has no RowNames.");
        endif
        [tf, ixRow] = ismember (rowRef, this.RowNames);
        if (! all (tf))
          error ("table: no such named row in table: '%s'.", ...
                 strjoin (rowRef(! tf), ", "));
        endif
      else
        error ("table: unsupported row indexing operand type: '%s'.", ...
               class (rowRef));
      endif
      ixVar = resolveVarRef (this, varRef);
    endfunction

    ## Return a subset of rows defined by the numerical or logical vector ixRows
    function tbl = subsetrows (this, ixRows)
      tbl = this;
      s = struct ('type', '()', 'subs', {{ixRows,':'}});
      for i = 1:width (this)
        tbl.VariableValues{i} = subsref (tbl.VariableValues{i}, s);
      endfor
      if (! isempty (this.RowNames))
        tbl.RowNames = tbl.RowNames(ixRows);
      endif
    endfunction

    ## Build consistent numeric row proxies for two tables sharing the same set
    ## of variable names, so that equal rows (compared by variable value, in the
    ## variable order of TBLA) map to equal proxy rows.  Returns an errmsg body
    ## (empty on success) emitted by the caller under its own name.
    function [proxyA, proxyB, errmsg] = rowProxies (tblA, tblB)
      proxyA = [];
      proxyB = [];
      errmsg = '';
      if (width (tblA) != width (tblB)
          || ! isempty (setdiff (tblA.VariableNames, tblB.VariableNames)))
        errmsg = "the two tables must have the same variable names.";
        return;
      endif
      for ix = 1:width (tblA)
        jx = find (strcmp (tblA.VariableNames{ix}, tblB.VariableNames), 1);
        [pa, pb, e] = key_col_proxy (tblA.VariableValues{ix}, ...
                                     tblB.VariableValues{jx});
        if (! isempty (e))
          errmsg = e;
          return;
        endif
        proxyA = [proxyA, pa];
        proxyB = [proxyB, pb];
      endfor
    endfunction

    ## Build one side of an outer join from a row-index vector IDX (zeros mark
    ## rows with no match, filled with missing values).  Returns an errmsg body
    ## (empty on success) emitted by the caller under its own name.
    function [out, errmsg] = joinBuildSide (this, idx)
      out = this;
      errmsg = '';
      nout = numel (idx);
      pos = (idx > 0);
      for j = 1:width (this)
        p = this.VariableValues{j};
        if (any (pos))
          src = idx;
          src(! pos) = idx(find (pos, 1));
          col = p(src, :);
          [col, errmsg] = set_var_missing (col, ! pos);
        else
          [col, errmsg] = missing_rows (p, nout);
        endif
        if (! isempty (errmsg))
          return;
        endif
        out.VariableValues{j} = col;
      endfor
      out.RowNames = {};
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
      val_is_scalar = (isscalar (value) || (ischar (value) && ...
        (size (value, 1) == 1 || isequal (size (value), [0 0]))));
      if (n_rows != 1 && (isscalar (value) || (ischar (value) &&
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
                elseif (isa (tmp, 'string'))
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
        case '()'
          if (numel (s.subs) != 2)
            error (strcat ("table.subsasgn: ()-indexing of table", ...
                           " requires exactly two arguments."));
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          out = this;
          out = subsetrows (out, ixRow);
          out = subsetvars (out, ixVar);

        case '.'
          if (! ischar (s.subs))
            error (strcat ("table.subsasgn: .-index argument must be a", ...
                           " character vector."));
          endif
          ## Handle special cases: "Properties" and "DimensionNames"
          if isequal (s.subs, 'Properties')
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
          UD = strrep (UD, 'cell', 'cellstr');
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
          elseif (isa (cpValue, 'string'))
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
              elseif (isa (cpValue{idx}, 'string'))
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
      fprintf ("%+24s: '%s'\n", 'Description', D);
      fprintf ("%+24s: %s\n", 'UserData', UD);
      fprintf ("%+24s: %s\n", 'DimensionNames', DN);
      fprintf ("%+24s: %s\n", 'VariableNames', VN);
      fprintf ("%+24s: %s\n", 'VariableDescriptions', VD);
      fprintf ("%+24s: %s\n", 'VariableUnits', VU);
      fprintf ("%+24s: %s\n", 'VariableContinuity', VC);
      fprintf ("%+24s: %s\n", 'RowNames', RN);
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
              strline2 = [strline2, repmat("_", [1, T.optLen(varL_idx)]), ...
                          colgap];
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
        padPT = sprintf ("%%-%ds", rnLen);
        padfn = @(x) sprintf (padPT, x);
        rowNM = cellfun (padfn, this.RowNames, 'UniformOutput', false);
        ## Print table header
        fprintf ("    %s%s\n", repmat (" ", [1, rnLen]), strhead1);
        fprintf ("    %s%s\n\n", repmat (" ", [1, rnLen]), strline1);
        if (nested)
          fprintf ("    %s%s\n", repmat (" ", [1, rnLen]), strhead2);
          fprintf ("    %s%s\n\n", repmat (" ", [1, rnLen]), strline2);
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
              tmpData = arrayfun (numfun, data(:,c), 'UniformOutput', false);
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
            tmpData = arrayfun (numfun, data, 'UniformOutput', false);
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
              tmpData = repmat ({'false'}, size (data(:,c)));
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
            tmpData = repmat ({'false'}, size (data));
            tmpData(data) = "true";
            colData = [colData, tmpData];
            dataLen = 5;
            optLen = max ([varNLen, dataLen, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
          endif
        ## Categorical
        elseif (isa (data, {'categorical'}))
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
        elseif (any (isa (data, {'datetime', 'duration', 'calendarDuration'})))
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
        elseif (isa (data, 'string'))
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
              tmpData = cellfun (fcn, data(:,c), 'UniformOutput', false);
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
            tmpData = cellfun (fcn, data, 'UniformOutput', false);
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
        elseif (isa (data, 'struct'))
          if (cols > 1)
            rowSpat_c = "";
            for c = 1:cols
              tmpData = repmat ({'<struct>'}, size (data(:,c)));
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
            tmpData = repmat ({'<struct>'}, size (data));
            colData = [colData, tmpData];
            optLen = max ([varNLen, 8, minLen]);
            T.optLen = [T.optLen, optLen];
            rowSpat = [rowSpat, sprintf("%%-%ds", optLen), colgap];
          endif
        ## Tables (nested)
        elseif (isa (data, 'table'))
          if (nested)
            tmpData = repmat ({'<table>'}, [height(data), 1]);
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
    function s = summary_for_variables (this)
      for v = 1:width (this)
        varName = this.VariableNames{v};
        val = this.VariableValues{v};
        s.(varName).Size = size (val);
        s.(varName).Type = class (val);
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
        if (islogical (val))
          s.(varName).True = sum (val, 1);
          s.(varName).False = sum (! val, 1);
        elseif (isa (val, 'duration'))
          ## Work in seconds (native 'median' does not omit NaN), then
          ## rebuild durations preserving the variable's display format.
          sec = seconds (val);
          fmt = val.Format;
          mn = seconds (__nanmin__ (sec));
          md = seconds (median (sec, 'omitnan'));
          mx = seconds (__nanmax__ (sec));
          mn.Format = fmt;
          md.Format = fmt;
          mx.Format = fmt;
          s.(varName).Min = mn;
          s.(varName).Median = md;
          s.(varName).Max = mx;
          s.(varName).NumMissing = sum (isnan (sec), 1);
        elseif (isa (val, 'datetime'))
          ## Operate on datenum-valued doubles (NaT mapped to NaN), then
          ## rebuild datetimes from the resulting statistics.
          dn = datetime_to_datenum (val);
          s.(varName).Min = datetime (__nanmin__ (dn), ...
                                      'ConvertFrom', 'datenum');
          s.(varName).Median = datetime (median (dn, 'omitnan'), ...
                                         'ConvertFrom', 'datenum');
          s.(varName).Max = datetime (__nanmax__ (dn), ...
                                      'ConvertFrom', 'datenum');
          s.(varName).NumMissing = sum (isnan (dn), 1);
        elseif (isa (val, 'calendarDuration'))
          ## 'calendarDuration' is not totally ordered (months and days are
          ## not interconvertible), so Min/Median/Max are undefined; report
          ## only the count of missing values.
          s.(varName).NumMissing = sum (ismissing (val), 1);
        elseif (isnumeric (val))
          s.(varName).Min = __nanmin__ (val);
          s.(varName).Median = median (val, 'omitnan');
          s.(varName).Max = __nanmax__ (val);
          s.(varName).NumMissing = sum (isnan (val), 1);
        endif
        ## No need to summarize values in 'cell', 'cellstr', 'string',
        ## 'categorical', and 'struct' variable types.

        ## Fix me: as soon as CustomProperties are introduced in table class
      endfor
    endfunction

  endmethods

  ## Shared helper for the house-format ODS exporters ('table2ods' and the
  ## standalone 'struct2ods').  Hidden rather than private so 'struct2ods' can
  ## reuse the exact flattening + metadata assembly.
  methods (Hidden)

    ## Build the house-format ODS parts for THIS table: the data grid V (with
    ## ISO-formatted datetime/duration values), the per-column ODS value types,
    ## and the metadata block (a descriptive comment row followed by the
    ## variable types, names, descriptions, and units, mirroring the header
    ## block that 'table2csv' writes so 'ods2table' can reuse its parser).
    ## CALLER names the function for error reporting.
    function [V, vtype, meta] = __ods_parts__ (this, caller)
      [V, N, T, D, U] = table2cellarrays (this, 'iso');
      ## Nested tables and structs carry a multi-row (cell) type entry
      if (any (cellfun (@iscell, T)))
        error ("%s: nested tables and structs are not supported; flatten them before writing.", caller);
      endif
      Ccols = size (V, 2);
      vtype = cell (1, Ccols);
      for c = 1:Ccols
        vtype{c} = ods_value_type (T{c});
      endfor
      txt = strcat ("# varTypes %d rows; varNames %d rows;", ...
                    " varDescriptions %d rows; varUnits %d rows.");
      ## A table with no variables carries only the descriptive comment
      if (Ccols == 0)
        meta = {sprintf(txt, 0, 0, 0, 0)};
        return;
      endif
      Trows = cellfun (@(x) size (x, 1), T);
      Tmaxr = max (Trows);
      Nrows = cellfun (@(x) size (x, 1), N);
      Nmaxr = max (Nrows);
      isvar = cellfun (@(x) ! isempty (x), N(1,:));
      Drows = cellfun (@(x) size (x, 1), D);
      if (all (cellfun (@(x) ! isempty (x), D(isvar))))
        Dmaxr = max (Drows(isvar));
      else
        Dmaxr = 0;
      endif
      Urows = cellfun (@(x) size (x, 1), U);
      if (all (cellfun (@(x) ! isempty (x), U(isvar))))
        Umaxr = max (Urows(isvar));
      else
        Umaxr = 0;
      endif
      Header = repmat ({''}, Nmaxr + Tmaxr + Dmaxr + Umaxr, Ccols);
      for c = 1:Ccols
        if (isvar(c))
          Header{1,c} = T{c};
          Header{1 + Tmaxr,c} = N{c};
          if (Dmaxr)
            Header{1 + Tmaxr + Nmaxr,c} = D{c};
          endif
          if (Umaxr)
            Header{1 + Tmaxr + Nmaxr + Dmaxr,c} = U{c};
          endif
        else
          Header{1,c} = 'RowNames';
        endif
      endfor
      cmt = repmat ({''}, 1, Ccols);
      cmt{1} = sprintf (txt, Tmaxr, Nmaxr, Dmaxr, Umaxr);
      meta = [cmt; Header];
    endfunction

  endmethods

  ## Private methods for exporting (saving) Tables and Properties to files
  methods (Access = private)

    ## Export table to cell arrays
    function [V, N, T, D, U] = table2cellarrays (this, fmt = 'display')
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
            V = [V, cellstr(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'calendarDuration'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'categorical'))
          for col = 1:ncols
            V = [V, cellstr(var_V(:,col))];
            N = [N, this.VariableNames{ix}];
            T = [T, 'categorical'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'datetime'))
          for col = 1:ncols
            if (strcmp (fmt, 'iso'))
              V = [V, datetime2iso(var_V(:,col))];
            else
              V = [V, cellstr(var_V(:,col))];
            endif
            N = [N, this.VariableNames{ix}];
            T = [T, 'datetime'];
            D = [D, this.VariableDescriptions(ix)];
            U = [U, this.VariableUnits(ix)];
          endfor
        elseif (isa (var_V, 'duration'))
          for col = 1:ncols
            if (strcmp (fmt, 'iso'))
              V = [V, duration2iso(var_V(:,col))];
            else
              V = [V, cellstr(var_V(:,col))];
            endif
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
          [tmpV, tmpN, tmpT tmpD, tmpU] = table2cellarrays (var_V, fmt);
          V = [V, tmpV];
          nestedN = {};
          nestedT = {};
          nestedD = {};
          nestedU = {};
          for col = 1:size (tmpV, 2)
            nestedN = [nestedN, {{this.VariableNames{ix}; tmpN{col}}}];
            nestedT = [nestedT, {{'table'; tmpT{col}}}];
            nestedD = [nestedD, {{this.VariableDescriptions{ix}; tmpD{col}}}];
            nestedU = [nestedU, {{this.VariableUnits{ix}; tmpU{col}}}];
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
            nestedN = [nestedN, {{this.VariableNames{ix}; tmpN{col}}}];
            nestedT = [nestedT, {{'struct'; tmpT{col}}}];
            nestedD = [nestedD, {{this.VariableDescriptions{ix}; ''}}];
            nestedU = [nestedU, {{this.VariableUnits{ix}; ''}}];
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

## Convert a datetime array to datenum-valued doubles of the same size,
## mapping NaT to NaN.  Used by 'summary'.  Core 'datenum' cannot process the
## NaN date components of a NaT, so those rows are substituted with a valid
## placeholder before conversion and set back to NaN afterwards.
function dn = datetime_to_datenum (v)
  sz = size (v);
  DV = datevec (v);                     # (numel)-by-6 in column-major order
  nat = any (isnan (DV), 2);
  DV(nat,:) = 0;
  DV(nat,2:3) = 1;                      # valid month/day placeholder
  dn = datenum (DV);
  dn(nat) = NaN;
  dn = reshape (dn, sz);
endfunction

## Return a logical mask, the same size as a table variable V, that flags the
## missing entries.  Used by 'fillmissing'.  Char arrays have no standard
## missing value and nested tables are treated as non-missing.
function M = var_missing_mask (v)
  if (isa (v, 'table'))
    M = false (size (v));
  elseif (any (isa (v, {'calendarDuration', 'categorical', 'datetime', ...
                        'duration', 'string'})))
    M = ismissing (v);
  elseif (ischar (v))
    M = false (size (v));
  else  # numeric, logical, cellstr
    M = __ismissing__ (v);
  endif
endfunction

## Expand the 'constant' fill value into a 1-by-NVARS cell, one value per
## targeted variable (scalar broadcast, per-variable vector, or per-variable
## cell).  Used by 'fillmissing'.
function fvals = resolve_const_values (constVal, nvars)
  if (iscell (constVal))
    if (isscalar (constVal))
      fvals = repmat (constVal, 1, nvars);
    elseif (numel (constVal) == nvars)
      fvals = reshape (constVal, 1, nvars);
    else
      error (strcat ("table.fillmissing: a cell array of fill values must", ...
                     " have one element per targeted variable."));
    endif
  elseif (ischar (constVal) || isscalar (constVal))
    fvals = repmat ({constVal}, 1, nvars);
  elseif (isvector (constVal) && numel (constVal) == nvars)
    fvals = num2cell (reshape (constVal, 1, nvars));
  else
    error (strcat ("table.fillmissing: the fill value must be a scalar, a", ...
                   " vector with one element per targeted variable, or a", ...
                   " cell array of per-variable values."));
  endif
endfunction

## Fill every missing entry of variable V (mask M) with the constant FV.  Used
## by 'fillmissing'.  VARNAME names the variable for error reporting.
function [v, filled] = fill_constant (v, M, fv, varname)
  filled = M;
  try
    if (iscellstr (v))
      if (ischar (fv))
        fv = {fv};
      elseif (! (iscellstr (fv) && isscalar (fv)))
        error ("incompatible");
      endif
      v(M) = fv;
    else
      v(M) = fv;
    endif
  catch
    error (strcat ("table.fillmissing: the fill value is incompatible", ...
                   " with variable '%s'."), varname);
  end_try_catch
endfunction

## For a column with logical missing mask M, return the source row index SI for
## each row: SI(i) is the row whose value should fill row i (0 when none is
## reachable).  METHOD is 'previous', 'next', or 'nearest'.  Used by
## 'fillmissing'.
function si = fill_neighbor_idx (m, method)
  m = m(:);
  n = numel (m);
  idx = (1:n)';
  vp = idx;
  vp(m) = 0;
  sp = cummax (vp);                 # previous non-missing index (0 if none)
  vn = idx;
  vn(m) = n + 1;
  sn = flipud (cummin (flipud (vn)));
  sn(sn == n + 1) = 0;              # next non-missing index (0 if none)
  switch (method)
    case 'previous'
      si = sp;
    case 'next'
      si = sn;
    case 'nearest'
      si = idx;
      for i = find (m)'
        if (sp(i) == 0 && sn(i) == 0)
          si(i) = 0;
        elseif (sp(i) == 0)
          si(i) = sn(i);
        elseif (sn(i) == 0)
          si(i) = sp(i);
        elseif (sn(i) - i <= i - sp(i))
          si(i) = sn(i);            # tie favors the later (next) value
        else
          si(i) = sp(i);
        endif
      endfor
  endswitch
endfunction

## Linearly interpolate the missing entries of numeric column COL (mask M).
## ENDVALS controls leading/trailing gaps ('extrap', 'none', or a numeric
## scalar).  Returns the filled column and a logical mask of filled rows.  Used
## by 'fillmissing'.
function [col, filled] = fill_linear (col, m, endVals)
  m = m(:);
  n = numel (m);
  filled = false (n, 1);
  known = ! m;
  if (sum (known) < 2)
    return;                         # need at least two anchors to interpolate
  endif
  x = (1:n)';
  xk = x(known);
  yk = double (col(known));
  lo = xk(1);
  hi = xk(end);
  ## Interior gaps via linear interpolation
  interior = m & x > lo & x < hi;
  if (any (interior))
    col(interior) = interp1 (xk, yk, x(interior), 'linear');
    filled(interior) = true;
  endif
  ## Leading and trailing gaps via the 'EndValues' option
  ends = m & (x < lo | x > hi);
  if (any (ends))
    if (ischar (endVals) || (isa (endVals, 'string') && isscalar (endVals)))
      switch (lower (char (endVals)))
        case 'extrap'
          col(ends) = interp1 (xk, yk, x(ends), 'linear', 'extrap');
          filled(ends) = true;
        case 'none'
          ## leave the end gaps missing
        otherwise
          error (strcat ("table.fillmissing: unsupported 'EndValues'", ...
                         " option '%s'."), lower (char (endVals)));
      endswitch
    elseif (isnumeric (endVals) && isscalar (endVals))
      col(ends) = endVals;
      filled(ends) = true;
    else
      error (strcat ("table.fillmissing: 'EndValues' must be 'extrap',", ...
                     " 'none', or a numeric scalar."));
    endif
  endif
endfunction

## Split a 'standardizeMissing' indicator into a numeric row vector NUMIND and a
## cellstr row TXTIND of text indicators.  Used by 'standardizeMissing'.
function [numInd, txtInd] = std_normalize_indicator (indicator)
  numInd = [];
  txtInd = {};
  if (iscell (indicator) && ! iscellstr (indicator))
    for i = 1:numel (indicator)
      e = indicator{i};
      if (ischar (e))
        txtInd{end+1} = e;
      elseif (isa (e, 'string'))
        tmp = cellstr (e);
        txtInd = [txtInd, tmp(:)'];
      elseif (iscellstr (e))
        txtInd = [txtInd, e(:)'];
      elseif (isnumeric (e) || islogical (e))
        numInd = [numInd, double(e(:)')];
      else
        error (strcat ("table.standardizeMissing: unsupported indicator", ...
                       " element of class '%s'."), class (e));
      endif
    endfor
  elseif (iscellstr (indicator))
    txtInd = indicator(:)';
  elseif (ischar (indicator))
    txtInd = {indicator};
  elseif (isa (indicator, 'string'))
    tmp = cellstr (indicator);
    txtInd = tmp(:)';
  elseif (isnumeric (indicator) || islogical (indicator))
    numInd = double (indicator(:)');
  else
    error (strcat ("table.standardizeMissing: invalid INDICATOR of class", ...
                   " '%s'."), class (indicator));
  endif
endfunction

## Replace entries of variable V that match a (type-compatible) indicator with
## the standard missing value of V's class.  Used by 'standardizeMissing'.
function v = std_apply_indicator (v, numInd, txtInd)
  if (isfloat (v))
    if (! isempty (numInd))
      v(ismember (v, numInd)) = NaN;
    endif
  elseif (iscellstr (v))
    if (! isempty (txtInd))
      v(ismember (v, txtInd)) = {''};
    endif
  elseif (isa (v, 'string'))
    if (! isempty (txtInd))
      v(ismember (cellstr (v), txtInd)) = string (missing);
    endif
  elseif (isa (v, 'categorical'))
    if (! isempty (txtInd))
      v(ismember (cellstr (v), txtInd)) = categorical (missing);
    endif
  endif
  ## logical, integer, duration, datetime, calendarDuration, and nested table
  ## variables have no compatible standard missing value here; pass through.
endfunction

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
  se = cell2mat (cellfun (@(x) numel (x), data, 'UniformOutput', false)) == 1;
  ve = cell2mat (cellfun (@(x) size (x,1), data, 'UniformOutput', false)) == 1;

  ## Catch 'cell' scalars
  tmp = cell2mat (cellfun (@iscell, data(se), 'UniformOutput', false)) == 1;
  idx_cell(se) = tmp;
  sf = @(x) sprintf ("1x1 cell");
  out_str(idx_cell) = (cellfun (sf, data(idx_cell), ...
                       'UniformOutput', false));
  ## Catch 'char' scalars or row vectors
  tmp = cell2mat (cellfun (@ischar, data(ve), 'UniformOutput', false));
  idx_charvec(ve) = tmp;
  sf = @(x) sprintf ("'%s'", x);
  out_str(idx_charvec) = (cellfun (sf, data(idx_charvec), ...
                          'UniformOutput', false));
  ## Catch 'logical' scalars or row vectors
  tmp = cell2mat (cellfun (@islogical, data(ve), 'UniformOutput', false)) == 1;
  idx_logical(ve) = tmp;
  sf = @(x) sprintf ("[%s]", strtrim (sprintf ("%d ", x)));
  out_str(idx_logical) = (cellfun (sf, data(idx_logical), ...
                          'UniformOutput', false));
  ## Catch 'numeric' scalars or row vectors
  tmp = cell2mat (cellfun (@isnumeric, data(ve), 'UniformOutput', false)) == 1;
  idx_numeric(ve) = tmp;
  sf = @(x) sprintf ("[%s]", strtrim (sprintf ("%g ", x)));
  out_str(idx_numeric) = (cellfun (sf, data(idx_numeric), ...
                          'UniformOutput', false));
  ## Catch 'object' scalars
  tmp = cell2mat (cellfun (@isobject, data(se), 'UniformOutput', false)) == 1;
  idx_struct(se) = tmp;
  sf = @(x) sprintf ("1x1 %s", class (x));
  out_str(idx_struct) = (cellfun (sf, data(idx_struct), ...
                         'UniformOutput', false));
  ## Catch 'string' scalars or row vectors
  tmp = cell2mat (cellfun (@isstring, data(ve), 'UniformOutput', false)) == 1;
  idx_string(ve) = tmp;
  sf = @(x) sprintf ("[%s]", strtrim (sprintf ("%s    ", dispstrings (x){:})));
  out_str(idx_string) = (cellfun (sf, data(idx_string), ...
                         'UniformOutput', false));
  ## Catch scalar elements of struct type
  tmp = cell2mat (cellfun (@isstruct, data(se), 'UniformOutput', false)) == 1;
  idx_struct(se) = tmp;
  sf = @(x) sprintf ("1x1 struct");
  out_str(idx_struct) = (cellfun (sf, data(idx_struct), ...
                         'UniformOutput', false));

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
    tmp = cell2mat (cellfun (@iscell, data(me), 'UniformOutput', false)) == 1;
    idx_cell(me) = tmp;
    sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' cell']), size (x));
    out_str(idx_cell) = (cellfun (sf, data(idx_cell), ...
                         'UniformOutput', false));
    ## 'char' arrays
    tmp = cell2mat (cellfun (@ischar, data(me), 'UniformOutput', false));
    idx_charvec(me) = tmp;
    sf = @(x) sprintf (strcat (strjoin (repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                               ' char'), size (x));
    out_str(idx_charvec) = (cellfun (sf, data(idx_charvec), ...
                            'UniformOutput', false));
    ## 'logical' arrays
    tmp = cell2mat (cellfun (@islogical, data(me), ...
                             'UniformOutput', false)) == 1;
    idx_logical(me) = tmp;
    sf = @(x) sprintf (strcat (strjoin (repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                               ' logical'), size (x));
    out_str(idx_logical) = (cellfun (sf, data(idx_logical), ...
                            'UniformOutput', false));
    ## 'numeric' arrays
    tmp = cell2mat (cellfun (@isnumeric, data(me), ...
                             'UniformOutput', false)) == 1;
    idx_numeric(me) = tmp;
    sf = @(x) sprintf (strcat (strjoin (repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                               ' array'), size (x));
    out_str(idx_numeric) = (cellfun (sf, data(idx_numeric), ...
                            'UniformOutput', false));
    ## 'object' arrays
    tmp = cell2mat (cellfun (@isstring, data(me), 'UniformOutput', false)) == 1;
    idx_string(me) = tmp;
    sf = @(x) sprintf (strcat (strjoin (repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                               ' %s'), size (x), class (x));
    out_str(idx_string) = (cellfun (sf, data(idx_string), ...
                           'UniformOutput', false));
    ## 'string' arrays
    tmp = cell2mat (cellfun (@isstring, data(me), 'UniformOutput', false)) == 1;
    idx_string(me) = tmp;
    sf = @(x) sprintf (strcat (strjoin (repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                               ' string'), size (x));
    out_str(idx_string) = (cellfun (sf, data(idx_string), ...
                           'UniformOutput', false));
    ## 'struct' arrays
    tmp = cell2mat (cellfun (@isstruct, data(me), 'UniformOutput', false)) == 1;
    idx_struct(me) = tmp;
    sf = @(x) sprintf (strcat (strjoin (repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                               ' struct'), size (x));
    out_str(idx_struct) = (cellfun (sf, data(idx_struct), ...
                           'UniformOutput', false));
  endif

  ## Get optimal length
  strLen = max (cellfun (@length, out_str)) + 2;
  optLen = max ([varLen, strLen]);

  ## Pad data according to optimal length
  ## numeric and logical is right aligned, everything else is left aligned
  Ra = sprintf ("{%%+%ds}", optLen - 2);
  La = sprintf ("{%%-%ds}", optLen - 2);
  fcn = @(x) sprintf (Ra, x);
  outData(pad_B) = cellfun (fcn, out_str(pad_B), 'UniformOutput', false);
  fcn = @(x) sprintf (La, x);
  outData(pad_A) = cellfun (fcn, out_str(pad_A), 'UniformOutput', false);
  outData = outData(:);
endfunction

## Helper function for unstack method to get default aggregation function
## and missing values according to the data type of the stacked variable
function [mcvec, aggrFcn] = get_default_aggrFcn (vvals, nrows, aggrFcn)
  ## Get columns of stacked variable
  vcols = size (vvals, 2);
  ## Handle each specific data type
  if (any (isa (vvals, {'single', 'double'})))
    mcvec =  NaN (nrows, vcols, 'like', vvals);
    if (isempty (aggrFcn))  # add default aggrevation function
      aggrFcn = @sum;
    else  # check that it produces correct output
      tmpval = 1:5;
      try
        tmpval = aggrFcn (tmpval);
      catch
        aggrFcn = strcat ("table.unstack: invalid 'AggregationFunction'", ...
                          " for numeric data.");
      end_try_catch
      if (! isscalar (tmpval))
        aggrFcn = strcat ("table.unstack: 'AggregationFunction'", ...
                          " must return a scalar value.");
      endif
    endif
  elseif (isnumeric (vvals))  # integer types have no missing value, use 0
    mcvec =  zeros (nrows, vcols, 'like', vvals);
    if (isempty (aggrFcn))  # add default aggrevation function
      aggrFcn = @sum;
    else  # check that it produces correct output
      tmpval = 1:5;
      try
        tmpval = aggrFcn (tmpval);
      catch
        aggrFcn = strcat ("table.unstack: invalid 'AggregationFunction'", ...
                          " for numeric data.");
      end_try_catch
      if (! isscalar (tmpval))
        aggrFcn = strcat ("table.unstack: 'AggregationFunction'", ...
                          " must return a scalar value.");
      endif
    endif
  elseif (isa (vvals, 'calendarDuration'))
    mcvec =  repmat (calendarDuration ([0, 0, 0]), nrows, vcols);
    if (isempty (aggrFcn))  # add default aggrevation function
      aggrFcn = @unique;
    else  # check that it produces correct output
      tmpval = calendarDuration (1:5, 0, 0);
      try
        tmpval = aggrFcn (tmpval);
      catch
        aggrFcn = strcat ("table.unstack: invalid 'AggregationFunction'", ...
                          " for calendarDuration data.");
      end_try_catch
      if (! isscalar (tmpval))
        aggrFcn = strcat ("table.unstack: 'AggregationFunction'", ...
                          " must return a scalar value.");
      endif
    endif
  elseif (isa (vvals, 'duration'))
    mcvec =  repmat (duration ([0, 0, 0]), nrows, vcols);
    if (isempty (aggrFcn))  # add default aggrevation function
      aggrFcn = @unique;
    else  # check that it produces correct output
      tmpval = duration (1:5, 0, 0);
      try
        tmpval = aggrFcn (tmpval);
      catch
        aggrFcn = strcat ("table.unstack: invalid 'AggregationFunction'", ...
                          " for duration data.");
      end_try_catch
      if (! isscalar (tmpval))
        aggrFcn = strcat ("table.unstack: 'AggregationFunction'", ...
                          " must return a scalar value.");
      endif
    endif
  elseif (islogical (vvals))
    mcvec =  false (nrows, vcols);
    if (isempty (aggrFcn))  # add default aggrevation function
      aggrFcn = @unique;
    else  # check that it produces correct output
      tmpval = [false, false, true, true, false];
      try
        tmpval = aggrFcn (tmpval);
      catch
        aggrFcn = strcat ("table.unstack: invalid 'AggregationFunction'", ...
                          " for logical data.");
      end_try_catch
      if (! isscalar (tmpval))
        aggrFcn = strcat ("table.unstack: 'AggregationFunction'", ...
                          " must return a scalar value.");
      endif
    endif
  elseif (isa (vvals, 'categorical'))
    mcvec =  repmat (categorical (NaN), nrows, vcols);
    if (isempty (aggrFcn))  # add default aggrevation function
      aggrFcn = @unique;
    else  # check that it produces correct output
      tmpval = categorical (1:5);
      try
        tmpval = aggrFcn (tmpval);
      catch
        aggrFcn = strcat ("table.unstack: invalid 'AggregationFunction'", ...
                          " for categorical data.");
      end_try_catch
      if (! isscalar (tmpval))
        aggrFcn = strcat ("table.unstack: 'AggregationFunction'", ...
                          " must return a scalar value.");
      endif
    endif
  else  # all other data types (string, cellstr, datetime, ...)
    if (iscellstr (vvals))
      vt = 'cellstr';
    else
      vt = class (vvals);
    endif
    tmpl = table ('Size', [nrows, 1], 'VariableTypes', {vt});
    mcvec = repmat (tmpl.Var1, 1, vcols);
    if (isempty (aggrFcn))  # add default aggrevation function
      aggrFcn = @unique;
    endif
  endif

  ## Enforce a scalar aggregation result, erroring on e.g. conflicting
  ## non-numeric values under the default @unique, matching MATLAB.
  if (! ischar (aggrFcn))
    baseFcn = aggrFcn;
    aggrFcn = @(x) enforce_scalar_aggr (baseFcn (x));
  endif
endfunction

## Error out when an unstack aggregation function returns a non-scalar value.
function val = enforce_scalar_aggr (val)
  if (size (val, 1) > 1)
    error (strcat ("table.unstack: 'AggregationFunction' must return", ...
                   " a scalar value."));
  endif
endfunction

## Map a key variable kind to a comparison category.  Returns an empty
## character vector for types that cannot be used as keys.
function k = key_kind (col)
  if (isa (col, 'categorical') || isa (col, 'string') || iscellstr (col)
      || ischar (col))
    k = 'text';
  elseif (isa (col, 'datetime'))
    k = 'datetime';
  elseif (isa (col, 'duration'))
    k = 'duration';
  elseif (isa (col, 'calendarDuration'))
    k = 'calendarDuration';
  elseif (isnumeric (col) || islogical (col))
    k = 'numeric';
  else
    k = '';
  endif
endfunction

## Validate the optional SETORDER argument shared by the set operations.
## Returns the lower-cased order ('sorted' default) and an errmsg body (empty on
## success) emitted by the caller under its own name.
function [order, errmsg] = parse_set_order (args)
  order = 'sorted';
  errmsg = '';
  if (! isempty (args))
    if (numel (args) > 1)
      errmsg = "too many input arguments.";
    elseif (! (ischar (args{1}) && isrow (args{1})
               && any (strcmpi (args{1}, {'sorted', 'stable'}))))
      errmsg = "SETORDER must be either 'sorted' or 'stable'.";
    else
      order = lower (args{1});
    endif
  endif
endfunction

## Build the disambiguation suffixes used by the join methods when a variable
## name is shared by both tables.  MATLAB derives them from the input argument
## names (e.g. inputs L and R give '_L'/'_R'); fall back to '_left'/'_right'
## when an input has no workspace name.
function [lsuf, rsuf] = join_suffixes (leftName, rightName)
  if (isempty (leftName))
    leftName = 'left';
  endif
  if (isempty (rightName))
    rightName = 'right';
  endif
  lsuf = ['_', leftName];
  rsuf = ['_', rightName];
endfunction

## Encode two cellstr key columns into consistent integer codes so that equal
## strings (across both columns) map to the same code.
function [lp, rp] = text_codes (lc, rc)
  nl = numel (lc);
  [~, ~, ic] = unique ([lc(:); rc(:)]);
  lp = ic(1:nl);
  rp = ic(nl+1:end);
endfunction

## Build consistent numeric key proxies for the same key variable taken from
## two tables, so that equal key values map to equal proxy rows.  Returns an
## errmsg body (empty on success) emitted by the caller under its own name.
function [lp, rp, errmsg] = key_col_proxy (lcol, rcol)
  lp = [];
  rp = [];
  errmsg = '';
  kl = key_kind (lcol);
  kr = key_kind (rcol);
  if (isempty (kl))
    errmsg = sprintf ("unsupported key variable type '%s'.", class (lcol));
    return;
  elseif (isempty (kr))
    errmsg = sprintf ("unsupported key variable type '%s'.", class (rcol));
    return;
  elseif (! strcmp (kl, kr))
    errmsg = "key variables have incompatible types.";
    return;
  endif
  switch (kl)
    case 'text'
      [lp, rp] = text_codes (cellstr (lcol), cellstr (rcol));
    case 'datetime'
      lp = datetime_to_datenum (lcol);
      rp = datetime_to_datenum (rcol);
    case 'duration'
      lp = days (lcol);
      rp = days (rcol);
    case 'calendarDuration'
      lp = lcol.proxyArray;
      rp = rcol.proxyArray;
    case 'numeric'
      lp = double (lcol);
      rp = double (rcol);
  endswitch
  if (size (lp, 2) != size (rp, 2))
    lp = [];
    rp = [];
    errmsg = "key variables have incompatible sizes.";
  endif
endfunction

## Recognised 'groupbins' time-unit keyword names (plus 'none').  Only 'none' is
## currently honoured; the rest are detected so a time-unit binning argument is
## not mistaken for a method, then reported as not yet supported.
function kw = gb_keywords ()
  kw = {'none', 'second', 'minute', 'hour', 'day', 'week', 'month', ...
        'quarter', 'year', 'decade', 'century', 'dayname', 'monthname', ...
        'dayofweek', 'dayofmonth', 'dayofyear', 'hourofday', 'weekofmonth', ...
        'weekofyear', 'monthofyear', 'quarterofyear', 'secondofminute', ...
        'minuteofhour'};
endfunction

## True if X is a single 'groupbins' binning-scheme element: a non-empty numeric,
## datetime, duration, or calendarDuration array (bin edges or a bin count/width)
## or a binning keyword char vector/string (see 'gb_keywords').
function tf = gb_is_scheme_elem (x)
  tf = false;
  if ((isnumeric (x) || islogical (x)) && ! isempty (x))
    tf = true;
  elseif (isa (x, 'datetime') || isa (x, 'duration') ...
          || isa (x, 'calendarDuration'))
    tf = true;
  elseif ((ischar (x) && isrow (x)) || (isa (x, 'string') && isscalar (x)))
    tf = any (strcmpi (char (x), gb_keywords ()));
  endif
endfunction

## True if X is a 'groupbins' binning specification: a single scheme element (see
## 'gb_is_scheme_elem') or a cell array whose every element is one.  Used to tell
## a 'groupbins' positional argument apart from a METHOD argument, since method
## names and function handles are never binning specs.
function tf = is_groupbins_spec (x)
  tf = gb_is_scheme_elem (x);
  if (! tf && iscell (x) && ! isempty (x))
    tf = all (cellfun (@gb_is_scheme_elem, x(:)'));
  endif
endfunction

## Normalise a 'groupbins' SCHEME into a 1-by-K cell of per-variable schemes for
## the K grouping variables: a cell scheme must already hold one entry per
## variable, any other scheme is applied to every grouping variable.  CALLER
## names the method for the error message.  Returns an errmsg body emitted by the
## caller (empty on success).
function [schemes, errmsg] = gb_normalise_schemes (scheme, K, caller)
  errmsg = '';
  schemes = {};
  if (iscell (scheme))
    if (numel (scheme) != K)
      errmsg = sprintf (strcat ("GROUPBINS as a cell array must hold one", ...
                                " binning scheme per grouping variable (%d)."), ...
                        K);
      return;
    endif
    schemes = scheme(:)';
  else
    schemes = repmat ({scheme}, 1, K);
  endif
endfunction

## Bin the grouping-variable columns COLS (a cell array of values) using the
## 'groupbins' SCHEME and the IncludedEdge INCEDGE ('left'/'right').  NAMES holds
## the variable names for error messages.  Each binnable column is replaced by a
## categorical whose categories are the bin interval labels; a per-variable
## scheme of 'none' leaves its column unchanged.  Returns the updated COLS and an
## errmsg body emitted by the caller (empty on success).
function [cols, errmsg] = bin_groupvars (cols, names, scheme, incEdge, caller)
  errmsg = '';
  K = numel (cols);
  [schemes, errmsg] = gb_normalise_schemes (scheme, K, caller);
  if (! isempty (errmsg))
    return;
  endif
  for j = 1:K
    sj = schemes{j};
    if (((ischar (sj) && isrow (sj)) || (isa (sj, 'string') && isscalar (sj))) ...
        && strcmpi (char (sj), 'none'))
      continue;                     # no binning for this variable
    endif
    [b, errmsg] = bin_group_col (cols{j}, sj, incEdge, names{j});
    if (! isempty (errmsg))
      return;
    endif
    cols{j} = b;
  endfor
endfunction

## Bin one grouping-variable column COL into a categorical whose categories are
## the bin interval labels, using the binning SCHEME and the IncludedEdge INCEDGE.
## SCHEME is a numeric scalar (number of equal-width bins) or a vector of bin
## edges of the same type as COL (numeric, datetime, or duration); a value
## outside the outer edges becomes <undefined>.  With INCEDGE 'left' (default)
## bins are [e1,e2),...,[en-1,en]; with 'right' they are [e1,e2],(e2,e3],...,
## (en-1,en].  VARNAME names the variable for error messages.  Time-unit and
## bin-width schemes are not yet supported.  Returns an errmsg body emitted by
## the caller (empty on success).
function [binned, errmsg] = bin_group_col (col, scheme, incEdge, varname)
  binned = [];
  errmsg = '';

  ## Reject the not-yet-supported time-unit and bin-width schemes up front.
  if (((ischar (scheme) && isrow (scheme)) ...
       || (isa (scheme, 'string') && isscalar (scheme))) ...
      && ! strcmpi (char (scheme), 'none'))
    errmsg = sprintf (strcat ("binning grouping variable '%s' by time unit", ...
                              " '%s' is not yet supported; use bin edges or a", ...
                              " number of bins."), varname, char (scheme));
    return;
  endif
  if (isa (scheme, 'duration') || isa (scheme, 'calendarDuration'))
    errmsg = sprintf (strcat ("binning grouping variable '%s' by a bin width", ...
                              " is not yet supported; use bin edges or a", ...
                              " number of bins."), varname);
    return;
  endif

  ## Map COL to a numeric proxy and record its type for typed edge labels.
  if (isa (col, 'datetime'))
    proxy = datetime_to_datenum (col)(:);
    ctype = 'datetime';
  elseif (isa (col, 'duration'))
    proxy = days (col)(:);
    ctype = 'duration';
  elseif (isnumeric (col) || islogical (col))
    proxy = double (col)(:);
    ctype = 'numeric';
  else
    errmsg = sprintf (strcat ("binning is not supported for grouping variable", ...
                              " '%s' of type '%s'."), varname, class (col));
    return;
  endif

  ## Resolve the bin edges in proxy units.
  if (isscalar (scheme) && (isnumeric (scheme) || islogical (scheme)))
    nb = double (scheme);
    if (! (nb >= 1 && nb == fix (nb)))
      errmsg = sprintf (strcat ("the number of bins for grouping variable", ...
                                " '%s' must be a positive integer."), varname);
      return;
    endif
    good = proxy(! isnan (proxy));
    if (isempty (good))
      edgesP = [0, 1];
    elseif (min (good) == max (good))
      edgesP = [min(good), min(good) + 1];
    else
      edgesP = min (good) + (0:nb) * (max (good) - min (good)) / nb;
    endif
  elseif (isnumeric (scheme))
    if (! strcmp (ctype, 'numeric'))
      errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be", ...
                                " of type '%s'."), varname, ctype);
      return;
    endif
    edgesP = sort (double (scheme(:)'));
  elseif (isa (scheme, 'datetime'))
    if (! strcmp (ctype, 'datetime'))
      errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be", ...
                                " of type '%s'."), varname, ctype);
      return;
    endif
    edgesP = sort (datetime_to_datenum (scheme)(:)');
  else
    errmsg = sprintf ("invalid binning scheme for grouping variable '%s'.", ...
                      varname);
    return;
  endif
  if (numel (edgesP) < 2 || any (isnan (edgesP)) || any (diff (edgesP) <= 0))
    errmsg = sprintf (strcat ("bin edges for grouping variable '%s' must be at", ...
                              " least two finite, strictly increasing values."), ...
                      varname);
    return;
  endif
  nb = numel (edgesP) - 1;

  ## Assign each row to a bin (out-of-range -> <undefined>).
  left = ! strcmpi (incEdge, 'right');
  idx = NaN (numel (proxy), 1);
  for k = 1:nb
    if (left)
      if (k < nb)
        in = proxy >= edgesP(k) & proxy < edgesP(k+1);
      else
        in = proxy >= edgesP(k) & proxy <= edgesP(k+1);
      endif
    else
      if (k == 1)
        in = proxy >= edgesP(k) & proxy <= edgesP(k+1);
      else
        in = proxy > edgesP(k) & proxy <= edgesP(k+1);
      endif
    endif
    idx(in) = k;
  endfor

  ## Build the interval labels and the categorical.
  estr = bin_edge_labels (edgesP, ctype);
  labs = cell (1, nb);
  for k = 1:nb
    if (left)
      br = '[';  bl = ')';
      if (k == nb)
        bl = ']';
      endif
    else
      br = '(';  bl = ']';
      if (k == 1)
        br = '[';
      endif
    endif
    labs{k} = sprintf ("%s%s, %s%s", br, estr{k}, estr{k+1}, bl);
  endfor
  rowLab = repmat ({''}, numel (idx), 1);
  ok = ! isnan (idx);
  rowLab(ok) = labs(idx(ok));
  binned = categorical (rowLab, labs);
endfunction

## Format the bin edges EDGESP (numeric proxy values) as a cell array of label
## strings of the type CTYPE: numeric edges through 'bin_num_str', datetime and
## duration edges through their displayed text.
function s = bin_edge_labels (edgesP, ctype)
  n = numel (edgesP);
  s = cell (1, n);
  switch (ctype)
    case 'datetime'
      dt = datetime (edgesP(:), 'ConvertFrom', 'datenum');
      for i = 1:n
        s{i} = char (dt(i));
      endfor
    case 'duration'
      for i = 1:n
        s{i} = char (days (edgesP(i)));
      endfor
    otherwise
      for i = 1:n
        s{i} = bin_num_str (edgesP(i));
      endfor
  endswitch
endfunction

## Format a numeric bin-edge value V for an interval label: an integer prints
## without a decimal point, anything else through 'num2str'.
function s = bin_num_str (v)
  if (isfinite (v) && v == fix (v) && abs (v) < 1e15)
    s = sprintf ("%d", v);
  else
    s = num2str (v);
  endif
endfunction

## Build a single-column grouping proxy for one grouping variable COL: a numeric
## matrix P (one row per element) whose sort order matches COL's value order, so
## that 'unique (P, "rows")' recovers the sorted unique groups, together with a
## logical MISS mask flagging the elements that findgroups treats as missing
## (NaN/NaT/<missing>/''/<undefined>).  Returns an errmsg body (empty on success)
## emitted by the caller under its own name.
function [p, miss, errmsg] = group_col_proxy (col)
  p = [];
  miss = [];
  errmsg = '';
  if (isa (col, 'categorical'))
    ## Categorical groups follow category order (ordinal or reordered), which the
    ## underlying category codes encode; <undefined> maps to NaN.
    p = double (col)(:);
    miss = isnan (p);
    return;
  endif
  k = key_kind (col);
  if (isempty (k))
    errmsg = sprintf ("unsupported grouping variable type '%s'.", class (col));
    return;
  endif
  switch (k)
    case 'text'
      c = cellstr (col);
      c = c(:);
      miss = cellfun (@isempty, c);
      [~, ~, ic] = unique (c);
      p = ic(:);
    case 'datetime'
      p = datetime_to_datenum (col)(:);
      miss = isnan (p);
    case 'duration'
      p = days (col)(:);
      miss = isnan (p);
    case 'calendarDuration'
      p = col.proxyArray;
      miss = any (isnan (p), 2);
    case 'numeric'
      p = double (col)(:);
      miss = isnan (p);
  endswitch
endfunction

## Group table rows by the grouping-variable columns GRPCOLS (a cell array of
## variable values, one per grouping variable), using 'group_col_proxy' on each.
## Returns G, an n-by-1 vector of group numbers (NaN for rows holding a missing
## value in any grouping variable), NGROUPS, the number of groups, REPROWS, a
## representative row index per group in sorted group order, and an errmsg body
## (empty on success) emitted by the caller.
function [G, ngroups, repRows, errmsg] = group_table_rows (grpCols)
  errmsg = '';
  ngroups = 0;
  repRows = [];
  n = size (grpCols{1}, 1);
  P = [];
  miss = false (n, 1);
  for j = 1:numel (grpCols)
    [p, m, e] = group_col_proxy (grpCols{j});
    if (! isempty (e))
      G = [];
      errmsg = e;
      return;
    endif
    P = [P, p];
    miss = miss | m;
  endfor
  G = NaN (n, 1);
  keep = find (! miss);
  if (! isempty (keep))
    [~, ia, ic] = unique (P(keep,:), "rows");
    G(keep) = ic;
    repRows = keep(ia);
    ngroups = numel (ia);
  endif
endfunction

## Build the grouping-variable columns of a grouped apply output from the
## grouping-variable values GRPCOLS: GCOLS holds the value of each grouping
## variable at the representative rows REPROWS, and GCOUNT the number of rows in
## each group, derived from the group-number vector G.
function [gcols, gcount] = group_output_cols (grpCols, G, repRows)
  ngroups = numel (repRows);
  gcols = cell (1, numel (grpCols));
  for p = 1:numel (grpCols)
    gcols{p} = grpCols{p}(repRows,:);
  endfor
  gcount = accumarray (G(! isnan (G)), 1, [ngroups, 1]);
endfunction

## Build the row keep-mask for 'groupfilter' by applying the filter function
## METHOD to each data variable's per-group slice.  DATACOLS is a cell array of
## data-variable values; G the n-by-1 group numbers (1..NG), every row assigned
## to a group.  For each group METHOD receives the variable's slice and must
## return a logical scalar (keep/drop the whole group) or a logical vector with
## one element per group row.  The per-variable masks are combined with logical
## AND, so a row is kept only when the condition holds across all data variables.
## Returns KEEP (n-by-1 logical) and an errmsg body emitted by the caller.
function [keep, errmsg] = gf_keep_mask (method, dataCols, G, ng)
  errmsg = '';
  n = numel (G);
  keep = true (n, 1);
  for d = 1:numel (dataCols)
    col = dataCols{d};
    for g = 1:ng
      rows = find (G == g);
      if (isempty (rows))
        continue;
      endif
      r = method (col(rows,:));
      if (! (islogical (r) || isnumeric (r)))
        errmsg = "the filter function must return a logical result.";
        return;
      endif
      r = logical (r(:));
      if (isscalar (r))
        m = repmat (r, numel (rows), 1);
      elseif (numel (r) == numel (rows))
        m = r;
      else
        errmsg = strcat ("the filter function must return a logical scalar", ...
                         " or a logical vector with one element per group", ...
                         " row.");
        return;
      endif
      keep(rows) = keep(rows) & m;
    endfor
  endfor
endfunction

## Transform one data variable COL group by group for 'grouptransform', applying
## METHOD (a transform-name char vector or a function handle) to each group's
## slice and returning OUT, the transformed values the same size as COL.  G is
## the n-by-1 group-number vector (1..NG), every row assigned to a group.  A
## function handle must return a single row (broadcast) or one row per group row.
## Returns an errmsg body (empty on success) emitted by the caller.
function [out, errmsg] = gt_transform_col (method, col, G, ng)
  out = [];
  errmsg = '';
  if (! (isnumeric (col) || islogical (col)))
    errmsg = sprintf (strcat ("grouptransform requires numeric or logical", ...
                              " data; got '%s'"), class (col));
    return;
  endif
  x = double (col);
  out = x;
  for g = 1:ng
    rows = find (G == g);
    if (isempty (rows))
      continue;
    endif
    slice = x(rows,:);
    if (is_function_handle (method))
      r = method (slice);
      if (! (isnumeric (r) || islogical (r)))
        errmsg = "the transform function must return a numeric result.";
        out = [];
        return;
      endif
      if (size (r, 1) == 1)
        r = repmat (r, numel (rows), 1);
      endif
      if (! isequal (size (r), size (slice)))
        errmsg = strcat ("the transform function must return a result the", ...
                         " same size as the group, or a single row.");
        out = [];
        return;
      endif
      out(rows,:) = r;
    else
      for c = 1:columns (slice)
        out(rows,c) = gt_apply_named (method, slice(:,c));
      endfor
    endif
  endfor
endfunction

## Apply a single named transform METHOD to the column vector X (a group's slice
## of one data variable), returning the transformed values V the same size as X.
## NaN values are omitted when computing the group statistics; the centring and
## scaling methods leave NaN in place, while 'meanfill'/'linearfill' fill them.
function v = gt_apply_named (method, x)
  nan = isnan (x);
  xo = x(! nan);
  switch (method)
    case 'meancenter'
      v = x - mean (xo);
    case 'zscore'
      v = (x - mean (xo)) / std (xo);
    case 'norm'
      v = x / norm (xo);
    case 'rescale'
      mn = min (xo);
      mx = max (xo);
      v = (x - mn) / (mx - mn);
    case 'meanfill'
      v = x;
      v(nan) = mean (xo);
    case 'linearfill'
      v = gt_linearfill (x);
  endswitch
endfunction

## Fill the missing values of the column vector X by linear interpolation over
## the non-missing positions, leaving leading and trailing missing values (and
## any group with fewer than two non-missing values) unchanged.
function v = gt_linearfill (x)
  v = x;
  idx = find (! isnan (x));
  if (numel (idx) >= 2)
    pos = (1:numel (x))';
    vi = interp1 (idx, x(idx), pos, "linear");
    fill = isnan (x) & pos > idx(1) & pos < idx(end);
    v(fill) = vi(fill);
  endif
endfunction

## Normalise the 'groupsummary' METHOD argument into a cell array of method specs
## METHODS (each a method-name char vector or a function handle) and a parallel
## cell array of display names METHNAMES used to build output variable names
## (the method name, or 'fun<n>' for the n-th function handle).  Returns an
## errmsg body (empty on success) emitted by the caller.
function [methods, methNames, errmsg] = gs_normalise_methods (method)
  methods = {};
  methNames = {};
  errmsg = '';
  if (isempty (method) && ! iscell (method) && ! ischar (method)
      && ! is_function_handle (method))
    return;   # no method requested: counts only
  endif
  if (is_function_handle (method) || (ischar (method) && isrow (method))
      || isa (method, 'string'))
    items = {method};
  elseif (iscell (method))
    items = method(:)';
  else
    errmsg = strcat ("METHOD must be a method name, a function handle, or a", ...
                     " cell array of method names and function handles.");
    return;
  endif
  known = {'sum', 'mean', 'median', 'mode', 'var', 'std', 'min', 'max', ...
           'range', 'nnz', 'nummissing', 'numunique'};
  nfun = 0;
  for k = 1:numel (items)
    it = items{k};
    if (is_function_handle (it))
      nfun++;
      methods{end+1} = it;
      methNames{end+1} = sprintf ("fun%d", nfun);
    elseif ((ischar (it) && isrow (it)) || (isa (it, 'string') && isscalar (it)))
      nm = lower (char (it));
      if (! any (strcmp (nm, known)))
        errmsg = sprintf ("'%s' is not a supported method name.", char (it));
        return;
      endif
      methods{end+1} = nm;
      methNames{end+1} = nm;
    else
      errmsg = strcat ("each method must be a method name or a function", ...
                       " handle.");
      return;
    endif
  endfor
endfunction

## Group table rows for 'groupsummary' by the grouping-variable values GRPCOLS,
## treating each grouping variable's missing values as a single group value.
## Returns G, an n-by-1 vector of group numbers (1..NGROUPS); NGROUPS; REPROWS,
## a representative row index per group; and an errmsg body emitted by the
## caller.  Groups are sorted by grouping value with missing groups last.  When
## INCMISS is false, rows holding a missing grouping value are dropped (labelled
## NaN in G and excluded from NGROUPS/REPROWS).
function [G, ngroups, repRows, errmsg] = gs_group_rows (grpCols, incMiss)
  errmsg = '';
  G = [];
  ngroups = 0;
  repRows = [];
  n = size (grpCols{1}, 1);
  KEY = [];
  SORT = [];
  anyMiss = false (n, 1);
  for j = 1:numel (grpCols)
    [p, m, e] = group_col_proxy (grpCols{j});
    if (! isempty (e))
      errmsg = e;
      return;
    endif
    pc = p;
    pc(m,:) = 0;                  # collapse all missing values of this variable
    KEY = [KEY, pc, double(m)];
    sp = p;
    sp(m,:) = Inf;                # sort missing groups last
    SORT = [SORT, sp];
    anyMiss = anyMiss | m;
  endfor

  [~, ia, ic] = unique (KEY, "rows");
  ng = numel (ia);
  grpMiss = anyMiss(ia);
  [~, ord] = sortrows (SORT(ia,:));
  reps = ia(ord);
  grpMiss = grpMiss(ord);
  pos = zeros (ng, 1);
  pos(ord) = 1:ng;
  G = pos(ic);

  if (! incMiss && any (grpMiss))
    keep = find (! grpMiss);
    newId = NaN (ng, 1);
    newId(keep) = 1:numel (keep);
    G = newId(G);
    reps = reps(keep);
    ng = numel (keep);
  endif
  ngroups = ng;
  repRows = reps;
endfunction

## Group the rows of 'groupsummary'/'groupcounts' by the grouping-variable values
## GRPCOLS (already binned when a GROUPBINS argument was given).  Returns G, the
## n-by-1 group numbers (NaN for an excluded row), NG the number of groups, GCOLS
## a 1-by-K cell of the typed grouping-variable output columns (one value per
## group), and an errmsg body emitted by the caller.  When INCEMPTY is true the
## unused categories of a categorical (or binned) grouping variable contribute
## empty groups, built from the full level machinery; otherwise only the observed
## groups are returned, in ascending grouping-value order with missing groups
## last.
function [G, ng, gcols, errmsg] = gs_grouping (grpCols, incMiss, incEmpty)
  errmsg = '';
  gcols = {};
  K = numel (grpCols);
  n = size (grpCols{1}, 1);
  if (incEmpty)
    [G, ng, lvlOf, levVals, ~, errmsg] = ...
            pivot_dimension (grpCols, n, incMiss, true);
    if (! isempty (errmsg))
      G = []; ng = 0;
      return;
    endif
    gcols = cell (1, K);
    for j = 1:K
      gcols{j} = levVals{j}(lvlOf(:,j), :);
    endfor
  else
    [G, ng, repRows, errmsg] = gs_group_rows (grpCols, incMiss);
    if (! isempty (errmsg))
      return;
    endif
    gcols = cell (1, K);
    for j = 1:K
      gcols{j} = grpCols{j}(repRows, :);
    endfor
  endif
endfunction

## Apply a single 'groupsummary' method M (a method-name char vector or a
## function handle) to the column slice X of one group, returning a row result V.
## Named methods omit missing values (except 'nummissing'); a function handle
## receives X unchanged and must return a single row.  Returns an errmsg body
## (empty on success) emitted by the caller.
function [v, errmsg] = gs_apply_method (m, x)
  v = [];
  errmsg = '';
  if (is_function_handle (m))
    v = m (x);
    if (size (v, 1) != 1)
      errmsg = "a function handle method must return a single row";
    endif
    return;
  endif

  ## Type-agnostic counting methods.
  if (strcmp (m, 'nummissing'))
    v = sum (gs_missing_mask (x), 1);
    return;
  endif
  if (strcmp (m, 'numunique'))
    miss = gs_missing_mask (x);
    if (size (x, 2) == 1)
      v = numel (unique (x(! miss,:)));
    else
      v = zeros (1, size (x, 2));
      for c = 1:size (x, 2)
        col = x(:,c);
        v(c) = numel (unique (col(! miss(:,c))));
      endfor
    endif
    return;
  endif

  ## The remaining named methods require numeric or logical data.
  if (! (isnumeric (x) || islogical (x)))
    errmsg = sprintf (strcat ("named method '%s' is not supported for", ...
                              " variables of type '%s'; use a function", ...
                              " handle"), m, class (x));
    return;
  endif
  x = double (x);
  ## An empty group (e.g. an unused IncludeEmptyGroups category) takes the
  ## method's empty value: 0 for the additive 'sum'/'nnz', NaN otherwise.
  if (rows (x) == 0)
    if (any (strcmp (m, {'sum', 'nnz'})))
      v = zeros (1, columns (x));
    else
      v = NaN (1, columns (x));
    endif
    return;
  endif
  nan = isnan (x);
  cnt = sum (! nan, 1);
  z = x;
  z(nan) = 0;
  switch (m)
    case 'sum'
      v = sum (z, 1);
    case 'mean'
      v = sum (z, 1) ./ cnt;
    case 'min'
      v = min (x, [], 1);
    case 'max'
      v = max (x, [], 1);
    case 'range'
      v = max (x, [], 1) - min (x, [], 1);
    case 'nnz'
      v = sum (x != 0 & ! nan, 1);
    case {'median', 'mode', 'var', 'std'}
      v = NaN (1, size (x, 2));
      for c = 1:size (x, 2)
        col = x(! nan(:,c), c);
        if (! isempty (col))
          switch (m)
            case 'median'
              v(c) = median (col);
            case 'mode'
              v(c) = mode (col);
            case 'var'
              v(c) = var (col);
            case 'std'
              v(c) = std (col);
          endswitch
        endif
      endfor
  endswitch
endfunction

## Return a logical mask the size of X flagging its missing elements, used by the
## type-agnostic 'groupsummary' methods.  Supports the numeric, logical, text,
## datetime, duration, calendarDuration, and categorical variable types.
function mask = gs_missing_mask (x)
  if (isa (x, 'datetime'))
    mask = isnan (datetime_to_datenum (x));
  elseif (isa (x, 'duration'))
    mask = isnan (days (x));
  elseif (isa (x, 'calendarDuration'))
    mask = any (isnan (x.proxyArray), 2);
  elseif (isa (x, 'categorical') || isa (x, 'string'))
    mask = ismissing (x);
  elseif (iscellstr (x))
    mask = cellfun (@isempty, x);
  elseif (islogical (x))
    mask = false (size (x));
  elseif (isnumeric (x))
    mask = isnan (x);
  else
    mask = false (size (x));
  endif
endfunction

## Build the level structure of one 'pivot' grouping variable COL.  Returns IDX,
## an n-by-1 vector of level indices (1..L) for the rows of COL (NaN for a row
## holding a missing value when INCMISS is false, so that row is excluded from
## every group), LEVVALS, a typed column vector with one representative value per
## level used to build row labels and column names, MISSLVL, a 1-by-L logical
## flagging the missing level, and an errmsg body (empty on success).  Levels are
## the sorted unique values of COL; a categorical variable uses its category
## order, and when INCEMPTY is true every category is a level even if unused in
## the data.  A missing value forms one extra level, sorted last, when INCMISS.
function [idx, levVals, missLvl, errmsg] = pivot_levels (col, incMiss, incEmpty)
  idx = [];
  levVals = [];
  missLvl = [];
  errmsg = '';
  n = size (col, 1);
  [p, miss, errmsg] = group_col_proxy (col);
  if (! isempty (errmsg))
    return;
  endif
  if (isa (col, 'categorical') && incEmpty)
    ## Every category is a level, in category order; codes are the proxy.
    cats = categories (col);
    L = numel (cats);
    idx = double (col)(:);
    levVals = categorical (cats(:), cats, 'Ordinal', isordinal (col));
    missLvl = false (1, L);
  else
    ## Observed levels only, sorted by proxy value ascending.
    idx = NaN (n, 1);
    keep = find (! miss);
    if (isempty (keep))
      levVals = col([], :);
      L = 0;
      missLvl = [];
    else
      [~, ia, ic] = unique (p(keep,:), "rows");
      idx(keep) = ic;
      levVals = col(keep(ia), :);
      L = numel (ia);
      missLvl = false (1, L);
    endif
  endif
  ## A missing value forms one extra level, sorted last, when included.
  if (any (miss))
    if (incMiss)
      L = L + 1;
      idx(miss) = L;
      mrow = find (miss, 1);
      levVals = [levVals; col(mrow, :)];
      missLvl = [missLvl, true];
    else
      idx(miss) = NaN;
    endif
  endif
endfunction

## Group the rows of one 'pivot' dimension (rows or columns) defined by the
## grouping-variable columns GRPCOLS (a cell array, empty for an omitted
## dimension).  N is the table height.  Returns GID, an n-by-1 group index per
## row (NaN when the row is excluded), NG, the number of groups, LVLOF, an
## ng-by-K matrix of per-variable level indices for each group, LEVVALS, a
## 1-by-K cell of the per-variable typed level values from 'pivot_levels',
## MISSLVLS, a 1-by-K cell of the per-variable missing-level logical flags, and
## an errmsg body (empty on success).  When INCEMPTY is true the groups span the
## full Cartesian product of the variables' levels (so unused combinations appear
## as empty groups); otherwise only the observed combinations are kept, sorted in
## ascending level order with the first variable varying slowest.
function [gid, ng, lvlOf, levVals, missLvls, errmsg] = ...
                              pivot_dimension (grpCols, n, incMiss, incEmpty)
  errmsg = '';
  K = numel (grpCols);
  if (K == 0)
    ## An omitted dimension is a single group holding every row.
    gid = ones (n, 1);
    ng = 1;
    lvlOf = zeros (1, 0);
    levVals = {};
    missLvls = {};
    return;
  endif
  idxAll = NaN (n, K);
  levVals = cell (1, K);
  missLvls = cell (1, K);
  sizes = zeros (1, K);
  for j = 1:K
    [idx, lv, ml, errmsg] = pivot_levels (grpCols{j}, incMiss, incEmpty);
    if (! isempty (errmsg))
      gid = []; ng = 0; lvlOf = [];
      return;
    endif
    idxAll(:,j) = idx;
    levVals{j} = lv;
    missLvls{j} = ml;
    sizes(j) = size (lv, 1);
  endfor
  gid = NaN (n, 1);
  if (incEmpty)
    ## Full Cartesian product, first variable slowest (most significant).
    ng = prod (sizes);
    lvlOf = ones (ng, K);
    period = 1;
    for j = K:-1:1
      lvlOf(:,j) = mod (floor ((0:ng-1)' / period), sizes(j)) + 1;
      period = period * sizes(j);
    endfor
    valid = all (! isnan (idxAll), 2);
    lin = zeros (n, 1);
    period = 1;
    for j = K:-1:1
      col = idxAll(:,j);
      col(isnan (col)) = 1;
      lin = lin + (col - 1) * period;
      period = period * sizes(j);
    endfor
    gid(valid) = lin(valid) + 1;
  else
    ## Observed combinations only, in ascending level order.
    valid = all (! isnan (idxAll), 2);
    if (! any (valid))
      ng = 0;
      lvlOf = zeros (0, K);
      return;
    endif
    [u, ~, ic] = unique (idxAll(valid,:), "rows");
    ng = size (u, 1);
    lvlOf = u;
    gid(valid) = ic;
  endif
endfunction

## Map a scalar grouping value VAL to the character vector used as a 'pivot'
## column variable name (or part of one).  Logical values render as 'true' or
## 'false', numeric values through 'num2str', and text, categorical, datetime,
## duration, and calendarDuration values through their displayed text.  A missing
## value renders as '<undefined>' so the resulting name is never empty.
function s = pivot_value_name (val)
  if (isa (val, 'categorical'))
    if (ismissing (val))
      s = '<undefined>';
    else
      c = cellstr (val);
      s = c{1};
    endif
  elseif (isa (val, 'string'))
    if (ismissing (val))
      s = '<missing>';
    else
      s = char (val);
    endif
  elseif (iscellstr (val))
    s = val{1};
  elseif (ischar (val))
    s = val;
  elseif (islogical (val))
    if (val)
      s = 'true';
    else
      s = 'false';
    endif
  elseif (isa (val, 'datetime') || isa (val, 'duration') ...
          || isa (val, 'calendarDuration'))
    s = char (val);
  elseif (isnumeric (val))
    s = num2str (val);
  else
    s = char (val);
  endif
  if (isempty (s))
    s = '<undefined>';
  endif
endfunction

## Return the value 'pivot' places in an empty cell for the named method M.
function val = pivot_empty_value (m)
  if (any (strcmp (m, {'sum', 'nnz', 'nummissing', 'numunique'})))
    val = 0;
  else
    val = NaN;
  endif
endfunction

## Return a one-row missing value MV of the same type as X for 'pivot' Method
## 'none' empty cells; OK is false when the type has no missing value.
function [mv, ok] = pivot_missing_scalar (x)
  ok = true;
  mv = [];
  mm = any (gs_missing_mask (x), 2);
  if (any (mm))
    mv = x(find (mm, 1), :);
  elseif (isnumeric (x))
    mv = nan (1, size (x, 2));
  else
    ok = false;
  endif
endfunction

## Build one 'pivot' output column for Method 'none' (rearrange without
## aggregating): column C of the pivot, length NR, of the same type as DATAVALS.
## Each cell must hold at most one value; an empty cell is filled with a missing
## value.  RGID and CGID are the per-row group indices.  Returns an errmsg body
## (empty on success) emitted by the caller.
function [col, errmsg] = pivot_none_column (dataVals, rGid, cGid, c, nR)
  errmsg = '';
  col = [];
  srcRow = ones (nR, 1);
  hasVal = false (nR, 1);
  for r = 1:nR
    rows = find (rGid == r & cGid == c);
    if (numel (rows) > 1)
      errmsg = strcat ("Method 'none' allows at most one value per cell, but", ...
                       " a group holds several; specify an aggregating method");
      return;
    elseif (numel (rows) == 1)
      srcRow(r) = rows;
      hasVal(r) = true;
    endif
  endfor
  col = dataVals(srcRow, :);
  if (! all (hasVal))
    [mv, ok] = pivot_missing_scalar (dataVals);
    if (! ok)
      errmsg = sprintf (strcat ("Method 'none' with empty cells is not", ...
                                " supported for data of type '%s'"), ...
                        class (dataVals));
      col = [];
      return;
    endif
    col(! hasVal, :) = mv;
  endif
endfunction

## Build the cellstr RowNames for a 'pivot' with RowLabelPlacement 'rownames'
## from the row-label columns ROWLABELCOLS (a 1-by-K cell of typed columns),
## joining the per-variable displayed values of each row with '_'.
function rn = pivot_row_names (rowLabelCols)
  K = numel (rowLabelCols);
  nR = size (rowLabelCols{1}, 1);
  rn = cell (nR, 1);
  for r = 1:nR
    parts = cell (1, K);
    for j = 1:K
      parts{j} = pivot_value_name (rowLabelCols{j}(r, :));
    endfor
    rn{r} = strjoin (parts, '_');
  endfor
endfunction

## Append the 'Overall_<method>' totals label LABEL as one extra element of the
## row-label column COL for 'pivot' IncludeTotals, keeping its type.  OK is false
## when COL cannot hold the text label (a numeric, datetime, or duration row
## label), so the caller falls back to row-name labelling.
function [col, ok] = pivot_append_label (col, label)
  ok = true;
  if (isa (col, 'categorical'))
    if (! iscategory (col, label))
      col = addcats (col, label);
    endif
    col(end+1, 1) = label;
  elseif (isa (col, 'string'))
    col(end+1, 1) = label;
  elseif (iscellstr (col))
    col(end+1, 1) = {label};
  else
    ok = false;
  endif
endfunction

## Validate an 'IncludedEdge' binning option VAL for method CALLER, returning it
## lowercased as 'left' or 'right'.
function e = check_included_edge (caller, val)
  if (isa (val, 'string') && isscalar (val))
    val = char (val);
  endif
  if (! (ischar (val) && isrow (val) ...
         && any (strcmpi (val, {'left', 'right'}))))
    error ("table.%s: 'IncludedEdge' must be 'left' or 'right'.", caller);
  endif
  e = lower (val);
endfunction

## Validate a logical-scalar 'pivot' option VAL named NAME, returning it as a
## logical scalar.
function tf = pivot_logical_opt (name, val)
  if (! (isscalar (val) && (islogical (val) || isnumeric (val))))
    error ("table.pivot: '%s' must be a logical scalar.", name);
  endif
  tf = logical (val);
endfunction

## Compute one 'pivot' cell value: apply METHOD to the data of the rows ROWS of
## one row-and-column group.  HASDV flags whether a data variable is given;
## DATAVALS holds its values.  'count' counts rows (or, with a data variable, the
## non-missing data values), 'percentage' is the row count as a percentage of
## TOTALASSIGNED, an empty cell takes the named method's empty value, and every
## other named method or function handle is applied through 'gs_apply_method'.
## Returns an errmsg body (empty on success) emitted by the caller.
function [v, errmsg] = pivot_cell_value (method, hasDV, dataVals, rows, ...
                                         totalAssigned)
  errmsg = '';
  if (ischar (method) && strcmp (method, 'count'))
    if (! hasDV)
      v = numel (rows);
    else
      v = sum (! any (gs_missing_mask (dataVals(rows,:)), 2));
    endif
  elseif (ischar (method) && strcmp (method, 'percentage'))
    v = 100 * numel (rows) / totalAssigned;
  elseif (isempty (rows))
    ## An empty cell takes the method's empty value; a function handle is not
    ## invoked on an empty slice, the cell is left missing.
    if (ischar (method))
      v = pivot_empty_value (method);
    else
      v = NaN;
    endif
  else
    [v, errmsg] = gs_apply_method (method, dataVals(rows,:));
  endif
endfunction

## Return the name used to prefix 'varfun' output variables: the name of FUNC,
## or 'Fun' when FUNC is an anonymous function handle.
function fname = apply_func_name (func)
  fstr = func2str (func);
  if (isempty (fstr) || fstr(1) == '@')
    fname = 'Fun';
  else
    fname = fstr;
  endif
endfunction

## Validate and normalise an OutputFormat value for the apply methods: map
## 'auto' to 'table' and accept 'table', 'uniform', and 'cell'.  CALLER names
## the method for error messages.
function fmt = check_output_format (caller, fmt)
  if (isa (fmt, 'string'))
    fmt = char (fmt);
  endif
  if (! (ischar (fmt) && isrow (fmt)))
    error ("table.%s: 'OutputFormat' must be a character vector.", caller);
  endif
  switch (lower (fmt))
    case {'auto', 'table'}
      fmt = 'table';
    case 'uniform'
      fmt = 'uniform';
    case 'cell'
      fmt = 'cell';
    case 'timetable'
      error ("table.%s: 'timetable' OutputFormat is not supported.", caller);
    otherwise
      error ("table.%s: invalid 'OutputFormat' value '%s'.", caller, fmt);
  endswitch
endfunction

## Build the cell array of input arguments passed to FUNC for the rows selected
## by the logical mask ROWS, taken from the input-variable values INCOLS (a cell
## array of variable values).  When SEPIN is true each variable's selected rows
## form a separate argument; otherwise they are horizontally concatenated into a
## single argument.  When EXTRACTCELL is true the contents of cell-valued
## variables are extracted.
function args = build_row_args (inCols, rows, sepIn, extractCell)
  vals = cell (1, numel (inCols));
  for k = 1:numel (inCols)
    col = inCols{k};
    if (extractCell && iscell (col))
      sub = col(rows);
      if (numel (sub) == 1)
        vals{k} = sub{1};
      else
        vals{k} = vertcat (sub{:});
      endif
    else
      vals{k} = col(rows,:);
    endif
  endfor
  if (sepIn)
    args = vals;
  else
    args = {horzcat(vals{:})};
  endif
endfunction

## Call FUNC with the arguments ARGS, requesting NOUT outputs, and return them
## in a 1-by-max(NOUT,1) cell row.  When ERRHANDLER is non-empty it is called
## with a struct describing any error thrown by FUNC (fields 'identifier',
## 'message', and 'index' set to IDX) followed by ARGS, and its outputs are
## used instead.
function out = apply_func (func, errHandler, idx, nout, args)
  out = cell (1, max (nout, 1));
  if (isempty (errHandler))
    [out{1:nout}] = func (args{:});
  else
    try
      [out{1:nout}] = func (args{:});
    catch err
      S = struct ('identifier', err.identifier, 'message', err.message, ...
                  'index', idx);
      [out{1:nout}] = errHandler (S, args{:});
    end_try_catch
  endif
endfunction

## Assemble the output of an apply method from the R-by-C cell array of per-row
## (or per-group) results RES with output names OUTNAMES.  For grouped output
## the grouping columns GCOLS (named GNAMES) and the GCOUNT counts are
## prepended; for ungrouped output these are empty.  FMT selects the 'table',
## 'uniform', or 'cell' return format; CALLER names the method for error
## messages.
function out = build_apply_result (caller, fmt, res, outNames, gcols, ...
                                   gnames, gcount, rowNames)
  if (nargin < 8)
    rowNames = {};
  endif
  C = size (res, 2);
  switch (fmt)
    case 'table'
      rescols = cell (1, C);
      for c = 1:C
        rescols{c} = vertcat (res{:,c});
      endfor
      if (isempty (gcols) && isempty (gcount))
        vars = rescols;
        names = outNames;
      else
        vars = [gcols, {gcount}, rescols];
        names = [gnames, {'GroupCount'}, outNames];
      endif
      if (isempty (rowNames))
        out = table (vars{:}, 'VariableNames', names);
      else
        out = table (vars{:}, 'VariableNames', names, 'RowNames', rowNames);
      endif
    case 'uniform'
      out = [];
      for c = 1:C
        colvals = res(:,c);
        if (! all (cellfun (@isscalar, colvals)))
          error (strcat ("table.%s: OutputFormat 'uniform' requires FUNC", ...
                         " to return a scalar for each call."), caller);
        endif
        out = [out, vertcat(colvals{:})];
      endfor
    case 'cell'
      out = res;
  endswitch
endfunction

## Assemble the output of a grouped 'rowfun' or 'varfun' from the NG-by-C cell
## array of per-group results RES.  Unlike an aggregating apply, FUNC may return
## several rows for a group; each group g therefore contributes
## 'size (RES{g,1}, 1)' rows and the grouping columns GCOLS (named GNAMES) and the
## GCOUNT counts are replicated to match before the per-group results are
## stacked.  FMT selects the 'table', 'uniform', or 'cell' return format; CALLER
## names the method for error messages.
function out = build_grouped_apply_result (caller, fmt, res, outNames, gcols, ...
                                           gnames, gcount)
  ng = size (res, 1);
  C = size (res, 2);
  switch (fmt)
    case 'table'
      repIdx = [];
      for g = 1:ng
        repIdx = [repIdx; repmat(g, size (res{g,1}, 1), 1)];
      endfor
      rescols = cell (1, C);
      for c = 1:C
        rescols{c} = vertcat (res{:,c});
      endfor
      gcolsR = cell (1, numel (gcols));
      for p = 1:numel (gcols)
        gcolsR{p} = gcols{p}(repIdx,:);
      endfor
      vars = [gcolsR, {gcount(repIdx)}, rescols];
      names = [gnames, {'GroupCount'}, outNames];
      out = table (vars{:}, 'VariableNames', names);
    case 'uniform'
      out = [];
      for c = 1:C
        colvals = res(:,c);
        if (! all (cellfun (@isscalar, colvals)))
          error (strcat ("table.%s: OutputFormat 'uniform' requires FUNC", ...
                         " to return a scalar for each call."), caller);
        endif
        out = [out, vertcat(colvals{:})];
      endfor
    case 'cell'
      out = res;
  endswitch
endfunction

## Detect the cell/non-cell mix of variable values VALS that cannot form a
## homogeneous array.  Returns the column indices [LO, HI] (in column order) of
## the first cell and first non-cell variable, or [] when VALS are not such a
## mix.  Callers emit the incompatibility error under their own method name.
function pair = mixed_cell_pair (vals)
  isCellVar = cellfun (@iscell, vals);
  if (any (isCellVar) && ! all (isCellVar))
    pair = sort ([find(isCellVar, 1), find(! isCellVar, 1)]);
  else
    pair = [];
  endif
endfunction

## Set the rows of a variable V selected by the logical MASK to the standard
## missing value for V's type.  Returns an errmsg body for unsupported types.
function [v, errmsg] = set_var_missing (v, mask)
  errmsg = '';
  if (! any (mask))
    return;
  endif
  if (isa (v, 'string'))
    v(mask) = string (missing);
  elseif (isa (v, 'categorical'))
    v(mask) = categorical (missing);
  elseif (isa (v, 'datetime'))
    v(mask) = NaT;
  elseif (isa (v, 'duration'))
    v(mask) = missing;
  elseif (isa (v, 'calendarDuration'))
    v(mask,:) = NaN;
  elseif (iscellstr (v))
    v(mask) = {''};
  elseif (islogical (v))
    v(mask,:) = false;
  elseif (isinteger (v))
    v(mask,:) = 0;
  elseif (isfloat (v))
    v(mask,:) = NaN;
  else
    errmsg = sprintf (strcat ("cannot create missing values for a variable", ...
                              " of type '%s'."), class (v));
  endif
endfunction

## Create an N-row array of standard missing values matching the type and width
## of PROTO.  Used when one input table has no rows to replicate from.  Returns
## an errmsg body for unsupported types.
function [col, errmsg] = missing_rows (proto, n)
  errmsg = '';
  col = [];
  w = max (size (proto, 2), 1);
  if (isa (proto, 'string'))
    col = repmat (string (missing), n, w);
  elseif (isa (proto, 'categorical'))
    col = repmat (categorical (missing), n, w);
  elseif (isa (proto, 'datetime'))
    col = repmat (NaT, n, w);
  elseif (isa (proto, 'duration'))
    col = hours (NaN (n, w));
  elseif (isa (proto, 'calendarDuration'))
    col = calmonths (NaN (n, w));
  elseif (iscellstr (proto))
    col = repmat ({''}, n, w);
  elseif (islogical (proto))
    col = false (n, w);
  elseif (isinteger (proto))
    col = zeros (n, w, class (proto));
  elseif (isfloat (proto))
    col = NaN (n, w);
  else
    errmsg = sprintf (strcat ("cannot create missing values for a variable", ...
                              " of type '%s'."), class (proto));
  endif
endfunction

## Map a variable type name to the ODS cell value type used by 'table2ods'.
## Numeric types become 'float', logical becomes 'boolean', datetime and
## duration map to the native 'date' and 'time' types, and everything else
## (text, categorical, calendarDuration, cell) is written as a 'string'.
function vt = ods_value_type (typestr)
  switch (typestr)
    case 'logical'
      vt = 'boolean';
    case 'datetime'
      vt = 'date';
    case 'duration'
      vt = 'time';
    case {'double', 'single', 'int8', 'int16', 'int32', 'int64', ...
          'uint8', 'uint16', 'uint32', 'uint64'}
      vt = 'float';
    otherwise
      vt = 'string';
  endswitch
endfunction

## Add, replace, or append table T to the struct of tables S (read from an
## existing house workbook) for the sheet named SHEET, per WRITEMODE.  The
## struct is later written back with 'struct2ods'; sheet names that are not
## valid field names ride along as the 'ActualSheetName' custom property.
function s = merge_table_into_struct (s, T, sheet, writeMode)
  ## Find the field whose sheet name (ActualSheetName, else field name) matches.
  fields = fieldnames (s);
  targetField = '';
  for i = 1:numel (fields)
    fsheet = fields{i};
    cp = s.(fields{i}).Properties.CustomProperties;
    if (isstruct (cp) && isfield (cp, 'ActualSheetName') ...
        && ! isempty (cp.ActualSheetName))
      fsheet = cp.ActualSheetName;
    endif
    if (strcmp (fsheet, sheet))
      targetField = fields{i};
      break;
    endif
  endfor

  if (strcmp (writeMode, 'append') && ! isempty (targetField))
    ## Append the rows; table vertcat errors if the variables are incompatible.
    combined = [s.(targetField); T];
    s.(targetField) = copy_actual_sheet_name (combined, s.(targetField));
  elseif (! isempty (targetField))
    ## Replace the sheet, keeping its resolved name.
    s.(targetField) = copy_actual_sheet_name (T, s.(targetField));
  else
    ## A new sheet: canonicalise SHEET to a unique field name and stash the
    ## original name when it had to change.
    fn = matlab.lang.makeValidName (sheet);
    base = fn;
    j = 1;
    while (isfield (s, fn))
      fn = sprintf ("%s_%d", base, j);
      j += 1;
    endwhile
    if (! strcmp (fn, sheet))
      T = addprop (T, 'ActualSheetName', 'table');
      T.Properties.CustomProperties.ActualSheetName = sheet;
    endif
    s.(fn) = T;
  endif
endfunction

## Copy the 'ActualSheetName' custom property from SRC onto T, if SRC carries it.
function T = copy_actual_sheet_name (T, src)
  cp = src.Properties.CustomProperties;
  if (isstruct (cp) && isfield (cp, 'ActualSheetName') ...
      && ! isempty (cp.ActualSheetName))
    tcp = T.Properties.CustomProperties;
    if (! (isstruct (tcp) && isfield (tcp, 'ActualSheetName')))
      T = addprop (T, 'ActualSheetName', 'table');
    endif
    T.Properties.CustomProperties.ActualSheetName = cp.ActualSheetName;
  endif
endfunction

## Prepare the flat value/name/type cell arrays produced by 'table2cellarrays'
## for the MATLAB-compatible 'writetable' output: strip or keep the leading row
## names column (which carries an empty variable name) per WRITEROWNAMES, and
## de-duplicate the shared names of a multicolumn variable with _1, _2, ...
## suffixes, matching MATLAB.
function [names, V, T] = writetable_prep (V, N, T, writeRowNames)
  hasRN = (! isempty (N) && isempty (N{1}));
  rnCol = {};
  if (hasRN)
    rnCol = V(:,1);
    V(:,1) = [];  N(:,1) = [];  T(:,1) = [];
  endif
  names = {};
  c = 1;
  n = numel (N);
  while (c <= n)
    c2 = c;
    while (c2 < n && strcmp (N{c2+1}, N{c}))
      c2 += 1;
    endwhile
    k = c2 - c + 1;
    if (k == 1)
      names{end+1} = N{c};
    else
      for j = 1:k
        names{end+1} = sprintf ("%s_%d", N{c}, j);
      endfor
    endif
    c = c2 + 1;
  endwhile
  if (writeRowNames && hasRN)
    V = [rnCol, V];
    names = [{'Row'}, names];
    T = [{'cellstr'}, T];
  endif
endfunction

## Translate a MATLAB delimiter (named or literal) into a single character for
## 'writetable'.
function d = wt_resolve_delimiter (delim)
  if (isa (delim, 'string'))
    delim = char (delim);
  endif
  if (! ischar (delim))
    error ("table.writetable: 'Delimiter' must be a character vector or string.");
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
        error ("table.writetable: unsupported 'Delimiter' value '%s'.", delim);
      endif
  endswitch
endfunction

## Format a datetime column as a column cell of ISO 8601 strings for 'table2ods'.
## NaT values yield an empty string, which the writer records as a missing (empty)
## cell.  The wall-clock components are used; any TimeZone is not encoded in the
## value (mirroring the datetime display round-trip of the CSV path).
function C = datetime2iso (dt)
  [Y, M, D] = ymd (dt(:));
  [h, m, s] = hms (dt(:));
  n = numel (Y);
  C = cell (n, 1);
  for i = 1:n
    if (isnan (Y(i)))
      C{i} = '';
    else
      C{i} = sprintf ("%04d-%02d-%02dT%02d:%02d:%s", ...
                      Y(i), M(i), D(i), h(i), m(i), iso_seconds (s(i)));
    endif
  endfor
endfunction

## Format a duration column as a column cell of ISO 8601 duration strings
## (@code{PTnHnMnS}) for 'table2ods'.  NaN values yield an empty string (written
## as a missing cell).  Hours are not wrapped at 24, so durations of any
## magnitude are preserved; negative durations carry a leading minus sign.
function C = duration2iso (du)
  tot = seconds (du(:));
  n = numel (tot);
  C = cell (n, 1);
  for i = 1:n
    if (isnan (tot(i)))
      C{i} = '';
    else
      a = abs (tot(i));
      H = floor (a / 3600);
      MI = floor (mod (a, 3600) / 60);
      S = mod (a, 60);
      sgn = '';
      if (tot(i) < 0)
        sgn = '-';
      endif
      C{i} = sprintf ("%sPT%dH%dM%sS", sgn, H, MI, iso_seconds (S));
    endif
  endfor
endfunction

## Format a seconds value for an ISO 8601 string: a two-digit integer when whole,
## otherwise a fractional part (up to microseconds) with trailing zeros trimmed.
function str = iso_seconds (s)
  si = floor (s);
  frac = round ((s - si) * 1e6);
  if (frac >= 1e6)                       # rounded up to a whole second
    si += 1;
    frac = 0;
  endif
  if (frac == 0)
    str = sprintf ("%02d", si);
  else
    fs = regexprep (sprintf ("%06d", frac), '0+$', '');
    str = sprintf ("%02d.%s", si, fs);
  endif
endfunction
