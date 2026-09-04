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

classdef table < tabular
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
  ## table, you can also use curly brackets much like cell arrays to retrieve
  ## the contents of the table.  In this case, the original data types of the
  ## selected variables are returned.
  ##
  ## An empty numeric variable subscript selects no variables, so
  ## @code{@var{tbl}(:,[])} is a table of the same height with nothing in
  ## it.  An empty cell is not a subscript and is refused, as it is on any
  ## other array.
  ##
  ## Assigning an empty matrix to a subscripted table deletes rows or
  ## variables.  @code{@var{tbl}(@var{rows},:) = []} removes the referenced
  ## rows, @code{@var{tbl}(:,@var{vars}) = []} removes the referenced
  ## variables, and @code{@var{tbl}.@var{varname} = []} removes a single
  ## variable by name.  One of the two subscripts must be a colon, and when
  ## both of them are, the rows are removed.  A table that has lost its last
  ## variable keeps its height, so a five-row table displays as
  ## @qcode{5x0} rather than as empty.
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

  endproperties

################################################################################
##                         **    Subclass hooks    **                         ##
################################################################################
##                                                                            ##
## The eleven hooks 'tabular' declares, implemented for a table, whose rows   ##
## are labelled by 'RowNames' and may carry no labels at all.                 ##
##                                                                            ##
################################################################################

  methods (Access = protected)

    ## True when this table has 'RowNames'.  They are optional, and a table
    ## created without them keeps the empty cell array the property defaults
    ## to, so emptiness is the whole test.
    function tf = hasRowLabels (this)
      tf = ! isempty (this.RowNames);
    endfunction

    ## The 'RowNames' cell array exactly as stored: a column cellstr of
    ## unique, nonempty names, one per row, or empty when the table has none.
    function out = getRowLabels (this)
      out = this.RowNames;
    endfunction

    ## The constant 'RowNames', which is the property name.  It is not the
    ## row dimension name: 'DimensionNames{1}' also reaches the labels, but it
    ## defaults to 'Row' and the user may rename it.
    function out = rowLabelName (this)
      out = 'RowNames';
    endfunction

    ## The 'RowNames' unchanged.  A table's labels are already character data,
    ## so displaying and exporting them needs no rendering step and this hook
    ## has nothing to do beyond what 'getRowLabels' does.
    function out = rowLabelStrings (this)
      out = this.RowNames;
    endfunction

    ## The one property a table publishes about its row labels, under the
    ## name it declares them with.  It is also what 'rowLabelName' answers
    ## here, but the two are not the same question and a timetable answers
    ## them differently.
    function out = rowLabelProperties (this)
      out = struct ();
      out.RowNames = this.RowNames;
    endfunction

    ## Nothing.  A table's row names are printed with no heading over them
    ## and no rule under them, not even the row dimension name, which may be
    ## renamed without changing the display at all.
    function out = rowLabelHeader (this)
      out = '';
    endfunction

    ## A table orders by its row names under either spelling.
    function out = rowLabelKeyNames (this)
      out = {'RowNames', this.DimensionNames{1}};
    endfunction

    ## Row names given as a cellstr, or cleared when there are none.
    function this = setRowLabels (this, labels)
      this.RowNames = labels;
    endfunction

    ## A table is already a plain table; only its row names go.
    function out = plainTable (this)
      out = this;
      out.RowNames = {};
    endfunction

    ## Row names are unique, so grouping by them would put every row in a
    ## group of its own; they are not a grouping key.
    function tf = groupsByLabels (this)
      tf = false;
    endfunction

    ## A bare 'sortrows (tbl)' orders by every variable, not by the names.
    function tf = sortsByLabelsByDefault (this)
      tf = false;
    endfunction

    ## Row names do not tell two otherwise equal rows apart.
    function tf = uniqueIncludesLabels (this)
      tf = false;
    endfunction

    ## A row name never disqualifies its row.
    function tf = usableRowLabels (this)
      tf = true (height (this), 1);
    endfunction

    ## A table has only the order of its rows to interpolate against.
    function [x, ownPoints, errmsg] = fillSamplePoints (this)
      x = (1:height (this))';
      ownPoints = false;
      errmsg = '';
    endfunction

    ## The one row label property a table has.  'RowNames' is the only name
    ## it recognises; anything else is not a table property at all and the
    ## caller says so.
    function [this, handled] = setRowLabelProperty (this, name, val, chain_s)
      handled = isequal (name, 'RowNames');
      if (! handled)
        return;
      endif
      ## Check for empty input to remove RowNames from table.
      if (isempty (val))
        this.RowNames = {};
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
            this = removevars (this, ixVar);
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
    endfunction

    ## Keeps the 'RowNames' entries picked out by IXROWS, in the order given,
    ## so that reordering or repeating rows carries their names along.  A
    ## table with no row names is left alone: indexing the empty cell array
    ## would raise instead of staying empty.
    function this = subsetRowLabels (this, ixRows)
      if (! isempty (this.RowNames))
        this.RowNames = this.RowNames(ixRows);
      endif
    endfunction

    ## Drops the 'RowNames' outright, putting the property back to the empty
    ## cell array that marks a table carrying no row labels.
    function this = clearRowLabels (this)
      this.RowNames = {};
    endfunction

    ## Row names must stay unique, so a repeated name takes a numbered one:
    ## 'r1' repeated becomes 'r1', 'r1_1'.  A table with no row names has
    ## nothing to repeat.
    function this = repeatRowLabels (this, n, elementwise)
      if (isempty (this.RowNames))
        return;
      endif
      base = this.RowNames;
      nrow = numel (base);
      if (elementwise)
        ix = repelem ((1:nrow)', n, 1);
      else
        ix = repmat ((1:nrow)', n, 1);
      endif
      names = base(ix);
      seen = zeros (1, nrow);
      for k = 1:numel (ix)
        v = ix(k);
        seen(v)++;
        if (seen(v) > 1)
          names{k} = sprintf ('%s_%d', base{v}, seen(v) - 1);
        endif
      endfor
      this.RowNames = names;
    endfunction

    ## A table built from an apply method's output.  Row names are carried
    ## only where the method has some to give, which the mapping methods do
    ## and the reducing ones do not; ROWIX names input rows and means nothing
    ## to a class whose labels do not follow them.
    function out = assembleApply (this, vars, names, rowLabels, rowIx)
      if (isempty (rowLabels))
        out = table (vars{:}, 'VariableNames', names);
      else
        out = table (vars{:}, 'VariableNames', names, 'RowNames', rowLabels);
      endif
    endfunction

    ## Wraps the metadata struct that 'getProperties' assembles in a
    ## 'datatypes.tabular.TableProperties', the class that adds 'RowNames' to
    ## the shared properties and fixes the order the whole set displays in.
    function out = makeProperties (this)
      out = datatypes.tabular.TableProperties (getProperties (this), ...
                                               this.CustomPropTypes);
    endfunction

    ## Matches ROWREF, a cellstr of row names, against the 'RowNames' and
    ## returns their positions.  Raises when the table has no row names at all
    ## to match against, and again naming every reference that is not one of
    ## them.  Row names are unique, so each match is a single row.
    function ixRows = resolveRowRef (this, rowRef)
      ## A table's rows are labelled by name, so a row time is no more a
      ## reference to one of them than a struct is.
      if (! iscellstr (rowRef))
        error ("table: unsupported row indexing operand type: '%s'", ...
               class (rowRef));
      endif
      if (isempty (this.RowNames))
        error ("table: this table has no RowNames.");
      endif
      [tf, ixRows] = ismember (rowRef, this.RowNames);
      if (! all (tf))
        error ("table: no such named row in table: '%s'", ...
               strjoin (rowRef(! tf), ", "));
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
    ## creates a new table with the given variables.  The variables passed as
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
    ## table.  @var{varNames} must be either a cell array of character vectors
    ## or a string array with the same number of nonempty and unique elements as
    ## the number of table variables.
    ##
    ## @code{@var{tbl} = table (@dots{}, @qcode{'RowNames'}, @var{rowNames})}
    ## specifies the row names to use in the constructed table.  @var{rowNames}
    ## must be either a cell array of character vectors or a string array with
    ## the same number of nonempty and unique elements as the number of rows in
    ## the table.
    ##
    ## @code{@var{tbl} = table (@dots{}, @qcode{'DimensionNames'},
    ## @var{dimNames})} specifies the dimension names to use in the constructed
    ## table.  @var{dimNames} must be either a two-element cell array of
    ## character vectors or a two-element string array with nonempty and unique
    ## elements.
    ##
    ## @code{@var{tbl} = table ()} returns an empty table with 0 rows and 0
    ## variables.
    ##
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
                       " reserved name: '%s'"), this.DimensionNames{idr});
      endif
      ## Check for conflict between VariableNames and DimensionNames
      idx = ismember (this.DimensionNames, VariableNames);
      if (any (idx))
        error ("table: duplicate dimension and variable name: '%s'", ...
               this.DimensionNames{idx});
      endif

      ## Construct a preallocated table with default values.  SIZEROWS holds
      ## the requested row count, which is the only record of it when no
      ## variable is asked for.
      sizeRows = [];
      if ((numel (args) == 2 || numel (args) == 4)
          && strcmpi (args{1}, 'Size')
          && (numel (args) == 2 || strcmpi (args{3}, 'VariableTypes')))
        ## Validate the size specifier
        if (! isnumeric (args{2}) || numel (args{2}) != 2)
          error ("table: 'Size' must be a two-element numeric vector.");
        endif
        ## Get number of rows and variables
        nr = args{2}(1);
        nv = args{2}(2);
        sizeRows = nr;
        ## Get variable types.  'Size' on its own gives every variable a
        ## double, which is what a size with no variables needs and what a
        ## preallocation without a stated type is taken to mean.
        if (numel (args) == 4)
          varTypes = args{4};
        else
          varTypes = repmat ({'double'}, 1, nv);
        endif
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
              error ("table: unsupported variable type: '%s'", varTypes{i});
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
                         " (%d) and variable values (%d)."), ...
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
      ## Descriptions and units stay unset until they are given: an empty
      ## property says 'none', which is not the same as one blank per
      ## variable and is what an empty assignment returns them to.
      this.VariableDescriptions = {};
      this.VariableUnits = {};
      this.VariableNames = VariableNames(:)';
      this.VariableValues = VariableValues;
      this.VariableTypes = cellfun ('class', VariableValues, ...
                                    'UniformOutput', false);
      if (! isempty (VariableValues))
        this.RowCount = 0;
      elseif (! isempty (sizeRows))
        this.RowCount = sizeRows;
      endif
      if (! isempty (RowNames))
        if (! isempty (VariableValues))
          nrows = size (VariableValues{1}, 1);
        elseif (! isempty (sizeRows))
          nrows = sizeRows;
        else
          nrows = 0;
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
      A = varsAsArray (this, 'table2array');
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
      C = varsAsCell (this);
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
## 'table2csv'        'table2ods'        'writetable'                         ##
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
    ## whenever @emph{any} variable has a non-empty description or unit,
    ## respectively (the others are left empty).
    ##
    ## Note the following round-trip limitations when reading the file back
    ## with @code{csv2table}: @code{calendarDuration} and @code{categorical}
    ## variables are returned as cell arrays of character vectors (their values
    ## are not reconstructed), missing @code{string} values are read back as
    ## empty strings, and datetime and duration display formats are not
    ## preserved, although the values themselves are exact.
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
      ## Descriptions and units are written when any variable carries one (the
      ## rest are left empty); nested variables expand them to as many rows as
      ## varNames/varTypes.
      Drows = cellfun (@(x) max (1, size (x, 1)), D);
      if (! isempty (this.VariableDescriptions)
          || any (cellfun (@(x) ! isempty (x), D(isvar))))
        Dmaxr = max (Drows(isvar));
      else
        Dmaxr = 0;
      endif
      Urows = cellfun (@(x) max (1, size (x, 1)), U);
      if (! isempty (this.VariableUnits)
          || any (cellfun (@(x) ! isempty (x), U(isvar))))
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
          Header{1,c} = rowLabelName (this);
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
    ## The data sheet (named @qcode{Sheet1} by default) carries one natively
    ## typed cell per value, and a hidden @qcode{__datatypes_meta__} sheet
    ## carries the variable types, names, descriptions, and units needed to
    ## restore the exact Octave types on read-back.  Variables map to ODS cell
    ## types as follows:
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
    ## written whenever @emph{any} variable has a non-empty description or unit,
    ## respectively (the others are left empty).  A zone-aware @code{datetime}
    ## variable keeps its @code{TimeZone} on read-back.
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
    ## are not reconstructed), and datetime and duration display formats are not
    ## preserved, although the values themselves are exact.
    ##
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
    ## text; @qcode{.ods} as an OpenDocument spreadsheet; and @qcode{.xlsx} and
    ## @qcode{.xlsm} as Excel spreadsheets.  Use the @qcode{'FileType'} option
    ## to override the inferred type.
    ##
    ## Unlike @code{table2csv}/@code{table2ods}, no type metadata is written:
    ## the file holds only an optional variable-name header row followed by the
    ## data, so it can be read by other applications.  Type information is
    ## recovered by @code{readtable} through automatic detection (text) or the
    ## native cell types (spreadsheet).  The following options are supported:
    ##
    ## @multitable @columnfractions 0.28 0.72
    ## @headitem @var{Name} @tab @var{Value}
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
    ## write.  The default is the first sheet of an existing workbook, or
    ## @qcode{'Sheet1'} for a new file.
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
    ## @qcode{'Sheet'} (defaulting to the first existing sheet) is added or
    ## replaced while every other sheet is preserved, unless @qcode{'WriteMode'}
    ## is @qcode{'replacefile'}.  For ODS, existing foreign spreadsheets (for
    ## example those written by LibreOffice) are updated in place, keeping their
    ## other parts; for Excel (@qcode{.xlsx}, @qcode{.xlsm}) the workbook is
    ## read back and rewritten, so only its cell values are preserved.
    ##
    ## Dates written to Excel use its 1900 serial date system, which counts a
    ## 29 February 1900 that never existed: 1900-03-01 is serial 61, and every
    ## earlier date is one less than its plain day count.  That system has no
    ## serial below 0 (1899-12-31), so any earlier date is written as text
    ## rather than as a date, as MATLAB does.  MATLAB spells that text in the
    ## datetime display format; here it is written in ISO 8601 form.
    ##
    ## Nested tables and structures are not supported, and the legacy binary
    ## formats @qcode{.xls} and @qcode{.xlsb} are not supported either; use
    ## @qcode{.xlsx}, @qcode{.ods}, or a text format.
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
          case {'.ods', '.fods', '.xlsx', '.xlsm'}
            fileType = 'spreadsheet';
          case {'.xls', '.xlsb'}
            error (strcat ("table.writetable: '%s' Excel files are not", ...
                           " supported; use '.xlsx', '.ods', or a text", ...
                           " format."), ext);
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
      ## Office Open XML (.xlsx/.xlsm) uses a separate courier from ODS.
      isXlsx = any (strcmpi (ext, {'.xlsx', '.xlsm'}));

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
      [names, V, T] = tabular.writetable_prep (V, N, T, writeRowNames);

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
          vtype{c} = tabular.ods_value_type (T{c});
        endfor
        opts = struct ();
        ## Append mode writes data rows only, never a header.
        if (writeVarNames && ! strcmp (writeMode, 'append'))
          opts.header = names;
        else
          opts.header = {};
        endif
        ## Writing into an existing workbook with no explicit 'Sheet' targets the
        ## first existing sheet (MATLAB behaviour), not a new 'Sheet1'.
        if (isempty (sheet) && exist (file, 'file') ...
            && ! strcmp (writeMode, 'replacefile'))
          if (isXlsx)
            [~, ~, ~, exNames] = __xlsx2table__ (file);
          else
            [~, ~, ~, exNames] = __ods2table__ (file);
          endif
          if (iscell (exNames) && ! isempty (exNames))
            sheet = exNames{1};
          endif
        endif
        if (! isempty (sheet))
          opts.sheetname = sheet;
        endif
        if (isXlsx)
          if (exist (file, 'file') && ! strcmp (writeMode, 'replacefile'))
            ## Merge into an existing workbook by reading it back, modifying the
            ## struct of tables, and rewriting (interop re-encode, like the
            ## incremental table2ods path).
            s = xlsx2struct (file);
            s = merge_table_into_struct (s, this, sheet, writeMode);
            struct2xlsx (file, s);
            msg = 0;
          else
            ## A fresh single-sheet write.
            if (! isempty (range))
              [r1, c1] = __a1ref__ (range);
              opts.roff = r1 - 1;
              opts.coff = c1 - 1;
            endif
            opts.macro = strcmpi (ext, '.xlsm');
            msg = __table2xlsx__ (file, V, vtype, opts);
          endif
        else
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
        endif
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
    function [varargout] = summary (this, varargin)
      if (! isempty (varargin))
        error ("table.summary: too many input arguments.");
      endif
      s = summaryOf (this);
      if (nargout == 0)
        summaryPrint (this, s, inputname (1));
      elseif (nargout == 1)
        varargout{1} = s;
      else
        error ("table.summary: invalid number of output arguments.");
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
      if (! isempty (this.VariableValues))
        out = size (this.VariableValues{1}, 1);
      elseif (! isempty (this.RowNames))
        ## The row names outlive the variables and count the rows themselves.
        out = numel (this.RowNames);
      else
        out = this.RowCount;
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
    ## table @var{tbl}.  @var{k} must be a real, nonnegative, integer scalar
    ## value.  If @var{tbl} has fewer than @var{k} rows, then all rows are
    ## displayed, and @var{k} of zero displays none.
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
      if (nargin < 2)
        k = [];
      endif
      [ixRows, errmsg] = headTailRows (this, k, false);
      if (! isempty (errmsg))
        error ("table.head: %s", errmsg);
      endif
      out = subsetrows (this, ixRows);
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
    ## table @var{tbl}.  @var{k} must be a real, nonnegative, integer scalar
    ## value.  If @var{tbl} has fewer than @var{k} rows, then all rows are
    ## displayed, and @var{k} of zero displays none.
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
      if (nargin < 2)
        k = [];
      endif
      [ixRows, errmsg] = headTailRows (this, k, true);
      if (! isempty (errmsg))
        error ("table.tail: %s", errmsg);
      endif
      out = subsetrows (this, ixRows);
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
    ## ascending order based on the values in the first variable.  If elements
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
    ## character argument.  This Octave-specific syntax facilitates the use of
    ## the @var{direction} input argument when no particular variable needs to
    ## be selected to sort on.  Additionally, @var{vars} can be a
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
    ## the specified variables and/or row names used for sorting the table.
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
      [index, errmsg] = sortrowsIndex (this, varargin);
      if (! isempty (errmsg))
        error ("table.sortrows: %s", errmsg);
      endif
      tbl = subsetrows (this, index);
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
      [ia, ic, errmsg] = uniqueIndex (this, varargin);
      if (! isempty (errmsg))
        error ("table.unique: %s", errmsg);
      endif
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
    ## according to its row names.  For this syntax to work, @var{rowDimName}
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
    ## character argument.  This Octave-specific syntax facilitates the use of
    ## the @var{direction} input argument when no particular variable needs to
    ## be selected to sort on.  Additionally, @var{vars} can be a
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
    ## the specified variables and/or row names used for sorting the table.
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
      [TF, errmsg] = issortedrowsCheck (this, varargin);
      if (! isempty (errmsg))
        error ("table.issortedrows: %s", errmsg);
      endif
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
    ## Return the top rows of a table.
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
    ## Octave-specific syntax facilitates the use of the @var{direction} input
    ## argument when no particular variable needs to be selected to sort on.
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
    ## variables and/or row names used for sorting the table.  The order
    ## specified by @var{direction} always takes precedence over the order
    ## defined by a numerical vector of integers in @var{vars}.  @var{direction}
    ## must always be the 3rd input argument.  If you want to omit passing
    ## selected variables and allow @code{sortrows} to work on consecutive
    ## variables until all ties are resolved, then you can leave the second
    ## input argument empty, as in
    ## @code{sortrows (@var{tblA}, @{[]@}, @var{direction})} or pass a
    ## colon argument for @var{vars} as in
    ## @code{sortrows (@var{tblA}, @{':'@}, @var{direction})}.
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
      [ix, errmsg] = topkrowsIndex (this, k, varargin);
      if (! isempty (errmsg))
        error ("table.topkrows: %s", errmsg);
      endif
      tbl = subsetrows (this, ix);
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
    ## adds the new variables after (i.e. to the right of) the table variable
    ## specified in @var{location}, which can be a character vector, a string
    ## scalar, a scalar integer value, or even a logical vector with
    ## @qcode{width (@var{tblA})} elements, as long as it indexes a single
    ## variable in @var{tblA}.
    ##
    ## @code{@var{tblB} = addvars (@dots{}, @code{'Before'}, @var{location})}
    ## adds the new variables before (i.e. to the left of) the table variable
    ## specified in @var{location}, which can be a character vector, a string
    ## scalar, a scalar integer value, or even a logical vector with
    ## @qcode{width (@var{tblA})} elements, as long as it indexes a single
    ## variable in @var{tblA}.
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
      ## Only the public method can read the caller's names.
      argNames = cell (1, numel (varargin));
      for i = 1:numel (varargin)
        argNames{i} = inputname (i + 1);
      endfor
      [tbl, errmsg] = addvarsResult (this, argNames, varargin{:});
      if (! isempty (errmsg))
        error ("table.addvars: %s", errmsg);
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
    ## @item a string array specifying a single or multiple variables.
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
    function tbl = renamevars (this, varargin)
      [tbl, errmsg] = renamevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.renamevars: %s", errmsg);
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
    ## moved.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be moved.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @code{@var{tblB} = movevars (@dots{}, @code{'After'}, @var{location})}
    ## moves the selected variables after (i.e. to the right of) the table
    ## variable specified in @var{location}, which can be a character vector, a
    ## string scalar, a scalar integer value, or even a logical vector with
    ## @qcode{width (@var{tblA})} elements, as long as it indexes a single
    ## variable in @var{tblA} which is not selected by @var{vars}.
    ##
    ## @code{@var{tblB} = movevars (@dots{}, @code{'Before'}, @var{location})}
    ## moves the selected variables before (i.e. to the left of) the table
    ## variable specified in @var{location}, which can be a character vector, a
    ## string scalar, a scalar integer value, or even a logical vector with
    ## @qcode{width (@var{tblA})} elements, as long as it indexes a single
    ## variable in @var{tblA} which is not selected by @var{vars}.
    ##
    ## @end deftypefn
    function tbl = movevars (this, varargin)
      [tbl, errmsg] = movevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.movevars: %s", errmsg);
      endif
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
    ## @item a string array specifying a single or multiple variables.
    ## @item a numeric array of integer values indexing the variables to be
    ## removed.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be removed.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @end deftypefn
    function tbl = removevars (this, varargin)
      [tbl, errmsg] = removevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.removevars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tblB} =} splitvars (@var{tblA})
    ## @deftypefnx {table} {@var{tblB} =} splitvars (@var{tblA}, @var{vars})
    ## @deftypefnx {table} {@var{tblB} =} splitvars (@dots{}, @qcode{'NewVariableNames'}, @var{NewNames})
    ##
    ## Split multicolumn variables in a table.
    ##
    ## @code{@var{tblB} = splitvars (@var{tblA})} splits multicolumn variables
    ## in @var{tblA} so that they are single-column variables in @var{tblB},
    ## while all single-column variables in @var{tblA} are copied to @var{tblB}
    ## unaltered.  Each newly created single-column variable in @var{tblB} is
    ## uniquely named by joining the name of its parent multicolumn variable in
    ## @var{tblA} and the corresponding column number.  If a variable in
    ## @var{tblA} contains a table, then each variable of this nested table is
    ## returned as a newly created variable in @var{tblB}.  By default, these
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
    ## split.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be split.
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
      [tbl, errmsg] = splitvarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.splitvars: %s", errmsg);
      endif
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
    ## will result in a multicolumn variable of @qcode{'string'} data type, by
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
    ## merged.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be merged.
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
    function tbl = mergevars (this, varargin)
      [tbl, errmsg] = mergevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.mergevars: %s", errmsg);
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
    function tbl = convertvars (this, varargin)
      [tbl, errmsg] = convertvarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.convertvars: %s", errmsg);
      endif
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
    ## variables of @var{tblB} are arrays, otherwise they are cell arrays.  If
    ## the input table @var{tblA} contains @qcode{RowNames}, then those names
    ## become the variable names of the output table @var{tblB}, otherwise the
    ## variable names of @var{tblB} are generated automatically.
    ## @code{rows2vars} cannot handle multicolumn variables or nested tables.
    ##
    ## @code{@var{tblB} = rows2vars (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for reorienting the table with the
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
    ## @qcode{'DataVariables'} Name-Value paired argument.
    ## @item @qcode{'VariableNamingRule'} must be a character vector specifying
    ## the rule for naming variables in the output table @var{tblB}.  When set
    ## to @qcode{'modify'} (default), the variable names are modified so that
    ## they are valid variable identifiers.  When set to @qcode{'preserve'}, the
    ## original names are preserved.
    ## @end itemize
    ##
    ## @end deftypefn
    function tbl = rows2vars (this, varargin)
      [tbl, errmsg] = rows2varsResult (this, varargin);
      if (! isempty (errmsg))
        error ("table.rows2vars: %s", errmsg);
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
    ## stacked.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be stacked.
    ## @item a @qcode{vartype} object used to create a subscript that selects
    ## variables of a specified type.
    ## @end itemize
    ##
    ## @code{@var{tblB} = stack (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for stacking table variables with the
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
      if (nargin < 2)
        vars = [];
      endif
      [tbl, idxA, errmsg] = stackResult (this, vars, varargin);
      if (! isempty (errmsg))
        error ("table.stack: %s", errmsg);
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
    ## unstacked.
    ## @item a logical vector of the same length as the width of the table
    ## @var{tblA} indexing as @qcode{true} the variables to be unstacked.
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
    ## specifies additional parameters for unstacking table variables with the
    ## following Name-Value paired arguments.
    ##
    ## @itemize
    ## @item @qcode{'GroupingVariables'} specifies the variables that should be
    ## used as grouping variables.  All valid schemes for indexing a table
    ## variable can be used.  If grouping variables have missing values, the
    ## data from corresponding rows are not aggregated in the output table.
    ## Table row names cannot be assigned as a grouping variable, since these
    ## must be unique for each row, which would defeat the purpose of unstacking
    ## a table onto itself.
    ## @item @qcode{'ConstantVariables'} specifies the variables that are
    ## constant within each group.  All valid schemes for indexing a table
    ## variable can be used.  The values for these variables in the output are
    ## taken from the first row in each group in the input.  By default, no
    ## variable is treated as constant unless specified.  However, if the input
    ## table has row names, these effectively are treated as constant variables.
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
    ## forces all variable names to be valid Octave variable names.
    ## @qcode{'preserve'} preserves the original names taken from the input
    ## table, which can have any Unicode characters, including spaces and
    ## non-ASCII characters.
    ## @end itemize
    ##
    ## @code{[@var{tblB}, @var{idxA}] = unstack (@dots{})} also returns an index
    ## vector, @var{idxA}, indicating the correspondence between the rows in
    ## @var{tblB} and the rows in @var{tblA}.
    ##
    ## @end deftypefn
    function [tbl, idxA] = unstack (this, vars, ivar, varargin)
      if (nargin < 3)
        vars = [];
        ivar = [];
      endif
      [tbl, idxA, errmsg] = unstackResult (this, vars, ivar, varargin);
      if (! isempty (errmsg))
        error ("table.unstack: %s", errmsg);
      endif
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
      [tbl, errmsg] = inner2outerResult (this);
      if (! isempty (errmsg))
        error ("table.inner2outer: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{T} =} addprop (@var{T}, @var{propertyNames}, @var{propertyTypes})
    ##
    ## Add custom properties to a table.
    ##
    ## @code{@var{T} = addprop (@var{T}, @var{propertyNames},
    ## @var{propertyTypes})} adds properties that contain custom metadata to the
    ## table @var{T}.  The input argument @var{propertyNames} specifies the
    ## names of the custom properties to be added and @var{propertyTypes} the
    ## type of each corresponding custom property, that is whether the metadata
    ## values contained in the property apply to table @var{T} as a whole, or
    ## to the variables of @var{T}.  Both @var{propertyNames} and
    ## @var{propertyTypes} can be character vectors, cell arrays of character
    ## vectors, or strings.  When defined as cell arrays of character vectors or
    ## strings, they must have the same number of elements.
    ##
    ## Valid @var{propertyTypes} are either @qcode{'table'} or
    ## @qcode{'variable'}.  When defined as @qcode{'table'}, the custom property
    ## can contain any value of any type and size, which applies as metadata to
    ## the table as a whole and is stored exactly as it is given.  When defined
    ## as @qcode{'variable'}, the custom property contains a vector with one
    ## element per variable in the table.
    ##
    ## A @qcode{'variable'} property is cleared by assigning an empty 0-by-0
    ## value, such as @code{[]} or @code{@{@}}, whatever the width of the
    ## table; empty values of any other size are not accepted.  A character
    ## vector is not a valid value for a @qcode{'variable'} property: use a
    ## cell array of character vectors or a string array instead.
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
        this.CustomPropTypes.(Names{idx}) = Types{idx};
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
          this.CustomPropTypes = rmfield (this.CustomPropTypes, ...
                                          existingNames(tf));
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

  methods (Access = public)

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
    ## match.  The key variables of @var{tblR} must contain unique combinations
    ## of values, and every key combination in @var{tblL} must be present in
    ## @var{tblR}.
    ##
    ## By default @var{tbl} contains all the variables of @var{tblL} followed by
    ## the non-key variables of @var{tblR}.  Whenever a non-key variable name
    ## appears in both tables, a suffix derived from each input's argument name
    ## is appended to the conflicting names (for inputs named @var{tblL} and
    ## @var{tblR}, the suffixes @qcode{'_tblL'} and @qcode{'_tblR'}; when an
    ## input has no name, @qcode{'_left'} and @qcode{'_right'} are used).  The
    ## row names of @var{tblL}, if any, are preserved.
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
    ## Variables of @var{tblL} and @var{tblR} to include in @var{tbl}.  By
    ## default @qcode{'LeftVariables'} is all the variables of @var{tblL} and
    ## @qcode{'RightVariables'} is the non-key variables of @var{tblR}.
    ##
    ## @item @qcode{'KeepOneCopy'}
    ## Names of non-key variables that occur in both tables for which only the
    ## copy from @var{tblL} is kept (no suffix is added and the @var{tblR} copy
    ## is dropped).
    ## @end table
    ##
    ## @code{[@var{tbl}, @var{ixR}] = join (@dots{})} also returns the index
    ## vector @var{ixR} that identifies, for each row of @var{tbl}, the matching
    ## row of @var{tblR}.
    ##
    ## @end deftypefn
    function [tbl, ixR] = join (tblL, tblR, varargin)
      if (nargin < 2)
        error ("table.join: too few input arguments.");
      endif
      ## The caller's own names for the operands are read here, before
      ## the shared body, which cannot see them.
      [tbl, ixR, errmsg] = joinResult (tblL, tblR, varargin, ...
                                       inputname (1), inputname (2));
      if (! isempty (errmsg))
        error ("table.join: %s", errmsg);
      endif
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
    ## variables}, which by default are the variables that share the same name
    ## in both tables.  Each row of @var{tbl} is formed by horizontally
    ## concatenating a row of @var{tblL} with a row of @var{tblR} whose key
    ## variables share the same combination of values.  If @math{m} rows in
    ## @var{tblL} and @math{n} rows in @var{tblR} share the same key
    ## combination, then @var{tbl} contains all @math{m*n} pairings for that
    ## combination.  The rows of @var{tbl} are sorted by the values of the key
    ## variables, and any row names are dropped.
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
    ## @code{[@var{tbl}, @var{ixL}, @var{ixR}] = innerjoin (@dots{})} also
    ## returns the row-index vectors @var{ixL} and @var{ixR} such that @var{tbl}
    ## is the horizontal concatenation of @code{@var{tblL}(@var{ixL}, leftVars)}
    ## and @code{@var{tblR}(@var{ixR}, rightVars)}.
    ##
    ## @end deftypefn
    function [tbl, ixL, ixR] = innerjoin (tblL, tblR, varargin)
      if (nargin < 2)
        error ("table.innerjoin: too few input arguments.");
      endif
      [tbl, ixL, ixR, errmsg] = innerjoinResult (tblL, tblR, ...
                                    varargin, inputname (1), ...
                                    inputname (2));
      if (! isempty (errmsg))
        error ("table.innerjoin: %s", errmsg);
      endif
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
    ## variables}, which by default are the variables that share the same name
    ## in both tables.  Unlike @code{innerjoin}, an outer join also keeps the
    ## rows of each table that have no match in the other table, filling the
    ## variables taken from the non-matching table with missing values
    ## (@qcode{NaN}, @qcode{NaT}, @qcode{<undefined>}, empty string, etc., as
    ## appropriate).  If @math{m} rows in @var{tblL} and @math{n} rows in
    ## @var{tblR} share the same key combination, then @var{tbl} contains all
    ## @math{m*n} pairings for that combination.  The rows of @var{tbl} are
    ## sorted by the values of the key variables and any row names are dropped.
    ##
    ## By default @var{tbl} contains all the variables of @var{tblL} followed
    ## by all the variables of @var{tblR}.  Because the key variables are kept
    ## from both tables, conflicting names receive a suffix derived from each
    ## input's argument name (for inputs named @var{tblL} and @var{tblR}, the
    ## suffixes @qcode{'_tblL'} and @qcode{'_tblR'}; when an input has no name,
    ## @qcode{'_left'} and @qcode{'_right'} are used).  See @qcode{'MergeKeys'}
    ## to combine the keys into single columns instead.
    ##
    ## @code{@var{tbl} = outerjoin (@var{tblL}, @var{tblR}, @var{Name},
    ## @var{Value})} customizes the join with the following options:
    ##
    ## @table @asis
    ## @item @qcode{'Type'}
    ## The type of outer join: @qcode{'full'} (default) keeps unmatched rows
    ## from both tables, @qcode{'left'} keeps all rows of @var{tblL} and only
    ## matching rows of @var{tblR}, and @qcode{'right'} keeps all rows of
    ## @var{tblR} and only matching rows of @var{tblL}.
    ##
    ## @item @qcode{'MergeKeys'}
    ## A logical scalar (default @qcode{false}).  When @qcode{true}, each pair
    ## of key variables is merged into a single variable that takes the value
    ## from @var{tblL} where a matching left row exists and from @var{tblR}
    ## otherwise.  The merged variable is named after the left key when both
    ## keys share the same name, or @qcode{'leftName_rightName'} when their
    ## names differ.
    ##
    ## @item @qcode{'Keys'}
    ## Variables to use as keys in both tables, given as variable names or
    ## indices.  It cannot be combined with @qcode{'LeftKeys'} or
    ## @qcode{'RightKeys'}.
    ##
    ## @item @qcode{'LeftKeys'}, @qcode{'RightKeys'}
    ## Variables to use as keys in @var{tblL} and @var{tblR}, respectively,
    ## when the key variables have different names.  They must be specified
    ## together and reference the same number of variables.
    ##
    ## @item @qcode{'LeftVariables'}, @qcode{'RightVariables'}
    ## Variables of @var{tblL} and @var{tblR} to include in @var{tbl}.  By
    ## default all the variables of each table are included.
    ## @end table
    ##
    ## @code{[@var{tbl}, @var{ixL}, @var{ixR}] = outerjoin (@dots{})} also
    ## returns the row-index vectors @var{ixL} and @var{ixR} that identify the
    ## row of @var{tblL} and @var{tblR}, respectively, corresponding to each
    ## row of @var{tbl}.  A zero indicates a row of @var{tbl} that has no
    ## corresponding row in that table.
    ##
    ## @end deftypefn
    function [tbl, ixL, ixR] = outerjoin (tblL, tblR, varargin)
      if (nargin < 2)
        error ("table.outerjoin: too few input arguments.");
      endif
      [tbl, ixL, ixR, errmsg] = outerjoinResult (tblL, tblR, ...
                                    varargin, inputname (1), ...
                                    inputname (2));
      if (! isempty (errmsg))
        error ("table.outerjoin: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} union (@var{tblA}, @var{tblB})
    ## @deftypefnx {table} {@var{tbl} =} union (@var{tblA}, @var{tblB}, @var{setOrder})
    ## @deftypefnx {table} {[@var{tbl}, @var{ixA}, @var{ixB}] =} union (@dots{})
    ##
    ## Union of two tables by rows.
    ##
    ## @code{@var{tbl} = union (@var{tblA}, @var{tblB})} returns the combined
    ## set of rows of @var{tblA} and @var{tblB}, with duplicate rows removed.
    ## Both tables must have the same variable names, although not necessarily
    ## in the same order; @var{tbl} keeps the variable order of @var{tblA}.
    ## Rows are compared by their variable values only (row names are ignored),
    ## and by default @var{tbl} is sorted by those values.
    ##
    ## @code{@var{tbl} = union (@var{tblA}, @var{tblB}, @var{setOrder})}
    ## controls the ordering of @var{tbl}.  @var{setOrder} is either
    ## @qcode{'sorted'} (default) for ascending order, or @qcode{'stable'} to
    ## keep the order in which the rows appear in @var{tblA} and @var{tblB}.
    ##
    ## @code{[@var{tbl}, @var{ixA}, @var{ixB}] = union (@dots{})} also returns
    ## the index vectors @var{ixA} and @var{ixB} such that @var{tbl} is the
    ## vertical concatenation of @code{@var{tblA}(@var{ixA},:)} and
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
    ## rows common to both @var{tblA} and @var{tblB}, with duplicate rows
    ## removed.  Both tables must have the same variable names, although not
    ## necessarily in the same order; @var{tbl} keeps the variable order of
    ## @var{tblA}.  Rows are compared by their variable values only (row names
    ## are ignored), and by default @var{tbl} is sorted by those values.
    ##
    ## @code{@var{tbl} = intersect (@var{tblA}, @var{tblB}, @var{setOrder})}
    ## controls the ordering of @var{tbl}, either @qcode{'sorted'} (default) or
    ## @qcode{'stable'}.
    ##
    ## @code{[@var{tbl}, @var{ixA}, @var{ixB}] = intersect (@dots{})} also
    ## returns index vectors @var{ixA} and @var{ixB} such that @var{tbl} equals
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
    ## @code{@var{tbl} = setdiff (@var{tblA}, @var{tblB})} returns the set of
    ## rows that are present in @var{tblA} but not in @var{tblB}, with duplicate
    ## rows removed.  Both tables must have the same variable names, although
    ## not necessarily in the same order; @var{tbl} keeps the variable order of
    ## @var{tblA}.  Rows are compared by their variable values only (row names
    ## are ignored), and by default @var{tbl} is sorted by those values.
    ##
    ## @code{@var{tbl} = setdiff (@var{tblA}, @var{tblB}, @var{setOrder})}
    ## controls the ordering of @var{tbl}, either @qcode{'sorted'} (default) or
    ## @qcode{'stable'}.
    ##
    ## @code{[@var{tbl}, @var{ixA}] = setdiff (@dots{})} also returns the index
    ## vector @var{ixA} such that @var{tbl} equals
    ## @code{@var{tblA}(@var{ixA},:)}.
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
    ## @code{@var{tbl} = setxor (@var{tblA}, @var{tblB})} returns the set of
    ## rows that are present in either @var{tblA} or @var{tblB} but not in both,
    ## with duplicate rows removed.  Both tables must have the same variable
    ## names, although not necessarily in the same order; @var{tbl} keeps the
    ## variable order of @var{tblA}.  Rows are compared by their variable values
    ## only (row names are ignored), and by default @var{tbl} is sorted by those
    ## values.
    ##
    ## @code{@var{tbl} = setxor (@var{tblA}, @var{tblB}, @var{setOrder})}
    ## controls the ordering of @var{tbl}, either @qcode{'sorted'} (default) or
    ## @qcode{'stable'}.
    ##
    ## @code{[@var{tbl}, @var{ixA}, @var{ixB}] = setxor (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that @var{tbl} is the
    ## vertical concatenation of @code{@var{tblA}(@var{ixA},:)} and
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
    ## @deftypefn  {table} {@var{TF} =} ismissing (@var{tbl})
    ## @deftypefnx {table} {@var{TF} =} ismissing (@var{tbl}, @var{indicator})
    ## @deftypefnx {table} {@var{TF} =} ismissing (@dots{}, @qcode{'OutputFormat'}, @var{outFmt})
    ##
    ## Find missing values in table.
    ##
    ## @code{@var{TF} = ismissing (@var{tbl})} returns a logical array,
    ## @var{TF}, with any @qcode{true} values corresponding to missing elements
    ## in the input table @var{tbl}.
    ##
    ## Missing values are defined according to the data type of each variable in
    ## @var{tbl}:
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
    ## @item @qcode{char} and @qcode{string} indicators match categorical
    ## variables.
    ## @end itemize
    ##
    ## The output array @var{TF} has the same size as the input table @var{tbl}.
    ##
    ## @code{@var{TF} = ismissing (@dots{}, @qcode{'OutputFormat'},
    ## @var{outFmt})} specifies whether @var{TF} is returned as a logical array
    ## or as a table, which maintains the variable names and all other
    ## information of the input table @var{tbl}.  Specifying @var{outFmt} as
    ## @qcode{'logical'} (default) returns a logical array.  Specifying
    ## @var{outFmt} as @qcode{'tabular'} returns a table.
    ##
    ## @end deftypefn
    function TF = ismissing (this, varargin)
      [TF, errmsg] = ismissingResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.ismissing: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} rmmissing (@var{tblA})
    ## @deftypefnx {table} {@var{tbl} =} rmmissing (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {table} {[@var{tbl}, @var{TF}] =} rmmissing (@dots{})
    ##
    ## Remove missing table elements by rows.
    ##
    ## @code{@var{tbl} = rmmissing (@var{tblA})} returns a table with the rows
    ## of @var{tblA} that contain at least one missing value removed.  Missing
    ## values are determined per variable according to its data type
    ## (@code{NaN} for numeric, @code{NaT} for @code{datetime}, @code{<missing>}
    ## for @code{string}, @code{<undefined>} for @code{categorical},
    ## @code{@{''@}} for cellstr, etc.), as reported by @code{ismissing}.
    ##
    ## @code{@var{tbl} = rmmissing (@dots{}, @var{Name}, @var{Value})}
    ## customizes the operation with the following options:
    ##
    ## @table @asis
    ## @item @qcode{'MinNumMissing'}
    ## A positive integer @var{n} (default @code{1}).  A row is removed only
    ## when it has at least @var{n} variables with a missing value.
    ##
    ## @item @qcode{'DataVariables'}
    ## Restrict the search for missing values to the indicated subset of table
    ## variables, using the same variable referencing as the other @code{table}
    ## methods.  Variables outside the subset are not inspected, but all
    ## variables are kept in the output.
    ##
    ## @item @qcode{'MissingLocations'}
    ## Supply the missing-value locations explicitly instead of deriving them
    ## with @code{ismissing}.  The value is either a logical matrix with one row
    ## per row of the input and one column per inspected variable, or a
    ## @code{table} of logical variables whose names and sizes match the
    ## inspected variables.
    ## @end table
    ##
    ## @code{[@var{tbl}, @var{TF}] = rmmissing (@dots{})} also returns a logical
    ## column vector @var{TF}, with one element per row of @var{tblA}, that is
    ## @qcode{true} for each removed row.
    ##
    ## @end deftypefn
    function [tbl, TF] = rmmissing (this, varargin)
      [tbl, TF, errmsg] = rmmissingResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("table.rmmissing: %s", errmsg);
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
    ## Fill by linear interpolation of the neighbouring entries that are not
    ## missing.  Numeric, logical, @code{datetime} and @code{duration}
    ## variables can be interpolated; a targeted variable of any other type
    ## raises an error.
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
    ## Control how leading and trailing missing entries are filled, whatever
    ## the fill method is.  Leading entries are those before the first entry
    ## that is not missing, and trailing entries those after the last.  Valid
    ## values are @qcode{'extrap'} (default), which leaves them to the fill
    ## method itself, @qcode{'none'}, which leaves them missing,
    ## @qcode{'previous'}, @qcode{'next'} and @qcode{'nearest'}, which take
    ## the value of the nearest entry that is not missing on the side they
    ## name and leave the other side missing, or a scalar constant, which must
    ## be assignable to the variable it fills.
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
      [tbl, TF, errmsg] = fillmissingResult (tblA, varargin{:});
      if (! isempty (errmsg))
        error ("table.fillmissing: %s", errmsg);
      endif
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
    ## type (@code{NaN} for @code{double}/@code{single}, @qcode{''} for cell
    ## arrays of character vectors, @code{<missing>} for @code{string}, and
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
    function tbl = standardizeMissing (tblA, varargin)
      [tbl, errmsg] = standardizeMissingResult (tblA, varargin{:});
      if (! isempty (errmsg))
        error ("table.standardizeMissing: %s", errmsg);
      endif
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
    ## positive integer group numbers, with one element for each row of the
    ## table @var{T}.  Each variable of @var{T} acts as a grouping variable, and
    ## the groups are the unique combinations of values across those variables,
    ## sorted in ascending order.  If @var{N} groups are found, every integer
    ## between 1 and @var{N} labels a group.  Rows holding a missing value
    ## (@code{NaN}, @code{NaT}, @code{<missing>}, @code{''}, or
    ## @code{<undefined>}) in any grouping variable are labelled @code{NaN} in
    ## @var{G}.
    ##
    ## @code{[@var{G}, @var{TID}] = findgroups (@var{T})} also returns
    ## @var{TID}, a table whose rows are the sorted unique combinations
    ## identifying each group, with the same variables as @var{T}.
    ##
    ## @end deftypefn
    function [G, TID] = findgroups (this)
      if (nargin != 1)
        print_usage ();
      endif
      [G, TID, errmsg] = findgroupsResult (this);
      if (! isempty (errmsg))
        error ("table.findgroups: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{Y} =} splitapply (@var{func}, @var{T}, @var{G})
    ## @deftypefnx {table} {[@var{Y1}, @dots{}, @var{YM}] =} splitapply (@var{func}, @var{T}, @var{G})
    ##
    ## Split table data into groups and apply a function to each group.
    ##
    ## @code{@var{Y} = splitapply (@var{func}, @var{T}, @var{G})} splits the
    ## rows of the table @var{T} into groups according to the group numbers
    ## @var{G} (typically produced by @code{findgroups}), applies the function
    ## handle @var{func} to each group, and concatenates the per-group results
    ## into the output @var{Y}.  @var{G} must be a column vector of positive
    ## integers with one element per row of @var{T}; if it identifies @var{N}
    ## groups, every integer between 1 and @var{N} must occur at least once.
    ## Rows for which @var{G} is @code{NaN} are omitted.  Each variable of
    ## @var{T} is passed to @var{func} as a separate input argument, so
    ## @var{func} must accept as many arguments as @var{T} has variables.
    ##
    ## @code{[@var{Y1}, @dots{}, @var{YM}] = splitapply (@dots{})} returns the
    ## multiple outputs of @var{func}, each concatenated across groups.
    ##
    ## @end deftypefn
    function varargout = splitapply (func, this, G)
      if (nargin != 3)
        print_usage ();
      endif
      nout = max (nargout, 1);
      [results, N, errmsg] = splitapplyResult (this, func, G, nout);
      if (! isempty (errmsg))
        error ("table.splitapply: %s", errmsg);
      endif
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
      [B, errmsg] = varfunResult (A, func, varargin);
      if (! isempty (errmsg))
        error ("table.varfun: %s", errmsg);
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
      [B, errmsg] = rowfunResult (A, func, varargin);
      if (! isempty (errmsg))
        error ("table.rowfun: %s", errmsg);
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
    ## @code{@var{G} = groupsummary (@var{T}, @var{groupvars})} groups the rows
    ## of the table @var{T} by the grouping variables @var{groupvars} and
    ## returns the table @var{G} with one row per group, holding the grouping
    ## variables and a @qcode{GroupCount} variable counting the rows in each
    ## group.  @var{groupvars} selects the grouping variables by name, index,
    ## logical vector, function handle, or @code{vartype} subscript.
    ##
    ## @code{@var{G} = groupsummary (@var{T}, @var{groupvars}, @var{method})}
    ## also applies @var{method} to each data variable within each group and
    ## appends the results to @var{G}.  @var{method} is one of the method names
    ## below, a function handle, or a cell array of method names and@/or
    ## function handles:
    ##
    ## @table @asis
    ## @item @qcode{'sum'}, @qcode{'mean'}, @qcode{'median'}, @qcode{'mode'}
    ## @itemx @qcode{'var'}, @qcode{'std'}, @qcode{'min'}, @qcode{'max'}
    ## @itemx @qcode{'range'}, @qcode{'nnz'}
    ## Standard statistics, computed over numeric or logical data variables.
    ## @code{NaN} values are omitted (as in MATLAB) for every named method
    ## except @qcode{'nummissing'}.
    ##
    ## @item @qcode{'nummissing'}
    ## The number of missing values in the group, supported for data variables
    ## of any type.
    ##
    ## @item @qcode{'numunique'}
    ## The number of unique non-missing values in the group, supported for data
    ## variables of any type.
    ## @end table
    ##
    ## A function handle is applied to each group's slice of each data variable
    ## and must return a single row (its first dimension must be @code{1}); it
    ## receives the values with @code{NaN} included.
    ##
    ## @code{@var{G} = groupsummary (@var{T}, @var{groupvars}, @var{method},
    ## @var{datavars})} applies @var{method} only to the data variables selected
    ## by @var{datavars} (named, indexed, logical, function handle, or
    ## @code{vartype} subscript).  By default every variable that is not a
    ## grouping variable is a data variable.
    ##
    ## The computed variables of @var{G} are named @code{<method>_<datavar>},
    ## e.g.@: @qcode{mean_X}; results from a function handle are named
    ## @code{fun<n>_<datavar>}, where @var{n} is the position of the handle
    ## among the requested methods.  When several methods are requested the
    ## computed variables are ordered method first, then data variable.
    ##
    ## The optional @var{groupbins} argument bins the grouping variables before
    ## grouping.  A binning scheme is one of: a vector of bin edges; a positive
    ## integer number of equal-width bins spanning the data range; a
    ## @code{duration} scalar giving a fixed bin width (for a datetime or
    ## duration grouping variable); or, for a datetime grouping variable, a
    ## calendar-unit keyword (@qcode{'second'}, @qcode{'minute'},
    ## @qcode{'hour'}, @qcode{'day'}, @qcode{'week'}, @qcode{'month'},
    ## @qcode{'quarter'}, @qcode{'year'}, @qcode{'decade'}, or
    ## @qcode{'century'}) that bins by that calendar period.  A binned grouping
    ## variable becomes a categorical and is renamed @code{disc_<var>} for edge,
    ## bin-count, or width binning, or @code{<unit>_<var>} for calendar-unit
    ## binning.  Pass a cell array with one scheme per grouping variable to bin
    ## them differently, or @qcode{'none'} to leave a variable unbinned.
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
      [G, errmsg] = groupsummaryResult (T, groupvars, varargin);
      if (! isempty (errmsg))
        error ("table.groupsummary: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{G} =} groupcounts (@var{T}, @var{groupvars})
    ## @deftypefnx {table} {@var{G} =} groupcounts (@var{T}, @var{groupvars}, @var{groupbins})
    ## @deftypefnx {table} {@var{G} =} groupcounts (@dots{}, @var{Name}, @var{Value})
    ##
    ## Count the number of rows in each group of a table.
    ##
    ## @code{@var{G} = groupcounts (@var{T}, @var{groupvars})} groups the rows
    ## of the table @var{T} by the grouping variables @var{groupvars} and
    ## returns the table @var{G} with one row per group, holding the grouping
    ## variables, a @qcode{GroupCount} variable counting the rows in each group,
    ## and a @qcode{Percent} variable giving each group's count as a percentage
    ## of the total.  @var{groupvars} selects the grouping variables by name,
    ## index, logical vector, function handle, or @code{vartype} subscript.
    ##
    ## The optional @var{groupbins} argument bins the grouping variables before
    ## grouping, using bin edges, a number of equal-width bins, a
    ## @code{duration} bin width, or a datetime calendar-unit keyword, or a cell
    ## array with one scheme per grouping variable.  A binned grouping variable
    ## becomes a categorical and is renamed @code{disc_<var>} or
    ## @code{<unit>_<var>}.  See @code{groupsummary} for details.
    ##
    ## Groups are the sorted unique combinations of grouping values.  The
    ## following @var{Name}/@var{Value} pairs are accepted:
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
      [G, errmsg] = groupcountsResult (T, groupvars, varargin);
      if (! isempty (errmsg))
        error ("table.groupcounts: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{G} =} groupfilter (@var{T}, @var{groupvars}, @var{method})
    ## @deftypefnx {table} {@var{G} =} groupfilter (@var{T}, @var{groupvars}, @var{groupbins}, @var{method})
    ## @deftypefnx {table} {@var{G} =} groupfilter (@dots{}, @var{method}, @var{datavars})
    ##
    ## Filter the rows of a table by a per-group condition.
    ##
    ## @code{@var{G} = groupfilter (@var{T}, @var{groupvars}, @var{method})}
    ## groups the rows of the table @var{T} by the grouping variables
    ## @var{groupvars}, applies the filter function @var{method} to each group,
    ## and returns the table @var{G} holding the rows that satisfy the
    ## condition, in their original order and with all the variables of
    ## @var{T}.  @var{groupvars} selects the grouping variables by name, index,
    ## logical vector, function handle, or @code{vartype} subscript.
    ##
    ## @var{method} is a function handle applied to each group's slice of every
    ## data variable.  It must return either a logical scalar, which keeps or
    ## drops the whole group, or a logical vector with one element per row of
    ## the group, which keeps or drops the individual rows.  A row is kept only
    ## when the condition holds for it across all data variables.
    ##
    ## @code{@var{G} = groupfilter (@var{T}, @var{groupvars}, @var{method},
    ## @var{datavars})} applies @var{method} only to the data variables selected
    ## by @var{datavars} (named, indexed, logical, function handle, or
    ## @code{vartype} subscript).  By default every variable that is not a
    ## grouping variable is a data variable.
    ##
    ## Rows holding a missing value in a grouping variable form their own
    ## groups, to which @var{method} is applied like any other group.
    ##
    ## The optional @var{groupbins} argument bins the grouping variables before
    ## grouping, using bin edges, a number of equal-width bins, a
    ## @code{duration} bin width, or a datetime calendar-unit keyword, or a cell
    ## array with one scheme per grouping variable; see @code{groupsummary} for
    ## details.  The @qcode{'IncludedEdge'} Name-Value pair (@qcode{'left'} by
    ## default, or @qcode{'right'}) selects which bin edge is inclusive.
    ##
    ## @end deftypefn
    function G = groupfilter (T, groupvars, varargin)
      if (nargin < 3)
        print_usage ();
      endif
      [G, errmsg] = groupfilterResult (T, groupvars, varargin);
      if (! isempty (errmsg))
        error ("table.groupfilter: %s", errmsg);
      endif
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
    ## group, and returns the table @var{G} with the transformed values, one
    ## row per row of @var{T} and in the original order.  @var{groupvars}
    ## selects the grouping variables by name, index, logical vector, function
    ## handle, or @code{vartype} subscript.
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
    ## @var{datavars} (named, indexed, logical, function handle, or
    ## @code{vartype} subscript).  By default every variable that is not a
    ## grouping variable is a data variable.
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
    ## Rows holding a missing value in a grouping variable form their own
    ## groups, which are transformed like any other group.  The optional
    ## @var{groupbins} argument bins the grouping variables before grouping,
    ## using bin edges, a number of equal-width bins, a @code{duration} bin
    ## width, or a datetime calendar-unit keyword, or a cell array with one
    ## scheme per grouping variable; see @code{groupsummary} for details.
    ##
    ## @end deftypefn
    function G = grouptransform (T, groupvars, varargin)
      if (nargin < 3)
        print_usage ();
      endif
      [G, errmsg] = grouptransformResult (T, groupvars, varargin);
      if (! isempty (errmsg))
        error ("table.grouptransform: %s", errmsg);
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
    ## @var{rowvars})} reshapes the table @var{T} into the pivoted table
    ## @var{P}.  The unique combinations of the grouping variables @var{colvars}
    ## become the variables (columns) of @var{P}, the unique combinations of the
    ## grouping variables @var{rowvars} become its rows, and each cell holds one
    ## statistic computed over the rows of @var{T} that fall into that
    ## row-and-column group.  At least one of @qcode{'Columns'} or
    ## @qcode{'Rows'} is required; an omitted dimension collapses to a single
    ## group.  Each of @var{colvars} and @var{rowvars} selects variables by
    ## name, index, or logical vector, and may name several variables.
    ##
    ## Groups are the sorted unique combinations of the grouping values, with
    ## the first variable varying slowest; a categorical variable groups by its
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
    ## grouping variables before pivoting: a vector of bin edges, a number of
    ## equal-width bins, a @code{duration} bin width, or a datetime
    ## calendar-unit keyword (see @code{groupsummary}), or a cell array with one
    ## scheme per variable.  Each binned variable becomes a categorical.  The
    ## default @qcode{'none'} applies no binning.
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Either @qcode{'left'} (the default) or @qcode{'right'}, selecting which
    ## edge of each bin is inclusive when a binning scheme is given.
    ##
    ## @item @qcode{'OutputFormat'}
    ## @qcode{'flat'} (default) names each output column after the joined column
    ## grouping values (@qcode{@var{lvl}_@var{lvl}}).  @qcode{'nested'} instead
    ## groups two or more @qcode{'Columns'} variables into nested tables: one
    ## outer variable per level of the first column grouping variable, each a
    ## nested @code{table} whose variables are the next grouping variable's
    ## levels (recursively).  A marginal-total column, if any, stays a flat
    ## outer variable.
    ## @end table
    ##
    ## @end deftypefn
    function P = pivot (T, varargin)
      [P, errmsg] = pivotResult (T, varargin);
      if (! isempty (errmsg))
        error ("table.pivot: %s", errmsg);
      endif
    endfunction

  endmethods

################################################################################
##                       **    Auxiliary Methods    **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'cat'              'horzcat'          'iscolumn'         'isempty'         ##
## 'isequal'          'isequaln'         'ismatrix'         'isrow'           ##
## 'isscalar'         'istable'          'isvector'         'length'          ##
## 'ndims'            'numel'            'repelem'          'repmat'          ##
## 'size'             'squeeze'          'vertcat'                            ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {table} {@var{tbl} =} cat (@var{dim}, @var{tbl1}, @var{tbl2}, @dots{})
    ##
    ## Concatenate tables along the given dimension.
    ##
    ## @code{@var{tbl} = cat (@var{dim}, @var{tbl1}, @var{tbl2}, @dots{})}
    ## concatenates the input tables along dimension @var{dim}, which must be
    ## either 1 or 2, since a table always has exactly two dimensions.
    ##
    ## @code{cat (1, @dots{})} concatenates vertically and is equivalent to
    ## @code{vertcat}, whereas @code{cat (2, @dots{})} concatenates
    ## horizontally and is equivalent to @code{horzcat}.  The same
    ## requirements on variable names, row names and size apply, and a
    ## @qcode{0x0} operand that is not a character array takes no part in the
    ## concatenation.
    ##
    ## @end deftypefn
    function tbl = cat (dim, varargin)
      if (nargin < 1)
        print_usage ();
      endif
      if (! (isnumeric (dim) && isscalar (dim) && any (dim == [1, 2])))
        error ("table.cat: DIM must be 1 or 2 for a 2-D table.");
      endif
      if (dim == 1)
        tbl = vertcat (varargin{:});
      else
        tbl = horzcat (varargin{:});
      endif
    endfunction

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
      ## Null operands take no part in concatenation
      varargin = tabular.drop_null_operands (varargin);
      if (isempty (varargin))
        tbl = table ();
        return;
      elseif (numel (varargin) == 1 && istable (varargin{1}))
        tbl = varargin{1};
        return;
      endif
      ## All inputs must be tables
      are_tables = cellfun (@istable, varargin);
      if (! all (are_tables))
        error ("table.horzcat: all inputs must be tables.");
      endif
      ## All tables must have unique variable names
      varNames = cellfun (@(obj) obj.VariableNames, varargin, ...
                          'UniformOutput', false);
      varNames = [varNames{:}];
      if (numel (varNames) != numel (unique (varNames)))
        error (strcat ("table.horzcat: all input tables must have unique", ...
                       " variable names."));
      endif
      ## All tables must have the same rows (height).  A table with rows but
      ## no variables counts: it knows its height, and a 0-by-0 one is gone
      ## already, having been dropped as a null operand.
      numRows = cellfun (@height, varargin);
      if (numel (unique (numRows)) != 1)
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
          nA = numel (tbl.VariableValues);
          nB = numel (in.VariableValues);
          tbl.VariableContinuity = tabular.merge_continuity ( ...
                       tbl.VariableContinuity, nA, in.VariableContinuity, nB);
          tbl.VariableDescriptions = tabular.merge_meta ( ...
                       tbl.VariableDescriptions, nA, ...
                       in.VariableDescriptions, nB);
          tbl.VariableUnits = tabular.merge_meta (tbl.VariableUnits, nA, ...
                                                  in.VariableUnits, nB);
          tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
        endfor
      elseif (sum (has_RowNames) == 1) # only one input table has RowNames (ok)
        tbl = varargin{1};
        tbl.VariableNames = varNames;
        for i = 2:numel (varargin)
          in = varargin{i};
          nA = numel (tbl.VariableValues);
          nB = numel (in.VariableValues);
          tbl.VariableContinuity = tabular.merge_continuity ( ...
                       tbl.VariableContinuity, nA, in.VariableContinuity, nB);
          tbl.VariableDescriptions = tabular.merge_meta ( ...
                       tbl.VariableDescriptions, nA, ...
                       in.VariableDescriptions, nB);
          tbl.VariableUnits = tabular.merge_meta (tbl.VariableUnits, nA, ...
                                                  in.VariableUnits, nB);
          tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
          if (! isempty (in.RowNames))
            tbl.RowNames = in.RowNames;
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
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
            nA = numel (tbl.VariableValues);
            nB = numel (in.VariableValues);
            tbl.VariableContinuity = tabular.merge_continuity ( ...
                       tbl.VariableContinuity, nA, in.VariableContinuity, nB);
            tbl.VariableDescriptions = tabular.merge_meta ( ...
                       tbl.VariableDescriptions, nA, ...
                       in.VariableDescriptions, nB);
            tbl.VariableUnits = tabular.merge_meta (tbl.VariableUnits, nA, ...
                                                    in.VariableUnits, nB);
            tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
          else
            nA = numel (tbl.VariableValues);
            nB = numel (in.VariableValues);
            tbl.VariableContinuity = tabular.merge_continuity ( ...
                       tbl.VariableContinuity, nA, in.VariableContinuity, nB);
            tbl.VariableDescriptions = tabular.merge_meta ( ...
                       tbl.VariableDescriptions, nA, ...
                       in.VariableDescriptions, nB);
            tbl.VariableUnits = tabular.merge_meta (tbl.VariableUnits, nA, ...
                                                    in.VariableUnits, nB);
            tbl.VariableValues = [tbl.VariableValues, in.VariableValues];
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
        endfor
      endif

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, 'UniformOutput', false);
      tbl.VariableTypes = new_types;

      ## Merge custom properties across all inputs: table-scoped properties are
      ## unioned (the first input wins on a name clash) and variable-scoped
      ## properties are concatenated across the inputs' variable blocks, NaN-
      ## filling the block of any input that lacks the property.
      [cp, cpTypes] = merge_hcat_props (tbl, varargin);
      tbl.CustomProperties = cp;
      tbl.CustomPropTypes = cpTypes;
      tbl = setRowCount (tbl, height (tbl));
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
    ## @deftypefn  {table} {@var{TF} =} isequal (@var{A}, @var{B})
    ## @deftypefnx {table} {@var{TF} =} isequal (@var{A}, @var{B}, @dots{})
    ##
    ## Test tables for equality.
    ##
    ## @code{@var{TF} = isequal (@var{A}, @var{B})} returns a logical scalar
    ## @var{TF}, which is @qcode{true} when the tables @var{A} and @var{B} are
    ## the same size, carry the same variable names, row names and metadata,
    ## and each pair of corresponding variables holds equal values, and
    ## @qcode{false} otherwise.
    ##
    ## Variables are compared by value and not by class, exactly as
    ## @code{isequal} compares arrays elsewhere, so a table holding
    ## @code{int8 ([1; 2])} equals one holding @code{[1; 2]}.  The
    ## @qcode{VariableTypes} property, which only restates those classes,
    ## takes no part in the comparison.  Every other property does: two
    ## tables differing only in @qcode{Description}, @qcode{UserData},
    ## @qcode{VariableUnits}, @qcode{VariableDescriptions} or a custom
    ## property are not equal.
    ##
    ## As with @qcode{NaN}, missing values are never equal, so a missing
    ## element anywhere in either table makes the result @qcode{false}; use
    ## @code{isequaln} to treat missing values as equal.
    ##
    ## Further tables may be supplied, as in @code{isequal (@var{A}, @var{B},
    ## @var{C}, @dots{})}, in which case @var{TF} is @qcode{true} only when
    ## all of them are equal to one another.  Any argument that is not a
    ## table, a timetable included, makes the result @qcode{false} rather
    ## than raising an error.
    ##
    ## @end deftypefn
    function TF = isequal (varargin)
      if (nargin < 2)
        print_usage ();
      endif
      TF = false;
      if (all (cellfun (@(x) isa (x, 'table'), varargin)))
        TF = isequalResult (varargin{1}, varargin(2:end), false);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{TF} =} isequaln (@var{A}, @var{B})
    ## @deftypefnx {table} {@var{TF} =} isequaln (@var{A}, @var{B}, @dots{})
    ##
    ## Test tables for equality, treating missing values as equal.
    ##
    ## @code{@var{TF} = isequaln (@var{A}, @var{B})} is identical to
    ## @code{isequal (@var{A}, @var{B})} except that missing values are
    ## treated as equal to one another, in the same way that @code{isequaln}
    ## treats @qcode{NaN}.  It returns a logical scalar @var{TF}, which is
    ## @qcode{true} when the tables are the same size, carry the same
    ## variable names, row names and metadata, and each pair of corresponding
    ## elements is either equal or missing in both, and @qcode{false}
    ## otherwise.
    ##
    ## Further tables may be supplied, as in @code{isequaln (@var{A},
    ## @var{B}, @var{C}, @dots{})}, in which case @var{TF} is @qcode{true}
    ## only when all of them are equal to one another.  Any argument that is
    ## not a table, a timetable included, makes the result @qcode{false}
    ## rather than raising an error.
    ##
    ## @end deftypefn
    function TF = isequaln (varargin)
      if (nargin < 2)
        print_usage ();
      endif
      TF = false;
      if (all (cellfun (@(x) isa (x, 'table'), varargin)))
        TF = isequalResult (varargin{1}, varargin(2:end), true);
      endif
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
    ## @deftypefn {table} {@var{TF} =} isscalar (@var{tbl})
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
    ## Return @qcode{true} if input is a table.
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
    ## @deftypefn {table} {@var{tblB} =} repelem (@var{tblA}, @var{rows}, @var{columns})
    ##
    ## Replicate elements of a table.
    ##
    ## Replicates each row of the input table @var{tblA} @var{rows} times and
    ## each variable @var{columns} times, keeping the repeats of a row
    ## together, in a similar fashion to how @code{repelem} applies to a
    ## matrix.  Each repeated variable takes a numbered name, @qcode{x}
    ## becoming @qcode{x}, @qcode{x_1}.
    ##
    ## Both counts must be given.  A table has exactly two dimensions, so a
    ## lone count is not read as applying to both, as it is for a matrix.
    ##
    ## @end deftypefn
    function tbl = repelem (this, varargin)
      [tbl, errmsg] = repeatResult (this, varargin, true);
      if (! isempty (errmsg))
        error ("table.repelem: %s", errmsg);
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
      [tbl, errmsg] = repeatResult (this, varargin, false);
      if (! isempty (errmsg))
        error ("table.repmat: %s", errmsg);
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
    ## positions of the variable names are matched to those of the first input
    ## table.
    ##
    ## When any input table has row names, they must be unique across all input
    ## tables.  In such a case, rows coming from input tables without row names
    ## are assigned default @qcode{Row@var{N}} names, where @var{N} is the row's
    ## position in the output table.  Output table's @qcode{Description} and
    ## @qcode{UserData} properties are assigned using the first non-empty value.
    ##
    ## @end deftypefn
    function tbl = vertcat (varargin)
      ## Null operands take no part in concatenation
      varargin = tabular.drop_null_operands (varargin);
      if (isempty (varargin))
        tbl = table ();
        return;
      elseif (numel (varargin) == 1 && istable (varargin{1}))
        tbl = varargin{1};
        return;
      endif
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
      if (numel (sortedVarNames) > 1 && ! isequal (sortedVarNames{:}))
        error (strcat ("table.vertcat: input tables must have identical", ...
                       " variable names."));
      endif
      ## Identical variable names force identical widths, the sorted lists
      ## just compared being the same length as the widths they come from.
      numCols = width (varargin{1});
      ## With no variables to stack, nothing else carries the row count.
      totalRows = sum (cellfun (@height, varargin));
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
          if (isempty (tbl.VariableContinuity))
            tbl.VariableContinuity = in.VariableContinuity;
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
          ## Rows stack over identical variables, so the first table's custom
          ## properties already describe the result; adopt a later table's only
          ## when the first table has none.
          if (isempty (tbl.CustomProperties) && ! isempty (in.CustomProperties))
            tbl.CustomProperties = in.CustomProperties;
            tbl.CustomPropTypes = in.CustomPropTypes;
          endif
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
          if (isempty (tbl.VariableContinuity))
            tbl.VariableContinuity = in.VariableContinuity;
          endif
          if (isempty (tbl.Description))
            tbl.Description = in.Description;
          endif
          if (isempty (tbl.UserData))
            tbl.UserData = in.UserData;
          endif
          ## As above: adopt a later table's custom properties only when the
          ## first table has none (rows stack over identical variables).
          if (isempty (tbl.CustomProperties) && ! isempty (in.CustomProperties))
            tbl.CustomProperties = in.CustomProperties;
            tbl.CustomPropTypes = in.CustomPropTypes;
          endif
        endfor
      endif
      if (numCols == 0)
        tbl.RowCount = totalRows;
      endif
    endfunction

  endmethods

  methods (Static)

    ## -*- texinfo -*-
    ## @deftypefn  {table} {@var{tbl} =} table.empty ()
    ## @deftypefnx {table} {@var{tbl} =} table.empty (@var{n})
    ## @deftypefnx {table} {@var{tbl} =} table.empty (@var{r}, @var{v})
    ## @deftypefnx {table} {@var{tbl} =} table.empty (@var{sz})
    ##
    ## Create an empty table.
    ##
    ## @code{@var{tbl} = table.empty ()} returns a 0-by-0 table.
    ##
    ## @code{@var{tbl} = table.empty (@var{r}, @var{v})} returns a table with
    ## @var{r} rows and @var{v} variables, at least one of which must be
    ## zero.  A table with rows but no variables keeps its height, so
    ## @code{table.empty (5, 0)} answers 5 to @code{height}.  A table with
    ## variables but no rows names them @qcode{Var1} to @qcode{VarN} and
    ## gives each of them a @qcode{double} value, exactly as
    ## @code{table (@qcode{'Size'}, @dots{})} does.
    ##
    ## @code{@var{tbl} = table.empty (@var{sz})} takes the two dimensions
    ## from the two-element vector @var{sz}, and @code{table.empty (@var{n})}
    ## is the same as @code{table.empty (@var{n}, @var{n})}.
    ##
    ## @seealso{table, isempty, height, width}
    ## @end deftypefn
    function tbl = empty (varargin)
      [sz, errmsg] = tabular.emptySize ('table', varargin);
      if (! isempty (errmsg))
        error ('table.empty: %s', errmsg);
      endif
      tbl = table ('Size', sz, 'VariableTypes', ...
                   repmat ({'double'}, 1, sz(2)));
    endfunction

  endmethods

endclassdef

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
    if (isfield (cp, 'ActualSheetName') && ! isempty (cp.ActualSheetName))
      fsheet = cp.ActualSheetName;
    endif
    if (strcmp (fsheet, sheet))
      targetField = fields{i};
      break;
    endif
  endfor
  ## Select mode
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
  if (isfield (cp, 'ActualSheetName') && ! isempty (cp.ActualSheetName))
    tcp = T.Properties.CustomProperties;
    if (! isfield (tcp, 'ActualSheetName'))
      T = addprop (T, 'ActualSheetName', 'table');
    endif
    T.Properties.CustomProperties.ActualSheetName = cp.ActualSheetName;
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

