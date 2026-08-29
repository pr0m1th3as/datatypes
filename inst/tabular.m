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
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftp {datatypes} {} tabular
##
## Abstract superclass of @code{table} and @code{timetable}.
##
## @code{tabular} holds everything the two tabular classes share: their
## common metadata properties, their variable storage, and the machinery
## that does not depend on how rows are labelled.  It is abstract and cannot
## be instantiated; @code{isa (@var{obj}, "tabular")} is @code{true} for both
## @code{table} and @code{timetable}, and neither derives from the other.
##
## Everything that does depend on row labels is reached through a small set
## of hooks that each subclass implements: @code{table} labels its rows with
## @code{RowNames} and @code{timetable} with @code{RowTimes}.
##
## @end deftp
classdef (Abstract) tabular

  properties

    ## -*- texinfo -*-
    ## @deftp {tabular} {property} Description
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
    ## @deftp {tabular} {property} UserData
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
    ## @deftp {tabular} {property} DimensionNames
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
    ## @deftp {tabular} {property} VariableNames
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
    ## variable name does not exist, a new one is created.
    ##
    ## @end deftp
    VariableNames = {}

    ## -*- texinfo -*-
    ## @deftp {tabular} {property} VariableTypes
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
    ## @deftp {tabular} {property} VariableDescriptions
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
    ## @deftp {tabular} {property} VariableUnits
    ##
    ## Variable units
    ##
    ## Variable units, specified as a cell array of character vectors or a
    ## string array.  If specified as a string array, it is converted and stored
    ## internally as a cell array of character vectors.  If not empty (default),
    ## it must contain the same number of elements as the number of variables.
    ## If a specific variable does not have a unit, this can be specified with
    ## an individual empty character vector or an empty string.  You can access
    ## the @qcode{VariableUnits} property of a table @var{tbl} with
    ## @qcode{@var{tbl}.Properties.VariableUnits}.  You can further index
    ## specific variables to access their unit.
    ##
    ## @end deftp
    VariableUnits = {}

    ## -*- texinfo -*-
    ## @deftp {tabular} {property} VariableContinuity
    ##
    ## Variable continuity
    ##
    ## Continuity of each variable, specified as a cell array of character
    ## vectors or a string array carrying one element per variable, each of
    ## them @qcode{'unset'}, @qcode{'continuous'}, @qcode{'step'} or
    ## @qcode{'event'}.  It is empty by default, and assigning @code{@{@}} or
    ## @code{[]} clears it.  If specified as a string array, it is converted
    ## and stored internally as a cell array of character vectors.  You can
    ## access it with @qcode{@var{tbl}.Properties.VariableContinuity} and you
    ## can index individual variables to read or assign their continuity.
    ##
    ## A @code{table} carries the property but does not act on it, which is
    ## also how MATLAB behaves.  A @code{timetable} uses it to choose the
    ## default fill method of each variable when resampling.
    ##
    ## MATLAB stores this property as a @qcode{matlab.tabular.Continuity}
    ## enumeration.  Octave has no enumeration classes, so it is stored and
    ## returned here as a cell array of character vectors, as
    ## @qcode{VariableNames} and @qcode{VariableUnits} are.
    ##
    ## @end deftp
    VariableContinuity = []

    ## -*- texinfo -*-
    ## @deftp {tabular} {property} CustomProperties
    ##
    ## Customized metadata of table and its variables
    ##
    ## Custom properties that contain metadata of a table and its variables.
    ## By default, this is an empty container.  Each custom property holds
    ## either table metadata or per-variable metadata, according to the property
    ## type (@qcode{'table'} or @qcode{'variable'}) specified when the property
    ## is created with the @code{addprop} method.  A variable-scoped property
    ## holds one element per variable.
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

  properties (Access = protected)
    CustomPropTypes = {}
    VariableValues = {}
  endproperties

################################################################################
##                         **    Subclass hooks    **                         ##
################################################################################
##                                                                            ##
## Every subclass must implement all eight.  Octave's classdef has no         ##
## 'methods (Abstract)' block, so the contract cannot be declared; these      ##
## raising defaults stand in for it, and name the subclass that is missing    ##
## one because 'class (this)' resolves downwards.                             ##
##                                                                            ##
## Seven of them concern row labels, which is the whole of what separates     ##
## one tabular class from another; the eighth names the properties object.    ##
##                                                                            ##
## 'hasRowLabels'      whether the object carries row labels at all           ##
## 'getRowLabels'      the labels themselves, in their own type               ##
## 'rowLabelName'      the name the labels are known by                       ##
## 'rowLabelStrings'   the labels rendered for display                        ##
## 'subsetRowLabels'   the object with its labels subset by an index          ##
## 'clearRowLabels'    the object with its labels removed                     ##
## 'resolveRowRef'     a row reference resolved to row indices                ##
## 'makeProperties'    the properties object this class's metadata lives in   ##
##                                                                            ##
################################################################################

  methods (Access = protected)

    ## Whether this object carries row labels at all.  A 'table' answers false
    ## whenever it has no 'RowNames', which is the common case; a class whose
    ## labels are mandatory answers true always.
    function tf = hasRowLabels (this)
      error ("%s: subclass must implement hasRowLabels.", class (this));
    endfunction

    ## The labels themselves, in their own type: a cellstr for a 'table', a
    ## datetime or duration for a class that labels its rows by time.  Callers
    ## that need text ask 'rowLabelStrings' instead.
    function out = getRowLabels (this)
      error ("%s: subclass must implement getRowLabels.", class (this));
    endfunction

    ## The name the labels are known by, 'RowNames' for a 'table'.  Used
    ## wherever they have to be named rather than shown: the properties
    ## listing, the summary, and the header of an exported file.
    function out = rowLabelName (this)
      error ("%s: subclass must implement rowLabelName.", class (this));
    endfunction

    ## The labels rendered for display, as a column cellstr.  Separate from
    ## 'getRowLabels' because labels that are not already text must go through
    ## their own format before anything can print or export them.
    function out = rowLabelStrings (this)
      error ("%s: subclass must implement rowLabelStrings.", class (this));
    endfunction

    ## This object with its labels subset by IXROWS, the same index the caller
    ## has just applied to the variables.  A class whose labels are optional
    ## leaves them alone when there are none to subset.
    function this = subsetRowLabels (this, ixRows)
      error ("%s: subclass must implement subsetRowLabels.", class (this));
    endfunction

    ## This object with its labels removed.  Used where a result cannot carry
    ## them, as in the side of an outer join whose unmatched rows have no
    ## label to inherit.
    function this = clearRowLabels (this)
      error ("%s: subclass must implement clearRowLabels.", class (this));
    endfunction

    ## A row reference resolved to row indices.  ROWREF is a cellstr of label
    ## names for a 'table'; a class that labels rows by time resolves a time.
    ## Raises when the object carries no labels to match against.
    function ixRows = resolveRowRef (this, rowRef)
      error ("%s: subclass must implement resolveRowRef.", class (this));
    endfunction

    ## The properties object this class's metadata lives in, which is what
    ## 'tbl.Properties' returns.  Not a row label hook: it is here because it
    ## is the other thing that differs per subclass, each class having its own
    ## subclass of 'datatypes.tabular.TabularProperties'.
    function out = makeProperties (this)
      error ("%s: subclass must implement makeProperties.", class (this));
    endfunction

  endmethods

################################################################################
##                     **    Display and reference    **                      ##
################################################################################
##                                                                            ##
## The display path and the two reference overloads.  'subsasgn' is not       ##
## here: it is row-label aware throughout and stays with each subclass.       ##
##                                                                            ##
################################################################################

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
          pair = tabular.mixed_cell_pair (tbl.VariableValues);
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
            tbl = makeProperties (this);
          elseif (isequal (s.subs, this.DimensionNames{1}))
            tbl = getRowLabels (this);
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

  endmethods

################################################################################
##                **    Forbidden methods and flatteners    **                ##
################################################################################
##                                                                            ##
## The shape shims, which no tabular class supports, and the two              ##
## flatteners the spreadsheet exporters share.                                ##
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

    ## Shared helper for the house-format ODS exporters ('table2ods' and the
    ## standalone 'struct2ods').  Hidden rather than private so 'struct2ods'
    ## can reuse the exact flattening + metadata assembly.
    ##
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
        error (strcat ("%s: nested tables and structs are not supported;", ...
                       " flatten them before writing."), caller);
      endif
      Ccols = size (V, 2);
      vtype = cell (1, Ccols);
      for c = 1:Ccols
        vtype{c} = tabular.ods_value_type (T{c});
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
      if (any (cellfun (@(x) ! isempty (x), D(isvar))))
        Dmaxr = max (Drows(isvar));
      else
        Dmaxr = 0;
      endif
      Urows = cellfun (@(x) size (x, 1), U);
      if (any (cellfun (@(x) ! isempty (x), U(isvar))))
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
          Header{1,c} = rowLabelName (this);
        endif
      endfor
      cmt = repmat ({''}, 1, Ccols);
      cmt{1} = sprintf (txt, Tmaxr, Nmaxr, Dmaxr, Umaxr);
      meta = [cmt; Header];
    endfunction

    ## Build the MATLAB-interop spreadsheet parts for THIS table: the variable
    ## names (a header row), the flat data grid V with ISO-formatted
    ## datetime/duration values, and the per-column ODS value types.  No hidden
    ## metadata (interop format).  Shared by 'writetable' and 'struct2xlsx'.
    ## CALLER names the function for error reporting.
    function [names, V, vtype] = __interop_parts__ (this, caller)
      [V, N, T] = table2cellarrays (this, 'iso');
      if (any (cellfun (@iscell, T)))
        error (strcat ("%s: nested tables and structs are not supported;", ...
                       " flatten multicolumn variables with splitvars", ...
                       " before writing."), caller);
      endif
      [names, V, T] = tabular.writetable_prep (V, N, T, false);
      vtype = cell (1, numel (T));
      for c = 1:numel (T)
        vtype{c} = tabular.ods_value_type (T{c});
      endfor
    endfunction

  endmethods

################################################################################
##               **    Reference and assignment internals    **               ##
################################################################################
##                                                                            ##
## Private in 'table' before the split.  They are protected here because      ##
## private access does not reach a subclass, and every one of them reaches    ##
## the row labels through the hooks rather than directly.                     ##
##                                                                            ##
################################################################################

  methods (Access = protected)

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
        ixRow = resolveRowRef (this, rowRef);
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
      tbl = subsetRowLabels (tbl, ixRows);
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
        [pa, pb, e] = tabular.key_col_proxy (tblA.VariableValues{ix}, ...
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
      out = clearRowLabels (out);
    endfunction

    ## Merge the custom properties of a set of horizontally-combined tables
    ## (a cell array TABLES whose variables are concatenated in order).  Table-
    ## scoped properties are unioned with the first table winning on a name
    ## clash; variable-scoped properties are concatenated across the tables'
    ## variable blocks, filling the block of any table lacking the property with
    ## NaN (numeric) or an empty cell.  Table-scoped properties are listed
    ## before variable-scoped ones, matching MATLAB.
    function [cp, cpTypes] = merge_hcat_props (this, tables)
      widths = cellfun (@width, tables);
      cp = struct ();
      cpTypes = {};
      ## Pass 1: table-scoped properties (union, first table wins).
      for t = 1:numel (tables)
        T = tables{t};
        if (isempty (T.CustomProperties))
          continue;
        endif
        nm = fieldnames (T.CustomProperties);
        for i = 1:numel (nm)
          if (strcmp (T.CustomPropTypes{i}, 'table') && ! isfield (cp, nm{i}))
            cp.(nm{i}) = T.CustomProperties.(nm{i});
            cpTypes{end+1} = 'table';
          endif
        endfor
      endfor
      ## Pass 2: variable-scoped properties (union of names; per-table blocks
      ## concatenated, missing blocks filled to match the property's variables).
      seen = {};
      for t = 1:numel (tables)
        T = tables{t};
        if (isempty (T.CustomProperties))
          continue;
        endif
        nm = fieldnames (T.CustomProperties);
        for i = 1:numel (nm)
          if (! strcmp (T.CustomPropTypes{i}, 'variable') ...
              || any (strcmp (nm{i}, seen)))
            continue;
          endif
          seen{end+1} = nm{i};
          proto = T.CustomProperties.(nm{i});
          vec = [];
          for tt = 1:numel (tables)
            Tt = tables{tt};
            if (! isempty (Tt.CustomProperties) ...
                && isfield (Tt.CustomProperties, nm{i}))
              blk = reshape (Tt.CustomProperties.(nm{i}), 1, []);
            elseif (iscell (proto))
              blk = cell (1, widths(tt));
            else
              blk = NaN (1, widths(tt));
            endif
            vec = [vec, blk];
          endfor
          cp.(nm{i}) = vec;
          cpTypes{end+1} = 'variable';
        endfor
      endfor
      if (isempty (fieldnames (cp)))
        cp = [];
        cpTypes = {};
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
      if (! isempty (this.VariableContinuity))
        tbl.VariableContinuity = this.VariableContinuity(ixVars);
      endif
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
      out.VariableContinuity = this.VariableContinuity;
      out.(rowLabelName (this)) = getRowLabels (this);
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
        if (! isempty (this.VariableContinuity))
          tbl.VariableContinuity{ix_new_var} = 'unset';
        endif
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
            out = getRowLabels (this);
          elseif isequal (s.subs, this.DimensionNames{2})
            out = this.VariableNames;
          ## Everything else is indexing an existing variable name
          else
            out = getvar (this, s.subs);
          endif
      endswitch
    endfunction

  endmethods

################################################################################
##                       **    Printing internals    **                       ##
################################################################################
##                                                                            ##
## The rendering path: what turns a table's content into text, or into        ##
## the flat cell arrays that both the display and the exporters consume.      ##
##                                                                            ##
################################################################################

  methods (Access = protected)

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
      if (isempty (this.VariableContinuity))
        VC = "[]";
      else
        VC = strjoin (cellfun (@(x) ["'" x "'"], this.VariableContinuity, ...
                               "UniformOutput", false), "  ");
        VC = ["{" VC "}"];
      endif
      if (! hasRowLabels (this))
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
      fprintf ("%+24s: %s\n", rowLabelName (this), RN);
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
      if (hasRowLabels (this))
        rowLabels = rowLabelStrings (this);
        rnLen = max (cellfun (@length, rowLabels)) + 4;
        padPT = sprintf ("%%-%ds", rnLen);
        padfn = @(x) sprintf (padPT, x);
        rowNM = cellfun (padfn, rowLabels, 'UniformOutput', false);
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
        ## Missing
        elseif (isa (data, 'missing'))
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
            T.optLen = [T.optLen, optLen];
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
        if (isempty (this.VariableContinuity))
          s.(varName).Continuity = [];
        else
          s.(varName).Continuity = this.VariableContinuity{v};
        endif
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
          dn = tabular.datetime_to_datenum (val);
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
      endfor
    endfunction

    ## Export table to cell arrays
    function [V, N, T, D, U] = table2cellarrays (this, fmt = 'display')
      V = {};  # variable values
      N = {};  # variable names
      T = {};  # variable types
      D = {};  # variable descriptions
      U = {};  # variable units
      ## Process the row labels
      if (hasRowLabels (this))
        V = [V, rowLabelStrings(this)];
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
          ## Carry a non-empty TimeZone in the type string ('datetime <tz>') so
          ## the house readers can restore a zone-aware datetime.
          tz = var_V.TimeZone;
          if (isempty (tz))
            dttype = 'datetime';
          else
            dttype = ['datetime ', tz];
          endif
          for col = 1:ncols
            if (strcmp (fmt, 'iso'))
              V = [V, datetime2iso(var_V(:,col))];
            else
              V = [V, cellstr(var_V(:,col))];
            endif
            N = [N, this.VariableNames{ix}];
            T = [T, dttype];
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

################################################################################
##                      **    Shared static helpers    **                     ##
################################################################################
##                                                                            ##
## Local functions before the split, called from both this class and its      ##
## subclasses.  They take no tabular object, so ordinary dispatch would never ##
## find them; they are called as 'tabular.<name> (...)'.                      ##
##                                                                            ##
## They are Hidden rather than protected because a local function carries no  ##
## class context, so protected access is refused there, and several of the    ##
## callers are local functions.  Hidden keeps them out of 'methods' just as   ##
## protected would, so they stay off the class pages either way.              ##
##                                                                            ##
################################################################################

  methods (Static, Hidden)

    ## Convert a datetime array to datenum-valued doubles of the same size,
    ## mapping NaT to NaN.  Used by 'summary'.  Core 'datenum' cannot process
    ## the NaN date components of a NaT, so those rows are substituted with a
    ## valid placeholder before conversion and set back to NaN afterwards.
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

    ## Encode two cellstr key columns into consistent integer codes so that
    ## equal strings (across both columns) map to the same code.
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
      kl = tabular.key_kind (lcol);
      kr = tabular.key_kind (rcol);
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
          [lp, rp] = tabular.text_codes (cellstr (lcol), cellstr (rcol));
        case 'datetime'
          lp = tabular.datetime_to_datenum (lcol);
          rp = tabular.datetime_to_datenum (rcol);
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

    ## Detect the cell/non-cell mix of variable values VALS that cannot form a
    ## homogeneous array.  Returns the column indices [LO, HI] (in column order)
    ## of the first cell and first non-cell variable, or [] when VALS are not
    ## such a mix.  Callers emit the incompatibility error under their own
    ## method name.
    function pair = mixed_cell_pair (vals)
      isCellVar = cellfun (@iscell, vals);
      if (any (isCellVar) && ! all (isCellVar))
        pair = sort ([find(isCellVar, 1), find(! isCellVar, 1)]);
      else
        pair = [];
      endif
    endfunction

    ## Map a variable type name to the ODS cell value type used by 'table2ods'.
    ## Numeric types become 'float', logical becomes 'boolean', datetime and
    ## duration map to the native 'date' and 'time' types, and everything else
    ## (text, categorical, calendarDuration, cell) is written as a 'string'.
    function vt = ods_value_type (typestr)
      ## A zone-aware datetime carries its TimeZone in the type
      ## ('datetime <tz>').
      if (strncmp (typestr, 'datetime', 8))
        vt = 'date';
        return;
      endif
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

    ## Prepare the flat value/name/type cell arrays produced by
    ## 'table2cellarrays' for the MATLAB-compatible 'writetable' output: strip
    ## or keep the leading row names column (which carries an empty variable
    ## name) per WRITEROWNAMES, and de-duplicate the shared names of a
    ## multicolumn variable with _1, _2, ... suffixes, matching MATLAB.
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

