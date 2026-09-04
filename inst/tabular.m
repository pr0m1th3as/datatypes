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
    ## You can add an individual custom property only by using the
    ## @code{addprop} method and you can only remove a custom property with the
    ## @code{rmprop} method.  To access existing custom properties use dot name
    ## structure assignment as in
    ## @qcode{@var{tbl}.Properties.CustomProperties.@var{PropertyName}}, where
    ## @var{PropertyName} is the name used with the @code{addprop} method.
    ##
    ## The whole set may also be taken from another table by assigning that
    ## table's @qcode{CustomProperties} to this one, which replaces every
    ## custom property with those of the other table, their types included.  A
    ## variable-scoped property arriving that way must hold one element for
    ## each variable of the receiving table, or be a 0-by-0 empty.  Nothing
    ## else can be assigned there.
    ##
    ## @end deftp
    CustomProperties = []

  endproperties

  properties (Access = protected)
    CustomPropTypes = struct ()
    VariableValues = {}
    ## The row count, which outlives the last variable.  While an object has
    ## variables its first one is authoritative and this is not read, so it
    ## only has to be right at the moment the variables go: every path that
    ## drops one records it first.  A class whose row labels are mandatory
    ## reads its count off them and never reaches this.
    RowCount = 0
  endproperties

################################################################################
##                         **    Subclass hooks    **                         ##
################################################################################
##                                                                            ##
## Every subclass must implement all eleven.  Octave's classdef has no        ##
## 'methods (Abstract)' block, so the contract cannot be declared; these      ##
## raising defaults stand in for it, and name the subclass that is missing    ##
## one because 'class (this)' resolves downwards.                             ##
##                                                                            ##
## Ten of them concern row labels, which is the whole of what separates       ##
## one tabular class from another; the eleventh names the properties object.  ##
##                                                                            ##
## 'hasRowLabels'      whether the object carries row labels at all           ##
## 'getRowLabels'      the labels themselves, in their own type               ##
## 'rowLabelName'      the name the labels are known by                       ##
## 'rowLabelStrings'   the labels rendered for display                        ##
## 'rowLabelHeader'    the heading printed over them, if any                  ##
## 'rowLabelProperties'  the row label metadata, named as it is published     ##
## 'setRowLabelProperty'  one of those properties assigned                    ##
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

    ## The heading printed over the row label column, or empty for a class
    ## whose labels carry none.  A table's row names are headed by nothing at
    ## all, not even the row dimension name; a class whose labels are a
    ## dimension in their own right names them.
    function out = rowLabelHeader (this)
      error ("%s: subclass must implement rowLabelHeader.", class (this));
    endfunction

    ## The names that mean "order by the row labels" rather than by a
    ## variable.  A table answers to 'RowNames' and to its row dimension
    ## name, a timetable to its row dimension name alone.
    function out = rowLabelKeyNames (this)
      error ("%s: subclass must implement rowLabelKeyNames.", class (this));
    endfunction

    ## Whether a bare 'sortrows (obj)' orders by the row labels.  A table
    ## orders by every variable it has, a timetable by its row times.
    function tf = sortsByLabelsByDefault (this)
      error (strcat ("%s: subclass must implement", ...
                     " sortsByLabelsByDefault."), class (this));
    endfunction

    ## Whether the row labels are part of what makes a row distinct.  A
    ## timetable's row times are, a table's row names are not.
    function tf = uniqueIncludesLabels (this)
      error (strcat ("%s: subclass must implement", ...
                     " uniqueIncludesLabels."), class (this));
    endfunction

    ## The row label metadata as a struct, keyed by the names the properties
    ## object publishes it under.  Separate from 'rowLabelName', which names
    ## the labels for a file header and for a timetable is the row dimension
    ## name: a class may publish its labels under a different name than that,
    ## and may publish more than one property describing them.
    function out = rowLabelProperties (this)
      error ("%s: subclass must implement rowLabelProperties.", class (this));
    endfunction

    ## One row label property assigned.  'subsasgn' has already handled the
    ## nine shared properties and every name left over is one of these, so
    ## HANDLED reports whether NAME was recognised and an unrecognised name
    ## is refused by the caller.  CHAIN_S carries any further subscripts.
    function [this, handled] = setRowLabelProperty (this, name, val, chain_s)
      error ("%s: subclass must implement setRowLabelProperty.", class (this));
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
##               **    Display, reference and assignment    **                ##
################################################################################
##                                                                            ##
## The display path, the two reference overloads and the assignment          ##
## overload.  Only the last branch of 'subsasgn' is row-label aware, and      ##
## that one asks the subclass.                                               ##
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

    ## Custom display.  The header names the class rather than saying
    ## 'table', so that a timetable says what it is.
    ##
    ## An object with no rows still has columns to show, so the body is
    ## printed whenever there is a column: any variable will do, and for a
    ## class that heads its row label column, so will a row.  That is what
    ## leaves a table with no variables printing nothing at all while a
    ## timetable with none still shows the times it labels its rows by.
    function disp (this)
      if (isempty (this))
        fprintf ("  %dx%d empty %s\n\n", size (this), class (this));
      else
        fprintf ("  %dx%d %s\n\n", height (this), width (this), class (this));
      endif
      if (width (this) > 0
          || (height (this) > 0 && ! isempty (rowLabelHeader (this))))
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
      clstype = class (this);
      chain_s = s(2:end);
      s = s(1);
      switch (s.type)
        case '()'
          if (numel (s.subs) != 2)
            error (strcat ("%s.subsref: '()' indexing of %s requires", ...
                           " exactly two arguments."), clstype, clstype);
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          tbl = this;
          tbl = subsetrows (tbl, ixRow);
          tbl = subsetvars (tbl, ixVar);

        case '{}'
          if (numel (s.subs) != 2)
            error (strcat ("%s.subsref: '{}' indexing of %s requires", ...
                           " exactly two arguments."), clstype, clstype);
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          tbl = this;
          tbl = subsetrows (tbl, ixRow);
          tbl = subsetvars (tbl, ixVar);
          pair = tabular.incompatible_pair (tbl.VariableValues);
          if (! isempty (pair))
            error (strcat ("%s.subsref: cannot concatenate the %s", ...
                           " variables '%s' and '%s', because their types", ...
                           " are %s and %s."), clstype, clstype, ...
                   tbl.VariableNames{pair(1)}, ...
                   tbl.VariableNames{pair(2)}, ...
                   class (tbl.VariableValues{pair(1)}), ...
                   class (tbl.VariableValues{pair(2)}));
          endif
          try
            tbl = varsAsArray (tbl, 'subsref');
          catch
            error (strcat ("%s.subsref: %s cannot be concatenated", ...
                           " into a matrix."), clstype, clstype);
          end_try_catch

        case '.'
          ## A field name may be given as a string scalar, as in MATLAB.
          if (isstring (s.subs) && isscalar (s.subs))
            s.subs = char (s.subs);
          endif
          if (! (ischar (s.subs) && isrow (s.subs)))
            error (strcat ("%s.subsref: '.' index argument must be a", ...
                           " character vector or a string scalar."), clstype);
          endif
          ## Handle special cases: "Properties" and "DimensionNames"
          if (isequal (s.subs, 'Properties'))
            tbl = makeProperties (this);
          elseif (isequal (s.subs, this.DimensionNames{1}))
            tbl = getRowLabels (this);
          elseif (isequal (s.subs, this.DimensionNames{2}))
            try
              tbl = varsAsArray (this, 'subsref');
            catch
              tbl = varsAsCell (this);
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

    ## Class specific subscripted assignment.  Every branch is shared but
    ## the last: a property that is none of the nine belongs to the
    ## subclass's own row labels, and 'setRowLabelProperty' assigns it.
    function tbl = subsasgn (this, s, val)
      clstype = class (this);

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
            error (strcat ("%s.subsasgn: '()' indexing of %s", ...
                           " requires exactly two arguments."), ...
                   clstype, clstype);
          endif
          ## Assigning [] deletes rows or variables.  MATLAB's trigger is
          ## the literal empty matrix, which Octave cannot tell apart from
          ## any other 0-by-0 double, so any of those deletes.
          if (isempty (chain_s) && isa (rhs, 'double')
              && isequal (size (rhs), [0, 0]))
            tbl = deleteSubs (this, s.subs{1}, s.subs{2});
            return;
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          ## Check input data matches referenced elements
          if (! isequal (size (rhs), [numel(ixRow), numel(ixVar)]))
            error (strcat ("%s.subsasgn: input data mismatch indexed", ...
                           " dimensions."), clstype);
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
                error (strcat ("%s.subsasgn: input data type mismatch", ...
                               " indexed variable type."), clstype);
              end_try_catch
              tbl.VariableValues{ixVar(i)} = varData;
            endfor
          else                        # Octave specific
            for i = 1:numel (ixVar)
              varData = this.VariableValues{ixVar(i)};
              try
                varData(ixRow) = rhs(:,i);
              catch
                error (strcat ("%s.subsasgn: input data type mismatch", ...
                               " indexed variable type."), clstype);
              end_try_catch
              tbl.VariableValues{ixVar(i)} = varData;
            endfor
          endif

        ## {} not used in Octave for assigning values
        case '{}'
          error (strcat ("%s.subsasgn: '{}' invalid indexing for", ...
                         " assigning values. Use '()' instead."), clstype);

        case '.'
          ## A field name may be given as a string scalar, as in MATLAB.
          if (isstring (s.subs) && isscalar (s.subs))
            s.subs = char (s.subs);
          endif
          if (! (ischar (s.subs) && isrow (s.subs)))
            error (strcat ("%s.subsasgn: '.' index argument must be a", ...
                           " character vector or a string scalar."), clstype);
          endif
          ## Grab Properties
          if (isequal (s.subs, 'Properties'))
            ## no further recursion, everything is handled here
            if (isempty (chain_s))
              error ("%s.subsasgn: cannot assign new properties.", clstype);
            endif
            s = chain_s(1);
            ## A property name may be given as a string scalar, as in MATLAB.
            if (isstring (s.subs) && isscalar (s.subs))
              s.subs = char (s.subs);
            endif
            if (! (ischar (s.subs) && isrow (s.subs)))
              error (strcat ("%s.subsasgn: '.' index argument must be a", ...
                             " character vector or a string scalar."), clstype);
            endif

            ## Handle table properties
            if (isequal (s.subs, 'Description'))
              ## Check for valid input: character vector of string
              if (isa (val, 'string'))
                if (numel (val) > 1)
                  error (strcat ("%s.subsasgn: %s description must", ...
                                 " be a character vector or a string", ...
                                 " scalar."), clstype, clstype);
                endif
                val = cellstr (val){1};
              elseif (! ischar (val))
                error (strcat ("%s.subsasgn: %s description must be", ...
                               " a character vector or a string scalar."), ...
                       clstype, clstype);
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
                  error (strcat ("%s.subsasgn: cannot index", ...
                                 " DimensionNames with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple DimensionNames at once."), clstype);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:2];
                endif
                if (! all (ismember (idx, [1:2])))
                  error (strcat ("%s.subsasgn: out of bound index for", ...
                                 " DimensionNames."), clstype);
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("%s.subsasgn: DimensionNames must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."), clstype);
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
                error (strcat ("%s.subsasgn: DimensionNames must be a", ...
                               " two-element cell array of character", ...
                               " vectors or string array."), clstype);
              endif
              this.DimensionNames = val;
              tbl = this;

            elseif (isequal (s.subs, 'VariableNames'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("%s.subsasgn: cannot index", ...
                                 " VariableNames with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple VariableNames at once."), clstype);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("%s.subsasgn: out of bound index for", ...
                                 " VariableNames."), clstype);
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("%s.subsasgn: VariableNames must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."), clstype);
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
                error (strcat ("%s.subsasgn: VariableNames must be a", ...
                               " cell array of character vectors or a", ...
                               " string array matching the number of", ...
                               " variables."), clstype);
              endif
              this.VariableNames = val;
              tbl = this;

            elseif (isequal (s.subs, 'VariableTypes'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("%s.subsasgn: cannot index", ...
                                 " VariableTypes with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple VariableTypes at once."), clstype);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("%s.subsasgn: out of bound index for", ...
                                 " VariableTypes."), clstype);
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("%s.subsasgn: VariableTypes must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."), clstype);
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
                error (strcat ("%s.subsasgn: VariableTypes must be a", ...
                               " cell array of character vectors or a", ...
                               " string array matching the number of", ...
                               " variables."), clstype);
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
                  error (strcat ("%s.subsasgn: cannot index", ...
                                 " VariableDescriptions with more than", ...
                                 " one dimension. Use a vector to index", ...
                                 " multiple VariableDescriptions at", ...
                                 " once."), clstype);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("%s.subsasgn: out of bound index for", ...
                                 " VariableDescriptions."), clstype);
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("%s.subsasgn: VariableDescriptions", ...
                                 " must be a cell array of character", ...
                                 " vectors or a string array matching", ...
                                 " the number of indexed variables."), clstype);
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
                error (strcat ("%s.subsasgn: VariableDescriptions", ...
                               " must be a cell array of character", ...
                               " vectors or a string array matching the", ...
                               " number of variables."), clstype);
              endif
              this.VariableDescriptions = val;
              tbl = this;

            elseif (isequal (s.subs, 'VariableUnits'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("%s.subsasgn: cannot index", ...
                                 " VariableUnits with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple VariableUnits at once."), clstype);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("%s.subsasgn: out of bound index for", ...
                                 " VariableUnits."), clstype);
                endif
                if (ischar (val) || isa (val, 'string'))
                  val = cellstr (val);
                endif
                if (! (iscellstr (val) && numel (val) == numel (idx)))
                  error (strcat ("%s.subsasgn: VariableUnits must be", ...
                                 " a cell array of character vectors or", ...
                                 " a string array matching the number of", ...
                                 " indexed variables."), clstype);
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
                error (strcat ("%s.subsasgn: VariableUnits must be a", ...
                               " cell array of character vectors or a", ...
                               " string array matching the number of", ...
                               " variables."), clstype);
              endif
              this.VariableUnits = val;
              tbl = this;

            elseif (isequal (s.subs, 'VariableContinuity'))
              ## Check for further indexing of specific variable(s)
              if (numel (chain_s) > 1)
                idx = chain_s(2).subs;
                if (numel (idx) > 1)
                  error (strcat ("%s.subsasgn: cannot index", ...
                                 " VariableContinuity with more than one", ...
                                 " dimension. Use a vector to index", ...
                                 " multiple variables at once."), clstype);
                endif
                idx = cell2mat (idx);
                if (isequal (idx, ':'))
                  idx = [1:width(this)];
                endif
                if (! all (ismember (idx, [1:width(this)])))
                  error (strcat ("%s.subsasgn: out of bound index for", ...
                                 " VariableContinuity."), clstype);
                endif
                if (isempty (this.VariableContinuity))
                  this.VariableContinuity = repmat ({'unset'}, [1, width(this)]);
                endif
                val = check_continuity (val, numel (idx), true, clstype);
                this.VariableContinuity(idx) = val;
                tbl = this;
                return
              endif
              ## An empty value of any type clears the property
              if (isempty (val))
                this.VariableContinuity = [];
                tbl = this;
                return;
              endif
              this.VariableContinuity = check_continuity (val, width (this), ...
                                                          false, clstype);
              tbl = this;

            elseif (isequal (s.subs, 'CustomProperties'))
              ## Assigning the store itself replaces it whole, as in MATLAB,
              ## and only a store taken from a table can be assigned.  The
              ## types travel with it, so a variable property stays one.
              if (numel (chain_s) < 2)
                if (! isa (val, 'datatypes.tabular.CustomProperties'))
                  error (strcat ("%s.subsasgn: the value assigned to", ...
                                 " 'CustomProperties' must be a", ...
                                 " datatypes.tabular.CustomProperties", ...
                                 " object."), clstype);
                endif
                [cpVals, cpTypes] = unpack (val);
                cpNames = fieldnames (cpVals);
                for i = 1:numel (cpNames)
                  cpVal = cpVals.(cpNames{i});
                  if (! strcmp (cpTypes.(cpNames{i}), 'variable'))
                    continue;
                  endif
                  ## A 0-by-0 empty fits a table of any width.
                  if (ndims (cpVal) == 2 && all (size (cpVal) == 0))
                    continue;
                  endif
                  if (numel (cpVal) != width (this))
                    error (strcat ("%s.subsasgn: custom property '%s'", ...
                                   " must have one element for each", ...
                                   " variable in the table, or be a 0-by-0", ...
                                   " empty."), clstype, cpNames{i});
                  endif
                endfor
                ## An empty store is [] with no types, as everywhere else.
                if (isempty (cpNames))
                  this.CustomProperties = [];
                  this.CustomPropTypes = struct ();
                else
                  this.CustomProperties = cpVals;
                  this.CustomPropTypes = cpTypes;
                endif
                tbl = this;
                return;
              endif
              ## Check for valid indexing a custom property
              if (! strcmp (chain_s(2).type, '.'))
                error (strcat ("%s.subsasgn: use '.' notation to", ...
                               " index a custom property."), clstype);
              endif
              cpName = chain_s(2).subs;
              ## A property name may be given as a string scalar, as in MATLAB.
              if (isstring (cpName) && isscalar (cpName))
                cpName = char (cpName);
              endif
              if (! (ischar (cpName) && isrow (cpName)))
                error (strcat ("%s.subsasgn: indexing a custom property", ...
                               " requires a character vector or a string", ...
                               " scalar."), clstype);
              endif
              ## Check that referenced custom property exists
              if (isempty (this.CustomProperties))
                error (strcat ("%s.subsasgn: custom property '%s'", ...
                               " does not exist, use 'addprop' to add", ...
                               " it."), clstype, ...
                       cpName);
              endif
              existingNames = fieldnames (this.CustomProperties);
              if (! ismember (cpName, existingNames))
                error (strcat ("%s.subsasgn: custom property '%s'", ...
                               " does not exist, use 'addprop' to add", ...
                               " it."), clstype, ...
                       cpName);
              endif
              ## Get type of custom property
              cpType = this.CustomPropTypes.(cpName);
              if (strcmp (cpType, 'table'))
                ## A 'table' property is metadata the class never reads, so
                ## it holds any value of any type and size, stored as given.
                if (numel (chain_s) > 2)
                  error (strcat ("%s.subsasgn: custom property '%s'", ...
                                 " is a table property and cannot be", ...
                                 " indexed any further."), clstype, ...
                         cpName);
                endif
                this.CustomProperties.(cpName) = val;
              else
                maxIdx = width (this);
                ## Get further indexing (if available)
                if (numel (chain_s) > 2)
                  if (strcmp (chain_s(3).type, '.'))
                    error (strcat ("%s.subsasgn: custom property '%s'", ...
                                   " is a variable property and cannot", ...
                                   " be indexed any further with '.'", ...
                                   " notation."), clstype, ...
                           cpName);
                  endif
                  cpIdx = chain_s(3).subs;
                  if (numel (cpIdx) > 1)
                    error (strcat ("%s.subsasgn: cannot index a", ...
                                   " custom variable property in more", ...
                                   " than one dimension."), clstype);
                  endif
                  cpIdx = cell2mat (cpIdx);
                  if (isequal (cpIdx, ':'))
                    cpIdx = [1:maxIdx];
                  elseif (islogical (cpIdx))
                    ## A logical mask selects the variables it marks.  It may
                    ## be shorter than the table but never longer.
                    if (numel (cpIdx) > maxIdx)
                      error (strcat ("%s.subsasgn: out of bound index", ...
                                     " for custom variable property", ...
                                     " '%s'."), clstype, cpName);
                    endif
                    cpIdx = find (cpIdx);
                  endif
                  if (! all (ismember (cpIdx, [1:maxIdx])))
                    error (strcat ("%s.subsasgn: out of bound index", ...
                                   " for custom variable property", ...
                                   " '%s'."), clstype, cpName);
                  endif
                  ## Check input is a vector
                  if (! isvector (val))
                    error (strcat ("%s.subsasgn: assigned value to a", ...
                                   " custom variable property must be a", ...
                                   " vector."), clstype);
                  endif
                  if (numel (val) != numel (cpIdx))
                    error (strcat ("%s.subsasgn: input vector does", ...
                                   " not match the number of indexed", ...
                                   " variables in the custom variable", ...
                                   " property '%s'."), clstype, ...
                           cpName);
                  endif
                  ## A cleared property is re-expanded to the table's width
                  ## before the assignment, padded the way merging pads it,
                  ## so indexing can never leave a short vector behind.
                  tmp = this.CustomProperties.(cpName);
                  if (numel (tmp) != maxIdx)
                    if (iscell (tmp) || iscell (val))
                      tmp = cell (1, maxIdx);
                    else
                      tmp = NaN (1, maxIdx);
                    endif
                  endif
                  tmp(cpIdx) = val;
                  this.CustomProperties.(cpName) = tmp;
                else
                  ## A character vector is never a per-variable value, so the
                  ## type is checked before the size: '' is 0-by-0 and would
                  ## otherwise read as a clearing value.
                  if (ischar (val))
                    error (strcat ("%s.subsasgn: a character vector is", ...
                                   " not a valid value for a custom", ...
                                   " variable property, use a cell array", ...
                                   " of character vectors or a string", ...
                                   " array."), clstype);
                  endif
                  ## A 0-by-0 empty clears the property whatever the table's
                  ## width and is stored as []; empties of any other size are
                  ## not accepted.
                  if (ndims (val) == 2 && all (size (val) == 0))
                    val = [];
                  elseif (! isvector (val))
                    error (strcat ("%s.subsasgn: assigned value to a", ...
                                   " custom variable property must be a", ...
                                   " vector."), clstype);
                  elseif (numel (val) != maxIdx)
                    error (strcat ("%s.subsasgn: input vector does", ...
                                   " not match the number of variables", ...
                                   " in table."), clstype);
                  endif
                  this.CustomProperties.(cpName) = val;
                endif
              endif
              tbl = this;

            else
              ## Whatever is left names the subclass's own row labels, and
              ## only the subclass knows how to assign it.
              [tbl, handled] = setRowLabelProperty (this, s.subs, val, ...
                                                    chain_s);
              if (! handled)
                error ("%s.subsasgn: unknown %s property '%s'.", ...
                       clstype, clstype, s.subs);
              endif
            endif

          else
            ## Everything else is indexing a variable name (existing of new),
            ## except that assigning [] to one deletes it.
            if (isempty (chain_s) && isa (rhs, 'double')
                && isequal (size (rhs), [0, 0]))
              if (isequal (s.subs, this.DimensionNames{1}))
                error (strcat ("%s.subsasgn: cannot delete the row labels", ...
                               " of a %s."), clstype, clstype);
              endif
              tbl = deleteVars (this, s.subs);
            else
              tbl = setvar (this, s.subs, rhs);
            endif
          endif
      endswitch
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
      error ("%s.repelems: 'repelems' is not supported.", class (this));
    endfunction

    function out = reshape (this, varargin)
      error ("%s.reshape: 'reshape' is not supported.", class (this));
    endfunction

    function out = resize (this, varargin)
      error ("%s.resize: 'resize' is not supported.", class (this));
    endfunction

    function out = shiftdims (this, varargin)
      error ("%s.shiftdims: 'shiftdims' is not supported.", class (this));
    endfunction

    function out = vec (this, varargin)
      error ("%s.vec: 'vec' is not supported.", class (this));
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

    ## The names of the custom properties of one scope, in the order they were
    ## added.  TYPE is 'table' or 'variable'.  A property's type is held under
    ## the property's own name, so nothing here depends on two containers
    ## agreeing on an order.
    ## -*- texinfo -*-
    ## @deftypefn {tabular} {[@var{index}, @var{errmsg}] =} sortrowsIndex (@var{obj}, @var{args})
    ##
    ## Work out the row order @code{sortrows} asks for.
    ##
    ## @var{args} is the cell of arguments the public method was given, less
    ## the object itself.  The permutation comes back in @var{index}, and
    ## @var{errmsg} carries the body of any complaint so that the calling
    ## class can raise it under its own name.  The caller applies the order
    ## with @code{subsetrows}, which is where a subclass maintains whatever
    ## its row labels oblige it to.
    ##
    ## @end deftypefn
    function [index, errmsg] = sortrowsIndex (this, args_in)

      index = [];
      errmsg = '';
      varargin = args_in;

      ## Bound before use: a call inside [] or {} is split by the space
      ## before its paren and would run with no arguments.
      labelNames = rowLabelKeyNames (this);
      keyNames = [this.VariableNames, labelNames];


      ## Add defaults.  A class whose rows carry labels may sort by them
      ## when nothing else is asked for; a table sorts by every variable.
      if (sortsByLabelsByDefault (this))
        varRef = [];
        inLabels = 1;
      else
        varRef = ':';
        inLabels = 0;
      endif
      direction = {'ascend'};
      dir_given = false;

      ## Parse optional Name-Value paired arguments
      optNames = {'MissingPlacement', 'ComparisonMethod'};
      dfValues = {'auto', 'auto'};
      [MP, CM, args] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! ismember (MP, {'auto', 'first', 'last'}))
        errmsg = strcat ("'MissingPlacement' parameter can", ...
                       " be either 'auto', 'first', or 'last'.");
        return
      endif
      if (! ismember (CM, {'auto', 'real', 'abs'}))
        errmsg = strcat ("'ComparisonMethod' parameter can", ...
                       " be either 'auto', 'real', or 'abs'.");
        return
      endif

      ## Parse extra arguments
      nargs = numel (args);
      if (nargs > 2)
        errmsg = "invalid number of input arguments.";
        return
      endif
      if (nargs > 1)
        ## Matched without regard to case, as MATLAB does.
        direction = lower (cellstr (args{2}));
        dir_given = true;
        if (! all (ismember (direction, {'ascend', 'descend'})))
          errmsg = "invalid value for DIRECTION argument.";
          return
        endif
      endif
      if (nargs > 0)
        ## Given keys of its own, the object no longer falls back on its row
        ## labels; they take part only if named among those keys.
        inLabels = 0;
        ## RowNames and rowDimName take precedence over variable names
        arg1 = args{1};
        if (ischar (arg1) && isvector (arg1) &&
            ismember (arg1, labelNames))
          ## Check user's direction is scalar
          if (dir_given && numel (direction) != 1)
            errmsg = strcat ("DIRECTION must be a scalar", ...
                           " input when 'RowNames' or 'rowDimNames' are", ...
                           " selected.");
            return
          endif
          ## Handle special case here
          if (! hasRowLabels (this))
            index = [1:height(this)]';
            return
          else
            index = labelOrder (getRowLabels (this), direction{1}, MP);
            return
          endif
        endif

        ## At this point, VARS must be variable name(s)
        if (islogical (arg1))
          varRef = arg1;
          if (! (isvector (varRef) && numel (varRef) == width (this)))
            errmsg = strcat ("logical indexing vector does", ...
                           " not match table width.");
            return
          endif
          ## Check user's direction matches selected variables
          if (! isscalar (direction))
            if (dir_given && sum (varRef) != numel (direction))
              errmsg = "invalid size for DIRECTION argument.";
              return
            endif
          endif
        elseif (isnumeric (arg1))
          if (isempty (arg1))
            arg1 = [1:width(this)];
          endif
          if (! isvector (arg1) || any (fix (arg1) != arg1) || any (arg1 == 0))
            errmsg = strcat ("numerical indexing must be a", ...
                           " vector of nonzero integers.");
            return
          endif
          if (any (abs (arg1) > width (this)))
            errmsg = "numerical index exceeds table dimensions.";
            return
          endif
          varRef = arg1;
          ## If direction was given, ignore sign of numerical indexing
          if (dir_given)
            varRef = abs (varRef);
            ## Check user's direction matches selected variables
            if (! isscalar (direction))
              if (! isequal (size (varRef), size (direction)))
                errmsg = "invalid size for DIRECTION argument.";
                return
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
          elseif (! all (ismember (varRef, keyNames)))
            errmsg = "VARS indexes non-existing variable names.";
            return
          endif
          ## Check user's direction matches selected variables
          if (! isscalar (direction))
            if (strcmp (varRef, ':') && numel (direction) != width (this))
              errmsg = "invalid size for DIRECTION argument.";
              return
            elseif (! isequal (size (varRef), size (direction)))
              errmsg = "invalid size for DIRECTION argument.";
              return
            endif
          endif
          ## Check whether the row labels are among the indexed variables
          isLabel = ismember (varRef, labelNames);
          if (any (isLabel))
            inLabels = find (isLabel);
            varRef(isLabel) = [];
          endif
        elseif (isa (arg1, 'vartype'))
          varRef = arg1;
          ## Check user's direction is scalar
          if (dir_given && numel (direction) != 1)
            errmsg = strcat ("DIRECTION must be a scalar", ...
                           " input when variables are indexed with a", ...
                           " 'vartype' object.");
            return
          endif
        endif
      endif

      ## Resolve varRef to variables' indices
      ixVars = resolveVarRef (this, varRef);

      ## With nothing to sort by the order cannot change, and the sort
      ## itself has no key to build.
      if (isempty (ixVars) && inLabels == 0)
        index = (1:height (this))';
        return;
      endif
      ## Build a cell array for the selected variables to be used in sorting
      if (inLabels == 0)
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
        if (inLabels == ix)
          labels = getRowLabels (this);
          varVal(ix) = {labels};
          offset = 1;
        else
          varVal(ix) = this.VariableValues(ixVars(ix - offset));
        endif
      endfor

      ## Prepare a proxy array by converting every key to numeric proxies.
      ## The direction vector is widened by the proxy's own column count, so
      ## a key that yields no columns contributes no direction either.
      varValIdx = [];
      varValDir = [];
      for ix = 1:numel (varVal)

        if (strcmpi (direction{ix}, 'ascend'))
          tmpDir = 1;
        else
          tmpDir = -1;
        endif
        [p, badtype] = valueProxy (varVal{ix}, CM);
        if (! isempty (badtype))
          errmsg = sortBadType (badtype);
          return
        endif
        dirCols = repmat (tmpDir, 1, size (p, 2));
        varValIdx = [varValIdx, p];
        varValDir = [varValDir, dirCols];
      endfor

      ## Fix direction vector
      varValDir = [1:numel(varValDir)] .* varValDir;

      ## Do the actual sorting here
      [~, index] = sortrows (varValIdx, varValDir);
      index = index(:);

      ## Fix missing placement.  The first key decides it, and that key may
      ## be the row labels, so the mask is read off the key itself rather
      ## than off the object: a subclass need not implement 'ismissing'.
      mask = keyMissingMask (varVal{1});
      TFvec = mask(index);
      if (any (TFvec) && ! all (TFvec))
        is_nan = index(TFvec);
        no_nan = index(! TFvec);
        if (any (find (TFvec) == 1) && strcmpi (MP, 'last'))
          index = [no_nan; is_nan];
        elseif (any (find (TFvec) == numel (index)) && strcmpi (MP, 'first'))
          index = [is_nan; no_nan];
        endif
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tabular} {[@var{ia}, @var{ic}, @var{errmsg}] =} uniqueIndex (@var{obj}, @var{args})
    ##
    ## Work out which rows @code{unique} keeps.
    ##
    ## @var{args} is the cell of arguments the public method was given, less
    ## the object itself.  @var{ia} indexes the rows to keep and @var{ic}
    ## maps every original row onto one of them, as @code{unique} returns
    ## them.  @var{errmsg} carries the body of any complaint so that the
    ## calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [ia, ic, errmsg] = uniqueIndex (this, args_in)

      ia = [];
      ic = [];
      errmsg = '';

      ## Check max number of input arguments
      if (numel (args_in) > 1)
        errmsg = "too many input arguments.";
        return
      endif

      ## Handle 'setOrder' and 'occurrence' options
      opt = 'sorted';
      if (! isempty (args_in))
        opts = {'sorted', 'stable', 'first', 'last', 'rows'};
        if (any (strcmp (args_in{1}, opts)))
          opt = args_in{1};
        else
          errmsg = sprintf ("invalid option '%s'.", args_in{1});
          return
        endif
      endif
      ## Rows are the only thing a tabular object compares, so naming them
      ## changes nothing.
      if (strcmp (opt, 'rows'))
        opt = 'sorted';
      endif

      ## What makes a row distinct.  A timetable is told apart by its row
      ## times as well as by its variables; a table by its variables alone.
      keyVals = {};
      if (uniqueIncludesLabels (this) && hasRowLabels (this))
        labels = getRowLabels (this);
        keyVals(end+1) = {labels};
      endif
      for ix = 1:width (this)
        keyVals(end+1) = this.VariableValues(ix);
      endfor

      ## Every row of an object with nothing to compare is the same empty
      ## row, so they reduce to one; there is no proxy to build from.
      if (isempty (keyVals) && height (this) > 0)
        ia = 1;
        ic = ones (height (this), 1);
        return
      endif

      ## Prepare a proxy array by converting every key to numeric proxies
      proxy = [];
      for ix = 1:numel (keyVals)
        [p, badtype] = valueProxy (keyVals{ix}, 'auto');
        if (! isempty (badtype))
          errmsg = uniqueBadType (badtype);
          return
        endif
        proxy = [proxy, p];
      endfor

      ## Find unique rows in the proxy
      [~, ia, ic] = __unique__ (proxy, opt, 'rows');

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tabular} {[@var{ixRows}, @var{errmsg}] =} headTailRows (@var{obj}, @var{k}, @var{fromEnd})
    ##
    ## The rows @code{head} or @code{tail} returns.
    ##
    ## @var{k} is the count asked for, empty for the default of eight, and
    ## @var{fromEnd} says which end to count from.  Asking for more rows than
    ## there are yields all of them, and asking for none yields none.
    ## @var{errmsg} carries the body of any complaint so that the calling
    ## class can raise it under its own name.
    ##
    ## @end deftypefn
    function [ixRows, errmsg] = headTailRows (this, k, fromEnd)

      ixRows = [];
      errmsg = '';
      if (isempty (k))
        k = 8;
      endif
      if (! (isnumeric (k) && isscalar (k) && isreal (k) && isfinite (k))
          || fix (k) != k || k < 0)
        errmsg = strcat ("K must be a real, nonnegative, integer scalar", ...
                         " value.");
        return
      endif
      n = height (this);
      k = min (k, n);
      if (fromEnd)
        ixRows = ((n - k + 1):n)';
      else
        ixRows = (1:k)';
      endif

    endfunction

    function names = customPropsOfType (this, type)
      names = {};
      if (isempty (this.CustomProperties))
        return;
      endif
      names = fieldnames (this.CustomProperties);
      keep = cellfun (@(n) strcmp (this.CustomPropTypes.(n), type), names);
      names = names(keep);
    endfunction

    ## Resolve variable references to indices and variable names.
    ## Returns:
    ##   @var{ixVar} - numeric indices of the variables in @var{tbl}
    ##   @var{varNames} - a cellstr of the names of the indexed variables
    ##
    ## Raises an error if any of the specified variables could not be resolved,
    ## unless strictness is 'lenient', in which case it will return 0 for the
    ## index and '' for the name for each variable which could not be resolved.
    function [ixVar, varNames] = resolveVarRef (this, varRef, strictness)
      clstype = class (this);
      if (nargin < 3 || isempty (strictness))
        strictness = 'strict';
      endif
      if ((isnumeric (varRef) || islogical (varRef)) && isempty (varRef))
        ## An empty numeric or logical subscript selects no variables, as it
        ## does on any array.  An empty cell is not an index at all and is
        ## refused below, which is what indexing an ordinary array with one
        ## does.
        ixVar = zeros (1, 0);
        if (nargout > 1)
          varNames = cell (1, 0);
        endif
        return;
      endif
      if (! isvector (varRef))
        error ("%s: variable index must be a vector.", clstype);
      endif
      nvars = width (this);
      if (islogical (varRef))
        vec = numel (varRef);
        if (nvars != vec)
          error ("%s: variable logical index does not match %s width.", ...
                 clstype, clstype);
        endif
        ixVar = 1:nvars;
        ixVar(! varRef) = [];
      elseif (isnumeric (varRef))
        ixVar = varRef;
        ix_bad = find (ixVar > nvars | ixVar < 1);
        if (! isempty (ix_bad))
          error (strcat ("%s: variable index out of bounds: requested", ...
                         " index %d; %s has %d variables."), ...
                 clstype, ixVar(ix_bad(1)), clstype, nvars);
        endif
      elseif (ischar (varRef) && isequal (varRef, ':'))
        ixVar = 1:nvars;
      elseif (ischar (varRef) || iscellstr (varRef) || isa (varRef, 'string'))
        varRef = cellstr (varRef);
        [tf, ixVar] = ismember (varRef, this.VariableNames);
        if (isequal (strictness, 'strict'))
          if (! all (tf))
            if (sum (! tf) == 1)
              error ("%s: no such variable in %s: '%s'", ...
                     clstype, clstype, varRef{! tf});
            else
              missing_vars = strjoin (varRef(! tf), ', ');
              error ("%s: no such variables in %s: '%s'", ...
                     clstype, clstype, missing_vars);
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
        error ("%s: unsupported variable indexing operand type: '%s'", ...
               clstype, class (varRef));
      endif
      if (nargout > 1)
        varNames = repmat ({''}, size (ixVar));
        varNames(ixVar != 0) = this.VariableNames(ixVar(ixVar != 0));
      endif
    endfunction

    ## Resolve both row and variable references to indices.
    function [ixRow, ixVar] = resolveRowVarRefs (this, rowRef, varRef)
      clstype = class (this);
      if (isnumeric (rowRef) || islogical (rowRef))
        ixRow = rowRef;
      elseif ((ischar (rowRef) || isa (rowRef, 'string'))
              && isequal (rowRef, ':'))
        ## The type is checked before the comparison: 'isequal' against a
        ## row time would dispatch to the row time's own, which reads the
        ## colon as a time string and fails on it.
        ixRow = 1:height (this);
      elseif (ischar (rowRef) || iscellstr (rowRef) || isa (rowRef, 'string'))
        rowRef = cellstr (rowRef);
        ixRow = resolveRowRef (this, rowRef);
      elseif (isa (rowRef, 'rowfilter'))
        ## A filter is a condition on the variables and reads no row labels,
        ## so it selects from any tabular class the same way.
        ixRow = rowIndices (rowRef, this);
      else
        ## Anything else is a row reference of a kind only the subclass can
        ## read: a row time, a range of them, a tolerant match.  A class that
        ## does not take the kind offered refuses it in its own hook.
        ixRow = resolveRowRef (this, rowRef);
      endif
      ixVar = resolveVarRef (this, varRef);
    endfunction

    ## Return a subset of rows defined by the numerical or logical vector ixRows
    function tbl = subsetrows (this, ixRows)
      tbl = this;
      if (width (this) == 0)
        ## With no variable to index, nothing would raise on an out-of-range
        ## row and nothing would carry the new count, so both happen here.
        ixRows = validateRowIndex (this, ixRows);
        tbl.RowCount = numel (ixRows);
      endif
      s = struct ('type', '()', 'subs', {{ixRows,':'}});
      for i = 1:width (this)
        tbl.VariableValues{i} = subsref (tbl.VariableValues{i}, s);
      endfor
      tbl = subsetRowLabels (tbl, ixRows);
    endfunction

    ## Keep the stored row count normalised: an object carries it only while
    ## it has no variable to carry the height for it, and carries zero
    ## otherwise.  Without that, two objects equal in every observable way
    ## could still differ in this, and 'isequal' compares it.
    function tbl = setRowCount (this, nrows)
      tbl = this;
      if (width (this) == 0)
        tbl.RowCount = nrows;
      else
        tbl.RowCount = 0;
      endif
    endfunction

    ## Resolve a row subscript to a validated vector of row indices.  The
    ## variables normally raise on a bad row when they are indexed; this is
    ## for the paths where there is no variable left to do it.
    function ixRows = validateRowIndex (this, ixRows)
      clstype = class (this);
      nrows = height (this);
      if (ischar (ixRows) && isequal (ixRows, ':'))
        ixRows = 1:nrows;
      elseif (islogical (ixRows))
        if (numel (ixRows) > nrows)
          error ("%s: row logical index does not match %s height.", ...
                 clstype, clstype);
        endif
        ixRows = find (ixRows(:))';
      elseif (isnumeric (ixRows))
        ixRows = ixRows(:)';
        if (any (ixRows < 1 | ixRows != fix (ixRows)))
          error ("%s: row index must be a positive integer.", clstype);
        endif
        ix_bad = find (ixRows > nrows, 1);
        if (! isempty (ix_bad))
          error (strcat ("%s: row index out of bounds: requested index", ...
                         " %d; %s has %d rows."), clstype, ...
                 ixRows(ix_bad), clstype, nrows);
        endif
      endif
    endfunction

    ## Delete rows or variables, which is what assigning [] means.  One
    ## subscript must be ':' and the other names what goes; with both of
    ## them ':' the rows go.
    function tbl = deleteSubs (this, rowRef, varRef)
      rowColon = is_colon_ref (rowRef);
      varColon = is_colon_ref (varRef);
      if (! rowColon && ! varColon)
        error (strcat ("%s.subsasgn: deleting rows or variables by", ...
                       " assigning [] requires one subscript to be ':'."), ...
               class (this));
      endif
      if (rowColon && ! varColon)
        tbl = deleteVars (this, varRef);
      else
        tbl = deleteRows (this, rowRef);
      endif
    endfunction

    ## Delete the referenced variables.  An empty reference removes nothing.
    function tbl = deleteVars (this, varRef)
      if (isempty (varRef))
        tbl = this;
        return;
      endif
      ixVar = resolveVarRef (this, varRef);
      tbl = subsetvars (this, setdiff (1:width (this), ixVar(:)'));
    endfunction

    ## Delete the referenced rows.  An empty reference removes nothing.
    function tbl = deleteRows (this, rowRef)
      if (isempty (rowRef) && ! is_colon_ref (rowRef))
        tbl = this;
        return;
      endif
      ixRow = resolveRowVarRefs (this, rowRef, ':');
      ixRow = validateRowIndex (this, ixRow);
      tbl = subsetrows (this, setdiff (1:height (this), ixRow));
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
      cpTypes = struct ();
      ## Pass 1: table-scoped properties (union, first table wins).
      for t = 1:numel (tables)
        T = tables{t};
        if (isempty (T.CustomProperties))
          continue;
        endif
        nm = fieldnames (T.CustomProperties);
        for i = 1:numel (nm)
          if (strcmp (T.CustomPropTypes.(nm{i}), 'table') ...
              && ! isfield (cp, nm{i}))
            cp.(nm{i}) = T.CustomProperties.(nm{i});
            cpTypes.(nm{i}) = 'table';
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
          if (! strcmp (T.CustomPropTypes.(nm{i}), 'variable') ...
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
          cpTypes.(nm{i}) = 'variable';
        endfor
      endfor
      if (isempty (fieldnames (cp)))
        cp = [];
        cpTypes = struct ();
      endif
    endfunction

    ## Return a subset of variables defined by the numerical vector ixVars
    function tbl = subsetvars (this, ixVars)
      nrows = height (this);
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
        cpNames = customPropsOfType (this, 'variable');
        if (! isempty (cpNames))
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
      tbl = setRowCount (tbl, nrows);
    endfunction

    ## The variables laid side by side as one homogeneous array, and as a
    ## cell array.  Neither reads the row labels, so both serve every
    ## tabular class; CALLER names the method reporting a refusal, which is
    ## the public conversion for a table and the brace reference otherwise.
    function A = varsAsArray (this, caller)
      ## Handle empty table.  An object with rows but no variables still
      ## reports its height, so the array it becomes keeps it too.
      if isempty (this)
        A = zeros (size (this));
        return
      endif
      ## A mix of cell and non-cell variables cannot form a homogeneous array.
      ## Octave would silently promote single-row pieces to a cell (MATLAB
      ## errors), so guard explicitly and report the first incompatible pair.
      pair = tabular.incompatible_pair (this.VariableValues);
      if (! isempty (pair))
        error (strcat ("%s.%s: cannot concatenate the table", ...
                       " variables '%s' and '%s', because their types are", ...
                       " %s and %s."), class (this), caller, ...
               this.VariableNames{pair(1)}, ...
               this.VariableNames{pair(2)}, ...
               class (this.VariableValues{pair(1)}), ...
               class (this.VariableValues{pair(2)}));
      endif
      ## Add a try...catch block instead of heuristics
      try
        A = cat (2, this.VariableValues{:});
      catch
        error (strcat ("%s.%s: table cannot be concatenated", ...
                       " into a matrix due to incompatible variable", ...
                       " types."), class (this), caller);
      end_try_catch
    endfunction

    function C = varsAsCell (this)
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

    ## The stored variable values, and the types of the custom properties,
    ## reachable from a sibling class.  Dot access on an object of another
    ## class goes through that class's 'subsref' and is read as a variable
    ## name whatever the property's access, so an operation that mixes the
    ## two classes cannot read them directly and asks here instead.
    function out = varValues (this)
      out = this.VariableValues;
    endfunction

    function out = customPropTypes (this)
      out = this.CustomPropTypes;
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
      rowProps = rowLabelProperties (this);
      rowNames = fieldnames (rowProps);
      for i = 1:numel (rowNames)
        out.(rowNames{i}) = rowProps.(rowNames{i});
      endfor
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
      clstype = class (this);
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
        error ("%s.subsasgn: input value and %s height mismatch.", ...
               clstype, clstype);
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
          cpNames = customPropsOfType (this, 'variable');
          if (! isempty (cpNames))
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
      tbl = setRowCount (tbl, n_rows);
    endfunction

    ## Resolve subscripted reference for internal use called by subsasgn
    function out = single_subref (this, s)
      clstype = class (this);
      switch s.type
        case '()'
          if (numel (s.subs) != 2)
            error (strcat ("%s.subsasgn: ()-indexing of %s requires", ...
                           " exactly two arguments."), clstype, clstype);
          endif
          [ixRow, ixVar] = resolveRowVarRefs (this, s.subs{1}, s.subs{2});
          out = this;
          out = subsetrows (out, ixRow);
          out = subsetvars (out, ixVar);

        case '.'
          ## A field name may be given as a string scalar, as in MATLAB.
          if (isstring (s.subs) && isscalar (s.subs))
            s.subs = char (s.subs);
          endif
          if (! (ischar (s.subs) && isrow (s.subs)))
            error (strcat ("%s.subsasgn: .-index argument must be a", ...
                           " character vector or a string scalar."), clstype);
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
        rowHead = rowLabelHeader (this);
        ## The column is as wide as the wider of its labels and its heading,
        ## the labels sit at the left of it, and the heading is centred over
        ## it exactly as a variable's name is centred over its own column.
        rnWidth = max ([cellfun(@length, rowLabels)(:); length(rowHead)]);
        rnLen = rnWidth + 4;
        padPT = sprintf ("%%-%ds", rnLen);
        padfn = @(x) sprintf (padPT, x);
        rowNM = cellfun (padfn, rowLabels, 'UniformOutput', false);
        if (isempty (rowHead))
          rowHeadStr = repmat (" ", [1, rnLen]);
          rowLineStr = repmat (" ", [1, rnLen]);
        else
          padB = floor ((rnWidth - length (rowHead)) / 2);
          padA = rnWidth - length (rowHead) - padB;
          rowHeadStr = [repmat(" ", [1, padB]), rowHead, ...
                        repmat(" ", [1, padA]), colgap];
          rowLineStr = [repmat("_", [1, rnWidth]), colgap];
        endif
        ## Print table header
        fprintf ("    %s%s\n", rowHeadStr, strhead1);
        fprintf ("    %s%s\n\n", rowLineStr, strline1);
        if (nested)
          fprintf ("    %s%s\n", repmat (" ", [1, rnLen]), strhead2);
          fprintf ("    %s%s\n\n", repmat (" ", [1, rnLen]), strline2);
        endif
        ## Print table rows.  With no variables there is nothing to lay out
        ## beside the labels, and the row is the label alone.
        for iRow = 1:height (this)
          if (var_num > 0)
            strrow = sprintf (rowSpat, colData{iRow,:});
          else
            strrow = "";
          endif
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
      ## An object with no variables summarises to no fields, not to nothing.
      s = struct ();
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

    ## A 0x0 non-char operand takes no part in concatenation and is dropped,
    ## as in MATLAB: [], zeros (0, 0), {} and an empty table all vanish.  A
    ## 0x2 double and '' are not 0x0-and-non-char, so they survive here and
    ## are rejected by the caller's "all inputs must be tabular" check.
    function args = drop_null_operands (args)
      keep = true (1, numel (args));
      for i = 1:numel (args)
        arg = args{i};
        if (! ischar (arg) && ndims (arg) == 2 && all (size (arg) == 0))
          keep(i) = false;
        endif
      endfor
      args = args(keep);
    endfunction

    ## Merge the VariableContinuity of two horizontally combined operands.
    ## An operand carrying none contributes 'unset' for each of its
    ## variables, and the result is empty only when neither carries any.
    function vc = merge_continuity (a, na, b, nb)
      if (isempty (a) && isempty (b))
        vc = [];
        return;
      endif
      if (isempty (a))
        a = repmat ({'unset'}, [1, na]);
      endif
      if (isempty (b))
        b = repmat ({'unset'}, [1, nb]);
      endif
      vc = [a, b];
    endfunction

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
    ## Validate an 'empty' size specification and return it as [rows, vars].
    ## CLSTYPE names the class for the one message that has to say it.
    ## Returns an errmsg body (empty on success) emitted by the caller under
    ## its own name.
    function [sz, errmsg] = emptySize (clstype, args)
      sz = [0, 0];
      errmsg = '';
      if (numel (args) == 1)
        sz = args{1};
        if (! (isnumeric (sz) && isvector (sz) && ! isempty (sz)))
          errmsg = 'SZ must be a numeric vector.';
          return;
        endif
        sz = sz(:)';
        if (isscalar (sz))
          sz = [sz, sz];
        endif
      elseif (numel (args) > 1)
        isnum = cellfun (@(x) isnumeric (x) && isscalar (x), args);
        if (! all (isnum))
          errmsg = 'each dimension must be a numeric scalar.';
          return;
        endif
        sz = cell2mat (args(:)');
      endif
      if (numel (sz) > 2)
        if (any (sz(3:end) != 1))
          errmsg = sprintf ('a %s has only two dimensions.', clstype);
          return;
        endif
        sz = sz(1:2);
      endif
      if (any (sz < 0) || any (sz != fix (sz)))
        errmsg = 'each dimension must be a non-negative integer.';
        return;
      endif
      if (all (sz != 0))
        errmsg = 'at least one dimension must be zero.';
        return;
      endif
    endfunction

    ## The family a variable's type belongs to for concatenation into one
    ## array.  Numbers, logicals, character data and strings promote into one
    ## another, so they are one family; every other type concatenates only
    ## with its own kind.
    function fam = concat_family (val)
      if (iscell (val))
        fam = 'cell';
      elseif (isnumeric (val) || islogical (val) || ischar (val)
              || isa (val, 'string'))
        fam = 'promotable';
      elseif (isstruct (val))
        fam = 'struct';
      else
        fam = class (val);
      endif
    endfunction

    ## The first pair of variables that cannot form one array, or empty when
    ## they all can.  Octave coerces several cross-family mixes silently,
    ## a numeric beside a categorical coming back as the categorical's
    ## codes, so the refusal has to be made here rather than left to 'cat'.
    function pair = incompatible_pair (vals)
      pair = [];
      if (numel (vals) < 2)
        return;
      endif
      fams = cellfun (@tabular.concat_family, vals, 'UniformOutput', false);
      ix = find (! strcmp (fams, fams{1}), 1);
      if (! isempty (ix))
        pair = [1, ix];
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

## Whether a subscript is the colon that selects a whole dimension.
## Order row labels on their own, honouring the placement asked for the
## missing ones.  Octave's sort already puts them last ascending and first
## descending, which is what 'auto' means, so only 'first' and 'last' move
## anything.
## The numeric stand-in that one variable's values sort and compare by.
## Callers phrase their own complaint, so an unsupported type comes back
## named rather than as a message.  CM is the comparison method asked of
## numeric data, and is 'auto' where none applies.
function [p, badtype] = valueProxy (v, CM)

  p = [];
  badtype = '';
  if (isa (v, 'categorical'))
    p = double (v);
  elseif (isa (v, 'calendarDuration'))
    p = v.proxyArray;
  elseif (isa (v, 'datetime'))
    p = tabular.datetime_to_datenum (v);
  elseif (isa (v, 'duration'))
    p = days (v);
  elseif (isa (v, 'string'))
    c = cellstr (v);
    [~, ~, p] = __unique__ (c, 'rows');
  elseif (iscellstr (v))
    [~, ~, p] = __unique__ (v, 'rows');
  elseif (iscell (v))
    badtype = 'cell';
  elseif (isnumeric (v))
    if (strcmpi (CM, 'real') && iscomplex (v))
      p = real (v);
    elseif (strcmpi (CM, 'abs') && isreal (v))
      p = abs (v);
    else
      p = v;
    endif
  elseif (islogical (v))
    p = v;
  elseif (isstruct (v))
    badtype = 'struct';
  elseif (isa (v, 'table') || isa (v, 'timetable'))
    try
      p = table2array (v);
    catch
      badtype = 'nested';
    end_try_catch
  endif

endfunction

## How 'sortrows' names a type it cannot compare.
function msg = sortBadType (badtype)

  switch (badtype)
    case 'cell'
      msg = "cannot sort variables of 'cell' type.";
    case 'struct'
      msg = "cannot sort variables of 'struct' type.";
    otherwise
      msg = strcat ("cannot sort nested tables with mixed data", ...
                    " types.");
  endswitch

endfunction

## How 'unique' names a type it cannot compare.
function msg = uniqueBadType (badtype)

  switch (badtype)
    case 'cell'
      msg = strcat ("cannot find unique rows for variables of", ...
                    " 'cell' type.");
    case 'struct'
      msg = strcat ("cannot find unique rows for variables of", ...
                    " 'struct' type.");
    otherwise
      msg = strcat ("cannot find unique rows for nested tables with", ...
                    " mixed data types.");
  endswitch

endfunction

function index = labelOrder (labels, direction, MP)

  [~, index] = sort (labels, direction);
  index = index(:);
  mask = keyMissingMask (labels);
  TFvec = mask(index);
  if (any (TFvec) && ! all (TFvec))
    is_nan = index(TFvec);
    no_nan = index(! TFvec);
    if (strcmpi (MP, 'first'))
      index = [is_nan; no_nan];
    elseif (strcmpi (MP, 'last'))
      index = [no_nan; is_nan];
    endif
  endif

endfunction

## One logical per row saying whether the sort key is missing there.  Read
## off the key itself rather than off the object, so that a class without an
## 'ismissing' method can still be sorted, and so that a key which cannot be
## missing at all simply answers false.
function mask = keyMissingMask (v)

  m = __varmissing__ (v);
  if (size (m, 2) > 1)
    m = any (m, 2);
  endif
  mask = m(:);

endfunction

function tf = is_colon_ref (ref)
  tf = false;
  if (isa (ref, 'string') && isscalar (ref))
    ref = char (ref);
  endif
  if (ischar (ref) && isrow (ref))
    tf = isequal (ref, ':');
  endif
endfunction

## Validate a VariableContinuity assignment of N elements and return it as a
## cell array of character vectors.  A bare character vector is only valid
## when indexing individual variables, which is what ALLOWCHAR marks.
## CLSTYPE names the class the assignment was made on.
function val = check_continuity (val, n, allowchar, clstype)
  if (ischar (val) && ! allowchar)
    error (strcat ("%s.subsasgn: to assign to the VariableContinuity", ...
                   " property, use a string array or a cell array of", ...
                   " character vectors. A character vector can be assigned", ...
                   " only to an individual element of the property."), clstype);
  endif
  if (ischar (val) || isa (val, 'string'))
    val = cellstr (val);
  endif
  if (! iscellstr (val))
    error (strcat ("%s.subsasgn: VariableContinuity must be a cell array", ...
                   " of character vectors or a string array."), clstype);
  endif
  if (numel (val) != n)
    error (strcat ("%s.subsasgn: VariableContinuity property must have", ...
                   " one element for each variable in the table."), clstype);
  endif
  if (! all (ismember (val, {'continuous', 'step', 'event', 'unset'})))
    error (strcat ("%s.subsasgn: VariableContinuity property must be", ...
                   " specified with 'continuous', 'step', 'event', or", ...
                   " 'unset'."), clstype);
  endif
  val = val(:)';
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


## Every subclass must implement all eleven hooks, and the message names the
## subclass that is missing one.  The hooks are protected, so they can only be
## reached from inside a subclass: the fixture implements none of them and
## exposes one public wrapper per hook.
%!shared fixdir
%! fixdir = tempname ();
%! mkdir (fixdir);
%! src = {'classdef notable < tabular', ...
%!        '  methods', ...
%!        '    function out = call_hasRowLabels (this)', ...
%!        '      out = hasRowLabels (this);', ...
%!        '    endfunction', ...
%!        '    function out = call_getRowLabels (this)', ...
%!        '      out = getRowLabels (this);', ...
%!        '    endfunction', ...
%!        '    function out = call_rowLabelName (this)', ...
%!        '      out = rowLabelName (this);', ...
%!        '    endfunction', ...
%!        '    function out = call_rowLabelStrings (this)', ...
%!        '      out = rowLabelStrings (this);', ...
%!        '    endfunction', ...
%!        '    function out = call_rowLabelHeader (this)', ...
%!        '      out = rowLabelHeader (this);', ...
%!        '    endfunction', ...
%!        '    function out = call_rowLabelProperties (this)', ...
%!        '      out = rowLabelProperties (this);', ...
%!        '    endfunction', ...
%!        '    function out = call_setRowLabelProperty (this)', ...
%!        '      out = setRowLabelProperty (this, "x", 1, []);', ...
%!        '    endfunction', ...
%!        '    function out = call_subsetRowLabels (this)', ...
%!        '      out = subsetRowLabels (this, 1);', ...
%!        '    endfunction', ...
%!        '    function out = call_clearRowLabels (this)', ...
%!        '      out = clearRowLabels (this);', ...
%!        '    endfunction', ...
%!        '    function out = call_resolveRowRef (this)', ...
%!        '      out = resolveRowRef (this, 1);', ...
%!        '    endfunction', ...
%!        '    function out = call_makeProperties (this)', ...
%!        '      out = makeProperties (this);', ...
%!        '    endfunction', ...
%!        '  endmethods', ...
%!        'endclassdef'};
%! fid = fopen (fullfile (fixdir, 'notable.m'), 'w');
%! fprintf (fid, '%s\n', src{:});
%! fclose (fid);
%! addpath (fixdir);

## Test 'hasRowLabels' raises until the subclass implements it
%!error <notable: subclass must implement hasRowLabels.> ...
%! call_hasRowLabels (notable ());
## Test 'getRowLabels' raises until the subclass implements it
%!error <notable: subclass must implement getRowLabels.> ...
%! call_getRowLabels (notable ());
## Test 'rowLabelName' raises until the subclass implements it
%!error <notable: subclass must implement rowLabelName.> ...
%! call_rowLabelName (notable ());
## Test 'rowLabelStrings' raises until the subclass implements it
%!error <notable: subclass must implement rowLabelStrings.> ...
%! call_rowLabelStrings (notable ());
## Test 'rowLabelHeader' raises until the subclass implements it
%!error <notable: subclass must implement rowLabelHeader.> ...
%! call_rowLabelHeader (notable ());
## Test 'rowLabelProperties' raises until the subclass implements it
%!error <notable: subclass must implement rowLabelProperties.> ...
%! call_rowLabelProperties (notable ());
## Test 'setRowLabelProperty' raises until the subclass implements it
%!error <notable: subclass must implement setRowLabelProperty.> ...
%! call_setRowLabelProperty (notable ());
## Test 'subsetRowLabels' raises until the subclass implements it
%!error <notable: subclass must implement subsetRowLabels.> ...
%! call_subsetRowLabels (notable ());
## Test 'clearRowLabels' raises until the subclass implements it
%!error <notable: subclass must implement clearRowLabels.> ...
%! call_clearRowLabels (notable ());
## Test 'resolveRowRef' raises until the subclass implements it
%!error <notable: subclass must implement resolveRowRef.> ...
%! call_resolveRowRef (notable ());
## Test 'makeProperties' raises until the subclass implements it
%!error <notable: subclass must implement makeProperties.> ...
%! call_makeProperties (notable ());

## Test the fixture is removed again
%!test
%! rmpath (fixdir);
%! delete (fullfile (fixdir, 'notable.m'));
%! rmdir (fixdir);
%! assert_equal (exist (fixdir, 'dir'), 0);
