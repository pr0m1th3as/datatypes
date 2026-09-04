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
## Every subclass must implement all twenty-one.  Octave's classdef has no    ##
## 'methods (Abstract)' block, so the contract cannot be declared; these      ##
## raising defaults stand in for it, and name the subclass that is missing    ##
## one because 'class (this)' resolves downwards.                             ##
##                                                                            ##
## Nineteen of them concern row labels, which is the whole of what            ##
## separates one tabular class from another; the other two name the           ##
## properties object and build the result of an apply method.                 ##
##                                                                            ##
## 'hasRowLabels'      whether the object carries row labels at all           ##
## 'getRowLabels'      the labels themselves, in their own type               ##
## 'rowLabelName'      the name the labels are known by                       ##
## 'rowLabelStrings'   the labels rendered for display                        ##
## 'rowLabelHeader'    the heading printed over them, if any                  ##
## 'rowLabelKeyNames'  the names that mean "by the labels" rather than a var  ##
## 'sortsByLabelsByDefault'  whether a bare 'sortrows' orders by them         ##
## 'uniqueIncludesLabels'  whether they make a row distinct                   ##
## 'usableRowLabels'   which rows carry a label that can be used              ##
## 'fillSamplePoints'  the points the variables are sampled at                ##
## 'groupsByLabels'    whether a grouping reference may name them             ##
## 'rowLabelProperties'  the row label metadata, named as it is published     ##
## 'setRowLabelProperty'  one of those properties assigned                    ##
## 'subsetRowLabels'   the object with its labels subset by an index          ##
## 'clearRowLabels'    the object with its labels removed                     ##
## 'resolveRowRef'     a row reference resolved to row indices                ##
## 'makeProperties'    the properties object this class's metadata lives in   ##
## 'assembleApply'     an object built from an apply method's output          ##
## 'repeatRowLabels'   the labels with each row repeated N times              ##
## 'plainTable'        the variables as a table, the labels dropped           ##
## 'setRowLabels'      the object given a set of labels of its own type       ##
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

    ## Whether the row labels are still usable, one logical per row.  A
    ## table's row names never disqualify a row; a timetable's row times do
    ## when they are missing, there being no placing such a row in time.
    function tf = usableRowLabels (this)
      error (strcat ("%s: subclass must implement", ...
                     " usableRowLabels."), class (this));
    endfunction

    ## Whether a grouping reference may name the row labels instead of a
    ## variable.  A timetable's row times are an ordinary grouping key,
    ## named by the row dimension; a table's row names are not one, being
    ## unique and so grouping nothing together.
    function tf = groupsByLabels (this)
      error (strcat ("%s: subclass must implement", ...
                     " groupsByLabels."), class (this));
    endfunction

    ## The points the variables are sampled at, for the methods that
    ## interpolate or measure a distance.  A table has only the row order to
    ## go on; a timetable has its row times, which is why the same gap fills
    ## differently in the two.
    function [x, ownPoints, errmsg] = fillSamplePoints (this)
      error (strcat ("%s: subclass must implement", ...
                     " fillSamplePoints."), class (this));
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

    ## This object with LABELS as its row labels, given in the class's own
    ## type.  The counterpart of 'getRowLabels', for the few places that build
    ## a set of labels rather than subsetting the ones there are.
    function this = setRowLabels (this, labels)
      error (strcat ("%s: subclass must implement", ...
                     " setRowLabels."), class (this));
    endfunction

    ## The variables of this object as a plain table, its row labels dropped.
    ## A join lays the right operand's variables beside the left's, and only
    ## the left's labels survive, so the right side is reduced to this first.
    function out = plainTable (this)
      error (strcat ("%s: subclass must implement", ...
                     " plainTable."), class (this));
    endfunction

    ## This object with its row labels repeated N times, ELEMENTWISE placing
    ## each row's repeats together as 'repelem' does and otherwise repeating
    ## the whole block as 'repmat' does.  A class whose labels must be unique
    ## numbers the repeats; one whose labels may repeat leaves them alone.
    function this = repeatRowLabels (this, n, elementwise)
      error (strcat ("%s: subclass must implement", ...
                     " repeatRowLabels."), class (this));
    endfunction

    ## An object of this class assembled from the output of an apply method:
    ## VARS holds the variable values and NAMES their names.  Each class takes
    ## the row labels from the argument that means something to it and ignores
    ## the other: ROWLABELS holds them in the class's own type, empty where
    ## the result carries none, and ROWIX indexes the input row each output
    ## row takes its label from, empty where the caller has none to give.
    function out = assembleApply (this, vars, names, rowLabels, rowIx)
      error (strcat ("%s: subclass must implement", ...
                     " assembleApply."), class (this));
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
    ## agreeing on an order. -*- texinfo -*- @deftypefn {tabular} {[@var{index},
    ## @var{errmsg}] =} sortrowsIndex (@var{obj}, @var{args})
    ##
    ## Work out the row order @code{sortrows} asks for.
    ##
    ## @var{args} is the cell of arguments the public method was given, less the
    ## object itself.  The permutation comes back in @var{index}, and
    ## @var{errmsg} carries the body of any complaint so that the calling class
    ## can raise it under its own name.  The caller applies the order with
    ## @code{subsetrows}, which is where a subclass maintains whatever its row
    ## labels oblige it to.
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

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{ia}, @var{ic}, @var{errmsg}]
    ## =} uniqueIndex (@var{obj}, @var{args})
    ##
    ## Work out which rows @code{unique} keeps.
    ##
    ## @var{args} is the cell of arguments the public method was given, less the
    ## object itself.  @var{ia} indexes the rows to keep and @var{ic} maps every
    ## original row onto one of them, as @code{unique} returns them.
    ## @var{errmsg} carries the body of any complaint so that the calling class
    ## can raise it under its own name.
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

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{ixRows}, @var{errmsg}] =}
    ## headTailRows (@var{obj}, @var{k}, @var{fromEnd})
    ##
    ## The rows @code{head} or @code{tail} returns.
    ##
    ## @var{k} is the count asked for, empty for the default of eight, and
    ## @var{fromEnd} says which end to count from.  Asking for more rows than
    ## there are yields all of them, and asking for none yields none.
    ## @var{errmsg} carries the body of any complaint so that the calling class
    ## can raise it under its own name.
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

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{ixRows}, @var{errmsg}] =}
    ## topkrowsIndex (@var{obj}, @var{k}, @var{args})
    ##
    ## The rows @code{topkrows} keeps, in the order it reports them.
    ##
    ## @code{topkrows} is @code{sortrows} with a descending default and a count,
    ## so the ordering is worked out by @code{sortrowsIndex} and only the
    ## defaults differ: sorting runs downwards unless a direction is given, and
    ## missing keys go last rather than wherever the direction would put them.
    ## @var{errmsg} carries the body of any complaint so that the calling class
    ## can raise it under its own name.
    ##
    ## @end deftypefn
    function [ixRows, errmsg] = topkrowsIndex (this, k, args_in)

      ixRows = [];
      errmsg = '';

      ## Check for valid k
      if (! (isnumeric (k) && isscalar (k) && isreal (k) && isfinite (k)
             && fix (k) == k && k >= 0))
        errmsg = "K must be a nonnegative integer scalar.";
        return
      endif

      ## Split off any trailing Name-Value pairs, so that the positional
      ## arguments can be adjusted without disturbing them.
      optNames = {'MissingPlacement', 'ComparisonMethod'};
      nvStart = numel (args_in) + 1;
      for ii = 1:numel (args_in)
        if (ischar (args_in{ii}) && isrow (args_in{ii})
            && any (strcmp (args_in{ii}, optNames)))
          nvStart = ii;
          break;
        endif
      endfor
      pos = args_in(1:nvStart-1);
      nv = args_in(nvStart:end);

      ## With no explicit DIRECTION, that is with fewer than two positional
      ## arguments, enforce the descending default.
      if (numel (pos) < 2)
        if (numel (pos) == 0)
          ## No VARS.  A class whose rows carry labels ranks by them, as it
          ## does when sorting; every other ranks by all of its variables.
          if (sortsByLabelsByDefault (this))
            labelNames = rowLabelKeyNames (this);
            pos = {labelNames{1}, 'descend'};
          else
            pos = {':', 'descend'};
          endif
        elseif (isnumeric (pos{1}) && ! isempty (pos{1}))
          ## Signed numeric index: flip the sign convention relative to
          ## 'sortrows' so that a positive index ranks downwards and a
          ## negative index upwards.
          pos = {-pos{1}};
        else
          ## Named, logical, vartype, ':' or [] selection.
          pos = [pos, {'descend'}];
        endif
      endif

      ## Missing keys rank last however the sort runs, which is where
      ## 'topkrows' parts company with 'sortrows' and its 'auto'.
      if (! any (strcmp (nv, 'MissingPlacement')))
        nv = [nv, {'MissingPlacement', 'last'}];
      endif

      [index, errmsg] = sortrowsIndex (this, [pos, nv]);
      if (! isempty (errmsg))
        return
      endif
      if (k < numel (index))
        index = index(1:k);
      endif
      ixRows = index;

    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{TF}, @var{errmsg}] =}
    ## issortedrowsCheck (@var{obj}, @var{args})
    ##
    ## Whether the rows are already in the order asked for.
    ##
    ## @var{args} is the cell of arguments the public method was given, less the
    ## object itself, and takes the same forms @code{sortrows} does.  The sort
    ## is stable, so the rows are in that order exactly when sorting them would
    ## leave every one where it is.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [TF, errmsg] = issortedrowsCheck (this, args_in)

      TF = false;
      [index, errmsg] = sortrowsIndex (this, args_in);
      if (! isempty (errmsg))
        return
      endif
      TF = isequal (index(:)', 1:height (this));

    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## addvarsResult (@dots{})
    ##
    ## The object with variables added.
    ##
    ## Variables belong to a tabular object whatever labels its rows, so the
    ## work is the same for either class.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ## @var{argNames} carries the caller's own names for the new variables,
    ## which only the public method can read.

    ##
    ## @end deftypefn
    function [tbl, errmsg] = addvarsResult (this, argNames, varargin)

      tbl = this;
      errmsg = '';

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
        errmsg = "cannot use both 'After' and 'Before' options.";
        return
      endif
      ## All other errors will be handled by 'resolveVarRef' for invalid input
      msg_error1 = "LOCATION must index a single variable.";
      msg_error2 = strcat ("LOCATION must be either a", ...
                           " scalar integer, a character vector, or a", ...
                           " logical vector indexing a single table variable.");
      if (! isempty (After))
        if ((isnumeric (After) && isscalar (After)) || ischar (After) || ...
            (isa (After, 'string') && isscalar (After)))
          ix_insert = resolveVarRef (this, After);
        elseif (isvector (After) && islogical (After))
          ix_insert = resolveVarRef (this, After);
          if (numel (ix_insert) > 1)
            errmsg = msg_error1;
            return
          endif
        else
          errmsg = msg_error2;
          return
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
            errmsg = msg_error1;
            return
          endif
        else
          errmsg = msg_error2;
          return
        endif
      endif
      if (isempty (newVarNames))
        ## Create names for new variables
        offset = width (this);   # for incrementing automatic variable naming
        newVarNames = cell (size (args));
        for i = 1:numel (args)
          newVarNames{i} = argNames{i};
          ## A name taken from the caller's workspace is deconflicted rather
          ## than refused: the dimension names share the variables' namespace.
          if (any (strcmp (newVarNames{i}, this.DimensionNames)))
            suffix = 1;
            while (any (strcmp (sprintf ("%s_%d", argNames{i}, suffix), ...
                                [this.VariableNames, this.DimensionNames])))
              suffix++;
            endwhile
            newVarNames{i} = sprintf ("%s_%d", argNames{i}, suffix);
          endif
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
        clash = newVarNames(ismember (newVarNames, this.DimensionNames));
        if (! isempty (clash))
          errmsg = sprintf ("duplicate dimension and variable name: '%s'.", ...
                            clash{1});
          return
        endif
        if (numel (args) != numel (newVarNames))
          errmsg = strcat ("NEWNAMES does not match the", ...
                         " number of new variables.");
          return
        endif
        if (numel (__unique__ (newVarNames)) != numel (newVarNames))
          errmsg = "NEWNAMES contains duplicate names.";
          return
        endif
        idx = ismember (newVarNames, this.VariableNames);
        if (any (idx))
          if (sum (idx) == 1)
            errmsg = sprintf ("new variable name '%s' already exists.", ...
                   newVarNames{idx});
            return
          else
            msg_error3 = sprintf ("'%s', ", newVarNames{idx});
            msg_error3(end-1:end) = [];
            errmsg = sprintf ("new variable names %s already exist.", ...
                   msg_error3);
            return
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

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## movevarsResult (@dots{})
    ##
    ## The object with a variable moved.
    ##
    ## Variables belong to a tabular object whatever labels its rows, so the
    ## work is the same for either class.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, errmsg] = movevarsResult (this, vars, varargin)

      tbl = this;
      errmsg = '';

      ## Check input argument
      if (nargin < 2 || isempty (vars))
        errmsg = "too few input arguments.";
        return
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
        errmsg = "cannot use both 'After' and 'Before' options.";
        return
      endif

      ## All other errors will be handled by 'resolveVarRef' for invalid input
      msg_error1 = "LOCATION must index a single variable.";
      msg_error2 = strcat ("LOCATION must be either a", ...
                           " scalar integer, a character vector, or a", ...
                           " logical vector indexing a single table variable.");
      msg_error3 = strcat ("LOCATION does not index an", ...
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
            errmsg = msg_error1;
            return
          endif
        else
          errmsg = msg_error2;
          return
        endif
        ## Grab silent errors returned by 'resolveVarRef'
        if (any (ix_insert == 0))
          errmsg = msg_error3;
          return
        endif
      endif

      ## Get variables to be moved
      mvVar = resolveVarRef (this, vars, 'lenient');
      if (any (mvVar == 0))
        vars = cellstr (vars);
        errmsg = sprintf ("cannot index non-existing variable: '%s'", ...
               vars{find (mvVar == 0)});
        return
      endif

      ## Get variables that remain static
      stVar = 1:tbl_width;
      stVar(mvVar) = [];

      ## Construct remapping vector
      if (AB_insert)  # after
        if (ix_insert < tbl_width)
          ## Check LOCATION variable is a static one
          if (ismember (ix_insert, mvVar))
            errmsg = "LOCATION variable cannot be moved.";
            return
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
            errmsg = "LOCATION variable cannot be moved.";
            return
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

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## renamevarsResult (@dots{})
    ##
    ## The object with variables renamed.
    ##
    ## Variables belong to a tabular object whatever labels its rows, so the
    ## work is the same for either class.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, errmsg] = renamevarsResult (this, vars, newNames)

      tbl = this;
      errmsg = '';

      ## Check input arguments
      if (nargin < 3 || isempty (vars) || isempty (newNames))
        errmsg = "too few input arguments.";
        return
      endif
      if (! iscellstr (newNames) && ! isa (newNames, 'string') &&
          ! (ischar (newNames) && isvector (newNames)))
        errmsg = strcat ("NEWNAMES must be either a", ...
                       " character vector, a cell array of character", ...
                       " vectors, or a string array.");
        return
      endif

      ## Force to cellstring and get indices
      newNames = cellstr (newNames);
      ## The dimension names share the variables' namespace.
      clash = newNames(ismember (newNames, this.DimensionNames));
      if (! isempty (clash))
        errmsg = sprintf ("duplicate dimension and variable name: '%s'.", ...
                          clash{1});
        return
      endif
      if (numel (__unique__ (newNames)) != numel (newNames))
        errmsg = "NEWNAMES contains duplicate names.";
        return
      endif
      ixVars = resolveVarRef (this, vars, 'lenient');

      ## Check selected variables
      if (any (ixVars == 0))
        errmsg = sprintf ("cannot index non-existing variable: '%s'",...
               vars{find (ixVars == 0)});
        return
      elseif (numel (ixVars) != numel (newNames))
        errmsg = strcat ("number of names in NEWNAMES do", ...
                       " not match the selected variables specified by", ...
                       " VARS.");
        return
      endif

      ## Rename the indexed variables
      tbl = this;
      tbl.VariableNames(ixVars) = newNames;

      ## Check for duplicate names
      if (numel (__unique__ (tbl.VariableNames)) != numel (tbl.VariableNames))
        errmsg = strcat ("newly assigned variable name", ...
                       " already exists.");
        return
      endif
    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## removevarsResult (@dots{})
    ##
    ## The object with variables removed.
    ##
    ## Variables belong to a tabular object whatever labels its rows, so the
    ## work is the same for either class.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, errmsg] = removevarsResult (this, vars)

      tbl = this;
      errmsg = '';

      ## Check input argument
      if (nargin < 2 || isempty (vars))
        errmsg = "too few input arguments.";
        return
      endif

      ## Resolve variables to be removed
      ixVar = resolveVarRef (this, vars);

      ## Remove selected variables
      nrows = height (this);
      tbl = this;
      tbl.VariableTypes(ixVar) = [];
      tbl.VariableNames(ixVar) = [];
      tbl.VariableValues(ixVar) = [];
      tbl.VariableDescriptions(ixVar) = [];
      tbl.VariableUnits(ixVar) = [];
      if (! isempty (this.VariableContinuity))
        tbl.VariableContinuity(ixVar) = [];
        if (isempty (tbl.VariableContinuity))
          tbl.VariableContinuity = [];
        endif
      endif

      ## Check for custom variable properties and remove accordingly
      if (! isempty (this.CustomProperties))
        cpNames = customPropsOfType (this, 'variable');
        if (! isempty (cpNames))
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
      tbl = setRowCount (tbl, nrows);
    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## convertvarsResult (@dots{})
    ##
    ## The object with variables converted.
    ##
    ## Variables belong to a tabular object whatever labels its rows, so the
    ## work is the same for either class.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, errmsg] = convertvarsResult (this, vars, dataType)

      tbl = this;
      errmsg = '';

      ## Check input arguments
      if (nargin < 3 || isempty (vars) || isempty (dataType))
        errmsg = "too few input arguments.";
        return
      endif

      if (ischar (dataType))
        if (! isvector (dataType))
          errmsg = "DATATYPE must be a character vector.";
          return
        endif
      elseif (! isa (dataType, 'function_handle'))
        errmsg = sprintf (strcat ("DATATYPE must be either a", ...
                       " character vector or a function handle; got a", ...
                       " '%s'."), ...
               class (dataType));
        return
      endif

      ## Get variables to convert (input validation is done by 'resolveVarRef')
      [ixVars, varNames] = resolveVarRef (this, vars);
      tbl = this;

      ## Apply conversion
      for i = 1:numel (ixVars)
        try
          newVarValue = feval (dataType, this.VariableValues{ixVars(i)});
        catch
          errmsg = sprintf (strcat ("specified DATATYPE", ...
                         " conversion cannot be applied on selected", ...
                         " variable '%s'."), ...
                 varNames{i});
          return
        end_try_catch
        if (size (newVarValue, 1) != height (this))
          errmsg = sprintf (strcat ("specified DATATYPE", ...
                         " conversion on '%s' does not return the", ...
                         " appropriate amount of rows."), ...
                 varNames{i});
          return
        endif

        ## Write output
        tbl.VariableTypes{ixVars(i)} = class (newVarValue);
        tbl.VariableValues{ixVars(i)} = newVarValue;
      endfor

    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## mergevarsResult (@dots{})
    ##
    ## The object with variables merged.
    ##
    ## Variables belong to a tabular object whatever labels its rows, so the
    ## work is the same for either class.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, errmsg] = mergevarsResult (this, vars, varargin)

      tbl = this;
      errmsg = '';

      ## Check input argument
      if (nargin < 2 || isempty (vars))
        errmsg = "too few input arguments.";
        return
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'NewVariableName', 'MergeAsTable'};
      dfValues = {[], false};
      [newVarName, mergeAsTable] = parsePairedArguments (optNames, dfValues, ...
                                                         varargin(:));

      ## Check user input for 'MergeAsTable'
      if (! isscalar (mergeAsTable))
        errmsg = "invalid input for 'MergeAsTable'.";
        return
      endif
      if (! (isbool (mergeAsTable) || ismember (mergeAsTable, [0, 1])))
        errmsg = "invalid input for 'MergeAsTable'.";
        return
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
          errmsg = "invalid input for 'NewVariableName'.";
          return
        endif
      endif

      ## Gather remaining variables to be copied unaltered
      ixRem = 1:width (this);
      ixRem(ixVars) = [];
      tbl = subsetvars (this, ixRem);

      ## Check that new variable name does not conflict any existing variable
      if (ismember (newVarName, tbl.VariableNames))
        errmsg = "assigned 'NewVariableName' already exists.";
        return
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
          errmsg = strcat ("selected variables cannot be", ...
                         " merged into a multicolumn variable due to", ...
                         " incompatible variable types.");
          return
        end_try_catch
        tbl.VariableTypes{location} = class (newVarValue);
        tbl.VariableValues{location} = newVarValue;
        tbl.VariableNames(location) = newVarName;
        ## The merged variable is a new one, so its continuity is unset even
        ## though the first merged variable's slot is being reused.
        ## Units and descriptions do not survive a merge: one variable
        ## carries one of each and there is no saying which it should be.
        if (! isempty (tbl.VariableUnits))
          tbl.VariableUnits{location} = '';
        endif
        if (! isempty (tbl.VariableDescriptions))
          tbl.VariableDescriptions{location} = '';
        endif
        if (! isempty (tbl.VariableContinuity))
          tbl.VariableContinuity{location} = 'unset';
        endif
      endif

    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## splitvarsResult (@dots{})
    ##
    ## The object with a variable split.
    ##
    ## Variables belong to a tabular object whatever labels its rows, so the
    ## work is the same for either class.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, errmsg] = splitvarsResult (this, varargin)

      tbl = this;
      errmsg = '';

      ## Check max number of input arguments
      if (nargin > 4)
        errmsg = "too many input arguments.";
        return
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
                errmsg = strcat ("invalid input for", ...
                               " 'NewVariableNames'.");
                return
              endif
              idc += 1;
            elseif (iscellstr (newNames) && idc == 1)
              varNames = newNames;
            else
              errmsg = "invalid input for 'NewVariableNames'.";
              return
            endif
            if (numel (varNames) != col)
              errmsg = "wrong number of 'NewVariableNames'.";
              return
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
              ## Variable-scoped custom properties are already replicated to the
              ## split columns by subsetvars via the repeated indices in
              ## 'ix_remap', so no further handling is needed here.
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

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{TF}, @var{errmsg}] =}
    ## ismissingResult (@dots{})
    ##
    ## Which elements of the variables are missing.
    ##
    ## Missing values belong to the variables, which a tabular object holds the
    ## same way whatever labels its rows.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [TF, errmsg] = ismissingResult (this, varargin)

      TF = [];
      errmsg = '';

      ## Parse optional Name-Value paired arguments
      optNames = {'OutputFormat'};
      dfValues = {'logical'};
      [outFmt, indicator] = parsePairedArguments (optNames, dfValues, ...
                                                  varargin(:));

      if (! any (strcmpi (outFmt, {'logical', 'tabular'})))
        errmsg = "invalid value for 'OutputFormat'.";
        return
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
          errmsg = "INDICATOR must be a vector.";
          return
        endif
        ## NaN values for calendarDuration and duration
        nan_calendarDuration = nan_duration = false;
        ## Preprocess indicator if it is a cell array
        if (iscell (indicator) && ! iscellstr (indicator))
          ## Elements in indicator vector must be scalars (except char vectors)
          fcn = @(x) isscalar (x) | isempty (x) | (ischar (x) & isvector (x));
          all_scalar = all (cellfun (fcn, indicator));
          if (! all_scalar)
            errmsg = strcat ("INDICATOR must explicitly", ...
                           " contain scalar elements or character", ...
                           " vectors.");
            return
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

      ## Return appropriate OutputFormat.  The array comes from the shared
      ## accessor rather than from 'table2array', which only a table has.
      if (strcmpi (outFmt, 'logical'))
        TF = varsAsArray (this, 'ismissing');
      else
        TF = this;
      endif

    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{TF}, @var{errmsg}]
    ## =} rmmissingResult (@dots{})
    ##
    ## The object with its incomplete rows removed.
    ##
    ## Missing values belong to the variables, which a tabular object holds the
    ## same way whatever labels its rows.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, TF, errmsg] = rmmissingResult (this, varargin)

      tbl = this;
      TF = [];
      errmsg = '';
      ## A row whose label disqualifies it goes whatever the data
      ## says, and whatever 'DataVariables' or 'MinNumMissing' say.
      usable = usableRowLabels (this);
      ## Handle simple input argument first
      if (numel (varargin) == 0)
        TF = any (ismissing (this), 2);
        TF = TF | ! usable;
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
        errmsg = "'MinNumMissing' must be a positive integer.";
        return
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
          errmsg = sprintf (strcat ("'DataVariables' index a", ...
                         " non-existing variable: '%s'"), badname);
          return
        endif
        tmpT = subsetvars (this, dIxVars);
      else
        tmpT = this;
      endif
      if (! isempty (mLocs))
        if (islogical (mLocs))
          if (! isequal (size (mLocs), size (tmpT)))
            errmsg = strcat ("'MissingLocations' must be", ...
                           " a logical matrix of the same size as the", ...
                           " input table or the part of it referenced by", ...
                           " 'DataVariables'.");
            return
          endif
          TF = sum (mLocs, 2) >= minNum;
          TF = TF | ! usable;
          tbl = subsetrows (this, ! TF);
        elseif (isa (mLocs, 'table'))
          if (! all (ismember (tmpT.VariableNames, mLocs.VariableNames)))
            errmsg = strcat ("'MissingLocations' must be", ...
                           " a table with the same variable names as the", ...
                           " input table or the part of it referenced by", ...
                           " 'DataVariables'.");
            return
          endif
          TF = false (rows (this), 0);
          for jx = 1:width (tmpT)
            kx = find (strcmp (tmpT.VariableNames{jx}, mLocs.VariableNames), 1);
            varTF = mLocs.VariableValues{kx};
            if (! islogical (varTF))
              errmsg = strcat ("'MissingLocations' must", ...
                             " be a table with logical variables.");
              return
            endif
            if (! isequal (size (varTF), size (tmpT.VariableValues{jx})))
              errmsg = strcat ("'MissingLocations' must", ...
                             " be a table with the same variable sizes", ...
                             " as the input table or the part of it", ...
                             " referenced by 'DataVariables'.");
              return
            endif
            TF = [TF, any(varTF, 2)];
          endfor
          TF = sum (TF, 2) >= minNum;
          TF = TF | ! usable;
          tbl = subsetrows (this, ! TF);
        else
          errmsg = "invalid data type for 'MissingLocations'.";
          return
        endif
      else
        TF = sum (ismissing (tmpT), 2) >= minNum;
        TF = TF | ! usable;
        tbl = subsetrows (this, ! TF);
      endif

    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{TF}, @var{errmsg}]
    ## =} fillmissingResult (@dots{})
    ##
    ## The object with its missing values filled.
    ##
    ## Missing values belong to the variables, which a tabular object holds the
    ## same way whatever labels its rows.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, TF, errmsg] = fillmissingResult (tblA, varargin)

      tbl = tblA;
      TF = [];
      errmsg = '';

      ## Check input arguments
      if (nargin < 2)
        errmsg = "too few input arguments.";
        return
      endif

      ## Resolve the fill method (and the value for the 'constant' method)
      method = varargin{1};
      if (isa (method, 'string') && isscalar (method))
        method = char (method);
      endif
      if (! (ischar (method) && isrow (method)))
        errmsg = "METHOD must be a character vector.";
        return
      endif
      method = lower (method);
      rest = varargin(2:end);
      constVal = [];
      switch (method)
        case 'constant'
          if (isempty (rest))
            errmsg = strcat ("the 'constant' method", ...
                           " requires a fill value.");
            return
          endif
          constVal = rest{1};
          rest = rest(2:end);
        case {'previous', 'next', 'nearest', 'linear', 'spline', ...
              'pchip', 'makima'}
          ## supported; no extra positional argument
        case {'movmean', 'movmedian', 'knn', 'mean', 'median', 'mode'}
          errmsg = sprintf (strcat ("method '%s' is not supported", ...
                         " yet."), method);
          return
        otherwise
          errmsg = sprintf ("unknown method '%s'.", method);
          return
      endswitch

      ## Parse optional Name-Value paired arguments
      ## Where the rows sit, which decides how far apart a gap's neighbours
      ## are and what an interpolation runs against.  Read before anything is
      ## filled, so a timetable that cannot say is refused whatever the method.
      [x, ownPoints, errmsg] = fillSamplePoints (tblA);
      if (! isempty (errmsg))
        return
      endif

      optNames = {'DataVariables', 'EndValues', 'ReplaceValues'};
      dfValues = {[], 'extrap', true};
      [dVars, endVals, replace, extra] = parsePairedArguments (optNames, ...
                                                    dfValues, rest(:));
      ## Anything left over was asked for and not understood.  Saying so
      ## beats filling to a rule the caller did not ask for, which is what a
      ## misspelt option would otherwise get.
      if (! isempty (extra))
        name = extra{1};
        if (isa (name, 'string') && isscalar (name))
          name = char (name);
        endif
        if (ischar (name) && isrow (name) && strcmpi (name, 'SamplePoints'))
          if (ownPoints)
            errmsg = strcat ("'SamplePoints' is not accepted here; the", ...
                             " row times are the sample points.");
          else
            errmsg = "'SamplePoints' is not supported yet.";
          endif
        elseif (ischar (name) && isrow (name))
          errmsg = sprintf ("unknown option '%s'.", name);
        else
          errmsg = "unknown optional argument.";
        endif
        return
      endif
      if (! (islogical (replace) && isscalar (replace)))
        errmsg = "'ReplaceValues' must be a logical scalar.";
        return
      endif
      if (! replace)
        errmsg = strcat ("'ReplaceValues' set to false is", ...
                       " not supported yet.");
        return
      endif
      ## 'EndValues' is checked here rather than where the end gaps are
      ## filled, so that a bad value is refused whatever the data looks like.
      ## A keyword names how the ends are filled; anything else is a constant,
      ## which must be a scalar and is type-checked against each variable when
      ## it is written.
      endKeys = {'extrap', 'none', 'previous', 'next', 'nearest'};
      if (ischar (endVals) || (isa (endVals, 'string') && isscalar (endVals)))
        endVals = lower (char (endVals));
        if (! any (strcmp (endVals, endKeys)))
          errmsg = strcat ("'EndValues' must be 'extrap',", ...
                         " 'previous', 'next', 'nearest', 'none', or a", ...
                         " scalar constant.");
          return
        endif
      elseif (! isscalar (endVals))
        errmsg = "'EndValues' constant must be a scalar.";
        return
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
          errmsg = sprintf (strcat ("'DataVariables' index a", ...
                         " non-existing variable: '%s'"), badname);
          return
        endif
      endif

      ## Every targeted variable must have a type the 'linear' method can
      ## interpolate, and that is settled before anything is filled: the call
      ## is refused whether or not the variable has an entry missing.
      if (strcmp (method, 'linear'))
        for k = 1:numel (ixVars)
          v = tblA.VariableValues{ixVars(k)};
          if (isnumeric (v) || islogical (v) || isdatetime (v) ...
              || isduration (v))
            continue;
          endif
          errmsg = sprintf (strcat ("the 'linear' method does not", ...
                         " support the type of table variable '%s'."), ...
                 tblA.VariableNames{ixVars(k)});
          return
        endfor
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
        M = __varmissing__ (v);
        if (! any (M(:)))
          continue;
        endif
        filled = false (size (M));
        v0 = v;
        if (strcmp (method, 'constant'))
          [v, filled] = fill_constant (v, M, fillVals{k}, ...
                                       tbl.VariableNames{iv});
        else
          for c = 1:columns (M)
            m = M(:,c);
            if (! any (m))
              continue;
            endif
            if (any (strcmp (method, {'linear', 'spline', 'pchip', ...
                                      'makima'})))
              [v(:,c), filled(:,c)] = fill_interp (v(:,c), m, x, method);
            else
              si = fill_neighbor_idx (m, method, x);
              rows = m & si > 0;
              v(rows,c) = v(si(rows),c);
              filled(:,c) = rows;
            endif
          endfor
        endif
        ## Every method has now filled the end gaps the way it extrapolates,
        ## which is what 'extrap' asks for.  Any other 'EndValues' overrides
        ## them, whatever the method was.
        if (! (ischar (endVals) && strcmp (endVals, 'extrap')))
          for c = 1:columns (M)
            if (! any (M(:,c)))
              continue;
            endif
            [v(:,c), filled(:,c)] = apply_end_values (v(:,c), v0(:,c), ...
                                      M(:,c), filled(:,c), endVals, ...
                                      tbl.VariableNames{iv});
          endfor
        endif
        tbl.VariableValues{iv} = v;
        TF(:,iv) = any (filled, 2);
      endfor

    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{tbl}, @var{errmsg}] =}
    ## standardizeMissingResult (@dots{})
    ##
    ## The object with the given values made missing.
    ##
    ## Missing values belong to the variables, which a tabular object holds the
    ## same way whatever labels its rows.  @var{errmsg} carries the body of any
    ## complaint so that the calling class can raise it under its own name.
    ##
    ## @end deftypefn
    function [tbl, errmsg] = standardizeMissingResult (tblA, indicator, ...
                                                       varargin)

      tbl = tblA;
      errmsg = '';

      ## Check input arguments
      if (nargin < 2)
        errmsg = "too few input arguments.";
        return
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
          errmsg = sprintf (strcat ("'DataVariables' index", ...
                         " a non-existing variable: '%s'"), badname);
          return
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

    ## The comparison behind 'isequal' and 'isequaln' on both classes.  It
    ## compares the properties it names and nothing else, so the exclusions
    ## are the ones absent from the list: 'VariableTypes', which only
    ## restates the class of each variable and would make a table holding
    ## int8 unequal to one holding the same values as double, where the
    ## values themselves compare equal; and a timetable's 'TimeStep',
    ## 'SampleRate', 'StartTime' and step provenance, which describe the row
    ## times rather than adding to them, so a timetable told its step equals
    ## one that read the same step off the times it was given.  OTHERS holds
    ## the remaining arguments, already known to be of this same class.
    ## NANEQUAL selects 'isequaln' wherever a user value can sit.
    function TF = isequalResult (this, others, nanEqual)
      if (nanEqual)
        eqf = @isequaln;
      else
        eqf = @isequal;
      endif
      TF = false;
      for i = 1:numel (others)
        B = others{i};
        if (! isequal (size (this), size (B)))
          return;
        endif
        if (! (isequal (this.VariableNames, B.VariableNames)
               && isequal (this.DimensionNames, B.DimensionNames)
               && isequal (this.Description, B.Description)
               && isequal (this.VariableDescriptions, B.VariableDescriptions)
               && isequal (this.VariableUnits, B.VariableUnits)
               && isequal (this.VariableContinuity, B.VariableContinuity)
               && isequal (this.CustomPropTypes, B.CustomPropTypes)))
          return;
        endif
        if (! (eqf (this.UserData, B.UserData)
               && eqf (this.CustomProperties, B.CustomProperties)))
          return;
        endif
        if (hasRowLabels (this) != hasRowLabels (B))
          return;
        endif
        if (hasRowLabels (this)
            && ! eqf (getRowLabels (this), getRowLabels (B)))
          return;
        endif
        for j = 1:numel (this.VariableValues)
          if (! eqf (this.VariableValues{j}, B.VariableValues{j}))
            return;
          endif
        endfor
      endfor
      TF = true;
    endfunction

    ## The body behind 'varfun' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.
    function [B, errmsg] = varfunResult (this, func, args_in)
      B = [];
      errmsg = '';
      scope = sprintf ('%s.varfun', class (this));
      if (! is_function_handle (func))
        errmsg = 'FUNC must be a function handle.';
        return;
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'InputVariables', 'GroupingVariables', 'OutputFormat', ...
                  'ErrorHandler'};
      dfValues = {[], [], 'auto', []};
      [inVars, grpVars, outFmt, errHandler] = ...
                  parsePairedArguments (optNames, dfValues, args_in(:));
      outFmt = tabular.check_output_format (scope, outFmt, ...
                                            class (this));
      if (! isempty (errHandler) && ! is_function_handle (errHandler))
        errmsg = "'ErrorHandler' must be a function handle.";
        return;
      endif

      ## Resolve grouping variables and input variables (default input is
      ## every variable that is not a grouping variable).
      byLabels = false;
      gIx = [];
      if (! isempty (grpVars))
        [gIx, byLabels] = resolveGroupRef (this, grpVars);
      endif
      if (isempty (inVars))
        iIx = 1:width (this);
        iIx(ismember (iIx, gIx)) = [];
      else
        iIx = resolveVarRef (this, inVars)(:)';
      endif
      if (isempty (iIx))
        errmsg = 'there are no variables to which to apply FUNC.';
        return;
      endif

      ## Build the output variable names from the function and variable names.
      inNames = this.VariableNames(iIx);
      fname = tabular.apply_func_name (func);
      outNames = strcat (fname, '_', inNames);

      if (isempty (gIx) && ! byLabels)
        ## Ungrouped: apply FUNC to each whole variable.
        res = cell (1, numel (iIx));
        for k = 1:numel (iIx)
          col = this.VariableValues{iIx(k)};
          what = sprintf ("the variable '%s'", inNames{k});
          out = tabular.apply_func (func, errHandler, k, 1, {col}, scope, ...
                                    what);
          res{1,k} = out{1};
        endfor
        B = build_apply_result (this, scope, outFmt, res, outNames, {}, ...
                               {}, [], {}, []);
      else
        ## Grouped: apply FUNC to each group's slice of each variable.  The
        ## row labels group as a column of their own when they were named,
        ## and are reported as row labels of the result rather than as a
        ## grouping variable of it.
        inCols = this.VariableValues(iIx);
        grpCols = this.VariableValues(gIx);
        if (byLabels)
          labels = getRowLabels (this);
          grpCols = [{labels}, grpCols];
        endif
        [G, ng, repRows, gerr] = tabular.group_table_rows (grpCols);
        if (! isempty (gerr))
          errmsg = gerr;
          return;
        endif
        res = cell (ng, numel (iIx));
        for g = 1:ng
          rows = (G == g);
          for k = 1:numel (iIx)
            col = inCols{k};
            what = sprintf ("the variable '%s'", inNames{k});
            out = tabular.apply_func (func, errHandler, g, 1, ...
                                      {col(rows,:)}, scope, what);
            res{g,k} = out{1};
          endfor
        endfor
        [gcols, gcount] = tabular.group_output_cols ( ...
                              this.VariableValues(gIx), G, repRows);
        B = build_grouped_apply_result (this, scope, outFmt, res, outNames, ...
                                        gcols, this.VariableNames(gIx), ...
                                        gcount, repRows);
      endif
    endfunction

    ## The body behind 'repelem' and 'repmat' on both classes.  ELEMENTWISE
    ## selects between them: 'repelem' repeats each row and each variable in
    ## place, 'repmat' repeats the whole block.  Returns an errmsg body (empty
    ## on success) for the caller to raise under its own name.
    function [tbl, errmsg] = repeatResult (this, args, elementwise)
      tbl = [];
      errmsg = '';
      nargs = numel (args);
      if (elementwise)
        ## 'repelem' takes a count per dimension and nothing else: neither a
        ## lone count nor a size vector, a tabular object having exactly two
        ## dimensions and both being named.
        if (nargs != 2)
          errmsg = 'exactly three input arguments are required.';
          return;
        endif
        rows = args{1};
        cols = args{2};
        for v = {rows, cols}
          k = v{1};
          if (! (isnumeric (k) && isscalar (k) && isreal (k)
                 && k >= 0 && k == fix (k)))
            errmsg = strcat ("replication factors must be nonnegative", ...
                             " integer-valued scalars.");
            return;
          endif
        endfor
      else
        ## 'repmat' takes a lone count for both dimensions, a count for each,
        ## or the two of them as a size vector.
        if (nargs < 1)
          errmsg = 'too few input arguments.';
          return;
        endif
        if (nargs > 2)
          errmsg = 'only 2 dimensions are supported.';
          return;
        endif
        if (nargs == 1)
          sz = args{1};
          if (! isnumeric (sz))
            errmsg = strcat ("replication factors must be a row vector of", ...
                             " integers or integer scalars.");
            return;
          endif
          if (isscalar (sz))
            rows = cols = sz;
          elseif (numel (sz) == 2)
            rows = sz(1);
            cols = sz(2);
          else
            errmsg = 'only 2 dimensions are supported.';
            return;
          endif
        else
          rows = args{1};
          cols = args{2};
        endif
        for v = {rows, cols}
          k = v{1};
          if (! (isnumeric (k) && isscalar (k) && isreal (k)
                 && k == fix (k)))
            errmsg = strcat ("replication factors must be a row vector of", ...
                             " integers or integer scalars.");
            return;
          endif
        endfor
        ## A negative count repeats nothing, as it does for an array.
        rows = max (rows, 0);
        cols = max (cols, 0);
      endif

      tbl = this;
      ## Replicate the rows by the index they come from, then hand the row
      ## labels to the subclass, which alone knows whether a repeated label
      ## needs a name of its own.
      if (rows != 1)
        nrow = height (this);
        if (elementwise)
          ixRows = repelem ((1:nrow)', rows, 1);
        else
          ixRows = repmat ((1:nrow)', rows, 1);
        endif
        if (width (this) == 0)
          ## No variable carries the height, so the stored count does.
          tbl.RowCount = numel (ixRows);
        endif
        for i = 1:width (this)
          col = this.VariableValues{i};
          tbl.VariableValues{i} = col(ixRows,:);
        endfor
        tbl = repeatRowLabels (tbl, rows, elementwise);
      endif

      ## Replicate the variables the same way, numbering the repeats in the
      ## order they come out.  The height is carried across, since a result
      ## with no variables left has nothing else to carry it.
      if (cols != 1)
        nrowsOut = height (tbl);
        nvar = width (this);
        if (elementwise)
          ixVars = repelem ((1:nvar)', cols, 1)';
        else
          ixVars = repmat ((1:nvar)', cols, 1)';
        endif
        tbl.VariableTypes = tbl.VariableTypes(ixVars);
        tbl.VariableValues = tbl.VariableValues(ixVars);
        tbl.VariableDescriptions = tbl.VariableDescriptions(ixVars);
        tbl.VariableUnits = tbl.VariableUnits(ixVars);
        if (! isempty (tbl.VariableContinuity))
          tbl.VariableContinuity = tbl.VariableContinuity(ixVars);
        endif
        newNames = cell (1, numel (ixVars));
        seen = zeros (1, nvar);
        for k = 1:numel (ixVars)
          v = ixVars(k);
          seen(v)++;
          if (seen(v) == 1)
            newNames{k} = this.VariableNames{v};
          else
            newNames{k} = sprintf ('%s_%d', this.VariableNames{v}, ...
                                   seen(v) - 1);
          endif
        endfor
        tbl.VariableNames = newNames;
        ## Handle custom variable properties
        if (! isempty (this.CustomProperties))
          cp_names = customPropsOfType (this, 'variable');
          for i = 1:numel (cp_names)
            nm = cp_names{i};
            val = tbl.CustomProperties.(nm);
            tbl.CustomProperties.(nm) = val(ixVars);
          endfor
        endif
        tbl = setRowCount (tbl, nrowsOut);
      endif
    endfunction

    ## The body behind 'groupsummary' on both classes.  Returns an errmsg
    ## body (empty on success) for the caller to raise under its own name.
    ## Like 'groupcounts' the result is a table whatever the input was, its
    ## rows describing groups rather than the rows they came from.
    function [G, errmsg] = groupsummaryResult (this, groupvars, args_in)
      G = [];
      errmsg = '';
      scope = sprintf ('%s.groupsummary', class (this));

      ## Split the trailing arguments into the optional positional METHOD and
      ## DATAVARS arguments and any Name-Value pairs.  A Name-Value region
      ## starts at the first char-vector/string that names a known option.
      optNames = {'IncludeMissingGroups', 'IncludeEmptyGroups', ...
                  'IncludedEdge'};
      nvStart = numel (args_in) + 1;
      for k = 1:numel (args_in)
        a = args_in{k};
        if ((ischar (a) && isrow (a)) || (isa (a, 'string') && isscalar (a)))
          if (any (strcmpi (char (a), optNames)))
            nvStart = k;
            break;
          endif
        endif
      endfor
      posArgs = args_in(1:nvStart-1);
      nvArgs = args_in(nvStart:end);
      ## An optional GROUPBINS positional argument precedes METHOD.
      hasGroupbins = false;
      groupbins = [];
      if (! isempty (posArgs) && __groupbins__ ('is_spec', posArgs{1}))
        hasGroupbins = true;
        groupbins = posArgs{1};
        posArgs = posArgs(2:end);
      endif
      if (numel (posArgs) > 2)
        errmsg = 'too many positional arguments.';
        return;
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
      if (! (isscalar (incMiss)
             && (islogical (incMiss) || isnumeric (incMiss))))
        errmsg = "'IncludeMissingGroups' must be a logical scalar.";
        return;
      endif
      incMiss = logical (incMiss);
      if (! (isscalar (incEmpty)
             && (islogical (incEmpty) || isnumeric (incEmpty))))
        errmsg = "'IncludeEmptyGroups' must be a logical scalar.";
        return;
      endif
      incEmpty = logical (incEmpty);
      incEdge = tabular.check_included_edge (scope, incEdge);

      ## Normalise METHOD into parallel cell arrays of method specs and the
      ## display names used to build output variable names.
      [methods, methNames, errmsg] = tabular.gs_normalise_methods (method);
      if (! isempty (errmsg))
        return;
      endif

      ## Resolve grouping and data variables.  The default data variables are
      ## all variables that are not grouping variables.
      [gIx, byLabels] = resolveGroupRef (this, groupvars);
      if (isempty (gIx) && ! byLabels)
        errmsg = 'at least one grouping variable is required.';
        return;
      endif
      if (hasDataVars)
        dIx = resolveVarRef (this, datavars)(:)';
      else
        dIx = 1:width (this);
        dIx(ismember (dIx, gIx)) = [];
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given.  The
      ## row labels group as a column of their own when they were named.
      grpCols = this.VariableValues(gIx);
      grpNames = this.VariableNames(gIx);
      if (byLabels)
        labels = getRowLabels (this);
        lname = rowLabelName (this);
        grpCols = [{labels}, grpCols];
        grpNames = [{lname}, grpNames];
      endif
      if (hasGroupbins)
        [grpCols, grpNames, errmsg] = __groupbins__ ('bin', grpCols, ...
                                     grpNames, groupbins, incEdge, ...
                                     'groupsummary');
        if (! isempty (errmsg))
          return;
        endif
      endif

      ## Group the rows, treating missing grouping values as their own groups
      ## (sorted last) when IncludeMissingGroups is true; IncludeEmptyGroups
      ## adds the unused categories of a categorical or binned grouping
      ## variable as empty groups.
      [Grp, ng, gcols, errmsg] = tabular.gs_grouping (grpCols, incMiss, ...
                                                      incEmpty);
      if (! isempty (errmsg))
        return;
      endif
      gcount = accumarray (Grp(! isnan (Grp)), 1, [ng, 1]);

      ## Compute each method over each data variable.  Output columns are
      ## ordered data variable first, then method, to match MATLAB's order.
      datNames = this.VariableNames(dIx);
      rescols = {};
      resNames = {};
      for di = 1:numel (dIx)
        for mi = 1:numel (methods)
          col = this.VariableValues{dIx(di)};
          vals = cell (ng, 1);
          for g = 1:ng
            rows = (Grp == g);
            [v, merr] = tabular.gs_apply_method (methods{mi}, col(rows,:));
            if (! isempty (merr))
              errmsg = sprintf ("variable '%s': %s", datNames{di}, merr);
              return;
            endif
            vals{g} = v;
          endfor
          try
            rescols{end+1} = vertcat (vals{:});
          catch
            errmsg = sprintf (strcat ("the '%s' results for variable '%s'", ...
                                      " cannot be concatenated into a", ...
                                      " column."), methNames{mi}, ...
                              datNames{di});
            return;
          end_try_catch
          resNames{end+1} = sprintf ("%s_%s", methNames{mi}, datNames{di});
        endfor
      endfor

      vars = [gcols, {gcount}, rescols];
      names = [grpNames, {'GroupCount'}, resNames];
      G = table (vars{:}, 'VariableNames', names);
    endfunction

    ## The body behind 'join' on both classes.  NAMEL and NAMER are the
    ## caller's own names for the two operands, which name a collision.
    ## Returns an errmsg body (empty on success) for the caller to raise
    ## under its own name.  The result is of the left operand's class: its
    ## rows and its labels are kept and the right side contributes variables.
    function [tbl, ixR, errmsg] = joinResult (tblL, tblR, args_in, ...
                                              nameL, nameR)
      tbl = [];
      ixR = [];
      errmsg = '';
      if (! isa (tblR, 'tabular'))
        errmsg = 'both inputs must be tables or timetables.';
        return;
      endif

      ## Parse Name/Value options
      optNames = {'Keys', 'LeftKeys', 'RightKeys', 'LeftVariables', ...
                  'RightVariables', 'KeepOneCopy'};
      dfValues = {[], [], [], [], [], []};
      [Keys, LeftKeys, RightKeys, LeftVariables, RightVariables, ...
       KeepOneCopy, rem] = parsePairedArguments (optNames, dfValues, ...
                                                 args_in(:));
      if (! isempty (rem))
        errmsg = 'invalid optional input argument.';
        return;
      endif

      ## Resolve key columns on each side.  A class that groups by its row
      ## labels answers to their name, so a timetable joins on its row times.
      [lCols, rCols, ~, rKeyIdx, errmsg] = joinKeys (tblL, tblR, Keys, ...
                                                     LeftKeys, RightKeys);
      if (! isempty (errmsg))
        return;
      endif

      ## Resolve output variables on each side
      if (isempty (LeftVariables))
        lVarIdx = 1:width (tblL);
      else
        lVarIdx = resolveVarRef (tblL, LeftVariables);
      endif
      if (isempty (RightVariables))
        rVarIdx = setdiff (1:width (tblR), rKeyIdx(rKeyIdx > 0));
      else
        rVarIdx = resolveVarRef (tblR, RightVariables);
      endif

      ## Drop the right copy of a 'KeepOneCopy' variable shared with the left
      if (! isempty (KeepOneCopy))
        keepNames = cellstr (KeepOneCopy);
        rNames = tblR.VariableNames(rVarIdx);
        lNames = tblL.VariableNames(lVarIdx);
        dropMask = ismember (rNames, keepNames) & ismember (rNames, lNames);
        rVarIdx(dropMask) = [];
      endif

      ## Build consistent numeric key proxies for both sides
      [leftProxy, rightProxy, errmsg] = tabular.joinProxies (lCols, ...
                                                            rCols);
      if (! isempty (errmsg))
        return;
      endif

      ## The right key combinations must be unique
      if (rows (unique (rightProxy, 'rows')) != rows (rightProxy))
        errmsg = strcat ("the key variables of TBLR must contain unique", ...
                         " combinations of values.");
        return;
      endif

      ## Match each left row to its unique right row
      [tf, ixR] = ismember (leftProxy, rightProxy, 'rows');
      if (! all (tf))
        errmsg = strcat ("the key variables of TBLR must contain all", ...
                         " values of the key variables of TBLL.");
        return;
      endif

      ## Assemble the output: the left rows keep their labels, the right side
      ## contributes variables and nothing else.
      Lpart = subsetvars (tblL, lVarIdx);
      Rpart = subsetrows (subsetvars (tblR, rVarIdx), ixR);
      Rpart = plainTable (Rpart);
      [Lpart, Rpart] = suffixShared (Lpart, Rpart, nameL, nameR);
      tbl = horzcat (Lpart, Rpart);
    endfunction

    ## The body behind 'innerjoin' on both classes.  NAMEL and NAMER are the
    ## caller's own names for the two operands, which name a collision.
    ## Returns an errmsg body (empty on success) for the caller to raise
    ## under its own name.  The result is of the left operand's class and
    ## carries the labels of the left rows it matched, where those labels are
    ## a dimension of their own; a table's row names are dropped, the rows
    ## being pairs rather than the rows they came from.
    function [tbl, ixL, ixR, errmsg] = innerjoinResult (tblL, tblR, ...
                                                        args_in, nameL, nameR)
      tbl = [];
      ixL = [];
      ixR = [];
      errmsg = '';
      if (! isa (tblR, 'tabular'))
        errmsg = 'both inputs must be tables or timetables.';
        return;
      endif

      ## Parse Name/Value options
      optNames = {'Keys', 'LeftKeys', 'RightKeys', 'LeftVariables', ...
                  'RightVariables'};
      dfValues = {[], [], [], [], []};
      [Keys, LeftKeys, RightKeys, LeftVariables, RightVariables, rem] = ...
        parsePairedArguments (optNames, dfValues, args_in(:));
      if (! isempty (rem))
        errmsg = 'invalid optional input argument.';
        return;
      endif

      [lCols, rCols, ~, rKeyIdx, errmsg] = joinKeys (tblL, tblR, Keys, ...
                                                     LeftKeys, RightKeys);
      if (! isempty (errmsg))
        return;
      endif

      ## Resolve output variables on each side
      if (isempty (LeftVariables))
        lVarIdx = 1:width (tblL);
      else
        lVarIdx = resolveVarRef (tblL, LeftVariables);
      endif
      if (isempty (RightVariables))
        rVarIdx = setdiff (1:width (tblR), rKeyIdx(rKeyIdx > 0));
      else
        rVarIdx = resolveVarRef (tblR, RightVariables);
      endif

      ## Build consistent numeric key proxies for both sides
      [leftProxy, rightProxy, errmsg] = tabular.joinProxies (lCols, ...
                                                            rCols);
      if (! isempty (errmsg))
        return;
      endif

      ## Match key rows and lay out the Cartesian product, key-sorted
      Nl = height (tblL);
      [uKeys, ~, ic] = unique ([leftProxy; rightProxy], 'rows');
      icL = ic(1:Nl);
      icR = ic(Nl+1:end);
      for g = 1:rows (uKeys)
        lr = find (icL == g);
        rr = find (icR == g);
        if (! isempty (lr) && ! isempty (rr))
          ixL = [ixL; repelem(lr(:), numel (rr), 1)];
          ixR = [ixR; repmat(rr(:), numel (lr), 1)];
        endif
      endfor

      ## Assemble the output
      Lpart = subsetrows (subsetvars (tblL, lVarIdx), ixL);
      if (isempty (rowLabelHeader (tblL)))
        Lpart = clearRowLabels (Lpart);
      endif
      Rpart = plainTable (subsetrows (subsetvars (tblR, rVarIdx), ixR));
      [Lpart, Rpart] = suffixShared (Lpart, Rpart, nameL, nameR);
      tbl = horzcat (Lpart, Rpart);
    endfunction

    ## The key columns of a join, resolved on both sides from whichever of
    ## 'Keys', 'LeftKeys'/'RightKeys' or the default was given.  RKEYIDX
    ## indexes the right variables the keys used, 0 where a key is the row
    ## labels on either side.  Returns an errmsg body (empty on success).
    function [lCols, rCols, lKeyIdx, rKeyIdx, errmsg] = joinKeys (tblL, ...
                                 tblR, Keys, LeftKeys, RightKeys)
      lCols = {};
      rCols = {};
      lKeyIdx = [];
      rKeyIdx = [];
      errmsg = '';
      if (! isempty (Keys))
        if (! isempty (LeftKeys) || ! isempty (RightKeys))
          errmsg = strcat ("'Keys' cannot be combined with 'LeftKeys' or", ...
                           " 'RightKeys'.");
          return;
        endif
        [lCols, ~, lKeyIdx] = keyColumns (tblL, Keys);
        [rCols, ~, rKeyIdx] = keyColumns (tblR, Keys);
      elseif (! isempty (LeftKeys) || ! isempty (RightKeys))
        if (isempty (LeftKeys) || isempty (RightKeys))
          errmsg = strcat ("'LeftKeys' and 'RightKeys' must be specified", ...
                           " together.");
          return;
        endif
        [lCols, ~, lKeyIdx] = keyColumns (tblL, LeftKeys);
        [rCols, ~, rKeyIdx] = keyColumns (tblR, RightKeys);
        if (numel (lCols) != numel (rCols))
          errmsg = strcat ("'LeftKeys' and 'RightKeys' must reference the", ...
                           " same number of variables.");
          return;
        endif
      elseif (groupsByLabels (tblL) && groupsByLabels (tblR))
        ## Two classes labelled by the same kind of thing join on the labels.
        lCols = {getRowLabels(tblL)};
        rCols = {getRowLabels(tblR)};
        lKeyIdx = 0;
        rKeyIdx = 0;
      else
        isCommon = ismember (tblL.VariableNames, tblR.VariableNames);
        lKeyIdx = find (isCommon);
        if (isempty (lKeyIdx))
          errmsg = strcat ("cannot find any common key variables between", ...
                           " the two tables.");
          return;
        endif
        [~, rKeyIdx] = ismember (tblL.VariableNames(lKeyIdx), ...
                                 tblR.VariableNames);
        lCols = tblL.VariableValues(lKeyIdx);
        rCols = tblR.VariableValues(rKeyIdx);
      endif
    endfunction

    ## The two sides of a join with any name they share suffixed by the
    ## caller's own name for each operand.
    function [Lpart, Rpart] = suffixShared (Lpart, Rpart, nameL, nameR)
      shared = intersect (Lpart.VariableNames, Rpart.VariableNames);
      if (isempty (shared))
        return;
      endif
      [lsuf, rsuf] = tabular.join_suffixes (nameL, nameR);
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
    endfunction

    ## The body behind 'outerjoin' on both classes.  NAMEL and NAMER are the
    ## caller's own names for the two operands, which name a collision.
    ## Returns an errmsg body (empty on success) for the caller to raise
    ## under its own name.  The result is of the left operand's class; a row
    ## with no left row to come from carries no label of its own.
    function [tbl, ixL, ixR, errmsg] = outerjoinResult (tblL, tblR, ...
                                                        args_in, nameL, nameR)
      tbl = [];
      ixL = [];
      ixR = [];
      errmsg = '';
      if (! isa (tblR, 'tabular'))
        errmsg = 'both inputs must be tables or timetables.';
        return;
      endif

      ## Parse Name/Value options
      optNames = {'Keys', 'LeftKeys', 'RightKeys', 'LeftVariables', ...
                  'RightVariables', 'Type', 'MergeKeys'};
      dfValues = {[], [], [], [], [], 'full', false};
      [Keys, LeftKeys, RightKeys, LeftVariables, RightVariables, Type, ...
       MergeKeys, rem] = parsePairedArguments (optNames, dfValues, args_in(:));
      if (! isempty (rem))
        errmsg = 'invalid optional input argument.';
        return;
      endif

      ## Validate 'Type' and 'MergeKeys'
      if (! (ischar (Type) && isrow (Type))
          || ! any (strcmpi (Type, {'full', 'left', 'right'})))
        errmsg = "'Type' must be 'full', 'left', or 'right'.";
        return;
      endif
      Type = lower (Type);
      if (! (islogical (MergeKeys) && isscalar (MergeKeys)))
        errmsg = "'MergeKeys' must be a logical scalar.";
        return;
      endif

      ## Resolve key columns on each side.  A class that groups by its row
      ## labels answers to their name, so a timetable joins on its row times.
      [lCols, rCols, lKeyIdx, rKeyIdx, errmsg] = joinKeys (tblL, tblR, ...
                                          Keys, LeftKeys, RightKeys);
      if (! isempty (errmsg))
        return;
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

      ## Build consistent numeric key proxies for both sides
      [leftProxy, rightProxy, errmsg] = tabular.joinProxies (lCols, rCols);
      if (! isempty (errmsg))
        return;
      endif

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
          ixL = [ixL; repelem(lr(:), nr, 1)];
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
      [Lout, errmsg] = joinBuildSide (subsetvars (tblL, lVarIdx), ixL);
      if (! isempty (errmsg))
        return;
      endif
      [Rout, errmsg] = joinBuildSide (subsetvars (tblR, rVarIdx), ixR);
      if (! isempty (errmsg))
        return;
      endif
      Rout = plainTable (Rout);

      ## Optionally merge each key pair into a single variable.  A merged key
      ## keeps the left position; its name is the left key name when both keys
      ## share it, or 'leftName_rightName' when they differ.
      if (MergeKeys)
        [tfL, posL] = ismember (lKeyIdx, lVarIdx);
        [tfR, posR] = ismember (rKeyIdx, rVarIdx);
        tfL = tfL & (lKeyIdx > 0);
        tfR = tfR & (rKeyIdx > 0);
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
      [Lout, Rout] = suffixShared (Lout, Rout, nameL, nameR);
      tbl = horzcat (Lout, Rout);
    endfunction

    ## The body behind 'findgroups' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  The
    ## groups are read from the variables alone; row labels take no part, and
    ## TID is a plain table, its rows being groups rather than rows of the
    ## input.
    function [G, TID, errmsg] = findgroupsResult (this)
      G = [];
      TID = [];
      errmsg = '';
      nvar = width (this);
      n = height (this);
      if (nvar == 0)
        errmsg = 'T must have at least one variable.';
        return;
      endif
      ## Build the combined proxy matrix and the overall missing-row mask.
      P = [];
      miss = false (n, 1);
      for j = 1:nvar
        [p, m, errmsg] = tabular.group_col_proxy (this.VariableValues{j});
        if (! isempty (errmsg))
          return;
        endif
        P = [P, p];
        miss = miss | m;
      endfor
      ## Label the non-missing rows by sorted unique combination.
      G = NaN (n, 1);
      keep = find (! miss);
      ia = [];
      if (! isempty (keep))
        [~, ia, ic] = unique (P(keep,:), "rows");
        G(keep) = ic;
      endif
      if (isempty (keep))
        TID = plainTable (subsetrows (this, []));
      else
        repRows = keep(ia);
        idcols = cell (1, nvar);
        for j = 1:nvar
          col = this.VariableValues{j};
          idcols{j} = col(repRows,:);
        endfor
        TID = table (idcols{:}, "VariableNames", this.VariableNames);
      endif
    endfunction

    ## The body behind 'splitapply' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  The
    ## function receives one argument per variable, holding the rows of one
    ## group; row labels take no part.
    function [results, N, errmsg] = splitapplyResult (this, func, G, nout)
      results = {};
      N = 0;
      errmsg = '';
      if (! is_function_handle (func))
        errmsg = 'FUNC must be a function handle.';
        return;
      endif
      n = height (this);
      if (! (isnumeric (G) && isvector (G) && numel (G) == n))
        errmsg = strcat ("G must be a numeric vector with one element per", ...
                         " row of T.");
        return;
      endif
      G = G(:);
      gv = G(! isnan (G));
      if (any (gv != fix (gv)) || any (gv < 1))
        errmsg = 'G must contain positive integers.';
        return;
      endif
      if (! isempty (gv))
        N = max (gv);
        if (! isequal (unique (gv), (1:N)'))
          errmsg = strcat ("G must contain every integer between 1 and the", ...
                           " number of groups.");
          return;
        endif
      endif
      nvar = width (this);
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
    endfunction

    ## The body behind 'unstack' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  A class
    ## whose row labels make a row distinct groups by them, so a timetable
    ## with no grouping variable still answers with one row per row time, and
    ## the result is of the calling class.
    function [tbl, idxA, errmsg] = unstackResult (this, vars, ivar, args_in)
      tbl = [];
      idxA = [];
      errmsg = '';
      scope = sprintf ('%s.unstack', class (this));

      ## Check input argument
      if (isempty (vars))
        errmsg = "too few input arguments.";
        return;
      endif

      ## Define allowed vartypes (cellstr + numeric are checked in place)
      allowed = {'logical', 'string', 'categorical'};

      ## Parse optional Name-Value paired arguments
      optNames = {'GroupingVariables', 'ConstantVariables', ...
                  'NewDataVariableNames', 'AggregationFunction', ...
                  'VariableNamingRule'};
      dfValues = {[], [], [], [], 'modify'};
      [groupVars, constVars, newVarNames, aggrFcn, rule] = ...
                  parsePairedArguments (optNames, dfValues, args_in(:));

      ## Get variables to unstack
      [ixVars, ~] = resolveVarRef (this, vars, 'lenient');
      if (any (ixVars == 0))
        vars = cellstr (vars);
        errmsg = sprintf ("VARS index a non-existing variable: '%s'", ...
               vars{find (ixVars == 0)});
        return;
      endif
      ## Check that variables to unstack do not contain nested tables
      for i = ixVars
        if (isa (this.VariableValues{i}, 'table'))
          errmsg = "VARS must not index nested tables.";
          return;
        endif
      endfor
      ## Move variables to unstack into a new table
      VarsTable = subsetvars (this, ixVars);

      ## Get indicator variable
      [ixIvar, ~] = resolveVarRef (this, ivar, 'lenient');
      if (! isscalar (ixIvar))
        errmsg = "IVAR must index a single variable.";
        return;
      elseif (ixIvar == 0)
        ivar = cellstr (ivar);
        errmsg = sprintf ("IVAR indexes a non-existing variable: '%s'", ...
               ivar{find (ixIvar == 0)});
        return;
      endif
      ## Check indicator variable is not a multicolumn variable
      ## or member of the variables to be unstacked
      IvarValues = this.VariableValues{ixIvar};
      if (! isvector (IvarValues))
        errmsg = "IVAR must index a single column variable.";
        return;
      endif
      if (ismember (ixIvar, ixVars))
        errmsg = strcat ("IVAR cannot be any of the", ...
                       " variables to be unstacked as specified by VARS.");
        return;
      endif
      ## Check indicator variable is of a valid type
      if (! (iscellstr (IvarValues) || isnumeric (IvarValues)))
        if (! ismember (class (IvarValues), allowed))
          errmsg = sprintf (strcat ("IVAR indexes a variable of", ...
                         " invalid type: '%s'"), ...
                 class (IvarValues));
          return;
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
          errmsg = sprintf (strcat ("'ConstantVariables' index a", ...
                         " non-existing variable: '%s'"), ...
                 constVars{find (cIxVars == 0)});
          return;
        endif
        if (any (ismember (cIxVars, ixVars)))
          errmsg = strcat ("'ConstantVariables' cannot", ...
                         " contain any variables to be unstacked as", ...
                         " specified by VARS.");
          return;
        endif
        if (any (ismember (cIxVars, ixIvar)))
          errmsg = strcat ("'ConstantVariables' cannot", ...
                         " contain the indicator variable as specified", ...
                         " by IVAR.");
          return;
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
          errmsg = sprintf (strcat ("'GroupingVariables' index a", ...
                         " non-existing variable: '%s'"), ...
                 groupVars{find (gIxVars == 0)});
          return;
        endif
        if (any (ismember (gIxVars, ixVars)))
          errmsg = strcat ("'GroupingVariables' cannot", ...
                         " contain any variables to be unstacked as", ...
                         " specified by VARS.");
          return;
        endif
        if (any (ismember (gIxVars, ixIvar)))
          errmsg = strcat ("'GroupingVariables' cannot", ...
                         " contain the indicator variable as specified", ...
                         " by IVAR.");
          return;
        endif
      endif
      ## Exclude variables of invalid type as grouping variables (emit warning)
      for i = numel (gIxVars):-1:1
        GvarValues = this.VariableValues{gIxVars(i)};
        if (! (iscellstr (GvarValues) || isnumeric (GvarValues)))
          if (! ismember (class (GvarValues), allowed))
            invalid = this.VariableNames{gIxVars(i)};
            gIxVars(i) = [];
            warning (strcat ("%s: 'GroupingVariables' index a variable", ...
                             " of invalid type: '%s', which is", ...
                             " ignored."), scope, invalid);
          endif
        endif
      endfor

      ## Move grouping variables into a new table
      removeVar = setdiff (1:width (this), gIxVars);
      GvarTable = removevars (this, removeVar);

      ## Move constant variables into a new table
      if (! isempty (cIxVars))
        if (any (ismember (cIxVars, gIxVars)) && ! isempty (groupVars))
          errmsg = strcat ("'ConstantVariables' cannot", ...
                         " contain any grouping variables as specified", ...
                         " by 'GroupingVariables'.");
          return;
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
          errmsg = strcat ("'NewDataVariableNames' must be", ...
                         " either a cell array of character vectors, or", ...
                         " a string array.");
          return;
        endif
        if (numel (newVarNames) != numel (IvarNames))
          errmsg = strcat ("'NewDataVariableNames' do not", ...
                         " match the number of unique values in the", ...
                         " indicator variable.");
          return;
        endif
      endif

      ## Check user-defined aggregation function
      if (! isempty (aggrFcn))
        if (! is_function_handle (aggrFcn))
          errmsg = strcat ("'AggregationFunction' must be a", ...
                         " function handle.");
          return;
        endif
      endif

      ## Create table containing unique instances of grouping variables,
      ## otherwise use unique instances of the indicator variable.  Rows whose
      ## grouping variables contain missing values are excluded from unstacking,
      ## together with the corresponding indicator, data, and constant values,
      ## while the original row indices are retained for the returned index.
      ## A class whose row labels make a row distinct groups by them, so a
      ## timetable with no grouping variable still has one row per row time.
      if (! isempty (GvarTable) || uniqueIncludesLabels (this))
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
        ## With no grouping variable every row collapses into one, and the
        ## grouping table is the empty half of that single row.
        GvarTable = subsetrows (GvarTable, 1:min (1, height (GvarTable)));
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
          errmsg = "invalid input for 'VariableNamingRule'.";
          return;
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

        ## Replicate the unstacked variable's variable-scoped custom properties
        ## onto each new column (MATLAB copies them); table-scoped properties
        ## are carried by the constant and grouping variables through the final
        ## horzcat that assembles the output.
        if (! isempty (this.CustomProperties))
          cpNames = customPropsOfType (this, 'variable');
          for ci = 1:numel (cpNames)
            srcval = this.CustomProperties.(cpNames{ci})(ixVars);
            UvarTable.CustomProperties.(cpNames{ci}) = ...
                                              repmat (srcval, 1, ncols);
            UvarTable.CustomPropTypes.(cpNames{ci}) = 'variable';
          endfor
        endif

        ## Add type-specific NaN values and handle multicolumn variables
        ## Check that aggregation function returns suitable output
        [mcvec, aggrFcn] = tabular.get_default_aggrFcn (vvals, nrows, ...
                                                        aggrFcn, scope);
        if (ischar (aggrFcn))
          errmsg = aggrFcn;
          return;
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
          errmsg = "invalid input for 'VariableNamingRule'.";
          return;
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

        ## Replicate each unstacked variable's variable-scoped custom properties
        ## onto its new columns (MATLAB copies them); table-scoped properties
        ## are carried by the constant and grouping variables through the final
        ## horzcat that assembles the output.
        if (! isempty (this.CustomProperties))
          cpNames = customPropsOfType (this, 'variable');
          for ci = 1:numel (cpNames)
            blk = [];
            for i = 1:nvars
              srcval = this.CustomProperties.(cpNames{ci})(ixVars(i));
              blk = [blk, repmat(srcval, 1, ncols)];
            endfor
            UvarTable.CustomProperties.(cpNames{ci}) = blk;
            UvarTable.CustomPropTypes.(cpNames{ci}) = 'variable';
          endfor
        endif

        ## Process each separate variable to be unstacked
        vi = 1;
        for v = 1:nvars
          ## Get values of selected variable
          vvals = VarsTable.VariableValues{v};

          ## Add type-specific NaN values and handle multicolumn variables.
          ## Resolve the aggregation per variable into THISAGGR so that the
          ## original AGGRFCN (or its default placeholder) is not overwritten
          ## between variables of different types.
          [mcvec, thisAggr] = tabular.get_default_aggrFcn (vvals, nrows, ...
                                                           aggrFcn, scope);
          if (ischar (thisAggr))
            errmsg = thisAggr;
            return;
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

    ## The body behind 'inner2outer' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  The
    ## result is of the calling class, keeping its rows and their labels; only
    ## the nesting is turned inside out.
    function [tbl, errmsg] = inner2outerResult (this)
      errmsg = '';

      ## Identify the variables that are themselves tables (nested tables)
      isNested = cellfun (@istable, this.VariableValues);
      ixNest = find (isNested);
      if (isempty (ixNest))
        ## Nothing is nested, so there is nothing to turn inside out.
        tbl = this;
        return;
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
        errmsg = strcat ("an inner variable name clashes with an existing", ...
                         " variable name in TBLA.");
        tbl = [];
        return;
      endif

      ## Build the output: preserve table-level metadata and row names; drop
      ## variable-scoped custom properties since the variable identities change.
      tbl = this;
      tbl.VariableNames = outNames;
      tbl.VariableValues = outVals;
      tbl.VariableTypes = outTypes;
      tbl.VariableDescriptions = outDesc;
      tbl.VariableUnits = outUnits;
      if (! isempty (this.VariableContinuity))
        ## The variables are not the ones that carried it.
        tbl.VariableContinuity = repmat ({'unset'}, 1, numel (outNames));
      endif
      if (! isempty (this.CustomProperties))
        cpNames = customPropsOfType (this, 'variable');
        if (! isempty (cpNames))
          tbl.CustomProperties = rmfield (tbl.CustomProperties, cpNames);
          tbl.CustomPropTypes = rmfield (tbl.CustomPropTypes, cpNames);
        endif
      endif

    endfunction

    ## The body behind 'rows2vars' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  The
    ## result is a table whatever the input was: its rows are the variables of
    ## the input and its columns the rows, so nothing is left in the order the
    ## row labels described.
    function [tbl, errmsg] = rows2varsResult (this, args_in)
      tbl = [];
      errmsg = '';

      dimName = '';

      ## Parse optional Name-Value paired arguments
      optNames = {'DataVariables', 'VariableNamesSource', 'VariableNamingRule'};
      dfValues = {[], [], 'modify'};
      [varRef, source, rule] = parsePairedArguments (optNames, dfValues, ...
                                                     args_in(:));

      ## Check user input for 'DataVariables'
      if (! isempty (varRef))
        ixVar = resolveVarRef (this, varRef, 'lenient');
        if (any (ixVar == 0))
          varRef = cellstr (varRef);
          bad = find (ixVar == 0);
          errmsg = sprintf (strcat ("'DataVariables' index a", ...
                                    " non-existing variable: '%s'"), ...
                            varRef{bad(1)});
          return;
        endif
        tbl = subsetvars (this, ixVar);
      else
        tbl = this;
      endif

      ## Check user input for 'VariableNamesSource'
      if (! isempty (source))
        srcVar = resolveVarRef (this, source, 'lenient');
        if (! isscalar (srcVar))
          errmsg = strcat ("'VariableNamesSource' must index a single", ...
                           " variable.");
          return;
        elseif (any (srcVar == 0))
          source = cellstr (source);
          bad = find (srcVar == 0);
          errmsg = sprintf (strcat ("'VariableNamesSource' indexes a", ...
                                    " non-existing variable: '%s'"), ...
                            source{bad(1)});
          return;
        endif
        ## One name per row of the input, in the order the rows are in.
        ## Repeats are numbered below rather than refused: the source is an
        ## ordinary variable and nothing obliges it to hold distinct values.
        newVarNames = this.VariableValues{srcVar};
        if (! iscellstr (newVarNames))
          newVarNames = cellstr (string (newVarNames));
        endif
        if (numel (newVarNames) != height (this))
          errmsg = strcat ("the number of names taken from the variable", ...
                           " specified in 'VariableNamesSource' does not", ...
                           " match the number of rows in input table.");
          return;
        endif
        dimName = this.VariableNames{srcVar};
        ## Check that 'VariableNamesSource' does not specify a variable
        ## that is specified by 'DataVariables', otherwise remove it from
        ## returning table
        if (! isempty (varRef))
          if (ismember (srcVar, ixVar))
            errmsg = strcat ("'VariableNamesSource' cannot specify a", ...
                             " variable that is also specified by", ...
                             " 'DataVariables'.");
            return;
          endif
        else
          tbl = removevars (tbl, srcVar);
        endif
      elseif (hasRowLabels (this))
        ## The row labels name the new variables, rendered as the class
        ## displays them.  A class whose labels are a dimension of their own
        ## lends that dimension's name to the result; a table's row names are
        ## not a dimension and lend nothing.
        newVarNames = rowLabelStrings (this);
        dimName = rowLabelHeader (this);
      else
        rows = height (tbl);
        newVarNames = cell (1, rows);
        for i = 1:rows
          newVarNames{i} = sprintf ("Var%d", i);
        endfor
      endif
      newVarNames = newVarNames(:)';

      ## Handle variable naming rule
      if (strcmpi (rule, 'modify'))
        for i = 1:numel (newVarNames)
          if (! isvarname (newVarNames{i}))
            newVarNames{i} = matlab.lang.makeValidName (newVarNames{i});
          endif
        endfor
      elseif (! strcmpi (rule, 'preserve'))
        errmsg = "invalid input for 'VariableNamingRule'.";
        return;
      endif

      ## Number a repeated name rather than refusing it.
      seen = struct ();
      for i = 1:numel (newVarNames)
        key = matlab.lang.makeValidName (newVarNames{i});
        if (isfield (seen, key))
          seen.(key)++;
          newVarNames{i} = sprintf ('%s_%d', newVarNames{i}, seen.(key) - 1);
        else
          seen.(key) = 1;
        endif
      endfor

      ## Check for multicolumn variables and nested tables
      for i = 1:width (tbl)
        if (isa (tbl.VariableValues{i}, 'table'))
          errmsg = 'input table must not contain nested tables.';
          return;
        elseif (size (tbl.VariableValues{i}, 2) > 1)
          errmsg = 'input table must not contain multicolumn variables.';
          return;
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

      ## The columns now stand for whatever named them.
      if (! isempty (dimName))
        dn = tbl.DimensionNames;
        dn{2} = dimName;
        tbl.DimensionNames = dn;
      endif

      ## Remove any custom variable properties
      if (! isempty (tbl.CustomProperties))
        cp_names = customPropsOfType (this, 'variable');
        ## Remove custom variable properties only
        for i = 1:numel (cp_names)
          tbl = rmprop (tbl, cp_names{i});
        endfor
      endif

    endfunction

    ## The body behind 'stack' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  The
    ## constant part is subset and repeated, which carries whatever row
    ## labels the class has, and the stacked part is laid beside it, so the
    ## result is of the calling class.
    function [tbl, idxA, errmsg] = stackResult (this, vars, args_in)
      tbl = [];
      idxA = [];
      errmsg = '';

      ## Check input argument
      if (isempty (vars))
        errmsg = 'too few input arguments.';
        return;
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'ConstantVariables', 'NewDataVariableName', ...
                  'IndexVariableName'};
      dfValues = {[], [], []};
      [constVars, newVarName, idxVarName] = ...
                  parsePairedArguments (optNames, dfValues, args_in(:));

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
          bad = find (ix == 0);
          errmsg = sprintf (strcat ("VARS index a non-existing", ...
                                    " variable: '%s'"), gv{bad(1)});
          return;
        endif
        grpIx{g} = ix(:)';
        grpNames{g} = nm;
      endfor

      ## All groups must contain the same number of variables
      grpSize = numel (grpIx{1});
      if (any (cellfun (@numel, grpIx) != grpSize))
        errmsg = strcat ("all groups of variables to stack must be", ...
                         " the same size.");
        return;
      endif
      allStackIx = [grpIx{:}];

      ## Get constant variables to include
      if (isempty (constVars))
        cIxVars = setdiff (1:width (this), allStackIx);
      else
        cIxVars = resolveVarRef (this, constVars, 'lenient');
        if (any (cIxVars == 0))
          constVars = cellstr (constVars);
          bad = find (cIxVars == 0);
          errmsg = sprintf (strcat ("'ConstantVariables' index a", ...
                                    " non-existing variable: '%s'"), ...
                            constVars{bad(1)});
          return;
        endif
        if (any (ismember (cIxVars, allStackIx)))
          errmsg = strcat ("'ConstantVariables' cannot contain any", ...
                           " variables to be stacked as specified by", ...
                           " VARS.");
          return;
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
          errmsg = strcat ("'NewDataVariableName' must be a character", ...
                           " vector, or a cellstring or string array.");
          return;
        endif
        newVarName = cellstr (newVarName);
        if (numel (newVarName) != nGroup)
          errmsg = strcat ("the number of 'NewDataVariableName' names", ...
                           " must equal the number of variable groups to", ...
                           " stack.");
          return;
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
          errmsg = strcat ("'IndexVariableName' must be either a", ...
                           " character vector, or a cellstring or string", ...
                           " scalar.");
          return;
        endif
        idxVarName = char (idxVarName);
      endif

      ## Handle the constant variables first; 'subsetvars' carries whatever
      ## row labels the class has, and 'repelem' repeats them.
      constTable = subsetvars (this, cIxVars);
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
      ## The indicator has no continuity of its own; each stacked variable
      ## takes the continuity of the first variable of its group.
      if (! isempty (this.VariableContinuity))
        ndCont = cell (1, nGroup);
        for g = 1:nGroup
          ndCont{g} = this.VariableContinuity{grpIx{g}(1)};
        endfor
        stackedTable.VariableContinuity = [{'unset'}, ndCont];
      endif

      ## Merge tables
      tbl = [constTable, stackedTable];

      ## Assign variable types in the new table
      new_types = cellfun ('class', tbl.VariableValues, 'UniformOutput', false);
      tbl.VariableTypes = new_types;

      ## The index of the input row each output row came from.
      idxA = repelem ((1:nRow)', grpSize, 1);

    endfunction

    ## The body behind 'groupfilter' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  The
    ## result is of the calling class: the rows that survive keep their order
    ## and whatever labels they carried.
    function [G, errmsg] = groupfilterResult (this, groupvars, args_in)
      G = [];
      errmsg = '';
      scope = sprintf ('%s.groupfilter', class (this));

      ## Split off a trailing 'IncludedEdge' Name-Value option, then an
      ## optional GROUPBINS positional argument that precedes the filter
      ## function METHOD.
      optNames = {'IncludedEdge'};
      args = args_in;
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
      incEdge = tabular.check_included_edge (scope, incEdge);

      hasGroupbins = false;
      groupbins = [];
      if (! isempty (args) && __groupbins__ ('is_spec', args{1}))
        hasGroupbins = true;
        groupbins = args{1};
        args = args(2:end);
      endif

      ## The filter function METHOD is the first remaining argument.
      if (isempty (args))
        errmsg = 'a filter function METHOD is required.';
        return;
      endif
      method = args{1};
      if (! is_function_handle (method))
        errmsg = 'METHOD must be a function handle.';
        return;
      endif

      ## An optional DATAVARS argument may follow the filter function.
      rest = args(2:end);
      if (numel (rest) > 1)
        errmsg = 'too many positional arguments.';
        return;
      endif
      if (numel (rest) == 1)
        datavars = rest{1};
        hasDataVars = true;
      else
        datavars = [];
        hasDataVars = false;
      endif

      ## Resolve grouping and data variables.  The default data variables are
      ## all variables that are not grouping variables.
      [gIx, byLabels] = resolveGroupRef (this, groupvars);
      if (isempty (gIx) && ! byLabels)
        errmsg = 'at least one grouping variable is required.';
        return;
      endif
      if (hasDataVars)
        dIx = resolveVarRef (this, datavars)(:)';
      else
        dIx = 1:width (this);
        dIx(ismember (dIx, gIx)) = [];
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given, then
      ## group the rows, treating missing grouping values as their own groups
      ## so that every row belongs to exactly one group.  The row labels group
      ## as a column of their own when they were named.
      grpCols = this.VariableValues(gIx);
      grpNames = this.VariableNames(gIx);
      if (byLabels)
        labels = getRowLabels (this);
        lname = rowLabelName (this);
        grpCols = [{labels}, grpCols];
        grpNames = [{lname}, grpNames];
      endif
      if (hasGroupbins)
        [grpCols, ~, errmsg] = __groupbins__ ('bin', grpCols, grpNames, ...
                                              groupbins, incEdge, ...
                                              'groupfilter');
        if (! isempty (errmsg))
          return;
        endif
      endif
      [Grp, ng, ~, errmsg] = tabular.gs_group_rows (grpCols, true);
      if (! isempty (errmsg))
        return;
      endif

      ## Build the row keep-mask by applying METHOD to each data variable.
      [keep, errmsg] = tabular.gf_keep_mask (method, ...
                           this.VariableValues(dIx), Grp, ng);
      if (! isempty (errmsg))
        return;
      endif

      G = subsetrows (this, find (keep));
    endfunction

    ## The body behind 'groupcounts' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.  The
    ## result is a table whatever the input was, the counts describing groups
    ## rather than rows, so there is nothing left for row labels to label.
    function [G, errmsg] = groupcountsResult (this, groupvars, args_in)
      G = [];
      errmsg = '';
      scope = sprintf ('%s.groupcounts', class (this));

      ## An optional GROUPBINS positional argument may precede the Name-Value
      ## options; anything else after GROUPVARS must be a recognised option.
      optNames = {'IncludeMissingGroups', 'IncludeEmptyGroups', ...
                  'IncludedEdge'};
      hasGroupbins = false;
      groupbins = [];
      args = args_in;
      if (! isempty (args))
        a = args{1};
        isOpt = ((ischar (a) && isrow (a)) ...
                 || (isa (a, 'string') && isscalar (a))) ...
                && any (strcmpi (char (a), optNames));
        if (! isOpt)
          if (__groupbins__ ('is_spec', a))
            hasGroupbins = true;
            groupbins = a;
            args = args(2:end);
          else
            errmsg = strcat ("invalid argument; expected a GROUPBINS", ...
                             " binning scheme or a Name-Value option.");
            return;
          endif
        endif
      endif

      ## Parse Name-Value options.
      dfValues = {true, false, 'left'};
      [incMiss, incEmpty, incEdge] = ...
                  parsePairedArguments (optNames, dfValues, args(:));
      if (! (isscalar (incMiss)
             && (islogical (incMiss) || isnumeric (incMiss))))
        errmsg = "'IncludeMissingGroups' must be a logical scalar.";
        return;
      endif
      incMiss = logical (incMiss);
      if (! (isscalar (incEmpty)
             && (islogical (incEmpty) || isnumeric (incEmpty))))
        errmsg = "'IncludeEmptyGroups' must be a logical scalar.";
        return;
      endif
      incEmpty = logical (incEmpty);
      incEdge = tabular.check_included_edge (scope, incEdge);

      ## Resolve grouping variables.  The row labels count as a grouping key
      ## on a class that groups by them, named by the row dimension.
      [gIx, byLabels] = resolveGroupRef (this, groupvars);
      if (isempty (gIx) && ! byLabels)
        errmsg = 'at least one grouping variable is required.';
        return;
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given.
      grpCols = this.VariableValues(gIx);
      grpNames = this.VariableNames(gIx);
      if (byLabels)
        labels = getRowLabels (this);
        lname = rowLabelName (this);
        grpCols = [{labels}, grpCols];
        grpNames = [{lname}, grpNames];
      endif
      if (hasGroupbins)
        [grpCols, grpNames, errmsg] = __groupbins__ ('bin', grpCols, ...
                                     grpNames, groupbins, incEdge, ...
                                     'groupcounts');
        if (! isempty (errmsg))
          return;
        endif
      endif

      ## Group the rows, treating missing grouping values as their own groups
      ## (sorted last) when IncludeMissingGroups is true; IncludeEmptyGroups
      ## adds the unused categories of a categorical or binned grouping
      ## variable as empty groups.
      [Grp, ng, gcols, errmsg] = tabular.gs_grouping (grpCols, incMiss, ...
                                                      incEmpty);
      if (! isempty (errmsg))
        return;
      endif
      gcount = accumarray (Grp(! isnan (Grp)), 1, [ng, 1]);
      pcent = 100 * gcount / sum (gcount);

      vars = [gcols, {gcount, pcent}];
      names = [grpNames, {'GroupCount', 'Percent'}];
      G = table (vars{:}, 'VariableNames', names);
    endfunction

    ## The body behind 'grouptransform' on both classes.  Returns an errmsg
    ## body (empty on success) for the caller to raise under its own name.
    function [G, errmsg] = grouptransformResult (this, groupvars, args_in)
      G = [];
      errmsg = '';
      scope = sprintf ('%s.grouptransform', class (this));

      ## An optional GROUPBINS positional argument precedes the transform
      ## METHOD (a known method name or a function handle).
      args = args_in;
      hasGroupbins = false;
      groupbins = [];
      if (! isempty (args) && __groupbins__ ('is_spec', args{1}))
        hasGroupbins = true;
        groupbins = args{1};
        args = args(2:end);
      endif
      if (isempty (args))
        errmsg = 'a transform METHOD is required.';
        return;
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
        errmsg = strcat ("METHOD must be one of 'zscore', 'norm',", ...
                         " 'meancenter', 'rescale', 'meanfill',", ...
                         " 'linearfill', or a function handle.");
        return;
      endif

      ## Split the remaining arguments into the optional positional DATAVARS
      ## and any Name-Value pairs (a Name-Value region starts at the first
      ## option).
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
        errmsg = 'too many positional arguments.';
        return;
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
        errmsg = "'ReplaceValues' must be a logical scalar.";
        return;
      endif
      replaceVals = logical (replaceVals);
      incEdge = tabular.check_included_edge (scope, incEdge);

      ## Resolve grouping and data variables.  The default data variables are
      ## all variables that are not grouping variables.
      [gIx, byLabels] = resolveGroupRef (this, groupvars);
      if (isempty (gIx) && ! byLabels)
        errmsg = 'at least one grouping variable is required.';
        return;
      endif
      if (hasDataVars)
        dIx = resolveVarRef (this, datavars)(:)';
      else
        dIx = 1:width (this);
        dIx(ismember (dIx, gIx)) = [];
      endif

      ## Bin the grouping variables when a GROUPBINS argument was given, then
      ## group the rows, treating missing grouping values as their own groups
      ## so that every row belongs to exactly one group.  The row labels group
      ## as a column of their own when they were named.
      grpCols = this.VariableValues(gIx);
      grpNames = this.VariableNames(gIx);
      if (byLabels)
        labels = getRowLabels (this);
        lname = rowLabelName (this);
        grpCols = [{labels}, grpCols];
        grpNames = [{lname}, grpNames];
      endif
      if (hasGroupbins)
        [grpCols, ~, errmsg] = __groupbins__ ('bin', grpCols, grpNames, ...
                                              groupbins, incEdge, ...
                                              'grouptransform');
        if (! isempty (errmsg))
          return;
        endif
      endif
      [Grp, ng, ~, errmsg] = tabular.gs_group_rows (grpCols, true);
      if (! isempty (errmsg))
        return;
      endif

      ## Transform each data variable, group by group.
      transCols = cell (1, numel (dIx));
      for i = 1:numel (dIx)
        col = this.VariableValues{dIx(i)};
        [tc, terr] = tabular.gt_transform_col (method, col, Grp, ng);
        if (! isempty (terr))
          errmsg = sprintf ("variable '%s': %s", ...
                            this.VariableNames{dIx(i)}, terr);
          return;
        endif
        transCols{i} = tc;
      endfor

      if (replaceVals)
        G = this;
        for i = 1:numel (dIx)
          G.VariableValues{dIx(i)} = transCols{i};
        endfor
      else
        newNames = cell (1, numel (dIx));
        for i = 1:numel (dIx)
          newNames{i} = sprintf ("%s_%s", methDisp, ...
                                 this.VariableNames{dIx(i)});
        endfor
        G = addvars (this, transCols{:}, 'NewVariableNames', newNames);
      endif
    endfunction

    ## The body behind 'rowfun' on both classes.  Returns an errmsg body
    ## (empty on success) for the caller to raise under its own name.
    function [B, errmsg] = rowfunResult (this, func, args_in)
      B = [];
      errmsg = '';
      scope = sprintf ('%s.rowfun', class (this));
      if (! is_function_handle (func))
        errmsg = 'FUNC must be a function handle.';
        return;
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'InputVariables', 'GroupingVariables', ...
                  'OutputVariableNames', 'NumOutputs', 'SeparateInputs', ...
                  'ExtractCellContents', 'OutputFormat', 'ErrorHandler'};
      dfValues = {[], [], [], [], true, false, 'auto', []};
      [inVars, grpVars, outNames, numOut, sepIn, extractCell, outFmt, ...
       errHandler] = parsePairedArguments (optNames, dfValues, args_in(:));
      outFmt = tabular.check_output_format (scope, outFmt, ...
                                            class (this));
      if (! (isscalar (sepIn) && (islogical (sepIn) || isnumeric (sepIn))))
        errmsg = "'SeparateInputs' must be a logical scalar.";
        return;
      endif
      sepIn = logical (sepIn);
      if (! (isscalar (extractCell)
             && (islogical (extractCell) || isnumeric (extractCell))))
        errmsg = "'ExtractCellContents' must be a logical scalar.";
        return;
      endif
      extractCell = logical (extractCell);
      if (! isempty (errHandler) && ! is_function_handle (errHandler))
        errmsg = "'ErrorHandler' must be a function handle.";
        return;
      endif

      ## Resolve grouping variables and input variables (default input is
      ## every variable that is not a grouping variable).
      byLabels = false;
      gIx = [];
      if (! isempty (grpVars))
        [gIx, byLabels] = resolveGroupRef (this, grpVars);
      endif
      grouped = ! isempty (gIx) || byLabels;
      if (isempty (inVars))
        iIx = 1:width (this);
        iIx(ismember (iIx, gIx)) = [];
      else
        iIx = resolveVarRef (this, inVars)(:)';
      endif
      if (isempty (iIx))
        errmsg = 'there are no variables to which to apply FUNC.';
        return;
      endif

      ## Determine the number of outputs requested from FUNC.
      if (! isempty (numOut))
        if (! (isnumeric (numOut) && isscalar (numOut) && numOut >= 0
               && numOut == fix (numOut)))
          errmsg = "'NumOutputs' must be a nonnegative integer.";
          return;
        endif
        nout = numOut;
        if (! isempty (outNames) && numel (cellstr (outNames)) != nout)
          errmsg = strcat ("the number of 'OutputVariableNames' must", ...
                           " equal 'NumOutputs'.");
          return;
        endif
      elseif (! isempty (outNames))
        nout = numel (cellstr (outNames));
      else
        nout = 1;
      endif

      ## Build the output variable names.  Default names are 'Var<k>'; for
      ## grouped output the numbering continues past the grouping variables
      ## and the GroupCount column, so the first result is 'Var<ngroup+2>'.
      if (isempty (outNames))
        if (grouped)
          base = numel (gIx) + 1;
        else
          base = 0;
        endif
        resNames = arrayfun (@(k) sprintf ("Var%d", base + k), 1:nout, ...
                             "UniformOutput", false);
      else
        resNames = cellstr (outNames)(:)';
      endif

      inCols = this.VariableValues(iIx);
      ## A row laid side by side into one argument is a concatenation of the
      ## variables, and the same pairs refuse here as anywhere else.  The
      ## types do not change row by row, so it is asked once.
      if (! sepIn)
        pair = tabular.incompatible_pair (inCols);
        if (! isempty (pair))
          inNames = this.VariableNames(iIx);
          errmsg = sprintf (strcat ("cannot concatenate the table", ...
                                    " variables '%s' and '%s', because", ...
                                    " their types are %s and %s."), ...
                            inNames{pair(1)}, inNames{pair(2)}, ...
                            class (inCols{pair(1)}), ...
                            class (inCols{pair(2)}));
          return;
        endif
      endif
      if (! grouped)
        ## Ungrouped: apply FUNC to each row.  The result maps the input row
        ## for row, so it carries the labels of the rows it was built from.
        n = height (this);
        res = cell (n, max (nout, 1));
        for r = 1:n
          rows = false (n, 1);
          rows(r) = true;
          args = tabular.build_row_args (inCols, rows, sepIn, extractCell);
          res(r,:) = tabular.apply_func (func, errHandler, r, nout, args);
        endfor
        labels = {};
        if (hasRowLabels (this))
          labels = getRowLabels (this);
        endif
        B = build_apply_result (this, scope, outFmt, res(:,1:nout), ...
                                resNames, {}, {}, [], labels, (1:n)');
      else
        ## Grouped: apply FUNC to the rows of each group.
        grpCols = this.VariableValues(gIx);
        if (byLabels)
          labels = getRowLabels (this);
          grpCols = [{labels}, grpCols];
        endif
        [G, ng, repRows, gerr] = tabular.group_table_rows (grpCols);
        if (! isempty (gerr))
          errmsg = gerr;
          return;
        endif
        res = cell (ng, max (nout, 1));
        for g = 1:ng
          rows = (G == g);
          args = tabular.build_row_args (inCols, rows, sepIn, extractCell);
          res(g,:) = tabular.apply_func (func, errHandler, g, nout, args);
        endfor
        [gcols, gcount] = tabular.group_output_cols ( ...
                              this.VariableValues(gIx), G, repRows);
        B = build_grouped_apply_result (this, scope, outFmt, ...
                                        res(:,1:nout), resNames, gcols, ...
                                        this.VariableNames(gIx), gcount, ...
                                        repRows);
      endif
    endfunction

    ## Assemble the output of an apply method from the R-by-C cell array of
    ## per-row (or per-group) results RES with output names OUTNAMES.  For
    ## grouped output the grouping columns GCOLS (named GNAMES) and the GCOUNT
    ## counts are prepended; for ungrouped output these are empty.  FMT selects
    ## the 'table', 'uniform', or 'cell' return format; CALLER names the method
    ## for error messages.
    function out = build_apply_result (this, caller, fmt, res, outNames, ...
                                      gcols, gnames, gcount, rowLabels, rowIx)
      if (nargin < 9)
        rowLabels = {};
      endif
      if (nargin < 10)
        rowIx = [];
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
          ## An index that does not describe the rows just built cannot
          ## label them; the class falls back on what it does for a result
          ## with no index at all.
          if (! isempty (rowIx) && numel (rowIx) != size (rescols{1}, 1))
            rowIx = [];
          endif
          out = assembleApply (this, vars, names, rowLabels, rowIx);
        case 'uniform'
          out = [];
          for c = 1:C
            colvals = res(:,c);
            if (! all (cellfun (@isscalar, colvals)))
              error (strcat ("%s: OutputFormat 'uniform' requires FUNC", ...
                             " to return a scalar for each call."), caller);
            endif
            out = [out, vertcat(colvals{:})];
          endfor
        case 'cell'
          out = res;
      endswitch
    endfunction

    ## Assemble the output of a grouped 'rowfun' or 'varfun' from the NG-by-C
    ## cell array of per-group results RES.  Unlike an aggregating apply, FUNC
    ## may return several rows for a group; each group g therefore contributes
    ## 'size (RES{g,1}, 1)' rows and the grouping columns GCOLS (named GNAMES)
    ## and the GCOUNT counts are replicated to match before the per-group
    ## results are stacked.  FMT selects the 'table', 'uniform', or 'cell'
    ## return format; CALLER names the method for error messages.
    function out = build_grouped_apply_result (this, caller, fmt, res, ...
                                              outNames, gcols, gnames, ...
                                              gcount, rowIx)
      if (nargin < 9)
        rowIx = [];
      endif
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
          if (isempty (rowIx))
            outIx = [];
          else
            outIx = rowIx(repIdx);
          endif
          out = assembleApply (this, vars, names, {}, outIx);
        case 'uniform'
          out = [];
          for c = 1:C
            colvals = res(:,c);
            if (! all (cellfun (@isscalar, colvals)))
              error (strcat ("%s: OutputFormat 'uniform' requires FUNC", ...
                             " to return a scalar for each call."), caller);
            endif
            out = [out, vertcat(colvals{:})];
          endfor
        case 'cell'
          out = res;
      endswitch
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

    ## The key columns a join reference names, in the order it names them.
    ## A class that groups by its row labels answers to their name here too,
    ## and contributes the labels themselves as a key column, for which IXVAR
    ## reports 0, there being no variable to exclude from the result.
    function [cols, names, ixVar] = keyColumns (this, ref)
      isText = ischar (ref) || iscellstr (ref) || isa (ref, 'string');
      if (! isText)
        ix = resolveVarRef (this, ref);
        ixVar = ix(:)';
        cols = this.VariableValues(ixVar);
        names = this.VariableNames(ixVar);
        return;
      endif
      if (ischar (ref))
        want = {ref};
      else
        want = cellstr (ref);
      endif
      want = want(:)';
      if (groupsByLabels (this))
        keys = rowLabelKeyNames (this);
      else
        keys = {};
      endif
      cols = cell (1, numel (want));
      ixVar = zeros (1, numel (want));
      names = want;
      for k = 1:numel (want)
        if (any (strcmp (want{k}, keys)))
          cols{k} = getRowLabels (this);
        else
          ix = resolveVarRef (this, want(k));
          ixVar(k) = ix;
          cols{k} = this.VariableValues{ix};
        endif
      endfor
    endfunction

    ## Resolve a 'GroupingVariables' reference.  IXVAR indexes the variables
    ## named and BYLABELS says the row labels were named too, which only a
    ## class that groups by them answers to, under any of its label key
    ## names.  The rows are then grouped by the label column followed by
    ## those variables, while only the variables are reported back as
    ## grouping variables of the result.
    function [ixVar, byLabels] = resolveGroupRef (this, grpRef)
      byLabels = false;
      isText = ischar (grpRef) || iscellstr (grpRef) ...
               || isa (grpRef, 'string');
      if (groupsByLabels (this) && isText)
        if (ischar (grpRef))
          names = {grpRef};
        else
          names = cellstr (grpRef);
        endif
        names = names(:)';
        keys = rowLabelKeyNames (this);
        isLabel = ismember (names, keys);
        byLabels = any (isLabel);
        grpRef = names(! isLabel);
      endif
      if (byLabels && isempty (grpRef))
        ixVar = zeros (1, 0);
        return;
      endif
      ixVar = resolveVarRef (this, grpRef);
      ixVar = ixVar(:)';
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
      ## Labels that are a dimension of their own follow the rows they came
      ## from, and a row that came from none carries a missing label; a
      ## table's row names are dropped, the rows being pairs rather than the
      ## rows they came from.
      if (isempty (rowLabelHeader (this)) || ! any (pos))
        out = clearRowLabels (out);
      else
        src = idx;
        src(! pos) = idx(find (pos, 1));
        labels = getRowLabels (this);
        lab = labels(src);
        [lab, errmsg] = set_var_missing (lab, ! pos);
        if (! isempty (errmsg))
          return;
        endif
        out = setRowLabels (out, lab);
      endif
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

    ## -*- texinfo -*- @deftypefn {table} {@var{out} =} setvar (@var{tbl},
    ## @var{varRef}, @var{value})
    ##
    ## Set values to an existing or a new variable in table.
    ##
    ## This sets (adds or replaces) the value for a variable in @var{tbl}. It
    ## may be used to change the value of an existing variable, or add a new
    ## variable.
    ##
    ## @var{varRef} is a variable reference, either its index or its name. If
    ## you are adding a new variable, it must be a name, and not an index.
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
    ## -*- texinfo -*-
    ## @deftypefn {tabular} {@var{s} =} summaryOf (@var{obj})
    ##
    ## The summary structure, row labels first where the class has them.
    ##
    ## @end deftypefn
    function s = summaryOf (this)

      s = struct ();
      [lname, lentry] = summaryLabelEntry (this);
      if (! isempty (lname))
        s.(lname) = lentry;
      endif
      vs = summary_for_variables (this);
      f = fieldnames (vs);
      for i = 1:numel (f)
        s.(f{i}) = vs.(f{i});
      endfor

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tabular} {} summaryPrint (@var{obj}, @var{s}, @var{name})
    ##
    ## Print the summary of an object.
    ##
    ## @var{s} is the structure @code{summaryOf} built and @var{name} the
    ## caller's own name for the object, which only the public method can
    ## read and which is omitted where there is none.
    ##
    ## @end deftypefn
    function summaryPrint (this, s, name)

      sz = size (this);
      fprintf ('\n');
      if (isempty (name))
        fprintf ('%dx%d %s\n', sz(1), sz(2), class (this));
      else
        fprintf ('%s: %dx%d %s\n', name, sz(1), sz(2), class (this));
      endif
      if (! isempty (this.Description))
        fprintf ('Description: %s\n', this.Description);
      endif

      [lname, ~] = summaryLabelEntry (this);
      hasLabel = ! isempty (lname);
      if (hasLabel)
        fprintf ('Row Times:\n');
        fprintf ('    %s: %s\n', lname, s.(lname).Type);
      endif

      if (width (this) > 0)
        fprintf ('Variables:\n');
        for v = 1:width (this)
          nm = this.VariableNames{v};
          fprintf ('%s\n', summaryVarLine (nm, this.VariableValues{v}, ...
                                            s.(nm)));
        endfor
      endif

      ## The statistics of everything that has any, one row per column of a
      ## multi-column variable.  A nested table is left out: its statistics
      ## are tables themselves and there is no rendering them in a cell.
      names = {};
      if (hasLabel)
        names = {lname};
      endif
      for v = 1:width (this)
        if (! (isa (this.VariableValues{v}, 'table')
               || isa (this.VariableValues{v}, 'timetable')))
          names{end+1} = this.VariableNames{v};
        endif
      endfor
      ## With no rows there is nothing to report statistics about.
      if (sz(1) == 0)
        names = {};
      endif
      [labels, cells, cols] = summaryStatRows (s, names);
      if (isempty (labels))
        fprintf ('\n');
        return
      endif
      if (hasLabel)
        fprintf ('Statistics for applicable variables and row times:\n');
      else
        fprintf ('Statistics for applicable variables:\n');
      endif
      summaryStatTable (labels, cells, cols);
      fprintf ('\n');

    endfunction

    function s = summary_for_variables (this)
      ## An object with no variables summarises to no fields, not to nothing.
      s = struct ();
      for v = 1:width (this)
        varName = this.VariableNames{v};
        val = this.VariableValues{v};
        e = struct ();
        e.Size = size (val);
        e.Type = class (val);
        e.Description = '';
        if (! isempty (this.VariableDescriptions{v}))
          e.Description = this.VariableDescriptions{v};
        endif
        e.Units = '';
        if (! isempty (this.VariableUnits{v}))
          e.Units = this.VariableUnits{v};
        endif
        e.Continuity = [];
        if (! isempty (this.VariableContinuity))
          e.Continuity = this.VariableContinuity{v};
        endif
        s.(varName) = tabular.summaryStats (e, val);
      endfor
    endfunction

    ## -*- texinfo -*- @deftypefn {tabular} {[@var{name}, @var{entry}] =}
    ## summaryLabelEntry (@var{obj})
    ##
    ## The summary entry for the row labels, where the class has one.
    ##
    ## @var{name} is the field it is filed under and @var{entry} the entry
    ## itself.  A class whose row labels are not summarised answers with an
    ## empty name.
    ##
    ## @end deftypefn
    function [name, entry] = summaryLabelEntry (this)
      name = '';
      entry = [];
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

    ## -*- texinfo -*-
    ## @deftypefn {tabular} {@var{e} =} summaryStats (@var{e}, @var{val})
    ##
    ## Add the statistics half of a summary entry, chosen by the value's type.
    ##
    ## The order the fields are added in is the order they are reported in,
    ## and which of them a type gets was measured rather than reasoned: a
    ## logical carries counts and nothing else, an integer has no standard
    ## deviation, and only an ordinal categorical can be ordered.
    ##
    ## @end deftypefn
    function e = summaryStats (e, val)


    if (islogical (val))
      e.True = sum (val, 1);
      e.False = sum (! val, 1);

    elseif (isa (val, 'categorical'))
      e.Categories = categories (val);
      e.Counts = countcats (val);
      e.NumMissing = sum (ismissing (val), 1);
      if (isordinal (val))
        d = double (val);
        cats = categories (val);
        e.Min = catOf (cats, __nanmin__ (d), val);
        e.Median = catOf (cats, ceil (median (d, 'omitnan')), val);
        e.Max = catOf (cats, __nanmax__ (d), val);
      endif

    elseif (isa (val, 'duration'))
      sec = seconds (val);
      fmt = val.Format;
      e.NumMissing = sum (isnan (sec), 1);
      e.Min = durOf (__nanmin__ (sec), fmt);
      e.Median = durOf (median (sec, 'omitnan'), fmt);
      e.Max = durOf (__nanmax__ (sec), fmt);
      e.Mean = durOf (colmean (sec), fmt);
      e.Std = durOf (colstd (sec), fmt);

    elseif (isa (val, 'datetime'))
      ## Measured from the first known instant so that the time zone, which
      ## the arithmetic carries, survives into every statistic.
      e.TimeZone = val.TimeZone;
      nm = ismissing (val);
      e.NumMissing = sum (nm, 1);
      ix = find (! nm(:), 1);
      if (isempty (ix) && isempty (val))
        ## Nothing at all to measure from, not even a zero point.
        e.Min = val;
        e.Median = val;
        e.Max = val;
        e.Mean = val;
        e.Std = durOf (NaN, 'hh:mm:ss');
        return
      elseif (isempty (ix))
        sec = NaN (size (val));
        origin = val(1);
      else
        origin = val(ix);
        sec = seconds (val - origin);
      endif
      e.Min = origin + seconds (__nanmin__ (sec));
      e.Median = origin + seconds (median (sec, 'omitnan'));
      e.Max = origin + seconds (__nanmax__ (sec));
      e.Mean = origin + seconds (colmean (sec));
      e.Std = durOf (colstd (sec), 'hh:mm:ss');

    elseif (isa (val, 'calendarDuration'))
      ## Not totally ordered: months and days are not interconvertible, so
      ## there is nothing to report but the count.
      e.NumMissing = sum (ismissing (val), 1);

    elseif (isinteger (val))
      ## An integer median rounds to the type, and there is no deviation.
      e.NumMissing = zeros (1, columns (val));
      e.Min = min (val, [], 1);
      e.Median = cast (round (median (double (val), 1)), class (val));
      e.Max = max (val, [], 1);
      e.Mean = mean (double (val), 1);

    elseif (isnumeric (val))
        e.NumMissing = sum (isnan (val), 1);
      if (size (val, 1) == 0)
        ## With no rows there is no smallest and no largest, while the
        ## averages still answer, as NaN.
        e.Min = val;
        e.Median = NaN (1, columns (val));
        e.Max = val;
        e.Mean = NaN (1, columns (val));
        e.Std = NaN (1, columns (val));
        return
      endif
      e.Min = __nanmin__ (val);
      e.Median = median (val, 'omitnan');
      e.Max = __nanmax__ (val);
      ## A single stays single through its own statistics.
      e.Mean = cast (colmean (val), class (val));
      e.Std = cast (colstd (val), class (val));

    elseif (isa (val, 'table') || isa (val, 'timetable'))
      e.NumMissing = sum (__varmissing__ (val), 1);

    else
      ## Text, cells, structs and anything else carry a count alone.
      m = __varmissing__ (val);
      if (size (m, 1) != size (val, 1))
        m = false (size (val, 1), 1);
      endif
      e.NumMissing = sum (m, 1);
    endif
    endfunction

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

    ## Build a single-column grouping proxy for one grouping variable COL: a
    ## numeric matrix P (one row per element) whose sort order matches COL's
    ## value order, so that 'unique (P, "rows")' recovers the sorted unique
    ## groups, together with a logical MISS mask flagging the elements that
    ## findgroups treats as missing (NaN/NaT/<missing>/''/<undefined>).  Returns
    ## an errmsg body (empty on success) emitted by the caller under its own
    ## name.
    function [p, miss, errmsg] = group_col_proxy (col)
      p = [];
      miss = [];
      errmsg = '';
      if (isa (col, 'categorical'))
        ## Categorical groups follow category order (ordinal or reordered),
        ## which the underlying category codes encode; <undefined> maps to NaN.
        p = double (col)(:);
        miss = isnan (p);
        return;
      endif
      k = tabular.key_kind (col);
      if (isempty (k))
        errmsg = sprintf ("unsupported grouping variable type '%s'.", ...
                          class (col));
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
          p = tabular.datetime_to_datenum (col)(:);
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

    ## Group table rows by the grouping-variable columns GRPCOLS (a cell array
    ## of variable values, one per grouping variable), using 'group_col_proxy'
    ## on each. Returns G, an n-by-1 vector of group numbers (NaN for rows
    ## holding a missing value in any grouping variable), NGROUPS, the number of
    ## groups, REPROWS, a representative row index per group in sorted group
    ## order, and an errmsg body (empty on success) emitted by the caller.
    function [G, ngroups, repRows, errmsg] = group_table_rows (grpCols)
      errmsg = '';
      ngroups = 0;
      repRows = [];
      n = size (grpCols{1}, 1);
      P = [];
      miss = false (n, 1);
      for j = 1:numel (grpCols)
        [p, m, e] = tabular.group_col_proxy (grpCols{j});
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
    ## variable at the representative rows REPROWS, and GCOUNT the number of
    ## rows in each group, derived from the group-number vector G.
    function [gcols, gcount] = group_output_cols (grpCols, G, repRows)
      ngroups = numel (repRows);
      gcols = cell (1, numel (grpCols));
      for p = 1:numel (grpCols)
        gcols{p} = grpCols{p}(repRows,:);
      endfor
      gcount = accumarray (G(! isnan (G)), 1, [ngroups, 1]);
    endfunction

    ## Validate an 'IncludedEdge' binning option VAL for method CALLER,
    ## returning it lowercased as 'left' or 'right'.
    function e = check_included_edge (caller, val)
      if (isa (val, 'string') && isscalar (val))
        val = char (val);
      endif
      if (! (ischar (val) && isrow (val) ...
             && any (strcmpi (val, {'left', 'right'}))))
        error ("%s: 'IncludedEdge' must be 'left' or 'right'.", caller);
      endif
      e = lower (val);
    endfunction

    ## Group table rows for 'groupsummary' by the grouping-variable values
    ## GRPCOLS, treating each grouping variable's missing values as a single
    ## group value. Returns G, an n-by-1 vector of group numbers (1..NGROUPS);
    ## NGROUPS; REPROWS, a representative row index per group; and an errmsg
    ## body emitted by the caller.  Groups are sorted by grouping value with
    ## missing groups last.  When INCMISS is false, rows holding a missing
    ## grouping value are dropped (labelled NaN in G and excluded from
    ## NGROUPS/REPROWS).
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
        [p, m, e] = tabular.group_col_proxy (grpCols{j});
        if (! isempty (e))
          errmsg = e;
          return;
        endif
        pc = p;
        ## collapse all missing values of this variable
        pc(m,:) = 0;
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

    ## Transform one data variable COL group by group for 'grouptransform',
    ## applying METHOD (a transform-name char vector or a function handle) to
    ## each group's slice and returning OUT, the transformed values the same
    ## size as COL.  G is the n-by-1 group-number vector (1..NG), every row
    ## assigned to a group.  A function handle must return a single row
    ## (broadcast) or one row per group row. Returns an errmsg body (empty on
    ## success) emitted by the caller.
    function [out, errmsg] = gt_transform_col (method, col, G, ng)
      out = [];
      errmsg = '';
      if (! (isnumeric (col) || islogical (col)))
        errmsg = sprintf (strcat ("grouptransform requires numeric or", ...
                                  " logical data; got '%s'."), class (col));
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
            errmsg = strcat ("the transform function must return a", ...
                             " result the same size as the group, or a", ...
                             " single row.");
            out = [];
            return;
          endif
          out(rows,:) = r;
        else
          for c = 1:columns (slice)
            out(rows,c) = tabular.gt_apply_named (method, slice(:,c));
          endfor
        endif
      endfor
    endfunction

    ## Apply a single named transform METHOD to the column vector X (a group's
    ## slice of one data variable), returning the transformed values V the same
    ## size as X. NaN values are omitted when computing the group statistics;
    ## the centring and scaling methods leave NaN in place, while
    ## 'meanfill'/'linearfill' fill them.
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
          v = tabular.gt_linearfill (x);
      endswitch
    endfunction

    ## Fill the missing values of the column vector X by linear interpolation
    ## over the non-missing positions, leaving leading and trailing missing
    ## values (and any group with fewer than two non-missing values) unchanged.
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

    ## Group the rows of 'groupsummary'/'groupcounts' by the grouping-variable
    ## values GRPCOLS (already binned when a GROUPBINS argument was given).
    ## Returns G, the n-by-1 group numbers (NaN for an excluded row), NG the
    ## number of groups, GCOLS a 1-by-K cell of the typed grouping-variable
    ## output columns (one value per group), and an errmsg body emitted by the
    ## caller.  When INCEMPTY is true the unused categories of a categorical (or
    ## binned) grouping variable contribute empty groups, built from the full
    ## level machinery; otherwise only the observed groups are returned, in
    ## ascending grouping-value order with missing groups last.
    function [G, ng, gcols, errmsg] = gs_grouping (grpCols, incMiss, incEmpty)
      errmsg = '';
      gcols = {};
      K = numel (grpCols);
      n = size (grpCols{1}, 1);
      if (incEmpty)
        [G, ng, lvlOf, levVals, ~, errmsg] = ...
                tabular.pivot_dimension (grpCols, n, incMiss, true);
        if (! isempty (errmsg))
          G = []; ng = 0;
          return;
        endif
        gcols = cell (1, K);
        for j = 1:K
          gcols{j} = levVals{j}(lvlOf(:,j), :);
        endfor
      else
        [G, ng, repRows, errmsg] = tabular.gs_group_rows (grpCols, incMiss);
        if (! isempty (errmsg))
          return;
        endif
        gcols = cell (1, K);
        for j = 1:K
          gcols{j} = grpCols{j}(repRows, :);
        endfor
      endif
    endfunction

    ## Group the rows of one 'pivot' dimension (rows or columns) defined by the
    ## grouping-variable columns GRPCOLS (a cell array, empty for an omitted
    ## dimension).  N is the table height.  Returns GID, an n-by-1 group index
    ## per row (NaN when the row is excluded), NG, the number of groups, LVLOF,
    ## an ng-by-K matrix of per-variable level indices for each group, LEVVALS,
    ## a 1-by-K cell of the per-variable typed level values from 'pivot_levels',
    ## MISSLVLS, a 1-by-K cell of the per-variable missing-level logical flags,
    ## and an errmsg body (empty on success).  When INCEMPTY is true the groups
    ## span the full Cartesian product of the variables' levels (so unused
    ## combinations appear as empty groups); otherwise only the observed
    ## combinations are kept, sorted in ascending level order with the first
    ## variable varying slowest.
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
        [idx, lv, ml, errmsg] = tabular.pivot_levels (grpCols{j}, ...
                                                     incMiss, incEmpty);
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

    ## Build the level structure of one 'pivot' grouping variable COL.  Returns
    ## IDX, an n-by-1 vector of level indices (1..L) for the rows of COL (NaN
    ## for a row holding a missing value when INCMISS is false, so that row is
    ## excluded from every group), LEVVALS, a typed column vector with one
    ## representative value per level used to build row labels and column names,
    ## MISSLVL, a 1-by-L logical flagging the missing level, and an errmsg body
    ## (empty on success).  Levels are the sorted unique values of COL; a
    ## categorical variable uses its category order, and when INCEMPTY is true
    ## every category is a level even if unused in the data.  A missing value
    ## forms one extra level, sorted last, when INCMISS.
    function [idx, levVals, missLvl, errmsg] = pivot_levels (col, ...
                                                             incMiss, incEmpty)
      idx = [];
      levVals = [];
      missLvl = [];
      errmsg = '';
      n = size (col, 1);
      [p, miss, errmsg] = tabular.group_col_proxy (col);
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

    ## Normalise the METHOD argument of 'groupsummary' into parallel cell
    ## arrays of method specs and the display names used to build output
    ## variable names.  A name is matched without regard to case and may be
    ## abbreviated to any unambiguous prefix; 'all' stands for every named
    ## method, in the order they are reported.  Returns an errmsg body (empty
    ## on success).
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
        errmsg = strcat ("METHOD must be a method name, a function", ...
                         " handle, or a cell array of method names and", ...
                         " function handles.");
        return;
      endif
      ## The order 'all' reports its methods in.
      known = {'mean', 'sum', 'min', 'max', 'range', 'median', 'mode', ...
               'var', 'std', 'nummissing', 'nnz', 'numunique'};
      nfun = 0;
      for k = 1:numel (items)
        it = items{k};
        if (is_function_handle (it))
          nfun++;
          methods{end+1} = it;
          methNames{end+1} = sprintf ("fun%d", nfun);
        elseif ((ischar (it) && isrow (it))
                || (isa (it, 'string') && isscalar (it)))
          [nm, errmsg] = tabular.gs_match_method (char (it), known);
          if (! isempty (errmsg))
            return;
          endif
          if (strcmp (nm, 'all'))
            methods = [methods, known];
            methNames = [methNames, known];
          else
            methods{end+1} = nm;
            methNames{end+1} = nm;
          endif
        else
          errmsg = strcat ("each method must be a method name or a", ...
                           " function handle.");
          return;
        endif
      endfor
    endfunction

    ## One 'groupsummary' method name resolved against the known list plus
    ## 'all', matched without regard to case and accepting any unambiguous
    ## abbreviation, as MATLAB does.
    function [nm, errmsg] = gs_match_method (name, known)
      nm = '';
      errmsg = '';
      cand = [known, {'all'}];
      lname = lower (name);
      if (isempty (lname))
        errmsg = "a method name cannot be empty.";
        return;
      endif
      hit = find (strcmp (lname, cand));
      if (isempty (hit))
        hit = find (strncmp (lname, cand, numel (lname)));
      endif
      if (numel (hit) == 1)
        nm = cand{hit};
      elseif (isempty (hit))
        errmsg = sprintf ("'%s' is not a supported method name.", name);
      else
        errmsg = sprintf (strcat ("'%s' matches more than one method", ...
                                  " name."), name);
      endif
    endfunction

    ## Apply a single 'groupsummary' method M (a method-name char vector or a
    ## function handle) to the column slice X of one group, returning a row
    ## result V. Named methods omit missing values (except 'nummissing'); a
    ## function handle receives X unchanged and must return a single row.
    ## Returns an errmsg body (empty on success) emitted by the caller.
    function [v, errmsg] = gs_apply_method (m, x)
      v = [];
      errmsg = '';
      if (is_function_handle (m))
        v = m (x);
        if (size (v, 1) != 1)
          errmsg = "a function handle method must return a single row.";
        endif
        return;
      endif

      ## Type-agnostic counting methods.
      if (strcmp (m, 'nummissing'))
        v = sum (tabular.gs_missing_mask (x), 1);
        return;
      endif
      if (strcmp (m, 'numunique'))
        miss = tabular.gs_missing_mask (x);
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

      ## 'std' and 'var' are defined on floating point data, as they are
      ## everywhere else in Octave, so an integer variable is refused rather
      ## than silently widened to double.
      if (any (strcmp (m, {'std', 'var'})) && isinteger (x))
        errmsg = sprintf (strcat ("named method '%s' is not supported for", ...
                                  " variables of type '%s'."), m, class (x));
        return;
      endif

      ## The remaining named methods require numeric or logical data.
      if (! (isnumeric (x) || islogical (x)))
        errmsg = sprintf (strcat ("named method '%s' is not supported for", ...
                                  " variables of type '%s'; use a function", ...
                                  " handle."), m, class (x));
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

    ## Return a logical mask the size of X flagging its missing elements, used
    ## by the type-agnostic 'groupsummary' methods.  Supports the numeric,
    ## logical, text, datetime, duration, calendarDuration, and categorical
    ## variable types.
    function mask = gs_missing_mask (x)
      if (isa (x, 'datetime'))
        mask = isnan (tabular.datetime_to_datenum (x));
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

    ## Build the row keep-mask for 'groupfilter' by applying the filter function
    ## METHOD to each data variable's per-group slice.  DATACOLS is a cell array
    ## of data-variable values; G the n-by-1 group numbers (1..NG), every row
    ## assigned to a group.  For each group METHOD receives the variable's slice
    ## and must return a logical scalar (keep/drop the whole group) or a logical
    ## vector with one element per group row.  The per-variable masks are
    ## combined with logical AND, so a row is kept only when the condition holds
    ## across all data variables. Returns KEEP (n-by-1 logical) and an errmsg
    ## body emitted by the caller.
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
            errmsg = strcat ("the filter function must return a logical", ...
                             " scalar or a logical vector with one element", ...
                             " per group", ...
                             " row.");
            return;
          endif
          keep(rows) = keep(rows) & m;
        endfor
      endfor
    endfunction

    ## Build the disambiguation suffixes used by the join methods when a
    ## variable name is shared by both tables.  MATLAB derives them from the
    ## input argument names (e.g. inputs L and R give '_L'/'_R'); fall back to
    ## '_left'/'_right' when an input has no workspace name.
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

    ## The numeric proxies a join matches its key columns by.
    function [leftProxy, rightProxy, errmsg] = joinProxies (lCols, rCols)
      leftProxy = [];
      rightProxy = [];
      errmsg = '';
      for k = 1:numel (lCols)
        [lp, rp, errmsg] = tabular.key_col_proxy (lCols{k}, rCols{k});
        if (! isempty (errmsg))
          return;
        endif
        leftProxy = [leftProxy, lp];
        rightProxy = [rightProxy, rp];
      endfor
    endfunction

    ## Return a logical mask, the same size as a table variable V, that flags
    ## the missing entries.  Used by 'fillmissing'.  Char arrays have no
    ## standard missing value and nested tables are treated as non-missing.
    ## Expand the 'constant' fill value into a 1-by-NVARS cell, one value per
    ## targeted variable (scalar broadcast, per-variable vector, or per-variable
    ## cell).  Used by 'fillmissing'.
    function [mcvec, aggrFcn] = get_default_aggrFcn (vvals, nrows, ...
                                                     aggrFcn, scope)
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
            aggrFcn = strcat ("invalid 'AggregationFunction'", ...
                              " for numeric data.");
          end_try_catch
          if (! isscalar (tmpval))
            aggrFcn = strcat ("'AggregationFunction'", ...
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
            aggrFcn = strcat ("invalid 'AggregationFunction'", ...
                              " for numeric data.");
          end_try_catch
          if (! isscalar (tmpval))
            aggrFcn = strcat ("'AggregationFunction'", ...
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
            aggrFcn = strcat ("invalid 'AggregationFunction'", ...
                              " for calendarDuration data.");
          end_try_catch
          if (! isscalar (tmpval))
            aggrFcn = strcat ("'AggregationFunction'", ...
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
            aggrFcn = strcat ("invalid 'AggregationFunction'", ...
                              " for duration data.");
          end_try_catch
          if (! isscalar (tmpval))
            aggrFcn = strcat ("'AggregationFunction'", ...
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
            aggrFcn = strcat ("invalid 'AggregationFunction'", ...
                              " for logical data.");
          end_try_catch
          if (! isscalar (tmpval))
            aggrFcn = strcat ("'AggregationFunction'", ...
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
            aggrFcn = strcat ("invalid 'AggregationFunction'", ...
                              " for categorical data.");
          end_try_catch
          if (! isscalar (tmpval))
            aggrFcn = strcat ("'AggregationFunction'", ...
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
        ## Inside the class a dot reads a property, so the column is taken
        ## from where it is stored rather than by its name.
        mcvec = repmat (tmpl.VariableValues{1}, 1, vcols);
        if (isempty (aggrFcn))  # add default aggrevation function
          aggrFcn = @unique;
        endif
      endif

      ## Enforce a scalar aggregation result, erroring on e.g. conflicting
      ## non-numeric values under the default @unique, matching MATLAB.
      if (! ischar (aggrFcn))
        baseFcn = aggrFcn;
        aggrFcn = @(x) tabular.enforce_scalar_aggr (baseFcn (x), scope);
      endif
    endfunction

    ## Error out when an unstack aggregation function returns a non-scalar
    ## value.
    function val = enforce_scalar_aggr (val, scope)
      if (size (val, 1) > 1)
        error (strcat ("%s: 'AggregationFunction' must return", ...
                       " a scalar value."), scope);
      endif
    endfunction

    ## Build the cell array of input arguments passed to FUNC for the rows
    ## selected by the logical mask ROWS, taken from the input-variable values
    ## INCOLS (a cell array of variable values).  When SEPIN is true each
    ## variable's selected rows form a separate argument; otherwise they are
    ## horizontally concatenated into a single argument.  When EXTRACTCELL is
    ## true the contents of cell-valued variables are extracted.
    function args = build_row_args (inCols, rows, sepIn, extractCell)
      vals = cell (1, numel (inCols));
      for k = 1:numel (inCols)
        col = inCols{k};
        ## Extraction unwraps a cell variable for the argument it becomes on
        ## its own.  It says nothing about a row laid side by side into one
        ## argument, which is the variables as they are stored.
        if (sepIn && extractCell && iscell (col))
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

    ## Return the name used to prefix 'varfun' output variables: the name of
    ## FUNC, or 'Fun' when FUNC is an anonymous function handle.
    function fname = apply_func_name (func)
      fstr = func2str (func);
      if (isempty (fstr) || fstr(1) == '@')
        fname = 'Fun';
      else
        fname = fstr;
      endif
    endfunction

    ## Validate and normalise an OutputFormat value for the apply methods: map
    ## 'auto' to 'table' and accept 'table', 'uniform', and 'cell'.  CALLER
    ## names the method for error messages.
    function fmt = check_output_format (caller, fmt, clsname)
      if (nargin < 3)
        clsname = 'table';
      endif
      if (isa (fmt, 'string'))
        fmt = char (fmt);
      endif
      if (! (ischar (fmt) && isrow (fmt)))
        error ("%s: 'OutputFormat' must be a character vector.", caller);
      endif
      low = lower (fmt);
      ## A class answers to its own name as well as to 'table', both meaning
      ## an object of that class; the other class's name does not, there
      ## being no row times to invent or discard.
      if (any (strcmp (low, {'auto', 'table', lower(clsname)})))
        fmt = 'table';
      elseif (strcmp (low, 'uniform'))
        fmt = 'uniform';
      elseif (strcmp (low, 'cell'))
        fmt = 'cell';
      elseif (any (strcmp (low, {'table', 'timetable'})))
        error ("%s: '%s' OutputFormat is not supported.", caller, low);
      else
        error ("%s: invalid 'OutputFormat' value '%s'.", caller, fmt);
      endif
    endfunction

    ## Call FUNC with the arguments ARGS, requesting NOUT outputs, and return
    ## them in a 1-by-max(NOUT,1) cell row.  When ERRHANDLER is non-empty it is
    ## called with a struct describing any error thrown by FUNC (fields
    ## 'identifier', 'message', and 'index' set to IDX) followed by ARGS, and
    ## its outputs are used instead.  WHAT names what FUNC was applied to, as
    ## in "the variable 'x'", and where it is given a failure is reported
    ## against it under SCOPE rather than being left to speak for itself.
    function out = apply_func (func, errHandler, idx, nout, args, scope, what)
      out = cell (1, max (nout, 1));
      if (isempty (errHandler))
        if (nargin < 7 || isempty (what))
          [out{1:nout}] = func (args{:});
        else
          try
            [out{1:nout}] = func (args{:});
          catch
            error (strcat ("%s: applying the function '%s' to %s", ...
                           " generated an error."), scope, ...
                   tabular.apply_func_name (func), what);
          end_try_catch
        endif
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

function [col, filled] = apply_end_values (col, col0, m, filled, endVals, vname)
  [head, tail] = end_gaps (m);
  if (! any (head | tail))
    return;
  endif
  known = find (! m(:));
  if (ischar (endVals))
    switch (endVals)
      case 'none'
        col(head | tail) = col0(head | tail);
        filled(head | tail) = false;
      case 'previous'
        col(head) = col0(head);
        filled(head) = false;
        if (! isempty (known))
          col(tail) = col(known(end));
          filled(tail) = true;
        endif
      case 'next'
        col(tail) = col0(tail);
        filled(tail) = false;
        if (! isempty (known))
          col(head) = col(known(1));
          filled(head) = true;
        endif
      case 'nearest'
        if (isempty (known))
          col(head | tail) = col0(head | tail);
          filled(head | tail) = false;
        else
          col(head) = col(known(1));
          col(tail) = col(known(end));
          filled(head | tail) = true;
        endif
      otherwise
        error (strcat ("table.fillmissing: unsupported 'EndValues'", ...
                       " option '%s'."), endVals);
    endswitch
  else
    try
      col(head | tail) = endVals;
    catch
      error (strcat ("table.fillmissing: the 'EndValues' constant", ...
                     " cannot be", ...
                     " assigned to table variable '%s'."), vname);
    end_try_catch
    filled(head | tail) = true;
  endif
endfunction

## Split a 'standardizeMissing' indicator into a numeric row vector NUMIND and a
## cellstr row TXTIND of text indicators.  Used by 'standardizeMissing'.

function [head, tail] = end_gaps (m)
  m = m(:);
  known = find (! m);
  if (isempty (known))
    head = m;
    tail = false (size (m));
    return;
  endif
  x = (1:numel (m))';
  head = m & x < known(1);
  tail = m & x > known(end);
endfunction

## Impose 'EndValues' on the end gaps of a column.  COL is the column after
## the fill method has run and COL0 the column before it, so a value the
## method placed in an end gap can be taken back out.  A keyword takes the
## value of the anchor on the side it names and leaves the other side missing;
## a constant is written directly and must be assignable to the variable.

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

function [col, filled] = fill_interp (col, m, x, method)
  m = m(:);
  n = numel (m);
  filled = false (n, 1);
  known = ! m;
  if (sum (known) < 2)
    return;                         # need at least two anchors to interpolate
  endif
  x = x(:);
  xk = x(known);
  ## A datetime or a duration is interpolated in seconds and put back through
  ## the class's own arithmetic, so its type, format and time zone survive.  A
  ## datetime has no natural zero, so it is measured from its first known
  ## entry.
  origin = [];
  if (isdatetime (col))
    origin = col(xk(1));
    yk = seconds (col(known) - origin);
  elseif (isduration (col))
    yk = seconds (col(known));
  else
    yk = double (col(known));
  endif
  lo = xk(1);
  hi = xk(end);
  vals = NaN (n, 1);
  ## Interior gaps are interpolated; the end gaps are extrapolated, which is
  ## what 'EndValues' 'extrap' asks of this method.  Any other value overrides
  ## them in the caller.
  interior = m & x > lo & x < hi;
  if (any (interior))
    vals(interior) = interp1 (xk, yk, x(interior), method);
  endif
  ends = m & (x < lo | x > hi);
  if (any (ends))
    vals(ends) = interp1 (xk, yk, x(ends), method, 'extrap');
  endif
  if (isdatetime (col))
    col(m) = origin + seconds (vals(m));
  elseif (isduration (col))
    col(m) = seconds (vals(m));
  else
    col(m) = vals(m);
  endif
  filled(m) = true;
endfunction

## The leading and trailing runs of missing entries of a column with missing
## mask M.  Everything before the first known entry is a leading gap and
## everything after the last is a trailing one.  When nothing is known there
## is no anchor to sit between, so every entry is a leading gap, which is what
## a constant end value fills and what every keyword leaves alone.

function si = fill_neighbor_idx (m, method, x)
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
        elseif (x(sn(i)) - x(i) <= x(i) - x(sp(i)))
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

## Helper function for unstack method to get default aggregation function
## and missing values according to the data type of the stacked variable

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


## One category value per column statistic, taken by its code.
function c = catOf (cats, code, proto)

  c = proto([]);
  for i = 1:numel (code)
    if (isnan (code(i)) || code(i) < 1 || code(i) > numel (cats))
      c(i) = proto(1);
      c(i) = missing;
    else
      c(i) = categorical (cats(code(i)), cats, 'Ordinal', isordinal (proto));
    endif
  endfor

endfunction

## A duration of X seconds carrying the format asked for.
function d = durOf (x, fmt)

  d = seconds (x);
  d.Format = fmt;

endfunction

## Column means and sample deviations with the missing entries left out.
function m = colmean (x)

  m = NaN (1, columns (x));
  for c = 1:columns (x)
    k = x(! isnan (x(:,c)), c);
    if (! isempty (k))
      m(c) = sum (k) / numel (k);
    endif
  endfor

endfunction

function sd = colstd (x)

  sd = NaN (1, columns (x));
  for c = 1:columns (x)
    k = x(! isnan (x(:,c)), c);
    if (numel (k) > 1)
      mu = sum (k) / numel (k);
      sd(c) = sqrt (sum ((k - mu) .^ 2) / (numel (k) - 1));
    elseif (numel (k) == 1)
      ## One observation deviates from itself by nothing.
      sd(c) = 0;
    endif
  endfor

endfunction

## How one variable is described on its own line of a summary.
function out = summaryVarLine (name, val, e)

  t = class (val);
  nr = size (val, 1);
  nc = size (val, 2);
  if (isa (val, 'table') || isa (val, 'timetable') || nr == 0)
    d = sprintf ('%dx%d %s', nr, nc, t);
  elseif (nc > 1)
    d = sprintf ('%d-column %s', nc, t);
  elseif (islogical (val))
    d = sprintf ('logical (%d true)', sum (val));
  elseif (isa (val, 'categorical'))
    n = numel (categories (val));
    if (isordinal (val))
      d = sprintf ('ordinal categorical (%d categories)', n);
    else
      d = sprintf ('categorical (%d categories)', n);
    endif
  elseif (iscellstr (val))
    d = 'cell array of character vectors';
  else
    d = t;
  endif
  extra = {};
  if (! isempty (e.Units))
    extra{end+1} = e.Units;
  endif
  if (! isempty (e.Description))
    extra{end+1} = e.Description;
  endif
  if (! isempty (extra))
    d = sprintf ('%s (%s)', d, strjoin (extra, ', '));
  endif
  out = sprintf ('    %s: %s', name, d);

endfunction

## The rows of the statistics block.  A multi-column variable contributes one
## row per column, named the way a subscript would reach it, and a column of
## the block is kept only where something reports that statistic.
function [labels, cells, cols] = summaryStatRows (s, names)

  stats = {'NumMissing', 'Min', 'Median', 'Max', 'Mean', 'Std'};
  keep = false (1, numel (stats));
  labels = {};
  cells = {};
  for i = 1:numel (names)
    e = s.(names{i});
    if (! isfield (e, 'NumMissing'))
      continue;
    endif
    nc = max (1, numel (e.NumMissing));
    for c = 1:nc
      if (nc == 1)
        labels{end+1} = names{i};
      else
        labels{end+1} = sprintf ('%s(:,%d)', names{i}, c);
      endif
      row = cell (1, numel (stats));
      for k = 1:numel (stats)
        row{k} = '';
        if (isfield (e, stats{k}))
          v = e.(stats{k});
          if (numel (v) >= c)
            row{k} = summaryCell (v(c));
            keep(k) = true;
          endif
        endif
      endfor
      cells{end+1} = row;
    endfor
  endfor
  cols = stats(keep);
  for i = 1:numel (cells)
    cells{i} = cells{i}(keep);
  endfor

endfunction

## One statistic rendered for the block: a whole number bare, anything else
## to four decimals, and a typed value through its own display.
function out = summaryCell (v)

  try
    if (isnumeric (v))
      if (isreal (v) && ! isnan (v) && ! isinf (v) && v == fix (v))
        out = sprintf ('%d', v);
      else
        out = strtrim (sprintf ('%.4f', v));
        if (isnan (v))
          out = 'NaN';
        elseif (isinf (v))
          out = sprintf ('%g', v);
        elseif (! isreal (v))
          out = strtrim (num2str (v));
        endif
      endif
    elseif (islogical (v))
      out = sprintf ('%d', v);
    elseif (ischar (v))
      out = v;
    else
      out = char (string (v));
    endif
  catch
    out = '';
  end_try_catch

endfunction

## Lay the statistics block out in columns, each as wide as the widest thing
## in it and its heading, values to the right.
function summaryStatTable (labels, cells, cols)

  lw = 0;
  for i = 1:numel (labels)
    lw = max (lw, numel (labels{i}));
  endfor
  cw = zeros (1, numel (cols));
  for k = 1:numel (cols)
    cw(k) = numel (cols{k});
    for i = 1:numel (cells)
      cw(k) = max (cw(k), numel (cells{i}{k}));
    endfor
  endfor
  line = [blanks(4 + lw)];
  for k = 1:numel (cols)
    line = [line, blanks(4), padleft(cols{k}, cw(k))];
  endfor
  fprintf ('%s\n', line);
  for i = 1:numel (cells)
    line = ['    ', padright(labels{i}, lw)];
    for k = 1:numel (cols)
      line = [line, blanks(4), padleft(cells{i}{k}, cw(k))];
    endfor
    fprintf ('%s\n', line);
  endfor

endfunction

function out = padleft (str, w)
  out = [blanks(w - numel (str)), str];
endfunction

function out = padright (str, w)
  out = [str, blanks(w - numel (str))];
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

## Format a datetime column as a column cell of ISO 8601 strings for
## 'table2ods'. NaT values yield an empty string, which the writer records as a
## missing (empty) cell.  The wall-clock components are used; any TimeZone is
## not encoded in the value (mirroring the datetime display round-trip of the
## CSV path).
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

## Format a seconds value for an ISO 8601 string: a two-digit integer when
## whole, otherwise a fractional part (up to microseconds) with trailing zeros
## trimmed.
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
