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
##                    **    Row label hooks    **                             ##
################################################################################
##                                                                            ##
## Every subclass must implement all seven.  Octave's classdef has no         ##
## 'methods (Abstract)' block, so the contract cannot be declared; these      ##
## raising defaults stand in for it, and name the subclass that is missing    ##
## one because 'class (this)' resolves downwards.                             ##
##                                                                            ##
## 'hasRowLabels'      whether the object carries row labels at all           ##
## 'getRowLabels'      the labels themselves, in their own type               ##
## 'rowLabelName'      the name the labels are known by                       ##
## 'rowLabelStrings'   the labels rendered for display                        ##
## 'subsetRowLabels'   the object with its labels subset by an index          ##
## 'clearRowLabels'    the object with its labels removed                     ##
## 'resolveRowRef'     a row reference resolved to row indices                ##
##                                                                            ##
################################################################################

  methods (Access = protected)

    function tf = hasRowLabels (this)
      error ("%s: subclass must implement hasRowLabels.", class (this));
    endfunction

    function out = getRowLabels (this)
      error ("%s: subclass must implement getRowLabels.", class (this));
    endfunction

    function out = rowLabelName (this)
      error ("%s: subclass must implement rowLabelName.", class (this));
    endfunction

    function out = rowLabelStrings (this)
      error ("%s: subclass must implement rowLabelStrings.", class (this));
    endfunction

    function this = subsetRowLabels (this, ixRows)
      error ("%s: subclass must implement subsetRowLabels.", class (this));
    endfunction

    function this = clearRowLabels (this)
      error ("%s: subclass must implement clearRowLabels.", class (this));
    endfunction

    function ixRows = resolveRowRef (this, rowRef)
      error ("%s: subclass must implement resolveRowRef.", class (this));
    endfunction

  endmethods

endclassdef
