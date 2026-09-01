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
## @deftp {datatypes.tabular} {} TabularProperties
##
## Abstract superclass of the properties objects of @code{table} and
## @code{timetable}.
##
## It carries the metadata both classes share and renders it; each subclass
## adds the metadata of its own row labels and states the order the whole
## set is displayed in.  Objects of these classes are what
## @qcode{@var{tbl}.Properties} returns; they are never constructed directly.
##
## @end deftp
classdef (Abstract) TabularProperties

  properties
    Description = ''
    UserData = []
    DimensionNames = {}
    VariableNames = {}
    VariableTypes = {}
    VariableDescriptions = {}
    VariableUnits = {}
    VariableContinuity = []
    CustomProperties = []
  endproperties

  methods (Access = protected)

    ## The property names in the order they are displayed.  Every subclass
    ## must state it: inheritance alone would append the subclass's own row
    ## label metadata after 'CustomProperties', where MATLAB shows it before.
    function names = displayOrder (this)
      error ("%s: subclass must implement displayOrder.", class (this));
    endfunction

  endmethods

  methods (Hidden)

    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    function disp (this)
      fprintf ("\n  %s with properties:\n\n", strsplit (class (this), '.'){end});
      names = displayOrder (this);
      for i = 1:numel (names)
        if (strcmp (names{i}, 'CustomProperties'))
          fprintf ("%s\n", customPropertyText (this));
        else
          fprintf ("%+24s: %s\n", names{i}, ...
                   datatypes.tabular.TabularProperties.formatValue ( ...
                     this.(names{i})));
        endif
      endfor
    endfunction

  endmethods

  methods (Access = private)

    function txt = customPropertyText (this)
      if (isempty (this.CustomProperties))
        txt = ["        CustomProperties: No custom properties are set.\n", ...
               "      Use 'addprop' and 'rmprop' methods to modify", ...
               " CustomProperties."];
        return;
      endif
      txt = ["\n   Custom Properties (access using t.Properties.", ...
             "CustomProperties.<name>):"];
      cpNames = fieldnames (this.CustomProperties);
      for i = 1:numel (cpNames)
        cpValue = subsref (this.CustomProperties, ...
                           substruct ('.', cpNames{i}));
        txt = [txt, sprintf("\n%+24s: %s", cpNames{i}, ...
               datatypes.tabular.TabularProperties.formatValue (cpValue))];
      endfor
    endfunction

  endmethods

  methods (Static, Hidden)

    ## Render one metadata value the way MATLAB renders it in this listing.
    ## Called from a private method, so it is Hidden rather than protected.
    function str = formatValue (val)
      if (ischar (val))
        str = sprintf ("'%s'", val);
      elseif (isempty (val))
        if (iscell (val))
          str = "{}";
        else
          str = "[]";
        endif
      elseif (iscellstr (val))
        ## Only a row of names is written out; anything else is summarized by
        ## its size, as MATLAB does.  'RowNames' is the property this reaches.
        if (all (cellfun (@isempty, val)))
          str = "{}";
        elseif (isrow (val))
          str = ["{", strtrim(sprintf ("'%s'  ", val{:})), "}"];
        else
          str = sprintf ("{%s cell}", sizestr (val));
        endif
      elseif (isa (val, 'string'))
        str = ["[", strtrim(sprintf ("""%s""  ", cellstr(val){:})), "]"];
      elseif (islogical (val) || isnumeric (val))
        if (isscalar (val))
          str = ["[", strtrim(disp (val)), "]"];
        else
          str = sprintf ("[%s %s]", sizestr (val), class (val));
        endif
      else
        str = sprintf ("{%s %s}", sizestr (val), class (val));
      endif
    endfunction

  endmethods

endclassdef

## The size of a value the way it is written in this listing, as in '2x1'.
function str = sizestr (val)
  str = strjoin (arrayfun (@(x) sprintf ("%d", x), size (val), ...
                           "UniformOutput", false), "x");
endfunction
