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
## @deftp {datatypes.tabular} {} TableProperties
##
## The properties object of a @code{table}.
##
## It carries the metadata every tabular class shares, from
## @code{datatypes.tabular.TabularProperties}, and adds @qcode{RowNames}.
## It is what @qcode{@var{tbl}.Properties} returns and cannot be constructed
## directly.
##
## @end deftp
classdef TableProperties < datatypes.tabular.TabularProperties

  properties
    RowNames = {}
  endproperties

  methods (Access = {?table})

    function this = TableProperties (s, cpTypes)
      if (nargin < 1)
        return;
      endif
      if (nargin < 2)
        cpTypes = struct ();
      endif
      names = fieldnames (s);
      for i = 1:numel (names)
        if (strcmp (names{i}, 'CustomProperties'))
          this.CustomProperties = datatypes.tabular.CustomProperties ( ...
                                    s.CustomProperties, cpTypes);
        else
          this.(names{i}) = s.(names{i});
        endif
      endfor
    endfunction

  endmethods

  methods (Access = protected)

    ## MATLAB lists the row label metadata after the variable metadata and
    ## before the custom properties, which is not where inheritance would
    ## place it.
    function names = displayOrder (this)
      names = {'Description', 'UserData', 'DimensionNames', 'VariableNames', ...
               'VariableTypes', 'VariableDescriptions', 'VariableUnits', ...
               'VariableContinuity', 'RowNames', 'CustomProperties'};
    endfunction

  endmethods

endclassdef
