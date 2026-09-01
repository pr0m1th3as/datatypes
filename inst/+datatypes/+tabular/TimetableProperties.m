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
## @deftp {datatypes.tabular} {} TimetableProperties
##
## The properties object of a @code{timetable}.
##
## It carries the metadata every tabular class shares, from
## @code{datatypes.tabular.TabularProperties}, and adds the four that
## describe the row times: @qcode{RowTimes}, @qcode{StartTime},
## @qcode{SampleRate} and @qcode{TimeStep}.  It is what
## @qcode{@var{tt}.Properties} returns and cannot be constructed directly.
##
## @end deftp
classdef TimetableProperties < datatypes.tabular.TabularProperties

  properties
    RowTimes = []
    StartTime = []
    SampleRate = []
    TimeStep = []
  endproperties

  methods (Access = {?timetable})

    function this = TimetableProperties (s, cpTypes)
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

    ## MATLAB lists the row time metadata after the variable metadata and
    ## before the custom properties, which is where a table lists 'RowNames'
    ## and is not where inheritance would place it.
    function names = displayOrder (this)
      names = {'Description', 'UserData', 'DimensionNames', 'VariableNames', ...
               'VariableTypes', 'VariableDescriptions', 'VariableUnits', ...
               'VariableContinuity', 'RowTimes', 'StartTime', 'SampleRate', ...
               'TimeStep', 'CustomProperties'};
    endfunction

  endmethods

endclassdef
