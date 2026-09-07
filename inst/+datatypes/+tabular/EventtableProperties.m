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
## @deftp {datatypes.tabular} {} EventtableProperties
##
## The properties object of an @code{eventtable}.
##
## It carries everything a @code{timetable} publishes, from
## @code{datatypes.tabular.TimetableProperties}, and adds the three that say
## which variables describe the events: @qcode{EventLabelsVariable},
## @qcode{EventLengthsVariable} and @qcode{EventEndsVariable}.  It is what
## @qcode{@var{et}.Properties} returns and cannot be constructed directly.
##
## The three are listed first, ahead of @qcode{Description}, because they are
## what distinguishes an event table from the timetable it otherwise is.
##
## @end deftp
classdef EventtableProperties < datatypes.tabular.TimetableProperties

  properties
    EventLabelsVariable = []
    EventLengthsVariable = []
    EventEndsVariable = []
  endproperties

  methods (Access = {?eventtable})

    function this = EventtableProperties (s, cpTypes)
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

    ## The three event properties come first, ahead of the metadata every
    ## tabular class shares, which is neither where inheritance would place
    ## them nor where the row time properties sit.
    function names = displayOrder (this)
      names = {'EventLabelsVariable', 'EventLengthsVariable', ...
               'EventEndsVariable', 'Description', 'UserData', ...
               'DimensionNames', 'VariableNames', 'VariableTypes', ...
               'VariableDescriptions', 'VariableUnits', ...
               'VariableContinuity', 'RowTimes', 'StartTime', ...
               'SampleRate', 'TimeStep', 'CustomProperties'};
    endfunction

  endmethods

endclassdef
