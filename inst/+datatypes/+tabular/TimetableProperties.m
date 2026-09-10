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
## describe the row times, @qcode{RowTimes}, @qcode{StartTime},
## @qcode{SampleRate} and @qcode{TimeStep}, together with @qcode{Events}.
## It is what
## @qcode{@var{tt}.Properties} returns and cannot be constructed directly.
##
## @end deftp
classdef TimetableProperties < datatypes.tabular.TabularProperties

  properties
    RowTimes = []
    StartTime = []
    SampleRate = []
    TimeStep = []
    Events = []
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
               'TimeStep', 'Events', 'CustomProperties'};
    endfunction

  endmethods

endclassdef

## The properties object of a timetable.  It cannot be constructed, so every
## fixture reaches it through a timetable.  Measured on MATLAB R2024a.

## Test the object a timetable hands back is this class
%!assert_equal (class (timetable (1, 'RowTimes', datetime (2024, 1, 1)) ...
%!                     .Properties), 'datatypes.tabular.TimetableProperties')

## Test the display order names the time metadata before the custom
## properties, where a table names its row labels
%!test
%! TT = timetable ((1:2)', 'TimeStep', hours (1), ...
%!                 'StartTime', datetime (2024, 1, 1));
%! p = properties (TT.Properties);
%! assert_equal (p(end-6:end-1), {'VariableContinuity'; 'RowTimes'; ...
%!                                'StartTime'; 'SampleRate'; 'TimeStep'; ...
%!                                'Events'});
%!test
%! TT = timetable ((1:2)', 'TimeStep', hours (1), ...
%!                 'StartTime', datetime (2024, 1, 1));
%! p = properties (TT.Properties);
%! assert_equal (p{end}, 'CustomProperties');
%! assert_equal (any (strcmp (p, 'RowNames')), false);
## Test 'fieldnames' answers as 'properties' does
%!test
%! TT = timetable ((1:2)', 'TimeStep', hours (1), ...
%!                 'StartTime', datetime (2024, 1, 1));
%! assert_equal (fieldnames (TT.Properties), properties (TT.Properties));

## Test the time metadata reads back through the object
%!test
%! TT = timetable ((1:2)', 'TimeStep', hours (1), ...
%!                 'StartTime', datetime (2024, 1, 1));
%! assert_equal (TT.Properties.TimeStep, hours (1));
%! assert_equal (TT.Properties.StartTime, datetime (2024, 1, 1));
%! assert_equal (TT.Properties.DimensionNames, {'Time', 'Variables'});
