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

  properties (Constant, Access = private, Hidden)

    ## 'Events' is declared by 'TimetableProperties' and a classdef subclass
    ## cannot delete an inherited property.  Leaving it out of 'displayOrder'
    ## is not enough either, since a property omitted from that list is
    ## deliberately appended rather than dropped, so it is shadowed here by
    ## one that is not declared in public terms and does not reach the list
    ## at all.  'subsref' below refuses the name so that asking for it says
    ## why rather than reporting an access error.
    Events = []

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

  methods (Hidden)

    ## 'Events' is declared by 'TimetableProperties' and a classdef subclass
    ## cannot delete an inherited property, so the name is refused here
    ## rather than answered with the default it would otherwise carry.  Every
    ## other name goes on to the inherited reader.
    function varargout = subsref (this, s)
      if (strcmp (s(1).type, '.'))
        name = s(1).subs;
        if (isstring (name) && isscalar (name))
          name = char (name);
        endif
        if (ischar (name) && strcmp (name, 'Events'))
          error (strcat ("eventtable: 'Events' is not a property of an", ...
                         " event table; an event table cannot carry an", ...
                         " event table."));
        endif
      endif
      varargout{1} = subsref@datatypes.tabular.TabularProperties (this, s);
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

## The properties object of an event table.  It cannot be constructed, so
## every fixture reaches it through an event table.  Measured on MATLAB
## R2024a.

## Test the object an event table hands back is this class
%!assert_equal (class (eventtable (datetime (2024, 1, 1)).Properties), ...
%!              'datatypes.tabular.EventtableProperties')

## Test the three event designations are named first, before the metadata
## every tabular class shares
%!test
%! E = eventtable (datetime (2024, 1, 1) + hours ((0:1)'));
%! p = properties (E.Properties);
%! assert_equal (p(1:3), {'EventLabelsVariable'; 'EventLengthsVariable'; ...
%!                        'EventEndsVariable'});
%! assert_equal (p{4}, 'Description');
## Test the time metadata is named as a timetable's is
%!test
%! E = eventtable (datetime (2024, 1, 1) + hours ((0:1)'));
%! p = properties (E.Properties);
%! assert_equal (p{end}, 'CustomProperties');
%! assert_equal (any (strcmp (p, 'RowTimes')), true);
## Test 'fieldnames' answers as 'properties' does
%!test
%! E = eventtable (datetime (2024, 1, 1) + hours ((0:1)'));
%! assert_equal (fieldnames (E.Properties), properties (E.Properties));

## An event table carries events; it is not carried by one, so 'Events' is
## the one property of a timetable it does not have.
## Test 'Events' is absent from the display order
%!test
%! E = eventtable (datetime (2024, 1, 1) + hours ((0:1)'));
%! assert_equal (any (strcmp (properties (E.Properties), 'Events')), false);
## Test reading it is refused rather than answered
%!error <eventtable: 'Events' is not a property of an event table; an event table cannot carry an event table.> ...
%! eventtable (datetime (2024, 1, 1)).Properties.Events;

## Test the designations read back through the object
%!test
%! E = eventtable (datetime (2024, 1, 1) + hours ([1 3])');
%! assert_equal (E.Properties.EventLabelsVariable, 'EventLabels');
%! assert_equal (isempty (E.Properties.EventLengthsVariable), true);
%! assert_equal (isempty (E.Properties.EventEndsVariable), true);
