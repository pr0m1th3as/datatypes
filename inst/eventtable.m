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

classdef eventtable < timetable
  ## -*- texinfo -*-
  ## @deftp {datatypes} eventtable
  ##
  ## Timetable of events, describing what was happening rather than what was
  ## measured.
  ##
  ## An event table is a @code{timetable} whose rows are events.  Each row has
  ## a time, optionally a label naming the event, and optionally a length or
  ## an end time making it an interval rather than an instant.  It exists so
  ## that the rows of another timetable can be selected by what was happening
  ## when they were recorded, rather than by the clock alone.
  ##
  ## An event table @emph{is} a timetable and inherits every one of its
  ## methods.  @code{help} does not follow method inheritance, so
  ## @code{help eventtable.sortrows} does not resolve and
  ## @code{help timetable.sortrows} is where that method is documented; where
  ## a method behaves differently for an event table, its own documentation
  ## says so.
  ##
  ## Three properties say which variables describe the events, and each holds
  ## a variable @emph{name} rather than the values themselves:
  ##
  ## @itemize
  ## @item
  ## @qcode{EventLabelsVariable}, the variable naming each event.
  ##
  ## @item
  ## @qcode{EventLengthsVariable}, how long each event lasted.
  ##
  ## @item
  ## @qcode{EventEndsVariable}, when each event finished.
  ##
  ## @end itemize
  ##
  ## The last two are mutually exclusive: an event says its extent one way or
  ## the other, never both.  An event with neither is an instant.
  ##
  ## Because the properties hold names, an operation that stops a name
  ## resolving affects the property.  Removing the variable clears it, and so
  ## does deleting it by assigning an empty matrix; @emph{renaming} the
  ## variable carries the property across to the new name, which is a
  ## deliberate departure from MATLAB, where the designation is lost and
  ## renaming back does not restore it.  Moving a variable or converting its
  ## type leaves the property alone.
  ##
  ## An event table cannot itself carry an event table.  It has no
  ## @qcode{Events} property, and asking for one raises.
  ##
  ## An interval event is half-open on the right: an event beginning at
  ## 02:00 and lasting two hours covers 02:00 and 03:00 but not 04:00.  An
  ## event of zero length therefore covers nothing at all.
  ##
  ## @seealso{timetable, table, istimetable}
  ## @end deftp

  properties

    ## -*- texinfo -*-
    ## @deftp {eventtable} {property} EventLabelsVariable
    ##
    ## Variable naming each event
    ##
    ## Variable naming each event, given as the name of one of the event
    ## table's own variables, or empty when no variable names the events.
    ## Assigning a name points the property at that variable; assigning
    ## @code{[]} clears it.  An event table built from a vector of times
    ## alone labels its events @qcode{"Event 1"}, @qcode{"Event 2"} and so
    ## on and points this property at them, while one built from a timetable
    ## leaves it empty, because nothing says which of that timetable's
    ## variables holds labels.
    ##
    ## @end deftp
    EventLabelsVariable = []

    ## -*- texinfo -*-
    ## @deftp {eventtable} {property} EventLengthsVariable
    ##
    ## Variable holding how long each event lasted
    ##
    ## Variable holding how long each event lasted, given as the name of one
    ## of the event table's own variables, or empty when the events are
    ## instants.  The variable holds a @code{duration} or a
    ## @code{calendarDuration}.  It cannot be set while
    ## @qcode{EventEndsVariable} is set; an event states its extent as a
    ## length or as an end, not as both.
    ##
    ## @end deftp
    EventLengthsVariable = []

    ## -*- texinfo -*-
    ## @deftp {eventtable} {property} EventEndsVariable
    ##
    ## Variable holding when each event finished
    ##
    ## Variable holding when each event finished, given as the name of one of
    ## the event table's own variables, or empty when the events are
    ## instants.  The variable holds the same type as the row times.  It
    ## cannot be set while @qcode{EventLengthsVariable} is set; an event
    ## states its extent as a length or as an end, not as both.
    ##
    ## @end deftp
    EventEndsVariable = []

  endproperties

  properties (Constant, Access = private, Hidden)

    ## An event table is not something that can carry an event table, so the
    ## property inherited from 'timetable' is shadowed by one that cannot be
    ## read, written or discovered.  A classdef subclass cannot delete an
    ## inherited property, and hiding it alone would leave the slot writable:
    ## a merge body storing an event table here would build a cycle that
    ## every recursive operation would follow, and nothing would report it at
    ## the time.  Constant and private together make that state
    ## unrepresentable rather than merely forbidden.  'isprop' still answers
    ## true for the name, and the value is never anything but this one.
    ##
    ## Nothing inherited from 'timetable' may name this property; the two
    ## accessors below are the only way in, and both refuse.
    Events = []

  endproperties

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {eventtable} {@var{et} =} eventtable ()
    ## @deftypefnx {eventtable} {@var{et} =} eventtable (@var{eventTimes})
    ## @deftypefnx {eventtable} {@var{et} =} eventtable (@var{tt})
    ## @deftypefnx {eventtable} {@var{et} =} eventtable (@dots{}, @var{Name}, @var{Value})
    ##
    ## Create an event table.
    ##
    ## @code{@var{et} = eventtable ()} returns an empty event table with no
    ## events and no variables.
    ##
    ## @code{@var{et} = eventtable (@var{eventTimes})} returns an event table
    ## whose events happen at @var{eventTimes}, a @code{datetime} or
    ## @code{duration} vector.  With no labels given, the events are labelled
    ## @qcode{"Event 1"}, @qcode{"Event 2"} and so on in a variable named
    ## @qcode{EventLabels}, and @qcode{EventLabelsVariable} names it.
    ##
    ## @code{@var{et} = eventtable (@var{tt})} converts the timetable
    ## @var{tt}, whose row times become the event times and whose variables
    ## are kept as they are.  No variable is guessed to be the labels, so
    ## @qcode{EventLabelsVariable} is left empty unless
    ## @qcode{'EventLabelsVariable'} says which one it is.
    ##
    ## The following @var{Name}-@var{Value} options are supported:
    ##
    ## @multitable @columnfractions 0.28 0.72
    ## @headitem @var{Name} @tab @var{Value}
    ##
    ## @item @qcode{'EventLabels'} @tab A scalar or a vector with one element
    ## per event, naming the events.  It may be of any type other than
    ## @code{datetime}, @code{duration}, @code{calendarDuration},
    ## @code{table} and @code{timetable}.  A scalar labels every event alike.
    ## It is added as a variable named @qcode{EventLabels}.
    ##
    ## @item @qcode{'EventLengths'} @tab A @code{duration} or
    ## @code{calendarDuration}, scalar or one element per event, saying how
    ## long each event lasted.  Only a @code{duration} is accepted when the
    ## event times are durations.  It is added as a variable named
    ## @qcode{EventLengths}.
    ##
    ## @item @qcode{'EventEnds'} @tab A scalar or one element per event, of
    ## the same type as the event times, saying when each event finished.  It
    ## is added as a variable named @qcode{EventEnds}.
    ##
    ## @item @qcode{'EventLabelsVariable'} @tab The name of a variable of
    ## @var{tt} that names the events.  Only with a timetable input.
    ##
    ## @item @qcode{'EventLengthsVariable'} @tab The name of a variable of
    ## @var{tt} that holds the event lengths.  Only with a timetable input.
    ##
    ## @item @qcode{'EventEndsVariable'} @tab The name of a variable of
    ## @var{tt} that holds the event end times.  Only with a timetable input.
    ##
    ## @end multitable
    ##
    ## A length and an end are mutually exclusive, however they are given,
    ## and so are @qcode{'EventLabels'} and @qcode{'EventLabelsVariable'}.
    ##
    ## @seealso{timetable, table2timetable}
    ## @end deftypefn
    function this = eventtable (varargin)

      ## An empty event table: no events, no variables, nothing designated.
      if (nargin == 0)
        this = this@timetable ();
        return
      endif

      optNames = {'EventLabels', 'EventLengths', 'EventEnds', ...
                  'EventLabelsVariable', 'EventLengthsVariable', ...
                  'EventEndsVariable'};
      dfValues = {missing, missing, missing, missing, missing, missing};
      [Labels, Lengths, Ends, LabelsVar, LengthsVar, EndsVar, args] = ...
        parsePairedArguments (optNames, dfValues, varargin(:));

      if (numel (args) != 1)
        error (strcat ("eventtable: the event times or the timetable they", ...
                       " come from must be given exactly once."));
      endif
      src = args{1};
      fromTT = isa (src, 'timetable');
      if (! (fromTT || isdatetime (src) || isduration (src)))
        error (strcat ("eventtable: first input must be a timetable, or a", ...
                       " datetime or duration vector."));
      endif

      ## The three '*Variable' options name a variable of the input, so they
      ## need an input that has variables to name.
      if (! fromTT)
        named = {'EventLabelsVariable', LabelsVar; ...
                 'EventLengthsVariable', LengthsVar; ...
                 'EventEndsVariable', EndsVar};
        for i = 1:rows (named)
          if (wasGiven (named{i,2}))
            error (strcat ("eventtable: '%s' names a variable of a", ...
                           " timetable and cannot be used with a vector", ...
                           " of event times."), named{i,1});
          endif
        endfor
      endif

      ## An event states its extent one way or the other, and is labelled by
      ## given values or by a named variable, never by both.
      if ((wasGiven (Lengths) || wasGiven (LengthsVar))
          && (wasGiven (Ends) || wasGiven (EndsVar)))
        error (strcat ("eventtable: specify either the event lengths or", ...
                       " the event ends, but not both."));
      endif
      if (wasGiven (Labels) && wasGiven (LabelsVar))
        error (strcat ("eventtable: specify either the event labels or an", ...
                       " event labels variable, but not both."));
      endif

      ## The event times, and the variables the input already carries.
      if (fromTT)
        eventTimes = src.Properties.RowTimes;
        varNames = src.Properties.VariableNames;
        varValues = cell (1, numel (varNames));
        for i = 1:numel (varNames)
          varValues{i} = getvar (src, i);
        endfor
        dimNames = src.Properties.DimensionNames;
      else
        eventTimes = src(:);
        varNames = {};
        varValues = {};
        dimNames = {'Time', 'Variables'};
      endif
      nev = numel (eventTimes);

      ## Values given as options become variables of the event table, named
      ## after the option that carried them.  A vector input with no labels
      ## given gets generated ones, which is the only place a label is
      ## invented; a timetable input never has one guessed for it.
      lblVar = [];
      lenVar = [];
      endVar = [];
      if (wasGiven (Labels))
        Labels = checkEventLabels (Labels, nev);
        [varNames, varValues] = addEventVar (varNames, varValues, ...
                                             'EventLabels', Labels);
        lblVar = 'EventLabels';
      elseif (! fromTT)
        auto = cell (nev, 1);
        for i = 1:nev
          auto{i} = sprintf ("Event %d", i);
        endfor
        [varNames, varValues] = addEventVar (varNames, varValues, ...
                                             'EventLabels', string (auto));
        lblVar = 'EventLabels';
      endif
      if (wasGiven (Lengths))
        Lengths = checkEventLengths (Lengths, nev, eventTimes);
        [varNames, varValues] = addEventVar (varNames, varValues, ...
                                             'EventLengths', Lengths);
        lenVar = 'EventLengths';
      endif
      if (wasGiven (Ends))
        Ends = checkEventEnds (Ends, nev, eventTimes);
        [varNames, varValues] = addEventVar (varNames, varValues, ...
                                             'EventEnds', Ends);
        endVar = 'EventEnds';
      endif

      this = this@timetable (eventTimes, varValues{:}, ...
                             'VariableNames', varNames, ...
                             'DimensionNames', dimNames);

      ## The metadata of a converted timetable travels with its variables.
      if (fromTT)
        this = adoptSourceMetadata (this, src);
      endif

      ## The '*Variable' options name a variable that is already there, so
      ## they are resolved once the object exists to resolve them against.
      if (wasGiven (LabelsVar))
        lblVar = checkEventVariable (this, LabelsVar, 'EventLabelsVariable');
      endif
      if (wasGiven (LengthsVar))
        lenVar = checkEventVariable (this, LengthsVar, ...
                                     'EventLengthsVariable');
      endif
      if (wasGiven (EndsVar))
        endVar = checkEventVariable (this, EndsVar, 'EventEndsVariable');
      endif
      this.EventLabelsVariable = lblVar;
      this.EventLengthsVariable = lenVar;
      this.EventEndsVariable = endVar;
    endfunction

  endmethods

  methods (Static)

    ## -*- texinfo -*-
    ## @deftypefn  {eventtable} {@var{et} =} eventtable.empty ()
    ## @deftypefnx {eventtable} {@var{et} =} eventtable.empty (@var{r}, @var{v})
    ## @deftypefnx {eventtable} {@var{et} =} eventtable.empty (@var{sz})
    ##
    ## Create an empty event table.
    ##
    ## The arguments are those of @code{timetable.empty}, which this is in
    ## every respect but the class of what comes back: a 0-by-0 event table
    ## with no arguments, and otherwise one of @var{r} rows and @var{v}
    ## variables, at least one of the two being zero.  None of the three
    ## event properties is set, there being no variable for one to name.
    ##
    ## @seealso{eventtable, timetable}
    ## @end deftypefn
    function et = empty (varargin)
      [sz, errmsg] = tabular.emptySize ('eventtable', varargin);
      if (! isempty (errmsg))
        error ('eventtable.empty: %s', errmsg);
      endif
      et = eventtable (timetable ('Size', sz, 'VariableTypes', ...
                                  repmat ({'double'}, 1, sz(2)), ...
                                  'RowTimes', NaT (sz(1), 1)));
    endfunction

  endmethods

  methods (Access = protected)

    ## The three event properties travel with the four a timetable publishes
    ## about its row times.  'Events' is not among them: an event table does
    ## not carry one, so the field goes before the properties object is
    ## handed the rest.
    function out = rowLabelProperties (this)
      out = rowLabelProperties@timetable (this);
      out = rmfield (out, 'Events');
      out.EventLabelsVariable = this.EventLabelsVariable;
      out.EventLengthsVariable = this.EventLengthsVariable;
      out.EventEndsVariable = this.EventEndsVariable;
    endfunction

    ## An event table has no attached event table and never will, so the
    ## reader answers with nothing rather than reaching the shadowed
    ## property, which it could not read anyway.
    function out = eventsOf (this)
      out = [];
    endfunction

    ## A designation naming a variable that is no longer there is dropped.
    ## Removing the variable, and deleting it by assigning an empty matrix,
    ## both arrive here; moving one or converting its type do too and change
    ## nothing, the name still resolving.
    function this = varsChanged (this)
      names = this.VariableNames;
      this.EventLabelsVariable = carried (this.EventLabelsVariable, names);
      this.EventLengthsVariable = carried (this.EventLengthsVariable, names);
      this.EventEndsVariable = carried (this.EventEndsVariable, names);
    endfunction

    ## A designation follows the variable it names across a rename.  MATLAB
    ## clears it instead, and does not restore it when the variable is
    ## renamed back, so a cosmetic operation loses the designation for good;
    ## see deviation D6.
    function this = varsRenamed (this, oldNames, newNames)
      this.EventLabelsVariable = renamed (this.EventLabelsVariable, ...
                                          oldNames, newNames);
      this.EventLengthsVariable = renamed (this.EventLengthsVariable, ...
                                           oldNames, newNames);
      this.EventEndsVariable = renamed (this.EventEndsVariable, ...
                                        oldNames, newNames);
    endfunction

    ## An event table is the more derived class, so a result built from one
    ## and a plain timetable is an event table however the two were ordered.
    ## The three properties come from this operand; one naming a variable the
    ## result does not carry is left unset, the designation having nothing to
    ## point at.
    function tbl = promoteResult (this, tbl)
      if (isa (tbl, 'eventtable'))
        return;
      endif
      tbl = eventtable (tbl);
      names = tbl.VariableNames;
      tbl.EventLabelsVariable = carried (this.EventLabelsVariable, names);
      tbl.EventLengthsVariable = carried (this.EventLengthsVariable, names);
      tbl.EventEndsVariable = carried (this.EventEndsVariable, names);
    endfunction

    ## An event table has nothing to detach.
    function this = detachEvents (this)
    endfunction

    ## A binary operation over event tables has nothing to carry: an event
    ## table holds no event table of its own.
    function [tbl, errmsg] = carryEvents (tbl, ops)
      errmsg = '';
    endfunction

    ## Nothing may attach an event table to an event table.
    function this = setEventsOf (this, val)
      error (strcat ("eventtable.subsasgn: 'Events' is not a property of", ...
                     " an event table; an event table cannot carry an", ...
                     " event table."));
    endfunction

    ## One of the seven assigned.  The three event properties hold a variable
    ## name, so a name is checked against the variables this object has and
    ## an empty matrix clears the designation; anything else is left to the
    ## timetable, which owns the row time four.
    function [this, handled] = setRowLabelProperty (this, name, val, chain_s)
      handled = true;
      switch (name)
        case 'EventLabelsVariable'
          this.EventLabelsVariable = checkEventVariable (this, val, name);

        case 'EventLengthsVariable'
          val = checkEventVariable (this, val, name);
          if (! isempty (val) && ! isempty (this.EventEndsVariable))
            error (strcat ("eventtable.subsasgn: 'EventLengthsVariable'", ...
                           " cannot be set while 'EventEndsVariable' is", ...
                           " set; clear that one first."));
          endif
          this.EventLengthsVariable = val;

        case 'EventEndsVariable'
          val = checkEventVariable (this, val, name);
          if (! isempty (val) && ! isempty (this.EventLengthsVariable))
            error (strcat ("eventtable.subsasgn: 'EventEndsVariable'", ...
                           " cannot be set while 'EventLengthsVariable'", ...
                           " is set; clear that one first."));
          endif
          this.EventEndsVariable = val;

        otherwise
          [this, handled] = setRowLabelProperty@timetable (this, name, ...
                                                           val, chain_s);
      endswitch
    endfunction

    function out = makeProperties (this)
      out = datatypes.tabular.EventtableProperties (getProperties (this), ...
                                                    this.CustomPropTypes);
    endfunction

    ## The variable metadata a converted timetable brings with it.  The
    ## variables themselves went in through the constructor; this is
    ## everything about them, and about the object, that the constructor has
    ## no argument for.  It is a method rather than a local function because
    ## it reads the source's protected state.
    function this = adoptSourceMetadata (this, src)
      props = getProperties (src);
      this.Description = props.Description;
      this.UserData = props.UserData;
      this.VariableDescriptions = props.VariableDescriptions;
      this.VariableUnits = props.VariableUnits;
      this.VariableContinuity = props.VariableContinuity;
      this.CustomProperties = props.CustomProperties;
      this.CustomPropTypes = customPropTypes (src);
    endfunction

  endmethods

endclassdef

## A value given as an option becomes a variable, appended after whatever the
## input already carried.  A name the input already uses is a conflict the
## caller has to resolve, since silently replacing a variable would lose it.
function [names, values] = addEventVar (names, values, name, val)
  if (any (strcmp (names, name)))
    error (strcat ("eventtable: the input already has a variable named", ...
                   " '%s'; give the values with '%sVariable' instead."), ...
           name, name);
  endif
  names{end+1} = name;
  values{end+1} = val;
endfunction

## Event labels may be of any type that can name a thing, which rules out the
## time types, whose values describe when rather than what, and the tabular
## types, which are not values at all.
function val = checkEventLabels (val, nev)
  bad = {'datetime', 'duration', 'calendarDuration', 'table', 'timetable'};
  if (any (cellfun (@(c) isa (val, c), bad)))
    error (strcat ("eventtable: 'EventLabels' must not be a datetime, a", ...
                   " duration, a calendarDuration, a table or a timetable."));
  endif
  ## A character matrix labels one event per row, which is how Octave holds a
  ## list of names in char; a character row vector is a single label.
  if (ischar (val) && rows (val) > 1)
    val = cellstr (val);
  endif
  val = broadcastEventVar (val, nev, 'EventLabels');
endfunction

## An event lasts for a duration or for a calendar duration, and a calendar
## length has no meaning against row times that are themselves durations,
## there being no calendar to place them in.
function val = checkEventLengths (val, nev, eventTimes)
  if (isduration (eventTimes))
    if (! isduration (val))
      error (strcat ("eventtable: 'EventLengths' must be a duration when", ...
                     " the event times are durations."));
    endif
  elseif (! (isduration (val) || iscalendarduration (val)))
    error (strcat ("eventtable: 'EventLengths' must be a duration or a", ...
                   " calendarDuration."));
  endif
  val = broadcastEventVar (val, nev, 'EventLengths');
endfunction

## An event ends at a time of the same kind as it began.
function val = checkEventEnds (val, nev, eventTimes)
  if (! strcmp (class (val), class (eventTimes)))
    error (strcat ("eventtable: 'EventEnds' must be a %s, the same type", ...
                   " as the event times."), class (eventTimes));
  endif
  val = broadcastEventVar (val, nev, 'EventEnds');
endfunction

## One value labels, lengthens or ends every event alike; otherwise there is
## one value per event.
function val = broadcastEventVar (val, nev, opt)
  ## A character row vector is one value, whatever its length.
  if (ischar (val) && isrow (val))
    val = repmat ({val}, nev, 1);
    return
  endif
  if (isscalar (val))
    val = repmat (val(:), nev, 1);
  elseif (numel (val) == nev)
    val = val(:);
  else
    error (strcat ("eventtable: '%s' must be a scalar or have one element", ...
                   " per event; got %d for %d events."), ...
           opt, numel (val), nev);
  endif
endfunction

## A variable name for one of the three event properties, resolved against
## the variables THIS actually has.  An empty matrix clears the designation,
## which is how a property is unset; an empty character vector does not,
## being a name of no characters rather than the absence of a name.
function name = checkEventVariable (this, val, prop)
  if (isnumeric (val) && isempty (val))
    name = [];
    return
  endif
  if (isstring (val) && isscalar (val))
    val = char (val);
  endif
  if (! (ischar (val) && isrow (val)))
    error (strcat ("eventtable: '%s' must be the name of a variable, given", ...
                   " as a character vector or a string scalar, or [] to", ...
                   " clear it."), prop);
  endif
  names = this.Properties.VariableNames;
  if (! any (strcmp (names, val)))
    error ("eventtable: '%s' names no variable of this event table: '%s'.", ...
           prop, val);
  endif
  name = val;
endfunction

## An event property carried onto a promoted result, or unset where the
## variable it names is not among that result's.
function val = carried (val, names)
  if (! isempty (val) && ! any (strcmp (names, val)))
    val = [];
  endif
endfunction

## An event property carried across a rename: where it names one of the
## renamed variables it takes that variable's new name, and otherwise it is
## left as it stands.
function val = renamed (val, oldNames, newNames)
  if (isempty (val))
    return;
  endif
  ix = find (strcmp (cellstr (oldNames), val), 1);
  if (! isempty (ix))
    newNames = cellstr (newNames);
    val = newNames{ix};
  endif
endfunction

function tf = wasGiven (x)
  tf = ! isa (x, 'missing');
endfunction
