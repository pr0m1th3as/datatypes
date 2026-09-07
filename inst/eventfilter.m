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

classdef eventfilter
  ## -*- texinfo -*-
  ## @deftp {datatypes} eventfilter
  ##
  ## Subscript into a timetable by what was happening rather than by when.
  ##
  ## A utility class that builds a condition against the variables of the
  ## event table attached to a timetable, and selects the rows the matching
  ## events cover.  The condition picks out @emph{events}; the rows follow
  ## from them.
  ##
  ## @example
  ## @group
  ## ef = eventfilter (tt);
  ## tt(ef.EventLabels == "rain", :)
  ## @end group
  ## @end example
  ##
  ## An event covers the times from its own onwards and stops short of its
  ## end, and an event with neither a length nor an end covers only a row at
  ## exactly its own time.  Rows covered by more than one matching event are
  ## selected once: a filter answers which rows, not how often.
  ##
  ## @qcode{Time} names the event's own time, not the row's, so
  ## @code{ef.Time > d} selects the rows of the events that began after
  ## @var{d}.
  ##
  ## @seealso{eventtable, timerange, withtol, timetable}
  ## @end deftp

  properties (SetAccess = private, Hidden)
    ## The names a condition may be written against
    varNames
    ## The variable a comparison is waiting for, empty once one is made
    pending
    ## The condition, as a function of the event table it will be applied to
    condition
    ## The condition as written, for the display
    text
    ## The labels of the bare-label form, empty for every other filter
    labels
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    ## Custom display
    function disp (this)
      if (isempty (this.condition) && isempty (this.labels))
        fprintf ("  eventfilter with no constraint\n\n");
      else
        fprintf ("  eventfilter with constraint:\n\n    %s\n\n", this.text);
      endif
      if (! isempty (this.varNames))
        fprintf ("  VariableNames: %s\n\n", strjoin (this.varNames, ", "));
      endif
    endfunction

    ## Class specific subscripted reference.  Every '.' name is a variable of
    ## the event table the filter will be applied to, so the filter has no
    ## readable properties of its own.
    function varargout = subsref (this, s)
      chain_s = s(2:end);
      s = s(1);
      if (! strcmp (s.type, '.'))
        error ("eventfilter.subsref: only '.' indexing is supported.");
      endif
      name = s.subs;
      if (isstring (name) && isscalar (name))
        name = char (name);
      endif
      if (! (ischar (name) && isrow (name)))
        error (strcat ("eventfilter.subsref: '.' index argument must be a", ...
                       " character vector or a string scalar."));
      endif
      if (! any (strcmp (this.varNames, name)))
        error (strcat ("eventfilter: no variable named '%s' to filter on;", ...
                       " use one of: %s"), name, ...
               strjoin (this.varNames, ", "));
      endif
      out = this;
      out.pending = name;
      if (! isempty (chain_s))
        out = subsref (out, chain_s);
      endif
      varargout{1} = out;
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {eventfilter} {@var{ef} =} eventfilter (@var{tt})
    ## @deftypefnx {eventfilter} {@var{ef} =} eventfilter (@var{eventLabels})
    ##
    ## Create an event filter.
    ##
    ## @code{@var{ef} = eventfilter (@var{tt})} creates a filter that may be
    ## written against the variables of the event table attached to the
    ## timetable @var{tt}, together with @qcode{Time}, the events' own times.
    ## @var{tt} must have an event table attached, since the names come from
    ## it.
    ##
    ## @code{@var{ef} = eventfilter (@var{eventLabels})} creates a filter
    ## that matches the events carrying any of the given labels, without a
    ## timetable to take the names from.  It is a whole condition already and
    ## needs no comparison written against it.
    ##
    ## A filter is used as a row subscript, and selects the rows the matching
    ## events cover.  Conditions combine with @code{&} and @code{|};
    ## @code{~} is not supported, an event filter having no complement worth
    ## the name.
    ##
    ## @seealso{eventtable, timerange, timetable}
    ## @end deftypefn
    function this = eventfilter (arg)

      if (nargin != 1)
        print_usage ();
      endif
      this.varNames = {};
      this.pending = '';
      this.condition = [];
      this.text = '';
      this.labels = [];

      if (isa (arg, 'timetable'))
        ev = arg.Properties.Events;
        if (isempty (ev))
          error (strcat ("eventfilter: the timetable has no event table", ...
                         " attached, so there are no event variables to", ...
                         " filter on; assign one to 'Properties.Events'", ...
                         " first."));
        endif
        props = ev.Properties;
        this.varNames = [props.DimensionNames(1), props.VariableNames];
      elseif (ischar (arg) || iscellstr (arg) || isa (arg, 'string')
              || iscategorical (arg))
        ## A character row vector is one label, not a column of letters,
        ## which is what indexing it would make of it.
        if (iscategorical (arg))
          lab = arg(:);
        else
          lab = cellstr (arg);
          lab = string (lab(:));
        endif
        this.labels = lab;
        this.text = sprintf ("event labels in %s", labelstr (arg));
      else
        error (strcat ("eventfilter: input must be a timetable carrying", ...
                       " an event table, or a list of event labels."));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {eventfilter} {@var{names} =} properties (@var{ef})
    ##
    ## Return the names a condition may be written against.
    ##
    ## They are the variables of the event table the filter came from,
    ## together with @qcode{Time}.  A filter made from a list of labels
    ## carries none, having been given no event table to read them from.
    ##
    ## @end deftypefn
    function names = properties (this)
      names = this.varNames(:);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {eventfilter} {@var{ix} =} rowIndices (@var{ef}, @var{tt})
    ##
    ## Return the positions of the rows of @var{tt} the filter selects.
    ##
    ## @end deftypefn
    function ix = rowIndices (this, tt)
      ev = filterEvents (this, tt);
      mask = eventMask (this, ev);
      ix = find (coveredRows (ev, mask, tt.Properties.RowTimes));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {eventfilter} {@var{t} =} eventTimes (@var{ef}, @var{tt})
    ##
    ## Return the times of the events of @var{tt} the filter matches.
    ##
    ## The times come back in the order the event table holds them.  It is
    ## what a @code{timerange} reads when an event filter is given as one of
    ## its bounds.
    ##
    ## @end deftypefn
    function t = eventTimes (this, tt)
      ev = filterEvents (this, tt);
      mask = eventMask (this, ev);
      rt = ev.Properties.RowTimes;
      t = rt(mask);
    endfunction

  endmethods

  methods (Access = private)

    ## The event table the filter is to be applied against, with the two
    ## refusals a filter can meet on its way to one.
    function ev = filterEvents (this, tt)
      if (isa (tt, 'eventtable'))
        error (strcat ("eventfilter: an event filter selects the rows of a", ...
                       " timetable that its events cover, so it cannot", ...
                       " subscript an event table; attach the event table", ...
                       " to a timetable and subscript that."));
      endif
      if (! isa (tt, 'timetable'))
        error ("eventfilter: an event filter subscripts a timetable.");
      endif
      ev = tt.Properties.Events;
      if (isempty (ev))
        error (strcat ("eventfilter: the timetable has no event table", ...
                       " attached, so no event covers any of its rows;", ...
                       " assign one to 'Properties.Events' first."));
      endif
    endfunction

    ## Which events the condition picks out.
    function mask = eventMask (this, ev)
      if (! isempty (this.labels))
        lv = ev.Properties.EventLabelsVariable;
        if (isempty (lv))
          error (strcat ("eventfilter: the event table does not say which", ...
                         " of its variables holds the labels, so a filter", ...
                         " written against labels cannot be applied to it."));
        endif
        ## Both sides are read as text so that a label given as a string
        ## matches a categorical labels variable and the other way round.
        col = ev.(lv);
        mask = ismember (string (col(:)), string (this.labels));
        return
      endif
      if (isempty (this.condition))
        error (strcat ("eventfilter: the filter carries no condition;", ...
                       " compare one of its variables with a value first."));
      endif
      mask = this.condition (ev);
      mask = mask(:);
      if (! (islogical (mask) && numel (mask) == height (ev)))
        error (strcat ("eventfilter: the condition did not yield one true", ...
                       " or false value for each event."));
      endif
    endfunction

  endmethods

  methods (Static, Hidden)

    ## Build the condition for one comparison, as 'rowfilter' does, except
    ## that the condition reads an event table rather than the timetable the
    ## filter will subscript.
    function out = compare (a, b, sym, fcn, mirror)
      if (isa (a, 'eventfilter') && isa (b, 'eventfilter'))
        error (strcat ("eventfilter: '%s' compares a variable with a", ...
                       " value, not two filters."), sym);
      endif
      swapped = ! isa (a, 'eventfilter');
      if (swapped)
        ef = b;
        val = a;
      else
        ef = a;
        val = b;
      endif
      if (isempty (ef.pending))
        error (strcat ("eventfilter: '%s' needs a variable to compare;", ...
                       " name one with '.' first."), sym);
      endif
      name = ef.pending;
      out = ef;
      out.pending = '';
      if (swapped)
        out.condition = @(e) fcn (val, e.(name));
        out.text = sprintf ("%s %s %s", name, mirror, labelstr (val));
      else
        out.condition = @(e) fcn (e.(name), val);
        out.text = sprintf ("%s %s %s", name, sym, labelstr (val));
      endif
    endfunction

    function [ef, ca, cb] = combination (a, b, op)
      if (! (isa (a, 'eventfilter') && isa (b, 'eventfilter')))
        error ("eventfilter: '%s' combines two filters.", op);
      endif
      if (isempty (a.condition) || isempty (b.condition))
        error (strcat ("eventfilter: '%s' combines two conditions; compare", ...
                       " a variable with a value on both sides first."), op);
      endif
      ef = a;
      ef.varNames = unique ([a.varNames, b.varNames], 'stable');
      ca = a.condition;
      cb = b.condition;
    endfunction

  endmethods

  methods (Hidden)

    function out = eq (a, b)
      out = eventfilter.compare (a, b, '==', @eq, '==');
    endfunction

    function out = ne (a, b)
      out = eventfilter.compare (a, b, '~=', @ne, '~=');
    endfunction

    function out = lt (a, b)
      out = eventfilter.compare (a, b, '<', @lt, '>');
    endfunction

    function out = le (a, b)
      out = eventfilter.compare (a, b, '<=', @le, '>=');
    endfunction

    function out = gt (a, b)
      out = eventfilter.compare (a, b, '>', @gt, '<');
    endfunction

    function out = ge (a, b)
      out = eventfilter.compare (a, b, '>=', @ge, '<=');
    endfunction

    function out = and (a, b)
      [out, ca, cb] = eventfilter.combination (a, b, '&');
      out.condition = @(e) ca(e) & cb(e);
      out.text = sprintf ("%s & %s", a.text, b.text);
    endfunction

    function out = or (a, b)
      [out, ca, cb] = eventfilter.combination (a, b, '|');
      out.condition = @(e) ca(e) | cb(e);
      out.text = sprintf ("%s | %s", a.text, b.text);
    endfunction

    function out = not (a)
      error (strcat ("eventfilter: an event filter does not support the", ...
                     " negation operator '~'; write the condition the", ...
                     " other way round."));
    endfunction

  endmethods

endclassdef

## Which rows of ROWTIMES the events MASK picks out cover.  An interval runs
## from the event's own time and stops short of its end, so an event of zero
## length covers nothing; an event with neither a length nor an end covers a
## row at exactly its own time.  Rows covered more than once are selected
## once.
function keep = coveredRows (ev, mask, rowTimes)
  props = ev.Properties;
  evTimes = props.RowTimes;
  if (! isempty (props.EventLengthsVariable))
    evEnds = evTimes + ev.(props.EventLengthsVariable);
  elseif (! isempty (props.EventEndsVariable))
    evEnds = ev.(props.EventEndsVariable);
  else
    evEnds = [];
  endif
  keep = false (numel (rowTimes), 1);
  for k = find (mask(:)')
    if (isempty (evEnds))
      keep = keep | (rowTimes == evTimes(k));
    else
      keep = keep | (rowTimes >= evTimes(k) & rowTimes < evEnds(k));
    endif
  endfor
endfunction

## A value rendered for the display of a condition.
function txt = labelstr (val)
  if (ischar (val))
    txt = sprintf ("'%s'", val);
  elseif (isa (val, 'string') || iscategorical (val) || iscellstr (val))
    c = cellstr (string (val(:)'));
    txt = strjoin (strcat ('"', c, '"'), ", ");
  elseif (isnumeric (val) && isscalar (val))
    txt = num2str (val);
  else
    txt = sprintf ("<%s>", class (val));
  endif
endfunction

%!shared TT, ET
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! ET = eventtable (t([3 6]), 'EventLabels', string ({'rain'; 'snow'}), ...
%!                  'EventLengths', hours ([2; 1]));
%! TT = timetable (t, (1:8)', 'VariableNames', {'v'});
%! TT.Properties.Events = ET;

## Test a filter is built from a timetable carrying events
%!assert (class (eventfilter (TT)), 'eventfilter')

## Test a filter is built from a list of labels
%!assert (class (eventfilter ("rain")), 'eventfilter')

## Test the names a condition may be written against
%!test
%! assert_equal (properties (eventfilter (TT))', ...
%!               {'Time', 'EventLabels', 'EventLengths'});

## Test a comparison yields another filter rather than a logical
%!test
%! EF = eventfilter (TT);
%! assert_equal (class (EF.EventLabels == "rain"), 'eventfilter');

## Test an interval event covers its own time and stops short of its end
%!test
%! EF = eventfilter (TT);
%! assert_equal (TT(EF.EventLabels == "rain", :).v', [3, 4]);

## Test a one-hour event covers only its own row
%!test
%! EF = eventfilter (TT);
%! assert_equal (TT(EF.EventLabels == "snow", :).v', 6);

## Test an event given an end covers the same rows as one given a length
%!test
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t(3), 'EventLabels', "rain", ...
%!                                   'EventEnds', t(5));
%! EF = eventfilter (A);
%! assert_equal (A(EF.EventLabels == "rain", :).v', [3, 4]);

## Test an instantaneous event covers only a row at its own time
%!test
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]), 'EventLabels', ...
%!                                   string ({'rain'; 'snow'}));
%! EF = eventfilter (A);
%! assert_equal (A(EF.EventLabels == "rain", :).v', 3);

## Test an event of zero length covers nothing
%!test
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]), 'EventLabels', ...
%!                                   string ({'rain'; 'snow'}), ...
%!                                   'EventLengths', hours ([0; 0]));
%! EF = eventfilter (A);
%! assert_equal (height (A(EF.EventLabels == "rain", :)), 0);

## Test rows covered by two matching events are selected once
%!test
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 4]), 'EventLabels', ...
%!                                   string ({'rain'; 'rain'}), ...
%!                                   'EventLengths', hours ([3; 3]));
%! EF = eventfilter (A);
%! assert_equal (A(EF.EventLabels == "rain", :).v', [3, 4, 5, 6]);

## Test a condition matching no event selects no row
%!test
%! EF = eventfilter (TT);
%! assert_equal (height (TT(EF.EventLabels == "nothing", :)), 0);

## Test '~=' selects the events whose label differs
%!test
%! EF = eventfilter (TT);
%! assert_equal (TT(EF.EventLabels ~= "rain", :).v', 6);

## Test '&' combines two conditions
%!test
%! EF = eventfilter (TT);
%! f = EF.EventLabels == "rain" & EF.EventLengths < hours (3);
%! assert_equal (TT(f, :).v', [3, 4]);

## Test '|' combines two conditions
%!test
%! EF = eventfilter (TT);
%! f = EF.EventLabels == "rain" | EF.EventLabels == "snow";
%! assert_equal (TT(f, :).v', [3, 4, 6]);

## Test 'Time' compares the event's time and not the row's
%!test
%! EF = eventfilter (TT);
%! rt = TT.Properties.RowTimes;
%! assert_equal (TT(EF.Time > rt(3), :).v', 6);
%! assert_equal (TT(EF.Time <= rt(3), :).v', [3, 4]);

## Test a comparison on the lengths variable
%!test
%! EF = eventfilter (TT);
%! assert_equal (TT(EF.EventLengths >= hours (2), :).v', [3, 4]);

## Test a comparison on an event data variable
%!test
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! E = addvars (ET, [7; 8], 'NewVariableNames', {'amount'});
%! A.Properties.Events = E;
%! EF = eventfilter (A);
%! assert_equal (A(EF.amount > 7, :).v', 6);

## Test a filter of one bare label used as a subscript
%!assert (TT(eventfilter ("rain"), :).v', [3, 4])

## Test a filter of several bare labels
%!assert (TT(eventfilter (string ({'rain'; 'snow'})), :).v', [3, 4, 6])

## Test a bare label no event carries selects nothing
%!assert (height (TT(eventfilter ("zzz"), :)), 0)

## Test a filter combines with a variable subscript
%!test
%! EF = eventfilter (TT);
%! out = TT(EF.EventLabels == "rain", {'v'});
%! assert_equal (out.v', [3, 4]);

## Test the result is a plain timetable
%!test
%! EF = eventfilter (TT);
%! assert_equal (class (TT(EF.EventLabels == "rain", :)), 'timetable');

## Test a filter is reusable and answers the same twice
%!test
%! EF = eventfilter (TT);
%! f = EF.EventLabels == "rain";
%! assert_equal (isequal (TT(f, :), TT(f, :)), true);

## Test rows are deleted through a filter
%!test
%! EF = eventfilter (TT);
%! A = TT;
%! A(EF.EventLabels == "rain", :) = [];
%! assert_equal (A.v', [1, 2, 5, 6, 7, 8]);

## Test a duration-keyed timetable
%!test
%! td = seconds ((0:7))';
%! A = timetable (td, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (td([3 6]), 'EventLabels', ...
%!                                   string ({'rain'; 'snow'}), ...
%!                                   'EventLengths', seconds ([2; 1]));
%! EF = eventfilter (A);
%! assert_equal (A(EF.EventLabels == "rain", :).v', [3, 4]);

## Test a range running from one event to another
%!test
%! EF = eventfilter (TT);
%! a = EF.EventLabels == "rain";
%! b = EF.EventLabels == "snow";
%! assert_equal (TT(timerange (a, b), :).v', [3, 4, 5]);

## Test a filter built from a timetable carrying no events
%!error <eventfilter: the timetable has no event table attached, so there are no event variables to filter on; assign one to 'Properties.Events' first.> ...
%! eventfilter (timetable ((1:3)', 'TimeStep', hours (1)));

## Test a filter applied to a timetable carrying no events
%!error <eventfilter: the timetable has no event table attached, so no event covers any of its rows; assign one to 'Properties.Events' first.> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! B = timetable (t, (1:8)', 'VariableNames', {'v'});
%! EF = eventfilter (A);
%! B(EF.EventLabels == "Event 1", :);

## Test a filter applied to an event table
%!error <eventfilter: an event filter selects the rows of a timetable that its events cover, so it cannot subscript an event table; attach the event table to a timetable and subscript that.> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! EF = eventfilter (A);
%! E = A.Properties.Events;
%! E(EF.EventLabels == "Event 1", :);

## Test the negation operator
%!error <eventfilter: an event filter does not support the negation operator '~'; write the condition the other way round.> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! ~ eventfilter (A);

## Test a filter carrying no condition
%!error <eventfilter: the filter carries no condition; compare one of its variables with a value first.> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! A(eventfilter (A), :);

## Test a name no event variable answers to
%!error <eventfilter: no variable named 'Nope' to filter on; use one of: Time, EventLabels> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! eventfilter (A).Nope;

## Test an input that is neither a timetable nor a list of labels
%!error <eventfilter: input must be a timetable carrying an event table, or a list of event labels.> ...
%! eventfilter (5);

## Test comparing two filters
%!error <eventfilter: '==' compares a variable with a value, not two filters.> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! EF = eventfilter (A);
%! EF.EventLabels == EF.EventLabels;

## Test only '.' indexing is supported
%!error <eventfilter.subsref: only '.' indexing is supported.> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! EF = eventfilter (A); EF(1);

## Test an event filter bound needs one at the other end
%!error <timerange: an event filter bound needs an event filter at the other end too; a range runs from one event to another.> ...
%! t = datetime (2024, 1, 1) + hours ((0:7))';
%! A = timetable (t, (1:8)', 'VariableNames', {'v'});
%! A.Properties.Events = eventtable (t([3 6]));
%! EF = eventfilter (A);
%! timerange (EF.EventLabels == "Event 1", t(1));
