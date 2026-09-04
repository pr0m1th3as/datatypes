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

classdef timetable < tabular
  ## -*- texinfo -*-
  ## @deftp {datatypes} timetable
  ##
  ## Array of tabular data whose rows are labelled by time.
  ##
  ## A timetable collects heterogeneous data into columnar variables exactly
  ## as a @code{table} does, but labels each of its rows with a time instead
  ## of a name.  The row times are a @code{datetime} or a @code{duration}
  ## vector, one element per row, and they are what the type exists for: rows
  ## can be selected, aligned and resampled by when they happened.
  ##
  ## Unlike the row names of a @code{table}, the row times need not be unique,
  ## sorted, or present; duplicates, a descending order and missing times are
  ## all accepted and merely make the timetable irregular.
  ##
  ## A timetable is @emph{regular} when its rows are evenly spaced.  The
  ## spacing is reported by the @qcode{TimeStep} property and its reciprocal
  ## @qcode{SampleRate}, and a timetable that is not evenly spaced reports a
  ## @qcode{NaN} time step.  A timetable told its step at construction, by
  ## @qcode{'TimeStep'} or @qcode{'SampleRate'}, remembers it even where a
  ## subset is too short to imply one, so a single row taken out of it is
  ## still hourly; one that read its step off the row times it was given has
  ## nothing to fall back on and reports @qcode{NaN} there.
  ##
  ## A timetable is not a @code{table} and neither is a subclass of the
  ## other; both derive from the same abstract class, so @code{istabular} is
  ## true for either while @code{istable} and @code{istimetable} each single
  ## one of them out.
  ##
  ## Assigning an empty matrix to a subscripted timetable deletes rows or
  ## variables exactly as it does for a @code{table}, and the row times go
  ## with the rows they label.  The row times themselves cannot be deleted.
  ##
  ## Besides the @code{timetable} constructor, you can also use
  ## @code{table2timetable} and @code{array2timetable} to create timetables
  ## from the respective data types.
  ##
  ## @seealso{table, istimetable, istabular, isregular, datetime, duration}
  ## @end deftp

  properties

    ## -*- texinfo -*-
    ## @deftp {timetable} {property} RowTimes
    ##
    ## Row times
    ##
    ## Row times, specified as a @code{datetime} or @code{duration} vector
    ## with one element for each row of the timetable.  They are stored as a
    ## column vector whatever shape they are given in.  Unlike the row names
    ## of a table they need not be unique, sorted or present: duplicate
    ## times, a descending order and missing times are all accepted, and each
    ## of them merely makes the timetable irregular.  You can access them
    ## with @qcode{@var{tt}.Properties.RowTimes} and assign them the same
    ## way, which recomputes @qcode{TimeStep}.
    ##
    ## @end deftp
    RowTimes = []

    ## -*- texinfo -*-
    ## @deftp {timetable} {property} StartTime
    ##
    ## Time of the first row
    ##
    ## Time of the first row, of the same type as @qcode{RowTimes}.  It
    ## follows the first row: deleting the first row leaves @qcode{StartTime}
    ## naming the row that is now first.  A timetable left with no rows keeps
    ## the value it had.  Assigning it shifts every row time by the same
    ## amount and preserves @qcode{TimeStep}.
    ##
    ## @end deftp
    StartTime = []

    ## -*- texinfo -*-
    ## @deftp {timetable} {property} SampleRate
    ##
    ## Sampling rate in hertz
    ##
    ## Sampling rate, specified as a positive numeric scalar, being the
    ## reciprocal of @qcode{TimeStep} measured in seconds.  It is
    ## @qcode{NaN} whenever the time step is a @code{calendarDuration},
    ## a calendar step having no fixed length in seconds, and whenever the
    ## timetable is irregular.  Assigning it regenerates the row times from
    ## @qcode{StartTime}, as assigning @qcode{TimeStep} does.
    ##
    ## @end deftp
    SampleRate = []

    ## -*- texinfo -*-
    ## @deftp {timetable} {property} TimeStep
    ##
    ## Spacing between row times
    ##
    ## Spacing between consecutive row times, specified as a @code{duration}
    ## or @code{calendarDuration} scalar, or a @qcode{NaN} duration when the
    ## timetable is irregular.  Two rows or more always imply a step of their
    ## own and it is read off them afresh, so a subset with a gap in it steps
    ## by nothing and a reversed one steps backwards.  Fewer than two rows
    ## imply nothing, and there it matters how the step was arrived at: one
    ## given by @qcode{'TimeStep'} or @qcode{'SampleRate'} is remembered,
    ## while one read off the row times is not and becomes @qcode{NaN}, its
    ## class resetting to @code{duration} with it.  A freshly built one-row
    ## timetable has no step either way.  A negative step is as regular as a
    ## positive one.  Assigning it regenerates the row times from
    ## @qcode{StartTime}, even when the timetable was irregular.
    ##
    ## @end deftp
    TimeStep = []

  endproperties

  properties (Access = protected)

    ## Whether the time step was declared rather than inferred.  It is set
    ## when a step is given, by 'TimeStep' or by 'SampleRate', and cleared
    ## when the row times are given instead and the step read off them.  It
    ## decides one thing only: what becomes of the step when a result is left
    ## with fewer than two rows and there is no spacing left to read.  A
    ## declared step is remembered there, an inferred one is not.
    StepDeclared = false

  endproperties

################################################################################
##                         **    Subclass hooks    **                         ##
################################################################################
##                                                                            ##
## The eleven hooks 'tabular' declares, implemented for a timetable, whose   ##
## rows are labelled by 'RowTimes' and always carry labels.                   ##
##                                                                            ##
################################################################################

  methods (Access = protected)

    ## Always true.  A timetable's row times are not optional the way a
    ## table's row names are: every row has one, even if it is missing.
    function tf = hasRowLabels (this)
      tf = true;
    endfunction

    ## The 'RowTimes' vector exactly as stored, a datetime or duration
    ## column with one element per row.
    function out = getRowLabels (this)
      out = this.RowTimes;
    endfunction

    ## The name of the row dimension, which is what the row times are known
    ## by.  Unlike a table, whose labels answer to the fixed 'RowNames', a
    ## timetable's answer to whatever 'DimensionNames{1}' says, and that is
    ## taken from the name of the variable they were built from.
    function out = rowLabelName (this)
      out = this.DimensionNames{1};
    endfunction

    ## The row times rendered for display, as a column cellstr.  Row times
    ## are not text, so unlike a table this hook has real work to do, and it
    ## renders them in whatever format they carry.
    function out = rowLabelStrings (this)
      if (isempty (this.RowTimes))
        out = {};
      else
        out = cellstr (this.RowTimes);
        out = out(:);
      endif
    endfunction

    ## The row dimension name, which heads the column of row times as a
    ## variable's name heads its own.  Unlike a table, whose row names are
    ## printed bare, a timetable's row times are a named dimension and are
    ## shown as one.
    function out = rowLabelHeader (this)
      out = this.DimensionNames{1};
    endfunction

    ## A timetable orders by its row times, which are named by the row
    ## dimension and by nothing else.
    function out = rowLabelKeyNames (this)
      out = this.DimensionNames(1);
    endfunction

    ## Row times given outright, the step read off them afresh.
    function this = setRowLabels (this, labels)
      this = applyRowTimes (this, labels, true);
    endfunction

    ## The variables alone, as a table: the row times label rows and a table
    ## has no rows to label.
    function out = plainTable (this)
      vals = this.VariableValues;
      if (isempty (vals))
        out = table.empty (height (this), 0);
      else
        out = table (vals{:}, 'VariableNames', this.VariableNames);
        out.Properties.VariableDescriptions = this.VariableDescriptions;
        out.Properties.VariableUnits = this.VariableUnits;
      endif
    endfunction

    ## The row times are an ordinary grouping key, named by the row
    ## dimension.
    function tf = groupsByLabels (this)
      tf = true;
    endfunction

    ## A bare 'sortrows (tt)' orders by the row times.
    function tf = sortsByLabelsByDefault (this)
      tf = true;
    endfunction

    ## Row times are part of what makes a row distinct.
    function tf = uniqueIncludesLabels (this)
      tf = true;
    endfunction

    ## A missing row time disqualifies its row: there is no placing such a
    ## row in time, whatever its variables hold.
    function tf = usableRowLabels (this)
      tf = ! ismissing (this.RowTimes);
      tf = tf(:);
    endfunction

    ## The row times are summarised beside the variables, under the row
    ## dimension's name.  They carry no description, units or continuity, and
    ## report instead where they start, how fast they run and how far apart
    ## they step.
    function [name, entry] = summaryLabelEntry (this)
      name = this.DimensionNames{1};
      rt = this.RowTimes;
      e = struct ();
      e.Size = size (rt);
      e.Type = class (rt);
      if (isdatetime (rt))
        e.TimeZone = rt.TimeZone;
      endif
      e.SampleRate = this.SampleRate;
      e.StartTime = this.StartTime;
      entry = tabular.summaryStats (e, rt);
      entry.TimeStep = this.TimeStep;
    endfunction

    ## A timetable interpolates against its row times, measured in seconds
    ## from the first of them.  A row time that is missing places nothing, so
    ## the whole object is refused rather than filled around the gap.
    function [x, ownPoints, errmsg] = fillSamplePoints (this)
      x = [];
      ownPoints = true;
      errmsg = '';
      rt = this.RowTimes;
      if (any (ismissing (rt)))
        errmsg = "row times must not be missing.";
        return
      endif
      if (isempty (rt))
        x = zeros (0, 1);
      elseif (isdatetime (rt))
        x = seconds (rt - rt(1));
      else
        x = seconds (rt);
      endif
      x = x(:);
    endfunction

    ## The four properties a timetable publishes about its row times, keyed
    ## by the names they are published under.  'RowTimes' is the stored
    ## truth; the other three describe it and are maintained with it.
    function out = rowLabelProperties (this)
      out = struct ();
      out.RowTimes = this.RowTimes;
      out.StartTime = this.StartTime;
      out.SampleRate = this.SampleRate;
      out.TimeStep = this.TimeStep;
    endfunction

    ## One of those four assigned.  Each of them writes through to the
    ## others, since all four describe the same vector: new row times imply
    ## a new step, and a new step, rate or start regenerates the row times.
    function [this, handled] = setRowLabelProperty (this, name, val, chain_s)
      handled = true;
      switch (name)
        case 'RowTimes'
          val = checkRowTimes (val, height (this));
          this.StepDeclared = false;
          this = applyRowTimes (this, val, true);

        case 'StartTime'
          val = checkStartTime (val);
          this = applyRowTimes (this, reanchored (this, val), false);

        case 'TimeStep'
          val = checkTimeStep (val, this.StartTime);
          rt = steppedTimes (this.StartTime, val, height (this));
          this.StepDeclared = true;
          this = applyRowTimes (this, rt, true, val);

        case 'SampleRate'
          val = seconds (1 / checkSampleRate (val));
          rt = steppedTimes (this.StartTime, val, height (this));
          this.StepDeclared = true;
          this = applyRowTimes (this, rt, true, val);

        otherwise
          handled = false;
      endswitch
    endfunction

    ## Keeps the row times picked out by IXROWS, in the order given, and
    ## brings the step back into agreement with them.  A result of two rows
    ## or more implies a step of its own, so it is read off afresh: a
    ## reversed subset of an hourly timetable steps by minus one hour and a
    ## subset with a gap in it steps by nothing.  A shorter result implies
    ## nothing, and there the step is kept only if it was declared: a single
    ## row taken out of a timetable built with 'TimeStep' stays hourly, one
    ## taken out of a timetable built from its row times does not.
    function this = subsetRowLabels (this, ixRows)
      rt = this.RowTimes(ixRows);
      this = applyRowTimes (this, rt, numel (rt) > 1);
    endfunction

    ## Drops the row times, leaving an empty vector of the type they had and
    ## no step.  A timetable cannot really be without them, so this is only
    ## reached where a result carries no rows to label.
    function this = clearRowLabels (this)
      this.RowTimes = this.RowTimes([]);
      this.TimeStep = seconds (NaN);
      this.SampleRate = NaN;
    endfunction

    ## Matches ROWREF against the row times and returns the rows it picks
    ## out.  A reference may be a range of times or a tolerant match, each of
    ## which knows how to select from a vector of times; or a datetime or
    ## duration vector, or text that converts to one, which matches exactly.
    ## Row times need not be unique, so one reference may pick out several
    ## rows, and they come back in reference order.  Raises naming every
    ## reference that matches none.
    function ixRows = resolveRowRef (this, rowRef)
      if (isa (rowRef, 'timerange') || isa (rowRef, 'withtol'))
        ixRows = rowIndices (rowRef, this.RowTimes);
        return
      endif
      ref = rowRefTimes (rowRef, this.RowTimes);
      ixRows = [];
      unmatched = {};
      for i = 1:numel (ref)
        hit = find (this.RowTimes == ref(i));
        if (isempty (hit))
          unmatched{end+1} = cellstr (ref(i)){1};
        endif
        ixRows = [ixRows; hit(:)];
      endfor
      if (! isempty (unmatched))
        error ("timetable: no such row time in timetable: '%s'", ...
               strjoin (unmatched, ", "));
      endif
    endfunction

    ## Row times need not be unique, so a repeated row simply repeats its
    ## time; the step is recomputed from the result, which a repetition
    ## generally makes irregular.
    function this = repeatRowLabels (this, n, elementwise)
      nrow = numel (this.RowTimes);
      if (elementwise)
        ix = repelem ((1:nrow)', n, 1);
      else
        ix = repmat ((1:nrow)', n, 1);
      endif
      this = applyRowTimes (this, this.RowTimes(ix), true);
    endfunction

    ## A timetable built from an apply method's output.  Each output row takes
    ## the row time of the input row ROWIX names it came from; with no index
    ## to go on the rows take the row times from the top of the input.
    ## ROWLABELS means nothing here, the row times following the index.
    function out = assembleApply (this, vars, names, rowLabels, rowIx)
      if (isempty (vars))
        nrows = 0;
      else
        nrows = size (vars{1}, 1);
      endif
      if (isempty (rowIx))
        ## With no index to go on the result takes the first row times, one
        ## per row it has: a reduction to a single row takes the first, and a
        ## function returning several takes as many from the top.
        n = min (nrows, height (this));
        rowIx = (1:n)';
        if (n < nrows && n > 0)
          rowIx = [rowIx; repmat(n, nrows - n, 1)];
        endif
      endif
      rt = this.RowTimes(rowIx(:));
      out = timetable (vars{:}, 'RowTimes', rt, 'VariableNames', names);
    endfunction

    ## Wraps the metadata struct that 'getProperties' assembles in a
    ## 'datatypes.tabular.TimetableProperties', the class that adds the four
    ## row time properties to the shared ones and fixes the order the whole
    ## set displays in.
    function out = makeProperties (this)
      out = datatypes.tabular.TimetableProperties (getProperties (this), ...
                                                   this.CustomPropTypes);
    endfunction

  endmethods

################################################################################
##                       **    The row time engine    **                      ##
################################################################################
##                                                                            ##
## 'RowTimes' is the stored truth and 'StartTime', 'SampleRate' and           ##
## 'TimeStep' describe it.  'applyRowTimes' is the only writer of all four,   ##
## so the four cannot drift apart, and every method that touches a row goes   ##
## through it.                                                                ##
##                                                                            ##
################################################################################

  methods (Access = protected)

    ## The row time state set in one operation: RT becomes the row times and
    ## the other three properties are brought into agreement with it.
    ## INFERSTEP asks for the step to be read off RT; when it is false the
    ## stored step is kept, which is what carries a step through a subset
    ## too short to imply one and through a shift of the start time.
    ## 'StartTime' follows the first row and, when there is no first row,
    ## stays as it was.
    ##
    ## GIVENSTEP is the step the caller generated RT from, when there was
    ## one.  The step is still read off RT, because a calendar step need not
    ## reproduce the times it generated and an unreproducible one leaves the
    ## timetable irregular.  But where RT implies any step at all, the
    ## caller's own object is the one kept: a step given as 'hours (1)' goes
    ## on reading as one hour rather than as the 01:00:00 a difference of
    ## datetimes comes out as, and one given as 'hours (24)' stays a
    ## duration rather than becoming the calendar day it also describes.
    function this = applyRowTimes (this, rt, inferStep, givenStep)
      oldStep = this.TimeStep;
      this.RowTimes = rt(:);
      if (inferStep)
        [this.TimeStep, this.SampleRate] = stepOf (this.RowTimes);
        if (nargin > 3 && (numel (this.RowTimes) < 2
                           || ! any (ismissing (this.TimeStep))))
          this.TimeStep = givenStep;
          this.SampleRate = stepRate (givenStep);
        elseif (this.StepDeclared && ! any (ismissing (this.TimeStep)))
          ## A declared step also names the unit the class goes on reporting
          ## in, so a step read off a subset reads in that unit too: an
          ## hourly timetable subset to every other row reports '2 hr', not
          ## the '02:00:00' a difference of datetimes comes out as.
          this.TimeStep = carryStepFormat (oldStep, this.TimeStep);
        endif
      elseif (numel (this.RowTimes) < 2 && ! this.StepDeclared)
        ## Fewer than two rows imply no spacing of their own, and an object
        ## that was never told one has nothing to keep.  The class resets
        ## with the value, so an inferred calendar step becomes a duration.
        this.TimeStep = seconds (NaN);
        this.SampleRate = NaN;
      endif
      if (! isempty (this.RowTimes))
        this.StartTime = this.RowTimes(1);
      elseif (! this.StepDeclared)
        ## The start was only ever the first row time and there is none
        ## left, so it goes missing rather than empty.
        this.StartTime = missingTimes (this.RowTimes, 1);
      endif
    endfunction

    ## The metadata a stacked operand contributes: whatever the result does
    ## not carry already, the first input to have set a thing keeping it.
    function tbl = adoptMetadata (tbl, in)
      props = getProperties (in);
      fields = {'VariableDescriptions', 'VariableUnits', ...
                'VariableContinuity', 'Description', 'UserData'};
      for i = 1:numel (fields)
        if (isempty (tbl.(fields{i})))
          tbl.(fields{i}) = props.(fields{i});
        endif
      endfor
      if (isempty (tbl.CustomProperties)
          && ! isempty (props.CustomProperties))
        tbl.CustomProperties = props.CustomProperties;
        tbl.CustomPropTypes = customPropTypes (in);
      endif
    endfunction

    ## The row times re-anchored so that the first of them is ST, which is
    ## what assigning 'StartTime' does.  Every row keeps its offset from the
    ## first, so an irregular timetable stays exactly as irregular as it was,
    ## and a calendar-stepped one keeps its calendar offsets: moving a
    ## monthly timetable to the 15th of a month gives the 15th of every
    ## following month, where shifting by a fixed number of days would not.
    ##
    ## ST need not be of the type the row times already have.  Giving a
    ## datetime to a timetable keyed by elapsed time is how the recording is
    ## said to have begun on a date, and giving a duration to a dated one
    ## reduces it back to elapsed time.
    function rt = reanchored (this, st)
      rt = this.RowTimes;
      if (isempty (rt))
        rt = st([]);
      elseif (iscalendarduration (this.TimeStep))
        rt = steppedTimes (st, this.TimeStep, numel (rt));
      else
        rt = st + (rt - rt(1));
      endif
    endfunction

  endmethods

  methods (Static, Access = protected)

    ## The variable names of every operand of a concatenation, and the row
    ## dimension name the result takes.  Also the place both concatenations
    ## refuse an input that is not tabular at all.  A table put before the
    ## timetable needs no check here: concatenation dispatches on the first
    ## operand, so such a call reaches 'table' and is refused there.
    ##
    ## Metadata is read through 'getProperties' rather than from the
    ## properties themselves, because a public property read from outside
    ## its own class goes through 'subsref' and is taken for a variable
    ## name; a concatenation that mixes the two classes reads a table's.
    function [names, dimName] = catOperands (args, caller)
      if (! all (cellfun (@istabular, args)))
        error (strcat ("timetable.%s: all inputs must be tables or", ...
                       " timetables."), caller);
      endif
      names = cell (1, numel (args));
      dimName = 'Time';
      seen = false;
      for i = 1:numel (args)
        props = getProperties (args{i});
        names{i} = props.VariableNames;
        if (! seen && istimetable (args{i})
            && ! strcmp (props.DimensionNames{1}, 'Time'))
          ## An explicit name outranks the default whichever side it is on,
          ## and the first explicit one wins.
          dimName = props.DimensionNames{1};
          seen = true;
        endif
      endfor
    endfunction

  endmethods

################################################################################
##                     **    Create timetable    **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'timetable'                                                                ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{tt} =} timetable (@var{rowTimes}, @var{var1}, @dots{}, @var{varN})
    ## @deftypefnx {timetable} {@var{tt} =} timetable (@var{var1}, @dots{}, @var{varN}, @qcode{'RowTimes'}, @var{rowTimes})
    ## @deftypefnx {timetable} {@var{tt} =} timetable (@var{var1}, @dots{}, @var{varN}, @qcode{'TimeStep'}, @var{dt})
    ## @deftypefnx {timetable} {@var{tt} =} timetable (@var{var1}, @dots{}, @var{varN}, @qcode{'SampleRate'}, @var{fs})
    ## @deftypefnx {timetable} {@var{tt} =} timetable (@dots{}, @qcode{'StartTime'}, @var{t0})
    ## @deftypefnx {timetable} {@var{tt} =} timetable (@qcode{'Size'}, @var{sz}, @qcode{'VariableTypes'}, @var{varTypes}, @dots{})
    ## @deftypefnx {timetable} {@var{tt} =} timetable (@dots{}, @qcode{'VariableNames'}, @var{varNames})
    ## @deftypefnx {timetable} {@var{tt} =} timetable (@dots{}, @qcode{'DimensionNames'}, @var{dimNames})
    ##
    ## Create a new timetable.
    ##
    ## @code{@var{tt} = timetable (@var{rowTimes}, @var{var1}, @dots{},
    ## @var{varN})} creates a timetable whose rows are labelled by
    ## @var{rowTimes}, a @code{datetime} or @code{duration} vector with one
    ## element for each row, and whose variables are the remaining input
    ## arguments.  Variable names are taken from the names of the input
    ## variables, and the row dimension is named after @var{rowTimes} itself.
    ##
    ## @code{@var{tt} = timetable (@var{var1}, @dots{}, @var{varN},
    ## @qcode{'RowTimes'}, @var{rowTimes})} does the same with the row times
    ## given as a Name-Value pair, in which case the row dimension is named
    ## @qcode{'Time'}.
    ##
    ## @code{@var{tt} = timetable (@var{var1}, @dots{}, @var{varN},
    ## @qcode{'TimeStep'}, @var{dt})} generates the row times instead of
    ## taking them, starting at @var{t0} and stepping by @var{dt}, a
    ## @code{duration} or @code{calendarDuration} scalar.  A calendar step
    ## requires a @code{datetime} start time.
    ##
    ## @code{@var{tt} = timetable (@var{var1}, @dots{}, @var{varN},
    ## @qcode{'SampleRate'}, @var{fs})} generates them at @var{fs} rows per
    ## second, which is the same as a @qcode{TimeStep} of
    ## @code{seconds (1 / @var{fs})}.
    ##
    ## @code{@var{tt} = timetable (@dots{}, @qcode{'StartTime'}, @var{t0})}
    ## sets the time of the first row for either of the two generating
    ## forms.  It defaults to a zero @code{duration} in the units of the
    ## step, so that a timetable generated without one is keyed by elapsed
    ## time rather than by date.
    ##
    ## @code{@var{tt} = timetable (@qcode{'Size'}, @var{sz},
    ## @qcode{'VariableTypes'}, @var{varTypes}, @dots{})} creates a
    ## preallocated timetable of the given size, filled with the default
    ## value of each type.  @var{sz} must be a two-element numeric array
    ## giving the number of rows and of variables.  The row times must still
    ## be supplied, by any one of @qcode{'RowTimes'}, @qcode{'TimeStep'} or
    ## @qcode{'SampleRate'}.
    ##
    ## @code{@var{tt} = timetable (@dots{}, @qcode{'VariableNames'},
    ## @var{varNames})} specifies the variable names to use, as a cell array
    ## of character vectors or a string array with one nonempty and unique
    ## element per variable.
    ##
    ## @code{@var{tt} = timetable (@dots{}, @qcode{'DimensionNames'},
    ## @var{dimNames})} specifies the two dimension names to use, naming the
    ## rows and the variables.
    ##
    ## @code{@var{tt} = timetable ()} returns an empty timetable with 0 rows
    ## and 0 variables.
    ##
    ## @seealso{table2timetable, array2timetable, isregular}
    ## @end deftypefn
    function this = timetable (varargin)

      ## Return an empty timetable object
      if (nargin == 0)
        this.DimensionNames = {'Time', 'Variables'};
        this.RowTimes = NaT (0, 1);
        this.StartTime = NaT;
        this.TimeStep = seconds (NaN);
        this.SampleRate = NaN;
        return
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'VariableNames', 'DimensionNames', 'RowTimes', ...
                  'TimeStep', 'SampleRate', 'StartTime'};
      dfValues = {{}, {}, missing, missing, missing, missing};
      [VariableNames, DimensionNames, RowTimes, TimeStep, SampleRate, ...
       StartTime, args] = parsePairedArguments (optNames, dfValues, ...
                                                varargin(:));

      ## The row times are said exactly once, either as the leading
      ## argument or by one of the three Name-Value pairs.
      given = [wasGiven(RowTimes), wasGiven(TimeStep), wasGiven(SampleRate)];
      leading = ! isempty (args) && ...
                (isdatetime (args{1}) || isduration (args{1}));
      if (leading && any (given))
        error (strcat ("timetable: row times were given both as the first", ...
                       " argument and as a Name-Value pair."));
      endif
      if (sum (given) > 1)
        error (strcat ("timetable: only one of 'RowTimes', 'TimeStep' and", ...
                       " 'SampleRate' may be given."));
      endif
      if (! leading && ! any (given))
        error (strcat ("timetable: row times are required; give them as", ...
                       " the first argument or with 'RowTimes',", ...
                       " 'TimeStep' or 'SampleRate'."));
      endif

      ## The row dimension is named after the vector the row times came from,
      ## which only the leading form has a name for; the other forms leave it
      ## at the default.  The name is read before ARGS is shortened.
      rowDimName = 'Time';
      if (leading)
        inName = inputname (1);
        if (! isempty (inName))
          rowDimName = inName;
        endif
        RowTimes = args{1};
        args(1) = [];
      endif

      ## Check optional Name-Value paired arguments
      if (! isempty (VariableNames))
        if (! (iscellstr (VariableNames) || isa (VariableNames, 'string')))
          error (strcat ("timetable: 'VariableNames' must be either a", ...
                         " cell array of character vectors or a string", ...
                         " array."));
        endif
        VariableNames = cellstr (VariableNames);
        if (any (cellfun (@isempty, VariableNames)))
          error ("timetable: 'VariableNames' must contain nonempty names.");
        endif
      endif
      if (isempty (DimensionNames))
        DimensionNames = {rowDimName, 'Variables'};
      endif
      if (! (iscellstr (DimensionNames) || isa (DimensionNames, 'string'))
          || numel (DimensionNames) != 2)
        error (strcat ("timetable: 'DimensionNames' must be either a", ...
                       " two-element cell array of character vectors or", ...
                       " a two-element string array."));
      endif
      this.DimensionNames = cellstr (DimensionNames);
      ## Dimension names cannot match reserved timetable identifiers
      reserved = {'Properties', 'RowTimes', 'VariableNames', ':'};
      idr = ismember (this.DimensionNames, reserved);
      if (any (idr))
        error (strcat ("timetable: 'DimensionNames' cannot include the", ...
                       " reserved name: '%s'"), this.DimensionNames{idr});
      endif
      ## Check for conflict between VariableNames and DimensionNames
      idx = ismember (this.DimensionNames, VariableNames);
      if (any (idx))
        error ("timetable: duplicate dimension and variable name: '%s'", ...
               this.DimensionNames{idx});
      endif

      ## Construct a preallocated timetable with default values
      preallocated = (numel (args) == 4 && strcmpi (args{1}, 'Size') &&
                                           strcmpi (args{3}, 'VariableTypes'));
      if (preallocated)
        ## Validate the size specifier
        if (! isnumeric (args{2}) || numel (args{2}) != 2)
          error ("timetable: 'Size' must be a two-element numeric vector.");
        endif
        ## Get number of rows and variables
        nr = args{2}(1);
        nv = args{2}(2);
        ## Get variable types
        varTypes = args{4};
        if (! iscellstr (varTypes) || numel (varTypes) != nv)
          error (strcat ("timetable: 'VariableTypes' must be a", ...
                         " cellstring array of the same number of", ...
                         " elements as defined in SZ(2)."));
        endif

        ## Check optional arguments
        if (! isempty (VariableNames) && numel (VariableNames) != nv)
          error (strcat ("timetable: inconsistent number of", ...
                         " 'VariableNames' and 'VariableTypes'."));
        elseif (isempty (VariableNames))
          VariableNames = cell (1, nv);
          for i = 1:nv
            VariableNames{i} = sprintf ("Var%d", i);
          endfor
        endif

        ## Populate variables with defaults
        VariableValues = cell (1, nv);
        for i = 1:nv
          VariableValues{i} = defaultColumn (varTypes{i}, nr);
        endfor
        nrows = nr;

      ## Construct a timetable with data from input arguments
      else
        ## Get variable names from input arguments
        if (isempty (VariableNames))
          VariableNames = cell (size (args));
          for i = 1:numel (args)
            VariableNames{i} = inputname (i + leading);
            if (isempty (VariableNames{i}))
              VariableNames{i} = sprintf ("Var%d", i);
            endif
          endfor
        endif
        ## Check for unique names in input arguments
        [uqNames, ix] = __unique__ (VariableNames);
        if (numel (uqNames) < numel (VariableNames))
          ixBad = 1:numel (VariableNames);
          ixBad(ix) = [];
          error ("timetable: duplicate variable names: %s", ...
                 strjoin (VariableNames(ixBad), ", "));
        endif
        ## Check number of variable names and input arguments
        if (numel (VariableNames) != numel (args))
          error (strcat ("timetable: inconsistent number of variable", ...
                         " names (%d) and variable values (%d)."), ...
                 numel (VariableNames), numel (args));
        endif
        ## Check size of input variables
        nrows = [];
        if (! isempty (args))
          nrows = size (args{1}, 1);
          if (ndims (args{1}) > 2)
            error (strcat ("timetable: variable values must not have more", ...
                           " than 2 dimensions: input 1 '%s' has %d."), ...
                   VariableNames{1}, ndims (args{1}));
          endif
          for i = 2:numel (args)
            if (ndims (args{i}) > 2)
              error (strcat ("timetable: variable values must not have", ...
                             " more than 2 dimensions: input %d '%s'", ...
                             " has %d."), ...
                     i, VariableNames{i}, ndims (args{i}));
            endif
            nrows2 = size (args{i}, 1);
            if (nrows != nrows2)
              error (strcat ("timetable: inconsistent sizes between", ...
                             " variables: var '%s' has %d rows; var '%s'", ...
                             " has %d rows."), ...
                     VariableNames{1}, nrows, VariableNames{i}, nrows2);
            endif
          endfor
        endif
        VariableValues = args(:)';
      endif

      ## Resolve the row times, which either were given or are generated from
      ## a step.  A generated vector needs a row count, and with no variables
      ## to count there is none, so it comes out empty.
      if (leading || given(1))
        RowTimes = checkRowTimes (RowTimes, nrows);
        StartTime = startOf (RowTimes);
      else
        if (given(3))
          TimeStep = seconds (1 / checkSampleRate (SampleRate));
        endif
        if (! wasGiven (StartTime))
          StartTime = defaultStart (TimeStep);
        else
          StartTime = checkStartTime (StartTime);
        endif
        TimeStep = checkTimeStep (TimeStep, StartTime);
        if (isempty (nrows))
          nrows = 0;
        endif
        RowTimes = steppedTimes (StartTime, TimeStep, nrows);
      endif

      ## Construction
      this.VariableDescriptions = repmat ({''}, [1, numel(VariableNames)]);
      this.VariableUnits = repmat ({''}, [1, numel(VariableNames)]);
      this.VariableNames = VariableNames(:)';
      this.VariableValues = VariableValues;
      this.VariableTypes = cellfun ('class', VariableValues, ...
                                    'UniformOutput', false);
      this.StartTime = StartTime;
      this.StepDeclared = ! (leading || given(1));
      if (leading || given(1))
        this = applyRowTimes (this, RowTimes, true);
      else
        this = applyRowTimes (this, RowTimes, true, TimeStep);
      endif
    endfunction

  endmethods

################################################################################
##                    **    Concatenation Operations    **                    ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'vertcat'          'horzcat'          'cat'                                ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{tt} =} vertcat (@var{tt1}, @var{tt2}, @dots{})
    ##
    ## Concatenate timetables vertically.
    ##
    ## @code{@var{tt} = vertcat (@var{tt1}, @var{tt2}, @dots{})} stacks the
    ## rows of its inputs, which must all have the same variable names.  It
    ## is the operation @code{[@var{tt1}; @var{tt2}]} performs.
    ##
    ## The row times are stacked in the order the inputs are given and are
    ## never sorted, so a later block that starts before an earlier one ends
    ## simply makes the result irregular, as duplicate times do.  The time
    ## step is read afresh from the stacked times, so two blocks that meet
    ## exactly keep the step they share and any other pair loses it.
    ##
    ## A @code{table} may be stacked onto a timetable, but only after it:
    ## its rows carry no times and are labelled with missing ones.  The row
    ## dimension is named after the first input that does not use the
    ## default name.
    ##
    ## @seealso{horzcat, cat}
    ## @end deftypefn
    function tbl = vertcat (varargin)
      varargin = tabular.drop_null_operands (varargin);
      if (isempty (varargin))
        tbl = timetable ();
        return;
      elseif (numel (varargin) == 1 && istimetable (varargin{1}))
        tbl = varargin{1};
        return;
      endif
      [names, dimName] = timetable.catOperands (varargin, 'vertcat');

      ## Every input names the same variables, in whatever order.
      sorted = cellfun (@sort, names, 'UniformOutput', false);
      if (numel (sorted) > 1 && ! isequal (sorted{:}))
        error (strcat ("timetable.vertcat: all inputs must have identical", ...
                       " variable names."));
      endif
      numCols = numel (names{1});

      tbl = varargin{1};
      tbl.DimensionNames{1} = dimName;
      rt = tbl.RowTimes;
      for i = 2:numel (varargin)
        in = varargin{i};
        ixVars = cellfun (@(x) find (ismember (names{1}, x)), names{i});
        in = subsetvars (in, ixVars);
        inVals = varValues (in);
        for v = 1:numCols
          tbl.VariableValues{v} = [tbl.VariableValues{v}; inVals{v}];
        endfor
        ## A table brings rows but no times, and they are labelled missing.
        if (istimetable (in))
          rt = [rt; getRowLabels(in)];
        else
          rt = [rt; missingTimes(rt, height (in))];
        endif
        tbl = adoptMetadata (tbl, in);
      endfor
      tbl.VariableTypes = cellfun ('class', tbl.VariableValues, ...
                                   'UniformOutput', false);
      ## Concatenation reads its step off the times it ends up with.
      tbl.StepDeclared = false;
      tbl = applyRowTimes (tbl, rt, true);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{tt} =} horzcat (@var{tt1}, @var{tt2}, @dots{})
    ##
    ## Concatenate timetables horizontally.
    ##
    ## @code{@var{tt} = horzcat (@var{tt1}, @var{tt2}, @dots{})} appends the
    ## variables of its inputs, which must all have distinct variable names
    ## and, where they are timetables, identical row times.  It is the
    ## operation @code{[@var{tt1}, @var{tt2}]} performs.
    ##
    ## A @code{table} may be appended to a timetable, but only after it, and
    ## it must have as many rows as the timetable has.  The row times and the
    ## time step are those of the timetable, and the row dimension is named
    ## after the first input that does not use the default name.
    ##
    ## @seealso{vertcat, cat}
    ## @end deftypefn
    function tbl = horzcat (varargin)
      varargin = tabular.drop_null_operands (varargin);
      if (isempty (varargin))
        tbl = timetable ();
        return;
      elseif (numel (varargin) == 1 && istimetable (varargin{1}))
        tbl = varargin{1};
        return;
      endif
      [names, dimName] = timetable.catOperands (varargin, 'horzcat');

      ## Variable names are distinct across the whole result.  This is
      ## checked before the row times, as in MATLAB, so two blocks that
      ## share a name are refused for the name whatever their times.
      allNames = [names{:}];
      if (numel (allNames) != numel (__unique__ (allNames)))
        error (strcat ("timetable.horzcat: all inputs must have unique", ...
                       " variable names."));
      endif
      if (numel (__unique__ (cellfun (@height, varargin))) != 1)
        error (strcat ("timetable.horzcat: all inputs must have the same", ...
                       " number of rows."));
      endif
      for i = 2:numel (varargin)
        if (istimetable (varargin{i})
            && ! isequal (getRowLabels (varargin{i}),
                          getRowLabels (varargin{1})))
          error (strcat ("timetable.horzcat: all timetables being", ...
                         " concatenated must have the same row times."));
        endif
      endfor

      tbl = varargin{1};
      tbl.DimensionNames{1} = dimName;
      tbl.VariableNames = allNames;
      for i = 2:numel (varargin)
        in = varargin{i};
        props = getProperties (in);
        inVals = varValues (in);
        tbl.VariableContinuity = tabular.merge_continuity ( ...
                     tbl.VariableContinuity, numel (tbl.VariableValues), ...
                     props.VariableContinuity, numel (inVals));
        tbl.VariableValues = [tbl.VariableValues, inVals];
        tbl.VariableDescriptions = [tbl.VariableDescriptions, ...
                                    props.VariableDescriptions];
        tbl.VariableUnits = [tbl.VariableUnits, props.VariableUnits];
        if (isempty (tbl.Description))
          tbl.Description = props.Description;
        endif
        if (isempty (tbl.UserData))
          tbl.UserData = props.UserData;
        endif
      endfor
      tbl.VariableTypes = cellfun ('class', tbl.VariableValues, ...
                                   'UniformOutput', false);
      [cp, cpTypes] = merge_hcat_props (tbl, varargin);
      tbl.CustomProperties = cp;
      tbl.CustomPropTypes = cpTypes;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{tt} =} cat (@var{dim}, @var{tt1}, @var{tt2}, @dots{})
    ##
    ## Concatenate timetables along a dimension.
    ##
    ## @code{@var{tt} = cat (@var{dim}, @var{tt1}, @var{tt2}, @dots{})}
    ## concatenates along @var{dim}, which must be 1 or 2: a timetable has
    ## two dimensions and there is nothing to stack along a third.
    ## @code{cat (1, @dots{})} is @code{vertcat} and @code{cat (2, @dots{})}
    ## is @code{horzcat}.
    ##
    ## @seealso{vertcat, horzcat}
    ## @end deftypefn
    function tbl = cat (dim, varargin)
      if (nargin < 1)
        print_usage ();
      endif
      if (! (isnumeric (dim) && isscalar (dim) && any (dim == [1, 2])))
        error ("timetable.cat: DIM must be 1 or 2 for a 2-D timetable.");
      endif
      if (dim == 1)
        tbl = vertcat (varargin{:});
      else
        tbl = horzcat (varargin{:});
      endif
    endfunction

  endmethods

################################################################################
##                    **    Size and Shape Operations    **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'height'           'width'            'size'             'numel'          ##
## 'ndims'            'length'           'isempty'          'repelem'         ##
## 'repmat'                                                                  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{H} =} height (@var{tt})
    ##
    ## Number of rows in timetable.
    ##
    ## @code{@var{H} = height (@var{tt})} returns the number of rows in the
    ## timetable @var{tt} as a scalar.  It is the equivalent of
    ## @qcode{size (@var{tt}, 1)}.
    ##
    ## The count comes from the row times rather than from the variables, so
    ## a timetable whose variables have all been removed still reports the
    ## rows it labels.
    ##
    ## @seealso{width, size}
    ## @end deftypefn
    function out = height (this)
      out = numel (this.RowTimes);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{W} =} width (@var{tt})
    ##
    ## Number of variables in timetable.
    ##
    ## @code{@var{W} = width (@var{tt})} returns the number of variables in
    ## the timetable @var{tt} as a scalar.  It is the equivalent of
    ## @qcode{size (@var{tt}, 2)}.  The row times are not a variable and are
    ## not counted.
    ##
    ## @seealso{height, size}
    ## @end deftypefn
    function out = width (this)
      out = numel (this.VariableNames);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{sz} =} size (@var{tt})
    ## @deftypefnx {timetable} {@var{dim_sz} =} size (@var{tt}, @var{dim})
    ## @deftypefnx {timetable} {[@var{rows}, @var{columns}] =} size (@var{tt})
    ##
    ## Size of a timetable.
    ##
    ## @code{@var{sz} = size (@var{tt})} returns a two-element row vector
    ## with the number of rows and the number of variables in @var{tt}.
    ##
    ## @code{@var{dim_sz} = size (@var{tt}, @var{dim})} returns the size
    ## along the dimension @var{dim}.  A timetable has two dimensions, so any
    ## dimension above the second has size 1.
    ##
    ## @seealso{height, width, ndims, numel}
    ## @end deftypefn
    function varargout = size (this, dim)
      sz = [height(this), width(this)];
      if (nargin == 2)
        ## Sizes along the requested dimension(s); dimensions above 2 are 1.
        dim_sz = ones (1, numel (dim));
        valid = dim <= 2;
        dim_sz(valid) = sz(dim(valid));
        if (nargout > 1)
          varargout = num2cell (dim_sz);
        else
          varargout{1} = dim_sz;
        endif
      elseif (nargout <= 1)
        varargout{1} = sz;
      else
        varargout{1} = sz(1);
        varargout{2} = sz(2);
        [varargout{3:nargout}] = deal (1);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{N} =} numel (@var{tt})
    ##
    ## Number of elements in timetable.
    ##
    ## @code{@var{N} = numel (@var{tt})} returns the number of rows times the
    ## number of variables in @var{tt}.
    ##
    ## @seealso{size, height, width}
    ## @end deftypefn
    function out = numel (this, varargin)
      if (nargin < 2)
        out = prod (size (this));
      else
        ## Given subscripts, report how many elements a reference with them
        ## would produce, which is what the classdef machinery asks in order
        ## to size a chained reference.  A '()' reference into a timetable
        ## yields one timetable whatever the subscripts select, an empty one
        ## included, so the answer is always one.
        out = 1;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{N} =} ndims (@var{tt})
    ##
    ## Number of dimensions in timetable.
    ##
    ## @code{@var{N} = ndims (@var{tt})} always returns 2.  A timetable is
    ## always a two-dimensional container of rows and variables, whatever the
    ## shape of the values held in those variables.
    ##
    ## @seealso{size}
    ## @end deftypefn
    function out = ndims (this)
      out = 2;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {} length (@var{tt})
    ##
    ## Length is not defined for a timetable.
    ##
    ## @code{length (@var{tt})} always raises.  A timetable has two
    ## dimensions that mean different things, and the larger of them is not
    ## a useful answer about either; ask @code{height}, @code{width} or
    ## @code{size} for the one that is wanted.
    ##
    ## @seealso{height, width, size}
    ## @end deftypefn
    function out = length (this, varargin)
      error (strcat ("timetable.length: 'length' is not defined for a", ...
                     " timetable; use 'height', 'width' or 'size'."));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{TF} =} isempty (@var{tt})
    ##
    ## True for an empty timetable.
    ##
    ## @code{@var{TF} = isempty (@var{tt})} returns true when @var{tt} has no
    ## rows or no variables, and false otherwise.
    ##
    ## @seealso{size, height, width}
    ## @end deftypefn
    function TF = isempty (this)
      TF = prod (size (this)) == 0;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{tt2} =} repelem (@var{tt}, @var{rows}, @var{cols})
    ##
    ## Repeat each row and variable of a timetable in place.
    ##
    ## @code{@var{tt2} = repelem (@var{tt}, @var{rows}, @var{cols})} repeats
    ## each row of @var{tt} @var{rows} times and each variable @var{cols}
    ## times, keeping the repeats of a row together.  Both counts must be
    ## given, a timetable having exactly two dimensions.  Each repeated row
    ## carries the row time of the row it came from, so the result has
    ## repeated row times and is generally irregular; each repeated variable
    ## takes a numbered name, @qcode{A} becoming @qcode{A}, @qcode{A_1}.
    ##
    ## @seealso{repmat, timetable}
    ## @end deftypefn
    function tt2 = repelem (this, varargin)
      [tt2, errmsg] = repeatResult (this, varargin, true);
      if (! isempty (errmsg))
        error ("timetable.repelem: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{tt2} =} repmat (@var{tt}, @var{sz})
    ## @deftypefnx {timetable} {@var{tt2} =} repmat (@var{tt}, @var{rows}, @var{cols})
    ##
    ## Repeat a timetable as a block.
    ##
    ## @code{@var{tt2} = repmat (@var{tt}, @var{rows}, @var{cols})} repeats
    ## the whole timetable @var{rows} times downwards and @var{cols} times
    ## across.  Given a single argument both counts take it.  Each copy carries
    ## the row times it came from, so the result has repeated row times and is
    ## generally irregular; each repeated variable takes a numbered name,
    ## @qcode{A} becoming @qcode{A}, @qcode{A_1}.
    ##
    ## @seealso{repelem, timetable}
    ## @end deftypefn
    function tt2 = repmat (this, varargin)
      [tt2, errmsg] = repeatResult (this, varargin, false);
      if (! isempty (errmsg))
        error ("timetable.repmat: %s", errmsg);
      endif
    endfunction

  endmethods

################################################################################
##                       **    Regularity and Type    **                      ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'isregular'        'istimetable'                                          ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} isregular (@var{tt})
    ## @deftypefnx {timetable} {@var{TF} =} isregular (@var{tt}, @var{unit})
    ##
    ## True when the row times of a timetable are evenly spaced.
    ##
    ## @code{@var{TF} = isregular (@var{tt})} returns true when @var{tt} is
    ## regular with respect to absolute time, that is when consecutive row
    ## times are separated by the same fixed length of time.  A calendar
    ## month has no fixed length, so a monthly timetable answers false here
    ## and true for @qcode{'months'} below.
    ##
    ## @code{@var{TF} = isregular (@var{tt}, @var{unit})} returns true when
    ## the row times are evenly spaced by a whole number of @var{unit}, one
    ## of @qcode{'time'}, @qcode{'days'}, @qcode{'weeks'},
    ## @qcode{'months'}, @qcode{'quarters'} or @qcode{'years'}.
    ## @qcode{'time'} means absolute time and is what the one-argument form
    ## asks.  There is no unit smaller than a day: an hourly timetable is
    ## regular in time and in nothing else.
    ##
    ## The calendar units are measured on the calendar rather than in
    ## elapsed time, which is what separates them.  Across a daylight saving
    ## change a run of calendar days is regular in @qcode{'days'} and regular
    ## in nothing else, one of those days being an hour shorter than the
    ## rest, and such a timetable reports no time step at all while still
    ## answering true here.
    ##
    ## A timetable with fewer than two rows has no spacing to measure and
    ## answers from the time step it remembers, so a single row taken out of
    ## a timetable told it was hourly is still regular in time, while one
    ## taken out of a timetable that read its step off its row times is
    ## not.
    ##
    ## @seealso{timetable}
    ## @end deftypefn
    function TF = isregular (this, unit)
      if (nargin < 2)
        unit = 'time';
      endif
      if (isstring (unit) && isscalar (unit))
        unit = char (unit);
      endif
      units = {'time', 'days', 'weeks', 'months', 'quarters', 'years'};
      if (! (ischar (unit) && isrow (unit)) || ! any (strcmp (unit, units)))
        error (strcat ("timetable.isregular: UNIT must be one of 'time',", ...
                       " 'days', 'weeks', 'months', 'quarters' or", ...
                       " 'years'."));
      endif
      rt = this.RowTimes;
      if (strcmp (unit, 'time'))
        if (numel (rt) > 1)
          ds = seconds (diff (rt));
          TF = ! any (ismissing (rt)) && ds(1) != 0 && all (ds == ds(1));
        else
          TF = isduration (this.TimeStep) && ! any (ismissing (this.TimeStep));
        endif
        return
      endif
      ## Every other unit is measured on the row times rather than on the
      ## stored step, so that a run of calendar days survives a daylight
      ## saving change even though its step does not.
      if (numel (rt) > 1)
        cd = calendarStepOf (rt);
      elseif (iscalendarduration (this.TimeStep))
        cd = this.TimeStep;
      else
        cd = [];
      endif
      TF = unitDivides (unit, cd);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{TF} =} istimetable (@var{tt})
    ##
    ## True if input is a @code{timetable}, false otherwise.
    ##
    ## @code{@var{TF} = istimetable (@var{tt})} always returns true for a
    ## timetable, irrespective of its size.
    ##
    ## @seealso{istable, istabular}
    ## @end deftypefn
    function TF = istimetable (this)
      TF = true;
    endfunction

  endmethods

################################################################################
##                          **    Comparison    **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'isequal'          'isequaln'                                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} isequal (@var{A}, @var{B})
    ## @deftypefnx {timetable} {@var{TF} =} isequal (@var{A}, @var{B}, @dots{})
    ##
    ## Test timetables for equality.
    ##
    ## @code{@var{TF} = isequal (@var{A}, @var{B})} returns a logical scalar
    ## @var{TF}, which is @qcode{true} when the timetables @var{A} and @var{B}
    ## are the same size, carry the same row times, variable names and
    ## metadata, and each pair of corresponding variables holds equal values,
    ## and @qcode{false} otherwise.
    ##
    ## The row times take part in the comparison and the properties that
    ## merely describe them do not, so @qcode{TimeStep}, @qcode{SampleRate}
    ## and @qcode{StartTime} are excluded.  A timetable told its time step at
    ## construction therefore equals one that read the same step off the row
    ## times it was given, although the two report a different
    ## @qcode{TimeStep} once subset to a single row.
    ##
    ## Variables are compared by value and not by class, exactly as
    ## @code{isequal} compares arrays elsewhere, so a timetable holding
    ## @code{int8 ([1; 2])} equals one holding @code{[1; 2]}.  The
    ## @qcode{VariableTypes} property, which only restates those classes,
    ## takes no part in the comparison.  Every other property does: two
    ## timetables differing only in @qcode{Description}, @qcode{UserData},
    ## @qcode{VariableUnits}, @qcode{VariableDescriptions},
    ## @qcode{VariableContinuity} or a custom property are not equal.
    ##
    ## As with @qcode{NaN}, missing values are never equal, so a missing row
    ## time or element anywhere in either timetable makes the result
    ## @qcode{false}; use @code{isequaln} to treat missing values as equal.
    ##
    ## Further timetables may be supplied, as in @code{isequal (@var{A},
    ## @var{B}, @var{C}, @dots{})}, in which case @var{TF} is @qcode{true}
    ## only when all of them are equal to one another.  Any argument that is
    ## not a timetable, a table included, makes the result @qcode{false}
    ## rather than raising an error.
    ##
    ## @end deftypefn
    function TF = isequal (varargin)
      if (nargin < 2)
        print_usage ();
      endif
      TF = false;
      if (all (cellfun (@(x) isa (x, 'timetable'), varargin)))
        TF = isequalResult (varargin{1}, varargin(2:end), false);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} isequaln (@var{A}, @var{B})
    ## @deftypefnx {timetable} {@var{TF} =} isequaln (@var{A}, @var{B}, @dots{})
    ##
    ## Test timetables for equality, treating missing values as equal.
    ##
    ## @code{@var{TF} = isequaln (@var{A}, @var{B})} is identical to
    ## @code{isequal (@var{A}, @var{B})} except that missing values are
    ## treated as equal to one another, in the same way that @code{isequaln}
    ## treats @qcode{NaN}.  Two timetables whose row times are @qcode{NaT} in
    ## the same places are therefore equal, where @code{isequal} calls them
    ## unequal.
    ##
    ## Further timetables may be supplied, as in @code{isequaln (@var{A},
    ## @var{B}, @var{C}, @dots{})}, in which case @var{TF} is @qcode{true}
    ## only when all of them are equal to one another.  Any argument that is
    ## not a timetable, a table included, makes the result @qcode{false}
    ## rather than raising an error.
    ##
    ## @end deftypefn
    function TF = isequaln (varargin)
      if (nargin < 2)
        print_usage ();
      endif
      TF = false;
      if (all (cellfun (@(x) isa (x, 'timetable'), varargin)))
        TF = isequalResult (varargin{1}, varargin(2:end), true);
      endif
    endfunction

  endmethods

################################################################################
##                        **    Missing Data    **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'ismissing'        'anymissing'       'rmmissing'                          ##
## 'standardizeMissing'                                                       ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} ismissing (@var{tt})
    ## @deftypefnx {timetable} {@var{TF} =} ismissing (@var{tt}, @var{indicator})
    ##
    ## Find missing values in the variables of a timetable.
    ##
    ## @code{@var{TF} = ismissing (@var{tt})} returns a logical array with one
    ## row per row of @var{tt} and one column per variable, true where the
    ## variable is missing there.  What counts as missing depends on the type:
    ## @qcode{NaN} for numeric data, @qcode{NaT} for @code{datetime},
    ## @qcode{<undefined>} for @code{categorical}, and an empty character
    ## vector or string.
    ##
    ## The row times are not read.  They label the rows rather than being one
    ## of them, so a missing row time is not reported here and does not make
    ## @code{anymissing} true.  @code{rmmissing} does drop such a row, being
    ## about what can be kept rather than about what is missing.
    ##
    ## @code{@var{TF} = ismissing (@var{tt}, @var{indicator})} treats the
    ## values in @var{indicator} as missing as well.
    ##
    ## @seealso{anymissing, rmmissing, standardizeMissing, timetable}
    ## @end deftypefn
    function TF = ismissing (this, varargin)
      [TF, errmsg] = ismissingResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.ismissing: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{TF} =} anymissing (@var{tt})
    ##
    ## True when any variable of a timetable has a missing value.
    ##
    ## @code{@var{TF} = anymissing (@var{tt})} is @code{any (ismissing
    ## (@var{tt})(:))} and reads the same thing: the variables, and not the
    ## row times.  A timetable whose only missing value is a row time
    ## answers false here and still loses that row to @code{rmmissing}.
    ##
    ## @seealso{ismissing, rmmissing, timetable}
    ## @end deftypefn
    function TF = anymissing (this)
      TF = any (any (ismissing (this)));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} rmmissing (@var{ttA})
    ## @deftypefnx {timetable} {@var{ttB} =} rmmissing (@var{ttA}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{ttB}, @var{TF}] =} rmmissing (@dots{})
    ##
    ## Remove the incomplete rows of a timetable.
    ##
    ## @code{@var{ttB} = rmmissing (@var{ttA})} removes every row holding a
    ## missing value.
    ##
    ## @strong{A row whose row time is missing is removed whatever its
    ## variables hold}, and whatever @qcode{'DataVariables'} or
    ## @qcode{'MinNumMissing'} say: a row that cannot be placed in time is
    ## not a row a timetable can keep.  That is a precondition rather than a
    ## report of missingness, which is why @code{ismissing} does not mark
    ## such a row and @code{anymissing} does not count it.
    ##
    ## @qcode{'DataVariables'} names the variables to judge a row by, and
    ## @qcode{'MinNumMissing'} how many missing values a row must hold before
    ## it goes.  Neither reaches the row times.
    ##
    ## @code{[@var{ttB}, @var{TF}] = rmmissing (@dots{})} also returns a
    ## logical column marking the rows that were removed, the ones dropped
    ## for their row times included.
    ##
    ## The time step is read afresh from the rows that survive.
    ##
    ## @seealso{ismissing, standardizeMissing, timetable}
    ## @end deftypefn
    function [tbl, TF] = rmmissing (this, varargin)
      [tbl, TF, errmsg] = rmmissingResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.rmmissing: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} standardizeMissing (@var{ttA}, @var{indicator})
    ## @deftypefnx {timetable} {@var{ttB} =} standardizeMissing (@dots{}, @qcode{'DataVariables'}, @var{vars})
    ##
    ## Make given values missing in a timetable.
    ##
    ## @code{@var{ttB} = standardizeMissing (@var{ttA}, @var{indicator})}
    ## replaces every value matching @var{indicator} with the missing value
    ## of its own type, so that @code{ismissing} and @code{rmmissing} will
    ## afterwards treat it as missing.
    ##
    ## @qcode{'DataVariables'} restricts it to the variables named.  The row
    ## times are not a data variable and are never rewritten, so the time
    ## step is unchanged.
    ##
    ## @seealso{ismissing, rmmissing, timetable}
    ## @end deftypefn
    function tbl = standardizeMissing (this, varargin)
      [tbl, errmsg] = standardizeMissingResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.standardizeMissing: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} fillmissing (@var{ttA}, @var{method})
    ## @deftypefnx {timetable} {@var{ttB} =} fillmissing (@var{ttA}, @qcode{'constant'}, @var{v})
    ## @deftypefnx {timetable} {@var{ttB} =} fillmissing (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{ttB}, @var{TF}] =} fillmissing (@dots{})
    ##
    ## Fill the missing values of a timetable.
    ##
    ## @code{@var{ttB} = fillmissing (@var{ttA}, @var{method})} replaces each
    ## missing value by one worked out from the values around it.
    ## @qcode{'previous'}, @qcode{'next'} and @qcode{'nearest'} copy a
    ## neighbouring value; @qcode{'linear'}, @qcode{'spline'},
    ## @qcode{'pchip'} and @qcode{'makima'} interpolate; and
    ## @qcode{'constant'} takes the value given after it.
    ##
    ## @strong{The row times are what the filling runs against}, not the
    ## order of the rows.  A gap an hour after its left neighbour and two
    ## hours before its right one is filled a third of the way between them
    ## by @qcode{'linear'}, and takes the left value under
    ## @qcode{'nearest'}, where counting rows would put it midway and call
    ## the two neighbours equally close.  The row times are already the
    ## sample points, so @qcode{'SamplePoints'} is not accepted; a timetable
    ## whose row times are not all known is refused outright rather than
    ## filled around the gap.
    ##
    ## @qcode{'DataVariables'} names the variables to fill and
    ## @qcode{'EndValues'} says what to do with a gap that has no neighbour
    ## on one side, taking @qcode{'extrap'}, another method name, or a
    ## constant.
    ##
    ## @code{[@var{ttB}, @var{TF}] = fillmissing (@dots{})} also returns a
    ## logical array marking what was filled.  The row times themselves are
    ## never filled and the time step is unchanged.
    ##
    ## @seealso{ismissing, rmmissing, standardizeMissing, timetable}
    ## @end deftypefn
    function [tbl, TF] = fillmissing (this, varargin)
      [tbl, TF, errmsg] = fillmissingResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.fillmissing: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {} summary (@var{tt})
    ## @deftypefnx {timetable} {@var{s} =} summary (@var{tt})
    ##
    ## Summarise a timetable.
    ##
    ## @code{summary (@var{tt})} prints what the timetable holds: its size,
    ## its description where it has one, the row times, the variables with
    ## their types and any units and descriptions, and a table of statistics
    ## for everything that has any.
    ##
    ## @code{@var{s} = summary (@var{tt})} returns that as a structure
    ## instead, one field per variable and one for the row times, filed under
    ## the row dimension's name and coming first.
    ##
    ## The row times report where they start, how fast they run and how far
    ## apart they step, in @qcode{StartTime}, @qcode{SampleRate} and
    ## @qcode{TimeStep}, alongside the statistics a @code{datetime} or
    ## @code{duration} variable would report.  They carry no description,
    ## units or continuity, having none.
    ##
    ## Which statistics a variable reports follows its type.  Numeric,
    ## @code{datetime} and @code{duration} variables report the count of
    ## missing values, the smallest, the median, the largest, the mean and
    ## the standard deviation; an integer reports no deviation and its median
    ## rounds to its own type; an ordinal @code{categorical} is ordered but
    ## has no mean; a plain one reports its categories and their counts; a
    ## @code{logical} reports how many are true and false and appears in no
    ## statistics; and everything else reports the count of missing values
    ## alone.  A variable of several columns reports one row per column.
    ##
    ## @seealso{ismissing, timetable}
    ## @end deftypefn
    function [varargout] = summary (this, varargin)
      if (! isempty (varargin))
        error ("timetable.summary: too many input arguments.");
      endif
      s = summaryOf (this);
      if (nargout == 0)
        summaryPrint (this, s, inputname (1));
      elseif (nargout == 1)
        varargout{1} = s;
      else
        error ("timetable.summary: invalid number of output arguments.");
      endif
    endfunction

  endmethods

################################################################################
##                     **    Variable Management    **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'addvars'          'removevars'       'movevars'         'renamevars'      ##
## 'convertvars'      'mergevars'        'splitvars'                          ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} addvars (@var{ttA}, @var{var1}, @dots{}, @var{varN})
    ## @deftypefnx {timetable} {@var{ttB} =} addvars (@dots{}, @qcode{'Before'}, @var{location})
    ## @deftypefnx {timetable} {@var{ttB} =} addvars (@dots{}, @qcode{'After'}, @var{location})
    ## @deftypefnx {timetable} {@var{ttB} =} addvars (@dots{}, @qcode{'NewVariableNames'}, @var{newNames})
    ##
    ## Add variables to a timetable.
    ##
    ## @code{@var{ttB} = addvars (@var{ttA}, @var{var1}, @dots{})} appends
    ## each array as a new variable at the right-hand end.  Every one must
    ## have as many rows as the timetable has.
    ##
    ## Unnamed variables take the name of the workspace variable they came
    ## from, or @qcode{Var@var{N}} where there is none.  A name that would
    ## collide with the row dimension is not allowed and one that arrives by
    ## the workspace route is given a suffix instead.
    ##
    ## @qcode{'Before'} and @qcode{'After'} place the new variables beside an
    ## existing one, named or numbered.  The row times are not a variable and
    ## cannot be used as the location, nor can position zero.
    ##
    ## The row times are untouched, so the time step is unchanged.
    ##
    ## @seealso{removevars, movevars, timetable}
    ## @end deftypefn
    function tbl = addvars (this, varargin)
      ## Only the public method can read the caller's names.
      argNames = cell (1, numel (varargin));
      for i = 1:numel (varargin)
        argNames{i} = inputname (i + 1);
      endfor
      [tbl, errmsg] = addvarsResult (this, argNames, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.addvars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{ttB} =} removevars (@var{ttA}, @var{vars})
    ##
    ## Remove variables from a timetable.
    ##
    ## @code{@var{ttB} = removevars (@var{ttA}, @var{vars})} deletes the
    ## variables named, numbered, selected by a logical vector or picked out
    ## by a @code{vartype}.
    ##
    ## The row times are not a variable and cannot be removed: a timetable
    ## keeps them whatever else goes.  Removing every variable leaves a
    ## timetable with none, still carrying its row times and its time step,
    ## rather than an empty one.
    ##
    ## @seealso{addvars, movevars, timetable}
    ## @end deftypefn
    function tbl = removevars (this, varargin)
      [tbl, errmsg] = removevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.removevars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} movevars (@var{ttA}, @var{vars}, @qcode{'Before'}, @var{location})
    ## @deftypefnx {timetable} {@var{ttB} =} movevars (@var{ttA}, @var{vars}, @qcode{'After'}, @var{location})
    ##
    ## Move variables within a timetable.
    ##
    ## @code{@var{ttB} = movevars (@var{ttA}, @var{vars}, @dots{})} puts the
    ## variables named in @var{vars} before or after @var{location}, which
    ## names or numbers another variable.  Each variable takes its units,
    ## description and continuity with it.
    ##
    ## The row times are not a variable: they can be neither moved nor used
    ## as the location, and they stay where they are whatever else is
    ## reordered.
    ##
    ## @seealso{addvars, removevars, timetable}
    ## @end deftypefn
    function tbl = movevars (this, varargin)
      [tbl, errmsg] = movevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.movevars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{ttB} =} renamevars (@var{ttA}, @var{vars}, @var{newNames})
    ##
    ## Rename variables in a timetable.
    ##
    ## @code{@var{ttB} = renamevars (@var{ttA}, @var{vars}, @var{newNames})}
    ## gives each variable in @var{vars} the matching name in
    ## @var{newNames}.  Everything else about the variable is kept, its units
    ## and its description included.
    ##
    ## Dimension names are not renamed here: assign to
    ## @qcode{@var{tt}.Properties.DimensionNames} for that.  A variable
    ## cannot be given the row dimension's name either, the two sharing one
    ## namespace.
    ##
    ## @seealso{movevars, timetable}
    ## @end deftypefn
    function tbl = renamevars (this, varargin)
      [tbl, errmsg] = renamevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.renamevars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{ttB} =} convertvars (@var{ttA}, @var{vars}, @var{dataType})
    ##
    ## Convert variables of a timetable to a given type.
    ##
    ## @code{@var{ttB} = convertvars (@var{ttA}, @var{vars}, @var{dataType})}
    ## converts each variable in @var{vars}, which may be named, numbered,
    ## selected by a logical vector or picked out by a @code{vartype}.
    ## @var{dataType} is a type name or a function handle that performs the
    ## conversion.
    ##
    ## The row times are not a data variable and cannot be converted; assign
    ## to @qcode{@var{tt}.Properties.RowTimes} to change them.
    ##
    ## @seealso{renamevars, timetable}
    ## @end deftypefn
    function tbl = convertvars (this, varargin)
      [tbl, errmsg] = convertvarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.convertvars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} mergevars (@var{ttA}, @var{vars})
    ## @deftypefnx {timetable} {@var{ttB} =} mergevars (@dots{}, @qcode{'NewVariableName'}, @var{name})
    ## @deftypefnx {timetable} {@var{ttB} =} mergevars (@dots{}, @qcode{'MergeAsTable'}, @var{tf})
    ##
    ## Combine several variables of a timetable into one.
    ##
    ## @code{@var{ttB} = mergevars (@var{ttA}, @var{vars})} replaces the
    ## variables in @var{vars} with a single multi-column one, put where the
    ## first of them was.  It is named @qcode{Var@var{N}} unless
    ## @qcode{'NewVariableName'} says otherwise, and the merged variables are
    ## no longer reachable by their old names.
    ##
    ## The units and descriptions of the merged variables do not survive:
    ## one variable carries one of each, and there is no saying which it
    ## should be.  The remaining variables keep theirs.
    ##
    ## The row times are not a variable and cannot be merged.
    ##
    ## @seealso{splitvars, timetable}
    ## @end deftypefn
    function tbl = mergevars (this, varargin)
      [tbl, errmsg] = mergevarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.mergevars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} splitvars (@var{ttA})
    ## @deftypefnx {timetable} {@var{ttB} =} splitvars (@var{ttA}, @var{vars})
    ## @deftypefnx {timetable} {@var{ttB} =} splitvars (@dots{}, @qcode{'NewVariableNames'}, @var{newNames})
    ##
    ## Split multi-column variables of a timetable into one each.
    ##
    ## @code{@var{ttB} = splitvars (@var{ttA})} splits every multi-column
    ## variable, and every nested table, into one variable per column.
    ## @code{@var{ttB} = splitvars (@var{ttA}, @var{vars})} splits only those
    ## named.
    ##
    ## The new variables are named after the one they came from with a column
    ## number appended, unless @qcode{'NewVariableNames'} gives them names.
    ## Splitting a variable that was merged does not bring back the units the
    ## merge discarded.
    ##
    ## The row times are not a variable and cannot be split.
    ##
    ## @seealso{mergevars, timetable}
    ## @end deftypefn
    function tbl = splitvars (this, varargin)
      [tbl, errmsg] = splitvarsResult (this, varargin{:});
      if (! isempty (errmsg))
        error ("timetable.splitvars: %s", errmsg);
      endif
    endfunction

  endmethods

################################################################################
##                       **    First and Last Rows    **                      ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'head'             'tail'                                                  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {} head (@var{tt})
    ## @deftypefnx {timetable} {} head (@var{tt}, @var{k})
    ## @deftypefnx {timetable} {@var{out} =} head (@var{tt}, @var{k})
    ##
    ## Display or return the first @var{k} rows of a timetable.
    ##
    ## @code{head (@var{tt})} displays the first eight rows of @var{tt}, or
    ## all of them if it has fewer.  @code{head (@var{tt}, @var{k})} displays
    ## the first @var{k} instead.  @var{k} must be a real, nonnegative,
    ## integer scalar value; a @var{k} of zero displays no rows at all rather
    ## than raising.
    ##
    ## @code{@var{out} = head (@var{tt}, @var{k})} returns those rows in a
    ## new timetable instead of displaying them.  If @var{k} is omitted or
    ## empty it defaults to eight.
    ##
    ## The rows come back in the order the timetable holds them.  @code{head}
    ## takes the first rows and not the earliest ones, so on a timetable
    ## whose times are out of order it returns whatever happens to be stored
    ## first.  Sort it beforehand to ask the other question.
    ##
    ## The time step is read afresh from the row times that are kept, so the
    ## head of an hourly timetable is hourly and the head of a timetable
    ## whose spacing changes partway may not be.
    ##
    ## @seealso{tail, sortrows, timetable}
    ## @end deftypefn
    function [varargout] = head (this, k)
      if (nargin < 2)
        k = [];
      endif
      [ixRows, errmsg] = headTailRows (this, k, false);
      if (! isempty (errmsg))
        error ("timetable.head: %s", errmsg);
      endif
      out = subsetrows (this, ixRows);
      if (nargout == 0)
        print_table (out);
      elseif (nargout == 1)
        varargout{1} = out;
      else
        error ("timetable.head: invalid number of output arguments.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {} tail (@var{tt})
    ## @deftypefnx {timetable} {} tail (@var{tt}, @var{k})
    ## @deftypefnx {timetable} {@var{out} =} tail (@var{tt}, @var{k})
    ##
    ## Display or return the last @var{k} rows of a timetable.
    ##
    ## @code{tail (@var{tt})} displays the last eight rows of @var{tt}, or
    ## all of them if it has fewer.  @code{tail (@var{tt}, @var{k})} displays
    ## the last @var{k} instead.  @var{k} must be a real, nonnegative,
    ## integer scalar value; a @var{k} of zero displays no rows at all rather
    ## than raising.
    ##
    ## @code{@var{out} = tail (@var{tt}, @var{k})} returns those rows in a
    ## new timetable instead of displaying them.  If @var{k} is omitted or
    ## empty it defaults to eight.
    ##
    ## The rows come back in the order the timetable holds them, and they are
    ## the last rows rather than the latest ones.  The time step is read
    ## afresh from the row times that are kept.
    ##
    ## @seealso{head, sortrows, timetable}
    ## @end deftypefn
    function [varargout] = tail (this, k)
      if (nargin < 2)
        k = [];
      endif
      [ixRows, errmsg] = headTailRows (this, k, true);
      if (! isempty (errmsg))
        error ("timetable.tail: %s", errmsg);
      endif
      out = subsetrows (this, ixRows);
      if (nargout == 0)
        print_table (out);
      elseif (nargout == 1)
        varargout{1} = out;
      else
        error ("timetable.tail: invalid number of output arguments.");
      endif
    endfunction

  endmethods

################################################################################
##                     **    Row Ordering and Sets    **                      ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'sortrows'         'unique'           'topkrows'                           ##
## 'issorted'         'issortedrows'                                          ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} sortrows (@var{ttA})
    ## @deftypefnx {timetable} {@var{ttB} =} sortrows (@var{ttA}, @var{rowDimName})
    ## @deftypefnx {timetable} {@var{ttB} =} sortrows (@var{ttA}, @var{vars})
    ## @deftypefnx {timetable} {@var{ttB} =} sortrows (@var{ttA}, @dots{}, @var{direction})
    ## @deftypefnx {timetable} {@var{ttB} =} sortrows (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{ttB}, @var{index}] =} sortrows (@dots{})
    ##
    ## Sort the rows of a timetable.
    ##
    ## @code{@var{ttB} = sortrows (@var{ttA})} orders the rows by their row
    ## times, earliest first.  Rows sharing a time keep the order they were
    ## in: the row times alone decide, and the variables are never consulted
    ## to break a tie.
    ##
    ## @code{@var{ttB} = sortrows (@var{ttA}, @var{rowDimName})} does the
    ## same, naming the row times through the first of
    ## @code{@var{ttA}.Properties.DimensionNames}.  That name is the only way
    ## to reach them: a numeric index counts the variables, so
    ## @code{sortrows (@var{ttA}, 1)} orders by the first variable and not by
    ## the times, and there is no index that means the row times.  Renaming
    ## the row dimension renames the key with it.
    ##
    ## @code{@var{ttB} = sortrows (@var{ttA}, @var{vars})} orders by one or
    ## more variables, named, indexed by number, selected by a logical vector
    ## or picked out by a @code{vartype}.  A negative index sorts that
    ## variable in descending order.  The row dimension name may appear among
    ## @var{vars}, in which case the row times take their turn as a key like
    ## any other.
    ##
    ## @code{@var{ttB} = sortrows (@dots{}, @var{direction})} sorts as
    ## @qcode{'ascend'} or @qcode{'descend'}, either one direction for every
    ## key or one per key.  A direction must follow the keys it applies to,
    ## so @code{sortrows (@var{ttA}, 'descend')} is an error rather than a
    ## reversed sort: with nothing else given, the first argument is read as
    ## the variables to sort by.
    ##
    ## The name-value pair @qcode{'MissingPlacement'} takes
    ## @qcode{'auto'} (the default), @qcode{'first'} or @qcode{'last'} and
    ## says where missing keys go.  @qcode{'auto'} puts them last when
    ## sorting up and first when sorting down.  @qcode{'ComparisonMethod'}
    ## takes @qcode{'auto'}, @qcode{'real'} or @qcode{'abs'} and applies to
    ## numeric variables.  Both must follow the keys as well.
    ##
    ## @code{[@var{ttB}, @var{index}] = sortrows (@dots{})} also returns the
    ## permutation, so that @code{@var{ttA}(@var{index},:)} is @var{ttB}.
    ##
    ## The time step is read afresh from the sorted row times rather than
    ## carried over, so sorting an out-of-order hourly timetable makes it
    ## regular again, and sorting one that ran backwards turns its step from
    ## minus one hour to plus one.
    ##
    ## @seealso{issortedrows, timetable}
    ## @end deftypefn
    function [tbl, index] = sortrows (this, varargin)
      [index, errmsg] = sortrowsIndex (this, varargin);
      if (! isempty (errmsg))
        error ("timetable.sortrows: %s", errmsg);
      endif
      tbl = subsetrows (this, index);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} unique (@var{ttA})
    ## @deftypefnx {timetable} {@var{ttB} =} unique (@var{ttA}, @var{setOrder})
    ## @deftypefnx {timetable} {[@var{ttB}, @var{ia}, @var{ic}] =} unique (@dots{})
    ##
    ## Unique rows of a timetable.
    ##
    ## @code{@var{ttB} = unique (@var{ttA})} returns the distinct rows of
    ## @var{ttA}, ordered by row time.  A row is its time together with its
    ## values, so two rows count as one only when they agree on both.  Rows
    ## sharing a time but differing in a variable are all kept, as are rows
    ## sharing their values at different times; the row times are a column of
    ## the comparison and not a key that overrides it.
    ##
    ## @code{@var{ttB} = unique (@var{ttA}, @var{setOrder})} chooses the
    ## order of the result.  @qcode{'sorted'} is the default and
    ## @qcode{'stable'} keeps the rows in the order they were met.
    ## @qcode{'first'} and @qcode{'last'} say which of a set of equal rows is
    ## the one reported in @var{ia}.  @qcode{'rows'} is accepted and changes
    ## nothing, rows being the only thing a timetable compares.
    ##
    ## @code{[@var{ttB}, @var{ia}, @var{ic}] = unique (@dots{})} also returns
    ## index vectors, such that @var{ttB} is @code{@var{ttA}(@var{ia},:)} and
    ## @var{ttA} is @code{@var{ttB}(@var{ic},:)}.
    ##
    ## A timetable with no variables is compared on its row times alone, so
    ## repeated times reduce to one.  The time step is read afresh from the
    ## row times that survive.
    ##
    ## Variables of @code{cell}, other than a cell array of character
    ## vectors, and of @code{struct} have no order to compare and are
    ## refused.
    ##
    ## @seealso{sortrows, timetable}
    ## @end deftypefn
    function [tbl, ia, ic] = unique (this, varargin)
      [ia, ic, errmsg] = uniqueIndex (this, varargin);
      if (! isempty (errmsg))
        error ("timetable.unique: %s", errmsg);
      endif
      tbl = subsetrows (this, ia);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttB} =} topkrows (@var{ttA}, @var{k})
    ## @deftypefnx {timetable} {@var{ttB} =} topkrows (@var{ttA}, @var{k}, @var{vars})
    ## @deftypefnx {timetable} {@var{ttB} =} topkrows (@var{ttA}, @var{k}, @var{vars}, @var{direction})
    ## @deftypefnx {timetable} {@var{ttB} =} topkrows (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{ttB}, @var{index}] =} topkrows (@dots{})
    ##
    ## The top @var{k} rows of a timetable, in sorted order.
    ##
    ## @code{@var{ttB} = topkrows (@var{ttA}, @var{k})} returns the @var{k}
    ## rows with the latest row times, latest first.  This is where it parts
    ## company with @code{head}, which takes the first rows as they are
    ## stored: @code{topkrows} ranks them.
    ##
    ## @code{@var{ttB} = topkrows (@var{ttA}, @var{k}, @var{vars})} ranks by
    ## one or more variables instead, named, indexed by number, selected by a
    ## logical vector or picked out by a @code{vartype}.  The row dimension
    ## name may be used to rank by the row times explicitly.  A numeric index
    ## counts the variables, and its sign is read the other way round from
    ## @code{sortrows}: a positive index ranks downwards.
    ##
    ## @code{@var{ttB} = topkrows (@dots{}, @var{direction})} sorts as
    ## @qcode{'ascend'} or @qcode{'descend'}, the latter being the default
    ## and what makes these the top rows rather than the bottom ones.
    ##
    ## Missing keys rank last however the sort runs, which is the one place
    ## @code{topkrows} differs from @code{sortrows} beyond its direction.
    ## @qcode{'MissingPlacement'} overrides that, and
    ## @qcode{'ComparisonMethod'} applies to numeric variables as usual.
    ##
    ## Asking for more rows than there are returns all of them, still
    ## ranked, and a @var{k} of zero returns none.
    ##
    ## @code{[@var{ttB}, @var{index}] = topkrows (@dots{})} also returns the
    ## rows chosen, so that @code{@var{ttA}(@var{index},:)} is @var{ttB}.
    ##
    ## The time step is read afresh from the row times that are kept, so the
    ## default ranking of a regular timetable steps backwards.
    ##
    ## @seealso{head, sortrows, timetable}
    ## @end deftypefn
    function [tbl, ix] = topkrows (this, k, varargin)
      [ix, errmsg] = topkrowsIndex (this, k, varargin);
      if (! isempty (errmsg))
        error ("timetable.topkrows: %s", errmsg);
      endif
      tbl = subsetrows (this, ix);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{TF} =} issorted (@var{tt})
    ##
    ## True when the row times of a timetable are in ascending order.
    ##
    ## @code{@var{TF} = issorted (@var{tt})} returns true when each row time
    ## is at or after the one before it.  The ordering is not strict, so
    ## repeated times are sorted; a timetable with any missing row time is
    ## not, and one with fewer than two rows has nothing out of order and so
    ## is.  Only the row times are read: the variables take no part.
    ##
    ## This is the whole of the question @code{issorted} answers here.  It
    ## takes no dimension, no direction and no options; use
    ## @code{issortedrows} to ask about a direction, about a variable, or
    ## about where missing values should fall.
    ##
    ## A @code{table} has no @code{issorted}, its row names being labels
    ## rather than an ordering.
    ##
    ## @seealso{issortedrows, sortrows, timetable}
    ## @end deftypefn
    function TF = issorted (this, varargin)
      if (nargin > 1)
        error (strcat ("timetable.issorted: no options are accepted;", ...
                       " use 'issortedrows' instead."));
      endif
      TF = issorted (this.RowTimes);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} issortedrows (@var{tt})
    ## @deftypefnx {timetable} {@var{TF} =} issortedrows (@var{tt}, @var{rowDimName})
    ## @deftypefnx {timetable} {@var{TF} =} issortedrows (@var{tt}, @var{vars})
    ## @deftypefnx {timetable} {@var{TF} =} issortedrows (@var{tt}, @dots{}, @var{direction})
    ## @deftypefnx {timetable} {@var{TF} =} issortedrows (@dots{}, @var{Name}, @var{Value})
    ##
    ## True when the rows of a timetable are already in a given order.
    ##
    ## @code{@var{TF} = issortedrows (@var{tt})} returns true when the rows
    ## are in ascending order of their row times, which is the order
    ## @code{sortrows} would put them in with nothing else asked.
    ##
    ## Every form @code{sortrows} accepts is accepted here and asks the same
    ## question of it: the row dimension name, one or more variables named,
    ## indexed or selected, a direction, and the @qcode{'MissingPlacement'}
    ## and @qcode{'ComparisonMethod'} pairs.  A direction must follow the
    ## keys it applies to, as it must there.
    ##
    ## The sort is stable, so the answer is exactly whether sorting would
    ## leave every row where it already is.
    ##
    ## @seealso{issorted, sortrows, timetable}
    ## @end deftypefn
    function TF = issortedrows (this, varargin)
      [TF, errmsg] = issortedrowsCheck (this, varargin);
      if (! isempty (errmsg))
        error ("timetable.issortedrows: %s", errmsg);
      endif
    endfunction

  endmethods

################################################################################
##                        **    Range Predicates    **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'containsrange'    'overlapsrange'    'withinrange'                        ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} containsrange (@var{tt}, @var{ref})
    ## @deftypefnx {timetable} {[@var{TF}, @var{whichRows}] =} containsrange (@var{tt}, @var{ref})
    ##
    ## True when a timetable spans the whole of a range of times.
    ##
    ## @code{@var{TF} = containsrange (@var{tt}, @var{ref})} returns true when
    ## every instant of @var{ref} lies inside the range of @var{tt}, that is
    ## between its earliest and its latest row time, both included.  The row
    ## times need be neither sorted nor unique, since only the two ends of the
    ## range are read, and missing row times take no part in it.
    ##
    ## @var{ref} says what range is meant, in one of three ways.  A
    ## @code{timerange} gives its own bounds, and says at each end whether the
    ## bound itself belongs to the range.  A @code{timetable} gives the range
    ## between its earliest and its latest row time, both ends included.  A
    ## @code{datetime} or @code{duration} scalar gives a single instant.  A
    ## reference whose times are of the other kind is refused: elapsed time
    ## and a calendar cannot be compared.
    ##
    ## @code{[@var{TF}, @var{whichRows}] = containsrange (@dots{})} also
    ## returns a column of logicals, one per row of @var{tt}, saying which of
    ## its rows fall in @var{ref}.  That answer is the same for all three
    ## range predicates and is independent of @var{TF}: rows may fall in a
    ## range the timetable does not contain.
    ##
    ## A timetable whose row times are all missing has no range and answers
    ## false, and so does a reference naming no instant, such as a @code{NaT}.
    ##
    ## @seealso{overlapsrange, withinrange, timerange, timetable}
    ## @end deftypefn
    function [TF, whichRows] = containsrange (this, ref)

      if (nargin < 2)
        error ("timetable.containsrange: not enough input arguments.");
      endif
      [R, errmsg] = rangeRef (ref, this.RowTimes);
      if (! isempty (errmsg))
        error ("timetable.containsrange: %s", errmsg);
      endif
      whichRows = R.whichRows;
      [tlo, thi, spanOk] = timeSpan (this.RowTimes);
      TF = spanOk && R.valid && R.hasLo && R.hasHi ...
           && R.lo >= tlo && R.hi <= thi;

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} overlapsrange (@var{tt}, @var{ref})
    ## @deftypefnx {timetable} {[@var{TF}, @var{whichRows}] =} overlapsrange (@var{tt}, @var{ref})
    ##
    ## True when a timetable and a range of times have any instant in common.
    ##
    ## @code{@var{TF} = overlapsrange (@var{tt}, @var{ref})} returns true when
    ## the range of @var{tt}, from its earliest to its latest row time, shares
    ## at least one instant with @var{ref}.  Meeting at a single instant is
    ## enough: a range that starts exactly at the last row time overlaps.
    ##
    ## Whether the ends themselves count is @var{ref}'s to say.  A
    ## @code{timerange} that excludes its lower bound does not overlap a
    ## timetable that reaches only as far as that bound.
    ##
    ## @var{ref} takes the same three forms as in @code{containsrange}: a
    ## @code{timerange}, a @code{timetable} whose earliest and latest row
    ## times give the range, or a @code{datetime} or @code{duration} scalar
    ## naming one instant.
    ##
    ## @code{[@var{TF}, @var{whichRows}] = overlapsrange (@dots{})} also
    ## returns a column of logicals saying which rows of @var{tt} fall in
    ## @var{ref}.  A timetable can overlap a range without any of its rows
    ## falling in it, the overlap being of the ranges and not of the rows.
    ##
    ## @seealso{containsrange, withinrange, timerange, timetable}
    ## @end deftypefn
    function [TF, whichRows] = overlapsrange (this, ref)

      if (nargin < 2)
        error ("timetable.overlapsrange: not enough input arguments.");
      endif
      [R, errmsg] = rangeRef (ref, this.RowTimes);
      if (! isempty (errmsg))
        error ("timetable.overlapsrange: %s", errmsg);
      endif
      whichRows = R.whichRows;
      [tlo, thi, spanOk] = timeSpan (this.RowTimes);
      if (! (spanOk && R.valid))
        TF = false;
        return
      endif
      above = ! R.hasLo || boundMet (thi, R.lo, R.closedLeft, 'above');
      below = ! R.hasHi || boundMet (tlo, R.hi, R.closedRight, 'below');
      TF = above && below;

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{TF} =} withinrange (@var{tt}, @var{ref})
    ## @deftypefnx {timetable} {[@var{TF}, @var{whichRows}] =} withinrange (@var{tt}, @var{ref})
    ##
    ## True when the whole range of a timetable lies inside a range of times.
    ##
    ## @code{@var{TF} = withinrange (@var{tt}, @var{ref})} returns true when
    ## every row time of @var{tt} lies inside @var{ref}, which is to say that
    ## its earliest and its latest row time both do.
    ##
    ## Whether the ends themselves count is @var{ref}'s to say, and this is
    ## where it tells.  A @code{timerange} is half open by default, excluding
    ## the time it ends at, so a timetable whose last row falls exactly on
    ## that time is @emph{not} within it; the same range built closed
    ## contains that instant, and the answer turns true.
    ##
    ## @var{ref} takes the same three forms as in @code{containsrange}: a
    ## @code{timerange}, a @code{timetable} whose earliest and latest row
    ## times give the range, or a @code{datetime} or @code{duration} scalar
    ## naming one instant.  A timetable is within a single instant only when
    ## every row time it has is that instant.
    ##
    ## @code{[@var{TF}, @var{whichRows}] = withinrange (@dots{})} also returns
    ## a column of logicals saying which rows of @var{tt} fall in @var{ref}.
    ## When @var{TF} is true they are all true, this being the one predicate
    ## for which the two answers agree.
    ##
    ## @seealso{containsrange, overlapsrange, timerange, timetable}
    ## @end deftypefn
    function [TF, whichRows] = withinrange (this, ref)

      if (nargin < 2)
        error ("timetable.withinrange: not enough input arguments.");
      endif
      [R, errmsg] = rangeRef (ref, this.RowTimes);
      if (! isempty (errmsg))
        error ("timetable.withinrange: %s", errmsg);
      endif
      whichRows = R.whichRows;
      [tlo, thi, spanOk] = timeSpan (this.RowTimes);
      if (! (spanOk && R.valid))
        TF = false;
        return
      endif
      above = ! R.hasLo || boundMet (tlo, R.lo, R.closedLeft, 'above');
      below = ! R.hasHi || boundMet (thi, R.hi, R.closedRight, 'below');
      TF = above && below;

    endfunction

  endmethods

################################################################################
##             **    Apply Functions to Timetable Contents    **              ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'varfun'           'rowfun'           'grouptransform'   'groupcounts'     ##
## 'groupsummary'     'groupfilter'      'stack'            'rows2vars'       ##
## 'join'             'innerjoin'        'outerjoin'        'inner2outer'     ##
## 'findgroups'       'splitapply'       'unstack'          'pivot'           ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{B} =} varfun (@var{func}, @var{A})
    ## @deftypefnx {timetable} {@var{B} =} varfun (@var{func}, @var{A}, @var{Name}, @var{Value}, @dots{})
    ##
    ## Apply a function to each variable of a timetable.
    ##
    ## @code{@var{B} = varfun (@var{func}, @var{A})} applies the function
    ## handle @var{func} separately to each variable of the timetable @var{A}
    ## and returns the results in the timetable @var{B}.  Each variable is
    ## passed whole, so a function that reduces a column to a scalar gives a
    ## one-row result.  The output variables are named for the function and
    ## the variable they came from, as in @qcode{mean_Speed}, and every row of
    ## the result carries the row time of the first row it was computed from.
    ##
    ## @code{@var{B} = varfun (@var{func}, @var{A}, @var{Name}, @var{Value},
    ## @dots{})} modifies the operation through the following
    ## @var{Name}/@var{Value} pairs:
    ##
    ## @table @asis
    ## @item @qcode{'InputVariables'}
    ## The variables of @var{A} that @var{func} is applied to, given as
    ## variable names, indices, a logical vector, or a function handle.  By
    ## default every variable of @var{A} that is not a grouping variable is
    ## used.
    ##
    ## @item @qcode{'GroupingVariables'}
    ## One or more variables of @var{A} that define groups of rows, or the row
    ## dimension name, which groups by the row times themselves.  @var{func}
    ## is then applied once to each group and @var{B} has one row per group,
    ## carrying the row time of the first row of the group.  Grouping
    ## variables appear in @var{B} alongside a @qcode{GroupCount} variable;
    ## the row times do not, being the row times of the result.  Rows with a
    ## missing value in any grouping variable are omitted.
    ##
    ## @item @qcode{'OutputFormat'}
    ## One of @qcode{'table'} (the default, and also selected by
    ## @qcode{'auto'}), which returns the results in a timetable;
    ## @qcode{'uniform'}, which requires @var{func} to return a scalar and
    ## returns them in an array; or @qcode{'cell'}, which returns them in a
    ## cell array.
    ##
    ## @item @qcode{'ErrorHandler'}
    ## A function handle called whenever @var{func} raises an error, receiving
    ## a structure with fields @qcode{identifier}, @qcode{message} and
    ## @qcode{index}, followed by the arguments @var{func} was called with.
    ## Its outputs are used in place of the ones @var{func} did not return.
    ## @end table
    ##
    ## @end deftypefn
    function B = varfun (func, A, varargin)
      if (nargin < 2)
        print_usage ();
      endif
      [B, errmsg] = varfunResult (A, func, varargin);
      if (! isempty (errmsg))
        error ("timetable.varfun: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{B} =} rowfun (@var{func}, @var{A})
    ## @deftypefnx {timetable} {@var{B} =} rowfun (@var{func}, @var{A}, @var{Name}, @var{Value}, @dots{})
    ##
    ## Apply a function to each row of a timetable.
    ##
    ## @code{@var{B} = rowfun (@var{func}, @var{A})} applies the function
    ## handle @var{func} to each row of the timetable @var{A} and returns the
    ## results in the timetable @var{B}, which has one row for each row of
    ## @var{A} and carries its row times.  By default the value of each
    ## variable in the row is passed to @var{func} as a separate input
    ## argument, and the output variables of @var{B} are named @qcode{Var1},
    ## @qcode{Var2}, and so on.
    ##
    ## @code{@var{B} = rowfun (@var{func}, @var{A}, @var{Name}, @var{Value},
    ## @dots{})} modifies the operation through the following
    ## @var{Name}/@var{Value} pairs:
    ##
    ## @table @asis
    ## @item @qcode{'InputVariables'}
    ## The variables of @var{A} that are passed to @var{func}, given as
    ## variable names, indices, a logical vector, or a function handle.  By
    ## default every variable of @var{A} that is not a grouping variable is
    ## used.
    ##
    ## @item @qcode{'GroupingVariables'}
    ## One or more variables of @var{A} that define groups of rows, or the row
    ## dimension name, which groups by the row times themselves.  @var{func}
    ## is then applied once to each group, receiving the values of each input
    ## variable across the rows of the group, and @var{B} has one row per
    ## group, carrying the row time of the first row of the group.  Grouping
    ## variables appear in @var{B} alongside a @qcode{GroupCount} variable;
    ## the row times do not, being the row times of the result.  Rows with a
    ## missing value in any grouping variable are omitted.
    ##
    ## @item @qcode{'OutputVariableNames'}
    ## The names of the output variables of @var{B}, one per output of
    ## @var{func}.
    ##
    ## @item @qcode{'NumOutputs'}
    ## The number of output arguments to request from @var{func}.  It defaults
    ## to the number of @qcode{'OutputVariableNames'} if those are given,
    ## otherwise to @code{1}.
    ##
    ## @item @qcode{'SeparateInputs'}
    ## A logical scalar.  When @code{true} (the default), the value of each
    ## input variable is passed to @var{func} as a separate argument.  When
    ## @code{false}, the values of the row are horizontally concatenated and
    ## passed as a single argument.
    ##
    ## @item @qcode{'ExtractCellContents'}
    ## A logical scalar.  When @code{true}, the contents of cell-valued
    ## variables are extracted before being passed to @var{func}.  It defaults
    ## to @code{false}.
    ##
    ## @item @qcode{'OutputFormat'}
    ## One of @qcode{'table'} (the default, and also selected by
    ## @qcode{'auto'}), which returns the results in a timetable;
    ## @qcode{'uniform'}, which requires @var{func} to return a scalar and
    ## returns them in an array; or @qcode{'cell'}, which returns them in a
    ## cell array.
    ##
    ## @item @qcode{'ErrorHandler'}
    ## A function handle called whenever @var{func} raises an error, receiving
    ## a structure with fields @qcode{identifier}, @qcode{message} and
    ## @qcode{index}, followed by the arguments @var{func} was called with.
    ## Its outputs are used in place of the ones @var{func} did not return.
    ## @end table
    ##
    ## @end deftypefn
    function B = rowfun (func, A, varargin)
      if (nargin < 2)
        print_usage ();
      endif
      [B, errmsg] = rowfunResult (A, func, varargin);
      if (! isempty (errmsg))
        error ("timetable.rowfun: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{G} =} grouptransform (@var{tt}, @var{groupvars}, @var{method})
    ## @deftypefnx {timetable} {@var{G} =} grouptransform (@var{tt}, @var{groupvars}, @var{method}, @var{datavars})
    ## @deftypefnx {timetable} {@var{G} =} grouptransform (@var{tt}, @var{groupvars}, @var{groupbins}, @var{method}, @dots{})
    ## @deftypefnx {timetable} {@var{G} =} grouptransform (@dots{}, @var{Name}, @var{Value})
    ##
    ## Transform the variables of a timetable group by group.
    ##
    ## @code{@var{G} = grouptransform (@var{tt}, @var{groupvars},
    ## @var{method})} groups the rows of @var{tt} by the variables named in
    ## @var{groupvars} and applies @var{method} to each data variable within
    ## each group.  The result has the same rows as @var{tt}, in the same
    ## order and carrying the same row times, so a transform never moves a row
    ## in time.
    ##
    ## @var{groupvars} names one or more variables, or the row dimension name,
    ## which groups by the row times themselves.  @var{method} is one of
    ## @qcode{'zscore'}, @qcode{'norm'}, @qcode{'meancenter'},
    ## @qcode{'rescale'}, @qcode{'meanfill'} and @qcode{'linearfill'}, or a
    ## function handle applied to each group.
    ##
    ## @code{@var{G} = grouptransform (@var{tt}, @var{groupvars},
    ## @var{method}, @var{datavars})} transforms only the variables named in
    ## @var{datavars}.  By default every variable that is not a grouping
    ## variable is transformed.
    ##
    ## @code{@var{G} = grouptransform (@var{tt}, @var{groupvars},
    ## @var{groupbins}, @var{method}, @dots{})} bins the grouping variables
    ## before grouping, @var{groupbins} being a bin count, a vector of edges,
    ## or a time unit for a datetime or duration variable.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'ReplaceValues'}
    ## A logical scalar.  When @code{true} (the default) each transformed
    ## variable replaces the one it came from; when @code{false} the results
    ## are appended as new variables named for the method and the variable
    ## they came from, as in @qcode{zscore_Speed}.
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Which edge of a bin is included, @qcode{'left'} (the default) or
    ## @qcode{'right'}.  It applies only where @var{groupbins} was given.
    ## @end table
    ##
    ## @end deftypefn
    function G = grouptransform (tt, groupvars, varargin)
      if (nargin < 3)
        print_usage ();
      endif
      [G, errmsg] = grouptransformResult (tt, groupvars, varargin);
      if (! isempty (errmsg))
        error ("timetable.grouptransform: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{G} =} groupcounts (@var{tt}, @var{groupvars})
    ## @deftypefnx {timetable} {@var{G} =} groupcounts (@var{tt}, @var{groupvars}, @var{groupbins})
    ## @deftypefnx {timetable} {@var{G} =} groupcounts (@dots{}, @var{Name}, @var{Value})
    ##
    ## Count the rows of a timetable in each group.
    ##
    ## @code{@var{G} = groupcounts (@var{tt}, @var{groupvars})} groups the
    ## rows of @var{tt} by the variables named in @var{groupvars} and returns
    ## a @code{table} with one row per group, carrying the grouping variables,
    ## a @qcode{GroupCount} variable and a @qcode{Percent} variable.  The
    ## result is a table and not a timetable: its rows describe groups rather
    ## than instants, so there is nothing left for row times to label.
    ##
    ## @var{groupvars} names one or more variables, or the row dimension name,
    ## which groups by the row times themselves.
    ##
    ## @code{@var{G} = groupcounts (@var{tt}, @var{groupvars},
    ## @var{groupbins})} bins the grouping variables before grouping,
    ## @var{groupbins} being a bin count, a vector of edges, or a time unit
    ## for a datetime or duration variable.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'IncludeMissingGroups'}
    ## A logical scalar.  When @code{true} (the default) rows whose grouping
    ## value is missing form a group of their own, sorted last.
    ##
    ## @item @qcode{'IncludeEmptyGroups'}
    ## A logical scalar.  When @code{true} the unused categories of a
    ## categorical or binned grouping variable are reported as groups with a
    ## count of zero.  It defaults to @code{false}.
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Which edge of a bin is included, @qcode{'left'} (the default) or
    ## @qcode{'right'}.  It applies only where @var{groupbins} was given.
    ## @end table
    ##
    ## @end deftypefn
    function G = groupcounts (tt, groupvars, varargin)
      if (nargin < 2)
        print_usage ();
      endif
      [G, errmsg] = groupcountsResult (tt, groupvars, varargin);
      if (! isempty (errmsg))
        error ("timetable.groupcounts: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{G} =} groupsummary (@var{tt}, @var{groupvars})
    ## @deftypefnx {timetable} {@var{G} =} groupsummary (@var{tt}, @var{groupvars}, @var{method})
    ## @deftypefnx {timetable} {@var{G} =} groupsummary (@var{tt}, @var{groupvars}, @var{method}, @var{datavars})
    ## @deftypefnx {timetable} {@var{G} =} groupsummary (@var{tt}, @var{groupvars}, @var{groupbins}, @dots{})
    ## @deftypefnx {timetable} {@var{G} =} groupsummary (@dots{}, @var{Name}, @var{Value})
    ##
    ## Summarise the variables of a timetable group by group.
    ##
    ## @code{@var{G} = groupsummary (@var{tt}, @var{groupvars})} groups the
    ## rows of @var{tt} by the variables named in @var{groupvars} and returns
    ## a @code{table} with one row per group, carrying the grouping variables
    ## and a @qcode{GroupCount} variable.  The result is a table and not a
    ## timetable: its rows describe groups rather than instants, so there is
    ## nothing left for row times to label.
    ##
    ## @var{groupvars} names one or more variables, or the row dimension name,
    ## which groups by the row times themselves.
    ##
    ## @code{@var{G} = groupsummary (@var{tt}, @var{groupvars}, @var{method})}
    ## also applies @var{method} to each data variable of each group, adding
    ## one variable per method and data variable named for both, as in
    ## @qcode{mean_Speed}.  @var{method} is one of @qcode{'mean'},
    ## @qcode{'sum'}, @qcode{'min'}, @qcode{'max'}, @qcode{'range'},
    ## @qcode{'median'}, @qcode{'mode'}, @qcode{'var'}, @qcode{'std'},
    ## @qcode{'nummissing'}, @qcode{'nnz'} and @qcode{'numunique'}, or
    ## @qcode{'all'} for every one of them in that order, or a function handle,
    ## or a cell array of any of these.  A name may be abbreviated to any
    ## unambiguous prefix.  @qcode{'std'} and @qcode{'var'} are not defined on
    ## an integer variable and refuse one.
    ##
    ## @code{@var{G} = groupsummary (@var{tt}, @var{groupvars}, @var{method},
    ## @var{datavars})} summarises only the variables named in @var{datavars}.
    ## By default every variable that is not a grouping variable is used.
    ##
    ## @code{@var{G} = groupsummary (@var{tt}, @var{groupvars},
    ## @var{groupbins}, @dots{})} bins the grouping variables before grouping,
    ## @var{groupbins} being a bin count, a vector of edges, or a time unit for
    ## a datetime or duration variable.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'IncludeMissingGroups'}
    ## A logical scalar.  When @code{true} (the default) rows whose grouping
    ## value is missing form a group of their own, sorted last.
    ##
    ## @item @qcode{'IncludeEmptyGroups'}
    ## A logical scalar.  When @code{true} the unused categories of a
    ## categorical or binned grouping variable are reported as groups with a
    ## count of zero.  It defaults to @code{false}.
    ##
    ## @item @qcode{'IncludedEdge'}
    ## Which edge of a bin is included, @qcode{'left'} (the default) or
    ## @qcode{'right'}.  It applies only where @var{groupbins} was given.
    ## @end table
    ##
    ## @end deftypefn
    function G = groupsummary (tt, groupvars, varargin)
      if (nargin < 2)
        print_usage ();
      endif
      [G, errmsg] = groupsummaryResult (tt, groupvars, varargin);
      if (! isempty (errmsg))
        error ("timetable.groupsummary: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{G} =} groupfilter (@var{tt}, @var{groupvars}, @var{method})
    ## @deftypefnx {timetable} {@var{G} =} groupfilter (@var{tt}, @var{groupvars}, @var{method}, @var{datavars})
    ## @deftypefnx {timetable} {@var{G} =} groupfilter (@var{tt}, @var{groupvars}, @var{groupbins}, @var{method}, @dots{})
    ## @deftypefnx {timetable} {@var{G} =} groupfilter (@dots{}, @qcode{'IncludedEdge'}, @var{edge})
    ##
    ## Keep the rows of a timetable whose group passes a test.
    ##
    ## @code{@var{G} = groupfilter (@var{tt}, @var{groupvars}, @var{method})}
    ## groups the rows of @var{tt} by the variables named in @var{groupvars},
    ## applies the function handle @var{method} to each data variable of each
    ## group, and returns the rows of the groups it accepted.  @var{method}
    ## receives the values of one variable across the rows of one group and
    ## answers with a logical scalar, keeping or dropping the whole group, or
    ## with one logical per row of the group, keeping those rows.  The rows
    ## that survive keep their order and their row times.
    ##
    ## @var{groupvars} names one or more variables, or the row dimension name,
    ## which groups by the row times themselves.
    ##
    ## @code{@var{G} = groupfilter (@var{tt}, @var{groupvars}, @var{method},
    ## @var{datavars})} applies @var{method} only to the variables named in
    ## @var{datavars}.  By default every variable that is not a grouping
    ## variable is used, and a row is kept only where every one of them
    ## accepts it.
    ##
    ## @code{@var{G} = groupfilter (@var{tt}, @var{groupvars},
    ## @var{groupbins}, @var{method}, @dots{})} bins the grouping variables
    ## before grouping, @var{groupbins} being a bin count, a vector of edges,
    ## or a time unit for a datetime or duration variable.  The
    ## @qcode{'IncludedEdge'} option then says which edge of a bin is
    ## included, @qcode{'left'} (the default) or @qcode{'right'}.
    ##
    ## @end deftypefn
    function G = groupfilter (tt, groupvars, varargin)
      if (nargin < 3)
        print_usage ();
      endif
      [G, errmsg] = groupfilterResult (tt, groupvars, varargin);
      if (! isempty (errmsg))
        error ("timetable.groupfilter: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{tt2} =} stack (@var{tt}, @var{vars})
    ## @deftypefnx {timetable} {@var{tt2} =} stack (@var{tt}, @var{vars}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{tt2}, @var{index}] =} stack (@dots{})
    ##
    ## Stack several variables of a timetable into one.
    ##
    ## @code{@var{tt2} = stack (@var{tt}, @var{vars})} replaces the variables
    ## named in @var{vars} with a single variable holding their values one
    ## under the other, and an indicator variable naming which of them each
    ## value came from.  Every other variable is carried along, repeated once
    ## per stacked variable, and so is the row time of the row it came from:
    ## a timetable of two rows stacking two variables has four rows and two
    ## pairs of equal row times, which generally makes it irregular.
    ##
    ## @var{vars} names the variables to stack.  A cell array of variable
    ## references stacks several groups at once, one new variable per group,
    ## each group holding the same number of variables.
    ##
    ## @code{[@var{tt2}, @var{index}] = stack (@dots{})} also returns
    ## @var{index}, naming the row of @var{tt} each row of @var{tt2} came
    ## from.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'ConstantVariables'}
    ## The variables carried along unstacked.  By default every variable that
    ## is not being stacked is carried.
    ##
    ## @item @qcode{'NewDataVariableName'}
    ## The name of the stacked variable, one per group.  By default the names
    ## of the stacked variables are joined with underscores.
    ##
    ## @item @qcode{'IndexVariableName'}
    ## The name of the indicator variable.  By default it is the name of the
    ## stacked variable followed by @qcode{_Indicator}.
    ## @end table
    ##
    ## @seealso{timetable}
    ## @end deftypefn
    function [tt2, index] = stack (tt, vars, varargin)
      if (nargin < 2)
        vars = [];
      endif
      [tt2, index, errmsg] = stackResult (tt, vars, varargin);
      if (! isempty (errmsg))
        error ("timetable.stack: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{tbl} =} rows2vars (@var{tt})
    ## @deftypefnx {timetable} {@var{tbl} =} rows2vars (@var{tt}, @var{Name}, @var{Value})
    ##
    ## Turn the rows of a timetable into variables.
    ##
    ## @code{@var{tbl} = rows2vars (@var{tt})} returns a @code{table} whose
    ## rows are the variables of @var{tt} and whose variables are its rows.
    ## The first variable, @qcode{OriginalVariableNames}, names the variables
    ## of @var{tt}; the rest are named for the row times they came from,
    ## rendered as the timetable displays them and made into valid names, and
    ## the second dimension of the result takes the name of the row dimension.
    ##
    ## The result is a table and not a timetable: its rows are variables and
    ## there is no longer a time to label them by.
    ##
    ## Where the variables are not all of one type the result holds cell
    ## arrays, every value being wrapped so that one variable can carry them
    ## all.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'DataVariables'}
    ## The variables of @var{tt} that become rows.  By default all of them do.
    ##
    ## @item @qcode{'VariableNamesSource'}
    ## A variable of @var{tt} whose values name the new variables instead of
    ## the row times.  A repeated name is numbered rather than refused, and
    ## the second dimension of the result takes the name of that variable.
    ##
    ## @item @qcode{'VariableNamingRule'}
    ## @qcode{'modify'} (the default) makes each new name a valid identifier;
    ## @qcode{'preserve'} keeps it as it is.
    ## @end table
    ##
    ## @end deftypefn
    function tbl = rows2vars (tt, varargin)
      [tbl, errmsg] = rows2varsResult (tt, varargin);
      if (! isempty (errmsg))
        error ("timetable.rows2vars: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttC} =} join (@var{ttL}, @var{tblR})
    ## @deftypefnx {timetable} {@var{ttC} =} join (@var{ttL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{ttC}, @var{index}] =} join (@dots{})
    ##
    ## Join a timetable with another tabular object.
    ##
    ## @code{@var{ttC} = join (@var{ttL}, @var{tblR})} returns a timetable
    ## with every row of @var{ttL}, its row times kept, and beside it the row
    ## of @var{tblR} whose key matches.  The right operand contributes
    ## variables only, so the result is a timetable exactly as long as
    ## @var{ttL}.
    ##
    ## The key must name a row of @var{tblR} for every row of @var{ttL}, and
    ## the keys of @var{tblR} must be unique.  With no key named, two
    ## timetables join on their row times; a timetable and a table have no
    ## key in common unless one is named, since the row times are not a
    ## variable.
    ##
    ## @code{[@var{ttC}, @var{index}] = join (@dots{})} also returns
    ## @var{index}, naming the row of @var{tblR} each row took.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'Keys'}
    ## The variables to match on, named on both sides.  The row dimension name
    ## names the row times.
    ##
    ## @item @qcode{'LeftKeys'}, @qcode{'RightKeys'}
    ## The variables to match on, named separately for each side and given
    ## together.  They must name the same number of keys.
    ##
    ## @item @qcode{'LeftVariables'}, @qcode{'RightVariables'}
    ## The variables each side contributes.  By default the left contributes
    ## all of its own and the right all but its keys.
    ##
    ## @item @qcode{'KeepOneCopy'}
    ## Variables that both sides carry and only the left contributes.
    ## @end table
    ##
    ## A variable name carried by both sides and contributed by both is
    ## suffixed with the caller's own name for each operand, falling back to
    ## @qcode{_left} and @qcode{_right}.
    ##
    ## @end deftypefn
    function [ttC, index] = join (ttL, tblR, varargin)
      if (nargin < 2)
        error ("timetable.join: too few input arguments.");
      endif
      ## The caller's own names for the operands are read here, before the
      ## shared body, which cannot see them.
      [ttC, index, errmsg] = joinResult (ttL, tblR, varargin, ...
                                         inputname (1), inputname (2));
      if (! isempty (errmsg))
        error ("timetable.join: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttC} =} innerjoin (@var{ttL}, @var{tblR})
    ## @deftypefnx {timetable} {@var{ttC} =} innerjoin (@var{ttL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{ttC}, @var{iL}, @var{iR}] =} innerjoin (@dots{})
    ##
    ## Join a timetable with another tabular object, keeping matched rows.
    ##
    ## @code{@var{ttC} = innerjoin (@var{ttL}, @var{tblR})} returns a
    ## timetable holding one row for every pair of rows of @var{ttL} and
    ## @var{tblR} whose keys match, in key order.  Each row carries the row
    ## time of the row of @var{ttL} it came from, so a key matched more than
    ## once repeats its row time and the result is generally irregular.
    ##
    ## With no key named, two timetables join on their row times; a timetable
    ## and a table have no key in common unless one is named, since the row
    ## times are not a variable.
    ##
    ## @code{[@var{ttC}, @var{iL}, @var{iR}] = innerjoin (@dots{})} also
    ## returns the rows of @var{ttL} and of @var{tblR} each row came from.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'Keys'}
    ## The variables to match on, named on both sides.  The row dimension name
    ## names the row times.
    ##
    ## @item @qcode{'LeftKeys'}, @qcode{'RightKeys'}
    ## The variables to match on, named separately for each side and given
    ## together.
    ##
    ## @item @qcode{'LeftVariables'}, @qcode{'RightVariables'}
    ## The variables each side contributes.  By default the left contributes
    ## all of its own and the right all but its keys.
    ## @end table
    ##
    ## @end deftypefn
    function [ttC, iL, iR] = innerjoin (ttL, tblR, varargin)
      if (nargin < 2)
        error ("timetable.innerjoin: too few input arguments.");
      endif
      [ttC, iL, iR, errmsg] = innerjoinResult (ttL, tblR, varargin, ...
                                               inputname (1), inputname (2));
      if (! isempty (errmsg))
        error ("timetable.innerjoin: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{ttC} =} outerjoin (@var{ttL}, @var{tblR})
    ## @deftypefnx {timetable} {@var{ttC} =} outerjoin (@var{ttL}, @var{tblR}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{ttC}, @var{iL}, @var{iR}] =} outerjoin (@dots{})
    ##
    ## Join a timetable with another tabular object, keeping unmatched rows.
    ##
    ## @code{@var{ttC} = outerjoin (@var{ttL}, @var{tblR})} returns a
    ## timetable holding one row for every pair of rows of @var{ttL} and
    ## @var{tblR} whose keys match, and one for every row of either that
    ## matched nothing, its variables from the other side filled with missing
    ## values.  A row that came from a row of @var{ttL} carries its row time;
    ## a row that matched nothing on the left has none of its own.
    ##
    ## With no key named, two timetables join on their row times; a timetable
    ## and a table have no key in common unless one is named.
    ##
    ## @code{[@var{ttC}, @var{iL}, @var{iR}] = outerjoin (@dots{})} also
    ## returns the rows of @var{ttL} and of @var{tblR} each row came from,
    ## @code{0} where it came from neither.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'Keys'}, @qcode{'LeftKeys'}, @qcode{'RightKeys'}
    ## The variables to match on.  The row dimension name names the row times.
    ##
    ## @item @qcode{'LeftVariables'}, @qcode{'RightVariables'}
    ## The variables each side contributes.  By default each contributes all
    ## of its own, keys included.
    ##
    ## @item @qcode{'Type'}
    ## @qcode{'full'} (the default) keeps the unmatched rows of both sides,
    ## @qcode{'left'} and @qcode{'right'} only those of the side named.
    ##
    ## @item @qcode{'MergeKeys'}
    ## A logical scalar.  When @code{true} each pair of keys becomes a single
    ## variable in the left one's position, taking its value from whichever
    ## side had a row.
    ## @end table
    ##
    ## @end deftypefn
    function [ttC, iL, iR] = outerjoin (ttL, tblR, varargin)
      if (nargin < 2)
        error ("timetable.outerjoin: too few input arguments.");
      endif
      [ttC, iL, iR, errmsg] = outerjoinResult (ttL, tblR, varargin, ...
                                               inputname (1), inputname (2));
      if (! isempty (errmsg))
        error ("timetable.outerjoin: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{tt2} =} inner2outer (@var{tt})
    ##
    ## Turn the nesting of a timetable inside out.
    ##
    ## @code{@var{tt2} = inner2outer (@var{tt})} returns a timetable whose
    ## variables are named for the variables of the tables nested inside
    ## @var{tt}, each holding a nested table named for the variables of
    ## @var{tt} that held them.  The rows and their row times are untouched:
    ## only the nesting is turned inside out.
    ##
    ## An inner variable name held by one nested table alone becomes a plain
    ## variable rather than a nested one.  A timetable holding nothing nested
    ## is returned as it is.
    ##
    ## @seealso{timetable}
    ## @end deftypefn
    function tt2 = inner2outer (tt)
      [tt2, errmsg] = inner2outerResult (tt);
      if (! isempty (errmsg))
        error ("timetable.inner2outer: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{G} =} findgroups (@var{tt})
    ## @deftypefnx {timetable} {[@var{G}, @var{TID}] =} findgroups (@var{tt})
    ##
    ## Number the groups the rows of a timetable fall into.
    ##
    ## @code{@var{G} = findgroups (@var{tt})} returns a column of group
    ## numbers, one per row of @var{tt}, numbering the distinct combinations
    ## of its variables in sorted order.  A row holding a missing value in any
    ## variable belongs to no group and is numbered @code{NaN}.
    ##
    ## The row times take no part: a timetable groups by its variables, as a
    ## table does.
    ##
    ## @code{[@var{G}, @var{TID}] = findgroups (@var{tt})} also returns a
    ## @code{table} with one row per group, holding the combination of values
    ## that defines it.  It is a table and not a timetable, its rows being
    ## groups rather than instants.
    ##
    ## @seealso{splitapply, timetable}
    ## @end deftypefn
    function [G, TID] = findgroups (tt)
      if (nargin != 1)
        print_usage ();
      endif
      [G, TID, errmsg] = findgroupsResult (tt);
      if (! isempty (errmsg))
        error ("timetable.findgroups: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{Y} =} splitapply (@var{func}, @var{tt}, @var{G})
    ## @deftypefnx {timetable} {[@var{Y1}, @dots{}] =} splitapply (@var{func}, @var{tt}, @var{G})
    ##
    ## Apply a function to each group of rows of a timetable.
    ##
    ## @code{@var{Y} = splitapply (@var{func}, @var{tt}, @var{G})} splits the
    ## rows of @var{tt} into the groups the numbers in @var{G} name, calls
    ## @var{func} once per group with one argument per variable holding that
    ## group's rows, and stacks the results.  @var{G} holds one number per row
    ## of @var{tt}, as @code{findgroups} returns; a row numbered @code{NaN}
    ## belongs to no group and is left out.
    ##
    ## The row times take no part: @var{func} receives the variables alone.
    ##
    ## @code{[@var{Y1}, @dots{}] = splitapply (@dots{})} asks @var{func} for as
    ## many outputs as are requested and stacks each of them.
    ##
    ## @seealso{findgroups, timetable}
    ## @end deftypefn
    function varargout = splitapply (func, tt, G)
      if (nargin != 3)
        print_usage ();
      endif
      nout = max (nargout, 1);
      [results, N, errmsg] = splitapplyResult (tt, func, G, nout);
      if (! isempty (errmsg))
        error ("timetable.splitapply: %s", errmsg);
      endif
      varargout = cell (1, nout);
      for k = 1:nout
        varargout{k} = vertcat (results{:,k});
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{tt2} =} unstack (@var{tt}, @var{vars}, @var{ivar})
    ## @deftypefnx {timetable} {@var{tt2} =} unstack (@var{tt}, @var{vars}, @var{ivar}, @var{Name}, @var{Value})
    ## @deftypefnx {timetable} {[@var{tt2}, @var{index}] =} unstack (@dots{})
    ##
    ## Spread one variable of a timetable across several.
    ##
    ## @code{@var{tt2} = unstack (@var{tt}, @var{vars}, @var{ivar})} replaces
    ## the variable named in @var{vars} with one variable per distinct value
    ## of the indicator variable @var{ivar}, each holding the values that
    ## carried that indicator.  Rows sharing a group become one row.
    ##
    ## The row times group: with no @qcode{'GroupingVariables'} given, one row
    ## comes back per distinct row time, so @var{tt2} is a timetable carrying
    ## those times.  A group that has no value for an indicator is filled with
    ## a missing value.
    ##
    ## @code{[@var{tt2}, @var{index}] = unstack (@dots{})} also returns
    ## @var{index}, naming a row of @var{tt} in each group.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'GroupingVariables'}
    ## The variables whose combinations define the rows of the result,
    ## alongside the row times.
    ##
    ## @item @qcode{'ConstantVariables'}
    ## The variables carried along unchanged, taken from one row of each group.
    ##
    ## @item @qcode{'NewDataVariableNames'}
    ## The names of the new variables, one per distinct indicator value.
    ##
    ## @item @qcode{'AggregationFunction'}
    ## The function applied where a group holds several values for one
    ## indicator.  By default numeric data are summed and everything else must
    ## be unique within its group.
    ##
    ## @item @qcode{'VariableNamingRule'}
    ## @qcode{'modify'} (the default) makes each new name a valid identifier;
    ## @qcode{'preserve'} keeps it as it is.
    ## @end table
    ##
    ## @seealso{stack, timetable}
    ## @end deftypefn
    function [tt2, index] = unstack (tt, vars, ivar, varargin)
      if (nargin < 3)
        vars = [];
        ivar = [];
      endif
      [tt2, index, errmsg] = unstackResult (tt, vars, ivar, varargin);
      if (! isempty (errmsg))
        error ("timetable.unstack: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timetable} {@var{P} =} pivot (@var{tt}, @var{Name}, @var{Value}, @dots{})
    ##
    ## Summarise a timetable across two grouping dimensions.
    ##
    ## @code{@var{P} = pivot (@var{tt}, @qcode{'Rows'}, @var{rowvars},
    ## @qcode{'Columns'}, @var{colvars})} groups the rows of @var{tt} by
    ## @var{rowvars} down the page and by @var{colvars} across it, and returns
    ## a @code{table} holding one row per row group and one variable per
    ## column group.  At least one of the two must be given.
    ##
    ## Either may name the row dimension, which groups by the row times: as
    ## @qcode{'Rows'} they become a variable of the result, and as
    ## @qcode{'Columns'} they name its variables.  The result is a table and
    ## not a timetable, its rows and columns being groups rather than
    ## instants.
    ##
    ## With no @qcode{'DataVariable'} the cells count the rows of each group;
    ## with one they summarise its values, by @qcode{'Method'}, which defaults
    ## to summing them.
    ##
    ## The following @var{Name}/@var{Value} pairs are accepted:
    ##
    ## @table @asis
    ## @item @qcode{'Rows'}, @qcode{'Columns'}
    ## The variables grouping down the page and across it, or the row
    ## dimension name.
    ##
    ## @item @qcode{'DataVariable'}
    ## The variable summarised in each cell.
    ##
    ## @item @qcode{'Method'}
    ## The summary applied to each cell, a method name or a function handle.
    ##
    ## @item @qcode{'RowsBinMethod'}, @qcode{'ColumnsBinMethod'}
    ## Binning applied to the grouping variables before grouping, with
    ## @qcode{'IncludedEdge'} saying which edge of a bin is included.
    ##
    ## @item @qcode{'IncludeMissingGroups'}, @qcode{'IncludeEmptyGroups'}
    ## Whether a group of missing values, and whether an unused category, are
    ## reported.
    ##
    ## @item @qcode{'IncludeTotals'}
    ## Whether a marginal row and column are added, labelled
    ## @qcode{Overall_<method>}.
    ##
    ## @item @qcode{'RowLabelPlacement'}
    ## Whether the row groups become a variable of the result or its row
    ## names.
    ##
    ## @item @qcode{'OutputFormat'}
    ## @qcode{'flat'} (the default) or @qcode{'nested'}, which groups the data
    ## variables into nested tables by the column hierarchy.
    ## @end table
    ##
    ## @seealso{groupsummary, timetable}
    ## @end deftypefn
    function P = pivot (tt, varargin)
      [P, errmsg] = pivotResult (tt, varargin);
      if (! isempty (errmsg))
        error ("timetable.pivot: %s", errmsg);
      endif
    endfunction

  endmethods

  methods (Static)

    ## -*- texinfo -*-
    ## @deftypefn  {timetable} {@var{tt} =} timetable.empty ()
    ## @deftypefnx {timetable} {@var{tt} =} timetable.empty (@var{n})
    ## @deftypefnx {timetable} {@var{tt} =} timetable.empty (@var{r}, @var{v})
    ## @deftypefnx {timetable} {@var{tt} =} timetable.empty (@var{sz})
    ##
    ## Create an empty timetable.
    ##
    ## @code{@var{tt} = timetable.empty ()} returns a 0-by-0 timetable.
    ##
    ## @code{@var{tt} = timetable.empty (@var{r}, @var{v})} returns a
    ## timetable with @var{r} rows and @var{v} variables, at least one of
    ## which must be zero.  The row times are @qcode{NaT}, one per row, since
    ## a row of a timetable is labelled whether or not it holds anything.  A
    ## timetable with variables but no rows names them @qcode{Var1} to
    ## @qcode{VarN} and gives each of them a @qcode{double} value.
    ##
    ## @code{@var{tt} = timetable.empty (@var{sz})} takes the two dimensions
    ## from the two-element vector @var{sz}, and
    ## @code{timetable.empty (@var{n})} is the same as
    ## @code{timetable.empty (@var{n}, @var{n})}.
    ##
    ## @seealso{timetable, table, isempty, height, width}
    ## @end deftypefn
    function tt = empty (varargin)
      [sz, errmsg] = tabular.emptySize ('timetable', varargin);
      if (! isempty (errmsg))
        error ('timetable.empty: %s', errmsg);
      endif
      tt = timetable ('Size', sz, 'VariableTypes', ...
                      repmat ({'double'}, 1, sz(2)), ...
                      'RowTimes', NaT (sz(1), 1));
    endfunction

  endmethods

endclassdef

## Whether an optional argument was given at all.  The parser reports an
## absent one as its default, and for the row times an empty value is a
## legitimate thing to give, a timetable with no rows being built from an
## empty vector.  A 'missing' is the sentinel because none of the arguments
## it stands in for can ever be one.
## Resolve the second argument of a range predicate into the interval it
## describes, and into the rows of the timetable that fall in it.  All three
## predicates need the same values and differ only in how they compare that
## interval with the span of the row times, so the reading is done once here.
## 'valid' is false where the reference names no instant at all, which makes
## every predicate false rather than vacuously true.
function [R, errmsg] = rangeRef (ref, rowTimes)

  R = struct ('valid', false, 'lo', [], 'hi', [], 'hasLo', false, ...
              'hasHi', false, 'closedLeft', true, 'closedRight', true, ...
              'whichRows', false (numel (rowTimes), 1));
  errmsg = '';

  ## A range knows its own bounds and its own closure, and it is the range
  ## that refuses a bound of the wrong kind, exactly as it does when it is
  ## used as a subscript.
  if (isa (ref, 'timerange'))
    [lo, hi, cl, cr] = interval (ref, rowTimes);
    R.valid = true;
    R.lo = lo;
    R.hi = hi;
    R.hasLo = ! isnumeric (lo);
    R.hasHi = ! isnumeric (hi);
    R.closedLeft = cl;
    R.closedRight = cr;
    R.whichRows(rowIndices (ref, rowTimes)) = true;
    return
  endif

  if (isa (ref, 'timetable'))
    other = ref.Properties.RowTimes;
    if (! strcmp (class (other), class (rowTimes)))
      errmsg = sprintf (strcat ("a timetable with %s row times cannot be", ...
                                " compared against one with %s row times."), ...
                        class (rowTimes), class (other));
      return
    endif
    [lo, hi, ok] = timeSpan (other);
    if (! ok)
      return
    endif
  elseif ((isdatetime (ref) || isduration (ref)) && isscalar (ref))
    if (! strcmp (class (ref), class (rowTimes)))
      errmsg = sprintf (strcat ("a timetable with %s row times cannot be", ...
                                " compared against a %s."), ...
                        class (rowTimes), class (ref));
      return
    endif
    if (ismissing (ref))
      return
    endif
    lo = ref;
    hi = ref;
  else
    errmsg = strcat ("REF must be a timetable, a timerange, or a datetime", ...
                     " or duration scalar.");
    return
  endif

  ## Both remaining forms name a closed interval, so the defaults stand.
  R.valid = true;
  R.lo = lo;
  R.hi = hi;
  R.hasLo = true;
  R.hasHi = true;
  R.whichRows = rowTimes >= lo & rowTimes <= hi;

endfunction

## The closed interval the row times themselves span.  Missing times are not
## part of it, and a timetable left with none has no span to compare.
function [lo, hi, ok] = timeSpan (rowTimes)

  lo = [];
  hi = [];
  keep = ! ismissing (rowTimes);
  ok = any (keep);
  if (! ok)
    return
  endif
  rt = rowTimes(keep);
  lo = min (rt);
  hi = max (rt);

endfunction

## Compare one end of a span against one bound, letting the bound say whether
## standing on it counts.  'above' asks whether T reaches the lower bound B,
## 'below' whether it stays under the upper one.
function tf = boundMet (t, b, closed, side)

  if (strcmp (side, 'above'))
    if (closed)
      tf = t >= b;
    else
      tf = t > b;
    endif
  else
    if (closed)
      tf = t <= b;
    else
      tf = t < b;
    endif
  endif

endfunction

function tf = wasGiven (x)
  tf = ! isa (x, 'missing');
endfunction

## The default value of a column of NR rows of the named type, used by the
## preallocating constructor form.  Kept beside the constructor rather than
## shared with 'table', whose own list differs: a timetable cannot hold a
## timetable variable and says so with its own scope.
function col = defaultColumn (vartype, nr)
  switch (vartype)
    case {'double', 'single', 'int8', 'uint8', 'int16', 'uint16', ...
          'int32', 'uint32', 'int64', 'uint64'}
      col = zeros (nr, 1, vartype);
    case {'doublenan', 'doubleNaN'}
      col = NaN (nr, 1, 'double');
    case {'singlenan', 'singleNaN'}
      col = NaN (nr, 1, 'single');
    case 'logical'
      col = logical (zeros (nr, 1));
    case 'categorical'
      col = categorical (NaN (nr, 1));
    case 'datetime'
      col = NaT (nr, 1);
    case 'duration'
      col = seconds (zeros (nr, 1));
    case 'calendarDuration'
      col = calendarDuration (zeros (nr, 3));
    case 'string'
      col = string (NaN (nr, 1));
    case {'cellstr', 'char'}
      col = repmat (cellstr (""), nr, 1);
    case 'cell'
      col = cell (nr, 1);
    case 'struct'
      col = repmat (struct, nr, 1);
    case 'table'
      col = table ([]);
    case 'timetable'
      error ("timetable: 'timetable' variable type not supported yet.");
    otherwise
      error ("timetable: unsupported variable type: '%s'", vartype);
  endswitch
endfunction

## Validate a row time vector of N rows and return it as a column.  N may be
## empty, which asks only that the vector be of a usable type.
function rt = checkRowTimes (rt, n)
  if (! (isdatetime (rt) || isduration (rt)))
    error (strcat ("timetable: 'RowTimes' must be a datetime or a", ...
                   " duration vector."));
  endif
  if (! isvector (rt) && ! isempty (rt))
    error ("timetable: 'RowTimes' must be a vector.");
  endif
  if (! isempty (n) && numel (rt) != n)
    error (strcat ("timetable: the number of 'RowTimes' (%d) must equal", ...
                   " the number of rows (%d)."), numel (rt), n);
  endif
  rt = rt(:);
endfunction

## Validate a start time, a datetime or duration scalar.  It need not match
## the type the row times already have: assigning one of the other kind
## re-types them, which is how an elapsed-time timetable is given a date to
## have begun on.
function st = checkStartTime (st)
  if (! (isdatetime (st) || isduration (st)))
    error (strcat ("timetable: 'StartTime' must be a datetime or a", ...
                   " duration scalar."));
  endif
  if (! isscalar (st))
    error ("timetable: 'StartTime' must be a scalar.");
  endif
endfunction

## Validate a time step against the start time it will be counted from.  A
## calendar step has no fixed length, so it can only be counted on a
## calendar and needs a datetime to count from.
function ts = checkTimeStep (ts, st)
  if (! (isduration (ts) || iscalendarduration (ts)))
    error (strcat ("timetable: 'TimeStep' must be a duration or a", ...
                   " calendarDuration scalar."));
  endif
  if (! isscalar (ts))
    error ("timetable: 'TimeStep' must be a scalar.");
  endif
  if (iscalendarduration (ts) && ! isdatetime (st))
    error (strcat ("timetable: 'StartTime' must be a datetime when", ...
                   " 'TimeStep' is a calendarDuration."));
  endif
  if (iscalendarduration (ts))
    dv = datevec (ts);
    if (any (dv(4:6)) || ((dv(1) != 0 || dv(2) != 0) && dv(3) != 0))
      error (strcat ("timetable: a calendarDuration 'TimeStep' must name", ...
                     " a single calendar unit; use 'caldays', 'calweeks',", ...
                     " 'calmonths', 'calquarters' or 'calyears'."));
    endif
  endif
endfunction

## Validate a sample rate, a real numeric scalar in hertz.  Neither the sign
## nor the magnitude is constrained: a negative rate steps backwards, which
## is as regular as stepping forwards, and the degenerate rates simply carry
## their arithmetic into the row times, a rate of zero giving an infinite
## step and an infinite rate giving no step at all.
function fs = checkSampleRate (fs)
  if (! (isnumeric (fs) && isscalar (fs) && isreal (fs)))
    error ("timetable: 'SampleRate' must be a numeric scalar.");
  endif
endfunction

## The start time a generated timetable takes when none was given: a zero of
## the step's own kind, so that the row times read as elapsed time in the
## units the step was written in.  A calendar step has no such zero and is
## refused before this is reached.
function st = defaultStart (ts)
  st = seconds (0);
  if (isduration (ts))
    ## A zero of the step's own units, taken by copying its format rather
    ## than by subtracting it from itself, which would be NaN for a step
    ## that is not finite.
    st.Format = ts.Format;
  endif
endfunction

## N row times counted from ST in steps of TS.
function rt = steppedTimes (st, ts, n)
  if (n == 0)
    rt = st([]);
  else
    rt = st + ts * (0:n-1)';
    ## The first row time is the start time itself.  Reaching it as
    ## 'start + step * 0' costs nothing until the step is not finite, where
    ## 'Inf * 0' is NaN and would poison a row that has no step in it.
    rt(1) = st;
  endif
endfunction

## N missing row times of the same type as RT, which is what the rows of a
## plain table are labelled with when one is stacked onto a timetable.
function m = missingTimes (rt, n)
  m = repmat (startOf (rt([])), [n, 1]);
endfunction

## The first of a vector of row times, or a missing one of the same type
## when the vector has none to take.
function st = startOf (rt)
  if (isempty (rt))
    if (isdatetime (rt))
      st = NaT;
      st.TimeZone = rt.TimeZone;
    else
      st = seconds (NaN);
    endif
  else
    st = rt(1);
  endif
endfunction

## The time step implied by a vector of row times, and the sample rate that
## goes with it.  Fewer than two rows imply nothing, and neither does a
## vector carrying a missing time, a repeated time or an uneven gap: all of
## them give a NaN duration, which is what marks a timetable as irregular.
##
## A calendar step is preferred wherever there is one, and the two halves of
## the calendar are inferred on different terms.  Months, quarters and years
## have no fixed length in seconds, so an even calendar spacing is all they
## can be asked for.  Days and weeks are meant to be absolute, so they must
## be even in absolute time as well: a run of calendar days interrupted by a
## clock change contains a day of 23 hours and is not regular at all.
##
## Failing that, a vector evenly spaced in absolute time gives a duration
## step, keeping the format the difference came out in.
function [ts, fs] = stepOf (rt)
  ts = seconds (NaN);
  if (numel (rt) >= 2 && ! any (ismissing (rt)))
    d = diff (rt);
    ds = seconds (d);
    absReg = (ds(1) != 0 && all (ds == ds(1)));
    cd = calendarStepOf (rt);
    if (! isempty (cd) && (calendarMonths (cd) != 0 || absReg))
      ts = stepUnit (cd);
    elseif (absReg)
      ts = d(1);
    endif
  endif
  fs = stepRate (ts);
endfunction

## The constant calendar difference of a vector of row times, or empty when
## it has none.  Duration row times carry no calendar and never have one.
##
## A calendar step names one calendar unit and no time of day.  An hourly
## run, a run of days-plus-two-hours and a run of months-plus-a-day are each
## evenly spaced on the calendar in a sense, and none of them is a calendar
## step; the last is refused for an explicit 'TimeStep' too.
function cd = calendarStepOf (rt)
  cd = [];
  if (! isdatetime (rt) || numel (rt) < 2 || any (ismissing (rt)))
    return
  endif
  c = caldiff (rt);
  dv = datevec (c);
  if (all (all (dv == dv(1,:))) && any (dv(1,1:3)) && ! any (dv(1,4:6))
      && ! ((dv(1,1) != 0 || dv(1,2) != 0) && dv(1,3) != 0))
    cd = c(1);
  endif
endfunction

## A calendar step written in the coarsest whole unit it fills.  'caldiff'
## reports in years, months and days, so a weekly run comes back as '7d' and
## a quarterly one as '3mo'; a step names its own unit instead.  The value is
## the same either way and only the unit it is written in changes.
function cd = stepUnit (cd)
  dv = datevec (cd);
  months = dv(1) * 12 + dv(2);
  if (months == 0 && dv(3) != 0 && mod (dv(3), 7) == 0)
    cd = calweeks (dv(3) / 7);
  elseif (dv(3) == 0 && months != 0 && mod (months, 3) == 0
          && mod (months, 12) != 0)
    cd = calquarters (months / 3);
  endif
endfunction

## A calendar step counted in whole months, years and quarters included.
function m = calendarMonths (cd)
  dv = datevec (cd);
  m = dv(1) * 12 + dv(2);
endfunction

## The sample rate of a time step: rows per second, and NaN for a calendar
## step, which has no fixed length in seconds to take a reciprocal of.  A
## NaN step gives a NaN rate by the same arithmetic.
## The format of a declared step carried onto a step recomputed from the row
## times.  Only a duration has one to carry, and a calendar step that gave way
## to a duration, or the reverse, keeps whatever the new value came with.
function step = carryStepFormat (oldStep, step)
  if (isa (oldStep, 'duration') && isa (step, 'duration'))
    step.Format = oldStep.Format;
  endif
endfunction

function fs = stepRate (ts)
  if (iscalendarduration (ts))
    fs = NaN;
  else
    fs = 1 / seconds (ts);
  endif
endfunction

## Whether a calendar step is a whole, nonzero number of UNIT.  The two
## families do not mix: a step in days is a whole number of days and of
## weeks but of no months, and a step in months is a whole number of months,
## quarters or years but of no days.
function tf = unitDivides (unit, cd)
  tf = false;
  if (isempty (cd))
    return
  endif
  dv = datevec (cd);
  months = dv(1) * 12 + dv(2);
  days = dv(3);
  switch (unit)
    case 'days'
      tf = months == 0 && days != 0;
    case 'weeks'
      tf = months == 0 && days != 0 && mod (days, 7) == 0;
    case 'months'
      tf = days == 0 && months != 0;
    case 'quarters'
      tf = days == 0 && months != 0 && mod (months, 3) == 0;
    case 'years'
      tf = days == 0 && months != 0 && mod (months, 12) == 0;
  endswitch
endfunction

## A row time reference converted to the type of the row times it will be
## matched against, so that a date may be given as text.
function ref = rowRefTimes (rowRef, rt)
  if (isdatetime (rowRef) || isduration (rowRef))
    ref = rowRef(:);
    if (! strcmp (class (ref), class (rt)))
      error (strcat ("timetable: a row time subscript must be a %s, to", ...
                     " match the row times."), class (rt));
    endif
    return
  endif
  txt = cellstr (rowRef);
  try
    if (isdatetime (rt))
      ref = datetime (txt);
      ref.TimeZone = rt.TimeZone;
    else
      ref = duration (txt);
    endif
  catch
    error (strcat ("timetable: a row subscript could not be read as a", ...
                   " %s: '%s'"), class (rt), strjoin (txt, ", "));
  end_try_catch
  ref = ref(:);
endfunction
