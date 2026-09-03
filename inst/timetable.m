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
  ## @qcode{NaN} time step.  @qcode{TimeStep} is remembered rather than
  ## recomputed on demand, so a single row taken out of an hourly timetable
  ## still knows that it came from one.
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
    ## timetable is irregular.  It is stored rather than recomputed, so a
    ## subset too short to imply a step keeps the one it was taken from,
    ## while a freshly built one-row timetable has none.  A negative step is
    ## as regular as a positive one.  Assigning it regenerates the row times
    ## from @qcode{StartTime}, even when the timetable was irregular.
    ##
    ## @end deftp
    TimeStep = []

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
          this = applyRowTimes (this, val, true);

        case 'StartTime'
          val = checkStartTime (val);
          this = applyRowTimes (this, reanchored (this, val), false);

        case 'TimeStep'
          val = checkTimeStep (val, this.StartTime);
          rt = steppedTimes (this.StartTime, val, height (this));
          this = applyRowTimes (this, rt, true, val);

        case 'SampleRate'
          val = seconds (1 / checkSampleRate (val));
          rt = steppedTimes (this.StartTime, val, height (this));
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
    ## nothing, and there the stored step is kept, which is how a single row
    ## taken out of an hourly timetable stays hourly (§1.3).
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
      this.RowTimes = rt(:);
      if (inferStep)
        [this.TimeStep, this.SampleRate] = stepOf (this.RowTimes);
        if (nargin > 3 && (numel (this.RowTimes) < 2
                           || ! any (ismissing (this.TimeStep))))
          this.TimeStep = givenStep;
          this.SampleRate = stepRate (givenStep);
        endif
      endif
      if (! isempty (this.RowTimes))
        this.StartTime = this.RowTimes(1);
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
## 'ndims'            'length'           'isempty'                           ##
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
    ## an hourly timetable is still regular in time.
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

endclassdef

## Whether an optional argument was given at all.  The parser reports an
## absent one as its default, and for the row times an empty value is a
## legitimate thing to give, a timetable with no rows being built from an
## empty vector.  A 'missing' is the sentinel because none of the arguments
## it stands in for can ever be one.
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
      ts = cd;
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

## A calendar step counted in whole months, years and quarters included.
function m = calendarMonths (cd)
  dv = datevec (cd);
  m = dv(1) * 12 + dv(2);
endfunction

## The sample rate of a time step: rows per second, and NaN for a calendar
## step, which has no fixed length in seconds to take a reciprocal of.  A
## NaN step gives a NaN rate by the same arithmetic.
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
