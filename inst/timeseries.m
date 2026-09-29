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

classdef timeseries
  ## -*- texinfo -*-
  ## @deftp {datatypes} timeseries
  ##
  ## Array of samples indexed by time.
  ##
  ## A @code{timeseries} object holds a sequence of samples, each taken at a
  ## time on a common time vector, together with the metadata that describes
  ## them: the units of the data and of the time, an optional absolute start
  ## date, the method of evaluating the series between samples, a quality code
  ## per sample, and named events.
  ##
  ## Each sample may be a scalar, a vector, or an array.  For data of two
  ## dimensions or fewer each row is one sample, and the time vector runs down
  ## the first dimension; for data of three dimensions or more the time vector
  ## runs along the last dimension.  @qcode{IsTimeFirst} reports which.
  ##
  ## @code{timeseries} is a value class: every method returns a modified copy
  ## and leaves its input unchanged.  An array of @code{timeseries} objects is
  ## built by concatenation, as @code{[@var{ts1}, @var{ts2}]}.
  ##
  ## MATLAB recommends @code{timetable} for new code; @code{timeseries} is
  ## provided for code that exchanges it.
  ##
  ## Property names are case-sensitive in dot indexing, as for every other
  ## class; MATLAB also reads @code{@var{ts}.name} for
  ## @code{@var{ts}.Name}.  @code{get} and @code{set} accept any case.
  ##
  ## @seealso{timetable, tsdata.timemetadata, tsdata.datametadata,
  ## tsdata.qualmetadata, tsdata.event, tsdata.interpolation}
  ## @end deftp

  properties (Dependent)
    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} Events
    ##
    ## The events of the series.
    ##
    ## A vector of @code{tsdata.event} objects, or @code{[]} when there are
    ## none, by default.
    ##
    ## @end deftp
    Events

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} Name
    ##
    ## The name of the series.
    ##
    ## A character vector, @qcode{'unnamed'} by default, and @qcode{''} for a
    ## series created with no argument.  A string scalar is converted; any
    ## other value is refused, where MATLAB accepts it.
    ##
    ## @end deftp
    Name

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} UserData
    ##
    ## Any data the user attaches, @code{[]} by default.
    ##
    ## @end deftp
    UserData

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} Data
    ##
    ## The samples.
    ##
    ## A numeric or logical array of any class, holding one sample per time.
    ## For data of two dimensions or fewer each row is a sample; for data of
    ## three dimensions or more each slice along the last dimension is one.
    ## A row vector assigned to a series of as many times is taken as that
    ## many scalar samples and stored as a @code{1x1xN} array.  Any other
    ## number of samples than there are times is refused, except while one of
    ## the two is empty, which lets a series be built by assigning @qcode{Time}
    ## and @qcode{Data} in turn.
    ##
    ## @end deftp
    Data

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} DataInfo
    ##
    ## The metadata of the data, a @code{tsdata.datametadata} object.
    ##
    ## @end deftp
    DataInfo

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} Time
    ##
    ## The time vector.
    ##
    ## A column vector of finite, non-decreasing times, in
    ## @qcode{@var{ts}.TimeInfo.Units}, counted from
    ## @qcode{@var{ts}.TimeInfo.StartDate} when that is set.  Repeated times
    ## are allowed.  An unsorted time vector is refused on assignment, since
    ## sorting it would move samples it was not given with.  It is stored as
    ## @code{double}: MATLAB keeps an integer class, and then rounds new times
    ## to it.
    ##
    ## @end deftp
    Time

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} TimeInfo
    ##
    ## The metadata of the time vector, a @code{tsdata.timemetadata} object.
    ## Its @qcode{Length}, @qcode{Start}, @qcode{End} and @qcode{Increment}
    ## follow @qcode{Time} and cannot be assigned.
    ##
    ## @end deftp
    TimeInfo

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} Quality
    ##
    ## A quality code per sample.
    ##
    ## Integers from -128 to 127, stored as @code{double}, @code{[]} by
    ## default.  Either one code per sample, as a vector, or one per data
    ## element, as an array the size of @qcode{Data}.  Their meaning is listed
    ## in @qcode{QualityInfo}.
    ##
    ## @end deftp
    Quality

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} QualityInfo
    ##
    ## The description of the quality codes, a @code{tsdata.qualmetadata}
    ## object.
    ##
    ## @end deftp
    QualityInfo

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} IsTimeFirst
    ##
    ## Whether the time vector runs along the first dimension of the data.
    ##
    ## @code{true} for data of two dimensions or fewer and @code{false} for
    ## data of three dimensions or more.  It follows the data: assigning the
    ## value it already has is accepted, and assigning the other is refused,
    ## since it would misalign the data with the time vector.
    ##
    ## @end deftp
    IsTimeFirst

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} TreatNaNasMissing
    ##
    ## Whether the statistics methods treat @code{NaN} as missing data.
    ##
    ## A logical scalar, @code{true} by default; 0 and 1 are converted.
    ##
    ## @end deftp
    TreatNaNasMissing

    ## -*- texinfo -*-
    ## @deftp {timeseries} {property} Length
    ##
    ## The number of samples, which is the length of the time vector.
    ## Read-only.
    ##
    ## @end deftp
    Length
  endproperties

  properties (Access = private)
    events_ = []
    name_ = 'unnamed'
    userData_ = []
    data_ = []
    dataInfo_ = tsdata.datametadata ()
    time_ = zeros (0, 1)
    timeInfo_ = tsdata.timemetadata ()
    quality_ = []
    qualityInfo_ = tsdata.qualmetadata ()
    treatNaN_ = true
  endproperties

  methods (Hidden)

    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      if (! isscalar (this))
        disp (this);
        return;
      endif
      fprintf ("\n  timeseries\n\n  Common Properties:\n");
      names = {'Name', 'Time', 'TimeInfo', 'Data', 'DataInfo'};
      for i = 1:numel (names)
        fprintf ("%16s: %s\n", names{i}, ...
                 timeseries.dispValue (this.(names{i})));
      endfor
      fprintf ("\n");
    endfunction

    function disp (this)
      if (! isscalar (this))
        fprintf ("  %s timeseries array\n\n", sizestr (this));
        return;
      endif
      fprintf ("\n  timeseries with properties:\n\n");
      names = properties (this);
      for i = 1:numel (names)
        fprintf ("%21s: %s\n", names{i}, ...
                 timeseries.dispValue (this.(names{i})));
      endfor
      fprintf ("\n");
    endfunction

  endmethods

  methods (Static, Hidden)

    ## The body of 'get' for 'timeseries' and the 'tsdata' classes.  OBJ is
    ## the object or array, SCOPE the name errors are raised under, and the
    ## remaining argument, if any, the property name or names.
    function out = propertyGet (obj, scope, varargin)
      if (numel (varargin) > 1)
        error ("%s: too many input arguments.", scope);
      endif
      allNames = properties (class (obj));
      if (isempty (varargin))
        if (isscalar (obj))
          out = struct ();
          for i = 1:numel (allNames)
            out.(allNames{i}) = obj.(allNames{i});
          endfor
        else
          out = cell (numel (obj), numel (allNames));
          for k = 1:numel (obj)
            for i = 1:numel (allNames)
              out{k,i} = obj(k).(allNames{i});
            endfor
          endfor
        endif
        return;
      endif
      names = varargin{1};
      isList = iscell (names) || (isstring (names) && ! isscalar (names));
      if (isstring (names))
        names = cellstr (names);
      endif
      if (ischar (names) && isrow (names))
        names = {names};
      endif
      if (! iscellstr (names))
        error (strcat ("%s: NAME must be a character vector or a cell", ...
                       " array of character vectors."), scope);
      endif
      for i = 1:numel (names)
        [names{i}, errmsg] = propertyName (names{i}, allNames);
        if (! isempty (errmsg))
          error ("%s: %s", scope, errmsg);
        endif
      endfor
      out = cell (numel (obj), numel (names));
      for k = 1:numel (obj)
        for i = 1:numel (names)
          out{k,i} = obj(k).(names{i});
        endfor
      endfor
      if (! isList)
        if (isscalar (obj))
          out = out{1};
        else
          out = reshape (out, size (obj));
        endif
      endif
    endfunction

    ## The body of 'set' for 'timeseries' and the 'tsdata' classes: returns
    ## OBJ with the property name and value pairs in VARARGIN set on every
    ## element.  Writing the result back to the caller stays with each
    ## class's 'set', since 'inputname' names the arguments of its caller.
    function obj = propertySet (obj, scope, varargin)
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      allNames = properties (class (obj));
      for i = 1:2:numel (varargin)
        name = varargin{i};
        if (! isText (name))
          error ("%s: NAME must be a character vector.", scope);
        endif
        [name, errmsg] = propertyName (char (name), allNames);
        if (! isempty (errmsg))
          error ("%s: %s", scope, errmsg);
        endif
        for k = 1:numel (obj)
          obj(k).(name) = varargin{i+1};
        endfor
      endfor
    endfunction

    ## A property value as the property listings of 'timeseries' and the
    ## 'tsdata' classes show it: text quoted, a scalar written out, anything
    ## else summarized by its size and class.
    function str = dispValue (val)
      if (ischar (val) && (isrow (val) || isempty (val)))
        str = sprintf ("'%s'", val);
      elseif (isempty (val) && (isnumeric (val) || islogical (val)))
        if (isequal (size (val), [0, 0]))
          str = '[]';
        else
          str = sprintf ("[%s %s]", sizestr (val), class (val));
        endif
      elseif ((isnumeric (val) || islogical (val)) && isscalar (val))
        str = num2str (val);
      elseif (isa (val, 'function_handle'))
        str = func2str (val);
        if (str(1) != '@')
          str = ['@', str];
        endif
      elseif (iscell (val))
        str = sprintf ("{%s cell}", sizestr (val));
      else
        str = sprintf ("[%s %s]", sizestr (val), class (val));
      endif
    endfunction

  endmethods

################################################################################
##                   ** Create and describe 'timeseries' **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'timeseries'       'get'              'set'                                ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} timeseries ()
    ## @deftypefnx {timeseries} {@var{ts} =} timeseries (@var{name})
    ## @deftypefnx {timeseries} {@var{ts} =} timeseries (@var{data})
    ## @deftypefnx {timeseries} {@var{ts} =} timeseries (@var{data}, @var{time})
    ## @deftypefnx {timeseries} {@var{ts} =} timeseries (@var{data}, @var{time}, @var{quality})
    ## @deftypefnx {timeseries} {@var{ts} =} timeseries (@dots{}, @qcode{'Name'}, @var{name})
    ##
    ## Create a @code{timeseries} object.
    ##
    ## @code{@var{ts} = timeseries ()} returns an empty series with no name.
    ##
    ## @code{@var{ts} = timeseries (@var{name})} returns an empty series named
    ## @var{name}, a character vector or a string scalar.  MATLAB takes a
    ## string scalar here as the data of a one-sample series.
    ##
    ## @code{@var{ts} = timeseries (@var{data})} returns a series of the
    ## samples in @var{data}, a numeric or logical array, at times 0, 1, 2,
    ## @dots{} seconds.  For data of two dimensions or fewer each row is a
    ## sample; for data of three dimensions or more each slice along the last
    ## dimension is one.  A row vector of more than one element is taken as
    ## that many scalar samples and stored as a @code{1x1xN} array.
    ##
    ## @code{@var{ts} = timeseries (@var{data}, @var{time})} takes the samples
    ## at @var{time}, which is one of:
    ##
    ## @itemize
    ## @item a numeric vector of one time per sample, in seconds;
    ## @item a cell array of character vectors or a string array of dates, one
    ## per sample, which gives @qcode{Time} in days from the earliest of them,
    ## with that date as @qcode{TimeInfo.StartDate};
    ## @item a cell array of numeric scalars, read as a numeric vector;
    ## @item @code{[]}, for the default times.
    ## @end itemize
    ##
    ## A row vector @var{data} with as many elements as @var{time} is taken as
    ## that many scalar samples; with a single time it is one sample.  The
    ## times need not be sorted: the samples are sorted with them, keeping the
    ## order of samples at equal times.  They are stored as @code{double}
    ## whatever their class.  @code{datetime} and @code{duration} arrays are
    ## not accepted, as in MATLAB.
    ##
    ## @code{@var{ts} = timeseries (@var{data}, @var{time}, @var{quality})}
    ## also sets a quality code per sample: integers from -128 to 127, as a
    ## vector of one per sample or an array the size of @var{data}, or
    ## @code{[]} for none.
    ##
    ## @code{@var{ts} = timeseries (@dots{}, @qcode{'Name'}, @var{name})} names
    ## the series; the option name is matched in any case, and a later
    ## occurrence overrides an earlier one.  A @var{name} that is not text is
    ## refused, where MATLAB ignores it.
    ##
    ## @code{@var{ts} = timeseries (@var{ts0})} returns the series @var{ts0}
    ## itself.
    ##
    ## @seealso{timetable, istimeseries, tsdata.timemetadata}
    ## @end deftypefn
    function this = timeseries (varargin)

      if (nargin == 0)
        this.name_ = '';
        return;
      endif

      ## A series or a name alone
      arg1 = varargin{1};
      if (nargin == 1 && isa (arg1, 'timeseries'))
        this = arg1;
        return;
      endif
      if (isText (arg1))
        if (nargin > 1)
          error ("timeseries: DATA must be a numeric or logical array.");
        endif
        this.name_ = char (arg1);
        return;
      endif
      if (! (isnumeric (arg1) || islogical (arg1)))
        error ("timeseries: DATA must be a numeric or logical array.");
      endif
      data = arg1;

      ## Positional arguments end at the first option name
      args = varargin(2:end);
      nPos = numel (args);
      for i = 1:numel (args)
        if (isText (args{i}))
          nPos = i - 1;
          break;
        endif
      endfor
      if (nPos > 2)
        error ("timeseries: too many input arguments.");
      endif
      opts = args(nPos+1:end);
      if (mod (numel (opts), 2) != 0)
        error ("timeseries: name-value arguments must be in pairs.");
      endif
      for i = 1:2:numel (opts)
        if (! strcmpi (opts{i}, 'Name'))
          error ("timeseries: invalid optional paired argument.");
        endif
        if (! isText (opts{i+1}))
          error ("timeseries: 'Name' must be a character vector.");
        endif
        this.name_ = char (opts{i+1});
      endfor

      ## Time vector
      time = [];
      startDate = '';
      if (nPos >= 1)
        [time, startDate, errmsg] = parseTime (args{1});
        if (! isempty (errmsg))
          error ("timeseries: %s", errmsg);
        endif
      endif

      ## Reading a row vector as that many scalar samples
      if (isrow (data) && numel (data) > 1
          && (isempty (time) || numel (time) == numel (data)))
        data = reshape (data, 1, 1, numel (data));
      endif
      nSamples = sampleCount (data);
      if (nPos < 1 || isempty (time))
        time = (0:nSamples-1)';
      elseif (numel (time) != nSamples)
        error (strcat ("timeseries: DATA and TIME must have the same", ...
                       " number of samples."));
      endif

      ## Quality
      quality = [];
      if (nPos == 2)
        [quality, errmsg] = qualityValue (args{2}, data, nSamples);
        if (! isempty (errmsg))
          error ("timeseries: QUALITY %s", errmsg);
        endif
      endif

      ## Sort the whole record by time, stably
      if (any (diff (time) < 0))
        [time, idx] = sort (time);
        data = takeSamples (data, idx);
        if (! isempty (quality))
          quality = takeSamples (quality, idx);
        endif
      endif

      this.data_ = data;
      this.time_ = time;
      this.quality_ = quality;
      this.timeInfo_.TimeVector = time;
      if (! isempty (startDate))
        this.timeInfo_.Units = 'days';
        this.timeInfo_.StartDate = startDate;
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{S} =} get (@var{ts})
    ## @deftypefnx {timeseries} {@var{value} =} get (@var{ts}, @var{name})
    ## @deftypefnx {timeseries} {@var{values} =} get (@var{ts}, @var{names})
    ##
    ## Return property values.
    ##
    ## @code{@var{S} = get (@var{ts})} returns a structure of every property
    ## of the series @var{ts}.
    ##
    ## @code{@var{value} = get (@var{ts}, @var{name})} returns the property
    ## @var{name}, a character vector or a string scalar matched in any case.
    ##
    ## @code{@var{values} = get (@var{ts}, @var{names})} returns a row cell
    ## array of the properties named in the cell array @var{names}.
    ##
    ## For an array of series, @code{get} with one @var{name} returns a cell
    ## array the size of the array, with a cell array of @var{names} a cell
    ## array with a row per series and a column per property, and with no
    ## name a cell array with a row per series and a column per property of
    ## every property.
    ##
    ## @seealso{timeseries.set, timeseries}
    ## @end deftypefn
    function out = get (this, varargin)
      out = timeseries.propertyGet (this, 'timeseries.get', varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {} set (@var{ts}, @var{name}, @var{value})
    ## @deftypefnx {timeseries} {} set (@var{ts}, @var{name1}, @var{value1}, @dots{})
    ## @deftypefnx {timeseries} {@var{ts2} =} set (@var{ts}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {timeseries} {@var{S} =} set (@var{ts})
    ##
    ## Set property values.
    ##
    ## @code{set (@var{ts}, @var{name}, @var{value}, @dots{})} sets each
    ## property @var{name}, matched in any case, to its @var{value}, in the
    ## order given, in the variable @var{ts} of the caller.  @var{ts} must
    ## therefore be a variable, not an expression such as an indexed element
    ## or @code{@var{tc}.Series}; set those with dot assignment.  For an array
    ## of series every element is set.
    ##
    ## @code{@var{ts2} = set (@var{ts}, @var{name}, @var{value}, @dots{})}
    ## returns the modified series and leaves @var{ts} as it was, so
    ## @var{ts} may then be any expression.
    ##
    ## @code{@var{S} = set (@var{ts})} returns what @code{get (@var{ts})}
    ## does.  A name with no value is refused; MATLAB returns the current
    ## value quoted as text.
    ##
    ## @seealso{timeseries.get, timeseries}
    ## @end deftypefn
    function varargout = set (this, varargin)
      if (nargin == 1)
        varargout{1} = get (this);
        return;
      endif
      this = timeseries.propertySet (this, 'timeseries.set', varargin{:});
      if (nargout > 0)
        varargout{1} = this;
        return;
      endif
      ## With no output the caller's own variable is set, as in MATLAB
      name = inputname (1);
      if (isempty (name))
        error (strcat ("timeseries.set: with no output argument, TS must", ...
                       " be a variable; set anything else with dot", ...
                       " assignment."));
      endif
      assignin ('caller', name, this);
    endfunction

  endmethods

  ## Property access
  methods

    function val = get.Events (this)
      val = this.events_;
    endfunction

    function this = set.Events (this, val)
      if (isnumeric (val) && isempty (val))
        this.events_ = [];
      elseif (isa (val, 'tsdata.event') && (isvector (val) || isempty (val)))
        this.events_ = val;
      else
        error (strcat ("timeseries: 'Events' must be a vector of", ...
                       " tsdata.event objects."));
      endif
    endfunction

    function val = get.Name (this)
      val = this.name_;
    endfunction

    function this = set.Name (this, val)
      if (! isText (val))
        error ("timeseries: 'Name' must be a character vector.");
      endif
      val = char (val);
      if (isempty (val))
        val = '';
      endif
      this.name_ = val;
    endfunction

    function val = get.UserData (this)
      val = this.userData_;
    endfunction

    function this = set.UserData (this, val)
      this.userData_ = val;
    endfunction

    function val = get.Data (this)
      val = this.data_;
    endfunction

    function this = set.Data (this, val)
      if (iscell (val))
        error ("timeseries: 'Data' must not be a cell array.");
      endif
      if (! (isnumeric (val) || islogical (val)))
        error ("timeseries: 'Data' must be a numeric or logical array.");
      endif
      n = numel (this.time_);
      if (isrow (val) && numel (val) > 1 && numel (val) == n)
        val = reshape (val, 1, 1, n);
      endif
      if (! isempty (val) && n > 0 && sampleCount (val) != n)
        error ("timeseries: 'Data' must have one sample per time.");
      endif
      this.data_ = val;
    endfunction

    function val = get.DataInfo (this)
      val = this.dataInfo_;
    endfunction

    function this = set.DataInfo (this, val)
      if (! (isa (val, 'tsdata.datametadata') && isscalar (val)))
        error (strcat ("timeseries: 'DataInfo' must be a", ...
                       " tsdata.datametadata object."));
      endif
      this.dataInfo_ = val;
    endfunction

    function val = get.Time (this)
      val = this.time_;
    endfunction

    function this = set.Time (this, val)
      if (! (isnumeric (val) && isreal (val)
             && (isvector (val) || isempty (val))))
        error ("timeseries: 'Time' must be a numeric vector.");
      endif
      val = double (val(:));
      if (! all (isfinite (val)))
        error ("timeseries: 'Time' must be finite.");
      endif
      if (any (diff (val) < 0))
        error ("timeseries: 'Time' must be non-decreasing.");
      endif
      if (! isempty (this.data_) && ! isempty (val)
          && sampleCount (this.data_) != numel (val))
        error ("timeseries: 'Time' must have one time per sample.");
      endif
      if (! isempty (this.quality_) && numel (val) != numel (this.time_))
        error ("timeseries: 'Time' must have one time per quality code.");
      endif
      this.time_ = val;
      this.timeInfo_.TimeVector = val;
    endfunction

    function val = get.TimeInfo (this)
      val = this.timeInfo_;
    endfunction

    function this = set.TimeInfo (this, val)
      if (! (isa (val, 'tsdata.timemetadata') && isscalar (val)))
        error (strcat ("timeseries: 'TimeInfo' must be a", ...
                       " tsdata.timemetadata object."));
      endif
      ## The derived properties always follow this series' own time vector
      val.TimeVector = this.time_;
      this.timeInfo_ = val;
    endfunction

    function val = get.Quality (this)
      val = this.quality_;
    endfunction

    function this = set.Quality (this, val)
      [val, errmsg] = qualityValue (val, this.data_, numel (this.time_));
      if (! isempty (errmsg))
        error ("timeseries: 'Quality' %s", errmsg);
      endif
      this.quality_ = val;
    endfunction

    function val = get.QualityInfo (this)
      val = this.qualityInfo_;
    endfunction

    function this = set.QualityInfo (this, val)
      if (! (isa (val, 'tsdata.qualmetadata') && isscalar (val)))
        error (strcat ("timeseries: 'QualityInfo' must be a", ...
                       " tsdata.qualmetadata object."));
      endif
      this.qualityInfo_ = val;
    endfunction

    function val = get.IsTimeFirst (this)
      val = ndims (this.data_) <= 2;
    endfunction

    function this = set.IsTimeFirst (this, val)
      if (! ((islogical (val) || isnumeric (val)) && isscalar (val)
             && any (val == [0, 1])))
        error ("timeseries: 'IsTimeFirst' must be a logical scalar.");
      endif
      if (logical (val) != (ndims (this.data_) <= 2))
        error (strcat ("timeseries: 'IsTimeFirst' cannot be %s: the", ...
                       " time vector runs along the %s dimension of", ...
                       " data of %d dimensions."), mat2str (logical (val)), ...
               ifelse (logical (val), 'last', 'first'), ndims (this.data_));
      endif
    endfunction

    function val = get.TreatNaNasMissing (this)
      val = this.treatNaN_;
    endfunction

    function this = set.TreatNaNasMissing (this, val)
      if (! ((islogical (val) || isnumeric (val)) && isscalar (val)
             && any (val == [0, 1])))
        error ("timeseries: 'TreatNaNasMissing' must be a logical scalar.");
      endif
      this.treatNaN_ = logical (val);
    endfunction

    function val = get.Length (this)
      val = numel (this.time_);
    endfunction

    function this = set.Length (this, val)
      error ("timeseries: 'Length' is read-only.");
    endfunction

  endmethods

endclassdef

## Resolve NAME, in any case, to one of the property names ALLNAMES.  Returns
## the property's own spelling and an empty ERRMSG, or the body of the
## message the caller raises.
function [name, errmsg] = propertyName (name, allNames)
  errmsg = '';
  idx = find (strcmpi (name, allNames), 1);
  if (isempty (idx))
    errmsg = sprintf ("unknown property: '%s'", name);
  else
    name = allNames{idx};
  endif
endfunction

## The size of X as MATLAB writes it in a summary, as '3x1'.
function str = sizestr (x)
  str = strjoin (arrayfun (@num2str, size (x), 'UniformOutput', false), 'x');
endfunction

## True for a character vector (or '') and for a string scalar.
function tf = isText (x)
  tf = (ischar (x) && (isrow (x) || isempty (x))) ...
       || (isa (x, 'string') && isscalar (x));
endfunction

## The number of samples in DATA: rows for two dimensions or fewer, the size
## of the last dimension otherwise.
function n = sampleCount (data)
  if (ndims (data) <= 2)
    n = size (data, 1);
  else
    n = size (data, ndims (data));
  endif
endfunction

## Reorder or select the samples of DATA by the index IDX.
function data = takeSamples (data, idx)
  if (ndims (data) <= 2)
    data = data(idx,:);
  else
    subs = repmat ({':'}, 1, ndims (data));
    subs{end} = idx;
    data = data(subs{:});
  endif
endfunction

## Read a constructor TIME argument.  Returns a double column, the start date
## of a time vector given as dates ('' otherwise), and an empty ERRMSG, or the
## body of the message the caller raises.
function [time, startDate, errmsg] = parseTime (time)
  startDate = '';
  errmsg = '';
  if (isa (time, 'datetime') || isa (time, 'duration'))
    errmsg = "TIME must be a numeric vector or dates as text.";
    return;
  endif
  if (iscell (time) && ! isempty (time)
      && all (cellfun (@(x) isnumeric (x) && isscalar (x), time(:))))
    time = cell2mat (time(:));
  endif
  if (iscellstr (time) || isa (time, 'string'))
    try
      days = datenum (cellstr (time(:)));
    catch
      errmsg = "TIME holds text that is not a date.";
      return;
    end_try_catch
    t0 = min (days);
    startDate = datestr (t0, 'dd-mmm-yyyy HH:MM:SS');
    time = days - t0;
    return;
  endif
  if (! (isnumeric (time) && isreal (time)
         && (isvector (time) || isempty (time))))
    errmsg = "TIME must be a numeric vector or dates as text.";
    return;
  endif
  time = double (time(:));
  if (! all (isfinite (time)))
    errmsg = "TIME must be finite.";
  endif
endfunction

## Validate quality codes for data DATA of N samples.  Returns them as double,
## a vector reshaped to lie along the time dimension, and an empty ERRMSG, or
## the rest of the message the caller raises after naming the codes.
function [q, errmsg] = qualityValue (q, data, n)
  errmsg = '';
  if (isempty (q) && (isnumeric (q) || islogical (q)))
    q = [];
    return;
  endif
  if (! ((isnumeric (q) || islogical (q)) && isreal (q)))
    errmsg = "must be an integer array.";
    return;
  endif
  if (! (all (q(:) == fix (q(:))) && all (q(:) >= -128 & q(:) <= 127)))
    errmsg = "must hold integers from -128 to 127.";
    return;
  endif
  q = double (q);
  if (isequal (size (q), size (data)) && n > 0)
    return;
  endif
  if (isvector (q) && numel (q) == n)
    if (ndims (data) <= 2)
      q = q(:);
    else
      q = reshape (q, [ones(1, ndims (data) - 1), n]);
    endif
    return;
  endif
  errmsg = "must have one code per sample or the size of the data.";
endfunction
