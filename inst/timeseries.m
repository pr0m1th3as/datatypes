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
  ## Each sample may be a scalar, a vector, or an array.  The time vector runs
  ## either down the first dimension of the data, each row being a sample, or
  ## along its last dimension, each slice being one; @qcode{IsTimeFirst}
  ## reports which, as the constructor describes.
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
    ## A numeric or logical array of any class, holding one sample per time
    ## and read against the time vector as the constructor describes: rows,
    ## or slices along the last dimension, with a matrix of as many columns
    ## as there are times stored as an @code{Rx1xN} array.  Any other number
    ## of samples than there are times is refused, except while one of the
    ## two is empty, which lets a series be built by assigning @qcode{Time}
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
    ## @code{true} when each row of the data is a sample, @code{false} when
    ## the time vector runs along the last dimension of the data, as the
    ## constructor describes.  It is kept when samples are selected, so a
    ## three-dimensional series cut to one sample holds a matrix with
    ## @qcode{IsTimeFirst} still @code{false}.  Assigning the value it already
    ## has is accepted, and assigning the other is refused, since it would
    ## misalign the data with the time vector.
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
    ## The dimension of the data the time vector runs along: 1, or the last
    ## dimension, which a matrix holding one sample takes as 3
    timeDim_ = 1
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
    ## With @var{N} times, data of three dimensions or more must have @var{N}
    ## slices along its last dimension.  A matrix with @var{N} rows has a
    ## sample per row; otherwise one with @var{N} columns is stored as an
    ## @code{Rx1xN} array, a sample per column, so a row vector of @var{N}
    ## elements gives @var{N} scalar samples; and with a single time the whole
    ## matrix is one sample.  The times need not be sorted: the samples are
    ## sorted with them, keeping the order of samples at equal times.  They are stored as @code{double}
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

      ## Which dimension of the data the time vector runs along
      if (nPos < 1 || isempty (time))
        [data, td] = orientData (data, []);
        time = (0:size (data, td) - 1)';
      else
        [data, td, errmsg] = orientData (data, numel (time));
        if (! isempty (errmsg))
          error (strcat ("timeseries: DATA and TIME must have the same", ...
                         " number of samples."));
        endif
      endif
      nSamples = numel (time);

      ## Quality
      quality = [];
      if (nPos == 2)
        [quality, errmsg] = qualityValue (args{2}, data, nSamples, td);
        if (! isempty (errmsg))
          error ("timeseries: QUALITY %s", errmsg);
        endif
      endif

      ## Sort the whole record by time, stably
      if (any (diff (time) < 0))
        [time, idx] = sort (time);
        data = takeSamples (data, idx, td);
        if (! isempty (quality))
          quality = takeSamples (quality, idx, td);
        endif
      endif

      this.timeDim_ = td;
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

################################################################################
##                    ** Data and sample operations **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'getdatasamples'   'getdatasamplesize' 'getsamples'   'getsampleusingtime' ##
## 'addsample'        'delsample'         'append'       'hasduplicatetimes'  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{data} =} getdatasamples (@var{ts}, @var{ind})
    ##
    ## Return the data of selected samples.
    ##
    ## @code{@var{data} = getdatasamples (@var{ts}, @var{ind})} returns the
    ## data of the samples @var{ind} of the series @var{ts}, in the order
    ## given, repeats included: the rows @var{ind} of @qcode{Data} for a series
    ## whose time runs down its first dimension, the slices @var{ind} along the
    ## last dimension otherwise.  @var{ind} is a vector of positive integers
    ## not exceeding @qcode{Length}, or a logical mask no longer than
    ## @qcode{Length}; @code{[]} returns @code{[]}.
    ##
    ## @seealso{timeseries.getsamples, timeseries.getdatasamplesize}
    ## @end deftypefn
    function data = getdatasamples (this, varargin)
      if (numel (varargin) != 1)
        error ("timeseries.getdatasamples: invalid number of input arguments.");
      endif
      mustBeScalar (this, 'getdatasamples');
      [idx, errmsg] = sampleIndex (varargin{1}, numel (this.time_), true);
      if (! isempty (errmsg))
        error ("timeseries.getdatasamples: IND %s", errmsg);
      endif
      if (isempty (idx))
        data = [];
      else
        data = takeSamples (this.data_, idx, this.timeDim_);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{sz} =} getdatasamplesize (@var{ts})
    ##
    ## Return the size of one sample.
    ##
    ## @code{@var{sz} = getdatasamplesize (@var{ts})} returns the size of each
    ## sample of the series @var{ts}: @code{[1 @var{C}]} for data of @var{C}
    ## columns with a sample per row, and the size of a slice for data whose
    ## time runs along the last dimension, so @code{[2 3]} for a
    ## @code{2x3x4} array.  It is @code{[]} for a series with no samples.
    ##
    ## @seealso{timeseries.getdatasamples}
    ## @end deftypefn
    function sz = getdatasamplesize (this)
      mustBeScalar (this, 'getdatasamplesize');
      if (isempty (this.time_) || isempty (this.data_))
        sz = [];
      else
        sz = sampleSize (this.data_, this.timeDim_);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts2} =} getsamples (@var{ts}, @var{ind})
    ##
    ## Return a series of selected samples.
    ##
    ## @code{@var{ts2} = getsamples (@var{ts}, @var{ind})} returns the series
    ## @var{ts} holding only the samples @var{ind}, which are taken in time
    ## order whatever order they are given in, repeats included.  @var{ind}
    ## is as for @code{getdatasamples}.  Their times and quality codes go with
    ## them, and every other property is kept.
    ##
    ## MATLAB names the result @qcode{'unnamed'}, and for an empty @var{ind}
    ## returns @code{timeseries ()} with every property reset; here the name
    ## and properties are kept in both cases.
    ##
    ## @seealso{timeseries.getdatasamples, timeseries.getsampleusingtime,
    ## timeseries.delsample}
    ## @end deftypefn
    function this = getsamples (this, varargin)
      if (numel (varargin) != 1)
        error ("timeseries.getsamples: invalid number of input arguments.");
      endif
      mustBeScalar (this, 'getsamples');
      [idx, errmsg] = sampleIndex (varargin{1}, numel (this.time_), true);
      if (! isempty (errmsg))
        error ("timeseries.getsamples: IND %s", errmsg);
      endif
      this = subset (this, sort (idx));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts2} =} getsampleusingtime (@var{ts}, @var{t})
    ## @deftypefnx {timeseries} {@var{ts2} =} getsampleusingtime (@var{ts}, @var{t}, @qcode{'AllowDuplicateTimes'}, @var{tf})
    ## @deftypefnx {timeseries} {@var{ts2} =} getsampleusingtime (@var{ts}, @var{t1}, @var{t2})
    ##
    ## Return a series of the samples at a time or between two times.
    ##
    ## @code{@var{ts2} = getsampleusingtime (@var{ts}, @var{t})} returns the
    ## series @var{ts} holding only the sample at time @var{t}.  When several
    ## samples share that time the call is refused, unless
    ## @qcode{'AllowDuplicateTimes'} is @code{true}, which returns them all.
    ## MATLAB returns them all for @code{false} too.
    ##
    ## @code{@var{ts2} = getsampleusingtime (@var{ts}, @var{t1}, @var{t2})}
    ## returns the samples from @var{t1} to @var{t2}, both included.
    ##
    ## For a series with no @qcode{TimeInfo.StartDate} the times are numbers
    ## in its units.  For a series with one they are dates: text, a
    ## @code{datenum}, or a @code{datetime}, which MATLAB does not accept.
    ## No sample in range, @code{NaN}, or @var{t1} after @var{t2} give a
    ## series with no samples.  Every property but the samples is kept;
    ## MATLAB names the result @qcode{'unnamed'} and, when it is empty,
    ## resets every property.
    ##
    ## @seealso{timeseries.getsamples, timeseries.delsample}
    ## @end deftypefn
    function this = getsampleusingtime (this, varargin)
      mustBeScalar (this, 'getsampleusingtime');
      scope = 'timeseries.getsampleusingtime';
      nargs = numel (varargin);
      if (nargs < 1 || nargs > 3)
        error ("%s: invalid number of input arguments.", scope);
      endif
      allowDup = false;
      isRange = nargs == 2;
      if (nargs == 3)
        if (! (isText (varargin{2})
               && strcmpi (varargin{2}, 'AllowDuplicateTimes')))
          error ("%s: invalid optional paired argument.", scope);
        endif
        allowDup = varargin{3};
        if (! ((islogical (allowDup) || isnumeric (allowDup))
               && isscalar (allowDup) && any (allowDup == [0, 1])))
          error ("%s: 'AllowDuplicateTimes' must be a logical scalar.", scope);
        endif
      endif
      [t, tol, errmsg] = relativeTime (this, varargin{1}, true);
      if (isempty (errmsg) && ! isscalar (t))
        errmsg = "must be a single time.";
      endif
      if (! isempty (errmsg))
        error ("%s: %s %s", scope, ifelse (isRange, 'T1', 'T'), errmsg);
      endif
      if (isRange)
        [t2, tol2, errmsg] = relativeTime (this, varargin{2}, true);
        if (isempty (errmsg) && ! isscalar (t2))
          errmsg = "must be a single time.";
        endif
        if (! isempty (errmsg))
          error ("%s: T2 %s", scope, errmsg);
        endif
        idx = find (this.time_ >= t - tol & this.time_ <= t2 + tol2);
      else
        idx = find (abs (this.time_ - t) <= tol);
        if (numel (idx) > 1 && ! allowDup)
          error (strcat ("%s: several samples share time T; set", ...
                         " 'AllowDuplicateTimes' to true to return them", ...
                         " all."), scope);
        endif
      endif
      this = subset (this, idx);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} addsample (@var{ts}, @qcode{'Data'}, @var{data}, @qcode{'Time'}, @var{time})
    ## @deftypefnx {timeseries} {@var{ts} =} addsample (@dots{}, @qcode{'Quality'}, @var{quality})
    ## @deftypefnx {timeseries} {@var{ts} =} addsample (@dots{}, @qcode{'OverwriteFlag'}, @var{tf})
    ## @deftypefnx {timeseries} {@var{ts} =} addsample (@var{ts}, @var{S})
    ##
    ## Add samples to a series.
    ##
    ## @code{@var{ts} = addsample (@var{ts}, @qcode{'Data'}, @var{data},
    ## @qcode{'Time'}, @var{time})} inserts the samples @var{data} at the
    ## times @var{time}, one sample per time, keeping the series in time
    ## order; a sample at a time already present goes after the samples
    ## there.  @var{data} is read against @var{time} as the constructor reads
    ## data against times, and each sample must have the size of the series'
    ## samples; a series with no samples takes any.  For a series with no
    ## @qcode{TimeInfo.StartDate} @var{time} is numbers in its units; for one
    ## with a start date it may also be dates as text, or a @code{datetime}.
    ## The class of the data follows concatenation, so an @code{int8} sample
    ## makes a @code{double} series @code{int8}.
    ##
    ## The option names are matched in any case, and a later occurrence
    ## overrides an earlier one.  They may also be given as the fields of a
    ## structure @var{S}.
    ##
    ## @table @asis
    ## @item @qcode{'Quality'}
    ## The quality codes of the new samples, one per sample or one per data
    ## element, as the series has them.  Refused for a series without quality
    ## codes.  When a series with codes is given none, each new sample takes
    ## the code of the sample nearest in time, the later one on a tie.
    ##
    ## @item @qcode{'OverwriteFlag'}
    ## When @code{true}, a new sample at a time already present replaces the
    ## data of every sample there, and their quality codes if
    ## @qcode{'Quality'} is given, instead of being inserted.  @code{false} by
    ## default.  MATLAB refuses 0 and 1 here.
    ## @end table
    ##
    ## @seealso{timeseries.delsample, timeseries.append}
    ## @end deftypefn
    function this = addsample (this, varargin)
      mustBeScalar (this, 'addsample');
      scope = 'timeseries.addsample';
      args = varargin;
      if (numel (args) == 1 && isstruct (args{1}) && isscalar (args{1}))
        args = [fieldnames(args{1}), struct2cell(args{1})]';
        args = args(:)';
      endif
      if (mod (numel (args), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      optNames = {'Data', 'Time', 'Quality', 'OverwriteFlag'};
      opts = {[], [], [], false};
      for i = 1:2:numel (args)
        k = [];
        if (isText (args{i}))
          k = find (strcmpi (args{i}, optNames));
        endif
        if (isempty (k))
          error ("%s: invalid optional paired argument.", scope);
        endif
        opts{k} = args{i+1};
      endfor
      [data, time, quality, overwrite] = opts{:};
      if (iscell (data) || ! (isnumeric (data) || islogical (data)))
        error ("%s: 'Data' must be a numeric or logical array.", scope);
      endif
      if (isempty (data))
        error ("%s: 'Data' must not be empty.", scope);
      endif
      if (isempty (time))
        error ("%s: 'Time' must not be empty.", scope);
      endif
      [time, tol, errmsg] = relativeTime (this, time, false);
      if (! isempty (errmsg))
        error ("%s: 'Time' %s", scope, errmsg);
      endif
      if (! ((islogical (overwrite) || isnumeric (overwrite))
             && isscalar (overwrite) && any (overwrite == [0, 1])))
        error ("%s: 'OverwriteFlag' must be a logical scalar.", scope);
      endif
      k = numel (time);
      [data, td, errmsg] = orientData (data, k);
      if (! isempty (errmsg))
        error ("%s: 'Data' must have one sample per time.", scope);
      endif

      ## A series with no samples takes them as the constructor would
      if (isempty (this.time_) && isempty (this.data_))
        if (! isempty (quality))
          error (strcat ("%s: 'Quality' cannot be added to a series", ...
                         " without quality codes."), scope);
        endif
        [time, idx] = sort (time);
        this.data_ = takeSamples (data, idx, td);
        this.time_ = time;
        this.timeDim_ = td;
        this.timeInfo_.TimeVector = time;
        if (isempty (this.name_))
          this.name_ = 'unnamed';
        endif
        return;
      endif
      if (isempty (this.time_) || isempty (this.data_))
        error ("%s: the 'Data' and 'Time' of TS do not agree.", scope);
      endif

      ## New samples laid out as the series' own
      tdS = this.timeDim_;
      ss = sampleSize (this.data_, tdS);
      if (! isequal (sampleSize (data, td), ss))
        error ("%s: 'Data' must hold samples of size %s.", scope, ...
               strjoin (arrayfun (@num2str, ss, 'UniformOutput', false), 'x'));
      endif
      data = layOut (data, td, tdS, ss, k);

      ## Quality codes of the new samples
      hasQ = ! isempty (this.quality_);
      perElement = hasQ && numel (this.quality_) != numel (this.time_);
      if (! isempty (quality))
        if (! hasQ)
          error (strcat ("%s: 'Quality' cannot be added to a series", ...
                         " without quality codes."), scope);
        endif
        if (! ((isnumeric (quality) || islogical (quality))
               && isreal (quality) && all (quality(:) == fix (quality(:)))
               && all (quality(:) >= -128 & quality(:) <= 127)))
          error ("%s: 'Quality' must hold integers from -128 to 127.", scope);
        endif
        quality = double (quality);
        if (perElement && numel (quality) == numel (data))
          quality = reshape (quality, size (data));
        elseif (! perElement && numel (quality) == k)
          if (tdS > 1)
            quality = reshape (quality, [ones(1, tdS - 1), k]);
          else
            quality = quality(:);
          endif
        else
          error (strcat ("%s: 'Quality' must have one code per new sample", ...
                         " or one per new data element."), scope);
        endif
      elseif (hasQ)
        ## The code of the nearest sample in time, the later on a tie
        near = zeros (k, 1);
        for i = 1:k
          d = abs (this.time_ - time(i));
          near(i) = find (d == min (d), 1, 'last');
        endfor
        quality = takeSamples (this.quality_, near, tdS);
      endif

      ## Replace the samples at times already present
      keep = true (k, 1);
      if (overwrite)
        for i = 1:k
          at = find (abs (this.time_ - time(i)) <= tol);
          if (isempty (at))
            continue;
          endif
          keep(i) = false;
          for j = at'
            this.data_ = putSample (this.data_, j, ...
                                    takeSamples (data, i, tdS), tdS);
            if (! isempty (quality) && ! isempty (opts{3}))
              this.quality_ = putSample (this.quality_, j, ...
                                         takeSamples (quality, i, tdS), tdS);
            endif
          endfor
        endfor
      endif

      ## Insert the rest, keeping the series in time order
      if (any (keep))
        cdim = max (tdS, 1);
        newData = cat (cdim, this.data_, takeSamples (data, find (keep), tdS));
        newTime = [this.time_; time(keep)];
        if (hasQ)
          newQ = cat (cdim, this.quality_, ...
                      takeSamples (quality, find (keep), tdS));
        endif
        [newTime, idx] = sort (newTime);
        this.data_ = takeSamples (newData, idx, tdS);
        this.time_ = newTime;
        if (hasQ)
          this.quality_ = takeSamples (newQ, idx, tdS);
        endif
        this.timeInfo_.TimeVector = newTime;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} delsample (@var{ts}, @qcode{'Index'}, @var{ind})
    ## @deftypefnx {timeseries} {@var{ts} =} delsample (@var{ts}, @qcode{'Value'}, @var{time})
    ##
    ## Remove samples from a series.
    ##
    ## @code{@var{ts} = delsample (@var{ts}, @qcode{'Index'}, @var{ind})}
    ## removes the samples @var{ind}, a vector of positive integers not
    ## exceeding @qcode{Length}; repeats are allowed and @code{[]} removes
    ## nothing.  A logical mask is refused, as in MATLAB.
    ##
    ## @code{@var{ts} = delsample (@var{ts}, @qcode{'Value'}, @var{time})}
    ## removes every sample at each of the times @var{time}; a time no sample
    ## has removes nothing.  For a series with no @qcode{TimeInfo.StartDate}
    ## the times are numbers in its units; for one with a start date they may
    ## also be dates as text, or a @code{datetime}.
    ##
    ## The option names are matched in any case.  Every property but the
    ## samples is kept.  MATLAB turns quality codes held per data element of
    ## three-dimensional data into a vector here; they lose the slice of each
    ## removed sample, as the data does.
    ##
    ## @seealso{timeseries.addsample, timeseries.getsamples}
    ## @end deftypefn
    function this = delsample (this, varargin)
      mustBeScalar (this, 'delsample');
      scope = 'timeseries.delsample';
      if (numel (varargin) != 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      [opt, val] = varargin{:};
      n = numel (this.time_);
      if (isText (opt) && strcmpi (opt, 'Index'))
        [idx, errmsg] = sampleIndex (val, n, false);
        if (! isempty (errmsg))
          error ("%s: 'Index' %s", scope, errmsg);
        endif
      elseif (isText (opt) && strcmpi (opt, 'Value'))
        [t, tol, errmsg] = relativeTime (this, val, false);
        if (! isempty (errmsg))
          error ("%s: 'Value' %s", scope, errmsg);
        endif
        idx = [];
        for i = 1:numel (t)
          idx = [idx; find(abs (this.time_ - t(i)) <= tol)];
        endfor
      else
        error ("%s: the option must be 'Index' or 'Value'.", scope);
      endif
      keep = true (n, 1);
      keep(idx) = false;
      this = subset (this, find (keep));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} append (@var{ts1}, @var{ts2}, @dots{})
    ##
    ## Join series end to end in time.
    ##
    ## @code{@var{ts} = append (@var{ts1}, @var{ts2}, @dots{})} returns one
    ## series holding the samples of every argument in turn.  An argument may
    ## be an array of series, taken element by element, and series with no
    ## samples are skipped.  Each series must start at or after the end of the
    ## one before, so equal times where two meet give a repeated time; a gap
    ## is kept.  Their samples must be of one size; the class of the data
    ## follows concatenation.
    ##
    ## Series with a @qcode{TimeInfo.StartDate} and series without one cannot
    ## be mixed.  Times are converted to the coarsest units among them, and
    ## dates are counted from the first series' start date.
    ##
    ## Quality codes are joined when every series has them and refused when
    ## only some do; MATLAB then keeps too few codes or drops them.  The
    ## result takes its name, @qcode{DataInfo.Units} and @qcode{QualityInfo}
    ## from the series when they all agree, and otherwise @qcode{'unnamed'},
    ## @qcode{''} and none; MATLAB always names it @qcode{'unnamed'} and drops
    ## @qcode{QualityInfo}.  Events are gathered from every series, and every
    ## other property is the first series'.
    ##
    ## @seealso{timeseries.addsample}
    ## @end deftypefn
    function out = append (varargin)
      scope = 'timeseries.append';
      list = {};
      for i = 1:numel (varargin)
        if (! isa (varargin{i}, 'timeseries'))
          error ("%s: every argument must be a timeseries.", scope);
        endif
        for k = 1:numel (varargin{i})
          list{end+1} = varargin{i}(k);
        endfor
      endfor
      if (isempty (list))
        out = varargin{1};
        return;
      endif
      full = list(cellfun (@(x) x.Length > 0, list));
      if (isempty (full))
        out = list{1};
        return;
      elseif (numel (full) == 1)
        out = full{1};
        return;
      endif
      first = full{1};
      absolute = cellfun (@(x) ! isempty (x.timeInfo_.StartDate), full);
      if (any (absolute) && ! all (absolute))
        error (strcat ("%s: series with and without a start date cannot", ...
                       " be appended."), scope);
      endif
      ss = sampleSize (first.data_, first.timeDim_);
      if (! all (cellfun (@(x) isequal (sampleSize (x.data_, x.timeDim_), ...
                                        ss), full)))
        error ("%s: every series must have samples of the same size.", scope);
      endif
      hasQ = cellfun (@(x) ! isempty (x.quality_), full);
      if (any (hasQ) && ! all (hasQ))
        error (strcat ("%s: either every series or none must have quality", ...
                       " codes."), scope);
      endif

      ## Times in the coarsest units, from the first start date
      units = cellfun (@(x) x.timeInfo_.Units, full, 'UniformOutput', false);
      [~, coarsest] = max (cellfun (@unitRank, units));
      unit = units{coarsest};
      td = first.timeDim_;
      times = cell (1, numel (full));
      datas = times;
      quals = times;
      for i = 1:numel (full)
        x = full{i};
        ns = x.time_ * nsPerUnit (x.timeInfo_.Units);
        if (all (absolute))
          ns = ns + (datenum (x.timeInfo_.StartDate) ...
                     - datenum (first.timeInfo_.StartDate)) * 864e11;
        endif
        times{i} = ns / nsPerUnit (unit);
        n = x.Length;
        datas{i} = layOut (x.data_, x.timeDim_, td, ss, n);
        if (all (hasQ))
          q = x.quality_;
          if (numel (q) == n)
            if (td == 1)
              q = q(:);
            else
              q = reshape (q, [ones(1, td - 1), n]);
            endif
          else
            q = layOut (q, x.timeDim_, td, ss, n);
          endif
          quals{i} = q;
        endif
        if (i > 1 && times{i}(1) < times{i-1}(end) ...
                                  - 1e-12 * max (1, abs (times{i-1}(end))))
          error (strcat ("%s: each series must start at or after the end", ...
                         " of the one before."), scope);
        endif
      endfor
      out = first;
      out.data_ = cat (td, datas{:});
      out.time_ = vertcat (times{:});
      out.timeDim_ = td;
      out.timeInfo_.Units = unit;
      out.timeInfo_.TimeVector = out.time_;
      if (all (hasQ))
        try
          out.quality_ = cat (td, quals{:});
        catch
          error (strcat ("%s: quality codes must be given the same way in", ...
                         " every series."), scope);
        end_try_catch
      endif
      names = cellfun (@(x) x.name_, full, 'UniformOutput', false);
      if (! all (strcmp (names, names{1})))
        out.name_ = 'unnamed';
      endif
      dunits = cellfun (@(x) x.dataInfo_.Units, full, 'UniformOutput', false);
      if (! all (strcmp (dunits, dunits{1})))
        out.dataInfo_.Units = '';
      endif
      qi = first.qualityInfo_;
      for i = 2:numel (full)
        qj = full{i}.qualityInfo_;
        if (! (isequal (qj.Code, qi.Code)
               && isequal (qj.Description, qi.Description)))
          out.qualityInfo_ = tsdata.qualmetadata ();
          break;
        endif
      endfor
      events = {};
      for i = 1:numel (full)
        if (! isempty (full{i}.events_))
          events{end+1} = full{i}.events_(:)';
        endif
      endfor
      if (isempty (events))
        out.events_ = [];
      else
        out.events_ = [events{:}];
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{tf} =} hasduplicatetimes (@var{ts})
    ##
    ## True where a series repeats a time.
    ##
    ## @code{@var{tf} = hasduplicatetimes (@var{ts})} returns @code{true} when
    ## two samples of the series @var{ts} share a time.  For an array of
    ## series it returns a logical array of its size, one answer per series;
    ## MATLAB returns a single answer that can miss a series with repeated
    ## times.
    ##
    ## @seealso{timeseries.getsampleusingtime}
    ## @end deftypefn
    function tf = hasduplicatetimes (this)
      tf = false (size (this));
      for k = 1:numel (this)
        tf(k) = any (diff (this(k).time_) == 0);
      endfor
    endfunction

  endmethods

  methods (Access = private)

    ## The series holding only the samples IDX, in that order, with every
    ## other property kept.
    function this = subset (this, idx)
      idx = idx(:);
      if (! isempty (this.data_))
        this.data_ = takeSamples (this.data_, idx, this.timeDim_);
      endif
      this.time_ = this.time_(idx);
      if (! isempty (this.quality_))
        this.quality_ = takeSamples (this.quality_, idx, this.timeDim_);
      endif
      this.timeInfo_.TimeVector = this.time_;
    endfunction

    ## Convert user times X to times of this series.  Numbers are relative
    ## times in its units, or, for a series with a start date when
    ## NUMISDATENUM is true, datenums; text and datetime values are dates.
    ## Returns them as a double column, the tolerance a match allows, and an
    ## empty ERRMSG, or the rest of the message the caller raises.
    function [t, tol, errmsg] = relativeTime (this, x, numIsDatenum)
      tol = 0;
      errmsg = '';
      startDate = this.timeInfo_.StartDate;
      isDate = ischar (x) || iscellstr (x) || isa (x, 'string') ...
               || isa (x, 'datetime');
      if (isempty (startDate))
        if (isDate || ! (isnumeric (x) && isreal (x)))
          t = [];
          errmsg = "must be numeric for a series without a start date.";
          return;
        endif
        t = double (x(:));
      elseif (isnumeric (x) && isreal (x) && ! numIsDatenum)
        t = double (x(:));
      else
        try
          if (isa (x, 'datetime'))
            dn = datenum (x(:));
          elseif (isnumeric (x) && isreal (x))
            dn = double (x(:));
          else
            dn = datenum (cellstr (x));
            dn = dn(:);
          endif
        catch
          t = [];
          errmsg = "must be dates, as text, datenum or datetime values.";
          return;
        end_try_catch
        nsu = nsPerUnit (this.timeInfo_.Units);
        t = (dn - datenum (startDate)) * 864e11 / nsu;
        ## Dates reach a series only to the precision of a datenum
        tol = 1e-9 * 864e11 / nsu;
      endif
      if (any (isnan (t)) && numel (t) > 1)
        errmsg = "must not be NaN.";
      endif
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
      if (n > 0 && ! isempty (val))
        [val, td, errmsg] = orientData (val, n);
        if (! isempty (errmsg))
          error ("timeseries: 'Data' must have one sample per time.");
        endif
      elseif (ndims (val) <= 2)
        td = 1;
      else
        td = ndims (val);
      endif
      this.data_ = val;
      this.timeDim_ = td;
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
      if (! isempty (this.quality_) && numel (val) != numel (this.time_))
        error ("timeseries: 'Time' must have one time per quality code.");
      endif
      if (! isempty (this.data_) && ! isempty (val))
        [data, td, errmsg] = orientData (this.data_, numel (val));
        if (! isempty (errmsg))
          error ("timeseries: 'Time' must have one time per sample.");
        endif
        this.data_ = data;
        this.timeDim_ = td;
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
      [val, errmsg] = qualityValue (val, this.data_, numel (this.time_), ...
                                    this.timeDim_);
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
      val = this.timeDim_ == 1;
    endfunction

    function this = set.IsTimeFirst (this, val)
      if (! ((islogical (val) || isnumeric (val)) && isscalar (val)
             && any (val == [0, 1])))
        error ("timeseries: 'IsTimeFirst' must be a logical scalar.");
      endif
      if (logical (val) != (this.timeDim_ == 1))
        error (strcat ("timeseries: 'IsTimeFirst' cannot be %s: the", ...
                       " time vector runs along the %s dimension of", ...
                       " the data."), mat2str (logical (val)), ...
               ifelse (logical (val), 'last', 'first'));
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

## Raise unless OBJ is a single series.
function mustBeScalar (obj, method)
  if (! isscalar (obj))
    error ("timeseries.%s: TS must be a single series.", method);
  endif
endfunction

## Validate sample indices IND for a series of N samples.  A logical mask no
## longer than N is allowed where ALLOWLOGICAL is true.  Returns the indices
## and an empty ERRMSG, or the rest of the message the caller raises.
function [idx, errmsg] = sampleIndex (ind, n, allowLogical)
  errmsg = '';
  idx = [];
  if (islogical (ind) && allowLogical)
    if (! (isvector (ind) || isempty (ind)) || numel (ind) > n)
      errmsg = "must not be a logical mask longer than the series.";
      return;
    endif
    idx = find (ind(:));
  elseif (isnumeric (ind) && isreal (ind) && (isvector (ind) || isempty (ind)))
    idx = double (ind(:));
    if (! all (idx == fix (idx) & idx >= 1 & idx <= n))
      errmsg = "must be positive integers not exceeding the number of samples.";
      idx = [];
    endif
  else
    errmsg = "must be positive integers not exceeding the number of samples.";
  endif
endfunction

## Lay out DATA, N samples of size SS along dimension TDFROM, along dimension
## TDTO instead.
function data = layOut (data, tdFrom, tdTo, ss, n)
  if (tdFrom == tdTo)
    return;
  endif
  if (tdTo == 1)
    data = permute (data, [tdFrom, 1:tdFrom-1]);
    data = reshape (data, [n, ss(2:end)]);
  else
    data = permute (data, [2:ndims(data), 1]);
    data = reshape (data, [ss, n]);
  endif
endfunction

## Put SAMPLE, one sample laid out along dimension TD, at position J of DATA.
function data = putSample (data, j, sample, td)
  subs = repmat ({':'}, 1, max (ndims (data), td));
  subs{td} = j;
  data(subs{:}) = sample;
endfunction

## Nanoseconds in one time unit, each exact in double, so converting as
## T * NSFROM / NSTO is exact wherever the result is representable.
function ns = nsPerUnit (unit)
  switch (unit)
    case 'weeks'
      ns = 6048e11;
    case 'days'
      ns = 864e11;
    case 'hours'
      ns = 36e11;
    case 'minutes'
      ns = 6e10;
    case 'seconds'
      ns = 1e9;
    case 'milliseconds'
      ns = 1e6;
    case 'microseconds'
      ns = 1e3;
    otherwise
      ns = 1;
  endswitch
endfunction

function r = unitRank (unit)
  r = find (strcmp (unit, {'nanoseconds', 'microseconds', 'milliseconds', ...
                           'seconds', 'minutes', 'hours', 'days', 'weeks'}));
endfunction

## Find the dimension of DATA the time vector runs along, given N times, or
## the default reading when N is empty.  Returns DATA, reshaped when a matrix
## holds a sample per column, the dimension TD, and an empty ERRMSG, or the
## body of the message the caller raises.  The rule is MATLAB's: three
## dimensions or more run time along the last; a matrix of N rows along the
## first; one of N columns along a third, as an Rx1xN array; and with a
## single time a matrix is one sample, whatever its size.
function [data, td, errmsg] = orientData (data, n)
  errmsg = '';
  nd = ndims (data);
  if (isempty (n))
    if (nd >= 3)
      td = nd;
    elseif (isrow (data) && numel (data) > 1)
      data = reshape (data, 1, 1, numel (data));
      td = 3;
    else
      td = 1;
    endif
  elseif (nd >= 3)
    td = nd;
    if (size (data, nd) != n)
      errmsg = "must have one sample per time.";
    endif
  elseif (size (data, 1) == n)
    td = 1;
  elseif (size (data, 2) == n)
    data = reshape (data, size (data, 1), 1, n);
    td = 3;
  elseif (n == 1)
    td = 3;
  else
    td = 1;
    errmsg = "must have one sample per time.";
  endif
endfunction

## Select the samples IDX of DATA, whose time runs along dimension TD.
function data = takeSamples (data, idx, td)
  subs = repmat ({':'}, 1, max (ndims (data), td));
  subs{td} = idx;
  data = data(subs{:});
endfunction

## The size of one sample of DATA, whose time runs along dimension TD.
function sz = sampleSize (data, td)
  if (td == 1)
    sz = [1, size(data, 2)];
  else
    sz = size (data);
    sz(end+1:td) = 1;
    sz = sz(1:td-1);
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

## Validate quality codes for data DATA of N samples along dimension TD.
## Returns them as double, a vector laid along the time dimension, and an
## empty ERRMSG, or the rest of the message the caller raises after naming
## the codes.
function [q, errmsg] = qualityValue (q, data, n, td)
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
    if (td == 1)
      q = q(:);
    else
      q = reshape (q, [ones(1, td - 1), n]);
    endif
    return;
  endif
  errmsg = "must have one code per sample or the size of the data.";
endfunction
