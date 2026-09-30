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
  ## @seealso{tscollection, timeseries2timetable, timetable,
  ## tsdata.timemetadata, tsdata.datametadata, tsdata.qualmetadata,
  ## tsdata.event, tsdata.interpolation}
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
    ## @item a cell array of character vectors, a string array or a
    ## @code{datetime} array of dates, one per sample, which gives @qcode{Time}
    ## in days from the earliest of them, with that date as
    ## @qcode{TimeInfo.StartDate}; each date is read by its own format, a time
    ## zone is dropped and the clock time kept, and @code{NaT} is refused;
    ## @item a @code{duration} array, relative times in the units its display
    ## format names: @qcode{'s'}, @qcode{'m'}, @qcode{'h'} and @qcode{'d'} give
    ## seconds, minutes, hours and days, any other format seconds;
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
    ## sorted with them, keeping the order of samples at equal times.  They
    ## are stored as @code{double} whatever their class.  MATLAB accepts
    ## neither @code{datetime} nor @code{duration} arrays here.
    ##
    ## @code{@var{ts} = timeseries (@var{data}, @var{time}, @var{quality})}
    ## also sets a quality code per sample: integers from -128 to 127, as a
    ## vector of one per sample or an array the size of @var{data} as stored,
    ## or @code{[]} for none.  A matrix stored a sample per column, as an
    ## @code{Rx1xN} array, takes codes per element in that shape too; MATLAB
    ## refuses them in the matrix's own @code{RxN} shape, as here.
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
      units = '';
      if (nPos >= 1)
        [time, startDate, units, errmsg] = parseTime (args{1});
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
      if (! isempty (units))
        this.timeInfo_.Units = units;
      endif
      if (! isempty (startDate))
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
      if (! all (isfinite (time)))
        error ("%s: 'Time' must be finite.", scope);
      endif
      if (! ((islogical (overwrite) || isnumeric (overwrite))
             && isscalar (overwrite) && any (overwrite == [0, 1])))
        error ("%s: 'OverwriteFlag' must be a logical scalar.", scope);
      endif
      k = numel (time);
      tdS = this.timeDim_;
      if (k == 1 && tdS > 1 && ! isempty (this.data_)
          && isequal (size (data), size (takeSamples (this.data_, 1, tdS))))
        ## One sample shaped as the series' own, whatever its dimensions
        td = tdS;
      else
        [data, td, errmsg] = orientData (data, k);
        if (! isempty (errmsg))
          error ("%s: 'Data' must have one sample per time.", scope);
        endif
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
          dvX = tsdata.timemetadata.dateVector (x.timeInfo_.StartDate);
          dv1 = tsdata.timemetadata.dateVector (first.timeInfo_.StartDate);
          ns = ns + tsdata.timemetadata.dateOffset (dvX, dv1);
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

################################################################################
##                        ** Time representation **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'getabstime'       'setabstime'       'setuniformtime'                     ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{dates} =} getabstime (@var{ts})
    ##
    ## Return the dates of the samples as text.
    ##
    ## @code{@var{dates} = getabstime (@var{ts})} returns a column cell array
    ## of character vectors, the date of each sample of the series @var{ts}:
    ## its time, in @qcode{TimeInfo.Units}, counted from
    ## @qcode{TimeInfo.StartDate}.  They are written in the @code{datestr}
    ## format @qcode{TimeInfo.Format}, or @qcode{'dd-mmm-yyyy HH:MM:SS'} when
    ## that is empty; seconds are rounded to the millisecond and the rest cut
    ## off, as @code{datestr} does.  A series with no samples gives @code{@{@}}.
    ## A series with no @qcode{TimeInfo.StartDate} is refused.
    ##
    ## MATLAB uses @qcode{TimeInfo.Format} only when it is one of the formats
    ## its documentation lists, such as @qcode{'dd-mmm-yyyy'} or
    ## @qcode{'mm/dd/yyyy'}, and writes the default for any other,
    ## @qcode{'yyyy-mm-dd'} included; here every format is used.
    ##
    ## @seealso{timeseries.setabstime, tsdata.timemetadata}
    ## @end deftypefn
    function dates = getabstime (this)
      mustBeScalar (this, 'getabstime');
      startDate = this.timeInfo_.StartDate;
      if (isempty (startDate))
        error ("timeseries.getabstime: TS has no 'TimeInfo.StartDate'.");
      endif
      if (isempty (this.time_))
        dates = {};
        return;
      endif
      fmt = this.timeInfo_.Format;
      if (isempty (fmt))
        fmt = 'dd-mmm-yyyy HH:MM:SS';
      endif
      dv0 = tsdata.timemetadata.dateVector (startDate);
      dn = datenum (dv0(1), dv0(2), dv0(3)) ...
           + ((dv0(4) * 3600 + dv0(5) * 60 + dv0(6)) * 1e9 ...
              + this.time_ * nsPerUnit (this.timeInfo_.Units)) / 864e11;
      dates = cellstr (datestr (dn, fmt));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} setabstime (@var{ts}, @var{dates})
    ## @deftypefnx {timeseries} {@var{ts} =} setabstime (@var{ts}, @var{dates}, @var{format})
    ##
    ## Set the time vector of a series from dates.
    ##
    ## @code{@var{ts} = setabstime (@var{ts}, @var{dates})} gives the samples
    ## of the series @var{ts} the dates @var{dates}, one per sample, in order:
    ## a cell array of character vectors, a string array, a character matrix
    ## of one date per row, a character vector for a single sample, or a
    ## @code{datetime} array.  The earliest date becomes
    ## @qcode{TimeInfo.StartDate}, stored as @qcode{'dd-mmm-yyyy HH:MM:SS'}
    ## with milliseconds as @qcode{'.FFF'} when the seconds are not whole, and
    ## @qcode{Time} holds each date's offset from it in the series' own
    ## @qcode{TimeInfo.Units}, which are kept: a series in seconds given one
    ## date a day gets times @code{[0 86400 172800 @dots{}]}.  Each date is read
    ## by its own format.  Dates given as times of day alone, such as
    ## @qcode{'13:00:00'}, give times since midnight and no start date, as in
    ## MATLAB.
    ##
    ## @code{@var{ts} = setabstime (@var{ts}, @var{dates}, @var{format})} reads
    ## every date strictly in the @code{datestr} format @var{format}.  A date
    ## not written in @var{format} is refused.  The start date is stored as
    ## above whatever the format, where MATLAB writes it in @var{format}, from
    ## which every later reading has to guess it again: @qcode{'dd/mm/yyyy'}
    ## then comes back as a date of another month.
    ##
    ## The dates need not be sorted: the samples, with their quality codes,
    ## are sorted with them, keeping the order of samples at equal dates, as
    ## the constructor sorts them.
    ##
    ## MATLAB differs in four ways.  It does not move the samples with unsorted
    ## dates, so each is given another sample's date.  It reads every date by
    ## the format of the first, so @code{@{'01-Jan-2024', '2024-01-02'@}} gives
    ## a date in year 7.  It forces text into @var{format} when it does not
    ## fit, so @qcode{'yyyy-mm-dd'} on @qcode{'01-Jan-2024'} gives a date in
    ## year 6.  And it takes @code{datenum} values, @code{datetime} values and
    ## plain numbers as relative seconds, dropping the dates; here numbers are
    ## refused, since a @code{datenum} cannot be told from a plain number, and
    ## a @code{datetime} is read as its dates.  Convert a @code{datenum}
    ## @var{x} with @code{datetime (@var{x}, 'ConvertFrom', 'datenum')}.
    ##
    ## @seealso{timeseries.getabstime, timeseries.setuniformtime}
    ## @end deftypefn
    function this = setabstime (this, dates, format)
      mustBeScalar (this, 'setabstime');
      scope = 'timeseries.setabstime';
      if (nargin < 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (nargin < 3)
        format = '';
      elseif (isa (format, 'string') && isscalar (format))
        format = char (format);
      elseif (! (ischar (format) && isrow (format)))
        error ("%s: FORMAT must be a character vector.", scope);
      endif
      if (isnumeric (dates) || islogical (dates))
        error (strcat ("%s: DATES must be dates, as text or datetime", ...
                       " values; convert a datenum X with datetime (X,", ...
                       " 'ConvertFrom', 'datenum')."), scope);
      endif
      [dv, errmsg] = dateVectors (dates, format);
      if (! isempty (errmsg))
        error ("%s: DATES %s", scope, errmsg);
      endif
      if (any (dv(:,1) < 0 | dv(:,1) > 9999))
        error ("%s: DATES must fall in the years 0 to 9999.", scope);
      endif
      n = numel (this.time_);
      if (rows (dv) != n)
        error ("%s: DATES must hold one date per sample.", scope);
      endif
      if (n == 0)
        return;
      endif

      ## Times of day alone give times since midnight and no start date
      isTimeOnly = false;
      if (! isa (dates, 'datetime'))
        txt = dates;
        if (ischar (txt) || isa (txt, 'string'))
          txt = cellstr (txt);
        endif
        timeOfDay = ['^\s*\d{1,2}:\d{2}(:\d{2}(\.\d+)?)?', ...
                     '\s*([AaPp][Mm])?\s*$'];
        isTimeOnly = all (cellfun (@(x) ! isempty (regexp (x, timeOfDay, ...
                                                           'once')), txt(:)));
      endif
      if (isTimeOnly)
        ns = (dv(:,4) * 3600 + dv(:,5) * 60 + dv(:,6)) * 1e9;
        startDate = '';
      else
        [~, dv0] = dateOffsets (dv);
        startDate = tsdata.timemetadata.dateText (dv0);
        ## Counted from the start date as stored, to the millisecond
        ns = tsdata.timemetadata.dateOffset ( ...
               dv, tsdata.timemetadata.dateVector (startDate));
      endif
      time = ns / nsPerUnit (this.timeInfo_.Units);

      ## Sort the whole record, as the constructor does (MATLAB sorts the
      ## times alone and leaves each sample with another's date)
      [time, idx] = sort (time);
      this.data_ = takeSamples (this.data_, idx, this.timeDim_);
      if (! isempty (this.quality_))
        this.quality_ = takeSamples (this.quality_, idx, this.timeDim_);
      endif
      this.time_ = time;
      this.timeInfo_.TimeVector = time;
      this.timeInfo_.StartDate = startDate;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} setuniformtime (@var{ts}, @qcode{'StartTime'}, @var{start})
    ## @deftypefnx {timeseries} {@var{ts} =} setuniformtime (@var{ts}, @qcode{'Interval'}, @var{step})
    ## @deftypefnx {timeseries} {@var{ts} =} setuniformtime (@var{ts}, @qcode{'EndTime'}, @var{end})
    ## @deftypefnx {timeseries} {@var{ts} =} setuniformtime (@var{ts}, @var{name1}, @var{value1}, @var{name2}, @var{value2}, @dots{})
    ##
    ## Give a series a uniform time vector.
    ##
    ## @code{@var{ts} = setuniformtime (@var{ts}, @dots{})} replaces the time
    ## vector of the series @var{ts} by one of evenly spaced times, one per
    ## sample, in its own units; the samples are not moved.  The times are
    ## fixed by any one or two of these options, or all three when they
    ## agree, the names matched in any case and a later occurrence overriding
    ## an earlier one:
    ##
    ## @multitable @columnfractions 0.4 0.6
    ## @headitem Given @tab Times
    ## @item @qcode{'StartTime'} @tab from @var{start}, a step of 1
    ## @item @qcode{'Interval'} @tab from 0, a step of @var{step}
    ## @item @qcode{'EndTime'} @tab from 0 to @var{end}
    ## @item @qcode{'StartTime'}, @qcode{'Interval'} @tab from @var{start}, a
    ## step of @var{step}
    ## @item @qcode{'StartTime'}, @qcode{'EndTime'} @tab from @var{start} to
    ## @var{end}, both exact
    ## @item @qcode{'Interval'}, @qcode{'EndTime'} @tab to @var{end}, a step of
    ## @var{step}
    ## @end multitable
    ##
    ## Each value is a finite real scalar in the series' units, relative to
    ## @qcode{TimeInfo.StartDate} when the series has one.  A step of zero gives
    ## equal times.  A negative step, a start after the end, or an end below
    ## zero given alone are refused, since the times would run backwards;
    ## MATLAB then empties the time vector and keeps the data.  MATLAB also
    ## ignores @code{NaN}, which is refused here, and a series of one sample
    ## given equal start and end times, which MATLAB refuses, gets that time.
    ##
    ## @seealso{timeseries.setabstime, tsdata.timemetadata}
    ## @end deftypefn
    function this = setuniformtime (this, varargin)
      mustBeScalar (this, 'setuniformtime');
      scope = 'timeseries.setuniformtime';
      if (isempty (varargin))
        error (strcat ("%s: at least one of 'StartTime', 'Interval' or", ...
                       " 'EndTime' is required."), scope);
      endif
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      optNames = {'StartTime', 'Interval', 'EndTime'};
      vals = {[], [], []};
      for i = 1:2:numel (varargin)
        k = [];
        if (isText (varargin{i}))
          k = find (strcmpi (varargin{i}, optNames));
        endif
        if (isempty (k))
          error ("%s: invalid optional paired argument.", scope);
        endif
        v = varargin{i+1};
        if (! (isnumeric (v) && isreal (v) && isscalar (v)))
          error ("%s: '%s' must be a real scalar.", scope, optNames{k});
        endif
        if (! isfinite (v))
          error ("%s: '%s' must be finite.", scope, optNames{k});
        endif
        vals{k} = double (v);
      endfor
      n = numel (this.time_);
      if (n == 0)
        error ("%s: TS has no samples.", scope);
      endif
      [t0, dt, t1] = vals{:};
      given = ! cellfun (@isempty, vals);
      if (given(2) && dt < 0)
        error ("%s: 'Interval' must not be negative.", scope);
      endif
      if (given(1) && given(3) && t0 > t1)
        error ("%s: 'StartTime' must not come after 'EndTime'.", scope);
      endif
      if (isequal (given, [false, false, true]) && t1 < 0)
        error (strcat ("%s: 'EndTime' given alone must not be negative,", ...
                       " since the times start at 0."), scope);
      endif
      mismatch = strcat ("%s: 'StartTime', 'Interval' and 'EndTime' do", ...
                         " not fit the number of samples.");
      k = (0:n-1)';
      switch (bin2dec (char (given + '0')))
        case 4    # StartTime
          time = t0 + k;
        case 2    # Interval
          time = k * dt;
        case 1    # EndTime
          if (n == 1)
            error (mismatch, scope);
          endif
          time = linspace (0, t1, n)';
        case 6    # StartTime and Interval
          time = t0 + k * dt;
        case 5    # StartTime and EndTime
          if (n == 1)
            if (t0 != t1)
              error (mismatch, scope);
            endif
            time = t0;
          else
            time = linspace (t0, t1, n)';
          endif
        case 3    # Interval and EndTime
          time = t1 - flipud (k) * dt;
        otherwise # all three
          tol = 1e-12 * max ([1, abs(t0), abs(t1)]);
          if (abs (t0 + (n - 1) * dt - t1) > tol)
            error (mismatch, scope);
          endif
          if (n == 1)
            time = t0;
          else
            time = linspace (t0, t1, n)';
          endif
      endswitch
      this.time_ = time;
      this.timeInfo_.TimeVector = time;
    endfunction

  endmethods

################################################################################
##                              ** Events **                                  ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'addevent'         'delevent'         'gettsbeforeevent'                   ##
## 'gettsbeforeatevent'                  'gettsatevent'                       ##
## 'gettsafteratevent'                   'gettsafterevent'                    ##
## 'gettsbetweenevents'                                                       ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} addevent (@var{ts}, @var{event})
    ## @deftypefnx {timeseries} {@var{ts} =} addevent (@var{ts}, @var{name}, @var{time})
    ##
    ## Add events to a series.
    ##
    ## @code{@var{ts} = addevent (@var{ts}, @var{event})} appends the
    ## @code{tsdata.event} object or array @var{event} to the @qcode{Events} of
    ## the series @var{ts}, in the order given.
    ##
    ## @code{@var{ts} = addevent (@var{ts}, @var{name}, @var{time})} appends an
    ## event named @var{name} at @var{time}.  @var{name} is a character vector
    ## or a string scalar, and @var{time} a real number in the series' units,
    ## counted from its @qcode{TimeInfo.StartDate} when it has one; for a
    ## series with a start date @var{time} may also be a date, as text or a
    ## @code{datetime}.  For several events, @var{name} is a cell array of
    ## character vectors or a string array, and @var{time} a cell array, a
    ## numeric vector or a string array of as many times.
    ##
    ## Events are kept in the order they are added, not sorted; an event
    ## whose name and time match one already held is not added again, while
    ## the same name at another time is.
    ##
    ## A date on a series with no start date is refused, since the series has
    ## no calendar to place it on; MATLAB accepts it and reads the event as
    ## time 0.  A @code{NaN} time is refused too.
    ##
    ## @seealso{timeseries.delevent, tsdata.event, timeseries.gettsatevent}
    ## @end deftypefn
    function this = addevent (this, varargin)
      mustBeScalar (this, 'addevent');
      scope = 'timeseries.addevent';
      if (numel (varargin) == 1)
        events = varargin{1};
        if (! isa (events, 'tsdata.event'))
          error ("%s: EVENT must be a tsdata.event object.", scope);
        endif
        events = events(:)';
      elseif (numel (varargin) == 2)
        [names, times] = varargin{:};
        if (isText (names))
          names = {char(names)};
          times = {times};
        elseif (iscellstr (names) || isa (names, 'string'))
          names = cellstr (names);
          if (isnumeric (times) || isa (times, 'string'))
            times = num2cell (times);
            if (isa (varargin{2}, 'string'))
              times = cellfun (@char, times, 'UniformOutput', false);
            endif
          elseif (! iscell (times))
            times = {times};
          endif
          if (numel (times) != numel (names))
            error ("%s: NAME and TIME must hold as many events.", scope);
          endif
        else
          error (strcat ("%s: NAME must be a character vector, a string or", ...
                         " a cell array of character vectors."), scope);
        endif
        evs = cell (1, numel (names));
        startDate = this.timeInfo_.StartDate;
        for i = 1:numel (names)
          t = times{i};
          isDate = ischar (t) || isa (t, 'string') || isa (t, 'datetime');
          if (isDate)
            if (isempty (startDate))
              error (strcat ("%s: TS has no start date to place a dated", ...
                             " event on; set 'TimeInfo.StartDate' or use", ...
                             " setabstime first."), scope);
            endif
            try
              e = tsdata.event (names{i}, t);
            catch err
              error ("%s: TIME is not a date.", scope);
            end_try_catch
          else
            if (! (isnumeric (t) && isreal (t) && isscalar (t) && ! isnan (t)))
              error ("%s: TIME must be a real number or a date.", scope);
            endif
            e = tsdata.event (names{i}, double (t));
            e.Units = this.timeInfo_.Units;
            e.StartDate = startDate;
          endif
          evs{i} = e;
        endfor
        events = [evs{:}];
      else
        error ("%s: invalid number of input arguments.", scope);
      endif
      ## 'arrayfun' would call its function once with the whole array
      if (isempty (this.timeInfo_.StartDate))
        for i = 1:numel (events)
          if (! isempty (events(i).StartDate))
            error (strcat ("%s: TS has no start date to place a dated", ...
                           " event on; set 'TimeInfo.StartDate' or use", ...
                           " setabstime first."), scope);
          endif
        endfor
      endif
      held = this.events_;
      for i = 1:numel (events)
        isHeld = false;
        for j = 1:numel (held)
          isHeld = isHeld || sameEvent (held(j), events(i));
        endfor
        if (isHeld)
          continue;
        endif
        if (isempty (held))
          held = events(i);
        else
          ## Appended along the list as it lies, a row or a column
          held(end+1) = events(i);
        endif
      endfor
      this.events_ = held;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} delevent (@var{ts}, @var{name})
    ## @deftypefnx {timeseries} {@var{ts} =} delevent (@var{ts}, @var{name}, @var{n})
    ##
    ## Remove events from a series.
    ##
    ## @code{@var{ts} = delevent (@var{ts}, @var{name})} removes the first event
    ## named @var{name}, a character vector or a string scalar matched in case,
    ## from the @qcode{Events} of the series @var{ts}; for a cell array of
    ## names, the first event of each.
    ##
    ## @code{@var{ts} = delevent (@var{ts}, @var{name}, @var{n})} removes the
    ## @var{n}th event of each name instead, counting only the events of that
    ## name in the order they are held.
    ##
    ## A name no event has, or an @var{n} beyond the events of that name, is
    ## refused; MATLAB then removes nothing, silently.
    ##
    ## @seealso{timeseries.addevent}
    ## @end deftypefn
    function this = delevent (this, varargin)
      mustBeScalar (this, 'delevent');
      scope = 'timeseries.delevent';
      if (numel (varargin) < 1 || numel (varargin) > 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      names = varargin{1};
      if (isText (names))
        names = {char(names)};
      elseif (! (iscellstr (names) || isa (names, 'string')))
        error (strcat ("%s: NAME must be a character vector, a string or a", ...
                       " cell array of character vectors."), scope);
      endif
      names = cellstr (names);
      n = 1;
      if (numel (varargin) == 2)
        n = varargin{2};
      endif
      drop = zeros (1, numel (names));
      for i = 1:numel (names)
        [drop(i), errmsg] = findEvent (this.events_, names{i}, n);
        if (! isempty (errmsg))
          error ("%s: %s", scope, errmsg);
        endif
      endfor
      keep = true (1, numel (this.events_));
      keep(drop) = false;
      if (any (keep))
        this.events_ = this.events_(keep);
      else
        this.events_ = [];
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts2} =} gettsbeforeevent (@var{ts}, @var{event})
    ## @deftypefnx {timeseries} {@var{ts2} =} gettsbeforeevent (@var{ts}, @var{event}, @var{n})
    ##
    ## Return a series of the samples before the event.
    ##
    ## @code{@var{ts2} = gettsbeforeevent (@var{ts}, @var{event})} returns the
    ## series @var{ts} holding only the samples whose time is strictly before
    ## the time of @var{event}, compared exactly.  @var{event} is the name of an
    ## event of @var{ts}, a character vector or a string scalar matched in case,
    ## which selects the first event of that name; or a @code{tsdata.event}
    ## object, which need not belong to @var{ts}.
    ##
    ## @code{@var{ts2} = gettsbeforeevent (@var{ts}, @var{event}, @var{n})} uses
    ## the @var{n}th event named @var{event}, counting only the events of that
    ## name in the order they are held.  For a @code{tsdata.event} object
    ## @var{n} can only be 1.  A name no event has, or an @var{n} beyond the
    ## events of that name, is refused; MATLAB then returns an empty series.  So
    ## is a dated event on a series with no start date, which MATLAB reads as
    ## time 0.
    ##
    ## @seealso{timeseries.gettsbeforeatevent, timeseries.gettsatevent,
    ## timeseries.gettsafteratevent, timeseries.gettsafterevent,
    ## timeseries.gettsbetweenevents, timeseries.addevent}
    ## @end deftypefn
    function this = gettsbeforeevent (this, varargin)
      mustBeScalar (this, 'gettsbeforeevent');
      t = eventQueryTime (this, 'gettsbeforeevent', varargin{:});
      this = subset (this, find (this.time_ < t));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts2} =} gettsbeforeatevent (@var{ts}, @var{event})
    ## @deftypefnx {timeseries} {@var{ts2} =} gettsbeforeatevent (@var{ts}, @var{event}, @var{n})
    ##
    ## Return a series of the samples before and at the event.
    ##
    ## @code{@var{ts2} = gettsbeforeatevent (@var{ts}, @var{event})} returns the
    ## series @var{ts} holding only the samples whose time is before or at the
    ## time of @var{event}, compared exactly.  @var{event} is the name of an
    ## event of @var{ts}, a character vector or a string scalar matched in case,
    ## which selects the first event of that name; or a @code{tsdata.event}
    ## object, which need not belong to @var{ts}.
    ##
    ## @code{@var{ts2} = gettsbeforeatevent (@var{ts}, @var{event}, @var{n})}
    ## uses the @var{n}th event named @var{event}, counting only the events of
    ## that name in the order they are held.  For a @code{tsdata.event} object
    ## @var{n} can only be 1.  A name no event has, or an @var{n} beyond the
    ## events of that name, is refused; MATLAB then returns an empty series.  So
    ## is a dated event on a series with no start date, which MATLAB reads as
    ## time 0.
    ##
    ## @seealso{timeseries.gettsbeforeevent, timeseries.gettsatevent,
    ## timeseries.gettsafteratevent, timeseries.gettsafterevent,
    ## timeseries.gettsbetweenevents, timeseries.addevent}
    ## @end deftypefn
    function this = gettsbeforeatevent (this, varargin)
      mustBeScalar (this, 'gettsbeforeatevent');
      t = eventQueryTime (this, 'gettsbeforeatevent', varargin{:});
      this = subset (this, find (this.time_ <= t));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts2} =} gettsatevent (@var{ts}, @var{event})
    ## @deftypefnx {timeseries} {@var{ts2} =} gettsatevent (@var{ts}, @var{event}, @var{n})
    ##
    ## Return a series of the samples at the event.
    ##
    ## @code{@var{ts2} = gettsatevent (@var{ts}, @var{event})} returns the
    ## series @var{ts} holding only the samples whose time is at the time of
    ## @var{event}, compared exactly.  @var{event} is the name of an event of
    ## @var{ts}, a character vector or a string scalar matched in case, which
    ## selects the first event of that name; or a @code{tsdata.event} object,
    ## which need not belong to @var{ts}.
    ##
    ## @code{@var{ts2} = gettsatevent (@var{ts}, @var{event}, @var{n})} uses the
    ## @var{n}th event named @var{event}, counting only the events of that name
    ## in the order they are held.  For a @code{tsdata.event} object @var{n} can
    ## only be 1.  A name no event has, or an @var{n} beyond the events of that
    ## name, is refused; MATLAB then returns an empty series.  So is a dated
    ## event on a series with no start date, which MATLAB reads as time 0.
    ##
    ## @seealso{timeseries.gettsbeforeevent, timeseries.gettsbeforeatevent,
    ## timeseries.gettsafteratevent, timeseries.gettsafterevent,
    ## timeseries.gettsbetweenevents, timeseries.addevent}
    ## @end deftypefn
    function this = gettsatevent (this, varargin)
      mustBeScalar (this, 'gettsatevent');
      t = eventQueryTime (this, 'gettsatevent', varargin{:});
      this = subset (this, find (this.time_ == t));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts2} =} gettsafteratevent (@var{ts}, @var{event})
    ## @deftypefnx {timeseries} {@var{ts2} =} gettsafteratevent (@var{ts}, @var{event}, @var{n})
    ##
    ## Return a series of the samples at and after the event.
    ##
    ## @code{@var{ts2} = gettsafteratevent (@var{ts}, @var{event})} returns the
    ## series @var{ts} holding only the samples whose time is at or after the
    ## time of @var{event}, compared exactly.  @var{event} is the name of an
    ## event of @var{ts}, a character vector or a string scalar matched in case,
    ## which selects the first event of that name; or a @code{tsdata.event}
    ## object, which need not belong to @var{ts}.
    ##
    ## @code{@var{ts2} = gettsafteratevent (@var{ts}, @var{event}, @var{n})}
    ## uses the @var{n}th event named @var{event}, counting only the events of
    ## that name in the order they are held.  For a @code{tsdata.event} object
    ## @var{n} can only be 1.  A name no event has, or an @var{n} beyond the
    ## events of that name, is refused; MATLAB then returns an empty series.  So
    ## is a dated event on a series with no start date, which MATLAB reads as
    ## time 0.
    ##
    ## @seealso{timeseries.gettsbeforeevent, timeseries.gettsbeforeatevent,
    ## timeseries.gettsatevent, timeseries.gettsafterevent,
    ## timeseries.gettsbetweenevents, timeseries.addevent}
    ## @end deftypefn
    function this = gettsafteratevent (this, varargin)
      mustBeScalar (this, 'gettsafteratevent');
      t = eventQueryTime (this, 'gettsafteratevent', varargin{:});
      this = subset (this, find (this.time_ >= t));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts2} =} gettsafterevent (@var{ts}, @var{event})
    ## @deftypefnx {timeseries} {@var{ts2} =} gettsafterevent (@var{ts}, @var{event}, @var{n})
    ##
    ## Return a series of the samples after the event.
    ##
    ## @code{@var{ts2} = gettsafterevent (@var{ts}, @var{event})} returns the
    ## series @var{ts} holding only the samples whose time is strictly after the
    ## time of @var{event}, compared exactly.  @var{event} is the name of an
    ## event of @var{ts}, a character vector or a string scalar matched in case,
    ## which selects the first event of that name; or a @code{tsdata.event}
    ## object, which need not belong to @var{ts}.
    ##
    ## @code{@var{ts2} = gettsafterevent (@var{ts}, @var{event}, @var{n})} uses
    ## the @var{n}th event named @var{event}, counting only the events of that
    ## name in the order they are held.  For a @code{tsdata.event} object
    ## @var{n} can only be 1.  A name no event has, or an @var{n} beyond the
    ## events of that name, is refused; MATLAB then returns an empty series.  So
    ## is a dated event on a series with no start date, which MATLAB reads as
    ## time 0.
    ##
    ## @seealso{timeseries.gettsbeforeevent, timeseries.gettsbeforeatevent,
    ## timeseries.gettsatevent, timeseries.gettsafteratevent,
    ## timeseries.gettsbetweenevents, timeseries.addevent}
    ## @end deftypefn
    function this = gettsafterevent (this, varargin)
      mustBeScalar (this, 'gettsafterevent');
      t = eventQueryTime (this, 'gettsafterevent', varargin{:});
      this = subset (this, find (this.time_ > t));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts2} =} gettsbetweenevents (@var{ts}, @var{event1}, @var{event2})
    ## @deftypefnx {timeseries} {@var{ts2} =} gettsbetweenevents (@var{ts}, @var{event1}, @var{event2}, @var{n1}, @var{n2})
    ##
    ## Return a series of the samples between two events.
    ##
    ## @code{@var{ts2} = gettsbetweenevents (@var{ts}, @var{event1},
    ## @var{event2})} returns the series @var{ts} holding only the samples from
    ## the time of @var{event1} to the time of @var{event2}, both included; none
    ## when @var{event2} comes before @var{event1}.  Each event is a name or a
    ## @code{tsdata.event} object, as for @code{gettsafterevent}.
    ##
    ## @code{@var{ts2} = gettsbetweenevents (@var{ts}, @var{event1},
    ## @var{event2}, @var{n1}, @var{n2})} uses the @var{n1}th event named
    ## @var{event1} and the @var{n2}th named @var{event2}; for an event given
    ## as a @code{tsdata.event} object its number can only be 1.  Names and
    ## numbers no event has are refused, as for @code{gettsafterevent}.
    ##
    ## @seealso{timeseries.gettsafterevent, timeseries.gettsbeforeevent,
    ## timeseries.addevent}
    ## @end deftypefn
    function this = gettsbetweenevents (this, varargin)
      mustBeScalar (this, 'gettsbetweenevents');
      scope = 'gettsbetweenevents';
      if (numel (varargin) == 2)
        t1 = eventQueryTime (this, scope, varargin{1});
        t2 = eventQueryTime (this, scope, varargin{2});
      elseif (numel (varargin) == 4)
        t1 = eventQueryTime (this, scope, varargin{1}, varargin{3});
        t2 = eventQueryTime (this, scope, varargin{2}, varargin{4});
      else
        error ("timeseries.%s: invalid number of input arguments.", scope);
      endif
      this = subset (this, find (this.time_ >= t1 & this.time_ <= t2));
    endfunction

  endmethods

################################################################################
##               ** Interpolation, resampling, synchronising **               ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'getinterpmethod'  'setinterpmethod'  'resample'         'synchronize'     ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{name} =} getinterpmethod (@var{ts})
    ##
    ## Return the name of the interpolation method.
    ##
    ## @code{@var{name} = getinterpmethod (@var{ts})} returns
    ## @qcode{'linear'}, @qcode{'zoh'} or @qcode{'myFuncHandle'}, the
    ## @qcode{Name} of @qcode{@var{ts}.DataInfo.Interpolation}.
    ##
    ## @seealso{timeseries.setinterpmethod, tsdata.interpolation}
    ## @end deftypefn
    function name = getinterpmethod (this)
      mustBeScalar (this, 'getinterpmethod');
      name = this.dataInfo_.Interpolation.Name;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} setinterpmethod (@var{ts}, @var{name})
    ## @deftypefnx {timeseries} {@var{ts} =} setinterpmethod (@var{ts}, @var{fh})
    ## @deftypefnx {timeseries} {@var{ts} =} setinterpmethod (@var{ts}, @var{ip})
    ##
    ## Set the interpolation method.
    ##
    ## @code{@var{ts} = setinterpmethod (@var{ts}, @var{name})} sets the method
    ## @code{resample} and @code{synchronize} use to @var{name},
    ## @qcode{'linear'} or @qcode{'zoh'} (zero-order hold, keeping each value
    ## until the next sample), matched in any case and stored in lower case.
    ## MATLAB stores any name, @qcode{'cubic'} included, and fails only on
    ## resampling; here another name is refused.
    ##
    ## @code{@var{ts} = setinterpmethod (@var{ts}, @var{fh})} sets a method
    ## evaluated by the function handle @var{fh}, called as MATLAB calls it:
    ## @code{@var{newData} = @var{fh} (@var{newTime}, @var{oldTime},
    ## @var{oldData})}, with the times as columns and the data with time along
    ## its first dimension, so three-dimensional data is passed permuted.
    ##
    ## @code{@var{ts} = setinterpmethod (@var{ts}, @var{ip})} sets the method
    ## held by the @code{tsdata.interpolation} object @var{ip}.
    ##
    ## @seealso{timeseries.getinterpmethod, timeseries.resample,
    ## tsdata.interpolation}
    ## @end deftypefn
    function this = setinterpmethod (this, method)
      mustBeScalar (this, 'setinterpmethod');
      scope = 'timeseries.setinterpmethod';
      if (nargin < 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (isText (method))
        method = lower (char (method));
        if (! any (strcmp (method, {'linear', 'zoh'})))
          error ("%s: METHOD must be 'linear' or 'zoh': '%s'", scope, method);
        endif
        method = tsdata.interpolation (method);
      elseif (isa (method, 'function_handle') && isscalar (method))
        method = tsdata.interpolation (method);
      elseif (! (isa (method, 'tsdata.interpolation') && isscalar (method)))
        error (strcat ("%s: METHOD must be 'linear', 'zoh', a function", ...
                       " handle or a tsdata.interpolation object."), scope);
      endif
      this.dataInfo_.Interpolation = method;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} resample (@var{ts}, @var{time})
    ## @deftypefnx {timeseries} {@var{ts} =} resample (@var{ts}, @var{time}, @var{method})
    ## @deftypefnx {timeseries} {@var{ts} =} resample (@var{ts}, @var{time}, @var{method}, @var{code})
    ##
    ## Evaluate a series at new times.
    ##
    ## @code{@var{ts} = resample (@var{ts}, @var{time})} returns the series
    ## @var{ts} with its samples replaced by its values at @var{time}, found
    ## with its own interpolation method.  @var{time} is numeric, in the
    ## series' units; for a series with a @qcode{TimeInfo.StartDate} it may
    ## also be dates, as text or a @code{datetime}, and numbers count from the
    ## start date.  The new times are sorted, repeats kept.  Samples that are
    ## @code{NaN} are left out before interpolating, as in MATLAB; at a time
    ## outside the series the data is @code{NaN}.  The class of the data is
    ## kept.  Every property but the samples is kept.
    ##
    ## @code{@var{ts} = resample (@var{ts}, @var{time}, @var{method})} uses
    ## @var{method}, @qcode{'linear'} or @qcode{'zoh'} in any case, instead;
    ## @code{[]} keeps the series' own.
    ##
    ## @code{@var{ts} = resample (@var{ts}, @var{time}, @var{method},
    ## @var{code})} gives the quality code @var{code}, an integer listed in
    ## @qcode{QualityInfo.Code}, to every new time the series did not already
    ## hold.  @code{[]} gives none.
    ##
    ## A series with quality codes gives each other new time the code of the
    ## nearest sample, a tie going to the later, as MATLAB does; under
    ## @qcode{'zoh'} a value takes the code of the sample it is held from,
    ## where MATLAB takes the nearest one's, which may be the next sample's.
    ## Outside the series a code is the nearest sample's too, where MATLAB
    ## raises for a series with codes.  Custom methods are called on the data
    ## only.
    ##
    ## @seealso{timeseries.synchronize, timeseries.setinterpmethod}
    ## @end deftypefn
    function this = resample (this, varargin)
      mustBeScalar (this, 'resample');
      scope = 'timeseries.resample';
      if (numel (varargin) < 1 || numel (varargin) > 3)
        error ("%s: invalid number of input arguments.", scope);
      endif
      method = '';
      code = [];
      if (numel (varargin) >= 2)
        method = varargin{2};
      endif
      if (numel (varargin) == 3)
        code = varargin{3};
      endif
      if (isempty (this.time_) && isempty (this.data_))
        return;
      endif
      [t, ~, errmsg] = relativeTime (this, varargin{1}, false);
      if (! isempty (errmsg))
        error ("%s: TIME %s", scope, errmsg);
      endif
      if (! all (isfinite (t)))
        error ("%s: TIME must be finite.", scope);
      endif
      [method, errmsg] = interpName (method);
      if (! isempty (errmsg))
        error ("%s: METHOD %s", scope, errmsg);
      endif
      [this, errmsg] = resampleAt (this, sort (t), method, code);
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {[@var{ts1}, @var{ts2}] =} synchronize (@var{ts1}, @var{ts2}, @var{method})
    ## @deftypefnx {timeseries} {[@var{ts1}, @var{ts2}] =} synchronize (@dots{}, @var{name}, @var{value})
    ##
    ## Resample two series onto common times.
    ##
    ## @code{[@var{ts1}, @var{ts2}] = synchronize (@var{ts1}, @var{ts2},
    ## @var{method})} resamples both series onto one set of times within the
    ## span both cover, chosen by @var{method}, matched in any case:
    ##
    ## @table @asis
    ## @item @qcode{'union'}
    ## every time of either series in that span;
    ## @item @qcode{'intersection'}
    ## the times both series hold, within the tolerance;
    ## @item @qcode{'uniform'}
    ## times from the start of the span, a step of @qcode{'Interval'} apart.
    ## @end table
    ##
    ## Series that do not overlap become series with no samples.  Each keeps its
    ## own units, the times converted exactly; MATLAB converts them with errors
    ## near the tenth digit.  Dated series are placed by their start dates, and
    ## both results count from the earlier of the two unless
    ## @qcode{'KeepOriginalTimes'} is @code{true}.  A dated series with an
    ## undated one is refused, since the undated one has no calendar; MATLAB
    ## then reads both as undated.
    ##
    ## The options, matched in any case:
    ##
    ## @table @asis
    ## @item @qcode{'Interval'}
    ## the step of @qcode{'uniform'}, a positive number in the first series'
    ## units, 1 by default; MATLAB accepts zero and negative steps and gives one
    ## sample or none.
    ## @item @qcode{'InterpMethod'}
    ## @qcode{'linear'} or @qcode{'zoh'}, for both series; by default each uses
    ## its own.
    ## @item @qcode{'QualityCode'}
    ## a quality code given to the times each series did not already hold, as
    ## for @code{resample}.
    ## @item @qcode{'KeepOriginalTimes'}
    ## @code{true} to keep each dated series on its own start date.
    ## @item @qcode{'tolerance'}
    ## how close two times must be to count as one, in the first series'
    ## units, @code{1e-10} by default; the second series' time is kept.  This
    ## is the documented meaning; MATLAB ignores any tolerance above about
    ## @code{1e-10}, so there @code{0.1} matches no times @code{0.05} apart,
    ## while here it matches them.
    ## @end table
    ##
    ## Quality codes follow the rules of @code{resample}.
    ##
    ## @seealso{timeseries.resample, timeseries.append}
    ## @end deftypefn
    function [ts1, ts2] = synchronize (ts1, ts2, varargin)
      scope = 'timeseries.synchronize';
      if (nargin < 3)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (! isa (ts1, 'timeseries'))
        error ("%s: TS1 must be a timeseries.", scope);
      endif
      if (! isa (ts2, 'timeseries'))
        error ("%s: TS2 must be a timeseries.", scope);
      endif
      mustBeScalar (ts1, 'synchronize');
      mustBeScalar (ts2, 'synchronize');
      how = varargin{1};
      methods = {'union', 'intersection', 'uniform'};
      if (! (isText (how) && any (strcmpi (how, methods))))
        error (strcat ("%s: METHOD must be 'union', 'intersection' or", ...
                       " 'uniform'."), scope);
      endif
      how = lower (char (how));
      opts = varargin(2:end);
      if (mod (numel (opts), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      optNames = {'Interval', 'InterpMethod', 'QualityCode', ...
                  'KeepOriginalTimes', 'tolerance'};
      vals = {1, '', [], false, 1e-10};
      for i = 1:2:numel (opts)
        k = [];
        if (isText (opts{i}))
          k = find (strcmpi (opts{i}, optNames));
        endif
        if (isempty (k))
          error ("%s: invalid optional paired argument.", scope);
        endif
        vals{k} = opts{i+1};
      endfor
      [interval, method, code, keep, tol] = vals{:};
      if (! (isnumeric (interval) && isreal (interval) && isscalar (interval)
             && isfinite (interval) && interval > 0))
        error ("%s: 'Interval' must be a positive number.", scope);
      endif
      [method, errmsg] = interpName (method);
      if (! isempty (errmsg))
        error ("%s: 'InterpMethod' %s", scope, errmsg);
      endif
      if (! ((islogical (keep) || isnumeric (keep)) && isscalar (keep)
             && any (keep == [0, 1])))
        error ("%s: 'KeepOriginalTimes' must be a logical scalar.", scope);
      endif
      if (! (isnumeric (tol) && isreal (tol) && isscalar (tol) && tol >= 0))
        error ("%s: 'tolerance' must be a non-negative number.", scope);
      endif
      if (isempty (ts1.time_) || isempty (ts2.time_))
        error ("%s: TS1 and TS2 must hold samples.", scope);
      endif
      sd1 = ts1.timeInfo_.StartDate;
      sd2 = ts2.timeInfo_.StartDate;
      if (isempty (sd1) != isempty (sd2))
        error (strcat ("%s: one series has a start date and the other has", ...
                       " none; give the other one with setabstime or", ...
                       " 'TimeInfo.StartDate'."), scope);
      endif

      ## Both on one scale: nanoseconds from the earlier start date
      ns1 = nsPerUnit (ts1.timeInfo_.Units);
      ns2 = nsPerUnit (ts2.timeInfo_.Units);
      off1 = 0;
      off2 = 0;
      refDate = '';
      if (! isempty (sd1))
        [off, ~] = dateOffsets ([tsdata.timemetadata.dateVector(sd1);
                                  tsdata.timemetadata.dateVector(sd2)]);
        off1 = off(1);
        off2 = off(2);
        if (off1 == 0)
          refDate = sd1;
        else
          refDate = sd2;
        endif
      endif
      t1 = ts1.time_ * ns1 + off1;
      t2 = ts2.time_ * ns2 + off2;
      tolNs = tol * ns1;
      lo = max (t1(1), t2(1));
      hi = min (t1(end), t2(end));
      if (lo > hi + tolNs)
        grid = zeros (0, 1);
      else
        in1 = t1(t1 >= lo - tolNs & t1 <= hi + tolNs);
        in2 = t2(t2 >= lo - tolNs & t2 <= hi + tolNs);
        switch (how)
          case 'union'
            grid = in2;
            for i = 1:numel (in1)
              if (! any (abs (in2 - in1(i)) <= tolNs))
                grid(end+1,1) = in1(i);
              endif
            endfor
          case 'intersection'
            grid = zeros (0, 1);
            for i = 1:numel (in1)
              k = find (abs (in2 - in1(i)) <= tolNs, 1);
              if (! isempty (k))
                grid(end+1,1) = in2(k);
              endif
            endfor
          otherwise
            step = interval * ns1;
            grid = lo + (0:floor ((hi - lo) / step + 1e-9))' * step;
        endswitch
        grid = unique (grid);
      endif

      ## Series that do not overlap become series with no samples
      if (isempty (grid))
        ts1 = subset (ts1, []);
        ts2 = subset (ts2, []);
        if (! isempty (refDate) && ! keep)
          ts1.timeInfo_.StartDate = refDate;
          ts2.timeInfo_.StartDate = refDate;
        endif
        return;
      endif

      ## Each series evaluated in its own frame: at its own time where the
      ## grid holds that time, or matches it within the tolerance.  The
      ## results are labelled with the grid times, counted from the earlier
      ## start date unless each keeps its own.
      label1 = off1;
      label2 = off2;
      if (! isempty (refDate) && ! keep)
        label1 = 0;
        label2 = 0;
      endif
      [at1, lab1] = frameTimes (grid, t1, ts1.time_, off1, ns1, tolNs, label1);
      [at2, lab2] = frameTimes (grid, t2, ts2.time_, off2, ns2, tolNs, label2);
      [ts1, errmsg] = resampleAt (ts1, at1, method, code, true);
      if (! isempty (errmsg))
        error ("%s: %s", scope, regexprep (errmsg, '^TS ', 'TS1 '));
      endif
      [ts2, errmsg] = resampleAt (ts2, at2, method, code, true);
      if (! isempty (errmsg))
        error ("%s: %s", scope, regexprep (errmsg, '^TS ', 'TS2 '));
      endif
      ts1.time_ = lab1;
      ts2.time_ = lab2;
      ts1.timeInfo_.TimeVector = lab1;
      ts2.timeInfo_.TimeVector = lab2;
      if (! isempty (refDate) && ! keep)
        ts1.timeInfo_.StartDate = refDate;
        ts2.timeInfo_.StartDate = refDate;
      endif
    endfunction

  endmethods

################################################################################
##                              ** Statistics **                              ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'mean'             'median'           'mode'             'std'             ##
## 'var'              'sum'              'min'              'max'             ##
## 'iqr'              'getqualitydesc'                                        ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} mean (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} mean (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the mean of a series over time.
    ##
    ## @code{@var{v} = mean (@var{ts})} returns the mean of each data element
    ## over time, so @var{v} has the size of one sample; with @qcode{'time'}
    ## weighting it is the time-weighted mean.  @var{v} is @code{double}
    ## whatever the class of the data, and @code{[]} for a series with no
    ## samples.
    ##
    ## @code{@var{v} = mean (@var{ts}, @var{name}, @var{value}, @dots{})} takes
    ## these options, which all nine statistics share, the names matched in any
    ## case:
    ##
    ## @table @asis
    ## @item @qcode{'Quality'}
    ## the quality codes of samples to leave out, as missing; a value left
    ## without samples is @code{NaN}, a sum 0.  A code no sample has leaves
    ## nothing out, with a warning.
    ## @item @qcode{'MissingData'}
    ## @qcode{'remove'}, the default, leaves missing values out;
    ## @qcode{'interpolate'} fills those between samples linearly in time
    ## first.
    ## @item @qcode{'Weighting'}
    ## @qcode{'none'}, the default, or @qcode{'time'}, which weights each sample
    ## by the time it covers: an interior sample by the mean of its two
    ## intervals, an end sample by its one interval, the weights scaled to a
    ## mean of 1, so that uniform time changes nothing.
    ## @end table
    ##
    ## @code{NaN} is missing while @qcode{TreatNaNasMissing} is @code{true};
    ## when it is @code{false}, @code{mean}, @code{median}, @code{std},
    ## @code{var} and @code{sum} give @code{NaN} where the data holds one,
    ## while @code{min}, @code{max}, @code{mode} and @code{iqr} still skip it,
    ## as in MATLAB.
    ##
    ## MATLAB applies @qcode{'time'} weighting by multiplying the data by the
    ## weights and taking the ordinary statistic of the product, which is
    ## right for @code{mean} and @code{sum} only: there the minimum of
    ## @code{[1 2 4 8 16]} at times @code{[0 1 3 7 8]} is 0.5556.  Here each
    ## statistic is the time-weighted one: 1 for that minimum.  MATLAB also
    ## fails on logical data, and in @code{std}, @code{var} and @code{iqr} on
    ## integer data, which here are computed in @code{double}.
    ##
    ## @seealso{timeseries.median, timeseries.std, timeseries.sum}
    ## @end deftypefn
    function v = mean (this, varargin)
      v = statistic (this, 'mean', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} median (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} median (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the median of a series over time.
    ##
    ## @code{@var{v} = median (@var{ts})} returns the median of each data
    ## element over time, so @var{v} has the size of one sample; with
    ## @qcode{'time'} weighting it is the time-weighted median.  @var{v} keeps
    ## the class of the data, and is @code{[]} for a series with no samples.
    ## Where a value is left without samples it is @code{NaN}, and @var{v} is
    ## then @code{double}, as integer and logical classes hold no @code{NaN}.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.mean, timeseries.mode, timeseries.iqr}
    ## @end deftypefn
    function v = median (this, varargin)
      v = statistic (this, 'median', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} mode (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} mode (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the mode of a series over time.
    ##
    ## @code{@var{v} = mode (@var{ts})} returns the most frequent value of each
    ## data element over time, the smallest on a tie, so @var{v} has the size
    ## of one sample; with @qcode{'time'} weighting it is the value of most
    ## total weight.  @var{v} keeps the class of the data, and is @code{[]} for
    ## a series with no samples.  Where a value is left without samples it is
    ## @code{NaN}, and @var{v} is then @code{double}, as integer and logical
    ## classes hold no @code{NaN}.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.mean, timeseries.median}
    ## @end deftypefn
    function v = mode (this, varargin)
      v = statistic (this, 'mode', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} std (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} std (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the standard deviation of a series over time.
    ##
    ## @code{@var{v} = std (@var{ts})} returns the standard deviation of each
    ## data element over time, normalised by @math{N-1}, so @var{v} has the
    ## size of one sample; with @qcode{'time'} weighting it is taken about the
    ## time-weighted mean.  @var{v} is @code{double} whatever the class of the
    ## data, and @code{[]} for a series with no samples.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.mean, timeseries.var}
    ## @end deftypefn
    function v = std (this, varargin)
      v = statistic (this, 'std', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} var (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} var (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the variance of a series over time.
    ##
    ## @code{@var{v} = var (@var{ts})} returns the variance of each data
    ## element over time, normalised by @math{N-1}, so @var{v} has the size of
    ## one sample; with @qcode{'time'} weighting it is taken about the
    ## time-weighted mean.  @var{v} is @code{double} whatever the class of the
    ## data, and @code{[]} for a series with no samples.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.mean, timeseries.std}
    ## @end deftypefn
    function v = var (this, varargin)
      v = statistic (this, 'var', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} sum (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} sum (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the sum of a series over time.
    ##
    ## @code{@var{v} = sum (@var{ts})} returns the sum of each data element
    ## over time, so @var{v} has the size of one sample; with @qcode{'time'}
    ## weighting it is the sum of the weighted samples.  @var{v} is
    ## @code{double} whatever the class of the data, and @code{[]} for a series
    ## with no samples.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.mean}
    ## @end deftypefn
    function v = sum (this, varargin)
      v = statistic (this, 'sum', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} min (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} min (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the minimum of a series over time.
    ##
    ## @code{@var{v} = min (@var{ts})} returns the smallest value of each data
    ## element over time, so @var{v} has the size of one sample; weighting
    ## leaves it unchanged.  @var{v} keeps the class of the data, and is
    ## @code{[]} for a series with no samples.  Where a value is left without
    ## samples it is @code{NaN}, and @var{v} is then @code{double}, as integer
    ## and logical classes hold no @code{NaN}.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.max, timeseries.mean}
    ## @end deftypefn
    function v = min (this, varargin)
      v = statistic (this, 'min', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} max (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} max (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the maximum of a series over time.
    ##
    ## @code{@var{v} = max (@var{ts})} returns the largest value of each data
    ## element over time, so @var{v} has the size of one sample; weighting
    ## leaves it unchanged.  @var{v} keeps the class of the data, and is
    ## @code{[]} for a series with no samples.  Where a value is left without
    ## samples it is @code{NaN}, and @var{v} is then @code{double}, as integer
    ## and logical classes hold no @code{NaN}.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.min, timeseries.mean}
    ## @end deftypefn
    function v = max (this, varargin)
      v = statistic (this, 'max', varargin{:});
    endfunction


    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{v} =} iqr (@var{ts})
    ## @deftypefnx {timeseries} {@var{v} =} iqr (@var{ts}, @var{name}, @var{value}, @dots{})
    ##
    ## Return the interquartile range of a series over time.
    ##
    ## @code{@var{v} = iqr (@var{ts})} returns the interquartile range of each
    ## data element over time, the quartiles placed by the @code{(k - 0.5)/n}
    ## rule, so @var{v} has the size of one sample; with @qcode{'time'}
    ## weighting the cumulative weight takes the place of @var{k}.  @var{v} is
    ## @code{double} whatever the class of the data, and @code{[]} for a series
    ## with no samples.
    ##
    ## The options, and where MATLAB differs, are given under
    ## @code{timeseries.mean}.
    ##
    ## @seealso{timeseries.median, timeseries.mean}
    ## @end deftypefn
    function v = iqr (this, varargin)
      v = statistic (this, 'iqr', varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{desc} =} getqualitydesc (@var{ts})
    ##
    ## Return the description of each quality code.
    ##
    ## @code{@var{desc} = getqualitydesc (@var{ts})} returns a cell array the
    ## size of @qcode{@var{ts}.Quality} holding, for each code, its description
    ## in @qcode{@var{ts}.QualityInfo}: the element of @qcode{Description} at
    ## the position of the code in @qcode{Code}.  A series without quality
    ## codes gives @code{@{@}}.  A code with no description is refused.
    ##
    ## @seealso{tsdata.qualmetadata}
    ## @end deftypefn
    function desc = getqualitydesc (this)
      mustBeScalar (this, 'getqualitydesc');
      q = this.quality_;
      if (isempty (q))
        desc = {};
        return;
      endif
      codes = this.qualityInfo_.Code;
      text = this.qualityInfo_.Description;
      desc = cell (size (q));
      for i = 1:numel (q)
        k = find (codes == q(i), 1);
        if (isempty (k) || ! iscell (text) || k > numel (text))
          error (strcat ("timeseries.getqualitydesc: quality code %d", ...
                         " has no description in 'QualityInfo'."), q(i));
        endif
        desc{i} = text{k};
      endfor
    endfunction

  endmethods

################################################################################
##                     ** Arithmetic and comparison **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'plus'             'minus'            'times'            'mtimes'          ##
## 'rdivide'          'ldivide'          'mrdivide'         'mldivide'        ##
## 'eq'               'isequal'          'isequalwithequalnans'               ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} plus (@var{a}, @var{b})
    ##
    ## The sum of series, @code{@var{a} + @var{b}}.
    ##
    ## @code{@var{ts} = plus (@var{a}, @var{b})} returns the sum of @var{a} and
    ## @var{b}, sample by sample.  One of them may be a number: a scalar, or an
    ## array the size of one sample.
    ##
    ## Two series must hold as many samples, of sizes the operator accepts,
    ## at times in the same units.  Two dated series must be at the same
    ## instants; two undated ones may also differ by a constant offset, and
    ## the result then takes the times of @var{a}, with a warning.  A dated
    ## series and an undated one are refused.  The result is named
    ## @qcode{'unnamed'} and takes its other properties from @var{a}, with the
    ## events of both, no quality codes and linear interpolation;
    ## @qcode{DataInfo.Units} is kept where both series have the same.  With a
    ## number, every property of the series is kept.  The data is
    ## @code{double} whatever its class.
    ##
    ## @seealso{timeseries.minus, timeseries.times, timeseries.mtimes}
    ## @end deftypefn
    function ts = plus (a, b)
      ts = arithmetic (a, b, 'plus');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} minus (@var{a}, @var{b})
    ##
    ## The difference of series, @code{@var{a} - @var{b}}.
    ##
    ## @code{@var{ts} = minus (@var{a}, @var{b})} returns the difference of
    ## @var{a} and @var{b}, sample by sample.  One of them may be a number: a
    ## scalar, or an array the size of one sample.
    ##
    ## The rules for two series are given under @code{timeseries.plus}.
    ##
    ## @seealso{timeseries.plus}
    ## @end deftypefn
    function ts = minus (a, b)
      ts = arithmetic (a, b, 'minus');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} times (@var{a}, @var{b})
    ##
    ## The element-wise product of series, @code{@var{a} .* @var{b}}.
    ##
    ## @code{@var{ts} = times (@var{a}, @var{b})} returns the element-wise
    ## product of @var{a} and @var{b}, sample by sample.  One of them may be a
    ## number: a scalar, or an array the size of one sample.
    ##
    ## The rules for two series are given under @code{timeseries.plus},
    ## save that @qcode{DataInfo.Units} is dropped, since a product or
    ## quotient of units is the unit of neither; MATLAB keeps it where both
    ## series have the same.
    ##
    ## @seealso{timeseries.mtimes, timeseries.plus}
    ## @end deftypefn
    function ts = times (a, b)
      ts = arithmetic (a, b, 'times');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} mtimes (@var{a}, @var{b})
    ##
    ## The matrix product of series, @code{@var{a} * @var{b}}.
    ##
    ## @code{@var{ts} = mtimes (@var{a}, @var{b})} returns the matrix product
    ## of @var{a} and @var{b}, sample by sample.  One of them may be a number
    ## whose size fits the product with one sample.
    ##
    ## The rules for two series are given under @code{timeseries.plus},
    ## save that @qcode{DataInfo.Units} is dropped, since a product or
    ## quotient of units is the unit of neither; MATLAB keeps it where both
    ## series have the same.
    ##
    ## @seealso{timeseries.times, timeseries.plus}
    ## @end deftypefn
    function ts = mtimes (a, b)
      ts = arithmetic (a, b, 'mtimes');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} rdivide (@var{a}, @var{b})
    ##
    ## The element-wise quotient of series, @code{@var{a} ./ @var{b}}.
    ##
    ## @code{@var{ts} = rdivide (@var{a}, @var{b})} returns the element-wise
    ## quotient of @var{a} and @var{b}, sample by sample.  One of them may be a
    ## number: a scalar, or an array the size of one sample.
    ##
    ## The rules for two series are given under @code{timeseries.plus},
    ## save that @qcode{DataInfo.Units} is dropped, since a product or
    ## quotient of units is the unit of neither; MATLAB keeps it where both
    ## series have the same.
    ##
    ## @seealso{timeseries.ldivide, timeseries.mrdivide}
    ## @end deftypefn
    function ts = rdivide (a, b)
      ts = arithmetic (a, b, 'rdivide');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} ldivide (@var{a}, @var{b})
    ##
    ## The element-wise left quotient, @code{@var{a} .\ @var{b}}.
    ##
    ## @code{@var{ts} = ldivide (@var{a}, @var{b})} returns the element-wise
    ## quotient of @var{b} by @var{a}, sample by sample.  One of them may be
    ## a number: a scalar, or an array the size of one sample.
    ##
    ## The rules for two series are given under @code{timeseries.plus},
    ## save that @qcode{DataInfo.Units} is dropped, since a product or
    ## quotient of units is the unit of neither; MATLAB keeps it where both
    ## series have the same.
    ##
    ## @seealso{timeseries.rdivide, timeseries.mldivide}
    ## @end deftypefn
    function ts = ldivide (a, b)
      ts = arithmetic (a, b, 'ldivide');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} mrdivide (@var{a}, @var{b})
    ##
    ## The matrix right division of series, @code{@var{a} / @var{b}}.
    ##
    ## @code{@var{ts} = mrdivide (@var{a}, @var{b})} returns the matrix right
    ## division of @var{a} by @var{b}, sample by sample; each result keeps the
    ## size of one sample.  One of them may be a number whose size fits the
    ## division with one sample.
    ##
    ## The rules for two series are given under @code{timeseries.plus},
    ## save that @qcode{DataInfo.Units} is dropped, since a product or
    ## quotient of units is the unit of neither; MATLAB keeps it where both
    ## series have the same.
    ##
    ## A number divided by a series, @code{2 / @var{ts}}, divides each sample;
    ## MATLAB divides by the whole data column as one matrix.
    ##
    ## @seealso{timeseries.mldivide, timeseries.rdivide}
    ## @end deftypefn
    function ts = mrdivide (a, b)
      ts = arithmetic (a, b, 'mrdivide');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{ts} =} mldivide (@var{a}, @var{b})
    ##
    ## The matrix left division of series, @code{@var{a} \ @var{b}}.
    ##
    ## @code{@var{ts} = mldivide (@var{a}, @var{b})} returns the matrix left
    ## division of @var{b} by @var{a}, sample by sample; each result keeps the
    ## size of one sample.  One of them may be a number whose size fits the
    ## division with one sample.
    ##
    ## The rules for two series are given under @code{timeseries.plus},
    ## save that @qcode{DataInfo.Units} is dropped, since a product or
    ## quotient of units is the unit of neither; MATLAB keeps it where both
    ## series have the same.
    ##
    ## @seealso{timeseries.mrdivide, timeseries.ldivide}
    ## @end deftypefn
    function ts = mldivide (a, b)
      ts = arithmetic (a, b, 'mldivide');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{tf} =} eq (@var{a}, @var{b})
    ##
    ## True when two series are equal, @code{@var{a} == @var{b}}.
    ##
    ## @code{@var{tf} = eq (@var{a}, @var{b})} returns a single logical for the
    ## whole series, as in MATLAB, not one per sample: @code{true} when
    ## @var{a} and @var{b} are series equal in every property but
    ## @qcode{UserData}, the class of the data aside, and @code{false} when
    ## either is not a series.  @code{NaN} is unequal to itself.
    ##
    ## @seealso{timeseries.isequal, timeseries.isequalwithequalnans}
    ## @end deftypefn
    function tf = eq (a, b)
      tf = sameSeries (a, b, false);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{tf} =} isequal (@var{a}, @var{b})
    ##
    ## True when two series are equal.
    ##
    ## @code{@var{tf} = isequal (@var{a}, @var{b})} is @code{@var{a} ==
    ## @var{b}}: every property but @qcode{UserData} compared, the class of the
    ## data aside, @code{NaN} unequal to itself.
    ##
    ## @seealso{timeseries.eq, timeseries.isequalwithequalnans}
    ## @end deftypefn
    function tf = isequal (a, b)
      tf = sameSeries (a, b, false);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {timeseries} {@var{tf} =} isequalwithequalnans (@var{a}, @var{b})
    ##
    ## True when two series are equal, @code{NaN} equal to itself.
    ##
    ## @code{@var{tf} = isequalwithequalnans (@var{a}, @var{b})} is
    ## @code{isequal (@var{a}, @var{b})} with @code{NaN} counted as equal to
    ## @code{NaN}.
    ##
    ## @seealso{timeseries.isequal, timeseries.eq}
    ## @end deftypefn
    function tf = isequalwithequalnans (a, b)
      tf = sameSeries (a, b, true);
    endfunction

  endmethods

################################################################################
##                       ** Filtering and detrending **                       ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'detrend'          'filter'           'idealfilter'                        ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} detrend (@var{ts}, @var{type})
    ## @deftypefnx {timeseries} {@var{ts} =} detrend (@var{ts}, @var{type}, @var{ind})
    ##
    ## Remove a polynomial trend from a series.
    ##
    ## @code{@var{ts} = detrend (@var{ts}, @var{type})} subtracts from each
    ## column of the data the polynomial fitted to it by least squares against
    ## @qcode{Time}: its mean for @qcode{'constant'} or 0, a straight line for
    ## @qcode{'linear'} or 1, and a polynomial of that degree for any larger
    ## integer.  @code{NaN} values are left out of the fit and stay @code{NaN}.
    ## Every other property is kept.
    ##
    ## @code{@var{ts} = detrend (@var{ts}, @var{type}, @var{ind})} detrends
    ## only the columns @var{ind} of the data, or its rows where
    ## @qcode{IsTimeFirst} is @code{false}.
    ##
    ## The data must be @code{double} or @code{single}, of two dimensions.
    ## MATLAB fits against the positions of the samples, closing up the gaps
    ## left by @code{NaN}, so its line is wrong where the time is not uniform
    ## or a value is missing; it also returns a series of fewer than three
    ## samples unchanged, where here @qcode{'constant'} of @code{[1 3]} is
    ## @code{[-1 1]}.
    ##
    ## @seealso{timeseries.filter, timeseries.idealfilter}
    ## @end deftypefn
    function this = detrend (this, type, ind)
      mustBeScalar (this, 'detrend');
      if (nargin < 2)
        error ("timeseries.detrend: invalid number of input arguments.");
      endif
      if (isText (type) && any (strcmpi (type, {'constant', 'linear'})))
        p = double (strcmpi (type, 'linear'));
      elseif (isnumeric (type) && isreal (type) && isscalar (type)
              && type >= 0 && type == fix (type))
        p = double (type);
      else
        error (strcat ("timeseries.detrend: TYPE must be 'constant',", ...
                       " 'linear' or a polynomial degree."));
      endif
      if (nargin < 3)
        [X, cols] = signalData (this, 'detrend', true);
      else
        [X, cols] = signalData (this, 'detrend', true, ind);
      endif
      t = this.time_;
      for j = cols(:)'
        ok = ! isnan (X(:,j));
        if (! any (ok))
          continue;
        endif
        x = X(ok,j) - mean (X(ok,j));
        if (p > 0)
          mu = mean (t(ok));
          sc = max (abs (t(ok) - mu));
          if (sc == 0)
            sc = 1;
          endif
          V = ((t(ok) - mu) / sc) .^ (0:p);
          x -= V * (V \ x);
        endif
        X(ok,j) = x;
      endfor
      this.data_ = signalBack (this, X);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} filter (@var{ts}, @var{b}, @var{a})
    ## @deftypefnx {timeseries} {@var{ts} =} filter (@var{ts}, @var{b}, @var{a}, @var{ind})
    ##
    ## Filter a series.
    ##
    ## @code{@var{ts} = filter (@var{ts}, @var{b}, @var{a})} runs each column
    ## of the data through the filter of numerator coefficients @var{b} and
    ## denominator coefficients @var{a}, as @code{filter (@var{b}, @var{a},
    ## @var{x})} does, along time from a zero initial state.  @code{NaN}
    ## values are first filled by the interpolation method of the series, so
    ## the data must not begin or end with one.  Every other property is
    ## kept.
    ##
    ## @code{@var{ts} = filter (@var{ts}, @var{b}, @var{a}, @var{ind})}
    ## filters only the columns @var{ind} of the data, or its rows where
    ## @qcode{IsTimeFirst} is @code{false}.
    ##
    ## The data must be @code{double} or @code{single}, and the times
    ## uniformly spaced.  MATLAB filters data of three dimensions across its
    ## rows when @var{ind} is not given, and filters a series with irregular
    ## times as if they were uniform, though its documentation requires them
    ## to be.
    ##
    ## @seealso{timeseries.idealfilter, timeseries.detrend}
    ## @end deftypefn
    function this = filter (this, b, a, ind)
      mustBeScalar (this, 'filter');
      if (nargin < 3)
        error ("timeseries.filter: invalid number of input arguments.");
      endif
      if (! (isnumeric (b) && isvector (b) && isnumeric (a) && isvector (a)))
        error ("timeseries.filter: B and A must be numeric vectors.");
      endif
      if (a(1) == 0)
        error ("timeseries.filter: the first element of A must not be zero.");
      endif
      if (nargin < 4)
        [X, cols] = signalData (this, 'filter', false);
      else
        [X, cols] = signalData (this, 'filter', false, ind);
      endif
      if (! isUniform (this.time_))
        error ("timeseries.filter: the times of TS must be uniformly spaced.");
      endif
      [X, errmsg] = fillGaps (this, X, cols);
      if (! isempty (errmsg))
        error ("timeseries.filter: %s", errmsg);
      endif
      if (! isempty (X) && ! isempty (cols))
        X(:,cols) = filter (b, a, X(:,cols), [], 1);
      endif
      this.data_ = signalBack (this, X);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {timeseries} {@var{ts} =} idealfilter (@var{ts}, @var{intervals}, @var{type})
    ## @deftypefnx {timeseries} {@var{ts} =} idealfilter (@var{ts}, @var{intervals}, @var{type}, @var{ind})
    ##
    ## Filter a series with an ideal filter.
    ##
    ## @code{@var{ts} = idealfilter (@var{ts}, @var{intervals}, @var{type})}
    ## removes the mean of each column of the data, then keeps the frequencies
    ## in @var{intervals} (@var{type} @qcode{'pass'}) or removes them
    ## (@qcode{'notch'}) through the discrete Fourier transform.  The mean is
    ## not added back.  @var{intervals} holds one frequency interval per row,
    ## @code{[@var{lo}, @var{hi}]}, in cycles per unit of
    ## @qcode{TimeInfo.Units}; a frequency @var{f} is in it when
    ## @code{@var{lo} < @var{f} <= @var{hi}}.  @code{NaN} values are first
    ## filled by the interpolation method of the series, so the data must not
    ## begin or end with one.  A series with irregular times is resampled
    ## first, by its interpolation method, onto as many uniformly spaced times
    ## from its first time to its last, which it keeps.  Every other property
    ## is kept.
    ##
    ## @code{@var{ts} = idealfilter (@var{ts}, @var{intervals}, @var{type},
    ## @var{ind})} filters only the columns @var{ind} of the data, or its rows
    ## where @qcode{IsTimeFirst} is @code{false}.
    ##
    ## The data must be @code{double} or @code{single}, of two dimensions.
    ## MATLAB returns complex @code{NaN} throughout for data that begins or
    ## ends with @code{NaN}, keeps the mean of a series of fewer than three
    ## samples, and reads only the first and last columns of
    ## @var{intervals}.
    ##
    ## @seealso{timeseries.filter, timeseries.detrend}
    ## @end deftypefn
    function this = idealfilter (this, intervals, type, ind)
      mustBeScalar (this, 'idealfilter');
      if (nargin < 3)
        error ("timeseries.idealfilter: invalid number of input arguments.");
      endif
      if (! (isnumeric (intervals) && isreal (intervals)
             && ismatrix (intervals) && columns (intervals) == 2
             && rows (intervals) >= 1 && ! any (isnan (intervals(:)))))
        error (strcat ("timeseries.idealfilter: INTERVALS must be a", ...
                       " numeric matrix of two columns, one interval", ...
                       " per row."));
      endif
      if (! (isText (type) && any (strcmp (type, {'pass', 'notch'}))))
        error ("timeseries.idealfilter: TYPE must be 'pass' or 'notch'.");
      endif
      if (nargin < 4)
        [X, cols] = signalData (this, 'idealfilter', true);
      else
        [X, cols] = signalData (this, 'idealfilter', true, ind);
      endif
      [X, errmsg] = fillGaps (this, X, cols);
      if (! isempty (errmsg))
        error ("timeseries.idealfilter: %s", errmsg);
      endif
      n = numel (this.time_);
      if (n == 0 || isempty (cols))
        return;
      endif
      if (! isUniform (this.time_))
        this.data_ = signalBack (this, X);
        t = linspace (this.time_(1), this.time_(end), n)';
        this = resampleAt (this, t, [], []);
        X = signalData (this, 'idealfilter', true);
      endif
      t = this.time_;
      dt = 1;
      if (n > 1)
        dt = (t(end) - t(1)) / (n - 1);
      endif
      k = (0:n-1)';
      f = min (k, n - k) / (n * dt);
      lo = double (min (intervals, [], 2))';
      hi = double (max (intervals, [], 2))';
      keep = any (lo < f & f <= hi, 2);
      if (strcmp (type, 'notch'))
        keep = ! keep;
      endif
      Y = X(:,cols) - mean (X(:,cols), 1);
      X(:,cols) = real (ifft (fft (Y, [], 1) .* keep, [], 1));
      this.data_ = signalBack (this, X);
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

    ## The time, in this series' units, of the event a query names: EVENT is
    ## a name, with the occurrence N among the events of that name, or a
    ## tsdata.event object.  Errors are raised under 'timeseries.METHOD'.
    function t = eventQueryTime (this, method, varargin)
      scope = ['timeseries.', method];
      if (numel (varargin) < 1 || numel (varargin) > 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      event = varargin{1};
      if (isa (event, 'tsdata.event') && isscalar (event))
        ## An object is one event, so its only occurrence is the first
        if (numel (varargin) == 2 && ! isequal (varargin{2}, 1))
          error ("%s: N must be 1 for a tsdata.event object.", scope);
        endif
        e = event;
      elseif (isText (event))
        n = 1;
        if (numel (varargin) == 2)
          n = varargin{2};
        endif
        [k, errmsg] = findEvent (this.events_, char (event), n);
        if (! isempty (errmsg))
          error ("%s: %s", scope, errmsg);
        endif
        e = this.events_(k);
      else
        error (strcat ("%s: EVENT must be an event name or a tsdata.event", ...
                       " object."), scope);
      endif
      startDate = this.timeInfo_.StartDate;
      eUnits = e.Units;
      if (isempty (eUnits))
        eUnits = this.timeInfo_.Units;
      endif
      ## The event's own time where it counts as the series does, since a
      ## round trip through nanoseconds need not return the same number
      if (strcmp (eUnits, this.timeInfo_.Units)
          && (isempty (e.StartDate) || strcmp (e.StartDate, startDate)))
        t = e.Time;
        return;
      endif
      ns = e.Time * nsPerUnit (eUnits);
      if (! isempty (e.StartDate))
        if (isempty (startDate))
          error (strcat ("%s: TS has no start date to place the dated", ...
                         " event '%s' on."), scope, e.Name);
        endif
        [off, ~] = dateOffsets ([tsdata.timemetadata.dateVector(startDate);
                                  tsdata.timemetadata.dateVector(e.StartDate)]);
        ## 'dateOffsets' counts from the earlier of the two
        if (off(1) == 0)
          ns += off(2);
        else
          ns -= off(1);
        endif
      endif
      t = ns / nsPerUnit (this.timeInfo_.Units);
      ## A time converted from other units or another date lands on a sample
      ## it names only up to rounding; it is that sample's time
      d = abs (this.time_ - t);
      k = find (d == min (d), 1);
      if (! isempty (k) && d(k) <= 1e-12 * max (1, abs (t)))
        t = this.time_(k);
      endif
    endfunction

    ## The series evaluated at the sorted times T, in its own units, by the
    ## method METHOD ('' for its own), the times it did not hold given the
    ## quality code CODE.  A series of one sample is refused unless ALLOWONE.
    ## Returns an empty ERRMSG, or the body of the message the caller raises.
    function [this, errmsg] = resampleAt (this, t, method, code, allowOne)
      errmsg = '';
      t = t(:);
      n = numel (this.time_);
      ## 'synchronize' evaluates a single sample at its own time, as MATLAB
      ## does; 'resample' needs two
      if (n < 1 || (n < 2 && ! (nargin > 4 && allowOne)))
        errmsg = "TS must hold at least two samples.";
        return;
      endif
      ip = this.dataInfo_.Interpolation;
      if (! isempty (method))
        ip = tsdata.interpolation (method);
      elseif (isempty (ip.Fhandle))
        ip = tsdata.interpolation ('linear');
      endif
      hasQ = ! isempty (this.quality_);
      if (! isempty (code))
        if (! hasQ)
          errmsg = "CODE is given for a series without quality codes.";
          return;
        endif
        if (! (isnumeric (code) && isreal (code) && isscalar (code)
               && code == fix (code)))
          errmsg = "CODE must be an integer.";
          return;
        endif
        if (! any (this.qualityInfo_.Code == code))
          errmsg = sprintf ("CODE %d is not listed in 'QualityInfo.Code'.", ...
                            code);
          return;
        endif
      endif

      ## The data with time first, as the interpolant takes and returns it
      td = this.timeDim_;
      ss = sampleSize (this.data_, td);
      od = this.data_;
      if (td > 1)
        od = permute (od, [td, 1:td-1]);
      endif
      nd = ip.Fhandle (t, this.time_, double (od));
      if (! (isnumeric (nd) || islogical (nd)) || size (nd, 1) != numel (t)
          || numel (nd) != numel (t) * prod (ss))
        errmsg = "the interpolation function must return one sample per time.";
        return;
      endif
      if (td == 1)
        nd = reshape (nd, [numel(t), ss(2:end)]);
      else
        nd = permute (reshape (nd, [numel(t), ss]), [2:td, 1]);
      endif
      if (islogical (this.data_))
        nd = nd != 0;
      else
        nd = cast (nd, class (this.data_));
      endif

      ## Quality: the sample a 'zoh' value is held from, otherwise the nearest,
      ## a tie going to the later; CODE at the times not already held
      if (hasQ)
        src = zeros (numel (t), 1);
        isZoh = strcmp (ip.Name, 'zoh');
        ## A sample that is NaN throughout holds no value
        held = ! all (isnan (reshape (double (od), numel (this.time_), [])), 2);
        for i = 1:numel (t)
          k = [];
          if (isZoh)
            k = find (this.time_ <= t(i) & held, 1, 'last');
          endif
          if (isempty (k))
            d = abs (this.time_ - t(i));
            k = find (d == min (d), 1, 'last');
          endif
          src(i) = k;
        endfor
        q = takeSamples (this.quality_, src, td);
        if (! isempty (code))
          isNew = ! ismember (t, this.time_);
          q = putSample (q, find (isNew), double (code), td);
        endif
        this.quality_ = q;
      endif
      this.data_ = nd;
      this.time_ = t;
      this.timeInfo_.TimeVector = t;
    endfunction

    ## The statistic FN of the series over time, with the options in
    ## VARARGIN: one value per data element, shaped as one sample.
    function v = statistic (this, fn, varargin)
      scope = ['timeseries.', fn];
      mustBeScalar (this, fn);
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      codes = [];
      fill = false;
      weighted = false;
      for i = 1:2:numel (varargin)
        [name, val] = varargin{i:i+1};
        if (! isText (name))
          error ("%s: invalid optional paired argument.", scope);
        endif
        switch (lower (char (name)))
          case 'quality'
            if (! ((isnumeric (val) || islogical (val)) && isreal (val)
                   && all (val(:) == fix (val(:)))))
              error ("%s: 'Quality' must hold integer codes.", scope);
            endif
            codes = double (val(:));
          case 'missingdata'
            if (! (isText (val)
                   && any (strcmpi (val, {'remove', 'interpolate'}))))
              error ("%s: 'MissingData' must be 'remove' or 'interpolate'.", ...
                     scope);
            endif
            fill = strcmpi (val, 'interpolate');
          case 'weighting'
            if (! (isText (val) && any (strcmpi (val, {'none', 'time'}))))
              error ("%s: 'Weighting' must be 'none' or 'time'.", scope);
            endif
            weighted = strcmpi (val, 'time');
          otherwise
            error ("%s: invalid optional paired argument.", scope);
        endswitch
      endfor
      n = numel (this.time_);
      if (n == 0 || isempty (this.data_))
        v = [];
        return;
      endif
      td = this.timeDim_;
      ss = sampleSize (this.data_, td);
      cls = class (this.data_);

      ## Time first, one column per data element
      X = this.data_;
      if (td > 1)
        X = permute (X, [td, 1:td-1]);
      endif
      X = double (reshape (X, n, []));
      m = columns (X);

      ## Missing: NaN while it counts as missing, and the excluded codes
      miss = false (n, m);
      if (this.treatNaN_)
        miss = isnan (X);
      endif
      if (! isempty (codes))
        q = this.quality_;
        if (isempty (q) || ! any (ismember (codes, q(:))))
          warning ("%s: no sample has the quality codes given.", scope);
        else
          absent = unique (codes(! ismember (codes, q(:))));
          if (numel (absent) == 1)
            warning ("%s: no sample has the quality code %d.", scope, absent);
          elseif (! isempty (absent))
            warning ("%s: no sample has the quality codes %s.", scope, ...
                     strjoin (arrayfun (@num2str, absent', ...
                                        'UniformOutput', false), ', '));
          endif
          if (td > 1)
            q = permute (q, [td, 1:td-1]);
          endif
          q = reshape (q, n, []);
          if (columns (q) == 1)
            q = repmat (q, 1, m);
          endif
          miss |= ismember (q, codes);
        endif
      endif

      ## Missing values between samples filled linearly in time
      t = this.time_;
      if (fill)
        for j = 1:m
          ok = ! miss(:,j);
          if (sum (ok) >= 2)
            inner = miss(:,j) & t > min (t(ok)) & t < max (t(ok));
            X(inner,j) = interp1 (t(ok), X(ok,j), t(inner), 'linear');
            miss(inner,j) = false;
          endif
        endfor
      endif

      ## The time each sample covers, scaled to a mean of 1
      w = ones (n, 1);
      if (weighted && n > 1)
        d = diff (t);
        w = [d(1); (d(1:end-1) + d(2:end)) / 2; d(end)];
        if (all (w == 0))
          w = ones (n, 1);
        endif
        w = w / mean (w);
      endif

      v = NaN (1, m);
      skipsNaN = any (strcmp (fn, {'min', 'max', 'mode', 'iqr'}));
      for j = 1:m
        keep = ! miss(:,j);
        if (skipsNaN)
          keep &= ! isnan (X(:,j));
        endif
        v(j) = columnStatistic (fn, X(keep,j), w(keep));
      endfor
      v = reshape (v, [ss, 1]);
      if (numel (ss) == 2 && ss(1) == 1)
        v = reshape (v, ss);
      endif
      ## A value left without samples is NaN, so the class of integer or
      ## logical data is kept only when every value has samples
      if (any (strcmp (fn, {'median', 'mode', 'min', 'max'}))
          && ! ((isinteger (this.data_) || islogical (this.data_))
                && any (isnan (v(:)))))
        if (strcmp (cls, 'logical'))
          v = v != 0 & ! isnan (v);
        else
          v = cast (v, cls);
        endif
      endif
    endfunction

    ## The arithmetic OP of A and B, either or both a series.
    function ts = arithmetic (a, b, op)
      scope = ['timeseries.', op];
      f = str2func (op);
      aIs = isa (a, 'timeseries');
      bIs = isa (b, 'timeseries');
      if (aIs)
        mustBeScalar (a, op);
      endif
      if (bIs)
        mustBeScalar (b, op);
      endif
      if (aIs && bIs)
        n = numel (a.time_);
        if (numel (b.time_) != n)
          error ("%s: A and B must hold as many samples.", scope);
        endif
        if (! strcmp (a.timeInfo_.Units, b.timeInfo_.Units))
          error ("%s: A and B must have their times in the same units.", ...
                 scope);
        endif
        sdA = a.timeInfo_.StartDate;
        sdB = b.timeInfo_.StartDate;
        if (isempty (sdA) != isempty (sdB))
          error (strcat ("%s: A and B must both have a start date or", ...
                         " neither."), scope);
        endif
        if (! isempty (sdA))
          [off, ~] = dateOffsets ([tsdata.timemetadata.dateVector(sdA);
                                    tsdata.timemetadata.dateVector(sdB)]);
          nsu = nsPerUnit (a.timeInfo_.Units);
          ## Instants a microsecond apart, rounding aside, are the same
          if (any (abs ((a.time_ * nsu + off(1)) - (b.time_ * nsu + off(2)))
                   > 1e3))
            error ("%s: A and B must be dated at the same instants.", scope);
          endif
        elseif (n > 0)
          d = b.time_ - a.time_;
          tol = 1e-12 * max ([1; abs(a.time_)]);
          if (any (abs (d - d(1)) > tol))
            error (strcat ("%s: the times of A and B must be equal or", ...
                           " differ by a constant offset."), scope);
          endif
          if (d(1) != 0)
            warning (strcat ("%s: the times of A and B differ by a", ...
                             " constant offset; the result takes", ...
                             " the times of A."), scope);
          endif
        endif
        if (a.timeDim_ != b.timeDim_)
          error ("%s: A and B must hold samples of the same shape.", scope);
        endif
        [data, td, errmsg] = sampleOp (f, double (a.data_), ...
                                       double (b.data_), a.timeDim_, n, ...
                                       op, true);
        ts = a;
        ts.name_ = 'unnamed';
        units = '';
        if (any (strcmp (op, {'plus', 'minus'}))
            && strcmp (a.dataInfo_.Units, b.dataInfo_.Units))
          units = a.dataInfo_.Units;
        endif
        ts.dataInfo_.Units = units;
        ts.dataInfo_.Interpolation = tsdata.interpolation ('linear');
        ts.quality_ = [];
        ts.qualityInfo_ = tsdata.qualmetadata ();
        if (isempty (a.events_))
          ts.events_ = b.events_;
        elseif (! isempty (b.events_))
          ts.events_ = [a.events_(:)', b.events_(:)'];
        endif
      else
        if (aIs)
          ts = a;
          num = b;
          left = true;
        else
          ts = b;
          num = a;
          left = false;
        endif
        if (! ((isnumeric (num) || islogical (num)) && ! isempty (num)))
          error ("%s: the other operand must be a series or a number.", scope);
        endif
        n = numel (ts.time_);
        num = double (num);
        if (left)
          [data, td, errmsg] = sampleOp (f, double (ts.data_), num, ...
                                         ts.timeDim_, n, op, false);
        else
          [data, td, errmsg] = sampleOp (@(x, y) f (y, x), ...
                                         double (ts.data_), num, ...
                                         ts.timeDim_, n, op, false);
        endif
      endif
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      q = ts.quality_;
      if (! isempty (q) && numel (q) != n && ! isequal (size (data), size (q)))
        error (strcat ("%s: the result changes the size of the samples, so", ...
                       " the quality codes held per data element cannot", ...
                       " follow it."), scope);
      endif
      ## Codes held per sample lie along the time dimension of the result
      if (! isempty (q) && numel (q) == n)
        if (td == 1)
          ts.quality_ = q(:);
        else
          ts.quality_ = reshape (q, [ones(1, td - 1), n]);
        endif
      endif
      ts.data_ = data;
      ts.timeDim_ = td;
    endfunction

    ## The data of the series as a double matrix, time first and one column
    ## per data element, and the columns the optional IND selects, all when
    ## it is not given.  Errors are raised under 'timeseries.METHOD'; with
    ## TWODIMS, data of more than two dimensions is refused.
    function [X, cols] = signalData (this, method, twoDims, varargin)
      scope = ['timeseries.', method];
      data = this.data_;
      if (! isfloat (data) || iscomplex (data))
        error ("%s: the data must be real, of class double or single.", ...
               scope);
      endif
      if (twoDims && ndims (data) > 2)
        error ("%s: the data must have two dimensions.", scope);
      endif
      n = numel (this.time_);
      td = this.timeDim_;
      ss = sampleSize (data, td);
      if (td > 1)
        data = permute (data, [td, 1:td-1]);
      endif
      X = double (reshape (data, n, []));
      if (td == 1)
        nc = size (this.data_, 2);
        what = 'columns';
      else
        nc = ss(1);
        what = 'rows';
      endif
      if (isempty (varargin))
        cols = 1:columns (X);
        return;
      endif
      ind = varargin{1};
      if (! (isnumeric (ind) && isreal (ind)
             && (isvector (ind) || isempty (ind))
             && all (ind == fix (ind) & ind >= 1 & ind <= nc)))
        error (strcat ("%s: IND must be positive integers not exceeding", ...
                       " the number of %s of the data."), scope, what);
      endif
      if (td == 1)
        cols = unique (double (ind(:)))';
      else
        mask = false ([ss, 1]);
        mask(ind,:) = true;
        cols = find (mask(:))';
      endif
    endfunction

    ## The data of the series from X, a matrix laid out as 'signalData'
    ## returns it, in the class and layout of the data.
    function data = signalBack (this, X)
      td = this.timeDim_;
      n = numel (this.time_);
      if (td == 1)
        data = X;
      else
        ss = sampleSize (this.data_, td);
        data = permute (reshape (X, [n, ss]), [2:td, 1]);
      endif
      data = cast (data, class (this.data_));
    endfunction

    ## The columns COLS of X, laid out as 'signalData' returns it, with each
    ## NaN filled by the interpolation method of the series.  A column that
    ## begins or ends with NaN gives the rest of an error message in ERRMSG.
    function [X, errmsg] = fillGaps (this, X, cols)
      errmsg = '';
      if (isempty (X))
        return;
      endif
      ip = this.dataInfo_.Interpolation;
      if (isempty (ip.Fhandle))
        ip = tsdata.interpolation ('linear');
      endif
      t = this.time_;
      for j = cols(:)'
        gap = isnan (X(:,j));
        if (! any (gap))
          continue;
        endif
        if (gap(1) || gap(end))
          errmsg = "the data must not begin or end with NaN.";
          return;
        endif
        v = ip.Fhandle (t(gap), t(! gap), X(! gap,j));
        if (! (isnumeric (v) || islogical (v)) || numel (v) != sum (gap))
          errmsg = strcat ("the interpolation function must return one", ...
                           " sample per time.");
          return;
        endif
        X(gap,j) = v(:);
      endfor
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
        dv0 = tsdata.timemetadata.dateVector (startDate);
        nsu = nsPerUnit (this.timeInfo_.Units);
        if (isnumeric (x) && isreal (x))
          ## A datenum reaches a series only to its own precision
          t = (double (x(:)) - datenum (dv0)) * 864e11 / nsu;
          tol = 1e-9 * 864e11 / nsu;
        else
          [dv, dverr] = dateVectors (x, '');
          if (! isempty (dverr))
            t = [];
            errmsg = "must be dates, as text, datenum or datetime values.";
            return;
          endif
          ## Dates are exact to the microsecond, rounding apart
          t = tsdata.timemetadata.dateOffset (dv, dv0) / nsu;
          tol = 1e3 / nsu;
        endif
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
      if (n > 0 && isequal (size (val), size (this.data_)))
        td = this.timeDim_;
      elseif (n > 0 && ! isempty (val))
        [val, td, errmsg] = orientData (val, n);
        if (! isempty (errmsg))
          error ("timeseries: 'Data' must have one sample per time.");
        endif
      elseif (ndims (val) <= 2)
        td = 1;
      else
        td = ndims (val);
      endif
      q = this.quality_;
      if (! isempty (q) && numel (q) != n && ! isequal (size (val), size (q)))
        error (strcat ("timeseries: 'Data' must keep the size of the", ...
                       " quality codes held per data element; clear", ...
                       " 'Quality' first."));
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
      if (! isempty (this.data_) && ! isempty (val)
          && numel (val) != numel (this.time_))
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
      ## Events dated on their own count from the start date being removed,
      ## since a series without one cannot place a date
      old = this.timeInfo_.StartDate;
      if (! isempty (old) && isempty (val.StartDate))
        dv0 = tsdata.timemetadata.dateVector (old);
        for k = 1:numel (this.events_)
          e = this.events_(k);
          if (isempty (e.StartDate))
            continue;
          endif
          eUnits = e.Units;
          if (isempty (eUnits))
            eUnits = val.Units;
          endif
          ns = e.Time * nsPerUnit (eUnits) + tsdata.timemetadata.dateOffset ...
                 (tsdata.timemetadata.dateVector (e.StartDate), dv0);
          e.StartDate = '';
          e.Time = ns / nsPerUnit (eUnits);
          this.events_(k) = e;
        endfor
      endif
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

## Find the Nth event named NAME in EVENTS, counting only those of that name.
## Returns its index and an empty ERRMSG, or the body of the message the
## caller raises.
function [k, errmsg] = findEvent (events, name, n)
  k = 0;
  errmsg = '';
  if (isempty (events))
    errmsg = "TS has no events.";
    return;
  endif
  if (! (isnumeric (n) && isreal (n) && isscalar (n) && n == fix (n) && n >= 1))
    errmsg = "N must be a positive integer.";
    return;
  endif
  idx = find (strcmp ({events.Name}, name));
  if (isempty (idx))
    errmsg = sprintf ("TS has no event named '%s'.", name);
  elseif (n > numel (idx))
    errmsg = sprintf ("TS has no event number %d named '%s'.", n, name);
  else
    k = idx(n);
  endif
endfunction

## True when events A and B have the same name and the same time.
function tf = sameEvent (a, b)
  tf = strcmp (a.Name, b.Name) && a.Time == b.Time ...
       && strcmp (a.Units, b.Units) && strcmp (a.StartDate, b.StartDate);
endfunction

## Read an interpolation method name for 'resample' or 'synchronize': '' or
## [] for the series' own.  Returns it in lower case and an empty ERRMSG, or
## the rest of the message the caller raises after naming it.
function [method, errmsg] = interpName (method)
  errmsg = '';
  if (isempty (method) && (isnumeric (method) || ischar (method)))
    method = '';
    return;
  endif
  if (! isText (method)
      || ! any (strcmpi (char (method), {'linear', 'zoh'})))
    errmsg = "must be 'linear' or 'zoh'.";
    return;
  endif
  method = lower (char (method));
endfunction

## The statistic FN of the column X with the weights W, W all ones when
## unweighted.  Unweighted they are MATLAB's; weighted, the time-weighted
## statistic, which with equal weights is the same.
function v = columnStatistic (fn, x, w)
  n = numel (x);
  if (n == 0)
    if (strcmp (fn, 'sum'))
      v = 0;
    else
      v = NaN;
    endif
    return;
  endif
  switch (fn)
    case 'mean'
      v = sum (w .* x) / sum (w);
    case 'sum'
      v = sum (w .* x);
    case {'var', 'std'}
      if (n == 1)
        v = 0;
      else
        w = w / mean (w);
        mu = sum (w .* x) / sum (w);
        v = sum (w .* (x - mu) .^ 2) / (n - 1);
      endif
      if (strcmp (fn, 'std'))
        v = sqrt (v);
      endif
    case 'median'
      v = weightedQuantile (x, w, 0.5);
    case 'iqr'
      v = weightedQuantile (x, w, 0.75) - weightedQuantile (x, w, 0.25);
    case 'min'
      v = min (x);
    case 'max'
      v = max (x);
    case 'mode'
      [u, ~, k] = unique (x);
      total = accumarray (k(:), w(:));
      [~, i] = max (total);
      v = u(i);
  endswitch
endfunction

## The quantile P of X under the weights W: the (k - 0.5)/n rule, with the
## cumulative weight for k, linear between and constant beyond the ends.
## NaN in X gives NaN.
function v = weightedQuantile (x, w, p)
  if (any (isnan (x)))
    v = NaN;
    return;
  endif
  [x, i] = sort (x(:));
  w = w(i);
  if (numel (x) == 1)
    v = x;
    return;
  endif
  pos = (cumsum (w) - w / 2) / sum (w);
  if (p <= pos(1))
    v = x(1);
  elseif (p >= pos(end))
    v = x(end);
  else
    k = find (pos <= p, 1, 'last');
    if (pos(k) == p)
      v = x(k);
    else
      v = x(k) + (p - pos(k)) / (pos(k+1) - pos(k)) * (x(k+1) - x(k));
    endif
  endif
endfunction

## Apply F to the data X, N samples along dimension TD, and Y: another
## series' data when BOTHSERIES, else a number, a scalar or one sample's
## size.  Element-wise operators broadcast; matrix operators run per sample,
## named by OP.  Returns the data, the dimension its time runs along, and an
## empty ERRMSG, or the body of the message the caller raises.
function [data, td, errmsg] = sampleOp (f, x, y, td, n, op, bothSeries)
  data = [];
  errmsg = '';
  matrixOp = any (strcmp (op, {'mtimes', 'mrdivide', 'mldivide'}));
  ss = sampleSize (x, td);
  if (! matrixOp)
    if (bothSeries)
      if (! isequal (size (x), size (y)))
        errmsg = "A and B must hold samples of the same size.";
        return;
      endif
    elseif (! (isscalar (y) || isequal (size (y), ss)
               || (td == 1 && isequal (size (y), [1, ss(2)]))))
      errmsg = "a number must be a scalar or the size of one sample.";
      return;
    endif
    data = f (x, y);
    return;
  endif
  if (n == 0)
    data = x;
    return;
  endif
  ## A scalar sample is multiplied only by a number, as MATLAB applies matrix
  ## rules there; a division keeps the size of the sample
  isDivision = ! strcmp (op, 'mtimes');
  if (isequal (ss, [1, 1]) && ! bothSeries && ! isscalar (y)
      && strcmp (op, 'mtimes'))
    errmsg = "the sizes of the samples do not fit the operator.";
    return;
  endif
  parts = cell (1, n);
  for k = 1:n
    xk = reshape (takeSamples (x, k, td), ss);
    if (bothSeries)
      yk = reshape (takeSamples (y, k, td), ss);
    else
      yk = y;
    endif
    try
      r = f (xk, yk);
    catch
      errmsg = "the sizes of the samples do not fit the operator.";
      return;
    end_try_catch
    if ((k > 1 && ! isequal (size (r), size (parts{1})))
        || (isDivision && ! isequal (size (r), ss)))
      errmsg = "the sizes of the samples do not fit the operator.";
      return;
    endif
    parts{k} = r;
  endfor
  ## The results laid as the series lays its samples: rows where time runs
  ## first and a sample is a row, slices along a new last dimension otherwise
  rs = size (parts{1});
  if (isequal (rs, [1, 1]))
    data = vertcat (parts{:});
    td = 1;
  elseif (td == 1 && rs(1) == 1)
    data = vertcat (parts{:});
  else
    data = cat (numel (rs) + 1, parts{:});
    td = numel (rs) + 1;
  endif
endfunction

## True when the series A and B agree in every property but UserData, the
## class of the data aside; NaN equal to itself when EQUALNANS.
function tf = sameSeries (a, b, equalNans)
  tf = false;
  if (! (isa (a, 'timeseries') && isa (b, 'timeseries'))
      || ! isequal (size (a), size (b)))
    return;
  endif
  if (equalNans)
    same = @(x, y) isequaln (x, y);
  else
    same = @(x, y) isequal (x, y);
  endif
  for k = 1:numel (a)
    x = a(k);
    y = b(k);
    ok = strcmp (x.Name, y.Name) && same (double (x.Data), double (y.Data)) ...
         && isequal (x.Time, y.Time) && x.IsTimeFirst == y.IsTimeFirst ...
         && isequal (x.Quality, y.Quality) ...
         && x.TreatNaNasMissing == y.TreatNaNasMissing;
    ti = {x.TimeInfo, y.TimeInfo};
    ok = ok && strcmp (ti{1}.Units, ti{2}.Units) ...
         && strcmp (ti{1}.StartDate, ti{2}.StartDate) ...
         && strcmp (ti{1}.Format, ti{2}.Format) ...
         && isequal (ti{1}.UserData, ti{2}.UserData);
    di = {x.DataInfo, y.DataInfo};
    ok = ok && strcmp (di{1}.Units, di{2}.Units) ...
         && strcmp (di{1}.Interpolation.Name, di{2}.Interpolation.Name) ...
         && isequal (di{1}.UserData, di{2}.UserData) ...
         && isequal (di{1}.InterpretSingleRowDataAs3D, ...
                     di{2}.InterpretSingleRowDataAs3D);
    if (ok && strcmp (di{1}.Interpolation.Name, 'myFuncHandle'))
      ok = strcmp (func2str (di{1}.Interpolation.Fhandle), ...
                   func2str (di{2}.Interpolation.Fhandle));
    endif
    qi = {x.QualityInfo, y.QualityInfo};
    ok = ok && isequal (qi{1}.Code, qi{2}.Code) ...
         && isequal (qi{1}.Description, qi{2}.Description) ...
         && isequal (qi{1}.UserData, qi{2}.UserData);
    ev = {x.Events, y.Events};
    ok = ok && numel (ev{1}) == numel (ev{2});
    for j = 1:numel (ev{1})
      if (! ok)
        break;
      endif
      e1 = ev{1}(j);
      e2 = ev{2}(j);
      ok = strcmp (e1.Name, e2.Name) && e1.Time == e2.Time ...
           && strcmp (e1.Units, e2.Units) ...
           && strcmp (e1.StartDate, e2.StartDate) ...
           && isequal (e1.EventData, e2.EventData);
    endfor
    if (! ok)
      return;
    endif
  endfor
  tf = true;
endfunction

## The times at which 'synchronize' evaluates a series, and the times it
## labels the result with, for the GRID of times in nanoseconds on a common
## scale.  TNS are the series' own times on that scale, OWN the same times in
## its units, OFF its offset on the scale and NS its nanoseconds per unit.  A
## grid time the series holds is its own time exactly, and one within TOLNS
## of a time it holds is evaluated there.  Labels count from OFFLABEL.
function [evalT, labelT] = frameTimes (grid, tNs, own, off, ns, tolNs, offLabel)
  [isOwn, k] = ismember (grid, tNs);
  evalT = (grid - off) / ns;
  evalT(isOwn) = own(k(isOwn));
  for i = find (! isOwn)'
    [d, j] = min (abs (tNs - grid(i)));
    if (d <= tolNs)
      evalT(i) = own(j);
    endif
  endfor
  labelT = (grid - offLabel) / ns;
  if (offLabel == off)
    labelT(isOwn) = own(k(isOwn));
  endif
endfunction

## True when the times T are uniformly spaced: a single step, positive, up to
## rounding.  No time or one time counts as uniform.
function tf = isUniform (t)
  d = diff (t(:));
  tf = isempty (d) || (all (d > 0) && max (d) - min (d) <= 1e-10 * max (d));
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
## of a time vector given as dates ('' otherwise), the units the argument
## implies ('' for plain numbers), and an empty ERRMSG, or the body of the
## message the caller raises.
function [time, startDate, units, errmsg] = parseTime (time)
  startDate = '';
  units = '';
  errmsg = '';
  if (isa (time, 'duration'))
    if (! all (isfinite (seconds (time(:)))))
      errmsg = "TIME must be finite.";
      return;
    endif
    ## MATLAB's own pairing of units and formats in 'timeseries2timetable',
    ## read backwards
    switch (time.Format)
      case 'm'
        units = 'minutes';
        time = minutes (time(:));
      case 'h'
        units = 'hours';
        time = hours (time(:));
      case 'd'
        units = 'days';
        time = days (time(:));
      otherwise
        units = 'seconds';
        time = seconds (time(:));
    endswitch
    time = double (time);
    return;
  endif
  if (iscell (time) && ! isempty (time)
      && all (cellfun (@(x) isnumeric (x) && isscalar (x), time(:))))
    time = cell2mat (time(:));
  endif
  if ((iscellstr (time) || isa (time, 'string') || isa (time, 'datetime'))
      && isempty (time))
    time = [];
    return;
  endif
  if (iscellstr (time) || isa (time, 'string') || isa (time, 'datetime'))
    [dv, errmsg] = dateVectors (time, '');
    if (! isempty (errmsg))
      errmsg = ["TIME ", errmsg];
      return;
    endif
    if (any (dv(:,1) < 0 | dv(:,1) > 9999))
      errmsg = "TIME must fall in the years 0 to 9999.";
      return;
    endif
    [~, dv0] = dateOffsets (dv);
    startDate = tsdata.timemetadata.dateText (dv0);
    ## Counted from the start date as stored, to the millisecond
    ns = tsdata.timemetadata.dateOffset ( ...
           dv, tsdata.timemetadata.dateVector (startDate));
    time = ns / 864e11;
    units = 'days';
    return;
  endif
  if (! (isnumeric (time) && isreal (time)
         && (isvector (time) || isempty (time))))
    errmsg = "TIME must be a numeric vector, dates or durations.";
    return;
  endif
  time = double (time(:));
  if (! all (isfinite (time)))
    errmsg = "TIME must be finite.";
  endif
endfunction

## Read dates given as text (a cellstr, a string array, a character matrix
## of one date per row, a character vector) or as a datetime array, into a
## date vector per row.  With FMT each text date is read strictly in that
## format; without, each is read by its own.  Returns an empty ERRMSG, or
## the end of the message the caller raises after naming the argument.
function [dv, errmsg] = dateVectors (x, fmt)
  dv = zeros (0, 6);
  errmsg = '';
  if (isa (x, 'datetime'))
    if (any (isnat (x(:))))
      errmsg = "must not hold NaT.";
      return;
    endif
    dv = datevec (x(:));
    return;
  endif
  if (ischar (x))
    x = cellstr (x);
  elseif (isa (x, 'string'))
    x = cellstr (x);
  endif
  if (! iscellstr (x))
    errmsg = "must be dates, as text or datetime values.";
    return;
  endif
  x = x(:);
  dv = zeros (numel (x), 6);
  for i = 1:numel (x)
    try
      [dvi, ok] = tsdata.timemetadata.dateVector (x{i});
      if (isempty (fmt) && ok)
        dv(i,:) = dvi;
      elseif (isempty (fmt))
        dv(i,:) = datevec (x{i});
      else
        dv(i,:) = datevec (x{i}, fmt);
      endif
    catch
      if (isempty (fmt))
        errmsg = sprintf ("holds text that is not a date: '%s'", x{i});
      else
        errmsg = sprintf ("holds a date not written as '%s': '%s'", ...
                          fmt, x{i});
      endif
      dv = zeros (0, 6);
      return;
    end_try_catch
  endfor
endfunction

## The offsets of the dates DV, one date vector per row, from the earliest,
## in nanoseconds, and the earliest as a date vector.  Whole days and the
## seconds of the day are taken apart, so a whole number of seconds is exact.
function [ns, dv0] = dateOffsets (dv)
  ns = zeros (rows (dv), 1);
  dv0 = [];
  if (isempty (dv))
    return;
  endif
  dayNum = datenum (dv(:,1), dv(:,2), dv(:,3));
  secs = dv(:,4) * 3600 + dv(:,5) * 60 + dv(:,6);
  [~, i0] = min (dayNum * 86400 + secs);
  ns = (dayNum - dayNum(i0)) * 864e11 + (secs - secs(i0)) * 1e9;
  dv0 = dv(i0,:);
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

## 'pkg test' reads the BISTs from the file it is testing, so a class
## whose suite lives in inst/tests/ reads as carrying none.  This block
## answers for it and checks nothing itself.
%!test
%! ## The suite for this class is inst/tests/timeseries.m-tst.
