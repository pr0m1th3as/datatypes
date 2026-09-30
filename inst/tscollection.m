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

classdef tscollection
  ## -*- texinfo -*-
  ## @deftp {datatypes} tscollection
  ##
  ## Collection of time series on one time vector.
  ##
  ## A @code{tscollection} object holds several @code{timeseries} objects,
  ## its members, which share one time vector: a sample of the collection is
  ## the sample of every member at one time.  Each member is reached by its
  ## name, as @code{@var{tc}.@var{name}}, and keeps its own data, quality
  ## codes, events and metadata; only the time vector and its
  ## @qcode{TimeInfo} are the collection's.
  ##
  ## @code{tscollection} is a value class: every method returns a modified
  ## copy and leaves its input unchanged.
  ##
  ## Its size is the number of samples by the number of members, and
  ## @code{@var{tc}(@var{i}, @var{j})} selects samples @var{i} of members
  ## @var{j}, the members given by position or by name.
  ##
  ## Member names are unique ignoring case, and read with the exact name;
  ## MATLAB reads @code{@var{tc}.A} for a member @qcode{a}.  A member cannot
  ## take the name of one of the four properties.
  ##
  ## @seealso{timeseries, istscollection, timeseries2timetable,
  ## tsdata.timemetadata}
  ## @end deftp

  properties (Dependent)
    ## -*- texinfo -*-
    ## @deftp {tscollection} {property} Name
    ##
    ## The name of the collection.
    ##
    ## A character vector, @qcode{'unnamed'} by default.  A string scalar is
    ## converted; any other value is refused, where MATLAB accepts it.
    ##
    ## @end deftp
    Name

    ## -*- texinfo -*-
    ## @deftp {tscollection} {property} Time
    ##
    ## The time vector every member shares.
    ##
    ## A column vector of finite, non-decreasing times, in
    ## @qcode{@var{tc}.TimeInfo.Units}, counted from
    ## @qcode{@var{tc}.TimeInfo.StartDate} when that is set, and stored as
    ## @code{double}.  Repeated times are allowed.  Assigning it gives every
    ## member the new times, without moving any sample; it must hold as many
    ## times as there are samples, and an unsorted one is refused.
    ##
    ## @end deftp
    Time

    ## -*- texinfo -*-
    ## @deftp {tscollection} {property} TimeInfo
    ##
    ## The metadata of the time vector, a @code{tsdata.timemetadata} object,
    ## which every member shares.  Assigning it, or one of its properties,
    ## assigns it to every member.  Its @qcode{Length}, @qcode{Start},
    ## @qcode{End} and @qcode{Increment} follow @qcode{Time} and cannot be
    ## assigned.
    ##
    ## @end deftp
    TimeInfo

    ## -*- texinfo -*-
    ## @deftp {tscollection} {property} Length
    ##
    ## The number of samples, which is the length of the time vector.
    ## Read-only.
    ##
    ## @end deftp
    Length
  endproperties

  properties (Access = private)
    name_ = 'unnamed'
    time_ = zeros (0, 1)
    timeInfo_ = tsdata.timemetadata ()
    ## The members, in order, and the name each is reached by
    members_ = {}
    names_ = {}
  endproperties

  methods (Hidden)

    function display (this)
      disp (this, true);
    endfunction

    function disp (this, summary)
      if (nargin > 1 && summary)
        fprintf ("\nTime Series Collection Object: %s\n\n", this.name_);
        if (isempty (this.time_))
          fprintf ("      Empty\n\n");
          return;
        endif
        fprintf ("Time vector characteristics\n\n");
        if (isempty (this.timeInfo_.StartDate))
          units = this.timeInfo_.Units;
          fprintf ("      %-22s%s %s\n", 'Start time', ...
                   num2str (this.time_(1)), units);
          fprintf ("      %-22s%s %s\n", 'End time', ...
                   num2str (this.time_(end)), units);
        else
          dates = getabstime (this);
          fprintf ("      %-22s%s\n", 'Start date', dates{1});
          fprintf ("      %-22s%s\n", 'End date', dates{end});
        endif
        fprintf ("\nMember Time Series Objects:\n\n");
        fprintf ("      %s\n", this.names_{:});
        fprintf ("\n\n");
        return;
      endif
      sz = size (this);
      fprintf ("  %dx%d tscollection array with properties:\n\n", sz);
      names = {'Name', 'Time', 'TimeInfo', 'Length'};
      for i = 1:numel (names)
        fprintf ("%12s: %s\n", names{i}, ...
                 timeseries.dispValue (subsref (this, ...
                                                struct ('type', '.', ...
                                                        'subs', names{i}))));
      endfor
    endfunction

  endmethods

################################################################################
##                  ** Create and describe 'tscollection' **                  ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'tscollection'     'get'              'set'              'fieldnames'      ##
## 'properties'       'size'             'length'           'isempty'         ##
## 'isequal'                                                                  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} tscollection ()
    ## @deftypefnx {tscollection} {@var{tc} =} tscollection (@var{ts})
    ## @deftypefnx {tscollection} {@var{tc} =} tscollection (@{@var{ts1}, @var{ts2}, @dots{}@})
    ## @deftypefnx {tscollection} {@var{tc} =} tscollection (@var{time})
    ## @deftypefnx {tscollection} {@var{tc} =} tscollection (@dots{}, @qcode{'Name'}, @var{name})
    ##
    ## Create a @code{tscollection} object.
    ##
    ## @code{@var{tc} = tscollection ()} returns an empty collection, with no
    ## samples and no members.
    ##
    ## @code{@var{tc} = tscollection (@var{ts})} returns a collection of the
    ## single @code{timeseries} @var{ts}, on its time vector and its
    ## @qcode{TimeInfo}.  A series with no samples gives an empty collection,
    ## as in MATLAB.
    ##
    ## @code{@var{tc} = tscollection (@{@var{ts1}, @var{ts2}, @dots{}@})}
    ## returns a collection of the series in the cell array, a row or a column,
    ## on the time vector of the first; every other series must have the same
    ## times, in the same units, and the same @qcode{TimeInfo.StartDate}.  An
    ## empty cell array gives an empty collection, and a series with no
    ## samples is refused, as in MATLAB.
    ##
    ## @code{@var{tc} = tscollection (@var{time})} returns a collection with no
    ## members on the numeric vector @var{time}, which must be finite and is
    ## sorted, in seconds.
    ##
    ## Each member is named by @code{matlab.lang.makeValidName} of the name of
    ## its series, which it then takes: a series named @qcode{'my data'}
    ## becomes the member @qcode{myData}, and one named @qcode{''} the member
    ## @qcode{x}.  The names must be unique ignoring case, and none may be the
    ## name of a property: @qcode{Name}, @qcode{Time}, @qcode{TimeInfo} or
    ## @qcode{Length}.  MATLAB refuses only @qcode{Length}, and a member named
    ## @qcode{Time} then hides the time vector.
    ##
    ## @code{@var{tc} = tscollection (@dots{}, @qcode{'Name'}, @var{name})}
    ## names the collection, @qcode{'unnamed'} by default; the option name is
    ## matched in any case, and a later occurrence overrides an earlier one.
    ##
    ## MATLAB ignores a second positional argument, so that
    ## @code{tscollection (@var{ts1}, @var{ts2})} holds @var{ts1} only; here it
    ## is refused.
    ##
    ## @seealso{timeseries, tscollection.addts, istscollection}
    ## @end deftypefn
    function this = tscollection (varargin)

      if (nargin == 0)
        return;
      endif
      arg1 = varargin{1};
      opts = varargin(2:end);
      if (! isempty (opts) && ! isText (opts{1}))
        error ("tscollection: too many input arguments.");
      endif
      if (mod (numel (opts), 2) != 0)
        error ("tscollection: name-value arguments must be in pairs.");
      endif
      for i = 1:2:numel (opts)
        if (! (isText (opts{i}) && strcmpi (opts{i}, 'Name')))
          error ("tscollection: invalid optional paired argument.");
        endif
        if (! isText (opts{i+1}))
          error ("tscollection: 'Name' must be a character vector.");
        endif
        this.name_ = char (opts{i+1});
      endfor

      if (isa (arg1, 'timeseries'))
        if (! isscalar (arg1))
          error (strcat ("tscollection: TS must be a single timeseries;", ...
                         " give several in a cell array."));
        endif
        ## A series with no samples gives an empty collection, as in MATLAB
        if (arg1.Length == 0)
          return;
        endif
        list = {arg1};
      elseif (iscell (arg1))
        if (! all (cellfun (@(x) isa (x, 'timeseries') && isscalar (x), ...
                            arg1(:))))
          error (strcat ("tscollection: a cell array must hold one", ...
                         " timeseries in each cell."));
        endif
        if (any (cellfun (@(x) x.Length == 0, arg1(:))))
          error (strcat ("tscollection: a timeseries with no samples", ...
                         " cannot join a collection in a cell array."));
        endif
        list = arg1(:)';
      elseif (isnumeric (arg1) && isreal (arg1)
              && (isvector (arg1) || isempty (arg1)))
        time = double (arg1(:));
        if (! all (isfinite (time)))
          error ("tscollection: TIME must be finite.");
        endif
        this.time_ = sort (time);
        this.timeInfo_.TimeVector = this.time_;
        return;
      else
        error (strcat ("tscollection: the first argument must be a time", ...
                       " vector, a timeseries or a cell array of timeseries."));
      endif
      if (! isempty (list))
        this.time_ = list{1}.Time;
        ti = list{1}.TimeInfo;
        ti.TimeVector = this.time_;
        this.timeInfo_ = ti;
      endif
      [this, errmsg] = addMembers (this, list);
      if (! isempty (errmsg))
        error ("tscollection: %s", errmsg);
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{S} =} get (@var{tc})
    ## @deftypefnx {tscollection} {@var{value} =} get (@var{tc}, @var{name})
    ## @deftypefnx {tscollection} {@var{values} =} get (@var{tc}, @var{names})
    ##
    ## Return property values and members.
    ##
    ## @code{@var{S} = get (@var{tc})} returns a structure of the four
    ## properties of the collection @var{tc} followed by its members.
    ##
    ## @code{@var{value} = get (@var{tc}, @var{name})} returns the property or
    ## member @var{name}, a character vector or a string scalar matched in any
    ## case.
    ##
    ## @code{@var{values} = get (@var{tc}, @var{names})} returns a row cell
    ## array of the properties and members named in the cell array
    ## @var{names}.
    ##
    ## @seealso{tscollection.set, tscollection.fieldnames}
    ## @end deftypefn
    function out = get (this, varargin)
      scope = 'tscollection.get';
      if (numel (varargin) > 1)
        error ("%s: too many input arguments.", scope);
      endif
      allNames = fieldnames (this);
      if (isempty (varargin))
        out = struct ();
        for i = 1:numel (allNames)
          out.(allNames{i}) = fieldValue (this, allNames{i});
        endfor
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
      out = cell (1, numel (names));
      for i = 1:numel (names)
        k = find (strcmpi (names{i}, allNames), 1);
        if (isempty (k))
          error ("%s: unknown property: '%s'", scope, names{i});
        endif
        out{i} = fieldValue (this, allNames{k});
      endfor
      if (! isList)
        out = out{1};
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {} set (@var{tc}, @var{name}, @var{value})
    ## @deftypefnx {tscollection} {} set (@var{tc}, @var{name1}, @var{value1}, @dots{})
    ## @deftypefnx {tscollection} {@var{tc2} =} set (@var{tc}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tscollection} {@var{S} =} set (@var{tc})
    ##
    ## Set property values and members.
    ##
    ## @code{set (@var{tc}, @var{name}, @var{value}, @dots{})} sets each
    ## property or member @var{name}, matched in any case, to its @var{value},
    ## in the order given, in the variable @var{tc} of the caller, as dot
    ## assignment does.  @var{tc} must therefore be a variable.
    ##
    ## @code{@var{tc2} = set (@var{tc}, @var{name}, @var{value}, @dots{})}
    ## returns the modified collection and leaves @var{tc} as it was.
    ##
    ## @code{@var{S} = set (@var{tc})} returns what @code{get (@var{tc})}
    ## does.
    ##
    ## @seealso{tscollection.get}
    ## @end deftypefn
    function varargout = set (this, varargin)
      scope = 'tscollection.set';
      if (nargin == 1)
        varargout{1} = get (this);
        return;
      endif
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      allNames = fieldnames (this);
      for i = 1:2:numel (varargin)
        name = varargin{i};
        if (! isText (name))
          error ("%s: NAME must be a character vector.", scope);
        endif
        k = find (strcmpi (char (name), allNames), 1);
        if (isempty (k))
          error ("%s: unknown property: '%s'", scope, char (name));
        endif
        this = subsasgn (this, struct ('type', '.', 'subs', allNames{k}), ...
                         varargin{i+1});
      endfor
      if (nargout > 0)
        varargout{1} = this;
        return;
      endif
      ## With no output the caller's own variable is set, as in MATLAB
      name = inputname (1);
      if (isempty (name))
        error (strcat ("%s: with no output argument, TC must be a", ...
                       " variable."), scope);
      endif
      assignin ('caller', name, this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{names} =} fieldnames (@var{tc})
    ##
    ## Return the names of the properties and members.
    ##
    ## @code{@var{names} = fieldnames (@var{tc})} returns a column cell array
    ## of @qcode{Name}, @qcode{Time}, @qcode{TimeInfo} and @qcode{Length},
    ## followed by the names of the members of @var{tc} in order.
    ##
    ## @seealso{tscollection.properties, tscollection.gettimeseriesnames}
    ## @end deftypefn
    function names = fieldnames (this)
      names = [{'Name'; 'Time'; 'TimeInfo'; 'Length'}; this.names_(:)];
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{names} =} properties (@var{tc})
    ##
    ## Return the names of the properties and members.
    ##
    ## @code{@var{names} = properties (@var{tc})} returns what
    ## @code{fieldnames (@var{tc})} does: the members are reached as properties
    ## are.
    ##
    ## @seealso{tscollection.fieldnames}
    ## @end deftypefn
    function names = properties (this)
      names = fieldnames (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{sz} =} size (@var{tc})
    ## @deftypefnx {tscollection} {@var{dim_sz} =} size (@var{tc}, @var{dim})
    ## @deftypefnx {tscollection} {[@var{nsamples}, @var{nmembers}] =} size (@var{tc})
    ##
    ## Return the number of samples and the number of members.
    ##
    ## @code{@var{sz} = size (@var{tc})} returns @code{[@var{nsamples},
    ## @var{nmembers}]}.  @code{size (@var{tc}, @var{dim})} returns the size
    ## along dimension @var{dim}, a positive integer or a vector of them;
    ## dimensions above 2 have size 1.  With several outputs each takes one
    ## dimension, where MATLAB refuses more than one output.
    ##
    ## @seealso{tscollection.length, tscollection.isempty}
    ## @end deftypefn
    function varargout = size (this, dim)
      sz = [numel(this.time_), numel(this.names_)];
      if (nargin == 2)
        if (! (isnumeric (dim) && isreal (dim) && ! isempty (dim)
               && all (dim(:) == fix (dim(:)) & dim(:) >= 1)))
          error ("tscollection.size: DIM must be positive integers.");
        endif
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
    ## @deftypefn {tscollection} {@var{n} =} length (@var{tc})
    ##
    ## Return the number of samples.
    ##
    ## @code{@var{n} = length (@var{tc})} returns @qcode{@var{tc}.Length}, the
    ## number of times, whatever the number of members.
    ##
    ## @seealso{tscollection.size}
    ## @end deftypefn
    function n = length (this)
      n = numel (this.time_);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{tf} =} isempty (@var{tc})
    ##
    ## True for a collection with no samples.
    ##
    ## @code{@var{tf} = isempty (@var{tc})} returns @code{true} when the
    ## collection @var{tc} has no times, whether it has members or not, and
    ## @code{false} for a collection with times and no members.
    ##
    ## @seealso{tscollection.size}
    ## @end deftypefn
    function tf = isempty (this)
      tf = isempty (this.time_);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{tf} =} isequal (@var{tc1}, @var{tc2}, @dots{})
    ##
    ## True when collections are equal.
    ##
    ## @code{@var{tf} = isequal (@var{tc1}, @var{tc2}, @dots{})} returns
    ## @code{true} when every argument is a @code{tscollection} with the same
    ## name, time vector and @qcode{TimeInfo}, and the same members in the same
    ## order, each equal as @code{isequal} compares series, @code{NaN} unequal
    ## to itself.
    ##
    ## @seealso{timeseries.isequal}
    ## @end deftypefn
    function tf = isequal (varargin)
      if (nargin < 2)
        error ("tscollection.isequal: invalid number of input arguments.");
      endif
      tf = false;
      a = varargin{1};
      for i = 2:nargin
        b = varargin{i};
        if (! (isa (a, 'tscollection') && isa (b, 'tscollection')))
          return;
        endif
        if (! (strcmp (a.name_, b.name_) && isequal (a.time_, b.time_)
               && sameTimeInfo (a.timeInfo_, b.timeInfo_, true)
               && isequal (a.names_, b.names_)))
          return;
        endif
        for k = 1:numel (a.members_)
          if (! isequal (a.members_{k}, b.members_{k}))
            return;
          endif
        endfor
      endfor
      tf = true;
    endfunction

  endmethods

################################################################################
##                              ** Members **                                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'addts'            'removets'         'gettimeseriesnames'                 ##
## 'settimeseriesnames'                  'setTimeseriesName'                  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} addts (@var{tc}, @var{ts})
    ## @deftypefnx {tscollection} {@var{tc} =} addts (@var{tc}, @{@var{ts1}, @var{ts2}, @dots{}@})
    ## @deftypefnx {tscollection} {@var{tc} =} addts (@var{tc}, @var{data}, @var{name})
    ##
    ## Add members to a collection.
    ##
    ## @code{@var{tc} = addts (@var{tc}, @var{ts})} adds the @code{timeseries}
    ## @var{ts} as the last member of @var{tc}, and
    ## @code{@var{tc} = addts (@var{tc}, @{@var{ts1}, @var{ts2}, @dots{}@})}
    ## adds each series in the cell array in turn.  Each must have the times of
    ## the collection, in the same units; a series with a
    ## @qcode{TimeInfo.StartDate} added to a collection without one loses its
    ## start date, with a warning, as in MATLAB, and the reverse is refused.  A
    ## collection with no samples and no members takes the times of the first
    ## series.  Each member is named as the constructor names it.
    ##
    ## @code{@var{tc} = addts (@var{tc}, @var{data}, @var{name})} adds a member
    ## @var{name} holding the samples @var{data}, a numeric or logical array of
    ## one sample per time of the collection, read against the time vector as
    ## the @code{timeseries} constructor reads it.
    ##
    ## Names must be unique ignoring case: MATLAB refuses @qcode{A} beside
    ## @qcode{a} here, though @code{settimeseriesnames} allows it there.
    ## MATLAB also ignores a fourth argument, which is refused here.
    ##
    ## @seealso{tscollection.removets, tscollection.settimeseriesnames}
    ## @end deftypefn
    function this = addts (this, varargin)
      scope = 'tscollection.addts';
      if (numel (varargin) < 1)
        error ("%s: invalid number of input arguments.", scope);
      elseif (numel (varargin) > 2)
        error ("%s: too many input arguments.", scope);
      endif
      arg = varargin{1};
      if (numel (varargin) == 2)
        name = varargin{2};
        if (! isText (name))
          error ("%s: NAME must be a character vector.", scope);
        endif
        if (iscell (arg) || ! (isnumeric (arg) || islogical (arg)))
          error ("%s: DATA must be a numeric or logical array.", scope);
        endif
        if (isempty (this.time_))
          error ("%s: TC has no times to add DATA on.", scope);
        endif
        try
          ts = timeseries (arg, this.time_, 'Name', char (name));
        catch
          error ("%s: DATA must have one sample per time of TC.", scope);
        end_try_catch
        ts.TimeInfo = this.timeInfo_;
        list = {ts};
      elseif (isa (arg, 'timeseries') && isscalar (arg))
        list = {arg};
      elseif (iscell (arg) && all (cellfun (@(x) isa (x, 'timeseries') ...
                                                 && isscalar (x), arg(:))))
        list = arg(:)';
      elseif (isnumeric (arg) || islogical (arg))
        error ("%s: DATA must be given with a NAME.", scope);
      else
        error (strcat ("%s: TS must be a timeseries or a cell array of", ...
                       " timeseries."), scope);
      endif
      if (isempty (this.time_) && isempty (this.names_) && ! isempty (list))
        this.time_ = list{1}.Time;
        ti = list{1}.TimeInfo;
        ti.TimeVector = this.time_;
        this.timeInfo_ = ti;
      endif
      [this, errmsg] = addMembers (this, list);
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{tc} =} removets (@var{tc}, @var{names})
    ##
    ## Remove members from a collection.
    ##
    ## @code{@var{tc} = removets (@var{tc}, @var{names})} removes the members
    ## @var{names}, a character vector, a cell array of character vectors or a
    ## string array, each the exact name of a member.  The time vector stays,
    ## so removing every member leaves a collection with times and no members.
    ## A name that is no member is refused, where MATLAB ignores it.
    ##
    ## @seealso{tscollection.addts, tscollection.gettimeseriesnames}
    ## @end deftypefn
    function this = removets (this, names)
      scope = 'tscollection.removets';
      if (nargin < 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      [names, errmsg] = nameList (names);
      if (! isempty (errmsg))
        error ("%s: NAMES %s", scope, errmsg);
      endif
      drop = false (size (this.names_));
      for i = 1:numel (names)
        k = find (strcmp (names{i}, this.names_));
        if (isempty (k))
          error ("%s: TC has no member named '%s'.", scope, names{i});
        endif
        drop(k) = true;
      endfor
      this.members_(drop) = [];
      this.names_(drop) = [];
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{names} =} gettimeseriesnames (@var{tc})
    ##
    ## Return the names of the members.
    ##
    ## @code{@var{names} = gettimeseriesnames (@var{tc})} returns a row cell
    ## array of the names of the members of @var{tc}, in order, or @code{@{@}}
    ## when it has none.
    ##
    ## @seealso{tscollection.settimeseriesnames, tscollection.fieldnames}
    ## @end deftypefn
    function names = gettimeseriesnames (this)
      if (isempty (this.names_))
        names = {};
      else
        names = this.names_;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{tc} =} settimeseriesnames (@var{tc}, @var{old}, @var{new})
    ##
    ## Rename a member.
    ##
    ## @code{@var{tc} = settimeseriesnames (@var{tc}, @var{old}, @var{new})}
    ## renames the member @var{old}, its exact name, to @var{new}, a valid
    ## variable name, both character vectors or string scalars; the series
    ## takes the new name too, and the member becomes the last, as in
    ## MATLAB.  @var{new} must not be the name of another member, ignoring
    ## case, nor of one of the four properties.
    ##
    ## MATLAB accepts the name of another member and drops that member, and
    ## accepts a name differing from another only in case, after which that
    ## member cannot be reached; both are refused here.
    ##
    ## @seealso{tscollection.gettimeseriesnames, tscollection.addts}
    ## @end deftypefn
    function this = settimeseriesnames (this, old, new)
      scope = 'tscollection.settimeseriesnames';
      if (nargin < 3)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (! isText (old))
        error ("%s: OLD must be a character vector.", scope);
      endif
      old = char (old);
      k = find (strcmp (old, this.names_));
      if (isempty (k))
        error ("%s: TC has no member named '%s'.", scope, old);
      endif
      if (! isText (new) || ! isvarname (char (new)))
        error ("%s: NEW must be a valid variable name.", scope);
      endif
      new = char (new);
      if (strcmp (new, old))
        return;
      endif
      others = this.names_;
      others(k) = [];
      errmsg = nameClash (new, others);
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      ## The renamed member moves to the end, as in MATLAB
      m = this.members_{k};
      m.Name = new;
      this.names_ = [others, {new}];
      this.members_(k) = [];
      this.members_{end+1} = m;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{tc} =} setTimeseriesName (@var{tc}, @var{old}, @var{new})
    ##
    ## Rename a member.
    ##
    ## @code{@var{tc} = setTimeseriesName (@var{tc}, @var{old}, @var{new})} is
    ## @code{settimeseriesnames}, kept for compatibility; MATLAB marks it
    ## obsolete.
    ##
    ## @seealso{tscollection.settimeseriesnames}
    ## @end deftypefn
    function this = setTimeseriesName (this, varargin)
      try
        this = settimeseriesnames (this, varargin{:});
      catch err
        error ("tscollection.setTimeseriesName: %s", ...
               regexprep (err.message, '^[\w.]+: ', ''));
      end_try_catch
    endfunction

  endmethods

################################################################################
##                          ** Samples and time **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'addsampletocollection'               'delsamplefromcollection'            ##
## 'getsampleusingtime'                  'hasduplicatetimes'                  ##
## 'getabstime'       'setabstime'       'resample'                           ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} addsampletocollection (@var{tc}, @qcode{'Time'}, @var{time}, @var{name1}, @var{data1}, @dots{})
    ##
    ## Add samples to every member.
    ##
    ## @code{@var{tc} = addsampletocollection (@var{tc}, @qcode{'Time'},
    ## @var{time}, @var{name1}, @var{data1}, @dots{})} inserts samples at the
    ## times @var{time}, keeping the collection in time order; a sample at a
    ## time already present goes after the samples there.  The member
    ## @var{name1}, its exact name, takes the samples @var{data1}, one per
    ## time, read against @var{time} as the @code{timeseries} constructor reads
    ## data against times and each of the size of the member's samples.  A
    ## member not named takes @code{NaN}, or 0 for integer data and
    ## @code{false} for logical data.  @qcode{'Time'} is matched in any case.
    ##
    ## For a collection with no @qcode{TimeInfo.StartDate} @var{time} is
    ## numbers in its units; for one with a start date it may also be dates as
    ## text, or a @code{datetime}.  A member with quality codes gives each new
    ## sample the code of the sample nearest in time, the later one on a tie,
    ## as MATLAB does.
    ##
    ## @seealso{tscollection.delsamplefromcollection, timeseries.addsample}
    ## @end deftypefn
    function this = addsampletocollection (this, varargin)
      scope = 'tscollection.addsampletocollection';
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      time = [];
      data = cell (size (this.names_));
      given = false (size (this.names_));
      for i = 1:2:numel (varargin)
        if (! isText (varargin{i}))
          error ("%s: invalid optional paired argument.", scope);
        endif
        key = char (varargin{i});
        if (strcmpi (key, 'Time'))
          time = varargin{i+1};
          continue;
        endif
        k = find (strcmp (key, this.names_));
        if (isempty (k))
          error ("%s: TC has no member named '%s'.", scope, key);
        endif
        if (given(k))
          error ("%s: member '%s' is given more than once.", scope, key);
        endif
        given(k) = true;
        data{k} = varargin{i+1};
      endfor
      if (isempty (time))
        error ("%s: 'Time' must not be empty.", scope);
      endif
      if (isnumeric (time) && ! all (isfinite (time(:))))
        error ("%s: 'Time' must be finite.", scope);
      endif
      if (ischar (time))
        nNew = rows (time);
      else
        nNew = numel (time);
      endif

      ## The new time vector, from a series of sample positions
      try
        p = addsample (proxy (this), 'Data', zeros (nNew, 1), 'Time', time);
      catch err
        error ("%s: %s", scope, proxyMessage (err));
      end_try_catch

      for k = 1:numel (this.members_)
        m = this.members_{k};
        ss = getdatasamplesize (m);
        if (given(k))
          d = data{k};
          if (iscell (d) || ! (isnumeric (d) || islogical (d)))
            error (strcat ("%s: the data of '%s' must be a numeric or", ...
                           " logical array."), scope, this.names_{k});
          endif
          try
            s = timeseries (d, (1:nNew)');
          catch
            error ("%s: the data of '%s' must have one sample per time.", ...
                   scope, this.names_{k});
          end_try_catch
          if (! isempty (ss) && ! isequal (getdatasamplesize (s), ss))
            error ("%s: the data of '%s' must hold samples of size %s.", ...
                   scope, this.names_{k}, ...
                   strjoin (arrayfun (@num2str, ss, 'UniformOutput', false), ...
                            'x'));
          endif
        else
          d = fillSamples (m, ss, nNew);
        endif
        this.members_{k} = addsample (m, 'Data', d, 'Time', time);
      endfor
      this.time_ = p.Time;
      this.timeInfo_.TimeVector = this.time_;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} delsamplefromcollection (@var{tc}, @qcode{'Index'}, @var{ind})
    ## @deftypefnx {tscollection} {@var{tc} =} delsamplefromcollection (@var{tc}, @qcode{'Value'}, @var{time})
    ##
    ## Remove samples from every member.
    ##
    ## @code{@var{tc} = delsamplefromcollection (@var{tc}, @qcode{'Index'},
    ## @var{ind})} removes the samples @var{ind}, a vector of positive integers
    ## not exceeding @qcode{Length}; repeats are allowed and @code{[]} removes
    ## nothing.  A logical mask is refused, as in MATLAB.
    ##
    ## @code{@var{tc} = delsamplefromcollection (@var{tc}, @qcode{'Value'},
    ## @var{time})} removes every sample at each of the times @var{time}; a
    ## time no sample has removes nothing.  For a collection with no
    ## @qcode{TimeInfo.StartDate} the times are numbers in its units; for one
    ## with a start date they may also be dates as text, or a @code{datetime}.
    ##
    ## The option names are matched in any case.  Removing every sample leaves
    ## the members, with no samples.
    ##
    ## @seealso{tscollection.addsampletocollection, timeseries.delsample}
    ## @end deftypefn
    function this = delsamplefromcollection (this, varargin)
      scope = 'tscollection.delsamplefromcollection';
      if (numel (varargin) != 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      try
        p = delsample (proxy (this), varargin{:});
      catch err
        error ("%s: %s", scope, proxyMessage (err));
      end_try_catch
      this = subsetSamples (this, p.Data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc2} =} getsampleusingtime (@var{tc}, @var{t})
    ## @deftypefnx {tscollection} {@var{tc2} =} getsampleusingtime (@var{tc}, @var{t}, @qcode{'AllowDuplicateTimes'}, @var{tf})
    ## @deftypefnx {tscollection} {@var{tc2} =} getsampleusingtime (@var{tc}, @var{t1}, @var{t2})
    ##
    ## Return a collection of the samples at a time or between two times.
    ##
    ## @code{@var{tc2} = getsampleusingtime (@var{tc}, @var{t})} returns the
    ## collection @var{tc} holding only the sample at time @var{t}.  When
    ## several samples share that time the call is refused, unless
    ## @qcode{'AllowDuplicateTimes'} is @code{true}, which returns them all.
    ##
    ## @code{@var{tc2} = getsampleusingtime (@var{tc}, @var{t1}, @var{t2})}
    ## returns the samples from @var{t1} to @var{t2}, both included.
    ##
    ## The times are read as @code{timeseries.getsampleusingtime} reads them.
    ## No sample in range gives a collection with no samples and every member.
    ## The name and every member's properties are kept; MATLAB names the
    ## result @qcode{'unnamed'} and, when it is empty, drops every member.
    ##
    ## @seealso{timeseries.getsampleusingtime}
    ## @end deftypefn
    function this = getsampleusingtime (this, varargin)
      scope = 'tscollection.getsampleusingtime';
      try
        p = getsampleusingtime (proxy (this), varargin{:});
      catch err
        error ("%s: %s", scope, proxyMessage (err));
      end_try_catch
      this = subsetSamples (this, p.Data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{tf} =} hasduplicatetimes (@var{tc})
    ##
    ## True where a collection repeats a time.
    ##
    ## @code{@var{tf} = hasduplicatetimes (@var{tc})} returns @code{true} when
    ## two samples of the collection @var{tc} share a time.
    ##
    ## @seealso{tscollection.getsampleusingtime}
    ## @end deftypefn
    function tf = hasduplicatetimes (this)
      tf = any (diff (this.time_) == 0);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {tscollection} {@var{dates} =} getabstime (@var{tc})
    ##
    ## Return the dates of the samples as text.
    ##
    ## @code{@var{dates} = getabstime (@var{tc})} returns a column cell array
    ## of character vectors, the date of each sample of the collection
    ## @var{tc}, written as @code{timeseries.getabstime} writes them: in the
    ## format @qcode{TimeInfo.Format}, or @qcode{'dd-mmm-yyyy HH:MM:SS'} when
    ## that is empty.  MATLAB writes the default whatever
    ## @qcode{TimeInfo.Format} says.  A collection with no
    ## @qcode{TimeInfo.StartDate} is refused.
    ##
    ## @seealso{tscollection.setabstime, timeseries.getabstime}
    ## @end deftypefn
    function dates = getabstime (this)
      if (isempty (this.timeInfo_.StartDate))
        error ("tscollection.getabstime: TC has no 'TimeInfo.StartDate'.");
      endif
      dates = getabstime (proxy (this));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} setabstime (@var{tc}, @var{dates})
    ## @deftypefnx {tscollection} {@var{tc} =} setabstime (@var{tc}, @var{dates}, @var{format})
    ##
    ## Set the time vector of a collection from dates.
    ##
    ## @code{@var{tc} = setabstime (@var{tc}, @var{dates})} gives the samples
    ## of the collection @var{tc} the dates @var{dates}, one per sample, in
    ## order, exactly as @code{timeseries.setabstime} gives them to a series,
    ## in the collection's own units; every member takes them.  With
    ## @var{format} every date is read strictly in it.
    ##
    ## The dates need not be sorted: every member's samples, with their quality
    ## codes, are sorted with them.  MATLAB sorts the times alone, so each
    ## sample is given another's date.  It also reads every date by the format
    ## of the first, forces text into @var{format}, and takes numbers and
    ## @code{datetime} values as relative times, dropping the dates; see
    ## @code{timeseries.setabstime} for how each is read here.
    ##
    ## @seealso{tscollection.getabstime, timeseries.setabstime}
    ## @end deftypefn
    function this = setabstime (this, varargin)
      scope = 'tscollection.setabstime';
      if (numel (varargin) < 1 || numel (varargin) > 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      try
        p = setabstime (proxy (this), varargin{:});
      catch err
        error ("%s: %s", scope, proxyMessage (err));
      end_try_catch
      for k = 1:numel (this.members_)
        this.members_{k} = setabstime (this.members_{k}, varargin{:});
      endfor
      this.time_ = p.Time;
      this.timeInfo_ = p.TimeInfo;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} resample (@var{tc}, @var{time})
    ## @deftypefnx {tscollection} {@var{tc} =} resample (@var{tc}, @var{time}, @var{method})
    ## @deftypefnx {tscollection} {@var{tc} =} resample (@var{tc}, @var{time}, @var{method}, @var{code})
    ##
    ## Evaluate every member at new times.
    ##
    ## @code{@var{tc} = resample (@var{tc}, @var{time})} returns the collection
    ## @var{tc} with every member evaluated at @var{time} by its own
    ## interpolation method, as @code{timeseries.resample} evaluates a series:
    ## at a time outside the collection the data is @code{NaN}, and a member
    ## with quality codes gives each new time the code of the nearest sample.
    ## MATLAB raises there for a member with quality codes.
    ##
    ## @code{@var{tc} = resample (@var{tc}, @var{time}, @var{method})} uses
    ## @var{method}, @qcode{'linear'} or @qcode{'zoh'}, for every member;
    ## @code{[]} keeps each member's own.
    ##
    ## @code{@var{tc} = resample (@var{tc}, @var{time}, @var{method},
    ## @var{code})} gives the quality code @var{code} to every new time the
    ## collection did not already hold, in each member with quality codes; it
    ## must be listed in the @qcode{QualityInfo.Code} of each of them.  Members
    ## without quality codes are resampled without it.
    ##
    ## @seealso{timeseries.resample}
    ## @end deftypefn
    function this = resample (this, varargin)
      scope = 'tscollection.resample';
      if (numel (varargin) < 1 || numel (varargin) > 3)
        error ("%s: invalid number of input arguments.", scope);
      endif
      method = [];
      code = [];
      if (numel (varargin) >= 2)
        method = varargin{2};
      endif
      if (numel (varargin) == 3)
        code = varargin{3};
      endif
      if (isempty (this.time_))
        return;
      endif
      if (numel (this.time_) < 2)
        error ("%s: TC must hold at least two samples.", scope);
      endif
      try
        p = resample (proxy (this), varargin{1}, method);
      catch err
        error ("%s: %s", scope, proxyMessage (err));
      end_try_catch
      hasQ = cellfun (@(m) ! isempty (m.Quality), this.members_);
      if (! isempty (code) && ! any (hasQ))
        error ("%s: CODE is given, but no member has quality codes.", scope);
      endif
      for k = 1:numel (this.members_)
        try
          if (hasQ(k))
            this.members_{k} = resample (this.members_{k}, varargin{1}, ...
                                         method, code);
          else
            this.members_{k} = resample (this.members_{k}, varargin{1}, ...
                                         method);
          endif
        catch err
          error ("%s: member '%s': %s", scope, this.names_{k}, ...
                 bareMessage (err));
        end_try_catch
      endfor
      this.time_ = p.Time;
      this.timeInfo_.TimeVector = this.time_;
    endfunction

  endmethods

################################################################################
##                    ** Indexing and concatenation **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'subsref'          'subsasgn'         'end'                                ##
## 'horzcat'          'vertcat'                                               ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} vertcat (@var{tc1}, @var{tc2}, @dots{})
    ##
    ## Join collections end to end in time.
    ##
    ## @code{@var{tc} = [@var{tc1}; @var{tc2}; @dots{}]} returns one collection
    ## holding the samples of every argument in turn.  The collections must
    ## have the same members, by name, taken in the order of the first, with a
    ## warning when another orders them differently, as in MATLAB.  Each must
    ## start at or after the end of the one before, so equal times where two
    ## meet give a repeated time.  Each member is joined as
    ## @code{timeseries.append} joins series: times in the coarsest units among
    ## them, dates counted from the first start date, and samples of one size.
    ##
    ## A member's quality codes are joined when it has them in every
    ## collection, and refused when only some do; MATLAB then gives a member
    ## that cannot be read, or drops them.  The result keeps the name of the
    ## first collection, where MATLAB names it @qcode{'unnamed'}.
    ##
    ## @seealso{tscollection.horzcat, timeseries.append}
    ## @end deftypefn
    function out = vertcat (varargin)
      scope = 'tscollection.vertcat';
      if (! all (cellfun (@(x) isa (x, 'tscollection'), varargin)))
        error ("%s: every argument must be a tscollection.", scope);
      endif
      out = varargin{1};
      if (nargin == 1)
        return;
      endif
      names = out.names_;
      for i = 2:nargin
        other = varargin{i}.names_;
        if (numel (other) != numel (names)
            || ! isempty (setdiff (other, names)))
          error ("%s: every collection must have the same members.", scope);
        endif
        if (! isequal (other, names))
          warning (strcat ("%s: the members are taken in the order of the", ...
                           " first collection."), scope);
        endif
      endfor

      ## The new time vector, from series of sample positions
      proxies = cellfun (@proxy, varargin, 'UniformOutput', false);
      try
        p = append (proxies{:});
      catch err
        error ("%s: %s", scope, collectionMessage (err));
      end_try_catch

      for k = 1:numel (names)
        list = cell (1, nargin);
        for i = 1:nargin
          c = varargin{i};
          list{i} = c.members_{strcmp (names{k}, c.names_)};
        endfor
        full = list(cellfun (@(m) m.Length > 0, list));
        sizes = cellfun (@getdatasamplesize, full, 'UniformOutput', false);
        if (! all (cellfun (@(s) isequal (s, sizes{1}), sizes)))
          error ("%s: the samples of '%s' must be of one size throughout.", ...
                 scope, names{k});
        endif
        hasQ = cellfun (@(m) ! isempty (m.Quality), full);
        if (any (hasQ) && ! all (hasQ))
          error (strcat ("%s: '%s' must have quality codes in every", ...
                         " collection or in none."), scope, names{k});
        endif
        m = append (list{:});
        m.Name = names{k};
        out.members_{k} = m;
      endfor
      out.time_ = p.Time;
      out.timeInfo_ = p.TimeInfo;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tscollection} {@var{tc} =} horzcat (@var{tc1}, @var{tc2}, @dots{})
    ##
    ## Join the members of collections.
    ##
    ## @code{@var{tc} = [@var{tc1}, @var{tc2}, @dots{}]} returns one collection
    ## holding the members of every argument, sorted by name, as in MATLAB,
    ## capitals before lower case letters.  The collections must
    ## have the same time vector, in the same units, and the same
    ## @qcode{TimeInfo.StartDate}.  A member name present in more than one is
    ## taken once when the members are equal in every property, @code{NaN}
    ## equal to itself, and refused otherwise; MATLAB compares their data
    ## alone.  Names differing only in case are refused.  The result keeps the name of the first collection, where
    ## MATLAB names it @qcode{'unnamed'}.
    ##
    ## @seealso{tscollection.vertcat, tscollection.addts}
    ## @end deftypefn
    function out = horzcat (varargin)
      scope = 'tscollection.horzcat';
      if (! all (cellfun (@(x) isa (x, 'tscollection'), varargin)))
        error ("%s: every argument must be a tscollection.", scope);
      endif
      out = varargin{1};
      for i = 2:nargin
        c = varargin{i};
        if (! (isequal (c.time_, out.time_)
               && sameTimeInfo (c.timeInfo_, out.timeInfo_, false)))
          error ("%s: every collection must have the same time vector.", scope);
        endif
        for k = 1:numel (c.names_)
          name = c.names_{k};
          j = find (strcmp (name, out.names_));
          if (! isempty (j))
            if (! isequalwithequalnans (out.members_{j}, c.members_{k}))
              error (strcat ("%s: the member '%s' shared by the", ...
                             " collections must be the same in each."), ...
                     scope, name);
            endif
            continue;
          endif
          errmsg = nameClash (name, out.names_);
          if (! isempty (errmsg))
            error ("%s: %s", scope, errmsg);
          endif
          out.names_{end+1} = name;
          out.members_{end+1} = c.members_{k};
        endfor
      endfor
      ## MATLAB sorts the members of the result by name
      if (nargin > 1)
        [out.names_, idx] = sort (out.names_);
        out.members_ = out.members_(idx);
      endif
    endfunction

  endmethods

  methods (Hidden)

    function last_index = end (this, end_dim, ndim_obj)
      if (end_dim == 2)
        last_index = numel (this.names_);
      elseif (ndim_obj > 2 && end_dim > 2)
        last_index = 1;
      else
        last_index = numel (this.time_);
      endif
    endfunction

    function varargout = subsref (this, s)
      scope = 'tscollection.subsref';
      switch (s(1).type)
        case '()'
          out = indexed (this, s(1).subs, scope);
        case '{}'
          error ("%s: '{}' indexing is not supported.", scope);
        case '.'
          name = fieldName (s(1).subs, scope);
          out = fieldValue (this, name);
          if (isempty (out) && ! any (strcmp (name, fieldnames (this))))
            error ("%s: TC has no property or member named '%s'.", ...
                   scope, name);
          endif
      endswitch
      if (numel (s) > 1)
        out = subsref (out, s(2:end));
      endif
      varargout{1} = out;
    endfunction

    function this = subsasgn (this, s, val)
      scope = 'tscollection.subsasgn';
      if (! strcmp (s(1).type, '.'))
        error (strcat ("%s: only '.' assignment is supported; select", ...
                       " samples and members with '()' and assign the", ...
                       " result."), scope);
      endif
      name = fieldName (s(1).subs, scope);
      chain = s(2:end);
      switch (name)
        case 'Name'
          if (! isempty (chain))
            val = subsasgn (this.name_, chain, val);
          endif
          if (! isText (val))
            error ("%s: 'Name' must be a character vector.", scope);
          endif
          this.name_ = char (val);
        case 'Time'
          if (! isempty (chain))
            val = subsasgn (this.time_, chain, val);
          endif
          this = setTime (this, val, scope);
        case 'TimeInfo'
          if (! isempty (chain))
            val = subsasgn (this.timeInfo_, chain, val);
          endif
          if (! (isa (val, 'tsdata.timemetadata') && isscalar (val)))
            error (strcat ("%s: 'TimeInfo' must be a tsdata.timemetadata", ...
                           " object."), scope);
          endif
          val.TimeVector = this.time_;
          this.timeInfo_ = val;
          for k = 1:numel (this.members_)
            this.members_{k}.TimeInfo = val;
          endfor
        case 'Length'
          error ("%s: 'Length' is read-only.", scope);
        otherwise
          k = find (strcmp (name, this.names_));
          if (isempty (k))
            if (! isempty (chain))
              error ("%s: TC has no member named '%s'.", scope, name);
            endif
            if (! (isa (val, 'timeseries') && isscalar (val)))
              error (strcat ("%s: a member must be a timeseries; remove", ...
                             " one with removets."), scope);
            endif
            val.Name = name;
            try
              this = addts (this, val);
            catch err
              error ("%s: %s", scope, bareMessage (err));
            end_try_catch
            return;
          endif
          m = this.members_{k};
          if (! isempty (chain))
            if (strcmp (chain(1).type, '.') && ischar (chain(1).subs))
              switch (chain(1).subs)
                case {'Time', 'TimeInfo'}
                  error (strcat ("%s: set the time of every member with", ...
                                 " TC.%s."), scope, chain(1).subs);
                case 'Name'
                  error ("%s: rename a member with settimeseriesnames.", ...
                         scope);
              endswitch
            endif
            val = subsasgn (m, chain, val);
          endif
          if (! (isa (val, 'timeseries') && isscalar (val)))
            error (strcat ("%s: a member must be a timeseries; remove one", ...
                           " with removets."), scope);
          endif
          if (! (isequal (val.Time, this.time_)
                 && sameTimeInfo (val.TimeInfo, this.timeInfo_, false)))
            error (strcat ("%s: the time vector of '%s' must match that of", ...
                           " TC."), scope, name);
          endif
          val.Name = name;
          val.TimeInfo = this.timeInfo_;
          this.members_{k} = val;
      endswitch
    endfunction

  endmethods

  methods (Access = private)

    ## Add the series in LIST, a cell array, as members of this collection,
    ## whose time vector they must share.  Returns an empty ERRMSG, or the
    ## body of the message the caller raises.
    function [this, errmsg] = addMembers (this, list)
      errmsg = '';
      for i = 1:numel (list)
        ts = list{i};
        key = matlab.lang.makeValidName (ts.Name);
        ti = ts.TimeInfo;
        if (! isempty (ti.StartDate) && isempty (this.timeInfo_.StartDate))
          warning (strcat ("tscollection: the start date of '%s' is", ...
                           " dropped, since the collection has none."), key);
          ti.StartDate = '';
        endif
        if (! (isequal (ts.Time, this.time_)
               && sameTimeInfo (ti, this.timeInfo_, false)))
          errmsg = sprintf (strcat ("the time vector of '%s' must match", ...
                                    " that of the collection."), key);
          return;
        endif
        errmsg = nameClash (key, this.names_);
        if (! isempty (errmsg))
          return;
        endif
        ts.Name = key;
        ts.TimeInfo = this.timeInfo_;
        this.names_{end+1} = key;
        this.members_{end+1} = ts;
      endfor
    endfunction

    ## The value of the property or member NAME, or [] for neither.
    function val = fieldValue (this, name)
      switch (name)
        case 'Name'
          val = this.name_;
        case 'Time'
          val = this.time_;
        case 'TimeInfo'
          val = this.timeInfo_;
        case 'Length'
          val = numel (this.time_);
        otherwise
          k = find (strcmp (name, this.names_));
          if (isempty (k))
            val = [];
          else
            val = this.members_{k};
          endif
      endswitch
    endfunction

    ## A series of the positions 1 to N of the samples, on this collection's
    ## time vector and TimeInfo, through which the 'timeseries' methods read
    ## times and select samples for every member alike.
    function p = proxy (this)
      n = numel (this.time_);
      p = timeseries ((1:n)', this.time_);
      p.TimeInfo = this.timeInfo_;
    endfunction

    ## The collection holding only the samples IDX, in time order.
    function this = subsetSamples (this, idx)
      idx = idx(:);
      this.time_ = this.time_(idx);
      this.timeInfo_.TimeVector = this.time_;
      for k = 1:numel (this.members_)
        this.members_{k} = getsamples (this.members_{k}, idx);
      endfor
    endfunction

    ## Assign the time vector TIME to the collection and every member.
    function this = setTime (this, time, scope)
      if (! (isnumeric (time) && isreal (time)
             && (isvector (time) || isempty (time))))
        error ("%s: 'Time' must be a numeric vector.", scope);
      endif
      time = double (time(:));
      if (! all (isfinite (time)))
        error ("%s: 'Time' must be finite.", scope);
      endif
      if (any (diff (time) < 0))
        error ("%s: 'Time' must be non-decreasing.", scope);
      endif
      if (numel (time) != numel (this.time_))
        error ("%s: 'Time' must have one time per sample.", scope);
      endif
      this.time_ = time;
      this.timeInfo_.TimeVector = time;
      for k = 1:numel (this.members_)
        this.members_{k}.Time = time;
      endfor
    endfunction

    ## The collection '()' indexing selects with the subscripts SUBS:
    ## samples, then members by position or name.
    function this = indexed (this, subs, scope)
      if (numel (subs) < 1 || numel (subs) > 2)
        error ("%s: '()' indexing takes one or two subscripts.", scope);
      endif
      n = numel (this.time_);
      ind = subs{1};
      if (ischar (ind) && strcmp (ind, ':'))
        idx = (1:n)';
      elseif (islogical (ind))
        if (! (isvector (ind) || isempty (ind)) || numel (ind) > n)
          error (strcat ("%s: a logical sample index must not be longer", ...
                         " than TC."), scope);
        endif
        idx = find (ind(:));
      elseif (isnumeric (ind) && isreal (ind))
        idx = double (ind(:));
        if (! all (idx == fix (idx) & idx >= 1 & idx <= n))
          error (strcat ("%s: sample indices must be positive integers not", ...
                         " exceeding the number of samples."), scope);
        endif
        idx = unique (idx);
      else
        error (strcat ("%s: sample indices must be positive integers, a", ...
                       " logical mask or ':'."), scope);
      endif
      this = subsetSamples (this, idx);
      if (numel (subs) < 2)
        return;
      endif
      ind = subs{2};
      nm = numel (this.names_);
      if (ischar (ind) && strcmp (ind, ':'))
        return;
      elseif (isnumeric (ind) && isreal (ind))
        k = double (ind(:))';
        if (! all (k == fix (k) & k >= 1 & k <= nm))
          error (strcat ("%s: member indices must be positive integers not", ...
                         " exceeding the number of members."), scope);
        endif
      elseif (islogical (ind))
        if (! (isvector (ind) || isempty (ind)) || numel (ind) > nm)
          error (strcat ("%s: a logical member index must not be longer", ...
                         " than the number of members."), scope);
        endif
        k = find (ind(:))';
      else
        [names, errmsg] = nameList (ind);
        if (! isempty (errmsg))
          error (strcat ("%s: members must be given by position, by name", ...
                         " or as ':'."), scope);
        endif
        k = zeros (1, numel (names));
        for i = 1:numel (names)
          j = find (strcmp (names{i}, this.names_));
          if (isempty (j))
            error ("%s: TC has no member named '%s'.", scope, names{i});
          endif
          k(i) = j;
        endfor
      endif
      k = unique (k, 'stable');
      this.names_ = this.names_(k);
      this.members_ = this.members_(k);
    endfunction

  endmethods

  ## Property access, for the texinfo and 'properties' of the class; dot
  ## access from outside goes through 'subsref'
  methods

    function val = get.Name (this)
      val = this.name_;
    endfunction

    function val = get.Time (this)
      val = this.time_;
    endfunction

    function val = get.TimeInfo (this)
      val = this.timeInfo_;
    endfunction

    function val = get.Length (this)
      val = numel (this.time_);
    endfunction

  endmethods

endclassdef

## True for a character vector (or '') and for a string scalar.
function tf = isText (x)
  tf = (ischar (x) && (isrow (x) || isempty (x))) ...
       || (isa (x, 'string') && isscalar (x));
endfunction

## Read a '.' subscript: a character vector or a string scalar.
function name = fieldName (subs, scope)
  if (isstring (subs) && isscalar (subs))
    subs = char (subs);
  endif
  if (! (ischar (subs) && isrow (subs)))
    error (strcat ("%s: '.' index argument must be a character vector or a", ...
                   " string scalar."), scope);
  endif
  name = subs;
endfunction

## Read member names given as a character vector, a cell array of character
## vectors or a string array.  Returns a cellstr and an empty ERRMSG, or the
## rest of the message the caller raises after naming them.
function [names, errmsg] = nameList (names)
  errmsg = '';
  if (isa (names, 'string'))
    names = cellstr (names);
  elseif (ischar (names) && (isrow (names) || isempty (names)))
    names = {names};
  endif
  if (! iscellstr (names))
    names = {};
    errmsg = strcat ("must be a character vector, a cell array of", ...
                     " character vectors or a string array.");
    return;
  endif
  names = names(:)';
endfunction

## The message a new member name NAME raises beside the names OTHERS, or ''
## when it may join them.
function errmsg = nameClash (name, others)
  errmsg = '';
  if (any (strcmp (name, {'Name', 'Time', 'TimeInfo', 'Length'})))
    errmsg = sprintf ("a member cannot be named '%s', a property of TC.", name);
  elseif (any (strcmpi (name, others)))
    errmsg = sprintf (strcat ("the member name '%s' conflicts with the", ...
                              " name of an existing member, ignoring", ...
                              " case."), name);
  endif
endfunction

## True when the time metadata A and B agree in units and start date, and,
## where FORMAT is true, in format.
function tf = sameTimeInfo (a, b, format)
  tf = strcmp (a.Units, b.Units) && strcmp (a.StartDate, b.StartDate);
  if (format)
    tf = tf && strcmp (a.Format, b.Format);
  endif
endfunction

## The message of ERR without the name of the function that raised it.
function msg = bareMessage (err)
  msg = regexprep (err.message, '^[\w.]+: ', '');
endfunction

## The message a 'timeseries' method raised on the proxy of a collection, as
## it applies to the collection.
function msg = proxyMessage (err)
  msg = bareMessage (err);
  msg = regexprep (msg, '\<TS\>', 'TC');
  msg = regexprep (msg, '\<a series\>', 'a collection');
endfunction

## The message 'append' raised over the time vectors of the collections, as
## it applies to them.
function msg = collectionMessage (err)
  msg = bareMessage (err);
  msg = strrep (msg, 'series with and without a start date cannot be', ...
                'collections with and without a start date cannot be');
  msg = strrep (msg, 'appended', 'concatenated');
  msg = strrep (msg, 'each series', 'each collection');
endfunction

## N samples of member M, whose samples are of size SS, filled with NaN, or
## with 0 for integer data and false for logical data.
function d = fillSamples (m, ss, n)
  data = m.Data;
  if (isempty (ss))
    ## A member with no samples keeps the shape of its empty data
    sz = size (data);
    if (isequal (sz, [0, 0]))
      ss = [1, 1];
    elseif (m.IsTimeFirst)
      ss = [1, sz(2:end)];
    else
      ss = sz(1:end-1);
    endif
  endif
  if (m.IsTimeFirst)
    sz = [n, ss(2:end)];
  else
    sz = [ss, n];
  endif
  if (islogical (data))
    d = false (sz);
  elseif (isinteger (data))
    d = zeros (sz, class (data));
  elseif (isa (data, 'single'))
    d = NaN (sz, 'single');
  else
    d = NaN (sz);
  endif
endfunction

## 'pkg test' reads the BISTs from the file it is testing, so a class
## whose suite lives in inst/tests/ reads as carrying none.  This block
## answers for it and checks nothing itself.
%!test
%! ## The suite for this class is inst/tests/tscollection.m-tst.
