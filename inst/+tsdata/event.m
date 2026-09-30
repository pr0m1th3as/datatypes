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
## @deftp {datatypes} tsdata.event
##
## An event of a @code{timeseries}.
##
## A @code{tsdata.event} object marks a named instant, with any data the user
## attaches to it.  The @qcode{Events} property of a @code{timeseries} holds
## an array of them.  An event carries its own time, in its own units and
## optionally from its own start date, so it keeps its instant when the
## series' time vector changes.
##
## @end deftp
classdef event

  properties
    ## -*- texinfo -*-
    ## @deftp {tsdata.event} {property} EventData
    ##
    ## Any data the user attaches, @code{[]} by default.
    ##
    ## @end deftp
    EventData = []

    ## -*- texinfo -*-
    ## @deftp {tsdata.event} {property} Name
    ##
    ## The name of the event, a character vector.  Names need not be unique.
    ##
    ## @end deftp
    Name = ''

    ## -*- texinfo -*-
    ## @deftp {tsdata.event} {property} Time
    ##
    ## The time of the event, a real scalar in @qcode{Units}, counted from
    ## @qcode{StartDate} when that is set.  @code{NaN} is refused, where
    ## MATLAB accepts it.
    ##
    ## @end deftp
    Time = 0

    ## -*- texinfo -*-
    ## @deftp {tsdata.event} {property} Units
    ##
    ## The units of @qcode{Time}.
    ##
    ## One of the units a @code{timeseries} time vector takes, or @qcode{''}.
    ## MATLAB accepts any value here.
    ##
    ## @end deftp
    Units = ''

    ## -*- texinfo -*-
    ## @deftp {tsdata.event} {property} StartDate
    ##
    ## The absolute date @qcode{Time} counts from, or @qcode{''} for a
    ## relative time.  A string scalar or a scalar @code{datetime} is converted
    ## to a character vector; text that is not a date is refused.
    ##
    ## @end deftp
    StartDate = ''
  endproperties

  methods

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.event} {@var{e} =} tsdata.event ()
    ## @deftypefnx {tsdata.event} {@var{e} =} tsdata.event (@var{name})
    ## @deftypefnx {tsdata.event} {@var{e} =} tsdata.event (@var{name}, @var{time})
    ##
    ## Create an event.
    ##
    ## @code{@var{e} = tsdata.event (@var{name})} returns an event named
    ## @var{name}, a character vector or a string scalar, at @qcode{Time} 0
    ## with no @qcode{Units}.  With no argument the name is @qcode{''}.
    ##
    ## @code{@var{e} = tsdata.event (@var{name}, @var{time})} places it at
    ## @var{time}.  A real scalar is a relative time in seconds.  A date, as
    ## text or as a scalar @code{datetime}, is an absolute time: the event is
    ## then at @qcode{Time} 0 in @qcode{'days'} from a @qcode{StartDate} of
    ## that date, written @qcode{'dd-mmm-yyyy HH:MM:SS'}.
    ##
    ## MATLAB ignores a third argument and refuses a @code{datetime}.
    ##
    ## @seealso{timeseries}
    ## @end deftypefn
    function this = event (name, time)
      if (nargin == 0)
        return;
      endif
      if (! ((ischar (name) && (isrow (name) || isempty (name)))
             || (isstring (name) && isscalar (name))))
        error ("tsdata.event: NAME must be a character vector.");
      endif
      this.Name = name;
      if (nargin < 2)
        return;
      endif
      if ((ischar (time) && isrow (time))
          || (isstring (time) && isscalar (time))
          || (isa (time, 'datetime') && isscalar (time)))
        [startDate, errmsg] = tsdata.timemetadata.dateValue (time, "TIME");
        if (! isempty (errmsg))
          error ("tsdata.event: %s", errmsg);
        endif
        this.StartDate = startDate;
        this.Units = 'days';
      else
        this.Time = time;
        this.Units = 'seconds';
      endif
    endfunction

    function this = set.Name (this, val)
      if (isstring (val) && isscalar (val))
        val = char (val);
      endif
      if (! (ischar (val) && (isrow (val) || isempty (val))))
        error ("tsdata.event: 'Name' must be a character vector.");
      endif
      if (isempty (val))
        val = '';
      endif
      this.Name = val;
    endfunction

    function this = set.Time (this, val)
      if (! (isnumeric (val) && isreal (val) && isscalar (val)
             && ! isnan (val)))
        error ("tsdata.event: 'Time' must be a real scalar.");
      endif
      this.Time = double (val);
    endfunction

    function this = set.Units (this, val)
      [val, errmsg] = tsdata.timemetadata.unitsValue (val, true);
      if (! isempty (errmsg))
        error ("tsdata.event: %s", errmsg);
      endif
      this.Units = val;
    endfunction

    function this = set.StartDate (this, val)
      [val, errmsg] = tsdata.timemetadata.dateValue (val, "'StartDate'");
      if (! isempty (errmsg))
        error ("tsdata.event: %s", errmsg);
      endif
      this.StartDate = val;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.event} {@var{S} =} get (@var{e})
    ## @deftypefnx {tsdata.event} {@var{value} =} get (@var{e}, @var{name})
    ## @deftypefnx {tsdata.event} {@var{values} =} get (@var{e}, @var{names})
    ##
    ## Return property values.
    ##
    ## @code{@var{S} = get (@var{e})} returns a structure of every
    ## property of @var{e}.  @code{@var{value} = get (@var{e},
    ## @var{name})} returns the property @var{name}, a character vector or a
    ## string scalar matched in any case, and @code{@var{values} = get
    ## (@var{e}, @var{names})} a row cell array of the properties named in
    ## the cell array @var{names}.  For an array, one @var{name} gives a cell
    ## array of its size, and no name, or several, a cell array with a row
    ## per object and a column per property.
    ##
    ## MATLAB refuses an output argument to @code{set} on an event and, given
    ## an array of events and one name, returns the first event's value only;
    ## here @code{set} returns the modified events as on any other class, and
    ## @code{get} returns one value per event.
    ##
    ## @seealso{tsdata.event.set, timeseries.get}
    ## @end deftypefn
    function out = get (this, varargin)
      out = timeseries.propertyGet (this, 'tsdata.event.get', ...
                                    varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.event} {} set (@var{e}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.event} {@var{e2} =} set (@var{e}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.event} {@var{S} =} set (@var{e})
    ##
    ## Set property values.
    ##
    ## @code{set (@var{e}, @var{name}, @var{value}, @dots{})} sets each
    ## property @var{name}, matched in any case, to its @var{value}, in the
    ## order given, in the variable @var{e} of the caller, which must
    ## therefore be a variable.  Metadata held by a @code{timeseries} is set
    ## with dot assignment instead, as
    ## @code{@var{ts}.TimeInfo.Units = 'days'}.
    ##
    ## @code{@var{e2} = set (@var{e}, @var{name}, @var{value}, @dots{})}
    ## returns the modified object and leaves @var{e} as it was.
    ## @code{@var{S} = set (@var{e})} returns what @code{get (@var{e})}
    ## does.
    ##
    ## @seealso{tsdata.event.get, timeseries.set}
    ## @end deftypefn
    function varargout = set (this, varargin)
      if (nargin == 1)
        varargout{1} = get (this);
        return;
      endif
      this = timeseries.propertySet (this, 'tsdata.event.set', ...
                                     varargin{:});
      if (nargout > 0)
        varargout{1} = this;
        return;
      endif
      ## With no output the caller's own variable is set, as in MATLAB
      name = inputname (1);
      if (isempty (name))
        error (strcat ("tsdata.event.set: with no output argument,", ...
                       " E must be a variable; set anything else", ...
                       " with dot assignment."));
      endif
      assignin ('caller', name, this);
    endfunction

  endmethods

  methods (Hidden)

    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    function disp (this)
      if (! isscalar (this))
        fprintf ("  %s tsdata.event array\n\n", sizestr (this));
        return;
      endif
      fprintf ("\n  event with properties:\n\n");
      names = {'EventData', 'Name', 'Time', 'Units', 'StartDate'};
      for i = 1:numel (names)
        fprintf ("%13s: %s\n", names{i}, ...
                 timeseries.dispValue (this.(names{i})));
      endfor
      fprintf ("\n");
    endfunction

  endmethods

endclassdef

## The size of X as MATLAB writes it in a summary, as '3x1'.
function str = sizestr (x)
  str = strjoin (arrayfun (@num2str, size (x), 'UniformOutput', false), 'x');
endfunction

## Test construction
%!test
%! e = tsdata.event ();
%! assert_equal (e.Name, '');
%! assert_equal (e.Time, 0);
%! assert_equal (e.Units, '');
%! assert_equal (e.StartDate, '');
%! assert_equal (e.EventData, []);
%!test
%! assert_equal (properties (tsdata.event ()), ...
%!               {'EventData'; 'Name'; 'Time'; 'Units'; 'StartDate'});
%!test
%! e = tsdata.event ('x');
%! assert_equal (e.Name, 'x');
%! assert_equal (e.Time, 0);
%! assert_equal (e.Units, '');
%!test
%! e = tsdata.event (string ('x'), 1);
%! assert_equal (e.Name, 'x');
%!test
%! e = tsdata.event ('', 1);
%! assert_equal (e.Name, '');
%! assert_equal (e.Time, 1);
## Test a numeric time is relative, in seconds
%!test
%! e = tsdata.event ('x', 5);
%! assert_equal (e.Time, 5);
%! assert_equal (e.Units, 'seconds');
%! assert_equal (e.StartDate, '');
%!test
%! e = tsdata.event ('x', int8 (3));
%! assert_equal (e.Time, 3);
%! assert_equal (class (e.Time), 'double');
%!test
%! e = tsdata.event ('x', -1);
%! assert_equal (e.Time, -1);
%!test
%! e = tsdata.event ('x', Inf);
%! assert_equal (e.Time, Inf);
## Test a date is absolute, day 0 from that date
%!test
%! e = tsdata.event ('x', '01-Jan-2024');
%! assert_equal (e.Time, 0);
%! assert_equal (e.Units, 'days');
%! assert_equal (e.StartDate, '01-Jan-2024 00:00:00');
%!test
%! e = tsdata.event ('x', '01-Jan-2024 06:00:00');
%! assert_equal (e.StartDate, '01-Jan-2024 06:00:00');
%!test
%! e = tsdata.event ('x', string ('01-Jan-2024'));
%! assert_equal (e.StartDate, '01-Jan-2024 00:00:00');
%!test
%! e = tsdata.event ('x', datetime (2024, 1, 1, 6, 0, 0));
%! assert_equal (e.Units, 'days');
%! assert_equal (e.StartDate, '01-Jan-2024 06:00:00');
## Test assignment
%!test
%! e = tsdata.event ('x', 5);
%! e.Units = 'minutes';
%! assert_equal (e.Units, 'minutes');
%! assert_equal (e.Time, 5);
%!test
%! e = tsdata.event ('x', 5);
%! e.Units = '';
%! assert_equal (e.Units, '');
%!test
%! e = tsdata.event ('x', 5);
%! e.StartDate = '01-Jan-2024';
%! assert_equal (e.StartDate, '01-Jan-2024 00:00:00');
%!test
%! e = tsdata.event ('x', 5);
%! e.EventData = {1, 'a'};
%! assert_equal (e.EventData, {1, 'a'});
%!test
%! e = tsdata.event ('x', 5);
%! e.Name = 'a b';
%! assert_equal (e.Name, 'a b');
%!test
%! e = tsdata.event ('x', 5);
%! e.Time = 7;
%! assert_equal (e.Time, 7);
## Test value semantics and arrays
%!test
%! a = tsdata.event ('x', 1);
%! b = a;
%! b.Time = 2;
%! assert_equal (a.Time, 1);
%!test
%! E = [tsdata.event('a', 1), tsdata.event('b', 2)];
%! assert_equal (size (E), [1, 2]);
%! assert_equal (E(2).Name, 'b');
## Test 'get'
%!test
%! S = get (tsdata.event ('x', 1));
%! assert_equal (fieldnames (S), {'EventData'; 'Name'; 'Time'; 'Units'; ...
%!               'StartDate'});
%!test
%! assert_equal (get (tsdata.event ('x', 1), 'Name'), 'x');
%! assert_equal (get (tsdata.event ('x', 1), 'name'), 'x');
%!test
%! assert_equal (get (tsdata.event ('x', 1), {'Name'}), {'x'});
## Test 'set' with no output sets the caller's variable
%!test
%! obj = tsdata.event ('x', 1);
%! set (obj, 'Name', 'y');
%! assert_equal (obj.Name, 'y');
%!test
%! obj = tsdata.event ('x', 1);
%! set (obj, 'NAME', 'y');
%! assert_equal (obj.Name, 'y');
## Test 'set' with an output leaves its input as it was
%!test
%! obj = tsdata.event ('x', 1);
%! obj2 = set (obj, 'Name', 'y');
%! assert_equal (obj2.Name, 'y');
%! assert_equal (obj.Name, 'x');
%!test
%! obj = tsdata.event ('x', 1);
%! assert_equal (fieldnames (set (obj)), fieldnames (get (obj)));
## Test 'get' and 'set' on an array of events
%!test
%! E = [tsdata.event('a', 1), tsdata.event('b', 2)];
%! assert_equal (get (E, 'Name'), {'a', 'b'});
%! assert_equal (get (E, {'Name', 'Time'}), {'a', 1; 'b', 2});
%!test
%! E = [tsdata.event('a', 1), tsdata.event('b', 2)];
%! set (E, 'Units', 'minutes');
%! assert_equal (E(2).Units, 'minutes');

%!error <tsdata.event: NAME must be a character vector.> tsdata.event (5, 1)
%!error <tsdata.event: NAME must be a character vector.> tsdata.event ({'x'}, 1)
%!error <tsdata.event: 'Time' must be a real scalar.> tsdata.event ('x', [1, 2])
%!error <tsdata.event: 'Time' must be a real scalar.> tsdata.event ('x', 1 + 2i)
%!error <tsdata.event: 'Time' must be a real scalar.> tsdata.event ('x', true)
%!error <tsdata.event: 'Time' must be a real scalar.> tsdata.event ('x', NaN)
%!error <tsdata.event: TIME is not a date: 'bogus'> tsdata.event ('x', 'bogus')
%!error <tsdata.event: TIME must not be NaT.> tsdata.event ('x', NaT)
%!error <tsdata.event: 'Name' must be a character vector.> ...
%! setfield (tsdata.event ('x', 1), 'Name', 5)
%!error <tsdata.event: 'Time' must be a real scalar.> ...
%! setfield (tsdata.event ('x', 1), 'Time', 'x')
%!error <tsdata.event: 'Units' must be 'weeks', 'days', 'hours', 'minutes', 'seconds', 'milliseconds', 'microseconds' or 'nanoseconds'.> ...
%! setfield (tsdata.event ('x', 1), 'Units', 'bogus')
%!error <tsdata.event: 'StartDate' is not a date: 'bogus'> ...
%! setfield (tsdata.event ('x', 1), 'StartDate', 'bogus')
%!error <tsdata.event.get: too many input arguments.> ...
%! get (tsdata.event ('x', 1), 'Name', 'Name')
%!error <tsdata.event.get: unknown property: 'Bogus'> get (tsdata.event ('x', 1), 'Bogus')
%!error <tsdata.event.get: NAME must be a character vector or a cell array of character vectors.> ...
%! get (tsdata.event ('x', 1), 5)
%!error <tsdata.event.set: with no output argument, E must be a variable; set anything else with dot assignment.> ...
%! set (tsdata.event ('x', 1), 'Name', 'y')
%!error <tsdata.event.set: name-value arguments must be in pairs.> ...
%! obj = tsdata.event ('x', 1); set (obj, 'Name');
%!error <tsdata.event.set: unknown property: 'Bogus'> ...
%! obj = tsdata.event ('x', 1); set (obj, 'Bogus', 1);
%!error <tsdata.event.set: NAME must be a character vector.> ...
%! obj = tsdata.event ('x', 1); set (obj, 5, 1);
