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
## @deftp {datatypes} tsdata.timemetadata
##
## The time metadata of a @code{timeseries}.
##
## A @code{tsdata.timemetadata} object describes the time vector of a
## @code{timeseries}: its units, an optional absolute start date, and the
## length, first and last times and uniform step derived from it.  It is what
## @qcode{@var{ts}.TimeInfo} returns.
##
## The derived properties @qcode{Length}, @qcode{Start}, @qcode{End} and
## @qcode{Increment} follow the time vector and cannot be assigned; change
## the time vector of the series instead, or use @code{setuniformtime}.
##
## @end deftp
classdef timemetadata

  properties
    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} Units
    ##
    ## The units of the time vector.
    ##
    ## One of @qcode{'weeks'}, @qcode{'days'}, @qcode{'hours'},
    ## @qcode{'minutes'}, @qcode{'seconds'}, @qcode{'milliseconds'},
    ## @qcode{'microseconds'} or @qcode{'nanoseconds'}, in any case, stored in
    ## lower case; @qcode{'seconds'} by default.  Assigning it relabels the
    ## time vector and does not rescale it.  MATLAB accepts any value here.
    ##
    ## @end deftp
    Units = 'seconds'

    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} UserData
    ##
    ## Any data the user attaches, @code{[]} by default.
    ##
    ## @end deftp
    UserData = []

    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} Format
    ##
    ## The display format of absolute times.
    ##
    ## A @code{datestr} format as a character vector, @qcode{''} by default,
    ## which @code{getabstime} writes the dates in.  Text holding no date field
    ## at all, such as @qcode{'bogus'}, is refused, where MATLAB accepts any
    ## value; MATLAB also ignores every format but the few its documentation
    ## lists, while here any @code{datestr} format is used.
    ##
    ## @end deftp
    Format = ''

    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} StartDate
    ##
    ## The absolute date the time vector counts from.
    ##
    ## A date as a character vector, or @qcode{''} for a relative time vector,
    ## by default.  A string scalar or a scalar @code{datetime} is converted to
    ## a character vector; text that is not a date is refused, where MATLAB
    ## accepts any value.
    ##
    ## @end deftp
    StartDate = ''
  endproperties

  properties (Dependent)
    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} Length
    ##
    ## The number of times in the time vector.  Read-only.
    ##
    ## @end deftp
    Length

    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} End
    ##
    ## The last time, or @code{[]} for an empty time vector.  Read-only.
    ##
    ## @end deftp
    End

    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} Increment
    ##
    ## The step of a uniform time vector.
    ##
    ## The step when every one is exactly equal and not zero, @code{NaN}
    ## otherwise and for a single time, @code{[]} for an empty time vector.
    ## Read-only; use @code{setuniformtime} to give a series a uniform time
    ## vector.  MATLAB still accepts an assignment here, with a warning that
    ## it will not in future, and rebuilds the time vector from it.
    ##
    ## @end deftp
    Increment

    ## -*- texinfo -*-
    ## @deftp {tsdata.timemetadata} {property} Start
    ##
    ## The first time, or @code{[]} for an empty time vector.  Read-only.
    ## MATLAB still accepts an assignment here, with a warning that it will
    ## not in future, and ignores it.
    ##
    ## @end deftp
    Start
  endproperties

  ## The time vector the derived properties are read from.  A series sets
  ## its own on every read of 'TimeInfo', so assigning it here changes no
  ## series.  It is not restricted to 'timeseries' by an access list, since
  ## resolving that list would load 'timeseries', whose defaults construct
  ## this class while it is still being defined.
  properties (Hidden)
    TimeVector = zeros (0, 1)
  endproperties

  methods

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.timemetadata} {@var{ti} =} tsdata.timemetadata ()
    ## @deftypefnx {tsdata.timemetadata} {@var{ti} =} tsdata.timemetadata (@var{time})
    ##
    ## Create a time metadata object.
    ##
    ## @code{@var{ti} = tsdata.timemetadata ()} returns an object for an empty
    ## time vector: @qcode{Units} @qcode{'seconds'}, @qcode{Format} and
    ## @qcode{StartDate} @qcode{''}, @qcode{Length} 0, and @qcode{Start},
    ## @qcode{End} and @qcode{Increment} @code{[]}.
    ##
    ## @code{@var{ti} = tsdata.timemetadata (@var{time})} derives
    ## @qcode{Length}, @qcode{Start}, @qcode{End} and @qcode{Increment} from
    ## the numeric vector @var{time}, which must be finite and non-decreasing.
    ## MATLAB gives an @qcode{Increment} of @code{NaN} here even for a uniform
    ## @var{time}; here it is the step, as for a @code{timeseries}.
    ##
    ## A @code{timeseries} creates its own; this is needed only to assign a
    ## whole new one to @qcode{@var{ts}.TimeInfo}, whose time vector is then
    ## the series' own.
    ##
    ## @seealso{timeseries}
    ## @end deftypefn
    function this = timemetadata (time)
      if (nargin == 0)
        return;
      endif
      this.TimeVector = time;
    endfunction

    function this = set.TimeVector (this, time)
      if (! (isnumeric (time) && isreal (time)
             && (isvector (time) || isempty (time))))
        error ("tsdata.timemetadata: TIME must be a numeric vector.");
      endif
      time = double (time(:));
      if (! all (isfinite (time)))
        error ("tsdata.timemetadata: TIME must be finite.");
      endif
      if (any (diff (time) < 0))
        error ("tsdata.timemetadata: TIME must be non-decreasing.");
      endif
      this.TimeVector = time;
    endfunction

    function this = set.Units (this, val)
      [val, errmsg] = tsdata.timemetadata.unitsValue (val, false);
      if (! isempty (errmsg))
        error ("tsdata.timemetadata: %s", errmsg);
      endif
      this.Units = val;
    endfunction

    function this = set.Format (this, val)
      if (isstring (val) && isscalar (val))
        val = char (val);
      endif
      if (! (ischar (val) && (isrow (val) || isempty (val))))
        error ("tsdata.timemetadata: 'Format' must be a character vector.");
      endif
      if (isempty (val))
        val = '';
      elseif (isempty (regexp (val, '[ymdHMSF]|AM|PM', 'once')))
        error (strcat ("tsdata.timemetadata: 'Format' holds no date", ...
                       " field: '%s'"), val);
      endif
      this.Format = val;
    endfunction

    function this = set.StartDate (this, val)
      [val, errmsg] = tsdata.timemetadata.dateValue (val, "'StartDate'");
      if (! isempty (errmsg))
        error ("tsdata.timemetadata: %s", errmsg);
      endif
      this.StartDate = val;
    endfunction

    function val = get.Length (this)
      val = numel (this.TimeVector);
    endfunction

    function this = set.Length (this, val)
      error ("tsdata.timemetadata: 'Length' is read-only.");
    endfunction

    function val = get.Start (this)
      if (isempty (this.TimeVector))
        val = [];
      else
        val = this.TimeVector(1);
      endif
    endfunction

    function this = set.Start (this, val)
      error (strcat ("tsdata.timemetadata: 'Start' is read-only; use", ...
                     " 'setuniformtime' to give a series a uniform time", ...
                     " vector."));
    endfunction

    function val = get.End (this)
      if (isempty (this.TimeVector))
        val = [];
      else
        val = this.TimeVector(end);
      endif
    endfunction

    function this = set.End (this, val)
      error (strcat ("tsdata.timemetadata: 'End' is read-only; use", ...
                     " 'setuniformtime' to give a series a uniform time", ...
                     " vector."));
    endfunction

    ## MATLAB sets the increment only when every step is exactly equal, so
    ## 0:0.1:1, whose steps differ in the last bit, has none.  Measured on
    ## R2024a; reproduced as it stands.
    function val = get.Increment (this)
      t = this.TimeVector;
      if (isempty (t))
        val = [];
        return;
      endif
      d = diff (t);
      if (! isempty (d) && d(1) != 0 && all (d == d(1)))
        val = d(1);
      else
        val = NaN;
      endif
    endfunction

    function this = set.Increment (this, val)
      error (strcat ("tsdata.timemetadata: 'Increment' is read-only; use", ...
                     " 'setuniformtime' to give a series a uniform time", ...
                     " vector."));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.timemetadata} {@var{S} =} get (@var{ti})
    ## @deftypefnx {tsdata.timemetadata} {@var{value} =} get (@var{ti}, @var{name})
    ## @deftypefnx {tsdata.timemetadata} {@var{values} =} get (@var{ti}, @var{names})
    ##
    ## Return property values.
    ##
    ## @code{@var{S} = get (@var{ti})} returns a structure of every
    ## property of @var{ti}.  @code{@var{value} = get (@var{ti},
    ## @var{name})} returns the property @var{name}, a character vector or a
    ## string scalar matched in any case, and @code{@var{values} = get
    ## (@var{ti}, @var{names})} a row cell array of the properties named in
    ## the cell array @var{names}.  For an array, one @var{name} gives a cell
    ## array of its size.
    ##
    ## @seealso{tsdata.timemetadata.set, timeseries.get}
    ## @end deftypefn
    function out = get (this, varargin)
      out = timeseries.propertyGet (this, 'tsdata.timemetadata.get', ...
                                    varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.timemetadata} {} set (@var{ti}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.timemetadata} {@var{ti2} =} set (@var{ti}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.timemetadata} {@var{S} =} set (@var{ti})
    ##
    ## Set property values.
    ##
    ## @code{set (@var{ti}, @var{name}, @var{value}, @dots{})} sets each
    ## property @var{name}, matched in any case, to its @var{value}, in the
    ## order given, in the variable @var{ti} of the caller, which must
    ## therefore be a variable.  Metadata held by a @code{timeseries} is set
    ## with dot assignment instead, as
    ## @code{@var{ts}.TimeInfo.Units = 'days'}.
    ##
    ## @code{@var{ti2} = set (@var{ti}, @var{name}, @var{value}, @dots{})}
    ## returns the modified object and leaves @var{ti} as it was.
    ## @code{@var{S} = set (@var{ti})} returns what @code{get (@var{ti})}
    ## does.
    ##
    ## @seealso{tsdata.timemetadata.get, timeseries.set}
    ## @end deftypefn
    function varargout = set (this, varargin)
      if (nargin == 1)
        varargout{1} = get (this);
        return;
      endif
      this = timeseries.propertySet (this, 'tsdata.timemetadata.set', ...
                                     varargin{:});
      if (nargout > 0)
        varargout{1} = this;
        return;
      endif
      ## With no output the caller's own variable is set, as in MATLAB
      name = inputname (1);
      if (isempty (name))
        error (strcat ("tsdata.timemetadata.set: with no output argument,", ...
                       " TI must be a variable; set anything else", ...
                       " with dot assignment."));
      endif
      assignin ('caller', name, this);
    endfunction

  endmethods

  methods (Static, Hidden)

    ## Validate a time unit.  Returns it in lower case and an empty ERRMSG,
    ## or the body of the message the caller raises under its own name.
    ## An empty unit is valid only where ALLOWEMPTY is true.
    function [val, errmsg] = unitsValue (val, allowEmpty)
      errmsg = '';
      if (isstring (val) && isscalar (val))
        val = char (val);
      endif
      if (allowEmpty && ischar (val) && isempty (val))
        val = '';
        return;
      endif
      units = {'weeks', 'days', 'hours', 'minutes', 'seconds', ...
               'milliseconds', 'microseconds', 'nanoseconds'};
      if (! (ischar (val) && isrow (val) && any (strcmpi (val, units))))
        errmsg = strcat ("'Units' must be 'weeks', 'days', 'hours',", ...
                         " 'minutes', 'seconds', 'milliseconds',", ...
                         " 'microseconds' or 'nanoseconds'.");
        return;
      endif
      val = lower (val);
    endfunction

    ## Validate a date given as text or as a scalar datetime.  LABEL names
    ## it in a message as printed: a quoted property or a bare argument.
    ## Returns it as a character vector, '' for an empty one, and an empty
    ## ERRMSG, or the body of the message the caller raises.
    function [val, errmsg] = dateValue (val, label)
      errmsg = '';
      if (isa (val, 'datetime') && isscalar (val))
        if (isnat (val))
          errmsg = sprintf ("%s must not be NaT.", label);
          return;
        endif
        val = datestr (datenum (val), 'dd-mmm-yyyy HH:MM:SS');
        return;
      elseif (isstring (val) && isscalar (val))
        val = char (val);
      endif
      if (! (ischar (val) && (isrow (val) || isempty (val))))
        errmsg = sprintf ("%s must be a date as a character vector.", label);
        return;
      endif
      if (isempty (val))
        val = '';
        return;
      endif
      try
        datevec (val);
      catch
        errmsg = sprintf ("%s is not a date: '%s'", label, val);
      end_try_catch
    endfunction

  endmethods

  methods (Hidden)

    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      fprintf ("\n  tsdata.timemetadata\n\n");
      fprintf ("%s", summaryText (this));
      fprintf ("  Common Properties:\n");
      fprintf ("          Units: '%s'\n", this.Units);
      fprintf ("         Format: '%s'\n", this.Format);
      fprintf ("      StartDate: '%s'\n\n", this.StartDate);
    endfunction

    function disp (this)
      fprintf ("\n  timemetadata with properties:\n\n");
      names = {'Units', 'UserData', 'Format', 'StartDate', 'Length', ...
               'End', 'Increment', 'Start'};
      for i = 1:numel (names)
        fprintf ("%13s: %s\n", names{i}, ...
                 timeseries.dispValue (this.(names{i})));
      endfor
      fprintf ("\n");
    endfunction

    ## The time summary MATLAB shows before the common properties, also used
    ## by 'timeseries'.
    function txt = summaryText (this)
      if (this.Length == 0)
        txt = "  Empty timeseries TimeMetaData object.\n\n";
        return;
      endif
      u = this.Units;
      if (isnan (this.Increment))
        txt = sprintf ("  Non-Uniform Time:\n    Length       %d\n\n", ...
                       this.Length);
      else
        txt = sprintf (strcat ("  Uniform Time:\n    Length       %d\n", ...
                               "    Increment    %g %s\n\n"), ...
                       this.Length, this.Increment, u);
      endif
      txt = [txt, sprintf("  Time Range:\n    Start        %g %s\n", ...
                          this.Start, u), ...
                  sprintf("    End          %g %s\n\n", this.End, u)];
    endfunction

  endmethods

endclassdef

## Test the defaults
%!test
%! ti = tsdata.timemetadata ();
%! assert_equal (ti.Units, 'seconds');
%! assert_equal (ti.UserData, []);
%! assert_equal (ti.Format, '');
%! assert_equal (ti.StartDate, '');
%! assert_equal (ti.Length, 0);
%! assert_equal (ti.End, []);
%! assert_equal (ti.Increment, []);
%! assert_equal (ti.Start, []);
%!test
%! assert_equal (properties (tsdata.timemetadata ()), {'Units'; ...
%!               'UserData'; 'Format'; 'StartDate'; 'Length'; 'End'; ...
%!               'Increment'; 'Start'});
## Test a time vector derives the read-only properties
%!test
%! ti = tsdata.timemetadata ([1, 2, 3]);
%! assert_equal (ti.Length, 3);
%! assert_equal (ti.Start, 1);
%! assert_equal (ti.End, 3);
%! assert_equal (ti.Increment, 1);
%!test
%! ti = tsdata.timemetadata ([1; 2; 4]);
%! assert_equal (ti.Length, 3);
%! assert_equal (ti.End, 4);
%! assert_equal (ti.Increment, NaN);
%!test
%! ti = tsdata.timemetadata (5);
%! assert_equal (ti.Length, 1);
%! assert_equal (ti.Start, 5);
%! assert_equal (ti.End, 5);
%! assert_equal (ti.Increment, NaN);
%!test
%! ti = tsdata.timemetadata ([]);
%! assert_equal (ti.Length, 0);
%! assert_equal (ti.Increment, []);
%!test
%! ti = tsdata.timemetadata (int8 ([0, 2, 4]));
%! assert_equal (ti.Increment, 2);
%! assert_equal (class (ti.Start), 'double');
## Test 'Increment' needs every step exactly equal and not zero
%!test
%! ti = tsdata.timemetadata (0:0.1:1);
%! assert_equal (ti.Increment, NaN);
%!test
%! ti = tsdata.timemetadata ([0, 1, 2 + 1e-14, 3]);
%! assert_equal (ti.Increment, NaN);
%!test
%! ti = tsdata.timemetadata ([0, 0, 0]);
%! assert_equal (ti.Increment, NaN);
%!test
%! ti = tsdata.timemetadata ([0, 0.5, 1]);
%! assert_equal (ti.Increment, 0.5);
## Test 'Units'
%!test
%! ti = tsdata.timemetadata ();
%! ti.Units = 'minutes';
%! assert_equal (ti.Units, 'minutes');
%!test
%! ti = tsdata.timemetadata ();
%! ti.Units = 'Minutes';
%! assert_equal (ti.Units, 'minutes');
%!test
%! ti = tsdata.timemetadata ();
%! ti.Units = string ('days');
%! assert_equal (ti.Units, 'days');
%!test
%! ti = tsdata.timemetadata ([0, 1, 2]);
%! ti.Units = 'hours';
%! assert_equal (ti.End, 2);
## Test 'Format'
%!test
%! ti = tsdata.timemetadata ();
%! ti.Format = 'dd-mmm-yyyy';
%! assert_equal (ti.Format, 'dd-mmm-yyyy');
%!test
%! ti = tsdata.timemetadata ();
%! ti.Format = 'dd-mmm-yyyy';
%! ti.Format = '';
%! assert_equal (ti.Format, '');
## Test 'StartDate'
%!test
%! ti = tsdata.timemetadata ();
%! ti.StartDate = '01-Jan-2024';
%! assert_equal (ti.StartDate, '01-Jan-2024');
%!test
%! ti = tsdata.timemetadata ();
%! ti.StartDate = string ('01-Jan-2024 06:00:00');
%! assert_equal (ti.StartDate, '01-Jan-2024 06:00:00');
%!test
%! ti = tsdata.timemetadata ();
%! ti.StartDate = datetime (2024, 1, 1, 6, 0, 0);
%! assert_equal (ti.StartDate, '01-Jan-2024 06:00:00');
%!test
%! ti = tsdata.timemetadata ();
%! ti.StartDate = '01-Jan-2024';
%! ti.StartDate = '';
%! assert_equal (ti.StartDate, '');
## Test 'UserData'
%!test
%! ti = tsdata.timemetadata ();
%! ti.UserData = 'note';
%! assert_equal (ti.UserData, 'note');
## Test value semantics
%!test
%! a = tsdata.timemetadata ([1, 2]);
%! b = a;
%! b.Units = 'days';
%! assert_equal (a.Units, 'seconds');
## Test 'get'
%!test
%! S = get (tsdata.timemetadata ([1, 2, 3]));
%! assert_equal (fieldnames (S), {'Units'; 'UserData'; 'Format'; ...
%!               'StartDate'; 'Length'; 'End'; 'Increment'; 'Start'});
%!test
%! assert_equal (get (tsdata.timemetadata ([1, 2, 3]), 'Units'), 'seconds');
%! assert_equal (get (tsdata.timemetadata ([1, 2, 3]), 'units'), 'seconds');
%!test
%! assert_equal (get (tsdata.timemetadata ([1, 2, 3]), {'Units'}), {'seconds'});
## Test 'set' with no output sets the caller's variable
%!test
%! obj = tsdata.timemetadata ([1, 2, 3]);
%! set (obj, 'Units', 'days');
%! assert_equal (obj.Units, 'days');
%!test
%! obj = tsdata.timemetadata ([1, 2, 3]);
%! set (obj, 'UNITS', 'days');
%! assert_equal (obj.Units, 'days');
## Test 'set' with an output leaves its input as it was
%!test
%! obj = tsdata.timemetadata ([1, 2, 3]);
%! obj2 = set (obj, 'Units', 'days');
%! assert_equal (obj2.Units, 'days');
%! assert_equal (obj.Units, 'seconds');
%!test
%! obj = tsdata.timemetadata ([1, 2, 3]);
%! assert_equal (fieldnames (set (obj)), fieldnames (get (obj)));

%!error <tsdata.timemetadata: TIME must be a numeric vector.> ...
%! tsdata.timemetadata ('abc')
%!error <tsdata.timemetadata: TIME must be a numeric vector.> ...
%! tsdata.timemetadata (ones (2, 2))
%!error <tsdata.timemetadata: TIME must be a numeric vector.> ...
%! tsdata.timemetadata ([1, 2i])
%!error <tsdata.timemetadata: TIME must be finite.> ...
%! tsdata.timemetadata ([1, NaN])
%!error <tsdata.timemetadata: TIME must be finite.> ...
%! tsdata.timemetadata ([1, Inf])
%!error <tsdata.timemetadata: TIME must be non-decreasing.> ...
%! tsdata.timemetadata ([2, 1])
%!error <tsdata.timemetadata: 'Units' must be 'weeks', 'days', 'hours', 'minutes', 'seconds', 'milliseconds', 'microseconds' or 'nanoseconds'.> ...
%! setfield (tsdata.timemetadata (), 'Units', 'bogus')
%!error <tsdata.timemetadata: 'Units' must be 'weeks', 'days', 'hours', 'minutes', 'seconds', 'milliseconds', 'microseconds' or 'nanoseconds'.> ...
%! setfield (tsdata.timemetadata (), 'Units', 'years')
%!error <tsdata.timemetadata: 'Units' must be 'weeks', 'days', 'hours', 'minutes', 'seconds', 'milliseconds', 'microseconds' or 'nanoseconds'.> ...
%! setfield (tsdata.timemetadata (), 'Units', 5)
%!error <tsdata.timemetadata: 'Format' must be a character vector.> ...
%! setfield (tsdata.timemetadata (), 'Format', 5)
%!error <tsdata.timemetadata: 'Format' holds no date field: 'bogus'> ...
%! setfield (tsdata.timemetadata (), 'Format', 'bogus')
%!error <tsdata.timemetadata: 'StartDate' must be a date as a character vector.> ...
%! setfield (tsdata.timemetadata (), 'StartDate', 5)
%!error <tsdata.timemetadata: 'StartDate' is not a date: 'bogus'> ...
%! setfield (tsdata.timemetadata (), 'StartDate', 'bogus')
%!error <tsdata.timemetadata: 'StartDate' must not be NaT.> ...
%! setfield (tsdata.timemetadata (), 'StartDate', NaT)
%!error <tsdata.timemetadata: 'Length' is read-only.> ...
%! setfield (tsdata.timemetadata (), 'Length', 5)
%!error <tsdata.timemetadata: 'Start' is read-only; use 'setuniformtime' to give a series a uniform time vector.> ...
%! setfield (tsdata.timemetadata (), 'Start', 5)
%!error <tsdata.timemetadata: 'End' is read-only; use 'setuniformtime' to give a series a uniform time vector.> ...
%! setfield (tsdata.timemetadata (), 'End', 5)
%!error <tsdata.timemetadata: 'Increment' is read-only; use 'setuniformtime' to give a series a uniform time vector.> ...
%! setfield (tsdata.timemetadata (), 'Increment', 2)
%!error <tsdata.timemetadata.get: too many input arguments.> ...
%! get (tsdata.timemetadata ([1, 2, 3]), 'Units', 'Units')
%!error <tsdata.timemetadata.get: unknown property: 'Bogus'> get (tsdata.timemetadata ([1, 2, 3]), 'Bogus')
%!error <tsdata.timemetadata.get: NAME must be a character vector or a cell array of character vectors.> ...
%! get (tsdata.timemetadata ([1, 2, 3]), 5)
%!error <tsdata.timemetadata.set: with no output argument, TI must be a variable; set anything else with dot assignment.> ...
%! set (tsdata.timemetadata ([1, 2, 3]), 'Units', 'days')
%!error <tsdata.timemetadata.set: name-value arguments must be in pairs.> ...
%! obj = tsdata.timemetadata ([1, 2, 3]); set (obj, 'Units');
%!error <tsdata.timemetadata.set: unknown property: 'Bogus'> ...
%! obj = tsdata.timemetadata ([1, 2, 3]); set (obj, 'Bogus', 1);
%!error <tsdata.timemetadata.set: NAME must be a character vector.> ...
%! obj = tsdata.timemetadata ([1, 2, 3]); set (obj, 5, 1);
