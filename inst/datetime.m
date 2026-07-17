## Copyright (C) 2024-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

classdef datetime
  ## -*- texinfo -*-
  ## @deftp {datatypes} datetime
  ##
  ## Array representing points in time using the Gregorian calendar.
  ##
  ## A @qcode{datetime} array stores internally the datetime points as double
  ## arrays representing whole years, months, days, hours, and minutes, as well
  ## as seconds including their fractional part.  The underlying implementation
  ## relies on the @qcode{'date.h'} C++ library.  The precision of this
  ## implementation is set at microseconds, which is substantial for typical
  ## times.
  ##
  ## A @code{datetime} array is a collection of date/time elements, with each
  ## element holding a complete date/time.  The @code{datetime} array also has
  ## @qcode{TimeZone} and @qcode{Format} properties associated with it, which
  ## apply to all elements in the array.
  ##
  ## @seealso{calendarDuration, duration}
  ## @end deftp

  properties (Constant)
    ## -*- texinfo -*-
    ## @deftp {datetime} {property} SystemTimeZone
    ##
    ## System time zone setting
    ##
    ## A read-only property specifying the local time zone of the system, where
    ## Octave is running.
    ##
    ## @end deftp
    SystemTimeZone = localtime (time ()).zone;
  endproperties

  properties (Access = private, Hidden)
    ## Whole years
    Year = 0
    ## Whole months
    Month = 0
    ## Whole days
    Day = 0
    ## Whole hours
    Hour = 0
    ## Whole minutes
    Minute = 0
    ## Seconds (including fractional seconds)
    Second = 0
  endproperties

  properties
    ## -*- texinfo -*-
    ## @deftp {datetime} {property} Format
    ##
    ## Display format
    ##
    ## Display format, specified as a character vector or string scalar.  If
    ## specified as a string scalar, it is converted and stored internally as
    ## a character vector.
    ##
    ## @end deftp
    Format = 'default'

    ## -*- texinfo -*-
    ## @deftp {datetime} {property} TimeZone
    ##
    ## Time zone
    ##
    ## Time zone, specified as a character vector or string scalar.  If
    ## specified as a string scalar, it is converted and stored internally as
    ## a character vector.
    ##
    ## @end deftp
    TimeZone = ''
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ('%s =\n', in_name);
      endif
      __disp__ (this, 'datetime', in_name);
    endfunction

    ## Custom display
    function disp (this)
      __disp__ (this, 'datetime');
    endfunction

  endmethods

################################################################################
##                 ** Create and convert 'datetime' type **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'datetime'         'dispstrings'      'cellstr'          'char'            ##
## 'ymd'              'hms'              'year'             'quarter'         ##
## 'month'            'week'             'day'              'hour'            ##
## 'minute'           'second'           'timeofday'        'tzoffset'        ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{T} =} datetime (@qcode{'now'})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@qcode{'today'})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@qcode{'tomorrow'})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@qcode{'yesterday'})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateStrings})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateStrings}, @qcode{'InputFormat'}, @var{INFMT})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateStrings},@
    ## @qcode{'InputFormat'}, @var{INFMT}, @qcode{'PivotYear'}, @var{PIVOT})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateVectors})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{Y}, @var{MO}, @var{D})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S}, @var{MS})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{X}, @qcode{'ConvertFrom'}, @var{TYPE})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@dots{}, @qcode{'Format'}, @var{FMT})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@dots{}, @qcode{'TimeZone'}, @var{TZ})
    ##
    ## Create a new array of datetime values.
    ##
    ## @code{@var{T} = datetime (@qcode{'now'})} returns a scalar datetime array
    ## corresponding to the current date and time. @code{@var{T} = datetime
    ## (@qcode{'now'})} is the same as @code{@var{T} = datetime ()}.  Except for
    ## @qcode{'now'}, the same syntax can be used with @qcode{'today'},
    ## @qcode{'tomorrow'} and @qcode{'yesterday'}.  These options return the
    ## respective date but with time set at midnight.
    ##
    ## @code{@var{T} = datetime (@var{DateStrings})} creates a datetime array
    ## from the text in @var{DateStrings} representing points in time.  In
    ## current implementation, @var{DateStrings} are parsed by Octave's core
    ## @code{datevec} function, hence supported text formats are currently those
    ## supported by @code{datevec}.
    ##
    ## @code{@var{T} = datetime (@var{DateStrings}, @qcode{'InputFormat'},
    ## @var{INFMT})} also allows to specify a particular input text format to
    ## parse @var{DateStrings}.  It is always preferable to specify the format
    ## @var{INFMT} if it is known.  Formats which do not specify a particular
    ## time component will have the value set to zero.  Formats which do not
    ## a date will default to January 1st of the current year.
    ##
    ## @code{@var{T} = datetime (@var{DateStrings}, @qcode{'InputFormat'},
    ## @var{INFMT}, @qcode{'PivotYear'}, @var{PIVOT})} also allows to specify a
    ## pivot year, which refers to the year at the start of the century to which
    ## two-digit years will be referenced.  When not specified, it defaults to
    ## the current years minus 50.
    ##
    ## @code{@var{T} = datetime (@var{DateVectors})} creates a column vector of
    ## datetime values from the date vectors in @var{DateVectors}.
    ##
    ## @code{@var{T} = datetime (@var{Y}, @var{MO}, @var{D}} creates an array of
    ## datetime values for corresponding elements of the @var{Y}, @var{MO}, and
    ## @var{D} arrays, which must be of the same size or scalars, must contain
    ## integer values, and they correspond to years, months, and days,
    ## respectively.
    ##
    ## @code{@var{T} = datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI},
    ## @var{S})} also adds time components to the constructed datetime arrays.
    ## @var{H}, @var{MI}, and @var{S} must be of the same size or scalars.
    ## @var{H} and @var{MI} must contain integer values, whereas @var{S} may
    ## also contain a fractional part.
    ##
    ## @code{@var{T} = datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI},
    ## @var{S}, @var{MS})} also adds a milliseconds component, @var{MS}, which
    ## may also have a fractional part.
    ##
    ## @code{@var{T} = datetime (@var{X}, @qcode{'ConvertFrom'}, @var{TYPE})}
    ## converts the numeric values in @var{X} to a datetime array according to
    ## the data type specified by @var{TYPE}.  The following types are
    ## supported:
    ##
    ## @itemize
    ## @item @qcode{'datenum'}
    ## @item @qcode{'excel'}
    ## @item @qcode{'posixtime'}
    ## @item @qcode{'epochtime'}
    ## @end itemize
    ##
    ## @code{@var{T} = datetime (@dots{}, @qcode{'Format'}, @var{FMT})}
    ## specifies the display format of the values in the output datetime array.
    ## Currently, only the default display format is implemented.
    ##
    ## @code{@var{T} = datetime (@dots{}, @qcode{'TimeZone'}, @var{TZ})} sets
    ## the time zone to the values in the output datetime array.  If not
    ## specified, the computer's local timezone is used.  Supported time zones
    ## are specified in the IANA's Time Zone Database.  You may specify a new
    ## time zone by setting the @qcode{'TimeZone'} property of the datetime
    ## array, in which case the new datetime values may include Daylight Saving
    ## Time (DST) in their computation.
    ##
    ## @seealso{NaT, datetime, isdatetime, calendarDuration, duration}
    ## @end deftypefn
    function this = datetime (varargin)

      ## Return an scalar datetime object with current local time
      if (nargin == 0)
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ ('now');
        return;
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'ConvertFrom', 'Format', 'InputFormat', ...
                  'Locale', 'PivotYear', 'TimeZone'};
      dfValues = {[], [], [], [], [], []};
      [ConvertFrom, Format, inputFormat, Locale, PivotYear, TimeZone, args] =...
                    parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional 'Format' and 'InputFormat' arguments
      if (! isempty (ConvertFrom))
        ## Call __datetime__ to check for valid ConvertFrom string and
        ## data input
        [~,~,~,~,~,~,errmsg] = __datetime__ (args{:}, 'ConvertFrom', ...
                                             ConvertFrom);
        if (! isnumeric (errmsg))
          error ("datetime: %s ", errmsg);
        elseif (! isempty (inputFormat))
          error ("datetime: 'ConvertFrom' cannot be used with 'InputFormat'.");
        endif
      endif
      if (! isempty (Format))
        if (! (ischar (Format) && isvector (Format)))
          error ("datetime: 'Format' must be a character vector.");
        endif
        this.Format = Format;
      endif
      if (! isempty (inputFormat))
        if (! (ischar (inputFormat) && isvector (inputFormat)))
          error ("datetime: 'InputFormat' must be a character vector.");
        elseif (! isempty (ConvertFrom))
          error ("datetime: 'InputFormat' cannot be used with 'ConvertFrom'.");
        elseif (strcmpi (this.Format, 'preserveinput'))
          this.Format = inputFormat;
        endif
      endif
      if (! isempty (Locale))
        if (! (ischar (Locale) && isvector (Locale)))
          error ("datetime: 'Locale' must be a character vector.");
        else
          warning ("datetime: 'Locale' is currently unimplemented.");
        endif
      endif
      if (! isempty (PivotYear))
        if (isempty (inputFormat))
          error ("datetime: 'PivotYear' can only be used with 'InputFormat'.");
        elseif (! (isnumeric (PivotYear) && isscalar (PivotYear)))
          error ("datetime: 'PivotYear' must be a numeric scalar.");
        elseif (fix (PivotYear) != PivotYear)
          error ("datetime: 'PivotYear' must be an integer value.");
        endif
      endif
      if (! isempty (TimeZone))
        ## Call __datetime__ to check for valid timezone string
        [~,~,~,~,~,~,errmsg] = __datetime__ (0, 0, 0, 'TimeZone', TimeZone);
        if (! isnumeric (errmsg))
          error ("datetime: %s ", errmsg);
        endif
        this.TimeZone = TimeZone;
      endif

      ## Datestrings are currently handled by 'datevec'
      if (iscellstr (args{1}) || isstring (args{1}) || ischar (args{1}))
        DateStrings = "";
        if (ischar (args{1}) && ! isvector (args{1}))
          error ("datetime: invalid type for 'DateStrings'.");
        elseif (ischar (args{1}) && !
                any (strcmpi (args, {'now', 'today', 'yesterday', 'tomorrow'})))
          DateStrings = cellstr (args{1});
        elseif (! ischar (args{1}))
          DateStrings = cellstr (args{1});
        endif
        if (! isempty (DateStrings))
          if (! isempty (inputFormat) && ! isempty (PivotYear))
            fcn = @(x) datevec (x, inputFormat, PivotYear);
          elseif (! isempty (inputFormat) && isempty (PivotYear))
            fcn = @(x) datevec (x, inputFormat);
          elseif (isempty (inputFormat) && ! isempty (PivotYear))
            fcn = @(x) datevec (x, PivotYear);
          else
            fcn = @(x) datevec (x);
          endif
          try
            DATEVEC = cellfun (fcn, DateStrings, "UniformOutput", false);
            DATEVEC = cell2mat (DATEVEC(:));
          catch
            if (! isempty (inputFormat))
              error ("datetime: invalid 'inputFormat'.");
            else
              error (strcat ("datetime: could not recognize date/time", ...
                             " format from input."));
            endif
          end_try_catch
          ## Split DATEVEC into individual date/time units and reshape
          this.Year = reshape (DATEVEC(:,1), size (DateStrings));
          this.Month = reshape (DATEVEC(:,2), size (DateStrings));
          this.Day = reshape (DATEVEC(:,3), size (DateStrings));
          this.Hour = reshape (DATEVEC(:,4), size (DateStrings));
          this.Minute = reshape (DATEVEC(:,5), size (DateStrings));
          this.Second = reshape (DATEVEC(:,6), size (DateStrings));
          return;
        endif
      endif

      ## Handle inputs (no errors here)
      if (! isempty (ConvertFrom) && ! isempty (TimeZone))
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (args{1}, 'ConvertFrom', ConvertFrom, ...
                                      'TimeZone', TimeZone);
      elseif (! isempty (ConvertFrom) && isempty (TimeZone))
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (args{1}, 'ConvertFrom', ConvertFrom);
      else
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (args{:});
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{cstr} =} dispstrings (@var{T})
    ##
    ## Get display formatted strings for each element of a datetime object.
    ##
    ## @code{@var{cstr} = dispstrings (@var{T})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## datetime @var{T}.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      ## Default display format
      mnames = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', ...
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
      ## Process all elements
      sz = size (this);
      cstr = cell (sz);
      for i = 1:prod (sz)
        if (isnan (this.Year(i)))
          cstr{i} = 'NaT';
        elseif (isinf (this.Year(i)))
          cstr{i} = num2str (this.Year(i));
        else
          if (this.Hour(i) != 0 || this.Minute(i) != 0 || this.Second(i) != 0)
            if (fix (this.Second(i)) == this.Second(i))
              str = sprintf ("%02d-%s-%04d %02d:%02d:%02d", this.Day(i), ...
                             mnames{this.Month(i)}, this.Year(i), ...
                             this.Hour(i), this.Minute(i), this.Second(i));
            else
              fmt = "%02d-%s-%04d %02d:%02d:%02d.%03d";
              sec = this.Second(i);
              str = sprintf (fmt, this.Day(i), mnames{this.Month(i)}, ...
                             this.Year(i), this.Hour(i), this.Minute(i), ...
                             fix (sec), rem (sec, 1) * 1000);
            endif
          else
            str = sprintf ("%02d-%s-%04d", this.Day(i), ...
                           mnames{this.Month(i)}, this.Year(i));
          endif
          cstr{i} = str;
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{cstr} =} cellstr (@var{T})
    ## @deftypefnx {datetime} {@var{cstr} =} cellstr (@var{T}, @var{Format})
    ##
    ## Convert datetime array to a cell array of character vectors.
    ##
    ## @code{@var{cstr} = cellstr (@var{T})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## datetime @var{T}.
    ##
    ## @end deftypefn
    function cstr = cellstr (this, Format = '')
      if (! isempty (Format))
        if (! (ischar (Format) && isvector (Format)))
          error ("datetime.cellstr: FORMAT must be a character vector.");
        endif
        this.Format = Format;
      endif
      cstr = dispstrings (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{cmat} =} char (@var{T})
    ## @deftypefnx {datetime} {@var{cmat} =} char (@var{T}, @var{Format})
    ##
    ## Convert datetime array to a character matrix.
    ##
    ## @code{@var{cmat} = char (@var{T})} returns a character matrix with one
    ## row per element in @var{T}.
    ##
    ## @end deftypefn
    function cmat = char (this, Format = '')
      cmat = char (cellstr (this, Format));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {[@var{Y}, @var{M}, @var{D}] =} ymd (@var{T})
    ##
    ## Year, Month, and Day components of datetime array.
    ##
    ## @code{[@var{Y}, @var{M}, @var{D}] = ymd (@var{T})} returns the year,
    ## month, and day components of the corresponding datetime values in @var{T}
    ## as separate numeric arrays.  @var{Y}, @var{M}, @var{D} contain integer
    ## values and have the same size as @var{T}.  Not-A-Time (@qcode{NaT})
    ## values in @var{T} are returned as @qcode{NaN} in the output arrays.
    ##
    ## @end deftypefn
    function [Y, M, D] = ymd (this)
      Y = this.Year;
      M = this.Month;
      D = this.Day;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {[@var{h}, @var{m}, @var{s}] =} hms (@var{T})
    ##
    ## Hour, Minute, and Second components of a datetime array.
    ##
    ## @code{[@var{h}, @var{m}, @var{s}] = hms (@var{T})} returns the hour,
    ## minute, and second components of the corresponding datetime values in
    ## @var{T} as separate numeric arrays.  @var{h} and @var{m} contain integer
    ## values and @var{s} may also contain a fractional part.  All outputs have
    ## the same size as @var{T}.  Not-A-Time (@qcode{NaT}) values in @var{T} are
    ## returned as @qcode{NaN} in the output arrays.
    ##
    ## @end deftypefn
    function [h, m, s] = hms (this)
      h = this.Hour;
      m = this.Minute;
      s = this.Second;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{Y} =} year (@var{T})
    ##
    ## Year component of a datetime array.
    ##
    ## @code{@var{Y} = year (@var{T})} returns the year number for each element
    ## of the input datetime array @var{T}.  The output @var{Y} is a
    ## @qcode{double} array and it has the same size as @var{T}.  Not-A-Time
    ## (@qcode{NaT}) values in @var{T} are returned as @qcode{NaN} in the output
    ## array.
    ##
    ## @end deftypefn
    function out = year (this)
      out = this.Year;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{Q} =} quarter (@var{T})
    ##
    ## Quarter component of a datetime array.
    ##
    ## @code{@var{Q} = quarter (@var{T})} returns the quarter number for each
    ## element of the input datetime array @var{T}.  The output @var{Q} is a
    ## @qcode{double} array containing integer values in the range @math{[1, 4]}
    ## and it has the same size as @var{T}.  Not-A-Time (@qcode{NaT}) values in
    ## @var{T} are returned as @qcode{NaN} in the output array.
    ##
    ## @end deftypefn
    function out = quarter (this)
      out = ceil (this.Month / 3);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{M} =} month (@var{T})
    ## @deftypefnx {datetime} {@var{M} =} month (@var{T}, @var{monthType})
    ##
    ## Month component of a datetime array.
    ##
    ## @code{@var{M} = month (@var{T})} returns the month number for each
    ## element of the input datetime array @var{T}.  The output @var{M} is a
    ## @qcode{double} array containing integer values in the range
    ## @math{[1, 12]} and it has the same size as @var{T}.  Not-A-Time
    ## (@qcode{NaT}) values in @var{T} are returned as @qcode{NaN} in the output
    ## array.
    ##
    ## @code{@var{M} = month (@var{T}, @var{monthType})} returns the month
    ## number or name for each element of the input datetime array @var{T} as
    ## specified by @var{monthType}, which may have any of the following
    ## options:
    ##
    ## @itemize
    ## @item @qcode{'monthofyear'} (default) returns the month number in a
    ## numeric array.
    ## @item @qcode{'name'} returns the corresponding full name of the month in
    ## a cell array of character vectors.
    ## @item @qcode{'shortname'} returns the corresponding 3-letter abbreviation
    ## of the month in a cell array of character vectors.
    ## @end itemize
    ##
    ## @end deftypefn
    function out = month (this, type = 'monthofyear')
      if (strcmpi (type, 'monthofyear'))
        out = this.Month;
      elseif (strcmpi (type, 'name'))
        mn = {'January', 'February', 'March', 'April', 'May', 'June', ...
              'July', 'August', 'September', 'October', 'November', 'December'};
        out = mn(this.Month);
      elseif (strcmpi (type, 'shortname'))
        mn = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', ...
              'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
        out = mn(this.Month);
      else
        error ("datetime: unrecognized MONTHTYPE.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{D} =} day (@var{T})
    ## @deftypefnx {datetime} {@var{D} =} day (@var{T}, @var{dayType})
    ##
    ## Day component of a datetime array.
    ##
    ## @code{@var{D} = day (@var{T})} returns the day number for each element of
    ## the input datetime array @var{T}.  The output @var{D} is a @qcode{double}
    ## array containing integer values in the range @math{[1, 31]}, depending on
    ## the month and year, and it has the same size as @var{T}.  Not-A-Time
    ## (@qcode{NaT}) values in @var{T} are returned as @qcode{NaN} in the output
    ## array.
    ##
    ## @code{@var{D} = day (@var{T}, @var{dayType})} returns the day number or
    ## name for each element of the input datetime array @var{T} as specified by
    ## @var{dayType}, which may have any of the following options:
    ##
    ## @itemize
    ## @item @qcode{'dayofmonth'} (default) returns the day-of-month number in a
    ## numeric array.  Depending on the month and year, it can range from 1 to
    ## 28, 29, 30, or 31.
    ## @item @qcode{'dayofweek'} returns the day-of-week number in a numeric
    ## array of @qcode{double} integer values ranging from 1 to 7, where 1
    ## corresponds to Sunday.
    ## @item @qcode{'iso-dayofweek'} returns the day-of-week number in a numeric
    ## array of @qcode{double} integer values ranging from 1 to 7, where 1
    ## corresponds to Monday according to the ISO 8601 standard.
    ## @item @qcode{'dayofyear'} returns the day-of-year number in a numeric
    ## array of @qcode{double} integer values ranging from 1 to 365 or 366,
    ## depending on the year.
    ## @item @qcode{'name'} returns the corresponding full name of the day in
    ## a cell array of character vectors.
    ## @item @qcode{'shortname'} returns the corresponding 3-letter abbreviation
    ## of the day in a cell array of character vectors.
    ## @end itemize
    ##
    ## @end deftypefn
    function out = day (this, type = 'dayofmonth')
      vtypes = {'dayofweek', 'iso-dayofweek', 'name', 'shortname'};
      if (strcmpi (type, 'dayofmonth'))
        out = this.Day;
      elseif (any (strcmpi (type, vtypes)))
        m = this.Month - 2;
        y = this.Year;
        ## Compute the Jan/Feb borrow mask once, before mutating 'm'; adjusting
        ## 'm' first would clear the mask before 'y' is decremented.
        janfeb = m < 1;
        m(janfeb) += 12;
        y(janfeb) -= 1;
        K = mod (y, 100);
        J = floor (y ./ 100);
        code = floor ((26 .* m - 2) ./ 10);
        out = mod ((this.Day + code + K + floor (K ./ 4) ...
                             + floor (J ./ 4) + 5 .* J), 7) + 1;
        if (strcmpi (type, 'iso-dayofweek'))
          out = mod (out - 1, 7);
          out(out == 0) = 7;
        elseif (strcmpi (type, 'name'))
          dn = {'Sunday', 'Monday', 'Tuesday', 'Wednesday', ...
                'Thursday', 'Friday', 'Saturday'};
          out = dn(out);
        elseif (strcmpi (type, 'shortname'))
          dn = {'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'};
          out = dn(out);
        endif
      elseif (strcmpi (type, 'dayofyear'))
        m = this.Month;
        y = this.Year;
        ## Column vector so that logical-indexed lookups keep column shape.
        cumdays = [0; 31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334];
        isly = mod (y, 4) == 0 & (mod (y, 100) != 0 | mod (y, 400) == 0);
        out = nan (size (m));
        valid = isfinite (m) & isfinite (this.Day);
        out(valid) = cumdays(m(valid)) + this.Day(valid)(:) ...
                     + double (isly(valid) & m(valid) > 2)(:);
      else
        error ("datetime: unrecognized DAYTYPE.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{h} =} hour (@var{T})
    ##
    ## Hour component of a datetime array.
    ##
    ## @code{@var{h} = hour (@var{T})} returns the hour number for each element
    ## of the input datetime array @var{T}.  The output @var{h} is a
    ## @qcode{double} array containing integer values in the range
    ## @math{[0, 23]} and it has the same size as @var{T}.  Not-A-Time
    ## (@qcode{NaT}) values in @var{T} are returned as @qcode{NaN} in the output
    ## array.
    ##
    ## @end deftypefn
    function out = hour (this)
      out = this.Hour;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{m} =} minute (@var{T})
    ##
    ## Minute component of a datetime array.
    ##
    ## @code{@var{m} = minute (@var{T})} returns the minute number for each
    ## element of the input datetime array @var{T}.  The output @var{m} is a
    ## @qcode{double} array containing integer values in the range
    ## @math{[0, 59]} and it has the same size as @var{T}.  Not-A-Time
    ## (@qcode{NaT}) values in @var{T} are returned as @qcode{NaN} in the output
    ## array.
    ##
    ## @end deftypefn
    function out = minute (this)
      out = this.Minute;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{s} =} second (@var{T})
    ## @deftypefnx {datetime} {@var{s} =} second (@var{T}, @var{secondType})
    ##
    ## Seconds component of a datetime array.
    ##
    ## @code{@var{s} = second (@var{T})} returns the number of seconds for each
    ## element of the input datetime array @var{T}.  The output @var{s} is a
    ## @qcode{double} array containing values in the range @math{[0, 60)},
    ## including any fractional part of the second, and it has the same size as
    ## @var{T}.  Not-A-Time (@qcode{NaT}) values in @var{T} are returned as
    ## @qcode{NaN} in the output array.
    ##
    ## @code{@var{s} = second (@var{T}, @var{secondType})} returns the seconds
    ## for each element of the input datetime array @var{T} as specified by
    ## @var{secondType}, which may have any of the following options:
    ##
    ## @itemize
    ## @item @qcode{'secondofminute'} (default) returns the second of the minute
    ## in a numeric array, in the range @math{[0, 60)}.
    ## @item @qcode{'secondofday'} returns the second of the day in a numeric
    ## array, in the range @math{[1, 86400)}.
    ## @end itemize
    ##
    ## @end deftypefn
    function out = second (this, secondType = 'secondofminute')
      if (strcmpi (secondType, 'secondofminute'))
        out = this.Second;
      elseif (strcmpi (secondType, 'secondofday'))
        out = this.Hour * 3600 + this.Minute * 60 + this.Second;
      else
        error ("datetime.second: unrecognized SECONDTYPE.");
      endif
    endfunction

  endmethods

  methods (Hidden)

    function out = week (this)
      error ("datetime.week: not implemented yet.");
    endfunction

    function out = timeofday (this)
      error ("datetime.timeofday: not implemented yet.");
    endfunction

    function out = tzoffset (this)
      error ("datetime.tzoffset: not implemented yet.");
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'size'             'ndims'            'numel'            'length'          ##
## 'keyHash'                                                                  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{sz} =} size (@var{T})
    ## @deftypefnx {datetime} {@var{dim_sz} =} size (@var{T}, @var{dim})
    ## @deftypefnx {datetime} {@var{dim_sz} =} size (@var{T}, @var{d1}, @var{d2}, @dots{})
    ## @deftypefnx {datetime} {[@var{rows}, @var{columns}, @dots{}, @var{dim_n_sz}] =} size (@dots{})
    ##
    ## Size of a datetime array.
    ##
    ## @code{@var{sz} = size (@var{T})} returns a row vector with the size
    ## (number of elements) of each dimension for the datetime array @var{T}.
    ##
    ## @code{@var{dim_sz} = size (@var{T}, @var{dim})} returns the size of
    ## the corresponding dimension specified in @var{dim}.  If @var{dim} is a
    ## vector, then @var{dim_sz} is a vector of the same length and with each
    ## element corresponding to a specified dimension.  Multiple dimensions may
    ## also be specified as separate arguments.
    ##
    ## With a single output argument, @code{size} returns a row vector.  When
    ## called with multiple output arguments, @code{size} returns the size of
    ## dimension N in the Nth argument.
    ##
    ## @end deftypefn
    function varargout = size (this, varargin)
      if (! isempty (varargin))
        sz = size (this.Year, varargin{:});
      else
        sz = size (this.Year);
      endif
      if (nargout == 0 || nargout == 1)
        varargout{1} = sz;
      elseif (numel (sz) != nargout)
        error (["datetime.size: nargout > 1 but does not", ...
                " match number of requested dimensions."]);
      else
        for i = 1:nargout
          varargout{i} = sz(i);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{out} =} ndims (@var{T})
    ##
    ## Number of dimensions in a datetime array.
    ##
    ## @code{@var{out} = ndims (@var{T})} returns the number of dimensions of
    ## the datetime array @var{T}.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = ndims (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{out} =} numel (@var{T})
    ##
    ## Total number of elements in a datetime array.
    ##
    ## @code{@var{out} = numel (@var{T})} returns the number of elements in the
    ## datetime array @var{T}, which is the product of the sizes of its
    ## dimensions.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = prod (size (this));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{N} =} length (@var{T})
    ##
    ## Length of a datetime vector.
    ##
    ## @code{@var{N} = length (@var{T})} returns the size of the longest
    ## dimension of the datetime array @var{T}, unless any of its dimensions
    ## has zero length, in which case @code{length (@var{T})} returns 0.
    ##
    ## @end deftypefn
    function N = length (this)
      if (isempty (this.Year))
        N = 0;
      else
        N = max (size (this.Year));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{hey} =} keyHash (@var{T})
    ##
    ## Generate a hash code for datetime array.
    ##
    ## @code{@var{h} = keyHash (@var{T})} generates a @qcode{uint64} scalar that
    ## represents the input array @var{T}.  @code{keyHash} utilizes the 64-bit
    ## FNV-1a variant of the Fowler-Noll-Vo non-cryptographic hash function.
    ##
    ## @code{@var{h} = keyHash (@var{T}), @var{base}} also generates a 64-bit
    ## hash code using @var{base} as the offset basis for the FNV-1a hash
    ## algorithm.  @var{base} must be a @qcode{uint64} integer type scalar.  Use
    ## this syntax to cascade @code{keyHash} on multiple objects for which a
    ## single hash code is required.
    ##
    ## Note that unlike MATLAB, this implementation does not use any random
    ## seed.  As a result, @code{keyHash} will always generate the exact same
    ## hash key for any particular input across different workers and Octave
    ## sessions.
    ##
    ## @end deftypefn
    function key = keyHash (this, base = [])
      ## Initialize string with size and class name
      size_str = sprintf ('%dx', size (this.Year))(1:end-1);
      flag_str = sprintf ('-TZ%s:', this.TimeZone);
      init_str = [size_str 'datetime' flag_str];
      if (base)
        if (! (isscalar (base) && isa (base, 'uint64')))
          error ("datetime.keyHash: BASE must be a UINT64 scalar.");
        endif
        key = __ckeyHash__(init_str, base);
      else
        key = __ckeyHash__(init_str);
      endif
      if (! isempty (this.Year))
        key = __nkeyHash__(this.Year(:), key);
        key = __nkeyHash__(this.Month(:), key);
        key = __nkeyHash__(this.Day(:), key);
        key = __nkeyHash__(this.Hour(:), key);
        key = __nkeyHash__(this.Minute(:), key);
        key = __nkeyHash__(this.Second(:), key);
      endif
    endfunction

  endmethods

################################################################################
##              ** Convert to other Numeric Representations **                ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'convertTo'        'exceltime'        'posixtime'        'juliandate'      ##
## 'yyyymmdd'         'datevec'                                               ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function out = convertTo (this, varargin)
      error ("datetime.convertTo: not implemented yet.");
    endfunction

    function out = exceltime (this, varargin)
      error ("datetime.exceltime: not implemented yet.");
    endfunction

    function out = posixtime (this, varargin)
      error ("datetime.posixtime: not implemented yet.");
    endfunction

    function out = juliandate (this, varargin)
      error ("datetime.juliandate: not implemented yet.");
    endfunction

    function out = yyyymmdd (this, varargin)
      error ("datetime.yyyymmdd: not implemented yet.");
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{DV} =} datevec (@var{T})
    ##
    ## Convert datetime array to date vectors.
    ##
    ## @code{@var{DV} = datevec (@var{T})} returns a numeric matrix with one row
    ## per element in @var{T}.
    ##
    ## @end deftypefn
    function varargout = datevec (this)
      DV = [this.Year(:), this.Month(:), this.Day(:), ...
            this.Hour(:), this.Minute(:), this.Second(:)];
      if (nargout == 0 || nargout == 1)
        varargout{1} = DV;
      elseif (nargout <= 6)
        for i = 1:nargout
          varargout{i} = DV(:,i);
        endfor
      else
        error ("datetime.datevec: too many output arguments.");
      endif
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'isbetween'        'iscolumn'         'isdst'            'isempty'         ##
## 'isequal'          'isequaln'         'isfinite'         'isinf'           ##
## 'ismatrix'         'ismember'         'isnat'            'isregular'       ##
## 'isrow'            'isscalar'         'issorted'         'issortedrows'    ##
## 'isvector'         'isweekend'                                             ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function TF = isbetween (this, varargin)
      error ("datetime.isbetween: not implemented yet.");
    endfunction

    function TF = isdst (this)
      error ("datetime.isdst: not implemented yet.");
    endfunction

    function TF = isregular (this)
      error ("datetime.isregular: not implemented yet.");
    endfunction

    function TF = isweekend (this)
      error ("datetime.isweekend: not implemented yet.");
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{TF} =} issorted (@var{A})
    ## @deftypefnx {datetime} {@var{TF} =} issorted (@var{A}, @var{dim})
    ## @deftypefnx {datetime} {@var{TF} =} issorted (@var{A}, @var{direction})
    ## @deftypefnx {datetime} {@var{TF} =} issorted (@var{A}, @var{dim}, @var{direction})
    ##
    ## Determine whether a datetime array is sorted.
    ##
    ## @code{@var{TF} = issorted (@var{A})} returns @qcode{true} if the elements
    ## of the datetime array @var{A} are sorted in ascending (non-decreasing)
    ## order along its first non-singleton dimension, and @qcode{false}
    ## otherwise.  For a matrix, every column (or row, depending on the operating
    ## dimension) must be sorted for @var{TF} to be @qcode{true}.  Not-A-Time
    ## (@qcode{NaT}) elements are treated as greater than any other value, so an
    ## array is sorted in ascending order only when its @qcode{NaT} elements
    ## come last.
    ##
    ## @code{@var{TF} = issorted (@var{A}, @var{dim})} operates along dimension
    ## @var{dim}.
    ##
    ## @code{@var{TF} = issorted (@var{A}, @var{direction})} tests whether the
    ## elements are sorted according to @var{direction}, which may be one of:
    ##
    ## @itemize
    ## @item @qcode{'ascend'} (default) tests non-decreasing order.
    ## @item @qcode{'descend'} tests non-increasing order.
    ## @item @qcode{'monotonic'} tests non-decreasing or non-increasing order.
    ## @item @qcode{'strictascend'} tests strictly increasing order.
    ## @item @qcode{'strictdescend'} tests strictly decreasing order.
    ## @item @qcode{'strictmonotonic'} tests strictly monotonic order.
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = issorted (A, varargin)
      dim = [];
      direction = 'ascend';
      valid = {'ascend', 'descend', 'monotonic', 'strictascend', ...
               'strictdescend', 'strictmonotonic'};
      for i = 1:numel (varargin)
        arg = varargin{i};
        if (isnumeric (arg))
          if (! isscalar (arg) || arg < 1 || arg != fix (arg))
            error ("datetime.issorted: DIM must be a positive integer.");
          endif
          dim = arg;
        elseif (ischar (arg) && isrow (arg))
          didx = find (strcmpi (arg, valid));
          if (isempty (didx))
            error ("datetime.issorted: invalid DIRECTION '%s'.", arg);
          endif
          direction = valid{didx};
        else
          error ("datetime.issorted: invalid input argument.");
        endif
      endfor
      if (isempty (dim))
        dim = find (size (A) != 1, 1);
        if (isempty (dim))
          dim = 1;
        endif
      endif
      ## NaT sorts as greater than any value; map it to +Inf on the serial.
      M = serial (A);
      M(isnan (M)) = Inf;
      if (dim > 2 || size (M, dim) < 2)
        TF = true;
        return;
      endif
      if (dim == 2)
        M = M.';
      endif
      lo = M(1:end-1, :);
      hi = M(2:end, :);
      switch (direction)
        case 'ascend'
          ok = all (lo <= hi, 1);
        case 'descend'
          ok = all (lo >= hi, 1);
        case 'strictascend'
          ok = all (lo < hi, 1);
        case 'strictdescend'
          ok = all (lo > hi, 1);
        case 'monotonic'
          ok = all (lo <= hi, 1) | all (lo >= hi, 1);
        case 'strictmonotonic'
          ok = all (lo < hi, 1) | all (lo > hi, 1);
      endswitch
      TF = all (ok(:));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{TF} =} issortedrows (@var{A})
    ## @deftypefnx {datetime} {@var{TF} =} issortedrows (@var{A}, @var{column})
    ## @deftypefnx {datetime} {@var{TF} =} issortedrows (@var{A}, @var{direction})
    ## @deftypefnx {datetime} {@var{TF} =} issortedrows (@var{A}, @var{column}, @var{direction})
    ##
    ## Determine whether the rows of a datetime array are sorted.
    ##
    ## @code{@var{TF} = issortedrows (@var{A})} returns @qcode{true} if the rows
    ## of the 2-D datetime array @var{A} are sorted in ascending order, i.e.@:
    ## lexicographically by the first column, ties broken by the second column,
    ## and so on, and @qcode{false} otherwise.  Not-A-Time (@qcode{NaT}) elements
    ## are treated as greater than any other value.
    ##
    ## @code{@var{TF} = issortedrows (@var{A}, @var{column})} checks the order
    ## using only the columns listed in @var{column}, in the given priority.  A
    ## negative entry checks the corresponding column for descending order.
    ##
    ## @code{@var{TF} = issortedrows (@var{A}, @var{direction})} checks for the
    ## order given by @var{direction}, which may be one of @qcode{'ascend'}
    ## (default), @qcode{'descend'}, @qcode{'monotonic'}, @qcode{'strictascend'},
    ## @qcode{'strictdescend'}, or @qcode{'strictmonotonic'}.  It may also be a
    ## cell array of @qcode{'ascend'}/@qcode{'descend'} strings, one per sort
    ## column.  For the strict options a matrix qualifies only when its first
    ## sort column is strictly monotonic and free of @qcode{NaT}.
    ##
    ## @code{@var{TF} = issortedrows (@var{A}, @var{column}, @var{direction})}
    ## combines an explicit column list with a @var{direction}.
    ##
    ## @end deftypefn
    function TF = issortedrows (A, varargin)
      if (ndims (A) != 2)
        error ("datetime.issortedrows: A must be a 2-D datetime array.");
      endif
      ncol = size (A, 2);
      keywords = {'ascend', 'descend', 'monotonic', 'strictascend', ...
                  'strictdescend', 'strictmonotonic'};
      column = [];
      direction = [];
      if (numel (varargin) >= 1)
        if (isnumeric (varargin{1}))
          column = varargin{1};
          if (numel (varargin) > 2)
            error ("datetime.issortedrows: too many input arguments.");
          elseif (numel (varargin) == 2)
            direction = varargin{2};
          endif
        else
          direction = varargin{1};
          if (numel (varargin) > 1)
            error ("datetime.issortedrows: COLUMN must precede DIRECTION.");
          endif
        endif
      endif
      if (isempty (column))
        column = 1:ncol;
      endif
      column = column(:).';
      if (any (column == 0) || any (column != fix (column)) ...
          || any (abs (column) > ncol))
        error ("datetime.issortedrows: COLUMN out of range.");
      endif
      colmag = abs (column);
      desc = column < 0;
      check = 'ascend';
      if (! isempty (direction))
        if (ischar (direction) && isrow (direction))
          check = lower (direction);
          if (! any (strcmp (check, keywords)))
            error ("datetime.issortedrows: invalid DIRECTION '%s'.", direction);
          endif
          if (any (strcmp (check, {'ascend', 'strictascend'})))
            desc = false (size (colmag));
          elseif (any (strcmp (check, {'descend', 'strictdescend'})))
            desc = true (size (colmag));
          endif
        elseif (iscellstr (direction))
          if (numel (direction) != numel (colmag))
            error (strcat ("datetime.issortedrows: DIRECTION must have one", ...
                           " entry per sort column."));
          endif
          desc = false (size (colmag));
          for j = 1:numel (direction)
            if (strcmpi (direction{j}, 'descend'))
              desc(j) = true;
            elseif (! strcmpi (direction{j}, 'ascend'))
              error ("datetime.issortedrows: invalid DIRECTION '%s'.", ...
                     direction{j});
            endif
          endfor
        else
          error ("datetime.issortedrows: invalid DIRECTION argument.");
        endif
      endif
      if (isempty (colmag))
        TF = true;
        return;
      endif
      S = serial (A);
      if (any (strcmp (check, {'strictascend', 'strictdescend', ...
                               'strictmonotonic'})))
        ## Strict: only the first sort column matters; it must be strictly
        ## monotonic and contain no NaT.
        p = S(:, colmag(1));
        if (any (isnan (p)))
          TF = false;
        else
          d = diff (p);
          if (strcmp (check, 'strictascend'))
            TF = all (d > 0);
          elseif (strcmp (check, 'strictdescend'))
            TF = all (d < 0);
          else
            TF = all (d > 0) || all (d < 0);
          endif
        endif
        return;
      endif
      ## Non-strict: lexicographic check on the selected columns.  NaT maps to
      ## +Inf (largest); each column is negated to fold in a descending key.
      K = S(:, colmag);
      K(isnan (K)) = Inf;
      Kd = K;
      for j = 1:numel (colmag)
        if (desc(j))
          Kd(:, j) = -Kd(:, j);
        endif
      endfor
      if (strcmp (check, 'monotonic'))
        TF = rowsNonDecreasing (A, Kd) || rowsNonDecreasing (A, -Kd);
      else
        TF = rowsNonDecreasing (A, Kd);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} iscolumn (@var{T})
    ##
    ## Return true if datetime array is a column vector.
    ##
    ## @code{@var{TF} = iscolumn (@var{T})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the datetime array @var{T} is a column
    ## vector, and @qcode{false} otherwise.  A column vector is a 2-D array for
    ## which @code{size (@var{X})} returns @code{[@var{N}, 1]} with non-negative
    ## @var{N}.
    ##
    ## @end deftypefn
    function TF = iscolumn (this)
      TF = iscolumn (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isempty (@var{T})
    ##
    ## Return true if datetime array is empty.
    ##
    ## @code{@var{TF} = isempty (@var{T})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the datetime array @var{T} is empty, and
    ## @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isempty (this)
      TF = isempty (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isequal (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{TF} =} isequal (@var{A}, @var{B}, @dots{})
    ##
    ## Test datetime arrays for equality.
    ##
    ## @code{@var{TF} = isequal (@var{A}, @var{B})} returns a logical scalar
    ## @var{TF}, which is @qcode{true} if the datetime arrays @var{A} and
    ## @var{B} are the same size and each pair of corresponding elements is the
    ## same point in time, and @qcode{false} otherwise.  As with @qcode{NaN},
    ## Not-A-Time (@qcode{NaT}) elements are never equal, so any @qcode{NaT} in
    ## either array makes the result @qcode{false}; use @code{isequaln} to treat
    ## @qcode{NaT} elements as equal.
    ##
    ## Additional arrays may be supplied, as in @code{isequal (@var{A}, @var{B},
    ## @var{C}, @dots{})}, in which case @var{TF} is @qcode{true} only when all
    ## of the arrays are equal to one another.  Any input argument that is not a
    ## datetime array, or a datetime array whose time zone is not compatible
    ## with the others (one zoned and one unzoned), makes the result
    ## @qcode{false} rather than raising an error.  Zoned arrays are compared by
    ## their absolute instants, so equal instants in different time zones are
    ## equal.
    ##
    ## @end deftypefn
    function TF = isequal (varargin)
      if (nargin < 2)
        print_usage ();
      endif
      TF = do_isequal (varargin, false);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isequaln (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{TF} =} isequaln (@var{A}, @var{B}, @dots{})
    ##
    ## Test datetime arrays for equality, treating Not-A-Time as equal.
    ##
    ## @code{@var{TF} = isequaln (@var{A}, @var{B})} is identical to
    ## @code{isequal (@var{A}, @var{B})} except that Not-A-Time (@qcode{NaT})
    ## elements are treated as equal to one another, in the same way that
    ## @code{isequaln} treats @qcode{NaN}.  It returns a logical scalar @var{TF},
    ## which is @qcode{true} if the datetime arrays @var{A} and @var{B} are the
    ## same size and each pair of corresponding elements is either the same
    ## point in time or both @qcode{NaT}, and @qcode{false} otherwise.
    ##
    ## Additional arrays may be supplied, as in @code{isequaln (@var{A},
    ## @var{B}, @var{C}, @dots{})}, in which case @var{TF} is @qcode{true} only
    ## when all of the arrays are equal to one another.  Any input argument that
    ## is not a datetime array, or a datetime array whose time zone is not
    ## compatible with the others (one zoned and one unzoned), makes the result
    ## @qcode{false} rather than raising an error.  Zoned arrays are compared by
    ## their absolute instants, so equal instants in different time zones are
    ## equal.
    ##
    ## @end deftypefn
    function TF = isequaln (varargin)
      if (nargin < 2)
        print_usage ();
      endif
      TF = do_isequal (varargin, true);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{out} =} isfinite (@var{T})
    ##
    ## Test for finite elements in datetime array.
    ##
    ## @code{@var{TF} = isfinite (@var{T})} returns a logical array @var{TF} of
    ## the same size as @var{T} containing @qcode{true} for each corresponding
    ## element of @var{T} that is finite and @qcode{false} otherwise.  Finite
    ## elements in datetime arrays are those which are neither @qcode{Inf} nor
    ## @qcode{NaT}.
    ##
    ## @end deftypefn
    function TF = isfinite (this)
      TF = isfinite (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{out} =} isinf (@var{T})
    ##
    ## Test for infinite elements in datetime array.
    ##
    ## @code{@var{TF} = isinf (@var{T})} returns a logical array @var{TF} of the
    ## same size as @var{T} containing @qcode{true} for each corresponding
    ## element of @var{T} that is infinite and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isinf (this)
      TF = isinf (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} ismatrix (@var{T})
    ##
    ## Return true if datetime array is a 2-D array.
    ##
    ## @code{@var{TF} = ismatrix (@var{T})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the datetime array @var{T} is a matrix, and
    ## @qcode{false} otherwise.  A matrix is an array of any type where
    ## @code{ndims (@var{X}) == 2} and for which @code{size (@var{X})} returns
    ## @code{[@var{H}, @var{W}]} with non-negative @var{H} and @var{W}.
    ##
    ## @end deftypefn
    function TF = ismatrix (this)
      TF = ismatrix (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{TF} =} ismissing (@var{T})
    ## @deftypefnx {datetime} {@var{TF} =} ismissing (@var{T}, @var{indicator})
    ##
    ## Test for missing elements in datetime array.
    ##
    ## @var{TF} is a logical array of the same size as @var{T}.
    ##
    ## @end deftypefn
    function TF = ismissing (this, varargin)
      if (nargin > 2)
        error ("datetime.ismissing: too many input arguments.");
      endif
      if (! isempty (varargin))
        indicator = varargin{1};
        TF = false (size (this));
        if (isvector (indicator))
          if (isa (indicator, 'datetime'))
            for i = 1:numel (indicator.Year)
              is_eq = indicator.Year(i)   == this.Year   & ...
                      indicator.Month(i)  == this.Month  & ...
                      indicator.Day(i)    == this.Day    & ...
                      indicator.Hour(i)   == this.Hour   & ...
                      indicator.Minute(i) == this.Minute & ...
                      indicator.Second(i) == this.Second;
              TF(is_eq) = true;
            endfor
          else
            error ("datetime.ismissing: INDICATOR must be a 'datetime' array.");
          endif
        else
          error ("datetime.ismissing: INDICATOR must be a vector.");
        endif
      else
        TF = isnan (this.Year);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isnat (@var{T})
    ##
    ## Test for Not-A-Time elements in datetime array.
    ##
    ## @code{@var{TF} = isnat (@var{T})} returns a logical array @var{TF} of the
    ## same size as @var{T} containing @qcode{true} for each corresponding
    ## element of @var{T} that is Not-A-Time (@qcode{NaT}) and @qcode{false}
    ## otherwise.  @qcode{NaT} is the equivalent of @qcode{NaN} in numeric
    ## arrays.
    ##
    ## If @var{T} is not a datetime array, @code{isnat} returns an error.
    ##
    ## @end deftypefn
    function TF = isnat (this)
      TF = isnan (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isrow (@var{T})
    ##
    ## Return true if datetime array is a row vector.
    ##
    ## @code{@var{TF} = isrow (@var{T})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the datetime array @var{T} is a row vector,
    ## and @qcode{false} otherwise.  A row vector is a 2-D array for which
    ## @code{size (@var{X})} returns @code{[1, @var{N}]} with non-negative
    ## @var{N}.
    ##
    ## @end deftypefn
    function TF = isrow (this)
      TF = isrow (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isscalar (@var{T})
    ##
    ## Return true if datetime array is a scalar.
    ##
    ## @code{@var{TF} = isscalar (@var{T})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the datetime array @var{T} is also a scalar,
    ## and @qcode{false} otherwise.  A scalar is a single element object for
    ## which @code{size (@var{X})} returns @code{[1, 1]}.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = isscalar (this.Year);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isvector (@var{T})
    ##
    ## Return true if datetime array is a vector.
    ##
    ## @code{@var{TF} = isvector (@var{T})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the datetime array @var{T} is a vector and
    ## @qcode{false} otherwise.  A vector is a 2-D array for which one of the
    ## dimensions is equal to 1 (either @math{1*N} or @math{N*1}).  By
    ## definition, a scalar is also a vector.
    ##
    ## @end deftypefn
    function TF = isvector (this)
      TF = isvector (this.Year);
    endfunction

  endmethods

################################################################################
##                   ** Sort, Filter, and Set Operations **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'sort'             'sortrows'         'unique'           'interp1'         ##
## 'intersect'        'setdiff'          'setxor'           'union'           ##
## 'min'              'max'                                                   ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} sort (@var{A})
    ## @deftypefnx {datetime} {@var{B} =} sort (@var{A}, @var{dim})
    ## @deftypefnx {datetime} {@var{B} =} sort (@var{A}, @var{direction})
    ## @deftypefnx {datetime} {@var{B} =} sort (@var{A}, @var{dim}, @var{direction})
    ## @deftypefnx {datetime} {@var{B} =} sort (@dots{}, @qcode{'MissingPlacement'}, @var{mp})
    ## @deftypefnx {datetime} {[@var{B}, @var{I}] =} sort (@dots{})
    ##
    ## Sort a datetime array.
    ##
    ## @code{@var{B} = sort (@var{A})} returns the elements of the datetime array
    ## @var{A} sorted in ascending order along its first non-singleton
    ## dimension.  For a matrix, each column is sorted independently.  Sorting is
    ## stable: elements that compare as equal keep their original relative order.
    ##
    ## @code{@var{B} = sort (@var{A}, @var{dim})} sorts along dimension
    ## @var{dim}.
    ##
    ## @code{@var{B} = sort (@var{A}, @var{direction})} sorts in the order given
    ## by @var{direction}, which is either @qcode{'ascend'} (default) or
    ## @qcode{'descend'}.
    ##
    ## @code{@var{B} = sort (@dots{}, @qcode{'MissingPlacement'}, @var{mp})}
    ## controls where Not-A-Time (@qcode{NaT}) elements are placed.  @var{mp} may
    ## be @qcode{'auto'} (default; @qcode{NaT} last for ascending order and first
    ## for descending order), @qcode{'first'}, or @qcode{'last'}.
    ##
    ## @code{[@var{B}, @var{I}] = sort (@dots{})} also returns an index array
    ## @var{I} of the same size as @var{A} such that @var{B} is @var{A} indexed
    ## by @var{I} along the operating dimension.
    ##
    ## @end deftypefn
    function [B, I] = sort (A, varargin)
      dim = [];
      direction = 'ascend';
      placement = 'auto';
      i = 1;
      while (i <= numel (varargin))
        arg = varargin{i};
        if (ischar (arg) && isrow (arg) && strcmpi (arg, 'MissingPlacement'))
          if (i == numel (varargin))
            error ("datetime.sort: 'MissingPlacement' requires a value.");
          endif
          placement = lower (varargin{i+1});
          if (! any (strcmp (placement, {'auto', 'first', 'last'})))
            error ("datetime.sort: invalid 'MissingPlacement' value.");
          endif
          i += 2;
        elseif (isnumeric (arg))
          if (! isscalar (arg) || arg < 1 || arg != fix (arg))
            error ("datetime.sort: DIM must be a positive integer.");
          endif
          dim = arg;
          i += 1;
        elseif (ischar (arg) && isrow (arg))
          if (strcmpi (arg, 'ascend'))
            direction = 'ascend';
          elseif (strcmpi (arg, 'descend'))
            direction = 'descend';
          else
            error ("datetime.sort: invalid DIRECTION '%s'.", arg);
          endif
          i += 1;
        else
          error ("datetime.sort: invalid input argument.");
        endif
      endwhile
      if (isempty (dim))
        dim = find (size (A) != 1, 1);
        if (isempty (dim))
          dim = 1;
        endif
      endif
      ## Sorting along a singleton or higher dimension is a no-op.
      if (dim > 2 || size (A, dim) < 2)
        B = A;
        I = ones (size (A));
        return;
      endif
      descend = strcmp (direction, 'descend');
      ## Resolve 'auto': NaT goes last for ascending, first for descending.
      if (strcmp (placement, 'auto'))
        if (descend)
          placement = 'first';
        else
          placement = 'last';
        endif
      endif
      ## Work on the numeric serial (NaT -> NaN); sort along columns, so
      ## transpose for a row-wise sort and transpose the result back.
      S = serial (A);
      Y = A.Year; MO = A.Month; D = A.Day;
      H = A.Hour; MI = A.Minute; SE = A.Second;
      if (dim == 2)
        S = S.';
        Y = Y.'; MO = MO.'; D = D.'; H = H.'; MI = MI.'; SE = SE.';
      endif
      [nr, nc] = size (S);
      idx = zeros (nr, nc);
      for j = 1:nc
        col = S(:, j);
        nat = isnan (col);
        finidx = find (! nat);
        natidx = find (nat);
        ## Stable order: sort by value, breaking ties by original position.
        if (descend)
          [~, ord] = sortrows ([-col(finidx), finidx]);
        else
          [~, ord] = sortrows ([col(finidx), finidx]);
        endif
        finsorted = finidx(ord);
        if (strcmp (placement, 'first'))
          idx(:, j) = [natidx; finsorted];
        else
          idx(:, j) = [finsorted; natidx];
        endif
      endfor
      ## Reorder the component arrays column-wise using linear indexing.
      lin = idx + repmat ((0:nc-1) .* nr, nr, 1);
      Y = Y(lin); MO = MO(lin); D = D(lin);
      H = H(lin); MI = MI(lin); SE = SE(lin);
      I = idx;
      if (dim == 2)
        Y = Y.'; MO = MO.'; D = D.'; H = H.'; MI = MI.'; SE = SE.';
        I = idx.';
      endif
      B = A;
      B.Year = Y; B.Month = MO; B.Day = D;
      B.Hour = H; B.Minute = MI; B.Second = SE;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} sortrows (@var{A})
    ## @deftypefnx {datetime} {@var{B} =} sortrows (@var{A}, @var{column})
    ## @deftypefnx {datetime} {@var{B} =} sortrows (@var{A}, @var{direction})
    ## @deftypefnx {datetime} {@var{B} =} sortrows (@var{A}, @var{column}, @var{direction})
    ## @deftypefnx {datetime} {@var{B} =} sortrows (@dots{}, @qcode{'MissingPlacement'}, @var{mp})
    ## @deftypefnx {datetime} {[@var{B}, @var{index}] =} sortrows (@dots{})
    ##
    ## Sort the rows of a datetime array.
    ##
    ## @code{@var{B} = sortrows (@var{A})} sorts the rows of the 2-D datetime
    ## array @var{A} in ascending order.  Rows are ordered lexicographically: by
    ## the first column, ties broken by the second column, and so on.  The sort
    ## is stable, so rows that compare as equal keep their original order.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{column})} sorts using only the
    ## columns listed in @var{column}, in the given priority.  A negative entry
    ## sorts the corresponding column in descending order.  Columns not listed
    ## are not used as sort keys.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{direction})} sorts every column in
    ## the given @var{direction}, either @qcode{'ascend'} (default) or
    ## @qcode{'descend'}.  @var{direction} may also be a cell array of strings
    ## with one such value per sort column.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{column}, @var{direction})} combines
    ## an explicit column list with a per-column @var{direction}.
    ##
    ## @code{@var{B} = sortrows (@dots{}, @qcode{'MissingPlacement'}, @var{mp})}
    ## controls where Not-A-Time (@qcode{NaT}) elements are placed.  @var{mp} may
    ## be @qcode{'auto'} (default; @qcode{NaT} sorts as the largest value, i.e.@:
    ## last for ascending and first for descending columns), @qcode{'first'}, or
    ## @qcode{'last'}.
    ##
    ## @code{[@var{B}, @var{index}] = sortrows (@dots{})} also returns a column
    ## index vector @var{index} that maps the rows of @var{A} to @var{B}, such
    ## that @code{@var{B} = @var{A}(@var{index}, :)}.
    ##
    ## @end deftypefn
    function [B, index] = sortrows (A, varargin)
      if (ndims (A) != 2)
        error ("datetime.sortrows: A must be a 2-D datetime array.");
      endif
      ncol = size (A, 2);
      ## Split the 'MissingPlacement' (and ignored 'ComparisonMethod') name-value
      ## pairs off from the positional COLUMN/DIRECTION arguments.
      placement = 'auto';
      pos = {};
      k = 1;
      while (k <= numel (varargin))
        a = varargin{k};
        if (ischar (a) && isrow (a) && strcmpi (a, 'MissingPlacement'))
          if (k == numel (varargin))
            error ("datetime.sortrows: 'MissingPlacement' requires a value.");
          endif
          placement = lower (varargin{k+1});
          if (! any (strcmp (placement, {'auto', 'first', 'last'})))
            error ("datetime.sortrows: invalid 'MissingPlacement' value.");
          endif
          k += 2;
        elseif (ischar (a) && isrow (a) && strcmpi (a, 'ComparisonMethod'))
          k += 2;
        else
          pos{end+1} = a;
          k += 1;
        endif
      endwhile
      if (numel (pos) > 2)
        error ("datetime.sortrows: too many input arguments.");
      endif
      ## Resolve the column selection and the per-column sort direction.
      column = [];
      direction = [];
      if (numel (pos) >= 1)
        if (isnumeric (pos{1}))
          column = pos{1};
          if (numel (pos) == 2)
            direction = pos{2};
          endif
        else
          direction = pos{1};
          if (numel (pos) == 2)
            error ("datetime.sortrows: COLUMN must precede DIRECTION.");
          endif
        endif
      endif
      if (isempty (column))
        column = 1:ncol;
      endif
      column = column(:).';
      if (any (column == 0) || any (column != fix (column)) ...
          || any (abs (column) > ncol))
        error ("datetime.sortrows: COLUMN out of range.");
      endif
      colmag = abs (column);
      desc = column < 0;
      if (! isempty (direction))
        if (ischar (direction) && isrow (direction))
          if (strcmpi (direction, 'descend'))
            desc = true (size (colmag));
          elseif (strcmpi (direction, 'ascend'))
            desc = false (size (colmag));
          else
            error ("datetime.sortrows: invalid DIRECTION '%s'.", direction);
          endif
        elseif (iscellstr (direction))
          if (numel (direction) != numel (colmag))
            error (strcat ("datetime.sortrows: DIRECTION must have one", ...
                           " entry per sort column."));
          endif
          desc = false (size (colmag));
          for j = 1:numel (direction)
            if (strcmpi (direction{j}, 'descend'))
              desc(j) = true;
            elseif (! strcmpi (direction{j}, 'ascend'))
              error ("datetime.sortrows: invalid DIRECTION '%s'.", ...
                     direction{j});
            endif
          endfor
        else
          error ("datetime.sortrows: invalid DIRECTION argument.");
        endif
      endif
      ## Build the numeric key matrix from each selected column's instant.  NaT
      ## maps to a signed-Inf sentinel that places it per MissingPlacement in the
      ## column's own direction ('auto' treats NaT as the largest value).
      S = serial (A);
      N = size (S, 1);
      K = S(:, colmag);
      for j = 1:numel (colmag)
        if (strcmp (placement, 'auto'))
          sentinel = Inf;
        elseif (strcmp (placement, 'last'))
          if (desc(j))
            sentinel = -Inf;
          else
            sentinel = Inf;
          endif
        else
          if (desc(j))
            sentinel = Inf;
          else
            sentinel = -Inf;
          endif
        endif
        col = K(:, j);
        col(isnan (col)) = sentinel;
        K(:, j) = col;
      endfor
      ## Append the original row index as a final ascending tie-break so equal
      ## rows keep their order, then defer to the built-in lexicographic sort.
      nkey = numel (colmag);
      spec = (1:nkey) .* (1 - 2 * desc);
      [~, index] = sortrows ([K, (1:N)'], [spec, nkey + 1]);
      index = index(:);
      B = subset (A, index, ':');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{M} =} min (@var{A})
    ## @deftypefnx {datetime} {@var{M} =} min (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{M} =} min (@var{A}, [], @var{dim})
    ## @deftypefnx {datetime} {@var{M} =} min (@dots{}, @var{nanflag})
    ## @deftypefnx {datetime} {@var{M} =} min (@var{A}, [], @qcode{'all'})
    ## @deftypefnx {datetime} {[@var{M}, @var{I}] =} min (@dots{})
    ##
    ## Minimum of a datetime array.
    ##
    ## @code{@var{M} = min (@var{A})} returns the smallest element of the
    ## datetime array @var{A} along its first non-singleton dimension.  For a
    ## matrix, @var{M} is a row vector with the minimum of each column.
    ## Not-A-Time (@qcode{NaT}) elements are omitted; a reduction over
    ## @qcode{NaT} elements only yields @qcode{NaT}.
    ##
    ## @code{@var{M} = min (@var{A}, @var{B})} returns an array the same size as
    ## @var{A} and @var{B} (after broadcasting) holding the element-wise minimum
    ## of the two datetime arrays.
    ##
    ## @code{@var{M} = min (@var{A}, [], @var{dim})} operates along dimension
    ## @var{dim}.  The empty second argument distinguishes this from the
    ## element-wise form.
    ##
    ## @code{@var{M} = min (@dots{}, @var{nanflag})} sets the treatment of
    ## @qcode{NaT}: @qcode{'omitnan'} (default) ignores @qcode{NaT}, while
    ## @qcode{'includenan'} returns @qcode{NaT} whenever a @qcode{NaT} takes
    ## part in the comparison.
    ##
    ## @code{@var{M} = min (@var{A}, [], @qcode{'all'})} returns the smallest
    ## element of the whole array.
    ##
    ## @code{[@var{M}, @var{I}] = min (@dots{})} also returns the indices of the
    ## minima.  A second output is not available for the element-wise form.
    ##
    ## @end deftypefn
    function [M, I] = min (A, varargin)
      [M, I] = minmaxImpl (A, varargin, false, nargout);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{M} =} max (@var{A})
    ## @deftypefnx {datetime} {@var{M} =} max (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{M} =} max (@var{A}, [], @var{dim})
    ## @deftypefnx {datetime} {@var{M} =} max (@dots{}, @var{nanflag})
    ## @deftypefnx {datetime} {@var{M} =} max (@var{A}, [], @qcode{'all'})
    ## @deftypefnx {datetime} {[@var{M}, @var{I}] =} max (@dots{})
    ##
    ## Maximum of a datetime array.
    ##
    ## @code{@var{M} = max (@var{A})} returns the largest element of the datetime
    ## array @var{A} along its first non-singleton dimension.  For a matrix,
    ## @var{M} is a row vector with the maximum of each column.  Not-A-Time
    ## (@qcode{NaT}) elements are omitted; a reduction over @qcode{NaT} elements
    ## only yields @qcode{NaT}.
    ##
    ## @code{@var{M} = max (@var{A}, @var{B})} returns an array the same size as
    ## @var{A} and @var{B} (after broadcasting) holding the element-wise maximum
    ## of the two datetime arrays.
    ##
    ## @code{@var{M} = max (@var{A}, [], @var{dim})} operates along dimension
    ## @var{dim}.  The empty second argument distinguishes this from the
    ## element-wise form.
    ##
    ## @code{@var{M} = max (@dots{}, @var{nanflag})} sets the treatment of
    ## @qcode{NaT}: @qcode{'omitnan'} (default) ignores @qcode{NaT}, while
    ## @qcode{'includenan'} returns @qcode{NaT} whenever a @qcode{NaT} takes
    ## part in the comparison.
    ##
    ## @code{@var{M} = max (@var{A}, [], @qcode{'all'})} returns the largest
    ## element of the whole array.
    ##
    ## @code{[@var{M}, @var{I}] = max (@dots{})} also returns the indices of the
    ## maxima.  A second output is not available for the element-wise form.
    ##
    ## @end deftypefn
    function [M, I] = max (A, varargin)
      [M, I] = minmaxImpl (A, varargin, true, nargout);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} unique (@var{A})
    ## @deftypefnx {datetime} {@var{B} =} unique (@var{A}, @var{setOrder})
    ## @deftypefnx {datetime} {@var{B} =} unique (@var{A}, @var{occurrence})
    ## @deftypefnx {datetime} {@var{B} =} unique (@var{A}, @var{setOrder}, @var{occurrence})
    ## @deftypefnx {datetime} {@var{B} =} unique (@var{A}, @var{occurrence}, @var{setOrder})
    ## @deftypefnx {datetime} {@var{B} =} unique (@var{A}, @dots{}, @qcode{'rows'})
    ## @deftypefnx {datetime} {[@var{B}, @var{ixA}, @var{ixB}] =} unique (@dots{})
    ##
    ## Unique values in a datetime array.
    ##
    ## @code{@var{B} = unique (@var{A})} returns the unique values of the
    ## datetime array @var{A} in sorted order.
    ##
    ## @code{@var{B} = unique (@var{A}, @var{setOrder})} returns the unique
    ## values of the datetime array @var{A} in an order as specified by
    ## @var{setOrder}, which can be either of the following values:
    ##
    ## @itemize
    ## @item @qcode{'sorted'} (default) returns the unique values sorted in
    ## ascending order.
    ## @item @qcode{'stable'} returns the unique values according to their order
    ## of occurrence.
    ## @end itemize
    ##
    ## @code{@var{B} = unique (@var{A}, @var{occurrence})} returns the unique
    ## values of the datetime array @var{tblA} according to their order of
    ## occurrence.  @var{occurrence} can be either of the following values:
    ##
    ## @itemize
    ## @item @qcode{'first'} (default) returns the first occurrence of each
    ## unique value, i.e. the lowest possible indices are returned.
    ## @item @qcode{'last'} returns the last occurrence of each unique value,
    ## i.e. the highest possible indices are returned.
    ## @end itemize
    ##
    ## You can specify @var{setOrder} and @var{occurrence} arguments together.
    ##
    ## @code{@var{B} = unique (@var{A}, @dots{}, @qcode{'rows'})} returns the
    ## unique rows of @var{A} by treating each row as a single entity.  The
    ## @qcode{'rows'} option can be used alone or in any combination with the
    ## @var{setOrder} and @var{occurrence} arguments.  @qcode{'rows'} can be
    ## placed at any position in the function's argument list after the input
    ## array @var{A}.  However, this syntax is only valid for 2-dimensional
    ## datetime arrays.
    ##
    ## @code{[@var{tblB}, @var{ixA}, @var{ixB}] = unique (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} using any of the previous syntaxes.
    ## @var{ixA} and @var{ixB} map the arrays @var{A} and @var{B} to one another
    ## such that @qcode{@var{B} = @var{A}(@var{ixA})} and
    ## @qcode{@var{A} = @var{B}(@var{ixB})}.  When the @qcode{'rows'} optional
    ## argument is specified, then @qcode{@var{B} = @var{A}(@var{ixA},:)} and
    ## @qcode{@var{tblA} = @var{tblB}(@var{ixB},:)}.
    ##
    ## @end deftypefn
    function [B, ixA, ixB] = unique (A, varargin)
      ## 'legacy' option is not supported
      if (any (strcmp ("legacy", varargin)))
        error ("datetime.unique: 'legacy' option is not supported.");
      endif
      ## Handle each property array separately
      [~, ~, Yidx] =  __unique__ (A.Year, varargin{:});
      [~, ~, MOidx] = __unique__ (A.Month, varargin{:});
      [~, ~, Didx] =  __unique__ (A.Day, varargin{:});
      [~, ~, Hidx] =  __unique__ (A.Hour, varargin{:});
      [~, ~, MIidx] = __unique__ (A.Minute, varargin{:});
      [~, ~, Sidx] =  __unique__ (A.Second, varargin{:});
      DT = [Yidx, MOidx, Didx, Hidx, MIidx, Sidx];
      ## Use indices to find unique datetime values
      if (any (strcmp ('rows', varargin)))
        [~, ixA, ixB] = __unique__ (DT, varargin{:});
        if (any (strcmp ('last', varargin)))
          [~, ixA, ~] = __unique__ (ixB, 'last');
        endif
        B = subset (A, ixA, ':');
      else
        [~, ixA, ixB] = __unique__ (DT, 'rows', varargin{:});
        if (any (strcmp ('last', varargin)))
          [~, ixA, ~] = __unique__ (ixB, 'last');
        endif
        B = subset (A, ixA);
        ## Match MATLAB: a non-row input yields a column (so an empty 0-by-0
        ## input returns a 0-by-1 result rather than 0-by-0).
        if (isempty (B) && ! isrow (A))
          B = reshape (B, numel (B), 1);
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{C} =} intersect (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{C} =} intersect (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {datetime} {@var{C} =} intersect (@dots{}, @var{order})
    ## @deftypefnx {datetime} {[@var{C}, @var{ixA}, @var{ixB}] =} intersect (@dots{})
    ##
    ## Set intersection of two datetime arrays.
    ##
    ## @code{@var{C} = intersect (@var{A}, @var{B})} returns the unique datetime
    ## values common to both @var{A} and @var{B}.  Either input may instead be a
    ## date/time character vector, string array, or cell array of character
    ## vectors, which is promoted to a datetime array before the operation.
    ## Membership is decided on the absolute instant, so two zoned inputs may be
    ## in different time zones; the result carries the time zone and display
    ## format of @var{A}.  Not-A-Time (@qcode{NaT}) elements are treated like
    ## @qcode{NaN} and never match.  @var{C} is a row vector when both @var{A}
    ## and @var{B} are row vectors and a column vector otherwise.
    ##
    ## @code{@var{C} = intersect (@var{A}, @var{B}, @qcode{'rows'})} treats each
    ## row of the datetime matrices @var{A} and @var{B}, which must have the same
    ## number of columns, as a single element and returns their common rows.
    ##
    ## @code{@dots{} = intersect (@dots{}, @var{order})} returns the values in
    ## @qcode{'sorted'} order (the default) or in @qcode{'stable'} order, i.e.@:
    ## the order in which they appear in @var{A}.
    ##
    ## @code{[@var{C}, @var{ixA}, @var{ixB}] = intersect (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that @code{@var{C} =
    ## @var{A}(@var{ixA})} and @code{@var{C} = @var{B}(@var{ixB})}, or the
    ## corresponding row selections when @qcode{'rows'} is used.
    ##
    ## @end deftypefn
    function [C, ixA, ixB] = intersect (A, B, varargin)
      if (any (strcmpi ('legacy', varargin)))
        error ("datetime.intersect: 'legacy' option is not supported.");
      endif
      A = dtSetPromote (A, B, 'intersect');
      B = dtSetPromote (B, A, 'intersect');
      [A, B] = prepSetOp (A, B, 'intersect');
      SA = serial (A);
      SB = serial (B);
      if (any (strcmpi ('rows', varargin)))
        [~, ixA, ixB] = intersect (SA, SB, varargin{:});
        C = subset (A, ixA, ':');
      else
        [~, ixA, ixB] = intersect (SA(:), SB(:), varargin{:});
        C = subset (A, ixA);
        C = reshapeSetResult (C, isrow (A) && isrow (B));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{C} =} union (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{C} =} union (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {datetime} {@var{C} =} union (@dots{}, @var{order})
    ## @deftypefnx {datetime} {[@var{C}, @var{ixA}, @var{ixB}] =} union (@dots{})
    ##
    ## Set union of two datetime arrays.
    ##
    ## @code{@var{C} = union (@var{A}, @var{B})} returns the unique datetime
    ## values present in either @var{A} or @var{B}.  Either input may instead be
    ## a date/time character vector, string array, or cell array of character
    ## vectors, which is promoted to a datetime array before the operation.
    ## Membership is decided on the absolute instant, so two zoned inputs may be
    ## in different time zones; the result carries the time zone and display
    ## format of @var{A}.  Distinct @qcode{NaT} elements are all retained.
    ## @var{C} is a row vector when both @var{A} and @var{B} are row vectors and
    ## a column vector otherwise.
    ##
    ## @code{@var{C} = union (@var{A}, @var{B}, @qcode{'rows'})} treats each row
    ## of the datetime matrices @var{A} and @var{B}, which must have the same
    ## number of columns, as a single element and returns their combined unique
    ## rows.
    ##
    ## @code{@dots{} = union (@dots{}, @var{order})} returns the values in
    ## @qcode{'sorted'} order (the default) or in @qcode{'stable'} order.
    ##
    ## @code{[@var{C}, @var{ixA}, @var{ixB}] = union (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that @var{C} is the combination
    ## of @code{@var{A}(@var{ixA})} and @code{@var{B}(@var{ixB})}, or the
    ## corresponding row selections when @qcode{'rows'} is used.
    ##
    ## @end deftypefn
    function [C, ixA, ixB] = union (A, B, varargin)
      if (any (strcmpi ('legacy', varargin)))
        error ("datetime.union: 'legacy' option is not supported.");
      endif
      A = dtSetPromote (A, B, 'union');
      B = dtSetPromote (B, A, 'union');
      [A, B] = prepSetOp (A, B, 'union');
      SA = serial (A);
      SB = serial (B);
      stable = any (strcmpi ('stable', varargin));
      if (any (strcmpi ('rows', varargin)))
        [~, ixA, ixB] = union (SA, SB, varargin{:});
        C = combineSets (A, B, ixA, ixB, SA, SB, true, stable);
      else
        [~, ixA, ixB] = union (SA(:), SB(:), varargin{:});
        C = combineSets (A, B, ixA, ixB, SA, SB, false, stable);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{C} =} setdiff (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{C} =} setdiff (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {datetime} {@var{C} =} setdiff (@dots{}, @var{order})
    ## @deftypefnx {datetime} {[@var{C}, @var{ixA}] =} setdiff (@dots{})
    ##
    ## Set difference of two datetime arrays.
    ##
    ## @code{@var{C} = setdiff (@var{A}, @var{B})} returns the unique datetime
    ## values in @var{A} that are not in @var{B}.  Either input may instead be a
    ## date/time character vector, string array, or cell array of character
    ## vectors, which is promoted to a datetime array before the operation.
    ## Membership is decided on the absolute instant, so two zoned inputs may be
    ## in different time zones; the result carries the time zone and display
    ## format of @var{A}.  @qcode{NaT} elements in @var{A} are all retained, as
    ## they never match an element of @var{B}.  @var{C} is a row vector when both
    ## @var{A} and @var{B} are row vectors and a column vector otherwise.
    ##
    ## @code{@var{C} = setdiff (@var{A}, @var{B}, @qcode{'rows'})} treats each
    ## row of the datetime matrices @var{A} and @var{B}, which must have the same
    ## number of columns, as a single element and returns the rows of @var{A}
    ## that are not rows of @var{B}.
    ##
    ## @code{@dots{} = setdiff (@dots{}, @var{order})} returns the values in
    ## @qcode{'sorted'} order (the default) or in @qcode{'stable'} order.
    ##
    ## @code{[@var{C}, @var{ixA}] = setdiff (@dots{})} also returns an index
    ## vector @var{ixA} such that @code{@var{C} = @var{A}(@var{ixA})}, or
    ## @code{@var{C} = @var{A}(@var{ixA},:)} when @qcode{'rows'} is used.
    ##
    ## @end deftypefn
    function [C, ixA] = setdiff (A, B, varargin)
      if (any (strcmpi ('legacy', varargin)))
        error ("datetime.setdiff: 'legacy' option is not supported.");
      endif
      A = dtSetPromote (A, B, 'setdiff');
      B = dtSetPromote (B, A, 'setdiff');
      [A, B] = prepSetOp (A, B, 'setdiff');
      SA = serial (A);
      SB = serial (B);
      if (any (strcmpi ('rows', varargin)))
        [~, ixA] = setdiff (SA, SB, varargin{:});
        C = subset (A, ixA, ':');
      else
        [~, ixA] = setdiff (SA(:), SB(:), varargin{:});
        C = subset (A, ixA);
        C = reshapeSetResult (C, isrow (A) && isrow (B));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{C} =} setxor (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{C} =} setxor (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {datetime} {@var{C} =} setxor (@dots{}, @var{order})
    ## @deftypefnx {datetime} {[@var{C}, @var{ixA}, @var{ixB}] =} setxor (@dots{})
    ##
    ## Set exclusive-or of two datetime arrays.
    ##
    ## @code{@var{C} = setxor (@var{A}, @var{B})} returns the unique datetime
    ## values that are in @var{A} or in @var{B} but not in both.  Either input
    ## may instead be a date/time character vector, string array, or cell array
    ## of character vectors, which is promoted to a datetime array before the
    ## operation.  Membership is decided on the absolute instant, so two zoned
    ## inputs may be in different time zones; the result carries the time zone
    ## and display format of @var{A}.  Distinct @qcode{NaT} elements are all
    ## retained.  @var{C} is a row vector when both @var{A} and @var{B} are row
    ## vectors and a column vector otherwise.
    ##
    ## @code{@var{C} = setxor (@var{A}, @var{B}, @qcode{'rows'})} treats each row
    ## of the datetime matrices @var{A} and @var{B}, which must have the same
    ## number of columns, as a single element and returns the rows that are in
    ## one input but not both.
    ##
    ## @code{@dots{} = setxor (@dots{}, @var{order})} returns the values in
    ## @qcode{'sorted'} order (the default) or in @qcode{'stable'} order.
    ##
    ## @code{[@var{C}, @var{ixA}, @var{ixB}] = setxor (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that @var{C} is the combination
    ## of @code{@var{A}(@var{ixA})} and @code{@var{B}(@var{ixB})}, or the
    ## corresponding row selections when @qcode{'rows'} is used.
    ##
    ## @end deftypefn
    function [C, ixA, ixB] = setxor (A, B, varargin)
      if (any (strcmpi ('legacy', varargin)))
        error ("datetime.setxor: 'legacy' option is not supported.");
      endif
      A = dtSetPromote (A, B, 'setxor');
      B = dtSetPromote (B, A, 'setxor');
      [A, B] = prepSetOp (A, B, 'setxor');
      SA = serial (A);
      SB = serial (B);
      stable = any (strcmpi ('stable', varargin));
      if (any (strcmpi ('rows', varargin)))
        [~, ixA, ixB] = setxor (SA, SB, varargin{:});
        C = combineSets (A, B, ixA, ixB, SA, SB, true, stable);
      else
        [~, ixA, ixB] = setxor (SA(:), SB(:), varargin{:});
        C = combineSets (A, B, ixA, ixB, SA, SB, false, stable);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{TF} =} ismember (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{TF} =} ismember (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {datetime} {[@var{TF}, @var{index}] =} ismember (@dots{})
    ##
    ## Test for datetime elements in a set.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B})} returns a logical array
    ## @var{TF} of the same size as @var{A} containing @qcode{true} for each
    ## corresponding element of @var{A} that is in @var{B} and @qcode{false}
    ## otherwise.  Either input may instead be a date/time character vector,
    ## string array, or cell array of character vectors, which is promoted to a
    ## datetime array before the test.  Membership is decided on the absolute
    ## instant, so two zoned inputs may be in different time zones.  Similarly to
    ## @qcode{NaN} values, Not-A-Time (@qcode{NaT}) elements are not equal with
    ## each other and always return @qcode{false}.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B}, @qcode{'rows'})} only
    ## applies to datetime matrices with the same number of columns, in which
    ## case the logical vector @var{TF} contains @qcode{true} for each row of
    ## @var{A} that is also a row in @var{B}.  @var{TF} has the same number of
    ## rows as @var{A}.
    ##
    ## @code{[@var{TF}, @var{index}] = ismember (@var{A}, @var{B})} also returns
    ## an index array of the same size as @var{A} containing the lowest index in
    ## @var{B} for each element of @var{A} that is a member of @var{B} and 0
    ## otherwise.  If the @qcode{'rows'} optional argument is used, then the
    ## returning index is a column vector with the same rows as @var{A} and it
    ## contains the lowest index in @var{B} for each row of @var{A} that is a
    ## member of @var{B} and 0 otherwise.
    ##
    ## @end deftypefn
    function [TF, index] = ismember (A, B, varargin)
      do_rows = false;
      if (! isempty (varargin))
        if (strcmpi (varargin{1}, 'rows'))
          do_rows = true;
          if (ndims (A) != 2 || ndims (A) != ndims (B))
            error ("datetime.ismember: 'rows' applies only to 2-D matrices.");
          endif
          if (size (A, 2) != size (B, 2))
            error (strcat ("datetime.ismember: 'rows' requires same", ...
                           " number of columns."));
          endif
        else
          error ("datetime.ismember: invalid optional argument.");
        endif
      endif
      A = dtSetPromote (A, B, 'ismember');
      B = dtSetPromote (B, A, 'ismember');
      [A, B] = prepSetOp (A, B, 'ismember');
      SA = serial (A);
      SB = serial (B);
      if (do_rows)
        [TF, index] = __ismember__ (SA, SB, 'rows');
      else
        [TF, index] = __ismember__ (SA, SB);
      endif
    endfunction

  endmethods

  methods (Hidden)

    function BI = interp1 (A, B, AI, varargin)
      error ("datetime.interp1: not implemented yet.");
    endfunction

  endmethods

################################################################################
##                         ** Arithmetic Operations **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'plus'             'minus'            'colon'                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{R} =} colon (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{R} =} colon (@var{A}, @var{step}, @var{B})
    ##
    ## Create a range of datetime values.
    ##
    ## @code{@var{R} = colon (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{R} = @var{A}:@var{B}} and returns a row vector of datetime
    ## values starting at @var{A} and increasing in steps of one calendar day up
    ## to, and possibly including, @var{B}.
    ##
    ## @code{@var{R} = colon (@var{A}, @var{step}, @var{B})} is the equivalent of
    ## the syntax @code{@var{R} = @var{A}:@var{step}:@var{B}} and uses the
    ## specified @var{step} between consecutive elements.  @var{step} may be:
    ##
    ## @itemize
    ## @item a @code{duration} or a numeric scalar (a number of fixed 24-hour
    ## days), in which case successive elements advance by a fixed amount of
    ## elapsed time; for a zoned range this is aware of daylight saving time.
    ##
    ## @item a @code{calendarDuration}, in which case successive elements advance
    ## in calendar units.  Each element is computed as @code{@var{A} + k*@var{step}}
    ## for @code{k = 0, 1, 2, @dots{}}, so month and year steps clamp the day of
    ## month independently for every element (e.g.@: a one-month step from
    ## 31 January yields 31 January, 28 February, 31 March, @dots{}).
    ## @end itemize
    ##
    ## The default step of @code{@var{A}:@var{B}} is one calendar day
    ## (@code{caldays (1)}), which preserves the time of day across daylight
    ## saving time changes.  A range whose @var{step} points away from @var{B}
    ## (for example an increasing step with @code{@var{A} > @var{B}}) is empty.
    ## @var{A} and @var{B} must be datetime scalars that are either both zoned or
    ## both unzoned, and must be finite.
    ##
    ## @end deftypefn
    function R = colon (varargin)
      if (nargin == 2)
        A = varargin{1};
        step = caldays (1);
        B = varargin{2};
      elseif (nargin == 3)
        A = varargin{1};
        step = varargin{2};
        B = varargin{3};
      else
        print_usage ();
      endif
      if (! (isa (A, 'datetime') && isa (B, 'datetime')))
        error ("datetime.colon: range endpoints must be datetime arrays.");
      endif
      if (! (isscalar (A) && isscalar (B)))
        error ("datetime.colon: range endpoints must be datetime scalars.");
      endif
      if (xor (isempty (A.TimeZone), isempty (B.TimeZone)))
        error (strcat ("datetime.colon: cannot create a range between a", ...
                       " datetime with a time zone and one without a time", ...
                       " zone."));
      endif
      if (! (isfinite (A) && isfinite (B)))
        error (strcat ("datetime.colon: range endpoints must be finite", ...
                       " (neither NaT nor Inf)."));
      endif
      if (! isscalar (step))
        error ("datetime.colon: STEP must be a scalar.");
      endif
      if (isa (step, 'calendarDuration'))
        R = colonCalendar (A, step, B);
      elseif (isa (step, 'duration'))
        R = colonLinear (A, days (step) * 86400, B);
      elseif (isnumeric (step) && isreal (step))
        R = colonLinear (A, double (step) * 86400, B);
      else
        error (strcat ("datetime.colon: STEP must be a duration,", ...
                       " calendarDuration, or numeric scalar."));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{C} =} plus (@var{A}, @var{B})
    ##
    ## Addition for datetime arrays.
    ##
    ## @code{@var{C} = plus (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} + @var{B}} and supports the following operand
    ## combinations, in either order.
    ##
    ## @itemize
    ## @item @code{datetime + duration} returns a @code{datetime} array shifted
    ## later by a fixed number of 24-hour days.  For a zoned array the shift is
    ## applied to the absolute instant, so it is aware of daylight saving time
    ## transitions.
    ##
    ## @item @code{datetime + calendarDuration} returns a @code{datetime} array
    ## shifted later in calendar units.  Whole months (and years) are applied
    ## first, clamping the day of month to the last day of the target month when
    ## necessary (e.g.@: 31 January plus one month is 28 February), then whole
    ## calendar days, and finally the time-of-day component as an instant.
    ##
    ## @item @code{datetime + X}, where @var{X} is a numeric or logical array,
    ## treats the elements of @var{X} as a number of fixed 24-hour days.
    ## @end itemize
    ##
    ## Adding two datetime arrays is not defined and raises an error.  @var{A}
    ## and @var{B} must be size compatible: they can be the same size, one can be
    ## scalar, or for every dimension their sizes must be equal or one of them
    ## must be 1.  Not-A-Time and infinite elements propagate to the result.
    ##
    ## @end deftypefn
    function C = plus (A, B)

      ## Addition is symmetric for every supported operand type, so commute the
      ## operands to keep the datetime array on the left.
      if (! isa (A, 'datetime'))
        [A, B] = deal (B, A);
      endif

      if (isa (B, 'datetime'))
        ## datetime + datetime is not defined (MATLAB parity)
        error (strcat ("datetime.plus: addition is not defined between two", ...
                       " datetime arrays."));
      elseif (isa (B, 'duration'))
        ## datetime + duration -> datetime (fixed-length instant shift)
        C = addSeconds (A, days (B) * 86400);
      elseif (isa (B, 'calendarDuration'))
        ## datetime + calendarDuration -> datetime (calendar-aware shift)
        C = addCalendar (A, B, 1);
      elseif (islogical (B) || (isnumeric (B) && isfloat (B)))
        ## numeric/logical operand: a number of fixed 24-hour days
        C = addSeconds (A, double (B) * 86400);
      elseif (isinteger (B))
        error (strcat ("datetime.plus: cannot add a '%s' array to a datetime", ...
                       " array; convert it to double or a duration first."), ...
               class (B));
      else
        error (strcat ("datetime.plus: cannot add a '%s' array to a datetime", ...
                       " array."), class (B));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{C} =} minus (@var{A}, @var{B})
    ##
    ## Subtraction for datetime arrays.
    ##
    ## @code{@var{C} = minus (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} - @var{B}} and supports the following operand
    ## combinations.
    ##
    ## @itemize
    ## @item @code{datetime - datetime} returns a @code{duration} array holding
    ## the elapsed time between the corresponding elements.  Both operands must
    ## either both have a time zone or both be unzoned; a zoned difference is
    ## computed from the absolute instants, so the two time zones may differ.
    ##
    ## @item @code{datetime - duration} returns a @code{datetime} array shifted
    ## earlier by a fixed number of 24-hour days.  For a zoned array the shift
    ## is applied to the absolute instant, so it is aware of daylight saving
    ## time transitions.
    ##
    ## @item @code{datetime - calendarDuration} returns a @code{datetime} array
    ## shifted earlier in calendar units.  Whole months (and years) are applied
    ## first, clamping the day of month to the last day of the target month when
    ## necessary (e.g.@: 31 March minus one month is 28 February), then whole
    ## calendar days, and finally the time-of-day component as an instant.
    ##
    ## @item @code{datetime - X}, where @var{X} is a numeric or logical array,
    ## treats the elements of @var{X} as a number of fixed 24-hour days.
    ## @end itemize
    ##
    ## @var{A} and @var{B} must be size compatible: they can be the same size,
    ## one can be scalar, or for every dimension their sizes must be equal or one
    ## of them must be 1.  Not-A-Time and infinite elements propagate to the
    ## result.
    ##
    ## @end deftypefn
    function C = minus (A, B)

      ## Only 'datetime - <operand>' is defined; a datetime array cannot be
      ## subtracted from a non-datetime left operand (MATLAB parity).
      if (! isa (A, 'datetime'))
        error (strcat ("datetime.minus: cannot subtract a datetime array", ...
                       " from a '%s' array."), class (A));
      endif

      if (isa (B, 'datetime'))
        ## datetime - datetime -> duration (elapsed time)
        if (xor (isempty (A.TimeZone), isempty (B.TimeZone)))
          error (strcat ("datetime.minus: cannot subtract a datetime array", ...
                         " with a time zone from one without a time zone."));
        endif
        C = duration (0, 0, serial (A) - serial (B));
      elseif (isa (B, 'duration'))
        ## datetime - duration -> datetime (fixed-length instant shift)
        C = addSeconds (A, - days (B) * 86400);
      elseif (isa (B, 'calendarDuration'))
        ## datetime - calendarDuration -> datetime (calendar-aware shift)
        C = addCalendar (A, B, -1);
      elseif (islogical (B) || (isnumeric (B) && isfloat (B)))
        ## numeric/logical operand: a number of fixed 24-hour days
        C = addSeconds (A, - double (B) * 86400);
      elseif (isinteger (B))
        error (strcat ("datetime.minus: cannot subtract a '%s' array from a", ...
                       " datetime array; convert it to double or a duration", ...
                       " first."), class (B));
      else
        error (strcat ("datetime.minus: cannot subtract a '%s' array from a", ...
                       " datetime array."), class (B));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{D} =} diff (@var{A})
    ## @deftypefnx {datetime} {@var{D} =} diff (@var{A}, @var{N})
    ## @deftypefnx {datetime} {@var{D} =} diff (@var{A}, @var{N}, @var{dim})
    ##
    ## Differences between successive datetime elements.
    ##
    ## @code{@var{D} = diff (@var{A})} returns a @code{duration} array holding the
    ## elapsed time between successive elements of @var{A} along its first
    ## non-singleton dimension.  The differences are computed from the absolute
    ## instants, so for a zoned array they are aware of daylight saving time
    ## transitions (a calendar day spanning a transition is 23 or 25 hours, not
    ## 24).  Not-A-Time elements propagate as @qcode{NaN} durations.
    ##
    ## @code{@var{D} = diff (@var{A}, @var{N})} applies @code{diff}
    ## recursively @var{N} times, returning the @var{N}-th order difference.
    ## @var{N} must be a positive integer scalar.
    ##
    ## @code{@var{D} = diff (@var{A}, @var{N}, @var{dim})} operates along
    ## dimension @var{dim}.
    ##
    ## @end deftypefn
    function D = diff (A, varargin)
      n = 1;
      dim = [];
      if (numel (varargin) > 2)
        error ("datetime.diff: too many input arguments.");
      endif
      if (numel (varargin) >= 1)
        n = varargin{1};
        if (! (isnumeric (n) && isscalar (n) && n > 0 && n == fix (n)))
          error (strcat ("datetime.diff: order N must be a positive integer", ...
                         " scalar."));
        endif
      endif
      if (numel (varargin) >= 2)
        dim = varargin{2};
        if (! (isnumeric (dim) && isscalar (dim) && dim > 0 ...
               && dim == fix (dim)))
          error (strcat ("datetime.diff: DIM must be a positive integer", ...
                         " scalar."));
        endif
      endif
      S = serial (A);
      if (! isempty (dim) && dim > ndims (S))
        ## Differencing along a trailing singleton dimension leaves at most one
        ## element there, so the result is empty along DIM (MATLAB parity).
        sz = size (S);
        sz(end+1:dim) = 1;
        sz(dim) = 0;
        DS = zeros (sz);
      elseif (isempty (dim))
        DS = diff (S, n);
      else
        DS = diff (S, n, dim);
      endif
      D = duration (0, 0, DS);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{D} =} caldiff (@var{A})
    ## @deftypefnx {datetime} {@var{D} =} caldiff (@var{A}, @var{components})
    ## @deftypefnx {datetime} {@var{D} =} caldiff (@var{A}, @var{components}, @var{dim})
    ##
    ## Calendar differences between successive datetime elements.
    ##
    ## @code{@var{D} = caldiff (@var{A})} returns a @code{calendarDuration} array
    ## holding the calendar difference between successive elements of @var{A}
    ## along its first non-singleton dimension.  Unlike @code{diff}, the result
    ## is expressed in whole calendar units (years, months, days) plus a leftover
    ## time, so it is aware of month lengths and, for a zoned array, of daylight
    ## saving time.  Not-A-Time elements yield @qcode{NaN}.
    ##
    ## @code{@var{D} = caldiff (@var{A}, @var{components})} expresses each
    ## difference using only the requested calendar components.
    ## @var{components} is one of @qcode{'Years'}, @qcode{'Quarters'},
    ## @qcode{'Months'}, @qcode{'Weeks'}, @qcode{'Days'}, or @qcode{'Time'}, or a
    ## cell array or string array containing several of them.  The default is
    ## @qcode{@{'Years', 'Months', 'Days', 'Time'@}}.
    ##
    ## @code{@var{D} = caldiff (@var{A}, @var{components}, @var{dim})} operates
    ## along dimension @var{dim}.
    ##
    ## @end deftypefn
    function D = caldiff (A, varargin)
      comps = [];
      dim = [];
      for k = 1:numel (varargin)
        x = varargin{k};
        if (ischar (x) || iscellstr (x) || isa (x, 'string'))
          comps = x;
        elseif (isnumeric (x) && isscalar (x))
          dim = x;
        else
          error ("datetime.caldiff: invalid input argument.");
        endif
      endfor
      if (isempty (dim))
        dim = find (size (A) != 1, 1);
        if (isempty (dim))
          dim = 1;
        endif
      endif
      n = size (A, dim);
      idx = repmat ({':'}, 1, max (ndims (A), dim));
      i1 = idx;  i1{dim} = 1:n-1;
      i2 = idx;  i2{dim} = 2:n;
      D = calDiff (subset (A, i1{:}), subset (A, i2{:}), comps, 'caldiff');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{D} =} between (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{D} =} between (@var{A}, @var{B}, @var{components})
    ##
    ## Calendar difference between two datetime arrays.
    ##
    ## @code{@var{D} = between (@var{A}, @var{B})} returns a
    ## @code{calendarDuration} array holding the calendar difference from each
    ## element of @var{A} to the corresponding element of @var{B}.  The result is
    ## signed (it is negative where @var{B} precedes @var{A}) and is expressed in
    ## whole calendar units plus a leftover time, taking month lengths and
    ## daylight saving time into account.  @var{A} and @var{B} must be the same
    ## size or one of them must be scalar.  Not-A-Time elements yield @qcode{NaN}.
    ##
    ## @code{@var{D} = between (@var{A}, @var{B}, @var{components})} expresses
    ## each difference using only the requested calendar components (see
    ## @code{caldiff}).  The default is @qcode{@{'Years', 'Months', 'Days',
    ## 'Time'@}}.
    ##
    ## @end deftypefn
    function D = between (A, B, varargin)
      if (numel (varargin) > 1)
        error ("datetime.between: too many input arguments.");
      endif
      comps = [];
      if (numel (varargin) == 1)
        comps = varargin{1};
      endif
      A = dtSetPromote (A, B, 'between');
      B = dtSetPromote (B, A, 'between');
      [A, B] = prepSetOp (A, B, 'between');
      [A, B] = broadcastPair (A, B, 'between');
      D = calDiff (A, B, comps, 'between');
    endfunction

  endmethods

################################################################################
##                        ** Relational Operations **                         ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'lt'               'le'               'gt'               'ge'              ##
## 'eq'               'ne'                                                    ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} lt (@var{A}, @var{B})
    ##
    ## Less-than comparison for datetime arrays.
    ##
    ## @code{@var{TF} = lt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} < @var{B}} and returns a logical array set to
    ## @qcode{true} where the corresponding element of @var{A} is an earlier
    ## point in time than that of @var{B}, and @qcode{false} otherwise.
    ##
    ## Both operands must be datetime arrays and either both have a time zone or
    ## both be unzoned; zoned arrays are compared by their absolute instants, so
    ## the two time zones may differ.  @var{A} and @var{B} must be size
    ## compatible.  Not-A-Time compares as @qcode{false} against anything, just
    ## like @code{NaN}.
    ##
    ## @end deftypefn
    function TF = lt (A, B)
      TF = relcompare (A, B, 'lt');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} le (@var{A}, @var{B})
    ##
    ## Less-than-or-equal comparison for datetime arrays.
    ##
    ## @code{@var{TF} = le (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} <= @var{B}} and returns a logical array set to
    ## @qcode{true} where the corresponding element of @var{A} is an earlier or
    ## equal point in time to that of @var{B}, and @qcode{false} otherwise.
    ##
    ## Both operands must be datetime arrays and either both have a time zone or
    ## both be unzoned; zoned arrays are compared by their absolute instants, so
    ## the two time zones may differ.  @var{A} and @var{B} must be size
    ## compatible.  Not-A-Time compares as @qcode{false} against anything, just
    ## like @code{NaN}.
    ##
    ## @end deftypefn
    function TF = le (A, B)
      TF = relcompare (A, B, 'le');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} gt (@var{A}, @var{B})
    ##
    ## Greater-than comparison for datetime arrays.
    ##
    ## @code{@var{TF} = gt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} > @var{B}} and returns a logical array set to
    ## @qcode{true} where the corresponding element of @var{A} is a later point
    ## in time than that of @var{B}, and @qcode{false} otherwise.
    ##
    ## Both operands must be datetime arrays and either both have a time zone or
    ## both be unzoned; zoned arrays are compared by their absolute instants, so
    ## the two time zones may differ.  @var{A} and @var{B} must be size
    ## compatible.  Not-A-Time compares as @qcode{false} against anything, just
    ## like @code{NaN}.
    ##
    ## @end deftypefn
    function TF = gt (A, B)
      TF = relcompare (A, B, 'gt');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} ge (@var{A}, @var{B})
    ##
    ## Greater-than-or-equal comparison for datetime arrays.
    ##
    ## @code{@var{TF} = ge (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} >= @var{B}} and returns a logical array set to
    ## @qcode{true} where the corresponding element of @var{A} is a later or
    ## equal point in time to that of @var{B}, and @qcode{false} otherwise.
    ##
    ## Both operands must be datetime arrays and either both have a time zone or
    ## both be unzoned; zoned arrays are compared by their absolute instants, so
    ## the two time zones may differ.  @var{A} and @var{B} must be size
    ## compatible.  Not-A-Time compares as @qcode{false} against anything, just
    ## like @code{NaN}.
    ##
    ## @end deftypefn
    function TF = ge (A, B)
      TF = relcompare (A, B, 'ge');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Equality comparison for datetime arrays.
    ##
    ## @code{@var{TF} = eq (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} == @var{B}} and returns a logical array set to
    ## @qcode{true} where the corresponding elements of @var{A} and @var{B} are
    ## the same point in time, and @qcode{false} otherwise.
    ##
    ## Both operands must be datetime arrays and either both have a time zone or
    ## both be unzoned; zoned arrays are compared by their absolute instants, so
    ## the two time zones may differ.  @var{A} and @var{B} must be size
    ## compatible.  Not-A-Time is never equal to anything, including another
    ## Not-A-Time, just like @code{NaN}.
    ##
    ## @end deftypefn
    function TF = eq (A, B)
      TF = relcompare (A, B, 'eq');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} ne (@var{A}, @var{B})
    ##
    ## Inequality comparison for datetime arrays.
    ##
    ## @code{@var{TF} = ne (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} != @var{B}} and returns a logical array set to
    ## @qcode{true} where the corresponding elements of @var{A} and @var{B} are
    ## not the same point in time, and @qcode{false} otherwise.
    ##
    ## Both operands must be datetime arrays and either both have a time zone or
    ## both be unzoned; zoned arrays are compared by their absolute instants, so
    ## the two time zones may differ.  @var{A} and @var{B} must be size
    ## compatible.  Not-A-Time is never equal to anything, so it compares as
    ## @qcode{true} against everything, including another Not-A-Time, just like
    ## @code{NaN}.
    ##
    ## @end deftypefn
    function TF = ne (A, B)
      TF = relcompare (A, B, 'ne');
    endfunction

  endmethods

################################################################################
##                           ** Array Operations **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'cat'              'horzcat'          'vertcat'          'repmat'          ##
## 'repelem'          'repelems'         'reshape'          'circshift'       ##
## 'permute'          'ipermute'         'transpose'        'ctranspose'      ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{C} =} cat (@var{dim}, @var{A}, @var{B}, @dots{})
    ##
    ## Concatenate datetime arrays.
    ##
    ## @code{@var{C} = cat (@var{dim}, @var{A}, @var{B}, @dots{})} concatenates
    ## datetime arrays @var{A}, @var{B}, @dots{} along dimension @var{dim}.  All
    ## input arrays must be datetime arrays and have the same size except along
    ## the operating dimension @var{dim}.
    ##
    ## @end deftypefn
    function out = cat (dim, varargin)
      args = varargin;
      f = @(x) isa (x, 'datetime');
      if (! all (cellfun (f, args)))
        error ("datetime: invalid input to constructor.");
      endif
      out = args{1};
      fieldArgs  = cellfun (@(obj) obj.Year, args, 'UniformOutput', false);
      out.Year   = cat (dim, fieldArgs{:});
      fieldArgs  = cellfun (@(obj) obj.Month, args, 'UniformOutput', false);
      out.Month  = cat (dim, fieldArgs{:});
      fieldArgs  = cellfun (@(obj) obj.Day, args, 'UniformOutput', false);
      out.Day    = cat (dim, fieldArgs{:});
      fieldArgs  = cellfun (@(obj) obj.Hour, args, 'UniformOutput', false);
      out.Hour   = cat (dim, fieldArgs{:});
      fieldArgs  = cellfun (@(obj) obj.Minute, args, 'UniformOutput', false);
      out.Minute = cat (dim, fieldArgs{:});
      fieldArgs  = cellfun (@(obj) obj.Second, args, 'UniformOutput', false);
      out.Second = cat (dim, fieldArgs{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{C} =} horzcat (@var{A}, @var{B}, @dots{})
    ##
    ## Horizontal concatenation of datetime arrays.
    ##
    ## @code{@var{C} = horzcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}, @var{B}, @dots{}]} and horizontally
    ## concatenates the datetime arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must be datetime arrays and have the same size except along the
    ## second dimension.
    ##
    ## @end deftypefn
    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{C} =} vertcat (@var{A}, @var{B}, @dots{})
    ##
    ## Vertical concatenation of datetime arrays.
    ##
    ## @code{@var{C} = vertcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}; @var{B}; @dots{}]} and vertically
    ## concatenates the datetime arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must be datetime arrays and have the same size except along the
    ## second dimension.
    ##
    ## @end deftypefn
    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} repmat (@var{A}, @var{n})
    ## @deftypefnx {datetime} {@var{B} =} repmat (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {datetime} {@var{B} =} repmat (@var{A}, @var{dimvec})
    ##
    ## Repeat copies of a datetime array.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{n})} returns a datetime array
    ## @var{B} containing @var{n} copies of the input datetime array @var{A}
    ## along every dimension of @var{A}.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{d1}, @dots{}, @var{dN})} returns an
    ## array @var{B} containing copies of @var{A} along the dimensions specified
    ## by the list of scalar integer values @var{d1}, @dots{}, @var{dN}, which
    ## specify how many copies of @var{A} are made in each dimension.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{dimvec})} is equivalent to the
    ## previous syntax with @code{@var{dimvec} = [@var{d1}, @dots{}, @var{dN}]}.
    ##
    ## @end deftypefn
    function this = repmat (this, varargin)
      this.Year   = repmat (this.Year, varargin{:});
      this.Month  = repmat (this.Month, varargin{:});
      this.Day    = repmat (this.Day, varargin{:});
      this.Hour   = repmat (this.Hour, varargin{:});
      this.Minute = repmat (this.Minute, varargin{:});
      this.Second = repmat (this.Second, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} repelem (@var{A}, @var{n})
    ## @deftypefnx {datetime} {@var{B} =} repelem (@var{A}, @var{d1}, @dots{}, @var{dN})
    ##
    ## Repeat copies of datetime array elements.
    ##
    ## @code{@var{B} = repelem (@var{A}, @var{n})} returns a datetime vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a datetime vector.  If @var{n} is a scalar, each element of @var{A} is
    ## repeated @var{n} times along the non-singleton dimension of @var{A}.  If
    ## @var{n} is a vector, it must have the same elements as @var{A}, in which
    ## case it specifies the number of times to repeat each corresponding
    ## element of @var{A}.
    ##
    ## @code{@var{B} = repelem (@var{A}, @var{d1}, @dots{}, @var{dN})} returns
    ## an array @var{B} with each element of @var{A} repeated according to the
    ## the list of input arguments @code{@var{d1}, @dots{}, @var{dN}} each
    ## corresponding to a different dimension @code{1:ndims (@var{A})} of the
    ## input array @var{A}.  @var{d1}, @dots{}, @var{dN} must be either scalars
    ## or vectors with the same length as the corresponding dimension of
    ## @var{A} containing non-negative integer values specifying the number of
    ## repetitions of each element along the corresponding dimension.
    ##
    ## @end deftypefn
    function this = repelem (this, varargin)
      this.Year   = repelem (this.Year, varargin{:});
      this.Month  = repelem (this.Month, varargin{:});
      this.Day    = repelem (this.Day, varargin{:});
      this.Hour   = repelem (this.Hour, varargin{:});
      this.Minute = repelem (this.Minute, varargin{:});
      this.Second = repelem (this.Second, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{B} =} repelems (@var{A}, @var{R})
    ##
    ## Construct a vector of repeated elements from a datetime array.
    ##
    ## @code{@var{B} = repelems (@var{A}, @var{R})} returns a datetime vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a datetime vector.  @var{R} must be a @math{2*N} matrix of integers.
    ## Entries in the first row of @var{R} correspond to the linear indexing of
    ## the elements in @var{A} to be repeated.  The corresponding entries in the
    ## second row of @var{R} specify the repeat count of each element.
    ##
    ## @end deftypefn
    function this = repelems (this, R)
      this.Year   = repelems (this.Year, R);
      this.Month  = repelems (this.Month, R);
      this.Day    = repelems (this.Day, R);
      this.Hour   = repelems (this.Hour, R);
      this.Minute = repelems (this.Minute, R);
      this.Second = repelems (this.Second, R);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} reshape (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {datetime} {@var{B} =} reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})
    ## @deftypefnx {datetime} {@var{B} =} reshape (@var{A}, @var{dimvec})
    ##
    ## Reshape datetime array.
    ##
    ## @code{@var{B} = reshape (@var{A}, @var{d1}, @dots{}, @var{dN})} returns
    ## a datetime array @var{B} with specified dimensions @var{d1}, @dots{},
    ## @var{dN}, whose elements are taken columnwise from the datetime array
    ## @var{A}.  The product of @var{d1}, @dots{}, @var{dN} must equal the total
    ## number of elements in @var{A}.
    ##
    ## @code{@var{B} = reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})} returns
    ## a datetime array @var{B} with one dimension unspecified which is
    ## calculated automatically so that the product of dimensions in @var{B}
    ## matches the total elements in @var{A}, which must be divisible the
    ## product of specified dimensions.  An empty matrix @qcode{([])} is used to
    ## flag the unspecified dimension.
    ##
    ## @end deftypefn
    function this = reshape (this, varargin)
      this.Year   = reshape (this.Year, varargin{:});
      this.Month  = reshape (this.Month, varargin{:});
      this.Day    = reshape (this.Day, varargin{:});
      this.Hour   = reshape (this.Hour, varargin{:});
      this.Minute = reshape (this.Minute, varargin{:});
      this.Second = reshape (this.Second, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} circshift (@var{A}, @var{n})
    ## @deftypefnx {datetime} {@var{B} =} circshift (@var{A}, @var{n}, @var{dim})
    ##
    ## Circularly shift the elements in a datetime array.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n})} circularly shifts the
    ## elements of the datetime array @var{A} according to @var{n}.  If @var{n}
    ## is a nonzero integer scalar, then the elements of @var{A} are shifted by
    ## @var{n} elements along the first non-singleton dimension of @var{A}.  If
    ## @var{n} is a vector, it must not be longer that the number of dimensions
    ## of @var{A} with each value of @var{n} corresponding to a dimension in
    ## @var{A}.   The sign of the value(s) in @var{n} specify the direction in
    ## the elements of @var{A} are shifted.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n}, @var{dim})} circularly
    ## shifts the elements of the datetime array @var{A} along the dimension
    ## specified by @var{dim}.  In this case, @var{n} must be a scalar integer
    ## value.
    ##
    ## @end deftypefn
    function this = circshift (this, varargin)
      this.Year   = circshift (this.Year, varargin{:});
      this.Month  = circshift (this.Month, varargin{:});
      this.Day    = circshift (this.Day, varargin{:});
      this.Hour   = circshift (this.Hour, varargin{:});
      this.Minute = circshift (this.Minute, varargin{:});
      this.Second = circshift (this.Second, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{B} =} permute (@var{A}, @var{dims})
    ##
    ## Generalized transpose for a datetime N-D array.
    ##
    ## @code{@var{B} = permute (@var{A}, @var{dims})} returns the generalized
    ## transpose of the datetime array @var{A} by rearranging its dimensions
    ## according to the permutation vector specified in @var{dims}.
    ##
    ## @var{dims} must index all the dimensions @code{1:ndims (@var{A})} of the
    ## input array @var{A}, in any order, but only once.  The @var{N}th
    ## dimension of @var{A} gets remapped to the dimension in @var{B} specified
    ## by @code{@var{dims}(@var{N})}.
    ##
    ## @end deftypefn
    function this = permute (this, order)
      this.Year   = permute (this.Year, order);
      this.Month  = permute (this.Month, order);
      this.Day    = permute (this.Day, order);
      this.Hour   = permute (this.Hour, order);
      this.Minute = permute (this.Minute, order);
      this.Second = permute (this.Second, order);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{A} =} ipermute (@var{B}, @var{dims})
    ##
    ## Inverse of the generalized transpose for a datetime N-D array.
    ##
    ## @code{@var{A} = ipermute (@var{B}, @var{dims})} returns the inverse of
    ## the generalized transpose performed by the @code{permute} function.  The
    ## expression @code{ipermute (permute (@var{A}, @var{dims}), @var{dims})}
    ## returns the original array @var{A}.
    ##
    ## @var{dims} must index all the dimensions @code{1:ndims (@var{B})} of the
    ## input array @var{B}, in any order, but only once.  The dimension of
    ## @var{B} specified in @code{@var{dims}(@var{N})} gets remapped to the
    ## @var{N}th dimension of @var{A}.
    ##
    ## @end deftypefn
    function this = ipermute (this, order)
      this.Year   = ipermute (this.Year, order);
      this.Month  = ipermute (this.Month, order);
      this.Day    = ipermute (this.Day, order);
      this.Hour   = ipermute (this.Hour, order);
      this.Minute = ipermute (this.Minute, order);
      this.Second = ipermute (this.Second, order);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{B} =} transpose (@var{A})
    ##
    ## Transpose a datetime matrix.
    ##
    ## @code{@var{B} = transpose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}.'} and returns the transpose of the datetime
    ## matrix @var{A}.
    ##
    ## @end deftypefn
    function this = transpose (this)
      this.Year   = transpose (this.Year);
      this.Month  = transpose (this.Month);
      this.Day    = transpose (this.Day);
      this.Hour   = transpose (this.Hour);
      this.Minute = transpose (this.Minute);
      this.Second = transpose (this.Second);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{B} =} ctranspose (@var{A})
    ##
    ## Transpose a datetime matrix.
    ##
    ## @code{@var{B} = ctranspose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}'} and returns the transpose of the datetime
    ## matrix @var{A}.  For datetime arrays, @code{ctranspose} is identical to
    ## @code{transpose}.
    ##
    ## @end deftypefn
    function this = ctranspose (this)
      this.Year   = ctranspose (this.Year);
      this.Month  = ctranspose (this.Month);
      this.Day    = ctranspose (this.Day);
      this.Hour   = ctranspose (this.Hour);
      this.Minute = ctranspose (this.Minute);
      this.Second = ctranspose (this.Second);
    endfunction

  endmethods

################################################################################
##                  ** Reference and Assignment Operations **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'end'              'subsref'          'subsasgn'                           ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Overloaded end keyword
    function last_index = end (this, end_dim, ndim_obj)
      lastdim = ndims (this);
      if (end_dim == ndim_obj && ndim_obj == 1)
        last_index = prod (size (this));
      elseif (end_dim == ndim_obj && end_dim < lastdim)
        last_index = prod (size (this)(end_dim:lastdim));
      else
        last_index = size (this, end_dim);
      endif
    endfunction

    ## Class specific subscripted reference
    function varargout = subsref (this, s)

      chain_s = s(2:end);
      s = s(1);
      switch (s.type)
        case '()'
          out = this;
          out.Year   = this.Year(s.subs{:});
          out.Month  = this.Month(s.subs{:});
          out.Day    = this.Day(s.subs{:});
          out.Hour   = this.Hour(s.subs{:});
          out.Minute = this.Minute(s.subs{:});
          out.Second = this.Second(s.subs{:});

        case '{}'
          error (["datetime.subsref: '{}' invalid indexing", ...
                  " for referencing values. Use '()' instead."]);

        case '.'
          if (! ischar (s.subs))
            error (["datetime.subsref: '.' index argument", ...
                    " must be a character vector."]);
          endif
          switch (s.subs)
            case 'proxyArray'  # used by 'table' class
              out = proxyArray (this);
            case 'Format'
              out = this.Format;
            case 'SystemTimeZone'
              out = this.SystemTimeZone;
            case 'TimeZone'
              out = this.TimeZone;
            case {'Year'}
              out = this.Year;
            case {'Month'}
              out = this.Month;
            case {'Day'}
              out = this.Day;
            case {'Hour'}
              out = this.Hour;
            case {'Minute'}
              out = this.Minute;
            case {'Second'}
              out = this.Second;
            otherwise
              error ("datetime.subsref: unrecognized property: %s", s.subs);
          endswitch
      endswitch

      ## Chained references
      if (! isempty (chain_s))
        out = subsref (out, chain_s);
      endif
      varargout{1} = out;

    endfunction

    ## Class specific subscripted assignment
    function this = subsasgn (this, s, val)

      if (numel (s) > 1)
      #  error ("duration.subsasgn: chained subscripts not allowed.");
        p = s(2);
        s = s(1);
        if (! strcmp (p.type, '()'))
          error (["datetime.subsasgn: '%s' invalid indexing", ...
                  " for assigning values. Use '()' instead."], p.type);
        endif
      else
        p.subs = {':'};
      endif
      switch s.type
        case '()'
          if (isempty (val))
            this.Year(s.subs{:})   = [];
            this.Month(s.subs{:})  = [];
            this.Day(s.subs{:})    = [];
            this.Hour(s.subs{:})   = [];
            this.Minute(s.subs{:}) = [];
            this.Second(s.subs{:}) = [];
            return;
          elseif (! isa (val, "datetime"))
            error (["datetime.subsasgn: cannot assign %s values", ...
                            "to a datetime array."], class (val));
          endif
          this.Year(s.subs{:})   = val.Year;
          this.Month(s.subs{:})  = val.Month;
          this.Day(s.subs{:})    = val.Day;
          this.Hour(s.subs{:})   = val.Hour;
          this.Minute(s.subs{:}) = val.Minute;
          this.Second(s.subs{:}) = val.Second;

        case '{}'
          error (["datetime.subsasgn: '{}' invalid indexing", ...
                  " for assigning values. Use '()' instead."]);

        case '.'
          if (! ischar (s.subs))
            error (["datetime.subsasgn: '.' index argument", ...
                    " must be a character vector."]);
          endif
          switch (s.subs)
            case 'Format'
              this.Format = val;
            case 'TimeZone'
              toTimeZone = val;
              if (! (ischar (toTimeZone) && (isrow (toTimeZone) ...
                                             || isempty (toTimeZone))))
                error (["datetime.subsasgn: 'TimeZone' must be a", ...
                        " character vector."]);
              endif
              ## Validate the target zone (empty means an unzoned array).
              if (! isempty (toTimeZone))
                [~,~,~,~,~,~,errmsg] = __datetime__ (0, 0, 0, ...
                                                     'TimeZone', toTimeZone);
                if (! isnumeric (errmsg))
                  error ("datetime.subsasgn: %s", errmsg);
                endif
              endif
              if (isempty (this.TimeZone) || isempty (toTimeZone))
                ## Attaching a zone to an unzoned array, or dropping the zone,
                ## reinterprets/keeps the wall-clock values without converting.
                this.TimeZone = toTimeZone;
              else
                ## Switching between two zones preserves the absolute instant,
                ## so the wall-clock values shift by the offset difference.
                [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
                 this.Second, errmsg] = __datetime__ (this.Year, this.Month, ...
                 this.Day, this.Hour, this.Minute, this.Second, ...
                 'TimeZone', this.TimeZone, 'toTimeZone', toTimeZone, ...
                 'Precision', 'microseconds');
                if (! isnumeric (errmsg))
                  error ("datetime.subsasgn: %s", errmsg);
                endif
                this.TimeZone = toTimeZone;
              endif
            case {'Year'}
              this.Year(p.subs{:})   = val;
              this = normalize (this);
            case {'Month'}
              this.Month(p.subs{:})  = val;
              this = normalize (this);
            case {'Day'}
              this.Day(p.subs{:})    = val;
              this = normalize (this);
            case {'Hour'}
              this.Hour(p.subs{:})   = val;
              this = normalize (this);
            case {'Minute'}
              this.Minute(p.subs{:}) = val;
              this = normalize (this);
            case {'Second'}
              this.Second(p.subs{:}) = val;
              this = normalize (this);
            otherwise
              error ("datetime.subsasgn: unrecognized property: %s", s.subs);
          endswitch
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Return true if the rows of the numeric matrix KD are in lexicographic
    ## non-decreasing order (used by 'issortedrows').  Equal entries (including
    ## the +Inf sentinel used for NaT, and genuine infinities) count as ties and
    ## are resolved by the next column.
    function tf = rowsNonDecreasing (this, KD)
      m = rows (KD);
      if (m < 2)
        tf = true;
        return;
      endif
      a = KD(1:end-1, :);
      b = KD(2:end, :);
      undecided = true (m - 1, 1);
      bad = false (m - 1, 1);
      for j = 1:columns (KD)
        lt = a(:, j) < b(:, j);
        gt = a(:, j) > b(:, j);
        bad = bad | (undecided & gt);
        undecided = undecided & ! (lt | gt);
      endfor
      tf = ! any (bad);
    endfunction

    ## Shared engine for 'min' and 'max'.  ARGS is the method's varargin, ISMAX
    ## selects max over min, and NOUT is the caller's nargout.  Handles both the
    ## reduction form (min (A), min (A, [], DIM), '-all', nan flags) and the
    ## two-array elementwise form (min (A, B)).
    function [M, I] = minmaxImpl (A, args, ismax, nout)
      if (ismax)
        fname = 'max';
      else
        fname = 'min';
      endif
      I = [];
      ## Two-array elementwise form is signalled by a non-'[]' second argument.
      elementwise = ! isempty (args) ...
                    && ! (isnumeric (args{1}) && isempty (args{1}));
      if (elementwise)
        if (nout > 1)
          error (strcat ("datetime.", fname, ": a second output is not", ...
                         " supported when comparing two arrays."));
        endif
        B = args{1};
        if (! isa (B, 'datetime'))
          error (strcat ("datetime.", fname, ": comparison of two arrays", ...
                         " requires both to be datetime."));
        endif
        if (xor (isempty (A.TimeZone), isempty (B.TimeZone)))
          error (strcat ("datetime.", fname, ": cannot compare a datetime", ...
                         " with a time zone to one without a time zone."));
        endif
        nanflag = 'omitnan';
        for k = 2:numel (args)
          x = args{k};
          if (ischar (x) && isrow (x) && strcmpi (x, 'omitnan'))
            nanflag = 'omitnan';
          elseif (ischar (x) && isrow (x) && strcmpi (x, 'includenan'))
            nanflag = 'includenan';
          else
            error (strcat ("datetime.", fname, ": invalid option in a", ...
                           " two-array comparison."));
          endif
        endfor
        ## Compare by absolute instant (zone-independent), but pick exact
        ## component values from whichever operand wins to avoid a lossy
        ## instant round-trip.
        SA = serial (A);
        SB = serial (B);
        common = size (SA + SB);
        SA = SA + zeros (common);
        SB = SB + zeros (common);
        if (ismax)
          takeA = (SA >= SB) | isnan (SB);
        else
          takeA = (SA <= SB) | isnan (SB);
        endif
        ## Express B's components in A's time zone so selected values are exact.
        if (isempty (A.TimeZone) || strcmp (A.TimeZone, B.TimeZone))
          YB = B.Year; MB = B.Month; DB = B.Day;
          hB = B.Hour; mB = B.Minute; sB = B.Second;
        else
          [YB, MB, DB, hB, mB, sB] = __datetime__ (B.Year, B.Month, B.Day, ...
              B.Hour, B.Minute, B.Second, 'TimeZone', B.TimeZone, ...
              'toTimeZone', A.TimeZone, 'Precision', 'microseconds');
        endif
        z = zeros (common);
        Y = A.Year + z; Mo = A.Month + z; D = A.Day + z;
        h = A.Hour + z; mi = A.Minute + z; s = A.Second + z;
        YB = YB + z; MB = MB + z; DB = DB + z;
        hB = hB + z; mB = mB + z; sB = sB + z;
        takeB = ! takeA;
        Y(takeB) = YB(takeB); Mo(takeB) = MB(takeB); D(takeB) = DB(takeB);
        h(takeB) = hB(takeB); mi(takeB) = mB(takeB); s(takeB) = sB(takeB);
        if (strcmp (nanflag, 'includenan'))
          nanpos = isnan (SA) | isnan (SB);
          Y(nanpos) = NaN; Mo(nanpos) = NaN; D(nanpos) = NaN;
          h(nanpos) = NaN; mi(nanpos) = NaN; s(nanpos) = NaN;
        endif
        M = A;
        M.Year = Y; M.Month = Mo; M.Day = D;
        M.Hour = h; M.Minute = mi; M.Second = s;
        return;
      endif
      ## Reduction form.  Skip the '[]' placeholder, then read DIM / 'all' /
      ## the NaN flag from the remaining arguments.
      rest = args;
      if (! isempty (rest))
        rest = rest(2:end);
      endif
      dim = [];
      allflag = false;
      nanflag = 'omitnan';
      for k = 1:numel (rest)
        x = rest{k};
        if (isnumeric (x))
          if (! isscalar (x) || x < 1 || x != fix (x))
            error (strcat ("datetime.", fname, ...
                           ": DIM must be a positive integer."));
          endif
          dim = x;
        elseif (ischar (x) && isrow (x))
          if (strcmpi (x, 'all'))
            allflag = true;
          elseif (strcmpi (x, 'omitnan'))
            nanflag = 'omitnan';
          elseif (strcmpi (x, 'includenan'))
            nanflag = 'includenan';
          else
            error (strcat ("datetime.", fname, ": invalid option '", x, "'."));
          endif
        else
          error (strcat ("datetime.", fname, ": invalid input argument."));
        endif
      endfor
      if (isempty (A))
        M = A;
        return;
      endif
      S = serial (A);
      if (allflag)
        Sv = S(:);
        if (ismax)
          [~, iv] = max (Sv, [], 1);
        else
          [~, iv] = min (Sv, [], 1);
        endif
        if (strcmp (nanflag, 'includenan') && any (isnan (Sv)))
          iv = find (isnan (Sv), 1);
        endif
        M = subset (A, iv);
        I = iv;
        return;
      endif
      if (isempty (dim))
        dim = find (size (A) != 1, 1);
        if (isempty (dim))
          dim = 1;
        endif
      endif
      if (dim > 2 || size (A, dim) < 2)
        M = A;
        I = ones (size (A));
        return;
      endif
      if (ismax)
        [~, I] = max (S, [], dim);
      else
        [~, I] = min (S, [], dim);
      endif
      if (strcmp (nanflag, 'includenan'))
        nanmask = any (isnan (S), dim);
        [~, firstnan] = max (isnan (S), [], dim);
        I(nanmask) = firstnan(nanmask);
      endif
      [nr, nc] = size (S);
      if (dim == 1)
        lin = I + (0:nc-1) * nr;
      else
        lin = (1:nr)' + (I - 1) * nr;
      endif
      M = subset (A, lin);
    endfunction

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.Year   = this.Year(varargin{:});
      this.Month  = this.Month(varargin{:});
      this.Day    = this.Day(varargin{:});
      this.Hour   = this.Hour(varargin{:});
      this.Minute = this.Minute(varargin{:});
      this.Second = this.Second(varargin{:});
    endfunction

    ## Numeric proxy used by 'table' and set operations for sorting, grouping,
    ## and set membership.  Each datetime element maps to its six canonical
    ## components [Year, Month, Day, Hour, Minute, Second]; for a datetime
    ## matrix, each column contributes a six-column block.  Not-A-Time (NaT)
    ## elements map to NaN across their components, just like the stored arrays.
    function out = proxyArray (this)
      [~, cols] = size (this.Year);
      if (cols > 1)
        out = [];
        for i = 1:cols
          SC = [this.Year(:,i), this.Month(:,i), this.Day(:,i), ...
                this.Hour(:,i), this.Minute(:,i), this.Second(:,i)];
          out = [out, SC];
        endfor
      else
        out = [this.Year(:), this.Month(:), this.Day(:), ...
               this.Hour(:), this.Minute(:), this.Second(:)];
      endif
    endfunction

    ## Re-canonicalise the component arrays after a direct component assignment
    ## (e.g. 'd.Month = 13' rolls the extra month into the year).  This routes
    ## the raw values back through the same C++ normaliser used by the
    ## constructor, at microsecond precision so no sub-second detail is lost.
    ## Not-A-Time and infinite elements are passed through unchanged.
    function this = normalize (this)
      if (isempty (this.Year))
        return;
      endif
      if (isempty (this.TimeZone))
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (this.Year, this.Month, this.Day, ...
         this.Hour, this.Minute, this.Second, 'Precision', 'microseconds');
      else
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (this.Year, this.Month, this.Day, ...
         this.Hour, this.Minute, this.Second, 'TimeZone', this.TimeZone, ...
         'toTimeZone', this.TimeZone, 'Precision', 'microseconds');
      endif
    endfunction

    ## Absolute instant of each element as POSIX seconds (double, microsecond
    ## precision).  Unzoned arrays are treated as UTC so the serial carries no
    ## system-zone daylight-saving offset; zoned arrays honour their zone (and
    ## DST).  Not-A-Time maps to NaN and infinite elements keep their sign.
    ## Used by the arithmetic and relational instant-based comparisons.
    function s = serial (this)
      if (isempty (this.TimeZone))
        tz = 'UTC';
      else
        tz = this.TimeZone;
      endif
      s = __datetime__ (this.Year, this.Month, this.Day, this.Hour, ...
                        this.Minute, this.Second, 'ConvertTo', 'posixtime', ...
                        'TimeZone', tz, 'Precision', 'microseconds');
    endfunction

    ## Enforce the zone-compatibility rule for a set operation and express B in
    ## A's time zone so membership is decided on a shared wall clock (the
    ## absolute instants are preserved by the conversion).  Both operands are
    ## already datetime arrays here; text/numeric promotion and rejection is
    ## done by the 'dtSetPromote' local function before this is called.
    function [A, B] = prepSetOp (A, B, op)
      if (xor (isempty (A.TimeZone), isempty (B.TimeZone)))
        error (strcat ("datetime.%s: cannot combine a datetime array with a", ...
                       " time zone with one without a time zone."), op);
      endif
      if (! isempty (A.TimeZone) && ! strcmp (A.TimeZone, B.TimeZone))
        [B.Year, B.Month, B.Day, B.Hour, B.Minute, B.Second] = __datetime__ ...
            (B.Year, B.Month, B.Day, B.Hour, B.Minute, B.Second, 'TimeZone', ...
             B.TimeZone, 'toTimeZone', A.TimeZone, 'Precision', 'microseconds');
        B.TimeZone = A.TimeZone;
      endif
    endfunction

    ## Broadcast two datetime arrays to a common size for an element-wise
    ## operation, erroring if their sizes are incompatible.
    function [A, B] = broadcastPair (A, B, op)
      try
        z = zeros (size (A.Year + B.Year));
      catch
        error (strcat ("datetime.%s: A and B must be of common size or", ...
                       " scalars."), op);
      end_try_catch
      A.Year = A.Year + z; A.Month = A.Month + z; A.Day = A.Day + z;
      A.Hour = A.Hour + z; A.Minute = A.Minute + z; A.Second = A.Second + z;
      B.Year = B.Year + z; B.Month = B.Month + z; B.Day = B.Day + z;
      B.Hour = B.Hour + z; B.Minute = B.Minute + z; B.Second = B.Second + z;
    endfunction

    ## Assemble the result of a two-source set operation ('union'/'setxor'),
    ## whose values are drawn from both A (at IXA) and B (at IXB).  The built-in
    ## returns those indices grouped A-then-B, which is exactly the 'stable'
    ## order; for the default 'sorted' order the gathered elements are reordered
    ## by their absolute instant (SA/SB), NaT sorting last.  DOROWS selects the
    ## row-wise variant.
    function C = combineSets (A, B, ixA, ixB, SA, SB, doRows, stable)
      if (doRows)
        C = vertcat (subset (A, ixA, ':'), subset (B, ixB, ':'));
        if (! stable)
          [~, perm] = sortrows ([SA(ixA,:); SB(ixB,:)]);
          C = subset (C, perm, ':');
        endif
      else
        Ca = subset (A, ixA);  Cb = subset (B, ixB);
        C = vertcat (reshape (Ca, numel (Ca), 1), reshape (Cb, numel (Cb), 1));
        if (! stable)
          sa = SA(ixA);  sb = SB(ixB);
          [~, perm] = sort ([sa(:); sb(:)]);
          C = subset (C, perm);
        endif
        C = reshapeSetResult (C, isrow (A) && isrow (B));
      endif
    endfunction

    ## Calendar-aware difference from A to B, element-wise on equal-sized inputs,
    ## expressed in the requested COMPS.  Whole units are taken greedily from the
    ## largest requested down to the smallest (years, quarters, months, then
    ## weeks, days), each taking the most that does not step past B; the day of
    ## month is clamped on month steps and a whole unit is only borrowed if the
    ## time of day allows it (31 Jan 10:00 to 28 Feb 08:00 is 27 days 22 hours,
    ## not one month).  When 'Time' is requested the leftover is its absolute
    ## instant difference, so it is daylight-saving aware.  Returns a
    ## calendarDuration; NaT operands yield NaN.
    function C = calDiff (A, B, comps, op)
      [f, fmt] = parseCalComponents (comps, op);
      Y1 = A.Year; M1 = A.Month; D1 = A.Day;
      h1 = A.Hour; mi1 = A.Minute; s1 = A.Second;
      Y2 = B.Year; M2 = B.Month; D2 = B.Day;
      h2 = B.Hour; mi2 = B.Minute; s2 = B.Second;
      sz = size (Y1);
      ## A Not-A-Time operand yields a NaN result.  Replace NaN components with a
      ## harmless placeholder for the calendar arithmetic, then restore NaN in
      ## the outputs, so month/day indexing never hits a NaN subscript.
      bad = isnan (Y1) | isnan (Y2);
      if (any (bad(:)))
        Y1(bad) = 2000; M1(bad) = 1; D1(bad) = 1;
        h1(bad) = 0; mi1(bad) = 0; s1(bad) = 0;
        Y2(bad) = 2000; M2(bad) = 1; D2(bad) = 1;
        h2(bad) = 0; mi2(bad) = 0; s2(bad) = 0;
      endif
      ## Direction: +1 where A < B (forward), -1 where A > B, 0 where equal.
      fwd = lexlt (Y1, M1, D1, h1, mi1, s1, Y2, M2, D2, h2, mi2, s2);
      bwd = lexlt (Y2, M2, D2, h2, mi2, s2, Y1, M1, D1, h1, mi1, s1);
      sgn = double (fwd) - double (bwd);
      Yc = Y1; Mc = M1; Dc = D1;    # current date; time stays A's throughout
      monthsOut = zeros (sz);
      daysOut = zeros (sz);
      munits = [];
      if (f.y)
        munits(end+1) = 12;
      endif
      if (f.q)
        munits(end+1) = 3;
      endif
      if (f.m)
        munits(end+1) = 1;
      endif
      for u = munits
        totalM = (Y2 - Yc) .* 12 + (M2 - Mc);
        kApprox = fix (totalM ./ u);
        [Yk, Mk, Dk] = dtAddMonths (Yc, Mc, Dc, kApprox .* u);
        candGT = lexlt (Y2, M2, D2, h2, mi2, s2, Yk, Mk, Dk, h1, mi1, s1);
        candLT = lexlt (Yk, Mk, Dk, h1, mi1, s1, Y2, M2, D2, h2, mi2, s2);
        over = (sgn > 0 & candGT) | (sgn < 0 & candLT);
        k = kApprox - sgn .* double (over);
        monthsOut = monthsOut + k .* u;
        [Yc, Mc, Dc] = dtAddMonths (Yc, Mc, Dc, k .* u);
      endfor
      dunits = [];
      if (f.w)
        dunits(end+1) = 7;
      endif
      if (f.d)
        dunits(end+1) = 1;
      endif
      if (! isempty (dunits))
        tLT = lexlt (0, 0, 0, h2, mi2, s2, 0, 0, 0, h1, mi1, s1);
        tGT = lexlt (0, 0, 0, h1, mi1, s1, 0, 0, 0, h2, mi2, s2);
        for u = dunits
          totalD = datenum (Y2, M2, D2) - datenum (Yc, Mc, Dc);
          wholeD = totalD - (sgn > 0 & tLT) + (sgn < 0 & tGT);
          k = fix (wholeD ./ u);
          daysOut = daysOut + k .* u;
          [Yc, Mc, Dc] = dtAddDays (Yc, Mc, Dc, k .* u);
        endfor
      endif
      if (f.t)
        cur = A;
        cur.Year = Yc; cur.Month = Mc; cur.Day = Dc;
        remSec = serial (B) - serial (cur);
      else
        remSec = zeros (sz);
      endif
      monthsOut(bad) = NaN;
      daysOut(bad) = NaN;
      remSec(bad) = NaN;
      Tdur = duration (0, 0, remSec);
      C = calendarDuration (zeros (sz), monthsOut, daysOut, Tdur, 'Format', fmt);
    endfunction

    ## Inverse of 'serial': map POSIX seconds back to the wall-clock components
    ## of this array's time zone.  For a zoned array the serial is first read as
    ## a UTC wall clock and then converted into the target zone (honouring DST).
    function [Y, M, D, h, m, s] = serial2components (this, ser)
      if (isempty (this.TimeZone))
        [Y, M, D, h, m, s] = __datetime__ (ser, 'ConvertFrom', 'posixtime', ...
                                           'Precision', 'microseconds');
      else
        [Y, M, D, h, m, s] = __datetime__ (ser, 'ConvertFrom', 'posixtime', ...
                                           'Precision', 'microseconds');
        [Y, M, D, h, m, s] = __datetime__ (Y, M, D, h, m, s, ...
                             'TimeZone', 'UTC', 'toTimeZone', this.TimeZone, ...
                             'Precision', 'microseconds');
      endif
    endfunction

    ## Shift each element by a fixed number of seconds applied to its absolute
    ## instant (daylight-saving aware for zoned arrays), then rebuild the
    ## wall-clock components.  DSEC may broadcast against the array size.  The
    ## Format and TimeZone properties are preserved.
    function this = addSeconds (this, dsec)
      ser = serial (this) + dsec;
      [Y, M, D, h, m, s] = serial2components (this, ser);
      this.Year = Y; this.Month = M; this.Day = D;
      this.Hour = h; this.Minute = m; this.Second = s;
    endfunction

    ## Shift each element by a calendarDuration (SGN is +1 for addition, -1 for
    ## subtraction).  Whole months are applied first with end-of-month day
    ## clamping, then whole calendar days (wall-clock preserving), then the
    ## time-of-day component as an instant.  Not-A-Time and infinite elements are
    ## carried through unchanged.
    function this = addCalendar (this, calD, sgn)
      dMonths = sgn * calmonths (calD);
      dDays   = sgn * caldays (calD);
      dTime   = sgn * days (time (calD)) * 86400;   # seconds

      ## Broadcast the instant components and the calendar deltas to a common
      ## size so the month math and the Not-A-Time / infinite masks all align.
      base = zeros (size (this.Year)) + zeros (size (this.Month)) ...
           + zeros (size (this.Day))  + zeros (size (this.Hour)) ...
           + zeros (size (this.Minute)) + zeros (size (this.Second)) ...
           + zeros (size (dMonths)) + zeros (size (dDays));
      Y  = this.Year + base;   M  = this.Month + base;  D  = this.Day + base;
      h  = this.Hour + base;   m  = this.Minute + base; s  = this.Second + base;
      dM = dMonths + base;     dD = dDays + base;

      ## An element is "live" only when both the instant and the calendar delta
      ## are finite; Not-A-Time and infinite inputs (from either operand)
      ## propagate straight through as the corresponding non-finite marker.
      ok = isfinite (Y) & isfinite (dM) & isfinite (dD);
      mk = Y + dM + dD;

      ## Add whole months, clamping the day to the last day of the target month
      ## (e.g. 31 Jan + 1 month -> 28 Feb), then add whole calendar days.
      total = Y * 12 + (M - 1) + dM;
      Y(ok) = floor (total(ok) / 12);
      M(ok) = mod (total(ok), 12) + 1;
      D(ok) = min (D(ok), eomday (Y(ok), M(ok))) + dD(ok);
      Y(! ok) = mk(! ok);  M(! ok) = mk(! ok);  D(! ok) = mk(! ok);
      h(! ok) = mk(! ok);  m(! ok) = mk(! ok);  s(! ok) = mk(! ok);

      this.Year = Y; this.Month = M; this.Day = D;
      this.Hour = h; this.Minute = m; this.Second = s;
      this = normalize (this);

      ## Add the time-of-day component as an instant (daylight-saving aware).
      if (any (dTime(:) != 0))
        this = addSeconds (this, dTime + zeros (size (this.Year)));
      endif
    endfunction

    ## Range with a fixed-length (duration/numeric) step of STEPSEC seconds.
    ## The endpoints' absolute instants are stepped by the ordinary numeric
    ## colon (so the inclusive-endpoint tolerance matches numeric ranges), then
    ## rebuilt into this array's time zone.  Empty and reversed ranges fall out
    ## naturally from the numeric colon.
    function R = colonLinear (A, stepSec, B)
      ser = serial (A) : stepSec : serial (B);
      [Y, M, D, h, m, s] = serial2components (A, ser);
      R = A;
      R.Year = Y; R.Month = M; R.Day = D;
      R.Hour = h; R.Minute = m; R.Second = s;
    endfunction

    ## Range with a calendarDuration STEP.  Each element is A + k*STEP for
    ## k = 0, 1, 2, ... (non-iterative, so month/year steps clamp the day of
    ## month per element).  The number of steps is found by bracketing then
    ## binary-searching the largest k whose element has not passed B, which
    ## keeps calendar arithmetic exact without assuming a fixed element spacing.
    function R = colonCalendar (A, step, B)
      first = A + step;
      if (first == A)
        error ("datetime.colon: STEP must be nonzero.");
      endif
      incr = first > A;
      if ((incr && A > B) || (! incr && A < B))
        R = A + (0:-1) .* step;   # empty range, keeps A's Format and TimeZone
        return;
      endif
      hi = 1;
      while (colon_within (A + hi .* step, B, incr) && hi < 2^40)
        hi *= 2;
      endwhile
      lo = 0;
      while (hi - lo > 1)
        mid = floor ((lo + hi) / 2);
        if (colon_within (A + mid .* step, B, incr))
          lo = mid;
        else
          hi = mid;
        endif
      endwhile
      R = A + (0:lo) .* step;
    endfunction

    ## Shared implementation of the six relational operators.  Both operands
    ## must be datetime and either both zoned or both unzoned; a zoned pair with
    ## differing zones is aligned onto A's zone (preserving the instant) so the
    ## wall-clock components can be compared directly.  Comparison is
    ## lexicographic on [Year Month Day Hour Minute Second] and therefore exact
    ## at every magnitude.  Not-A-Time (NaN components) never compares
    ## less/greater/equal, so only 'ne' returns true when a NaT is involved.
    function TF = relcompare (A, B, op)
      if (! (isa (A, 'datetime') && isa (B, 'datetime')))
        error ("datetime.%s: both operands must be datetime arrays.", op);
      endif
      if (xor (isempty (A.TimeZone), isempty (B.TimeZone)))
        error (strcat ("datetime.%s: cannot compare a datetime array with a", ...
                       " time zone to one without a time zone."), op);
      endif
      aY = A.Year; aM = A.Month; aD = A.Day;
      ah = A.Hour; am = A.Minute; asec = A.Second;
      if (! isempty (A.TimeZone) && ! strcmp (A.TimeZone, B.TimeZone))
        [bY, bM, bD, bh, bm, bsec] = __datetime__ (B.Year, B.Month, B.Day, ...
            B.Hour, B.Minute, B.Second, 'TimeZone', B.TimeZone, ...
            'toTimeZone', A.TimeZone, 'Precision', 'microseconds');
      else
        bY = B.Year; bM = B.Month; bD = B.Day;
        bh = B.Hour; bm = B.Minute; bsec = B.Second;
      endif
      switch (op)
        case 'eq'
          TF = (aY == bY) & (aM == bM) & (aD == bD) ...
             & (ah == bh) & (am == bm) & (asec == bsec);
        case 'ne'
          TF = ! ((aY == bY) & (aM == bM) & (aD == bD) ...
                & (ah == bh) & (am == bm) & (asec == bsec));
        case 'lt'
          TF = lexlt (aY, aM, aD, ah, am, asec, bY, bM, bD, bh, bm, bsec);
        case 'gt'
          TF = lexlt (bY, bM, bD, bh, bm, bsec, aY, aM, aD, ah, am, asec);
        case 'le'
          TF = lexlt (aY, aM, aD, ah, am, asec, bY, bM, bD, bh, bm, bsec) ...
             | ((aY == bY) & (aM == bM) & (aD == bD) ...
                & (ah == bh) & (am == bm) & (asec == bsec));
        case 'ge'
          TF = lexlt (bY, bM, bD, bh, bm, bsec, aY, aM, aD, ah, am, asec) ...
             | ((aY == bY) & (aM == bM) & (aD == bD) ...
                & (ah == bh) & (am == bm) & (asec == bsec));
      endswitch
    endfunction

  endmethods

endclassdef

## Shared back-end for 'isequal' (NANEQUAL false) and 'isequaln' (NANEQUAL
## true).  ARGS is the cell array of operands.  Returns true only when every
## operand is a datetime of the same size as the first and each pair of
## corresponding elements is the same point in time.  A non-datetime operand or
## a time-zone mismatch (one zoned, one unzoned) yields false rather than an
## error.  Zoned arrays are compared by absolute instant.  When NANEQUAL is
## true, Not-A-Time elements compare equal to one another (component-wise NaN
## matches NaN); otherwise any NaT makes the result false, as with NaN.
function TF = do_isequal (args, nanEqual)
  A = args{1};
  TF = true;
  for i = 2:numel (args)
    B = args{i};
    if (! (isa (A, 'datetime') && isa (B, 'datetime')))
      TF = false;
      return;
    endif
    if (! isequal (size (A), size (B)))
      TF = false;
      return;
    endif
    if (xor (isempty (A.TimeZone), isempty (B.TimeZone)))
      TF = false;
      return;
    endif
    if (isempty (A))
      continue;  # two empties of equal size compare equal
    endif
    ## Align B onto A's zone so the wall-clock components compare by instant.
    aY = A.Year; aM = A.Month; aD = A.Day;
    ah = A.Hour; am = A.Minute; asec = A.Second;
    if (! isempty (A.TimeZone) && ! strcmp (A.TimeZone, B.TimeZone))
      [bY, bM, bD, bh, bm, bsec] = __datetime__ (B.Year, B.Month, B.Day, ...
          B.Hour, B.Minute, B.Second, 'TimeZone', B.TimeZone, ...
          'toTimeZone', A.TimeZone, 'Precision', 'microseconds');
    else
      bY = B.Year; bM = B.Month; bD = B.Day;
      bh = B.Hour; bm = B.Minute; bsec = B.Second;
    endif
    if (nanEqual)
      E = ceq (aY, bY) & ceq (aM, bM) & ceq (aD, bD) ...
        & ceq (ah, bh) & ceq (am, bm) & ceq (asec, bsec);
    else
      E = (aY == bY) & (aM == bM) & (aD == bD) ...
        & (ah == bh) & (am == bm) & (asec == bsec);
    endif
    if (! all (E(:)))
      TF = false;
      return;
    endif
  endfor
endfunction

## Component equality that also treats NaN as equal to NaN (used by isequaln).
function TF = ceq (x, y)
  TF = (x == y) | (isnan (x) & isnan (y));
endfunction

## True while a candidate datetime X has not yet passed the range endpoint B,
## for an increasing (INCR true) or decreasing calendar range.  Used to bracket
## and binary-search the element count in 'colonCalendar'.
function TF = colon_within (X, B, incr)
  if (incr)
    TF = X <= B;
  else
    TF = X >= B;
  endif
endfunction

## Lexicographic strictly-less-than on datetime component arrays.  Returns true
## where the [Year Month Day Hour Minute Second] tuple of the first operand is
## strictly earlier than that of the second.  Any NaN component (Not-A-Time)
## makes the element false, matching NaN comparison semantics.  All arguments
## broadcast against each other element-wise.
function TF = lexlt (aY, aM, aD, ah, am, asec, bY, bM, bD, bh, bm, bsec)
  eqY = aY == bY;  eqM = aM == bM;  eqD = aD == bD;
  eqh = ah == bh;  eqm = am == bm;
  TF = (aY < bY) ...
     | (eqY & aM < bM) ...
     | (eqY & eqM & aD < bD) ...
     | (eqY & eqM & eqD & ah < bh) ...
     | (eqY & eqM & eqD & eqh & am < bm) ...
     | (eqY & eqM & eqD & eqh & eqm & asec < bsec);
endfunction

## Promote a set-operation operand to a datetime array.  A datetime is returned
## unchanged; text (character vector, string, or cellstr) is parsed by the
## constructor, inheriting REF's time zone so the two operands share a frame;
## numeric, logical, and duration operands are rejected the way MATLAB rejects
## them.  Defined at file scope (not as a method) so it dispatches correctly
## when the first set-operation argument is text rather than a datetime.
function d = dtSetPromote (x, ref, op)
  if (isa (x, 'datetime'))
    d = x;
  elseif (ischar (x) || iscellstr (x) || isa (x, 'string'))
    if (isempty (ref.TimeZone))
      d = datetime (x);
    else
      d = datetime (x, 'TimeZone', ref.TimeZone);
    endif
  elseif (isa (x, 'duration') || isa (x, 'calendarDuration'))
    error (strcat ("datetime.%s: comparison is not defined between datetime", ...
                   " and duration arrays."), op);
  else
    error (strcat ("datetime.%s: comparison is not defined between datetime", ...
                   " and numeric arrays."), op);
  endif
endfunction

## Orient a (non-'rows') set-operation result: a row vector when both operands
## were row vectors, and a column vector otherwise (matching MATLAB, so an empty
## result is 0-by-1 rather than 0-by-0).  C is a datetime array.
function C = reshapeSetResult (C, bothRows)
  if (bothRows)
    C = reshape (C, 1, numel (C));
  else
    C = reshape (C, numel (C), 1);
  endif
endfunction

## Number of days in month M of year Y (element-wise, proleptic Gregorian).
function d = dtDaysInMonth (Y, M)
  dpm = [31 28 31 30 31 30 31 31 30 31 30 31];
  d = dpm(M);
  d = reshape (d, size (M));
  leap = (mod (Y, 4) == 0 & mod (Y, 100) != 0) | (mod (Y, 400) == 0);
  d(M == 2 & leap) = 29;
endfunction

## Add K whole calendar months to the date (Y, M, D), clamping the day of month
## to the last valid day of the target month (e.g. 31 Jan + 1 month -> 28 Feb).
## Time of day is not represented here; it is carried unchanged by the caller.
function [Yo, Mo, Do] = dtAddMonths (Y, M, D, K)
  idx = Y .* 12 + (M - 1) + K;
  Yo = floor (idx ./ 12);
  Mo = idx - Yo .* 12 + 1;
  Do = min (D, dtDaysInMonth (Yo, Mo));
endfunction

## Add K whole calendar days to the date (Y, M, D), element-wise.
function [Yo, Mo, Do] = dtAddDays (Y, M, D, K)
  dn = datenum (Y(:), M(:), D(:)) + K(:);
  dv = datevec (dn);
  Yo = reshape (dv(:,1), size (Y));
  Mo = reshape (dv(:,2), size (Y));
  Do = reshape (dv(:,3), size (Y));
endfunction

## Error message shared by caldiff and between for an invalid COMPONENTS input.
function msg = calCompError (op)
  msg = strcat ("datetime.", op, ": COMPONENTS must be 'Years', 'Quarters',", ...
                " 'Months', 'Weeks', 'Days', or 'Time', or a string array or", ...
                " cell array containing those components.");
endfunction

## Parse the COMPONENTS argument of caldiff/between into presence flags for each
## calendar unit and the display Format string of the resulting calendarDuration
## (which always contains 'm', 'd', and 't', with 'y'/'q'/'w' added only when
## those units are requested).  An empty COMPONENTS selects the default set
## {Years, Months, Days, Time}.
function [f, fmt] = parseCalComponents (comps, op)
  if (isempty (comps))
    f = struct ('y', true, 'q', false, 'm', true, ...
                'w', false, 'd', true, 't', true);
  else
    if (ischar (comps) && isrow (comps))
      toks = {comps};
    elseif (iscellstr (comps))
      toks = comps(:)';
    elseif (isa (comps, 'string'))
      toks = cellstr (comps)(:)';
    else
      error (calCompError (op));
    endif
    f = struct ('y', false, 'q', false, 'm', false, ...
                'w', false, 'd', false, 't', false);
    for i = 1:numel (toks)
      switch (lower (toks{i}))
        case {'years', 'year', 'y'}
          f.y = true;
        case {'quarters', 'quarter', 'q'}
          f.q = true;
        case {'months', 'month', 'mo', 'm'}
          f.m = true;
        case {'weeks', 'week', 'w'}
          f.w = true;
        case {'days', 'day', 'd'}
          f.d = true;
        case {'time', 't'}
          f.t = true;
        otherwise
          error (calCompError (op));
      endswitch
    endfor
    if (! (f.y || f.q || f.m || f.w || f.d || f.t))
      error (calCompError (op));
    endif
  endif
  ## The calendarDuration Format must contain 'm', 'd', and 't', so a
  ## single-component result such as caldiff (..., 'Years') keeps them even
  ## though only years are populated.  This is invisible for a non-zero result
  ## (the other fields are zero and are not shown) but means an all-zero result
  ## displays as '0d' rather than MATLAB's '0y'/'0q'/'0w'; the stored value is
  ## the same.
  fmt = '';
  if (f.y)
    fmt = [fmt, 'y'];
  endif
  if (f.q)
    fmt = [fmt, 'q'];
  endif
  fmt = [fmt, 'm'];
  if (f.w)
    fmt = [fmt, 'w'];
  endif
  fmt = [fmt, 'dt'];
endfunction
