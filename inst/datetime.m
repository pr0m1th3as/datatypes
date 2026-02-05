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
    ## converts the numeric values in @var{X} to a datetime array accordinng to
    ## the data type specified by @var{TYPE}.  The following types are
    ## suppoerted:
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
                                   pairedArgs (optNames, dfValues, varargin(:));

      ## Check optional 'Format' and 'InputFormat' arguments
      if (! isempty (ConvertFrom))
        ## Call __datetime__ to check for valid ConvertFrom string and data input
        [~,~,~,~,~,~,errmsg] = __datetime__ (args{:}, 'ConvertFrom', ConvertFrom);
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
              error ("datetime: could not recognize date/time format from input.");
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
    ## @code{[@var{h}, @var{m}, @var{s}] = ymd (@var{T})} returns the hour,
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
        error ("datetime: unrecongized MONTHTYPE.");
      endif
      out = this.Month;
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
    ## @code{@var{D} = day (@var{T}, @var{monthType})} returns the day number or
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
        m(m < 1) += 12;
        y(m < 1) -= 1;
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
      elseif (any (strcmpi (type, 'dayofyear')))
        m = this.Month;
        y = this.Year;
        days = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
        isly = mod (y, 4) == 0 & (mod (y, 100) != 0 | mod (y, 400) == 0);
        if (isly && m > 2)
          out = days(m) + this.Day + 1;
        else
          out = days(m) + this.Day;
        endif
      else
        error ("datetime: unrecongized DAYTYPE.");
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
    ## @deftypefnx {datetime} {@var{s} =} second (@var{T}, @var{dayType})
    ##
    ## Seconds component of a datetime array.
    ##
    ## @code{@var{s} = second (@var{T})} returns the number of seconds for each
    ## element of the input datetime array @var{T}.  The output @var{s} is a
    ## @qcode{double} array containing values in the range @math{[1, 60)},
    ## including any fractional part of the second, and it has the same size as
    ## @var{T}.  Not-A-Time (@qcode{NaT}) values in @var{T} are returned as
    ## @qcode{NaN} in the output array.
    ##
    ## @code{@var{D} = second (@var{T}, @var{secondType})} returns the seconds
    ## for each element of the input datetime array @var{T} as specified by
    ## @var{secondType}, which may have any of the following options:
    ##
    ## @itemize
    ## @item @qcode{'secondofminute'} (default) returns the second of the minute
    ## in a numeric array, in the range @math{[1, 60)}.
    ## @item @qcode{'secondofday'} returns the second of the day in a numeric
    ## array, in the range @math{[1, 86400)}.
    ## @end itemize
    ##
    ## @end deftypefn
    function out = second (this)
      out = this.Second;
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
    ## For compatibility reasons with Octave's OOP interface and @code{subsasgn}
    ## behavior, datetime's @code{numel} is defined to always return 1.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = 1;
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
    ## FMV-1a variant of the Fowler-Noll-Vo non-cryptographic hash function.
    ##
    ## @code{@var{h} = keyHash (@var{T}), @var{base}} also generates a 64-bit
    ## hash code using @var{base} as the offset basis for the FNV-1a hash
    ## algorithm.  @var{base} must be a @qcode{uint64} integer type scalar.  Use
    ## this syntax to cascade @code{keyHash} on multiple objects for which a
    ## single hash code is required.
    ##
    ## Note that unlike MATLAB, this implementation does no use any random seed.
    ## As a result, @code{keyHash} will always generate the exact same hash key
    ## for any particular input across different workers and Octave sessions.
    ##
    ## @end deftypefn
    function key = keyHash (this, base = [])
      ## Initialize string with size and class name
      size_str = sprintf ('%dx', size (this.strs))(1:end-1);
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
        error ("datetime.datavec: too many output arguments.");
      endif
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'isbetween'        'iscolumm'         'isdst'            'isemtpy'         ##
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

    function TF = isequal (varargin)
      error ("datetime.isequal: not implemented yet.");
    endfunction

    function TF = isequaln (varargin)
      error ("datetime.isequaln: not implemented yet.");
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
    ## otherwise.  Similarly to @qcode{NaN} values, Not-A-Time (@qcode{NaT})
    ## elements are not equal with each other and always return @qcode{false}.
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
      ## Check input arguments
      do_rows = false;
      if (! isempty (varargin))
        if (strcmpi (varargin{1}, 'rows'))
          do_rows = true;
          if (ndims (A) != 2 || ndims (A) != ndims (B))
            error ("datetime.ismember: 'rows' applies only to 2-D matrices.");
          endif
          if (size (A, 2) != size (B, 2))
            error ("datetime.ismember: 'rows' requires same number of columns.");
          endif
        else
          error ("datetime.ismember: invalid optional argument.");
        endif
      endif
      if (! isa (B, 'datetime'))
        error ("datetime.ismember: B must be a 'datetime' array.");
      endif
      ## Find ismember
      if (do_rows)
        [TF_y, index_y] = ismember (A.Year, B.Year, 'rows');
        [TF_mo, index_mo] = ismember (A.Month, B.Month, 'rows');
        [TF_d, index_d] = ismember (A.Day, B.Day, 'rows');
        [TF_h, index_h] = ismember (A.Hour, B.Hour, 'rows');
        [TF_mi, index_mi] = ismember (A.Minute, B.Minute, 'rows');
        [TF_s, index_s] = ismember (A.Second, B.Second, 'rows');
      else
        [TF_y, index_y] = ismember (A.Year, B.Year);
        [TF_mo, index_mo] = ismember (A.Month, B.Month);
        [TF_d, index_d] = ismember (A.Day, B.Day);
        [TF_h, index_h] = ismember (A.Hour, B.Hour);
        [TF_mi, index_mi] = ismember (A.Minute, B.Minute);
        [TF_s, index_s] = ismember (A.Second, B.Second);
      endif
      TF = TF_y & TF_mo & TF_d & TF_h & TF_mi & TF_s;
      index = index_y(TF);
    endfunction

    function TF = isregular (this)
      error ("datetime.isregular: not implemented yet.");
    endfunction

    function TF = issorted (this, varargin)
      error ("datetime.issorted: not implemented yet.");
    endfunction

    function TF = issortedrows (this, varargin)
     error ("datetime.issortedrows: not implemented yet.");
    endfunction

    function TF = isweekend (this)
      error ("datetime.isweekend: not implemented yet.");
    endfunction

  endmethods

  methods (Access = public)

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
    ## @deftypefnx {datetime} {@var{TF} =} ismissing (@varT}, @var{indicator})
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
          if (! isa (varargin{1}, 'datetime'))
            for i = 1:numel (indicator)
              DT = indicator(i);
              is_eq = DT.Year == this.Year & DT.Month == this.Month & ...
                      DT.Day == this.Day & DT.Hour == this.Hour & ...
                      DT.Minute == this.Minute & DT.Second == this.Second;
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
    ## dimensions is equal to 1 (either @math{1xN} or @math{Nx1}).  By
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
##                                                                            ##
################################################################################

  methods (Hidden)

    function [B, index] = sort (A, varargin)
      error ("datetime.sort: not implemented yet.");
    endfunction

    function [B, index] = sortrows (A, varargin)
      error ("datetime.sortrows: not implemented yet.");
    endfunction

    function [B, ixA, ixB] = unique (A, varargin)
      error ("datetime.unique: not implemented yet.");
    endfunction

    function BI = interp1 (A, B, AI, varargin)
      error ("datetime.interp1: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = intersect (A, B, varargin)
      error ("datetime.intersect: not implemented yet.");
    endfunction

    function [C, index] = setdiff (A, B, varargin)
      error ("datetime.setdiff: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = setxor (A, B, varargin)
      error ("datetime.setxor: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = union (A, B, varargin)
      error ("datetime.union: not implemented yet.");
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
    ## @var{n} is a vector, it must have the same elemnts as @var{A}, in which
    ## case it specifies the number of times to repeat each corresponding
    ## element of @var{A}.
    ##
    ## @code{@var{B} =} repelem (@var{A}, @var{d1}, @dots{}, @var{dN}} returns
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
    ## a datetime vector.  @var{R} must be a @math{2xN} matrix of integers.
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
    ## Repeat copies of datetime array elements.
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
              error ("datetime.subsref: unrecongized property: %s", s.subs);
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
              if (isempty (this.TimeZone))
                TimeZone = this.SystemTimeZone;
              else
                TimeZone = this.TimeZone;
              endif
              [this.Y, this.M, this.D, this.h, this.m, this.s, errmsg] = ...
              __datetime__ (this.Y, this.M, this.D, this.h, this.m, this.s, ...
              'TimeZone', TimeZone, 'toTimeZone', toTimeZone);
              if (! isnumeric (errmsg))
                error ("datetime.subsasgn: %s", errmsg);
              endif
              this.TimeZone = toTimeZone;
            case {'Year'}
              this.Year(p.subs{:})   = val;
            case {'Month'}
              this.Month(p.subs{:})  = val;
            case {'Day'}
              this.Day(p.subs{:})    = val;
            case {'Hour'}
              this.Hour(p.subs{:})   = val;
            case {'Minute'}
              this.Minute(p.subs{:}) = val;
            case {'Second'}
              this.Second(p.subs{:}) = val;
            otherwise
              error ("datetime.subsasgn: unrecongized property: %s", s.subs);
          endswitch
      endswitch

    endfunction

  endmethods

  methods (Access = private)

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

  endmethods

endclassdef
