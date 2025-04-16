## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
##
## This file is part of the statistics package for GNU Octave.
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
  ## @deftp {Class} datetime
  ##
  ## Array representing points in time using the Gregorian calendar.
  ##
  ## The underlying implementation relies on the @qcode{'date.h'} C++ library
  ## and stores internally the datetime points are double arrays representing
  ## whole years, months, days, hours, and minutes, as well as seconds including
  ## their fractional part.  The precision of this implementation is set at
  ## microseconds, which is substantial for typical times.
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
    ## @deftp {datetime} {property} localtime
    ##
    ## A read-only property specifying the local time zone of the system, where
    ## Octave is running.
    ##
    ## @end deftp
    SystemTimeZone = localtime (time ()).zone;
  endproperties

  properties (Access = private, Hidden)
    ## Whole years
    Y = 0
    ## Whole months
    M = 0
    ## Whole days
    D = 0
    ## Whole hours
    h = 0
    ## Whole minutes
    m = 0
    ## Seconds (including fractional seconds)
    s = 0
  endproperties

  properties
    ## -*- texinfo -*-
    ## @deftp {datetime} {property} Format
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
    ## TimeZone, specified as a character vector or string scalar.  If
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
    ## @deftypefn  {datetime} {@var{Τ} =} datetime (@qcode{'now'})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@qcode{'today'})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@qcode{'tomorrow'})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@qcode{'yesterday'})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{DateStrings})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{DateStrings}, @qcode{'InputFormat'}, @var{INFMT})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{DateStrings},
    ## @qcode{'InputFormat'}, @var{INFMT}, @qcode{'PivotYear'}, @var{PIVOT})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{DateVectors})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{Y}, @var{MO}, @var{D})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S}, @var{MS})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@var{X}, @qcode{'ConvertFrom'}, @var{TYPE})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@dots{}, @qcode{'Format'}, @var{FMT})
    ## @deftypefnx {datetime} {@var{Τ} =} datetime (@dots{}, @qcode{'TimeZone'}, @var{TZ})
    ##
    ## Create a new array of datetime values.
    ##
    ## @code{@var{Τ} = datetime (@qcode{'now'})} returns a scalar datetime array
    ## corresponding to the current date and time. @code{@var{Τ} = datetime
    ## (@qcode{'now'})} is the same as @code{@var{Τ} = datetime ()}.  Except for
    ## @qcode{'now'}, the same syntax can be used with @qcode{'today'},
    ## @qcode{'tomorrow'} and @qcode{'yesterday'}.  These options return the
    ## respective date but with time set at midnight.
    ##
    ## @code{@var{Τ} = datetime (@var{DateStrings})} creates a datetime array
    ## from the text in @var{DateStrings} representing points in time.  In
    ## current implementation, @var{DateStrings} are parsed by Octave's core
    ## @code{datevec} function, hence supported text formats are currently those
    ## supported by @code{datevec}.
    ##
    ## @code{@var{Τ} = datetime (@var{DateStrings}, @qcode{'InputFormat'},
    ## @var{INFMT})} also allows to specify a particular input text format to
    ## parse @var{DateStrings}.  It is always preferable to specify the format
    ## @var{INFMT} if it is known.  Formats which do not specify a particular
    ## time component will have the value set to zero.  Formats which do not
    ## a date will default to January 1st of the current year.
    ##
    ## @code{@var{Τ} = datetime (@var{DateStrings}, @qcode{'InputFormat'},
    ## @var{INFMT}, @qcode{'PivotYear'}, @var{PIVOT})} also allows to specify a
    ## pivot year, which refers to the year at the start of the century to which
    ## two-digit years will be referenced.  When not specified, it defaults to
    ## the current years minus 50.
    ##
    ## @code{@var{Τ} = datetime (@var{DateVectors})} creates a column vector of
    ## datetime values from the date vectors in @var{DateVectors}.
    ##
    ## @code{@var{Τ} = datetime (@var{Y}, @var{MO}, @var{D}} creates an array of
    ## datetime values for corresponding elements of the @var{Y}, @var{MO}, and
    ## @var{D} arrays, which must be of the same size or scalars, must contain
    ## integer values, and they correspond to years, months, and days,
    ## respectively.
    ##
    ## @code{@var{Τ} = datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI},
    ## @var{S})} also adds time components to the constructed datetime arrays.
    ## @var{H}, @var{MI}, and @var{S} must be of the same size or scalars.
    ## @var{H} and @var{MI} must contain integer values, whereas @var{S} may
    ## also contain a fractional part.
    ##
    ## @code{@var{Τ} = datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI},
    ## @var{S}, @var{MS})} also adds a milliseconds component, @var{MS}, which
    ## may also have a fractional part.
    ##
    ## @code{@var{Τ} = datetime (@var{X}, @qcode{'ConvertFrom'}, @var{TYPE})}
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
    ## @code{@var{Τ} = datetime (@dots{}, @qcode{'Format'}, @var{FMT})}
    ## specifies the display format of the values in the output datetime array.
    ## Currently, only the default display format is implemented.
    ##
    ## @code{@var{Τ} = datetime (@dots{}, @qcode{'TimeZone'}, @var{TZ})} sets
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
        [this.Y, this.M, this.D, this.h, this.m, this.s] = __datetime__ ('now');
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
        if (! isempty (isnumeric))
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
          this.Y = reshape (DATEVEC(:,1), size (DateStrings));
          this.M = reshape (DATEVEC(:,2), size (DateStrings));
          this.D = reshape (DATEVEC(:,3), size (DateStrings));
          this.h = reshape (DATEVEC(:,4), size (DateStrings));
          this.m = reshape (DATEVEC(:,5), size (DateStrings));
          this.s = reshape (DATEVEC(:,6), size (DateStrings));
          return;
        endif
      endif

      ## Handle inputs (no errors here)
      if (! isempty (ConvertFrom) && ! isempty (TimeZone))
        [this.Y, this.M, this.D, this.h, this.m, this.s] = __datetime__ ...
                    (args{1}, 'ConvertFrom', ConvertFrom, 'TimeZone', TimeZone);
      elseif (! isempty (ConvertFrom) && isempty (TimeZone))
        [this.Y, this.M, this.D, this.h, this.m, this.s] = __datetime__ ...
                    (args{1}, 'ConvertFrom', ConvertFrom);
      else
        [this.Y, this.M, this.D, this.h, this.m, this.s] = __datetime__ (args{:});
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{cstr} =} dispstrings (@var{Τ})
    ##
    ## Get display formatted strings for each element of a datetime object.
    ##
    ## @code{@var{cstr} = dispstrings (@var{Τ})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## datetime @var{Τ}.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      ## Default display format
      mnames = {'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', ...
                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'};
      if (this.h != 0 || this.m != 0 || this.s != 0)
        if (fix (this.s) == this.s)
          fmt = "%02d-%s-%04d %02d:%02d:%02d";
          type = 1;
        else
          fmt = "%02d-%s-%04d %02d:%02d:%02d.%03d";
          type = 2;
        endif
      else
        fmt = "%02d-%s-%04d";
        type = 3;
      endif
      ## Process all elements
      sz = size (this);
      cstr = cell (sz);
      for i = 1:prod (sz)
        if (isnan (this.Y(i)))
          cstr{i} = 'NaT';
        elseif (isinf (this.Y(i)))
          cstr{i} = num2str (this.Y(i));
        else
          switch (type)
            case 1
              str = sprintf (fmt, this.D(i), mnames{this.M(i)}, this.Y(i), ...
                             this.h(i), this.m(i), this.s(i));
            case 2
              str = sprintf (fmt, this.D(i), mnames{this.M(i)}, this.Y(i), ...
                             this.h(i), this.m(i), fix (this.s(i)), ...
                             rem (this.s(i), 1) * 1000);
            case 3
              str = sprintf (fmt, this.D(i), mnames{this.M(i)}, this.Y(i));

          endswitch
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
    ## values and have the same size as @var{T}.
    ##
    ## @end deftypefn
    function [Y, M, D] = ymd (this)
      Y = this.Y;
      M = this.M;
      D = this.D;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {Method} {[@var{h}, @var{m}, @var{s}] =} hms (@var{T})
    ##
    ## Hour, Minute, and Second components of a datetime array.
    ##
    ## @code{[@var{h}, @var{m}, @var{s}] = ymd (@var{T})} returns the hour,
    ## miute, and second components of the corresponding datetime values in
    ## @var{T} as separate numeric arrays.  @var{h} and @var{m} contain integer
    ## values and @var{s} may also contain a fractional part.  All outputs have
    ## the same size as @var{T}.
    ##
    ## @end deftypefn
    function [h, m, s] = hms (this)
      h = this.h;
      m = this.m;
      s = this.d;
    endfunction

    function out = year (this)
      out = this.Y;
    endfunction

    function out = quarter (this)
      out = ceil (this.M / 3);
    endfunction

    function out = month (this)
      out = this.M;
    endfunction

    function out = week (this)
      error ("datetime.week: not implemented yet.");
    endfunction

    function out = day (this)
      out = this.D;
    endfunction

    function out = hour (this)
      out = this.h;
    endfunction

    function out = minute (this)
      out = this.m;
    endfunction

    function out = second (this)
      out = this.s;
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
## 'size'             'ndims'            'numel'            'histcounts'      ##
## 'min'              'max'              'mink'             'maxk'            ##
## 'plot'                                                                     ##
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
        sz = size (this.Y, varargin{:});
      else
        sz = size (this.Y);
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
      out = ndims (this.Y);
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

    function out = histcounts (this, varargin)
      error ("datetime.histcounts: not implemented yet.");
    endfunction

    function out = min (this, varargin)
      error ("datetime.min: not implemented yet.");
    endfunction

    function out = max (this, varargin)
      error ("datetime.max: not implemented yet.");
    endfunction

    function out = mink (this, varargin)
      error ("datetime.mink: not implemented yet.");
    endfunction

    function out = maxk (this, varargin)
      error ("datetime.maxk: not implemented yet.");
    endfunction

    function out = plot (this, varargin)
      error ("datetime.plot: not implemented yet.");
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

  methods (Access = public)

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
      DV = [this.Y(:), this.M(:), this.D(:), this.h(:), this.m(:), this.s(:)];
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

  methods (Access = public)

    function TF = isbetween (this, varargin)
      error ("datetime.isbetween: not implemented yet.");
    endfunction

    function TF = iscolumn (this)
      TF = iscolumn (this.Y);
    endfunction

    function TF = isdst (this)
      error ("datetime.isdst: not implemented yet.");
    endfunction

    function TF = isempty (this)
      TF = isempty (this.Y);
    endfunction

    function TF = isequal (varargin)
      error ("datetime.isequal: not implemented yet.");
    endfunction

    function TF = isequaln (varargin)
      error ("datetime.isequaln: not implemented yet.");
    endfunction

    function TF = isfinite (this)
      TF = isfinite (this.Y);
    endfunction

    function TF = isinf (this)
      TF = isinf (this.Y);
    endfunction

    function TF = ismatrix (this)
      TF = ismatrix (this.Y);
    endfunction

    function [TF, index] = ismember (A, B, varargin)
      error ("datetime.ismember: not implemented yet.");
    endfunction

    function TF = isnat (this)
      TF = isnan (this.Y);
    endfunction

    function TF = isregular (this)
      error ("datetime.isregular: not implemented yet.");
    endfunction

    function TF = isrow (this)
      TF = isrow (this.Y);
    endfunction

    function TF = isscalar (this)
      TF = isscalar (this.Y);
    endfunction

    function TF = issorted (this, varargin)
      error ("datetime.issorted: not implemented yet.");
    endfunction

    function TF = issortedrows (this, varargin)
     error ("datetime.issortedrows: not implemented yet.");
    endfunction

    function TF = isvector (this)
      TF = isvector (this.Y);
    endfunction

    function TF = isweekend (this)
      error ("datetime.isweekend: not implemented yet.");
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

  methods (Access = public)

    function [B, index] = sort (A, varargin)
      error ("datetime.sort: not implemented yet.");
    endfunction

    function [B, index] = sortrows (A, varargin)
      error ("datetime.sortrows: not implemented yet.");
    endfunction

    function [B, ixA, ixB] = unique (A, varargin)
      error ("datetime.unique: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = interp1 (A, B, varargin)
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
## 'reshape'          'circshift'        'permute'          'ipermute'        ##
## 'transpose'        'ctranspose'                                            ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function out = cat (dim, varargin)
      args = varargin;
      f = @(x) isa (x, 'datetime');
      if (! all (cellfun (f, args)))
        error ("datetime: invalid input to constructor.");
      endif
      out = args{1};
      fieldArgs = cellfun (@(obj) obj.Y, args, 'UniformOutput', false);
      out.Y = cat (dim, fieldArgs{:});
      fieldArgs = cellfun (@(obj) obj.M, args, 'UniformOutput', false);
      out.M = cat (dim, fieldArgs{:});
      fieldArgs = cellfun (@(obj) obj.D, args, 'UniformOutput', false);
      out.D = cat (dim, fieldArgs{:});
      fieldArgs = cellfun (@(obj) obj.h, args, 'UniformOutput', false);
      out.h = cat (dim, fieldArgs{:});
      fieldArgs = cellfun (@(obj) obj.m, args, 'UniformOutput', false);
      out.m = cat (dim, fieldArgs{:});
      fieldArgs = cellfun (@(obj) obj.s, args, 'UniformOutput', false);
      out.s = cat (dim, fieldArgs{:});
    endfunction

    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    function this = repmat (this, varargin)
      this.Y = repmat (this.Y, varargin{:});
      this.M = repmat (this.M, varargin{:});
      this.D = repmat (this.D, varargin{:});
      this.h = repmat (this.h, varargin{:});
      this.m = repmat (this.m, varargin{:});
      this.s = repmat (this.s, varargin{:});
    endfunction

    function this = reshape (this, varargin)
      this.Y = reshape (this.Y, varargin{:});
      this.M = reshape (this.M, varargin{:});
      this.D = reshape (this.D, varargin{:});
      this.h = reshape (this.h, varargin{:});
      this.m = reshape (this.m, varargin{:});
      this.s = reshape (this.s, varargin{:});
    endfunction

    function this = circshift (this, varargin)
      this.Y = circshift (this.Y, varargin{:});
      this.M = circshift (this.M, varargin{:});
      this.D = circshift (this.D, varargin{:});
      this.h = circshift (this.h, varargin{:});
      this.m = circshift (this.m, varargin{:});
      this.s = circshift (this.s, varargin{:});
    endfunction

    function this = permute (this, order)
      this.Y = permute (this.Y, order);
      this.M = permute (this.M, order);
      this.D = permute (this.D, order);
      this.h = permute (this.h, order);
      this.m = permute (this.m, order);
      this.s = permute (this.s, order);
    endfunction

    function this = ipermute (this, order)
      this.Y = ipermute (this.Y, order);
      this.M = ipermute (this.M, order);
      this.D = ipermute (this.D, order);
      this.h = ipermute (this.h, order);
      this.m = ipermute (this.m, order);
      this.s = ipermute (this.s, order);
    endfunction

    function this = transpose (this)
      this.Y = transpose (this.Y);
      this.M = transpose (this.M);
      this.D = transpose (this.D);
      this.h = transpose (this.h);
      this.m = transpose (this.m);
      this.s = transpose (this.s);
    endfunction

    function this = ctranspose (this)
      this.Y = ctranspose (this.Y);
      this.M = ctranspose (this.M);
      this.D = ctranspose (this.D);
      this.h = ctranspose (this.h);
      this.m = ctranspose (this.m);
      this.s = ctranspose (this.s);
    endfunction

  endmethods

################################################################################
##                  ** Reference and Assignment Operations **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'subsref'          'subsasgn'                                              ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Class specific subscripted reference
    function varargout = subsref (this, s)

      chain_s = s(2:end);
      s = s(1);
      switch (s.type)
        case '()'
          out = this;
          out.Y = this.Y(s.subs{:});
          out.M = this.M(s.subs{:});
          out.D = this.D(s.subs{:});
          out.h = this.h(s.subs{:});
          out.m = this.m(s.subs{:});
          out.s = this.s(s.subs{:});

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
              out = this.Y;
            case {'Month'}
              out = this.M;
            case {'Day'}
              out = this.D;
            case {'Hour'}
              out = this.h;
            case {'Minute'}
              out = this.m;
            case {'Second'}
              out = this.s;
            otherwise
              error ("datetime.subsref: unrecongized property: %s", ...
                     s.subs);
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
          if (! isa (val, "datetime"))
            error (["datetime.subsasgn: cannot assign %s values", ...
                            "to a datetime array."], class (val));
          endif
          this.Y(s.subs{:}) = val.Y;
          this.M(s.subs{:}) = val.M;
          this.D(s.subs{:}) = val.D;
          this.h(s.subs{:}) = val.h;
          this.m(s.subs{:}) = val.m;
          this.s(s.subs{:}) = val.s;

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
              this.Y(p.subs{:}) = val;
            case {'Month'}
              this.M(p.subs{:}) = val;
            case {'Day'}
              this.D(p.subs{:}) = val;
            case {'Hour'}
              this.h(p.subs{:}) = val;
            case {'Minute'}
              this.m(p.subs{:}) = val;
            case {'Second'}
              this.s(p.subs{:}) = val;
            otherwise
              error ("datetime.subsasgn: unrecongized property: %s", ...
                     s.subs);
          endswitch
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.Y = this.Y(varargin{:});
      this.M = this.M(varargin{:});
      this.D = this.D(varargin{:});
      this.h = this.h(varargin{:});
      this.m = this.m(varargin{:});
      this.s = this.s(varargin{:});
    endfunction

  endmethods

endclassdef
