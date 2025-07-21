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

classdef duration
  ## -*- texinfo -*-
  ## @deftp {Class} duration
  ##
  ## Array representing durations of time using fixed-length time units.
  ##
  ## @qcode{duration} values are stored internally as @qcode{double} type array
  ## representing numbers of elapsed days as a fixed-length time unit.  By
  ## default, fractional seconds of duration values are not displayed, but their
  ## actual precision is closer to nanoseconds for typical time lengths.
  ##
  ## @code{duration} arrays can be created through their constructor by
  ## combining numeric arrays representing individual fixed-length elapsed time
  ## units or through the functions @code{years}, @code{days}, @code{hours},
  ## @code{minutes}, @code{seconds}, and @code{calyears}, which create
  ## fixed-length durations in terms of a single duration units.  These
  ## functions are also available as methods of @code{duration} arrays to
  ## extract individual duration units as numeric arrays.
  ##
  ## @seealso{calendarDuration, datetime}
  ## @end deftp

  properties
    ## -*- texinfo -*-
    ## @deftp {duration} {property} Format
    ##
    ## Display format, specified as a character vector or string scalar.  If
    ## specified as a string scalar, it is converted and stored internally as
    ## a character vector.
    ##
    ## @end deftp
    Format = 'hh:mm:ss'
  endproperties

  properties (SetAccess = private, Hidden)
    ## Duration length in days
    Days = 0
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ('%s =\n', in_name);
      endif
      __disp__ (this, 'duration', in_name);
    endfunction

    ## Custom display
    function disp (this)
      __disp__ (this, 'duration');
    endfunction

  endmethods

################################################################################
##                 ** Create and convert 'duration' type **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'duration'         'dispstrings'      'cellstr'          'char'            ##
## 'datevec'          'hms'              'years'            'days'            ##
## 'hours'            'minutes'          'seconds'          'milliseconds'    ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{D} =} duration (@var{X})
    ## @deftypefnx {duration} {@var{D} =} duration (@var{H}, @var{MI}, @var{S})
    ## @deftypefnx {duration} {@var{D} =} duration (@var{H}, @var{MI}, @var{S}, @var{MS})
    ## @deftypefnx {duration} {@var{D} =} duration (@var{TimeStrings})
    ## @deftypefnx {duration} {@var{D} =} duration (@var{TimeStrings}, @qcode{'InputFormat'}, @var{INFMT})
    ## @deftypefnx {duration} {@var{D} =} duration (@dots{}, @qcode{'Format'}, @var{FMT})
    ##
    ## Create a new array of fixed-length time durations.
    ##
    ## @code{@var{D} = duration (@var{X})} creates a column vector of durations
    ## from a numeric matrix.
    ##
    ## @code{@var{D} = duration (@var{H}, @var{MI}, @var{S})} creates a duration
    ## array from numeric arrays containing the number of hours, minutes, and
    ## seconds specified by @var{H}, @var{MI} and @var{S}, respectively.
    ##
    ## @code{@var{D} = duration (@var{H}, @var{MI}, @var{S}, @var{MS})} creates
    ## a duration array from numeric arrays containing the number of hours,
    ## minutes, seconds, and milliseconds specified by @var{H}, @var{MI},
    ## @var{S}, and @var{MS}, respectively.
    ##
    ## @code{@var{D} =} duration (@var{TimeStrings})} creates a duration array
    ## from text that represents elapsed times.  @var{TimeStrings} can be a
    ## character vector, a cell array of character vectors, or a string array
    ## representing times using either the @qcode{'hh:mm:ss'} or the
    ## @qcode{'dd:hh:mm:ss'} format.
    ##
    ## @code{@var{D} =} duration (@var{TimeStrings}, @qcode{'InputFormat'},
    ## @var{INFMT})} creates a duration array from text that represents elapsed
    ## times according to the format specified by @var{INFMT}, which can be any
    ## of the following:
    ##
    ## @itemize
    ## @item @qcode{'dd:hh:mm:ss'}
    ## @item @qcode{'hh:mm:ss'}
    ## @item @qcode{'mm:ss'}
    ## @item @qcode{'hh:mm'}
    ## @item Any of the first three formats can also be appended with up to nine
    ## @qcode{S} characters to indicate fractional second digits, such as
    ## @qcode{'dd:hh:mm:ss.SS'} or @qcode{'mm:ss.SS'}.
    ## @end itemize
    ##
    ## @code{@var{D} = duration (@dots{}, @qcode{'Format'}, @var{FMT})}
    ## specifies the format in which @var{D} is displayed.  @var{FMT} can
    ## specify either a digital timer, which can have any of the valid formats
    ## for @qcode{'InputFormat'} as shown above or a single number with time
    ## units by specifying one of the following:
    ##
    ## @itemize
    ## @item @qcode{'y'} fixed-length years (1 year equals 365.2425 days)
    ## @item @qcode{'d'} fixed-length days (1 day equals 24 hours)
    ## @item @qcode{'h'} hours
    ## @item @qcode{'m'} minutes
    ## @item @qcode{'s'} seconds
    ## @end itemize
    ##
    ## @code{@var{D} = duration ()} returns a scalar array of durations with an
    ## elapsed time value of zero.  To create an empty duration array, use
    ## @code{duration ([], [], [])}.
    ##
    ## @seealso{years, days, hours, minutes, seconds, milliseconds, duration,
    ## isduration, calendarDuration, datetime}
    ## @end deftypefn
    function this = duration (varargin)

      ## Return a scalar duration object
      if (nargin == 0)
        return
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'Format', 'InputFormat'};
      dfValues = {[], []};
      [Format, inputFormat, args] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check optional 'Format' and 'InputFormat' arguments
      if (! isempty (Format))
        if (! (ischar (Format) && isvector (Format)))
          error ("duration: 'Format' must be a character vector.");
        else
          errmsg = checkFormatString (Format);
          if (! isempty (errmsg))
            error ("duration: %s", errmsg);
          endif
          this.Format = Format;
        endif
      endif
      if (! isempty (inputFormat))
        if (! (ischar (inputFormat) && isvector (inputFormat)))
          error ("duration: 'InputFormat' must be a character vector.");
        else
          errmsg = checkInputFormatString (inputFormat);
          if (! isempty (errmsg))
            error ("duration: %s", errmsg);
          endif
        endif
      endif

      ## Parse inputs
      switch (numel (args))

        ## this = duration ()
        case 0
          return

        ## this = duration (X)
        case 1
          X = args{1};
          if (isnumeric (X))
            if (! ismatrix (X))
              error ("duration: numeric X must be a matrix.");
            endif
            if (! isreal (X))
              error ("duration: numeric X must be real.");
            endif
            if (size (X, 2) == 3)
              H  = X(:,1);
              MI = X(:,2);
              S  = X(:,3);
            else
              error ("duration: X must have 3 columns.");
            endif
            [~, this.Days] = hms2days (H, MI, S);

          elseif (iscellstr (X) || ischar (X) || isa (X, "string"))
            if (! iscellstr (X))
              X = cellstr (X);
            endif
            this.Days = timestrings2days (X, inputFormat);
          endif

        ## this = duration (H, MI, S)
        case 3
          [H, MI, S] = args{:};
          if (! (isnumeric (H) && isnumeric (MI) && isnumeric (S)))
            error ("duration: Y, M, and D must be a numeric arrays.");
          endif
          if (! (isreal (H) && isreal (MI) && isreal (S)))
            error ("duration: Y, M, and D must be a real.");
          endif
          ## Expansion is handled by the helper function
          [err, days] = hms2days (H, MI, S);
          if (err > 0)
            error ("duration: H, MI, and S must be of common size or scalars.");
          endif
          this.Days = days;

        ## this = duration (H, MI, S, MS)
        case 4
          [H, MI, S, MS] = args{:};
          if (! (isnumeric (H) && isnumeric (MI) && isnumeric (S) && isnumeric (MS)))
            error ("duration: H, MI, S, and MS must be a numeric arrays.");
          endif
          if (! (isreal (H) && isreal (MI) && isreal (S) && isreal (MS)))
            error ("duration: H, MI, S, and MS must be a real.");
          endif
          ## Expansion is handled by the helper function
          [err, days] = hms2days (H, MI, S, MS);
          if (err > 0)
            error ("duration: H, MI, S, and MS must be of common size or scalars.");
          endif
          this.Days = days;

        otherwise
          error ("duration: invalid number of input arguments.");

      endswitch

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{cstr} =} dispstrings (@var{D})
    ##
    ## Get display formatted strings for each element of a duration array.
    ##
    ## @code{@var{cstr} = dispstrings (@var{D})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## duration @var{D}.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      ## Get display format
      fmt = strsplit (this.Format, '.')';
      if (numel (fmt) == 1)
        fmt = fmt{1};
        fracSec = false;
      else
        fmt = fmt{1};
        fracSec = true;
        fdigits = numel (fmt{2});
      endif
      ## Process all elements
      sz = size (this);
      cstr = cell (sz);
      for i = 1:prod (sz)
        d = this.Days(i);
        ## Handle NaNs and Infs early
        if (isnan (d))
          cstr{i} = 'NaN';
        elseif (isinf (d))
          cstr{i} = num2str (d);
        else
          ## Get sign for positive/negative duration
          str = '';
          if (d < 0)
            str = [str, '-'];
            #d = abs(d);
          endif
          ## Build string according to display format
          if (strcmp (fmt, 'y'))
            years = abs (d / 365.2425);
            if (years == 1)
              str = [str, sprintf('%g yr', years)];
            else
              str = [str, sprintf('%g yrs', years)];
            endif
          elseif (strcmp (fmt, 'd'))
            days = abs (d);
            if (days == 1)
              str = [str, sprintf('%g day', days)];
            else
              str = [str, sprintf('%g days', days)];
            endif
          elseif (strcmp (fmt, 'h'))
            str = [str, sprintf('%g hr', abs (d * 24))];
          elseif (strcmp (fmt, 'm'))
            str = [str, sprintf('%g min', abs (d * 1440))];
          elseif (strcmp (fmt, 's'))
            str = [str, sprintf('%g sec', abs (d * 86400))];
          elseif (strcmp (fmt, 'hh:mm'))
            x = d * 1440;
            h = abs (fix (x / 60));
            m = abs (fix (rem (x, 60)));
            str = [str, sprintf('%d:%d', h, m)];
          elseif (strcmp (fmt, 'mm:ss'))
            x = d * 86400;
            m = abs (fix (x / 60));
            s = abs (floor (rem (x, 60)));
            str = [str, sprintf('%d:%d', m, s)];
            if (fracSec)
              fs = rem (x, 60) - s; # fraction of a second
              ## Promote to integer value according to requested digits
              ms = abs (fix (fs * 10 ^ fdigits));
              str = [str, sprintf('.%d', ms)];
            endif
          elseif (strcmp (fmt, 'hh:mm:ss'))
            x = d * 86400;
            h = fix (x / 3600);
            x = x - h * 3600;
            m = fix (x / 60);
            x = x - m * 60;
            if (abs (x) - abs (round (x)) < 1e-10)
              x = round (1e10 * x) / 1e10;
            endif
            s = floor (x);
            if (abs (s) - abs (x) > 1e-8)
              s = fix (x);
            endif
            str = [str, sprintf('%02d:%02d:%02d', abs (h), abs (m), abs (s))];
            if (fracSec)
              fs = x - s; # fraction of a second
              ## Promote to integer value according to requested digits
              ms = abs (fix (fs * 10 ^ fdigits));
              str = [str, sprintf('.%d', ms)];
            endif
          elseif (strcmp (fmt, 'dd:hh:mm:ss'))
            x = d - fix (d);
            x = x * 86400;
            h = fix (x / 3600);
            x = x - h * 3600;
            m = fix (x / 60);
            x = x - m * 60;
            if (abs (x) - abs (round (x)) < 1e-10)
              x = round (1e10 * x) / 1e10;
            endif
            s = floor (x);
            if (abs (s) - abs (x) > 1e-8)
              s = fix (x);
            endif
            if (fix (d) > 0)
              str = [str, sprintf('%02d:%02d:%02d:%02d', ...
                                  abs (fix (d)), abs (h), abs (m), abs (s))];
            else
              str = [str, sprintf('%02d:%02d:%02d', h, m, s)];
            endif
            if (fracSec)
              fs = x - s; # fraction of a second
              ## Promote to integer value according to requested digits
              ms = abs (fix (fs * 10 ^ fdigits));
              str = [str, sprintf('.%d', ms)];
            endif
          endif
          cstr{i} = str;
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{cstr} =} cellstr (@var{D})
    ## @deftypefnx {duration} {@var{cstr} =} cellstr (@var{D}, @var{Format})
    ##
    ## Convert duration array to a cell array of character vectors.
    ##
    ## @code{@var{cstr} = cellstr (@var{D})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## duration @var{D}.
    ##
    ## @end deftypefn
    function cstr = cellstr (this, Format = '')
      if (! isempty (Format))
        if (! (ischar (Format) && isvector (Format)))
          error ("duration.cellstr: FORMAT must be a character vector.");
        else
          errmsg = checkFormatString (Format);
          if (! isempty (errmsg))
            error ("duration.cellstr: %s", errmsg);
          endif
          this.Format = Format;
        endif
      endif
      cstr = dispstrings (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{cmat} =} char (@var{D})
    ##
    ## Convert duration array to a character matrix.
    ##
    ## @code{@var{cmat} = char (@var{D})} returns a character matrix with one
    ## row per element in @var{D}.
    ##
    ## @end deftypefn
    function cmat = char (this, Format = '')
      cmat = char (cellstr (this, Format));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{DV} =} datevec (@var{DT})
    ## @deftypefnx {duration} {[@var{Y}, @var{MO}, @var{D}, @var{h}, @var{mi}, @var{s}] =} datevec (@var{DT})
    ##
    ## Convert duration array to date vectors.
    ##
    ## @code{@var{DV} = datevec (@var{DT})} returns an @math{Nx6} numeric matrix
    ## whose rows represent each element in @var{DT} and each column corresponds
    ## to years, months, days, hours, minutes, and seconds, respectively.  Since
    ## months cannot be represented as a fixed length of time, the second column
    ## of @var{DV} is always zero.  @var{DV} represents a length of time split
    ## accross different fixed-length elapsed time units.
    ##
    ## @code{[@var{Y}, @var{MO}, @var{D}, @var{h}, @var{mi}, @var{s}] = datevec
    ## (@var{DT})} returns the components of @var{DT} as individual variables,
    ## but unlike @var{DV} in the previous syntax, each variable has the same
    ## size as the duration array @var{DT}.
    ##
    ## @end deftypefn
    function varargout = datevec (this)
      d = this.Days;
      y = fix (d / 365.2425);
      x = rem (d, 365.2425);
      d = fix (x);
      x = x - d;
      x = x * 86400;
      h = fix (x / 3600);
      x = x - h * 3600;
      m = fix (x / 60);
      s = x - m * 60;
      DV = [y, 0, d, h, m, s];
      if (nargout == 0 || nargout == 1)
        varargout{1} = DV;
      elseif (nargout <= 6)
        for i = 1:nargout
          varargout{i} = reshape (DV(:,i), size (this));
        endfor
      else
        error ("duration.datavec: too many output arguments.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{H} =} hms (@var{D})
    ## @deftypefnx {duration} {[@var{H}, @var{M}] =} hms (@var{D})
    ## @deftypefnx {duration} {[@var{H}, @var{M}, @var{S}] =} hms (@var{D})
    ##
    ## Split duration array into separate time unit values.
    ##
    ## @code{[@var{H}, @var{M}, @var{S}] = hms (@var{D})} splits the duration
    ## array @var{D} into separate numeric arrays @var{H}, @var{M}, and @var{S},
    ## which correspond to hours, minutes, and seconds, repsectively.  Hours and
    ## minutes are returned as whole numbers, while seconds may also have a
    ## fractional part.
    ##
    ## @end deftypefn
    function varargout = hms (this)
      x = this.Days * 86400;
      h = fix (x / 3600);
      x = x - h * 3600;
      m = fix (x / 60);
      s = x - m * 60;
      if (nargout == 0 || nargout == 1)
        varargout{1} = h;
      elseif (nargout == 2)
        varargout{1} = h;
        varargout{2} = m;
      elseif (nargout == 3)
        varargout{1} = h;
        varargout{2} = m;
        varargout{3} = s;
      else
        error ("duration.hms: too many output arguments.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{X} =} years (@var{D})
    ##
    ## Duration equivalent numeric values in fixed-length years.
    ##
    ## @code{@var{X} = years (@var{D})} converts durations in @var{D} to the
    ## equivalent number of fixed-length years (1 year equals 365.2425 days).
    ## @var{X} is a double array of the same size as @var{D}.
    ##
    ## @end deftypefn
    function out = years (this)
      out = this.Days / 365.2425;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{X} =} days (@var{D})
    ##
    ## Duration equivalent numeric values in fixed-length days.
    ##
    ## @code{@var{X} = days (@var{D})} converts durations in @var{D} to the
    ## equivalent number of fixed-length days (1 day equals 24 hours).  @var{X}
    ## is a double array of the same size as @var{D}.
    ##
    ## @end deftypefn
    function out = days (this)
      out = this.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{X} =} hours (@var{D})
    ##
    ## Duration equivalent numeric values in hours.
    ##
    ## @code{@var{X} = hours (@var{D})} converts durations in @var{D} to the
    ## equivalent number of hours.  @var{X} is a double array of the same size
    ## as @var{D}.
    ##
    ## @end deftypefn
    function out = hours (this)
      out = this.Days * 24;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{X} =} minutes (@var{D})
    ##
    ## Duration equivalent numeric values in minutes.
    ##
    ## @code{@var{X} = minutes (@var{D})} converts durations in @var{D} to the
    ## equivalent number of minutes.  @var{X} is a double array of the same size
    ## as @var{D}.
    ##
    ## @end deftypefn
    function out = minutes (this)
      out = this.Days * 1440;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{X} =} seconds (@var{D})
    ##
    ## Duration equivalent numeric values in seconds.
    ##
    ## @code{@var{X} = seconds (@var{D})} converts durations in @var{D} to the
    ## equivalent number of seconds.  @var{X} is a double array of the same size
    ## as @var{D}.
    ##
    ## @end deftypefn
    function out = seconds (this)
      out = this.Days * 86400;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{X} =} milliseconds (@var{D})
    ##
    ## Duration equivalent numeric values in milliseconds.
    ##
    ## @code{@var{X} = milliseconds (@var{D})} converts durations in @var{D} to
    ## the equivalent number of milliseconds.  @var{X} is a double array of the
    ## same size as @var{D}.
    ##
    ## @end deftypefn
    function out = milliseconds (this)
      out = this.Days * 86400000;
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'size'             'ndims'            'numel'            'histcounts'      ##
## 'min'              'max'              'mink'             'maxk'            ##
## 'nnz'              'plot'                                                  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{sz} =} size (@var{D})
    ## @deftypefnx {duration} {@var{dim_sz} =} size (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{dim_sz} =} size (@var{D}, @var{d1}, @var{d2}, @dots{})
    ## @deftypefnx {duration} {[@var{rows}, @var{columns}, @dots{}, @var{dim_n_sz}] =} size (@dots{})
    ##
    ## Return the size of a duration array.
    ##
    ## @code{@var{sz} = size (@var{D})} returns a row vector with the size
    ## (number of elements) of each dimension for the duration array @var{D}.
    ##
    ## @code{@var{dim_sz} = size (@var{D}, @var{dim})} returns the size of
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
        sz = size (this.Days, varargin{:});
      else
        sz = size (this.Days);
      endif
      if (nargout == 0 || nargout == 1)
        varargout{1} = sz;
      elseif (numel (sz) != nargout)
        error (strcat ("duration.size: nargout > 1 but does not", ...
                       " match number of requested dimensions."));
      else
        for i = 1:nargout
          varargout{i} = sz(i);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{out} =} ndims (@var{D})
    ##
    ## Number of dimensions in a duration array.
    ##
    ## @code{@var{out} = ndims (@var{D})} returns the number of dimensions of
    ## the duration array @var{D}.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = ndims (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{out} =} numel (@var{D})
    ##
    ## Total number of elements in a duration array.
    ##
    ## For compatibility reasons with Octave's OOP interface and @code{subsasgn}
    ## behavior, duration's @code{numel} is defined to always return 1.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = 1;
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'isbetween'        'iscolumm'         'isempty'          'isequal'         ##
## 'isequaln'         'isfinite'         'isinf'            'ismatrix'        ##
## 'ismember'         'ismissing'        'isnan'            'isregular'       ##
## 'isrow'            'isscalar'         'issorted'         'issortedrows'    ##
## 'isvector'                                                                 ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function TF = isbetween (this, lower, upper, intervaltype = 'closed')
      if (nargin < 3)
        error ("duration.isbetween: too few input arguments.");
      endif
      [lower, upper] = promote (lower, upper);
      if (strcmpi (intervaltype, 'closed'))
        TF = lower <= this & this <= upper;
      elseif (strcmpi (intervaltype, 'open'))
        TF = lower < this & this < upper;
      elseif (any (strcmpi (intervaltype, {'openleft', 'closedright'})))
        TF = lower < this & this <= upper;
      elseif (any (strcmpi (intervaltype, {'openright', 'closedleft'})))
        TF = lower <= this & this < upper;
      else
        error ("duration.isbetween: invalid INTERVALTYPE option.");
      endif
    endfunction

    function TF = iscolumn (this)
      TF = iscolumn (this.Days);
    endfunction

    function TF = isempty (this)
      TF = isempty (this.Days);
    endfunction

    function TF = isequal (varargin)
      args = varargin;
      [args{:}] = promote (varargin{:});
      days = cellfun (@(obj) obj.Days, args, 'UniformOutput', false);
      TF = isequal (days{:});
    endfunction

    function TF = isequaln (varargin)
      args = varargin;
      [args{:}] = promote (varargin{:});
      days = cellfun (@(obj) obj.Days, args, 'UniformOutput', false);
      TF = isequal (days{:});
    endfunction

    function TF = isfinite (this)
      TF = isfinite (this.Days);
    endfunction

    function TF = isinf (this)
      TF = isinf (this.Days);
    endfunction

    function TF = ismatrix (this)
      TF = ismatrix (this.Days);
    endfunction

    function [TF, index] = ismember (A, B, varargin)
      ## Check input arguments
      do_rows = false;
      if (! isempty (varargin))
        if (strcmpi (varargin{1}, 'rows'))
          do_rows = true;
          if (ndims (A) != 2 || ndims (A) != ndims (B))
            error ("duration.ismember: 'rows' applies only to 2-D matrices.");
          endif
          if (size (A, 2) != size (B, 2))
            error ("duration.ismember: 'rows' requires same number of columns.");
          endif
        else
          error ("duration.ismember: invalid optional argument.");
        endif
      endif
      if (! isa (B, 'duration'))
        error ("duration.ismember: B must be a 'duration' array.");
      endif
      ## Find ismember
      if (do_rows)
        [TF, index] = ismember (A.Days, B.Days, 'rows');
      else
        [TF, index] = ismember (A.Days, B.Days);
      endif
    endfunction

    function TF = ismissing (this, varargin)
      if (nargin > 1)
        error ("duration.ismissing: too many input arguments.");
      endif
      if (! isempty (varargin))
        if (! isa (varargin{1}, 'categorical'))
          error (strcat ("duration.ismissing: INDICATOR", ...
                         " argument must be of 'duration' type."));
        endif
        indicator = varargin{1};
        TF = false (size (this));
        for i = 1:numel (indicator)
          days = days (indicator(i));
          TF(this.Days == days) = true;
        endfor
      else
        TF = this.IsNaN;
      endif
      TF = isnan (this.Days);
    endfunction

    function TF = isnan (this)
      TF = isnan (this.Days);
    endfunction

    function TF = isrow (this)
      TF = isrow (this.Days);
    endfunction

    function TF = isscalar (this)
      TF = isscalar (this.Days);
    endfunction

    function TF = issorted (this, varargin)
      if (isempty (varargin))
        sorted = sort (this);
      else
        sorted = sort (this, varargin{:});
      endif
      TF = isequal (this.Days, sorted.Days);
    endfunction

    function TF = issortedrows (this, varargin)
      if (isempty (varargin))
        sorted = sortrows (this);
      else
        sorted = sortrows (this, varargin{:});
      endif
      TF = isequal (this.Days, sorted.Days);
    endfunction

    function TF = isvector (this)
      TF = isvector (this.Days);
    endfunction

  endmethods

################################################################################
##                        ** Relational Operations **                         ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'eq'               'ge'               'gt'               'le'              ##
## 'lt'               'ne'                                                    ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function TF = eq (A, B)
      [A, B] = promote (A, B);
      TF = A.Days == B.Days;
    endfunction

    function TF = ge (A, B)
      [A, B] = promote (A, B);
      TF = A.Days >= B.Days;
    endfunction

    function TF = gt (A, B)
      [A, B] = promote (A, B);
      TF = A.Days > B.Days;
    endfunction

    function TF = le (A, B)
      [A, B] = promote (A, B);
      TF = A.Days <= B.Days;
    endfunction

    function TF = lt (A, B)
      [A, B] = promote (A, B);
      TF = A.Days < B.Days;
    endfunction

    function TF = ne (A, B)
      [A, B] = promote (A, B);
      TF = A.Days != B.Days;
    endfunction

  endmethods

################################################################################
##                       ** Mathematical Operations **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'abs'              'plus'             'uplus'            'minus'           ##
## 'uminus'           'times'            'mtimes'           'ldivide'         ##
## 'mldivide'         'rdivide'          'mrdivide'         'colon'           ##
## 'linspace'         'interp1'          'sum'              'cumsum'          ##
## 'diff'             'mean'             'median'           'mode'            ##
## 'floor'            'ceil'             'round'            'sign'            ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function B = abs (A)
      B = A;
      B.Days = abs (A.Days);
    endfunction

    function C = plus (A, B)
      if (isa (A, 'duration') && isa (B, 'duration'))
        C = A;
        C.Days = A.Days + B.Days;
      elseif (isnumeric (A))
        C = B;
        C.Days = B.Days + double (A);
      elseif (isnumeric (B))
        C = A;
        C.Days = A.Days + double (B);
      else
        error (strcat ("duration: addition is not defined between", ...
                       " '%s' and '%s' arrays."), class (A), class (B));
      endif
      C = fix_zero_precision (C);
    endfunction

    function B = uplus (A)
      B = A;
      B.Days = A.Days;
    endfunction

    function C = minus (A, B)
      if (isa (A, 'duration') && isa (B, 'duration'))
        C = A;
        C.Days = A.Days - B.Days;
      elseif (isnumeric (A))
        C = B;
        C.Days = B.Days - double (A);
      elseif (isnumeric (B))
        C = A;
        C.Days = A.Days - double (B);
      else
        error (strcat ("duration: subtraction is not defined between", ...
                       " '%s' and '%s' arrays."), class (A), class (B));
      endif
      C = fix_zero_precision (C);
    endfunction

    function B = uminus (A)
      B = A;
      B.Days = - A.Days;
    endfunction

    function C = times (A, B)
      if (isa (A, 'duration') && isnumeric (B))
        C = A;
        C.Days = A.Days .* double (B);
      elseif (isnumeric (A) && isa (B, 'duration'))
        C = B;
        C.Days = double (A) .* B.Days;
      else
        error (strcat ("duration: multiplication is not defined between", ...
                       " '%s' and '%s' arrays."), class (A), class (B));
      endif
    endfunction

    function C = mtimes (A, B)
      if (isa (A, 'duration') && isnumeric (B))
        C = A;
        C.Days = A.Days * double (B);
      elseif (isnumeric (A) && isa (B, 'duration'))
        C = B;
        C.Days = double (A) * B.Days;
      else
        error (strcat ("duration: matrix multiplication is not defined", ...
                       " between '%s' and '%s' arrays."), class (A), class (B));
      endif
    endfunction

    function C = ldivide (A, B)
      if (! isa (B, 'duration'))
        error (strcat ("duration: right-hand side must be a duration", ...
                       " array for left division: got '%s'"), class (B));
      endif
      if (isa (A, 'duration'))
        C = A.Days .\ B.Days;
      elseif (isnumeric (A))
        C = B;
        C.Days = double (A) .\ B.Days;
      else
        error (strcat ("duration: left division is not defined", ...
                       " between '%s' and 'duration' arrays"), class (A));
      endif
      C = fix_zero_precision (C);
    endfunction

    function C = mldivide (A, B)
      if (! isa (B, 'duration'))
        error (strcat ("duration: right-hand side must be a duration", ...
                       " array for matrix left division: got '%s'"), class (B));
      endif
      if (isa (A, 'duration'))
        C = A.Days \ B.Days;
      elseif (isnumeric (A))
        C = B;
        C.Days = double (A) \ B.Days;
      else
        error (strcat ("duration: matrix left division is not defined", ...
                       " between '%s' and 'duration' arrays"), class (A));
      endif
      C = fix_zero_precision (C);
    endfunction

    function C = rdivide (A, B)
      if (! isa (A, 'duration'))
        error (strcat ("duration: left-hand side must be a duration", ...
                       " array for right division: got '%s'"), class (A));
      endif
      if (isa (B, 'duration'))
        C = A.Days ./ B.Days;
      elseif (isnumeric (B))
        C = A;
        C.Days = A.Days ./ double (B);
      else
        error (strcat ("duration: right division is not defined", ...
                       " between 'duration' and '%s' arrays"), class (B));
      endif
      C = fix_zero_precision (C);
    endfunction

    function C = mrdivide (A, B)
      if (! isa (A, 'duration'))
        error (strcat ("duration: left-hand side must be a duration", ...
                       " for matrix right division: got '%s'"), class (A));
      endif
      if (isa (B, 'duration'))
        C = A.Days / B.Days;
      elseif (isnumeric (B))
        C = A;
        C.Days = A.Days / double (B);
      else
        error (strcat ("duration: matrix right division is not defined", ...
                       " between 'duration' and '%s' arrays"), class (B));
      endif
      C = fix_zero_precision (C);
    endfunction

    function C = colon (varargin)
      if (nargin < 2 || nargin > 3)
        error ("duration.colon: invalid number of input arguments.");
      endif
      if (! all (cellfun ('isscalar', varargin)))
        error ("duration.colon: input arguments must be scalars.");
      endif
      if (nargin == 2)
        [from, to] = promote (varargin{:});
        increment = days (1);
      else
        [from, increment, to] = promote (varargin{:});
      endif
      C = from;
      C.Days = from.Days:increment.Days:to.Days;
      C = fix_zero_precision (C);
    endfunction

    function C = linspace (A, B, n = 100)
      if (nargin < 2 || nargin > 3)
        error ("duration.linspace: invalid number of input arguments.");
      endif
      if (! isscalar (A) || ! isscalar (B))
        error ("duration.linspace: A and B must be scalars.");
      endif
      [A, B] = promote (A, B);
      C = A;
      C.Days = linspace (A.Days, B.Days, n);
      C = fix_zero_precision (C);
    endfunction

    function YI = interp1 (X, Y, XI, varargin)
      if (isa (Y, 'duration'))
        YI = Y;
        YI.Days = interp1 (X.Days, Y.Days, XI.Days, varargin{:});
        YI = fix_zero_precision (YI);
      else
        YI = interp1 (X.Days, Y, XI.Days, varargin{:});
      endif
    endfunction

    function S = sum (A, varargin)
      dim = [];
      if (! isempty (varargin))
        tmp = varargin{end};
        if (ischar (tmp) && isvector (tmp))
          if (strcmpi (tmp, 'omitnan'))
            A.Days(isnan (A)) = 0;
            varargin(end) = [];
          elseif (strcmpi (tmp, 'includenan'))
            includenan = true;
            varargin(end) = [];
          endif
        elseif (isa (tmp, 'string') && isscalar (tmp))
          if (strcmpi (tmp, 'omitnan'))
            A.Days(isnan (A)) = 0;
            varargin(end) = [];
          elseif (strcmpi (tmp, 'includenan'))
            includenan = true;
            varargin(end) = [];
          endif
        endif
      endif
      if (! isempty (varargin))
        if (strcmpi (varargin{end}, 'all'))
          dim = 'all';
        elseif (isnumeric (varargin{end}) && isscalar (varargin{end}))
          dim = varargin{end};
        endif
      endif
      S = A;
      if (isempty (dim))
        S.Days = sum (A.Days);
      elseif (isnumeric (dim))
        S.Days = sum (A.Days, dim);
      else
        S.Days = sum (A.Days(:));
      endif
      S = fix_zero_precision (S);
    endfunction

    function S = cumsum (A, varargin)
      dim = [];
      if (! isempty (varargin))
        tmp = varargin{end};
        if (ischar (tmp) && isvector (tmp))
          if (strcmpi (tmp, 'omitnan'))
            A.Days(isnan (A)) = 0;
            varargin(end) = [];
          elseif (strcmpi (tmp, 'includenan'))
            varargin(end) = [];
          endif
        elseif (isa (tmp, 'string') && isscalar (tmp))
          if (strcmpi (tmp, 'omitnan'))
            A.Days(isnan (A)) = 0;
            varargin(end) = [];
          elseif (strcmpi (tmp, 'includenan'))
            varargin(end) = [];
          endif
        endif
      endif
      if (! isempty (varargin))
        if (isnumeric (varargin{end}) && isscalar (varargin{end}))
          dim = varargin{end};
        endif
      endif
      S = A;
      if (isempty (dim))
        S.Days = sum (A.Days);
      else
        S.Days = cumsum (A.Days, dim);
      endif
      S = fix_zero_precision (S);
    endfunction

    function DT = diff (D, varargin)
      DT = D;
      DT.Days = diff (D.Days, varargin{:});
    endfunction

    function M = mean (D, varargin)
      M = D;
      M.Days = mean (D.Days, varargin{:});
    endfunction

    function M = median (D, varargin)
      M = D;
      M.Days = median (D.Days, varargin{:});
    endfunction

    function M = mode (D, varargin)
      M = D;
      M.Days = mode (D.Days, varargin{:});
    endfunction

    function B = floor (A, unit = 'seconds')
      B = A;
      if (strcmpi (unit, 'seconds'))
        B.Days = floor (seconds (A)) / 86400;
      elseif (strcmpi (unit, 'minutes'))
        B.Days = floor (minutes (A)) / 1440;
      elseif (strcmpi (unit, 'hours'))
        B.Days = floor (hours (A)) / 24;
      elseif (strcmpi (unit, 'days'))
        B.Days = floor (A.Days);
      elseif (strcmpi (unit, 'years'))
        B.Days = floor (years (A)) * 365.2425;
      else
        error ("duration.floor: invalid UNIT.");
      endif
    endfunction

    function B = ceil (A, unit = 'seconds')
      B = A;
      if (strcmpi (unit, 'seconds'))
        B.Days = ceil (seconds (A)) / 86400;
      elseif (strcmpi (unit, 'minutes'))
        B.Days = ceil (minutes (A)) / 1440;
      elseif (strcmpi (unit, 'hours'))
        B.Days = ceil (hours (A)) / 24;
      elseif (strcmpi (unit, 'days'))
        B.Days = ceil (A.Days);
      elseif (strcmpi (unit, 'years'))
        B.Days = ceil (years (A)) * 365.2425;
      else
        error ("duration.ceil: invalid UNIT.");
      endif
    endfunction

    function B = round (A, unit = 'seconds')
      B = A;
      if (strcmpi (unit, 'seconds'))
        B.Days = round (seconds (A)) / 86400;
      elseif (strcmpi (unit, 'minutes'))
        B.Days = round (minutes (A)) / 1440;
      elseif (strcmpi (unit, 'hours'))
        B.Days = round (hours (A)) / 24;
      elseif (strcmpi (unit, 'days'))
        B.Days = round (A.Days);
      elseif (strcmpi (unit, 'years'))
        B.Days = round (years (A)) * 365.2425;
      else
        error ("duration.round: invalid UNIT.");
      endif
    endfunction

    function out = sign (A)
      out = sign (A.Days);
    endfunction

  endmethods

################################################################################
##                   ** Sort, Filter, and Set Operations **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'sort'             'sortrows'         'unique'           'intersect'       ##
## 'setdiff'          'setxor'           'union'                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function [B, index] = sort (A, varargin)
      B = A;
      [B.Days, index] = sort (A.Days, varargin{:});
    endfunction

    function [B, index] = sortrows (A, varargin)
      if (ndims (A) != 2)
        error ("duration.sortrows: A must be a 2-D matrix.");
      endif
      col_dir = false;
      if (numel (varargin) > 0)
        col = varargin{1};
        if (isnumeric (col))
          if (! isvector (col) || fix (col) != col)
            error ("duration.sortrows: COL must be a vector of integers.");
          endif
        elseif ((ischar (col) && isvector (col)) ||
                (isscalar (col) && isa (col, 'string')))
          col = cellstr (col);
          if (strcmpi (col, 'ascend'))
            col = [1:size(A, 2)];
          elseif (strcmpi (col, 'descend'))
            col = -[1:size(A, 2)];
          else
            error (strcat ("duration.sortrows: DIRECTION can", ...
                           " be either 'ascend' or 'descend'."));
          endif
        else
          error ("duration.sortrows: invalid value for COL argument.");
        endif
        col_dir = true;
      endif
      if (numel (varargin) > 1)
        direction = cellstr (varargin{2});
        if (! all (ismember (direction, {'ascend', 'descend'})))
          error ("duration.sortrows: invalid value for DIRECTION argument.");
        endif
        if (isscalar (direction) && strcmpi (direction, 'ascend'))
          col = abs (col);
        elseif (isscalar (direction) && strcmpi (direction, 'descend'))
          col = - abs (col);
        else
          if (numel (direction) != numel (col))
            error ("duration.sortrows: DIRECTION does not match COL argument.");
          endif
          col = abs (col);
          idx = strcmpi (direction, 'descend');
          col(idx) = - col(idx);
        endif
      endif
      B = A;
      if (col_dir)
        [B.Days, index] = sortrows (A.Days, col);
      else
        [B.Days, index] = sortrows (A.Days);
      endif
    endfunction

    function [B, ixA, ixB] = unique (A, varargin)
      ## Handle 'rows' option
      do_rows = false;
      if (! isempty (varargin))
        idx = strcmpi ('rows', varargin(:));
        if (any (idx))
          do_rows = true;
          varargin(idx) = [];
          if (ndims (A) != 2)
            error ("duration.unique: 'rows' applies only to 2-D matrices.");
          endif
        endif
      endif
      ## Handle 'setOrder' and 'occurence' options
      opt = "sorted";
      if (! isempty (varargin))
        if (any (strcmp (varargin{1}, {"sorted", "stable", "first", "last"})))
          opt = varargin{1};
        else
          error ("duration.unique: invalid option '%s'.", varargin{1});
        endif
      endif
      ## Find unique
      if (do_rows)
        [~, ixA, ixB] = __unique__ (A.Days, 'rows', opt);
        B = subset (A, ixA, ':');
      else
        [~, ixA, ixB] = __unique__ (A.Days, opt);
        B = subset (A, ixA);
      endif
    endfunction

    function [C, ixA, ixB] = intersect (A, B, varargin)
      [~, ixA, ixB] = intersect (A.Days, B.Days, varargin{:});
      C = subset (A, ixA);
    endfunction

    function [C, index] = setdiff (A, B, varargin)
      [~, index] = setdiff (A.Days, B.Days, varargin{:});
      C = subset (A, index);
    endfunction

    function [C, ixA, ixB] = setxor (A, B, varargin)
      [~, ixA, ixB] = setxor (A.Days, B.Days, varargin{:});
      C = subset (A, ixA);
    endfunction

    function [C, ixA, ixB] = union (A, B, varargin)
      [~, ixA, ixB] = union (A.Days, B.Days, varargin{:});
      C = subset (A, ixA);
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
      [args{:}] = promote (varargin{:});
      out = args{1};
      days = cellfun (@(obj) obj.Days, args, 'UniformOutput', false);
      out.Days = cat (dim, days{:});
    endfunction

    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    function this = repmat (this, varargin)
      this.Days = repmat (this.Days, varargin{:});
    endfunction

    function this = reshape (this, varargin)
      this.Days = reshape (this.Days, varargin{:});
    endfunction

    function this = circshift (this, varargin)
      this.Days = circshift (this.Days, varargin{:});
    endfunction

    function this = permute (this, order)
      this.Days = permute (this.Days, order);
    endfunction

    function this = ipermute (this, order)
      this.Days = ipermute (this.Days, order);
    endfunction

    function this = transpose (this)
      this.Days = transpose (this.Days);
    endfunction

    function this = ctranspose (this)
      this.Days = ctranspose (this.Days);
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
          out.Days = this.Days(s.subs{:});

        case '{}'
          error (strcat ("duration.subsref: '{}' invalid indexing", ...
                         " for referencing values. Use '()' instead."));

        case '.'
          switch (s.subs)
            case 'Format'
              out = this.Format;
            otherwise
              error ("duration.subsref: unrecongized property: %s", s.subs);
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
        error ("duration.subsasgn: chained subscripts not allowed.");
      endif
      switch (s.type)
        case '()'
          if (! isa (val, "duration"))
            val = duration (val);
          endif
          this.Days(s.subs{:}) = val.Days;

        case '{}'
          error (strcat ("duration.subsasgn: '{}' invalid indexing", ...
                         " for assigning values. Use '()' instead."));

        case '.'
          if (! ischar (s.subs))
            error (strcat ("calendarDuration.subsasgn: '.' index", ...
                           " argument must be a character vector."));
          endif
          switch (s.subs)
            case 'Format'
              errmsg = checkFormatString (val);
              if (! isempty (errmsg))
                error ("duration.subsasgn: %s", errmsg);
              endif
              this.Format = val;
            otherwise
              error ("duration.subsasgn: unrecongized property: %s", s.subs);
          endswitch
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Promote numeric and string arrays to duration objects
    function varargout = promote (varargin)
      for i = 1:numel (varargin)
        x = varargin{i};
        if (isa (x, "duration"))
          varargout{i} = x;
        elseif (isnumeric (x))
          ncols = size (x, 2);
          if (isscalar (x))
            varargout{i} = duration (24 * x, 0, 0);
          elseif (ismatrix (x) && ncols == 3)
            varargout{i} = duration (x);
          else
            error ("duration: invalid size input to constructor.");
          endif
        elseif (iscellstr (x) || ischar (x) || isa (x, "string"))
          varargout{i} = duration (x);
        else
          error ("duration: invalid input to constructor.");
        endif
      endfor
    endfunction

    ## Return a subset of a duration array
    function out = subset (this, varargin)
      out = this;
      out.Days = this.Days(varargin{:});
    endfunction

    ## Fix floating point precision near zero
    function this = fix_zero_precision (this)
      this.Days(this.Days > -1e-15 & this.Days < 1e-15) = 0;
    endfunction

  endmethods

endclassdef

## Parse H, M, S, and MS numeric inputs into days
function [err, days] = hms2days (H, MI, S, MS = 0)
  err = 0;
  if (! isscalar (H) || ! isscalar (MI) || ! isscalar (S) || ! isscalar (MS))
    [err, H, MI, S, MS] = common_size (H, MI, S, MS);
    if (err > 0)
      days = NaN;
      return
    endif
  endif
  H = double (H);
  MI = double (MI);
  S = double (S);
  MS = double (MS);
  days = (H / 24) + (MI / 1440) + (S / 86400) + (MS / 86400000);
endfunction

## Parse TimeString inputs into days
function days = timestrings2days (TS, inputFormat)
  days = NaN (size (TS));
  ## Find default format (either 'dd:hh:mm:ss' or 'hh:mm:ss') from 1st element
  if (isempty (inputFormat))
    str1 = TS{1};
    nCols = numel (find (str1 == ':'));
    nDots = numel (find (str1 == '.'));
    if (nDots > 1)
      error ("duration: could not recognize time string format of '%s'.", str1);
    endif
    if (nCols > 3 || nCols < 1)
      error ("duration: could not recognize time string format of '%s'.", str1);
    endif
    if (nCols == 1)
      error ("duration: time string format is ambiguous.");
    endif
    if (nDots)
      cstr = strsplit (str1, '.');
      if (! isempty (cstr{2}) && isnan (str2double (cstr{2})))
        error ("duration: could not recognize time string format of '%s'.", str1);
      endif
      cstr = strsplit (cstr{1}, ':');
      if (numel (cstr) != nCols + 1)
        error ("duration: could not recognize time string format of '%s'.", str1);
      endif
      for i = 1:nCols + 1
        if (isnan (str2double (cstr{i})))
          error ("duration: could not recognize time string format of '%s'.", str1);
        endif
      endfor
    endif
  else
    nCols = numel (find (inputFormat == ':'));
    nDots = numel (find (inputFormat == '.'));
  endif
  ## Process all elements according to inputFormat (unrecognized return NaN)
  for i = 1:numel (TS)
    str1 = TS{i};
    if (numel (find (str1 == '.')))
      cstr = strsplit (str1, '.');
      if (isempty (cstr{2}))
        MS = 0;
      else
        MSnumber = str2double (cstr{2});
        MSdigits = 10 ^ (numel (cstr{2}) - 3);
        MS = MSnumber / MSdigits;
      endif
      cstr = strsplit (cstr{1}, ':');
      if (numel (cstr) != nCols + 1)
        D = H = MI = S = NaN;
      elseif (nCols == 1) # 'mm:ss' only
        D  = 0;
        H  = 0;
        MI = str2double (cstr{1});
        S  = str2double (cstr{2});
      elseif (nCols == 2) # 'hh:mm:ss'
        D  = 0;
        H  = str2double (cstr{1});
        MI = str2double (cstr{2});
        S  = str2double (cstr{3});
      else  # 'dd:hh:mm:ss'
        D  = str2double (cstr{1});
        H  = str2double (cstr{2});
        MI = str2double (cstr{3});
        S  = str2double (cstr{4});
      endif
    else
      cstr = strsplit (str1, ':');
      if (numel (cstr) != nCols + 1)
        D = H = MI = S = MS = NaN;
      elseif (nCols == 1) # either 'mm:ss' or 'hh:mm'
        if (strcmp (inputFormat, 'mm:ss'))
          D  = 0;
          H  = 0;
          MI = str2double (cstr{1});
          S  = str2double (cstr{2});
          MS = 0;
        else  # 'hh:mm'
          D  = 0;
          H  = str2double (cstr{1});
          MI = str2double (cstr{2});
          S  = 0;
          MS = 0;
        endif
      elseif (nCols == 2) # 'hh:mm:ss'
        D  = 0;
        H  = str2double (cstr{1});
        MI = str2double (cstr{2});
        S  = str2double (cstr{3});
        MS = 0;
      else  # 'dd:hh:mm:ss'
        D  = str2double (cstr{1});
        H  = str2double (cstr{2});
        MI = str2double (cstr{3});
        S  = str2double (cstr{4});
        MS = 0;
      endif
    endif
    days(i) = D + (H / 24) + (MI / 1440) + (S / 86400) + (MS / 86400000);
  endfor
endfunction

## Check 'Format' string
function errmsg = checkFormatString (Format)
  errmsg = "";
  Format = strsplit (Format, '.')';
  validFmt = {'y', 'd', 'h', 'm', 's', 'dd:hh:mm:ss','hh:mm:ss','mm:ss','hh:mm'};
  foundFmt = ismember (validFmt, Format(1));
  if (! any (foundFmt) || numel (Format) > 2)
    errmsg = "invalid display 'Format'.";
  endif
  if (any (foundFmt([1:5])) && numel (Format) > 1)
    errmsg = "invalid display 'Format'.";
  endif
  if (foundFmt(9) && numel (Format) > 1)
    errmsg = "'hh:mm' display format cannot indicate fractional second digits.";
  endif
  if (numel (Format) == 2)
    if (any (char (Format(2)) != 'S'))
      errmsg = "invalid display 'Format' for fractional second digits.";
    endif
    if (numel (Format{2}) > 9)
      errmsg = "more than nine fractional second digits in display 'Format'.";
    endif
  endif
endfunction

## Check 'InputFormat' string
function errmsg = checkInputFormatString (inputFormat)
  errmsg = '';
  inputFormat = strsplit (inputFormat, '.');
  validInFmt = {'dd:hh:mm:ss','hh:mm:ss','mm:ss','hh:mm'};
  foundInFmt = ismember (validInFmt, inputFormat(1));
  if (! any (foundInFmt) || numel (inputFormat) > 2)
    errmsg = "invalid 'InputFormat'.";
  endif
  if (foundInFmt(4) && numel (inputFormat) > 1)
    errmsg = "'hh:mm' input format cannot indicate fractional secons digits.";
  endif
  if (numel (inputFormat) == 2)
    if (any (char (inputFormat(2)) != 'S'))
      errmsg = "invalid 'InputFormat' for fractional second digits.";
    endif
    if (numel (inputFormat{2}) > 9)
      errmsg = "more than nine fractional second digits in 'InputFormat'.";
    endif
  endif
endfunction


%!test duration;
%!test duration (1, 2, 3);
%!test assert (duration (1, 2, 3) < duration (1, 2, 4))

