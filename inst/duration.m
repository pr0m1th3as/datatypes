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

classdef duration
  ## -*- texinfo -*-
  ## @deftp {datatypes} duration
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
    ## Display format
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
      [Format, inputFormat, args] = parsePairedArguments (optNames, ...
                                                          dfValues, varargin(:));

      ## Check optional 'Format' and 'InputFormat' arguments
      if (! isempty (Format))
        ## Convert string to character vector if necessary
        if (isstring (Format))
          if (! isscalar (Format))
            error (strcat ("duration: 'Format' must be a character", ...
                           " vector or a string scalar."));
          endif
          Format = char (Format);
        elseif (! (ischar (Format) && isrow (Format)))
          error (strcat ("duration: 'Format' must be a character", ...
                         " vector or a string scalar."));
        endif
        errmsg = checkFormatString (Format);
        if (! isempty (errmsg))
          error ("duration: %s", errmsg);
        endif
        this.Format = Format;
      endif
      if (! isempty (inputFormat))
        ## Convert string to character vector if necessary
        if (isstring (inputFormat))
          if (! isscalar (inputFormat))
            error (strcat ("duration: 'InputFormat' must be a", ...
                           " character vector or a string scalar."));
          endif
          inputFormat = char (inputFormat);
        elseif (! (ischar (inputFormat) && isrow (inputFormat)))
          error (strcat ("duration: 'InputFormat' must be a character", ...
                         " vector or a string scalar."));
        endif
        errmsg = checkInputFormatString (inputFormat);
        if (! isempty (errmsg))
          error ("duration: %s", errmsg);
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
            ## Return a warning if InputFormat is defined
            if (! isempty (inputFormat))
              warning ("duration: 'InputFormat' has no effect on numeric data.");
            endif

          elseif (iscellstr (X) || ischar (X) || isa (X, "string"))
            if (! iscellstr (X))
              X = cellstr (X);
            endif
            this.Days = timestrings2days (X, inputFormat);
          else
            error ("duration: invalid type of single input data argument.");
          endif

        ## this = duration (H, MI, S)
        case 3
          [H, MI, S] = args{:};
          if (! (isnumeric (H) && isnumeric (MI) && isnumeric (S)))
            error ("duration: H, MI, and S must be numeric arrays.");
          endif
          if (! (isreal (H) && isreal (MI) && isreal (S)))
            error ("duration: H, MI, and S must be real.");
          endif
          ## Expansion is handled by the helper function
          [err, days] = hms2days (H, MI, S);
          if (err > 0)
            error ("duration: H, MI, and S must be of common size or scalars.");
          endif
          this.Days = days;
          ## Return a warning if InputFormat is defined
          if (! isempty (inputFormat))
            warning ("duration: 'InputFormat' has no effect on numeric data.");
          endif

        ## this = duration (H, MI, S, MS)
        case 4
          [H, MI, S, MS] = args{:};
          if (! (isnumeric (H) && isnumeric (MI) && isnumeric (S) && isnumeric (MS)))
            error ("duration: H, MI, S, and MS must be numeric arrays.");
          endif
          if (! (isreal (H) && isreal (MI) && isreal (S) && isreal (MS)))
            error ("duration: H, MI, S, and MS must be real.");
          endif
          ## Expansion is handled by the helper function
          [err, days] = hms2days (H, MI, S, MS);
          if (err > 0)
            error ("duration: H, MI, S, and MS must be of common size or scalars.");
          endif
          this.Days = days;
          ## Return a warning if InputFormat is defined
          if (! isempty (inputFormat))
            warning ("duration: 'InputFormat' has no effect on numeric data.");
          endif

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
        fracSec = true;
        fdigits = numel (fmt{2});
        pat = sprintf ('.%%0%dd', fdigits);
        fmt = fmt{1};
      endif
      ## Process all elements
      sz = size (this);
      cstr = cell (sz);
      for i = 1:prod (sz)
        d = this.Days(i);
        ## Handle NaNs and Infs early
        if (! isfinite (d))
          switch (fmt)
            case 'y'
              cstr{i} = sprintf ('%g yrs', d);
            case 'd'
              cstr{i} = sprintf ('%g days', d);
            case 'h'
              cstr{i} = sprintf ('%g hr', d);
            case 'm'
              cstr{i} = sprintf ('%g min', d);
            case 's'
              cstr{i} = sprintf ('%g sec', d);
            otherwise
              cstr{i} = sprintf ('%g', d);
          endswitch
        else
          ## Get sign for positive/negative duration
          str = '';
          if (d < 0)
            str = [str, '-'];
            d = abs (d);
          endif
          ## Build string according to display format
          if (strcmp (fmt, 'y'))
            years = d / 365.2425;
            if (years == 1)
              str = [str, sprintf('%g yr', years)];
            else
              str = [str, sprintf('%g yrs', years)];
            endif
          elseif (strcmp (fmt, 'd'))
            if (d == 1)
              str = [str, sprintf('%g day', d)];
            else
              str = [str, sprintf('%g days', d)];
            endif
          elseif (strcmp (fmt, 'h'))
            str = [str, sprintf('%g hr', d * 24)];
          elseif (strcmp (fmt, 'm'))
            str = [str, sprintf('%g min', d * 1440)];
          elseif (strcmp (fmt, 's'))
            str = [str, sprintf('%g sec', d * 86400)];
          elseif (strcmp (fmt, 'hh:mm'))
            ## Convert to minutes
            x = d * 1440;
            ## Calculate hours
            tmp = x / 60;
            h = fix (tmp);
            ## Fix round-off errors with threshold scaled by hours
            if (abs (tmp - h - 1) < 1e-13 * (1 + h))
              h++;
            endif
            ## Calculate remaining minutes
            tmp = x - h * 60;
            m = fix (tmp);
            ## Fix round-off errors with threshold scaled by minutes
            if (abs (tmp - m - 1) < 1e-13 * (1 + m))
              m++;
            endif
            str = [str, sprintf('%02d:%02d', h, m)];
          elseif (strcmp (fmt, 'mm:ss'))
            ## Convert to seconds
            x = d * 86400;
            ## Calculate minutes
            tmp = x / 60;
            m = fix (tmp);
            ## Fix round-off errors with threshold scaled by minutes
            if (abs (tmp - m - 1) < 1e-13 * (1 + m))
              m++;
            endif
            ## Calculate remaining seconds
            tmp = x - m * 60;
            s = fix (tmp);
            ## Fix round-off errors with threshold scaled by seconds
            if (abs (tmp - s - 1) < 1e-12 * (1 + s))
              s++;
            endif
            str = [str, sprintf('%02d:%02d', m, s)];
            if (fracSec)
              fs = rem (x, 60) - s; # fraction of a second
              ## Promote to integer value according to requested digits
              ms = abs (fix (fs * 10 ^ fdigits));
              str = [str, sprintf(pat, ms)];
            endif
          elseif (strcmp (fmt, 'hh:mm:ss'))
            ## Convert to seconds
            x = d * 86400;
            ## Calculate hours
            tmp = x / 3600;
            h = fix (tmp);
            ## Fix round-off errors with threshold scaled by hours
            if (abs (tmp - h - 1) < 1e-13 * (1 + h))
              h++;
            endif
            ## Calculate remaining duration in seconds
            x = x - h * 3600;
            ## Calculate remaining minutes
            tmp = x / 60;
            m = fix (tmp);
            ## Fix round-off errors with threshold scaled by minutes
            if (abs (tmp - m - 1) < 1e-13 * (1 + m))
              m++;
            endif
            ## Calculate remaining seconds
            x = x - m * 60;
            s = fix (x);
            ## Fix round-off errors with threshold scaled by seconds
            if (abs (x - s - 1) < 1e-12 * (1 + s))
              s++;
            endif
            str = [str, sprintf('%02d:%02d:%02d', h, m, s)];
            if (fracSec)
              fs = x - s; # fraction of a second
              ## Round to nearest nanosecond
              fs = round (fs * 1e+9) * 1e-9;
              ## Promote to integer value according to requested digits
              ms = floor (fs * 10 ^ fdigits);
              str = [str, sprintf(pat, ms)];
            endif
          elseif (strcmp (fmt, 'dd:hh:mm:ss'))
            ## Calculate days
            tmp = fix (d);
            ## Fix round-off errors with threshold scaled by days
            if (abs (d - tmp - 1) < 1e-15 * (1 + d))
              tmp++;
            endif
            x = d - tmp;
            d = tmp;
            ## Calculate remaining duration in seconds
            x = x * 86400;
            ## Calculate hours
            tmp = x / 3600;
            h = fix (tmp);
            ## Fix round-off errors with threshold scaled by hours
            if (abs (tmp - h - 1) < 1e-13 * (1 + h))
              h++;
            endif
            ## Calculate remaining duration in seconds
            x = x - h * 3600;
            ## Calculate remaining minutes
            tmp = x / 60;
            m = fix (tmp);
            ## Fix round-off errors with threshold scaled by minutes
            if (abs (tmp - m - 1) < 1e-13 * (1 + m))
              m++;
            endif
            ## Calculate remaining seconds
            x = x - m * 60;
            s = fix (x);
            ## Fix round-off errors with threshold scaled by seconds
            if (abs (x - s - 1) < 1e-12 * (1 + s))
              s++;
            endif
            if (fix (d) > 0)
              str = [str, sprintf('%02d:%02d:%02d:%02d', d, h, m, s)];
            else
              str = [str, sprintf('%02d:%02d:%02d', h, m, s)];
            endif
            if (fracSec)
              fs = x - s; # fraction of a second
              ## Round to nearest nanosecond
              fs = round (fs * 1e+9) * 1e-9;
              ## Promote to integer value according to requested digits
              ms = floor (fs * 10 ^ fdigits);
              str = [str, sprintf(pat, ms)];
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
        ## Convert string to character vector if necessary
        if (isstring (Format))
          if (! isscalar (Format))
            error (strcat ("duration.cellstr: FORMAT must be a", ...
                           " character vector or a string scalar."));
          endif
          Format = char (Format);
        elseif (! (ischar (Format) && isrow (Format)))
          error (strcat ("duration.cellstr: FORMAT must be a", ...
                         " character vector or a string scalar."));
        endif
        errmsg = checkFormatString (Format);
        if (! isempty (errmsg))
          error ("duration.cellstr: %s", errmsg);
        endif
        this.Format = Format;
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
      if (! isempty (Format))
        ## Convert string to character vector if necessary
        if (isstring (Format))
          if (! isscalar (Format))
            error (strcat ("duration.char: FORMAT must be a", ...
                           " character vector or a string scalar."));
          endif
          Format = char (Format);
        elseif (! (ischar (Format) && isrow (Format)))
          error (strcat ("duration.char: FORMAT must be a", ...
                         " character vector or a string scalar."));
        endif
        errmsg = checkFormatString (Format);
        if (! isempty (errmsg))
          error ("duration.char: %s", errmsg);
        endif
        this.Format = Format;
      endif
      cmat = char (dispstrings (this));
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
    ## across different fixed-length elapsed time units.  The number of rows in
    ## @var{DV} equals to the number of elements in the duration array @var{DT}.
    ##
    ## @code{[@var{Y}, @var{MO}, @var{D}, @var{h}, @var{mi}, @var{s}] = datevec
    ## (@var{DT})} returns the components of @var{DT} as individual variables,
    ## but unlike @var{DV} in the previous syntax, each variable has the same
    ## size as the duration array @var{DT}.
    ##
    ## Values containing a fractional portion less than 1 picosecond are rounded
    ## to the nearest picosecond.
    ##
    ## @end deftypefn
    function varargout = datevec (this)
      d = this.Days;
      tmp = d / 365.2425;
      y = fix (tmp);
      ## Fix round-off errors with threshold scaled by years
      idx = abs (tmp - y - 1) < 1e-15 * y;
      y(idx) += 1;
      x = rem (d, 365.2425);
      d = fix (x);
      x = x - d;
      ## Get remaining duration in seconds
      x = x * 86400;
      tmp = x / 3600;
      h = fix (tmp);
      idx = abs (tmp - h - 1) < 1e-15 * h;
      h(idx) += 1;
      x = x - h * 3600;
      tmp = x / 60;
      m = fix (tmp);
      idx = abs (tmp - m - 1) < 1e-15 * m;
      m(idx) += 1;
      s = x - m * 60;
      ## Fix floating point precision to nearest picosecond
      s = round (s * 1e+12) * 1e-12;
      ## Add months column
      mo = zeros (size (s));
      mo(isnan (d)) = NaN;
      ## Transform to matrix
      DV = [y(:), mo(:), d(:), h(:), m(:), s(:)];
      if (nargout == 0 || nargout == 1)
        varargout{1} = DV;
      elseif (nargout <= 6)
        for i = 1:nargout
          ## Multiple outputs are reshaped to original input size
          varargout{i} = reshape (DV(:,i), size (this));
        endfor
      else
        error ("duration.datevec: too many output arguments.");
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
    ## which correspond to hours, minutes, and seconds, respectively.  Hours and
    ## minutes are returned as whole numbers, while seconds may also have a
    ## fractional part.
    ##
    ## Values containing a fractional portion less than 1 picosecond are rounded
    ## to the nearest picosecond.
    ##
    ## @end deftypefn
    function varargout = hms (this)
      x = this.Days * 86400;
      h = fix (x / 3600);
      tmp = x - h * 3600;
      idx = 3600 - tmp < 1e-12; # find round-off errors to whole hours
      if (any (idx(:)))
        h(idx) += 1;
        x(idx) -= h(idx) * 3600;
        x(! idx) = tmp(! idx);
      else
        x = tmp;
      endif
      m = fix (x / 60);
      tmp = x - m * 60;
      idx = 60 - tmp < 1e-12; # find round-off errors to whole minutes
      if (any (idx(:)))
        m(idx) += 1;
        x(idx) -= m(idx) * 60;
        x(! idx) = tmp(! idx);
      else
        x = tmp;
      endif
      s = x;
      idx = x < 1e-12; # find round-off errors to whole seconds
      if (any (idx(:)))
        s(idx) = 0;
      endif
      ## Fix floating point precision to nearest picosecond
      s = round (s * 1e12) / 1e12;
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
    ## Values containing a fractional portion less than 1 picosecond are rounded
    ## to the nearest picosecond.
    ##
    ## @end deftypefn
    function out = minutes (this)
      out = this.Days * 1440;
      ## Fix floating point precision to nearest picosecond
      out = round (out * 1e13) / 1e13;
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
    ## Values containing a fractional portion less than 1 picosecond are rounded
    ## to the nearest picosecond.
    ##
    ## @end deftypefn
    function out = seconds (this)
      out = this.Days * 86400;
      ## Fix floating point precision to nearest picosecond
      out = round (out * 1e12) / 1e12;
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
    ## Values containing a fractional portion less than 1 picosecond are rounded
    ## to the nearest picosecond.
    ##
    ## @end deftypefn
    function out = milliseconds (this)
      out = this.Days * 86400000;
      ## Fix floating point precision to nearest picosecond
      out = round (out * 1e9) / 1e9;
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'size'             'ndims'            'numel'            'nnz'             ##
## 'length'           'keyHash'                                               ##
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

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{out} =} nnz (@var{D})
    ##
    ## Number of nonzero elements in duration array.
    ##
    ## @code{@var{out} = nnz (@var{D})} returns the number of nonzero
    ## elements in the duration array @var{D}.
    ##
    ## @end deftypefn
    function out = nnz (this)
      d = this.Days(:);
      out = numel (d) - sum (d == 0);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{N} =} length (@var{D})
    ##
    ## Length of a duration vector.
    ##
    ## @code{@var{N} = length (@var{D})} returns the size of the longest
    ## dimension of the duration array @var{D}, unless any of its dimensions has
    ## zero length, in which case @code{length (@var{D})} returns 0.
    ##
    ## @end deftypefn
    function N = length (this)
      if (isempty (this.Days))
        N = 0;
      else
        N = max (size (this.Days));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{hey} =} keyHash (@var{D})
    ##
    ## Generate a hash code for duration array.
    ##
    ## @code{@var{h} = keyHash (@var{D})} generates a @qcode{uint64} scalar that
    ## represents the input array @var{D}.  @code{keyHash} utilizes the 64-bit
    ## FNV-1a variant of the Fowler-Noll-Vo non-cryptographic hash function.
    ##
    ## @code{@var{h} = keyHash (@var{D}, @var{base})} also generates a 64-bit
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
      size_str = sprintf ('%dx', size (this.Days))(1:end-1);
      init_str = [size_str 'duration'];
      if (base)
        if (! (isscalar (base) && isa (base, 'uint64')))
          error ("duration.keyHash: BASE must be a UINT64 scalar.");
        endif
        key = __ckeyHash__(init_str, base);
      else
        key = __ckeyHash__(init_str);
      endif
      if (! isempty (this.Days))
        key = __nkeyHash__(this.Days(:), key);
      endif
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'isbetween'        'iscolumn'         'isempty'          'isequal'         ##
## 'isequaln'         'isfinite'         'isinf'            'ismatrix'        ##
## 'ismember'         'ismissing'        'isnan'            'isregular'       ##
## 'isrow'            'isscalar'         'issorted'         'issortedrows'    ##
## 'isvector'                                                                 ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{TF} =} isbetween (@var{D}, @var{lower}, @var{upper})
    ## @deftypefnx {duration} {@var{TF} =} isbetween (@var{D}, @var{lower}, @var{upper}, @var{intervalType})
    ##
    ## Find duration elements within specified range.
    ##
    ## @code{@var{TF} = isbetween (@var{D}, @var{lower}, @var{upper})} returns a
    ## logical array, @var{TF}, which is the same size as the input duration
    ## array @var{D} and it contains @qcode{true} for each corresponding element
    ## which is within the range specified by @var{lower} and @var{upper} and
    ## @qcode{false} otherwise.  @var{lower} and @var{upper} must be duration
    ## arrays of compatible size with @var{D} or alternatively they can be
    ## specified as a character vector, a cell array of character vectors or a
    ## string array containing valid text duration representations.
    ##
    ## @code{@var{TF} = isbetween (@var{D}, @var{lower}, @var{upper},
    ## @var{intervalType})} specifies the type of interval for the @var{lower}
    ## and @var{upper} bounds and it can be one of the following values.
    ##
    ## @itemize
    ## @item @qcode{'closed'} (default) includes lower and upper bounds.
    ##
    ## @item @qcode{'open'} excludes lower and upper bounds.
    ##
    ## @item @qcode{'openleft'} or @qcode{'closedright'} exclude the lower and
    ## include the upper bound.  They have identical behavior.
    ##
    ## @item @qcode{'closedleft'} or @qcode{'openright'} include the lower and
    ## exclude the upper bound.  They have identical behavior.
    ## @end itemize
    ##
    ## @var{intervalType} can be specified either as a character vector or a
    ## string scalar.
    ##
    ## @end deftypefn
    function TF = isbetween (this, lower, upper, varargin)
      if (nargin < 3)
        error ("duration.isbetween: too few input arguments.");
      elseif (nargin == 3)
        intervaltype = 'closed';
      elseif (nargin == 4)
        intervaltype = varargin{1};
      elseif (nargin > 4)
        error (strcat ("duration.isbetween: optional paired arguments", ...
                       " are not suppoorted for duration arrays."));
      endif
      if (isnumeric (lower) || isnumeric (upper))
        error ("duration.isbetween: LOWER and UPPER cannot be numeric.");
      endif
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

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} iscolumn (@var{D})
    ##
    ## Return true if duration array is a column vector.
    ##
    ## @code{@var{TF} = iscolumn (@var{D})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the duration array @var{D} is a column vector
    ## and @qcode{false} otherwise.  A column vector is a 2-D array for which
    ## @code{size (@var{D})} returns @code{[@var{N}, 1]} with non-negative
    ## @var{N}.  By definition, a scalar is also a column vector.
    ##
    ## @end deftypefn
    function TF = iscolumn (this)
      TF = iscolumn (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} isempty (@var{D})
    ##
    ## Return true if duration array is empty.
    ##
    ## @code{@var{TF} = isempty (@var{D})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the duration array @var{D} is empty and
    ## @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isempty (this)
      TF = isempty (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{TF} =} isequal (@var{D1}, @var{D2})
    ## @deftypefnx {duration} {@var{TF} =} isequal (@var{D1}, @var{D2}, @dots{})
    ##
    ## Return true if duration arrays are equal.
    ##
    ## @code{@var{TF} = isequal (@var{D1}, @var{D2})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the duration arrays @var{D1} and
    ## @var{D2} contain the same values, and @qcode{false} otherwise.  Either
    ## @var{D1} or @var{D2} can also be specified as a character vector, a cell
    ## array of character vectors or a string array containing valid text
    ## duration representations.
    ##
    ## @code{@var{TF} = isequal (@var{D1}, @var{D2}, @dots{})} returns a logical
    ## scalar @var{TF}, which is @qcode{true}, if all input arguments are equal,
    ## and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isequal (varargin)
      args = varargin;
      [args{:}] = promote (varargin{:});
      days = cellfun (@(obj) obj.Days, args, 'UniformOutput', false);
      TF = isequal (days{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{TF} =} isequaln (@var{D1}, @var{D2})
    ## @deftypefnx {duration} {@var{TF} =} isequaln (@var{D1}, @var{D2}, @dots{})
    ##
    ## Return true if duration arrays are equal under the assumption that
    ## missing elements are equal.
    ##
    ## @code{@var{TF} = isequal (@var{D1}, @var{D2})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the duration arrays @var{D1} and
    ## @var{D2} contain the same values or corresponding missing elements, and
    ## @qcode{false} otherwise.  Either @var{D1} or @var{D2} can also be
    ## specified as a character vector, a cell array of character vectors or a
    ## string array containing valid text duration representations.
    ##
    ## @code{@var{TF} = isequal (@var{D1}, @var{D2}, @dots{})} returns a logical
    ## scalar @var{TF}, which is @qcode{true}, if all input arguments are equal
    ## under the assumption that missing elements are equal, and @qcode{false}
    ## otherwise.
    ##
    ## @end deftypefn
    function TF = isequaln (varargin)
      args = varargin;
      [args{:}] = promote (varargin{:});
      days = cellfun (@(obj) obj.Days, args, 'UniformOutput', false);
      TF = isequaln (days{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} isfinite (@var{D})
    ##
    ## Return true for duration elements that are finite.
    ##
    ## @code{@var{TF} = isfinite (@var{D})} returns a logical array @var{TF}
    ## of the same size as @var{calD} containing @qcode{true} for each
    ## corresponding element of @var{D} that is finite and @qcode{false}
    ## otherwise.  Finite elements are those which are neither infinite nor
    ## Not-A-Number.
    ##
    ## @end deftypefn
    function TF = isfinite (this)
      TF = isfinite (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} isinf (@var{D})
    ##
    ## Return true for duration elements that are infinite.
    ##
    ## @code{@var{TF} = isinf (@var{D})} returns a logical array @var{TF}
    ## of the same size as @var{D} containing @qcode{true} for each
    ## corresponding element of @var{calD} that is either @qcode{Inf} or
    ## @qcode{-Inf} and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isinf (this)
      TF = isinf (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} ismatrix (@var{D})
    ##
    ## Return true if duration array is a 2-D array.
    ##
    ## @code{@var{TF} = ismatrix (@var{D})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the duration array @var{D} is a matrix and
    ## @qcode{false} otherwise.  A matrix is an array of any type where
    ## @code{ndims (@var{D}) == 2}.  By definition, a scalar is also a matrix.
    ##
    ## @end deftypefn
    function TF = ismatrix (this)
      TF = ismatrix (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{TF} =} ismember (@var{A}, @var{B})
    ## @deftypefnx {duration} {@var{TF} =} ismember (@var{a}, @var{s}, @qcode{'rows'})
    ## @deftypefnx {duration} {[@var{TF}, @var{index}] =} ismember (@dots{})
    ## @deftypefnx {duration} {[@var{TF}, @var{index}] =} ismember (@dots{}, @qcode{'legacy'})
    ##
    ## Find duration elements in a set.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B})} returns a logical array
    ## @var{TF} of the same size as @var{A} containing @qcode{true} for each
    ## corresponding element of @var{A} that is in @var{B} and @qcode{false}
    ## otherwise.  @qcode{NaN} elements are not equal with each other and always
    ## return @qcode{false}.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B}, @qcode{'rows'})} only
    ## applies to duration matrices with the same number of columns, in which
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
    ## member of @var{B} and 0 otherwise.  If the @qcode{'legacy'} optional
    ## argument is specified, then the highest index of matched elements is
    ## returned.  Unless multiple matches exist, the @qcode{'legacy'} option has
    ## no effect on the returned @var{index}.
    ##
    ## @end deftypefn
    function varargout = ismember (A, B, varargin)
      ## Check input arguments
      if (! isa (B, 'duration'))
        B = promote (B);
      endif
      ## Check for 'rows' and 'legacy' optional arguments
      if (! isempty (varargin))
        if (! cellfun ('ischar', varargin));
          error ("duration.ismember: all options must be character vectors.");
        elseif (! all (strcmpi (varargin, 'rows') | strcmpi (varargin, 'legacy')))
          error ("duration.ismember: only 'rows' and 'legacy' are valid options.");
        endif
        do_rows = any (strcmpi ('rows', varargin));
        if (do_rows)
          if (ndims (A) != 2 || ndims (A) != ndims (B))
            error ("duration.ismember: 'rows' applies only to 2-D matrices.");
          endif
          if (size (A, 2) != size (B, 2))
            error ("duration.ismember: 'rows' requires same number of columns.");
          endif
        endif
        if (nargout > 1)
          [varargout{1}, varargout{2}] = __ismember__ (A.Days, B.Days, varargin{:});
        else
          varargout{1} = __ismember__ (A.Days, B.Days, varargin{:});
        endif
      else
        if (nargout > 1)
          [varargout{1}, varargout{2}] = __ismember__ (A.Days, B.Days);
        else
          varargout{1} = __ismember__ (A.Days, B.Days);
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{TF} =} ismissing (@var{D})
    ## @deftypefnx {duration} {@var{TF} =} ismissing (@var{D}, @var{indicator})
    ##
    ## Find missing elements in duration array.
    ##
    ## @code{@var{TF} = ismissing (@var{D})} returns a logical array, @var{TF},
    ## with the same dimensions as @var{D}, where @code{true} values match the
    ## standard missing values in the input duration array.
    ##
    ## The optional input @var{indicator} can be a scalar or a vector duration
    ## array, specifying alternative missing values in the input data.  When
    ## specifying @var{indicator} values, the standard missing values are
    ## ignored, unless explicitly stated in the @var{indicator}.
    ##
    ## @end deftypefn
    function TF = ismissing (this, varargin)
      if (nargin > 2)
        error ("duration.ismissing: too many input arguments.");
      endif
      if (! isempty (varargin))
        indicator = varargin{1};
        TF = false (size (this));
        if (isvector (indicator))
          if (isa (indicator, 'duration'))
            for i = 1:length (indicator)
              TF(this.Days == indicator.Days(i)) = true;
            endfor
          else
            error ("duration.ismissing: INDICATOR must be a 'duration' array.");
          endif
        else
          error ("duration.ismissing: INDICATOR must be a vector.");
        endif
      else
        TF = isnan (this.Days);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} isnan (@var{D})
    ##
    ## Return true for duration elements that are Not-A-Number.
    ##
    ## @code{@var{TF} = isnan (@var{D})} returns a logical array @var{TF} of
    ## the same size as @var{D} containing @qcode{true} for each corresponding
    ## element of @var{calD} that is @qcode{NaN} and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isnan (this)
      TF = isnan (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} isrow (@var{D})
    ##
    ## Return true if duration array is a row vector.
    ##
    ## @code{@var{TF} = isrow (@var{D})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the duration array @var{D} is a row vector
    ## and @qcode{false} otherwise.  A row vector is a 2-D array for which
    ## @code{size (@var{D})} returns @code{[1, @var{N}]} with non-negative
    ## @var{N}.  By definition, a scalar is also a row vector.
    ##
    ## @end deftypefn
    function TF = isrow (this)
      TF = isrow (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} isscalar (@var{D})
    ##
    ## Return true if duration array is a scalar.
    ##
    ## @code{@var{TF} = isscalar (@var{D})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the duration array @var{D} is also a scalar
    ## and @qcode{false} otherwise.  A scalar is a single element object for
    ## which @code{size (@var{D})} returns @code{[1, 1]}.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = isscalar (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{TF} =} issorted (@var{D})
    ## @deftypefnx {duration} {@var{TF} =} issorted (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{TF} =} issorted (@var{D}, @var{direction})
    ## @deftypefnx {duration} {@var{TF} =} issorted (@var{D}, @var{dim}, @var{direction})
    ## @deftypefnx {duration} {@var{TF} =} issorted (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ## @deftypefnx {duration} {@var{TF} =} issorted (@dots{}, @qcode{'ComparisonMethod'}, @var{CM})
    ##
    ## Return true if duration array is sorted.
    ##
    ## @code{@var{TF} = issorted (@var{D})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the duration array @var{D} is sorted in
    ## ascending order, and @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = issorted (@var{D}, @var{dim})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the duration array @var{D} is
    ## sorted in ascending order along the dimension @var{dim}, and
    ## @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = issorted (@var{D}, @var{direction})} returns a logical
    ## scalar @var{TF}, which is @qcode{true}, if the duration array @var{D}
    ## is sorted in the direction specified by @var{direction}, and
    ## @qcode{false} otherwise.  @var{direction} can be any of the following
    ## options:
    ##
    ## @itemize
    ## @item @qcode{'ascend'}, which is the default, checks is elements are in
    ## ascending order.
    ## @item @qcode{'descend'} checks if elements are in descending order.
    ## @item @qcode{'monotonic'} checks if elements are either in ascending or
    ## descending order.
    ## @item @qcode{'strictascend'} checks if elements are in ascending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictdescend'} checks if elements are in descending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictmonotonic'} checks if elements are either in
    ## ascending or descending order and there are no duplicate or undefined
    ## elements.
    ## @end itemize
    ##
    ## @code{@var{TF} = issorted (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where missing elements (@qcode{NaN}) are placed with one of the
    ## following options specified in @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @code{@var{TF} = issorted (@dots{}, @qcode{'ComparisonMethod'}, @var{CM})}
    ## specifies the comparison method for determining the order of elements
    ## with one of following options specified in @var{CM}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, sorts by @code{real (A)}.
    ## @item @qcode{'real'} sorts by @code{real (A)}.
    ## @item @qcode{'abs'} sorts by @code{abs (A)}.
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = issorted (this, varargin)
      ## Single input argument
      if (nargin == 1)
        TF = isequaln (this, sort (this));
        return;
      endif

      ## Get operating dimension
      cid = cellfun (@isnumeric, varargin);
      if (any (cid))
        dim = varargin{cid};
      else
        sz = size (this);
        dim = find (sz != 1, 1);
        if (isempty (dim)) # scalar
          dim = 1;
        endif
      endif

      ## Force strings to character vectors
      [varargin{:}] = convertStringsToChars (varargin{:});

      ## Get direction from input argument list
      valid_direction = {'ascend', 'descend', 'monotonic', 'strictascend', ...
                         'strictdescend', 'strictmonotonic'};
      fcn = @(x) ischar (x) && ismember (x, valid_direction);
      cid = cellfun (fcn, varargin);
      if (any (cid))
        direction = varargin{cid};
        switch (direction)
          case {'ascend', 'descend'}
            TF = isequaln (this, sort (this, varargin{:}));

          case 'monotonic'
            ## Check for either ascending or descending
            varargin{cid} = 'ascend';
            TF = isequaln (this, sort (this, varargin{:}));
            if (TF)
              return;
            endif
            varargin{cid} = 'descend';
            TF = isequaln (this, sort (this, varargin{:}));

          case 'strictascend'
            ## Check for missing values first (fast)
            if (any (ismissing (this)(:)))
              TF = false;
              return;
            endif
            varargin{cid} = strrep (direction, 'strict', '');
            sorted = sort (this, varargin{:});
            if (any ((diff (sorted, 1, dim) <= 0)(:)))
              TF = false;
              return;
            endif
            TF = isequaln (this, sorted);

          case 'strictdescend'
            ## Check for missing values first (fast)
            if (any (ismissing (this)(:)))
              TF = false;
              return;
            endif
            varargin{cid} = strrep (direction, 'strict', '');
            sorted = sort (this, varargin{:});
            if (any ((diff (sorted, 1, dim) >= 0)(:)))
              TF = false;
              return;
            endif
            TF = isequaln (this, sorted);

          case 'strictmonotonic'
            ## Check missing values first (fast)
            if (any (ismissing (this)(:)))
              TF = false;
              return;
            endif
            ## Check for either ascending or descending
            varargin{cid} = 'ascend';
            sorted = sort (this, varargin{:});
            if (any ((diff (sorted, 1, dim) <= 0)(:)))
              TF = false;
              return;
            endif
            TF = isequaln (this, sorted);
            if (TF)
              return;
            endif
            varargin{cid} = 'descend';
            sorted = sort (this, varargin{:});
            if (any ((diff (sorted, 1, dim) >= 0)(:)))
              TF = false;
              return;
            endif
            TF = isequaln (this, sorted);
        endswitch
      else
        ## No DIRECTION input argument
        TF = isequaln (this, sort (this, varargin{:}));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{TF} =} issortedrows (@var{D})
    ## @deftypefnx {duration} {@var{TF} =} issortedrows (@var{D}, @var{col})
    ## @deftypefnx {duration} {@var{TF} =} issortedrows (@var{D}, @var{direction})
    ## @deftypefnx {duration} {@var{TF} =} issortedrows (@var{D}, @var{col}, @var{direction})
    ## @deftypefnx {duration} {@var{TF} =} issortedrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ## @deftypefnx {duration} {@var{TF} =} issortedrows (@dots{}, @qcode{'ComparisonMethod'}, @var{CM})
    ##
    ## Return true if duration matrix rows are sorted.
    ##
    ## @code{@var{TF} = issortedrows (@var{D})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the rows in the 2-D duration array
    ## @var{D} are sorted in ascending order, and @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = issortedrows (@var{D}, @var{col})} returns a logical
    ## scalar @var{TF}, which is @qcode{true}, if the duration array @var{D}
    ## is sorted according to the columns specified by the vector @var{col}, and
    ## @qcode{false} otherwise.  @var{col} must explicitly contain non-zero
    ## integers whose absolute values index existing columns in @var{D}.
    ## Positive elements sort the corresponding columns in ascending order,
    ## while negative elements sort the corresponding columns in descending
    ## order.
    ##
    ## @code{@var{TF} = issortedrows (@var{D}, @var{direction})} checks if the
    ## rows in @var{D} are sorted according to the specified direction, which
    ## can be one of the following options:
    ##
    ## @itemize
    ## @item @qcode{'ascend'}, which is the default, checks is elements are in
    ## ascending order.
    ## @item @qcode{'descend'} checks if elements are in descending order.
    ## @item @qcode{'monotonic'} checks if elements are either in ascending or
    ## descending order.
    ## @item @qcode{'strictascend'} checks if elements are in ascending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictdescend'} checks if elements are in descending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictmonotonic'} checks if elements are either in
    ## ascending or descending order and there are no duplicate or undefined
    ## elements.
    ## @end itemize
    ##
    ## Alternatively, @var{direction} can be a cell array of character
    ## vectors specifying the sorting direction for each individual column of
    ## @var{D}, in which case the number of elements in @var{direction} must
    ## equal the number of columns in @var{D}.
    ##
    ## @code{@var{TF} = issortedrows (@var{D}, @var{col}, @var{direction})}
    ## checks if the rows in the duration array @var{D} are sorted according
    ## to the columns specified in @var{col} using the corresponding sorting
    ## direction specified in @var{direction}.  In this case, the sign of the
    ## values in @var{col} is ignored.  @var{col} and @var{direction} must have
    ## the same length, but not necessarily the same number of elements as the
    ## columns in @var{D}.
    ##
    ## @code{@var{TF} = issortedrows (@dots{}, @qcode{'MissingPlacement'},
    ## @var{MP})} specifies where missing elements (@qcode{<undefined>}) are
    ## placed with one of the following options specified in @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @code{@var{TF} = issortedrows (@dots{}, @qcode{'ComparisonMethod'},
    ## @var{CM})} specifies the comparison method for determining the order of
    ## elements with one of following options specified in @var{CM}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, sorts by @code{real (A)}.
    ## @item @qcode{'real'} sorts by @code{real (A)}.
    ## @item @qcode{'abs'} sorts by @code{abs (A)}.
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = issortedrows (this, varargin)
      ## Single input argument
      if (nargin == 1)
        TF = isequaln (this, sortrows (this));
        return;
      endif

      ## Force strings to character vectors or cell arrays of character vectors
      [varargin{:}] = convertStringsToChars (varargin{:});

      ## Get valid direction(s) from input argument list
      valid = {'ascend', 'descend', 'monotonic', 'strictascend', ...
               'strictdescend', 'strictmonotonic'};
      fcn = @(x) (ischar (x) && ismember (x, valid)) || iscellstr (x);
      cid = cellfun (fcn, varargin);
      if (any (cid))
        direction = cellstr (varargin{cid});

        ## Check for valid type of directions in cellstring
        if (! all (cellfun (@(x) ismember (x, valid), direction)))
          error ("duration.issortedrows: invalid DIRECTION value.");
        endif

        ## Handle non-strict modes first
        if (all (cellfun (@(x) ismember (x, {'ascend', 'descend'}), direction)))
          TF = isequaln (this, sortrows (this, varargin{:}));
          return;
        endif
        simple_types = {'ascend', 'descend', 'monotonic'};
        if (all (cellfun (@(x) ismember (x, simple_types), direction)))
          idx = strcmp (direction, 'monotonic');
          direction{idx} = 'ascend';
          varargin{cid} = direction;
          TF = isequaln (this, sortrows (this, varargin(:)));
          if (TF)
            return;
          endif
          direction{idx} = 'descend';
          varargin{cid} = direction;
          TF = isequaln (this, sort (this, varargin{:}));
          return;
        endif

        ## Handle strict modes
        strict_types = {'strictascend', 'strictdescend', 'strictmonotonic'};
        idx = cellfun (@(x) ismember (x, strict_types), direction);

        ## Get COL vector (if given) from input arguments and find
        ## the corresponding columns for which strict modes apply.
        col = cellfun (@isnumeric, varargin);
        if (col)   # COL is available
          col = varargin{col};
          if (numel (direction) != numel (col))
            error ("duration.issortedrows: COL and DIRECTION mismatch.");
          endif
          strict_idx = abs (col(idx));  # remove negative numbers (if any)
        else       # only DIRECTION is available
          strict_idx = idx;
        endif

        ## Check for missing values first (fast)
        if (any (ismissing (this(:,strict_idx))(:)))
          TF = false;
          return;
        endif

        ## Change strict modes to simple modes
        direction = strrep (direction, 'strict', '');
        varargin{cid} = direction;

        ## Operate with simple modes and check strictness on selected columns
        if (all (cellfun (@(x) ismember (x, {'ascend', 'descend'}), direction)))
          sorted = sortrows (this, varargin{:});
          ## Test 'strictness' on specific columns for unique rows
          tmpcol = sorted.Days(:, strict_idx);
          if (any (all (diff (tmpcol, 1, 1) == 0, 2)))
            TF = false;
            return;
          endif
          TF = isequaln (this, sorted);

        else  # 'monotonic' mode also exists
          idx = strcmp (direction, 'monotonic');
          direction{idx} = 'ascend';
          varargin{cid} = direction;
          sorted = sortrows (this, varargin{:});
          ## Test 'strictness' on specific columns for unique rows
          tmpcol = sorted.Days(:, strict_idx);
          if (any (all (diff (tmpcol, 1, 1) == 0, 2)))
            TF = false;
            return;
          endif
          TF = isequaln (this, sorted);
          if (TF)
            return;
          endif
          direction{idx} = 'descend';
          varargin{cid} = direction;
          sorted = sortrows (this, varargin{:});
          ## Test 'strictness' on specific columns for unique rows
          tmpcol = sorted.Days(:, strict_idx);
          if (any (all (diff (tmpcol, 1, 1) == 0, 2)))
            TF = false;
            return;
          endif
          TF = isequaln (this, sorted);
        endif
      else
        ## No DIRECTION input argument
        TF = isequaln (this, sortrows (this, varargin{:}));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} isvector (@var{D})
    ##
    ## Return true if duration array is a vector.
    ##
    ## @code{@var{TF} = isvector (@var{D})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the duration array @var{D} is a vector and
    ## @qcode{false} otherwise.  A vector is a 2-D array for which one of the
    ## dimensions is equal to 1 (either @math{1xN} or @math{Nx1}).  By
    ## definition, a scalar is also a vector.
    ##
    ## @end deftypefn
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

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Equality for duration arrays.
    ##
    ## @code{@var{TF} = eq (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} == @var{B}} and returns a logical array whose
    ## elements set to @qcode{true} where the corresponding elements of @var{A}
    ## and @var{B} are equal and set to @qcode{false} otherwise.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a character vector, a cell array
    ## of character vectors, or a string array representing duration strings or
    ## a numeric array representing days.
    ##
    ## @end deftypefn
    function TF = eq (A, B)
      [A, B] = promote (A, B);
      TF = A.Days == B.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} ge (@var{A}, @var{B})
    ##
    ## Equality for duration arrays.
    ##
    ## @code{@var{TF} = ge (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} >= @var{B}} and returns a logical array whose
    ## elements set to @qcode{true} where the corresponding elements of @var{A}
    ## are greater than or equal to @var{B} and set to @qcode{false} otherwise.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a character vector, a cell array
    ## of character vectors, or a string array representing duration strings or
    ## a numeric array representing days.
    ##
    ## @end deftypefn
    function TF = ge (A, B)
      [A, B] = promote (A, B);
      TF = A.Days >= B.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} gt (@var{A}, @var{B})
    ##
    ## Equality for duration arrays.
    ##
    ## @code{@var{TF} = gt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} > @var{B}} and returns a logical array whose
    ## elements set to @qcode{true} where the corresponding elements of @var{A}
    ## are greater than @var{B} and set to @qcode{false} otherwise.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a character vector, a cell array
    ## of character vectors, or a string array representing duration strings or
    ## a numeric array representing days.
    ##
    ## @end deftypefn
    function TF = gt (A, B)
      [A, B] = promote (A, B);
      TF = A.Days > B.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} le (@var{A}, @var{B})
    ##
    ## Equality for duration arrays.
    ##
    ## @code{@var{TF} = le (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} <= @var{B}} and returns a logical array whose
    ## elements set to @qcode{true} where the corresponding elements of @var{A}
    ## are less than or equal to @var{B} and set to @qcode{false} otherwise.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a character vector, a cell array
    ## of character vectors, or a string array representing duration strings or
    ## a numeric array representing days.
    ##
    ## @end deftypefn
    function TF = le (A, B)
      [A, B] = promote (A, B);
      TF = A.Days <= B.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} lt (@var{A}, @var{B})
    ##
    ## Equality for duration arrays.
    ##
    ## @code{@var{TF} = lt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} < @var{B}} and returns a logical array whose
    ## elements set to @qcode{true} where the corresponding elements of @var{A}
    ## are less than @var{B} and set to @qcode{false} otherwise.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a character vector, a cell array
    ## of character vectors, or a string array representing duration strings or
    ## a numeric array representing days.
    ##
    ## @end deftypefn
    function TF = lt (A, B)
      [A, B] = promote (A, B);
      TF = A.Days < B.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{TF} =} ne (@var{A}, @var{B})
    ##
    ## Equality for duration arrays.
    ##
    ## @code{@var{TF} = ne (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} != @var{B}} and returns a logical array whose
    ## elements set to @qcode{true} where the corresponding elements of @var{A}
    ## and @var{B} are not equal and set to @qcode{false} otherwise.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a character vector, a cell array
    ## of character vectors, or a string array representing duration strings or
    ## a numeric array representing days.
    ##
    ## @end deftypefn
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
## 'rdivide'          'colon'            'linspace'         'diff'            ##
## 'sum'              'cumsum'           'min'              'cummin'          ##
## 'max'              'cummax'           'floor'            'ceil'            ##
## 'round'            'sign'                                                  ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{B} =} abs (@var{A})
    ##
    ## Absolute value of the elements of a duration array.
    ##
    ## @code{@var{B} = abs (@var{A})} returns the absolute value of each element
    ## in the duration array @var{A}.  The returned duration array @var{B} has
    ## the same size as the input array @var{A}.
    ##
    ## @end deftypefn
    function B = abs (A)
      B = A;
      B.Days = abs (A.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} plus (@var{A}, @var{B})
    ##
    ## Addition for duration arrays.
    ##
    ## @code{@var{C} = plus (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} + @var{B}} and returns the sum of @var{A} and
    ## @var{B} by adding the corresponding elements.  @var{A} and @var{B} must
    ## be size compatible, which translates to they can be the same size, one
    ## can be scalar, or for every dimension, their dimension sizes must be
    ## equal or one of them must be 1.  The size of @var{C} is determined by the
    ## size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a numeric array in which case its
    ## elements are treated as a number of 24-hour days.  If the second argument
    ## @var{B} is a calendarDuration arrays, then the returned array @var{C} is
    ## also a calendarDuration array.
    ##
    ## @end deftypefn
    function C = plus (A, B)
      ## Overload methods for certain data types
      if (isa (B, 'calendarDuration'))
        C = B + A;
        return;
      endif
      if (isa (A, 'duration') && isa (B, 'duration'))
        C = A;
        C.Days = A.Days + B.Days;
      elseif (isnumeric (A))
        C = B;
        C.Days = double (A) + B.Days;
      elseif (isnumeric (B))
        C = A;
        C.Days = A.Days + double (B);
      else
        error (strcat ("duration: addition is not defined between", ...
                       " '%s' and '%s' arrays."), class (A), class (B));
      endif
      C = fix_zero_precision (C);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{B} =} uplus (@var{A})
    ##
    ## Unary plus for duration arrays.
    ##
    ## @code{@var{B} = uplus (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = + @var{A}} and returns the input array unaltered.
    ##
    ## @end deftypefn
    function B = uplus (A)
      B = A;
      B.Days = A.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} minus (@var{A}, @var{B})
    ##
    ## Subtraction for duration arrays.
    ##
    ## @code{@var{C} = minus (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} - @var{B}} and returns the subtraction of @var{B}
    ## from @var{A} by subtracting the corresponding elements.  @var{A} and
    ## @var{B} must be size compatible, which translates to they can be the same
    ## size, one can be scalar, or for every dimension, their dimension sizes
    ## must be equal or one of them must be 1.  The size of @var{C} is
    ## determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## One of the input arguments can also be a numeric array in which case its
    ## elements are treated as a number of 24-hour days.  If the second argument
    ## @var{B} is a calendarDuration arrays, then the returned array @var{C} is
    ## also a calendarDuration array.
    ##
    ## @end deftypefn
    function C = minus (A, B)
      ## Overload methods for certain data types
      if (isa (B, 'calendarDuration'))
        C = -B + A;
        return;
      endif
      if (isa (A, 'duration') && isa (B, 'duration'))
        C = A;
        C.Days = A.Days - B.Days;
      elseif (isnumeric (A))
        C = B;
        C.Days = double (A) - B.Days;
      elseif (isnumeric (B))
        C = A;
        C.Days = A.Days - double (B);
      else
        error (strcat ("duration: subtraction is not defined between", ...
                       " '%s' and '%s' arrays."), class (A), class (B));
      endif
      C = fix_zero_precision (C);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{B} =} uminus (@var{A})
    ##
    ## Unary minus for duration arrays.
    ##
    ## @code{@var{B} = uminus (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = - @var{A}} and returns the input array with its elements
    ## negated.
    ##
    ## @end deftypefn
    function B = uminus (A)
      B = A;
      B.Days = - A.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} times (@var{A}, @var{B})
    ##
    ## Element-wise multiplication for duration arrays.
    ##
    ## @code{@var{C} = times (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} .* @var{B}} and returns the element-by-element
    ## multiplication product between the corresponding elements of input arrays
    ## @var{A} and @var{B}, one of which must be a numeric array and the other a
    ## duration array.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they
    ## can be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## @end deftypefn
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

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} times (@var{A}, @var{B})
    ##
    ## Matrix multiplication for duration arrays.
    ##
    ## @code{@var{C} = mtimes (@var{A}, @var{B})} is the equivalent of the
    ## syntax @code{@var{C} = @var{A} * @var{B}} and returns the matrix
    ## multiplication product of input arrays @var{A} and @var{B}, one of which
    ## must be a numeric array and the other a duration array.
    ##
    ## The columns of @var{A} must equal the rows of @var{B} and the size of
    ## @var{C} is determined by the rows of @var{A} and the columns of @var{B}.
    ##
    ## @end deftypefn
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

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} ldivide (@var{A}, @var{B})
    ##
    ## Element-wise left division for duration arrays.
    ##
    ## @code{@var{C} = ldivide (@var{A}, @var{B})} is the equivalent of the
    ## syntax @code{@var{C} = @var{A} .\ @var{B}} and returns the element-wise
    ## division of the duration array @var{B} by the corresponding elements of
    ## input array @var{A}, which can either be a duration or a numeric array.
    ## If @var{A} is a duration array, then @var{C} is a double numeric array.
    ## If @var{A} is a numeric array, then @var{C} is a duration array.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they
    ## can be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## @end deftypefn
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
        C = fix_zero_precision (C);
      else
        error (strcat ("duration: left division is not defined", ...
                       " between '%s' and 'duration' arrays."), class (A));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} rdivide (@var{A}, @var{B})
    ##
    ## Element-wise right division for duration arrays.
    ##
    ## @code{@var{C} = rdivide (@var{A}, @var{B})} is the equivalent of the
    ## syntax @code{@var{C} = @var{A} ./ @var{B}} and returns the element-wise
    ## division of the duration array @var{A} by the corresponding elements of
    ## input array @var{B}, which can either be a duration or a numeric array.
    ## If @var{B} is a duration array, then @var{C} is a double numeric array.
    ## If @var{B} is a numeric array, then @var{C} is a duration array.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they
    ## can be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## @end deftypefn
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
        C = fix_zero_precision (C);
      else
        error (strcat ("duration: right division is not defined", ...
                       " between 'duration' and '%s' arrays."), class (B));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{R} =} colon (@var{Base}, @var{Limit})
    ## @deftypefnx {duration} {@var{R} =} colon (@var{Base}, @var{Increment}, @var{Limit})
    ##
    ## Create a range of durations in a vector.
    ##
    ## @code{@var{R} = colon (@var{Base}, @var{Limit})} is the equivalent of the
    ## syntax @code{@var{C} = @var{Base}:@var{Limit}} and returns a duration
    ## vector in the range from @var{Base} to @var{Limit} incremented by 24-hour
    ## days.
    ##
    ## @code{@var{R} = colon (@var{Base}, @var{Increment}, @var{Limit})} is
    ## equivalent to @code{@var{C} = @var{Base}:@var{Increment}:@var{Limit}}.
    ## dimension sizes must be equal or one of them must be 1.  The size of
    ## @var{C} is determined by the size compatibility of @var{A} and @var{B}.
    ##
    ## As long as one of the inputs is a duration scalar, the following types
    ## are additionally supported for the remaining input arguments:
    ##
    ## @itemize
    ## @item numeric scalar (24-hour day)
    ## @item character vector (duration string)
    ## @item cellstr scalar (duration string)
    ## @item string scalar (duration string)
    ## @end itemize
    ##
    ## @end deftypefn
    function R = colon (varargin)
      if (nargin < 2 || nargin > 3)
        error ("duration.colon: invalid number of input arguments.");
      endif
      idx = find (cellfun ('isduration', varargin), 1);
      R = varargin{idx};
      if (nargin == 2)
        [from, to] = promote (varargin{:});
        if (! isscalar (from) || ! isscalar (to))
          error ("duration.colon: input arguments must be scalars.");
        endif
        increment = days (1);
      else
        [from, increment, to] = promote (varargin{:});
        if (! isscalar (from) || ! isscalar (increment) || ! isscalar (to))
          error ("duration.colon: input arguments must be scalars.");
        endif
      endif
      R.Days = from.Days:increment.Days:to.Days;
      R = fix_zero_precision (R);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{R} =} linspace (@var{Start}, @var{End})
    ## @deftypefnx {duration} {@var{R} =} linspace (@var{Start}, @var{End}, @var{N})
    ##
    ## Create linearly spaced duration elements.
    ##
    ## @code{@var{R} = linspace (@var{Start}, @var{End})} returns 100 linearly
    ## spaced elements between @var{Start} and @var{End}.  If @var{Start} and
    ## @var{End} are scalars, then @var{R} is a vector.  If one or both inputs
    ## are vectors, then @var{R} is a matrix where each row is an independent
    ## sequence between @code{@var{Start}(idx_N)} and @code{@var{End}(idx_N)}.
    ##
    ## @code{@var{R} = linspace (@var{Start}, @var{End}, @var{N})} specifies the
    ## number (default is 100) of equally spaced elements between @var{Start}
    ## and @var{End}.  If @var{N} is not an integer value, then it is floored
    ## to the nearest integer.  If @var{N} is zero or negative, then an empty
    ## matrix is returned.  If @var{N} is one, then @var{End} is returned.
    ## If @var{N} greater than one, then @var{Start} and @var{End} are always
    ## included in the range.
    ##
    ## Either @var{Start} or @var{End} input arguments can also be one of the
    ## following types:
    ##
    ## @itemize
    ## @item numeric scalar or vector (24-hour days)
    ## @item character vector (duration string)
    ## @item cellstr scalar or vector (duration strings)
    ## @item string scalar or vector (duration strings)
    ## @end itemize
    ##
    ## @end deftypefn
    function R = linspace (A, B, n = 100)
      if (nargin < 2)
        error ("duration.linspace: too few input arguments.");
      endif
      if (isduration (A))
        R = A;
      else
        R = B;
      endif
      [A, B] = promote (A, B);
      R.Days = linspace (A.Days, B.Days, n);
      R = fix_zero_precision (R);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{DT} =} diff (@var{D})
    ## @deftypefnx {duration} {@var{DT} =} diff (@var{D}, @var{K})
    ## @deftypefnx {duration} {@var{DT} =} diff (@var{D}, @var{K}, @var{DIM})
    ##
    ## Compute differences between adjacent elements in a duration array.
    ##
    ## This method overloads the core @code{diff} function for duration arrays.
    ## The functionality is identical to core @code{diff} function.  Type
    ## @code{help diff} for more information.
    ##
    ## @end deftypefn
    function DT = diff (D, varargin)
      DT = D;
      DT.Days = diff (D.Days, varargin{:});
      DT = fix_zero_precision (DT);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{S} =} sum (@var{D})
    ## @deftypefnx {duration} {@var{S} =} sum (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{S} =} sum (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{S} =} sum (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{S} =} sum (@dots{}, @var{nanflag})
    ##
    ## Compute the sum of the elements of a duration array.
    ##
    ## This method overloads the core @code{sum} function for duration arrays.
    ## The functionality is identical to core @code{sum} function.  Type
    ## @code{help sum} for more information.
    ##
    ## @end deftypefn
    function S = sum (D, varargin)
      ## Force strings to character vectors or cell arrays of character vectors
      if (any (cellfun ('isstring', varargin)))
        [varargin{:}] = convertStringsToChars (varargin{:});
      endif
      S = D;
      S.Days = sum (D.Days, varargin{:});
      S = fix_zero_precision (S);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{CS} =} cumsum (@var{D})
    ## @deftypefnx {duration} {@var{CS} =} cumsum (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{CS} =} cumsum (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{CS} =} cumsum (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{CS} =} cumsum (@dots{}, @var{direction})
    ## @deftypefnx {duration} {@var{CS} =} cumsum (@dots{}, @var{nanflag})
    ##
    ## Compute the cumulative sum of the elements of a duration array.
    ##
    ## This method overloads the core @code{cumsum} function for duration
    ## arrays. The functionality is identical to core @code{cumsum} function.
    ## Type @code{help cumsum} for more information.
    ##
    ## @end deftypefn
    function CS = cumsum (D, varargin)
      ## Force strings to character vectors or cell arrays of character vectors
      if (any (cellfun ('isstring', varargin)))
        [varargin{:}] = convertStringsToChars (varargin{:});
      endif
      CS = D;
      CS.Days = cumsum (D.Days, varargin{:});
      CS = fix_zero_precision (CS);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} min (@var{D})
    ## @deftypefnx {duration} {@var{M} =} min (@var{D}, @qcode{[]}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} min (@var{D}, @qcode{[]}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} min (@var{D}, @qcode{[]}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{M} =} min (@var{D}, @qcode{[]}, @var{nanflag})
    ## @deftypefnx {duration} {@var{M} =} min (@var{D}, @qcode{[]}, @dots{}, @var{nanflag})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} min (@dots{})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} min (@dots{}, @qcode{'linear'})
    ## @deftypefnx {duration} {@var{M} =} min (@var{D1}, @var{D2})
    ## @deftypefnx {duration} {@var{M} =} min (@var{D1}, @var{D2}, @var{nanflag})
    ## @deftypefnx {duration} {@dots{} =} min (@dots{}, @qcode{'ComparisonMethod'}, @var{method})
    ##
    ## Find minimum values in duration arrays.
    ##
    ## This method overloads the core @code{min} function for duration
    ## arrays. The functionality is identical to core @code{min} function.
    ## Type @code{help min} for more information.
    ##
    ## @end deftypefn
    function varargout = min (D, varargin)
      M = D;
      if (isempty (varargin))
        if (nargout > 1)
          [M.Days, varargout{2}] = min (D.Days);
        else
          M.Days = min (D.Days);
        endif
        varargout{1} = M;
      else
        ## Force strings to character vectors or cell arrays of character vectors
        if (any (cellfun ('isstring', varargin)))
          [varargin{:}] = convertStringsToChars (varargin{:});
        endif
        ## Second argument is a duration
        if (isduration (varargin{1}))
          D2 = varargin{1};
          varargin(1) = [];
          ## Make sure first argument is also a duration
          if (! isduration (D))
            D = promote (D);
            M = D2;
          endif
          M.Days = min (D.Days, D2.Days, varargin{:});
          varargout{1} = M;
        else
          if (nargout > 1)
            [M.Days, varargout{2}] = min (D.Days, varargin{:});
          else
            M.Days = min (D.Days, varargin{:});
          endif
          varargout{1} = M;
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} cummin (@var{D})
    ## @deftypefnx {duration} {@var{M} =} cummin (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} cummin (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} cummin (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{M} =} cummin (@qcode{[]}, @var{nanflag})
    ## @deftypefnx {duration} {@var{M} =} cummin (@qcode{[]}, @var{direction})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} cummin (@dots{})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} cummin (@dots{}, @qcode{'linear'})
    ## @deftypefnx {duration} {@dots{} =} cummin (@dots{}, @qcode{'ComparisonMethod'}, @var{method})
    ##
    ## Return the cumulative minimum values in duration arrays.
    ##
    ## This method overloads the core @code{cummin} function for duration
    ## arrays. The functionality is identical to core @code{cummin} function.
    ## Type @code{help cummin} for more information.
    ##
    ## @end deftypefn
    function varargout = cummin (D, varargin)
      M = D;
      if (isempty (varargin))
        if (nargout > 1)
          [M.Days, varargout{2}] = cummin (D.Days);
        else
          M.Days = cummin (D.Days);
        endif
        varargout{1} = M;
      else
        ## Force strings to character vectors or cell arrays of character vectors
        if (any (cellfun ('isstring', varargin)))
          [varargin{:}] = convertStringsToChars (varargin{:});
        endif
        if (nargout > 1)
          [M.Days, varargout{2}] = cummin (D.Days, varargin{:});
        else
          M.Days = cummin (D.Days, varargin{:});
        endif
        varargout{1} = M;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} max (@var{D})
    ## @deftypefnx {duration} {@var{M} =} max (@var{D}, @qcode{[]}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} max (@var{D}, @qcode{[]}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} max (@var{D}, @qcode{[]}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{M} =} max (@var{D}, @qcode{[]}, @var{nanflag})
    ## @deftypefnx {duration} {@var{M} =} max (@var{D}, @qcode{[]}, @dots{}, @var{nanflag})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} max (@dots{})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} max (@dots{}, @qcode{'linear'})
    ## @deftypefnx {duration} {@var{M} =} max (@var{D1}, @var{D2})
    ## @deftypefnx {duration} {@var{M} =} max (@var{D1}, @var{D2}, @var{nanflag})
    ## @deftypefnx {duration} {@dots{} =} max (@dots{}, @qcode{'ComparisonMethod'}, @var{method})
    ##
    ## Find maximum values in duration arrays.
    ##
    ## This method overloads the core @code{max} function for duration
    ## arrays. The functionality is identical to core @code{max} function.
    ## Type @code{help max} for more information.
    ##
    ## @end deftypefn
    function varargout = max (D, varargin)
      M = D;
      if (isempty (varargin))
        if (nargout > 1)
          [M.Days, varargout{2}] = max (D.Days);
        else
          M.Days = max (D.Days);
        endif
        varargout{1} = M;
      else
        ## Force strings to character vectors or cell arrays of character vectors
        if (any (cellfun ('isstring', varargin)))
          [varargin{:}] = convertStringsToChars (varargin{:});
        endif
        ## Second argument is a duration
        if (isduration (varargin{1}))
          D2 = varargin{1};
          varargin(1) = [];
          ## Make sure first argument is also a duration
          if (! isduration (D))
            D = promote (D);
            M = D2;
          endif
          M.Days = max (D.Days, D2.Days, varargin{:});
          varargout{1} = M;
        else
          if (nargout > 1)
            [M.Days, varargout{2}] = max (D.Days, varargin{:});
          else
            M.Days = max (D.Days, varargin{:});
          endif
          varargout{1} = M;
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} cummax (@var{D})
    ## @deftypefnx {duration} {@var{M} =} cummax (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} cummax (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} cummax (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{M} =} cummax (@qcode{[]}, @var{nanflag})
    ## @deftypefnx {duration} {@var{M} =} cummax (@qcode{[]}, @var{direction})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} cummax (@dots{})
    ## @deftypefnx {duration} {[@var{M}, @var{index}] =} cummax (@dots{}, @qcode{'linear'})
    ## @deftypefnx {duration} {@dots{} =} cummax (@dots{}, @qcode{'ComparisonMethod'}, @var{method})
    ##
    ## Return the cumulative maximum values in duration arrays.
    ##
    ## This method overloads the core @code{cummax} function for duration
    ## arrays. The functionality is identical to core @code{cummax} function.
    ## Type @code{help cummax} for more information.
    ##
    ## @end deftypefn
    function varargout = cummax (D, varargin)
      M = D;
      if (isempty (varargin))
        if (nargout > 1)
          [M.Days, varargout{2}] = cummax (D.Days);
        else
          M.Days = cummax (D.Days);
        endif
        varargout{1} = M;
      else
        ## Force strings to character vectors or cell arrays of character vectors
        if (any (cellfun ('isstring', varargin)))
          [varargin{:}] = convertStringsToChars (varargin{:});
        endif
        if (nargout > 1)
          [M.Days, varargout{2}] = cummax (D.Days, varargin{:});
        else
          M.Days = cummax (D.Days, varargin{:});
        endif
        varargout{1} = M;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{Y} =} floor (@var{D})
    ## @deftypefnx {duration} {@var{Y} =} floor (@var{D}, @var{unit})
    ##
    ## Round toward negative infinity for duration arrays.
    ##
    ## @code{@var{Y} = floor (@var{D})} rounds each element of the duration
    ## array @var{D} to the largest integer number of seconds not greater than
    ## that element.
    ##
    ## @code{@var{Y} = floor (@var{D}, @var{unit})} rounds each element of the
    ## duration array @var{D} to the largest integer number of the specified
    ## unit of time not greater than that element.  @var{unit} must be one of
    ## the following values:
    ##
    ## @itemize
    ## @item @qcode{'seconds'} (default)
    ## @item @qcode{'minutes'}
    ## @item @qcode{'hours'}
    ## @item @qcode{'days'}
    ## @item @qcode{'years'}
    ## @end itemize
    ##
    ## @end deftypefn
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

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{Y} =} ceil (@var{D})
    ## @deftypefnx {duration} {@var{Y} =} ceil (@var{D}, @var{unit})
    ##
    ## Round toward negative infinity for duration arrays.
    ##
    ## @code{@var{Y} = ceil (@var{D})} rounds each element of the duration
    ## array @var{D} to the smallest integer number of seconds not less than
    ## that element.
    ##
    ## @code{@var{Y} = ceil (@var{D}, @var{unit})} rounds each element of the
    ## duration array @var{D} to the smallest integer number of the specified
    ## unit of time not less than that element.  @var{unit} must be one of the
    ## following values:
    ##
    ## @itemize
    ## @item @qcode{'seconds'} (default)
    ## @item @qcode{'minutes'}
    ## @item @qcode{'hours'}
    ## @item @qcode{'days'}
    ## @item @qcode{'years'}
    ## @end itemize
    ##
    ## @end deftypefn
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

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{Y} =} round (@var{D})
    ## @deftypefnx {duration} {@var{Y} =} round (@var{D}, @var{unit})
    ##
    ## Round to the nearest integer time unit for duration arrays.
    ##
    ## @code{@var{Y} = round (@var{D})} rounds each element of the duration
    ## array @var{D} to the nearest integer number of seconds to that element.
    ## In case of a tie, return the one further away from zero.
    ##
    ## @code{@var{Y} = round (@var{D}, @var{unit})} rounds each element of the
    ## duration array @var{D} to the nearest integer number of the specified
    ## unit of time to that element.  @var{unit} must be one of the following
    ## values:
    ##
    ## @itemize
    ## @item @qcode{'seconds'} (default)
    ## @item @qcode{'minutes'}
    ## @item @qcode{'hours'}
    ## @item @qcode{'days'}
    ## @item @qcode{'years'}
    ## @end itemize
    ##
    ## @end deftypefn
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

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{out} =} sign (@var{D})
    ## @deftypefnx {duration} {@var{out} =} sign (@var{D}, @var{unit})
    ##
    ## Compute the signum function for duration arrays.
    ##
    ## @code{@var{out} = sign (@var{D})} returns an array of doubles, @var{out},
    ## the same size as the duration array @var{D}, where each element has one
    ## following values:
    ##
    ## @itemize
    ## @item @qcode{1} if the corresponding element of D is greater than 0.
    ## @item @qcode{0} if the corresponding element of D is equal to 0.
    ## @item @qcode{-1} if the corresponding element of D is less than 0.
    ## @item @qcode{NaN} if the corresponding element of D is a missing value.
    ## @end itemize
    ##
    ## @end deftypefn
    function out = sign (A)
      out = sign (A.Days);
    endfunction

  endmethods

################################################################################
##                        ** Statistical Operations **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'bounds'           'center'           'histc'            'iqr'             ##
## 'kurtosis'         'mad'              'mape'             'mean'            ##
## 'median'           'mode'             'prctile'          'quantile'        ##
## 'range'            'rmse'             'skewness'         'std'             ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {[@var{s}, @var{l}] =} bounds (@var{D})
    ## @deftypefnx {duration} {[@var{s}, @var{l}] =} bounds (@var{D}, @var{dim})
    ## @deftypefnx {duration} {[@var{s}, @var{l}] =} bounds (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {[@var{s}, @var{l}] =} bounds (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {[@var{s}, @var{l}] =} bounds (@dots{}, @var{nanflag})
    ##
    ## Return the smallest and largest values of a duration array.
    ##
    ## This method is a specialization of the core @code{bounds} function for
    ## duration arrays.  The functionality is identical to core @code{bounds}
    ## function.  Type @code{help bounds} for more information.
    ##
    ## @end deftypefn
    function [s, l] = bounds (D, varargin)
      if (nargin < 1 || nargin > 3)
        error ("duration.bounds: invalid number of input arguments.");
      endif
      if (isempty (varargin))
        s = min (D);
        l = max (D);
      else
        s = min (D, [], varargin{:});
        l = max (D, [], varargin{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{C} =} center (@var{D})
    ## @deftypefnx {duration} {@var{C} =} center (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{C} =} center (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{C} =} center (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{C} =} center (@dots{}, @var{nanflag})
    ##
    ## Center values in a duration array.
    ##
    ## This method overloads the core @code{center} function for duration
    ## arrays.  The functionality is identical to core @code{center} function.
    ## Type @code{help center} for more information.
    ##
    ## @end deftypefn
    function C = center (D, varargin)
      C = D;
      C.Days = center (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{n} =} histc (@var{D}, @var{edges})
    ## @deftypefnx {duration} {@var{n} =} histc (@var{D}, @var{edges}, @var{dim})
    ## @deftypefnx {duration} {[@var{n}, @var{idx}] =} histc (@dots{})
    ##
    ## Compute histogram counts in a duration array.
    ##
    ## This method overloads the core @code{histc} function for duration
    ## arrays.  The functionality is identical to core @code{histc} function.
    ## Type @code{help histc} for more information.
    ##
    ## @end deftypefn
    function varargout = histc (D, varargin)
      if (nargout > 1)
        [varargout{1}, varargout{2}] = histc (D.Days, varargin{:});
      else
        varargout{1} = histc (D.Days, varargin{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{r} =} iqr (@var{D})
    ## @deftypefnx {duration} {@var{r} =} iqr (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{r} =} iqr (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{r} =} iqr (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {[@var{r}, @var{q}] =} iqr (@dots{})
    ##
    ## Compute the interquartile range of a duration array.
    ##
    ## This method overloads the core @code{iqr} function for duration arrays.
    ## The functionality is identical to core @code{iqr} function.
    ## Type @code{help iqr} for more information.
    ##
    ## @end deftypefn
    function varargout = iqr (D, varargin)
      R = D;
      if (nargout > 1)
        [R.Days, varargout{2}] = iqr (D.Days, varargin{:});
      else
        R.Days = iqr (D.Days, varargin{:});
      endif
      varargout{1} = R;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{k} =} kurtosis (@var{D})
    ## @deftypefnx {duration} {@var{k} =} kurtosis (@var{D}, @var{flag})
    ## @deftypefnx {duration} {@var{k} =} kurtosis (@var{D}, @var{flag}, @var{dim})
    ## @deftypefnx {duration} {@var{k} =} kurtosis (@var{D}, @var{flag}, @var{vecdim})
    ## @deftypefnx {duration} {@var{k} =} kurtosis (@var{D}, @var{flag}, @qcode{'all'})
    ##
    ## Compute the sample kurtosis of a duration array.
    ##
    ## This method overloads the core @code{kurtosis} function for duration
    ## arrays.  The functionality is identical to core @code{kurtosis} function.
    ## Type @code{help kurtosis} for more information.
    ##
    ## @end deftypefn
    function k = kurtosis (D, varargin)
      k = kurtosis (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} mad (@var{D})
    ## @deftypefnx {duration} {@var{M} =} mad (@var{D}, @var{opt})
    ## @deftypefnx {duration} {@var{M} =} mad (@var{D}, @var{opt}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} mad (@var{D}, @var{opt}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} mad (@var{D}, @var{opt}, @qcode{'all'})
    ##
    ## Compute the mean or median absolute deviation of a duration array.
    ##
    ## This method overloads the core @code{mad} function for duration arrays.
    ## The functionality is identical to core @code{mad} function.
    ## Type @code{help mad} for more information.
    ##
    ## @end deftypefn
    function M = mad (D, varargin)
      M = D;
      M.Days = mad (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{E} =} mape (@var{F}, @var{A})
    ## @deftypefnx {duration} {@var{E} =} mape (@var{F}, @var{A}, @var{dim})
    ## @deftypefnx {duration} {@var{E} =} mape (@var{F}, @var{A}, @var{vecdim})
    ## @deftypefnx {duration} {@var{E} =} mape (@var{F}, @var{A}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{E} =} mape (@dots{}, @var{nanflag})
    ## @deftypefnx {duration} {@var{E} =} mape (@dots{}, @var{zeroflag})
    ## @deftypefnx {duration} {@var{E} =} mape (@dots{}, @qcode{'Weights'}, @var{W})
    ##
    ## Compute the mean absolute percentage error between duration arrays.
    ##
    ## This method overloads the core @code{mape} function for duration arrays.
    ## The functionality is identical to core @code{mape} function.
    ## Type @code{help mape} for more information.
    ##
    ## Note that MAPE is expressed as a percentage.  Thus, the returned argument
    ## is a numeric array of double type and not a duration array.
    ##
    ## @end deftypefn
    function E = mape (F, A, varargin)
      E = mape (F.Days, A.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} mean (@var{D})
    ## @deftypefnx {duration} {@var{M} =} mean (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} mean (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} mean (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{M} =} mean (@dots{}, @var{nanflag})
    ## @deftypefnx {duration} {@var{M} =} mean (@dots{}, @var{outtype})
    ## @deftypefnx {duration} {@var{M} =} mean (@dots{}, @qcode{'Weights'}, @var{W})
    ##
    ## Compute the mean of the elements of a duration array.
    ##
    ## This method overloads the core @code{mean} function for duration arrays.
    ## The functionality is identical to core @code{mean} function.
    ## Type @code{help mean} for more information.
    ##
    ## @end deftypefn
    function M = mean (D, varargin)
      M = D;
      M.Days = mean (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} median (@var{D})
    ## @deftypefnx {duration} {@var{M} =} median (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} median (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} median (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{M} =} median (@dots{}, @var{nanflag})
    ## @deftypefnx {duration} {@var{M} =} median (@dots{}, @var{outtype})
    ##
    ## Compute the median value of the elements of a duration array.
    ##
    ## This method overloads the core @code{median} function for duration
    ## arrays.  The functionality is identical to core @code{median} function.
    ## Type @code{help median} for more information.
    ##
    ## @end deftypefn
    function M = median (D, varargin)
      M = D;
      M.Days = median (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{M} =} mode (@var{x})
    ## @deftypefnx {duration} {@var{M} =} mode (@var{x}, @var{dim})
    ## @deftypefnx {duration} {@var{M} =} mode (@var{x}, @var{vecdim})
    ## @deftypefnx {duration} {@var{M} =} mode (@var{x}, @qcode{'all'})
    ## @deftypefnx {duration} {[@var{M}, @var{F}, @var{C}] =} mode (@dots{})
    ##
    ## Compute the most frequently occurring value in a duration array.
    ##
    ## This method overloads the core @code{mode} function for duration arrays.
    ## The functionality is identical to core @code{mode} function.
    ## Type @code{help mode} for more information.
    ##
    ## @end deftypefn
    function [M, F, C] = mode (D, varargin)
      M = D;
      [M.Days, F, C] = mode (D.Days, varargin{:});
      if (nargout == 3)
        C = cellfun ('days', C, 'UniformOutput', false);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{Q} =} prctile (@var{D})
    ## @deftypefnx {duration} {@var{q} =} prctile (@var{D}, @var{p})
    ## @deftypefnx {duration} {@var{Q} =} prctile (@var{D}, @var{p}, @var{dim})
    ## @deftypefnx {duration} {@var{Q} =} prctile (@var{D}, @var{p}, @var{vecdim})
    ## @deftypefnx {duration} {@var{Q} =} prctile (@var{D}, @var{p}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{Q} =} prctile (@var{D}, @var{p}, @dots{}, @var{method})
    ##
    ## Compute the percentiles of a duration array.
    ##
    ## This method overloads the core @code{prctile} function for duration
    ## arrays.  The functionality is identical to core @code{prctile} function.
    ## Type @code{help prctile} for more information.
    ##
    ## @end deftypefn
    function Q = prctile (D, varargin)
      Q = D;
      Q.Days = prctile (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{Q} =} quantile (@var{D})
    ## @deftypefnx {duration} {@var{Q} =} quantile (@var{D}, @var{p})
    ## @deftypefnx {duration} {@var{Q} =} quantile (@var{D}, @var{n})
    ## @deftypefnx {duration} {@var{Q} =} quantile (@var{D}, @dots{}, @var{dim})
    ## @deftypefnx {duration} {@var{Q} =} quantile (@var{D}, @dots{}, @var{vecdim})
    ## @deftypefnx {duration} {@var{Q} =} quantile (@var{D}, @dots{}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{Q} =} quantile (@var{D}, @var{p}, @dots{}, @var{method})
    ## @deftypefnx {duration} {@var{Q} =} quantile (@var{D}, @var{n}, @dots{}, @var{method})
    ##
    ## Compute the quantiles of a duration array.
    ##
    ## This method overloads the core @code{quantile} function for duration
    ## arrays.  The functionality is identical to core @code{quantile} function.
    ## Type @code{help quantile} for more information.
    ##
    ## @end deftypefn
    function Q = quantile (D, varargin)
      Q = D;
      Q.Days = quantile (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{R} =} range (@var{D})
    ## @deftypefnx {duration} {@var{R} =} range (@var{D}, @var{dim})
    ## @deftypefnx {duration} {@var{R} =} range (@var{D}, @var{vecdim})
    ## @deftypefnx {duration} {@var{R} =} range (@var{D}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{R} =} range (@dots{}, @var{nanflag})
    ##
    ## Compute the range of a duration array.
    ##
    ## This method overloads the core @code{range} function for duration arrays.
    ## The functionality is identical to core @code{range} function.
    ## Type @code{help range} for more information.
    ##
    ## @end deftypefn
    function R = range (D, varargin)
      R = D;
      R.Days = range (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{E} =} rmse (@var{F}, @var{A})
    ## @deftypefnx {duration} {@var{E} =} rmse (@var{F}, @var{A}, @var{dim})
    ## @deftypefnx {duration} {@var{E} =} rmse (@var{F}, @var{A}, @var{vecdim})
    ## @deftypefnx {duration} {@var{E} =} rmse (@var{F}, @var{A}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{E} =} rmse (@dots{}, @var{nanflag})
    ## @deftypefnx {duration} {@var{E} =} rmse (@dots{}, @qcode{'Weights'}, @var{W})
    ##
    ## Compute the root mean squared error between duration arrays.
    ##
    ## This method overloads the core @code{rmse} function for duration arrays.
    ## The functionality is identical to core @code{rmse} function.
    ## Type @code{help rmse} for more information.
    ##
    ## @end deftypefn
    function E = rmse (F, A, varargin)
      E = F;
      E.Days = rmse (F.Days, A.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{y} =} skewness (@var{D})
    ## @deftypefnx {duration} {@var{y} =} skewness (@var{D}, @var{flag})
    ## @deftypefnx {duration} {@var{y} =} skewness (@var{D}, @var{flag}, @var{dim})
    ## @deftypefnx {duration} {@var{y} =} skewness (@var{D}, @var{flag}, @var{vecdim})
    ## @deftypefnx {duration} {@var{y} =} skewness (@var{D}, @var{flag}, @qcode{'all'})
    ##
    ## Compute the sample skewness of a duration array.
    ##
    ## This method overloads the core @code{skewness} function for duration
    ## arrays.  The functionality is identical to core @code{skewness} function.
    ## Type @code{help skewness} for more information.
    ##
    ## @end deftypefn
    function y = skewness (D, varargin)
      y = skewness (D.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{S} =} std (@var{D})
    ## @deftypefnx {duration} {@var{S} =} std (@var{D}, @var{w})
    ## @deftypefnx {duration} {@var{S} =} std (@var{D}, @var{w}, @var{dim})
    ## @deftypefnx {duration} {@var{S} =} std (@var{D}, @var{w}, @var{vecdim})
    ## @deftypefnx {duration} {@var{S} =} std (@var{D}, @var{w}, @qcode{'all'})
    ## @deftypefnx {duration} {@var{S} =} std (@dots{}, @var{nanflag})
    ## @deftypefnx {duration} {[@var{S}, @var{M}] =} std (@dots{})
    ##
    ## Compute the standard deviation of a duration array.
    ##
    ## This method overloads the core @code{std} function for duration arrays.
    ## The functionality is identical to core @code{std} function.
    ## Type @code{help std} for more information.
    ##
    ## @end deftypefn
    function varargout = std (D, varargin)
      if (nargout > 1)
        S = M = D;
        [S.Days, M.Days] = std (D.Days, varargin{:});
        varargout{1} = S;
        varargout{2} = M;
      else
        S = D;
        S.Days = std (D.Days, varargin{:});
        varargout{1} = S;
      endif
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

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{B} =} sort (@var{A})
    ## @deftypefnx {duration} {@var{B} =} sort (@var{A}, @var{dim})
    ## @deftypefnx {duration} {@var{B} =} sort (@var{A}, @var{direction})
    ## @deftypefnx {duration} {@var{B} =} sort (@var{A}, @var{dim}, @var{direction})
    ## @deftypefnx {duration} {@var{B} =} sort (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ## @deftypefnx {duration} {@var{B} =} sort (@dots{}, @qcode{'ComparisonMethod'}, @var{CM})
    ## @deftypefnx {duration} {[@var{B}, @var{index}] =} sort (@var{A}, @dots{})
    ##
    ## Sort elements in a duration array.
    ##
    ## @code{@var{B} = sort (@var{A})} sorts the duration array @var{A} in
    ## ascending order.  If @var{A} is a matrix, @code{sort (@var{A})} sorts
    ## each column of @var{A} in ascending order.  For multidimensional arrays,
    ## @code{mode (@var{A})} sorts along the first non-singleton dimension.
    ##
    ## @code{@var{B} = sort (@var{A}, @var{dim})} sorts along the dimension
    ## specified by @var{dim}.
    ##
    ## @code{@var{B} = sort (@var{A}, @var{direction})} also specifies the
    ## sorting direction, which can be either @qcode{'ascend'} (default) or
    ## @qcode{'descend'}.
    ##
    ## @code{@var{B} = sort (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where to place the missing elements (@qcode{<undefined>})
    ## returned in @var{B} with one of the following options specified in
    ## @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @code{@var{B} = sort (@dots{}, @qcode{'ComparisonMethod'}, @var{CM})}
    ## specifies the comparison method for determining the order of elements
    ## returned in @var{B} with one of following options:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, sorts by @code{real (A)}.
    ## @item @qcode{'real'} sorts by @code{real (A)}.
    ## @item @qcode{'abs'} sorts by @code{abs (A)}.
    ## @end itemize
    ##
    ## @code{[@var{B}, @var{index}] = sort (@var{A}, @dots{})} also returns a
    ## sorting index containing the original indices of the elements in the
    ## sorted array.  @var{index} is the same size as @var{A} and it comprises
    ## indexing vectors oriented along the operating dimensions.
    ##
    ## @itemize
    ## @item If @var{A} is a vector, then @var{index} contains the original
    ## linear indices of the elements in the sorted vector @var{B} such that
    ## @code{@var{B} = @var{A}(@var{index})}.
    ## @item If @var{A} is an @math{MxN} matrix and @qcode{@var{dim} = 1}, then
    ## @var{index} contains the original row indices of the elements in the
    ## sorted vector @var{B} such that for @qcode{j = 1:N},
    ## @code{@var{B}(:,j) = @var{A}(@var{index}(:,j),j)}.
    ## @end itemize
    ##
    ## @end deftypefn
    function [B, index] = sort (A, varargin)
      ## Parse and validate optional paired arguments
      optNames = {'MissingPlacement', 'ComparisonMethod'};
      dfValues = {'auto', 'auto'};
      [MP, CM, args] = parsePairedArguments (optNames, dfValues, varargin(:));
      if (! ismember (MP, {'auto', 'first', 'last'}))
        error ("duration.sort: invalid value for 'MissingPlacement'.");
      endif
      if (! ismember (CM, {'auto', 'real', 'abs'}))
        error ("duration.sort: invalid value for 'ComparisonMethod'.");
      endif

      ## Force strings to character vectors
      [args{:}] = convertStringsToChars (args{:});

      ## Get direction
      cid = cellfun (@ischar, args);
      if (any (cid))
        dir = args{cid};
      else
        dir = 'ascend';
      endif
      ## Get operating dimension
      szA = size (A);
      cid = cellfun (@isnumeric, args);
      if (any (cid))
        dim = args{cid};
      else
        dim = find (szA != 1, 1);
        if (isempty (dim)) # scalar
          dim = 1;
        endif
      endif

      ## Apply comparison method (Octave specific)
      data = A.Days;
      if (strcmp (CM, 'abs'))
        data = abs (data);
      endif

      ## Special handling for missing elements when missing placement overrides
      ## default behavior (only if missing data actually exist).
      is_nan = isnan (data);
      if (any (is_nan(:)))
        ## FIX ME: this workaround will be removed once the 'sort' function
        ## in core Octave supports 'MissingPlacement' optional argument.
        ## This implementation fails the edge case where -Inf and -realmax
        ## elements are present along the operating dimension.
        if ((strcmp (dir, 'ascend') && strcmp (MP, {'first'})) ||
            (strcmp (dir, 'descend') && strcmp (MP, {'last'})))
          ## Convert missing values to -Inf so that they are placed
          ## appropriately according to 'MissingPlacement' specification.
          ## If -Inf values already exist in data, then convert them to the
          ## next smallest possible value
          is_m_inf = data == -Inf;
          if (any (is_m_inf(:)))
            m_inf_rep = - realmax;
            data(is_m_inf) = m_inf_rep;
          endif
          data(is_nan) = -Inf;
        endif
      endif

      ## Sort values
      [~, index] = sort (data, args{:});

      ## Calculate linear index
      n_dims = ndims (A);
      dimarg = cell (1, n_dims);
      for i = 1:n_dims
        if (i == dim)
          dimarg{i} = index;
        else
          dim_sz = szA(i);
          tmpvec = ones (1, n_dims);
          tmpvec(i) = dim_sz;
          tmp_sz = szA;
          tmp_sz(i) = 1;
          dimarg{i} = repmat (reshape ([1:dim_sz], tmpvec), tmp_sz);
        endif
      endfor

      ## Return sorted duration array
      B = subset (A, sub2ind (szA, dimarg{:}));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{B} =} sortrows (@var{A})
    ## @deftypefnx {duration} {@var{B} =} sortrows (@var{A}, @var{col})
    ## @deftypefnx {duration} {@var{B} =} sortrows (@var{A}, @var{direction})
    ## @deftypefnx {duration} {@var{B} =} sortrows (@var{A}, @var{col}, @var{direction})
    ## @deftypefnx {duration} {@var{B} =} sortrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ## @deftypefnx {duration} {[@var{B}, @var{index}] =} sortrows (@var{A}, @dots{})
    ##
    ## Sort rows in a duration array.
    ##
    ## @code{@var{B} = sortrows (@var{A})} sorts the rows of the 2-D duration
    ## array @var{A} in ascending order.  The sorted array @var{B} has the same
    ## size as @var{A}.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{col})} sorts @var{A} according to
    ## to the columns specified by the numeric vector @var{col}, which must
    ## explicitly contain non-zero integers whose absolute values index existing
    ## columns in @var{A}.  Positive elements sort the corresponding columns in
    ## ascending order, while negative elements sort the corresponding columns
    ## in descending order.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{direction})} also specifies the
    ## sorting direction, which can be either @qcode{'ascend'} (default) or
    ## @qcode{'descend'} applying to all columns in @var{A}.  Alternatively,
    ## @var{direction} can be either a string array or a cell array of character
    ## vectors specifying the sorting direction for each individual column of
    ## @var{A}, in which case the number of elements in @var{direction} must
    ## equal the number of columns in @var{A}.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{col}, @var{direction})} sorts the
    ## categorical array @var{A} according to the columns specified in @var{col}
    ## using the corresponding sorting direction specified in @var{direction}.
    ## In this case, the sign of the values in @var{col} is ignored.  @var{col}
    ## and @var{direction} must have the same number of elements, but not
    ## necessarily equal to the columns of @var{A}.
    ##
    ## @code{@var{B} = sortrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where to place the missing elements (@qcode{NaN}) returned in
    ## @var{B} with any of the following options specified in @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @code{[@var{B}, @var{index}] = sortrows (@var{A}, @dots{})} also returns
    ## an index vector containing the original row indices of @var{A} in the
    ## sorted matrix @var{B} such that @code{@var{B} = @var{A}(@var{index},:)}.
    ##
    ## @end deftypefn
    function [B, index] = sortrows (A, varargin)
      ## Input array must be a matrix
      if (ndims (A) != 2)
        error ("duration.sortrows: A must be a 2-D matrix.");
      endif

      ## Parse and validate optional paired arguments
      optNames = {'MissingPlacement', 'ComparisonMethod'};
      dfValues = {'auto', 'auto'};
      [MP, CM, args] = parsePairedArguments (optNames, dfValues, varargin(:));
      if (! ismember (MP, {'auto', 'first', 'last'}))
        error ("duration.sortrows: invalid value for 'MissingPlacement'.");
      endif
      if (! ismember (CM, {'auto', 'real', 'abs'}))
        error ("duration.sortrows: invalid value for 'ComparisonMethod'.");
      endif

      ## Create column sorting vector according to relevant inputs (if any)
      col = [1:columns(A)];
      col_dir = false;
      if (numel (args) > 0)
        col = args{1};
        if (isnumeric (col))
          if (! isvector (col) || fix (col) != col || any (col == 0))
            error ("duration.sortrows: COL must be a vector of nonzero integers.");
          endif
        elseif ((ischar (col) && isvector (col)) ||
                (isscalar (col) && (iscellstr (col) || isstring (col))))
          col = cellstr (col);
          if (strcmpi (col, 'ascend'))
            col = [1:size(A, 2)];
          elseif (strcmpi (col, 'descend'))
            col = -[1:size(A, 2)];
          else
            error (strcat ("duration.sortrows: DIRECTION can", ...
                           " be either 'ascend' or 'descend'."));
          endif
        elseif (iscellstr (col) || isstring (col))
          col = cellstr (col);
          if (! all (ismember (col, {'ascend', 'descend'})))
            error ("duration.sortrows: invalid value for DIRECTION argument.");
          endif
          ncols = columns (A);
          if (numel (col) != ncols)
            error (strcat ("duration.sortrows: DIRECTION does", ...
                           " not match number of columns in A."));
          endif
          idx = strcmpi (col, 'descend');
          col = [1:ncols];
          col(idx) = - col(idx);
        else
          error ("duration.sortrows: invalid type for COL or DIRECTION argument.");
        endif
        col_dir = true;
      endif
      if (numel (args) > 1)
        direction = cellstr (args{2});
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

      ## Apply comparison method (Octave specific)
      data = A.Days;
      if (strcmp (CM, 'abs'))
        data = abs (data);
      endif

      ## FIX ME: this workaround will be removed once the 'sortrows' function
      ## in core Octave supports 'MissingPlacement' optional argument.
      ## This implementation fails the edge case where -Inf and -realmax
      ## elements are present along the operating dimension.
      pos_dir = col > 0;
      neg_dir = col < 0;
      fix_pos_dir = any (pos_dir) && strcmp (MP, {'first'});
      fix_neg_dir = any (neg_dir) && strcmp (MP, {'last'});
      if (fix_pos_dir || fix_neg_dir)
        ## Apply on selected columns
        if (fix_pos_dir)
          col_idx = col(pos_dir);
        else  # must be fix_neg_dir
          col_idx = - col(neg_dir);
        endif
        cdata = data(:,col_idx);
        ## Only if missing data exist in operating columns.
        is_nan = isnan (cdata);
        if (any (is_nan(:)))
          ## Convert missing values to -Inf so that they are placed
          ## appropriately according to 'MissingPlacement' specification.
          ## If -Inf values already exist in data, then convert them to the
          ## next smallest possible value
          is_m_inf = cdata == -Inf;
          if (any (is_m_inf(:)))
            m_inf_rep = - realmax;
            cdata(is_m_inf) = m_inf_rep;
          endif
          cdata(is_nan) = -Inf;
          data(:,col_idx) = cdata;
          ## Sort values
          [~, index] = sortrows (data, col);
        else
          ## Sort values without special handling
          [~, index] = sortrows (data, col);
        endif
      else
        ## Sort values with no undefined elements
        [~, index] = sortrows (data, col);
      endif

      ## Return sorted duration array
      B = subset (A, index, ':');
    endfunction

    function [B, ixA, ixB] = unique (A, varargin)
      ## 'legacy' option is not supported
      if (any (strcmp ("legacy", varargin)))
        error ("duration.unique: 'legacy' option is not supported.");
      endif
      B = A;
      [B.Days, ixA, ixB] = __unique__ (A.Days, varargin{:});
    endfunction

    function BI = interp1 (A, B, AI, varargin)
      A_isDur = isa (A, 'duration');
      B_isDur = isa (B, 'duration');
      AIisDur = isa (AI, 'duration');
      if (xor (A_isDur, AIisDur))
        error ("duration.interp1: if A is a duration array, AI must be also.");
      endif
      if (B_isDur)
        if (A_isDur)
          BI = days (interp1 (A.Days, B.Days, AI.Days, varargin{:}));
        elseif (isnumeric (A) && isnumeric (AI))
          BI = days (interp1 (A, B.Days, AI, varargin{:}));
        else
          error (strcat ("duration.interp1: if A is not a duration", ...
                         " array, then both A and AI must be numeric."));
        endif
        BI = fix_zero_precision (BI);
      elseif (isnumeric (B))
        BI = interp1 (A.Days, B, AI.Days, varargin{:});
      else
        error ("duration.interp1: B must be a duration or numeric array.");
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
## 'repelem'          'repelems'         'reshape'          'circshift'       ##
## 'permute'          'ipermute'         'transpose'        'ctranspose'      ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} cat (@var{dim}, @var{A}, @var{B}, @dots{})
    ##
    ## Concatenate duration arrays.
    ##
    ## @code{@var{C} = cat (@var{dim}, @var{A}, @var{B}, @dots{})} concatenates
    ## duration arrays @var{A}, @var{B}, @dots{} along dimension @var{dim}.  All
    ## input arrays must have the same size except along the operating dimension
    ## @var{dim}.  Any of the input arrays may also be string arrays or cell
    ## arrays of character vectors of compatible size.  Additionally, an input
    ## can be a numeric matrix, which when parsed to the constructor will return
    ## a duration array of compatible size.
    ##
    ## @end deftypefn
    function out = cat (dim, varargin)
      ## If any of the input arrays is a calendarDuration array, then convert
      ## the first input to calendarDuration array and call the corresponding
      ## method.
      if (any (cellfun ('iscalendarduration', varargin)))
        if (isduration (varargin{1}))
          varargin{1} = calendarDuration (0, 0, 0, varargin{1});
        elseif (isnumeric (varargin{1}))
          if (isempty (varargin{i}))
            varargout{i} = calendarDuration ([], [], []);
          else
            varargout{i} = calendarDuration (0, 0, 0, 24 * varargin{i}, 0, 0);
          endif
        else
          error ("calendarDuration: invalid input to constructor.");
        endif
        out = cat (dim, varargin{:});
      else
        args = varargin;
        [args{:}] = promote (varargin{:});
        out = args{1};
        days = cellfun (@(obj) obj.Days, args, 'UniformOutput', false);
        out.Days = cat (dim, days{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} horzcat (@var{A}, @var{B}, @dots{})
    ##
    ## Horizontal concatenation of duration arrays.
    ##
    ## @code{@var{C} = horzcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}, @var{B}, @dots{}]} and horizontally
    ## concatenates the duration arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must have the same size except along the second dimension.  Any of
    ## the input arrays may also be string arrays or cell arrays of character
    ## vectors of compatible size.  Additionally, an input can be a numeric
    ## matrix, which when parsed to the constructor will return a duration array
    ## of compatible size.
    ##
    ## @end deftypefn
    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{C} =} vertcat (@var{A}, @var{B}, @dots{})
    ##
    ## Vertical concatenation of duration arrays.
    ##
    ## @code{@var{C} = vertcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}; @var{B}; @dots{}]} and vertically
    ## concatenates the duration arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must have the same size except along the first dimension.  All of
    ## the input arrays may also be string arrays or cell arrays of character
    ## vectors of compatible size.  Additionally, an input can be a numeric
    ## matrix, which when parsed to the constructor will return a duration array
    ## of compatible size.
    ##
    ## @end deftypefn
    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{B} =} repmat (@var{A}, @var{n})
    ## @deftypefnx {duration} {@var{B} =} repmat (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {duration} {@var{B} =} repmat (@var{A}, @var{dimvec})
    ##
    ## Repeat copies of a duration array.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{n})} returns a duration array
    ## @var{B} containing @var{n} copies of the input duration array @var{A}
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
      this.Days = repmat (this.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{B} =} repelem (@var{A}, @var{n})
    ## @deftypefnx {duration} {@var{B} =} repelem (@var{A}, @var{d1}, @dots{}, @var{dN})
    ##
    ## Repeat copies of duration array elements.
    ##
    ## @code{@var{B} = repelem (@var{A}, @var{n})} returns a duration vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a duration vector.  If @var{n} is a scalar, each element of @var{A} is
    ## repeated @var{n} times along the non-singleton dimension of @var{A}.  If
    ## @var{n} is a vector, it must have the same elements as @var{A}, in which
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
      this.Days = repelem (this.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{B} =} repelems (@var{A}, @var{R})
    ##
    ## Construct a vector of repeated duration array.
    ##
    ## @code{@var{B} = repelems (@var{A}, @var{R})} returns a duration vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a duration vector.  @var{R} must be a @math{2xN} matrix of integers.
    ## Entries in the first row of @var{R} correspond to the linear indexing of
    ## the elements in @var{A} to be repeated.  The corresponding entries in the
    ## second row of @var{R} specify the repeat count of each element.
    ##
    ## @end deftypefn
    function this = repelems (this, R)
      this.Days = repelems (this.Days, R);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{B} =} reshape (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {duration} {@var{B} =} reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})
    ## @deftypefnx {duration} {@var{B} =} reshape (@var{A}, @var{dimvec})
    ##
    ## Reshape duration array.
    ##
    ## @code{@var{B} = reshape (@var{A}, @var{d1}, @dots{}, @var{dN})} returns a
    ## duration array @var{B} with specified dimensions @var{d1}, @dots{},
    ## @var{dN}, whose elements are taken columnwise from the duration array
    ## @var{A}.  The product of @var{d1}, @dots{}, @var{dN} must equal the total
    ## number of elements in @var{A}.
    ##
    ## @code{@var{B} = reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})} returns
    ## a duration array @var{B} with one dimension unspecified which is
    ## calculated automatically so that the product of dimensions in @var{B}
    ## matches the total elements in @var{A}, which must be divisible the
    ## product of specified dimensions.  An empty matrix @qcode{([])} is used to
    ## flag the unspecified dimension.
    ##
    ## @end deftypefn
    function this = reshape (this, varargin)
      this.Days = reshape (this.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {duration} {@var{B} =} circshift (@var{A}, @var{n})
    ## @deftypefnx {duration} {@var{B} =} circshift (@var{A}, @var{n}, @var{dim})
    ##
    ## Circularly shift the elements in a duration array.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n})} circularly shifts the
    ## elements of the duration array @var{A} according to @var{n}.  If @var{n}
    ## is a nonzero integer scalar, then the elements of @var{A} are shifted by
    ## @var{n} elements along the first non-singleton dimension of @var{A}.  If
    ## @var{n} is a vector, it must not be longer that the number of dimensions
    ## of @var{A} with each value of @var{n} corresponding to a dimension in
    ## @var{A}.   The sign of the value(s) in @var{n} specify the direction in
    ## the elements of @var{A} are shifted.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n}, @var{dim})} circularly
    ## shifts the elements of the duration array @var{A} along the dimension
    ## specified by @var{dim}.  In this case, @var{n} must be a scalar integer
    ## value.
    ##
    ## @end deftypefn
    function this = circshift (this, varargin)
      this.Days = circshift (this.Days, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{B} =} permute (@var{A}, @var{dims})
    ##
    ## Generalized transpose for a duration N-D array.
    ##
    ## @code{@var{B} = permute (@var{A}, @var{dims})} returns the generalized
    ## transpose of the duration array @var{A} by rearranging its dimensions
    ## according to the permutation vector specified in @var{dims}.
    ##
    ## @var{dims} must index all the dimensions @code{1:ndims (@var{A})} of the
    ## input array @var{A}, in any order, but only once.  The @var{N}th
    ## dimension of @var{A} gets remapped to the dimension in @var{B} specified
    ## by @code{@var{dims}(@var{N})}.
    ##
    ## @end deftypefn
    function this = permute (this, order)
      this.Days = permute (this.Days, order);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{A} =} ipermute (@var{B}, @var{dims})
    ##
    ## Inverse of the generalized transpose for a duration N-D array.
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
      this.Days = ipermute (this.Days, order);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{B} =} transpose (@var{A})
    ##
    ## Transpose a duration matrix.
    ##
    ## @code{@var{B} = transpose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}.'} and returns the transpose of the duration
    ## matrix @var{A}.
    ##
    ## @end deftypefn
    function this = transpose (this)
      this.Days = transpose (this.Days);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {duration} {@var{B} =} ctranspose (@var{A})
    ##
    ## Transpose a duration matrix.
    ##
    ## @code{@var{B} = ctranspose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}'} and returns the transpose of the duration
    ## matrix @var{A}.  For duration arrays, @code{ctranspose} is identical to
    ## @code{transpose}.
    ##
    ## @end deftypefn
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

    ## Overload 'end' keyword
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
              error ("duration.subsref: unrecognized property: '%s'", s.subs);
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
          if (isempty (val))
            this.Days(s.subs{:}) = [];
          elseif (isa (val, 'missing'))
            this.Days(s.subs{:}) = NaN;
          elseif (iscellstr (val) || ischar (val) || isstring (val))
            if (! iscellstr (val))
              val = cellstr (val);
            endif
            this.Days(s.subs{:}) = timestrings2days (val, []);
          elseif (isnumeric (val))
            this.Days(s.subs{:}) = double (val);
          elseif (isa (val, "duration"))
            this.Days(s.subs{:}) = val.Days;
          else
            error (strcat ("duration.subsasgn: assignment value must", ...
                           " be a duration array, a numeric array of", ...
                           " days, or a text represention of durations."));
          endif

        case '{}'
          error (strcat ("duration.subsasgn: '{}' invalid indexing", ...
                         " for assigning values. Use '()' instead."));

        case '.'
          switch (s.subs)
            case 'Format'
              ## Convert string to character vector if necessary
              if (isstring (val))
                if (! isscalar (val))
                  error (strcat ("duration.subsasgn: 'Format' must be a", ...
                                 " character vector or a string scalar."));
                endif
                val = char (val);
              elseif (! (ischar (val) && isrow (val)))
                error (strcat ("duration.subsasgn: 'Format' must be a", ...
                               " character vector or a string scalar."));
              endif
              errmsg = checkFormatString (val);
              if (! isempty (errmsg))
                error ("duration.subsasgn: %s", errmsg);
              endif
              this.Format = val;
            otherwise
              error ("duration.subsasgn: unrecognized property: '%s'", s.subs);
          endswitch
      endswitch

    endfunction

  endmethods

  methods (Access = private)

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

## Promote numeric and string arrays to duration objects
function varargout = promote (varargin)
  for i = 1:numel (varargin)
    x = varargin{i};
    if (isa (x, "duration"))
      varargout{i} = x;
    elseif (isnumeric (x))
      varargout{i} = days (x);
    elseif (iscellstr (x) || ischar (x) || isa (x, "string"))
      varargout{i} = duration (x);
    else
      error ("duration: invalid input to constructor.");
    endif
  endfor
endfunction

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
    pnd_sign = 1;
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
        if (MI < 0)
          pnd_sign = -1;
          MI = -MI;
        endif
        S  = str2double (cstr{2});
        if (S >= 60)
          S = NaN;
        endif
      elseif (nCols == 2) # 'hh:mm:ss'
        D  = 0;
        H  = str2double (cstr{1});
        if (H < 0)
          pnd_sign = -1;
          H = -H;
        endif
        MI = str2double (cstr{2});
        S  = str2double (cstr{3});
        if (MI >= 60 || S >= 60)
          MI = S = NaN;
        endif
      else  # 'dd:hh:mm:ss'
        D  = str2double (cstr{1});
        if (D < 0)
          pnd_sign = -1;
          D = -D;
        endif
        H  = str2double (cstr{2});
        MI = str2double (cstr{3});
        S  = str2double (cstr{4});
        if (H >= 24 || MI >= 60 || S >= 60)
          H = MI = S = NaN;
        endif
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
          if (MI < 0)
            pnd_sign = -1;
            MI = -MI;
          endif
          S  = str2double (cstr{2});
          MS = 0;
        else  # 'hh:mm'
          D  = 0;
          H  = str2double (cstr{1});
          if (H < 0)
            pnd_sign = -1;
            H = -H;
          endif
          MI = str2double (cstr{2});
          if (MI >= 60)
            MI = NaN;
          endif
          S  = 0;
          MS = 0;
        endif
      elseif (nCols == 2) # 'hh:mm:ss'
        D  = 0;
        H  = str2double (cstr{1});
        if (H < 0)
          pnd_sign = -1;
          H = -H;
        endif
        MI = str2double (cstr{2});
        S  = str2double (cstr{3});
        if (MI >= 60 || S >= 60)
          MI = S = NaN;
        endif
        MS = 0;
      else  # 'dd:hh:mm:ss'
        D  = str2double (cstr{1});
        if (D < 0)
          pnd_sign = -1;
          D = -D;
        endif
        H  = str2double (cstr{2});
        MI = str2double (cstr{3});
        S  = str2double (cstr{4});
        if (H >= 24 || MI >= 60 || S >= 60)
          H = MI = S = NaN;
        endif
        MS = 0;
      endif
    endif
    days(i) = D + (H / 24) + (MI / 1440) + (S / 86400) + (MS / 86400000);
    days(i) *= pnd_sign;
  endfor
endfunction

## Check 'Format' string
function errmsg = checkFormatString (Format)
  errmsg = "";
  Format = strsplit (Format, '.')';
  validFmt = {'y', 'd', 'h', 'm', 's', 'dd:hh:mm:ss','hh:mm:ss','mm:ss','hh:mm'};
  foundFmt = ismember (validFmt, Format(1));
  if (! any (foundFmt) || numel (Format) > 2)
    errmsg = "invalid display format.";
  endif
  if (any (foundFmt([1:5])) && numel (Format) > 1)
    errmsg = "invalid display format.";
  endif
  if (foundFmt(9) && numel (Format) > 1)
    errmsg = "'hh:mm' display format cannot indicate fractional second digits.";
  endif
  if (numel (Format) == 2)
    if (any (char (Format(2)) != 'S'))
      errmsg = "invalid display format for fractional second digits.";
    endif
    if (numel (Format{2}) > 9)
      errmsg = "more than nine fractional second digits in display format.";
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
    errmsg = "'hh:mm' input format cannot indicate fractional second digits.";
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

