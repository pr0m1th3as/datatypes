## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

classdef calendarDuration
  ## -*- texinfo -*-
  ## @deftp {Class} calendarDuration
  ##
  ## Array representing durations of time using flexible-length calendar
  ## date/time units.
  ##
  ## Each @code{calendarDuration} element stores internally the number of whole
  ## months, the number of whole days, and a @code{duration} object representing
  ## hours, minutes, and seconds.  It can be used to simplify calculations on
  ## @code{datetime} arrays involving calendar units.
  ##
  ## @code{calendarDuration} arrays can be created through their constructor by
  ## combining numeric arrays representing individual calendar duration units or
  ## through the functions @code{caldays}, @code{calweeks}, @code{calmonths},
  ## @code{calquarters}, and @code{calyears}, which create calendar durations in
  ## terms of a single calendar duration unit.  These functions are also
  ## available as methods of @code{calendarDuration} arrays to extract
  ## individual calendar duration units as numeric arrays.
  ##
  ## @seealso{datetime, duration}
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
    Format = 'ymdt'
  endproperties

  properties (SetAccess = private)
    ## Whole calendar months
    Months = 0
    ## Whole calendar days
    Days = 0
    ## Time as duration
    Time = duration
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ('%s =\n', in_name);
      endif
      __disp__ (this, 'calendarDuration', in_name);
    endfunction

    ## Custom display
    function disp (this)
      __disp__ (this, 'calendarDuration');
    endfunction

  endmethods

################################################################################
##             ** Create and convert 'calendarDuration' type **               ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'duration'         'dispstrings'      'cellstr'          'char'            ##
## 'datevec'          'time'             'split'            'caldays'         ##
## 'calweeks'         'calmonths'        'calquarters'      'calyears'        ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{calD} =} calendarDuration (@var{X})
    ## @deftypefnx {calendarDuration} {@var{calD} =} calendarDuration (@var{Y}, @var{MO}, @var{D})
    ## @deftypefnx {calendarDuration} {@var{calD} =} calendarDuration (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S})
    ## @deftypefnx {calendarDuration} {@var{calD} =} calendarDuration (@var{Y}, @var{MO}, @var{D}, @var{T})
    ## @deftypefnx {calendarDuration} {@var{calD} =} calendarDuration (@dots{}, @qcode{'Format'}, @var{FMT})
    ##
    ## Create a new array of calendar durations.
    ##
    ## @code{@var{calD} = calendarDuration (@var{X})} returns an array of
    ## calendar durations from numeric matrix @var{X}, which must have either
    ## three or six columns, representing years, months, days, hours, minutes,
    ## and seconds, accordingly.  All but seconds must be represented as whole
    ## duration units by integer values.
    ##
    ## @code{@var{calD} = calendarDuration (@var{Y}, @var{MO}, @var{D})} returns
    ## an array of calendar durations from numeric arrays @var{Y}, @var{MO}, and
    ## @var{D}, which correspond to years, months, and days, respectively.  The
    ## size of @var{calD} is the common size of the numeric input arguments,
    ## which must be of the same size or scalars.  A scalar input functions as a
    ## constant array of the same size as the other inputs.
    ##
    ## @code{@var{calD} = calendarDuration (@var{Y}, @var{MO}, @var{D}, @var{H},
    ## @var{MI}, @var{S})} returns an array of calendar durations from numeric
    ## arrays @var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, and @var{S}, which
    ## correspond to years, months, days, hours, minutes, and seconds,
    ## respectively.  The size of @var{calD} is the common size of the numeric
    ## input arguments, which must be of the same size or scalars.  A scalar
    ## input functions as a constant array of the same size as the other inputs.
    ##
    ## @code{@var{calD} = calendarDuration (@var{Y}, @var{MO}, @var{D},
    ## @var{T})} returns an array of calendar durations from numeric arrays
    ## @var{Y}, @var{MO}, and @var{D}, which correspond to years, months, and
    ## days, as well as a time duration array @var{T}.  The size of @var{calD}
    ## is the common size of the data input arguments, which must be of the same
    ## size or scalars.  A scalar input functions as a constant array of the
    ## same size as the other inputs.
    ##
    ## Numeric input arrays @var{Y}, @var{MO}, @var{D}, @var{H}, and @var{MI}
    ## must contain integer values corresponding to whole calendar units.
    ## @var{S} can also be contain fractions of seconds.
    ##
    ## @code{@var{calD} = calendarDuration (@dots{}, @qcode{'Format'},
    ## @var{FMT})} specifies the format in which @var{calD} is displayed.
    ## @var{FMT} must be a character vector containing the following letters.
    ##
    ## @itemize
    ## @item @qcode{'y'} years
    ## @item @qcode{'q'} quarters of a year
    ## @item @qcode{'m'} months
    ## @item @qcode{'w'} weeks
    ## @item @qcode{'d'} days
    ## @item @qcode{'t'} time duration
    ## @end itemize
    ##
    ## @code{@var{calD} = calendarDuration ()} returns a scalar array of
    ## calendar durations with a value of zero days.  To create an empty
    ## calendarDuration array, use @code{calendarDuration ([], [], [])}.
    ##
    ## @seealso{calyears, calquarters, calmonths, calweeks, caldays,
    ## calendarDuration, iscalendarduration, datetime, duration}
    ## @end deftypefn
    function this = calendarDuration (varargin)

      ## Return a scalar calendarDuration object
      if (nargin == 0)
        return
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'Format'};
      dfValues = {[]};
      [Format, args] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check optional 'Format' argument
      if (! isempty (Format))
        if (! (ischar (Format) && isvector (Format)))
          error ("calendarDuration: 'Format' must be a character vector.");
        else
          errmsg = checkFormatString (Format);
          if (! isempty (errmsg))
            error ("calendarDuration: %s", errmsg);
          endif
          this.Format = Format;
        endif
      endif

      ## Parse inputs
      switch (numel (args))

        ## this = calendarDuration ()
        case 0
          return

        ## this = calendarDuration (X)
        case 1
          X = args{1};
          if (! (isnumeric (X) && ismatrix (X)))
            error ("calendarDuration: X must be a numeric matrix.");
          endif
          if (size (X, 2) == 3)
            tmp = X(:);
            tmp(isnan(tmp)) = 0;
            if (any (fix (tmp) != tmp))
              error (["calendarDuration: years, months,", ...
                      " and days must be integer values."]);
            endif
            Y = X(:,1);
            M = X(:,2);
            D = X(:,3);
            T = duration (zeros (size (X)));
          elseif (size (X, 2) == 6)
            tmp = X(:,[1:5]);
            tmp(isnan(tmp)) = 0;
            if (any (fix (tmp(:)) != tmp(:)))
              error (["calendarDuration: years, months, days,", ...
                      " hours, and minutes must be integer values."]);
            endif
            Y = X(:,1);
            M = X(:,2);
            D = X(:,3);
            T = duration (X(:,[4:6]));
          else
            error ("calendarDuration: X must have either 3 or 6 columns.");
          endif

        ## this = calendarDuration (Y, M, D)
        case 3
          [Y, M, D] = args{:};
          if (! (isnumeric (Y) && isnumeric (M) && isnumeric (D)))
            error ("calendarDuration: Y, MO, and D must be a numeric arrays.");
          endif
          ## Expand as necessary
          if (! isscalar (Y) || ! isscalar (M) || ! isscalar (D))
            [err, Y, M, D] = common_size (Y, M, D);
            if (err > 0)
              error (["calendarDuration: Y, MO, and D must", ...
                      " be of common size or scalars."]);
            endif
          endif
          tmp = [Y(:), M(:), D(:)];
          tmp(isnan(tmp)) = 0;
          if (any (fix (tmp(:)) != tmp(:)))
            error (["calendarDuration: years, months,", ...
                    " and days must be integer values."]);
          endif
          T = repmat (duration (0, 0, 0), size (Y));

        ## this = calendarDuration (Y, M, D, T)
        case 4
          [Y, M, D, T] = args{:};
          if (! (isnumeric (Y) && isnumeric (M) && isnumeric (D)))
            error ("calendarDuration: Y, MO, and D must be a numeric arrays.");
          endif
          if (! isa (T, "duration"))
            error ("calendarDuration: T must be a duration array.");
          endif
          ## Expand as necessary
          t = ones (size (T));
          if (! isscalar (Y) || ! isscalar (M) || ! isscalar (D) || ! isscalar (t))
            [err, Y, M, D, t] = common_size (Y, M, D, t);
            if (err > 0)
              error (["calendarDuration: Y, MO, D, and T", ...
                      " must be of common size or scalars."]);
            endif
            if (! isequal (size (T), size (t)))
              T = repmat (T, size (t));
            endif
          endif
          tmp = [Y(:), M(:), D(:)];
          tmp(isnan(tmp)) = 0;
          if (any (fix (tmp(:)) != tmp(:)))
            error (["calendarDuration: years, months,", ...
                    " and days must be integer values."]);
          endif

        ## this = calendarDuration (Y, M, D, H, MI, S)
        case 6
          [Y, M, D, H, MI, S] = args{:};
          ## Expand as necessary
          if (! isscalar (Y) || ! isscalar (M) || ! isscalar (D) ||
              ! isscalar (H) || ! isscalar (MI) || ! isscalar (S))
            [err, Y, M, D, H, MI, S] = common_size (Y, M, D, H, MI, S);
            if (err > 0)
              error (["calendarDuration: Y, MO, D, H, MI, and", ...
                      " S must be of common size or scalars."]);
            endif
          endif
          tmp = [Y(:), M(:), D(:), H(:), MI(:)];
          tmp(isnan(tmp)) = 0;
          if (any (fix (tmp(:)) != tmp(:)))
            error (["calendarDuration: years, months, days,", ...
                    " hours, and minutes must be integer values."]);
          endif
          T = duration (H, MI, S);

        otherwise
          error ("calendarDuration: invalid number of input arguments.");

      endswitch

      ## Construction
      this.Months = Y * 12 + M;
      this.Days = D;
      this.Time = T;

      ## Broadcast NaNs
      this = broadcastProperties (this);

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{cstr} =} dispstrings (@var{calD})
    ##
    ## Get display formatted strings for each element of a calendarDuration
    ## array.
    ##
    ## @code{@var{cstr} = dispstrings (@var{calD})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## calendarDuration array, @var{calD}.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      ## Process all elements
      sz = size (this);
      cstr = cell (sz);
      for i = 1:prod (sz)
        calDur = subset (this, i);
        if (isnan (calDur.Months))
          cstr{i} = 'NaN';
        elseif (isinf (calDur.Months))
          cstr{i} = num2str (calDur.Months);
        else
          els = {};
          ## Check Format contains 'y' to split between years and months
          if (! isempty (strfind (calDur.Format, 'y')))
            years = fix (calDur.Months / 12);
            months = rem (calDur.Months, 12);
            if (years != 0)
              els{end+1} = sprintf ('%dy', years);
            endif
            if (months != 0)
              els{end+1} = sprintf ('%dmo', months);
            endif
          else
            if (calDur.Months != 0)
              els{end+1} = sprintf ('%dmo', calDur.Months);
            endif
          endif
          if (calDur.Days != 0)
            ## Check Format contains 'w' to print whole weeks and subtract
            ## them from days
            if (! isempty (strfind (calDur.Format, 'w')))
              weeks = fix (calDur.Days / 7);
              if (weeks != 0)
                els{end+1} = sprintf ('%dw', weeks);
                calDur.Days -= weeks * 7;
              endif
            endif
            if (calDur.Days != 0)
              els{end+1} = sprintf ('%dd', calDur.Days);
            endif
          endif
          millis = milliseconds (calDur.Time);
          if (abs (millis) > 4e-12)
            sec = millis / 1000;
            fracSec = rem (sec,1);
            x = fix (sec);
            hours = fix (x / (60 * 60));
            x = rem (x, (60 * 60));
            minutes = fix (x / 60);
            x = rem (x, 60);
            seconds = x;
            msec = round (fracSec * 1000);
            if (abs (msec) == 1000)
              seconds = seconds + (msec / 1000);
              msec = 0;
            endif
            if (msec >= 1)
              str = sprintf ('%ds', msec / 1000);
              str(1) = [];
              els{end+1} = sprintf ('%dh %dm %d%s', hours, minutes, seconds, str);
            elseif (fracSec > 0 && fracSec < 0.001 && seconds == 0)
              els{end+1} = sprintf ('%dh %dm %0.0es', hours, minutes, fracSec);
            else
              els{end+1} = sprintf ('%dh %dm %ds', hours, minutes, seconds);
            endif
          endif
          if isempty (els)
            els = {'0d'};
          endif
          cstr{i} = strjoin (els, ' ');
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{cstr} =} cellstr (@var{calD})
    ## @deftypefnx {calendarDuration} {@var{cstr} =} cellstr (@var{calD}, @var{Format})
    ##
    ## Convert calendarDuration array to a cell array of character vectors.
    ##
    ## @code{@var{cstr} = cellstr (@var{calD})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## calendarDuration @var{calD}.
    ##
    ## @end deftypefn
    function cstr = cellstr (this, Format = '')
      if (! isempty (Format))
        if (! (ischar (Format) && isvector (Format)))
          error ("calendarDuration.cellstr: FORMAT must be a character vector.");
        else
          errmsg = checkFormatString (Format);
          if (! isempty (errmsg))
            error ("calendarDuration.cellstr: %s", errmsg);
          endif
          this.Format = Format;
        endif
      endif
      cstr = dispstrings (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{cmat} =} char (@var{calD})
    ##
    ## Convert calendarDuration array to a character matrix.
    ##
    ## @code{@var{cmat} = char (@var{calD})} returns a character matrix with
    ## one row per element in @var{calD}.
    ##
    ## @end deftypefn
    function cmat = char (this, Format = '')
      cmat = char (cellstr (this, Format));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{DV} =} datevec (@var{calD})
    ##
    ## Convert calendarDuration array to date vectors.
    ##
    ## @code{@var{DV} = datevec (@var{calD})} returns a numeric matrix with
    ## one row per element in @var{calD}.
    ##
    ## @end deftypefn
    function varargout = datevec (this)
      [h, m, s] = hms (this.Time);
      years = fix (this.Months / 12);
      months = rem (this.Months, 12);
      DV = [years(:), months(:), this.Days(:), h(:), m(:), s(:)];
      if (nargout == 0 || nargout == 1)
        varargout{1} = DV;
      elseif (nargout <= 6)
        for i = 1:nargout
          varargout{i} = DV(:,i);
        endfor
      else
        error ("calendarDuration.datavec: too many output arguments.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{T} =} time (@var{calD})
    ##
    ## Return time portion of calendarDuration array.
    ##
    ## @code{@var{T} = time (@var{calD})} returns a duration array @var{T} with
    ## the time portions of the calendarDuration array @var{calD}.
    ##
    ## @end deftypefn
    function T = time (this)
      T = this.Time;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {[@dots{}] =} split (@var{calD}, @var{units})
    ##
    ## Split calendarDuration array into numeric time units.
    ##
    ## @code{[@dots{}] = split (@var{calD}, @var{units})} splits the calendar
    ## duration units in @var{calD} into separate numeric arrays according to
    ## date/time units specified in @var{units}, which must be either a cell
    ## array of character vectors or a string array containing any of the
    ## following date/time units in descending order.
    ##
    ## @enumerate
    ## @item @qcode{'years'}
    ## @item @qcode{'quarters'}
    ## @item @qcode{'months'}
    ## @item @qcode{'weeks'}
    ## @item @qcode{'days'}
    ## @item @qcode{'time'}
    ## @end enumerate
    ##
    ## When a single date/time unit is specified, @var{units} may also be a
    ## character vector.  When @qcode{'time'} is specified in @var{units}, the
    ## corresponding returning argument is a @code{duration} array.  The values
    ## of years, quarters, and months are computed independently from the values
    ## of weeks and days in @var{calD}, with larger units taking precedence when
    ## specified  The same applies for duration arrays, when requested.
    ##
    ## @end deftypefn
    function varargout = split (this, units)
      ## Check input
      if (nargin < 2)
        error ("calendarDuration.datavec: too few input arguments.");
      endif
      units = cellstr (units);
      valid_units = {'years', 'quarters', 'months', 'weeks', 'days', 'time'};
      idx_units = ismember (tolower (units), valid_units);
      if (! all (idx_units))
        error ("calendarDuration.datavec: '%s' is not a valid time unit.", ...
               units{find (! idx_units)});
      endif
      idx_order = cellfun (@(x) find (strcmpi (x, valid_units)), units);
      if (any (diff (idx_order) < 0))
        error (["calendarDuration.datavec: UNITS must", ...
                " be specified in descending order."]);
      endif
      ## Check output
      n_args = numel (units);
      if (nargout != n_args)
        error ("calendarDuration.datavec: wrong number of output arguments.");
      endif
      months = this.Months;
      days = this.Days;
      for i = 1:numel (n_args)
        unit = units{i};
        if (strcmpi (units, 'years'))
          varargout{i} = fix (months / 12);
          months = months - fix (months / 12);
        elseif (strcmpi (units, 'quarters'))
          varargout{i} = fix (months / 3);
          months = months - fix (months / 3);
        elseif (strcmpi (units, 'months'))
          varargout{i} = months .* this.Sign;
        elseif (strcmpi (units, 'weeks'))
          varargout{i} = fix (days / 7);
          days = days - fix (days / 7);
        elseif (strcmpi (units, 'days'))
          varargout{i} = days;
        elseif (strcmpi (units, 'time'))
          varargout{i} = this.Time;
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{X} =} caldays (@var{calD})
    ##
    ## Calendar duration in days.
    ##
    ## @code{@var{X} = caldays (@var{calD})} returns a numeric array with the
    ## number of days as represented in @var{calD}.
    ##
    ## @code{caldays} is also available as a function, in which case it performs
    ## the opposite conversion.
    ##
    ## @seealso{calendarDuration.calyears, calendarDuration.calquarters,
    ## calendarDuration.calmonths, calendarDuration.calweeks, caldays}
    ## @end deftypefn
    function out = caldays (this)
      out = this.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{X} =} calweeks (@var{calD})
    ##
    ## Calendar duration in weeks.
    ##
    ## @code{@var{X} = calweeks (@var{calD})} returns a numeric array with the
    ## number of weeks as represented in @var{calD}.
    ##
    ## @code{calweeks} is also available as a function, in which case it
    ## performs the opposite conversion.
    ##
    ## @seealso{calendarDuration.calyears, calendarDuration.calquarters,
    ## calendarDuration.calmonths, calendarDuration.caldays, calweeks}
    ## @end deftypefn
    function out = calweeks (this)
      out = fix (this.Days / 7);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{X} =} calmonths (@var{calD})
    ##
    ## Calendar duration in months.
    ##
    ## @code{@var{X} = calmonths (@var{calD})} returns a numeric array with the
    ## number of months as represented in @var{calD}.
    ##
    ## @code{calmonths} is also available as a function, in which case it
    ## performs the opposite conversion.
    ##
    ## @seealso{calendarDuration.calyears, calendarDuration.calquarters,
    ## calendarDuration.calweeks, calendarDuration.caldays, calmonths}
    ## @end deftypefn
    function out = calmonths (this)
      out = this.Months;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{X} =} calquarters (@var{calD})
    ##
    ## Calendar duration in quarters.
    ##
    ## @code{@var{X} = calmonths (@var{calD})} returns a numeric array with the
    ## number of quarters as represented in @var{calD}.
    ##
    ## @code{calmonths} is also available as a function, in which case it
    ## performs the opposite conversion.
    ##
    ## @seealso{calendarDuration.calyears, calendarDuration.calmonths,
    ## calendarDuration.calweeks, calendarDuration.caldays, calquarters}
    ## @end deftypefn
    function out = calquarters (this)
      out = fix (this.Months / 3);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{X} =} calyears (@var{calD})
    ##
    ## Calendar duration in years.
    ##
    ## @code{@var{X} = calmonths (@var{calD})} returns a numeric array with the
    ## number of years as represented in @var{calD}.
    ##
    ## @code{calmonths} is also available as a function, in which case it
    ## performs the opposite conversion.
    ##
    ## @seealso{calendarDuration.calquarters, calendarDuration.calmonths,
    ## calendarDuration.calweeks, calendarDuration.caldays, calyears}
    ## @end deftypefn
    function out = calyears (this)
      out = fix (this.Months / 12);
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'size'             'ndims'            'numel'            'nnz'             ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{sz} =} size (@var{calD})
    ## @deftypefnx {calendarDuration} {@var{dim_sz} =} size (@var{calD}, @var{dim})
    ## @deftypefnx {calendarDuration} {@var{dim_sz} =} size (@var{calD}, @var{d1}, @var{d2}, @dots{})
    ## @deftypefnx {calendarDuration} {[@var{rows}, @var{columns}, @dots{}, @var{dim_n_sz}] =} size (@dots{})
    ##
    ## Size of a calendarDuration array.
    ##
    ## @code{@var{sz} = size (@var{calD})} returns a row vector with the size
    ## (number of elements) of each dimension for the calendar duration array
    ## @var{calD}.
    ##
    ## @code{@var{dim_sz} = size (@var{calD}, @var{dim})} returns the size of
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
        sz = size (this.Months, varargin{:});
      else
        sz = size (this.Months);
      endif
      if (nargout == 0 || nargout == 1)
        varargout{1} = sz;
      elseif (numel (sz) != nargout)
        error (["calendarDuration.size: nargout > 1 but does", ...
                " not match number of requested dimensions."]);
      else
        for i = 1:nargout
          varargout{i} = sz(i);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{out} =} ndims (@var{calD})
    ##
    ## Number of dimensions in a calendarDuration array.
    ##
    ## @code{@var{out} = ndims (@var{calD})} returns the number of dimensions
    ## of the calendar duration array @var{calD}.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = ndims (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{out} =} numel (@var{calD})
    ##
    ## Total number of elements in a calendarDuration array.
    ##
    ## For compatibility reasons with Octave's OOP interface and @code{subsasgn}
    ## behavior, calendarDuration's @code{numel} is defined to always return 1.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = 1;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{out} =} nns (@var{calD})
    ##
    ## Number of nonzero elements in calendarDuration array.
    ##
    ## @code{@var{out} = nns (@var{calD})} returns the number of nonzero
    ## elements in the calendar duration array @var{calD}.
    ##
    ## @end deftypefn
    function out = nnz (this)
      m = this.Months(:);
      d = this.Days(:);
      h = hours (this.Time(:));
      out = numel (m) - sum (m == 0 & d == 0 & h == 0);
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'iscolumm'         'isempty'          'isequal'          'isequaln'        ##
## 'isfinite'         'isinf'            'ismatrix'         'ismissing'       ##
## 'isnan'            'isrow'            'isscalar'         'isvector'        ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} iscolumn (@var{calD})
    ##
    ## Return true if calendarDuration array is a column vector.
    ##
    ## @end deftypefn
    function TF = iscolumn (this)
      TF = iscolumn (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isempty (@var{calD})
    ##
    ## Return true if calendarDuration array is empty.
    ##
    ## @end deftypefn
    function TF = isempty (this)
      TF = isempty (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isequal (@var{calD1}, @var{calD2}, @dots{})
    ##
    ## Return true if all calendarDuration arrays are equal.
    ##
    ## @end deftypefn
    function TF = isequal (this, varargin)
      if (numel (varargin) < 1)
        error ("calendarDuration.isequal: too few input arguments.");
      endif
      args = cell (size (varargin));
      for i = 1:numel (varargin)
        if (! isa (varargin{i}, 'calendarDuration'))
          error (["calendarDuration.isequal: all input arguments", ...
                  " must be calendarDuration arrays."]);
        endif
        args(:) = proxyArray (varargin{i});
      endfor
      TF = isequal (proxyArray (this), args{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isequaln (@var{calD1}, @var{calD2}, @dots{})
    ##
    ## Return true if all calendarDuration arrays are equal under the additional
    ## assumption that @qcode{NaN == NaN}.
    ##
    ## @end deftypefn
    function TF = isequaln (this, varargin)
      if (numel (varargin) < 1)
        error ("calendarDuration.isequaln: too few input arguments.");
      endif
      args = cell (size (varargin));
      for i = 1:numel (varargin)
        if (! isa (varargin{i}, 'calendarDuration'))
          error (["calendarDuration.isequaln: all input arguments", ...
                  " must be calendarDuration arrays."]);
        endif
        args(:) = proxyArray (varargin{i});
      endfor
      TF = isequaln (proxyArray (this), args{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isfinite (@var{calD})
    ##
    ## Return a logical array which is true where the elements of
    ## calendarDuration array @var{calD} are finite and false where they are
    ## not.  @var{TF} and @var{calD} are of the same size.
    ##
    ## @end deftypefn
    function TF = isfinite (this)
      TF = isfinite (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isinf (@var{calD})
    ##
    ## Return a logical array which is true where the elements of
    ## calendarDuration array @var{calD} are infinite and false where they are
    ## not.  @var{TF} and @var{calD} are of the same size.
    ##
    ## @end deftypefn
    function TF = isinf (this)
      TF = isinf (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} ismatrix (@var{calD})
    ##
    ## Return true if calendarDuration array is a 2-D array.
    ##
    ## @end deftypefn
    function TF = ismatrix (this)
      TF = ismatrix (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} ismissing (@var{calD})
    ##
    ## Find missing data in a calendar duration array.
    ##
    ## Missing values in calendarDuration arrays are represented by @qcode{NaN},
    ## thus @code{@var{TF} = ismissing (@var{calD})} is equivalent to
    ## @code{@var{TF} = isnan (@var{calD})}.
    ##
    ## Note: @code{ismissing} for calendarDuration arrays does not support a
    ## second @var{Indicator} arguments.
    ##
    ## @end deftypefn
    function TF = ismissing (this, varargin)
      if (nargin > 1)
        error ("calendarDuration.ismissing: Indicators are not supported.");
      endif
      TF = isnan (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isnan (@var{calD})
    ##
    ## Return a logical array which is true where the elements of @var{calD} are
    ## @qcode{NaN} values and false where they are not.  @var{TF} and @var{calD}
    ## are of the same size.
    ##
    ## @end deftypefn
    function TF = isnan (this)
      TF = isnan (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isrow (@var{calD})
    ##
    ## Return true if calendarDuration array is a row vector.
    ##
    ## @end deftypefn
    function TF = isrow (this)
      TF = isrow (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isscalar (@var{calD})
    ##
    ## Return true if calendarDuration array is a scalar.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = isscalar (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isscalar (@var{calD})
    ##
    ## Return true if calendarDuration array is a vector.
    ##
    ## @end deftypefn
    function TF = isvector (this)
      TF = isvector (this.Months);
    endfunction

  endmethods

################################################################################
##                       ** Mathematical Operations **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'minus'            'uminus'           'plus'             'uplus'           ##
## 'times'            'mtimes'                                                ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function out = minus (A, B)
      if (isa (A, 'calendarDuration') && isa (B, 'calendarDuration'))
        out = A;
        out.Months = A.Months - B.Months;
        out.Days = A.Days - B.Days;
        out.Time = A.Time - B.Time;
      elseif (isa (A, 'calendarDuration') && isa (B, 'duration'))
        out = repmat (A, size (B));
        out.Time = A.Time - B;
      elseif (isa (A, 'duration') && isa (B, 'calendarDuration'))
        out = repmat (B, size (A));
        out.Time = A - B.Time;
      elseif (isa (A, 'double'))
        out = repmat (B, size (A));
        out.Time = B.Time - A;
      elseif (isa (B, 'double'))
        out = repmat (A, size (B));
        out.Time = A.Time - B;
      else
        error (["calendarDuration: subtraction is not defined", ...
                " between '%s' and '%s' arrays"], class (A), class (B));
      endif
      out = broadcastProperties (out);
    endfunction

    function out = uminus (A)
      out = A;
      out.Months = -A.Months;
      out.Days = -A.Days;
      out.Time = -A.Time;
    endfunction

    function out = plus (A, B)
      if (isa (A, 'calendarDuration') && isa (B, 'calendarDuration'))
        out = A;
        out.Months = A.Months + B.Months;
        out.Days = A.Days + B.Days;
        out.Time = A.Time + B.Time;
        out = broadcastProperties (out);
      elseif (isa (A, 'calendarDuration') && isa (B, 'duration'))
        out = repmat (A, size (B));
        out.Time = A.Time + B;
      elseif (isa (A, 'duration') && isa (B, 'calendarDuration'))
        out = repmat (B, size (A));
        out.Time = A + B.Time;
      elseif (isa (A, 'double'))
        out = repmat (B, size (A));
        out.Time = B.Time + A;
      elseif (isa (B, 'double'))
        out = repmat (A, size (B));
        out.Time = A.Time + B;
      else
        error (["calendarDuration: addition is not defined", ...
                " between '%s' and '%s' arrays"], class (A), class (B));
      endif
    endfunction

    function out = uplus (A)
      out = A;
      out.Months = A.Months;
      out.Days = A.Days;
      out.Time = A.Time;
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
      error ("calendarDuration.sort: not implemented yet.");
    endfunction

    function [B, index] = sortrows (A, varargin)
      error ("calendarDuration.sortrows: not implemented yet.");
    endfunction

    function [B, ixA, ixB] = unique (A, varargin)
      error ("calendarDuration.unique: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = interp1 (A, B, varargin)
      error ("calendarDuration.interp1: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = intersect (A, B, varargin)
      error ("calendarDuration.intersect: not implemented yet.");
    endfunction

    function [C, index] = setdiff (A, B, varargin)
      error ("calendarDuration.setdiff: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = setxor (A, B, varargin)
      error ("calendarDuration.setxor: not implemented yet.");
    endfunction

    function [C, ixA, ixB] = union (A, B, varargin)
      error ("calendarDuration.union: not implemented yet.");
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
      fieldArgs = cellfun (@(obj) obj.Months, args, 'UniformOutput', false);
      out.Months = cat (dim, fieldArgs{:});
      fieldArgs = cellfun (@(obj) obj.Days, args, 'UniformOutput', false);
      out.Days = cat (dim, fieldArgs{:});
      fieldArgs = cellfun (@(obj) obj.Time, args, 'UniformOutput', false);
      out.Time = cat (dim, fieldArgs{:});
      out = broadcastProperties (out);
    endfunction

    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    function this = repmat (this, varargin)
      this.Months = repmat (this.Months, varargin{:});
      this.Days   = repmat (this.Days, varargin{:});
      this.Time   = repmat (this.Time, varargin{:});
    endfunction

    function this = reshape (this, varargin)
      this.Months = reshape (this.Months, varargin{:});
      this.Days   = reshape (this.Days, varargin{:});
      this.Time   = reshape (this.Time, varargin{:});
    endfunction

    function this = circshift (this, varargin)
      this.Months = circshift (this.Months, varargin{:});
      this.Days   = circshift (this.Days, varargin{:});
      this.Time   = circshift (this.Time, varargin{:});
    endfunction

    function this = permute (this, order)
      this.Months = permute (this.Months, order);
      this.Days   = permute (this.Days, order);
      this.Time   = permute (this.Time, order);
    endfunction

    function this = ipermute (this, order)
      this.Months = ipermute (this.Months, order);
      this.Days   = ipermute (this.Days, order);
      this.Time   = ipermute (this.Time, order);
    endfunction

    function this = transpose (this)
      this.Months = transpose (this.Months);
      this.Days   = transpose (this.Days);
      this.Time   = transpose (this.Time);
    endfunction

    function this = ctranspose (this)
      this.Months = ctranspose (this.Months);
      this.Days   = ctranspose (this.Days);
      this.Time   = ctranspose (this.Time);
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
          out.Months = this.Months(s.subs{:});
          out.Days   = this.Days(s.subs{:});
          out.Time   = this.Time(s.subs{:});

        case '{}'
          error (["calendarDuration.subsref: '{}' invalid indexing", ...
                  " for referencing values. Use '()' instead."]);

        case '.'
          if (! ischar (s.subs))
            error (["calendarDuration.subsref: '.' index argument", ...
                    " must be a character vector."]);
          endif
          switch (s.subs)
            case 'proxyArray'  # used by 'table' class
              out = proxyArray (this);
            case 'Format'
              out = this.Format;
            otherwise
              error ("calendarDuration.subsref: unrecongized property: %s", ...
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
        error ("calendarDuration.subsasgn: chained subscripts not allowed.");
      endif
      switch s.type
        case '()'
          if (! isa (val, "calendarDuration"))
            val = calendarDuration (val);
          endif
          this.Months(s.subs{:}) = val.Months;
          this.Days(s.subs{:})   = val.Days;
          this.Time(s.subs{:})   = val.Time;

        case '{}'
          error (["calendarDuration.subsasgn: '{}' invalid indexing", ...
                  " for assigning values. Use '()' instead."]);

        case '.'
          if (! ischar (s.subs))
            error (["calendarDuration.subsasgn: '.' index argument", ...
                    " must be a character vector."]);
          endif
          switch (s.subs)
            case 'Format'
              errmsg = checkFormatString (val);
              if (! isempty (errmsg))
                error ("calendarDuration.subsargn: %s", errmsg);
              endif
              this.Format = val;
            otherwise
              error ("calendarDuration.subsasgn: unrecongized property: %s", ...
                     s.subs);
          endswitch
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Broadcast properties
    function this = broadcastProperties (this)
      ## Handle NaNs and Infs first
      is_nan = isnan (this.Months) | isnan (this.Days) | isnan (this.Time);
      isPinf = Inf == this.Months | Inf == this.Days | Inf == this.Time;
      isNinf = -Inf == this.Months | -Inf == this.Days | -Inf == this.Time;
      if (any (is_nan(:)) || any (isPinf(:) & isNinf(:)))
        is_nan = is_nan | (isPinf & isNinf);
        isPinf = isPinf & ! is_nan;
        isNinf = isNinf & ! is_nan;
      endif
      ## Broadcast NaNs
      this.Months(is_nan) = NaN;
      this.Days(is_nan) = NaN;
      this.Time(is_nan) = duration ([NaN, NaN, NaN]);
      ## Broadcast Infs
      if (any (isPinf(:)))
        this.Months(isPinf) = Inf;
        this.Days(isPinf) = Inf;
        this.Time(isPinf) = duration ([Inf, Inf, Inf]);
      endif
      if (any (isNinf(:)))
        this.Months(isNinf) = -Inf;
        this.Days(isNinf) = -Inf;
        this.Time(isNinf) = duration ([-Inf, -Inf, -Inf]);
      endif
    endfunction

    ## Promote numeric arrays to calendarDuration objects
    function varargout = promote (varargin)
      for i = 1:numel (varargin)
        if (isa (varargin{i}, "calendarDuration"))
          varargout{i} = varargin{i};
        elseif (isnumeric (varargin{i}))
          ncols = size (varargin{i}, 2);
          if (isscalar (varargin{i}))
            varargout{i} = calendarDuration (0, 0, 0, 24 * varargin{i}, 0, 0);
          elseif (ismatrix (varargin{i}) && (ismember (ncols, [3, 4, 6])))
            varargout{i} = calendarDuration (varargin{i});
          endif
        elseif (iscellstr (X) || ischar (X) || isa (X, "string"))
          varargout{i} = duration (varargin{i});
        else
          error ("duration: invalid input to constructor.");
        endif
      endfor
    endfunction

    ## Create a proxy array for sorting and set operations in tables
    function out = proxyArray (this)
      ## Handle shape (for multicolumn calendarDuration matrix)
      [rows, cols] = size (this);
      if (cols > 1)
        out = [];
        for i = 1:cols
          dt = hours (this.Time(:,i));
          SC = [this.Months(:,i), this.Days(:,i), dt];
          out = [out, SC];
        endfor
      else
        dt = hours (this.Time);
        out = [this.Months, this.Days, dt];
      endif
    endfunction

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.Months = this.Months(varargin{:});
      this.Days   = this.Days(varargin{:});
      this.Time   = this.Time(varargin{:});
    endfunction

  endmethods

endclassdef

## Check 'Format' string
function errmsg = checkFormatString (Format)
  errmsg = '';
  sf = @(x) strfind (Format, x);
  sp = cellfun (sf, {'y','q','m','w','d','t'}, 'UniformOutput', false);
  ## Check for duplicate characters
  if (any (cellfun (@(x) numel (x) > 1, sp)))
    errmsg = "'Format' contains duplicate characters.";
  endif
  ## Check for 'm', 'd', and 't' being present
  if (any (cellfun (@isempty, sp([3,5,6]))))
    errmsg = "'Format' must contain 'm', 'd', and 't'.";
  endif
  ## Check order
  if (any (diff (cell2mat (sp)) < 1))
    errmsg = "'Format' has invalid order of characters.";
  endif
endfunction

