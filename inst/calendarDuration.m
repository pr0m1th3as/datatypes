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

classdef calendarDuration
  ## -*- texinfo -*-
  ## @deftp {datatypes} calendarDuration
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
    ## Display format
    ##
    ## Display format, specified as a character vector or string scalar.  If
    ## specified as a string scalar, it is converted and stored internally as
    ## a character vector.
    ##
    ## @end deftp
    Format = 'ymdt'
  endproperties

  properties (SetAccess = private, Hidden)
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

  methods (Static, Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{E} =} calendarDuration.empty ()
    ## @deftypefnx {calendarDuration} {@var{E} =} calendarDuration.empty (@var{sz})
    ## @deftypefnx {calendarDuration} {@var{E} =} calendarDuration.empty (@var{m}, @var{n}, @dots{})
    ##
    ## Create an empty calendarDuration array.
    ##
    ## @code{@var{E} = calendarDuration.empty ()} returns a @math{0*0} empty
    ## calendarDuration array.  @code{calendarDuration.empty (@var{m}, @var{n},
    ## @dots{})} or @code{calendarDuration.empty (@var{sz})} returns an empty
    ## calendarDuration array of the requested size, which must have at least
    ## one dimension equal to zero.  A lone dimension gives a square size, so
    ## @code{calendarDuration.empty (3)} is an error while
    ## @code{calendarDuration.empty (0)} is @math{0*0}.  As for @code{zeros}, a
    ## negative dimension counts as zero, and a size vector with nothing in it
    ## names no size and gives @math{0*0}.
    ##
    ## @end deftypefn
    function E = empty (varargin)
      if (nargin == 0)
        sz = [0, 0];
      elseif (nargin == 1 && ! isscalar (varargin{1}))
        sz = double (varargin{1}(:)).';
      else
        sz = [varargin{:}];
      endif
      if (! (isnumeric (sz) && isrow (sz) && all (sz == fix (sz))))
        error ("calendarDuration.empty: dimensions must be integer values.");
      endif
      ## A negative dimension is no smaller than none at all, and a size vector
      ## holding no dimensions names no size: 'zeros' and every other array
      ## constructor read both the same way.
      sz = max (sz, 0);
      if (isempty (sz))
        sz = [0, 0];
      elseif (isscalar (sz))
        sz = [sz, sz];
      endif
      if (all (sz != 0))
        error (strcat ("calendarDuration.empty: at least one dimension", ...
                       " must be zero for an empty array."));
      endif
      E = calendarDuration (zeros (sz), zeros (sz), zeros (sz));
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
    ## @deftypefnx {calendarDuration} {@var{calD} =} calendarDuration (@var{calD2})
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
    ## @code{@var{calD} = calendarDuration (@var{calD2})} returns a copy of the
    ## calendarDuration array @var{calD2}, which keeps its size as well as its
    ## @qcode{'Format'} property unless a new format is specified.
    ##
    ## @code{@var{calD} = calendarDuration (@dots{}, @qcode{'Format'},
    ## @var{FMT})} specifies the format in which @var{calD} is displayed.
    ## @var{FMT} must be a character vector or a string scalar containing the
    ## following letters.
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
    ## Each character must be specified only once in the same order as they
    ## appear in the above list.  @qcode{'m'}, @qcode{'d'}, and @qcode{'t'}
    ## characters must always be included in the format specification.  No
    ## other character is accepted.
    ##
    ## @strong{Note:} MATLAB does not reject a format that omits a required
    ## character.  It warns and silently substitutes a repaired format, so that
    ## @qcode{'ymd'} becomes @qcode{'ymdt'}.  An invalid format is an error
    ## here, because rewriting what the user asked for hides the typo that
    ## caused it.
    ##
    ## @strong{Note:} MATLAB also accepts a numeric array in place of @var{T},
    ## which it reads as a count of @emph{milliseconds}, thereby exposing the
    ## internal storage of its @code{duration} type.  This is undocumented and
    ## contradicts MATLAB's own documentation, which requires @var{T} to be a
    ## duration array, and it is not reproduced here.  Use @code{milliseconds
    ## (@var{N})} to say so explicitly.
    ##
    ## @code{@var{calD} = calendarDuration ()} returns a scalar array of
    ## calendar durations with a value of zero days.  To create an empty
    ## calendarDuration array, use @code{calendarDuration ([], [], [])}.  A
    ## @qcode{'Format'} may be given on its own, as in @code{calendarDuration
    ## (@qcode{'Format'}, @qcode{'mdt'})}, which returns the same zero scalar
    ## in that format.  MATLAB requires data alongside the option and rejects
    ## this.
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
      [Format, args] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional 'Format' argument.  It was supplied only if the parser
      ## consumed its Name/Value pair: testing the returned value for emptiness
      ## would read an explicit 'Format', [] as no format having been given.
      fmtGiven = numel (args) != numel (varargin);
      if (fmtGiven)
        [errmsg, Format] = checkFormatString (Format);
        if (! isempty (errmsg))
          error ("calendarDuration: 'Format' %s", errmsg);
        endif
        this.Format = Format;
      endif

      ## Parse inputs
      switch (numel (args))

        ## this = calendarDuration ()
        case 0
          return

        ## this = calendarDuration (X)
        case 1
          X = args{1};
          ## A calendarDuration passes through unchanged, keeping its own
          ## display format unless another one was specified here.
          if (isa (X, 'calendarDuration'))
            if (fmtGiven)
              X.Format = Format;
            endif
            this = X;
            return
          endif
          if (isa (X, 'duration'))
            error (strcat ("calendarDuration: X must be numeric. Convert a", ...
                           " duration array with 'years', 'days', 'hours',", ...
                           " 'minutes', or 'seconds' first."));
          endif
          if (! (isnumeric (X) && ismatrix (X)))
            error ("calendarDuration: X must be a numeric matrix.");
          endif
          if (! isreal (X))
            error ("calendarDuration: X must be real.");
          endif
          if (size (X, 2) == 3)
            tmp = X(:);
            tmp(isnan (tmp)) = 0;
            if (any (fix (tmp) != tmp))
              error (strcat ("calendarDuration: years, months,", ...
                             " and days must be integer values."));
            endif
            Y = X(:,1);
            M = X(:,2);
            D = X(:,3);
            T = duration (zeros (size (X)));
          elseif (size (X, 2) == 6)
            tmp = X(:,[1:5]);
            tmp(isnan (tmp)) = 0;
            if (any (fix (tmp) != tmp, 'all'))
              error (strcat ("calendarDuration: years, months, days,", ...
                             " hours, and minutes must be integer values."));
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
          if (! (isreal (Y) && isreal (M) && isreal (D)))
            error ("calendarDuration: Y, MO, and D must be real.");
          endif
          ## Expand as necessary
          if (! isscalar (Y) || ! isscalar (M) || ! isscalar (D))
            [err, Y, M, D] = common_size (Y, M, D);
            if (err > 0)
              error (strcat ("calendarDuration: Y, MO, and D must", ...
                             " be of common size or scalars."));
            endif
          endif
          tmp = [Y(:), M(:), D(:)];
          tmp(isnan (tmp)) = 0;
          if (any (fix (tmp) != tmp, 'all'))
            error (strcat ("calendarDuration: years, months,", ...
                           " and days must be integer values."));
          endif
          T = repmat (duration (0, 0, 0), size (Y));

        ## this = calendarDuration (Y, M, D, T)
        case 4
          [Y, M, D, T] = args{:};
          if (! (isnumeric (Y) && isnumeric (M) && isnumeric (D)))
            error ("calendarDuration: Y, MO, and D must be a numeric arrays.");
          endif
          if (! (isreal (Y) && isreal (M) && isreal (D)))
            error ("calendarDuration: Y, MO, and D must be real.");
          endif
          if (! isa (T, "duration"))
            error ("calendarDuration: T must be a duration array.");
          endif
          ## Expand as necessary
          t = ones (size (T));
          if (! isscalar (Y) || ! isscalar (M) || ! isscalar (D) || ...
              ! isscalar (t))
            [err, Y, M, D, t] = common_size (Y, M, D, t);
            if (err > 0)
              error (strcat ("calendarDuration: Y, MO, D, and T", ...
                             " must be of common size or scalars."));
            endif
            if (! isequal (size (T), size (t)))
              T = repmat (T, size (t));
            endif
          endif
          tmp = [Y(:), M(:), D(:)];
          tmp(isnan (tmp)) = 0;
          if (any (fix (tmp) != tmp, 'all'))
            error (strcat ("calendarDuration: years, months,", ...
                           " and days must be integer values."));
          endif

        ## this = calendarDuration (Y, M, D, H, MI, S)
        case 6
          [Y, M, D, H, MI, S] = args{:};
          if (! (isnumeric (Y) && isnumeric (M) && isnumeric (D) &&
                 isnumeric (H) && isnumeric (MI) && isnumeric (S)))
            error (strcat ("calendarDuration: Y, MO, D, H, MI,", ...
                           " and S must be numeric arrays."));
          endif
          if (! (isreal (Y) && isreal (M) && isreal (D) &&
                 isreal (H) && isreal (MI) && isreal (S)))
            error ("calendarDuration: numeric input data must be real.");
          endif
          ## Expand as necessary
          if (! isscalar (Y) || ! isscalar (M) || ! isscalar (D) ||
              ! isscalar (H) || ! isscalar (MI) || ! isscalar (S))
            [err, Y, M, D, H, MI, S] = common_size (Y, M, D, H, MI, S);
            if (err > 0)
              error (strcat ("calendarDuration: Y, MO, D, H, MI, and", ...
                             " S must be of common size or scalars."));
            endif
          endif
          tmp = [Y(:), M(:), D(:), H(:), MI(:)];
          tmp(isnan (tmp)) = 0;
          if (any (fix (tmp) != tmp, 'all'))
            error (strcat ("calendarDuration: years, months, days,", ...
                           " hours, and minutes must be integer values."));
          endif
          T = duration (H, MI, S);

        otherwise
          error ("calendarDuration: invalid number of input arguments.");

      endswitch

      ## Construction
      this.Months = double (Y) * 12 + double (M);
      this.Days = double (D);
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
    ## calendarDuration array, @var{calD}.  The returned text representations
    ## in @var{cstr} are formatted according to the @qcode{'Format'} property
    ## of the input array @var{calD}.
    ##
    ## Whole calendar units are rendered with up to six significant digits and
    ## the seconds component with five, each switching to exponent notation
    ## beyond that, so a million days reads @qcode{'1e+06d'}.  Rounding applies
    ## to a component on its own and never carries into the one above it, which
    ## is why @code{59.9999} seconds read @qcode{'60s'} rather than a further
    ## minute.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      ## Process all elements
      sz = size (this);
      cstr = cell (sz);
      allels = cell (sz);
      ## An element with nothing in it renders as a single zero, and the unit
      ## that zero carries is taken from the rest of the array: the smallest
      ## unit any element actually uses, or days when none uses any.  So a zero
      ## beside '2mo' reads '0mo', beside '3d' reads '0d', and beside a time
      ## reads '0s'.  MATLAB does the same; it is what stops a lone zero
      ## claiming a unit the array never mentions.
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
              els{end+1} = sprintf ('%sy', fmtUnit (years));
            endif
            if (months != 0)
              ## Check Format contains 'q' to split between quarters and months
              if (! isempty (strfind (calDur.Format, 'q')))
                quarters = fix (months / 3);
                months = rem (months, 3);
                if (quarters != 0)
                  els{end+1} = sprintf ('%sq', fmtUnit (quarters));
                endif
                if (months != 0)
                  els{end+1} = sprintf ('%smo', fmtUnit (months));
                endif
              else
                els{end+1} = sprintf ('%smo', fmtUnit (months));
              endif
            endif
          else
            if (calDur.Months != 0)
              ## Check Format contains 'q' to split between quarters and months
              if (! isempty (strfind (calDur.Format, 'q')))
                quarters = fix (calDur.Months / 3);
                months = rem (calDur.Months, 3);
                if (quarters != 0)
                  els{end+1} = sprintf ('%sq', fmtUnit (quarters));
                endif
                if (months != 0)
                  els{end+1} = sprintf ('%smo', fmtUnit (months));
                endif
              else
                els{end+1} = sprintf ('%smo', fmtUnit (calDur.Months));
              endif
            endif
          endif
          if (calDur.Days != 0)
            ## Check Format contains 'w' to print whole weeks and subtract
            ## them from days
            if (! isempty (strfind (calDur.Format, 'w')))
              weeks = fix (calDur.Days / 7);
              if (weeks != 0)
                els{end+1} = sprintf ('%sw', fmtUnit (weeks));
                calDur.Days -= weeks * 7;
              endif
            endif
            if (calDur.Days != 0)
              els{end+1} = sprintf ('%sd', fmtUnit (calDur.Days));
            endif
          endif
          millis = milliseconds (calDur.Time);
          if (abs (millis) > 4e-12)
            ## Every component is truncated towards zero, so all three carry
            ## the sign of the time as a whole.
            sec = millis / 1000;
            hours = fix (sec / 3600);
            rest = sec - hours * 3600;
            minutes = fix (rest / 60);
            ## The seconds component keeps whatever fraction it has, down to
            ## the sub-millisecond, and is rounded only by the five significant
            ## digits it is rendered with.  Rounding is per component and never
            ## carries into the one above, so 59.9999 seconds render as '60s'.
            seconds = rest - minutes * 60;
            els{end+1} = sprintf ('%sh %sm %ss', fmtUnit (hours), ...
                                  fmtUnit (minutes), fmtSecs (seconds));
          endif
          ## Left empty for now; the unit is chosen once the whole array
          ## has been rendered (see zeroRenderUnit below).
          cstr{i} = strjoin (els, ' ');
          allels{i} = els;
        endif
      endfor
      ## Give every wholly zero element the smallest unit the array actually
      ## rendered anywhere, so a zero never claims a unit nothing else uses.
      ## NaN elements also render no components, but they already carry their
      ## own string, so key on the rendered text rather than the component list.
      empty = cellfun (@isempty, cstr);
      if (any (empty(:)))
        cstr(empty) = {zeroRenderUnit(allels(! empty))};
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{cstr} =} cellstr (@var{calD})
    ## @deftypefnx {calendarDuration} {@var{cstr} =} cellstr (@var{calD}, @var{FMT})
    ##
    ## Convert calendarDuration array to a cell array of character vectors.
    ##
    ## @code{@var{cstr} = cellstr (@var{calD})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## @var{calD}.  @var{cstr} contains the string representations of the
    ## calendar durations in @var{calD}.
    ##
    ## @code{@var{cstr} = cellstr (@var{calD}, @var{FMT})} further specifies
    ## the format of the returned string representations.  @var{FMT} must be a
    ## character vector conforming to the same specifications required by the
    ## constructor's @qcode{'Format'} property paired argument.  Note that
    ## @code{cellstr} only accepts @var{FMT} as a single argument and not as a
    ## property paired argument.
    ##
    ## @end deftypefn
    function cstr = cellstr (this, FMT = '')
      if (! isempty (FMT))
        [errmsg, FMT] = checkFormatString (FMT);
        if (! isempty (errmsg))
          error ("calendarDuration.cellstr: FMT %s", errmsg);
        endif
        this.Format = FMT;
      endif
      cstr = dispstrings (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{cmat} =} char (@var{calD})
    ## @deftypefnx {calendarDuration} {@var{cmat} =} char (@var{calD}, @var{FMT})
    ##
    ## Convert calendarDuration array to a character matrix.
    ##
    ## @code{@var{cmat} = char (@var{calD})} returns a character matrix with
    ## one row per element in @var{calD}, taken in column-major order.  The
    ## second optional argument, @var{FMT}, can be used to specify the format of
    ## the returned string representations of the calendarDuration input array
    ## @var{calD}.
    ##
    ## Rows shorter than the widest are padded on the @emph{left}, so the
    ## character matrix is right-justified, as MATLAB returns it and as the
    ## array itself is displayed.
    ##
    ## @end deftypefn
    function cmat = char (this, FMT = '')
      if (! isempty (FMT))
        [errmsg, FMT] = checkFormatString (FMT);
        if (! isempty (errmsg))
          error ("calendarDuration.char: FMT %s", errmsg);
        endif
        this.Format = FMT;
      endif
      cmat = strjust (char (dispstrings (this)), 'right');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{DV} =} datevec (@var{calD})
    ## @deftypefnx {calendarDuration} {[@var{Y}, @var{MO}] =} datevec (@var{calD})
    ## @deftypefnx {calendarDuration} {[@var{Y}, @var{MO}, @var{D}] =} datevec (@var{calD})
    ## @deftypefnx {calendarDuration} {[@var{Y}, @var{MO}, @var{D}, @var{H}] =} datevec (@var{calD})
    ## @deftypefnx {calendarDuration} {[@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}] =} datevec (@var{calD})
    ## @deftypefnx {calendarDuration} {[@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S}] =} datevec (@var{calD})
    ##
    ## Convert calendarDuration array to date vectors.
    ##
    ## @code{@var{DV} = datevec (@var{calD})} returns an @math{N*6} numeric
    ## matrix, where @math{N} is the number of elements in @var{calD} and the
    ## columns corresponds to years, months, days, hours, minutes, and seconds,
    ## respectively.
    ##
    ## When @code{datevec} is called with more than one output arguments, then
    ## it returns the components of the date vectors as individual variables
    ## @var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI},and @var{S} corresponding
    ## to years, months, days, hours, minutes, and seconds, respectively.  In
    ## this case, the individual variables have the same size as the input array
    ## @var{calD}.
    ##
    ## Every component carries the sign of the span as a whole, so a negative
    ## calendar duration returns negative components throughout.  An element
    ## that is not finite has no components to divide between: all six take that
    ## same infinity, or @code{NaN}, which is what @code{split} returns for it
    ## as well.
    ##
    ## @end deftypefn
    function varargout = datevec (this)
      [h, m, s] = hms (this.Time);
      years = fix (this.Months / 12);
      months = rem (this.Months, 12);
      ## An infinite span divides into no whole years with a remainder: every
      ## one of its components is that same infinity, which is what 'split'
      ## returns for it too.  'rem' would leave NaN in the months alone.
      nf = ! isfinite (this.Months);
      if (any (nf, 'all'))
        months(nf) = this.Months(nf);
      endif
      DV = [years(:), months(:), this.Days(:), h(:), m(:), s(:)];
      if (nargout == 0 || nargout == 1)
        varargout{1} = DV;
      elseif (nargout <= 6)
        varargout{1} = years;
        varargout{2} = months;
        if (nargout > 2)
          varargout{3} = this.Days;
        endif
        if (nargout > 3)
          varargout{4} = h;
        endif
        if (nargout > 4)
          varargout{5} = m;
        endif
        if (nargout > 5)
          varargout{6} = s;
        endif
      else
        error ("calendarDuration.datevec: too many output arguments.");
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
    ## Split calendarDuration array into numeric and duration units.
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
    ## corresponding returned argument is a @code{duration} array.  The values
    ## of years, quarters, and months are computed independently from the values
    ## of weeks and days in @var{calD}, with larger units taking precedence when
    ## specified  The same applies for duration arrays, when requested.
    ##
    ## Each unit may be abbreviated to any leading part of its name and is
    ## matched without regard to case, so @qcode{'y'}, @qcode{'Year'} and
    ## @qcode{'YEARS'} all name years.  No abbreviation is ambiguous, the six
    ## names starting with six different letters.  A unit named more than once
    ## still asks for a single component.
    ##
    ## Fewer output arguments than units may be requested, in which case only
    ## the leading ones are returned; asking for more is an error.  An element
    ## that is not finite keeps that same value in every component it is divided
    ## into, so splitting an infinite calendar duration gives infinite years,
    ## months and days alike.
    ##
    ## @end deftypefn
    function varargout = split (this, units)
      ## Check input
      if (nargin < 2)
        error ("calendarDuration.split: too few input arguments.");
      endif
      if (ischar (units) && ndims (units) == 2 && size (units, 1) <= 1)
        ## A character vector names one unit and is taken as it stands: routing
        ## it through 'cellstr' would strip trailing blanks and quietly accept
        ## a name that is not one.
        units = {units};
      elseif (isstring (units) || ischar (units))
        units = cellstr (units);
      elseif (! iscellstr (units))
        error ("calendarDuration.split: invalid input type for UNITS.");
      endif
      if (isempty (units))
        error ("calendarDuration.split: UNITS must name at least one unit.");
      endif
      valid_units = {'years', 'quarters', 'months', 'weeks', 'days', 'time'};
      ## A unit may be abbreviated to any leading part of its name and is
      ## matched without regard to case, so 'y', 'Year' and 'YEARS' all name
      ## years.  No abbreviation is ambiguous, the six names starting with six
      ## different letters.
      idx_order = zeros (1, numel (units));
      for i = 1:numel (units)
        unit = units{i};
        if (isempty (unit))
          error ("calendarDuration.split: UNITS must not contain empty names.");
        endif
        found = strncmpi (unit, valid_units, numel (unit));
        if (! any (found))
          error ("calendarDuration.split: '%s' is not a valid time unit.", unit);
        endif
        idx_order(i) = find (found, 1);
      endfor
      if (any (diff (idx_order) < 0))
        error (strcat ("calendarDuration.split: UNITS must", ...
                       " be specified in descending order."));
      endif
      ## A unit named more than once still asks for one component.
      idx_order = unique (idx_order, 'stable');
      ## Check output.  Fewer outputs than units may be requested, in which case
      ## only the leading ones are returned; a lone output is always returned,
      ## even where none was asked for.
      n_args = numel (idx_order);
      if (nargout > n_args)
        error ("calendarDuration.split: too many output arguments.");
      endif
      n_out = max (nargout, 1);
      ## An element that is not finite keeps that same value in every component
      ## it is divided into: subtracting whole units off an infinity otherwise
      ## leaves NaN behind in the remainder.
      months = this.Months;
      days = this.Days;
      nf_months = ! isfinite (months);
      nf_days = ! isfinite (days);
      for i = 1:n_out
        switch (idx_order(i))
          case 1
            years = fix (months / 12);
            months = months - years * 12;
            months(nf_months) = this.Months(nf_months);
            varargout{i} = years;
          case 2
            quarters = fix (months / 3);
            months = months - quarters * 3;
            months(nf_months) = this.Months(nf_months);
            varargout{i} = quarters;
          case 3
            varargout{i} = months;
          case 4
            weeks = fix (days / 7);
            days = days - weeks * 7;
            days(nf_days) = this.Days(nf_days);
            varargout{i} = weeks;
          case 5
            varargout{i} = days;
          case 6
            varargout{i} = this.Time;
        endswitch
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
    ## A month has no fixed number of days, so an array carrying any month at
    ## all cannot be stated in days and is an error rather than a guess.  Use
    ## @code{split} on such an array, which reports each unit separately.  The
    ## sign plays no part: a negative number of months is refused exactly as a
    ## positive one is.
    ##
    ## A @code{NaN} or infinite month count is not refused.  It is not known to
    ## be non-zero, so it propagates as @code{NaN} or @code{Inf}, just as
    ## @code{calmonths}, @code{calquarters} and @code{calyears} already report
    ## it.  MATLAB instead refuses such a span when it was built by the
    ## constructor, while accepting the very same span built by @code{caldays};
    ## two spellings of one value cannot sensibly disagree, so both are
    ## accepted here.
    ##
    ## @code{caldays} is also available as a function, in which case it performs
    ## the opposite conversion.
    ##
    ## @seealso{calendarDuration.calyears, calendarDuration.calquarters,
    ## calendarDuration.calmonths, calendarDuration.calweeks, caldays}
    ## @end deftypefn
    function out = caldays (this)
      if (any (this.Months != 0 & isfinite (this.Months), 'all'))
        error ("calendarDuration.caldays: %s", monthlessErrMsg ());
      endif
      out = this.Days;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{X} =} calweeks (@var{calD})
    ##
    ## Calendar duration in weeks.
    ##
    ## @code{@var{X} = calweeks (@var{calD})} returns a numeric array with the
    ## number of weeks as represented in @var{calD}, rounded towards negative
    ## infinity, so that @math{-15} days is @math{-3} weeks and not @math{-2}.
    ##
    ## As for @code{caldays}, an array carrying any month at all cannot be
    ## stated in weeks and is an error rather than a guess, whatever the sign
    ## of that month count.  Use @code{split} on such an array.  A @code{NaN}
    ## or infinite month count propagates rather than being refused, on the
    ## same reasoning as @code{caldays}.
    ##
    ## @code{calweeks} is also available as a function, in which case it
    ## performs the opposite conversion.
    ##
    ## @seealso{calendarDuration.calyears, calendarDuration.calquarters,
    ## calendarDuration.calmonths, calendarDuration.caldays, calweeks}
    ## @end deftypefn
    function out = calweeks (this)
      if (any (this.Months != 0 & isfinite (this.Months), 'all'))
        error ("calendarDuration.calweeks: %s", monthlessErrMsg ());
      endif
      out = floor (this.Days / 7);
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
    ## @code{@var{X} = calquarters (@var{calD})} returns a numeric array with
    ## the number of quarters as represented in @var{calD}, rounded towards
    ## negative infinity, so that @math{-1} month is @math{-1} quarter.
    ##
    ## @code{calquarters} is also available as a function, in which case it
    ## performs the opposite conversion.
    ##
    ## @seealso{calendarDuration.calyears, calendarDuration.calmonths,
    ## calendarDuration.calweeks, calendarDuration.caldays, calquarters}
    ## @end deftypefn
    function out = calquarters (this)
      out = floor (this.Months / 3);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{X} =} calyears (@var{calD})
    ##
    ## Calendar duration in years.
    ##
    ## @code{@var{X} = calyears (@var{calD})} returns a numeric array with the
    ## number of years as represented in @var{calD}, rounded towards negative
    ## infinity, so that @math{-1} month is @math{-1} year.
    ##
    ## @code{calyears} is also available as a function, in which case it
    ## performs the opposite conversion.
    ##
    ## @seealso{calendarDuration.calquarters, calendarDuration.calmonths,
    ## calendarDuration.calweeks, calendarDuration.caldays, calyears}
    ## @end deftypefn
    function out = calyears (this)
      out = floor (this.Months / 12);
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
        error (strcat ("calendarDuration.size: number of output arguments", ...
                       " does not match number of requested dimensions."));
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
    ## @code{@var{out} = numel (@var{calD})} returns the number of elements in
    ## the calendarDuration array @var{calD}.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = numel (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{out} =} nnz (@var{calD})
    ##
    ## Number of nonzero elements in calendarDuration array.
    ##
    ## @code{@var{out} = nnz (@var{calD})} returns the number of nonzero
    ## elements in the calendar duration array @var{calD}.
    ##
    ## @end deftypefn
    function out = nnz (this)
      m = this.Months(:);
      d = this.Days(:);
      h = hours (this.Time(:));
      out = numel (m) - sum (m == 0 & d == 0 & h == 0);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{N} =} length (@var{calD})
    ##
    ## Length of a calendarDuration vector.
    ##
    ## @code{@var{N} = length (@var{calD})} returns the size of the longest
    ## dimension of the calendarDuration array @var{calD}, unless any of its
    ## dimensions has zero length, in which case @code{length (@var{calD})}
    ## returns 0.
    ##
    ## @end deftypefn
    function N = length (this)
      if (isempty (this.Months))
        N = 0;
      else
        N = max (size (this.Months));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{hey} =} keyHash (@var{calD})
    ## @deftypefnx {calendarDuration} {@var{hey} =} keyHash (@var{calD}, @var{base})
    ##
    ## Generate a hash code for a calendarDuration array.
    ##
    ## @code{@var{h} = keyHash (@var{calD})} generates a @qcode{uint64} scalar
    ## that represents the input array @var{calD}.  @code{keyHash} utilizes the
    ## 64-bit FNV-1a variant of the Fowler-Noll-Vo non-cryptographic hash
    ## function.
    ##
    ## @code{@var{h} = keyHash (@var{calD}), @var{base}} also generates a 64-bit
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
      size_str = sprintf ('%dx', size (this.Months))(1:end-1);
      init_str = [size_str 'calendarDuration'];
      if (base)
        if (! (isscalar (base) && isa (base, 'uint64')))
          error ("calendarDuration.keyHash: BASE must be a UINT64 scalar.");
        endif
        key = __ckeyHash__(init_str, base);
      else
        key = __ckeyHash__(init_str);
      endif
      ## Compute hash with underlying calendarDuration array values
      if (! isempty (this.Months))
        key = __nkeyHash__(this.Months(:), key);
        key = __nkeyHash__(this.Days(:), key);
        key = keyHash (this.Time, key);
      endif
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'iscolumn'         'isempty'          'isequal'          'isequaln'        ##
## 'isfinite'         'isinf'            'ismatrix'         'ismissing'       ##
## 'isnan'            'isreal'           'isrow'            'isscalar'        ##
## 'isvector'                                                                 ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} iscolumn (@var{calD})
    ##
    ## Return true if calendarDuration array is a column vector.
    ##
    ## @code{@var{TF} = iscolumn (@var{calD})} returns a logical scalar
    ## @var{TF}, which is @qcode{true} if the calendar duration array @var{calD}
    ## is a column vector and @qcode{false} otherwise.  A column vector is a 2-D
    ## array for which @code{size (@var{X})} returns @code{[@var{N}, 1]} with
    ## non-negative @var{N}.
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
    ## @code{@var{TF} = isempty (@var{calD})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the calendar duration array @var{calD} is empty
    ## and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isempty (this)
      TF = isempty (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{TF} =} isequal (@var{calD1}, @var{calD2})
    ## @deftypefnx {calendarDuration} {@var{TF} =} isequal (@var{calD1}, @var{calD2}, @dots{})
    ##
    ## Return true if calendarDuration arrays are equal.
    ##
    ## @code{@var{TF} = isequal (@var{calD1}, @var{calD2})} returns a logical
    ## scalar @var{TF}, which is @qcode{true} if the calendar duration arrays
    ## @var{calD1} and @var{calD2} contain the same values and @qcode{false}
    ## otherwise.
    ##
    ## @code{@var{TF} = isequal (@var{calD1}, @var{calD2}, @dots{})} returns a
    ## logical scalar @var{TF}, which is @qcode{true} if all input arguments are
    ## calendar duration arrays with equal values in each corresponding elements
    ## and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isequal (this, varargin)
      if (numel (varargin) < 1)
        error ("calendarDuration.isequal: too few input arguments.");
      endif
      n_dim = size (this);
      for i = 1:numel (varargin)
        tmp = varargin{i};
        if (! isa (tmp, 'calendarDuration'))
          ## isequal answers about any pair of values and never refuses one:
          ## an argument of another class simply is not equal to a span.
          TF = false;
          return;
        endif
        if (! isequal (n_dim, size (tmp)))
          TF = false;
          return;
        endif
        if (this == tmp)
          TF = true;
        else
          TF = false;
          return;
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{TF} =} isequaln (@var{calD1}, @var{calD2})
    ## @deftypefnx {calendarDuration} {@var{TF} =} isequaln (@var{calD1}, @var{calD2}, @dots{})
    ##
    ## Return true if calendarDuration arrays are equal under the assumption
    ## that missing elements are equal.
    ##
    ## @code{@var{TF} = isequaln (@var{calD1}, @var{calD2})} returns a logical
    ## scalar @var{TF}, which is @qcode{true} if the calendar duration arrays
    ## @var{calD1} and @var{calD2} contain the same values or corresponding
    ## missing elements and @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = isequaln (@var{calD1}, @var{calD2}, @dots{})} returns a
    ## logical scalar @var{TF}, which is @qcode{true} if all input arguments
    ## are calendar duration arrays with equal values or corresponding missing
    ## elements and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isequaln (this, varargin)
      if (numel (varargin) < 1)
        error ("calendarDuration.isequaln: too few input arguments.");
      endif
      n_dim = size (this);
      ## Force NaNs to zeros
      i_nan = isnan (this);
      if (any (i_nan, 'all'))
        this.Months(i_nan) = 0;
        this.Days(i_nan) = 0;
        this.Time(i_nan) = duration (0, 0, 0);
      endif
      for i = 1:numel (varargin)
        tmp = varargin{i};
        if (! isa (tmp, 'calendarDuration'))
          ## isequal answers about any pair of values and never refuses one:
          ## an argument of another class simply is not equal to a span.
          TF = false;
          return;
        endif
        if (! isequal (n_dim, size (tmp)))
          TF = false;
          return;
        endif
        ## Force NaNs to zeros
        i_nan = isnan (tmp);
        if (any (i_nan, 'all'))
          tmp.Months(i_nan) = 0;
          tmp.Days(i_nan) = 0;
          tmp.Time(i_nan) = duration (0, 0, 0);
        endif
        if (this == tmp)
          TF = true;
        else
          TF = false;
          return;
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isfinite (@var{calD})
    ##
    ## Return true for calendar durations that are finite.
    ##
    ## @code{@var{TF} = isfinite (@var{calD})} returns a logical array @var{TF}
    ## of the same size as @var{calD} containing @qcode{true} for each
    ## corresponding element of @var{calD} that is finite and @qcode{false}
    ## otherwise.  Finite elements are those which are neither infinite nor
    ## Not-A-Number.
    ##
    ## @end deftypefn
    function TF = isfinite (this)
      TF = isfinite (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isinf (@var{calD})
    ##
    ## Return true for calendar durations that are infinite.
    ##
    ## @code{@var{TF} = isinf (@var{calD})} returns a logical array @var{TF}
    ## of the same size as @var{calD} containing @qcode{true} for each
    ## corresponding element of @var{calD} that is either @qcode{Inf} or
    ## @qcode{-Inf} and @qcode{false} otherwise.
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
    ## @code{@var{TF} = ismatrix (@var{calD})} returns a logical scalar
    ## @var{TF}, which is @qcode{true} if the calendarDuration array @var{calD}
    ## is a matrix and @qcode{false} otherwise.  A matrix is an array of any
    ## type where @code{ndims (@var{X}) == 2} and for which
    ## @code{size (@var{X})} returns @code{[@var{H}, @var{W}]} with non-negative
    ## @var{H} and @var{W}.
    ##
    ## @end deftypefn
    function TF = ismatrix (this)
      TF = ismatrix (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} ismissing (@var{calD})
    ##
    ## Find missing data in a calendarDuration array.
    ##
    ## Missing values in calendarDuration arrays are represented by @qcode{NaN},
    ## thus @code{@var{TF} = ismissing (@var{calD})} is equivalent to
    ## @code{@var{TF} = isnan (@var{calD})}.
    ##
    ## Note: @code{ismissing} for calendarDuration arrays does not support a
    ## second @var{Indicator} argument.
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
    ## Return true for calendar durations that are Not-A-Number.
    ##
    ## @code{@var{TF} = isnan (@var{calD})} returns a logical array @var{TF}
    ## of the same size as @var{calD} containing @qcode{true} for each
    ## corresponding element of @var{calD} that is @qcode{NaN} and @qcode{false}
    ## otherwise.
    ##
    ## @end deftypefn
    function TF = isnan (this)
      TF = isnan (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isreal (@var{calD})
    ##
    ## Always return true for calendarDuration arrays.
    ##
    ## @code{@var{TF} = isreal (@var{calD})} always returns a logical scalar
    ## @qcode{true} value, if the input argument is a calendarDuration array.
    ##
    ## @end deftypefn
    function TF = isreal (this)
      TF = true;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isrow (@var{calD})
    ##
    ## Return true if calendarDuration array is a row vector.
    ##
    ## @code{@var{TF} = isrow (@var{calD})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the calendarDuration array @var{calD} is a row
    ## vector and @qcode{false} otherwise.  A row vector is a 2-D array for
    ## which @code{size (@var{X})} returns @code{[1, @var{N}]} with non-negative
    ## @var{N}.
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
    ## @code{@var{TF} = isscalar (@var{calD})} returns a logical scalar
    ## @var{TF}, which is @qcode{true} if the calendarDuration array @var{calD}
    ## is also a scalar and @qcode{false} otherwise.  A scalar is a single
    ## element object for which @code{size (@var{X})} returns @code{[1, 1]}.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = isscalar (this.Months);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} isvector (@var{calD})
    ##
    ## Return true if calendarDuration array is a vector.
    ##
    ## @code{@var{TF} = isvector (@var{calD})} returns a logical scalar
    ## @var{TF}, which is @qcode{true} if the calendarDuration array @var{calD}
    ## is a vector and @qcode{false} otherwise.  A vector is a 2-D array for
    ## which one of the dimensions is equal to 1 (either @math{1*N} or
    ## @math{N*1}).  By definition, a scalar is also a vector.
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

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} minus (@var{A}, @var{B})
    ##
    ## Subtraction for calendarDuration arrays.
    ##
    ## @code{@var{C} = minus (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} - @var{B}} and returns the result of subtracting
    ## the corresponding elements of @var{B} from those of @var{A}.  @var{C} is
    ## a calendarDuration array of the same size as the input arguments after
    ## the necessary (if required) expansion.  @var{A} and @var{B} must be size
    ## compatible, which translates to they can be the same size, one can be
    ## scalar, or for every dimension, their dimension sizes must be equal or
    ## one of them must be 1.
    ##
    ## Either @var{A} or @var{B} may also be a duration or a numeric array with
    ## the latter representing duration days and being internally converted to a
    ## duration array with the @code{days ()} function.
    ##
    ## @end deftypefn
    function out = minus (A, B)
      if (isa (A, 'calendarDuration') && isa (B, 'calendarDuration'))
        out = A;
        out.Months = A.Months - B.Months;
        out.Days = A.Days - B.Days;
        out.Time = A.Time - B.Time;
      elseif (isa (A, 'calendarDuration') && isa (B, 'duration'))
        out = A;
        tmp = zeros (size (B));
        out.Months = A.Months - tmp;
        out.Days = A.Days - tmp;
        out.Time = A.Time - B;
      elseif (isa (A, 'calendarDuration') && isScaleType (B))
        out = A;
        tmp = zeros (size (B));
        out.Months = out.Months - tmp;
        out.Days = out.Days - tmp;
        out.Time = out.Time - days (double (B));
      elseif (isScaleType (A) && isa (B, 'calendarDuration'))
        out = B;
        tmp = zeros (size (A));
        out.Months = -out.Months + tmp;
        out.Days = -out.Days + tmp;
        out.Time = -out.Time + days (double (A));
      else
        error (strcat ("calendarDuration: subtraction is not defined", ...
                       " between '%s' and '%s' arrays."), class (A), class (B));
      endif
      out = broadcastProperties (out);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} minus (@var{A})
    ##
    ## Unary minus for calendarDuration arrays.
    ##
    ## @code{@var{C} = uminus (@var{A})} is the equivalent of the syntax
    ## @code{@var{C} = -@var{A}} and returns @var{A} after negating its
    ## elements.  @var{C} is a calendarDuration array of the same size as
    ## @var{A}.
    ##
    ## @end deftypefn
    function out = uminus (A)
      out = A;
      out.Months = -A.Months;
      out.Days = -A.Days;
      out.Time = -A.Time;
      out = dropNegZero (out);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} plus (@var{A}, @var{B})
    ##
    ## Addition for calendarDuration arrays.
    ##
    ## @code{@var{C} = plus (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} + @var{B}} and returns the result of adding the
    ## corresponding elements of @var{A} and @var{B}.  @var{C} is a
    ## calendarDuration array of the same size as the input arguments after the
    ## necessary (if required) expansion.  @var{A} and @var{B} must be size
    ## compatible, which translates to they can be the same size, one can be
    ## scalar, or for every dimension, their dimension sizes must be equal or
    ## one of them must be 1.
    ##
    ## Either @var{A} or @var{B} may also be a duration or a numeric array with
    ## the latter representing duration days and being internally converted to a
    ## duration array with the @code{days ()} function.
    ##
    ## @end deftypefn
    function out = plus (A, B)
      if (isa (B, 'datetime'))
        out = B + A;
        return;
      endif
      if (isa (A, 'calendarDuration') && isa (B, 'calendarDuration'))
        out = A;
        out.Months = A.Months + B.Months;
        out.Days = A.Days + B.Days;
        out.Time = A.Time + B.Time;
      elseif (isa (A, 'calendarDuration') && isa (B, 'duration'))
        out = A;
        tmp = zeros (size (B));
        out.Months = A.Months + tmp;
        out.Days = A.Days + tmp;
        out.Time = A.Time + B;
      elseif (isa (A, 'calendarDuration') && isScaleType (B))
        out = A;
        tmp = zeros (size (B));
        out.Months = A.Months + tmp;
        out.Days = A.Days + tmp;
        out.Time = A.Time + days (double (B));
      elseif (isScaleType (A) && isa (B, 'calendarDuration'))
        out = B;
        tmp = zeros (size (A));
        out.Months = B.Months + tmp;
        out.Days = B.Days + tmp;
        out.Time = B.Time + days (double (A));
      else
        error (strcat ("calendarDuration: addition is not defined", ...
                       " between '%s' and '%s' arrays."), class (A), class (B));
      endif
      out = broadcastProperties (out);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} uplus (@var{A})
    ##
    ## Unary plus for calendarDuration arrays.
    ##
    ## @code{@var{C} = uplus (@var{A})} is the equivalent of the syntax
    ## @code{@var{C} = +@var{A}} and returns a copy of @var{A}.  @var{C} is a
    ## calendarDuration array of the same size as @var{A}.
    ##
    ## @end deftypefn
    function out = uplus (A)
      out = A;
      out.Months = A.Months;
      out.Days = A.Days;
      out.Time = A.Time;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} times (@var{A}, @var{B})
    ##
    ## Element-by-element multiplication for calendarDuration arrays.
    ##
    ## @code{@var{C} = times (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{C} = @var{A} .* @var{B}} and returns the element-by-element
    ## multiplication product of inputs @var{A} and @var{B}.  Either @var{A} or
    ## @var{B} must be a calendarDuration array and its complement must be a
    ## double array.
    ##
    ## @var{C} is a calendarDuration array of the same size as the input
    ## arguments after the necessary (if required) expansion.  @var{A} and
    ## @var{B} must be size compatible, which translates to they can be the same
    ## size, one can be scalar, or for every dimension, their dimension sizes
    ## must be equal or one of them must be 1.
    ##
    ## @end deftypefn
    function out = times (A, B)
      if (isa (A, 'calendarDuration') && isScaleType (B))
        out = A;
        tmp = double (B);
      elseif (isScaleType (A) && isa (B, 'calendarDuration'))
        out = B;
        tmp = double (A);
      else
        error (strcat ("calendarDuration: multiplication is not defined", ...
                       " between '%s' and '%s' arrays."), class (A), class (B));
      endif
      if (isNonIntegral (tmp))
        error (strcat ("calendarDuration: multiplication by non-integer", ...
                       " values is not defined."));
      endif
      M = out.Months;  D = out.Days;  T = seconds (out.Time);
      out.Months = out.Months .* tmp;
      out.Days = out.Days .* tmp;
      out.Time = out.Time .* tmp;
      out = scaleNonFinite (out, M, D, T, tmp);
      out = broadcastProperties (out);
      out = dropNegZero (out);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} mtimes (@var{A}, @var{B})
    ##
    ## Matrix multiplication for calendarDuration arrays.
    ##
    ## @code{@var{C} = mtimes (@var{A}, @var{B})} is the equivalent of the
    ## syntax @code{@var{C} = @var{A} * @var{B}} and returns the matrix
    ## multiplication product of inputs @var{A} and @var{B}.  Either @var{A} or
    ## @var{B} must be a calendarDuration array and its complement must be a
    ## double array.
    ##
    ## @var{C} is a calendarDuration array of the same size as the input
    ## arguments after the necessary (if required) expansion.  @var{A} and
    ## @var{B} must be size compatible, which translates to they can be the same
    ## size, one can be scalar, or for every dimension, their dimension sizes
    ## must be equal or one of them must be 1.
    ##
    ## @end deftypefn
    function out = mtimes (A, B)
      if (isa (A, 'calendarDuration') && isScaleType (B))
        out = A;
        tmp = double (B);
        M = A.Months;  D = A.Days;  T = seconds (A.Time);
        out.Months = A.Months * tmp;
        out.Days = A.Days * tmp;
        out.Time = A.Time * tmp;
      elseif (isScaleType (A) && isa (B, 'calendarDuration'))
        out = B;
        tmp = double (A);
        M = B.Months;  D = B.Days;  T = seconds (B.Time);
        out.Months = tmp * B.Months;
        out.Days = tmp * B.Days;
        out.Time = tmp * B.Time;
      else
        error (strcat ("calendarDuration: matrix multiplication is", ...
                       " not defined between '%s' and '%s' arrays."), ...
               class (A), class (B));
      endif
      if (isNonIntegral (tmp))
        error (strcat ("calendarDuration: matrix multiplication by", ...
                       " non-integer values is not defined."));
      endif
      out = scaleNonFinite (out, M, D, T, tmp);
      out = broadcastProperties (out);
      out = dropNegZero (out);
    endfunction

  endmethods

################################################################################
##                    ** Equality and Filter Operations **                    ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'eq'               'ne'               'unique'                             ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Test for equality between calendarDuration arrays.
    ##
    ## @code{@var{TF} = eq (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} == @var{B}} and returns a logical array with
    ## elements set to @qcode{true} where calendarDuration arrays @var{A} and
    ## @var{B} are equal, otherwise set to @qcode{false}.  Missing values are
    ## not equal to each other.  Hence, any @code{NaN} values in @var{A} or
    ## @var{B} result to @qcode{false} elements in @var{TF}.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of the
    ## output @var{TF} is the same as the size of input arrays after their
    ## expansion according to the broadcasting rules.
    ##
    ## MATLAB defines no equality for calendarDuration arrays, so this is an
    ## Octave extension.  Two spans are compared component by component, which
    ## is what MATLAB's own @code{isequal} does, so the two agree element for
    ## element: @code{calweeks (1) == caldays (7)} is @qcode{true}, both being
    ## seven days, while @code{caldays (1) == calendarDuration (0, 0, 0, 24,
    ## 0, 0)} is @qcode{false}, a day and twenty-four hours sitting in
    ## different components.
    ##
    ## @end deftypefn
    function TF = eq (A, B)
      if (! (iscalendarduration (A) && iscalendarduration (B)))
        error (strcat ("calendarDuration.eq: equality is not defined", ...
                       " between '%s' and '%s' arrays."), class (A), class (B));
      endif
      TF = A.Months == B.Months & A.Days == B.Days & A.Time == B.Time;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{TF} =} ne (@var{A}, @var{B})
    ##
    ## Test for inequality between calendarDuration arrays.
    ##
    ## @code{@var{TF} = ne (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} != @var{B}} and returns a logical array with
    ## elements set to @qcode{true} where calendarDuration arrays @var{A} and
    ## @var{B} are not equal, otherwise set to @qcode{false}.  Missing values
    ## are not equal to each other.  Hence, any @code{NaN} values in @var{A} or
    ## @var{B} result to @qcode{true} elements in @var{TF}.
    ##
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.  The size of the
    ## output @var{TF} is the same as the size of input arrays after their
    ## expansion according to the broadcasting rules.
    ##
    ## MATLAB defines no equality for calendarDuration arrays, so this is an
    ## Octave extension.  Two spans are compared component by component, which
    ## is what MATLAB's own @code{isequal} does, so the two agree element for
    ## element: @code{calweeks (1) == caldays (7)} is @qcode{true}, both being
    ## seven days, while @code{caldays (1) == calendarDuration (0, 0, 0, 24,
    ## 0, 0)} is @qcode{false}, a day and twenty-four hours sitting in
    ## different components.
    ##
    ## @end deftypefn
    function TF = ne (A, B)
      if (! (iscalendarduration (A) && iscalendarduration (B)))
        error (strcat ("calendarDuration.ne: inequality is not defined", ...
                       " between '%s' and '%s' arrays."), class (A), class (B));
      endif
      TF = A.Months != B.Months | A.Days != B.Days | A.Time != B.Time;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{B} =} unique (@var{A})
    ## @deftypefnx {calendarDuration} {@var{B} =} unique (@var{A}, @var{setOrder})
    ## @deftypefnx {calendarDuration} {@var{B} =} unique (@var{A}, @var{occurrence})
    ## @deftypefnx {calendarDuration} {@var{B} =} unique (@var{A}, @var{setOrder}, @var{occurrence})
    ## @deftypefnx {calendarDuration} {@var{B} =} unique (@var{A}, @var{occurrence}, @var{setOrder})
    ## @deftypefnx {calendarDuration} {@var{B} =} unique (@var{A}, @dots{}, @qcode{'rows'})
    ## @deftypefnx {calendarDuration} {[@var{B}, @var{ixA}, @var{ixB}] =} unique (@dots{})
    ##
    ## Unique values in a calendarDuration array.
    ##
    ## @code{@var{B} = unique (@var{A})} returns the unique values of the
    ## calendarDuration array @var{A} in sorted order.
    ##
    ## @code{@var{B} = unique (@var{A}, @var{setOrder})} returns the unique
    ## values of the calendarDuration array @var{A} in an order as specified by
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
    ## values of the calendarDuration array @var{tblA} according to their order
    ## of occurrence.  @var{occurrence} can be either of the following values:
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
    ## calendarDuration arrays.
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
        error ("calendarDuration.unique: 'legacy' option is not supported.");
      endif
      ## Handle each property array separately
      [~, ~, Midx] = __unique__ (A.Months, varargin{:});
      [~, ~, Didx] = __unique__ (A.Days, varargin{:});
      [~, ~, Tidx] = unique (A.Time, varargin{:});
      ## Use indices to find unique calendarDuration values
      if (any (strcmp ('rows', varargin)))
        [~, ixA, ixB] = __unique__ ([Midx, Didx, Tidx], varargin{:});
        if (any (strcmp ('last', varargin)))
          [~, ixA, ~] = __unique__ (ixB, 'last');
        endif
        B = subset (A, ixA, ':');
      else
        [~, ixA, ixB] = __unique__ ([Midx, Didx, Tidx], 'rows', varargin{:});
        if (any (strcmp ('last', varargin)))
          [~, ixA, ~] = __unique__ (ixB, 'last');
        endif
        B = subset (A, ixA);
      endif
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
    ## @deftypefn {calendarDuration} {@var{C} =} cat (@var{dim}, @var{A}, @var{B}, @dots{})
    ##
    ## Concatenate calendarDuration arrays.
    ##
    ## @code{@var{C} = cat (@var{dim}, @var{A}, @var{B}, @dots{})} concatenates
    ## calendarDuration arrays @var{A}, @var{B}, @dots{} along dimension
    ## @var{dim}.  All input arrays must have the same size except along the
    ## operating dimension @var{dim}.  Any of the input arrays may also be
    ## string arrays or cell arrays of character vectors of compatible size.
    ## Additionally, an input can be a numeric matrix, which when parsed to the
    ## constructor will return a calendarDuration array of compatible size.
    ##
    ## @end deftypefn
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
      ## The result displays in the union of the inputs' formats, so
      ## concatenating weeks with quarters keeps both rather than widening
      ## either back to days and months.
      fmts = cellfun (@(obj) obj.Format, args, 'UniformOutput', false);
      out.Format = unionFormat (fmts);
      out = broadcastProperties (out);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} horzcat (@var{A}, @var{B}, @dots{})
    ##
    ## Horizontal concatenation of calendarDuration arrays.
    ##
    ## @code{@var{C} = horzcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}, @var{B}, @dots{}]} and horizontally
    ## concatenates the calendarDuration arrays @var{A}, @var{B}, @dots{}.  All
    ## input arrays must have the same size except along the second dimension.
    ## Any of the input arrays may also be string arrays or cell arrays of
    ## character vectors of compatible size.  Additionally, an input can be a
    ## numeric matrix, which when parsed to the constructor will return a
    ## calendarDuration array of compatible size.
    ##
    ## @end deftypefn
    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{C} =} vertcat (@var{A}, @var{B}, @dots{})
    ##
    ## Vertical concatenation of calendarDuration arrays.
    ##
    ## @code{@var{C} = vertcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}; @var{B}; @dots{}]} and vertically
    ## concatenates the calendarDuration arrays @var{A}, @var{B}, @dots{}.  All
    ## input arrays must have the same size except along the first dimension.
    ## Any of the input arrays may also be string arrays or cell arrays of
    ## character vectors of compatible size.  Additionally, an input can be a
    ## numeric matrix, which when parsed to the constructor will return a
    ## calendarDuration array of compatible size.
    ##
    ## @end deftypefn
    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{B} =} repmat (@var{A}, @var{n})
    ## @deftypefnx {calendarDuration} {@var{B} =} repmat (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {calendarDuration} {@var{B} =} repmat (@var{A}, @var{dimvec})
    ##
    ## Repeat copies of a calendarDuration array.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{n})} returns a calendarDuration
    ## array @var{B} containing @var{n} copies of the input calendarDuration
    ## array @var{A} along every dimension of @var{A}.
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
      this.Months = repmat (this.Months, varargin{:});
      this.Days   = repmat (this.Days, varargin{:});
      this.Time   = repmat (this.Time, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{B} =} repelem (@var{A}, @var{n})
    ## @deftypefnx {calendarDuration} {@var{B} =} repelem (@var{A}, @var{d1}, @dots{}, @var{dN})
    ##
    ## Repeat copies of calendarDuration array elements.
    ##
    ## @code{@var{B} = repelem (@var{A}, @var{n})} returns a calendarDuration
    ## vector @var{B} containing repeated elements of the input @var{A}, which
    ## must be a calendarDuration vector.  If @var{n} is a scalar, each element
    ## of @var{A} is repeated @var{n} times along the non-singleton dimension of
    ## @var{A}.  If @var{n} is a vector, it must have the same elements as
    ## @var{A}, in which case it specifies the number of times to repeat each
    ## corresponding element of @var{A}.
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
      this.Months = repelem (this.Months, varargin{:});
      this.Days   = repelem (this.Days, varargin{:});
      this.Time   = repelem (this.Time, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{B} =} repelems (@var{A}, @var{R})
    ##
    ## Construct a vector of repeated elements from a calendarDuration array.
    ##
    ## @code{@var{B} = repelems (@var{A}, @var{R})} returns a calendarDuration
    ## vector @var{B} containing repeated elements of the input @var{A}, which
    ## must be a calendarDuration vector.  @var{R} must be a @math{2*N} matrix
    ## of integers.  Entries in the first row of @var{R} correspond to the
    ## linear indexing of the elements in @var{A} to be repeated.  The
    ## corresponding entries in the second row of @var{R} specify the repeat
    ## count of each element.
    ##
    ## @end deftypefn
    function this = repelems (this, R)
      this.Months = repelems (this.Months, R);
      this.Days   = repelems (this.Days, R);
      this.Time   = repelems (this.Time, R);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{B} =} reshape (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {calendarDuration} {@var{B} =} reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})
    ## @deftypefnx {calendarDuration} {@var{B} =} reshape (@var{A}, @var{dimvec})
    ##
    ## Reshape calendarDuration array.
    ##
    ## @code{@var{B} = reshape (@var{A}, @var{d1}, @dots{}, @var{dN})} returns a
    ## calendarDuration array @var{B} with specified dimensions @var{d1},
    ## @dots{}, @var{dN}, whose elements are taken columnwise from the
    ## calendarDuration array @var{A}.  The product of @var{d1}, @dots{},
    ## @var{dN} must equal the total number of elements in @var{A}.
    ##
    ## @code{@var{B} = reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})} returns
    ## a calendarDuration array @var{B} with one dimension unspecified which is
    ## calculated automatically so that the product of dimensions in @var{B}
    ## matches the total elements in @var{A}, which must be divisible the
    ## product of specified dimensions.  An empty matrix @qcode{([])} is used to
    ## flag the unspecified dimension.
    ##
    ## @end deftypefn
    function this = reshape (this, varargin)
      this.Months = reshape (this.Months, varargin{:});
      this.Days   = reshape (this.Days, varargin{:});
      this.Time   = reshape (this.Time, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {calendarDuration} {@var{B} =} circshift (@var{A}, @var{n})
    ## @deftypefnx {calendarDuration} {@var{B} =} circshift (@var{A}, @var{n}, @var{dim})
    ##
    ## Circularly shift the elements in a calendarDuration array.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n})} circularly shifts the
    ## elements of the calendarDuration array @var{A} according to @var{n}.  If
    ## @var{n} is a nonzero integer scalar, then the elements of @var{A} are
    ## shifted by @var{n} elements along the first non-singleton dimension of
    ## @var{A}.  If @var{n} is a vector, it must not be longer that the number
    ## of dimensions of @var{A} with each value of @var{n} corresponding to a
    ## dimension in @var{A}.   The sign of the value(s) in @var{n} specify the
    ## direction in the elements of @var{A} are shifted.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n}, @var{dim})} circularly
    ## shifts the elements of the calendarDuration array @var{A} along the
    ## dimension specified by @var{dim}.  In this case, @var{n} must be a scalar
    ## integer value.
    ##
    ## @end deftypefn
    function this = circshift (this, varargin)
      this.Months = circshift (this.Months, varargin{:});
      this.Days   = circshift (this.Days, varargin{:});
      this.Time   = circshift (this.Time, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{B} =} permute (@var{A}, @var{dims})
    ##
    ## Generalized transpose for a calendarDuration N-D array.
    ##
    ## @code{@var{B} = permute (@var{A}, @var{dims})} returns the generalized
    ## transpose of the calendarDuration array @var{A} by rearranging its
    ## dimensions according to the permutation vector specified in @var{dims}.
    ##
    ## @var{dims} must index all the dimensions @code{1:ndims (@var{A})} of the
    ## input array @var{A}, in any order, but only once.  The @var{N}th
    ## dimension of @var{A} gets remapped to the dimension in @var{B} specified
    ## by @code{@var{dims}(@var{N})}.
    ##
    ## @end deftypefn
    function this = permute (this, order)
      this.Months = permute (this.Months, order);
      this.Days   = permute (this.Days, order);
      this.Time   = permute (this.Time, order);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{A} =} ipermute (@var{B}, @var{dims})
    ##
    ## Inverse of the generalized transpose for a calendarDuration N-D array.
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
      this.Months = ipermute (this.Months, order);
      this.Days   = ipermute (this.Days, order);
      this.Time   = ipermute (this.Time, order);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{B} =} transpose (@var{A})
    ##
    ## Transpose a calendarDuration matrix.
    ##
    ## @code{@var{B} = transpose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}.'} and returns the transpose of the
    ## calendarDuration matrix @var{A}.
    ##
    ## @end deftypefn
    function this = transpose (this)
      this.Months = transpose (this.Months);
      this.Days   = transpose (this.Days);
      this.Time   = transpose (this.Time);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {calendarDuration} {@var{B} =} ctranspose (@var{A})
    ##
    ## Transpose a calendarDuration matrix.
    ##
    ## @code{@var{B} = ctranspose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}'} and returns the transpose of the
    ## calendarDuration matrix @var{A}.  For calendarDuration arrays,
    ## @code{ctranspose} is identical to @code{transpose}.
    ##
    ## @end deftypefn
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
          out.Months = this.Months(s.subs{:});
          out.Days   = this.Days(s.subs{:});
          out.Time   = this.Time(s.subs{:});

        case '{}'
          error (strcat ("calendarDuration.subsref: '{}' invalid indexing", ...
                         " for referencing values. Use '()' instead."));

        case '.'
          if (! ischar (s.subs))
            error (strcat ("calendarDuration.subsref: '.' index", ...
                           " argument must be a character vector."));
          endif
          switch (s.subs)
            case 'proxyArray'  # used by 'table' class
              out = proxyArray (this);
            case 'Format'
              out = this.Format;
            otherwise
              error (strcat ("calendarDuration.subsref: unrecognized", ...
                             " property: '%s'"), s.subs);
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
          if (isempty (val))
            this.Months(s.subs{:}) = [];
            this.Days(s.subs{:})   = [];
            this.Time(s.subs{:})   = [];
            return;
          elseif (isnumeric (val))
            tmp = zeros (size (val));
            this.Months(s.subs{:}) = tmp;
            this.Days(s.subs{:})   = tmp;
            this.Time(s.subs{:})   = duration (24 * double (val), 0, 0);
            this = broadcastProperties (this);
          elseif (isa (val, "calendarDuration"))
            this.Months(s.subs{:}) = val.Months;
            this.Days(s.subs{:})   = val.Days;
            this.Time(s.subs{:})   = val.Time;
            this = broadcastProperties (this);
          elseif (isa (val, "duration"))
            this.Months(s.subs{:}) = 0;
            this.Days(s.subs{:})   = 0;
            this.Time(s.subs{:})   = val;
            this = broadcastProperties (this);
          else
            error (strcat ("calendarDuration.subsasgn: assignment value", ...
                           " must be calendarDuration or duration array", ...
                           " or a numeric array representing 24-hour days."));
          endif

        case '{}'
          error (strcat ("calendarDuration.subsasgn: '{}' invalid indexing", ...
                         " for assigning values. Use '()' instead."));

        case '.'
          if (! ischar (s.subs))
            error (strcat ("calendarDuration.subsasgn: '.' index", ...
                           " argument must be a character vector."));
          endif
          switch (s.subs)
            case 'Format'
              [errmsg, val] = checkFormatString (val);
              if (! isempty (errmsg))
                error ("calendarDuration.subsasgn: 'Format' %s", errmsg);
              endif
              this.Format = val;
            otherwise
              error (strcat ("calendarDuration.subsasgn: unrecognized", ...
                             " property: '%s'"), s.subs);
          endswitch
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Negating a span, or scaling it by a negative number, turns a zero
    ## component into a negative zero, which MATLAB does not keep and which
    ## shows in any printed component.  Adding zero maps -0 to +0 and leaves
    ## every other value exactly as it was, NaN and the infinities included.
    ## A non-finite factor scales the span as a whole rather than component by
    ## component: 0 * Inf would leave NaN in every empty component, but MATLAB
    ## takes the sign of the span, so 3d * Inf is Inf and -5h * Inf is -Inf.
    ## A span whose components disagree in sign, or that is entirely zero, has
    ## no sign and gives NaN.  M, D and T are the components before scaling.
    function this = scaleNonFinite (this, M, D, T, fac)
      if (! (isscalar (fac) || isscalar (M) || size_equal (M, fac)))
        return;   # a true matrix product is outside the measured domain
      endif
      if (all (isfinite (fac(:))))
        return;
      endif
      pos = M > 0 | D > 0 | T > 0;
      neg = M < 0 | D < 0 | T < 0;
      sgn = NaN (size (pos));
      sgn(pos & ! neg) = 1;
      sgn(neg & ! pos) = -1;
      val = sgn .* fac;
      nf = ! isfinite (zeros (size (sgn)) + fac);
      this.Months(nf) = val(nf);
      this.Days(nf) = val(nf);
      this.Time(nf) = seconds (val(nf));
    endfunction

    function this = dropNegZero (this)
      this.Months = this.Months + 0;
      this.Days   = this.Days + 0;
      this.Time   = this.Time + seconds (0);
    endfunction

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.Months = this.Months(varargin{:});
      this.Days   = this.Days(varargin{:});
      this.Time   = this.Time(varargin{:});
    endfunction

    ## Broadcast properties
    function this = broadcastProperties (this)
      ## Handle NaNs and Infs first
      is_nan = isnan (this.Months) | isnan (this.Days) | isnan (this.Time);
      isPinf = Inf == this.Months | Inf == this.Days | Inf == this.Time;
      isNinf = -Inf == this.Months | -Inf == this.Days | -Inf == this.Time;
      if (any (is_nan, 'all') || any (isPinf & isNinf, 'all'))
        is_nan = is_nan | (isPinf & isNinf);
        isPinf = isPinf & ! is_nan;
        isNinf = isNinf & ! is_nan;
      endif
      ## Broadcast NaNs
      this.Months(is_nan) = NaN;
      this.Days(is_nan) = NaN;
      this.Time(is_nan) = duration ([NaN, NaN, NaN]);
      ## Broadcast Infs
      if (any (isPinf, 'all'))
        this.Months(isPinf) = Inf;
        this.Days(isPinf) = Inf;
        this.Time(isPinf) = duration ([Inf, Inf, Inf]);
      endif
      if (any (isNinf, 'all'))
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
        elseif (isa (varargin{i}, "duration"))
          varargout{i} = calendarDuration (0, 0, 0, varargin{i});
        elseif (isnumeric (varargin{i}))
          if (isempty (varargin{i}))
            varargout{i} = calendarDuration ([], [], []);
          else
            varargout{i} = calendarDuration (0, 0, 0, 24 * varargin{i}, 0, 0);
          endif
        else
          error ("calendarDuration: invalid input to constructor.");
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

  endmethods

endclassdef

## Days and weeks are only meaningful on their own.  A month has no fixed
## number of days, so an array carrying any month at all cannot be stated in
## either unit, and MATLAB refuses to guess: it directs the caller to 'split',
## which reports each unit separately.  The test is 'not zero', not 'positive':
## a negative month count is refused just as a positive one is.  It is also
## restricted to FINITE months, which is a deviation and a deliberate one.
## MATLAB refuses caldays (calendarDuration (0, 0, NaN)) but accepts
## caldays (caldays (NaN)), and likewise for Inf -- the constructor path throws
## where the component-builder path returns the value.  Since broadcasting puts
## the NaN in every component, the two are one value here and no predicate over
## Months can tell them apart; propagating is chosen over throwing because a
## NaN month is unknown rather than known to be non-zero, because it is what
## calmonths/calquarters/calyears already do, and because throwing would break
## the caldays (caldays (x)) == x round trip that MATLAB itself preserves.
## The message body is shared so that the two callers cannot drift apart; each
## emits it under its own name.
## Only a double or a logical may be added to, subtracted from, or multiplied
## with a span; MATLAB refuses every other numeric type, single included.
function TF = isScaleType (x)
  TF = isa (x, 'double') || islogical (x);
endfunction

## A span cannot be scaled by a fraction -- a fraction of a month has no
## meaning.  Non-finite factors are exempt, being scale rather than count.
function TF = isNonIntegral (x)
  TF = any (isfinite (x(:)) & fix (x(:)) != x(:));
endfunction

function errmsg = monthlessErrMsg ()
  errmsg = strcat ("cannot convert a calendarDuration to days or weeks when", ...
                   " it contains a non-zero number of months. Use 'split'", ...
                   " instead.");
endfunction

## Check 'Format' string.  The validated format is returned as well, so that a
## string scalar is converted to a character vector once, where it is checked,
## rather than at each of the callers that store it.
function [errmsg, Format] = checkFormatString (Format)
  ## A string scalar is accepted and stored as a character vector
  if (isa (Format, 'string') && isscalar (Format))
    Format = char (Format);
  endif
  ## Check for character vector.  An empty character vector is let through to
  ## report the characters it is missing rather than its type.
  if (! (ischar (Format) && (isvector (Format) || isempty (Format))))
    errmsg = "must be a character vector or a string scalar.";
    return;
  endif
  errmsg = '';
  sf = @(x) strfind (Format, x);
  sp = cellfun (sf, {'y','q','m','w','d','t'}, 'UniformOutput', false);
  ## Check for characters outside the recognized set
  if (! all (ismember (Format, 'yqmwdt')))
    errmsg = "must only contain characters from the 'yqmwdt' sequence.";
  ## Check for duplicate characters
  elseif (any (cellfun (@(x) numel (x) > 1, sp)))
    errmsg = "contains duplicate characters.";
  ## Check for 'm', 'd', and 't' being present
  elseif (any (cellfun (@isempty, sp([3,5,6]))))
    errmsg = "must contain 'm', 'd', and 't'.";
  ## Check order
  elseif (any (diff (cell2mat (sp)) < 1))
    errmsg = "has invalid order of characters.";
  endif
endfunction

## Render a whole component the way MATLAB does: six significant digits, and an
## exponent past them, so a million days reads '1e+06d'.  Adding zero turns the
## negative zero that truncating a negative time leaves behind into a plain one,
## which is what keeps '0h' from reading '-0h'.
function s = fmtUnit (v)
  s = sprintf ('%g', v + 0);
endfunction

## Render the seconds component, which unlike the units above may carry a
## fraction.  MATLAB shows it to five significant digits, so nothing is lost to
## a millisecond grid and 0.9995 seconds is not rounded up to a whole one.
function s = fmtSecs (v)
  s = sprintf ('%.5g', v + 0);
endfunction

## Unit a wholly zero element renders in: the smallest one any element of the
## array actually uses, or days when none does.  The ladder runs from the
## largest unit to the smallest, and time is rendered as seconds.
## Unit a wholly zero element renders in: the smallest one the array actually
## used anywhere, or days when nothing did.  The ladder runs largest to
## smallest, matching the order calendarDuration prints its components in.
function u = zeroRenderUnit (els)
  ladder = {'y', 'q', 'mo', 'w', 'd', 's'};
  used = regexp (strjoin ([els{:}, {''}], ' '), '[a-z]+', 'match');
  ## Any time component at all renders the zero in seconds.
  if (any (ismember (used, {'h', 'm', 's'})))
    u = '0s';
    return;
  endif
  found = find (ismember (ladder, used), 1, 'last');
  if (isempty (found))
    u = '0d';
  else
    u = ['0', ladder{found}];
  endif
endfunction

## Union of a set of display formats, in the canonical component order.  Every
## calendarDuration format contains 'm', 'd' and 't', so the union always does
## too and stays a valid format.
function fmt = unionFormat (fmts)
  ladder = 'yqmwdt';
  all = [fmts{:}];
  fmt = ladder(ismember (ladder, all));
endfunction
