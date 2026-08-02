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
  ## When a display @code{Format} contains a time-zone name field
  ## (@qcode{z}/@qcode{zz}/@qcode{zzz}), the name is rendered according to a
  ## session-wide style set by @code{datetime.zoneNameStyle}.  The default,
  ## @qcode{'iana'}, is an Octave-specific extension that shows the IANA
  ## abbreviation for every zone (e.g. @qcode{EEST}); @qcode{'matlab'} restores
  ## MATLAB's behaviour of naming only North American zones and showing a
  ## numeric @qcode{UTC+3}-style offset elsewhere.  A single @code{Format} may
  ## override the session style with @qcode{zzzz} (force @qcode{'iana'}) or
  ## @qcode{zzzzz} (force @qcode{'matlab'}).  See @code{datetime.zoneNameStyle}.
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
    ## UTC offset in seconds for each element, and the only thing that tells
    ## apart the two instants sharing a wall clock across a fall-back
    ## transition: 01:30 on 3 November 2024 in America/New_York is -4*3600 on
    ## its first pass and -5*3600 on its second.  Zero for an unzoned array and
    ## for the leap-second zone, both of which are UTC-based, so
    ##
    ##     serial = naive_seconds (components) - Offset
    ##
    ## holds without a branch.  Seconds rather than a boolean fold because
    ## Australia/Lord_Howe shifts by half an hour and Pacific/Chatham sits at
    ## a quarter hour.  NaN for Not-A-Time.
    ##
    ## INVARIANT: Offset always agrees with (components, TimeZone).  A
    ## wall-clock operation goes through 'normalize', which recomputes it as
    ## the later of an ambiguous pair; an instant-based operation goes through
    ## 'serial2components', which carries the true one through.  An
    ## instant-derived result must therefore NOT be passed to 'normalize'
    ## afterwards, or the fold is silently lost.
    Offset = 0
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
    ## The value @qcode{'default'} is a data-dependent sentinel: a date-only
    ## format (@qcode{'dd-MMM-uuuu'}) is used when every element sits at
    ## midnight, and a date and time format (@qcode{'dd-MMM-uuuu HH:mm:ss'})
    ## otherwise.  The value @qcode{'defaultdate'} always selects the
    ## date-only format.  Reading the property returns the resolved pattern,
    ## never the sentinel itself.
    ##
    ## A custom pattern is built from the following Unicode (LDML) fields;
    ## repeating a letter widens or names the field.  Any other text is
    ## copied verbatim, and text in single quotes is always literal.
    ##
    ## @multitable @columnfractions 0.18 0.82
    ## @item @code{y}, @code{u} @tab Year; @code{yy} uses the last two digits.
    ## @item @code{M} @tab Month: number (@code{M}, @code{MM}), abbreviated
    ## (@code{MMM}), full (@code{MMMM}), or initial (@code{MMMMM}).
    ## @item @code{d} @tab Day of the month.
    ## @item @code{D} @tab Day of the year.
    ## @item @code{e} @tab Day of the week: number (Sunday is 1), abbreviated
    ## (@code{eee}), full (@code{eeee}), or initial (@code{eeeee}).
    ## @item @code{H}, @code{h} @tab Hour, 24-hour and 12-hour clock.
    ## @item @code{m} @tab Minute.
    ## @item @code{s} @tab Whole second (fractional seconds are truncated).
    ## @item @code{S} @tab Fractional second, one digit per @code{S}.
    ## @item @code{a} @tab AM or PM.
    ## @item @code{Q} @tab Quarter of the year.
    ## @item @code{G} @tab Era.
    ## @item @code{W} @tab Week of the month.
    ## @item @code{z} @tab Time-zone name (style set by
    ## @code{datetime.zoneNameStyle}; @code{zzzz}/@code{zzzzz} force it).
    ## @item @code{Z}, @code{X}, @code{x} @tab Numeric time-zone offset.
    ## @end multitable
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
    ## Besides the zones of the IANA Time Zone Database, the value
    ## @qcode{'UTCLeapSeconds'} selects UTC with its inserted leap seconds made
    ## representable, so that the 60th second of a minute exists on the 27 dates
    ## that have one.  It is not an IANA zone and @code{timezones} does not list
    ## it.  An array in that zone counts elapsed SI seconds, which is what makes
    ## its arithmetic differ from a UTC array's across an inserted second, and
    ## for that reason it cannot be combined or compared with an array that does
    ## not have leap seconds.  Its @code{Format} is fixed to
    ## @qcode{"uuuu-MM-dd'T'HH:mm:ss'Z'"}, optionally with one to nine
    ## fractional-second digits, since no other pattern can write a 60th second;
    ## moving such an array to any other zone folds an inserted second back onto
    ## the 59th and restores the ordinary default format.
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

  methods (Static, Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{E} =} datetime.empty ()
    ## @deftypefnx {datetime} {@var{E} =} datetime.empty (@var{sz})
    ## @deftypefnx {datetime} {@var{E} =} datetime.empty (@var{m}, @var{n}, @dots{})
    ##
    ## Create an empty datetime array.
    ##
    ## @code{@var{E} = datetime.empty ()} returns a @math{0*0} empty datetime
    ## array.  @code{datetime.empty (@var{m}, @var{n}, @dots{})} or
    ## @code{datetime.empty (@var{sz})} returns an empty datetime array of the
    ## requested size, which must have at least one dimension equal to zero.  A
    ## lone dimension gives a square size, so @code{datetime.empty (3)} is an
    ## error while @code{datetime.empty (0)} is @math{0*0}.  As for
    ## @code{zeros}, a negative dimension counts as zero, and a size vector with
    ## nothing in it names no size and gives @math{0*0}.
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
        error ("datetime.empty: dimensions must be integer values.");
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
        error (strcat ("datetime.empty: at least one dimension must be", ...
                       " zero for an empty array."));
      endif
      E = datetime (nan (sz), 'ConvertFrom', 'datenum');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{style} =} datetime.zoneNameStyle ()
    ## @deftypefnx {datetime} {} datetime.zoneNameStyle (@var{style})
    ##
    ## Query or set the session-wide style used to render time-zone names.
    ##
    ## The @qcode{z}, @qcode{zz}, and @qcode{zzz} fields of a display
    ## @code{Format} render a time-zone name.  Two styles are available,
    ## selected by this session-wide preference:
    ##
    ## @table @asis
    ## @item @qcode{'iana'} (default)
    ## The IANA time-zone database abbreviation active at each instant, such as
    ## @qcode{EDT}, @qcode{EEST}, @qcode{JST}, or @qcode{UTC}.  This is an
    ## Octave-specific extension: a named abbreviation is shown for every zone.
    ##
    ## @item @qcode{'matlab'}
    ## MATLAB-compatible rendering: a named abbreviation is shown only for the
    ## North American zones plus @qcode{GMT} and @qcode{UTC}; every other zone
    ## renders as a numeric UTC offset such as @qcode{UTC+3} or
    ## @qcode{UTC+5:30}.
    ## @end table
    ##
    ## @code{@var{style} = datetime.zoneNameStyle ()} returns the current
    ## style.  @code{datetime.zoneNameStyle (@var{style})} sets it to
    ## @var{style}, either @qcode{'iana'} or @qcode{'matlab'}.  The setting
    ## persists for the current Octave session.
    ##
    ## An individual @code{Format} string can override the session style with
    ## the Octave-specific fields @qcode{zzzz} (force @qcode{'iana'}) and
    ## @qcode{zzzzz} (force @qcode{'matlab'}).
    ##
    ## @end deftypefn
    function out = zoneNameStyle (style)
      persistent current;
      if (isempty (current))
        current = 'iana';
      endif
      if (nargin < 1)
        out = current;
      else
        if (! (ischar (style) && isrow (style)) ...
            || ! any (strcmpi (style, {'iana', 'matlab'})))
          error ("datetime.zoneNameStyle: STYLE must be 'iana' or 'matlab'.");
        endif
        current = lower (style);
        if (nargout > 0)
          out = current;
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {} datetime.setDefaultFormats (@qcode{'default'}, @var{fmt})
    ## @deftypefnx {datetime} {} datetime.setDefaultFormats (@qcode{'defaultdate'}, @var{fmt})
    ## @deftypefnx {datetime} {} datetime.setDefaultFormats (@qcode{'reset'})
    ##
    ## Set the default display formats of datetime arrays.
    ##
    ## A datetime array whose @qcode{Format} property was never set explicitly
    ## displays with one of two default formats, and this method chooses them.
    ## The choice between the two is made from the values: an array whose
    ## elements all sit at midnight uses the date-only default, and any other
    ## array uses the date-and-time default.  The choice is made afresh on every
    ## display, so an array moves between the two when its components change.
    ##
    ## @code{datetime.setDefaultFormats (@qcode{'default'}, @var{fmt})} sets the
    ## date-and-time default to @var{fmt}, and
    ## @code{datetime.setDefaultFormats (@qcode{'defaultdate'}, @var{fmt})} sets
    ## the date-only default.  @var{fmt} is a display format, given as a
    ## character vector or string scalar, and is validated as the @qcode{Format}
    ## property is.  Arrays that already exist take the new format on their next
    ## display; those with an explicitly set @qcode{Format} are unaffected.
    ##
    ## @code{datetime.setDefaultFormats (@qcode{'reset'})} restores both factory
    ## formats, @qcode{'dd-MMM-uuuu HH:mm:ss'} and @qcode{'dd-MMM-uuuu'}.
    ##
    ## Both formats persist across Octave sessions.
    ##
    ## @strong{Deviation from MATLAB.}  MATLAB accepts a second argument after
    ## @qcode{'reset'} and silently ignores it, resetting both defaults whatever
    ## it says, so @code{datetime.setDefaultFormats ('reset', 'default')} reads
    ## as a request to reset one and quietly resets two.  Anything at all is
    ## accepted there, a number included.  Here that is an error, since no
    ## correct program can depend on the argument being ignored.
    ##
    ## @end deftypefn
    function setDefaultFormats (what, fmt)
      if (nargin < 1)
        error ("datetime.setDefaultFormats: too few input arguments.");
      endif
      what = convertStringsToChars (what);
      if (! (ischar (what) && isrow (what)) ...
          || ! any (strcmpi (what, {'default', 'defaultdate', 'reset'})))
        error (strcat ("datetime.setDefaultFormats: the first input must", ...
                       " be 'default', 'defaultdate', or 'reset'."));
      endif
      ## Unlike MATLAB, which takes a second argument here and discards it.
      if (strcmpi (what, 'reset'))
        if (nargin > 1)
          error (strcat ("datetime.setDefaultFormats: 'reset' takes no", ...
                         " further arguments; it resets both defaults."));
        endif
        dtDefaultFormats ('reset');
        return;
      endif
      if (nargin < 2)
        error ("datetime.setDefaultFormats: too few input arguments.");
      endif
      fmt = convertStringsToChars (fmt);
      if (! (ischar (fmt) && isrow (fmt)))
        error (strcat ("datetime.setDefaultFormats: FMT must be a", ...
                       " character vector or a string scalar."));
      endif
      if (any (strcmpi (fmt, {'default', 'defaultdate', 'preserveinput'})))
        error (strcat ("datetime.setDefaultFormats: FMT must be a display", ...
                       " format, not the name of a default."));
      endif
      dtValidateFormat (fmt);
      dtDefaultFormats ('set', lower (what), fmt);
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
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateStrings}, @qcode{'MixedFormats'}, @var{TF})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateStrings}, @qcode{'InputFormat'}, @var{INFMT})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateStrings},@
    ## @qcode{'InputFormat'}, @var{INFMT}, @qcode{'PivotYear'}, @var{PIVOT})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateStrings},@
    ## @qcode{'InputFormat'}, @var{INFMT}, @qcode{'Locale'}, @var{LOCALE})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{DateVectors})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{Y}, @var{MO}, @var{D})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S}, @var{MS})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{X}, @qcode{'ConvertFrom'}, @var{TYPE})
    ## @deftypefnx {datetime} {@var{T} =} datetime (@var{D})
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
    ## from the text in @var{DateStrings} representing points in time.  Without
    ## an @qcode{'InputFormat'}, one format is detected from the first piece of
    ## text that is not blank, and every other piece is then read with that same
    ## format: text written in a different one is not given a format of its own
    ## but becomes @code{NaT}, so that a column of dates is read as the single
    ## thing it is meant to be.  Text no format can be detected from at all is
    ## refused outright.  A date that the detected format cannot make sense of,
    ## such as @qcode{'2024-04-31'}, is refused in the same way an explicit
    ## @qcode{'InputFormat'} refuses it.
    ##
    ## The formats detected are:
    ##
    ## @multitable @columnfractions 0.34 0.66
    ## @item @qcode{'uuuu-MM-dd'} @tab optionally followed by @qcode{'HH:mm'},
    ## @qcode{'HH:mm:ss'}, or either with fractional seconds, the date and the
    ## time separated by a space or by @qcode{'T'}.
    ## @item @qcode{'dd-MMM-uuuu'} @tab month named in full or abbreviated,
    ## optionally followed by a time as above.
    ## @item @qcode{'MMMM d, uuuu'} @tab as in @qcode{'March 15, 2024'}.
    ## @item @qcode{'MM/dd/uuuu'} @tab or @qcode{'dd/MM/uuuu'}; see below.
    ## @item @qcode{'HH:mm:ss'} @tab a time alone, taking today's date.
    ## @end multitable
    ##
    ## Three further shapes are read that MATLAB does not detect, and are
    ## Octave extensions: @qcode{'uuuu/MM/dd'} with an optional time,
    ## @qcode{'dd MMMM uuuu'} as in @qcode{'15 March 2024'}, and a year alone.
    ## Numeric fields need not be padded, so @qcode{'2024-2-9'} reads as
    ## @qcode{'2024-02-09'} does.
    ##
    ## A date written with slashes is ambiguous: @qcode{'03/09/2024'} is the 3rd
    ## of September to some readers and the 9th of March to others.  The whole
    ## array decides, since one entry naming a day past the twelfth settles the
    ## order for all of them; where nothing settles it the American reading is
    ## taken and a warning
    ## (@qcode{'Octave:datetime:ambiguous-format'}) is raised.
    ##
    ## @code{@var{T} = datetime (@var{DateStrings}, @qcode{'MixedFormats'},
    ## @code{true})} detects a format for each piece of text separately instead,
    ## so text gathered from several sources into one column is read whatever
    ## each entry happens to be written in.  This is an Octave extension, off by
    ## default; MATLAB has no equivalent.  It reads the wider set of formats
    ## Octave's core @code{datevec} accepts, and rolls an impossible date over
    ## rather than refusing it, so @qcode{'2024-04-31'} is read as the 1st of
    ## May.  It cannot tell a mistake from a format it has not seen before,
    ## which is why it is not the default.
    ##
    ## @code{@var{T} = datetime (@var{DateStrings}, @qcode{'InputFormat'},
    ## @var{INFMT})} also allows to specify a particular input text format to
    ## parse @var{DateStrings}.  It is always preferable to specify the format
    ## @var{INFMT} if it is known.  @var{INFMT} uses the Unicode LDML date field
    ## symbols (@qcode{'y'}, @qcode{'M'}, @qcode{'d'}, @qcode{'H'}, @qcode{'h'},
    ## @qcode{'m'}, @qcode{'s'}, @qcode{'S'}, @qcode{'a'}, @dots{}), the same
    ## set used for display formats; text between single quotes is treated as a
    ## literal.  Formats which do not specify a particular time component will
    ## have the value set to zero.  Formats which do not specify any date
    ## component default to the current date, whereas a partially specified date
    ## defaults its missing month and day to 1.
    ##
    ## A string that does not match @var{INFMT}, or that names a date which does
    ## not exist, such as @qcode{'2024-04-31'} or a 29th of February outside a
    ## leap year, cannot be converted.  A lone such string is an error; within
    ## an array only that element is lost and becomes @code{NaT}, so that one
    ## unreadable entry does not cost the rest of the array.
    ##
    ## @code{@var{T} = datetime (@var{DateStrings}, @qcode{'InputFormat'},
    ## @var{INFMT}, @qcode{'PivotYear'}, @var{PIVOT})} also allows to specify a
    ## pivot year, which refers to the year at the start of the century to which
    ## two-digit years will be referenced.  When not specified, it defaults to
    ## the current year minus 50.
    ##
    ## @code{@var{T} = datetime (@var{DateStrings}, @qcode{'InputFormat'},
    ## @var{INFMT}, @qcode{'Locale'}, @var{LOCALE})} interprets the month names,
    ## weekday names, and day-period markers in @var{DateStrings} according to
    ## @var{LOCALE}, given as an @qcode{'xx_YY'} identifier whose language part
    ## selects the names.  The supported languages are @qcode{'en'} (the
    ## default), @qcode{'fr'}, @qcode{'de'}, @qcode{'es'}, @qcode{'it'},
    ## @qcode{'pt'}, and @qcode{'el'}; @qcode{'system'} is treated as
    ## @qcode{'en'}.  Both full (@qcode{'MMMM'}/@qcode{'eeee'}) and abbreviated
    ## (@qcode{'MMM'}/@qcode{'eee'}) month and weekday names are recognized
    ## case-insensitively; for Greek, matching is also accent-insensitive, so
    ## the accentless all-caps spelling is accepted, and the genitive month
    ## forms (@qcode{'Μαρτίου'}) are used.  A weekday name is validated but does
    ## not otherwise affect the result.
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
    ## @item @qcode{'yyyymmdd'}
    ## @item @qcode{'tt2000'} -- requires @code{int64} input and the
    ## @qcode{'UTCLeapSeconds'} time zone (see @code{convertTo}).
    ## @end itemize
    ##
    ## @code{@var{T} = datetime (@var{D})}, where @var{D} is already a datetime
    ## array, copies it: the components, the @code{TimeZone} and the
    ## @code{Format} all carry over.  A @qcode{'Format'} or @qcode{'TimeZone'}
    ## may still be given to change either, and take effect exactly as assigning
    ## those properties does, so attaching a zone to an unzoned array keeps its
    ## wall-clock values whereas changing between two zones keeps the instant.
    ## The options describing how text is read, @qcode{'InputFormat'},
    ## @qcode{'Locale'} and @qcode{'PivotYear'}, have nothing to act on and are
    ## ignored, while @qcode{'ConvertFrom'} is an error.
    ##
    ## @code{@var{T} = datetime (@dots{}, @qcode{'Format'}, @var{FMT})}
    ## specifies the display format of the values in the output datetime array.
    ## @var{FMT} uses the same Unicode LDML date field symbols as
    ## @qcode{'InputFormat'}, with @qcode{'z'}, @qcode{'Z'}, @qcode{'X'}, and
    ## @qcode{'x'} additionally naming the time zone, and text between single
    ## quotes taken literally.  The default format renders a date alone when
    ## every element sits at midnight and a date with a time otherwise; a
    ## @code{NaT} carries no time of day and does not affect that choice.
    ##
    ## @code{@var{T} = datetime (@dots{}, @qcode{'TimeZone'}, @var{TZ})} sets
    ## the time zone of the values in the output datetime array.  If not
    ## specified, the array is unzoned: its values are wall-clock readings that
    ## name no absolute instant, and no daylight saving rule applies to them.
    ## Supported time zones are those of the IANA Time Zone Database.  A zone
    ## may also be attached, changed, or dropped afterwards through the
    ## @qcode{'TimeZone'} property; attaching one reinterprets the wall-clock
    ## values in that zone, whereas changing between two zones preserves the
    ## absolute instant and shifts the wall-clock values by the difference in
    ## offset.
    ##
    ## Twice a year a zone that observes Daylight Saving Time (DST) has
    ## wall-clock readings that name no unique instant.  Where the clock goes
    ## back an hour repeats, and such a reading is taken at the later of the two
    ## offsets, that is, standard time.  Where the clock goes forward an
    ## interval is skipped, and a reading inside it is one no clock in that zone
    ## ever shows.  Given as numeric components, such a reading is moved ahead
    ## by the length of the skipped interval, so that with the usual one-hour
    ## skip @qcode{'02:30'} becomes @qcode{'03:30'}; given as text it is instead
    ## rejected, since text is a claim about a reading that never occurred.  A
    ## lone such string is an error, whereas within an array only that element
    ## is lost and becomes @code{NaT}; without an @qcode{'InputFormat'} the
    ## whole input is rejected, as when no format can be detected at all.
    ##
    ## In the @qcode{'UTCLeapSeconds'} zone the seconds component may reach 60,
    ## naming an inserted leap second, but only on one of the 27 dates that has
    ## one and only in the last minute of the day; anywhere else it rolls over
    ## as usual.  The seconds component is counted along the leap-second
    ## timeline, so @qcode{'23:59:61'} on such a date is the next midnight
    ## rather than one second past it, whereas an hour or minute that overflows
    ## over the inserted second entirely.  Text naming a leap second that was
    ## never inserted is rejected, exactly as text naming a wall clock a zone
    ## skips is.  Without an @qcode{'InputFormat'} such an array reads only the
    ## ISO 8601 UTC shape it also writes.
    ##
    ## @strong{Deviations from MATLAB} when copying a datetime array and giving
    ## a @qcode{'TimeZone'}.  MATLAB's constructor keeps the display format of
    ## the array it copies, while its @code{TimeZone} property assignment
    ## replaces it for the same change of zone; the two disagree, so here both
    ## follow the one rule, that of the property.  Leaving
    ## @qcode{'UTCLeapSeconds'} therefore restores the ordinary default format
    ## rather than keeping the ISO 8601 pattern, whose @qcode{'Z'} would
    ## misdescribe any zone but UTC.  For the same reason, entering
    ## @qcode{'UTCLeapSeconds'} works here, the locked format being applied,
    ## where MATLAB carries the copied format across and then rejects it as one
    ## that zone does not allow.
    ##
    ## @seealso{NaT, datetime, isdatetime, calendarDuration, duration}
    ## @end deftypefn
    function this = datetime (varargin)

      ## Return an scalar datetime object with current local time
      if (nargin == 0)
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ ('now');
        this.Offset = zeros (size (this.Year));
        return;
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'ConvertFrom', 'Format', 'InputFormat', 'Locale', ...
                  'MixedFormats', 'PivotYear', 'TimeZone'};
      dfValues = {[], [], [], [], false, [], []};
      [ConvertFrom, Format, inputFormat, Locale, MixedFormats, PivotYear, ...
       TimeZone, args] = parsePairedArguments (optNames, dfValues, varargin(:));
      if (! ((islogical (MixedFormats) || isnumeric (MixedFormats))
             && isscalar (MixedFormats)))
        error ("datetime: 'MixedFormats' must be a logical scalar.");
      endif
      MixedFormats = logical (MixedFormats);

      ## A datetime input is copied: its components, time zone and display
      ## format all carry over.  'Format' and 'TimeZone' may still be given to
      ## change either, and are applied by the same rules as assigning the
      ## properties -- attaching a zone to an unzoned array keeps its wall clock
      ## while changing between two zones keeps the instant.  'InputFormat',
      ## 'Locale' and 'PivotYear' describe how text is read and so have nothing
      ## to act on here; MATLAB ignores them and so do we.
      if (isa (args{1}, 'datetime'))
        if (numel (args) > 1)
          error (strcat ("datetime: a datetime array must be the only", ...
                         " positional input."));
        elseif (! isempty (ConvertFrom))
          error (strcat ("datetime: 'ConvertFrom' cannot be used with a", ...
                         " datetime array input."));
        endif
        this = args{1};
        ## Routing through subsasgn keeps one rule in the class for what a
        ## change of zone means.  MATLAB has two: its constructor carries the
        ## copied display format across the change while its property assignment
        ## replaces it.  So leaving 'UTCLeapSeconds' here restores the default
        ## format rather than keeping a pattern whose 'Z' would misdescribe any
        ## zone but UTC, and entering it works rather than failing on a format
        ## carried over from an array that never had leap seconds.  Documented
        ## in the constructor's docstring.
        ## An unset option arrives as [], so an empty character vector is told
        ## apart by its class: given as '' it means drop the zone, which is not
        ## the same as saying nothing about it.
        if (ischar (TimeZone) || ! isempty (TimeZone))
          this = subsasgn (this, substruct ('.', 'TimeZone'), TimeZone);
        endif
        if (! isempty (Format))
          this = subsasgn (this, substruct ('.', 'Format'), Format);
        endif
        return;
      endif

      ## Nothing else non-numeric can name a date.  A duration and a
      ## calendarDuration measure elapsed time rather than name an instant, so
      ## neither can be converted to one.
      if (! (isnumeric (args{1}) || islogical (args{1}) || ischar (args{1})
             || iscellstr (args{1}) || isa (args{1}, 'string')))
        error (strcat ("datetime: input data must be a numeric array, a", ...
                       " string array, a cell array of character vectors,", ...
                       " or a character vector."));
      endif

      ## Check optional 'Format' and 'InputFormat' arguments.  'yyyymmdd' and
      ## 'tt2000' are handled in M-code below, so they bypass the builtin
      ## ConvertFrom check.
      if (! isempty (ConvertFrom) && ! any (strcmpi (ConvertFrom, ...
                                                    {'yyyymmdd', 'tt2000'})))
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
        dtValidateFormat (Format);
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
        endif
        ## Validate against the supported locale set (errors if unsupported).
        dtLocaleNames (Locale);
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
      if (dtIsLeapZone (TimeZone))
        ## Only one display pattern can render a 60th second, so a leap-second
        ## array carries it instead of the data-dependent default, and any other
        ## pattern the caller asked for is rejected.
        if (isempty (Format))
          this.Format = dtLeapFormat ();
        else
          dtValidateLeapFormat (Format, 'datetime');
        endif
      endif

      ## Datestrings are currently handled by 'datevec'
      if (iscellstr (args{1}) || isstring (args{1}) || ischar (args{1}))
        DateStrings = "";
        ## The relative-day keywords are text too, but name no date to parse and
        ## are answered by the builtin further down.
        isRelDay = ischar (args{1}) && isrow (args{1}) ...
                   && any (strcmpi (args{1}, {'now', 'today', 'yesterday', ...
                                              'tomorrow'}));
        if (ischar (args{1}) && ndims (args{1}) > 2)
          error ("datetime: invalid type for 'DateStrings'.");
        elseif (! isRelDay)
          if (ischar (args{1}) && rows (args{1}) == 0
                               && columns (args{1}) > 0)
            ## A character matrix with no rows holds no text at all, unlike ''
            ## which is one empty piece of it.  Octave's 'cellstr' does not draw
            ## that line, reporting one empty string for either.
            DateStrings = cell (0, 1);
          else
            ## A character matrix holds one date per row, which is what
            ## 'cellstr' reads it as, trailing blanks trimmed.
            DateStrings = cellstr (args{1});
          endif
        endif
        if (! isRelDay)
          if (isempty (DateStrings))
            ## A container holding no text at all gives an empty datetime rather
            ## than a missing one: there is no element for a NaT to occupy.
            ## Note this is not the same as an empty character vector, which
            ## 'cellstr' reports as one piece of text and which becomes a NaT.
            [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
             this.Second] = deal (nan (size (DateStrings)));
            this.Offset = nan (size (this.Year));
            return;
          endif
          ## Empty text names no date at all.  It is missing rather than
          ## unreadable, so it becomes NaT and leaves the text beside it alone,
          ## as MATLAB does: a blank field among imported dates must not cost
          ## the whole column.  Only the rest is handed to a parser.
          strs = DateStrings(:);
          blank = cellfun (@isempty, strs);
          DATEVEC = nan (numel (strs), 6);
          if (! all (blank))
            live = strs(! blank);
            if (! isempty (inputFormat))
              ## LDML-aware parse under the supplied 'InputFormat'.  MATLAB's
              ## default pivot for two-digit years is the current year minus 50.
              dtValidateFormat (inputFormat);
              if (! isempty (PivotYear))
                pivot = PivotYear;
              else
                now6 = clock ();
                pivot = now6(1) - 50;
              endif
              DV = dtParseInput (live, inputFormat, pivot, Locale, ...
                                 dtIsLeapZone (TimeZone));
            elseif (dtIsLeapZone (TimeZone))
              ## A leap-second array reads text in the one shape it can also
              ## write, so nothing is auto-detected here: the string must be
              ## the ISO 8601 UTC form, with or without fractional seconds.
              ## Anything else is rejected even where a general datetime array
              ## would accept it.
              DV = dtParseLeapText (live);
            elseif (MixedFormats)
              ## The Octave extension: read every string on its own terms, so a
              ## column gathered from several sources parses whatever each entry
              ## happens to be written in.  Core 'datevec' detects per element
              ## and is what makes that possible; it also rolls an impossible
              ## date over rather than refusing it.
              fcn = @(x) datevec (x);
              try
                DV = cellfun (fcn, live, "UniformOutput", false);
                DV = cell2mat (DV(:));
              catch
                error (strcat ("datetime: could not recognize date/time", ...
                               " format from input."));
              end_try_catch
            else
              ## No format supplied: detect one from the text and read every
              ## string with it, as MATLAB does.  A string the detected format
              ## cannot read becomes NaT rather than costing the rest, which is
              ## the rule an explicit 'InputFormat' already follows; text no
              ## format can be detected from at all is refused outright.
              detected = dtDetectFormat (live);
              if (isempty (detected))
                error (strcat ("datetime: could not recognize the", ...
                               " date/time format of '%s'."), live{1});
              endif
              try
                DV = dtParseInput (live, detected, dtDefaultPivot (), '', ...
                                   dtIsLeapZone (TimeZone));
              catch
                ## Only a lone string raises here, the array case losing just
                ## the offending element.  Its message names an 'InputFormat'
                ## the caller never gave, so say instead what is true: the text
                ## could not be read.
                error (strcat ("datetime: could not recognize date/time", ...
                               " format from input."));
              end_try_catch
            endif
            DATEVEC(! blank, :) = DV;
          endif
          ## Split DATEVEC into individual date/time units and reshape
          this.Year = reshape (DATEVEC(:,1), size (DateStrings));
          this.Month = reshape (DATEVEC(:,2), size (DateStrings));
          this.Day = reshape (DATEVEC(:,3), size (DateStrings));
          this.Hour = reshape (DATEVEC(:,4), size (DateStrings));
          this.Minute = reshape (DATEVEC(:,5), size (DateStrings));
          this.Second = reshape (DATEVEC(:,6), size (DateStrings));
          if (! isempty (TimeZone))
            ## Text naming a wall clock its zone never shows -- one inside the
            ## interval the clock skips going forward -- is rejected, matching
            ## MATLAB.  (Numeric components are instead moved ahead by the gap;
            ## MATLAB draws that distinction too.)  Normalizing in the zone
            ## moves exactly those elements, because every other parsed wall
            ## clock is already canonical, so an element that moves marks text
            ## to reject.  The same test catches text naming a leap second that
            ## was never inserted: a genuine one survives normalization on the
            ## leap-second timeline untouched, a spurious one rolls over.
            if (dtIsLeapZone (TimeZone))
              [Yz, Mz, Dz, Hz, MIz, Sz] = dtLeapNormalize (this.Year, ...
                                          this.Month, this.Day, this.Hour, ...
                                          this.Minute, this.Second);
            else
              [Yz, Mz, Dz, Hz, MIz, Sz] = __datetime__ (this.Year, ...
                                  this.Month, this.Day, this.Hour, ...
                                  this.Minute, this.Second, 'TimeZone', ...
                                  TimeZone, 'toTimeZone', TimeZone, ...
                                  'Precision', 'microseconds');
            endif
            skipped = (Yz != this.Year | Mz != this.Month | Dz != this.Day ...
                       | Hz != this.Hour | MIz != this.Minute ...
                       | abs (Sz - this.Second) > 1e-3) & ! isnan (this.Year);
            if (any (skipped(:)))
              if (isempty (inputFormat))
                ## Without a format, an unreadable element condemns the whole
                ## input, as it does when 'datevec' fails to detect a format.
                error (strcat ("datetime: could not recognize date/time", ...
                               " format from input."));
              elseif (numel (skipped) == 1)
                error (strcat ("datetime: could not parse the date/time", ...
                               " string '%s' with 'InputFormat' '%s'."), ...
                       DateStrings{1}, inputFormat);
              endif
              this.Year(skipped) = NaN;
              this.Month(skipped) = NaN;
              this.Day(skipped) = NaN;
              this.Hour(skipped) = NaN;
              this.Minute(skipped) = NaN;
              this.Second(skipped) = NaN;
            endif
          endif
          this.Offset = dtOffsetOf (this.Year, this.Month, this.Day, ...
                                    this.Hour, this.Minute, this.Second, ...
                                    this.TimeZone);
          return;
        endif
      endif

      ## Handle inputs (no errors here).  INSTOFF stays empty unless a branch
      ## builds from an instant and measures its own offset; see the choke
      ## point at the end of the constructor.
      instOff = [];
      if (! isempty (ConvertFrom) && strcmpi (ConvertFrom, 'yyyymmdd'))
        ## Decompose a YYYYMMDD integer into year, month, and day components
        ## and construct through the normal component path.
        x = args{1};
        if (! isnumeric (x))
          error ("datetime: 'yyyymmdd' input must be numeric.");
        endif
        Y = floor (x ./ 10000);
        M = floor (mod (x, 10000) ./ 100);
        D = mod (x, 100);
        dtCheckIntegerComponents ({Y, M, D});
        if (! isempty (TimeZone))
          [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
           this.Second] = __datetime__ (Y, M, D, 'TimeZone', TimeZone, ...
                                        'Precision', 'microseconds');
        else
          [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
           this.Second] = __datetime__ (Y, M, D, 'Precision', 'microseconds');
        endif
      elseif (! isempty (ConvertFrom) && strcmpi (ConvertFrom, 'tt2000'))
        ## A tt2000 time is a count of SI seconds, so it can name an inserted
        ## second and only means anything in the leap-second zone.  MATLAB
        ## insists on int64 input rather than rounding a double, since a double
        ## cannot hold a nanosecond count of this magnitude exactly.
        x = args{1};
        if (! isa (x, 'int64'))
          error (strcat ("datetime: input for converting from tt2000 times", ...
                         " must be int64."));
        elseif (! dtIsLeapZone (TimeZone))
          error (strcat ("datetime: to create datetimes from tt2000 times,", ...
                         " the 'TimeZone' parameter must be", ...
                         " 'UTCLeapSeconds'."));
        endif
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = dtLeapComponents (dtTT20002Serial (x));
      elseif (! isempty (ConvertFrom) && ! isempty (TimeZone))
        if (strcmpi (ConvertFrom, 'posixtime'))
          ## POSIX time is an absolute UTC instant, so read it as a UTC wall
          ## clock and then convert into the requested zone (honouring DST).
          [Yp, Mp, Dp, hp, mp, sp] = __datetime__ (args{1}, 'ConvertFrom', ...
                                                   'posixtime', 'Precision', ...
                                                   'microseconds');
          [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
           this.Second] = __datetime__ (Yp, Mp, Dp, hp, mp, sp, 'TimeZone', ...
                          'UTC', 'toTimeZone', TimeZone, 'Precision', ...
                          'microseconds');
          ## This branch alone names an INSTANT rather than a wall clock, and
          ## which of the two moments sharing a clock on a fall-back day was
          ## meant is settled by the number given, not by resolve_local.  So
          ## the offset is measured against that number here, and the
          ## wall-clock derivation at the end of the constructor skipped.
          instOff = round (__datetime__ (this.Year, this.Month, this.Day, ...
                           this.Hour, this.Minute, this.Second, 'ConvertTo', ...
                           'posixtime', 'TimeZone', 'UTC', 'Precision', ...
                           'microseconds') - double (args{1}));
        else
          [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
           this.Second] = __datetime__ (args{1}, 'ConvertFrom', ConvertFrom, ...
                                        'TimeZone', TimeZone, 'Precision', ...
                                        'microseconds');
        endif
      elseif (! isempty (ConvertFrom) && isempty (TimeZone))
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (args{1}, 'ConvertFrom', ConvertFrom, ...
                                      'Precision', 'microseconds');
      else
        dtCheckIntegerComponents (args);
        if (dtIsLeapZone (TimeZone))
          ## Let the builtin validate the shape of the positional arguments,
          ## then normalize on the leap-second timeline instead, where a 60th
          ## second of a minute exists and so must survive the round trip.
          [~,~,~,~,~,~, errmsg] = __datetime__ (args{:}, 'Precision', ...
                                                'microseconds');
          if (! isnumeric (errmsg))
            error ("datetime: %s ", errmsg);
          endif
          [Yr, Mr, Dr, hr, mir, sr] = dtSplitComponents (args);
          [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
           this.Second] = dtLeapNormalize (Yr, Mr, Dr, hr, mir, sr);
        elseif (! isempty (TimeZone))
          ## Normalize the wall clock in its own zone, so that a time the
          ## local clock never shows -- the gap when the clock goes forward --
          ## moves ahead by the length of that gap, as MATLAB does.  Round
          ## tripping through the zone leaves every other wall clock alone.
          [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
           this.Second] = __datetime__ (args{:}, 'TimeZone', TimeZone, ...
                                        'toTimeZone', TimeZone, ...
                                        'Precision', 'microseconds');
        else
          [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
           this.Second] = __datetime__ (args{:}, 'Precision', 'microseconds');
        endif
      endif

      ## Every branch above builds the array from a WALL CLOCK, so the offset
      ## follows from resolve_local's choices.  Setting it here rather than in
      ## each branch keeps the size invariant (Offset always matches Year) at
      ## one choke point.  The exception is a branch that was handed an instant
      ## and has already measured its own offset against it, which no reading
      ## of the resulting wall clock could recover.
      if (isempty (instOff))
        this.Offset = dtOffsetOf (this.Year, this.Month, this.Day, ...
                                  this.Hour, this.Minute, this.Second, ...
                                  this.TimeZone);
      else
        this.Offset = instOff + zeros (size (this.Year));
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
      ## Resolve the (possibly sentinel) Format to a concrete LDML pattern,
      ## then render every element under that single array-wide pattern.
      fmt = dtResolveFormat (this.Format, this.Hour, this.Minute, ...
                             this.Second);
      cstr = dtFormatStrings (this.Year, this.Month, this.Day, this.Hour, ...
                              this.Minute, this.Second, this.TimeZone, ...
                              this.Offset, fmt, datetime.zoneNameStyle ());
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

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{W} =} week (@var{T})
    ## @deftypefnx {datetime} {@var{W} =} week (@var{T}, @var{weekType})
    ##
    ## Week component of a datetime array.
    ##
    ## @code{@var{W} = week (@var{T})} returns the week-of-year number for each
    ## element of the input datetime array @var{T}.  The output @var{W} is a
    ## @qcode{double} array containing integer values in the range
    ## @math{[1, 54]} and it has the same size as @var{T}.  Weeks are counted
    ## from Sunday to Saturday, and the week containing January 1 is week 1.
    ## Not-A-Time (@qcode{NaT}) values in @var{T} are returned as @qcode{NaN}
    ## in the output array.
    ##
    ## @code{@var{W} = week (@var{T}, @var{weekType})} returns the week number
    ## for each element of the input datetime array @var{T} as specified by
    ## @var{weekType}, which may have any of the following options:
    ##
    ## @itemize
    ## @item @qcode{'weekofyear'} (default) returns the week of the year in a
    ## numeric array, in the range @math{[1, 54]}.
    ## @item @qcode{'weekofmonth'} returns the week of the month in a numeric
    ## array, in the range @math{[1, 6]}.
    ## @end itemize
    ##
    ## @end deftypefn
    function out = week (this, weekType = 'weekofyear')
      ## Weekday (Sun = 1 @dots{} Sat = 7) of each element, NaN for NaT.
      dow = day (this, 'dayofweek');
      if (strcmpi (weekType, 'weekofyear'))
        D = day (this, 'dayofyear');
        ## 0-based weekday of January 1 derived from this element's own
        ## weekday and day-of-year (0 = Sunday @dots{} 6 = Saturday).
        w0 = mod ((dow - 1) - (D - 1), 7);
        out = floor ((D - 1 + w0) ./ 7) + 1;
      elseif (strcmpi (weekType, 'weekofmonth'))
        dm = this.Day;
        w0 = mod ((dow - 1) - (dm - 1), 7);
        out = floor ((dm - 1 + w0) ./ 7) + 1;
      else
        error ("datetime.week: unrecognized WEEKTYPE.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{D} =} timeofday (@var{T})
    ##
    ## Elapsed time since midnight of a datetime array.
    ##
    ## @code{@var{D} = timeofday (@var{T})} returns the elapsed time since
    ## midnight for each element of the input datetime array @var{T} as a
    ## @code{duration} array @var{D} of the same size as @var{T}.  For datetime
    ## arrays with a time zone, the result accounts for any daylight saving time
    ## shift occurring since midnight, so it may differ from the displayed clock
    ## time on the day of a transition.  Not-A-Time (@qcode{NaT}) values in
    ## @var{T} are returned as @qcode{NaN} durations.
    ##
    ## @end deftypefn
    function out = timeofday (this)
      out = this - dateshift (this, 'start', 'day');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{DT} =} tzoffset (@var{T})
    ##
    ## Time zone offset of a datetime array.
    ##
    ## @code{@var{DT} = tzoffset (@var{T})} returns the offset from UTC of each
    ## element of the input datetime array @var{T} as a @code{duration} array
    ## @var{DT} of the same size as @var{T}.  The offset is positive for time
    ## zones east of UTC and includes the additional hour when daylight saving
    ## time is in effect.  If @var{T} has no time zone, or for Not-A-Time
    ## (@qcode{NaT}) values, the corresponding offset is @qcode{NaN}.
    ##
    ## @end deftypefn
    function out = tzoffset (this)
      if (isempty (this.TimeZone))
        secs = nan (size (this));
      else
        ## The stored offset is the answer already, and reading it rather than
        ## resolving the wall clock again is what tells the two passes over a
        ## repeated clock apart: the tz database would give both the later one.
        secs = this.Offset + zeros (size (this.Year));
      endif
      out = duration (0, 0, secs);
      out.Format = 'hh:mm';
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
        error (strcat ("datetime.size: nargout > 1 but does not", ...
                       " match number of requested dimensions."));
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
        ## Hashed alongside the components because it is part of what makes an
        ## element itself: two elements sharing a wall clock across a fall-back
        ## are not equal and so must not be given the same key.
        key = __nkeyHash__((this.Offset + zeros (size (this.Year)))(:), key);
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

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{P} =} posixtime (@var{T})
    ##
    ## Convert datetime array to POSIX time.
    ##
    ## @code{@var{P} = posixtime (@var{T})} returns the number of seconds
    ## elapsed since the epoch @code{1970-01-01 00:00:00 UTC} for each element
    ## of the input datetime array @var{T}.  The output @var{P} is a
    ## @qcode{double} array of the same size as @var{T} and includes any
    ## fractional seconds.  Datetime arrays without a time zone are treated as
    ## UTC.  Not-A-Time (@qcode{NaT}) values are returned as @qcode{NaN}.
    ##
    ## POSIX time has no stamp of its own for an inserted leap second, so for a
    ## @qcode{'UTCLeapSeconds'} array the 60th second of a minute shares the
    ## stamp of the second that follows it: @code{posixtime} of
    ## @code{2016-12-31T23:59:60Z} and of @code{2017-01-01T00:00:00Z} are both
    ## @code{1483228800}.  Every other conversion folds the other way (see
    ## @code{convertTo}).
    ##
    ## @end deftypefn
    function out = posixtime (this)
      if (dtIsLeapZone (this.TimeZone))
        out = dtLeapPosix (this.Year, this.Month, this.Day, this.Hour, ...
                           this.Minute, this.Second);
      else
        out = serial (this);
      endif
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

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{S} =} datestr (@var{T})
    ## @deftypefnx {datetime} {@var{S} =} datestr (@var{T}, @var{f})
    ## @deftypefnx {datetime} {@var{S} =} datestr (@dots{}, @var{opt})
    ##
    ## Convert datetime array to a character array of date strings.
    ##
    ## @code{@var{S} = datestr (@var{T})} converts the datetime array @var{T} to
    ## a character array @var{S} with one date string per row, using the default
    ## format of the core @code{datestr} function.
    ##
    ## @code{@var{S} = datestr (@var{T}, @var{f})} uses the format specified by
    ## @var{f}, a format number or a format string that follows the @emph{legacy}
    ## @code{datestr} field codes, in which @qcode{'mm'} denotes months and
    ## @qcode{'MM'} denotes minutes.  Any further arguments @var{opt} are passed
    ## on to the core @code{datestr} function.
    ##
    ## @code{datestr} is provided for compatibility with legacy code.  It renders
    ## the wall-clock components of @var{T} and, for a zoned array, does not
    ## include the time zone.  For time-zone-aware formatting with the modern
    ## Unicode (LDML) field codes, use @code{char}, @code{cellstr}, or set the
    ## @qcode{Format} property of @var{T} instead.
    ##
    ## The legacy field codes have no way to write a 60th second, so for a
    ## @qcode{'UTCLeapSeconds'} array an inserted second folds backward onto the
    ## 59th, as it does in @code{datenum}.
    ##
    ## @end deftypefn
    function S = datestr (this, varargin)
      DV = datevec (this);
      if (dtIsLeapZone (this.TimeZone))
        DV(:,6) = dtLeapBackFold (DV(:,6));
      endif
      S = datestr (DV, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{S} =} string (@var{T})
    ## @deftypefnx {datetime} {@var{S} =} string (@var{T}, @var{fmt})
    ##
    ## Convert datetime array to a string array.
    ##
    ## @code{@var{S} = string (@var{T})} converts the datetime array @var{T} to a
    ## @code{string} array @var{S} of the same size, formatting each element with
    ## the display format of @var{T} (its @qcode{Format} property).  Not-A-Time
    ## (@qcode{NaT}) values become missing string elements.
    ##
    ## @code{@var{S} = string (@var{T}, @var{fmt})} formats each element with the
    ## Unicode (LDML) format @var{fmt} instead of the @qcode{Format} property of
    ## @var{T} (see @code{char}).
    ##
    ## @end deftypefn
    function s = string (this, fmt)
      if (nargin < 2)
        cstr = cellstr (this);
      else
        cstr = cellstr (this, fmt);
      endif
      nat = isnat (this);
      if (any (nat, 'all'))
        cstr(nat) = {missing};
      endif
      s = string (cstr);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{D} =} yyyymmdd (@var{T})
    ##
    ## Convert datetime array to @code{YYYYMMDD} numeric form.
    ##
    ## @code{@var{D} = yyyymmdd (@var{T})} returns a @qcode{double} array @var{D}
    ## of the same size as @var{T} in which each element encodes the date of the
    ## corresponding datetime as @code{@var{year} * 10000 + @var{month} * 100 +
    ## @var{day}}.  The time of day is ignored.  Not-A-Time (@qcode{NaT}) values
    ## are returned as @qcode{NaN}.
    ##
    ## @end deftypefn
    function out = yyyymmdd (this)
      out = this.Year * 10000 + this.Month * 100 + this.Day;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{N} =} datenum (@var{T})
    ##
    ## Convert datetime array to serial date numbers.
    ##
    ## @code{@var{N} = datenum (@var{T})} returns a @qcode{double} array @var{N}
    ## of the same size as @var{T} holding the serial date number of each
    ## element, where @code{1} corresponds to January 1 of the year 0000 and the
    ## fractional part represents the time of day.  The time zone of a zoned
    ## @var{T} is ignored; its wall-clock components are used.  Not-A-Time
    ## (@qcode{NaT}) values are returned as @qcode{NaN}, and infinite datetimes
    ## preserve their sign.
    ##
    ## A serial date number has no room for an inserted leap second, so for a
    ## @qcode{'UTCLeapSeconds'} array the 60th second of a minute folds backward
    ## onto the 59th: @code{datenum} of @code{2016-12-31T23:59:60Z} equals that
    ## of @code{2016-12-31T23:59:59Z}.
    ##
    ## @end deftypefn
    function out = datenum (this)
      Y = this.Year;  M = this.Month;  D = this.Day;
      h = this.Hour;  mi = this.Minute;  s = this.Second;
      if (dtIsLeapZone (this.TimeZone))
        s = dtLeapBackFold (s);
      endif
      out = nan (size (Y));
      ## Core datenum errors on NaN components, so screen NaT out first and let
      ## infinite datetimes carry their sign through unchanged.
      fin = isfinite (Y);
      out(fin) = datenum (Y(fin), M(fin), D(fin), h(fin), mi(fin), s(fin));
      out(isinf (Y)) = Y(isinf (Y));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{E} =} exceltime (@var{T})
    ## @deftypefnx {datetime} {@var{E} =} exceltime (@var{T}, @var{dateType})
    ##
    ## Convert datetime array to Excel serial date numbers.
    ##
    ## @code{@var{E} = exceltime (@var{T})} returns a @qcode{double} array
    ## @var{E} of the same size as @var{T} holding the Excel serial date number
    ## of each element, using the Excel 1900 date system, in which the fractional
    ## part represents the time of day.  As in Excel, the year 1900 is treated as
    ## a leap year, so serial numbers on or after 1900-03-01 account for the
    ## nonexistent date 1900-02-29.
    ##
    ## @code{@var{E} = exceltime (@var{T}, @var{dateType})} selects the date
    ## system: @qcode{'1900'} (default) or @qcode{'1904'}.  The 1904 system
    ## counts days from 1904-01-01 and has no leap-year anomaly.
    ##
    ## The time zone of a zoned @var{T} is ignored; its wall-clock components are
    ## used.  Not-A-Time (@qcode{NaT}) values are returned as @qcode{NaN}.
    ##
    ## @end deftypefn
    function out = exceltime (this, dateType = '1900')
      if (! (ischar (dateType) && isrow (dateType) ...
             && any (strcmpi (dateType, {'1900', '1904'}))))
        error ("datetime.exceltime: DATETYPE must be '1900' or '1904'.");
      endif
      dn = datenum (this);
      if (strcmpi (dateType, '1904'))
        out = dn - datenum (1904, 1, 1);
      else
        ## Excel's 1900 system counts days from the serial-0 epoch 1899-12-30,
        ## but wrongly treats 1900 as a leap year, so serials on or after
        ## 1900-03-01 are one greater, spanning the nonexistent 1900-02-29.
        out = dn - datenum (1899, 12, 30);
        out(dn < datenum (1900, 3, 1)) -= 1;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{J} =} juliandate (@var{T})
    ## @deftypefnx {datetime} {@var{J} =} juliandate (@var{T}, @var{dateType})
    ##
    ## Convert datetime array to Julian dates.
    ##
    ## @code{@var{J} = juliandate (@var{T})} returns a @qcode{double} array
    ## @var{J} of the same size as @var{T} holding the Julian date of each
    ## element, that is, the number of days (including a fractional part) since
    ## noon UTC on January 1, 4713 BCE.  Unlike @code{datenum}, the result is
    ## based on the absolute UTC instant, so the time zone of a zoned @var{T} is
    ## taken into account; datetime arrays without a time zone are treated as
    ## UTC.  Not-A-Time (@qcode{NaT}) values are returned as @qcode{NaN}.
    ##
    ## @code{@var{J} = juliandate (@var{T}, @var{dateType})} selects the kind of
    ## Julian date: @qcode{'juliandate'} (default) or
    ## @qcode{'modifiedjuliandate'}, the latter being the Julian date minus
    ## @code{2400000.5}.
    ##
    ## For a @qcode{'UTCLeapSeconds'} array a Julian day that holds an inserted
    ## second is 86401 seconds long, and the fractional part is that fraction of
    ## the day's true length.  Julian days run from noon to noon and modified
    ## Julian days from midnight to midnight, so on such a day the two are
    ## stretched over different spans and are not related by exactly
    ## @code{2400000.5}; both are @qcode{'juliandate'} for the day at hand.
    ##
    ## @end deftypefn
    function out = juliandate (this, dateType = 'juliandate')
      if (! (ischar (dateType) && isrow (dateType) && any (strcmpi (dateType, ...
             {'juliandate', 'modifiedjuliandate'}))))
        error (strcat ("datetime.juliandate: DATETYPE must be 'juliandate'", ...
                       " or 'modifiedjuliandate'."));
      endif
      if (dtIsLeapZone (this.TimeZone))
        out = dtLeapJulian (this.Year, this.Month, this.Day, this.Hour, ...
                            this.Minute, this.Second, ...
                            strcmpi (dateType, 'modifiedjuliandate'));
        return;
      endif
      ## Julian date of the Unix epoch (1970-01-01 00:00 UTC) is 2440587.5; the
      ## POSIX instant places the array on the absolute UTC timeline.
      out = posixtime (this) / 86400 + 2440587.5;
      if (strcmpi (dateType, 'modifiedjuliandate'))
        out -= 2400000.5;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{X} =} convertTo (@var{T}, @var{dateType})
    ## @deftypefnx {datetime} {@var{X} =} convertTo (@var{T}, @qcode{'epochtime'}, @var{Name}, @var{Value})
    ##
    ## Convert datetime array to a numeric representation.
    ##
    ## @code{@var{X} = convertTo (@var{T}, @var{dateType})} converts the datetime
    ## array @var{T} to the numeric date/time representation named by
    ## @var{dateType}, returning an array @var{X} of the same size as @var{T}.
    ## It is a convenience wrapper around the individual conversion methods.
    ## @var{dateType} may be one of:
    ##
    ## @itemize
    ## @item @qcode{'datenum'} -- serial date number, @code{double} (see
    ## @code{datenum}).
    ## @item @qcode{'excel'} or @qcode{'excel1900'} -- Excel serial date, 1900
    ## system, @code{double} (see @code{exceltime}).
    ## @item @qcode{'excel1904'} -- Excel serial date, 1904 system, @code{double}.
    ## @item @qcode{'juliandate'} -- Julian date, @code{double} (see
    ## @code{juliandate}).
    ## @item @qcode{'modifiedjuliandate'} -- modified Julian date, @code{double}.
    ## @item @qcode{'posixtime'} -- seconds since the Unix epoch, @code{double}
    ## (see @code{posixtime}).
    ## @item @qcode{'yyyymmdd'} -- @code{YYYYMMDD} numeric date, @code{double}
    ## (see @code{yyyymmdd}).
    ## @item @qcode{'epochtime'} -- ticks since an epoch, @code{int64} (see
    ## below).
    ## @item @qcode{'ntp'} -- NTP timestamp, @code{uint64}, valid from
    ## 1900-01-01 up to 2036-02-07.
    ## @item @qcode{'ntfs'} -- NTFS/@w{FILETIME} 100-ns ticks since 1601-01-01,
    ## @code{uint64}.
    ## @item @qcode{'.net'} -- @w{.NET} 100-ns ticks since 0001-01-01,
    ## @code{uint64}.
    ## @item @qcode{'tt2000'} -- CDF @w{TT2000} nanoseconds since the J2000
    ## Terrestrial Time epoch, @code{int64} (see below).
    ## @end itemize
    ##
    ## For the @code{double} conversions, Not-A-Time (@qcode{NaT}) values are
    ## returned as @qcode{NaN}.  The integer conversions cannot represent
    ## @qcode{NaN}, so a @qcode{NaT} value, an infinite datetime, or a datetime
    ## outside the target format's representable range raises an error.
    ##
    ## @code{@var{X} = convertTo (@var{T}, @qcode{'epochtime'}, @var{Name},
    ## @var{Value})} accepts the options @qcode{'Epoch'} (a scalar datetime
    ## marking tick zero; default @code{1970-01-01}) and @qcode{'TicksPerSecond'}
    ## (a positive scalar; default @code{1}).  The epoch and @var{T} must both be
    ## zoned or both be unzoned.
    ##
    ## @code{@var{X} = convertTo (@var{T}, @qcode{'tt2000'})} returns the number
    ## of nanoseconds since the J2000 Terrestrial Time epoch,
    ## @code{2000-01-01T11:58:55.816Z}, as an @code{int64} array.  Because that
    ## count includes leap seconds, @var{T} must be in the
    ## @qcode{'UTCLeapSeconds'} time zone.  The inverse is
    ## @code{datetime (@var{X}, @qcode{'ConvertFrom'}, @qcode{'tt2000'},
    ## @qcode{'TimeZone'}, @qcode{'UTCLeapSeconds'})}, which likewise requires
    ## both the @code{int64} type and that zone.
    ##
    ## Each conversion treats an inserted leap second the way its own format
    ## does.  @code{posixtime} folds it forward onto the following second, while
    ## @code{datenum}, @code{exceltime}, @qcode{'epochtime'}, @qcode{'ntp'},
    ## @qcode{'ntfs'} and @qcode{'.net'} fold it backward onto the preceding
    ## one; @code{juliandate} stretches the day that holds it (see
    ## @code{juliandate}), and @qcode{'tt2000'} counts it.
    ##
    ## @strong{Deviations from MATLAB.}  Two results differ deliberately for a
    ## @qcode{'UTCLeapSeconds'} array, because MATLAB's own are inconsistent.
    ##
    ## @itemize
    ## @item @qcode{'epochtime'} with an @qcode{'Epoch'}: MATLAB folds the array
    ## onto the POSIX timeline but measures the epoch on the leap-second one, so
    ## its tick count is short by the number of seconds inserted before that
    ## epoch.  Counting from @code{2016-12-31} it returns @code{86373} for
    ## @code{2016-12-31T23:59:59Z}, 26 short.  Both operands are folded the same
    ## way here, giving @code{86399}.  Only the offset differs: a difference
    ## between two such counts is the same in either.
    ## @item @qcode{'tt2000'} between 1960 and 1972: before 1972 UTC did not
    ## tick with atomic time, and its offset from it is tabulated by the IERS
    ## as a base value plus a rate per day.  That table is followed here; MATLAB
    ## evaluates the rate half a day from where the table places it, leaving its
    ## results up to @code{1.3} milliseconds off.  Outside those twelve years
    ## the two agree exactly.
    ## @end itemize
    ##
    ## @end deftypefn
    function out = convertTo (this, dateType, varargin)
      if (nargin < 2)
        error ("datetime.convertTo: not enough input arguments.");
      endif
      if (! (ischar (dateType) && isrow (dateType)))
        error ("datetime.convertTo: DATETYPE must be a character vector.");
      endif
      dt = tolower (dateType);

      ## 'epochtime' is the only type taking Name/Value options.
      if (strcmp (dt, 'epochtime'))
        [epochVal, ticks] = parsePairedArguments ({'Epoch', 'TicksPerSecond'}, ...
                                                  {[], 1}, varargin(:));
        if (! (isnumeric (ticks) && isscalar (ticks) && isreal (ticks) ...
               && ticks > 0))
          error (strcat ("datetime.convertTo: 'TicksPerSecond' must be a", ...
                         " positive scalar."));
        endif
        if (isempty (epochVal))
          epochMs = 0;
        else
          if (! (isa (epochVal, 'datetime') && isscalar (epochVal)))
            error ("datetime.convertTo: 'Epoch' must be a scalar datetime.");
          endif
          if (xor (isempty (this.TimeZone), isempty (epochVal.TimeZone)))
            error (strcat ("datetime.convertTo: the epoch and the input", ...
                           " datetime array must both have a time zone, or", ...
                           " must both be unzoned."));
          endif
          dtCheckLeapPair (this, epochVal, 'convertTo');
          epochMs = round (epochBase (epochVal) * 1000);
        endif
        ms = round (epochBase (this) * 1000);
        res = round ((ms - epochMs) ./ 1000 .* ticks);
        if (! all (isfinite (ms(:))) ...
            || any (abs (res(:)) > double (intmax ('int64'))))
          error (strcat ("datetime.convertTo: 'epochtime' conversion is not", ...
                         " supported for missing values, infinite datetimes,", ...
                         " or datetimes outside the int64 range for the given", ...
                         " epoch and TicksPerSecond."));
        endif
        out = reshape (int64 (res), size (this));
        return;
      endif

      if (! isempty (varargin))
        error ("datetime.convertTo: too many input arguments.");
      endif
      switch (dt)
        case 'datenum'
          out = datenum (this);
        case {'excel', 'excel1900'}
          out = exceltime (this, '1900');
        case 'excel1904'
          out = exceltime (this, '1904');
        case 'juliandate'
          out = juliandate (this, 'juliandate');
        case 'modifiedjuliandate'
          out = juliandate (this, 'modifiedjuliandate');
        case 'posixtime'
          out = posixtime (this);
        case 'yyyymmdd'
          out = yyyymmdd (this);
        case 'ntp'
          out = reshape (dtFixedEpoch (epochBase (this), 'ntp'), size (this));
        case 'ntfs'
          out = reshape (dtFixedEpoch (epochBase (this), 'ntfs'), size (this));
        case '.net'
          out = reshape (dtFixedEpoch (epochBase (this), 'dotnet'), ...
                         size (this));
        case 'tt2000'
          if (! dtIsLeapZone (this.TimeZone))
            error (strcat ("datetime.convertTo: to convert datetimes to", ...
                           " tt2000 times, the 'TimeZone' property of the", ...
                           " input must be 'UTCLeapSeconds'."));
          endif
          out = reshape (dtSerial2TT2000 (serial (this)), size (this));
        otherwise
          error ("datetime.convertTo: unrecognized conversion type '%s'.", ...
                 dateType);
      endswitch
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

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isdst (@var{T})
    ##
    ## Determine which datetime values fall in daylight saving time.
    ##
    ## @code{@var{TF} = isdst (@var{T})} returns a logical array @var{TF} of the
    ## same size as the input datetime array @var{T}, where each element is
    ## @qcode{true} if the corresponding datetime falls within daylight saving
    ## time in its time zone, and @qcode{false} otherwise.  If @var{T} has no
    ## time zone, or for Not-A-Time (@qcode{NaT}) values, the corresponding
    ## element of @var{TF} is @qcode{false}.
    ##
    ## @end deftypefn
    function TF = isdst (this)
      if (isempty (this.TimeZone))
        TF = false (size (this));
      else
        [Y, M, D, h, m, s] = dtOwnFoldClock (this.Year, this.Month, ...
                             this.Day, this.Hour, this.Minute, ...
                             this.Second, this.Offset, this.TimeZone);
        TF = dtIsDst (Y, M, D, h, m, s, this.TimeZone);
      endif
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{tf} =} isregular (@var{T})
    ## @deftypefnx {datetime} {@var{tf} =} isregular (@var{T}, @var{unit})
    ## @deftypefnx {datetime} {[@var{tf}, @var{dt}] =} isregular (@dots{})
    ##
    ## Determine whether a datetime vector is regularly spaced.
    ##
    ## @code{@var{tf} = isregular (@var{T})} returns @qcode{true} if the elements
    ## of the datetime vector @var{T} are equally spaced in time, and
    ## @qcode{false} otherwise.  A scalar or empty @var{T}, or one containing a
    ## Not-A-Time (@qcode{NaT}) value, is not regular.  Neither is a @var{T}
    ## that does not move: a step of zero describes no spacing, so a vector of
    ## repeated instants is not regularly spaced.
    ##
    ## @code{@var{tf} = isregular (@var{T}, @var{unit})} tests for regular
    ## spacing with respect to @var{unit}, which may be @qcode{'time'} (the
    ## default), @qcode{'years'}, @qcode{'quarters'}, @qcode{'months'},
    ## @qcode{'weeks'}, or @qcode{'days'}.  With a calendar unit, @var{T} is
    ## regular when successive elements differ by the same whole number of that
    ## unit, which -- unlike @qcode{'time'} -- accounts for varying month lengths
    ## and daylight saving time.
    ##
    ## @code{[@var{tf}, @var{dt}] = isregular (@dots{})} also returns the common
    ## time step @var{dt}.  For @qcode{'time'} it is a @code{duration}; for a
    ## calendar unit it is a @code{calendarDuration}.  When @var{T} is not
    ## regular, @var{dt} is @qcode{NaN}.
    ##
    ## Steps are compared as stored, to the microsecond this class keeps.  A
    ## spacing that differs by less than that cannot be seen and reads as
    ## regular, while one that rounds differently at the microsecond -- thirds
    ## of a second, say -- reads as irregular.  MATLAB stores instants more
    ## finely and so draws that line elsewhere.
    ##
    ## @end deftypefn
    function [TF, dt] = isregular (this, unit = 'time')
      units = {'time', 'years', 'quarters', 'months', 'weeks', 'days'};
      if (! (ischar (unit) && isrow (unit) && any (strcmpi (unit, units))))
        error (strcat ("datetime.isregular: UNIT must be 'time', 'years',", ...
                       " 'quarters', 'months', 'weeks', or 'days'."));
      endif
      unit = tolower (unit);
      istime = strcmp (unit, 'time');

      ## A scalar or empty array is never regular; anything else must be a
      ## vector (MATLAB rejects matrices).
      if (numel (this) < 2)
        TF = false;
        if (istime)
          dt = duration (0, 0, NaN);
        else
          dt = calmonths (NaN);
        endif
        return;
      elseif (! isvector (this))
        error ("datetime.isregular: input must be a datetime vector.");
      endif

      if (istime)
        ## Fixed-length regularity: every successive instant difference equal.
        d = diff (this);
        ds = seconds (d);
        ## A sequence that does not move has no spacing to be regular about, so
        ## a step of zero is not regular, as MATLAB reads it too.
        TF = isfinite (ds(1)) && ds(1) != 0 && all (ds(:) == ds(1));
        if (TF)
          dt = d(1);
        else
          dt = duration (0, 0, NaN);
        endif
      else
        ## Calendar regularity: the full calendar difference must be constant
        ## and consist purely of whole units of the requested kind.
        d = caldiff (this);
        moA = calmonths (d);
        dyA = caldays (d);
        tA = seconds (split (d, 'time'));
        allEqual = ! any (isnan ([moA(:); dyA(:); tA(:)])) ...
                   && all (moA(:) == moA(1)) && all (dyA(:) == dyA(1)) ...
                   && all (tA(:) == tA(1));
        mo = moA(1);  dy = dyA(1);  ti = tA(1);
        switch (unit)
          case 'years'
            pure = mod (mo, 12) == 0 && dy == 0 && ti == 0;
          case 'quarters'
            pure = mod (mo, 3) == 0 && dy == 0 && ti == 0;
          case 'months'
            pure = dy == 0 && ti == 0;
          case 'weeks'
            pure = mo == 0 && ti == 0 && mod (dy, 7) == 0;
          case 'days'
            pure = mo == 0 && ti == 0;
        endswitch
        ## As for time, a step of no calendar at all is not a spacing.
        TF = allEqual && pure && ! (mo == 0 && dy == 0 && ti == 0);
        if (TF)
          dt = d(1);
        else
          dt = calmonths (NaN);
        endif
      endif
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{TF} =} isweekend (@var{T})
    ##
    ## Determine which datetime values fall on a weekend.
    ##
    ## @code{@var{TF} = isweekend (@var{T})} returns a logical array @var{TF} of
    ## the same size as the input datetime array @var{T}, where each element is
    ## @qcode{true} if the corresponding datetime falls on a Saturday or Sunday,
    ## and @qcode{false} otherwise.  Not-A-Time (@qcode{NaT}) values return
    ## @qcode{false}.
    ##
    ## @end deftypefn
    function TF = isweekend (this)
      dow = day (this, 'dayofweek');
      TF = dow == 1 | dow == 7;
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{TF} =} isbetween (@var{X}, @var{lower}, @var{upper})
    ## @deftypefnx {datetime} {@var{TF} =} isbetween (@var{X}, @var{lower}, @var{upper}, @var{intervalType})
    ##
    ## Determine which datetime values lie within an interval.
    ##
    ## @code{@var{TF} = isbetween (@var{X}, @var{lower}, @var{upper})} returns a
    ## logical array @var{TF}, the same size as the broadcast of its inputs,
    ## containing @qcode{true} where the element of @var{X} lies between the
    ## corresponding @var{lower} and @var{upper} bounds.  @var{lower} and
    ## @var{upper} may each be a datetime array or a date/time character vector,
    ## string array, or cell array of character vectors, and either may be scalar
    ## to broadcast against @var{X}.  The comparison is made on the absolute
    ## instant, so zoned inputs may be in different time zones.  A Not-A-Time
    ## element in any input makes the corresponding result @qcode{false}.
    ##
    ## @code{@var{TF} = isbetween (@var{X}, @var{lower}, @var{upper},
    ## @var{intervalType})} selects which endpoints are included.
    ## @var{intervalType} is @qcode{'closed'} (the default,
    ## @w{@var{lower} @leq{} @var{X} @leq{} @var{upper}}), @qcode{'open'}
    ## (both endpoints excluded), @qcode{'openleft'} (exclude @var{lower}), or
    ## @qcode{'openright'} (exclude @var{upper}).
    ##
    ## @end deftypefn
    function TF = isbetween (X, varargin)
      if (numel (varargin) < 2)
        error ("datetime.isbetween: not enough input arguments.");
      endif
      if (numel (varargin) > 3)
        error ("datetime.isbetween: too many input arguments.");
      endif
      lo = varargin{1};
      hi = varargin{2};
      itype = 'closed';
      if (numel (varargin) == 3)
        itype = varargin{3};
        if (! (ischar (itype) && isrow (itype) && any (strcmpi (itype, ...
               {'closed', 'open', 'openleft', 'openright'}))))
          error (strcat ("datetime.isbetween: interval type must be", ...
                         " 'closed', 'open', 'openleft', or 'openright'."));
        endif
      endif
      if (isa (X, 'datetime'))
        ref = X;
      elseif (isa (lo, 'datetime'))
        ref = lo;
      else
        ref = hi;
      endif
      X  = dtIsbetweenArg (X,  ref);
      lo = dtIsbetweenArg (lo, ref);
      hi = dtIsbetweenArg (hi, ref);
      zoned = [! isempty(X.TimeZone), ! isempty(lo.TimeZone), ! isempty(hi.TimeZone)];
      if (any (zoned) && ! all (zoned))
        error (strcat ("datetime.isbetween: cannot combine a datetime array", ...
                       " with a time zone with one without a time zone."));
      endif
      dtCheckLeapPair (X, lo, 'isbetween');
      dtCheckLeapPair (X, hi, 'isbetween');
      sX = serial (X);  sL = serial (lo);  sU = serial (hi);
      switch (lower (itype))
        case 'closed'
          TF = sL <= sX & sX <= sU;
        case 'open'
          TF = sL < sX & sX < sU;
        case 'openleft'
          TF = sL < sX & sX <= sU;
        case 'openright'
          TF = sL <= sX & sX < sU;
      endswitch
    endfunction

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
## 'maxk'             'mink'             'sort'             'sortrows'        ##
## 'topkrows'         'unique'           'interp1'          'intersect'       ##
## 'setdiff'          'setxor'           'union'            'min'             ##
## 'max'                                                                      ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} maxk (@var{A}, @var{K})
    ## @deftypefnx {datetime} {@var{B} =} maxk (@var{A}, @var{K}, @var{dim})
    ## @deftypefnx {datetime} {[@var{B}, @var{index}] =} maxk (@var{A}, @dots{})
    ##
    ## Find the latest elements in a datetime array.
    ##
    ## @code{@var{B} = maxk (@var{A}, @var{K})} returns the @var{K} latest
    ## elements of the datetime array @var{A} in descending order.  If @var{A}
    ## is a vector, then @var{B} is a vector with @var{K} elements.  If @var{A}
    ## is a matrix, then @code{maxk} operates along each column of @var{A} and
    ## @var{B} has @var{K} rows.  For multidimensional arrays, @code{maxk}
    ## operates along the first non-singleton dimension.  Elements are ranked
    ## by the instant they name, so an array with a time zone is ranked by
    ## absolute time rather than by its wall clock.
    ##
    ## @code{@var{B} = maxk (@var{A}, @var{K}, @var{dim})} operates along the
    ## dimension specified by @var{dim}.
    ##
    ## @var{K} must be a nonnegative integer scalar.  If @var{K} is larger than
    ## the number of elements along the operating dimension, then all of them
    ## are returned.  Infinite datetimes are ranked as ordinary values.
    ##
    ## Missing elements (@qcode{NaT}) are not ranked.  They are appended after
    ## the ranked elements in their original order, and hence they only appear
    ## in @var{B} when @var{K} exceeds the number of non-missing elements along
    ## the operating dimension.  Unlike @code{sort}, @code{maxk} has no
    ## @qcode{'MissingPlacement'} option, since @qcode{NaT} elements are always
    ## placed last.  Elements comparing as equal keep their original order.
    ##
    ## @code{[@var{B}, @var{index}] = maxk (@var{A}, @dots{})} also returns an
    ## index array containing the indices of the returned elements of @var{A}
    ## along the operating dimension.
    ##
    ## @end deftypefn
    function [B, index] = maxk (A, K, varargin)
      if (nargin < 2)
        error ("datetime.maxk: too few input arguments.");
      endif
      [index, lidx, errmsg] = __minmaxk__ (serial (A), K, true, varargin);
      if (! isempty (errmsg))
        error ("datetime.maxk: %s", errmsg);
      endif
      B = subset (A, lidx);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{B} =} mink (@var{A}, @var{K})
    ## @deftypefnx {datetime} {@var{B} =} mink (@var{A}, @var{K}, @var{dim})
    ## @deftypefnx {datetime} {[@var{B}, @var{index}] =} mink (@var{A}, @dots{})
    ##
    ## Find the earliest elements in a datetime array.
    ##
    ## @code{@var{B} = mink (@var{A}, @var{K})} returns the @var{K} earliest
    ## elements of the datetime array @var{A} in ascending order.  If @var{A}
    ## is a vector, then @var{B} is a vector with @var{K} elements.  If @var{A}
    ## is a matrix, then @code{mink} operates along each column of @var{A} and
    ## @var{B} has @var{K} rows.  For multidimensional arrays, @code{mink}
    ## operates along the first non-singleton dimension.  Elements are ranked
    ## by the instant they name, so an array with a time zone is ranked by
    ## absolute time rather than by its wall clock.
    ##
    ## @code{@var{B} = mink (@var{A}, @var{K}, @var{dim})} operates along the
    ## dimension specified by @var{dim}.
    ##
    ## @var{K} must be a nonnegative integer scalar.  If @var{K} is larger than
    ## the number of elements along the operating dimension, then all of them
    ## are returned.  Infinite datetimes are ranked as ordinary values.
    ##
    ## Missing elements (@qcode{NaT}) are not ranked.  They are appended after
    ## the ranked elements in their original order, and hence they only appear
    ## in @var{B} when @var{K} exceeds the number of non-missing elements along
    ## the operating dimension.  Unlike @code{sort}, @code{mink} has no
    ## @qcode{'MissingPlacement'} option, since @qcode{NaT} elements are always
    ## placed last.  Elements comparing as equal keep their original order.
    ##
    ## @code{[@var{B}, @var{index}] = mink (@var{A}, @dots{})} also returns an
    ## index array containing the indices of the returned elements of @var{A}
    ## along the operating dimension.
    ##
    ## @end deftypefn
    function [B, index] = mink (A, K, varargin)
      if (nargin < 2)
        error ("datetime.mink: too few input arguments.");
      endif
      [index, lidx, errmsg] = __minmaxk__ (serial (A), K, false, varargin);
      if (! isempty (errmsg))
        error ("datetime.mink: %s", errmsg);
      endif
      B = subset (A, lidx);
    endfunction

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
      ## Sorting along a singleton dimension is a no-op.
      if (size (A, dim) < 2)
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
      ## Work on the numeric serial (NaT -> NaN).  Bring the sorted dimension to
      ## the front and flatten everything after it, so the column loop below
      ## holds for an array of any number of dimensions; the result is put back
      ## the way it came at the end.  Transposing instead would only ever have
      ## worked for a matrix.
      S = serial (A);
      Y = A.Year; MO = A.Month; D = A.Day;
      H = A.Hour; MI = A.Minute; SE = A.Second;
      nd = max (ndims (S), dim);
      sz = size (S);
      sz(end+1:nd) = 1;
      perm = [dim, 1:dim-1, dim+1:nd];
      psz = sz(perm);
      flat = @(V) reshape (permute (V, perm), psz(1), []);
      S = flat (S);
      Y = flat (Y); MO = flat (MO); D = flat (D);
      H = flat (H); MI = flat (MI); SE = flat (SE);
      OF = flat (A.Offset);
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
      ## Reorder the component arrays column-wise using linear indexing, then
      ## restore the original shape and dimension order.
      lin = idx + repmat ((0:nc-1) .* nr, nr, 1);
      back = @(V) ipermute (reshape (V(lin), psz), perm);
      Y = back (Y); MO = back (MO); D = back (D);
      H = back (H); MI = back (MI); SE = back (SE);
      OF = back (OF);
      I = ipermute (reshape (idx, psz), perm);
      B = A;
      B.Year = Y; B.Month = MO; B.Day = D;
      B.Hour = H; B.Minute = MI; B.Second = SE;
      B.Offset = OF;
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
    ## @deftypefn  {datetime} {@var{B} =} topkrows (@var{A}, @var{K})
    ## @deftypefnx {datetime} {@var{B} =} topkrows (@var{A}, @var{K}, @var{col})
    ## @deftypefnx {datetime} {@var{B} =} topkrows (@var{A}, @var{K}, @var{direction})
    ## @deftypefnx {datetime} {@var{B} =} topkrows (@var{A}, @var{K}, @var{col}, @var{direction})
    ## @deftypefnx {datetime} {@var{B} =} topkrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ## @deftypefnx {datetime} {[@var{B}, @var{index}] =} topkrows (@var{A}, @dots{})
    ##
    ## Top K sorted rows of a datetime array.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K})} returns the top @var{K} rows
    ## of the 2-D datetime array @var{A} sorted in descending order as a group.
    ## @var{K} must be a nonnegative integer scalar.  If @var{K} is larger than
    ## the number of rows in @var{A}, then all of them are returned.
    ##
    ## Missing elements (@qcode{NaT}) are not ranked.  Within each sort
    ## column they are placed after the elements that are defined, whichever
    ## direction is asked for, so a row is demoted only by a missing element in
    ## a column that actually decides its position.  Rows comparing as equal
    ## keep their original order.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K}, @var{col})} sorts using only
    ## the columns listed in the numeric vector @var{col}, which must contain
    ## positive integers indexing existing columns in @var{A}.  Columns are used
    ## as sort keys in the order given, and those not listed are not used at
    ## all.  The direction is descending unless @var{direction} says otherwise.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K}, @var{direction})} sorts in
    ## the given @var{direction}, either @qcode{'descend'} (default) or
    ## @qcode{'ascend'} applying to all columns in @var{A}.  Alternatively,
    ## @var{direction} can be a cell array of character vectors specifying the
    ## sorting direction for each individual column of @var{A}.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K}, @var{col}, @var{direction})}
    ## combines an explicit column list with a per-column @var{direction}.
    ##
    ## @code{@var{B} = topkrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where the missing elements are placed within each sort column,
    ## with any of the following options specified in @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'last'}, which is the default, places missing elements last
    ## whichever direction is asked for.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'auto'} places missing elements last for an ascending sort
    ## and first for a descending one, as @code{sortrows} does.
    ## @end itemize
    ##
    ## This is an Octave extension: MATLAB has no such option here and always
    ## ranks as @qcode{'last'} does.
    ##
    ## @code{[@var{B}, @var{index}] = topkrows (@var{A}, @dots{})} also returns
    ## an index vector containing the original row indices of @var{A} in
    ## @var{B}, such that @code{@var{B} = @var{A}(@var{index},:)}.
    ##
    ## @end deftypefn
    function [B, index] = topkrows (A, K, varargin)
      ## Check input arguments
      if (nargin < 2)
        error ("datetime.topkrows: too few input arguments.");
      endif
      if (ndims (A) != 2)
        error ("datetime.topkrows: A must be a 2-D array.");
      endif
      [col, direction, MP, errmsg] = ...
                      __topkrowsargs__ (K, columns (A), varargin);
      if (! isempty (errmsg))
        error ("datetime.topkrows: %s", errmsg);
      endif
      ## Sort rows, keeping missing elements out of the ranking
      if (isempty (col))
        [B, index] = sortrows (A, direction, 'MissingPlacement', MP);
      else
        [B, index] = sortrows (A, col, direction, 'MissingPlacement', MP);
      endif
      ## Return top K rows
      if (K < numel (index))
        B = subset (B, 1:K, ':');
        index = index(1:K);
      endif
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
      ## The offset joins the components as a seventh key, negated so that the
      ## larger offset -- the earlier of the two moments a repeated wall clock
      ## names -- codes lower and the row order stays chronological.  It is a
      ## constant column for an unzoned array and for any array that does not
      ## straddle a fall-back, so it changes nothing outside that case.
      [~, ~, OFidx] = __unique__ (-A.Offset, varargin{:});
      DT = [Yidx, MOidx, Didx, Hidx, MIidx, Sidx, OFidx];
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

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{VI} =} interp1 (@var{X}, @var{V}, @var{XI})
    ## @deftypefnx {datetime} {@var{VI} =} interp1 (@dots{}, @var{method})
    ## @deftypefnx {datetime} {@var{VI} =} interp1 (@dots{}, @var{method}, @var{extrap})
    ##
    ## One-dimensional interpolation involving datetime arrays.
    ##
    ## @code{@var{VI} = interp1 (@var{X}, @var{V}, @var{XI})} interpolates to find
    ## @var{VI}, the values of the underlying function @code{@var{V} = f
    ## (@var{X})} at the query points @var{XI}.  Datetime arrays are handled by
    ## interpolating on their absolute (POSIX) instants:
    ##
    ## @itemize
    ## @item When the sample points @var{X} are a datetime array, the query
    ## points @var{XI} must also be a datetime array, and vice versa.
    ## @item When the sampled values @var{V} are a datetime array, @var{VI} is a
    ## datetime array carrying the time zone and display format of @var{V};
    ## otherwise @var{VI} is numeric.
    ## @end itemize
    ##
    ## The optional @var{method} (@qcode{'linear'} by default) and @var{extrap}
    ## arguments are passed to the built-in @code{interp1} (see its documentation
    ## for the supported interpolation methods and extrapolation options).  Query
    ## points outside the range of @var{X}, and Not-A-Time query points, yield
    ## @qcode{NaN} or @qcode{NaT} unless extrapolation is requested.
    ##
    ## @end deftypefn
    function BI = interp1 (A, B, AI, varargin)
      vIsDT = isa (B, 'datetime');
      if (isa (A, 'datetime'))
        if (! isa (AI, 'datetime'))
          error (strcat ("datetime.interp1: query points must be a datetime", ...
                         " array when the sample points are datetime."));
        endif
        x = serial (A);
        xq = serial (AI);
      else
        if (isa (AI, 'datetime'))
          error (strcat ("datetime.interp1: query points must be numeric when", ...
                         " the sample points are numeric."));
        endif
        x = A;
        xq = AI;
      endif
      if (vIsDT)
        v = serial (B);
      else
        v = B;
      endif
      vi = interp1 (x, v, xq, varargin{:});
      if (vIsDT)
        BI = fromReducedSerial (B, vi);
      else
        BI = vi;
      endif
    endfunction

  endmethods

################################################################################
##                        ** Descriptive Statistics **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'mean'             'median'           'mode'             'std'             ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{M} =} mean (@var{A})
    ## @deftypefnx {datetime} {@var{M} =} mean (@var{A}, @var{dim})
    ## @deftypefnx {datetime} {@var{M} =} mean (@var{A}, @qcode{'all'})
    ## @deftypefnx {datetime} {@var{M} =} mean (@dots{}, @var{nanflag})
    ##
    ## Mean of a datetime array.
    ##
    ## @code{@var{M} = mean (@var{A})} returns the mean of the datetime array
    ## @var{A} as a scalar datetime, computed as the average of the absolute
    ## instants along the first non-singleton dimension.  A @var{dim} or
    ## @qcode{'all'} argument selects the dimension(s) to operate on.  The result
    ## carries the @code{Format} and @code{TimeZone} of @var{A}.
    ##
    ## By default a Not-A-Time element makes the corresponding result
    ## @qcode{NaT}; pass @qcode{'omitnat'} (equivalently @qcode{'omitmissing'})
    ## to ignore missing values, or @qcode{'includenat'} to keep the default.
    ##
    ## @end deftypefn
    function R = mean (A, varargin)
      args = dtStatFlags (varargin);
      R = fromReducedSerial (A, mean (serial (A), args{:}));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{M} =} median (@var{A})
    ## @deftypefnx {datetime} {@var{M} =} median (@var{A}, @var{dim})
    ## @deftypefnx {datetime} {@var{M} =} median (@var{A}, @qcode{'all'})
    ## @deftypefnx {datetime} {@var{M} =} median (@dots{}, @var{nanflag})
    ##
    ## Median of a datetime array.
    ##
    ## @code{@var{M} = median (@var{A})} returns the median of the datetime array
    ## @var{A} as a datetime, computed on the absolute instants along the first
    ## non-singleton dimension (for an even number of elements the average of the
    ## two middle instants).  A @var{dim} or @qcode{'all'} argument selects the
    ## dimension(s).  The result carries the @code{Format} and @code{TimeZone} of
    ## @var{A}.  Missing-value handling matches @code{mean}.
    ##
    ## @end deftypefn
    function R = median (A, varargin)
      args = dtStatFlags (varargin);
      R = fromReducedSerial (A, median (serial (A), args{:}));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{M} =} mode (@var{A})
    ## @deftypefnx {datetime} {@var{M} =} mode (@var{A}, @var{dim})
    ## @deftypefnx {datetime} {@var{M} =} mode (@var{A}, @qcode{'all'})
    ## @deftypefnx {datetime} {[@var{M}, @var{F}, @var{C}] =} mode (@dots{})
    ##
    ## Most frequent value of a datetime array.
    ##
    ## @code{@var{M} = mode (@var{A})} returns the most frequently occurring
    ## datetime in @var{A} along the first non-singleton dimension; when several
    ## values are equally frequent the smallest is returned.  A @var{dim} or
    ## @qcode{'all'} argument selects the dimension(s).  Not-A-Time elements are
    ## ignored.  The result carries the @code{Format} and @code{TimeZone} of
    ## @var{A}.
    ##
    ## @code{[@var{M}, @var{F}, @var{C}] = mode (@dots{})} also returns the
    ## frequency @var{F} of the modal value and a cell array @var{C} whose
    ## elements list all values that achieve that frequency.
    ##
    ## @end deftypefn
    function [R, F, C] = mode (A, varargin)
      if (nargout > 2)
        [ser, F, C] = mode (serial (A), varargin{:});
        C = cellfun (@(x) fromReducedSerial (A, x), C, 'UniformOutput', false);
      elseif (nargout == 2)
        [ser, F] = mode (serial (A), varargin{:});
      else
        ser = mode (serial (A), varargin{:});
      endif
      R = fromReducedSerial (A, ser);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{S} =} std (@var{A})
    ## @deftypefnx {datetime} {@var{S} =} std (@var{A}, @var{w})
    ## @deftypefnx {datetime} {@var{S} =} std (@var{A}, @var{w}, @var{dim})
    ## @deftypefnx {datetime} {@var{S} =} std (@var{A}, @var{w}, @qcode{'all'})
    ## @deftypefnx {datetime} {@var{S} =} std (@dots{}, @var{nanflag})
    ## @deftypefnx {datetime} {[@var{S}, @var{M}] =} std (@dots{})
    ##
    ## Standard deviation of a datetime array.
    ##
    ## @code{@var{S} = std (@var{A})} returns the standard deviation of the
    ## absolute instants of @var{A} as a @code{duration}.  The weight @var{w}
    ## selects the normalisation (@code{0}, the default, divides by @math{N-1};
    ## @code{1} divides by @math{N}), and a @var{dim} or @qcode{'all'} argument
    ## selects the dimension(s).  Missing-value handling matches @code{mean}.
    ##
    ## @code{[@var{S}, @var{M}] = std (@dots{})} also returns the mean @var{M} as
    ## a datetime.
    ##
    ## @end deftypefn
    function [S, M] = std (A, varargin)
      args = dtStatFlags (varargin);
      if (nargout > 1)
        [sSec, mSec] = std (serial (A), args{:});
        M = fromReducedSerial (A, mSec);
      else
        sSec = std (serial (A), args{:});
      endif
      S = duration (0, 0, sSec);
    endfunction

  endmethods

################################################################################
##                              ** Binning **                                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'discretize'       'histcounts'                                            ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{bin} =} discretize (@var{T}, @var{edges})
    ## @deftypefnx {datetime} {@var{bin} =} discretize (@var{T}, @var{N})
    ## @deftypefnx {datetime} {@var{bin} =} discretize (@var{T}, @var{dur})
    ## @deftypefnx {datetime} {@var{bin} =} discretize (@var{T}, @var{unit})
    ## @deftypefnx {datetime} {@var{Y} =} discretize (@dots{}, @var{values})
    ## @deftypefnx {datetime} {@var{C} =} discretize (@dots{}, 'categorical')
    ## @deftypefnx {datetime} {@var{C} =} discretize (@dots{}, 'categorical', @var{names})
    ## @deftypefnx {datetime} {[@var{bin}, @var{E}] =} discretize (@dots{})
    ##
    ## Group datetimes into bins.
    ##
    ## @code{@var{bin} = discretize (@var{T}, @var{edges})} returns, for each
    ## element of @var{T}, the index of the bin of the @qcode{datetime} vector
    ## @var{edges} that contains it.  Bins are half open,
    ## @code{[@var{E}(j), @var{E}(j+1))}, except the last which is closed at
    ## both ends.  Elements outside the edges, and @qcode{NaT} elements, give
    ## @qcode{NaN}.
    ##
    ## @code{@var{bin} = discretize (@var{T}, @var{N})} uses @var{N} bins
    ## spanning the data, placed on whole calendar or clock units wherever that
    ## can be done without leaving a bin unused.
    ##
    ## @code{@var{bin} = discretize (@var{T}, @var{dur})} uses bins one
    ## @var{dur} wide, where @var{dur} is a scalar @qcode{duration} or
    ## @qcode{calendarDuration}, aligned to whole multiples of that width.
    ##
    ## @code{@var{bin} = discretize (@var{T}, @var{unit})} uses bins one named
    ## unit wide, @var{unit} being one of @qcode{'second'}, @qcode{'minute'},
    ## @qcode{'hour'}, @qcode{'day'}, @qcode{'week'}, @qcode{'month'},
    ## @qcode{'quarter'}, @qcode{'year'}, @qcode{'decade'} or
    ## @qcode{'century'}.  These land on real calendar boundaries: a
    ## @qcode{'week'} bin starts on a Sunday, a @qcode{'quarter'} on 1 January,
    ## 1 April, 1 July or 1 October, and a @qcode{'decade'} on a year that is a
    ## multiple of ten.
    ##
    ## @code{@var{Y} = discretize (@dots{}, @var{values})} returns
    ## @code{@var{values}(@var{bin})} instead of the bin index, and
    ## @code{@var{C} = discretize (@dots{}, 'categorical')} returns a
    ## @qcode{categorical} array whose categories are named after the bins.
    ##
    ## @code{[@var{bin}, @var{E}] = discretize (@dots{})} also returns the bin
    ## edges as a @qcode{datetime} array carrying this array's @qcode{Format}
    ## and @qcode{TimeZone}.
    ##
    ## Note that whether a bin follows the calendar depends on how its width
    ## is given, not on how long that width is.  A named unit (@qcode{'day'}
    ## and coarser) or a @qcode{calendarDuration} width begins at
    ## @strong{local midnight}, so in a time zone that observes daylight
    ## saving the bin holding a transition is 23 or 25 hours long while its
    ## neighbours are 24.  A @qcode{duration} width is a fixed span of elapsed
    ## time whatever its length: @code{days (1)} bins are each exactly 24
    ## hours, and their edges therefore read an hour later on the far side of
    ## a transition.  The two agree for an unzoned array, and for a zoned one
    ## that spans no transition.
    ##
    ## When @var{T} is empty the edges are anchored on the epoch,
    ## @qcode{1970-01-01}.  This is @strong{deliberately unlike MATLAB}, which
    ## answers an empty @qcode{datetime} with edges taken from the current
    ## clock, so that the same call returns a different result every time it is
    ## run.
    ##
    ## @end deftypefn
    function [BIN, EDGES] = discretize (this, arg2, varargin)

      if (nargin < 2)
        error ("datetime.discretize: not enough input arguments.");
      endif
      xv = serial (this)(:);
      [s2c, c2s, d2s] = dtCalHandles (this);
      isCount = false;
      if (isa (arg2, 'datetime'))
        ev = d2s (arg2)(:).';
      else
        [ev, isCount] = dtBinEdges (xv, arg2, s2c, c2s, ...
                                    'datetime.discretize');
      endif
      EDGES = fromReducedSerial (this, ev);
      if (isCount)
        EDGES = reshape (EDGES, 1, numel (ev));
      endif

      ## Bins asked for as a 'categorical' are named after the edges as they
      ## DISPLAY, not after their serial values, so the names are built here
      ## and handed to the numeric function rather than left to it.  The last
      ## bin is closed at both ends but is labelled half open, as MATLAB
      ## labels it.
      ic = find (cellfun (@(a) dtIsTextScalar (a) ...
                          && strcmpi (char (a), 'categorical'), varargin), 1);
      if (! isempty (ic))
        named = numel (varargin) > ic ...
                && ! (dtIsTextScalar (varargin{ic+1}) ...
                      && strcmpi (char (varargin{ic+1}), 'IncludedEdge'));
        if (! named)
          strs = dispstrings (EDGES);
          nm = cell (1, numel (ev) - 1);
          for j = 1:numel (nm)
            nm{j} = sprintf ("[%s, %s)", strs{j}, strs{j+1});
          endfor
          varargin = [varargin(1:ic), {nm}, varargin(ic+1:end)];
        endif
      endif

      ## Delegate the assignment and the value mapping to the numeric function
      [BIN, ~] = discretize (reshape (xv, size (this.Year)), ev, varargin{:});

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{N} =} histcounts (@var{T})
    ## @deftypefnx {datetime} {@var{N} =} histcounts (@var{T}, @var{nbins})
    ## @deftypefnx {datetime} {@var{N} =} histcounts (@var{T}, @var{edges})
    ## @deftypefnx {datetime} {@var{N} =} histcounts (@dots{}, @var{Name}, @var{Value})
    ## @deftypefnx {datetime} {[@var{N}, @var{edges}] =} histcounts (@dots{})
    ## @deftypefnx {datetime} {[@var{N}, @var{edges}, @var{bin}] =} histcounts (@dots{})
    ##
    ## Histogram bin counts for datetimes.
    ##
    ## @code{@var{N} = histcounts (@var{T})} bins the datetimes in @var{T},
    ## chosen automatically, and returns the number of elements in each bin.
    ## @var{T} is treated as @code{@var{T}(:)} and @qcode{NaT} elements are
    ## excluded.
    ##
    ## @code{@var{N} = histcounts (@var{T}, @var{nbins})} and
    ## @code{@var{N} = histcounts (@var{T}, @var{edges})} bin by count and by
    ## explicit @qcode{datetime} edges respectively.
    ##
    ## @qcode{'BinWidth'} takes a scalar @qcode{duration} or
    ## @qcode{calendarDuration} and, @strong{as an Octave extension}, any of
    ## the unit names listed for @code{discretize}; MATLAB accepts only the
    ## two classes here and requires @qcode{'BinMethod'} for a named unit.
    ## The two spellings give identical results, so
    ## @code{histcounts (@var{T}, 'BinWidth', 'day')} is simply another way of
    ## writing @code{histcounts (@var{T}, 'BinMethod', 'day')} -- and note
    ## that neither is the same as @code{'BinWidth', days (1)}, for the reason
    ## given below.  @qcode{'BinLimits'} takes a two-element @qcode{datetime},
    ## and @qcode{'BinEdges'} a @qcode{datetime} vector.
    ## @qcode{'BinMethod'} accepts @qcode{'auto'}, @qcode{'scott'},
    ## @qcode{'fd'}, @qcode{'sturges'} and @qcode{'sqrt'}, and any of the named
    ## calendar units listed for @code{discretize}, but not
    ## @qcode{'integers'}, which has no meaning for a datetime.
    ##
    ## @qcode{'Normalization'} accepts @qcode{'count'}, @qcode{'cumcount'},
    ## @qcode{'probability'}, @qcode{'percentage'} and @qcode{'cdf'}.
    ## @qcode{'countdensity'} and @qcode{'pdf'} are not accepted, since a
    ## density per unit time has no meaning here.
    ##
    ## As for @code{discretize}, a named unit or a @qcode{calendarDuration}
    ## width begins at local midnight and so varies in length across a
    ## daylight-saving transition, while a @qcode{duration} width is a fixed
    ## span of elapsed time whatever its length, and an empty @var{T} anchors
    ## the edges on the epoch rather than on the current clock as MATLAB does.
    ##
    ## @seealso{discretize, histcounts}
    ## @end deftypefn
    function [N, EDGES, BIN] = histcounts (this, varargin)

      xv = serial (this)(:);
      [s2c, c2s, d2s] = dtCalHandles (this);
      args = dtHistArgs (varargin, xv, s2c, c2s, d2s, 'datetime.histcounts');
      if (nargout > 2)
        [N, ev, BIN] = histcounts (xv, args{:});
        BIN = reshape (BIN, size (this.Year));
      else
        [N, ev] = histcounts (xv, args{:});
      endif
      EDGES = fromReducedSerial (this, ev);

    endfunction

  endmethods

################################################################################
##                         ** Arithmetic Operations **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'plus'             'minus'            'colon'            'linspace'        ##
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
      dtCheckLeapPair (A, B, 'colon');
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
    ## @deftypefn  {datetime} {@var{R} =} linspace (@var{A}, @var{B})
    ## @deftypefnx {datetime} {@var{R} =} linspace (@var{A}, @var{B}, @var{N})
    ##
    ## Create linearly spaced datetime values.
    ##
    ## @code{@var{R} = linspace (@var{A}, @var{B})} returns a row vector of 100
    ## datetime values spaced equally between the scalar datetimes @var{A} and
    ## @var{B}, both of which are included.
    ##
    ## @code{@var{R} = linspace (@var{A}, @var{B}, @var{N})} returns @var{N}
    ## values instead of 100.  A non-integer @var{N} is floored; if @var{N} is
    ## one, @var{B} alone is returned, and if @var{N} is zero or negative the
    ## result is empty.
    ##
    ## The spacing is by absolute instant, so for zoned inputs it is daylight
    ## saving aware: a span that loses an hour to a transition is divided into
    ## equal instants, whose wall-clock readings are therefore not equally
    ## spaced.  The two endpoints may be in different time zones, in which case
    ## the result is expressed in the zone of @var{A}, but they must either both
    ## be zoned or both be unzoned.  @var{R} carries the @code{Format} and
    ## @code{TimeZone} of @var{A}.
    ##
    ## A Not-A-Time endpoint makes every value that depends on it @code{NaT},
    ## leaving only the opposite endpoint; an infinite endpoint likewise carries
    ## its infinity through the values that depend on it.
    ##
    ## @end deftypefn
    function R = linspace (A, B, n = 100)
      if (nargin < 2)
        error ("datetime.linspace: too few input arguments.");
      endif
      if (! (isa (A, 'datetime') && isa (B, 'datetime')))
        error ("datetime.linspace: both endpoints must be datetime arrays.");
      endif
      if (! (isscalar (A) && isscalar (B)))
        error ("datetime.linspace: endpoints must be datetime scalars.");
      endif
      if (xor (isempty (A.TimeZone), isempty (B.TimeZone)))
        error (strcat ("datetime.linspace: cannot combine a datetime array", ...
                       " with a time zone with one without a time zone."));
      endif
      dtCheckLeapPair (A, B, 'linspace');
      if (! (isnumeric (n) && isscalar (n) && isreal (n)))
        error ("datetime.linspace: N must be a real numeric scalar.");
      endif
      R = A;
      sA = serial (A);
      sB = serial (B);
      ## Step from the first instant rather than calling the numeric linspace,
      ## which spaces a range differently once an endpoint is not finite: MATLAB
      ## takes each interior point as the first instant plus so many steps, and
      ## keeps both endpoints exactly.  With the step itself infinite or
      ## Not-A-Number, plain IEEE arithmetic then gives every case MATLAB
      ## does -- an interior point infinitely far from a finite start is that
      ## infinity, and one between two infinities, or reached by stepping
      ## backwards from an infinite start, is Not-A-Time.
      n = floor (double (n));
      if (n < 1)
        ser = zeros (1, 0);
      elseif (n == 1)
        ser = sB;
      else
        ser = sA + (0:n-1) * ((sB - sA) / (n - 1));
        ser(1) = sA;
        ser(end) = sB;
      endif
      [Y, M, D, h, mi, s, off] = serial2components (A, ser);
      R.Year = Y; R.Month = M; R.Day = D;
      R.Hour = h; R.Minute = mi; R.Second = s;
      R.Offset = off;
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
        dtCheckLeapPair (A, B, 'minus');
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

    ## -*- texinfo -*-
    ## @deftypefn  {datetime} {@var{R} =} dateshift (@var{A}, @qcode{'start'}, @var{unit})
    ## @deftypefnx {datetime} {@var{R} =} dateshift (@var{A}, @qcode{'end'}, @var{unit})
    ## @deftypefnx {datetime} {@var{R} =} dateshift (@dots{}, @var{rule})
    ## @deftypefnx {datetime} {@var{R} =} dateshift (@var{A}, @qcode{'dayofweek'}, @var{dow})
    ## @deftypefnx {datetime} {@var{R} =} dateshift (@var{A}, @qcode{'dayofweek'}, @var{dow}, @var{rule})
    ##
    ## Shift datetime values to calendar boundaries.
    ##
    ## @code{@var{R} = dateshift (@var{A}, @qcode{'start'}, @var{unit})} returns a
    ## datetime array in which each element of @var{A} is moved back to the start
    ## of the calendar @var{unit} that contains it, with the finer components set
    ## to zero.  @var{unit} is @qcode{'second'}, @qcode{'minute'},
    ## @qcode{'hour'}, @qcode{'day'}, @qcode{'week'}, @qcode{'month'},
    ## @qcode{'quarter'}, or @qcode{'year'}.  A week starts on Sunday.
    ##
    ## @code{@var{R} = dateshift (@var{A}, @qcode{'end'}, @var{unit})} moves each
    ## element to the end of its unit: the start of the next second, minute,
    ## hour, or day, and the last day (at midnight) of the week, month, quarter,
    ## or year.
    ##
    ## @code{@var{R} = dateshift (@dots{}, @var{rule})} first shifts each element
    ## by @var{rule} whole units.  @var{rule} is @qcode{'current'} (the default),
    ## @qcode{'next'}, @qcode{'previous'}, @qcode{'nearest'}, or an integer
    ## number of units.
    ##
    ## @code{@var{R} = dateshift (@var{A}, @qcode{'dayofweek'}, @var{dow})} moves
    ## each element to the next date, on or after it, whose day of the week is
    ## @var{dow} (a number from 1 for Sunday to 7 for Saturday, or a day name),
    ## keeping the time of day.  A trailing @var{rule} of @qcode{'previous'},
    ## @qcode{'nearest'}, @qcode{'current'} (the day within the current week), or
    ## an integer occurrence selects a different date.
    ##
    ## Not-A-Time and infinite elements are returned unchanged.
    ##
    ## @end deftypefn
    function R = dateshift (this, op, varargin)
      ops = {'start', 'end', 'dayofweek'};
      if (! (ischar (op) && isrow (op) && any (strcmpi (op, ops))))
        error (strcat ("datetime.dateshift: second input must be 'start',", ...
                       " 'end', or 'dayofweek'."));
      endif
      op = lower (op);
      if (numel (varargin) < 1)
        error ("datetime.dateshift: not enough input arguments.");
      endif
      if (numel (varargin) > 2)
        error ("datetime.dateshift: too many input arguments.");
      endif
      Y = this.Year; M = this.Month; D = this.Day;
      h = this.Hour; mi = this.Minute; s = this.Second;
      elapsed = 0;
      keep = isnan (Y) | isinf (Y);
      if (any (keep(:)))
        Y(keep) = 2000; M(keep) = 1; D(keep) = 1;
        h(keep) = 0; mi(keep) = 0; s(keep) = 0;
      endif

      if (strcmp (op, 'dayofweek'))
        dow = varargin{1};
        if (ischar (dow) && isrow (dow))
          dow = dsDayName (dow);
        elseif (! (isnumeric (dow) && isscalar (dow)))
          dow = NaN;
        endif
        if (! (isscalar (dow) && dow >= 1 && dow <= 7 && dow == fix (dow)))
          error (strcat ("datetime.dateshift: day of week must be a number", ...
                         " from 1 to 7 or a day name."));
        endif
        kind = 'next';  n = 0;
        if (numel (varargin) == 2)
          [kind, n] = dsRule (varargin{2});
        endif
        dowT = weekday (datenum (Y, M, D));
        dNext = mod (dow - dowT, 7);
        dPrev = mod (dowT - dow, 7);
        switch (kind)
          case 'next'
            delta = dNext;
          case 'previous'
            delta = -dPrev;
          case 'current'
            delta = dow - dowT;
          case 'nearest'
            delta = dNext;
            useprev = dPrev < dNext;
            delta(useprev) = -dPrev(useprev);
          case 'int'
            if (n >= 1)
              delta = dNext + (n - 1) * 7;
            elseif (n <= -1)
              delta = -(dPrev + (- n - 1) * 7);
            else
              delta = dow - dowT;
            endif
        endswitch
        [Y, M, D] = dtAddDays (Y, M, D, delta);
      else
        unit = varargin{1};
        units = {'second', 'minute', 'hour', 'day', 'week', 'month', ...
                 'quarter', 'year'};
        if (! (ischar (unit) && isrow (unit) && any (strcmpi (unit, units))))
          error (strcat ("datetime.dateshift: unit must be 'second',", ...
                         " 'minute', 'hour', 'day', 'week', 'month',", ...
                         " 'quarter', or 'year'."));
        endif
        unit = lower (unit);
        n = 0;
        if (numel (varargin) == 2)
          [kind, nint] = dsRule (varargin{2});
          switch (kind)
            case 'current'
              n = 0;
            case 'next'
              n = 1;
            case 'previous'
              n = -1;
            case 'int'
              n = nint;
            case 'nearest'
              [cY, cM, cD, ch, cm, cs] = dsStartComp (Y, M, D, h, mi, s, unit);
              sc = dsSerialOf (this, cY, cM, cD, ch, cm, cs);
              [nY, nM, nD, nh, nm, ns] = ...
                  dsShiftUnits (cY, cM, cD, ch, cm, cs, unit, 1);
              sn = dsSerialOf (this, nY, nM, nD, nh, nm, ns);
              st = serial (this);
              st(keep) = sc(keep);
              n = double ((st - sc) >= (sn - st));
          endswitch
        endif
        [Y, M, D, h, mi, s] = dsShiftUnits (Y, M, D, h, mi, s, unit, n);
        ## The end of a sub-day unit is its start advanced by one unit of
        ## ELAPSED time, which is not the wall clock one unit later: on the day
        ## a clock goes back, the hour beginning at 01:00 daylight time ends at
        ## 01:00 standard time -- a moment the component arithmetic cannot
        ## name, since it reads 02:00 on a clock that never shows it.  Taking
        ## the start and stepping the instant names it and leaves every other
        ## case alone, the two agreeing wherever no clock is put back.  The
        ## calendar units keep the component form: their 'end' is a date at
        ## midnight rather than the following boundary, so it is not a step.
        ## The leap-second zone keeps it too, for the opposite reason: it has
        ## no daylight saving to straddle, and a minute there may hold
        ## sixty-one seconds, so a step of sixty would stop inside it.
        if (strcmp (op, 'start'))
          [Y, M, D, h, mi, s] = dsStartComp (Y, M, D, h, mi, s, unit);
        elseif (any (strcmp (unit, {'second', 'minute', 'hour'})) ...
                && ! dtIsLeapZone (this.TimeZone))
          [Y, M, D, h, mi, s] = dsStartComp (Y, M, D, h, mi, s, unit);
          switch (unit)
            case 'second'
              elapsed = 1;
            case 'minute'
              elapsed = 60;
            case 'hour'
              elapsed = 3600;
          endswitch
        else
          [Y, M, D, h, mi, s] = dsEndComp (Y, M, D, h, mi, s, unit);
        endif
      endif

      R = this;
      R.Year = Y; R.Month = M; R.Day = D;
      R.Hour = h; R.Minute = mi; R.Second = s;
      R.Offset = dtOffsetOf (R.Year, R.Month, R.Day, R.Hour, R.Minute, ...
                             R.Second, R.TimeZone);
      R = normalize (R);
      ## Shifting within the day does not move an element out of the fold it is
      ## in: the start of the hour containing 01:30 daylight time is 01:00
      ## daylight time, not the 01:00 that the clock shows again an hour later.
      srcOff = this.Offset + zeros (size (R.Year));
      R = keepFold (R, srcOff);
      if (elapsed != 0)
        R = addSeconds (R, elapsed);
      endif
      if (any (keep(:)))
        R.Year(keep) = this.Year(keep); R.Month(keep) = this.Month(keep);
        R.Day(keep) = this.Day(keep); R.Hour(keep) = this.Hour(keep);
        R.Minute(keep) = this.Minute(keep); R.Second(keep) = this.Second(keep);
        R.Offset(keep) = srcOff(keep);
      endif
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
    ## the inputs @var{A}, @var{B}, @dots{} along dimension @var{dim}.  They
    ## must have the same size except along the operating dimension @var{dim}.
    ##
    ## At least one input must be a datetime array.  Date/time text, whether a
    ## character vector, a cellstr, or a @code{string} array, is converted; a
    ## @math{0*0} empty @code{[]} or @code{@{@}} contributes nothing and is
    ## dropped, so an array may be accumulated from an empty start.  Any other
    ## input, an empty numeric array of non-zero size included, is an error.
    ##
    ## The first datetime input gives the result its @code{Format} and
    ## @code{TimeZone}, and is what date/time text is read against: such text
    ## names a wall clock in that time zone rather than an instant to be
    ## converted into it.  Text may therefore appear first, as in
    ## @code{[@qcode{'2024-01-01'}, @var{T}]}, and still be read in @var{T}'s
    ## zone.
    ##
    ## Zoned inputs need not share a time zone; each is converted into the zone
    ## of the first, which preserves the instant every element names and changes
    ## only its wall-clock reading.  A zoned input cannot be concatenated with
    ## an unzoned one, in either order: a wall clock that names no instant and
    ## one that does do not belong in the same array.  A bare @code{NaT} and
    ## @code{datetime.empty} are unzoned and so take part in that rule, whereas
    ## @code{NaT (@qcode{'TimeZone'}, @var{tz})} does not.
    ##
    ## Date/time text that is empty names no date and becomes @code{NaT}, so an
    ## empty character vector adds one missing element, whereas an empty
    ## @code{@{@}} holds no text at all and adds none.
    ##
    ## @end deftypefn
    function out = cat (dim, varargin)
      args = varargin;
      isdt = cellfun (@(x) isa (x, 'datetime'), args);
      ## An operand that is 0-by-0 and not a datetime -- the [] or {} an array
      ## is often accumulated from -- contributes nothing and is dropped.  An
      ## empty datetime is kept, since whether it carries a time zone still
      ## counts towards the test below, and so is an empty character vector,
      ## which is one piece of text naming no date and becomes a NaT.
      drop = ! isdt & cellfun (@(x) isequal (size (x), [0, 0]) ...
                               && ! ischar (x), args);
      args(drop) = [];
      isdt(drop) = [];
      ## Defensive only: Octave dispatches here just when some operand is a
      ## datetime, so this cannot be reached through a concatenation.
      if (! any (isdt))
        error (strcat ("datetime.cat: at least one input must be a", ...
                       " datetime array."));
      endif
      ## The first datetime operand gives the result its Format and TimeZone,
      ## and is what any date/time text is read against: such text names a wall
      ## clock in that zone rather than an instant to be converted into it, the
      ## rule the set operations and 'isbetween' already follow.
      out = args{find (isdt, 1)};
      for k = find (! isdt)
        args{k} = dtCatPromote (args{k}, out);
      endfor
      ## Every operand must agree on whether it is zoned at all, as in MATLAB:
      ## a wall clock that names no instant and one that does cannot sit in the
      ## same array.
      zoned = cellfun (@(x) ! isempty (x.TimeZone), args);
      if (any (zoned) && ! all (zoned))
        error (strcat ("datetime.cat: cannot concatenate a datetime array", ...
                       " that has a time zone with one that does not have a", ...
                       " time zone."));
      endif
      ## Zoned operands need not share a zone.  Concatenating changes which
      ## array an element belongs to, not the instant it names, so an operand
      ## from another zone is converted into the zone of the first rather than
      ## having its wall clock read as if it had always been there.  This also
      ## makes the leap-second check, which cannot be met by a conversion.
      for k = 1:numel (args)
        [~, args{k}] = prepSetOp (out, args{k}, 'cat');
      endfor
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
      fieldArgs  = cellfun (@(obj) obj.Offset, args, 'UniformOutput', false);
      out.Offset = cat (dim, fieldArgs{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datetime} {@var{C} =} horzcat (@var{A}, @var{B}, @dots{})
    ##
    ## Horizontal concatenation of datetime arrays.
    ##
    ## @code{@var{C} = horzcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}, @var{B}, @dots{}]} and horizontally
    ## concatenates the inputs @var{A}, @var{B}, @dots{}, which must have the
    ## same size except along the second dimension.  See @code{cat} for which
    ## inputs are accepted and how their time zones are resolved.
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
    ## concatenates the inputs @var{A}, @var{B}, @dots{}, which must have the
    ## same size except along the first dimension.  See @code{cat} for which
    ## inputs are accepted and how their time zones are resolved.
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
      this.Offset = repmat (this.Offset, varargin{:});
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
      this.Offset = repelem (this.Offset, varargin{:});
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
      this.Offset = repelems (this.Offset, R);
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
      this.Offset = reshape (this.Offset, varargin{:});
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
      this.Offset = circshift (this.Offset, varargin{:});
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
      this.Offset = permute (this.Offset, order);
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
      this.Offset = ipermute (this.Offset, order);
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
      this.Offset = transpose (this.Offset);
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
      this.Offset = ctranspose (this.Offset);
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
          out.Offset = this.Offset(s.subs{:});

        case '{}'
          error (strcat ("datetime.subsref: '{}' invalid indexing", ...
                         " for referencing values. Use '()' instead."));

        case '.'
          if (! ischar (s.subs))
            error (strcat ("datetime.subsref: '.' index argument", ...
                           " must be a character vector."));
          endif
          switch (s.subs)
            case 'proxyArray'  # used by 'table' class
              out = proxyArray (this);
            case 'Format'
              out = dtResolveFormat (this.Format, this.Hour, this.Minute, ...
                                     this.Second);
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
          error (strcat ("datetime.subsasgn: '%s' invalid indexing", ...
                         " for assigning values. Use '()' instead."), p.type);
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
            this.Offset(s.subs{:}) = [];
            return;
          elseif (! isa (val, "datetime"))
            error (strcat ("datetime.subsasgn: cannot assign %s values", ...
                           " to a datetime array."), class (val));
          endif
          ## Track which positions are real so that any elements created by
          ## growing the array (out-of-range assignment) can be filled with
          ## Not-A-Time; Octave would otherwise pad them with 0, i.e. the
          ## invalid 0000-00-00 datetime (MATLAB pads such gaps with NaT).
          filled = true (size (this.Year));
          filled(s.subs{:}) = true;
          this.Year(s.subs{:})   = val.Year;
          this.Month(s.subs{:})  = val.Month;
          this.Day(s.subs{:})    = val.Day;
          this.Hour(s.subs{:})   = val.Hour;
          this.Minute(s.subs{:}) = val.Minute;
          this.Second(s.subs{:}) = val.Second;
          this.Offset(s.subs{:}) = val.Offset;
          gap = ! filled;
          if (any (gap(:)))
            this.Year(gap) = NaN;    this.Month(gap) = NaN;
            this.Day(gap) = NaN;     this.Hour(gap) = NaN;
            this.Minute(gap) = NaN;  this.Second(gap) = NaN;
            this.Offset(gap) = NaN;
          endif

        case '{}'
          error (strcat ("datetime.subsasgn: '{}' invalid indexing", ...
                         " for assigning values. Use '()' instead."));

        case '.'
          if (! ischar (s.subs))
            error (strcat ("datetime.subsasgn: '.' index argument", ...
                           " must be a character vector."));
          endif
          switch (s.subs)
            case 'Format'
              if (! (ischar (val) && (isrow (val) || isempty (val))))
                error (strcat ("datetime.subsasgn: 'Format' must be a", ...
                               " character vector."));
              endif
              if (dtIsLeapZone (this.TimeZone))
                dtValidateLeapFormat (val, 'datetime.subsasgn');
              else
                dtValidateFormat (val);
              endif
              this.Format = val;
            case 'TimeZone'
              toTimeZone = val;
              if (! (ischar (toTimeZone) && (isrow (toTimeZone) ...
                                             || isempty (toTimeZone))))
                error (strcat ("datetime.subsasgn: 'TimeZone' must be a", ...
                               " character vector."));
              endif
              ## Validate the target zone (empty means an unzoned array).
              if (! isempty (toTimeZone))
                [~,~,~,~,~,~,errmsg] = __datetime__ (0, 0, 0, ...
                                                     'TimeZone', toTimeZone);
                if (! isnumeric (errmsg))
                  error ("datetime.subsasgn: %s", errmsg);
                endif
              endif
              ## Assigning the zone an array already has changes nothing.  Worth
              ## short-circuiting rather than round-tripping, because for the
              ## leap-second zone the round trip is not the identity: it would
              ## roll a 60th second over.
              if (strcmp (this.TimeZone, toTimeZone))
                return;
              endif
              wasLeap = dtIsLeapZone (this.TimeZone);
              nowLeap = dtIsLeapZone (toTimeZone);
              if (wasLeap)
                ## No other zone has a 60th second for an inserted one to move
                ## into, so it folds back onto the 59th, and the display format
                ## reverts to the data-dependent default.
                this.Second = dtLeapBackFold (this.Second);
                this.Format = 'default';
              endif
              if (isempty (toTimeZone))
                ## Dropping the zone keeps the wall-clock values as they are.
                this.TimeZone = toTimeZone;
                this.Offset = zeros (size (this.Year));
              elseif (isempty (this.TimeZone))
                ## Attaching a zone to an unzoned array reinterprets the
                ## wall-clock values in that zone without converting, but a
                ## wall clock the local clock never shows moves ahead by the
                ## gap, exactly as in the constructor.
                [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
                 this.Second, errmsg] = __datetime__ (this.Year, this.Month, ...
                 this.Day, this.Hour, this.Minute, this.Second, ...
                 'TimeZone', toTimeZone, 'toTimeZone', toTimeZone, ...
                 'Precision', 'microseconds');
                if (! isnumeric (errmsg))
                  error ("datetime.subsasgn: %s", errmsg);
                endif
                this.TimeZone = toTimeZone;
                this.Offset = dtOffsetOf (this.Year, this.Month, this.Day, ...
                              this.Hour, this.Minute, this.Second, toTimeZone);
              elseif (wasLeap || nowLeap)
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
                this.Offset = dtOffsetOf (this.Year, this.Month, this.Day, ...
                              this.Hour, this.Minute, this.Second, toTimeZone);
              else
                ## Switching zones preserves the absolute INSTANT, so it is
                ## taken from the stored offset rather than re-resolved from
                ## the wall clock: an element on the earlier pass of a repeated
                ## hour keeps its identity instead of collapsing to the later.
                ser = serial (this);
                this.TimeZone = toTimeZone;
                [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
                 this.Second, this.Offset] = serial2components (this, ser);
              endif
              if (nowLeap)
                this.Format = dtLeapFormat ();
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
        dtCheckLeapPair (A, B, fname);
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
        ## Express B's components in A's time zone so selected values are
        ## exact.  This is the same instant-preserving conversion the set
        ## operations need, so it goes through 'prepSetOp' rather than being
        ## repeated here -- which is also what keeps B's offset in step with
        ## its new wall clock, since the two are picked together below.
        [~, B] = prepSetOp (A, B, fname);
        YB = B.Year; MB = B.Month; DB = B.Day;
        hB = B.Hour; mB = B.Minute; sB = B.Second;
        z = zeros (common);
        Y = A.Year + z; Mo = A.Month + z; D = A.Day + z;
        h = A.Hour + z; mi = A.Minute + z; s = A.Second + z;
        of = A.Offset + z; ofB = B.Offset + z;
        YB = YB + z; MB = MB + z; DB = DB + z;
        hB = hB + z; mB = mB + z; sB = sB + z;
        takeB = ! takeA;
        Y(takeB) = YB(takeB); Mo(takeB) = MB(takeB); D(takeB) = DB(takeB);
        h(takeB) = hB(takeB); mi(takeB) = mB(takeB); s(takeB) = sB(takeB);
        of(takeB) = ofB(takeB);
        if (strcmp (nanflag, 'includenan'))
          nanpos = isnan (SA) | isnan (SB);
          Y(nanpos) = NaN; Mo(nanpos) = NaN; D(nanpos) = NaN;
          h(nanpos) = NaN; mi(nanpos) = NaN; s(nanpos) = NaN;
          of(nanpos) = NaN;
        endif
        M = A;
        M.Year = Y; M.Month = Mo; M.Day = D;
        M.Hour = h; M.Minute = mi; M.Second = s;
        M.Offset = of;
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
      if (size (A, dim) < 2)
        M = A;
        I = ones (size (A));
        return;
      endif
      ## Ask for the winner's position both ways: as a subscript along DIM,
      ## which is what the second output promises, and as a linear index, which
      ## is what picking the winning elements out of the component arrays needs.
      ## The linear form comes from the built-in rather than being worked out
      ## here, since doing that arithmetic by hand only holds for a matrix and
      ## quietly addresses the wrong page of anything larger.
      if (ismax)
        [~, I] = max (S, [], dim);
        [~, lin] = max (S, [], dim, 'linear');
      else
        [~, I] = min (S, [], dim);
        [~, lin] = min (S, [], dim, 'linear');
      endif
      if (strcmp (nanflag, 'includenan'))
        nanmask = any (isnan (S), dim);
        [~, firstnan] = max (isnan (S), [], dim);
        [~, firstnanlin] = max (isnan (S), [], dim, 'linear');
        I(nanmask) = firstnan(nanmask);
        lin(nanmask) = firstnanlin(nanmask);
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
      this.Offset = this.Offset(varargin{:});
    endfunction

    ## Numeric proxy used by 'table' and set operations for sorting, grouping,
    ## and set membership.  Each datetime element maps to its six canonical
    ## components [Year, Month, Day, Hour, Minute, Second] followed by the
    ## negated UTC offset; for a datetime matrix, each column contributes a
    ## seven-column block.  Not-A-Time (NaT) elements map to NaN across their
    ## components, just like the stored arrays.
    ##
    ## The offset is carried because the components alone do not identify an
    ## element: across a fall-back one tuple names two moments, which must
    ## group and sort apart.  It is negated so that the larger offset -- the
    ## earlier moment -- sorts first, leaving the block in chronological order.
    function out = proxyArray (this)
      [~, cols] = size (this.Year);
      off = this.Offset + zeros (size (this.Year));
      ## An unzoned array stores a zero offset for every element, Not-A-Time
      ## included; the proxy answers NaN across all of a NaT's columns, so the
      ## seventh is masked to match the six beside it.
      off(isnan (this.Year)) = NaN;
      if (cols > 1)
        out = [];
        for i = 1:cols
          SC = [this.Year(:,i), this.Month(:,i), this.Day(:,i), ...
                this.Hour(:,i), this.Minute(:,i), this.Second(:,i), ...
                -off(:,i)];
          out = [out, SC];
        endfor
      else
        out = [this.Year(:), this.Month(:), this.Day(:), ...
               this.Hour(:), this.Minute(:), this.Second(:), -off(:)];
      endif
    endfunction

    ## Re-canonicalise the component arrays after a direct component assignment
    ## (e.g. 'd.Month = 13' rolls the extra month into the year).  This routes
    ## the raw values back through the same C++ normaliser used by the
    ## constructor, at microsecond precision so no sub-second detail is lost.
    ## Not-A-Time and infinite elements are passed through unchanged.
    function this = normalize (this)
      if (isempty (this.Year))
        this.Offset = zeros (size (this.Year));
        return;
      endif
      if (dtIsLeapZone (this.TimeZone))
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = dtLeapNormalize (this.Year, this.Month, this.Day, ...
                        this.Hour, this.Minute, this.Second);
      elseif (isempty (this.TimeZone))
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (this.Year, this.Month, this.Day, ...
         this.Hour, this.Minute, this.Second, 'Precision', 'microseconds');
      else
        [this.Year, this.Month, this.Day, this.Hour, this.Minute, ...
         this.Second] = __datetime__ (this.Year, this.Month, this.Day, ...
         this.Hour, this.Minute, this.Second, 'TimeZone', this.TimeZone, ...
         'toTimeZone', this.TimeZone, 'Precision', 'microseconds');
      endif
      ## A wall clock names at most one instant here: an ambiguous one resolves
      ## to the LATER pass and one in a spring-forward gap is shifted past it,
      ## both in resolve_local.  Recomputing the offset from the normalized
      ## components therefore re-asserts that choice, which is right for every
      ## wall-clock operation and wrong for an instant-based one -- see the
      ## invariant on the Offset property.
      this.Offset = dtOffsetOf (this.Year, this.Month, this.Day, this.Hour, ...
                                this.Minute, this.Second, this.TimeZone);
    endfunction

    ## Absolute instant of each element as POSIX seconds (double, microsecond
    ## precision).  Unzoned arrays are treated as UTC so the serial carries no
    ## system-zone daylight-saving offset; zoned arrays honour their zone (and
    ## DST).  Not-A-Time maps to NaN and infinite elements keep their sign.
    ## Used by the arithmetic and relational instant-based comparisons.
    ##
    ## For a leap-second array the count is instead the continuous SI-second
    ## count of 'dtLeapSerial', which differs from POSIX time by the number of
    ## seconds inserted so far.  That is what makes the arithmetic, the
    ## relational operators, the orderings and the set operations count inserted
    ## seconds without any of them knowing about leap seconds; 'posixtime' asks
    ## for POSIX time proper and so does not go through here.
    function s = serial (this)
      if (dtIsLeapZone (this.TimeZone))
        s = dtLeapSerial (this.Year, this.Month, this.Day, this.Hour, ...
                          this.Minute, this.Second);
        return;
      endif
      ## Read the instant off the stored offset rather than asking the tz
      ## database to resolve the wall clock, which is exactly the question
      ## that has two answers on a fall-back day.  Offset is zero for an
      ## unzoned array, so the one expression serves both.
      nai = __datetime__ (this.Year, this.Month, this.Day, this.Hour, ...
                          this.Minute, this.Second, 'ConvertTo', ...
                          'posixtime', 'TimeZone', 'UTC', 'Precision', ...
                          'microseconds');
      s = nai - this.Offset;
    endfunction

    ## POSIX instant used by the integer fixed-epoch conversions ('epochtime',
    ## 'ntp', 'ntfs', '.net').  Ordinary arrays use their absolute instant; a
    ## leap-second array folds an inserted second backward, which is the rule
    ## every one of those formats follows.
    ##
    ## Deliberately unlike MATLAB, which applies this fold to the array but
    ## measures a user-supplied 'epochtime' Epoch on the leap-second timeline
    ## instead, so its count is short by the seconds inserted before that epoch
    ## -- 86373 rather than 86399 for 2016-12-31T23:59:59Z counted from
    ## 2016-12-31.  Mixing the two timelines cannot be right whichever is
    ## chosen, so both operands are folded alike here.  Documented in convertTo.
    function p = epochBase (this)
      if (dtIsLeapZone (this.TimeZone))
        p = dtLeapPosix (this.Year, this.Month, this.Day, this.Hour, ...
                         this.Minute, dtLeapBackFold (this.Second));
      else
        p = serial (this);
      endif
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
      dtCheckLeapPair (A, B, op);
      if (! isempty (A.TimeZone) && ! strcmp (A.TimeZone, B.TimeZone))
        ## Go through the instant rather than converting the wall clock in
        ## place -- see 'dtRezone'.  The pair is never a leap zone here, since
        ## 'dtCheckLeapPair' has just required both operands to agree and the
        ## only leap zone would then also have compared equal above.
        [B.Year, B.Month, B.Day, B.Hour, B.Minute, B.Second, B.Offset] = ...
            dtRezone (B.Year, B.Month, B.Day, B.Hour, B.Minute, B.Second, ...
                      B.Offset, B.TimeZone, A.TimeZone);
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
      A.Offset = A.Offset + z;
      B.Year = B.Year + z; B.Month = B.Month + z; B.Day = B.Day + z;
      B.Hour = B.Hour + z; B.Minute = B.Minute + z; B.Second = B.Second + z;
      B.Offset = B.Offset + z;
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
        ## CUR carries A's components no longer, so A's offset need not belong
        ## to it either; re-derive it before the instant below is taken, then
        ## give A's own back wherever it still describes CUR's clock.  Both
        ## halves are load-bearing: dropping the first leaves an hour of error
        ## whenever the calendar step crossed a transition, and dropping the
        ## second loses the fold when the step did not move the clock at all,
        ## so that the two passes over a repeated clock report no time between
        ## them rather than the hour that separates them.
        cur.Offset = dtOffsetOf (cur.Year, cur.Month, cur.Day, cur.Hour, ...
                                 cur.Minute, cur.Second, cur.TimeZone);
        cur = keepFold (cur, A.Offset + zeros (sz));
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

    ## Absolute instant of the given wall-clock components, interpreted in this
    ## array's time zone.  Used by dateshift's 'nearest' rule to measure how far
    ## an element sits into its current unit.
    function ser = dsSerialOf (this, Y, M, D, h, m, s)
      tmp = this;
      tmp.Year = Y; tmp.Month = M; tmp.Day = D;
      tmp.Hour = h; tmp.Minute = m; tmp.Second = s;
      ser = serial (normalize (tmp));
    endfunction

    ## Handles that carry this array's calendar into the binning helpers.  Those
    ## are local functions, so they have no 'this' and cannot reach the class's
    ## own conversions; only the class knows its zone, its leap seconds and its
    ## epoch, so the knowledge is passed in rather than duplicated there.  S2C
    ## maps a serial to its local year/month/day, C2S a local date to the serial
    ## of its midnight, and D2S any datetime to its serial.
    function [s2c, c2s, d2s] = dtCalHandles (this)
      s2c = @(s) serial2components (this, s);
      c2s = @(Y, M, D) dsSerialOf (this, Y, M, D, zeros (size (Y)), ...
                                   zeros (size (Y)), zeros (size (Y)));
      d2s = @(D) serial (D);
    endfunction

    ## Build a datetime from reduced POSIX seconds (the result of mean/median/
    ## mode/std on this array's serial), preserving the Format and TimeZone.
    function R = fromReducedSerial (this, ser)
      R = this;
      [Y, M, D, h, m, s, off] = serial2components (this, ser);
      R.Year = Y; R.Month = M; R.Day = D;
      R.Offset = off;
      R.Hour = h; R.Minute = m; R.Second = s;
    endfunction

    ## Inverse of 'serial': map POSIX seconds back to the wall-clock components
    ## of this array's time zone.  For a zoned array the serial is first read as
    ## a UTC wall clock and then converted into the target zone (honouring DST).
    ## A leap-second array inverts the continuous SI-second count instead, so a
    ## count that lands inside an inserted second yields a 60th second.
    function [Y, M, D, h, m, s, off] = serial2components (this, ser)
      if (dtIsLeapZone (this.TimeZone))
        [Y, M, D, h, m, s] = dtLeapComponents (ser);
        off = zeros (size (Y));
        return;
      elseif (isempty (this.TimeZone))
        [Y, M, D, h, m, s] = __datetime__ (ser, 'ConvertFrom', 'posixtime', ...
                                           'Precision', 'microseconds');
      else
        [Y, M, D, h, m, s] = __datetime__ (ser, 'ConvertFrom', 'posixtime', ...
                                           'Precision', 'microseconds');
        [Y, M, D, h, m, s] = __datetime__ (Y, M, D, h, m, s, ...
                             'TimeZone', 'UTC', 'toTimeZone', this.TimeZone, ...
                             'Precision', 'microseconds');
      endif
      ## The offset that belongs to THIS instant, which is what distinguishes
      ## the two passes over a repeated wall clock.  It is read back off the
      ## instant rather than looked up from the wall clock, since the latter is
      ## exactly the question that has no unique answer.  Offsets are whole
      ## seconds, so rounding clears the conversion's floating-point dust.
      off = zeros (size (Y));
      if (! isempty (this.TimeZone))
        off = round (__datetime__ (Y, M, D, h, m, s, 'ConvertTo', ...
                     'posixtime', 'TimeZone', 'UTC', ...
                     'Precision', 'microseconds') - ser);
      endif
    endfunction

    ## Shift each element by a fixed number of seconds applied to its absolute
    ## instant (daylight-saving aware for zoned arrays), then rebuild the
    ## wall-clock components.  DSEC may broadcast against the array size.  The
    ## Format and TimeZone properties are preserved.
    function this = addSeconds (this, dsec)
      ser = serial (this) + dsec;
      [Y, M, D, h, m, s, off] = serial2components (this, ser);
      this.Year = Y; this.Month = M; this.Day = D;
      this.Hour = h; this.Minute = m; this.Second = s;
      this.Offset = off;
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

      if (dtIsLeapZone (this.TimeZone))
        ## Calendar arithmetic keeps the wall clock, so an inserted second can
        ## be carried onto a minute that never had one; clamp it to the 59th
        ## second there, as the day of the month is clamped above.
        s = dtLeapClampSecond (s, dtLeapMinutePosix (Y, M, D, h, m, ...
                                                    zeros (size (Y))));
      endif
      srcOff = this.Offset + base;
      this.Year = Y; this.Month = M; this.Day = D;
      this.Hour = h; this.Minute = m; this.Second = s;
      this = normalize (this);
      ## Calendar arithmetic keeps the wall clock, and where that clock is a
      ## repeated one it keeps the fold as well: a day added to the pass that
      ## is still on daylight saving arrives on the pass that is still on it.
      ## 'normalize' has just resolved every element to the later pass, which
      ## is right only for elements that did not come from the earlier one.
      this = keepFold (this, srcOff);

      ## Add the time-of-day component as an instant (daylight-saving aware).
      if (any (dTime(:) != 0))
        this = addSeconds (this, dTime + zeros (size (this.Year)));
      endif
    endfunction

    ## Restore SRCOFF as this array's offset wherever it still describes the
    ## array's own wall clock, leaving the resolved offset in place everywhere
    ## else.  The test is the round trip: read the instant that SRCOFF names
    ## and put it back on the clock, and keep SRCOFF exactly when the clock
    ## comes back unchanged.  That admits the earlier pass of a repeated clock,
    ## which is the case this exists for, and refuses a stale offset carried in
    ## from a date whose zone was on the other side of a transition -- for
    ## which no fold of the target clock corresponds to it and the trip fails.
    function this = keepFold (this, srcOff)
      if (isempty (this.TimeZone) || dtIsLeapZone (this.TimeZone))
        return;
      endif
      nai = __datetime__ (this.Year, this.Month, this.Day, this.Hour, ...
                          this.Minute, this.Second, 'ConvertTo', ...
                          'posixtime', 'TimeZone', 'UTC', 'Precision', ...
                          'microseconds');
      [rY, rM, rD, rh, rm, rs] = serial2components (this, nai - srcOff);
      rnai = __datetime__ (rY, rM, rD, rh, rm, rs, 'ConvertTo', 'posixtime', ...
                           'TimeZone', 'UTC', 'Precision', 'microseconds');
      keep = isfinite (rnai) & isfinite (nai) & abs (rnai - nai) <= 1e-6;
      this.Offset(keep) = srcOff(keep);
    endfunction

    ## Range with a fixed-length (duration/numeric) step of STEPSEC seconds.
    ## The endpoints' absolute instants are stepped by the ordinary numeric
    ## colon (so the inclusive-endpoint tolerance matches numeric ranges), then
    ## rebuilt into this array's time zone.  Empty and reversed ranges fall out
    ## naturally from the numeric colon.
    function R = colonLinear (A, stepSec, B)
      ser = serial (A) : stepSec : serial (B);
      [Y, M, D, h, m, s, off] = serial2components (A, ser);
      R = A;
      R.Year = Y; R.Month = M; R.Day = D;
      R.Hour = h; R.Minute = m; R.Second = s;
      R.Offset = off;
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
      dtCheckLeapPair (A, B, op);
      ## Express B on A's clock through 'prepSetOp', which goes by the instant.
      ## Converting the components on their own would resolve an operand
      ## sitting on the earlier pass of a repeated clock to the later one and
      ## compare the wrong moment.
      [~, B] = prepSetOp (A, B, op);
      aY = A.Year; aM = A.Month; aD = A.Day;
      ah = A.Hour; am = A.Minute; asec = A.Second; aoff = A.Offset;
      bY = B.Year; bM = B.Month; bD = B.Day;
      bh = B.Hour; bm = B.Minute; bsec = B.Second; boff = B.Offset;
      ## Two elements are the same moment only if they also agree on the
      ## offset: identical components on a fall-back day name two of them.
      EQ = (aY == bY) & (aM == bM) & (aD == bD) ...
         & (ah == bh) & (am == bm) & (asec == bsec) & (aoff == boff);
      switch (op)
        case 'eq'
          TF = EQ;
        case 'ne'
          TF = ! EQ;
        case 'lt'
          TF = lexlt (aY, aM, aD, ah, am, asec, bY, bM, bD, bh, bm, bsec, ...
                      aoff, boff);
        case 'gt'
          TF = lexlt (bY, bM, bD, bh, bm, bsec, aY, aM, aD, ah, am, asec, ...
                      boff, aoff);
        case 'le'
          TF = lexlt (aY, aM, aD, ah, am, asec, bY, bM, bD, bh, bm, bsec, ...
                      aoff, boff) | EQ;
        case 'ge'
          TF = lexlt (bY, bM, bD, bh, bm, bsec, aY, aM, aD, ah, am, asec, ...
                      boff, aoff) | EQ;
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
    ## Leap seconds are part of what an array counts, so an array that has
    ## them is never equal to one that does not.
    if (dtIsLeapZone (A.TimeZone) != dtIsLeapZone (B.TimeZone))
      TF = false;
      return;
    endif
    if (isempty (A))
      continue;  # two empties of equal size compare equal
    endif
    ## Zoned operands are moved to UTC, where no wall clock names more than
    ## one moment.  That does two things at once: it aligns operands in
    ## different zones by their instant, and it separates the two moments that
    ## share a local clock on the day a clock goes back, which no comparison
    ## of local components could tell apart.  Assigning the zone preserves the
    ## instant, so nothing else is needed here.  The leap-second zone is left
    ## alone: both operands are already in it, it is its own frame, and moving
    ## out of it would fold an inserted second onto its neighbour.
    if (! isempty (A.TimeZone) && ! dtIsLeapZone (A.TimeZone))
      A.TimeZone = 'UTC';
      B.TimeZone = 'UTC';
    endif
    aY = A.Year; aM = A.Month; aD = A.Day;
    ah = A.Hour; am = A.Minute; asec = A.Second;
    bY = B.Year; bM = B.Month; bD = B.Day;
    bh = B.Hour; bm = B.Minute; bsec = B.Second;
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

## B's wall-clock components and UTC offset expressed in the time zone TZ,
## keeping each element's instant.  B is returned untouched when it is already
## in TZ or when TZ is empty.
##
## The route is the instant and not the wall clock, and the difference is the
## whole point: asking the tz database to convert B's components between zones
## makes it resolve them in B's own zone first, and on a fall-back day that
## resolution has two answers, of which it always takes the later.  An element
## sitting on the earlier pass would come out an hour wrong.  Reading the
## instant off B's stored offset, then putting it back on the clock in TZ,
## asks nothing that has two answers.
## Takes and returns the bare component arrays rather than the object, since
## a file-scope function reaches a datetime's properties through 'subsref',
## which exposes the six components but deliberately not the offset.
function [Y, M, D, h, m, s, off] = dtRezone (Y, M, D, h, m, s, off, ...
                                             fromTZ, TZ)
  off = off + zeros (size (Y));
  if (isempty (TZ) || strcmp (TZ, fromTZ))
    return;
  endif
  ser = __datetime__ (Y, M, D, h, m, s, 'ConvertTo', 'posixtime', ...
                      'TimeZone', 'UTC', 'Precision', 'microseconds') - off;
  [Y, M, D, h, m, s] = __datetime__ (ser, 'ConvertFrom', 'posixtime', ...
                                     'Precision', 'microseconds');
  [Y, M, D, h, m, s] = __datetime__ (Y, M, D, h, m, s, 'TimeZone', 'UTC', ...
                       'toTimeZone', TZ, 'Precision', 'microseconds');
  ## Offsets are whole seconds, so rounding clears the conversion's dust.
  off = round (__datetime__ (Y, M, D, h, m, s, 'ConvertTo', 'posixtime', ...
               'TimeZone', 'UTC', 'Precision', 'microseconds') - ser);
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
##
## AOFF and BOFF are the operands' UTC offsets, and break the tie the six
## components cannot: on the day a clock goes back the same tuple names two
## moments an hour apart, and the one still on daylight saving -- the earlier
## of the two -- is the one carrying the LARGER offset, whence the reversed
## test at the bottom.  They default to zero, which is the right answer for a
## caller comparing wall clocks rather than instants (calendar arithmetic does
## exactly that) and for any unzoned array.  The comparison stays componentwise
## and so stays exact at every magnitude, which a comparison of instants held
## as seconds in a double would not be.
function TF = lexlt (aY, aM, aD, ah, am, asec, bY, bM, bD, bh, bm, bsec, ...
                     aoff = 0, boff = 0)
  eqY = aY == bY;  eqM = aM == bM;  eqD = aD == bD;
  eqh = ah == bh;  eqm = am == bm;  eqs = asec == bsec;
  TF = (aY < bY) ...
     | (eqY & aM < bM) ...
     | (eqY & eqM & aD < bD) ...
     | (eqY & eqM & eqD & ah < bh) ...
     | (eqY & eqM & eqD & eqh & am < bm) ...
     | (eqY & eqM & eqD & eqh & eqm & asec < bsec) ...
     | (eqY & eqM & eqD & eqh & eqm & eqs & aoff > boff);
endfunction

## Promote a set-operation operand to a datetime array.  A datetime is returned
## unchanged; text (character vector, string, or cellstr) is parsed by the
## constructor, inheriting REF's time zone so the two operands share a frame;
## numeric, logical, and duration operands are rejected the way MATLAB rejects
## them.  Defined at file scope (not as a method) so it dispatches correctly
## when the first set-operation argument is text rather than a datetime.
## Promote a non-datetime operand of a concatenation to datetime.  Date/time
## text is read as a wall clock in REF's time zone, the same rule the set
## operations and 'isbetween' follow; nothing else can be concatenated with a
## datetime array.
function d = dtCatPromote (x, ref)
  if (ischar (x) || iscellstr (x) || isa (x, 'string'))
    if (isempty (ref.TimeZone))
      d = datetime (x);
    else
      d = datetime (x, 'TimeZone', ref.TimeZone);
    endif
  else
    error (strcat ("datetime.cat: all inputs must be datetime arrays or", ...
                   " date/time text."));
  endif
endfunction

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

## Promote a bound of 'isbetween' to a datetime array.  A datetime is returned
## unchanged; text is parsed by the constructor, inheriting REF's time zone;
## anything else (numeric, duration, ...) is rejected.  Defined at file scope so
## it dispatches correctly when the argument is text rather than a datetime.
function d = dtIsbetweenArg (x, ref)
  if (isa (x, 'datetime'))
    d = x;
  elseif (ischar (x) || iscellstr (x) || isa (x, 'string'))
    if (isempty (ref.TimeZone))
      d = datetime (x);
    else
      d = datetime (x, 'TimeZone', ref.TimeZone);
    endif
  else
    error (strcat ("datetime.isbetween: LOWER and UPPER must be datetime", ...
                   " arrays or date/time text."));
  endif
endfunction

## Translate datetime missing-value flags into the 'omitnan'/'includenan' flags
## understood by the core reduction functions, leaving dims, 'all', and the core
## flags untouched.
function args = dtStatFlags (args)
  for i = 1:numel (args)
    if (ischar (args{i}) && isrow (args{i}))
      switch (lower (args{i}))
        case {'omitnat', 'omitmissing'}
          args{i} = 'omitnan';
        case {'includenat', 'includemissing'}
          args{i} = 'includenan';
      endswitch
    endif
  endfor
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

## Map a day-of-week name to its number (Sunday = 1 .. Saturday = 7); NaN for an
## unrecognised name.
function n = dsDayName (name)
  names = {'sunday', 'monday', 'tuesday', 'wednesday', 'thursday', ...
           'friday', 'saturday'};
  n = find (strcmpi (name, names), 1);
  if (isempty (n))
    n = NaN;
  endif
endfunction

## Parse a dateshift RULE into a kind ('current'/'next'/'previous'/'nearest'/
## 'int') and, for the integer kind, its value.
function [kind, n] = dsRule (r)
  n = 0;
  msg = strcat ("datetime.dateshift: rule must be an integer, 'next',", ...
                " 'previous', 'current', or 'nearest'.");
  if (ischar (r) && isrow (r))
    if (any (strcmpi (r, {'current', 'next', 'previous', 'nearest'})))
      kind = lower (r);
    else
      error (msg);
    endif
  elseif (isnumeric (r) && isscalar (r) && isreal (r) && r == fix (r))
    kind = 'int';
    n = r;
  else
    error (msg);
  endif
endfunction

## Shift a date/time by N whole units (used by dateshift's rule).  Calendar units
## produce a canonical date; sub-day units may overflow and are canonicalised by
## the caller's final normalisation.
function [Y, M, D, h, mi, s] = dsShiftUnits (Y, M, D, h, mi, s, unit, n)
  switch (unit)
    case 'year'
      [Y, M, D] = dtAddMonths (Y, M, D, 12 .* n);
    case 'quarter'
      [Y, M, D] = dtAddMonths (Y, M, D, 3 .* n);
    case 'month'
      [Y, M, D] = dtAddMonths (Y, M, D, n);
    case 'week'
      [Y, M, D] = dtAddDays (Y, M, D, 7 .* n);
    case 'day'
      [Y, M, D] = dtAddDays (Y, M, D, n);
    case 'hour'
      h = h + n;
    case 'minute'
      mi = mi + n;
    case 'second'
      s = s + n;
  endswitch
endfunction

## Truncate a date/time down to the start of the given calendar unit.
function [Y, M, D, h, mi, s] = dsStartComp (Y, M, D, h, mi, s, unit)
  z = zeros (size (Y));
  switch (unit)
    case 'year'
      M = ones (size (M)); D = ones (size (D)); h = z; mi = z; s = z;
    case 'quarter'
      M = 3 .* floor ((M - 1) / 3) + 1; D = ones (size (D));
      h = z; mi = z; s = z;
    case 'month'
      D = ones (size (D)); h = z; mi = z; s = z;
    case 'week'
      dow = weekday (datenum (Y, M, D));
      [Y, M, D] = dtAddDays (Y, M, D, -(dow - 1));
      h = z; mi = z; s = z;
    case 'day'
      h = z; mi = z; s = z;
    case 'hour'
      mi = z; s = z;
    case 'minute'
      s = z;
    case 'second'
      s = floor (s);
  endswitch
endfunction

## Compute the end of the given calendar unit.  For the sub-day units and 'day'
## this is the start of the next unit; for a week it is the last day (Saturday);
## for month/quarter/year it is the last day of the unit at midnight.  The
## results may overflow and are canonicalised by the caller's normalisation.
function [Y, M, D, h, mi, s] = dsEndComp (Y, M, D, h, mi, s, unit)
  z = zeros (size (Y));
  switch (unit)
    case 'year'
      M = 12 .* ones (size (M)); D = 31 .* ones (size (D));
      h = z; mi = z; s = z;
    case 'quarter'
      qm = 3 .* ceil (M / 3);
      D = dtDaysInMonth (Y, qm); M = qm; h = z; mi = z; s = z;
    case 'month'
      D = dtDaysInMonth (Y, M); h = z; mi = z; s = z;
    case 'week'
      dow = weekday (datenum (Y, M, D));
      [Y, M, D] = dtAddDays (Y, M, D, 7 - dow);
      h = z; mi = z; s = z;
    case 'day'
      [Y, M, D] = dtAddDays (Y, M, D, 1);
      h = z; mi = z; s = z;
    case 'hour'
      h = h + 1; mi = z; s = z;
    case 'minute'
      mi = mi + 1; s = z;
    case 'second'
      s = floor (s) + 1;
  endswitch
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

## Resolve the Format property to a concrete LDML pattern.  The sentinels
## 'default' and 'defaultdate' are data-dependent, matching MATLAB: 'default'
## renders a date-only pattern when every element sits at midnight (all-zero
## time-of-day) and a date+time pattern otherwise; 'defaultdate' is always
## date-only.  A concrete user pattern is returned verbatim (case-sensitive,
## as 'M' and 'm' differ).  NaT elements carry no time of day and are left out
## of the decision, so a date-only array keeps its date-only rendering when a
## NaT sits beside it; an array holding nothing but NaT says nothing either
## way and keeps the date+time pattern.  An infinite datetime carries no time of
## day either, and is left out on the same grounds.
function fmt = dtResolveFormat (fmtProp, H, Mi, S)
  if (strcmpi (fmtProp, 'default'))
    [dflt, dfltdate] = dtDefaultFormats ();
    dated = isfinite (H(:));
    if (! isempty (H) && any (dated) && all ((H(dated) == 0) ...
                                             & (Mi(dated) == 0) ...
                                             & (S(dated) == 0)))
      fmt = dfltdate;
    else
      fmt = dflt;
    endif
  elseif (strcmpi (fmtProp, 'defaultdate'))
    [~, fmt] = dtDefaultFormats ();
  elseif (strcmpi (fmtProp, 'preserveinput'))
    fmt = dtDefaultFormats ();
  else
    fmt = fmtProp;
  endif
endfunction

## Owner of the two default display formats set by 'datetime.setDefaultFormats'.
##
## Called with no arguments it returns the date-and-time default and the
## date-only default, in that order.  @code{dtDefaultFormats ('set', which,
## fmt)} stores one of them, @var{which} being 'default' or 'defaultdate', and
## @code{dtDefaultFormats ('reset')} restores both factory patterns.
##
## The patterns persist across sessions in Octave's preferences, as MATLAB's do
## through its settings tree.  The session cache in front of the preferences is
## not an optimization detail: 'dtResolveFormat' runs on every display, and
## without it every rendering of a datetime array would read a file.  Clearing
## the cache is always safe, since it is re-seeded from the preference on the
## next call, which is what makes this survive the 'clear classes' that
## pkg-octave-doc runs between demos.
##
## A factory pattern is stored as the absence of a preference rather than as its
## value, so that 'reset' leaves nothing behind, matching what MATLAB's own
## 'reset' leaves in its settings tree.
function [dflt, dfltdate] = dtDefaultFormats (mode = 'get', which = '', ...
                                              fmt = '')
  persistent cache_dflt cache_date
  FACTORY_DFLT = 'dd-MMM-uuuu HH:mm:ss';
  FACTORY_DATE = 'dd-MMM-uuuu';
  if (isempty (cache_dflt))
    if (ispref ('datatypes', 'datetime_DefaultFormat'))
      cache_dflt = getpref ('datatypes', 'datetime_DefaultFormat');
    else
      cache_dflt = FACTORY_DFLT;
    endif
  endif
  if (isempty (cache_date))
    if (ispref ('datatypes', 'datetime_DefaultDateFormat'))
      cache_date = getpref ('datatypes', 'datetime_DefaultDateFormat');
    else
      cache_date = FACTORY_DATE;
    endif
  endif
  switch (mode)
    case 'set'
      if (strcmpi (which, 'default'))
        cache_dflt = fmt;
        setpref ('datatypes', 'datetime_DefaultFormat', fmt);
      else
        cache_date = fmt;
        setpref ('datatypes', 'datetime_DefaultDateFormat', fmt);
      endif
    case 'reset'
      cache_dflt = FACTORY_DFLT;
      cache_date = FACTORY_DATE;
      if (ispref ('datatypes', 'datetime_DefaultFormat'))
        rmpref ('datatypes', 'datetime_DefaultFormat');
      endif
      if (ispref ('datatypes', 'datetime_DefaultDateFormat'))
        rmpref ('datatypes', 'datetime_DefaultDateFormat');
      endif
  endswitch
  dflt = cache_dflt;
  dfltdate = cache_date;
endfunction

## Validate a user-supplied Format string, mirroring MATLAB's rejection of
## unsupported field letters.  The sentinels are accepted as-is.
function dtValidateFormat (fmt)
  if (any (strcmpi (fmt, {'default', 'defaultdate', 'preserveinput'})))
    return;
  endif
  supported = 'yuMdDeaHhmsSQGWzZXx';
  syms = __ldml__ ('symbols', fmt);
  for t = 1:numel (syms)
    if (! any (syms(t) == supported))
      error (strcat ("datetime: the format '%s' contains an unsupported", ...
                     " symbol: '%s'."), fmt, syms(t));
    endif
  endfor
endfunction

## Enforce MATLAB's rule that the year, month, day, hour, and minute components
## must be integer-valued; only seconds (and milliseconds) may be fractional.
## Not-A-Number and infinite values are permitted (they yield NaT / Inf).  ARGS
## is the positional argument list: a single numeric date-vector matrix (columns
## 1-5 are Y,M,D,H,MI) or separate component arrays (the first five are
## Y,M,D,H,MI).
function dtCheckIntegerComponents (args)
  if (numel (args) == 1 && isnumeric (args{1}))
    M = args{1};
    vals = M(:, 1:min (5, columns (M)));
  else
    vals = [];
    for k = 1:min (5, numel (args))
      if (isnumeric (args{k}))
        vals = [vals(:); args{k}(:)];
      endif
    endfor
  endif
  if (any (isfinite (vals(:)) & (fix (vals(:)) != vals(:))))
    error (strcat ("datetime: Year, Month, Day, Hour, and Minute components", ...
                   " must be integer values."));
  endif
endfunction

## Return the full and abbreviated month- and weekday-name tables for an LDML
## locale, used to parse 'MMMM'/'MMM' and 'eeee'/'eee' fields under
## 'InputFormat'.  Names are matched case-insensitively; abbreviations are
## stored without any trailing period (locales that write one -- fr, de, pt --
## have it consumed during parsing).  Only a curated set of locales is
## supported; English is the default and the fallback for 'system'.  The country
## part of an 'xx_YY' locale is ignored, as names vary by language, not region.
function [mFull, mAbbr, wFull, wAbbr, dpMark] = dtLocaleNames (locale)
  if (isempty (locale))
    lang = 'en';
  else
    lang = strtok (tolower (locale), '_');
  endif
  if (strcmp (lang, 'system'))
    lang = 'en';
  endif
  ## Day-period markers {am, pm}; English 'am'/'pm' is the default and is used
  ## by every supported locale except Spanish (overridden below).
  dpMark = {'am', 'pm'};
  switch (lang)
    case 'en'
      mFull = {'january','february','march','april','may','june','july', ...
               'august','september','october','november','december'};
      mAbbr = {'jan','feb','mar','apr','may','jun','jul','aug','sep', ...
               'oct','nov','dec'};
      wFull = {'sunday','monday','tuesday','wednesday','thursday','friday', ...
               'saturday'};
      wAbbr = {'sun','mon','tue','wed','thu','fri','sat'};
    case 'fr'
      mFull = {'janvier','février','mars','avril','mai','juin','juillet', ...
               'août','septembre','octobre','novembre','décembre'};
      mAbbr = {'janv','févr','mars','avr','mai','juin','juil','août', ...
               'sept','oct','nov','déc'};
      wFull = {'dimanche','lundi','mardi','mercredi','jeudi','vendredi', ...
               'samedi'};
      wAbbr = {'dim','lun','mar','mer','jeu','ven','sam'};
    case 'de'
      mFull = {'januar','februar','märz','april','mai','juni','juli', ...
               'august','september','oktober','november','dezember'};
      mAbbr = {'jan','feb','märz','apr','mai','juni','juli','aug', ...
               'sept','okt','nov','dez'};
      wFull = {'sonntag','montag','dienstag','mittwoch','donnerstag', ...
               'freitag','samstag'};
      wAbbr = {'so','mo','di','mi','do','fr','sa'};
    case 'es'
      mFull = {'enero','febrero','marzo','abril','mayo','junio','julio', ...
               'agosto','septiembre','octubre','noviembre','diciembre'};
      mAbbr = {'ene','feb','mar','abr','may','jun','jul','ago','sept', ...
               'oct','nov','dic'};
      wFull = {'domingo','lunes','martes','miércoles','jueves','viernes', ...
               'sábado'};
      wAbbr = {'dom','lun','mar','mié','jue','vie','sáb'};
      ## Spanish markers use a UTF-8 no-break space (U+00A0 = bytes 194 160).
      nbsp = char ([194, 160]);
      dpMark = {['a.', nbsp, 'm.'], ['p.', nbsp, 'm.']};
    case 'it'
      mFull = {'gennaio','febbraio','marzo','aprile','maggio','giugno', ...
               'luglio','agosto','settembre','ottobre','novembre','dicembre'};
      mAbbr = {'gen','feb','mar','apr','mag','giu','lug','ago','set', ...
               'ott','nov','dic'};
      wFull = {'domenica','lunedì','martedì','mercoledì','giovedì', ...
               'venerdì','sabato'};
      wAbbr = {'dom','lun','mar','mer','gio','ven','sab'};
    case 'pt'
      mFull = {'janeiro','fevereiro','março','abril','maio','junho','julho', ...
               'agosto','setembro','outubro','novembro','dezembro'};
      mAbbr = {'jan','fev','mar','abr','mai','jun','jul','ago','set', ...
               'out','nov','dez'};
      wFull = {'domingo','segunda-feira','terça-feira','quarta-feira', ...
               'quinta-feira','sexta-feira','sábado'};
      wAbbr = {'dom','seg','ter','qua','qui','sex','sáb'};
    case 'el'
      ## Greek month names are the genitive (format-context) forms -- a date
      ## reads "9 Μαρτίου 2024", not the nominative "Μάρτιος".  Greek has no
      ## ASCII letters, so matching against these canonical-case forms is in
      ## effect case-sensitive (strcmpi folds only ASCII).
      mFull = {'Ιανουαρίου','Φεβρουαρίου','Μαρτίου','Απριλίου','Μαΐου', ...
               'Ιουνίου','Ιουλίου','Αυγούστου','Σεπτεμβρίου','Οκτωβρίου', ...
               'Νοεμβρίου','Δεκεμβρίου'};
      mAbbr = {'Ιαν','Φεβ','Μαρ','Απρ','Μαΐ','Ιουν','Ιουλ','Αυγ','Σεπ', ...
               'Οκτ','Νοε','Δεκ'};
      wFull = {'Κυριακή','Δευτέρα','Τρίτη','Τετάρτη','Πέμπτη','Παρασκευή', ...
               'Σάββατο'};
      wAbbr = {'Κυρ','Δευ','Τρί','Τετ','Πέμ','Παρ','Σάβ'};
      dpMark = {'π.μ.', 'μ.μ.'};
    otherwise
      error (strcat ("datetime: unsupported 'Locale' '%s'; supported", ...
                     " languages are en, fr, de, es, it, pt, and el."), locale);
  endswitch
endfunction

## Fast path for the constructor's format auto-detection.  STRS is a cellstr
## column; the outputs are the N-by-6 date-vector matrix and a flag saying
## whether the fast path claimed the input.  Core 'datevec' auto-detects a long
## list of formats and must stay the authority on what is accepted, so this
## claims an array only when every string carries one of the shapes below AND
## every parsed component is in range -- the conditions under which the LDML
## parser was verified to reproduce 'datevec' element for element.  Anything
## else is declined and falls back to 'datevec' untouched.
## MATLAB's default pivot for a two-digit year: the current year less fifty.
## None of the auto-detected shapes carries a two-digit year, so this is never
## actually consulted there, but the parser wants a value.
function pivot = dtDefaultPivot ()
  now6 = clock ();
  pivot = now6(1) - 50;
endfunction

## Detect the one date/time format a set of strings is written in, returning it
## as an LDML pattern, or '' when none is recognized.  The format is taken from
## the first string, as MATLAB takes it, and every other string is then read
## with it; a string in a different format does not get one of its own.  Where a
## shape carries fractional seconds, the pattern is widened to the digits
## actually present.
##
## The slash shape is ambiguous: 03/09/2024 could be the 3rd of September or the
## 9th of March.  The whole set decides, since one string past the twelfth of a
## month settles it for all of them, and only when nothing does is the American
## reading assumed and a warning raised -- both as MATLAB does.
function fmt = dtDetectFormat (strs)
  persistent shapes;
  if (isempty (shapes))
    shapes = {
      '^\d{4}-\d{1,2}-\d{1,2}$',                   'yyyy-MM-dd';
      '^\d{4}-\d{1,2}-\d{1,2} \d{1,2}:\d{2}$',     'yyyy-MM-dd HH:mm';
      '^\d{4}-\d{1,2}-\d{1,2} \d{1,2}:\d{2}:\d{2}$', 'yyyy-MM-dd HH:mm:ss';
      '^\d{4}-\d{1,2}-\d{1,2} \d{1,2}:\d{2}:\d{2}\.\d+$', ...
                                                    'yyyy-MM-dd HH:mm:ss.';
      '^\d{4}-\d{1,2}-\d{1,2}T\d{1,2}:\d{2}$',     "yyyy-MM-dd'T'HH:mm";
      '^\d{4}-\d{1,2}-\d{1,2}T\d{1,2}:\d{2}:\d{2}$', ...
                                                    "yyyy-MM-dd'T'HH:mm:ss";
      '^\d{4}-\d{1,2}-\d{1,2}T\d{1,2}:\d{2}:\d{2}\.\d+$', ...
                                                    "yyyy-MM-dd'T'HH:mm:ss.";
      '^\d{4}/\d{1,2}/\d{1,2}$',                   'yyyy/MM/dd';
      '^\d{4}/\d{1,2}/\d{1,2} \d{1,2}:\d{2}$',     'yyyy/MM/dd HH:mm';
      '^\d{4}/\d{1,2}/\d{1,2} \d{1,2}:\d{2}:\d{2}$', 'yyyy/MM/dd HH:mm:ss';
      '^\d{1,2}/\d{1,2}/\d{4}$',                   '';
      '^\d{1,2}-[A-Za-z]{3,}-\d{4}$',               'dd-MMM-yyyy';
      '^\d{1,2}-[A-Za-z]{3,}-\d{4} \d{1,2}:\d{2}$', 'dd-MMM-yyyy HH:mm';
      '^\d{1,2}-[A-Za-z]{3,}-\d{4} \d{1,2}:\d{2}:\d{2}$', ...
                                                    'dd-MMM-yyyy HH:mm:ss';
      '^[A-Za-z]{3,} \d{1,2}, \d{4}$',              'MMMM d, yyyy';
      '^\d{1,2} [A-Za-z]{3,} \d{4}$',               'dd MMMM yyyy';
      '^\d{1,2}:\d{2}:\d{2}$',                     'HH:mm:ss';
      '^\d{1,2}:\d{2}$',                            'HH:mm';
      '^\d{4}$',                                    'yyyy'};
  endif
  fmt = '';
  first = strs{1};
  idx = 0;
  for k = 1:rows (shapes)
    if (! isempty (regexp (first, shapes{k,1}, 'once')))
      idx = k;
      break;
    endif
  endfor
  if (idx == 0)
    return;
  endif
  fmt = shapes{idx,2};
  if (isempty (fmt))
    fmt = dtSlashOrder (strs);
  elseif (fmt(end) == '.')
    ## Widen the fractional field to the digits the text actually carries.
    frac = regexp (first, '\.(\d+)$', 'tokens', 'once');
    fmt = [fmt, repmat('S', 1, numel (frac{1}))];
  endif
endfunction

## Decide whether a set of day/month/year strings separated by slashes is
## written the American way round or the European one.  A first field past 12
## can only be a day and a second field past 12 can only be a month, so one such
## string settles the whole set; with nothing to go on, MATLAB reads them the
## American way and says so.
function fmt = dtSlashOrder (strs)
  first = regexp (strs, '^(\d{1,2})/(\d{1,2})/', 'tokens', 'once');
  f1 = cellfun (@(t) str2double (t{1}), first);
  f2 = cellfun (@(t) str2double (t{2}), first);
  if (any (f1 > 12))
    fmt = 'dd/MM/yyyy';
  elseif (any (f2 > 12))
    fmt = 'MM/dd/yyyy';
  else
    fmt = 'MM/dd/yyyy';
    warning ('Octave:datetime:ambiguous-format', ...
             ["datetime: the text was read with the format 'MM/dd/yyyy'," ...
              " but 'dd/MM/yyyy' would read it just as well; give" ...
              " 'InputFormat' to say which is meant."]);
  endif
endfunction

## Parse a cell array of date/time strings under an LDML 'InputFormat',
## returning an N-by-6 date-vector matrix.  The work is done by the compiled
## __ldml__ helper: the format is tokenized like a display format, literals
## must match, and each field consumes characters from the string -- numeric
## fields take digits (up to the field's natural width, or exactly the run
## length when butted against another numeric field), name fields (MMM/MMMM,
## the weekday names and the day period 'a') take letters, matched case- and
## accent-insensitively against the locale tables.  Two-digit years are
## resolved against PIVOT.  Fields absent from the format default to the
## current date (year/month/day) or to zero (time), matching MATLAB.
function DV = dtParseInput (strs, fmt, pivot, locale, leapok = false)
  ## An unset 'Locale' arrives as [], which the helper reads as English.
  if (isempty (locale))
    locale = '';
  endif
  DV = __ldml__ ('parse', strs, fmt, pivot, locale, leapok);
endfunction

## Render each element of a datetime array to a display string under a
## concrete LDML pattern.  Y..S are the wall-clock component arrays; TZ is the
## time zone ('' for unzoned); FMT is a concrete pattern already resolved by
## dtResolveFormat.  NaT elements render as 'NaT' and infinite years as
## '-Inf'/'Inf', matching the component-store sentinels.
function cstr = dtFormatStrings (Y, M, D, H, Mi, S, TZ, OFF, fmt, zoneStyle)
  ## The zone-dependent quantities still come from the compiled tz database;
  ## the rendering itself is done by __ldml__.  Read them only when the format
  ## actually names a zone field: the 'z' name field needs both the
  ## abbreviation (mode 'iana', and the whitelist test of mode 'matlab') and
  ## the offset (the mode 'matlab' fallback).
  syms = __ldml__ ('symbols', fmt);
  hasZ = any (syms == 'z');
  hasTZ = ! isempty (TZ);
  needOff = (any (ismember (syms, 'ZXx')) || hasZ) && hasTZ;
  needAbbr = hasZ && hasTZ;
  off = [];
  abbr = {};
  if (needOff)
    ## The array carries its own offsets, so the numeric zone fields are read
    ## off them rather than resolved from the wall clock a second time.
    off = OFF + zeros (size (Y));
  endif
  if (needAbbr)
    ## The name has to be looked up, and looking it up by wall clock names the
    ## later of a repeated pair, so each element is first put in a moment that
    ## is unambiguous and in its own regime -- 'EDT' for the pass that is still
    ## on daylight saving, 'EST' for the one that is not.
    [aY, aM, aD, aH, aMi, aS] = dtOwnFoldClock (Y, M, D, H, Mi, S, OFF, TZ);
    abbr = dtZoneAbbrev (aY, aM, aD, aH, aMi, aS, TZ);
  endif
  cstr = __ldml__ ('format', Y, M, D, H, Mi, S, fmt, zoneStyle, hasTZ, ...
                   off, abbr);
endfunction

## Wall clock of each element moved, where it has to be, into a moment that
## names it unambiguously and lies in the same daylight-saving regime it is
## already in.  Elements whose clock names one moment are returned untouched.
##
## The tz database is asked questions -- is daylight saving in force, what is
## this zone called right now -- by wall clock, and on the day a clock is put
## back one wall clock names two moments.  Every such query therefore answers
## for the later of the two, which is the wrong answer for exactly the elements
## the stored offset was introduced to keep.  Those are the ones whose offset
## differs from the resolved one, and the difference is the length of the
## repeated window itself, so a clock stepped back by it leaves the window
## through the near end and lands in the regime that was still in force.  It is
## then unambiguous and the ordinary query answers for it correctly.
##
## Taking the length from the two offsets rather than assuming an hour is what
## makes this hold on Lord Howe (half an hour) and Chatham (three quarters),
## and asking the database after the step rather than reasoning that the
## earlier pass "is" the daylight one is what makes it hold in Ireland, where
## the database counts winter as the saving period and summer as standard.
function [Y, M, D, H, Mi, S] = dtOwnFoldClock (Y, M, D, H, Mi, S, off, TZ)
  if (isempty (TZ))
    return;
  endif
  d = off - dtZoneOffset (Y, M, D, H, Mi, S, TZ);
  amb = isfinite (d) & d != 0;
  if (! any (amb(:)))
    return;
  endif
  nai = __datetime__ (Y, M, D, H, Mi, S, 'ConvertTo', 'posixtime', ...
                      'TimeZone', 'UTC', 'Precision', 'microseconds');
  [bY, bM, bD, bH, bMi, bS] = __datetime__ (nai - d, 'ConvertFrom', ...
                              'posixtime', 'Precision', 'microseconds');
  Y(amb) = bY(amb); M(amb) = bM(amb); D(amb) = bD(amb);
  H(amb) = bH(amb); Mi(amb) = bMi(amb); S(amb) = bS(amb);
endfunction

## UTC offset (seconds east of UTC, negative west of Greenwich) for each
## element of a zoned datetime, derived without any new compiled support:
## reading the wall-clock components as if they were UTC and subtracting the
## true instant yields the local offset.
function off = dtZoneOffset (Y, M, D, H, Mi, S, TZ)
  utcAsIf = __datetime__ (Y, M, D, H, Mi, S, 'ConvertTo', 'posixtime', ...
                          'TimeZone', 'UTC', 'Precision', 'microseconds');
  instant = __datetime__ (Y, M, D, H, Mi, S, 'ConvertTo', 'posixtime', ...
                          'TimeZone', TZ, 'Precision', 'microseconds');
  off = utcAsIf - instant;
endfunction

## Zone abbreviation (e.g. 'EDT', 'EST', 'UTC') for each element of a zoned
## datetime, from the compiled tz database via the __datetime__ builtin.
function ab = dtZoneAbbrev (Y, M, D, H, Mi, S, TZ)
  ab = __datetime__ (Y, M, D, H, Mi, S, 'ConvertTo', 'zoneabbrev', ...
                     'TimeZone', TZ, 'Precision', 'microseconds');
endfunction

## Logical daylight-saving-time flag for each element of a zoned datetime,
## from the compiled tz database via the __datetime__ builtin.
function tf = dtIsDst (Y, M, D, H, Mi, S, TZ)
  tf = logical (__datetime__ (Y, M, D, H, Mi, S, 'ConvertTo', 'isdst', ...
                              'TimeZone', TZ, 'Precision', 'microseconds'));
endfunction

## Parse text into an N-by-6 date-vector matrix for a leap-second array with no
## 'InputFormat'.  The only shape accepted is the one such an array displays: an
## ISO 8601 UTC instant, with the fractional seconds optional and of any width.
## A string that does not match cannot be read, and since there is no format to
## name in the message, the whole input is rejected -- as it is when no format
## can be detected at all.  The seconds field is left as parsed, 60 included;
## the caller decides whether that second was really inserted.
function DV = dtParseLeapText (strs)
  pat = '^(-?\d+)-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d+)?)Z$';
  tok = regexp (strs, pat, 'tokens', 'once');
  if (any (cellfun (@isempty, tok)))
    error (strcat ("datetime: text for a 'UTCLeapSeconds' datetime array", ...
                   " must be an ISO 8601 UTC instant, as in", ...
                   " '2016-12-31T23:59:60Z', optionally with fractional", ...
                   " seconds."));
  endif
  DV = cellfun (@(t) str2double (t(:).'), tok, 'UniformOutput', false);
  DV = cell2mat (DV(:));
endfunction

## The display format a leap-second array carries.  It is a concrete pattern,
## not one of the data-dependent sentinels: the array must be able to render a
## 60th second, and only this ISO 8601 UTC pattern does.
function fmt = dtLeapFormat ()
  fmt = "uuuu-MM-dd'T'HH:mm:ss.SSS'Z'";
endfunction

## Validate a Format for a leap-second array.  Only the ISO 8601 UTC pattern is
## allowed, optionally with one to nine fractional-second digits; MATLAB rejects
## everything else, the sentinels included.  OP names the caller.
function dtValidateLeapFormat (fmt, op)
  ok = ischar (fmt) && isrow (fmt) ...
       && ! isempty (regexp (fmt, "^uuuu-MM-dd'T'HH:mm:ss(\\.S{1,9})?'Z'$", ...
                             'once'));
  if (! ok)
    error (strcat ("%s: the display format of a 'UTCLeapSeconds' datetime", ...
                   " array must be \"uuuu-MM-dd'T'HH:mm:ss'Z'\",", ...
                   " optionally with one to nine fractional second digits,", ...
                   " as in \"uuuu-MM-dd'T'HH:mm:ss.SSSSSSSSS'Z'\"."), op);
  endif
endfunction

## Enforce MATLAB's rule that an array with leap seconds cannot be combined
## with, or compared against, one without them -- not even a UTC array.  The two
## do not count the same seconds, so there is no shared timeline on which the
## operation would mean anything.  Callers make the zoned/unzoned check first,
## as MATLAB does.
function dtCheckLeapPair (A, B, op)
  if (dtIsLeapZone (A.TimeZone) != dtIsLeapZone (B.TimeZone))
    error (strcat ("datetime.%s: cannot combine or compare a datetime", ...
                   " array with leap seconds with one without leap", ...
                   " seconds."), op);
  endif
endfunction

## Split the constructor's positional numeric arguments into six component
## arrays, which is how the leap-second normaliser needs them: a single date
## vector matrix carries one element per row, separate arrays broadcast against
## each other, and a seventh argument holds milliseconds.  The shapes have
## already been validated by the builtin when this is called.
function [Y, M, D, h, mi, sec] = dtSplitComponents (args)
  if (numel (args) == 1)
    V = args{1};
    Y = V(:,1);  M = V(:,2);  D = V(:,3);
    if (columns (V) >= 6)
      h = V(:,4);  mi = V(:,5);  sec = V(:,6);
    else
      h = zeros (size (Y));  mi = h;  sec = h;
    endif
  else
    Y = args{1};  M = args{2};  D = args{3};
    if (numel (args) >= 6)
      h = args{4};  mi = args{5};  sec = args{6};
    else
      h = 0;  mi = 0;  sec = 0;
    endif
    if (numel (args) >= 7)
      sec = sec + args{7} / 1000;
    endif
  endif
endfunction

## True when TZ names the leap-second time zone.  'UTCLeapSeconds' is UTC with
## the inserted seconds made representable, and is the only time zone in which
## the 60th second of a minute exists.  It is not an IANA zone and, as in
## MATLAB, 'timezones' does not list it, but it is accepted wherever a zone is.
function tf = dtIsLeapZone (TZ)
  tf = ! isempty (TZ) && strcmp (TZ, 'UTCLeapSeconds');
endfunction

## POSIX time of each leap-second insertion in the shipped tz database, that is,
## of the first instant after each inserted second (78796800 for the second
## inserted at the end of 1972-06-30).  Returned as a row vector so the tests
## below broadcast against a column of instants.  Read once and cached: the
## table only changes when the package's tzdata does.
function ins = dtLeapInsertions ()
  persistent tbl = [];
  if (isempty (tbl))
    tbl = __datetime__ ('leapseconds')(:).';
  endif
  ins = tbl;
endfunction

## Position of each inserted second on the continuous SI-second timeline of
## 'dtLeapSerial'.  The n-th insertion is preceded by n-1 earlier ones, each of
## which pushed the timeline one second ahead of POSIX time, so it begins that
## many seconds after its own POSIX time.
function S = dtLeapStarts ()
  ins = dtLeapInsertions ();
  S = ins + (0:numel (ins) - 1);
endfunction

## POSIX time of the start of the minute named by the components, resolving any
## overflow above the seconds field by ordinary leap-free calendar arithmetic.
## The seconds are deliberately left out of the call: they may hold 60, which
## the C++ normaliser would roll into the next minute.  This is the seam that
## makes MATLAB's split behaviour fall out -- an hour or minute that overflows
## steps over an inserted second (23:60:00 on a leap day is the next midnight),
## while the seconds field is counted on the leap-second timeline by the callers
## below.
function p = dtLeapMinutePosix (Y, M, D, h, mi, z)
  p = __datetime__ (Y + z, M + z, D + z, h + z, mi + z, z, 'ConvertTo', ...
                    'posixtime', 'TimeZone', 'UTC', 'Precision', ...
                    'microseconds');
endfunction

## Continuous count of SI seconds since 1970-01-01T00:00:00 UTC for a POSIX
## instant: POSIX time plus every second inserted up to it.  An insertion is
## stamped with the POSIX time of the instant after it, so an instant equal to
## that stamp already has the inserted second behind it.
function s = dtPosix2Leap (p)
  ins = dtLeapInsertions ();
  s = p + reshape (sum (p(:) >= ins, 2), size (p));
endfunction

## Continuous count of SI seconds since 1970-01-01T00:00:00 UTC for leap-second
## wall-clock components.  Every UTC day contributes its true length, 86401
## seconds on a day that ends with an inserted second, so the count is injective
## across an inserted second and strictly increasing through it.  That is what
## lets every instant-based operation of the class -- arithmetic, comparison,
## sorting, set membership, interpolation -- count inserted seconds without
## knowing they exist.  Not-A-Time maps to NaN and infinite elements keep their
## sign.
function s = dtLeapSerial (Y, M, D, h, mi, sec)
  z = zeros (size (Y + M + D + h + mi + sec));
  s = dtPosix2Leap (dtLeapMinutePosix (Y, M, D, h, mi, z)) + (sec + z);
endfunction

## POSIX time of leap-second wall-clock components.  POSIX time cannot name an
## inserted second, and MATLAB resolves that by folding forward here: the 60th
## second of a minute shares its stamp with the following second, so 23:59:60
## reads as the next midnight.  Every other conversion folds backward instead
## (see dtLeapBackFold), which is not an inconsistency: the tz project's own
## reference conversion, time2posix(3), folds POSIX forward the same way, while
## C++20's utc_clock, Rust's chrono and java.time all fold backward.
function p = dtLeapPosix (Y, M, D, h, mi, sec)
  z = zeros (size (Y + M + D + h + mi + sec));
  p = dtLeapMinutePosix (Y, M, D, h, mi, z) + (sec + z);
endfunction

## Seconds field folded backward, that is, mapped to the last second that is not
## an inserted one.  The whole second drops by one and any fractional part is
## kept, so 23:59:60.25 is read as 23:59:59.25.  This is the rule every
## conversion other than 'posixtime' follows.
function sec = dtLeapBackFold (sec)
  sec = sec - (sec >= 60);
endfunction

## Inverse of 'dtLeapSerial': map a continuous SI-second count back to
## leap-second wall-clock components.  A count inside an inserted second yields
## the 60th second of its minute; any other count is an ordinary POSIX time once
## the whole seconds inserted before it have been taken back out.
function [Y, M, D, h, mi, sec] = dtLeapComponents (ser)
  S = dtLeapStarts ();
  sz = size (ser);
  v = ser(:);
  inLeap = any (v >= S & v < S + 1, 2);
  ## Reading an inserted second as the 59th second of its minute and adding one
  ## afterwards keeps any fractional part, and needs no leap-day lookup.
  posix = v - sum (v >= S + 1, 2) - inLeap;
  [Y, M, D, h, mi, sec] = __datetime__ (reshape (posix, sz), 'ConvertFrom', ...
                                        'posixtime', 'Precision', ...
                                        'microseconds');
  inLeap = reshape (inLeap, sz);
  sec(inLeap) += 1;
endfunction

## Re-canonicalise leap-second wall-clock components: read them as a count of
## SI seconds and read that count back.  Because the seconds field is counted on
## the leap-second timeline while the fields above it are not, this reproduces
## MATLAB's whole rule at once.  A 60th second stays put on a day that ends with
## an inserted second (2016-12-31 23:59:60) and rolls over on any other day or
## minute (2016-12-30 23:59:60 and 2016-12-31 23:58:60 both become the next
## minute); one second past it rolls to the next midnight (23:59:61 is
## 2017-01-01 00:00:00, not 00:00:01, because the minute really did hold 61
## seconds); one second before a midnight that follows an insertion lands on the
## inserted second; and a minute or hour that overflows steps over the insertion
## entirely (23:60:00 is the next midnight).
function [Yo, Mo, Do, ho, mio, so] = dtLeapNormalize (Y, M, D, h, mi, sec)
  [Yo, Mo, Do, ho, mio, so] = dtLeapComponents (dtLeapSerial (Y, M, D, h, ...
                                                              mi, sec));
endfunction

## Julian or modified Julian date of leap-second wall-clock components, under
## the stretched-day rule: the day number of the scale at hand, plus the SI
## seconds elapsed since that day began divided by the day's true length, 86401
## seconds on a day that holds an inserted second.  Julian days begin at noon
## and modified Julian days at midnight, so the two scales stretch different
## spans and on such a day differ by more than the usual 2400000.5.
function out = dtLeapJulian (Y, M, D, h, mi, sec, doModified)
  z = zeros (size (Y + M + D + h + mi + sec));
  Yv = Y + z;  Mv = M + z;  Dv = D + z;
  ser = dtLeapSerial (Y, M, D, h, mi, sec);
  if (doModified)
    ## Modified Julian day 40587 begins at midnight on 1970-01-01.
    dayNum = datenum (Yv, Mv, Dv) - datenum (1970, 1, 1) + 40587;
    hourOfDay = 0;
  else
    ## Julian day 2440588 begins at noon on 1970-01-01, so an element before
    ## noon still belongs to the Julian day that opened at the previous noon.
    dayNum = datenum (Yv, Mv, Dv) - datenum (1970, 1, 1) + 2440588;
    hourOfDay = 12;
    early = ser < dtLeapSerial (Yv, Mv, Dv, 12, 0, 0);
    dayNum(early) -= 1;
    Dv(early) -= 1;
  endif
  ## Taking the day's length as the distance to the next day of the same scale
  ## picks up an inserted second wherever one falls inside it.
  dayStart = dtLeapSerial (Yv, Mv, Dv, hourOfDay, 0, 0);
  dayLen = dtLeapSerial (Yv, Mv, Dv + 1, hourOfDay, 0, 0) - dayStart;
  out = dayNum + (ser - dayStart) ./ dayLen;
endfunction

## TAI-UTC offset, in seconds, of an instant before 1972-01-01, given as a POSIX
## time (which is also the SI-second count there, no second having been inserted
## yet).  Until 1972 UTC was kept near UT1 by running its seconds at a slightly
## different rate rather than by inserting whole ones, so the offset of that era
## is tabulated by the IERS as a base value plus a rate per day, referred to a
## Modified Julian Date.  Before 1960 there is no table and the offset is zero.
##
## The table is followed as published.  MATLAB's tt2000 disagrees across these
## twelve years by up to 1.3 ms, its rate evidently evaluated half a day from
## where the table refers it -- the discrepancy is exactly half a day's worth of
## each row's rate.  Being bug-compatible here would mean writing an arithmetic
## error into a published table, so it is not done; documented in convertTo.
## Outside 1960-1972 the two agree exactly, every probed anchor from 1707 to
## 2292 included.
function dAT = dtDeltaATPre (p)
  ## POSIX time from which the row applies, base offset in seconds, the MJD the
  ## base is referred to, and the rate in seconds per day.
  persistent T = [ -315619200, 1.4178180, 37300, 0.0012960;
                   -283996800, 1.4228180, 37300, 0.0012960;
                   -265680000, 1.3728180, 37300, 0.0012960;
                   -252460800, 1.8458580, 37665, 0.0011232;
                   -194659200, 1.9458580, 37665, 0.0011232;
                   -189388800, 3.2401300, 38761, 0.0012960;
                   -181526400, 3.3401300, 38761, 0.0012960;
                   -168307200, 3.4401300, 38761, 0.0012960;
                   -157766400, 3.5401300, 38761, 0.0012960;
                   -152668800, 3.6401300, 38761, 0.0012960;
                   -142128000, 3.7401300, 38761, 0.0012960;
                   -136771200, 3.8401300, 38761, 0.0012960;
                   -126230400, 4.3131700, 39126, 0.0025920;
                    -60480000, 4.2131700, 39126, 0.0025920];
  mjd = p / 86400 + 40587;
  dAT = zeros (size (p));
  for k = 1:rows (T)
    in = p >= T(k,1);
    dAT(in) = T(k,2) + (mjd(in) - T(k,3)) * T(k,4);
  endfor
endfunction

## Correction that carries the SI-second count of an instant before 1972 onto
## the scale tt2000 is measured on.  From 1972-01-01 the offset from TAI is the
## ten seconds UTC started out behind plus every second inserted since, and the
## count already carries both, so the epoch constant alone does the work and the
## correction is zero.  Earlier there were no inserted seconds and no ten-second
## head start either, so the tabulated offset of the era replaces them.
function corr = dtTT2000PreCorr (ser)
  corr = zeros (size (ser));
  early = ser < 63072000;                  # 1972-01-01T00:00:00Z
  if (any (early(:)))
    corr(early) = dtDeltaATPre (ser(early)) - 10;
  endif
endfunction

## Nanoseconds since the tt2000 epoch for a continuous SI-second count.  The
## epoch is the J2000 Terrestrial Time epoch, 2000-01-01T11:58:55.816Z, which is
## SI second 946727957.816 of the leap-second timeline (22 seconds had been
## inserted by then).  The arithmetic is carried on a whole-second part and a
## nanosecond part because the product overflows a double, whose 53-bit mantissa
## runs out at 2^53 nanoseconds -- about 104 days -- long before an int64 does.
function out = dtSerial2TT2000 (ser)
  ser = ser + dtTT2000PreCorr (ser);
  sw = floor (ser);
  dw = sw - 946727957;
  dn = round ((ser - sw) * 1e6) * 1000 - 816000000;
  ## Carry so that the nanosecond part stays in [0, 1e9).
  borrow = dn < 0;
  dw(borrow) -= 1;
  dn(borrow) += 1e9;
  ## An int64 spans -9223372037 s + 145224192 ns to 9223372036 s + 854775807 ns.
  ok = isfinite (ser) ...
       & (dw > -9223372037 | (dw == -9223372037 & dn >= 145224192)) ...
       & (dw <  9223372036 | (dw ==  9223372036 & dn <= 854775807));
  if (! all (ok(:)))
    error (strcat ("datetime.convertTo: 'tt2000' conversion is not", ...
                   " supported for missing values, infinite datetimes,", ...
                   " or datetimes outside the interval", ...
                   " [1707-09-22T12:12:10Z, 2292-04-11T11:46:08Z)."));
  endif
  ## Assemble the int64 without letting an intermediate overflow: below the
  ## epoch the whole-second part alone already saturates, so borrow a second
  ## from it first and take the remainder off afterwards.
  out = zeros (size (ser), 'int64');
  lo = dw < 0;
  out(lo) = (int64 (dw(lo)) + 1) * 1000000000 - (1000000000 - int64 (dn(lo)));
  out(! lo) = int64 (dw(! lo)) * 1000000000 + int64 (dn(! lo));
endfunction

## Continuous SI-second count of a tt2000 nanosecond value; the inverse of
## 'dtSerial2TT2000'.  The int64 is split before it is widened, since converting
## it whole to a double would lose the nanoseconds.
function ser = dtTT20002Serial (ns)
  if (any (ns(:) <= int64 (-9223372036854775805)))
    error (strcat ("datetime: int64 input values for tt2000 times must be", ...
                   " larger than -9223372036854775805."));
  endif
  dw = double (idivide (ns, int64 (1000000000), 'floor'));
  dn = double (mod (ns, int64 (1000000000)));
  w = 946727957 + dw;
  us = 816000 + round (dn / 1000);
  carry = us >= 1e6;
  w(carry) += 1;
  us(carry) -= 1e6;
  ser = w + us / 1e6;
  ## Undo the pre-1972 correction.  It depends on the instant it is applied to,
  ## but only through a rate of a few milliseconds per year, so a couple of
  ## passes settle it far below the microsecond the class stores.
  early = ser < 63072000;
  if (any (early(:)))
    target = ser(early);
    s = target;
    for k = 1:3
      s = target - (dtDeltaATPre (s) - 10);
    endfor
    ser(early) = s;
  endif
endfunction

## Clamp a seconds field that names an inserted second onto a minute that has
## none.  Calendar arithmetic preserves the wall clock, so adding a month or a
## calendar day to 2016-12-31 23:59:60 asks for a 60th second of a minute that
## never had one; MATLAB clamps it to the 59th, just as it clamps the day of the
## month when a month is added to the 31st.  P is the POSIX time of the start of
## each target minute.
function sec = dtLeapClampSecond (sec, p)
  ins = dtLeapInsertions ();
  keep = reshape (any (p(:) + 60 == ins, 2), size (p));
  sec(sec >= 60 & ! keep) -= 1;
endfunction

## Integer fixed-epoch conversions (NTP, NTFS/FILETIME, .NET) for convertTo.
## POSIX is the absolute UTC instant in seconds (from the 'serial' proxy).  The
## result is built from integer milliseconds through a whole-second/fraction
## split, keeping every intermediate below 2^53 so the final uint64 product is
## exact.  Missing, infinite, or out-of-range instants raise an error because
## the integer formats cannot represent NaN.
function out = dtFixedEpoch (posix, kind)
  switch (kind)
    case 'ntp'
      offset = 2208988800;   scale = 4294967296;  hiLimit = 4294967296;
      tname = 'NTP';
    case 'ntfs'
      offset = 11644473600;  scale = 10000000;    hiLimit = 1844674407370;
      tname = 'NTFS';
    case 'dotnet'
      offset = 62135596800;  scale = 10000000;    hiLimit = 1844674407370;
      tname = '.NET';
  endswitch
  ms = round (posix * 1000);
  secWhole = floor (ms / 1000) + offset;
  fracMs = ms - floor (ms / 1000) * 1000;
  ok = isfinite (ms) & secWhole >= 0 & secWhole < hiLimit;
  if (! all (ok(:)))
    error (strcat ("datetime.convertTo: '%s' conversion is not supported for", ...
                   " missing values, infinite datetimes, or datetimes outside", ...
                   " the representable range."), tname);
  endif
  frac = round (fracMs .* scale ./ 1000);
  out = uint64 (secWhole) .* uint64 (scale) + uint64 (frac);
endfunction


## Bin edges for a requested bin count, snapped to whole calendar or clock
## units.  XMIN, XMAX and the result all count seconds on the 'serial'
## timeline and NBINS is the number of bins asked for.  SER2CAL and CAL2SER
## carry the calendar: 'SER2CAL (s)' returns the year, month and day holding the
## instant S, and 'CAL2SER (Y, M, D)' the instant at which that date begins,
## both in the array's own time zone.  They are handed in rather than worked out
## here because only the class knows its zone, its leap seconds and its epoch.
## TICK is the half-width used when the data are constant and defaults to half a
## second, where a 'duration' uses half a millisecond.
##
## This is NOT the placement 'duration' uses and 'gridbinedges', its own local
## function, must not be borrowed for it.  Three differences separate them,
## each verified against
## R2024a and each of which on its own yields edges that look plausible and are
## wrong:
##
##   * the left edge is fixed BEFORE the width; 'duration' fixes the width first
##   * the provisional width rounds up STRICTLY (floor + 1, never ceil), so a
##     span that divides exactly still widens
##   * the acceptance test applies to the FINAL left edge and width, not to the
##     provisional one, which is what decides the hour/day boundary
##
## The ladder runs year, month, day, hour, minute, second, largest first, and
## the first unit that fits wins.  There is no week rung and no separate
## quarter, decade or century: a quarter is a three-month width and a decade a
## ten-year one, both of which fall out of the multiples.  Below one second no
## unit is left and the plain numeric rule takes over.
##
## The ladder is split, and the split is not where it looks.  Year, month AND
## DAY are true calendar units: their edges land on 1 January, on the 1st, and
## on local midnight, so a bin holding a daylight-saving transition is 23 or 25
## hours long while its neighbours are 24.  Only hour, minute and second are
## fixed spans of seconds, and they stay fixed across a transition -- an hour
## bin spanning a spring-forward covers two wall-clock hours.  Both halves are
## measured: in America/New_York a 5-day span gives bins of 48, 48 and 47 hours
## with every edge at local midnight, and a 1-day span at 3 bins gives 9-hour
## bins whose wall clock jumps from 20:00 to 06:00 across the gap.
##
## Every rung runs the same arithmetic, in 'dtBinGridIndex', on the INTEGER
## count of units between the one holding XMIN and the one holding XMAX.  It is
## that integer count, and never the elapsed time, that drives the width and
## the acceptance test: 2024-01-01 to 2024-12-31 is eleven months here and
## 11.97 by elapsed time, and eleven is what reproduces R2024a.  The same point
## in a different guise is that XMIN's position inside its unit reaches the
## result only through the unit it lands in, so sliding the data through a day
## moves where the bins sit but never how wide they are or which unit they use.
function edges = dtBinEdgesGrid (xmin, xmax, nbins, ser2cal, cal2ser, ...
                                 tick = 0.5)

  ## Constant data: an interval of one tick centred on the value.  The offsets
  ## are formed first and added once, rather than subtracting the tick and then
  ## stepping, so that only one rounding lands on a serial of order 1e9.
  if (xmin == xmax)
    edges = xmin + (-tick + (0:nbins) * (2 * tick / nbins));
    return;
  endif

  [yLo, mLo, dLo] = ser2cal (xmin);
  [yHi, mHi, dHi] = ser2cal (xmax);

  ## Calendar years.
  idx = dtBinGridIndex (yHi - yLo, nbins);
  if (! isempty (idx))
    one = ones (size (idx));
    edges = cal2ser (yLo + idx, one, one);
    return;
  endif

  ## Calendar months.
  idx = dtBinGridIndex ((12 * yHi + mHi) - (12 * yLo + mLo), nbins);
  if (! isempty (idx))
    one = ones (size (idx));
    [Y, M, D] = dtAddMonths (yLo * one, mLo * one, one, idx);
    edges = cal2ser (Y, M, D);
    return;
  endif

  ## Calendar days.  The count of days between the two dates is recovered by
  ## rounding the gap between their midnights: a daylight-saving shift moves
  ## those by an hour or two and an inserted leap second by one, never by
  ## anything approaching half a day.
  spanU = round ((cal2ser (yHi, mHi, dHi) - cal2ser (yLo, mLo, dLo)) / 86400);
  idx = dtBinGridIndex (spanU, nbins);
  if (! isempty (idx))
    one = ones (size (idx));
    [Y, M, D] = dtAddDays (yLo * one, mLo * one, dLo * one, idx);
    edges = cal2ser (Y, M, D);
    return;
  endif

  ## Fixed units: one hour, one minute, one second.  These are anchored on the
  ## LOCAL clock -- the midnight of the day holding XMIN -- and then stepped by
  ## a fixed span of seconds.  Flooring XMIN itself would anchor on absolute
  ## time, which lands the edges on the half hour in a zone offset by one
  ## (Asia/Kolkata, +05:30) and moves them off local midnight everywhere else.
  ## The two agree for a whole-hour zone with no transition in range, which is
  ## why every earlier probe missed it.
  [yA, mA, dA] = ser2cal (xmin);
  origin = cal2ser (yA, mA, dA);
  for g = [3600, 60, 1]
    nLo = floor ((xmin - origin) / g);
    idx = dtBinGridIndex (floor ((xmax - origin) / g) - nLo, nbins);
    if (! isempty (idx))
      edges = origin + g * (nLo + idx);
      return;
    endif
  endfor

  ## Finer than a second: no coarser unit is left to snap to, so the plain
  ## numeric rule takes over.  This is the terminating case of the ladder and
  ## not a separate regime.
  ##
  ## The origin is moved to the whole second below the data first.  That rule
  ## anchors the left edge on a decimal grid, and a serial runs to 1e9 seconds,
  ## where a double resolves only to about 2e-7 of a second -- far too coarse
  ## for a grid of, say, 0.4 s to land where it should.  The shift is exact
  ## (the span here is under a second) and MATLAB, which holds its instants in
  ## a different unit entirely, never meets the problem.
  origin = floor (xmin);
  edges = origin + __binedges__ (xmin - origin, xmax - origin, nbins);

endfunction

## Resolve the second argument of 'discretize' into explicit edges in serial
## seconds.  A datetime argument is handled by the caller, which is the only
## place that can convert one.  ISCOUNT reports a bin count, the one form whose
## edges MATLAB returns as a row whatever the shape of the input.
function [ev, isCount] = dtBinEdges (xv, arg2, s2c, c2s, scope)

  isCount = false;
  xf = xv(isfinite (xv));
  if (isa (arg2, 'duration') || isa (arg2, 'calendarDuration'))
    ev = dtUnitBinEdges (xf, arg2, s2c, c2s, scope);
  elseif (dtIsTextScalar (arg2))
    ev = dtUnitBinEdges (xf, char (arg2), s2c, c2s, scope);
  elseif (isnumeric (arg2) && isscalar (arg2) && ! islogical (arg2))
    if (! isreal (arg2) || ! isfinite (arg2) || arg2 < 1 || fix (arg2) != arg2)
      error ("%s: N must be a real positive integer.", scope);
    endif
    isCount = true;
    ev = dtCountBinEdges (xf, double (arg2), s2c, c2s, scope);
  elseif (isnumeric (arg2))
    error (strcat (scope, ": numeric bin edges are not accepted for a", ...
                   " datetime; give a datetime vector."));
  else
    error (strcat (scope, ": the second argument must be a datetime, a bin", ...
                   " count, a duration, a calendarDuration, or a unit name."));
  endif

endfunction

## True for a character vector or a scalar string
function tf = dtIsTextScalar (x)
  tf = (ischar (x) && isrow (x)) || (isa (x, 'string') && isscalar (x));
endfunction

## Day of the week as an offset from Sunday, 0 .. 6.
function k = dtWeekdayIndex (Y, M, D)
  k = weekday (datenum (Y, M, D)) - 1;
endfunction

## Classify a bin width into the kind of unit it steps in and how many of that
## unit one bin spans.  'fixed' counts seconds, 'day' whole days and 'month'
## whole months; a quarter is three months, a year twelve, a decade a hundred
## and twenty and a century twelve hundred, so one snapping rule serves them
## all.  SPEC is a scalar duration, a scalar calendarDuration, or a unit name.
function [kind, step, named] = dtUnitStep (spec, scope)

  ## NAMED distinguishes a unit given by name from one given as a width.
  ## They differ only when the data span an exact multiple of the unit:
  ## a named unit then opens one more bin, a width does not.  MATLAB
  ## makes that distinction for a datetime and not for a duration.
  named = ! (isa (spec, 'duration') || isa (spec, 'calendarDuration'));

  ## A unit NAME is accepted from every caller, including histcounts's
  ## 'BinWidth'.  That last one is an OCTAVE EXTENSION, deliberate and kept:
  ## MATLAB's 'BinWidth' takes only a duration or a calendarDuration and sends
  ## named units to 'BinMethod', which we also accept.  The two spellings are
  ## equivalent here, so refusing one buys the user nothing.  Documented in the
  ## histcounts docstring and in PENDING_NEWS.md.  Positionally -- discretize
  ## (T, 'day') -- a name is ordinary MATLAB behaviour, not an extension.
  if (! isa (spec, 'duration') && ! isa (spec, 'calendarDuration')
      && ! dtIsTextScalar (spec))
    error (strcat (scope, ": a bin width must be a scalar duration or", ...
                   " calendarDuration, or a unit name, and must be", ...
                   " positive and finite."));
  endif

  if (isa (spec, 'calendarDuration'))
    mo = calmonths (spec);
    dy = caldays (spec);
    sc = seconds (time (spec));
    if (! isscalar (spec) || ! all (isfinite ([mo, dy, sc]))
        || any ([mo, dy, sc] < 0) || ! any ([mo, dy, sc]))
      error (strcat (scope, ": a bin width must be positive and finite."));
    endif
    ## A MIXED width -- months and days together, or either with a time of
    ## day -- is accepted, as MATLAB accepts it; refusing it was an invention
    ## of ours.  Pure single-unit widths keep the branches they were verified
    ## on; only a mixture, or a calendarDuration carrying nothing but a time,
    ## needs the general grid.
    ## A month-bearing WIDTH anchors on 1 January of the data's year, which is
    ## not the 'month' branch's absolute grid: that one floors onto a multiple
    ## of the step counted from year zero, which a named 'decade' or 'century'
    ## needs (a decade must open on a year divisible by ten) but a width does
    ## not.  The two agree whenever the step divides 12, which is why only
    ## calyears (1) + calmonths (1), a step of 13, exposes the difference.
    if (sc == 0 && mo == 0)
      kind = 'day';   step = dy;
    else
      kind = 'cal';   step = [mo, dy, sc];
    endif
    return;
  endif

  if (isa (spec, 'duration'))
    kind = 'fixed';
    step = seconds (spec);
    if (! isscalar (step) || ! (step > 0) || ! isfinite (step))
      error (strcat (scope, ": a bin width must be positive and finite."));
    endif
    return;
  endif

  switch (lower (char (spec)))
    case 'second'
      kind = 'fixed'; step = 1;
    case 'minute'
      kind = 'fixed'; step = 60;
    case 'hour'
      kind = 'fixed'; step = 3600;
    case 'day'
      kind = 'day';   step = 1;
    case 'week'
      kind = 'week';  step = 7;
    case 'month'
      kind = 'month'; step = 1;
    case 'quarter'
      kind = 'month'; step = 3;
    case 'year'
      kind = 'month'; step = 12;
    case 'decade'
      kind = 'month'; step = 120;
    case 'century'
      kind = 'month'; step = 1200;
    otherwise
      error (strcat (scope, ": UNIT must be one of 'second', 'minute',", ...
                     " 'hour', 'day', 'week', 'month', 'quarter', 'year',", ...
                     " 'decade', or 'century'."));
  endswitch

endfunction

## Edges one unit wide covering the data, aligned to whole multiples of that
## unit.  Calendar units land on real boundaries: a day and a week on local
## midnight, a week on a Sunday, and a month, quarter, year, decade or century
## on the 1st of the month that starts the containing multiple.
##
## The bin count is 'floor (gap / step) + 1', so the largest value always opens
## a bin of its own rather than sitting on the closing edge -- 'day' bins over
## ten whole days give ten bins, not nine.  That is MATLAB's rule here and it
## differs from 'duration', which rounds the same quantity up instead.
function ev = dtUnitBinEdges (xf, spec, s2c, c2s, scope)

  [kind, step, named] = dtUnitStep (spec, scope);
  ## A named unit always opens a bin past the data; a width opens one
  ## only when the data do not end exactly on an edge.
  if (named)
    bins = @(gap) floor (gap / step) + 1;
  else
    bins = @(gap) max (1, ceil (gap / step));
  endif

  ## Empty data anchors on the epoch, for the reason given in dtCountBinEdges.
  if (isempty (xf))
    lo = 0;
    hi = 0;
  else
    lo = min (xf);
    hi = max (xf);
  endif

  switch (kind)
    case 'fixed'
      ## Anchored on local midnight, then stepped by a fixed span of seconds;
      ## see the note in dtBinEdgesGrid.
      [yA, mA, dA] = s2c (lo);
      if (mod (step, 86400) == 0)
        ## A duration width of whole days is anchored at local midnight on the
        ## 1st of the month holding the smallest element and stepped from
        ## there by a fixed span of seconds -- NOT from that element's own
        ## midnight.  The two agree unless a daylight-saving transition falls
        ## between the 1st and the data, which is why this is only visible in
        ## a zone such as Pacific/Chatham: seven 24-hour steps from 1 April
        ## 2024 land on the 7th at 23:00, the 7th being 25 hours long.
        ## Stepping in elapsed time is what puts days (2) and caldays (2) on
        ## the same opening edge away from a transition.
        origin = c2s (yA, mA, 1);
      else
        origin = c2s (yA, mA, dA);
      endif
      left = origin + step * floor ((lo - origin) / step);
      n = bins (hi - left);
      ev = left + (0:n) * step;

    case {'day', 'week'}
      [Y, M, D] = s2c (lo);
      ## Only the named 'week' unit starts on a Sunday.  A calendarDuration of
      ## seven days is a seven-day step on the day-of-month grid below, not a
      ## week grid, which is why the snap keys off the unit and not off the
      ## step: calweeks (1) over 10 June 2024 opens on Saturday the 8th.
      if (strcmp (kind, 'week'))
        [Y, M, D] = dtAddDays (Y, M, D, -dtWeekdayIndex (Y, M, D));
      elseif (step > 1)
        ## A width of several days lands on a grid anchored at the 1st of the
        ## month holding the smallest element, not on that element's own date:
        ## caldays (2) opens on an odd day of the month and caldays (3) on the
        ## 1st, 4th, 7th and so on.  The grid does not restart at a month
        ## boundary the bins happen to cross -- only the anchor is taken from
        ## the month.
        D = 1 + step * floor ((D - 1) / step);
      endif
      [Yh, Mh, Dh] = s2c (hi);
      gap = round ((c2s (Yh, Mh, Dh) - c2s (Y, M, D)) / 86400);
      n = bins (gap);
      ## GAP is measured midnight to midnight, so it discards HI's time of
      ## day.  A width that divides GAP exactly then closes on a last edge
      ## that the largest element has already passed, and that element falls
      ## outside every bin.  A named unit is immune, having opened its extra
      ## bin regardless.  One further step is always enough, the discarded
      ## remainder being under a day.
      if (! named)
        [Yn, Mn, Dn] = dtAddDays (Y, M, D, n * step);
        if (c2s (Yn, Mn, Dn) < hi)
          n += 1;
        endif
      endif
      k = (0:n) * step;
      one = ones (size (k));
      [Ye, Me, De] = dtAddDays (Y * one, M * one, D * one, k);
      ev = c2s (Ye, Me, De);

    case 'month'
      [Y, M, D] = s2c (lo);
      ## 12*Y is divisible by 3, 12, 120 and 1200 alike, so flooring the month
      ## index onto a multiple of STEP lands a quarter on January/April/July/
      ## October and a decade on a year divisible by ten, with no special case.
      idx = step * floor ((12 * Y + (M - 1)) / step);
      [Yh, Mh, Dh] = s2c (hi);
      n = bins ((12 * Yh + (Mh - 1)) - idx);
      ## The month index discards the day of the month, so a width needs the
      ## same guard as the day path above.
      if (! named)
        kn = idx + n * step;
        Yn = floor (kn / 12);
        if (c2s (Yn, kn - Yn * 12 + 1, 1) < hi)
          n += 1;
        endif
      endif
      k = idx + (0:n) * step;
      Ye = floor (k / 12);
      Me = k - Ye * 12 + 1;
      ev = c2s (Ye, Me, ones (size (k)));

    case 'cal'
      ## A mixed calendarDuration width.  The grid is anchored on 1 January of
      ## the smallest element's year when the width carries months, and on the
      ## 1st of its month when it does not, and steps by the WHOLE width --
      ## calendar part and time of day together.  The opening edge is the last
      ## grid point not past that element.  Because the anchor is always a 1st,
      ## no day-of-month clamping can occur, so the k-th point is computed
      ## directly rather than by stepping k times, and the two agree.
      [Y, M, D] = s2c (lo);
      if (step(1) != 0)
        M = 1;
      endif
      ## Mean Gregorian month, for the initial guess only; the walk below is
      ## what actually places the edge.
      slen = step(1) * 30.436875 * 86400 + step(2) * 86400 + step(3);
      k = max (0, floor ((lo - c2s (Y, M, 1)) / slen));
      while (k > 0 && dtCalGridPoint (Y, M, step, k, c2s) > lo)
        k -= 1;
      endwhile
      while (dtCalGridPoint (Y, M, step, k + 1, c2s) <= lo)
        k += 1;
      endwhile
      ev = dtCalGridPoint (Y, M, step, k, c2s);
      while (ev(end) < hi)
        k += 1;
        ev(end+1) = dtCalGridPoint (Y, M, step, k, c2s);
      endwhile
      if (numel (ev) < 2)
        ev(2) = dtCalGridPoint (Y, M, step, k + 1, c2s);
      endif
  endswitch

endfunction

## UTC offset in seconds for each element, derived from the WALL CLOCK, so an
## ambiguous time gets the later pass and one inside a gap the shifted one --
## resolve_local's choices.  Zero for an unzoned or leap-second array.  Used by
## every wall-clock operation; an instant-based one must take its offset from
## 'serial2components' instead, which reads it off the instant.
function off = dtOffsetOf (Y, M, D, h, m, s, tz)
  if (isempty (tz) || dtIsLeapZone (tz))
    off = zeros (size (Y));
    return;
  endif
  loc = __datetime__ (Y, M, D, h, m, s, 'ConvertTo', 'posixtime', ...
                      'TimeZone', tz, 'Precision', 'microseconds');
  nai = __datetime__ (Y, M, D, h, m, s, 'ConvertTo', 'posixtime', ...
                      'TimeZone', 'UTC', 'Precision', 'microseconds');
  off = round (nai - loc);
endfunction

## The K-th point of a mixed-calendarDuration bin grid, as a serial.  ANCHOR is
## the 1st of month M in year Y; STEP is [months days seconds].  The month part
## is applied first and the day part second, matching calendar addition, and the
## time of day is added last as an instant.  No clamping is possible because the
## anchor's day of month is 1.
function s = dtCalGridPoint (Y, M, step, k, c2s)
  tot = Y * 12 + (M - 1) + k * step(1);
  Yk = floor (tot / 12);
  [Yk, Mk, Dk] = dtAddDays (Yk, tot - Yk * 12 + 1, 1, k * step(2));
  s = c2s (Yk, Mk, Dk) + k * step(3);
endfunction

## Edges for a requested bin count over the data range.
function ev = dtCountBinEdges (xf, nbins, s2c, c2s, scope)

  if (! isnumeric (nbins) || ! isscalar (nbins) || ! isreal (nbins)
      || ! isfinite (nbins) || nbins < 1 || fix (nbins) != nbins)
    error (strcat (scope, ": 'NumBins' must be a real, finite, positive,", ...
                   " integer value."));
  endif
  nbins = double (nbins);
  if (isempty (xf))
    ## Deliberately unlike MATLAB, which answers an empty datetime with edges
    ## taken from the CURRENT CLOCK, so that the same call returns a different
    ## result every time it is run and no test of it can be written.  We anchor
    ## on the epoch, which is reproducible.  See section 14 of the coding style.
    ev = 0:nbins;
  else
    ev = dtBinEdgesGrid (min (xf), max (xf), nbins, s2c, c2s);
  endif

endfunction

## Bin count for one of the automatic bin-selection rules
function nbins = dtMethodBins (xf, lo, hi, method)

  n = numel (xf);
  if (n < 2 || isempty (lo) || hi == lo)
    nbins = 1;
    return;
  endif
  switch (method)
    case 'sturges'
      nbins = ceil (1 + log2 (n));
      return;
    case 'sqrt'
      nbins = ceil (sqrt (n));
      return;
    case 'fd'
      q = quantile (xf(:), [0.25, 0.75], 1, 5);
      rawWidth = 2 * (q(2) - q(1)) * n ^ (-1/3);
    otherwise
      rawWidth = 3.5 * std (xf(:)) * n ^ (-1/3);
  endswitch
  if (! (rawWidth > 0))
    nbins = 1;
  else
    nbins = max (1, ceil ((hi - lo) / rawWidth));
  endif

endfunction

## Clamp the outer edges into the requested limits.  MATLAB keeps the interior
## grid and cuts the two end bins short, so a 'BinLimits' of two data points
## returns edges that start and end exactly on them.
function ev = dtClampEdges (ev, lim)
  if (! isempty (lim))
    ev(1) = max (ev(1), lim(1));
    ev(end) = min (ev(end), lim(2));
  endif
endfunction

## Translate datetime-valued histcounts options into serial seconds, resolving
## any bin count, bin width or unit BinMethod into explicit edges so that every
## path goes through the same placement as 'discretize'.
function args = dtHistArgs (args, xv, s2c, c2s, d2s, scope)

  xf = xv(isfinite (xv));

  ## BinLimits decides the range everything else is derived from
  lim = [];
  for k = 1:numel (args) - 1
    if (dtIsTextScalar (args{k}) && strcmpi (char (args{k}), 'BinLimits'))
      v = args{k+1};
      if (isa (v, 'datetime'))
        v = d2s (v);
      endif
      if (isnumeric (v) && numel (v) == 2)
        lim = double (v(:)).';
      endif
    endif
  endfor
  if (! isempty (lim))
    xf = xf(xf >= lim(1) & xf <= lim(2));
    lo = lim(1);
    hi = lim(2);
  elseif (! isempty (xf))
    lo = min (xf);
    hi = max (xf);
  else
    lo = [];
    hi = [];
  endif

  ## A leading positional argument is a bin count or a set of edges.  Either
  ## way it is resolved here, and the option loop below starts past it.
  hasSpec = false;
  k = 1;
  if (! isempty (args) && ! dtIsTextScalar (args{1}))
    if (isa (args{1}, 'datetime'))
      dv = d2s (args{1});
      args{1} = dv(:).';
      hasSpec = true;
      k = 2;
    elseif (isnumeric (args{1}) && isscalar (args{1}) && ! islogical (args{1}))
      ev = dtClampEdges (dtCountBinEdges (xf, args{1}, s2c, c2s, scope), lim);
      args = [{'BinEdges', ev}, args(2:end)];
      hasSpec = true;
      k = 3;
    else
      k = 2;
    endif
  endif

  while (k < numel (args))
    if (! dtIsTextScalar (args{k}))
      k += 2;
      continue;
    endif
    switch (lower (char (args{k})))
      case 'numbins'
        args{k} = 'BinEdges';
        args{k+1} = dtClampEdges (dtCountBinEdges (xf, args{k+1}, s2c, c2s, ...
                                                   scope), lim);
        hasSpec = true;
      case 'binwidth'
        hasSpec = true;
        args{k} = 'BinEdges';
        args{k+1} = dtClampEdges (dtUnitBinEdges (xf, args{k+1}, s2c, c2s, ...
                                                   scope), lim);
      case 'binedges'
        hasSpec = true;
        if (isa (args{k+1}, 'datetime'))
          dv = d2s (args{k+1});
          args{k+1} = dv(:).';
        elseif (isnumeric (args{k+1}))
          error ("%s: '%s' must be a datetime.", scope, char (args{k}));
        endif
      case 'binlimits'
        if (isa (args{k+1}, 'datetime'))
          dv = d2s (args{k+1});
          args{k+1} = dv(:).';
        elseif (isnumeric (args{k+1}))
          error ("%s: '%s' must be a datetime.", scope, char (args{k}));
        endif
      case 'binmethod'
        if (dtIsTextScalar (args{k+1}))
          name = lower (char (args{k+1}));
          if (strcmp (name, 'integers'))
            error (strcat (scope, ": 'integers' is not a valid 'BinMethod'", ...
                           " for a datetime."));
          elseif (any (strcmp (name, {'auto', 'scott', 'fd', 'sturges', 'sqrt'})))
            args{k} = 'BinEdges';
            args{k+1} = dtClampEdges (dtCountBinEdges (xf, ...
                          dtMethodBins (xf, lo, hi, name), s2c, c2s, scope), lim);
          else
            args{k} = 'BinEdges';
            args{k+1} = dtClampEdges (dtUnitBinEdges (xf, name, s2c, c2s, ...
                                                       scope), lim);
          endif
        endif
        hasSpec = true;
      case 'normalization'
        if (dtIsTextScalar (args{k+1})
            && any (strcmpi (char (args{k+1}), {'countdensity', 'pdf'})))
          error (strcat (scope, ": '%s' is not a valid 'Normalization' for", ...
                         " a datetime; a density per unit time has no", ...
                         " meaning."), lower (char (args{k+1})));
        endif
    endswitch
    k += 2;
  endwhile

  ## Nothing said how to bin: use the automatic rule, which is a bin count from
  ## Scott's rule fed through the same grid edges as every other path
  if (! hasSpec)
    ev = dtClampEdges (dtCountBinEdges (xf, dtMethodBins (xf, lo, hi, 'auto'), ...
                                        s2c, c2s, scope), lim);
    args = [args, {'BinEdges', ev}];
  endif

endfunction

## Edge offsets for one rung of the ladder, in whole units.  SPANU is the count
## of units between the one holding the smallest value and the one holding the
## largest; the result is each edge's offset, in units, from the one holding the
## smallest value.  It is empty when this rung cannot be used, which is the
## signal to try the next one down.
##
## A rung is kept only when its width still needs every bin requested to span
## the data.  Rounding a width up to a unit that then leaves bins unused would
## both waste a bin and make the width jump about as the data change.
function idx = dtBinGridIndex (spanU, nbins)

  idx = [];

  ## The width is a strict round up of the span, so a span that divides
  ## exactly still widens.  Plain ceil () fails every case that divides
  ## exactly, and recomputing the width from the reach -- which looks like the
  ## natural thing to do -- is provably the same number for every NBINS >= 2
  ## and one unit too small at NBINS == 1, where it is the whole answer.
  width = floor (spanU / nbins) + 1;
  ## Centre the excess.  The reach the bins must cover is the span plus that
  ## shift; a rung too coarse to hold a whole unit of reach cannot be used.
  c = floor ((nbins * width - spanU) / 2);
  reach = spanU + c;
  if (reach < 1)
    return;
  endif

  if (ceil (reach / width) != nbins)
    return;
  endif

  idx = -c + (0:nbins) * width;

endfunction
