/*
Copyright (C) 2024-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>

This file is part of the datatypes package for GNU Octave.

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program; if not, see <http://www.gnu.org/licenses/>.
*/

#include <cmath>
#include <thread>
#include <iostream>
#include <locale>

#include <octave/oct.h>
#include <octave/parse.h>

#include "date/tz.h"

using namespace std;
using namespace date;

auto double2milli (double time_sec)
{
  using ds = chrono::duration<double>;
  local_time<ds> time{ds{time_sec}};
  auto tp = round<chrono::milliseconds>(time);
  return tp;
}

auto double2micro (double time_sec)
{
  using ds = chrono::duration<double>;
  local_time<ds> time{ds{time_sec}};
  auto tp = round<chrono::microseconds>(time);
  return tp;
}

auto double2nano (double time_sec)
{
  using ds = chrono::duration<double>;
  local_time<ds> time{ds{time_sec}};
  auto tp = round<chrono::nanoseconds>(time);
  return tp;
}

// A one-entry cache of the last regime found in a zone.  Looking a moment up
// in the tz database is by far the most expensive thing here -- an order of
// magnitude more than the component arithmetic around it, which is why a zoned
// conversion costs so much more than an unzoned one -- and the elements of a
// datetime array are nearly always close enough together to share a regime, so
// answering from the cache is what makes a long array cheap.  Each is a local
// variable of the loop that uses it, so nothing is shared between calls.
struct sys_cache
{
  const time_zone *tz = nullptr;
  sys_info si;
  bool valid = false;
};

// A moment names one regime, so the cached range is exactly the one the
// database reports and the cache is exact.
const sys_info&
cached_sys_info (const time_zone *tz, sys_seconds sys, sys_cache& zc)
{
  if (! zc.valid || zc.tz != tz || sys < zc.si.begin || sys >= zc.si.end)
  {
    zc.si = tz->get_info (sys);
    zc.tz = tz;
    zc.valid = true;
  }
  return zc.si;
}

struct local_cache
{
  const time_zone *tz = nullptr;
  local_info li;
  chrono::seconds lo{0}, hi{0};
  bool valid = false;
};

// A wall clock may name one moment, two, or none, and which of those it is can
// only be settled near a transition.  The cached answer is therefore kept only
// for clocks that name one moment, and only over the regime's range pulled in
// by a wide margin at each end, so anything within two days of a transition
// goes back to the database.  No transition has ever moved a clock by remotely
// that much, so the margin cannot hide an ambiguous or skipped clock.
const local_info&
cached_local_info (const time_zone *tz, local_time<chrono::microseconds> lt,
                   local_cache& lc)
{
  auto l = chrono::duration_cast<chrono::seconds> (lt.time_since_epoch ());
  if (lc.valid && lc.tz == tz && l >= lc.lo && l < lc.hi)
  {
    return lc.li;
  }
  lc.li = tz->get_info (lt);
  lc.tz = tz;
  lc.valid = false;
  if (lc.li.result == local_info::unique)
  {
    auto margin = chrono::hours {48};
    auto lo = lc.li.first.begin.time_since_epoch () + lc.li.first.offset
              + margin;
    auto hi = lc.li.first.end.time_since_epoch () + lc.li.first.offset
              - margin;
    if (lo < hi)
    {
      lc.lo = lo;
      lc.hi = hi;
      lc.valid = true;
    }
  }
  return lc.li;
}

// Resolve a wall-clock (local) time in 'timezone' to a zoned_time, applying
// MATLAB's rules for the two local times that do not name a unique instant.
// Where the clock goes back, the repeated hour is ambiguous and MATLAB takes
// the LATER offset, i.e. standard time.  Where the clock goes forward, the
// skipped interval does not exist and MATLAB shifts the wall clock forward by
// the length of the gap: 02:30 inside a one-hour gap becomes 03:30, and a
// half-hour gap (Australia/Lord_Howe) shifts by half an hour.  Shifting by the
// gap is the same as reading the wall clock at the pre-transition offset, and
// leaves a local time that is unique, so the make_zoned below cannot throw.
// Without this, date.h throws ambiguous_local_time or nonexistent_local_time,
// and the uncaught exception aborts the whole Octave session.
template <class Duration>
auto resolve_local (const string& timezone, local_time<Duration> lt)
{
  const time_zone *tz = locate_zone (timezone);
  auto info = tz->get_info (lt);
  if (info.result == local_info::nonexistent)
  {
    return make_zoned (tz, lt + (info.second.offset - info.first.offset));
  }
  else if (info.result == local_info::ambiguous)
  {
    return make_zoned (tz, lt, choose::latest);
  }
  return make_zoned (tz, lt);
}

auto from_to_tz_milli (double time_sec, string from_tzone, string to_tzone)
{
  auto tp = double2milli (time_sec);
  auto from = resolve_local (from_tzone, tp);
  auto to = make_zoned (to_tzone, from.get_sys_time ());
  return to;
}

auto from_to_tz_micro (double time_sec, string from_tzone, string to_tzone)
{
  auto tp = double2micro (time_sec);
  auto from = resolve_local (from_tzone, tp);
  auto to = make_zoned (to_tzone, from.get_sys_time ());
  return to;
}

auto from_to_tz_nano (double time_sec, string from_tzone, string to_tzone)
{
  auto tp = double2nano (time_sec);
  auto from = resolve_local (from_tzone, tp);
  auto to = make_zoned (to_tzone, from.get_sys_time ());
  return to;
}

RowVector seconds2vector (double time_sec, string precision)
{
  RowVector OUT(6);
  auto tp = double2micro (time_sec);
  auto day_tp = chrono::floor<days>(tp);
  hh_mm_ss time_tp{tp - day_tp};
  year_month_day date_tp{day_tp};
  OUT(0) = (int)date_tp.year();
  OUT(1) = (unsigned int)date_tp.month();
  OUT(2) = (unsigned int)date_tp.day();
  OUT(3) = time_tp.hours().count();
  OUT(4) = time_tp.minutes().count();
  OUT(5) = (double)time_tp.seconds().count() +
           (double)time_tp.subseconds().count() / 1000000;
  if (precision == "milliseconds")
  {
    OUT(5) = round (OUT(5) * 1000) / 1000;
  }
  return OUT;
}

// Components of a bare wall clock.  Split out of 'tz2vector' so that a caller
// which already holds the local time -- having added a known offset to a moment
// rather than built a zoned_time -- does not have to look the zone up again to
// get it back.
template <typename LocalType> RowVector
localtime2vector (const LocalType& t_local, string precision)
{
  RowVector OUT(6);
  auto today_local = chrono::floor<days>(t_local);
  hh_mm_ss time_local{t_local - today_local};
  year_month_day date_local{today_local};
  OUT(0) = (int)date_local.year();
  OUT(1) = (unsigned int)date_local.month();
  OUT(2) = (unsigned int)date_local.day();
  OUT(3) = time_local.hours().count();
  OUT(4) = time_local.minutes().count();
  if (precision == "milliseconds")
  {
    OUT(5) = (double)time_local.seconds().count() +
             (double)time_local.subseconds().count() / 1000;
  }
  else
  {
    OUT(5) = (double)time_local.seconds().count() +
             (double)time_local.subseconds().count() / 1000000;
  }
  return OUT;
}

template <typename ZonedType> RowVector tz2vector (const ZonedType& to, string precision)
{
  return localtime2vector (to.get_local_time (), precision);
}

// Aggregate a single set of (possibly non-canonical) date/time components into
// a local_time, mirroring the rollover math of the component-normalisation path
// below so the two stay in lockstep.  No time zone is involved: the result is a
// bare wall clock, which is what every mode here starts from -- some going
// on to resolve it against a zone, others already knowing the offset and
// needing only the arithmetic.  Callers must screen NaN/Inf beforehand.
// Whole days carried by the time components.  The division must FLOOR, and in
// 64 bits: truncating toward zero and then taking one more day off whenever the
// time is negative takes a day too many every time that division came out
// exact, so an hour component of -24 landed two days back rather than one.  The
// seconds left behind are read off by 'seconds2vector', which answers modulo a
// day whatever the sign it is given, so only this count has to be right.
long
time_extra_days (double time_sec)
{
  return (long) floor (time_sec / 86400.0);
}

auto
components2localtime (double Yv, double Mv, double Dv, double hv, double mv,
                      double sv, double xv, string precision)
{
  // Aggregate hours, minutes, seconds, and milliseconds into seconds,
  // calculate extra days to add later and map remaining hours, minutes, and
  // seconds to a local_time variable.
  double time_sec = hv * 3600 + mv * 60 + sv + xv / 1000;
  long extra_days = time_extra_days (time_sec);
  time_sec = remainder (time_sec, 86400);
  RowVector HMS = seconds2vector (time_sec, precision);
  int tmp_h = (int)HMS(3);
  int tmp_m = (int)HMS(4);
  int tmp_s = (int)HMS(5);
  double pr = 1000000;
  if (precision == "milliseconds")
  {
    pr = 1000;
  }
  double tmp_frac_sec = HMS(5) - tmp_s;
  int tmp_micro = (int)(round (tmp_frac_sec * pr));
  // Fix years / months
  int tmp_Y = (int)Yv + ((int)Mv / 12);
  int tmp_M = (int)Mv % 12;
  int tmp_D = (int)Dv + (int)extra_days;
  // Add/subtract months and days accordingly
  year_month_day ymd = year(tmp_Y)/(int)0/(int)0;
  if (tmp_M < 0)
  {
    ymd -= months{-tmp_M};
  }
  else
  {
    ymd += months{tmp_M};
  }
  if (tmp_D < 0)
  {
    ymd = sys_days{ymd} - days{-tmp_D};
  }
  else
  {
    ymd = sys_days{ymd} + days{tmp_D};
  }
  // Add time to date and interpret the wall-clock value in 'timezone'
  auto datetime = local_days{ymd} + chrono::hours{tmp_h}
                                  + chrono::minutes{tmp_m}
                                  + chrono::seconds{tmp_s}
                                  + chrono::microseconds{tmp_micro};
  return datetime;
}

auto
components2zoned (double Yv, double Mv, double Dv, double hv, double mv,
                  double sv, double xv, string timezone, string precision)
{
  return resolve_local (timezone,
                        components2localtime (Yv, Mv, Dv, hv, mv, sv, xv,
                                              precision));
}

// Everything the daylight-saving questions need about one wall clock, from a
// SINGLE tz-database lookup.  The database answers by local time, and a local
// time names one moment, two, or none; 'get_info' reports which, along with the
// regime on each side, and every question below is then a matter of choosing
// between them rather than of asking again.
//
// CHOSEN is the offset 'resolve_local' would settle on, which is the answer for
// an array built from a wall clock.  KEPT is SRCOFF where that still names this
// clock and CHOSEN otherwise, which is how calendar arithmetic and 'dateshift'
// keep an element on the pass it was already on; passing HAVESRC false asks for
// CHOSEN alone.  ISDST and ABBREV belong to whichever of the two candidates
// KEPT names, so they describe the element's own pass rather than the later
// one -- 'EDT' for the hour that is still on daylight saving and 'EST' for the
// hour that repeats it.  Reading the flag off the regime is also why this is
// right where reasoning from "the earlier pass is the daylight one" is not:
// Ireland's database entries count winter as the saving period.
//
// Callers must screen NaN/Inf.
struct local_fold
{
  double chosen;
  double kept;
  bool   isdst;
  string abbrev;
};

local_fold
components2fold (double Yv, double Mv, double Dv, double hv, double mv,
                 double sv, double xv, const time_zone *tz, string precision,
                 double srcOff, bool haveSrc, local_cache& lc)
{
  auto lt = components2localtime (Yv, Mv, Dv, hv, mv, sv, xv, precision);
  const local_info& info = cached_local_info (tz, lt, lc);
  // 'resolve_local' shifts a clock inside a gap forward past it, which lands
  // it in the regime after the transition, and takes the later of a repeated
  // pair; a clock that names one moment has only 'first' filled in.
  const sys_info *pick = (info.result == local_info::unique) ? &info.first
                                                             : &info.second;
  local_fold out;
  out.chosen = (double) pick->offset.count ();
  out.kept = out.chosen;
  if (haveSrc && info.result != local_info::nonexistent)
  {
    double so = round (srcOff);
    if (so == (double) info.first.offset.count ()
        || (info.result == local_info::ambiguous
            && so == (double) info.second.offset.count ()))
    {
      out.kept = so;
    }
  }
  const sys_info *own = pick;
  if (info.result == local_info::ambiguous
      && out.kept == (double) info.first.offset.count ())
  {
    own = &info.first;
  }
  out.isdst = own->save != chrono::minutes {0};
  out.abbrev = own->abbrev;
  return out;
}

// The instant a wall clock names, under the same rules as 'resolve_local': the
// later of a repeated pair, and a clock inside a gap shifted past it -- which
// is the same instant as reading it at the offset in force before the
// transition, whence the use of 'first' there.  Built from one cached lookup
// and a subtraction rather than from a zoned_time, which would look the clock
// up again to be asked for its instant.
sys_time<chrono::microseconds>
local2sys (const time_zone *tz, local_time<chrono::microseconds> lt,
           local_cache& lc)
{
  const local_info& info = cached_local_info (tz, lt, lc);
  auto off = (info.result == local_info::ambiguous) ? info.second.offset
                                                    : info.first.offset;
  return sys_time<chrono::microseconds> {lt.time_since_epoch ()} - off;
}

// Components and offset of an absolute instant, read in 'timezone'.  A moment
// names one wall clock, so nothing here has to be resolved, and the offset that
// the single lookup returns is added to reach the local time rather than a
// zoned_time being built -- which would look the same moment up again on every
// question asked of it.
void
sys2components (sys_time<chrono::microseconds> sys, const time_zone *tz,
                RowVector& OUT, double& off, string precision, sys_cache& zc)
{
  const sys_info& si = cached_sys_info (tz, chrono::floor<chrono::seconds>
                                            (sys), zc);
  local_time<chrono::microseconds> lt {(sys + si.offset).time_since_epoch ()};
  OUT = localtime2vector (lt, precision);
  off = (double) si.offset.count ();
}

void
sys2components (double posix_sec, const time_zone *tz, RowVector& OUT,
                double& off, sys_cache& zc)
{
  using ds = chrono::duration<double>;
  auto sys = round<chrono::microseconds> (sys_time<ds> {ds {posix_sec}});
  sys2components (sys, tz, OUT, off, "microseconds", zc);
}

sys_time<chrono::microseconds>
components2sys (double Yv, double Mv, double Dv, double hv, double mv,
               double sv, double xv, string timezone, string precision)
{
  auto in = components2zoned (Yv, Mv, Dv, hv, mv, sv, xv, timezone, precision);
  return chrono::time_point_cast<chrono::microseconds> (in.get_sys_time ());
}

auto timezone_precision (double time_sec, string timezone, string precision)
{
  auto tz = make_zoned (current_zone (), chrono::system_clock::now ());
  if (precision == "milliseconds")
  {
    auto tp = double2milli (time_sec);
    tz = resolve_local (timezone, tp);
  }
  else if (precision == "microseconds")
  {
    auto tp = double2micro (time_sec);
    tz = resolve_local (timezone, tp);
  }
  else
  {
    auto tp = double2nano (time_sec);
    using duration_type = std::chrono::duration<std::int64_t, std::ratio<1, 1000000>>; // microseconds
    tz = resolve_local (timezone, std::chrono::time_point_cast<duration_type>(tp));
  }
  return tz;
}

template <typename ZonedType> RowVector timezone2vector (const ZonedType& to)
{
  RowVector OUT(6);
  auto t_local = to.get_local_time();
  auto day_tp = chrono::floor<days>(t_local);
  hh_mm_ss time_tp{t_local - day_tp};
  year_month_day date_tp{day_tp};
  OUT(0) = (int)date_tp.year();
  OUT(1) = (unsigned int)date_tp.month();
  OUT(2) = (unsigned int)date_tp.day();
  OUT(3) = time_tp.hours().count();
  OUT(4) = time_tp.minutes().count();
  OUT(5) = (double)time_tp.seconds().count() +
             (double)time_tp.subseconds().count() / 1000000000;
  return OUT;
}

NDArray expand_input (dim_vector sz, octave_value args)
{
  NDArray OUT(sz, 0);
  if (args.is_scalar_type ())
  {
    for (int i = 0; i < sz.numel (); i++)
    {
      OUT(i) = args.scalar_value ();
    }
  }
  else
  {
    NDArray tmp = args.array_value ();
    for (int i = 0; i < sz.numel (); i++)
    {
      OUT(i) = tmp(i);
    }
  }
  return OUT;
}

double check_nan_inf (RowVector IN)
{
  int n = IN.numel ();
  double OUT = 0;
  bool is_nan = false;
  bool isPinf = false;
  bool isNinf = false;
  for (int i = 0; i < n; i++)
  {
    if (isnan (IN(i)))
    {
      is_nan = true;
    }
    else if (! is_nan && ! isNinf && isinf (IN(i)) && IN(i) > 0)
    {
      isPinf = true;
    }
    else if (! is_nan && ! isPinf && isinf (IN(i)) && IN(i) < 0)
    {
      isNinf = true;
    }
    else if (! is_nan && isNinf && isinf (IN(i)) && IN(i) > 0)
    {
      is_nan = true;
    }
    else if (! is_nan && isPinf && isinf (IN(i)) && IN(i) < 0)
    {
      is_nan = true;
    }
  }
  if (is_nan)
  {
    OUT = NAN;
  }
  else if (isPinf)
  {
    OUT = INFINITY;
  }
  else if (isNinf)
  {
    OUT = -INFINITY;
  }
  return OUT;
}

DEFUN_DLD(__datetime__, args, nargout,
          "-*- texinfo -*-\n\
 @deftypefn  {datatypes} {[@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S}]} __datetime__ (@dots{})\n\
 @deftypefnx {datatypes} {[@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S}, @var{errmsg}]} __datetime__ (@dots{})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@qcode{'now'})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@qcode{'today'})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@qcode{'tomorrow'})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@qcode{'yesterday'})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@var{Y}, @var{MO}, @var{D})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@var{Y}, @var{MO}, @var{D}, @var{H}, @var{MI}, @var{S}, @var{MS})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@var{X}, @qcode{'ConvertFrom'}, @var{dateType})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@dots{}, @qcode{'Precision'}, @var{precision})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@dots{}, @qcode{'TimeZone'}, @var{tzone}, @qcode{'toTimeZone'}, @var{totzone})\n\
 @deftypefnx {datatypes} {[@dots{}] =} __datetime__ (@dots{}, @qcode{'Offset'}, @var{off})\n\
\n\
\n\
Base function for datetime class. \n\
\n\n\
The @qcode{'Offset'} parameter gives the stored UTC offset of each element \n\
in seconds, which is what tells apart the two instants a repeated wall \n\
clock names on the day a zone puts its clock back.  It is accepted by the \n\
@qcode{'ConvertTo'} modes listed below; those that also read it answer for \n\
the wall clock alone when it is not given. \n\
\n\n\
@table @asis \n\
@item @qcode{'instant'} \n\
POSIX seconds of each element, taken from its offset by subtraction, with no \n\
time-zone lookup at all. \n\
\n\
@item @qcode{'fromposix'} \n\
The six components of each POSIX instant read in @var{tzone}, plus the \n\
offset in force there as a seventh output. \n\
\n\
@item @qcode{'rezone'} \n\
The six components of each element expressed in @var{totzone} while keeping \n\
its instant, plus the offset there as a seventh output. \n\
\n\
@item @qcode{'zoneoffset'} \n\
The offset the wall clock resolves to. \n\
\n\
@item @qcode{'keepfold'} \n\
The given offset where it still names this wall clock, and the resolved \n\
offset otherwise. \n\
\n\
@item @qcode{'isdst'}, @qcode{'zoneabbrev'} \n\
Daylight-saving flag and zone abbreviation, for the element's own pass over \n\
a repeated clock when an offset is given. \n\
@end table \n\
\n\n\
@end deftypefn")
{
  // The 'leapseconds' mode reports the leap-second table of the shipped tz
  // database as the POSIX time of each insertion, that is, of the first instant
  // after the inserted second.  It takes no data and returns a single output,
  // so it is answered ahead of the output-count guard below.
  if (args.length () == 1 && args(0).is_string ()
      && args(0).string_value () == "leapseconds")
  {
    octave_value_list ls_out (1);
    try
    {
      const auto& leaps = get_tzdb ().leap_seconds;
      ColumnVector OUT (leaps.size ());
      for (size_t i = 0; i < leaps.size (); i++)
      {
        OUT(i) = (double) leaps[i].date ().time_since_epoch ().count ();
      }
      ls_out(0) = OUT;
    }
    catch (const exception& e)
    {
      error ("__datetime__: TZDB error: %s", e.what ());
    }
    return ls_out;
  }

  // The 'ConvertTo' serial mode returns a single output; every other mode
  // requires either 6 or 7 output arguments.  Detect the mode up front so the
  // output-count guard below can exempt it.
  bool toSerial = false;
  for (int i = 0; i + 1 < args.length (); i++)
  {
    if (args(i).is_string () && args(i).string_value () == "ConvertTo")
    {
      toSerial = true;
    }
  }

  // Either 6 or 7 output arguments are required
  if (nargout > 7)
  {
    error ("__datetime__: too many output arguments.");
  }
  if (nargout < 6 && ! toSerial)
  {
    error ("__datetime__: too few output arguments.");
  }

  // Prepare input output arguments
  int nargin = args.length ();
  octave_value_list retval(nargout);
  for (int i = 0; i < nargout; i++)
  {
    retval(i) = 0;
  }

  // Add defaults
  string timezone;
  try
  {
    timezone = current_zone () -> name ();
  }
  catch (const exception& e)
  {
    octave_stdout << "__datetime__: TZDB error: " << e.what() << "\n";
    octave_stdout << "Falling back to UTC.\n";
    timezone = "UTC";
  }
  string to_tzone = timezone;
  // Whether a zone was NAMED, which is not the same question as which zone is
  // in force: the defaults above are the machine's, and an unzoned datetime
  // must not be resolved against them.
  bool haveZone = false;
  string precision = "milliseconds";
  bool doLeapSec = false;
  bool doConvert = false;
  string convertFrom = "";
  string convertTo = "";
  // The stored UTC offset of each element, which is what distinguishes the two
  // moments a repeated wall clock names.  Optional: without it the modes below
  // answer for the clock alone, as they did before it existed.
  octave_value offsetArg;
  bool haveOffset = false;

  // Parse paired arguments here
  while (nargin > 2 && args(nargin - 2).is_string ())
  {
    if (args(nargin - 2).string_value () == "ConvertFrom")
    {
      if (args(nargin - 1).is_string ())
      {
        convertFrom = args(nargin - 1).string_value ();
        doConvert = true;
      }
      else
      {
        if (nargout == 7)
        {
          retval(6) = "invalid type for 'ConvertFrom'.";
          return retval;
        }
        else
        {
          error ("__datetime__: invalid type for 'ConvertFrom'.");
        }
      }
    }
    else if (args(nargin - 2).string_value () == "ConvertTo")
    {
      if (args(nargin - 1).is_string ())
      {
        convertTo = args(nargin - 1).string_value ();
      }
      else
      {
        if (nargout == 7)
        {
          retval(6) = "invalid type for 'ConvertTo'.";
          return retval;
        }
        else
        {
          error ("__datetime__: invalid type for 'ConvertTo'.");
        }
      }
    }
    else if (args(nargin - 2).string_value () == "Offset")
    {
      if (args(nargin - 1).isnumeric ())
      {
        offsetArg = args(nargin - 1);
        haveOffset = true;
      }
      else
      {
        if (nargout == 7)
        {
          retval(6) = "invalid type for 'Offset'.";
          return retval;
        }
        else
        {
          error ("__datetime__: invalid type for 'Offset'.");
        }
      }
    }
    else if (args(nargin - 2).string_value () == "Precision")
    {
      if (args(nargin - 1).is_string ())
      {
        precision = args(nargin - 1).string_value ();
      }
      else
      {
        if (nargout == 7)
        {
          retval(6) = "invalid type for 'Precision'.";
          return retval;
        }
        else
        {
          error ("__datetime__: invalid type for 'Precision'.");
        }
      }
    }
    else if (args(nargin - 2).string_value () == "TimeZone")
    {
      if (args(nargin - 1).is_string ())
      {
        haveZone = true;
        if (args(nargin - 1).string_value () == "UTCLeapSeconds")
        {
          timezone = "UTC";
          doLeapSec = true;
        }
        else
        {
          timezone = args(nargin - 1).string_value ();
        }
      }
      else
      {
        if (nargout == 7)
        {
          retval(6) = "invalid type for 'TimeZone'.";
          return retval;
        }
        else
        {
          error ("__datetime__: invalid type for 'TimeZone'.");
        }
      }
    }
    else if (args(nargin - 2).string_value () == "toTimeZone")
    {
      if (args(nargin - 1).is_string ())
      {
        haveZone = true;
        if (args(nargin - 1).string_value () == "UTCLeapSeconds")
        {
          to_tzone = "UTC";
          doLeapSec = true;
        }
        else
        {
          to_tzone = args(nargin - 1).string_value ();
        }
      }
      else
      {
        if (nargout == 7)
        {
          retval(6) = "invalid type for 'toTimeZone'.";
          return retval;
        }
        else
        {
          error ("__datetime__: invalid type for 'toTimeZone'.");
        }
      }
    }
    //else if (args(nargin - 2).string_value () == "Format") {}
    //else if (args(nargin - 2).string_value () == "InputFormat") {}
    //else if (args(nargin - 2).string_value () == "Locale") {}
    //else if (args(nargin - 2).string_value () == "PivotYear") {}
    else
    {
      if (nargout == 7)
      {
        retval(6) = "unrecognized optional paired argument.";
        return retval;
      }
      else
      {
        error ("__datetime__: unrecognized optional paired argument.");
      }
    }
    nargin = nargin - 2;
  }

  // Check for valid timezone input arguments
  try
  {
    auto tmp = make_zoned(timezone, chrono::system_clock::now());
  }
  catch (exception)
  {
    if (nargout == 7)
    {
      retval(6) = "unrecognized timezone: '" + timezone + "'";
      return retval;
    }
    else
    {
      error ("__datetime__: invalid string value for 'TimeZone'.");
    }
  }
  try
  {
    auto tmp = make_zoned(to_tzone, chrono::system_clock::now());
  }
  catch (exception)
  {
    if (nargout == 7)
    {
      retval(6) = "unrecognized timezone: '" + to_tzone + "'";
      return retval;
    }
    else
    {
      error ("__datetime__: invalid string value for 'toTimeZone'.");
    }
  }

  // Handle relativeDay (only one argument left)
  if (args(0).is_string ())
  {
    Matrix Y(1,1);
    Matrix M(1,1);
    Matrix D(1,1);
    Matrix h(1,1);
    Matrix m(1,1);
    Matrix s(1,1);
    auto today = chrono::system_clock::now ();
    if (args(0).string_value () == "now")
    {
      auto tz = make_zoned(timezone, today);
      RowVector OUT = timezone2vector (tz);
      Y(0) = OUT(0); M(0) = OUT(1); D(0) = OUT(2); h(0) = OUT(3); m(0) = OUT(4);
      if (precision == "milliseconds")
      {
        s(0) = round (OUT(5) * 1000) / 1000;
      }
      else if (precision == "microseconds")
      {
        s(0) = round (OUT(5) * 1000000) / 1000000;
      }
      else
      {
        s(0) = OUT(5);
      }
    }
    else if (args(0).string_value () == "today")
    {
      auto tz = make_zoned(timezone, floor<days>(today));
      RowVector OUT = timezone2vector (tz);
      Y(0) = OUT(0); M(0) = OUT(1); D(0) = OUT(2);
      h(0) = 0; m(0) = 0; s(0) = 0;
    }
    else if (args(0).string_value () == "yesterday")
    {
      auto tz = make_zoned(timezone, floor<days>(today) - days{1});
      RowVector OUT = timezone2vector (tz);
      Y(0) = OUT(0); M(0) = OUT(1); D(0) = OUT(2);
      h(0) = 0; m(0) = 0; s(0) = 0;
    }
    else if (args(0).string_value () == "tomorrow")
    {
      auto tz = make_zoned(timezone, floor<days>(today) + days{1});
      RowVector OUT = timezone2vector (tz);
      Y(0) = OUT(0); M(0) = OUT(1); D(0) = OUT(2);
      h(0) = 0; m(0) = 0; s(0) = 0;
    }
    retval(0) = Y;
    retval(1) = M;
    retval(2) = D;
    retval(3) = h;
    retval(4) = m;
    retval(5) = s;
    return retval;
  }

  // Handle convertFrom (only one argument left)
  if (doConvert)
  {
    if (nargin > 1)
    {
      string errmsg = "only a single numeric array is allowed";
      errmsg += " when using the 'ConvertFrom' parameter.";
      if (nargout == 7)
      {
        retval(6) = errmsg;
        return retval;
      }
      else
      {
        errmsg = "__datetime__: " + errmsg;
        error ("%s", errmsg.c_str ());
      }
    }
    // Initialize output argument
    double time_sec;
    int n = args(0).numel ();
    Matrix Y(n,1);  // Years       (numeric)
    Matrix M(n,1);  // Months      (numeric)
    Matrix D(n,1);  // Days        (numeric)
    Matrix h(n,1);  // Hours       (numeric)
    Matrix m(n,1);  // Minutes     (numeric)
    Matrix s(n,1);  // Seconds     (numeric)
    // Fix datetimes from input argument to seconds
    // according to the requested date/time representation
    // Precision is limited to microseconds allowing for maximum range
    // between [-32768-01-01, 32767-12-31]. Set to 'milliseconds' just
    // rounds to nearest millisecond, while 'nanoseconds' is ignored.
    ColumnVector Dnum = args(0).column_vector_value ();
    if (convertFrom == "datenum")
    {
      for (int i = 0; i < n; i++)
      {
        if (isnan (Dnum(i)))
        {
          Y(i) = NAN; M(i) = NAN; D(i) = NAN;
          h(i) = NAN; m(i) = NAN; s(i) = NAN;
        }
        else if (isinf (Dnum(i)) && Dnum(i) > 0)
        {
          Y(i) = INFINITY; M(i) = INFINITY; D(i) = INFINITY;
          h(i) = INFINITY; m(i) = INFINITY; s(i) = INFINITY;
        }
        else if (isinf (Dnum(i)) && Dnum(i) < 0)
        {
          Y(i) = -INFINITY; M(i) = -INFINITY; D(i) = -INFINITY;
          h(i) = -INFINITY; m(i) = -INFINITY; s(i) = -INFINITY;
        }
        else
        {
          time_sec = (Dnum(i) - 719529) * 86400;    // to seconds
          RowVector OUT = seconds2vector (time_sec, precision);
          Y(i) = OUT(0); M(i) = OUT(1); D(i) = OUT(2);
          h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
        }
      }
    }
    else if (convertFrom == "excel")
    {
      for (int i = 0; i < n; i++)
      {
        if (isnan (Dnum(i)))
        {
          Y(i) = NAN; M(i) = NAN; D(i) = NAN;
          h(i) = NAN; m(i) = NAN; s(i) = NAN;
        }
        else if (isinf (Dnum(i)) && Dnum(i) > 0)
        {
          Y(i) = INFINITY; M(i) = INFINITY; D(i) = INFINITY;
          h(i) = INFINITY; m(i) = INFINITY; s(i) = INFINITY;
        }
        else if (isinf (Dnum(i)) && Dnum(i) < 0)
        {
          Y(i) = -INFINITY; M(i) = -INFINITY; D(i) = -INFINITY;
          h(i) = -INFINITY; m(i) = -INFINITY; s(i) = -INFINITY;
        }
        else
        {
          if (Dnum(i) <= 60)
          {
            time_sec = (Dnum(i) - 25568) * 86400;   // to seconds
          }
          else
          {
            time_sec = (Dnum(i) - 25569) * 86400;   // to seconds
          }
          RowVector OUT = seconds2vector (time_sec, precision);
          Y(i) = OUT(0); M(i) = OUT(1); D(i) = OUT(2);
          h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
        }
      }
    }
    else if (convertFrom == "posixtime")
    {
      for (int i = 0; i < n; i++)
      {
        if (isnan (Dnum(i)))
        {
          Y(i) = NAN; M(i) = NAN; D(i) = NAN;
          h(i) = NAN; m(i) = NAN; s(i) = NAN;
        }
        else if (isinf (Dnum(i)) && Dnum(i) > 0)
        {
          Y(i) = INFINITY; M(i) = INFINITY; D(i) = INFINITY;
          h(i) = INFINITY; m(i) = INFINITY; s(i) = INFINITY;
        }
        else if (isinf (Dnum(i)) && Dnum(i) < 0)
        {
          Y(i) = -INFINITY; M(i) = -INFINITY; D(i) = -INFINITY;
          h(i) = -INFINITY; m(i) = -INFINITY; s(i) = -INFINITY;
        }
        else
        {
          time_sec = Dnum(i);       // already in seconds
          RowVector OUT = seconds2vector (time_sec, precision);
          Y(i) = OUT(0); M(i) = OUT(1); D(i) = OUT(2);
          h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
        }
      }
    }
    else if (convertFrom == "epochtime")
    {
      if (doLeapSec)
      {
        for (int i = 0; i < n; i++)
        {
          if (isnan (Dnum(i)))
          {
            Y(i) = NAN; M(i) = NAN; D(i) = NAN;
            h(i) = NAN; m(i) = NAN; s(i) = NAN;
          }
          else if (isinf (Dnum(i)) && Dnum(i) > 0)
          {
            Y(i) = INFINITY; M(i) = INFINITY; D(i) = INFINITY;
            h(i) = INFINITY; m(i) = INFINITY; s(i) = INFINITY;
          }
          else if (isinf (Dnum(i)) && Dnum(i) < 0)
          {
            Y(i) = -INFINITY; M(i) = -INFINITY; D(i) = -INFINITY;
            h(i) = -INFINITY; m(i) = -INFINITY; s(i) = -INFINITY;
          }
          else
          {
            time_sec = Dnum(i);     // already in seconds
            // This is a workaround, since I don't know how to properly account
            // for leap seconds in a fashion that these are added to the given
            // representation instead of being substructed
            auto dt = chrono::duration_cast<chrono::microseconds>
                      (chrono::duration<double>{time_sec});
            auto tp0 = double2micro(0);
            auto from0 = make_zoned (timezone, tp0);
            auto from_utc = clock_cast<utc_clock>(from0.get_sys_time());
            auto to = make_zoned (timezone, clock_cast<chrono::system_clock>
                                                               (from_utc + dt));
            auto tp = double2micro(time_sec);
            auto ti = make_zoned (timezone, tp);
            auto out = make_zoned (timezone, ti.get_sys_time () +
                                   (ti.get_sys_time () - to.get_sys_time ()));
            RowVector OUT = tz2vector (out, precision);
            Y(i) = OUT(0); M(i) = OUT(1); D(i) = OUT(2);
            h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
          }
        }
      }
      else
      {
        for (int i = 0; i < n; i++)
        {
          if (isnan (Dnum(i)))
          {
            Y(i) = NAN; M(i) = NAN; D(i) = NAN;
            h(i) = NAN; m(i) = NAN; s(i) = NAN;
          }
          else if (isinf (Dnum(i)) && Dnum(i) > 0)
          {
            Y(i) = INFINITY; M(i) = INFINITY; D(i) = INFINITY;
            h(i) = INFINITY; m(i) = INFINITY; s(i) = INFINITY;
          }
          else if (isinf (Dnum(i)) && Dnum(i) < 0)
          {
            Y(i) = -INFINITY; M(i) = -INFINITY; D(i) = -INFINITY;
            h(i) = -INFINITY; m(i) = -INFINITY; s(i) = -INFINITY;
          }
          else
          {
            time_sec = Dnum(i);     // already in seconds
            RowVector OUT = seconds2vector (time_sec, precision);
            Y(i) = OUT(0); M(i) = OUT(1); D(i) = OUT(2);
            h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
          }
        }
      }
    }
    else
    {
      string errmsg = "unsupported option for the 'ConvertFrom' parameter.";
      if (nargout == 7)
      {
        retval(6) = errmsg;
        return retval;
      }
      else
      {
        errmsg = "__datetime__: " + errmsg;
        error ("%s", errmsg.c_str ());
      }
    }
    // Reshape output arguments
    retval(0) = Y.reshape (args(0).dims ());
    retval(1) = M.reshape (args(0).dims ());
    retval(2) = D.reshape (args(0).dims ());
    retval(3) = h.reshape (args(0).dims ());
    retval(4) = m.reshape (args(0).dims ());
    retval(5) = s.reshape (args(0).dims ());
    return retval;
  }

  // Handle single numeric matrix with either 3 or 6 columns
  // 'ConvertTo','fromposix' reads absolute instants, given as POSIX seconds,
  // into the wall clock of 'timezone' and reports the UTC offset in force at
  // each of them as a seventh output.  A moment names one clock, so no
  // resolution is involved and the offset comes back from the same lookup that
  // produced the components -- which is the whole reason this exists as one
  // mode rather than as a conversion followed by a separate offset query.
  if (convertTo == "fromposix")
  {
    dim_vector sz = args(0).dims ();
    NDArray P = args(0).array_value ();
    NDArray Y(sz, 0), M(sz, 0), D(sz, 0), h(sz, 0), m(sz, 0), s(sz, 0);
    NDArray OFF(sz, 0);
    const time_zone *tzp = locate_zone (timezone);
    sys_cache zc;
    for (int i = 0; i < sz.numel (); i++)
    {
      if (isnan (P(i)))
      {
        Y(i) = NAN; M(i) = NAN; D(i) = NAN;
        h(i) = NAN; m(i) = NAN; s(i) = NAN; OFF(i) = NAN;
      }
      else if (isinf (P(i)))
      {
        Y(i) = P(i); M(i) = P(i); D(i) = P(i);
        h(i) = P(i); m(i) = P(i); s(i) = P(i); OFF(i) = 0;
      }
      else
      {
        RowVector C(6);
        double off;
        sys2components (P(i), tzp, C, off, zc);
        Y(i) = C(0); M(i) = C(1); D(i) = C(2);
        h(i) = C(3); m(i) = C(4); s(i) = C(5); OFF(i) = off;
      }
    }
    retval(0) = Y; retval(1) = M; retval(2) = D;
    retval(3) = h; retval(4) = m; retval(5) = s;
    if (nargout == 7)
    {
      retval(6) = OFF;
    }
    return retval;
  }

  if (nargin == 1)
  {
    int n = args(0).rows ();
    ColumnVector Y(n);  // Years       (numeric)
    ColumnVector M(n);  // Months      (numeric)
    ColumnVector D(n);  // Days        (numeric)
    ColumnVector h(n);  // Hours       (numeric)
    ColumnVector m(n);  // Minutes     (numeric)
    ColumnVector s(n);  // Seconds     (numeric)
    if (args(0).ndims () != 2)
    {
      string errmsg = "single numeric data input must be a matrix";
      errmsg += " unless the 'ConvertFrom' parameter is used.";
      if (nargout == 7)
      {
        retval(6) = errmsg;
        return retval;
      }
      else
      {
        errmsg = "__datetime__: " + errmsg;
        error ("%s", errmsg.c_str ());
      }
    }
    if (args(0).columns () != 3 && args(0).columns () != 6)
    {
      string errmsg = "single numeric matrix must have three or six";
      errmsg += " columns unless the 'ConvertFrom' parameter is used.";
      if (nargout == 7)
      {
        retval(6) = errmsg;
        return retval;
      }
      else
      {
        errmsg = "__datetime__: " + errmsg;
        error ("%s", errmsg.c_str ());
      }
    }
    if (args(0).columns () == 3)
    {
      Matrix YMD = args(0).matrix_value ();
      for (int i = 0; i < n; i++)
      {
        if (round (YMD(i,0)) != YMD(i,0) || round (YMD(i,1)) != YMD(i,1) ||
            round (YMD(i,2)) != YMD(i,2))
        {
          string errmsg = "Year, Month, and Day components";
          errmsg += " must be integer values.";
          if (nargout == 7)
          {
            retval(6) = errmsg;
            return retval;
          }
          else
          {
            errmsg = "__datetime__: " + errmsg;
            error ("%s", errmsg.c_str ());
          }
        }
      }
      for (int i = 0; i < n; i++)
      {
        RowVector tmp(3);
        tmp(0) = YMD(i,0);
        tmp(1) = YMD(i,1);
        tmp(2) = YMD(i,2);
        double out = check_nan_inf (tmp);
        if (isnan (out))
        {
          Y(i) = NAN; M(i) = NAN; D(i) = NAN;
          h(i) = NAN; m(i) = NAN; s(i) = NAN;
        }
        else if (isinf (out))
        {
          Y(i) = out; M(i) = out; D(i) = out;
          h(i) = out; m(i) = out; s(i) = out;
        }
        else
        {
          // Fix years / months
          int tmp_Y = (int)YMD(i,0) + ((int)YMD(i,1) / 12);
          int tmp_M = (int)YMD(i,1) % 12;
          int tmp_D = (int)YMD(i,2);
          // Add/subtract months and days accordingly
          year_month_day ymd = year(tmp_Y)/(int)0/(int)0;
          if (tmp_M < 0)
          {
            ymd -= months{-tmp_M};
          }
          else
          {
            ymd += months{tmp_M};
          }
          if (tmp_D < 0)
          {
            ymd = sys_days{ymd} - days{-tmp_D};
          }
          else
          {
            ymd = sys_days{ymd} + days{tmp_D};
          }
          Y(i) = (int)ymd.year();
          M(i) = (unsigned int)ymd.month();
          D(i) = (unsigned int)ymd.day();
          h(i) = 0;
          m(i) = 0;
          s(i) = 0;
        }
      }
    }
    else if (args(0).columns () == 6)
    {
      Matrix YMDhms = args(0).matrix_value ();
      for (int i = 0; i < n; i++)
      {
        if (round (YMDhms(i,0)) != YMDhms(i,0) ||
            round (YMDhms(i,1)) != YMDhms(i,1) ||
            round (YMDhms(i,2)) != YMDhms(i,2) ||
            round (YMDhms(i,3)) != YMDhms(i,3) ||
            round (YMDhms(i,4)) != YMDhms(i,4))
        {
          string errmsg = "Year, Month, Day, Hour, and Minute";
          errmsg += " components must be integer values.";
          if (nargout == 7)
          {
            retval(6) = errmsg;
            return retval;
          }
          else
          {
            errmsg = "__datetime__: " + errmsg;
            error ("%s", errmsg.c_str ());
          }
        }
      }
      for (int i = 0; i < n; i++)
      {
        RowVector tmp(6);
        tmp(0) = YMDhms(i,0); tmp(1) = YMDhms(i,1);
        tmp(2) = YMDhms(i,2); tmp(3) = YMDhms(i,3);
        tmp(4) = YMDhms(i,4); tmp(5) = YMDhms(i,5);
        double out = check_nan_inf (tmp);
        if (isnan (out))
        {
          Y(i) = NAN; M(i) = NAN; D(i) = NAN;
          h(i) = NAN; m(i) = NAN; s(i) = NAN;
        }
        else if (isinf (out))
        {
          Y(i) = out; M(i) = out; D(i) = out;
          h(i) = out; m(i) = out; s(i) = out;
        }
        else
        {
          // Aggregate hours, minutes, and seconds into seconds, calculate extra
          // days for later and retrieve remaining hours, minutes, and seconds
          double time_sec = YMDhms(i,3) * 3600 + YMDhms(i,4) * 60 + YMDhms(i,5);
          long extra_days = time_extra_days (time_sec);
          time_sec = remainder (time_sec, 86400);
          RowVector OUT = seconds2vector (time_sec, precision);
          h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
          // Fix years / months
          int tmp_Y = (int)YMDhms(i,0) + ((int)YMDhms(i,1) / 12);
          int tmp_M = (int)YMDhms(i,1) % 12;
          int tmp_D = (int)YMDhms(i,2) + (int)extra_days;
          // Add/subtract months and days accordingly
          year_month_day ymd = year(tmp_Y)/(int)0/(int)0;
          if (tmp_M < 0)
          {
            ymd -= months{-tmp_M};
          }
          else
          {
            ymd += months{tmp_M};
          }
          if (tmp_D < 0)
          {
            ymd = sys_days{ymd} - days{-tmp_D};
          }
          else
          {
            ymd = sys_days{ymd} + days{tmp_D};
          }
          Y(i) = (int)ymd.year();
          M(i) = (unsigned int)ymd.month();
          D(i) = (unsigned int)ymd.day();
        }
      }
    }
    retval(0) = Y;
    retval(1) = M;
    retval(2) = D;
    retval(3) = h;
    retval(4) = m;
    retval(5) = s;
    return retval;
  }

  // Handle 3, 6, or 7 input data arguments
  if (nargin == 3 || nargin == 6 || nargin == 7)
  {
    // Check all input data arguments are numeric
    for (int i = 1; i < nargin; i++)
    {
      if (! args(i).isnumeric ())
      {
        string errmsg = "input data arguments must be numeric.";
        if (nargout == 7)
        {
          retval(6) = errmsg;
          return retval;
        }
        else
        {
          errmsg = "__datetime__: " + errmsg;
          error ("%s", errmsg.c_str ());
        }
      }
    }
    // Determine the common size: scalar arguments broadcast to the size of
    // the non-scalar arguments, which must all share a common size.
    dim_vector sz (1, 1);
    bool sized = false;
    for (int i = 0; i < nargin; i++)
    {
      if (args(i).is_scalar_type ())
      {
        continue;
      }
      dim_vector sz1 = args(i).dims ();
      if (! sized)
      {
        sz = sz1;
        sized = true;
      }
      else if (sz1 != sz)
      {
        string errmsg = "numeric data input arguments";
        errmsg += " must be of common size or scalars.";
        if (nargout == 7)
        {
          retval(6) = errmsg;
          return retval;
        }
        else
        {
          errmsg = "__datetime__: " + errmsg;
          error ("%s", errmsg.c_str ());
        }
      }
    }
    // Initialize output vectors with input data
    NDArray Y = expand_input (sz, args(0));   // Years        (numeric)
    NDArray M = expand_input (sz, args(1));   // Months       (numeric)
    NDArray D = expand_input (sz, args(2));   // Days         (numeric)
    NDArray h(sz, 0);                         // Hours        (numeric)
    NDArray m(sz, 0);                         // Minutes      (numeric)
    NDArray s(sz, 0);                         // Seconds      (numeric)
    NDArray x(sz, 0);                         // Milliseconds (internal use)

    if (nargin > 3)
    {
      h = expand_input (sz, args(3));
      m = expand_input (sz, args(4));
      s = expand_input (sz, args(5));
    }
    if (nargin > 6)
    {
      x = expand_input (sz, args(6));
    }
    NDArray OF(sz, 0);
    if (haveOffset)
    {
      OF = expand_input (sz, offsetArg);
    }
    // Beyond this point, all input data have common size

    // 'ConvertTo','instant' returns the absolute instant of each element as
    // POSIX seconds, taken from the stored offset rather than by resolving the
    // wall clock.  The offset already says which moment is meant, so this asks
    // the tz database nothing at all -- it is subtraction -- which is what
    // makes it the cheapest path in the class and the one every comparison,
    // ordering and set operation is built on.
    if (convertTo == "instant")
    {
      NDArray S(sz, 0);
      for (int i = 0; i < sz.numel (); i++)
      {
        RowVector tmp(7);
        tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
        tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
        double chk = check_nan_inf (tmp);
        if (isnan (chk))
        {
          S(i) = NAN;
        }
        else if (isinf (chk))
        {
          S(i) = chk;
        }
        else
        {
          auto lt = components2localtime (Y(i), M(i), D(i), h(i), m(i), s(i),
                                          x(i), precision);
          S(i) = (double) lt.time_since_epoch ().count () / 1000000.0 - OF(i);
        }
      }
      retval(0) = S;
      return retval;
    }

    // 'ConvertTo','zoneoffset' returns the UTC offset in seconds that the wall
    // clock resolves to, and 'keepfold' the offset an element should carry when
    // it arrives on this clock from another: the one it came with, where that
    // still names this clock, and the resolved one otherwise.  Both come from
    // one lookup.  Not-A-Time and infinite elements take a zero offset, the
    // same value an unzoned array carries.
    if (convertTo == "zoneoffset" || convertTo == "keepfold")
    {
      bool useSrc = (convertTo == "keepfold") && haveOffset;
      NDArray A(sz, 0);
      const time_zone *tzp = locate_zone (timezone);
      local_cache lc;
      for (int i = 0; i < sz.numel (); i++)
      {
        RowVector tmp(7);
        tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
        tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
        double chk = check_nan_inf (tmp);
        if (isnan (chk) || isinf (chk))
        {
          A(i) = isnan (chk) ? NAN : 0;
        }
        else
        {
          local_fold lf = components2fold (Y(i), M(i), D(i), h(i), m(i), s(i),
                                           x(i), tzp, precision, OF(i),
                                           useSrc, lc);
          A(i) = useSrc ? lf.kept : lf.chosen;
        }
      }
      retval(0) = A;
      return retval;
    }

    // 'ConvertTo','rezone' expresses each element in 'toTimeZone' while keeping
    // its instant, returning the new components and, as a seventh output, the
    // offset in force there.  It takes the instant from the stored offset, so
    // the source zone is not needed and -- more to the point -- nothing is
    // resolved on the way out: converting the clock instead would make the
    // database resolve it in the source zone first, which is the question with
    // two answers, and an element on the earlier pass would come out an hour
    // wrong.
    if (convertTo == "rezone")
    {
      NDArray rY(sz, 0), rM(sz, 0), rD(sz, 0);
      NDArray rh(sz, 0), rm(sz, 0), rs(sz, 0), rOFF(sz, 0);
      const time_zone *tzp = locate_zone (to_tzone);
      sys_cache zc;
      for (int i = 0; i < sz.numel (); i++)
      {
        RowVector tmp(7);
        tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
        tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
        double chk = check_nan_inf (tmp);
        if (isnan (chk))
        {
          rY(i) = NAN; rM(i) = NAN; rD(i) = NAN;
          rh(i) = NAN; rm(i) = NAN; rs(i) = NAN; rOFF(i) = NAN;
        }
        else if (isinf (chk))
        {
          rY(i) = chk; rM(i) = chk; rD(i) = chk;
          rh(i) = chk; rm(i) = chk; rs(i) = chk; rOFF(i) = 0;
        }
        else
        {
          auto lt = components2localtime (Y(i), M(i), D(i), h(i), m(i), s(i),
                                          x(i), precision);
          double p = (double) lt.time_since_epoch ().count () / 1000000.0
                     - OF(i);
          RowVector C(6);
          double off;
          sys2components (p, tzp, C, off, zc);
          rY(i) = C(0); rM(i) = C(1); rD(i) = C(2);
          rh(i) = C(3); rm(i) = C(4); rs(i) = C(5); rOFF(i) = off;
        }
      }
      retval(0) = rY; retval(1) = rM; retval(2) = rD;
      retval(3) = rh; retval(4) = rm; retval(5) = rs;
      if (nargout == 7)
      {
        retval(6) = rOFF;
      }
      return retval;
    }

    // 'ConvertTo','posixtime' returns POSIX seconds (double) instead of the six
    // canonical components.  The wall-clock components are interpreted in
    // 'timezone' (pass 'TimeZone','UTC' for unzoned datetimes so the serial is
    // free of any system-zone DST offset), Not-A-Time maps to NaN, and infinite
    // datetimes preserve their sign.
    if (convertTo == "posixtime")
    {
      const time_zone *tzp = locate_zone (timezone);
      local_cache lc;
      NDArray S(sz, 0);
      for (int i = 0; i < sz.numel (); i++)
      {
        RowVector tmp(7);
        tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
        tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
        double chk = check_nan_inf (tmp);
        if (isnan (chk))
        {
          S(i) = NAN;
        }
        else if (isinf (chk))
        {
          S(i) = chk;
        }
        else
        {
          auto lt = components2localtime (Y(i), M(i), D(i), h(i), m(i),
                                          s(i), x(i), precision);
          auto sys = local2sys (tzp, lt, lc);
          S(i) = (double) sys.time_since_epoch ().count () / 1000000.0;
        }
      }
      retval(0) = S;
      return retval;
    }

    // 'ConvertTo','zoneabbrev' returns the zone abbreviation (e.g. "EDT",
    // "EST", "UTC") active at each element, as a cell array of character
    // vectors.  The wall-clock components are interpreted in 'timezone';
    // Not-A-Time and infinite datetimes map to an empty string.
    if (convertTo == "zoneabbrev")
    {
      Cell A(sz);
      const time_zone *tzp = locate_zone (timezone);
      local_cache lc;
      for (int i = 0; i < sz.numel (); i++)
      {
        RowVector tmp(7);
        tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
        tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
        double chk = check_nan_inf (tmp);
        if (isnan (chk) || isinf (chk))
        {
          A(i) = "";
        }
        else
        {
          A(i) = components2fold (Y(i), M(i), D(i), h(i), m(i), s(i), x(i),
                                  tzp, precision, OF(i), haveOffset,
                                  lc).abbrev;
        }
      }
      retval(0) = A;
      return retval;
    }

    // 'ConvertTo','isdst' returns a 1/0 array (converted to logical by the
    // caller), true where daylight saving time is in effect at each element.
    // The wall-clock components are interpreted in 'timezone'; Not-A-Time and
    // infinite datetimes map to false.
    if (convertTo == "isdst")
    {
      NDArray A(sz, 0);
      const time_zone *tzp = locate_zone (timezone);
      local_cache lc;
      for (int i = 0; i < sz.numel (); i++)
      {
        RowVector tmp(7);
        tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
        tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
        double chk = check_nan_inf (tmp);
        if (isnan (chk) || isinf (chk))
        {
          A(i) = 0;
        }
        else
        {
          A(i) = components2fold (Y(i), M(i), D(i), h(i), m(i), s(i), x(i),
                                  tzp, precision, OF(i), haveOffset,
                                  lc).isdst ? 1 : 0;
        }
      }
      retval(0) = A;
      return retval;
    }

    // No zone was named, so there is no zone to resolve the wall clock
    // against: normalize the components and hand them straight back.  The path
    // below instead read the clock in the machine's own zone and wrote it back
    // out in the same one, which is the identity everywhere EXCEPT where
    // 'resolve_local' moves a clock -- so an unzoned wall clock falling in the
    // machine's spring-forward gap was silently shifted past it, and the answer
    // an unzoned datetime gave depended on where the code ran.  The matrix
    // forms of this constructor never consulted a zone, so the two spellings
    // also disagreed with each other.
    if (! haveZone)
    {
      for (int i = 0; i < sz.numel (); i++)
      {
        RowVector tmp(7);
        tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
        tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
        double out = check_nan_inf (tmp);
        if (isnan (out))
        {
          Y(i) = NAN; M(i) = NAN; D(i) = NAN;
          h(i) = NAN; m(i) = NAN; s(i) = NAN;
        }
        else if (isinf (out))
        {
          Y(i) = out; M(i) = out; D(i) = out;
          h(i) = out; m(i) = out; s(i) = out;
        }
        else
        {
          auto lt = components2localtime (Y(i), M(i), D(i), h(i), m(i), s(i),
                                          x(i), precision);
          RowVector OUT = localtime2vector (lt, precision);
          Y(i) = OUT(0); M(i) = OUT(1); D(i) = OUT(2);
          h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
        }
      }
      retval(0) = Y;
      retval(1) = M;
      retval(2) = D;
      retval(3) = h;
      retval(4) = m;
      retval(5) = s;
      return retval;
    }

    const time_zone *tzFrom = locate_zone (timezone);
    const time_zone *tzTo = locate_zone (to_tzone);
    local_cache lcn;
    sys_cache zcn;
    for (int i = 0; i < sz.numel (); i++)
    {
      RowVector tmp(7);
      tmp(0) = Y(i); tmp(1) = M(i); tmp(2) = D(i);
      tmp(3) = h(i); tmp(4) = m(i); tmp(5) = s(i); tmp(6) = x(i);
      double out = check_nan_inf (tmp);
      if (isnan (out))
      {
        Y(i) = NAN; M(i) = NAN; D(i) = NAN;
        h(i) = NAN; m(i) = NAN; s(i) = NAN; x(i) = NAN;
      }
      else if (isinf (out))
      {
        Y(i) = out; M(i) = out; D(i) = out;
        h(i) = out; m(i) = out; s(i) = out; x(i) = out;
      }
      else
      {
        // Aggregate hours, minutes, seconds, and milliseconds into seconds,
        // calculate extra days to add later and map remaining hours, minutes,
        // and seconds to a local_time variable
        double time_sec = h(i) * 3600 + m(i) * 60 + s(i) + x(i) / 1000;
        long extra_days = time_extra_days (time_sec);
        time_sec = remainder (time_sec, 86400);
        RowVector HMS = seconds2vector (time_sec, precision);
        int tmp_h = (int)HMS(3);
        int tmp_m = (int)HMS(4);
        int tmp_s = (int)HMS(5);
        double pr = 1000000;
        if (precision == "milliseconds")
        {
          pr = 1000;
        }
        double tmp_frac_sec = HMS(5) - tmp_s;
        int tmp_micro = (int)(round (tmp_frac_sec * pr));
        // Fix years / months
        int tmp_Y = (int)Y(i) + ((int)M(i) / 12);
        int tmp_M = (int)M(i) % 12;
        int tmp_D = (int)D(i) + (int)extra_days;
        // Add/subtract months and days accordingly
        year_month_day ymd = year(tmp_Y)/(int)0/(int)0;
        if (tmp_M < 0)
        {
          ymd -= months{-tmp_M};
        }
        else
        {
          ymd += months{tmp_M};
        }
        if (tmp_D < 0)
        {
          ymd = sys_days{ymd} - days{-tmp_D};
        }
        else
        {
          ymd = sys_days{ymd} + days{tmp_D};
        }
        // Add time to date
        auto datetime = local_days{ymd} + chrono::hours{tmp_h}
                                        + chrono::minutes{tmp_m}
                                        + chrono::seconds{tmp_s}
                                        + chrono::microseconds{tmp_micro};
        // Make timezone conversion, resolving the wall clock and reading the
        // instant back in the target zone through the cached lookups.
        auto sysp = local2sys (tzFrom, datetime, lcn);
        RowVector OUT(6);
        double dropOff;
        sys2components (sysp, tzTo, OUT, dropOff, precision, zcn);
        Y(i) = OUT(0); M(i) = OUT(1); D(i) = OUT(2);
        h(i) = OUT(3); m(i) = OUT(4); s(i) = OUT(5);
      }
    }
    // Return output arguments
    retval(0) = Y;
    retval(1) = M;
    retval(2) = D;
    retval(3) = h;
    retval(4) = m;
    retval(5) = s;
    return retval;
  }
  else
  {
    string errmsg = "numeric input data arguments must be";
    errmsg += " three, six, or seven separate arrays.";
    if (nargout == 7)
    {
      retval(6) = errmsg;
      return retval;
    }
    else
    {
      errmsg = "__datetime__: " + errmsg;
      error ("%s", errmsg.c_str ());
    }
  }
  // Should never reach this point! Exit safely, just in case.
  return retval;
}
