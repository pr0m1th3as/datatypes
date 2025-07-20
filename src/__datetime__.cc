/*
Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>

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

auto from_to_tz_milli (double time_sec, string from_tzone, string to_tzone)
{
  auto tp = double2milli (time_sec);
  auto from = make_zoned (from_tzone, tp);
  auto to = make_zoned (to_tzone, from.get_sys_time ());
  return to;
}

auto from_to_tz_micro (double time_sec, string from_tzone, string to_tzone)
{
  auto tp = double2micro (time_sec);
  auto from = make_zoned (from_tzone, tp);
  auto to = make_zoned (to_tzone, from.get_sys_time ());
  return to;
}

auto from_to_tz_nano (double time_sec, string from_tzone, string to_tzone)
{
  auto tp = double2nano (time_sec);
  auto from = make_zoned (from_tzone, tp);
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

template <typename ZonedType> RowVector tz2vector (const ZonedType& to, string precision)
{
  RowVector OUT(6);
  auto t_local = to.get_local_time();
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

auto timezone_precision (double time_sec, string timezone, string precision)
{
  auto tz = make_zoned (current_zone (), chrono::system_clock::now ());
  if (precision == "milliseconds")
  {
    auto tp = double2milli (time_sec);
    tz = make_zoned (timezone, tp);
  }
  else if (precision == "microseconds")
  {
    auto tp = double2micro (time_sec);
    tz = make_zoned (timezone, tp);
  }
  else
  {
    auto tp = double2nano (time_sec);
    using duration_type = std::chrono::duration<std::int64_t, std::ratio<1, 1000000>>; // microseconds
    tz = make_zoned (timezone, std::chrono::time_point_cast<duration_type>(tp));
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
\n\
\n\
Base fuction for datetime class. \n\
\n\n\
@end deftypefn")
{
  // Either 6 or 7 output arguments are reguired
  if (nargout > 7)
  {
    error ("__datetime__: too many output arguments.");
  }
  if (nargout < 6)
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
  string timezone = current_zone () -> name ();
  string to_tzone = timezone;
  string precision = "milliseconds";
  bool doLeapSec = false;
  bool doConvert = false;
  string convertFrom = "";

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
          errmsg += " must be inteder values.";
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
          errmsg += " components must be inteder values.";
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
          int extra_days = (int)time_sec / 86400;
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
    // Make sure they are of the same size or scalars
    dim_vector sz = args(0).dims ();
    for (int i = 1; i < nargin; i++)
    {
      dim_vector sz1 = args(i).dims ();
      if (args(i - 1).is_scalar_type ())
      {
        sz = sz1;
      }
      else if (! args(i).is_scalar_type ())
      {
        if (sz.ndims () != sz1.ndims ())
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
        for (int j = 0; j < sz.ndims (); j++)
        {
          if (sz(j) != sz1(j))
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
        sz = sz1;
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
    // Beyond this point, all input data have common size
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
        int extra_days = (int)time_sec / 86400;
        // Subtract one day for negative time, because it goes missing
        // from the conversion in 'seconds2vector' below.
        if (time_sec < 0)
        {
          extra_days -= 1;
        }
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
        int tmp_micro = (int)(round (remainder (HMS(5), 1) * pr));
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
        // Make timezone conversion
        auto in = make_zoned (timezone, datetime);
        auto out = make_zoned (to_tzone, in.get_sys_time ());
        RowVector OUT = tz2vector (out, precision);
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
