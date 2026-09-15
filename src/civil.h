/*
Copyright (C) 2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>

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

// Proleptic Gregorian civil arithmetic in 64 bits (Howard Hinnant's algorithms,
// the same ones 'date.h' uses).  It exists because 'date.h' stores a year in a
// 'short': every calendar question that does not also need timezone data is
// answered here instead, so that a year outside [-32767, 32767] is carried
// rather than truncated.  Timezone questions still go through 'date.h', which
// is the only place its 16 bits are a real limit -- there is no zone data out
// there to look up.  Every count is 'int64_t', never 'long', which is only 32
// bits on Windows.

#ifndef DATATYPES_CIVIL_H
#define DATATYPES_CIVIL_H

#include <cstdint>

namespace civil
{

  using std::int64_t;

  // Days from 1970-01-01 to the civil date Y-M-D.  M must lie in [1, 12]; D may
  // be any integer, and one outside its month simply names a day further along
  // (D = 0 is the last day of the previous month, which is the anchor the
  // component normalization below is built on).
  inline int64_t
  days_from_civil (int64_t y, int64_t m, int64_t d)
  {
    y -= (m <= 2);
    const int64_t era = (y >= 0 ? y : y - 399) / 400;
    const int64_t yoe = y - era * 400;
    const int64_t doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    const int64_t doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + doe - 719468;
  }

  // The civil date a day count names.  Exact inverse of 'days_from_civil' for
  // every canonical date.
  inline void
  civil_from_days (int64_t z, int64_t& y, int64_t& m, int64_t& d)
  {
    z += 719468;
    const int64_t era = (z >= 0 ? z : z - 146096) / 146097;
    const int64_t doe = z - era * 146097;
    const int64_t yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    const int64_t doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    const int64_t mp = (5 * doy + 2) / 153;
    d = doy - (153 * mp + 2) / 5 + 1;
    m = mp + (mp < 10 ? 3 : -9);
    y = yoe + era * 400 + (m <= 2);
  }

  // The day a component triple names, EXTRA_DAYS being the whole days its time
  // components carry.  The folding is the one the class has always applied and
  // which R2026a agrees with row for row: whole twelvemonths fold into the
  // year, the remainder is applied as months to day 0 of month 0 -- an anchor
  // lying in the PREVIOUS month -- and the days are added to that.  Month and
  // day may be anything; only the result has to be a real date.
  inline int64_t
  components2days (double Yv, double Mv, double Dv, int64_t extra_days)
  {
    const int64_t Y = (int64_t) Yv;
    const int64_t M = (int64_t) Mv;
    const int64_t yy = Y + M / 12;
    const int64_t dmi = M % 12 - 1;
    const int64_t dy = (dmi - (dmi < 0 ? 11 : 0)) / 12;
    return days_from_civil (yy + dy, dmi - dy * 12 + 1, 0)
           + (int64_t) Dv + extra_days;
  }

}

#endif
