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

#include <cmath>
#include <cstdint>
#include <limits>
#include <ctime>
#include <string>
#include <vector>

#include <octave/oct.h>

using namespace std;

// Locale name tables for the LDML 'InputFormat' parser.  These mirror the
// tables returned by dtLocaleNames in datetime.m: full and abbreviated month
// names, full and abbreviated weekday names (Sunday first), and the two day
// period markers.  All entries are lower case; matching folds case and Greek
// accents, so only the plain forms are needed here.  Spanish day periods carry
// a no-break space (U+00A0), matching MATLAB.
struct LocaleNames
{
  const char *lang;
  const char *mFull[12];
  const char *mAbbr[12];
  const char *wFull[7];
  const char *wAbbr[7];
  const char *dpMark[2];
};

static const LocaleNames LOCALES[] = {
  {"en",
   {"january", "february", "march", "april",
    "may", "june", "july", "august",
    "september", "october", "november", "december"},
   {"jan", "feb", "mar", "apr",
    "may", "jun", "jul", "aug",
    "sep", "oct", "nov", "dec"},
   {"sunday", "monday", "tuesday", "wednesday",
    "thursday", "friday", "saturday"},
   {"sun", "mon", "tue", "wed",
    "thu", "fri", "sat"},
   {"am", "pm"}},
  {"fr",
   {"janvier", "février", "mars", "avril",
    "mai", "juin", "juillet", "août",
    "septembre", "octobre", "novembre", "décembre"},
   {"janv", "févr", "mars", "avr",
    "mai", "juin", "juil", "août",
    "sept", "oct", "nov", "déc"},
   {"dimanche", "lundi", "mardi", "mercredi",
    "jeudi", "vendredi", "samedi"},
   {"dim", "lun", "mar", "mer",
    "jeu", "ven", "sam"},
   {"am", "pm"}},
  {"de",
   {"januar", "februar", "märz", "april",
    "mai", "juni", "juli", "august",
    "september", "oktober", "november", "dezember"},
   {"jan", "feb", "märz", "apr",
    "mai", "juni", "juli", "aug",
    "sept", "okt", "nov", "dez"},
   {"sonntag", "montag", "dienstag", "mittwoch",
    "donnerstag", "freitag", "samstag"},
   {"so", "mo", "di", "mi",
    "do", "fr", "sa"},
   {"am", "pm"}},
  {"es",
   {"enero", "febrero", "marzo", "abril",
    "mayo", "junio", "julio", "agosto",
    "septiembre", "octubre", "noviembre", "diciembre"},
   {"ene", "feb", "mar", "abr",
    "may", "jun", "jul", "ago",
    "sept", "oct", "nov", "dic"},
   {"domingo", "lunes", "martes", "miércoles",
    "jueves", "viernes", "sábado"},
   {"dom", "lun", "mar", "mié",
    "jue", "vie", "sáb"},
   // Spelled with an explicit no-break space (U+00A0), as MATLAB has it; a
   // literal one here is too easily flattened to a plain space by an editor.
   {"a.\u00A0m.", "p.\u00A0m."}},
  {"it",
   {"gennaio", "febbraio", "marzo", "aprile",
    "maggio", "giugno", "luglio", "agosto",
    "settembre", "ottobre", "novembre", "dicembre"},
   {"gen", "feb", "mar", "apr",
    "mag", "giu", "lug", "ago",
    "set", "ott", "nov", "dic"},
   {"domenica", "lunedì", "martedì", "mercoledì",
    "giovedì", "venerdì", "sabato"},
   {"dom", "lun", "mar", "mer",
    "gio", "ven", "sab"},
   {"am", "pm"}},
  {"pt",
   {"janeiro", "fevereiro", "março", "abril",
    "maio", "junho", "julho", "agosto",
    "setembro", "outubro", "novembro", "dezembro"},
   {"jan", "fev", "mar", "abr",
    "mai", "jun", "jul", "ago",
    "set", "out", "nov", "dez"},
   {"domingo", "segunda-feira", "terça-feira", "quarta-feira",
    "quinta-feira", "sexta-feira", "sábado"},
   {"dom", "seg", "ter", "qua",
    "qui", "sex", "sáb"},
   {"am", "pm"}},
  {"el",
   {"Ιανουαρίου", "Φεβρουαρίου", "Μαρτίου", "Απριλίου",
    "Μαΐου", "Ιουνίου", "Ιουλίου", "Αυγούστου",
    "Σεπτεμβρίου", "Οκτωβρίου", "Νοεμβρίου", "Δεκεμβρίου"},
   {"Ιαν", "Φεβ", "Μαρ", "Απρ",
    "Μαΐ", "Ιουν", "Ιουλ", "Αυγ",
    "Σεπ", "Οκτ", "Νοε", "Δεκ"},
   {"Κυριακή", "Δευτέρα", "Τρίτη", "Τετάρτη",
    "Πέμπτη", "Παρασκευή", "Σάββατο"},
   {"Κυρ", "Δευ", "Τρί", "Τετ",
    "Πέμ", "Παρ", "Σάβ"},
   {"π.μ.", "μ.μ."}}
};

static const int NLOCALES = sizeof (LOCALES) / sizeof (LOCALES[0]);

// Decode a UTF-8 byte string into code points.  Malformed bytes are passed
// through as their own code point, which cannot match any table entry and so
// simply fails to parse, as it does in the m-code path.
static void
utf8_decode (const string& s, vector<uint32_t>& cp)
{
  cp.clear ();
  size_t i = 0, n = s.size ();
  while (i < n)
  {
    unsigned char c = static_cast<unsigned char> (s[i]);
    uint32_t u;
    int extra;
    if (c < 0x80)
    {
      u = c;
      extra = 0;
    }
    else if ((c & 0xE0) == 0xC0)
    {
      u = c & 0x1F;
      extra = 1;
    }
    else if ((c & 0xF0) == 0xE0)
    {
      u = c & 0x0F;
      extra = 2;
    }
    else if ((c & 0xF8) == 0xF0)
    {
      u = c & 0x07;
      extra = 3;
    }
    else
    {
      cp.push_back (c);
      i++;
      continue;
    }
    if (i + extra >= n)
    {
      cp.push_back (c);
      i++;
      continue;
    }
    bool ok = true;
    for (int k = 1; k <= extra; k++)
    {
      unsigned char cc = static_cast<unsigned char> (s[i+k]);
      if ((cc & 0xC0) != 0x80)
      {
        ok = false;
        break;
      }
      u = (u << 6) | (cc & 0x3F);
    }
    if (! ok)
    {
      cp.push_back (c);
      i++;
      continue;
    }
    cp.push_back (u);
    i += extra + 1;
  }
}

// Case- and accent-fold a UTF-8 string to a code point vector, mirroring
// dtCaseFold in datetime.m.  ASCII capitals and the contiguous Greek capitals
// both lower case by +32; the accented Greek capitals, the accented lower case
// vowels, the dialytika forms and the final sigma map to their plain lower
// case base letters.  Latin accents outside ASCII are deliberately left alone,
// exactly as in the m-code.
static void
case_fold (const string& s, vector<uint32_t>& cp)
{
  static const uint32_t from[] = {902, 904, 905, 906, 908, 910, 911, 938, 939,
                                  940, 941, 942, 943, 972, 973, 974, 970, 971,
                                  912, 944, 962};
  static const uint32_t to[]   = {945, 949, 951, 953, 959, 965, 969, 953, 965,
                                  945, 949, 951, 953, 959, 965, 969, 953, 965,
                                  953, 965, 963};
  static const int nmap = sizeof (from) / sizeof (from[0]);

  utf8_decode (s, cp);
  for (size_t i = 0; i < cp.size (); i++)
  {
    uint32_t u = cp[i];
    if (u >= 65 && u <= 90)
    {
      u += 32;
    }
    else if (u >= 913 && u <= 937)
    {
      u += 32;
    }
    for (int k = 0; k < nmap; k++)
    {
      if (u == from[k])
      {
        u = to[k];
        break;
      }
    }
    cp[i] = u;
  }
}

// Folded name tables for one locale, built once and reused.
struct FoldedLocale
{
  bool built;
  vector<uint32_t> mFull[12];
  vector<uint32_t> mAbbr[12];
  vector<uint32_t> wFull[7];
  vector<uint32_t> wAbbr[7];
  vector<uint32_t> dpMark[2];
};

static FoldedLocale FOLDED[NLOCALES];

// Resolve a 'Locale' string to an index into LOCALES, mirroring the
// normalization in dtLocaleNames: empty or 'system' means English, and a
// regional suffix ('fr_FR') is ignored.  Returns -1 for an unknown language.
static int
locale_index (const string& locale)
{
  string lang;
  for (size_t i = 0; i < locale.size (); i++)
  {
    char c = locale[i];
    if (c == '_')
    {
      break;
    }
    lang += static_cast<char> (tolower (static_cast<unsigned char> (c)));
  }
  if (lang.empty () || lang == "system")
  {
    lang = "en";
  }
  for (int k = 0; k < NLOCALES; k++)
  {
    if (lang == LOCALES[k].lang)
    {
      return k;
    }
  }
  return -1;
}

static const FoldedLocale&
folded_locale (int idx)
{
  FoldedLocale& f = FOLDED[idx];
  if (! f.built)
  {
    const LocaleNames& L = LOCALES[idx];
    for (int k = 0; k < 12; k++)
    {
      case_fold (L.mFull[k], f.mFull[k]);
      case_fold (L.mAbbr[k], f.mAbbr[k]);
    }
    for (int k = 0; k < 7; k++)
    {
      case_fold (L.wFull[k], f.wFull[k]);
      case_fold (L.wAbbr[k], f.wAbbr[k]);
    }
    for (int k = 0; k < 2; k++)
    {
      case_fold (L.dpMark[k], f.dpMark[k]);
    }
    f.built = true;
  }
  return f;
}

// Return the 1-based index of the first entry of TBL whose folded form matches
// WORD, or 0 if none does (dtFoldFind).
static int
fold_find (const string& word, const vector<uint32_t> *tbl, int n)
{
  vector<uint32_t> fw;
  case_fold (word, fw);
  for (int k = 0; k < n; k++)
  {
    if (fw == tbl[k])
    {
      return k + 1;
    }
  }
  return 0;
}

// One LDML token: either a field run (sym set, n = run length) or a literal
// (sym = '\0', lit = the literal text).
struct Token
{
  char sym;
  int n;
  string lit;
};

// Split an LDML pattern into tokens (dtFormatTokens).  Single quotes delimit
// literal text and a doubled '' is a literal apostrophe.
static void
tokenize (const string& fmt, vector<Token>& toks)
{
  toks.clear ();
  size_t i = 0, L = fmt.size ();
  while (i < L)
  {
    char c = fmt[i];
    if (c == '\'')
    {
      if (i + 1 < L && fmt[i+1] == '\'')
      {
        Token t;
        t.sym = '\0';
        t.n = 0;
        t.lit = "'";
        toks.push_back (t);
        i += 2;
      }
      else
      {
        size_t j = i + 1;
        string buf;
        while (j < L && fmt[j] != '\'')
        {
          buf += fmt[j];
          j++;
        }
        Token t;
        t.sym = '\0';
        t.n = 0;
        t.lit = buf;
        toks.push_back (t);
        i = j + 1;
      }
    }
    else if (isalpha (static_cast<unsigned char> (c)))
    {
      size_t j = i;
      while (j < L && fmt[j] == c)
      {
        j++;
      }
      Token t;
      t.sym = c;
      t.n = static_cast<int> (j - i);
      toks.push_back (t);
      i = j;
    }
    else
    {
      Token t;
      t.sym = '\0';
      t.n = 0;
      t.lit = string (1, c);
      toks.push_back (t);
      i++;
    }
  }
}

// A name character is an ASCII letter or any byte above 127, which is a lead
// or continuation byte of an accented UTF-8 letter.
static inline bool
is_name_char (char c)
{
  unsigned char u = static_cast<unsigned char> (c);
  return (u > 127 || isalpha (u));
}

// Days from the civil epoch 1970-01-01 (Howard Hinnant's algorithm), used for
// the day-of-year field so that an out-of-range value rolls exactly as
// datenum/datevec do in the m-code path.
static long
days_from_civil (long y, unsigned m, unsigned d)
{
  y -= m <= 2;
  const long era = (y >= 0 ? y : y - 399) / 400;
  const unsigned yoe = static_cast<unsigned> (y - era * 400);
  const unsigned doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
  const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
  return era * 146097 + static_cast<long> (doe) - 719468;
}

static void
civil_from_days (long z, long& y, unsigned& m, unsigned& d)
{
  z += 719468;
  const long era = (z >= 0 ? z : z - 146096) / 146097;
  const unsigned doe = static_cast<unsigned> (z - era * 146097);
  const unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
  y = static_cast<long> (yoe) + era * 400;
  const unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
  const unsigned mp = (5 * doy + 2) / 153;
  d = doy - (153 * mp + 2) / 5 + 1;
  m = mp + (mp < 10 ? 3 : -9);
  y += (m <= 2);
}

// Length of a month, for validating a parsed date.  Returns 0 for a month
// outside 1..12, which fails the day test for any day.
static int
days_in_month (double Y, double M)
{
  static const int dpm[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  int m = static_cast<int> (M);
  if (m < 1 || m > 12)
  {
    return 0;
  }
  if (m == 2)
  {
    long y = static_cast<long> (Y);
    bool leap = (y % 4 == 0 && y % 100 != 0) || (y % 400 == 0);
    return (leap ? 29 : 28);
  }
  return dpm[m-1];
}

// Parse a cell array of date/time strings under an LDML 'InputFormat',
// returning an N-by-6 date-vector matrix.  This is a direct port of
// dtParseInput in datetime.m and must stay behaviourally identical to it.
static Matrix
ldml_parse (const Cell& strs, const string& fmt, double pivot, int lidx)
{
  vector<Token> toks;
  tokenize (fmt, toks);
  const int nt = static_cast<int> (toks.size ());
  const FoldedLocale& F = folded_locale (lidx);

  // Whether the format names any date field.  MATLAB defaults a wholly absent
  // date to today, but when some date field is present the missing parts fall
  // back to month 1 / day 1 (and the current year).
  bool hasDate = false;
  for (int t = 0; t < nt; t++)
  {
    char c = toks[t].sym;
    if (c && (c == 'y' || c == 'u' || c == 'M' || c == 'L' || c == 'd'
              || c == 'D'))
    {
      hasDate = true;
      break;
    }
  }

  // Today's local date, for the fields the format leaves out (the m-code
  // reads these from clock, which is local time as well).
  time_t tt = time (nullptr);
  struct tm lt;
  localtime_r (&tt, &lt);
  const double nowY = lt.tm_year + 1900;
  const double nowM = lt.tm_mon + 1;
  const double nowD = lt.tm_mday;

  // The two-digit year window, as in the m-code: base is the century of the
  // pivot, and a year below the pivot belongs to the next century.
  double pmod = fmod (pivot, 100.0);
  if (pmod < 0)
  {
    pmod += 100.0;
  }
  const double base = pivot - pmod;

  const octave_idx_type n = strs.numel ();
  Matrix DV (n, 6);

  for (octave_idx_type r = 0; r < n; r++)
  {
    const string s = strs(r).string_value ();
    const size_t L = s.size ();
    size_t pos = 0;
    double Yv = nowY;
    double Mv = hasDate ? 1 : nowM;
    double Dv = hasDate ? 1 : nowD;
    double Hv = 0, MIv = 0, Sv = 0;
    int ampm = 0;
    bool ok = true;

    for (int t = 0; t < nt && ok; t++)
    {
      const Token& tk = toks[t];

      if (tk.sym == '\0')
      {
        // Literal text must appear verbatim.
        size_t m = tk.lit.size ();
        if (pos + m <= L && s.compare (pos, m, tk.lit) == 0)
        {
          pos += m;
        }
        else
        {
          ok = false;
        }
      }
      else if ((tk.sym == 'e' && tk.n >= 3) || tk.sym == 'a')
      {
        // A weekday name carries no value (the year/month/day already fix the
        // date) but MATLAB rejects a token that is not a real weekday name, so
        // validate it.  A day period sets the AM/PM flag.  Both grab up to the
        // next literal so that compound names such as the Portuguese
        // "segunda-feira" and the Spanish "a. m." are captured whole.
        string word;
        if (t < nt - 1 && toks[t+1].sym == '\0')
        {
          size_t p = s.find (toks[t+1].lit, pos);
          if (p == string::npos)
          {
            ok = false;
            break;
          }
          word = s.substr (pos, p - pos);
          pos = p;
        }
        else if (t == nt - 1)
        {
          word = s.substr (pos);       // last token: the name runs to the end
          pos = L;
        }
        else
        {
          size_t j = pos;
          while (j < L && is_name_char (s[j]))
          {
            j++;
          }
          word = s.substr (pos, j - pos);
          pos = j;
        }
        if (tk.sym == 'a')
        {
          // A day period marker keeps any trailing period: the Spanish
          // markers are "a. m." and "p. m." and the Greek ones are
          // "π.μ."/"μ.μ.", so the period is part of the name.
          int idxA = fold_find (word, F.dpMark, 2);
          if (idxA == 1)
          {
            ampm = 1;
          }
          else if (idxA == 2)
          {
            ampm = 2;
          }
          else
          {
            ok = false;
          }
        }
        else
        {
          // A weekday name drops an abbreviation period before matching.
          if (! word.empty () && word[word.size()-1] == '.')
          {
            word.erase (word.size () - 1);
          }
          if (word.empty () || (fold_find (word, F.wFull, 7) == 0
                                && fold_find (word, F.wAbbr, 7) == 0))
          {
            ok = false;
          }
        }
      }
      else if (tk.sym == 'M' && tk.n >= 3)
      {
        // Month name: grab a run of name characters.
        size_t j = pos;
        while (j < L && is_name_char (s[j]))
        {
          j++;
        }
        string word = s.substr (pos, j - pos);
        pos = j;
        if (word.empty ())
        {
          ok = false;
          break;
        }
        int idx = fold_find (word, F.mFull, 12);
        if (idx == 0)
        {
          idx = fold_find (word, F.mAbbr, 12);
          if (idx == 0)
          {
            ok = false;
            break;
          }
          // Consume the trailing period of an abbreviation (fr/de/pt).
          if (pos < L && s[pos] == '.')
          {
            pos++;
          }
        }
        Mv = idx;
      }
      else
      {
        // Numeric field.  Butted against another numeric field, take exactly
        // the run length; otherwise take up to the field's natural width.
        bool nextNum = (t < nt - 1) && toks[t+1].sym != '\0'
          && ! (((toks[t+1].sym == 'M' || toks[t+1].sym == 'e')
                 && toks[t+1].n >= 3) || toks[t+1].sym == 'a');
        int w;
        if (nextNum)
        {
          w = tk.n;
        }
        else
        {
          switch (tk.sym)
          {
            case 'y':
            case 'u':
              w = (tk.n == 2 ? 2 : 6);
              break;
            case 'D':
              w = 3;
              break;
            case 'S':
              w = 9;
              break;
            default:
              w = 2;
              break;
          }
        }
        size_t j = pos;
        double val = 0;
        int ndig = 0;
        while (j < L && (j - pos) < static_cast<size_t> (w)
               && s[j] >= '0' && s[j] <= '9')
        {
          val = val * 10 + (s[j] - '0');
          j++;
          ndig++;
        }
        pos = j;
        if (ndig == 0)
        {
          ok = false;
          break;
        }
        switch (tk.sym)
        {
          case 'y':
          case 'u':
            if (tk.n == 2)
            {
              Yv = base + val;
              if (Yv < pivot)
              {
                Yv += 100;
              }
            }
            else
            {
              Yv = val;
            }
            break;
          case 'M':
            Mv = val;
            break;
          case 'd':
            Dv = val;
            break;
          case 'D':
          {
            // Day of year, resolved through the civil calendar.  Only the
            // month and day are taken, exactly as the m-code does, so a value
            // beyond the end of the year keeps the parsed year.
            long yy;
            unsigned mm, dd;
            civil_from_days (days_from_civil (static_cast<long> (Yv), 1, 1)
                             + static_cast<long> (val) - 1, yy, mm, dd);
            Mv = mm;
            Dv = dd;
            break;
          }
          case 'H':
          case 'h':
          case 'k':
          case 'K':
            Hv = val;
            break;
          case 'm':
            MIv = val;
            break;
          case 's':
            Sv = Sv + val;
            break;
          case 'S':
            Sv = Sv + val / pow (10.0, ndig);
            break;
          default:
            // 'Q'/'G'/'W'/'e' numeric forms carry no component here.
            break;
        }
      }
    }

    if (ok && pos < L)
    {
      ok = false;                  // trailing text not covered by the format
    }
    if (ok)
    {
      if (ampm == 2 && Hv < 12)
      {
        Hv += 12;
      }
      else if (ampm == 1 && Hv == 12)
      {
        Hv = 0;
      }
      // The components must name a date that exists.  MATLAB rejects
      // 2024-04-31 and 2023-02-29 just as it rejects unparseable text, and
      // likewise an hour past 23, a minute past 59, or a second reaching 60.
      ok = (Mv >= 1 && Mv <= 12 && Dv >= 1 && Dv <= days_in_month (Yv, Mv)
            && Hv >= 0 && Hv <= 23 && MIv >= 0 && MIv <= 59
            && Sv >= 0 && Sv < 60);
    }
    if (! ok)
    {
      // A lone string that cannot be converted is an error, but within an
      // array only the offending element is lost, becoming NaT, as MATLAB
      // does -- one bad row must not cost the whole column.
      if (n == 1)
      {
        error ("datetime: could not parse the date/time string '%s' with "
               "'InputFormat' '%s'.", s.c_str (), fmt.c_str ());
      }
      for (int c = 0; c < 6; c++)
      {
        DV(r,c) = numeric_limits<double>::quiet_NaN ();
      }
      continue;
    }
    DV(r,0) = Yv;
    DV(r,1) = Mv;
    DV(r,2) = Dv;
    DV(r,3) = Hv;
    DV(r,4) = MIv;
    DV(r,5) = Sv;
  }
  return DV;
}

// Display name tables for the renderer.  These are deliberately English-only
// and capitalized: MATLAB's 'Format' rendering is not locale aware, only
// 'InputFormat' parsing is.
static const char *R_MABBR[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
static const char *R_MFULL[12] = {"January", "February", "March", "April",
                                  "May", "June", "July", "August", "September",
                                  "October", "November", "December"};
static const char *R_WABBR[7] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri",
                                 "Sat"};
static const char *R_WFULL[7] = {"Sunday", "Monday", "Tuesday", "Wednesday",
                                 "Thursday", "Friday", "Saturday"};
static const char *R_QORD[4] = {"1st quarter", "2nd quarter", "3rd quarter",
                                "4th quarter"};

// sprintf ('%0*d', width, value).
static string
zeropad (int width, long value)
{
  char buf[64];
  snprintf (buf, sizeof (buf), "%0*ld", width, value);
  return string (buf);
}

// Numeric short UTC-offset form ('UTC', 'UTC+3', 'UTC+5:30') used by the
// MATLAB-compatible 'z' style for zones without a named abbreviation
// (dtZoneUTC).
static string
zone_utc (double offSec)
{
  if (fabs (offSec) < 1)
  {
    return "UTC";
  }
  double a = fabs (offSec);
  long hh = static_cast<long> (floor (a / 3600));
  long mm = static_cast<long> (floor (fmod (a, 3600) / 60 + 0.5));
  char sgn = (offSec < 0 ? '-' : '+');
  char buf[64];
  if (mm == 0)
  {
    snprintf (buf, sizeof (buf), "UTC%c%ld", sgn, hh);
  }
  else
  {
    snprintf (buf, sizeof (buf), "UTC%c%ld:%02ld", sgn, hh, mm);
  }
  return string (buf);
}

// MATLAB-compatible 'z' rendering: the IANA letter abbreviation only for the
// North American zones (plus GMT/UTC) that MATLAB names, keyed on the
// (abbreviation, offset) pair so collisions resolve correctly (dtZoneMatlab).
static string
zone_matlab (const string& ab, double offSec)
{
  static const char *names[] = {"EST", "EDT", "CST", "CDT", "MST", "MDT",
                                "PST", "PDT", "AKST", "AKDT", "HST", "HAST",
                                "HADT", "AST", "ADT", "GMT", "UTC"};
  static const int hours[] = {-5, -4, -6, -5, -7, -6, -8, -7, -9, -8, -10,
                              -10, -9, -4, -3, 0, 0};
  static const int nz = sizeof (hours) / sizeof (hours[0]);
  for (int k = 0; k < nz; k++)
  {
    if (ab == names[k] && fabs (offSec - hours[k] * 3600.0) < 1)
    {
      return ab;
    }
  }
  return zone_utc (offSec);
}

// Render one numeric time-zone offset field, the ISO-8601 families Z/X/x
// (dtZoneField).
static string
zone_field (char c, int nn, double offSec)
{
  double a = fabs (offSec);
  long hh = static_cast<long> (floor (a / 3600));
  long mm = static_cast<long> (floor (fmod (a, 3600) / 60));
  char sgn = (offSec < 0 ? '-' : '+');
  char buf[64];
  if (c == 'Z')
  {
    if (nn >= 1 && nn <= 3)
    {
      snprintf (buf, sizeof (buf), "%c%02ld%02ld", sgn, hh, mm);
    }
    else if (nn == 4)
    {
      // Localized GMT format; a zero offset is just 'UTC'.
      if (offSec == 0)
      {
        return "UTC";
      }
      snprintf (buf, sizeof (buf), "UTC%c%02ld:%02ld", sgn, hh, mm);
    }
    else
    {
      // ISO-8601 extended (ZZZZZ); a zero offset renders as 'Z'.
      if (offSec == 0)
      {
        return "Z";
      }
      snprintf (buf, sizeof (buf), "%c%02ld:%02ld", sgn, hh, mm);
    }
  }
  else if (c == 'X')
  {
    if (offSec == 0)
    {
      return "Z";
    }
    if (nn == 1)
    {
      if (mm == 0)
      {
        snprintf (buf, sizeof (buf), "%c%02ld", sgn, hh);
      }
      else
      {
        snprintf (buf, sizeof (buf), "%c%02ld%02ld", sgn, hh, mm);
      }
    }
    else if (nn == 2 || nn == 4)
    {
      snprintf (buf, sizeof (buf), "%c%02ld%02ld", sgn, hh, mm);
    }
    else
    {
      snprintf (buf, sizeof (buf), "%c%02ld:%02ld", sgn, hh, mm);
    }
  }
  else
  {
    if (nn == 1)
    {
      if (mm == 0)
      {
        snprintf (buf, sizeof (buf), "%c%02ld", sgn, hh);
      }
      else
      {
        snprintf (buf, sizeof (buf), "%c%02ld%02ld", sgn, hh, mm);
      }
    }
    else if (nn == 2 || nn == 4)
    {
      snprintf (buf, sizeof (buf), "%c%02ld%02ld", sgn, hh, mm);
    }
    else
    {
      snprintf (buf, sizeof (buf), "%c%02ld:%02ld", sgn, hh, mm);
    }
  }
  return string (buf);
}

// Octave's mod for a positive divisor: the result carries the divisor's sign.
static double
omod (double x, double y)
{
  double r = fmod (x, y);
  if (r != 0 && ((r < 0) != (y < 0)))
  {
    r += y;
  }
  return r;
}

// Render each element of a datetime array under a concrete LDML pattern, a
// port of dtFormatStrings in datetime.m.  The zone-dependent quantities OFF
// (offset in seconds) and ABBR (zone abbreviation) are supplied by the caller,
// which reads them from the compiled tz database; everything else is derived
// here.  NaT elements render as 'NaT' and infinite years as '-Inf'/'Inf'.
static Cell
ldml_format (const NDArray& Y, const NDArray& M, const NDArray& D,
             const NDArray& H, const NDArray& Mi, const NDArray& S,
             const string& fmt, const string& zoneStyle, bool hasTZ,
             const NDArray& off, const Cell& abbr)
{
  vector<Token> toks;
  tokenize (fmt, toks);
  const int nt = static_cast<int> (toks.size ());
  const octave_idx_type n = Y.numel ();
  Cell cstr (Y.dims ());

  for (octave_idx_type k = 0; k < n; k++)
  {
    if (octave::math::isnan (Y(k)))
    {
      cstr(k) = "NaT";
      continue;
    }
    else if (octave::math::isinf (Y(k)))
    {
      cstr(k) = (Y(k) > 0 ? "Inf" : "-Inf");
      continue;
    }

    const long yy = static_cast<long> (Y(k));
    const int mo = static_cast<int> (M(k));
    const int dd = static_cast<int> (D(k));
    const int hh = static_cast<int> (H(k));
    const int mi = static_cast<int> (Mi(k));

    string str;
    for (int t = 0; t < nt; t++)
    {
      const Token& tk = toks[t];
      if (tk.sym == '\0')
      {
        str += tk.lit;
        continue;
      }
      const char c = tk.sym;
      const int nn = tk.n;
      string piece;
      char buf[64];

      switch (c)
      {
        case 'y':
        case 'u':
          if (nn == 2)
          {
            piece = zeropad (2, static_cast<long> (omod (Y(k), 100.0)));
          }
          else
          {
            piece = zeropad (nn, yy);
          }
          break;

        case 'M':
          if (mo < 1 || mo > 12)
          {
            error ("datetime: month out of range while rendering.");
          }
          switch (nn)
          {
            case 1:
              snprintf (buf, sizeof (buf), "%d", mo);
              piece = buf;
              break;
            case 2:
              piece = zeropad (2, mo);
              break;
            case 3:
              piece = R_MABBR[mo-1];
              break;
            case 4:
              piece = R_MFULL[mo-1];
              break;
            default:
              piece = string (1, R_MFULL[mo-1][0]);
              break;
          }
          break;

        case 'd':
          if (nn == 1)
          {
            snprintf (buf, sizeof (buf), "%d", dd);
            piece = buf;
          }
          else
          {
            piece = zeropad (nn, dd);
          }
          break;

        case 'D':
        {
          long doy = days_from_civil (yy, mo, dd)
                     - days_from_civil (yy, 1, 1) + 1;
          piece = zeropad (nn, doy);
          break;
        }

        case 'e':
        {
          // weekday, Sunday = 1 (1970-01-01 was a Thursday).
          long z = days_from_civil (yy, mo, dd);
          int wd = static_cast<int> (omod (z + 4, 7.0)) + 1;
          switch (nn)
          {
            case 1:
              snprintf (buf, sizeof (buf), "%d", wd);
              piece = buf;
              break;
            case 2:
              piece = zeropad (2, wd);
              break;
            case 3:
              piece = R_WABBR[wd-1];
              break;
            case 4:
              piece = R_WFULL[wd-1];
              break;
            default:
              piece = string (1, R_WFULL[wd-1][0]);
              break;
          }
          break;
        }

        case 'H':
          if (nn == 1)
          {
            snprintf (buf, sizeof (buf), "%d", hh);
            piece = buf;
          }
          else
          {
            piece = zeropad (2, hh);
          }
          break;

        case 'h':
        {
          int h12 = static_cast<int> (omod (hh - 1, 12.0)) + 1;
          if (nn == 1)
          {
            snprintf (buf, sizeof (buf), "%d", h12);
            piece = buf;
          }
          else
          {
            piece = zeropad (2, h12);
          }
          break;
        }

        case 'm':
          if (nn == 1)
          {
            snprintf (buf, sizeof (buf), "%d", mi);
            piece = buf;
          }
          else
          {
            piece = zeropad (2, mi);
          }
          break;

        case 's':
        {
          long sec = static_cast<long> (floor (S(k)));
          if (nn == 1)
          {
            snprintf (buf, sizeof (buf), "%ld", sec);
            piece = buf;
          }
          else
          {
            piece = zeropad (2, sec);
          }
          break;
        }

        case 'S':
        {
          // Fractional seconds, truncated to nn digits.  Round to whole
          // microseconds first (the stored precision) so an exact value such
          // as .678 -- held as 0.67799999... -- yields the intended digits.
          long micros = static_cast<long> (floor ((S(k) - floor (S(k)))
                                                  * 1e6 + 0.5));
          if (nn <= 6)
          {
            long p = 1;
            for (int q = 0; q < 6 - nn; q++)
            {
              p *= 10;
            }
            piece = zeropad (nn, micros / p);
          }
          else
          {
            long p = 1;
            for (int q = 0; q < nn - 6; q++)
            {
              p *= 10;
            }
            piece = zeropad (nn, micros * p);
          }
          break;
        }

        case 'a':
          piece = (hh >= 12 ? "PM" : "AM");
          break;

        case 'Q':
        {
          int q = (mo - 1) / 3 + 1;
          switch (nn)
          {
            case 1:
              snprintf (buf, sizeof (buf), "%d", q);
              piece = buf;
              break;
            case 2:
              piece = zeropad (2, q);
              break;
            case 3:
              snprintf (buf, sizeof (buf), "Q%d", q);
              piece = buf;
              break;
            case 4:
              piece = R_QORD[q-1];
              break;
            default:
              snprintf (buf, sizeof (buf), "%d", q);
              piece = buf;
              break;
          }
          break;
        }

        case 'G':
          piece = "CE";
          break;

        case 'W':
        {
          // Week of the month, Sunday based, first week = week 1.
          long z1 = days_from_civil (yy, mo, 1);
          int firstDow = static_cast<int> (omod (z1 + 4, 7.0)) + 1;
          int w = (dd - 1 + (firstDow - 1)) / 7 + 1;
          snprintf (buf, sizeof (buf), "%d", w);
          piece = buf;
          break;
        }

        case 'z':
        case 'Z':
        case 'X':
        case 'x':
          if (! hasTZ)
          {
            piece = string (nn, '*');
          }
          else if (c == 'z')
          {
            // 'z'/'zz'/'zzz' follow the session style; 'zzzz' and 'zzzzz' are
            // Octave-specific per-format overrides.
            string zst;
            if (nn == 4)
            {
              zst = "iana";
            }
            else if (nn >= 5)
            {
              zst = "matlab";
            }
            else
            {
              zst = zoneStyle;
            }
            string ab = abbr(k).string_value ();
            if (zst == "iana")
            {
              piece = ab;
            }
            else
            {
              piece = zone_matlab (ab, off(k));
            }
          }
          else
          {
            piece = zone_field (c, nn, off(k));
          }
          break;

        default:
          error ("datetime: unsupported format symbol '%c'.", c);
      }
      str += piece;
    }
    cstr(k) = str;
  }
  return cstr;
}

DEFUN_DLD (__ldml__, args, ,
           "-*- texinfo -*-\n\
 @deftypefn {} {@var{DV} =} __ldml__ (\"parse\", @var{strs}, @var{fmt}, \
@var{pivot}, @var{locale})\n\
\n\
\n\
Parse date/time strings under an LDML @qcode{'InputFormat'} pattern. \n\
\n\
@var{strs} is a cell array of character vectors, @var{fmt} an LDML pattern, \
@var{pivot} the two-digit year pivot, and @var{locale} a language tag such as \
@qcode{'fr_FR'} (empty or @qcode{'system'} for English).  The return value is \
an N-by-6 date-vector matrix. \n\
\n\
This is a helper function for the @qcode{datetime} class of the `datatypes` \
package.  Do NOT use this function directly. \n\
\n\
@end deftypefn")
{
  if (args.length () < 1 || ! args(0).is_string ())
  {
    error ("__ldml__: first argument must be an action string.");
  }
  string action = args(0).string_value ();

  if (action == "parse")
  {
    if (args.length () != 5)
    {
      error ("__ldml__: 'parse' takes STRS, FMT, PIVOT, and LOCALE.");
    }
    if (! args(1).iscellstr ())
    {
      error ("__ldml__: STRS must be a cell array of character vectors.");
    }
    if (! args(2).is_string ())
    {
      error ("__ldml__: FMT must be a character vector.");
    }
    Cell strs = args(1).cell_value ();
    string fmt = args(2).string_value ();
    double pivot = args(3).double_value ();
    string locale = (args(4).is_string () ? args(4).string_value ()
                                          : string ());
    int lidx = locale_index (locale);
    if (lidx < 0)
    {
      error ("__ldml__: unsupported locale '%s'.", locale.c_str ());
    }
    return ovl (ldml_parse (strs, fmt, pivot, lidx));
  }

  if (action == "format")
  {
    if (args.length () != 12)
    {
      error ("__ldml__: 'format' takes Y, M, D, H, MI, S, FMT, ZONESTYLE, "
             "HASTZ, OFF, and ABBR.");
    }
    NDArray Y = args(1).array_value ();
    NDArray M = args(2).array_value ();
    NDArray D = args(3).array_value ();
    NDArray H = args(4).array_value ();
    NDArray Mi = args(5).array_value ();
    NDArray S = args(6).array_value ();
    string fmt = args(7).string_value ();
    string zoneStyle = args(8).string_value ();
    bool hasTZ = args(9).bool_value ();
    NDArray off = args(10).array_value ();
    Cell abbr = (args(11).iscell () ? args(11).cell_value () : Cell ());
    return ovl (ldml_format (Y, M, D, H, Mi, S, fmt, zoneStyle, hasTZ, off,
                             abbr));
  }

  error ("__ldml__: unknown action '%s'.", action.c_str ());
}
