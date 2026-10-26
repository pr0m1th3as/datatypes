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
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <string>

#include <octave/oct.h>
#include <octave/Cell.h>
#include <octave/oct-map.h>

using namespace std;

class json_writer
{
public:

  json_writer (bool pretty, bool preserve)
    : out (), pretty (pretty), preserve (preserve)
  { }

  string out;

  void
  value (const octave_value& v, int level)
  {
    if (v.isstruct ())
      {
        octave_map m = v.map_value ();
        if (m.nfields () == 0)
          out += "null";
        else
          object (m.contents ("keys")(0).cell_value (),
                  m.contents ("values")(0).cell_value (), level);
      }
    else if (v.iscell ())
      array (v.cell_value (), level);
    else if (v.is_string ())
      text (v.string_value ());
    else if (v.numel () != 1)
      numbers (v, level);
    else
      scalar (v);
  }

private:

  bool pretty;
  bool preserve;

  void
  newline (int level)
  {
    if (pretty)
      {
        out += '\n';
        out.append (4 * level, ' ');
      }
  }

  void
  object (const Cell& keys, const Cell& vals, int level)
  {
    if (keys.numel () == 0)
      {
        out += "{}";
        return;
      }
    out += '{';
    for (octave_idx_type k = 0; k < keys.numel (); k++)
      {
        if (k > 0)
          out += ',';
        newline (level + 1);
        key (keys(k));
        out += pretty ? ": " : ":";
        value (vals(k), level + 1);
      }
    newline (level);
    out += '}';
  }

  // A key given as text, or as a number or logical value written as text
  void
  key (const octave_value& k)
  {
    if (k.is_string ())
      text (k.string_value ());
    else if (k.islogical ())
      text (k.bool_value () ? "true" : "false");
    else if (k.isinteger ())
      text (integer (k));
    else
      text (shortest (k.double_value (), false));
  }

  void
  array (const Cell& items, int level)
  {
    if (items.numel () == 0)
      {
        out += "[]";
        return;
      }
    out += '[';
    for (octave_idx_type k = 0; k < items.numel (); k++)
      {
        if (k > 0)
          out += ',';
        newline (level + 1);
        value (items(k), level + 1);
      }
    newline (level);
    out += ']';
  }

  // A numeric or logical vector, as an array of its elements
  void
  numbers (const octave_value& v, int level)
  {
    octave_idx_type n = v.numel ();
    if (n == 0)
      {
        out += "[]";
        return;
      }
    // Indexing needs a value it may change
    octave_value w = v;
    out += '[';
    for (octave_idx_type k = 0; k < n; k++)
      {
        if (k > 0)
          out += ',';
        newline (level + 1);
        scalar (w.index_op (octave_value_list (octave_value (k + 1))));
      }
    newline (level);
    out += ']';
  }

  void
  scalar (const octave_value& v)
  {
    if (v.islogical ())
      out += v.bool_value () ? "true" : "false";
    else if (v.isinteger ())
      out += integer (v);
    else
      {
        double d = v.double_value ();
        if (std::isnan (d))
          out += preserve ? "NaN" : "null";
        else if (std::isinf (d))
          out += preserve ? (d > 0 ? "Infinity" : "-Infinity") : "null";
        else
          out += shortest (d, true);
      }
  }

  // An integer of any class, exactly
  static string
  integer (const octave_value& v)
  {
    char buf[32];
    if (v.is_uint64_type ())
      snprintf (buf, sizeof (buf), "%llu", static_cast<unsigned long long>
                (v.uint64_scalar_value ().value ()));
    else if (v.is_int64_type ())
      snprintf (buf, sizeof (buf), "%lld", static_cast<long long>
                (v.int64_scalar_value ().value ()));
    else if (v.is_uint32_type () || v.is_uint16_type ()
             || v.is_uint8_type ())
      snprintf (buf, sizeof (buf), "%llu", static_cast<unsigned long long>
                (v.uint32_scalar_value ().value ()));
    else
      snprintf (buf, sizeof (buf), "%lld", static_cast<long long>
                (v.int32_scalar_value ().value ()));
    return string (buf);
  }

  // The double D in the fewest digits that read back as D, laid out as
  // RapidJSON lays them out: in full up to 1e21, with '.0' when integral
  // and POINT is true, as 0.000ddd down to 1e-6, else with an exponent
  static string
  shortest (double d, bool point)
  {
    if (std::isnan (d))
      return "NaN";
    if (std::isinf (d))
      return d > 0 ? "Inf" : "-Inf";
    string sign = std::signbit (d) ? "-" : "";
    double a = fabs (d);
    if (a == 0)
      return sign + (point ? "0.0" : "0");
    char buf[40];
    int p = 1;
    for (; p <= 17; p++)
      {
        snprintf (buf, sizeof (buf), "%.*e", p - 1, a);
        if (strtod (buf, nullptr) == a)
          break;
      }
    // The digits and the decimal exponent of the first one
    string mant (buf, strchr (buf, 'e') - buf);
    int e10 = atoi (strchr (buf, 'e') + 1);
    string digits;
    for (char c : mant)
      if (c != '.')
        digits += c;
    while (digits.size () > 1 && digits.back () == '0')
      digits.pop_back ();
    int n = digits.size ();
    int kk = e10 + 1;
    string r;
    if (kk >= n && kk <= 21)
      {
        r = digits + string (kk - n, '0');
        if (point)
          r += ".0";
      }
    else if (kk > 0 && kk <= 21)
      r = digits.substr (0, kk) + "." + digits.substr (kk);
    else if (kk > -6 && kk <= 0)
      r = "0." + string (-kk, '0') + digits;
    else if (n == 1)
      r = digits + "e" + to_string (kk - 1);
    else
      r = digits.substr (0, 1) + "." + digits.substr (1) + "e"
          + to_string (kk - 1);
    return sign + r;
  }

  void
  text (const string& s)
  {
    out += '"';
    for (unsigned char c : s)
      {
        switch (c)
          {
          case '"':  out += "\\\""; break;
          case '\\': out += "\\\\"; break;
          case '\n': out += "\\n";  break;
          case '\r': out += "\\r";  break;
          case '\t': out += "\\t";  break;
          case '\b': out += "\\b";  break;
          case '\f': out += "\\f";  break;
          default:
            if (c < 0x20)
              {
                char buf[8];
                snprintf (buf, sizeof (buf), "\\u%04x", c);
                out += buf;
              }
            else
              out += static_cast<char> (c);
          }
      }
    out += '"';
  }
};

DEFUN_DLD (__dictionary2json__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {@var{errmsg} =} __dictionary2json__ (@var{tree}, @var{file}, @var{pretty}, @var{preserve})\n\
\n\
\n\
Write a tree of JSON values to a file.\n\
\n\
@var{tree} has the form @qcode{__json2dictionary__} returns: an object is a \
scalar structure with the fields @qcode{keys} and @qcode{values}, the keys \
text, numbers or logical values; an array a cell array; text a character \
row; a number or logical value a scalar, a vector of them an array; and \
@qcode{null} a 0-by-0 structure array with no fields.  @var{pretty} \
indents by four spaces, @var{preserve} writes @code{NaN} and @code{Inf} \
rather than @qcode{null}.  @var{errmsg} is empty, or names the file that \
could not be written.\n\
\n\
This is a helper IO function for the @qcode{writedictionary} method.  Do \
NOT call it directly.\n\
\n\
@end deftypefn")
{
  if (args.length () != 4)
    error ("__dictionary2json__: four input arguments are required.");
  json_writer w (args(2).bool_value (), args(3).bool_value ());
  w.value (args(0), 0);
  string file = args(1).string_value ();
  string errmsg;
  ofstream fd (file.c_str (), ios::out | ios::binary | ios::trunc);
  if (! fd.is_open ())
    errmsg = "unable to open the file '" + file + "' for writing.";
  else
    {
      fd << w.out;
      fd.close ();
      if (fd.fail ())
        errmsg = "unable to write the file '" + file + "'.";
    }
  octave_value_list retval (nargout > 0 ? 1 : 0);
  if (nargout > 0)
    retval(0) = errmsg;
  return retval;
}
