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

#include <cerrno>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <string>
#include <vector>

#include <octave/oct.h>
#include <octave/Cell.h>
#include <octave/oct-map.h>

using namespace std;

// Nesting deeper than this is refused: readdictionary follows the tree
// recursively, a few calls a level, within Octave's recursion limit
static const int max_depth = 64;

class json_parser
{
public:

  json_parser (const string& text, bool comments, bool commas, bool infnan)
    : s (text), pos (0), allow_comments (comments), allow_commas (commas),
      allow_infnan (infnan), err ()
  { }

  // The value of the whole text, or an error message in ERR
  octave_value
  parse ()
  {
    // A UTF-8 byte order mark is not part of the text
    if (s.compare (0, 3, "\xEF\xBB\xBF") == 0)
      pos = 3;
    octave_value v;
    if (! skip_space ())
      return v;
    if (pos >= s.size ())
      {
        fail ("the text holds no value");
        return v;
      }
    v = value (0);
    if (! err.empty ())
      return octave_value ();
    if (! skip_space ())
      return octave_value ();
    if (pos < s.size ())
      {
        fail ("unexpected text after the value");
        return octave_value ();
      }
    return v;
  }

  const string&
  error_message () const
  {
    return err;
  }

private:

  const string& s;
  size_t pos;
  bool allow_comments;
  bool allow_commas;
  bool allow_infnan;
  string err;

  // Record the first error, with the line and column of POS
  void
  fail (const string& what)
  {
    if (! err.empty ())
      return;
    size_t line = 1;
    size_t col = 1;
    for (size_t k = 0; k < pos && k < s.size (); k++)
      {
        if (s[k] == '\n')
          {
            line++;
            col = 1;
          }
        // A column counts characters, not the bytes that continue one
        else if ((static_cast<unsigned char> (s[k]) & 0xC0) != 0x80)
          col++;
      }
    err = what + " at line " + to_string (line) + ", column "
          + to_string (col) + ".";
  }

  // Skip white space and, when allowed, comments; false on an error
  bool
  skip_space ()
  {
    while (pos < s.size ())
      {
        char c = s[pos];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
          pos++;
        else if (c == '/' && pos + 1 < s.size ()
                 && (s[pos+1] == '/' || s[pos+1] == '*'))
          {
            if (! allow_comments)
              {
                fail ("comments are not allowed");
                return false;
              }
            if (s[pos+1] == '/')
              {
                while (pos < s.size () && s[pos] != '\n')
                  pos++;
              }
            else
              {
                size_t end = s.find ("*/", pos + 2);
                if (end == string::npos)
                  {
                    fail ("unterminated comment");
                    return false;
                  }
                pos = end + 2;
              }
          }
        else
          break;
      }
    return true;
  }

  bool
  starts_with (const char *word) const
  {
    return s.compare (pos, strlen (word), word) == 0;
  }

  octave_value
  value (int depth)
  {
    if (depth > max_depth)
      {
        fail ("nesting too deep");
        return octave_value ();
      }
    if (pos >= s.size ())
      {
        fail ("unexpected end of the text");
        return octave_value ();
      }
    char c = s[pos];
    if (c == '{')
      return object (depth);
    if (c == '[')
      return array (depth);
    if (c == '"')
      {
        string str;
        if (! text (str))
          return octave_value ();
        return octave_value (str);
      }
    if (starts_with ("true"))
      {
        pos += 4;
        return octave_value (true);
      }
    if (starts_with ("false"))
      {
        pos += 5;
        return octave_value (false);
      }
    if (starts_with ("null"))
      {
        pos += 4;
        // A struct array of no elements and no fields marks a null
        return octave_value (octave_map (dim_vector (0, 0)));
      }
    if (c == '-' || (c >= '0' && c <= '9') || c == 'N' || c == 'I')
      return number ();
    fail ("invalid value");
    return octave_value ();
  }

  octave_value
  object (int depth)
  {
    pos++;
    // Gathered first and made cells once: growing a cell copies it
    vector<octave_value> keys;
    vector<octave_value> vals;
    if (! skip_space ())
      return octave_value ();
    if (pos < s.size () && s[pos] == '}')
      {
        pos++;
        return make_object (keys, vals);
      }
    while (true)
      {
        if (pos >= s.size () || s[pos] != '"')
          {
            if (pos < s.size () && s[pos] == '}' && ! keys.empty ()
                && ! allow_commas)
              fail ("trailing commas are not allowed");
            else
              fail ("missing object key");
            return octave_value ();
          }
        string key;
        if (! text (key))
          return octave_value ();
        if (! skip_space ())
          return octave_value ();
        if (pos >= s.size () || s[pos] != ':')
          {
            fail ("missing ':' after an object key");
            return octave_value ();
          }
        pos++;
        if (! skip_space ())
          return octave_value ();
        octave_value v = value (depth + 1);
        if (! err.empty ())
          return octave_value ();
        keys.push_back (octave_value (key));
        vals.push_back (v);
        if (! skip_space ())
          return octave_value ();
        if (pos < s.size () && s[pos] == ',')
          {
            pos++;
            if (! skip_space ())
              return octave_value ();
            if (pos < s.size () && s[pos] == '}' && allow_commas)
              {
                pos++;
                break;
              }
            continue;
          }
        if (pos < s.size () && s[pos] == '}')
          {
            pos++;
            break;
          }
        fail ("missing ',' or '}' in an object");
        return octave_value ();
      }
    return make_object (keys, vals);
  }

  // A column cell of ITEMS
  static Cell
  column (const vector<octave_value>& items)
  {
    Cell c (dim_vector (items.size (), 1));
    for (size_t k = 0; k < items.size (); k++)
      c(k) = items[k];
    return c;
  }

  static octave_value
  make_object (const vector<octave_value>& keys,
               const vector<octave_value>& vals)
  {
    octave_scalar_map m;
    m.assign ("keys", octave_value (column (keys)));
    m.assign ("values", octave_value (column (vals)));
    return octave_value (m);
  }

  octave_value
  array (int depth)
  {
    pos++;
    vector<octave_value> items;
    if (! skip_space ())
      return octave_value ();
    if (pos < s.size () && s[pos] == ']')
      {
        pos++;
        return octave_value (column (items));
      }
    while (true)
      {
        if (pos < s.size () && s[pos] == ']')
          {
            fail (allow_commas ? "invalid value"
                               : "trailing commas are not allowed");
            return octave_value ();
          }
        octave_value v = value (depth + 1);
        if (! err.empty ())
          return octave_value ();
        items.push_back (v);
        if (! skip_space ())
          return octave_value ();
        if (pos < s.size () && s[pos] == ',')
          {
            pos++;
            if (! skip_space ())
              return octave_value ();
            if (pos < s.size () && s[pos] == ']' && allow_commas)
              {
                pos++;
                break;
              }
            continue;
          }
        if (pos < s.size () && s[pos] == ']')
          {
            pos++;
            break;
          }
        fail ("missing ',' or ']' in an array");
        return octave_value ();
      }
    return octave_value (column (items));
  }

  static void
  put_utf8 (string& out, unsigned long cp)
  {
    if (cp < 0x80)
      out += static_cast<char> (cp);
    else if (cp < 0x800)
      {
        out += static_cast<char> (0xC0 | (cp >> 6));
        out += static_cast<char> (0x80 | (cp & 0x3F));
      }
    else if (cp < 0x10000)
      {
        out += static_cast<char> (0xE0 | (cp >> 12));
        out += static_cast<char> (0x80 | ((cp >> 6) & 0x3F));
        out += static_cast<char> (0x80 | (cp & 0x3F));
      }
    else
      {
        out += static_cast<char> (0xF0 | (cp >> 18));
        out += static_cast<char> (0x80 | ((cp >> 12) & 0x3F));
        out += static_cast<char> (0x80 | ((cp >> 6) & 0x3F));
        out += static_cast<char> (0x80 | (cp & 0x3F));
      }
  }

  // Four hexadecimal digits at POS, or -1
  long
  hex4 ()
  {
    if (pos + 4 > s.size ())
      return -1;
    long v = 0;
    for (int k = 0; k < 4; k++)
      {
        char c = s[pos+k];
        v <<= 4;
        if (c >= '0' && c <= '9')
          v |= c - '0';
        else if (c >= 'a' && c <= 'f')
          v |= c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
          v |= c - 'A' + 10;
        else
          return -1;
      }
    pos += 4;
    return v;
  }

  // The text at POS, which starts with its opening quote, into OUT
  bool
  text (string& out)
  {
    pos++;
    while (true)
      {
        if (pos >= s.size ())
          {
            fail ("unterminated text");
            return false;
          }
        unsigned char c = s[pos];
        if (c == '"')
          {
            pos++;
            return true;
          }
        if (c < 0x20)
          {
            fail ("a control character in text must be escaped");
            return false;
          }
        if (c != '\\')
          {
            out += static_cast<char> (c);
            pos++;
            continue;
          }
        pos++;
        if (pos >= s.size ())
          {
            fail ("unterminated text");
            return false;
          }
        char e = s[pos++];
        switch (e)
          {
          case '"':  out += '"';  break;
          case '\\': out += '\\'; break;
          case '/':  out += '/';  break;
          case 'b':  out += '\b'; break;
          case 'f':  out += '\f'; break;
          case 'n':  out += '\n'; break;
          case 'r':  out += '\r'; break;
          case 't':  out += '\t'; break;
          case 'u':
            {
              long cp = hex4 ();
              if (cp < 0)
                {
                  fail ("invalid \\u escape");
                  return false;
                }
              // A high surrogate must be followed by a low one
              if (cp >= 0xD800 && cp <= 0xDBFF)
                {
                  long lo = -1;
                  if (pos + 1 < s.size () && s[pos] == '\\' && s[pos+1] == 'u')
                    {
                      pos += 2;
                      lo = hex4 ();
                    }
                  if (lo < 0xDC00 || lo > 0xDFFF)
                    {
                      fail ("invalid \\u escape");
                      return false;
                    }
                  cp = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
                }
              else if (cp >= 0xDC00 && cp <= 0xDFFF)
                {
                  fail ("invalid \\u escape");
                  return false;
                }
              put_utf8 (out, cp);
              break;
            }
          default:
            pos--;
            fail ("invalid escape in text");
            return false;
          }
      }
  }

  octave_value
  number ()
  {
    size_t start = pos;
    bool neg = false;
    if (s[pos] == '-')
      {
        neg = true;
        pos++;
      }
    // The lenient forms of the non-finite numbers
    if (allow_infnan)
      {
        if (! neg && starts_with ("NaN"))
          {
            pos += 3;
            return octave_value (octave_NaN);
          }
        if (starts_with ("Infinity") || starts_with ("Inf"))
          {
            pos += starts_with ("Infinity") ? 8 : 3;
            return octave_value (neg ? -octave_Inf : octave_Inf);
          }
      }
    if (pos >= s.size () || s[pos] < '0' || s[pos] > '9')
      {
        pos = start;
        fail ("invalid value");
        return octave_value ();
      }
    // No leading zeros, as JSON writes numbers
    if (s[pos] == '0' && pos + 1 < s.size ()
        && s[pos+1] >= '0' && s[pos+1] <= '9')
      {
        pos = start;
        fail ("invalid number");
        return octave_value ();
      }
    while (pos < s.size () && s[pos] >= '0' && s[pos] <= '9')
      pos++;
    bool integral = true;
    if (pos < s.size () && s[pos] == '.')
      {
        integral = false;
        pos++;
        if (pos >= s.size () || s[pos] < '0' || s[pos] > '9')
          {
            fail ("invalid number");
            return octave_value ();
          }
        while (pos < s.size () && s[pos] >= '0' && s[pos] <= '9')
          pos++;
      }
    if (pos < s.size () && (s[pos] == 'e' || s[pos] == 'E'))
      {
        integral = false;
        pos++;
        if (pos < s.size () && (s[pos] == '+' || s[pos] == '-'))
          pos++;
        if (pos >= s.size () || s[pos] < '0' || s[pos] > '9')
          {
            fail ("invalid number");
            return octave_value ();
          }
        while (pos < s.size () && s[pos] >= '0' && s[pos] <= '9')
          pos++;
      }
    string lit = s.substr (start, pos - start);
    double d = strtod (lit.c_str (), nullptr);
    // An integer beyond 2^53 is kept exactly when a 64-bit type holds it.
    // The double rounds 2^53 + 1 down to 2^53, so the test is on the exact
    // integer.
    const long long flint = 9007199254740992LL;
    if (integral && fabs (d) >= 9007199254740992.0)
      {
        errno = 0;
        if (neg)
          {
            long long v = strtoll (lit.c_str (), nullptr, 10);
            if (errno == 0 && v < -flint)
              return octave_value (octave_int64 (static_cast<int64_t> (v)));
          }
        else
          {
            unsigned long long v = strtoull (lit.c_str (), nullptr, 10);
            if (errno == 0 && v > static_cast<unsigned long long> (flint))
              return octave_value (octave_uint64 (static_cast<uint64_t> (v)));
          }
      }
    return octave_value (d);
  }
};

DEFUN_DLD (__json2dictionary__, args, nargout,
           "-*- texinfo -*-\n \
 @deftypefn {datatypes} {[@var{tree}, @var{errmsg}] =} __json2dictionary__ (@var{text}, @var{comments}, @var{commas}, @var{infnan})\n\
\n\
\n\
Parse JSON text into a tree that keeps the kinds of JSON values.\n\
\n\
An object is a scalar structure with the fields @qcode{keys} and \
@qcode{values}, two column cell arrays in file order, duplicate keys \
kept.  An array is a column cell array, text a character row, a number a \
@code{double}, or an @code{int64} or @code{uint64} for an integer beyond \
2^53 that one holds exactly, @qcode{true} and @qcode{false} logical \
scalars, and @qcode{null} a 0-by-0 structure array with no fields.  \
@var{comments}, @var{commas} and @var{infnan} allow comments, trailing \
commas and the non-finite numbers.  On an error @var{tree} is empty and \
@var{errmsg} names it with its line and column.\n\
\n\
This is a helper IO function for the @qcode{readdictionary} function.  Do \
NOT call it directly.\n\
\n\
@end deftypefn")
{
  if (args.length () != 4)
    error ("__json2dictionary__: four input arguments are required.");
  if (! args(0).is_string ())
    error ("__json2dictionary__: TEXT must be a character vector.");
  string text = args(0).string_value ();
  json_parser p (text, args(1).bool_value (), args(2).bool_value (),
                 args(3).bool_value ());
  octave_value tree = p.parse ();
  octave_value_list retval (nargout > 1 ? 2 : 1);
  retval(0) = p.error_message ().empty () ? tree : octave_value (Matrix ());
  if (nargout > 1)
    retval(1) = p.error_message ();
  return retval;
}
