## Copyright (C) 2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

## -*- texinfo -*-
## @deftypefn {datatypes} {@var{d} =} configureDictionary (@var{keyType}, @var{valueType})
##
## Create a dictionary with key and value types and no entries.
##
## @code{@var{d} = configureDictionary (@var{keyType}, @var{valueType})}
## returns a configured @code{dictionary} with no entries, whose keys are of
## class @var{keyType} and whose values are of class @var{valueType}, each
## given as a character vector or a string scalar.  The names are matched in
## case, and @qcode{'char'} gives @qcode{'string'}, since a character vector
## is stored as a string scalar.
##
## @var{valueType} may name a built-in type (@qcode{'double'},
## @qcode{'logical'}, @qcode{'cell'}, @qcode{'struct'},
## @qcode{'function_handle'}, @dots{}) or any class that is not abstract.
## @var{keyType} must also name a type that @code{keyHash} can hash: a
## built-in type or a class with a @code{keyHash} method, such as
## @code{string}, @code{datetime}, @code{duration}, @code{calendarDuration}
## and @code{categorical}.  MATLAB accepts any class there, a @code{table}
## or a @code{timetable} among them, and refuses the key only when an entry
## is added.
##
## @seealso{dictionary, dictionary.types, dictionary.isConfigured, keyHash}
## @end deftypefn
function d = configureDictionary (keyType, valueType)

  if (nargin != 2)
    print_usage ();
  endif
  [keyType, errmsg] = typeName (keyType, 'KEYTYPE');
  if (! isempty (errmsg))
    error ("configureDictionary: %s", errmsg);
  endif
  [valueType, errmsg] = typeName (valueType, 'VALUETYPE');
  if (! isempty (errmsg))
    error ("configureDictionary: %s", errmsg);
  endif
  if (! dictionary.hashable (keyType))
    error ("configureDictionary: unsupported key type '%s'.", keyType);
  endif
  d = dictionary.configured (keyType, valueType);

endfunction

## The class names that are not classdef classes
function names = builtinTypes ()
  names = {'double', 'single', 'int8', 'int16', 'int32', 'int64', 'uint8', ...
           'uint16', 'uint32', 'uint64', 'logical', 'char', 'cell', ...
           'struct', 'function_handle'};
endfunction

## The class named by X, the argument ARGNAME, with 'char' as 'string'
function [name, errmsg] = typeName (x, argname)
  name = '';
  errmsg = '';
  if (! ((ischar (x) && rows (x) <= 1) || (isa (x, 'string') && isscalar (x))))
    errmsg = sprintf ("%s must be a character vector or a string scalar.", ...
                      argname);
    return;
  endif
  name = char (x);
  if (strcmp (name, 'char'))
    name = 'string';
  elseif (! any (strcmp (name, builtinTypes ())))
    mc = meta.class.fromName (name);
    if (isempty (mc))
      errmsg = sprintf ("unknown type '%s'.", name);
    elseif (mc.Abstract)
      errmsg = sprintf ("'%s' is an abstract class.", name);
    endif
  endif
endfunction

%!test
%! [kt, vt] = types (configureDictionary ('string', 'double'));
%! assert_equal (kt, string ('string'));
%! assert_equal (vt, string ('double'));
%!test
%! [kt, vt] = types (configureDictionary (string ('int8'), string ('cell')));
%! assert_equal (kt, string ('int8'));
%! assert_equal (vt, string ('cell'));
%!test
%! [kt, vt] = types (configureDictionary ('char', 'char'));
%! assert_equal (kt, string ('string'));
%! assert_equal (vt, string ('string'));
%!assert_equal (isConfigured (configureDictionary ('double', 'double')), true)
%!assert_equal (numEntries (configureDictionary ('double', 'double')), 0)
%!test
%! [kt, vt] = types (configureDictionary ('logical', 'function_handle'));
%! assert_equal (kt, string ('logical'));
%! assert_equal (vt, string ('function_handle'));
%!test
%! [kt, vt] = types (configureDictionary ('struct', 'struct'));
%! assert_equal (kt, string ('struct'));
%! assert_equal (vt, string ('struct'));
%!test
%! [kt, vt] = types (configureDictionary ('cell', 'single'));
%! assert_equal (kt, string ('cell'));
%! assert_equal (vt, string ('single'));
%!test
%! [kt, vt] = types (configureDictionary ('function_handle', 'uint64'));
%! assert_equal (kt, string ('function_handle'));
%! assert_equal (vt, string ('uint64'));
%!test
%! [kt, vt] = types (configureDictionary ('datetime', 'duration'));
%! assert_equal (kt, string ('datetime'));
%! assert_equal (vt, string ('duration'));
%!test
%! [kt, vt] = types (configureDictionary ('calendarDuration', 'categorical'));
%! assert_equal (kt, string ('calendarDuration'));
%! assert_equal (vt, string ('categorical'));
%!test
%! [kt, vt] = types (configureDictionary ('categorical', 'calendarDuration'));
%! assert_equal (kt, string ('categorical'));
%! assert_equal (vt, string ('calendarDuration'));
%!test
%! [kt, vt] = types (configureDictionary ('duration', 'datetime'));
%! assert_equal (kt, string ('duration'));
%! assert_equal (vt, string ('datetime'));
%!test
%! [~, vt] = types (configureDictionary ('string', 'table'));
%! assert_equal (vt, string ('table'));
%!test
%! [~, vt] = types (configureDictionary ('string', 'timetable'));
%! assert_equal (vt, string ('timetable'));
%!test
%! [~, vt] = types (configureDictionary ('string', 'dictionary'));
%! assert_equal (vt, string ('dictionary'));
%!test
%! [~, vt] = types (configureDictionary ('string', 'tsdata.event'));
%! assert_equal (vt, string ('tsdata.event'));
%!test
%! [~, vt] = types (configureDictionary ('string', 'missing'));
%! assert_equal (vt, string ('missing'));

%!error<configureDictionary: KEYTYPE must be a character vector or a string scalar.> ...
%! configureDictionary (5, 'double')
%!error<configureDictionary: KEYTYPE must be a character vector or a string scalar.> ...
%! configureDictionary ({'string'}, 'double')
%!error<configureDictionary: KEYTYPE must be a character vector or a string scalar.> ...
%! configureDictionary (string ({'string', 'double'}), 'double')
%!error<configureDictionary: KEYTYPE must be a character vector or a string scalar.> ...
%! configureDictionary (['ab'; 'cd'], 'double')
%!error<configureDictionary: VALUETYPE must be a character vector or a string scalar.> ...
%! configureDictionary ('string', 5)
%!error<configureDictionary: unknown type 'String'.> ...
%! configureDictionary ('String', 'double')
%!error<configureDictionary: unknown type 'Double'.> ...
%! configureDictionary ('string', 'Double')
%!error<configureDictionary: unknown type ''.> configureDictionary ('', 'double')
%!error<configureDictionary: unknown type ''.> configureDictionary ('string', '')
%!error<configureDictionary: unknown type 'int128'.> ...
%! configureDictionary ('string', 'int128')
%!error<configureDictionary: 'tabular' is an abstract class.> ...
%! configureDictionary ('string', 'tabular')
%!error<configureDictionary: 'tabular' is an abstract class.> ...
%! configureDictionary ('tabular', 'double')
%!error<configureDictionary: unsupported key type 'table'.> ...
%! configureDictionary ('table', 'double')
%!error<configureDictionary: unsupported key type 'timetable'.> ...
%! configureDictionary ('timetable', 'double')
%!error<configureDictionary: unsupported key type 'timeseries'.> ...
%! configureDictionary ('timeseries', 'double')
