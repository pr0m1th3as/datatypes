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
## @deftypefn  {datatypes} {@var{d} =} readdictionary (@var{filename})
## @deftypefnx {datatypes} {@var{d} =} readdictionary (@var{filename}, @var{Name}, @var{Value})
##
## Read a dictionary from a JSON or an OpenDocument spreadsheet file.
##
## @code{@var{d} = readdictionary (@var{filename})} reads the JSON object in
## the file @var{filename}, a character vector or a string scalar naming a
## local file or an @qcode{http} or @qcode{https} address, and returns it as a
## @code{dictionary} with @code{string} keys in the order of the file.  The
## value type is chosen from the values, all of them together:
##
## @itemize
## @item numbers give @code{double}; integers beyond @math{2^53} give
## @code{uint64}, or @code{int64} when one is negative and all fit, and a
## number with a fraction among them gives @code{cell};
## @item text gives @code{string}; dates in ISO 8601 form without a time zone
## (@qcode{2024-01-01}, @qcode{2024-01-01T10:00:00}) or as
## @qcode{01-Jan-2024} give @code{datetime}, and times as
## @qcode{01:30:00} give @code{duration}; a timestamp with a zone mark
## (@qcode{Z}, @qcode{+02:00}) stays text;
## @item @qcode{true} and @qcode{false} give @code{logical};
## @item @qcode{null} is missing: @code{NaN}, a missing string or @code{NaT}
## among the values above, the @code{missing} type when all are
## @qcode{null}; among @code{logical} or 64-bit integer values, which have no
## missing value, it gives @code{cell};
## @item any other mixture, an array, or an object gives @code{cell}: an
## array becomes a column cell array of its elements, each typed alone, and
## an object a dictionary read by these rules, @qcode{@{@}} an unconfigured
## one.
## @end itemize
##
## @code{@var{d} = readdictionary (@var{filename}, @var{Name}, @var{Value})}
## specifies options with the following Name-Value paired arguments, whose
## names and text values are matched in any case.
##
## @multitable @columnfractions 0.25 0.75
## @headitem @var{Name} @tab @var{Value}
## @item @qcode{'FileType'} @tab @qcode{'auto'} (the default) tells the type
## from the extension, @qcode{.json}, @qcode{.ods} or @qcode{.fods};
## @qcode{'json'} or @qcode{'ods'} sets it.
## @item @qcode{'ValueType'} @tab @qcode{'auto'} (the default) as above, or
## one of @qcode{'cell'}, @qcode{'string'}, @qcode{'double'},
## @qcode{'single'}, the integer types, @qcode{'logical'},
## @qcode{'datetime'} and @qcode{'duration'}.  Text is never read as a
## number: it gives @code{NaN}, 0 or false; numbers give text under
## @qcode{'string'}, and @qcode{true} gives @qcode{"true"} or 1; arrays,
## objects and @qcode{null} give the missing value of the type.  Under
## @qcode{'datetime'} text is read as dates, each keeping the zone it states:
## @qcode{Z} gives UTC, an offset a fixed-offset zone, no mark no zone.
## Under @qcode{'cell'} every value is in a cell, objects read the same way.
## @item @qcode{'DuplicateKeyRule'} @tab @qcode{'auto'} and
## @qcode{'makeUnique'} rename a repeated key @qcode{a} as @qcode{a_1},
## @qcode{a_2} and so on, skipping names in use; @qcode{'preserveLast'}
## keeps the first position with the last value; @qcode{'error'} refuses.
## @item @qcode{'DictionarySelector'} @tab A JSON Pointer to the object to
## read, as @qcode{'/data/2'}, counting array elements from 0; @qcode{''} is
## the whole file.
## @item @qcode{'DictionaryNodeName'} @tab The name of the object to read:
## the first key of that name, depth first.  It cannot be given with
## @qcode{'DictionarySelector'}.
## @item @qcode{'DateLocale'} @tab The locale of month names in dates, as
## @qcode{'de_DE'}.
## @item @qcode{'ParsingMode'} @tab @qcode{'lenient'} (the default) allows
## comments, trailing commas and @code{NaN}, @code{Inf} and
## @code{Infinity}; @qcode{'strict'} refuses them.
## @item @qcode{'AllowComments'}, @qcode{'AllowTrailingCommas'},
## @qcode{'AllowInfAndNaN'} @tab Logical scalars allowing each form; each
## overrides @qcode{'ParsingMode'}.
## @end multitable
##
## The top level, or the object selected, must be a JSON object.  MATLAB's
## @qcode{'ValueType', 'datetime'} reads an ISO 8601 date as @code{NaT};
## here it reads it as the default does.
##
## An OpenDocument spreadsheet, @qcode{FileType} @qcode{'ods'}, gives one
## entry per sheet, the sheet name the key and the @code{table} or
## @code{timetable} read by @code{ods2struct} the value, so that a dictionary
## written by @code{writedictionary} comes back as written.  A workbook
## holding both tables and timetables gives @code{cell} values.  Reading
## ODS is an extension of this package; it takes no option but
## @qcode{'FileType'}.
##
## Remote locations other than @qcode{http} and @qcode{https} (Amazon S3,
## Azure Blob Storage, HDFS), compressed files and @qcode{'WebOptions'},
## which MATLAB accepts, are not supported.
##
## @seealso{dictionary, dictionary.writedictionary, ods2struct}
## @end deftypefn

function d = readdictionary (filename, varargin)

  if (nargin < 1)
    print_usage ();
  endif
  if (! ((ischar (filename) && rows (filename) <= 1)
         || (isa (filename, 'string') && isscalar (filename))))
    error (strcat ("readdictionary: FILENAME must be a character vector", ...
                   " or a string scalar."));
  endif
  filename = char (filename);
  if (mod (numel (varargin), 2) != 0)
    error ("readdictionary: name-value arguments must be in pairs.");
  endif

  ## An empty default marks an option not given
  optNames = {'FileType', 'ValueType', 'DuplicateKeyRule', ...
              'DictionarySelector', 'DictionaryNodeName', 'DateLocale', ...
              'ParsingMode', 'AllowComments', 'AllowTrailingCommas', ...
              'AllowInfAndNaN', 'WebOptions'};
  dfValues = repmat ({[]}, size (optNames));
  [fileType, valueType, dupRule, selector, nodeName, locale, mode, ...
   comments, commas, infnan, webOptions, args] = ...
                      parsePairedArguments (optNames, dfValues, varargin(:));
  if (! isempty (args))
    error ("readdictionary: invalid optional paired argument.");
  endif
  if (! isempty (webOptions))
    error ("readdictionary: 'WebOptions' is not supported.");
  endif

  ## Remote stores and archives are out of reach
  scheme = regexp (filename, '^(s3|wasbs?|hdfs)://', 'tokens', 'once');
  if (! isempty (scheme))
    error ("readdictionary: reading from '%s' locations is not supported.", ...
           scheme{1});
  endif
  [~, ~, ext] = fileparts (regexprep (filename, '[?#].*$', ''));
  if (any (strcmpi (ext, {'.gz', '.zip', '.tar', '.tgz', '.bz2', '.7z'})))
    error ("readdictionary: compressed files are not supported.");
  endif

  [fileType, errmsg] = choice (fileType, 'auto', {'auto', 'json', 'ods'}, ...
                               'FileType');
  if (! isempty (errmsg))
    error ("readdictionary: %s", errmsg);
  endif
  if (strcmp (fileType, 'auto'))
    switch (lower (ext))
      case '.json'
        fileType = 'json';
      case {'.ods', '.fods'}
        fileType = 'ods';
      otherwise
        error (strcat ("readdictionary: unable to tell the file type", ...
                       " from the extension; set 'FileType'."));
    endswitch
  endif

  ## A web address is read through a local copy
  isURL = ! isempty (regexp (filename, '^https?://', 'once'));
  if (isURL)
    local = [tempname() ext];
    [~, ok, msg] = urlwrite (filename, local);
    if (! ok)
      error ("readdictionary: unable to read '%s': %s", filename, msg);
    endif
    unwind_protect
      d = readlocal (local, fileType, valueType, dupRule, selector, ...
                     nodeName, locale, mode, comments, commas, infnan);
    unwind_protect_cleanup
      if (exist (local, 'file'))
        delete (local);
      endif
    end_unwind_protect
  else
    d = readlocal (filename, fileType, valueType, dupRule, selector, ...
                   nodeName, locale, mode, comments, commas, infnan);
  endif

endfunction

## The dictionary in the local file FILENAME
function d = readlocal (filename, fileType, valueType, dupRule, selector, ...
                        nodeName, locale, mode, comments, commas, infnan)
  ## The options are checked before the file is touched
  if (strcmp (fileType, 'ods'))
    given = {'ValueType', 'DuplicateKeyRule', 'DictionarySelector', ...
             'DictionaryNodeName', 'DateLocale', 'ParsingMode', ...
             'AllowComments', 'AllowTrailingCommas', 'AllowInfAndNaN'};
    used = ! cellfun ('isempty', {valueType, dupRule, selector, nodeName, ...
                                 locale, mode, comments, commas, infnan});
    if (any (used))
      error ("readdictionary: '%s' does not apply to ODS files.", ...
             given{find (used, 1)});
    endif
    if (! exist (filename, 'file'))
      error ("readdictionary: unable to find the file '%s'.", filename);
    endif
    d = readods (filename);
    return;
  endif

  ## Options of JSON
  [valueType, errmsg] = choice (valueType, 'auto', {'auto', 'cell', ...
                                'string', 'double', 'single', 'int8', ...
                                'uint8', 'int16', 'uint16', 'int32', ...
                                'uint32', 'int64', 'uint64', 'logical', ...
                                'datetime', 'duration'}, 'ValueType');
  if (! isempty (errmsg))
    error ("readdictionary: %s", errmsg);
  endif
  [dupRule, errmsg] = choice (dupRule, 'auto', {'auto', 'preserveLast', ...
                              'makeUnique', 'error'}, 'DuplicateKeyRule');
  if (! isempty (errmsg))
    error ("readdictionary: %s", errmsg);
  endif
  [mode, errmsg] = choice (mode, 'lenient', {'lenient', 'strict'}, ...
                           'ParsingMode');
  if (! isempty (errmsg))
    error ("readdictionary: %s", errmsg);
  endif
  lenient = strcmp (mode, 'lenient');
  flags = {comments, commas, infnan};
  flagNames = {'AllowComments', 'AllowTrailingCommas', 'AllowInfAndNaN'};
  for i = 1:3
    if (isempty (flags{i}))
      flags{i} = lenient;
    elseif (! ((islogical (flags{i}) || isnumeric (flags{i}))
               && isscalar (flags{i}) && ! isnan (flags{i})))
      error ("readdictionary: '%s' must be a logical scalar.", flagNames{i});
    else
      flags{i} = logical (flags{i});
    endif
  endfor
  if (! isempty (selector) && ! isempty (nodeName))
    error (strcat ("readdictionary: 'DictionarySelector' and", ...
                   " 'DictionaryNodeName' cannot be given together."));
  endif
  if (isempty (locale))
    locale = '';
  elseif (! ((ischar (locale) && rows (locale) == 1)
             || (isa (locale, 'string') && isscalar (locale))))
    error (strcat ("readdictionary: 'DateLocale' must be a character", ...
                   " vector or a string scalar."));
  endif
  locale = char (locale);

  if (! exist (filename, 'file'))
    error ("readdictionary: unable to find the file '%s'.", filename);
  endif
  text = fileread (filename);
  [tree, errmsg] = __json2dictionary__ (text, flags{:});
  if (! isempty (errmsg))
    error ("readdictionary: %s", errmsg);
  endif

  ## The object to read
  if (! isempty (selector))
    [tree, errmsg] = select (tree, selector);
  elseif (! isempty (nodeName))
    [tree, errmsg] = findNode (tree, nodeName);
  endif
  if (! isempty (errmsg))
    error ("readdictionary: %s", errmsg);
  endif
  if (! strcmp (kind (tree), 'object'))
    error ("readdictionary: the JSON value read must be an object.");
  endif
  [d, errmsg] = toDictionary (tree, valueType, dupRule, locale);
  if (! isempty (errmsg))
    error ("readdictionary: %s", errmsg);
  endif
endfunction

## The text option VALUE, one of OPTIONS in any case, or DEFAULT when empty
function [value, errmsg] = choice (value, default, options, name)
  errmsg = '';
  if (isempty (value))
    value = default;
    return;
  endif
  if (! ((ischar (value) && rows (value) == 1)
         || (isa (value, 'string') && isscalar (value))))
    errmsg = sprintf ("'%s' must be a character vector or a string scalar.",
                      name);
    return;
  endif
  match = strcmpi (char (value), options);
  if (! any (match))
    errmsg = sprintf ("invalid '%s' value '%s'.", name, char (value));
    return;
  endif
  value = options{match};
endfunction

## The kind of the parsed JSON value X
function k = kind (x)
  if (isstruct (x))
    if (isempty (fieldnames (x)))
      k = 'null';
    else
      k = 'object';
    endif
  elseif (iscell (x))
    k = 'array';
  elseif (ischar (x))
    k = 'text';
  elseif (islogical (x))
    k = 'bool';
  elseif (isinteger (x))
    k = 'int';
  else
    k = 'num';
  endif
endfunction

## The value at the JSON Pointer P within TREE
function [tree, errmsg] = select (tree, p)
  errmsg = '';
  if (! ((ischar (p) && rows (p) <= 1)
         || (isa (p, 'string') && isscalar (p))))
    errmsg = strcat ("'DictionarySelector' must be a character vector", ...
                     " or a string scalar.");
    return;
  endif
  p = char (p);
  if (isempty (p))
    return;
  endif
  if (p(1) != '/')
    errmsg = sprintf ("invalid JSON Pointer '%s'.", p);
    return;
  endif
  tokens = strsplit (p(2:end), '/');
  for i = 1:numel (tokens)
    t = strrep (strrep (tokens{i}, '~1', '/'), '~0', '~');
    switch (kind (tree))
      case 'object'
        idx = find (strcmp (tree.keys, t), 1);
        if (isempty (idx))
          errmsg = sprintf ("selector '%s' not found.", p);
          return;
        endif
        tree = tree.values{idx};
      case 'array'
        if (isempty (regexp (t, '^(0|[1-9][0-9]*)$', 'once'))
            || str2double (t) >= numel (tree))
          errmsg = sprintf ("selector '%s' not found.", p);
          return;
        endif
        tree = tree{str2double (t) + 1};
      otherwise
        errmsg = sprintf ("selector '%s' not found.", p);
        return;
    endswitch
  endfor
endfunction

## The value of the first key named NAME within TREE, depth first
function [node, errmsg] = findNode (tree, name)
  errmsg = '';
  if (! ((ischar (name) && rows (name) == 1)
         || (isa (name, 'string') && isscalar (name))))
    errmsg = strcat ("'DictionaryNodeName' must be a character vector", ...
                     " or a string scalar.");
    node = [];
    return;
  endif
  [node, found] = searchNode (tree, char (name));
  if (! found)
    errmsg = sprintf ("node '%s' not found.", char (name));
  endif
endfunction

function [node, found] = searchNode (tree, name)
  node = [];
  found = false;
  switch (kind (tree))
    case 'object'
      for i = 1:numel (tree.keys)
        if (strcmp (tree.keys{i}, name))
          node = tree.values{i};
          found = true;
          return;
        endif
        [node, found] = searchNode (tree.values{i}, name);
        if (found)
          return;
        endif
      endfor
    case 'array'
      for i = 1:numel (tree)
        [node, found] = searchNode (tree{i}, name);
        if (found)
          return;
        endif
      endfor
  endswitch
endfunction

## The dictionary of the JSON object TREE
function [d, errmsg] = toDictionary (tree, valueType, dupRule, locale)
  errmsg = '';
  keys = tree.keys;
  vals = tree.values;
  if (isempty (keys))
    d = dictionary ();
    return;
  endif
  [keys, errmsg] = uniqueKeys (keys, dupRule);
  if (! isempty (errmsg))
    d = [];
    return;
  endif
  keys = string (keys);
  if (strcmp (valueType, 'auto'))
    [V, errmsg] = autoValues (vals, locale);
  elseif (strcmp (valueType, 'cell'))
    V = cell (numel (vals), 1);
    for i = 1:numel (vals)
      [V{i}, errmsg] = element (vals{i}, 'cell', locale);
      if (! isempty (errmsg))
        d = [];
        return;
      endif
    endfor
  else
    V = typedValues (vals, valueType, locale);
  endif
  if (! isempty (errmsg))
    d = [];
    return;
  endif
  ## Dates in several zones are held one per entry, each as written
  if (strcmp (valueType, 'datetime') && iscell (V))
    d = configureDictionary ('string', 'datetime');
    for i = 1:numel (keys)
      d(keys(i)) = V{i};
    endfor
    return;
  endif
  d = dictionary (keys, V);
endfunction

## The keys KEYS by the rule DUPRULE: repeated ones renamed, kept for the
## dictionary to merge, or refused
function [keys, errmsg] = uniqueKeys (keys, dupRule)
  errmsg = '';
  u = unique (keys);
  if (numel (u) == numel (keys))
    return;
  endif
  switch (dupRule)
    case 'error'
      for i = 2:numel (keys)
        if (any (strcmp (keys(1:i-1), keys{i})))
          errmsg = sprintf ("duplicate key '%s'.", keys{i});
          return;
        endif
      endfor
    case 'preserveLast'
      ## The dictionary keeps the first position and the last value
    otherwise
      used = keys;
      seen = {};
      for i = 1:numel (keys)
        k = keys{i};
        if (any (strcmp (seen, k)))
          n = 1;
          while (any (strcmp (used, sprintf ('%s_%d', k, n))))
            n += 1;
          endwhile
          keys{i} = sprintf ('%s_%d', k, n);
          used{end+1} = keys{i};
        else
          seen{end+1} = k;
        endif
      endfor
  endswitch
endfunction

## The values VALS of one object under 'auto': one typed column, or a column
## cell array when they share no type
function [V, errmsg] = autoValues (vals, locale)
  errmsg = '';
  n = numel (vals);
  kinds = cellfun (@kind, vals, 'UniformOutput', false);
  isnull = strcmp (kinds, 'null');
  present = unique (kinds(! isnull));
  if (isempty (present))
    V = repmat (missing, n, 1);
    return;
  endif
  if (isequal (present, {'num'}))
    V = nan (n, 1);
    V(! isnull) = [vals{! isnull}];
    return;
  endif
  if (all (ismember (present, {'num', 'int'})) && ! any (isnull))
    [V, ok] = bigIntegers (vals);
    if (ok)
      return;
    endif
  elseif (isequal (present, {'bool'}) && ! any (isnull))
    V = [vals{:}]';
    return;
  elseif (isequal (present, {'text'}))
    [V, ok] = textColumn (vals, isnull, locale);
    if (ok)
      return;
    endif
  endif
  ## Anything else is a cell, each value typed alone
  V = cell (n, 1);
  for i = 1:n
    [V{i}, errmsg] = element (vals{i}, 'auto', locale);
    if (! isempty (errmsg))
      return;
    endif
  endfor
endfunction

## Integers beyond 2^53 among numbers: uint64, or int64 when one is negative
## and all fit; a fraction among them, or a negative number beside a uint64
## too large for int64 (ours: unmeasured), fails
function [V, ok] = bigIntegers (vals)
  V = [];
  ok = false;
  isint = cellfun ('isinteger', vals);
  num = vals(! isint);
  if (any (cellfun (@(x) x != fix (x) || ! isfinite (x), num)))
    return;
  endif
  neg = any (cellfun (@(x) x < 0, vals));
  if (neg)
    big = cellfun (@(x) isa (x, 'uint64') && x > intmax ('int64'), vals);
    if (any (big))
      return;
    endif
    V = int64 (zeros (numel (vals), 1));
  else
    V = uint64 (zeros (numel (vals), 1));
  endif
  for i = 1:numel (vals)
    V(i) = vals{i};
  endfor
  ok = true;
endfunction

## Text, with NULL marking the nulls: dates, durations or strings
function [V, ok] = textColumn (vals, isnull, locale)
  ok = true;
  txt = vals(! isnull);
  [dt, isdate, fmt] = cellfun (@(t) autoDate (t, locale), txt, ...
                               'UniformOutput', false);
  if (all ([isdate{:}]))
    V = NaT (numel (vals), 1);
    V.Format = fmt{1};
    V(! isnull) = vertcat (dt{:});
    return;
  endif
  isdur = cellfun (@isDuration, txt);
  if (all (isdur))
    V = duration (nan (numel (vals), 1), 0, 0);
    V(! isnull) = duration (txt);
    return;
  endif
  if (any ([isdate{:}]) || any (isdur))
    ## A date among other text is a cell of dates and text
    V = [];
    ok = false;
    return;
  endif
  V = repmat (string (missing), numel (vals), 1);
  V(! isnull) = string (txt);
endfunction

## The JSON value X as one element of a cell under the value type VT, 'auto'
## or 'cell'
function [v, errmsg] = element (x, vt, locale)
  errmsg = '';
  switch (kind (x))
    case 'null'
      v = missing;
    case 'object'
      [v, errmsg] = toDictionary (x, vt, 'auto', locale);
    case 'array'
      v = cell (numel (x), 1);
      for i = 1:numel (x)
        [v{i}, errmsg] = element (x{i}, 'auto', locale);
        if (! isempty (errmsg))
          return;
        endif
      endfor
    case 'text'
      [dt, isdate] = autoDate (x, locale);
      if (isdate)
        v = dt;
      elseif (isDuration (x))
        v = duration (x);
      else
        v = string (x);
      endif
    otherwise
      v = x;
  endswitch
endfunction

## The values VALS converted to the value type VT, never parsing text into a
## number; what does not fit is the type's missing value
function V = typedValues (vals, vt, locale)
  n = numel (vals);
  kinds = cellfun (@kind, vals, 'UniformOutput', false);
  switch (vt)
    case 'string'
      V = repmat (string (missing), n, 1);
      for i = 1:n
        switch (kinds{i})
          case 'text'
            V(i) = string (vals{i});
          case 'bool'
            V(i) = string (ifelse (vals{i}, 'true', 'false'));
          case {'num', 'int'}
            V(i) = string (vals{i});
        endswitch
      endfor
    case 'logical'
      V = false (n, 1);
      for i = 1:n
        if (any (strcmp (kinds{i}, {'num', 'int', 'bool'})))
          V(i) = vals{i} != 0 && ! isnan (double (vals{i}));
        endif
      endfor
    case 'datetime'
      dts = cell (n, 1);
      for i = 1:n
        if (strcmp (kinds{i}, 'text'))
          dts{i} = anyDate (vals{i}, locale);
        else
          dts{i} = NaT;
        endif
      endfor
      V = joinDates (dts);
    case 'duration'
      V = duration (nan (n, 1), 0, 0);
      for i = 1:n
        if (strcmp (kinds{i}, 'text') && isDuration (vals{i}))
          V(i) = duration (vals{i});
        endif
      endfor
    otherwise
      ## The numeric types: NaN, which the integer types make 0, for text,
      ## arrays, objects and null
      V = nan (n, 1);
      if (any (strcmp (vt, {'int64', 'uint64'})))
        V = zeros (n, 1, vt);
      endif
      ## A 64-bit integer assigned into a double array would make it one
      isfloat = any (strcmp (vt, {'double', 'single'}));
      for i = 1:n
        if (any (strcmp (kinds{i}, {'num', 'int', 'bool'})))
          if (isfloat)
            V(i) = double (vals{i});
          else
            V(i) = vals{i};
          endif
        endif
      endfor
      V = cast (V, vt);
  endswitch
endfunction

function s = ifelse (tf, a, b)
  if (tf)
    s = a;
  else
    s = b;
  endif
endfunction

## The text T as a date under 'auto': ISO 8601 without a zone, or
## dd-MMM-yyyy, with its display format
function [dt, isdate, fmt] = autoDate (t, locale)
  dt = NaT;
  isdate = false;
  fmt = '';
  ## Named tokens: Octave leaves out an optional group that took no part
  m = regexp (t, ['^(?<Y>\d{4})-(?<M>\d{2})-(?<D>\d{2})', ...
                  '(T(?<h>\d{2}):(?<mi>\d{2})(:(?<s>\d{2}(\.\d+)?))?)?$'], ...
              'names', 'once');
  if (! isempty (m))
    c = str2double ({m.Y, m.M, m.D, m.h, m.mi, m.s});
    if (! validDate (c))
      return;
    endif
    if (isempty (m.h))
      dt = datetime (c(1), c(2), c(3));
      fmt = 'uuuu-MM-dd';
    else
      c(isnan (c)) = 0;
      dt = datetime (c(1), c(2), c(3), c(4), c(5), c(6));
      fmt = "uuuu-MM-dd'T'HH:mm:ss";
      if (any (m.s == '.'))
        fmt = "uuuu-MM-dd'T'HH:mm:ss.SSS";
      endif
    endif
    dt.Format = fmt;
    isdate = ! isnat (dt);
    return;
  endif
  dmy = '^\d{1,2}-[^\W\d_]+-\d{4}( \d{2}:\d{2}:\d{2})?$';
  if (! isempty (regexp (t, dmy, 'once')))
    fmt = 'dd-MMM-uuuu';
    if (any (t == ' '))
      fmt = 'dd-MMM-uuuu HH:mm:ss';
    endif
    dt = readDate (t, fmt, locale);
    ## A day beyond the month would roll into the next one
    isdate = ! isnat (dt) && day (dt) == str2double (strtok (t, '-'));
    if (isdate)
      dt.Format = fmt;
    endif
  endif
endfunction

## The text T as a date under 'ValueType' 'datetime': the forms of 'auto',
## ISO 8601 with a zone mark keeping that zone, and the forms datetime reads
## for the locale; NaT when none fits
function dt = anyDate (t, locale)
  [dt, isdate] = autoDate (t, locale);
  if (isdate)
    return;
  endif
  m = regexp (t, ['^(?<Y>\d{4})-(?<M>\d{2})-(?<D>\d{2})T(?<h>\d{2}):', ...
                  '(?<mi>\d{2})(:(?<s>\d{2}(\.\d+)?))?', ...
                  '(?<z>Z|[+-]\d{2}:?\d{2})$'], 'names', 'once');
  if (! isempty (m))
    c = str2double ({m.Y, m.M, m.D, m.h, m.mi, m.s});
    c(isnan (c)) = 0;
    if (! validDate (c))
      return;
    endif
    if (strcmp (m.z, 'Z'))
      tz = 'UTC';
    else
      tz = regexprep (m.z, '^([+-]\d{2}):?(\d{2})$', '$1:$2');
    endif
    dt = datetime (c(1), c(2), c(3), c(4), c(5), c(6), 'TimeZone', tz);
    dt.Format = "uuuu-MM-dd'T'HH:mm:ssXXX";
    return;
  endif
  dt = readDate (t, '', locale);
endfunction

## The text T read by datetime in the format FMT, or as datetime guesses,
## then in the long forms of LOCALE; NaT when it cannot
function dt = readDate (t, fmt, locale)
  args = {};
  if (! isempty (locale))
    args = {'Locale', locale};
  endif
  forms = {fmt};
  if (isempty (fmt))
    forms = {'', 'd. MMMM uuuu', 'd MMMM uuuu', 'MMMM d, uuuu', 'dd.MM.uuuu'};
  endif
  for i = 1:numel (forms)
    try
      if (isempty (forms{i}))
        dt = datetime (t, args{:});
      else
        dt = datetime (t, 'InputFormat', forms{i}, args{:});
      endif
      return;
    catch
    end_try_catch
  endfor
  dt = NaT;
endfunction

## The dates DTS, one per value, joined when they share a zone, else kept
## as a cell for the dictionary to hold one per entry
function V = joinDates (dts)
  zones = cellfun (@(x) x.TimeZone, dts, 'UniformOutput', false);
  nat = cellfun (@isnat, dts);
  z = unique (zones(! nat));
  if (numel (z) <= 1)
    if (! isempty (z))
      for i = find (nat)'
        dts{i}.TimeZone = z{1};
      endfor
    endif
    V = vertcat (dts{:});
  else
    V = dts;
  endif
endfunction

## True for the date components C, year, month, day, hour, minute, second,
## within their ranges (absent ones NaN); datetime would roll them over
function tf = validDate (c)
  c(isnan (c)) = 0;
  tf = (c(2) >= 1 && c(2) <= 12 && c(3) >= 1 && c(3) <= eomday (c(1), c(2))
        && c(4) < 24 && c(5) < 60 && c(6) < 60);
endfunction

## True for text written as a duration, hh:mm:ss
function tf = isDuration (t)
  tf = ischar (t) && ! isempty (regexp (t, '^\d+:\d{2}:\d{2}(\.\d+)?$', ...
                                        'once'));
endfunction

## The dictionary in the OpenDocument spreadsheet FILENAME, one entry per
## sheet
function d = readods (filename)
  s = ods2struct (filename);
  f = fieldnames (s);
  if (isempty (f))
    d = dictionary ();
    return;
  endif
  keys = f;
  vals = cell (numel (f), 1);
  for i = 1:numel (f)
    t = s.(f{i});
    p = t.Properties.CustomProperties;
    if (isprop (p, 'ActualSheetName') || isfield (p, 'ActualSheetName'))
      keys{i} = char (t.Properties.CustomProperties.ActualSheetName);
      t = rmprop (t, 'ActualSheetName');
    endif
    vals{i} = t;
  endfor
  keys = string (keys);
  istt = cellfun (@(t) isa (t, 'timetable'), vals);
  if (all (istt) || ! any (istt))
    d = dictionary (keys(1), vals{1});
    for i = 2:numel (vals)
      d(keys(i)) = vals{i};
    endfor
  else
    d = dictionary (keys, vals);
  endif
endfunction

%!demo
%! ## `readdictionary` reads the JSON object in a file as a dictionary with
%! ## `string` keys, in the order of the file.  The value type follows the
%! ## values: here they are all numbers, so they are `double`.
%!
%! f = [tempname() '.json'];
%! text = '{"Boston": 55, "Denver": 48, "Dallas": 75}'
%! fid = fopen (f, 'w');
%! fputs (fid, text);
%! fclose (fid);
%! temps = readdictionary (f)
%! delete (f);

%!demo
%! ## Values of different kinds give `cell` values.  An array becomes a column
%! ## cell array and a nested object a dictionary of its own, so `{}` reaches
%! ## into both; `null` is `missing`.
%!
%! f = [tempname() '.json'];
%! text = ['{"id": 12345, "tags": ["new", "urgent"], ', ...
%!         '"sender": {"name": "Ann", "id": 7}, "reply": null}']
%! fid = fopen (f, 'w');
%! fputs (fid, text);
%! fclose (fid);
%! msg = readdictionary (f)
%! msg{'tags'}
%! sender = msg{'sender'}
%! delete (f);

%!demo
%! ## Text in ISO 8601 form becomes dates, and `hh:mm:ss` durations.  A
%! ## timestamp marked with a time zone stays text, unless dates are asked for
%! ## with `'ValueType'`, when each keeps the zone it states.
%!
%! f = [tempname() '.json'];
%! text = '{"start": "2024-03-01", "end": "2024-03-15"}'
%! fid = fopen (f, 'w');
%! fputs (fid, text);
%! fclose (fid);
%! dates = readdictionary (f)
%! text = ['{"utc": "2024-03-01T10:00:00Z", ', ...
%!         '"local": "2024-03-01T12:00:00+02:00"}']
%! fid = fopen (f, 'w');
%! fputs (fid, text);
%! fclose (fid);
%! asText = readdictionary (f)
%! asDates = readdictionary (f, 'ValueType', 'datetime')
%! v = values (asDates, 'cell');
%! v{2}.TimeZone
%! delete (f);

%!demo
%! ## `'DictionarySelector'` reads one object within the file by a JSON
%! ## Pointer, counting array elements from 0, and `'DuplicateKeyRule'` says
%! ## what a repeated key becomes: renamed by default, or the last value kept.
%!
%! f = [tempname() '.json'];
%! text = '{"site": {"a": 1, "a": 2, "b": 3}, "runs": [{"x": 10}, {"x": 20}]}'
%! fid = fopen (f, 'w');
%! fputs (fid, text);
%! fclose (fid);
%! readdictionary (f, 'DictionarySelector', '/site')
%! readdictionary (f, 'DictionarySelector', '/site', ...
%!                 'DuplicateKeyRule', 'preserveLast')
%! readdictionary (f, 'DictionarySelector', '/runs/1')
%! delete (f);

%!demo
%! ## An OpenDocument spreadsheet gives one entry per sheet, the sheet name
%! ## the key and its table or timetable the value, so a dictionary of tables
%! ## written by `writedictionary` comes back as it was.
%!
%! sales = dictionary (string ({'North', 'South 2024'}), ...
%!                     {table([10; 12], {'pens'; 'ink'}), ...
%!                      table(7, {'tape'})})
%! f = [tempname() '.ods'];
%! writedictionary (sales, f);
%! back = readdictionary (f)
%! back('South 2024')
%! delete (f);

## Write TXT to a temporary file, read it, and delete the file
%!function d = rj (txt, varargin)
%!  d = rjx (txt, '.json', varargin{:});
%!endfunction
%!function d = rjx (txt, ext, varargin)
%!  f = [tempname() ext];
%!  fid = fopen (f, 'w');
%!  fputs (fid, txt);
%!  fclose (fid);
%!  unwind_protect
%!    d = readdictionary (f, varargin{:});
%!  unwind_protect_cleanup
%!    delete (f);
%!  end_unwind_protect
%!endfunction

## Measured on MATLAB Online R2026b, unless marked otherwise
%!test
%! r = rj ('{"a": 1, "b": 2}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {1; 2});
%!test
%! r = rj ('{"b": 1, "a": 2, "c": 3}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('b'); string('a'); string('c')});
%! assert_equal (values (r, 'cell'), {1; 2; 3});
%!test
%! r = rj ('{"a": "x", "b": "y"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {string('x'); string('y')});
%!test
%! r = rj ('{"a": true, "b": false}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {true; false});
%!test
%! r = rj ('{"a": 1, "b": true}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {{1}; {true}});
%!test
%! r = rj ('{"a": 1, "b": "x"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {{1}; {string('x')}});
%!test
%! r = rj ('{"a": 1, "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {1; NaN});
%!test
%! r = rj ('{"a": "x", "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (vc4{1}, string ('x'));
%! assert_equal (class (vc4{2}), 'string');
%! assert_equal (ismissing (vc4{2}), true);
%!test
%! r = rj ('{"a": null, "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'missing'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'missing');
%! assert_equal (size (vc4{1}), [1, 1]);
%! assert_equal (class (vc4{2}), 'missing');
%! assert_equal (size (vc4{2}), [1, 1]);
%!test
%! r = rj ('{"a": true, "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (vc4{1}, {true});
%! ce5 = vc4{2};
%! assert_equal (size (ce5), [1, 1]);
%! assert_equal (class (ce5{1}), 'missing');
%! assert_equal (size (ce5{1}), [1, 1]);
%!test
%! r = rj ('{}');
%! assert_equal (isConfigured (r), false);
%!test
%! r = rj ('{"a": [1, 2, 3], "b": [4]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {{{1; 2; 3}}; {{4}}});
%!test
%! r = rj ('{"a": [1, "x"]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{{1; string('x')}}});
%!test
%! r = rj ('{"a": []}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{cell(0, 1)}});
%!test
%! r = rj ('{"a": [[1, 2], [3, 4]]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{{{1; 2}; {3; 4}}}});
%!test
%! r = rj ('{"a": [true, false]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{{true; false}}});
%!test
%! r = rj ('{"a": ["x", "y"]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{{string('x'); string('y')}}});
%!test
%! r = rj ('{"a": [1, null, 3]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! ce6 = ce5{1};
%! assert_equal (size (ce6), [3, 1]);
%! assert_equal (ce6{1}, 1);
%! assert_equal (class (ce6{2}), 'missing');
%! assert_equal (size (ce6{2}), [1, 1]);
%! assert_equal (ce6{3}, 3);
%!test
%! r = rj ('{"a": [{"x": 1}, {"y": 2}]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! ce6 = ce5{1};
%! assert_equal (size (ce6), [2, 1]);
%! dc7 = ce6{1};
%! [kt8, vt9] = types (dc7);
%! assert_equal ([kt8, vt9], string ({'string', 'double'}));
%! assert_equal (numEntries (dc7), 1);
%! assert_equal (keys (dc7, 'cell'), {string('x')});
%! assert_equal (values (dc7, 'cell'), {1});
%! dc12 = ce6{2};
%! [kt13, vt14] = types (dc12);
%! assert_equal ([kt13, vt14], string ({'string', 'double'}));
%! assert_equal (numEntries (dc12), 1);
%! assert_equal (keys (dc12, 'cell'), {string('y')});
%! assert_equal (values (dc12, 'cell'), {2});
%!test
%! r = rj ('{"a": {"x": 1, "y": 2}}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'double'}));
%! assert_equal (numEntries (dc6), 2);
%! assert_equal (keys (dc6, 'cell'), {string('x'); string('y')});
%! assert_equal (values (dc6, 'cell'), {1; 2});
%!test
%! r = rj ('{"a": {"x": 1}, "b": {"y": "s"}}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'double'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('x')});
%! assert_equal (values (dc6, 'cell'), {1});
%! ce11 = vc4{2};
%! assert_equal (size (ce11), [1, 1]);
%! dc12 = ce11{1};
%! [kt13, vt14] = types (dc12);
%! assert_equal ([kt13, vt14], string ({'string', 'string'}));
%! assert_equal (numEntries (dc12), 1);
%! assert_equal (keys (dc12, 'cell'), {string('y')});
%! assert_equal (values (dc12, 'cell'), {string('s')});
%!test
%! r = rj ('{"a": {"b": {"c": 1}}}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'cell'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('b')});
%! vc10 = values (dc6, 'cell');
%! ce11 = vc10{1};
%! assert_equal (size (ce11), [1, 1]);
%! dc12 = ce11{1};
%! [kt13, vt14] = types (dc12);
%! assert_equal ([kt13, vt14], string ({'string', 'double'}));
%! assert_equal (numEntries (dc12), 1);
%! assert_equal (keys (dc12, 'cell'), {string('c')});
%! assert_equal (values (dc12, 'cell'), {1});
%!test
%! r = rj ('{"a": {}, "b": 1}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! assert_equal (isConfigured (ce5{1}), false);
%! assert_equal (vc4{2}, {1});
%!error<readdictionary: the JSON value read must be an object.> ...
%! rj ('[1, 2]');
%!error<readdictionary: the JSON value read must be an object.> ...
%! rj ('[{"a": 1}, {"b": 2}]');
%!error<readdictionary: the JSON value read must be an object.> ...
%! rj ('5');
%!error<readdictionary: the JSON value read must be an object.> ...
%! rj ('"x"');
%!test
%! r = rj ('{"a": 1e20, "b": -0.5, "c": 3}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {1e+20; -0.5; 3});
%!test
%! r = rj ('{"a": 1e400}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {Inf});
%!test
%! r = rj ('{"a": 12345678901234567890}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint64'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), ...
%!               {typecast(uint32([3944680146, 2874452364]), 'uint64')});
%!test
%! r = rj (['{"a": NaN, "b": Infinity, "c": -Infinity, ', ...
%!          '"d": Inf, "e": -Inf}']);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 5);
%! assert_equal (keys (r, 'cell'), ...
%!               {string('a'); string('b'); string('c'); string('d'); ...
%!                string('e')});
%! assert_equal (values (r, 'cell'), {NaN; Inf; -Inf; Inf; -Inf});
%!error<readdictionary: invalid value at line 1, column 7.> ...
%! rj (['{"a": NaN, "b": Infinity, "c": -Infinity, ', ...
%!      '"d": Inf, "e": -Inf}'], 'AllowInfAndNaN', false);
%!error<readdictionary: invalid value at line 1, column 7.> ...
%! rj (['{"a": NaN, "b": Infinity, "c": -Infinity, ', ...
%!      '"d": Inf, "e": -Inf}'], 'ParsingMode', 'strict');
%!test
%! r = rj ('{"a": "2024-01-01", "b": "2024-02-03"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 00:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-02-03 00:00:00.000'});
%! assert_equal (x6.TimeZone, '');
## J2: ISO dates are read alike under ValueType datetime; R2026b gives NaT.
%!test
%! r = rj ('{"a": "2024-01-01", "b": "2024-02-03"}', 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 00:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-02-03 00:00:00.000'});
%! assert_equal (x6.TimeZone, '');
%!test
%! r = rj ('{"a": "01-Jan-2024 10:00:00"}', 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 10:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%!test
%! r = rj ('{"a": "1. Januar 2024"}', 'ValueType', 'datetime', 'DateLocale', ...
%!         'de_DE');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 00:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%!test
%! r = rj ('{"a": "01:30:00", "b": "2:00:00"}', 'ValueType', 'duration');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'duration'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'duration');
%! assert_equal (seconds (vc4{1}), 5400);
%! assert_equal (class (vc4{2}), 'duration');
%! assert_equal (seconds (vc4{2}), 7200);
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'cell');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {{1}; {2}});
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'string');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {string('1'); string('2')});
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'single');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'single'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {single(1); single(2)});
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'int8');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'int8'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {int8(1); int8(2)});
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'logical');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {true; true});
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'NaT'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'NaT'});
%! assert_equal (x6.TimeZone, '');
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'duration');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'duration'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'duration');
%! assert_equal (seconds (vc4{1}), NaN);
%! assert_equal (class (vc4{2}), 'duration');
%! assert_equal (seconds (vc4{2}), NaN);
%!test
%! r = rj ('{"a": 1, "b": 2}', 'ValueType', 'uint64');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint64'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {uint64(1); uint64(2)});
%!test
%! r = rj ('{"a": 1, "b": 2}', 'valuetype', 'cell');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {{1}; {2}});
%!error<readdictionary: invalid 'ValueType' value 'struct'.> ...
%! rj ('{"a": 1, "b": 2}', 'ValueType', 'struct');
%!test
%! r = rj ('{"a": 300, "b": -5, "c": 1.5}', 'ValueType', 'int8');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'int8'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {int8(127); int8(-5); int8(2)});
%!test
%! r = rj ('{"a": 300, "b": -5, "c": 1.5}', 'ValueType', 'uint8');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint8'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {uint8(255); uint8(0); uint8(2)});
%!test
%! r = rj ('{"a": 0, "b": 1, "c": 2}', 'ValueType', 'logical');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {false; true; true});
%!test
%! r = rj ('{"a": "5", "b": "x"}', 'ValueType', 'double');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {NaN; NaN});
%!test
%! r = rj ('{"a": 1, "b": "x"}', 'ValueType', 'double');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {1; NaN});
%!test
%! r = rj ('{"a": 1, "b": "x"}', 'ValueType', 'string');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {string('1'); string('x')});
%!test
%! r = rj ('{"a": {"x": 1}}', 'ValueType', 'cell');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'cell'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('x')});
%! assert_equal (values (dc6, 'cell'), {{1}});
%!test
%! r = rj ('{"a": 1, "a": 2, "b": 3}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('a_1'); string('b')});
%! assert_equal (values (r, 'cell'), {1; 2; 3});
%!test
%! r = rj ('{"a": 1, "a": 2, "b": 3}', 'DuplicateKeyRule', 'preserveLast');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {2; 3});
%!test
%! r = rj ('{"a": 1, "a": 2, "b": 3}', 'DuplicateKeyRule', 'makeUnique');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('a_1'); string('b')});
%! assert_equal (values (r, 'cell'), {1; 2; 3});
%!error<readdictionary: duplicate key 'a'.> ...
%! rj ('{"a": 1, "a": 2, "b": 3}', 'DuplicateKeyRule', 'error');
%!test
%! r = rj ('{"a": 1, "a": 2, "b": 3}', 'DuplicateKeyRule', 'auto');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('a_1'); string('b')});
%! assert_equal (values (r, 'cell'), {1; 2; 3});
%!test
%! r = rj ('{"a": 1, "a": 2, "a": 3, "a_1": 4}', 'DuplicateKeyRule', ...
%!         'makeUnique');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 4);
%! assert_equal (keys (r, 'cell'), ...
%!               {string('a'); string('a_2'); string('a_3'); string('a_1')});
%! assert_equal (values (r, 'cell'), {1; 2; 3; 4});
%!test
%! r = rj ('{"a": 1, "a": 2, "a": 3, "a_1": 4}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 4);
%! assert_equal (keys (r, 'cell'), ...
%!               {string('a'); string('a_2'); string('a_3'); string('a_1')});
%! assert_equal (values (r, 'cell'), {1; 2; 3; 4});
%!test
%! r = rj ('{"a": 1, "b": 2, "a": 3}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('a_1')});
%! assert_equal (values (r, 'cell'), {1; 2; 3});
%!test
%! r = rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!         'DictionarySelector', '/x');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('y'); string('z')});
%! assert_equal (values (r, 'cell'), {1; 2});
%!test
%! r = rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!         'DictionarySelector', '/w/1');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('r')});
%! assert_equal (values (r, 'cell'), {3});
%!error<readdictionary: the JSON value read must be an object.> ...
%! rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!     'DictionarySelector', '/w');
%!error<readdictionary: selector '/nope' not found.> ...
%! rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!     'DictionarySelector', '/nope');
%!test
%! r = rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!         'DictionarySelector', '');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('x'); string('w')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'double'}));
%! assert_equal (numEntries (dc6), 2);
%! assert_equal (keys (dc6, 'cell'), {string('y'); string('z')});
%! assert_equal (values (dc6, 'cell'), {1; 2});
%! ce11 = vc4{2};
%! assert_equal (size (ce11), [1, 1]);
%! ce12 = ce11{1};
%! assert_equal (size (ce12), [2, 1]);
%! dc13 = ce12{1};
%! [kt14, vt15] = types (dc13);
%! assert_equal ([kt14, vt15], string ({'string', 'double'}));
%! assert_equal (numEntries (dc13), 1);
%! assert_equal (keys (dc13, 'cell'), {string('q')});
%! assert_equal (values (dc13, 'cell'), {2});
%! dc18 = ce12{2};
%! [kt19, vt20] = types (dc18);
%! assert_equal ([kt19, vt20], string ({'string', 'double'}));
%! assert_equal (numEntries (dc18), 1);
%! assert_equal (keys (dc18, 'cell'), {string('r')});
%! assert_equal (values (dc18, 'cell'), {3});
%!error<readdictionary: invalid JSON Pointer 'x'.> ...
%! rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!     'DictionarySelector', 'x');
%!test
%! r = rj ('{"outer": {"inner": {"a": 1}}, "inner": {"b": 2}}', ...
%!         'DictionaryNodeName', 'inner');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {1});
%!test
%! r = rj ('{"outer": {"inner": {"a": 1}}, "inner": {"b": 2}}', ...
%!         'DictionaryNodeName', 'outer');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('inner')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'double'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('a')});
%! assert_equal (values (dc6, 'cell'), {1});
%!error<readdictionary: node 'nope' not found.> ...
%! rj ('{"outer": {"inner": {"a": 1}}, "inner": {"b": 2}}', ...
%!     'DictionaryNodeName', 'nope');
%!error<readdictionary: 'DictionarySelector' and 'DictionaryNodeName' cannot be given together.> ...
%! rj ('{"outer": {"inner": {"a": 1}}, "inner": {"b": 2}}', ...
%!     'DictionaryNodeName', 'inner', 'DictionarySelector', '/outer');
%!test
%! r = rj (strjoin ({'{"a": 1, // note', '"b": 2, /* block */', '}'}, "\n"));
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {1; 2});
%!error<readdictionary: comments are not allowed at line 1, column 10.> ...
%! rj (strjoin ({'{"a": 1, // note', '"b": 2, /* block */', '}'}, "\n"), ...
%!     'ParsingMode', 'strict');
%!error<readdictionary: comments are not allowed at line 1, column 10.> ...
%! rj (strjoin ({'{"a": 1, // note', '"b": 2, /* block */', '}'}, "\n"), ...
%!     'AllowComments', false);
%!error<readdictionary: trailing commas are not allowed at line 3, column 1.> ...
%! rj (strjoin ({'{"a": 1, // note', '"b": 2, /* block */', '}'}, "\n"), ...
%!     'AllowTrailingCommas', false);
%!test
%! r = rj (strjoin ({'{"a": 1, // note', '"b": 2, /* block */', '}'}, "\n"), ...
%!         'ParsingMode', 'strict', 'AllowComments', true, ...
%!         'AllowTrailingCommas', true);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {1; 2});
%!error<readdictionary: unable to tell the file type from the extension; set 'FileType'.> ...
%! rjx ('{"a": 1}', '.txt');
%!test
%! r = rjx ('{"a": 1}', '.txt', 'FileType', 'json');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {1});
%!error<readdictionary: invalid 'FileType' value 'text'.> ...
%! rj (strjoin ({'{"a": 1, // note', '"b": 2, /* block */', '}'}, "\n"), ...
%!     'FileType', 'text');
%!test
%! r = rj ('{"": 1, "In-Reply-To": 2, "9x": 3, "ä": 4, "a b": 5}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 5);
%! assert_equal (keys (r, 'cell'), ...
%!               {string(''); string('In-Reply-To'); string('9x'); ...
%!                string('ä'); string('a b')});
%! assert_equal (values (r, 'cell'), {1; 2; 3; 4; 5});
%!test
%! r = rj (['{"a\"b": 1, "c": "line\nbreak", "e": "tab\tx", ', '"u": "é"}']);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 4);
%! assert_equal (keys (r, 'cell'), ...
%!               {string('a"b'); string('c'); string('e'); string('u')});
%! vc4 = values (r, 'cell');
%! assert_equal (vc4{1}, {1});
%! ce5 = vc4{2};
%! assert_equal (size (ce5), [1, 1]);
%! assert_equal (class (ce5{1}), 'string');
%! assert_equal (ismissing (ce5{1}), false);
%! assert_equal (double (char (ce5{1}(1))), ...
%!               [108, 105, 110, 101, 10, 98, 114, 101, 97, 107]);
%! ce6 = vc4{3};
%! assert_equal (size (ce6), [1, 1]);
%! assert_equal (class (ce6{1}), 'string');
%! assert_equal (ismissing (ce6{1}), false);
%! assert_equal (double (char (ce6{1}(1))), [116, 97, 98, 9, 120]);
%! assert_equal (vc4{4}, {string('é')});
%!error<readdictionary: missing ',' or '}' in an object at line 1, column 8.> ...
%! rj ('{"a": 1');
%!error<readdictionary: invalid optional paired argument.> ...
%! rj ('{"a": 1', 'Foo', 1);
%!test
%! r = rj ('{"a": "5", "b": "x", "c": 7}', 'ValueType', 'int8');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'int8'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {int8(0); int8(0); int8(7)});
%!test
%! r = rj ('{"a": "5", "b": "x", "c": 7}', 'ValueType', 'uint64');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint64'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {uint64(0); uint64(0); uint64(7)});
%!test
%! r = rj ('{"a": "5", "b": "x", "c": 7}', 'ValueType', 'single');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'single'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {single(NaN); single(NaN); single(7)});
%!test
%! r = rj ('{"a": "5", "b": "x", "c": 7}', 'ValueType', 'logical');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {false; false; true});
%!test
%! r = rj ('{"a": "true", "b": "1", "c": 0}', 'ValueType', 'logical');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {false; false; false});
%!test
%! r = rj ('{"a": "x", "b": "01:30:00"}', 'ValueType', 'duration');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'duration'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'duration');
%! assert_equal (seconds (vc4{1}), NaN);
%! assert_equal (class (vc4{2}), 'duration');
%! assert_equal (seconds (vc4{2}), 5400);
%!test
%! r = rj ('{"a": "x", "b": "01-Jan-2024"}', 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'NaT'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-01-01 00:00:00.000'});
%! assert_equal (x6.TimeZone, '');
%!test
%! r = rj ('{"a": true, "b": false, "c": null}', 'ValueType', 'string');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! vc4 = values (r, 'cell');
%! assert_equal (vc4{1}, string ('true'));
%! assert_equal (vc4{2}, string ('false'));
%! assert_equal (class (vc4{3}), 'string');
%! assert_equal (ismissing (vc4{3}), true);
%!test
%! r = rj ('{"a": true, "b": null}', 'ValueType', 'double');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {1; NaN});
%!test
%! r = rj ('{"a": 1, "b": null}', 'ValueType', 'int8');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'int8'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {int8(1); int8(0)});
%!test
%! r = rj ('{"a": 1, "b": null}', 'ValueType', 'logical');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {true; false});
%!test
%! r = rj ('{"a": 1, "b": null}', 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'NaT'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'NaT'});
%! assert_equal (x6.TimeZone, '');
%!test
%! r = rj ('{"a": [1, 2], "b": 3}', 'ValueType', 'double');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {NaN; 3});
%!test
%! r = rj ('{"a": {"x": 1}, "b": 3}', 'ValueType', 'double');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {NaN; 3});
%!test
%! r = rj ('{"a": [1, 2], "b": "s"}', 'ValueType', 'string');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'string');
%! assert_equal (ismissing (vc4{1}), true);
%! assert_equal (vc4{2}, string ('s'));
%!test
%! r = rj ('{"a": {"x": 1}}', 'ValueType', 'string');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'string');
%! assert_equal (ismissing (vc4{1}), true);
%!test
%! r = rj ('{"a": 1, "b": "x", "c": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! vc4 = values (r, 'cell');
%! assert_equal (vc4{1}, {1});
%! assert_equal (vc4{2}, {string('x')});
%! ce5 = vc4{3};
%! assert_equal (size (ce5), [1, 1]);
%! assert_equal (class (ce5{1}), 'missing');
%! assert_equal (size (ce5{1}), [1, 1]);
%!test
%! r = rj ('{"a": [true, null]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! ce6 = ce5{1};
%! assert_equal (size (ce6), [2, 1]);
%! assert_equal (ce6{1}, true);
%! assert_equal (class (ce6{2}), 'missing');
%! assert_equal (size (ce6{2}), [1, 1]);
%!test
%! r = rj ('{"a": ["x", null]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! ce6 = ce5{1};
%! assert_equal (size (ce6), [2, 1]);
%! assert_equal (ce6{1}, string ('x'));
%! assert_equal (class (ce6{2}), 'missing');
%! assert_equal (size (ce6{2}), [1, 1]);
%!test
%! r = rj ('{"a": [1, 2, 3]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{{1; 2; 3}}});
%!test
%! r = rj ('{"a": [[1, 2], [3]]}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{{{1; 2}; {3}}}});
%!test
%! r = rj ('{"a": {"x": null}}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'missing'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('x')});
%! vc10 = values (dc6, 'cell');
%! assert_equal (class (vc10{1}), 'missing');
%! assert_equal (size (vc10{1}), [1, 1]);
%!test
%! r = rj ('{"a": {"x": 1}, "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'double'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('x')});
%! assert_equal (values (dc6, 'cell'), {1});
%! ce11 = vc4{2};
%! assert_equal (size (ce11), [1, 1]);
%! assert_equal (class (ce11{1}), 'missing');
%! assert_equal (size (ce11{1}), [1, 1]);
%!test
%! r = rj ('{"a": [1], "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (vc4{1}, {{1}});
%! ce5 = vc4{2};
%! assert_equal (size (ce5), [1, 1]);
%! assert_equal (class (ce5{1}), 'missing');
%! assert_equal (size (ce5{1}), [1, 1]);
%!test
%! r = rj ('{"a": "2024-01-01", "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 00:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'NaT'});
%! assert_equal (x6.TimeZone, '');
%!test
%! r = rj ('{"a": 12345678901234567890, "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (vc4{1}, ...
%!               {typecast(uint32([3944680146, 2874452364]), 'uint64')});
%! ce5 = vc4{2};
%! assert_equal (size (ce5), [1, 1]);
%! assert_equal (class (ce5{1}), 'missing');
%! assert_equal (size (ce5{1}), [1, 1]);
%!test
%! r = rj ('{"a": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'missing'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'missing');
%! assert_equal (size (vc4{1}), [1, 1]);
%!test
%! r = rj ('{"a": 1, "b": 12345678901234567890}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint64'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), ...
%!               {uint64(1); ...
%!                typecast(uint32([3944680146, 2874452364]), 'uint64')});
%!test
%! r = rj ('{"a": 1.5, "b": 12345678901234567890}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), ...
%!               {{1.5}; ...
%!                {typecast(uint32([3944680146, 2874452364]), 'uint64')}});
%!test
%! r = rj ('{"a": -12345678901234567890}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {-1.2345678901234567e+19});
%!test
%! r = rj ('{"a": -9223372036854775808, "b": 9007199254740993}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'int64'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), ...
%!               {intmin('int64'); typecast(uint32([1, 2097152]), 'int64')});
%!test
%! r = rj ('{"a": 9007199254740993}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint64'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), ...
%!               {typecast(uint32([1, 2097152]), 'uint64')});
%!test
%! r = rj ('{"a": 1e2, "b": 100}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {100; 100});
%!test
%! r = rj ('{"a": "2024-01-01T10:00:00Z"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {string('2024-01-01T10:00:00Z')});
%!test
%! r = rj ('{"a": "2024-01-01T10:00:00"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 10:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%!test
%! r = rj ('{"a": "2024-01-01T10:00:00+02:00"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {string('2024-01-01T10:00:00+02:00')});
%!test
%! r = rj ('{"a": "2024-01-01", "b": "x"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! x6 = ce5{1};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-01-01 00:00:00.000'});
%! assert_equal (x6.TimeZone, '');
%! assert_equal (vc4{2}, {string('x')});
%!test
%! r = rj ('{"a": "01-Jan-2024", "b": "02-Jan-2024"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 00:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-01-02 00:00:00.000'});
%! assert_equal (x6.TimeZone, '');
%!test
%! r = rj ('{"a": "01:30:00", "b": "02:00:00"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'duration'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'duration');
%! assert_equal (seconds (vc4{1}), 5400);
%! assert_equal (class (vc4{2}), 'duration');
%! assert_equal (seconds (vc4{2}), 7200);
%!test
%! r = rj ('{"a": "1 hr", "b": "2 hr"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {string('1 hr'); string('2 hr')});
%!test
%! r = rj ('{"a": "2024-01-01"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 00:00:00.000'});
%! assert_equal (x5.TimeZone, '');
## Ours: not measured on R2026b.
%!error<readdictionary: name-value arguments must be in pairs.> ...
%! rj ('{"a": 1}', 'FileType');
## Ours: not measured on R2026b.
%!error<readdictionary: 'ValueType' must be a character vector or a string scalar.> ...
%! rj ('{"a": 1}', 'ValueType', 5);
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 1}', 'ValueType', 'CELL');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {{1}});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 1}', 'filetype', 'JSON');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {1});
## Ours: not measured on R2026b.
%!error<readdictionary: invalid 'DuplicateKeyRule' value 'x'.> ...
%! rj ('{"a": 1}', 'DuplicateKeyRule', 'x');
## Ours: not measured on R2026b.
%!error<readdictionary: invalid 'ParsingMode' value 'x'.> ...
%! rj ('{"a": 1}', 'ParsingMode', 'x');
## Ours: not measured on R2026b.
%!error<readdictionary: 'AllowComments' must be a logical scalar.> ...
%! rj ('{"a": 1}', 'AllowComments', 'yes');
## Ours: not measured on R2026b.
%!error<readdictionary: 'AllowTrailingCommas' must be a logical scalar.> ...
%! rj ('{"a": 1}', 'AllowTrailingCommas', [true, true]);
## Ours: not measured on R2026b.
%!error<readdictionary: 'AllowInfAndNaN' must be a logical scalar.> ...
%! rj ('{"a": 1}', 'AllowInfAndNaN', NaN);
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 1}', 'AllowInfAndNaN', 1);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {1});
## Ours: not measured on R2026b.
%!error<readdictionary: 'DateLocale' must be a character vector or a string scalar.> ...
%! rj ('{"a": 1}', 'DateLocale', 5);
## Ours: not measured on R2026b.
%!error<readdictionary: 'DictionarySelector' must be a character vector or a string scalar.> ...
%! rj ('{"a": 1}', 'DictionarySelector', 5);
## Ours: not measured on R2026b.
%!error<readdictionary: 'DictionaryNodeName' must be a character vector or a string scalar.> ...
%! rj ('{"a": 1}', 'DictionaryNodeName', 5);
## Ours: not measured on R2026b.
%!error<readdictionary: 'WebOptions' is not supported.> ...
%! rj ('{"a": 1}', 'WebOptions', 1);
## Ours: not measured on R2026b.
%!error<readdictionary: compressed files are not supported.> ...
%! rjx ('{"a": 1}', '.gz');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!         'DictionarySelector', '/w/0');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('q')});
%! assert_equal (values (r, 'cell'), {2});
## Ours: not measured on R2026b.
%!error<readdictionary: selector '/w/2' not found.> ...
%! rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!     'DictionarySelector', '/w/2');
## Ours: not measured on R2026b.
%!error<readdictionary: selector '/w/01' not found.> ...
%! rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!     'DictionarySelector', '/w/01');
## Ours: not measured on R2026b.
%!error<readdictionary: the JSON value read must be an object.> ...
%! rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!     'DictionarySelector', '/x/y');
## Ours: not measured on R2026b.
%!error<readdictionary: selector '/' not found.> ...
%! rj ('{"x": {"y": 1, "z": 2}, "w": [{"q": 2}, {"r": 3}]}', ...
%!     'DictionarySelector', '/');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a/b": {"c": 1}, "m~n": {"d": 2}}', 'DictionarySelector', '/a~1b');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('c')});
%! assert_equal (values (r, 'cell'), {1});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a/b": {"c": 1}, "m~n": {"d": 2}}', 'DictionarySelector', '/m~0n');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('d')});
%! assert_equal (values (r, 'cell'), {2});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"arr": [{"t": {"q": 1}}]}', 'DictionaryNodeName', 't');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('q')});
%! assert_equal (values (r, 'cell'), {1});
## Ours: not measured on R2026b.
%!error<readdictionary: the JSON value read must be an object.> ...
%! rj ('{"t": 5}', 'DictionaryNodeName', 't');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 1, "b": 2, "a": 3}', 'DuplicateKeyRule', 'preserveLast');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {3; 2});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 1, "a": 2, "a_1": 3, "a_2": 4, "a": 5}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 5);
%! assert_equal (keys (r, 'cell'), ...
%!               {string('a'); string('a_3'); string('a_1'); string('a_2'); ...
%!                string('a_4')});
%! assert_equal (values (r, 'cell'), {1; 2; 3; 4; 5});
## Ours: not measured on R2026b.
%!error<readdictionary: duplicate key 'b'.> ...
%! rj ('{"b": 1, "a": 2, "b": 3}', 'DuplicateKeyRule', 'error');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": {"x": 1, "x": 2}}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'double'}));
%! assert_equal (numEntries (dc6), 2);
%! assert_equal (keys (dc6, 'cell'), {string('x'); string('x_1')});
%! assert_equal (values (dc6, 'cell'), {1; 2});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": -1, "b": 9223372036854775807}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'int64'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {int64(-1); intmax('int64')});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": -1, "b": 9223372036854775808}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), ...
%!               {{-1}; {typecast(uint32([0, 2147483648]), 'uint64')}});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 18446744073709551615}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint64'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {intmax('uint64')});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 18446744073709551616}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {1.8446744073709552e+19});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 9007199254740992}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {9007199254740992.0});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 12345678901234567890, "b": 1}', 'ValueType', 'int64');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'int64'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {intmax('int64'); int64(1)});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 12345678901234567890}', 'ValueType', 'double');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {1.2345678901234567e+19});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 12345678901234567890}', 'ValueType', 'string');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {string('12345678901234567890')});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": 1.5, "b": -2}', 'ValueType', 'string');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {string('1.5'); string('-2')});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": NaN, "b": 0, "c": 2}', 'ValueType', 'logical');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {false; false; true});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": [1], "b": {"x": 1}, "c": null}', 'ValueType', 'logical');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'logical'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! assert_equal (values (r, 'cell'), {false; false; false});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": [1], "b": {"x": 1}, "c": null}', 'ValueType', 'duration');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'duration'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'duration');
%! assert_equal (seconds (vc4{1}), NaN);
%! assert_equal (class (vc4{2}), 'duration');
%! assert_equal (seconds (vc4{2}), NaN);
%! assert_equal (class (vc4{3}), 'duration');
%! assert_equal (seconds (vc4{3}), NaN);
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": true, "b": false}', 'ValueType', 'uint8');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'uint8'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {uint8(1); uint8(0)});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": true}', 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'NaT'});
%! assert_equal (x5.TimeZone, '');
## Ours: not measured on R2026b.
%!test
%! r = rj (['{"a": "2024-01-01T10:00:00.250", ', ...
%!          '"b": "2024-01-02T11:30:00.500"}']);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 10:00:00.250'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-01-02 11:30:00.500'});
%! assert_equal (x6.TimeZone, '');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": "2024-01-01", "b": "01-Jan-2024"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 00:00:00.000'});
%! assert_equal (x5.TimeZone, '');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-01-01 00:00:00.000'});
%! assert_equal (x6.TimeZone, '');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": "01-Jan-2024 10:00:00"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 10:00:00.000'});
%! assert_equal (x5.TimeZone, '');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": "2024-13-01"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! assert_equal (values (r, 'cell'), {string('2024-13-01')});
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": "01:30:00", "b": null}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'duration'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! assert_equal (class (vc4{1}), 'duration');
%! assert_equal (seconds (vc4{1}), 5400);
%! assert_equal (class (vc4{2}), 'duration');
%! assert_equal (seconds (vc4{2}), NaN);
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": "01:30:00", "b": "x"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! assert_equal (class (ce5{1}), 'duration');
%! assert_equal (seconds (ce5{1}), 5400);
%! assert_equal (vc4{2}, {string('x')});
## Ours: not measured on R2026b.
%!test
%! r = rj (['{"a": "2024-01-01T10:00:00Z", ', ...
%!          '"b": "2024-01-01T10:00:00+02:00", ', ...
%!          '"c": "2024-01-01T10:00:00", "d": "no"}'], 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 4);
%! assert_equal (keys (r, 'cell'), ...
%!               {string('a'); string('b'); string('c'); string('d')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 10:00:00.000'});
%! assert_equal (x5.TimeZone, 'UTC');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-01-01 10:00:00.000'});
%! assert_equal (x6.TimeZone, '+02:00');
%! x7 = vc4{3};
%! x7.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x7), {'2024-01-01 10:00:00.000'});
%! assert_equal (x7.TimeZone, '');
%! x8 = vc4{4};
%! x8.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x8), {'NaT'});
%! assert_equal (x8.TimeZone, '');
## Ours: not measured on R2026b.
%!test
%! r = rj (['{"a": "2024-01-01T10:00:00Z", ', ...
%!          '"b": "2024-06-01T08:00:00Z", "c": null}'], 'ValueType', ...
%!         'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 3);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b'); string('c')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 10:00:00.000'});
%! assert_equal (x5.TimeZone, 'UTC');
%! x6 = vc4{2};
%! x6.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x6), {'2024-06-01 08:00:00.000'});
%! assert_equal (x6.TimeZone, 'UTC');
%! x7 = vc4{3};
%! x7.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x7), {'NaT'});
%! assert_equal (x7.TimeZone, 'UTC');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": "2024-01-01T10:00:00+0530"}', 'ValueType', 'datetime');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'datetime'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! x5 = vc4{1};
%! x5.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x5), {'2024-01-01 10:00:00.000'});
%! assert_equal (x5.TimeZone, '+05:30');
## Ours: not measured on R2026b.
%!test
%! r = rj (['{"a": ["2024-01-01", "x", "01:30:00", null, ', ...
%!          'true, 1, 12345678901234567890]}']);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! ce6 = ce5{1};
%! assert_equal (size (ce6), [7, 1]);
%! x7 = ce6{1};
%! x7.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x7), {'2024-01-01 00:00:00.000'});
%! assert_equal (x7.TimeZone, '');
%! assert_equal (ce6{2}, string ('x'));
%! assert_equal (class (ce6{3}), 'duration');
%! assert_equal (seconds (ce6{3}), 5400);
%! assert_equal (class (ce6{4}), 'missing');
%! assert_equal (size (ce6{4}), [1, 1]);
%! assert_equal (ce6{5}, true);
%! assert_equal (ce6{6}, 1);
%! assert_equal (ce6{7}, typecast (uint32([3944680146, 2874452364]), 'uint64'));
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": {"x": "2024-01-01"}}', 'ValueType', 'cell');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'cell'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('x')});
%! vc10 = values (dc6, 'cell');
%! ce11 = vc10{1};
%! assert_equal (size (ce11), [1, 1]);
%! x12 = ce11{1};
%! x12.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
%! assert_equal (cellstr (x12), {'2024-01-01 00:00:00.000'});
%! assert_equal (x12.TimeZone, '');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": {"b": {"c": {}}}}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'cell'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('a')});
%! vc4 = values (r, 'cell');
%! ce5 = vc4{1};
%! assert_equal (size (ce5), [1, 1]);
%! dc6 = ce5{1};
%! [kt7, vt8] = types (dc6);
%! assert_equal ([kt7, vt8], string ({'string', 'cell'}));
%! assert_equal (numEntries (dc6), 1);
%! assert_equal (keys (dc6, 'cell'), {string('b')});
%! vc10 = values (dc6, 'cell');
%! ce11 = vc10{1};
%! assert_equal (size (ce11), [1, 1]);
%! dc12 = ce11{1};
%! [kt13, vt14] = types (dc12);
%! assert_equal ([kt13, vt14], string ({'string', 'cell'}));
%! assert_equal (numEntries (dc12), 1);
%! assert_equal (keys (dc12, 'cell'), {string('c')});
%! vc16 = values (dc12, 'cell');
%! ce17 = vc16{1};
%! assert_equal (size (ce17), [1, 1]);
%! assert_equal (isConfigured (ce17{1}), false);
## Ours: not measured on R2026b.
%!error<readdictionary: missing ':' after an object key at line 1, column 6.> ...
%! rj ('{"a" 1}');
## Ours: not measured on R2026b.
%!error<readdictionary: missing object key at line 1, column 2.> ...
%! rj ('{a: 1}');
## Ours: not measured on R2026b.
%!error<readdictionary: invalid number at line 1, column 7.> ...
%! rj ('{"a": 01}');
## Ours: not measured on R2026b.
%!error<readdictionary: invalid number at line 1, column 9.> ...
%! rj ('{"a": 1.}');
## Ours: not measured on R2026b.
%!error<readdictionary: unterminated text at line 1, column 10.> ...
%! rj ('{"a": "x}');
## Ours: not measured on R2026b.
%!error<readdictionary: invalid escape in text at line 1, column 9.> ...
%! rj ('{"a": "\q"}');
## Ours: not measured on R2026b.
%!error<readdictionary: invalid \\u escape at line 1, column 10.> ...
%! rj ('{"a": "\u12"}');
## Ours: not measured on R2026b.
%!error<readdictionary: invalid \\u escape at line 1, column 14.> ...
%! rj ('{"a": "\ud83d"}');
## Ours: not measured on R2026b.
%!error<readdictionary: unexpected text after the value at line 1, column 10.> ...
%! rj ('{"a": 1} x');
## Ours: not measured on R2026b.
%!error<readdictionary: the text holds no value at line 1, column 1.> ...
%! rj ('');
## Ours: not measured on R2026b.
%!error<readdictionary: the text holds no value at line 1, column 11.> ...
%! rj ('/* only */');
## Ours: not measured on R2026b.
%!error<readdictionary: unterminated comment at line 1, column 10.> ...
%! rj ('{"a": 1} /* tail');
## Ours: not measured on R2026b.
%!error<readdictionary: invalid value at line 1, column 7.> ...
%! rj ('{"a": -}');
## Ours: not measured on R2026b.
%!error<readdictionary: missing ',' or '\]' in an array at line 1, column 4.> ...
%! rj ('[1 2]');
## Ours: not measured on R2026b.
%!test
%! r = rj ('{"a": "\ud83d\ude00", "b": "\u00e9\/"}');
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 2);
%! assert_equal (keys (r, 'cell'), {string('a'); string('b')});
%! assert_equal (values (r, 'cell'), {string('😀'); string('é/')});
## Ours: not measured on R2026b.
%!error<readdictionary: a control character in text must be escaped at line 1, column 9.> ...
%! rj (sprintf ('{"a": "x\ny"}'));
## Ours: not measured on R2026b.
%!test
%! r = rj ([char([239, 187, 191]), '{"bom": 1}']);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'double'}));
%! assert_equal (numEntries (r), 1);
%! assert_equal (keys (r, 'cell'), {string('bom')});
%! assert_equal (values (r, 'cell'), {1});
## Ours: not measured on R2026b.
%!test
%! r = rj (['{"a": "2024-13-01", "b": "2024-02-30", ', ...
%!          '"c": "31-Feb-2024", "d": "2024-01-01T24:00:00"}']);
%! [kt1, vt2] = types (r);
%! assert_equal ([kt1, vt2], string ({'string', 'string'}));
%! assert_equal (numEntries (r), 4);
%! assert_equal (keys (r, 'cell'), ...
%!               {string('a'); string('b'); string('c'); string('d')});
%! assert_equal (values (r, 'cell'), ...
%!               {string('2024-13-01'); string('2024-02-30'); ...
%!                string('31-Feb-2024'); string('2024-01-01T24:00:00')});

## The nesting the parser allows, which readdictionary follows
%!test
%! r = rj ([repmat('{"a": ', 1, 63), '[1]', repmat('}', 1, 63)]);
%! assert_equal (numEntries (r), 1);
%!error<readdictionary: nesting too deep at line 1, column 386.> ...
%! rj ([repmat('{"a": ', 1, 64), '[1]', repmat('}', 1, 64)]);

## File names and locations
%!error<readdictionary: FILENAME must be a character vector or a string scalar.> ...
%! readdictionary (5);
%!error<readdictionary: unable to find the file '/nonexistent/x.json'.> ...
%! readdictionary ('/nonexistent/x.json');
%!error<readdictionary: reading from 's3' locations is not supported.> ...
%! readdictionary ('s3://bucket/x.json');
%!error<readdictionary: reading from 'wasbs' locations is not supported.> ...
%! readdictionary ('wasbs://box@account/x.json');
%!error<readdictionary: reading from 'hdfs' locations is not supported.> ...
%! readdictionary ('hdfs:///data/x.json');
%!error<readdictionary: unable to tell the file type from the extension; set 'FileType'.> ...
%! readdictionary ('/nonexistent/x');

## OpenDocument spreadsheets, an extension of this package
%!test
%! s.Sales = table ([1; 2]);
%! s.Costs = table ([3; 4]);
%! f = [tempname() '.ods'];
%! unwind_protect
%!   struct2ods (f, s);
%!   r = readdictionary (f);
%!   assert_equal (keys (r), string ({'Sales'; 'Costs'}));
%!   assert_equal (types (r), string ('string'));
%!   [~, vt] = types (r);
%!   assert_equal (vt, string ('table'));
%!   assert_equal (r(string ('Costs')).Var1, [3; 4]);
%! unwind_protect_cleanup
%!   delete (f);
%! end_unwind_protect
%!test
%! s.T = timetable (hours ([1; 2]), [3; 4]);
%! f = [tempname() '.ods'];
%! unwind_protect
%!   struct2ods (f, s);
%!   r = readdictionary (f, 'FileType', 'ods');
%!   [~, vt] = types (r);
%!   assert_equal (vt, string ('timetable'));
%!   assert_equal (r(string ('T')).Time, hours ([1; 2]));
%! unwind_protect_cleanup
%!   delete (f);
%! end_unwind_protect
%!test
%! s.T = table (1);
%! s.TT = timetable (hours (1), 2);
%! f = [tempname() '.ods'];
%! unwind_protect
%!   struct2ods (f, s);
%!   r = readdictionary (f);
%!   [~, vt] = types (r);
%!   assert_equal (vt, string ('cell'));
%!   assert_equal (class (r{string('TT')}), 'timetable');
%! unwind_protect_cleanup
%!   delete (f);
%! end_unwind_protect
%!test
%! t = addprop (table (5), 'ActualSheetName', 'table');
%! t.Properties.CustomProperties.ActualSheetName = 'Q 2024';
%! s.Q_2024 = t;
%! f = [tempname() '.ods'];
%! unwind_protect
%!   struct2ods (f, s);
%!   r = readdictionary (f);
%!   assert_equal (keys (r), string ('Q 2024'));
%!   p = properties (r(string ('Q 2024')).Properties.CustomProperties);
%!   assert_equal (any (strcmp (p, 'ActualSheetName')), false);
%! unwind_protect_cleanup
%!   delete (f);
%! end_unwind_protect
%!error<readdictionary: 'ValueType' does not apply to ODS files.> ...
%! readdictionary ('/nonexistent/x.ods', 'ValueType', 'cell');
