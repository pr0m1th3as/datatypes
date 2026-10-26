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

classdef dictionary
  ## -*- texinfo -*-
  ## @deftp {datatypes} dictionary
  ##
  ## Map of unique keys to values.
  ##
  ## A @code{dictionary} object holds entries, each a key and the value it
  ## maps to.  Every key is of one type and every value of one type, the key
  ## type and the value type of the dictionary; two keys are the same key when
  ## @code{keyMatch} says so.
  ##
  ## A dictionary with no entries may have no key and value types yet, when
  ## it is unconfigured; the first entries added configure it, and
  ## @code{configureDictionary} creates one configured with no entries.
  ##
  ## @code{dictionary} is a value class, and a dictionary is always a scalar
  ## object: @code{numel}, @code{size} and @code{isempty} describe the object,
  ## not its entries, which @code{numEntries} counts.  Dictionaries cannot be
  ## concatenated.
  ##
  ## Entries are reached by indexing with keys, converted to the key type as
  ## @code{dictionary} converts them:
  ##
  ## @itemize
  ## @item @code{@var{d}(@var{keys})} returns the values of @var{keys}, an
  ## array of keys, in an array of the same shape; a key not in @var{d} is an
  ## error.
  ##
  ## @item @code{@var{d}(@var{keys}) = @var{values}} adds entries, or replaces
  ## the values of keys already present, which keep their positions;
  ## @var{values} has the size of @var{keys}, or is one value for all.  An
  ## unconfigured dictionary takes its types from the first entries.
  ##
  ## @item @code{@var{d}(@var{keys}) = []} removes the entries of
  ## @var{keys}, ignoring keys not in @var{d}.  A key removed and added
  ## again goes after the others.
  ##
  ## @item @code{@var{d}@{@var{keys}@}}, for a dictionary of @qcode{'cell'}
  ## values, returns the contents of their cells as a comma-separated list,
  ## and @code{@var{d}@{@var{key}@} = @var{value}} stores @var{value} in a
  ## cell under the single key @var{key}, @code{[]} included.
  ##
  ## @item @code{@var{d}.@var{method} (@dots{})} calls a method, as
  ## @code{@var{d}.keys ()}.
  ## @end itemize
  ##
  ## Indexing can continue into a value, as in @code{@var{d}(@var{key}).x}
  ## or @code{@var{d}@{@var{key}@}(2) = 5}; assigning into the contents of
  ## a new key starts from empty.  @code{end} is not supported.  Octave
  ## cannot tell @code{@var{d}(:)} from @code{@var{d}(':')}, so @qcode{':'}
  ## is the key @qcode{":"}, as it is in MATLAB in an assignment; use
  ## @code{keys} or @code{values} for every entry.
  ##
  ## @code{jsonencode} refuses a dictionary, as in MATLAB, and
  ## @code{writedictionary} writes one to a JSON file.
  ##
  ## @seealso{configureDictionary, isdictionary, keyHash, keyMatch}
  ## @end deftp

  properties (Access = private)
    ## The key and value types, '' while unconfigured
    keyType_ = ''
    valueType_ = ''
    ## One key and one value per entry, in insertion order, and an empty
    ## column of each type, which keeps its time zone or categories
    keys_ = cell (0, 1)
    values_ = cell (0, 1)
    keyEmpty_ = []
    valueEmpty_ = []
    ## The hash codes of the keys, sorted, and the entry of each
    hashes_ = zeros (0, 1, 'uint64')
    order_ = zeros (0, 1)
  endproperties

  methods (Hidden)

    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n\n", in_name);
      endif
      disp (this);
    endfunction

    function str = disp (this)
      ## With an output, the text it would print, as Octave's disp gives
      if (nargout > 0)
        str = evalc ('disp (this)');
        return;
      endif
      n = numel (this.keys_);
      if (n == 0)
        fprintf ("  %s.\n\n", summaryLine (this));
        return;
      endif
      fprintf ("  %s:\n\n", summaryLine (this));
      K = cell (n, 1);
      V = cell (n, 1);
      w = zeros (n, 1);
      for i = 1:n
        K{i} = itemText (this.keys_{i});
        V{i} = itemText (this.values_{i});
        w(i) = textWidth (K{i});
      endfor
      for i = 1:n
        fprintf ("    %s%s ⟼ %s\n", K{i}, blanks (max (w) - w(i)), V{i});
      endfor
      fprintf ("\n");
    endfunction

    ## A dictionary has no properties to list
    function names = properties (this)
      if (nargout == 0)
        fprintf ("\nNo properties for class dictionary.\n\n");
      else
        names = cell (0, 1);
      endif
    endfunction

    ## Refused, as in MATLAB, rather than exposing the private storage.  A
    ## dictionary among the arguments also brings 'struct' here, as in
    ## struct ('d', d), which the built-in function answers.
    function s = struct (varargin)
      if (nargin == 1)
        error (strcat ("dictionary.struct: cannot convert a dictionary", ...
                       " to a structure."));
      endif
      s = builtin ('struct', varargin{:});
    endfunction

    function json = jsonencode (this, varargin)
      error ("dictionary.jsonencode: cannot encode a dictionary as JSON text.");
    endfunction

  endmethods

################################################################################
##                  ** Create and describe 'dictionary' **                    ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'dictionary'       'types'            'isConfigured'     'numEntries'      ##
## 'numel'            'length'           'size'             'ndims'           ##
## 'isempty'          'dispstrings'                                           ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{d} =} dictionary ()
    ## @deftypefnx {dictionary} {@var{d} =} dictionary (@var{keys}, @var{values})
    ## @deftypefnx {dictionary} {@var{d} =} dictionary (@var{k1}, @var{v1}, @dots{}, @var{kN}, @var{vN})
    ##
    ## Create a @code{dictionary} object.
    ##
    ## @code{@var{d} = dictionary ()} returns an unconfigured dictionary, with
    ## no entries and no key and value types.
    ##
    ## @code{@var{d} = dictionary (@var{keys}, @var{values})} returns a
    ## dictionary mapping each element of the array @var{keys} to the element
    ## of @var{values} in the same position, in that order.  The two must be
    ## of the same size, or @var{values} a single value given to every key; an
    ## empty @var{keys} gives a configured dictionary with no entries.  A key
    ## repeated keeps its first position and takes its last value.  The class
    ## of @var{keys} is the key type and that of @var{values} the value type,
    ## except that a character array is a string array of its rows.
    ##
    ## The key type must be one that @code{keyHash} can hash: a built-in type
    ## or a class with a @code{keyHash} method; a cell array is a key type of
    ## its own, each cell one key.  An array of objects holds one key or value
    ## per object, but a @code{table} or @code{timetable} of any size is one
    ## value, where MATLAB accepts only a 1-by-1 one.
    ##
    ## @code{@var{d} = dictionary (@var{k1}, @var{v1}, @dots{}, @var{kN},
    ## @var{vN})} adds each pair of keys and values in turn, the first pair
    ## setting the key and value types and later pairs converted to them.
    ## Keys and values of another class are converted by the function of the
    ## type's name, as in MATLAB: @code{string (@var{x})}, @code{double
    ## (@var{x})}, @code{categorical (@var{x})} and so on, and refused where
    ## that fails or gives another class.  So @code{5} becomes @qcode{"5"} as
    ## a string, @qcode{"3.5"} becomes 3.5 as a @code{double} but is refused
    ## by @code{single} and the integer types, a character array gives its
    ## character codes as a number, a @code{categorical} its code, and
    ## @code{missing} gives @code{NaN}, @code{NaT} or an undefined category.
    ## The exceptions:
    ##
    ## @itemize
    ## @item a @qcode{'cell'} type takes only cell arrays, which are never
    ## wrapped, as in MATLAB;
    ## @item a @qcode{'struct'} type takes only structures, where MATLAB turns
    ## an object into a structure of its private properties;
    ## @item a @qcode{'table'} type makes a table of the variable
    ## @qcode{Var1} and refuses character arrays, as in MATLAB;
    ## @item a @qcode{'logical'} type takes a character as true for a nonzero
    ## code, as in MATLAB;
    ## @item a @qcode{'datetime'} type reads dates given as text in the time
    ## zone of the dictionary, and moves a key with a time zone to that of the
    ## keys; a value keeps its own.
    ## @end itemize
    ##
    ## A date with a time zone and one without are different keys, so that
    ## looking up one as the other finds nothing, and @code{keys} cannot join
    ## a dictionary holding both.  Dates given as text to a dictionary whose
    ## keys have a time zone are read in that zone, so that they find its
    ## keys; MATLAB reads them without one, a key that no date with a zone
    ## matches.
    ##
    ## @seealso{configureDictionary, dictionary.isConfigured, dictionary.keys,
    ## keyHash, keyMatch}
    ## @end deftypefn
    function this = dictionary (varargin)
      if (nargin == 0)
        return;
      endif
      if (mod (nargin, 2) != 0)
        error ("dictionary: each key must be followed by a value.");
      endif
      for i = 1:2:nargin
        [this, errmsg] = addEntries (this, varargin{i}, varargin{i+1});
        if (! isempty (errmsg))
          error ("dictionary: %s", errmsg);
        endif
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {[@var{keyType}, @var{valueType}] =} types (@var{d})
    ## @deftypefnx {dictionary} {@var{keyType} =} types (@var{d})
    ##
    ## Return the key and value types.
    ##
    ## @code{[@var{keyType}, @var{valueType}] = types (@var{d})} returns the
    ## class names of the keys and of the values of @var{d}, each a string
    ## scalar, or a missing string while @var{d} is unconfigured.
    ##
    ## @seealso{dictionary.isConfigured, configureDictionary}
    ## @end deftypefn
    function [keyType, valueType] = types (this)
      if (isempty (this.keyType_))
        keyType = string (missing);
        valueType = string (missing);
      else
        keyType = string (this.keyType_);
        valueType = string (this.valueType_);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{tf} =} isConfigured (@var{d})
    ##
    ## True for a dictionary with key and value types.
    ##
    ## @code{@var{tf} = isConfigured (@var{d})} returns @code{false} for an
    ## unconfigured dictionary, which has no entries and no key and value
    ## types yet, and @code{true} otherwise, entries or not.
    ##
    ## @seealso{dictionary.types, configureDictionary}
    ## @end deftypefn
    function tf = isConfigured (this)
      tf = ! isempty (this.keyType_);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{n} =} numEntries (@var{d})
    ##
    ## Return the number of entries.
    ##
    ## @code{@var{n} = numEntries (@var{d})} returns the number of entries in
    ## @var{d} as a @code{double}, 0 for an unconfigured dictionary.
    ##
    ## @seealso{dictionary.numel}
    ## @end deftypefn
    function n = numEntries (this)
      n = numel (this.keys_);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{n} =} numel (@var{d})
    ##
    ## Return 1, the number of elements of a scalar object.
    ##
    ## @code{@var{n} = numel (@var{d})} is always 1, however many entries
    ## @var{d} holds; @code{numEntries} counts them.
    ##
    ## @seealso{dictionary.numEntries, dictionary.size}
    ## @end deftypefn
    function n = numel (this, varargin)
      n = 1;
      ## Octave asks with the subscripts how many values '{}' yields
      if (numel (varargin) == 1 && ! isempty (this.keyType_))
        [K, errmsg] = convert (varargin{1}, this.keyType_, this.keyEmpty_, ...
                               'key');
        if (isempty (errmsg))
          n = prod (arraySize (K));
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{n} =} length (@var{d})
    ##
    ## Return 1, the length of a scalar object.
    ##
    ## @code{@var{n} = length (@var{d})} is always 1, however many entries
    ## @var{d} holds; @code{numEntries} counts them.
    ##
    ## @seealso{dictionary.numEntries, dictionary.size}
    ## @end deftypefn
    function n = length (this)
      n = 1;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{sz} =} size (@var{d})
    ## @deftypefnx {dictionary} {@var{dim_sz} =} size (@var{d}, @var{dim})
    ## @deftypefnx {dictionary} {@var{dim_sz} =} size (@var{d}, @var{d1}, @var{d2}, @dots{})
    ## @deftypefnx {dictionary} {[@var{rows}, @var{cols}, @dots{}] =} size (@var{d})
    ##
    ## Return the size of a scalar object.
    ##
    ## @code{@var{sz} = size (@var{d})} is always @code{[1, 1]}, however many
    ## entries @var{d} holds.  @code{size (@var{d}, @var{dim})} returns 1 for
    ## each dimension in @var{dim}, a positive integer or a vector of them,
    ## and @code{size (@var{d}, @var{d1}, @var{d2}, @dots{})} returns 1 for
    ## each dimension given.  With several outputs each takes one dimension.
    ##
    ## @seealso{dictionary.numel, dictionary.numEntries}
    ## @end deftypefn
    function varargout = size (this, varargin)
      if (nargin > 1)
        if (nargin > 2 && ! all (cellfun ('numel', varargin) == 1))
          error ("dictionary.size: each dimension must be a scalar.");
        endif
        if (! (all (cellfun ('isnumeric', varargin))
               && all (cellfun ('isreal', varargin))))
          error ("dictionary.size: DIM must be positive integers.");
        endif
        dim = [varargin{:}];
        if (! (! isempty (dim) && all (dim == fix (dim) & dim >= 1)))
          error ("dictionary.size: DIM must be positive integers.");
        endif
        if (nargout > 1)
          [varargout{1:nargout}] = deal (1);
        else
          varargout{1} = ones (1, numel (dim));
        endif
      elseif (nargout <= 1)
        varargout{1} = [1, 1];
      else
        [varargout{1:nargout}] = deal (1);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{n} =} ndims (@var{d})
    ##
    ## Return 2, the number of dimensions of a scalar object.
    ##
    ## @seealso{dictionary.size}
    ## @end deftypefn
    function n = ndims (this)
      n = 2;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{tf} =} isempty (@var{d})
    ##
    ## Return @code{false}: a dictionary is never an empty array.
    ##
    ## @code{@var{tf} = isempty (@var{d})} is @code{false} even for a
    ## dictionary with no entries, since the object itself is a scalar; test
    ## @code{numEntries (@var{d}) == 0} for a dictionary with no entries.
    ##
    ## @seealso{dictionary.numEntries, dictionary.isConfigured}
    ## @end deftypefn
    function tf = isempty (this)
      tf = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{cstr} =} dispstrings (@var{d})
    ##
    ## Return the line that describes a dictionary.
    ##
    ## @code{@var{cstr} = dispstrings (@var{d})} returns a 1-by-1 cell array
    ## holding the line that describes @var{d} by its key and value types and
    ## its number of entries, as @qcode{'dictionary (string ⟼ double) with 2
    ## entries'}, or @qcode{'dictionary with unset key and value types'} while
    ## it is unconfigured.  A cell array displays a dictionary by this line,
    ## and so does a dictionary holding dictionaries.
    ##
    ## @seealso{dictionary.types, dictionary.numEntries}
    ## @end deftypefn
    function cstr = dispstrings (this)
      cstr = {summaryLine(this)};
    endfunction

  endmethods

################################################################################
##                          ** Keys and values **                             ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'keys'             'values'           'isKey'                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{k} =} keys (@var{d})
    ## @deftypefnx {dictionary} {@var{k} =} keys (@var{d}, @var{format})
    ##
    ## Return the keys.
    ##
    ## @code{@var{k} = keys (@var{d})} returns the keys of @var{d} as a
    ## column array of the key type, in the order they were added.  Keys of a
    ## type that cannot be joined into one array, such as function handles,
    ## are refused unless there is exactly one.
    ##
    ## @code{@var{k} = keys (@var{d}, @var{format})} returns them as above for
    ## @var{format} @qcode{'uniform'}, the default, and as a column cell
    ## array, each key in a cell of its own, for @qcode{'cell'}.
    ## @var{format} is matched in any case and by its first letters.
    ##
    ## @var{d} must be configured.
    ##
    ## @seealso{dictionary.values, dictionary.isKey, dictionary.numEntries}
    ## @end deftypefn
    function k = keys (this, format = 'uniform')
      scope = 'dictionary.keys';
      [format, errmsg] = formatName (format, {'uniform', 'cell'});
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      if (isempty (this.keyType_))
        error ("%s: D is not configured.", scope);
      endif
      if (strcmp (format, 'cell'))
        k = this.keys_;
        return;
      endif
      [k, errmsg] = join (this.keys_, this.keyEmpty_, this.keyType_, 'keys');
      if (! isempty (errmsg))
        error ("%s: %s; use the 'cell' format.", scope, errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{v} =} values (@var{d})
    ## @deftypefnx {dictionary} {@var{v} =} values (@var{d}, @var{format})
    ##
    ## Return the values.
    ##
    ## @code{@var{v} = values (@var{d})} returns the values of @var{d} as a
    ## column array of the value type, in the order their keys were added.
    ## Values that cannot be joined into one array, such as function handles,
    ## dictionaries or structures with different fields, are refused unless
    ## there is exactly one.  Tables are joined only when each has one row,
    ## so that row @var{i} is the value of key @var{i}.
    ##
    ## @code{@var{v} = values (@var{d}, @var{format})} returns them as above
    ## for @var{format} @qcode{'uniform'}, the default, and as a column cell
    ## array, each value in a cell of its own, for @qcode{'cell'}.
    ## @var{format} is matched in any case and by its first letters.
    ##
    ## @var{d} must be configured.
    ##
    ## @seealso{dictionary.keys, dictionary.numEntries}
    ## @end deftypefn
    function v = values (this, format = 'uniform')
      scope = 'dictionary.values';
      [format, errmsg] = formatName (format, {'uniform', 'cell'});
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      if (isempty (this.keyType_))
        error ("%s: D is not configured.", scope);
      endif
      if (strcmp (format, 'cell'))
        v = this.values_;
        return;
      endif
      [v, errmsg] = join (this.values_, this.valueEmpty_, this.valueType_, ...
                          'values');
      if (! isempty (errmsg))
        error ("%s: %s; use the 'cell' format.", scope, errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{tf} =} isKey (@var{d}, @var{keys})
    ##
    ## True for each of the keys that is in the dictionary.
    ##
    ## @code{@var{tf} = isKey (@var{d}, @var{keys})} returns a logical array
    ## of the size of @var{keys}, true where the element of @var{keys} is a key
    ## of @var{d}.  The keys are first converted to the key type of @var{d}, as
    ## @code{dictionary} converts them, so that @code{isKey (@var{d}, 1)} on a
    ## dictionary of string keys asks for the key @qcode{"1"}; a class that
    ## cannot be converted is refused.  @var{d} must be configured.
    ##
    ## @seealso{dictionary.keys, keyMatch}
    ## @end deftypefn
    function tf = isKey (this, keys)
      scope = 'dictionary.isKey';
      if (nargin != 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (isempty (this.keyType_))
        error ("%s: D is not configured.", scope);
      endif
      [keys, errmsg] = convert (keys, this.keyType_, this.keyEmpty_, 'key');
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      if (isArray (keys))
        tf = false (size (keys));
      else
        tf = false;
      endif
      for i = 1:numel (tf)
        k = element (keys, i);
        try
          h = keyHash (k);
        catch err
          error ("%s: %s", scope, regexprep (err.message, '^keyHash: ', ''));
        end_try_catch
        tf(i) = locate (this.hashes_, this.order_, this.keys_, k, h) > 0;
      endfor
    endfunction

  endmethods

################################################################################
##                             ** Entries **                                  ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'insert'           'lookup'           'remove'           'entries'         ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{d} =} insert (@var{d}, @var{keys}, @var{values})
    ## @deftypefnx {dictionary} {@var{d} =} insert (@dots{}, @qcode{'Overwrite'}, @var{tf})
    ##
    ## Add entries.
    ##
    ## @code{@var{d} = insert (@var{d}, @var{keys}, @var{values})} adds the
    ## entries of @var{keys} and @var{values} to @var{d}, as
    ## @code{@var{d}(@var{keys}) = @var{values}} does: @var{values} has the
    ## size of @var{keys}, or is one value for all, and both are converted to
    ## the types of @var{d}, which an unconfigured dictionary takes from them.
    ## A key already present has its value replaced and keeps its position.
    ##
    ## @code{@var{d} = insert (@dots{}, @qcode{'Overwrite'}, @var{tf})} with
    ## @var{tf} false leaves the value of a key already present unchanged, and
    ## a key repeated in @var{keys} takes its first value; it still adds the
    ## keys not yet present.  @var{tf} is a logical or numeric scalar, true by
    ## default.  The option name is matched in any case.
    ##
    ## @seealso{dictionary.remove, dictionary.lookup, dictionary.isKey}
    ## @end deftypefn
    function this = insert (this, keys, vals, varargin)
      scope = 'dictionary.insert';
      if (nargin < 3)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      [overwrite, rest] = parsePairedArguments ({'Overwrite'}, {true}, ...
                                                varargin);
      if (! isempty (rest))
        error ("%s: invalid optional paired argument.", scope);
      endif
      if (! ((islogical (overwrite) || isnumeric (overwrite))
             && isscalar (overwrite) && ! isnan (overwrite)))
        error ("%s: 'Overwrite' must be a logical scalar.", scope);
      endif
      [this, errmsg] = addEntries (this, keys, vals, logical (overwrite));
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{v} =} lookup (@var{d}, @var{keys})
    ## @deftypefnx {dictionary} {@var{v} =} lookup (@dots{}, @qcode{'FallbackValue'}, @var{fallback})
    ##
    ## Return the values of keys.
    ##
    ## @code{@var{v} = lookup (@var{d}, @var{keys})} returns the values of
    ## @var{keys}, an array of keys, in an array of the same shape, as
    ## @code{@var{d}(@var{keys})} does; a key not in @var{d} is an error.
    ##
    ## @code{@var{v} = lookup (@dots{}, @qcode{'FallbackValue'},
    ## @var{fallback})} returns @var{fallback} for each key not in @var{d}
    ## instead.  @var{fallback} is a single value, converted to the value type
    ## of @var{d}, so that a dictionary of @qcode{'cell'} values needs a cell;
    ## it is checked only when some key is missing.  The option name is
    ## matched in any case.
    ##
    ## @var{d} must be configured.
    ##
    ## @seealso{dictionary.isKey, dictionary.insert, dictionary.values}
    ## @end deftypefn
    function v = lookup (this, keys, varargin)
      scope = 'dictionary.lookup';
      if (nargin < 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      names = varargin(1:2:end);
      names = names(cellfun (@(x) ischar (x) || isa (x, 'string'), names));
      given = any (strcmpi (cellfun (@char, names, 'UniformOutput', false), ...
                            'FallbackValue'));
      [fallback, rest] = parsePairedArguments ({'FallbackValue'}, {[]}, ...
                                               varargin);
      if (! isempty (rest))
        error ("%s: invalid optional paired argument.", scope);
      endif
      if (given)
        fallback = {fallback};
      else
        fallback = {};
      endif
      [v, errmsg] = lookupValues (this, {keys}, false, fallback);
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{d} =} remove (@var{d}, @var{keys})
    ##
    ## Remove entries.
    ##
    ## @code{@var{d} = remove (@var{d}, @var{keys})} removes the entries of
    ## @var{keys}, an array of keys converted to the key type of @var{d}, as
    ## @code{@var{d}(@var{keys}) = []} does.  A key not in @var{d} is ignored.
    ## A key removed and added again goes after the others.  @var{d} must be
    ## configured.
    ##
    ## @seealso{dictionary.insert, dictionary.isKey}
    ## @end deftypefn
    function this = remove (this, keys)
      scope = 'dictionary.remove';
      if (nargin != 2)
        error ("%s: invalid number of input arguments.", scope);
      endif
      if (isempty (this.keyType_))
        error ("%s: D is not configured.", scope);
      endif
      [this, errmsg] = removeEntries (this, keys);
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{e} =} entries (@var{d})
    ## @deftypefnx {dictionary} {@var{e} =} entries (@var{d}, @var{format})
    ##
    ## Return the keys and values together.
    ##
    ## @code{@var{e} = entries (@var{d})} returns a table with the variables
    ## @qcode{Key} and @qcode{Value}, one row per entry in the order the keys
    ## were added: the columns @code{keys (@var{d})} and @code{values
    ## (@var{d})} return.  Keys or values that a table cannot hold, function
    ## handles, dictionaries or values that cannot be joined, are refused.
    ##
    ## @code{@var{e} = entries (@var{d}, @var{format})} returns the table above
    ## for @var{format} @qcode{'table'}, the default; for @qcode{'struct'} a
    ## column structure array with the fields @qcode{Key} and @qcode{Value},
    ## one element per entry; and for @qcode{'cell'} a cell array of two
    ## columns, each key and value in a cell of its own.  @var{format} is
    ## matched in any case and by its first letters.
    ##
    ## @var{d} must be configured.
    ##
    ## @seealso{dictionary.keys, dictionary.values}
    ## @end deftypefn
    function e = entries (this, format = 'table')
      scope = 'dictionary.entries';
      [format, errmsg] = formatName (format, {'table', 'struct', 'cell'});
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      if (isempty (this.keyType_))
        error ("%s: D is not configured.", scope);
      endif
      switch (format)
        case 'cell'
          e = [this.keys_, this.values_];
        case 'struct'
          e = struct ('Key', this.keys_, 'Value', this.values_);
          if (isempty (this.keys_))
            e = reshape (e, 0, 1);
          endif
        case 'table'
          if (any (strcmp ({this.keyType_, this.valueType_}, ...
                           'function_handle'))
              || any (strcmp ({this.keyType_, this.valueType_}, 'dictionary')))
            error (strcat ("%s: a table cannot hold these entries; use the", ...
                          " 'cell' or 'struct' format."), scope);
          endif
          [k, errmsg] = join (this.keys_, this.keyEmpty_, this.keyType_, ...
                              'keys');
          if (isempty (errmsg))
            [v, errmsg] = join (this.values_, this.valueEmpty_, ...
                                this.valueType_, 'values');
          endif
          if (! isempty (errmsg))
            error (strcat ("%s: a table cannot hold these entries; use the", ...
                          " 'cell' or 'struct' format."), scope);
          endif
          e = table (k, v, 'VariableNames', {'Key', 'Value'});
      endswitch
    endfunction

  endmethods

################################################################################
##                               ** Files **                                  ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'writedictionary'                                                          ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {} writedictionary (@var{d}, @var{filename})
    ## @deftypefnx {dictionary} {} writedictionary (@var{d}, @var{filename}, @var{Name}, @var{Value})
    ##
    ## Write a dictionary to a JSON or an OpenDocument spreadsheet file.
    ##
    ## @code{writedictionary (@var{d}, @var{filename})} writes @var{d} as a
    ## JSON object to the file @var{filename}, a character vector or a string
    ## scalar, replacing any file of that name.  Each entry is a member, in
    ## the order of the keys, written as follows:
    ##
    ## @itemize
    ## @item keys as text: numbers in their shortest form (@qcode{"1.5"}),
    ## logical values as @qcode{"true"} and @qcode{"false"}, dates and
    ## durations as they display;
    ## @item a @code{double} or @code{single} in the fewest digits that read
    ## back as its value, always with a point (@qcode{1.0}), with an exponent
    ## only from @math{10^21} up and below @math{10^-6}; integers exactly;
    ## @item @code{NaN} and @code{Inf} as @qcode{NaN}, @qcode{Infinity} and
    ## @qcode{-Infinity}, or as @qcode{null} under
    ## @qcode{'PreserveInfAndNaN'} false; missing strings, @code{NaT} and
    ## @code{missing} as @qcode{null}; logical values as @qcode{true} and
    ## @qcode{false};
    ## @item dates and durations as text, as they display;
    ## @item a dictionary as an object, nested ones too; the contents of a
    ## cell as the value they hold, a vector of them as an array, and an empty
    ## one as @qcode{[]}.
    ## @end itemize
    ##
    ## @code{writedictionary (@var{d}, @var{filename}, @var{Name},
    ## @var{Value})} specifies options with the following Name-Value paired
    ## arguments, matched in any case.
    ##
    ## @multitable @columnfractions 0.25 0.75
    ## @headitem @var{Name} @tab @var{Value}
    ## @item @qcode{'FileType'} @tab @qcode{'auto'} (the default) tells the
    ## type from the extension, @qcode{.json}, @qcode{.ods} or @qcode{.fods};
    ## @qcode{'json'} or @qcode{'ods'} sets it.
    ## @item @qcode{'PrettyPrint'} @tab A logical scalar: true (the default)
    ## indents by four spaces, one member or element a line; false writes no
    ## spaces or line breaks.
    ## @item @qcode{'PreserveInfAndNaN'} @tab A logical scalar: true (the
    ## default) writes @code{NaN} and @code{Inf}, false writes @qcode{null}.
    ## @end multitable
    ##
    ## Nothing is written when any key or value cannot be.  Values of types
    ## other than @code{string}, the numeric types, @code{logical},
    ## @code{datetime}, @code{duration}, @code{missing}, @code{dictionary}
    ## and @code{cell} are refused, as in MATLAB, and so are matrices in
    ## cells and complex values, on which MATLAB fails; keys of types other
    ## than @code{string}, a cell array of text, the numeric types,
    ## @code{logical}, @code{datetime} and @code{duration} are refused, and so
    ## are missing string keys.
    ##
    ## An OpenDocument spreadsheet, @qcode{FileType} @qcode{'ods'}, holds one
    ## sheet per entry, named by the key and holding its @code{table} or
    ## @code{timetable}, written by @code{struct2ods}, so that
    ## @code{readdictionary} reads it back as it was.  The keys must be text
    ## and the values tables or timetables, in a cell or not.  Writing ODS is
    ## an extension of this package; it takes no option but
    ## @qcode{'FileType'}.
    ##
    ## Remote locations, which MATLAB writes to, are not supported.
    ##
    ## @seealso{readdictionary, struct2ods}
    ## @end deftypefn
    function writedictionary (this, filename, varargin)
      scope = 'dictionary.writedictionary';
      if (nargin < 2
          || ! ((ischar (filename) && rows (filename) <= 1)
                || (isa (filename, 'string') && isscalar (filename))))
        error (strcat ("%s: FILENAME must be a character vector or a", ...
                       " string scalar."), scope);
      endif
      filename = char (filename);
      if (mod (numel (varargin), 2) != 0)
        error ("%s: name-value arguments must be in pairs.", scope);
      endif
      [fileType, pretty, preserve, args] = parsePairedArguments ...
          ({'FileType', 'PrettyPrint', 'PreserveInfAndNaN'}, ...
           {[], [], []}, varargin(:));
      if (! isempty (args))
        error ("%s: invalid optional paired argument.", scope);
      endif
      if (! isempty (regexp (filename, '^(s3|wasbs?|hdfs|https?)://', 'once')))
        error ("%s: writing to remote locations is not supported.", scope);
      endif
      [errmsg, fileType] = fileKind (fileType, filename);
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      if (strcmp (fileType, 'ods'))
        if (! isempty (pretty) || ! isempty (preserve))
          name = 'PrettyPrint';
          if (isempty (pretty))
            name = 'PreserveInfAndNaN';
          endif
          error ("%s: '%s' does not apply to ODS files.", scope, name);
        endif
        errmsg = writeOds (this, filename);
        if (! isempty (errmsg))
          error ("%s: %s", scope, errmsg);
        endif
        return;
      endif
      flags = {pretty, preserve};
      names = {'PrettyPrint', 'PreserveInfAndNaN'};
      for i = 1:2
        if (isempty (flags{i}))
          flags{i} = true;
        elseif (! ((islogical (flags{i}) || isnumeric (flags{i}))
                   && isscalar (flags{i}) && ! isnan (flags{i})))
          error ("%s: '%s' must be a logical scalar.", scope, names{i});
        endif
      endfor
      ## Every key and value is checked before anything is written
      [tree, errmsg] = jsonObject (this);
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
      errmsg = __dictionary2json__ (tree, filename, logical (flags{1}), ...
                                    logical (flags{2}));
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
    endfunction

  endmethods

################################################################################
##                         ** Compare and hash **                             ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'isequal'          'isequaln'         'keyHash'                            ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{tf} =} isequal (@var{d1}, @var{d2}, @dots{})
    ##
    ## True when dictionaries hold the same entries.
    ##
    ## @code{@var{tf} = isequal (@var{d1}, @var{d2}, @dots{})} returns
    ## @code{true} when every argument is a dictionary with the same keys, in
    ## any order, each mapped to values that @code{isequal} finds equal, so
    ## that @code{NaN} values are unequal and values of different classes may
    ## be equal.  Keys of different classes are different keys.  Two
    ## unconfigured dictionaries are equal, an unconfigured and a configured
    ## one are not, and two configured dictionaries with no entries are equal
    ## whatever their types, as in MATLAB.
    ##
    ## @seealso{dictionary.isequaln, keyMatch}
    ## @end deftypefn
    function tf = isequal (varargin)
      if (nargin < 2)
        error ("dictionary.isequal: invalid number of input arguments.");
      endif
      tf = false;
      for i = 2:nargin
        if (! (isa (varargin{1}, 'dictionary')
               && isa (varargin{i}, 'dictionary')
               && sameEntries (varargin{1}, varargin{i}, false)))
          return;
        endif
      endfor
      tf = true;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {@var{tf} =} isequaln (@var{d1}, @var{d2}, @dots{})
    ##
    ## True when dictionaries hold the same entries, @code{NaN} values equal.
    ##
    ## @code{@var{tf} = isequaln (@var{d1}, @var{d2}, @dots{})} is
    ## @code{isequal} with values compared by @code{isequaln}, so that
    ## @code{NaN} and other missing values are equal to themselves.
    ##
    ## @seealso{dictionary.isequal, keyMatch}
    ## @end deftypefn
    function tf = isequaln (varargin)
      if (nargin < 2)
        error ("dictionary.isequaln: invalid number of input arguments.");
      endif
      tf = false;
      for i = 2:nargin
        if (! (isa (varargin{1}, 'dictionary')
               && isa (varargin{i}, 'dictionary')
               && sameEntries (varargin{1}, varargin{i}, true)))
          return;
        endif
      endfor
      tf = true;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {dictionary} {@var{key} =} keyHash (@var{d})
    ## @deftypefnx {dictionary} {@var{key} =} keyHash (@var{d}, @var{base})
    ##
    ## Generate a hash code for a dictionary.
    ##
    ## @code{@var{key} = keyHash (@var{d})} returns a 64-bit hash code of the
    ## dictionary @var{d} as a @qcode{uint64} scalar, from its key and value
    ## types and its entries in the order they were added, so that a
    ## dictionary can be a key of another.  Unlike @code{isequal}, the code,
    ## and with it @code{keyMatch}, depends on the order of the entries and on
    ## the types, as in MATLAB.  A dictionary whose keys or values
    ## @code{keyHash} cannot hash, such as tables, is refused.
    ##
    ## @code{@var{key} = keyHash (@var{d}, @var{base})} also generates a
    ## 64-bit hash code using @var{base} as the offset basis for the FNV-1a
    ## hash algorithm.  @var{base} must be a @qcode{uint64} integer type
    ## scalar.  Use this syntax to cascade @code{keyHash} on multiple objects
    ## for which a single hash code is required.
    ##
    ## Note that unlike MATLAB, this implementation does not use any random
    ## seed.  As a result, @code{keyHash} will always generate the exact same
    ## hash key for any particular input across different workers and Octave
    ## sessions.
    ##
    ## @seealso{keyHash, keyMatch, dictionary.isequal}
    ## @end deftypefn
    function key = keyHash (this, base = [])
      if (! isempty (base))
        if (! (isscalar (base) && isa (base, 'uint64')))
          error ("dictionary.keyHash: BASE must be a UINT64 scalar.");
        endif
        key = __ckeyHash__ ('1x1dictionary', base);
      else
        key = __ckeyHash__ ('1x1dictionary');
      endif
      key = __ckeyHash__ ({this.keyType_, this.valueType_}, key);
      for i = 1:numel (this.keys_)
        key = keyHash (this.keys_{i}, key);
        key = keyHash (this.values_{i}, key);
      endfor
    endfunction

  endmethods

################################################################################
##                    ** Indexing and concatenation **                        ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'subsref'          'subsasgn'         'end'                                ##
## 'horzcat'          'vertcat'          'cat'                                ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Lookup by '()' and '{}', and method calls by '.'
    function varargout = subsref (this, s)
      scope = 'dictionary.subsref';
      switch (s(1).type)
        case '()'
          [out, errmsg] = lookupValues (this, s(1).subs, false);
          if (! isempty (errmsg))
            error ("%s: %s", scope, errmsg);
          endif
          out = {out};
          s = s(2:end);
        case '{}'
          [out, errmsg] = lookupValues (this, s(1).subs, true);
          if (! isempty (errmsg))
            error ("%s: %s", scope, errmsg);
          endif
          s = s(2:end);
        case '.'
          name = s(1).subs;
          if (! (ischar (name) && any (strcmp (name, publicMethods ()))))
            error ("%s: no method named '%s'.", scope, char (name));
          endif
          args = {};
          if (numel (s) > 1 && strcmp (s(2).type, '()'))
            args = s(2).subs;
            s = s(3:end);
          else
            s = s(2:end);
          endif
          ## A method that returns nothing is called for its effect
          if (strcmp (name, 'writedictionary'))
            feval (name, this, args{:});
            varargout = {};
            return;
          endif
          out = cell (1, max (nargout, 1));
          [out{:}] = feval (name, this, args{:});
      endswitch
      if (isempty (s))
        varargout = out;
      else
        varargout = cell (1, max (nargout, 1));
        [varargout{:}] = subsref (out{1}, s);
      endif
    endfunction

    ## Insertion and update by '()' and '{}', deletion by '() = []'
    function this = subsasgn (this, s, val)
      scope = 'dictionary.subsasgn';
      switch (s(1).type)
        case '()'
          if (numel (s(1).subs) != 1)
            error ("%s: index with one array of keys.", scope);
          endif
          K = s(1).subs{1};
          if (numel (s) > 1)
            ## Assigning into a value needs it there, as in MATLAB
            [cur, errmsg] = lookupValues (this, s(1).subs, false);
            if (! isempty (errmsg))
              error ("%s: %s", scope, errmsg);
            endif
            val = subsasgn (cur, s(2:end), val);
          elseif (isa (val, 'double') && isequal (size (val), [0, 0]))
            if (isempty (this.keyType_))
              error ("%s: D is not configured.", scope);
            endif
            [this, errmsg] = removeEntries (this, K);
            if (! isempty (errmsg))
              error ("%s: %s", scope, errmsg);
            endif
            return;
          endif
          [this, errmsg] = addEntries (this, K, val);
        case '{}'
          if (! (isempty (this.keyType_) || strcmp (this.valueType_, 'cell')))
            error ("%s: '{}' indexing needs a dictionary of 'cell' values.", ...
                   scope);
          endif
          if (numel (s(1).subs) != 1)
            error ("%s: index with one array of keys.", scope);
          endif
          K = s(1).subs{1};
          if (! isempty (this.keyType_))
            [K, errmsg] = convert (K, this.keyType_, this.keyEmpty_, 'key');
            if (! isempty (errmsg))
              error ("%s: %s", scope, errmsg);
            endif
          endif
          if (prod (arraySize (asStored (K))) != 1)
            error ("%s: '{}' assignment takes a single key.", scope);
          endif
          if (numel (s) > 1)
            ## The contents of a new key start empty, as in MATLAB
            cur = [];
            if (! isempty (this.keyType_) && isKey (this, K))
              cur = subsref (this, s(1));
            endif
            val = subsasgn (cur, s(2:end), val);
          endif
          [this, errmsg] = addEntries (this, K, {val});
        case '.'
          error ("%s: a dictionary has no properties to assign.", scope);
      endswitch
      if (! isempty (errmsg))
        error ("%s: %s", scope, errmsg);
      endif
    endfunction

    function last = end (this, k, n)
      error ("dictionary.end: 'end' is not supported for a dictionary.");
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {} horzcat (@var{d1}, @var{d2}, @dots{})
    ##
    ## Refuse to concatenate: a dictionary is a scalar object.
    ##
    ## @code{[@var{d1}, @var{d2}]} raises an error, whatever the other
    ## arguments, as in MATLAB.  Add the entries of one dictionary to another
    ## by assignment instead.
    ##
    ## @seealso{dictionary.vertcat, dictionary.cat}
    ## @end deftypefn
    function out = horzcat (varargin)
      error ("dictionary.horzcat: dictionaries cannot be concatenated.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {} vertcat (@var{d1}, @var{d2}, @dots{})
    ##
    ## Refuse to concatenate: a dictionary is a scalar object.
    ##
    ## @code{[@var{d1}; @var{d2}]} raises an error, whatever the other
    ## arguments, as in MATLAB.
    ##
    ## @seealso{dictionary.horzcat, dictionary.cat}
    ## @end deftypefn
    function out = vertcat (varargin)
      error ("dictionary.vertcat: dictionaries cannot be concatenated.");
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {dictionary} {} cat (@var{dim}, @var{d1}, @var{d2}, @dots{})
    ##
    ## Refuse to concatenate: a dictionary is a scalar object.
    ##
    ## @code{cat (@var{dim}, @var{d1}, @var{d2}, @dots{})} raises an error,
    ## whatever the dimension and the other arguments, as in MATLAB.
    ##
    ## @seealso{dictionary.horzcat, dictionary.vertcat}
    ## @end deftypefn
    function out = cat (varargin)
      error ("dictionary.cat: dictionaries cannot be concatenated.");
    endfunction

  endmethods

  methods (Access = private)

    ## The line describing the dictionary, by its types and number of entries
    function t = summaryLine (this)
      if (isempty (this.keyType_))
        t = 'dictionary with unset key and value types';
        return;
      endif
      n = numel (this.keys_);
      if (n == 0)
        count = 'no entries';
      elseif (n == 1)
        count = '1 entry';
      else
        count = sprintf ('%d entries', n);
      endif
      t = sprintf ('dictionary (%s ⟼ %s) with %s', this.keyType_, ...
                   this.valueType_, count);
    endfunction

    ## True when OTHER has the same key type and holds the keys of this
    ## dictionary, in any order, with values equal by isequal or, for
    ## NANEQUAL, by isequaln; the value types may differ.
    function tf = sameEntries (this, other, nanEqual)
      if (isempty (this.keyType_) || isempty (other.keyType_))
        tf = isempty (this.keyType_) && isempty (other.keyType_);
        return;
      endif
      tf = false;
      if (! strcmp (this.keyType_, other.keyType_)
          || numel (this.keys_) != numel (other.keys_))
        return;
      endif
      for i = 1:numel (this.keys_)
        k = this.keys_{i};
        e = locate (other.hashes_, other.order_, other.keys_, k, keyHash (k));
        if (e == 0)
          return;
        endif
        if (nanEqual)
          same = isequaln (this.values_{i}, other.values_{e});
        else
          same = isequal (this.values_{i}, other.values_{e});
        endif
        if (! same)
          return;
        endif
      endfor
      tf = true;
    endfunction

    ## The values of the keys in SUBS, the subscripts of '()' or, for BRACE,
    ## of '{}': one array of the shape of the keys, or the contents of the
    ## cells in a cell array.  FALLBACK, when given as {value}, stands for the
    ## keys not in the dictionary.
    function [out, errmsg] = lookupValues (this, subs, brace, fallback = {})
      out = [];
      errmsg = '';
      if (isempty (this.keyType_))
        errmsg = "D is not configured.";
        return;
      endif
      if (brace && ! strcmp (this.valueType_, 'cell'))
        errmsg = "'{}' indexing needs a dictionary of 'cell' values.";
        return;
      endif
      if (numel (subs) != 1)
        errmsg = "index with one array of keys.";
        return;
      endif
      K = subs{1};
      ## Octave cannot tell d(:) from d(':'), so ':' is the key ":"
      colon = ischar (K) && strcmp (K, ':');
      [K, errmsg] = convert (K, this.keyType_, this.keyEmpty_, 'key');
      if (! isempty (errmsg))
        return;
      endif
      ksz = arraySize (K);
      n = prod (ksz);
      idx = zeros (n, 1);
      for i = 1:n
        k = element (K, i);
        try
          h = keyHash (k);
        catch err
          errmsg = regexprep (err.message, '^keyHash: ', '');
          return;
        end_try_catch
        idx(i) = locate (this.hashes_, this.order_, this.keys_, k, h);
        if (idx(i) == 0 && isempty (fallback))
          if (colon)
            errmsg = "':' is not supported; use 'keys' or 'values'.";
          elseif (n == 1)
            errmsg = "key not found.";
          else
            errmsg = sprintf ("element %d of the keys not found.", i);
          endif
          return;
        endif
      endfor
      if (brace)
        out = cellfun (@(c) c{1}, this.values_(idx)', 'UniformOutput', false);
        return;
      endif
      items = cell (n, 1);
      found = idx > 0;
      items(found) = this.values_(idx(found));
      if (! all (found))
        ## The fallback is read only when a key needs it, as in MATLAB
        [fb, errmsg] = convert (fallback{1}, this.valueType_, ...
                                this.valueEmpty_, 'value');
        if (! isempty (errmsg))
          return;
        endif
        ## Checked once converted, so that text is one string or date
        if (! isequal (arraySize (fb), [1, 1]))
          errmsg = "'FallbackValue' must be a scalar.";
          return;
        endif
        items(! found) = {fb};
      endif
      [out, errmsg] = join (items, this.valueEmpty_, this.valueType_, ...
                            'values');
      if (! isempty (errmsg))
        errmsg = [errmsg "; look up each key separately."];
      elseif (isArray (out) && ! isequal (size (out), ksz))
        out = reshape (out, ksz);
      endif
    endfunction

    ## Remove the entries of the keys K; a key not in the dictionary is
    ## ignored.
    function [this, errmsg] = removeEntries (this, K)
      [K, errmsg] = convert (K, this.keyType_, this.keyEmpty_, 'key');
      if (! isempty (errmsg))
        return;
      endif
      drop = false (numel (this.keys_), 1);
      for i = 1:prod (arraySize (K))
        k = element (K, i);
        try
          h = keyHash (k);
        catch err
          errmsg = regexprep (err.message, '^keyHash: ', '');
          return;
        end_try_catch
        e = locate (this.hashes_, this.order_, this.keys_, k, h);
        if (e > 0)
          drop(e) = true;
        endif
      endfor
      if (! any (drop))
        return;
      endif
      keep = ! drop;
      renumber = zeros (size (keep));
      renumber(keep) = 1:nnz (keep);
      sorted = keep(this.order_);
      this.hashes_ = this.hashes_(sorted);
      this.order_ = renumber(this.order_(sorted));
      this.keys_ = this.keys_(keep);
      this.values_ = this.values_(keep);
    endfunction

    ## Add the entries of the keys K and the values V, configuring the
    ## dictionary from them when it has no types yet.  Without OVERWRITE a key
    ## already present, or repeated in K, keeps its first value.
    function [this, errmsg] = addEntries (this, K, V, overwrite = true)
      if (isempty (this.keyType_))
        K = asStored (K);
        V = asStored (V);
        if (! dictionary.hashable (class (K)))
          errmsg = sprintf ("unsupported key type '%s'.", class (K));
          return;
        endif
        this.keyType_ = class (K);
        this.valueType_ = class (V);
      else
        [K, errmsg] = convert (K, this.keyType_, this.keyEmpty_, 'key');
        if (! isempty (errmsg))
          return;
        endif
        [V, errmsg] = convert (V, this.valueType_, this.valueEmpty_, 'value');
        if (! isempty (errmsg))
          return;
        endif
      endif
      ksz = arraySize (K);
      vsz = arraySize (V);
      if (! (isequal (ksz, vsz) || prod (vsz) == 1))
        errmsg = "KEYS and VALUES must be the same size, or VALUES a scalar.";
        return;
      endif
      errmsg = '';
      ## A dictionary with no entries takes its empty columns, which carry a
      ## time zone or categories, from the first entries added
      if (isempty (this.keys_))
        this.keyEmpty_ = emptyOf (K);
        this.valueEmpty_ = emptyOf (V);
      endif
      ## Work on locals, assigned back once
      keys = this.keys_;
      vals = this.values_;
      hashes = this.hashes_;
      order = this.order_;
      n = numel (keys);
      keys(end+1:end+prod (ksz),1) = {[]};
      vals(end+1:end+prod (ksz),1) = {[]};
      for i = 1:prod (ksz)
        k = element (K, i);
        if (prod (vsz) == 1)
          v = V;
        else
          v = element (V, i);
        endif
        try
          h = keyHash (k);
        catch err
          errmsg = regexprep (err.message, '^keyHash: ', '');
          return;
        end_try_catch
        e = locate (hashes, order, keys, k, h);
        if (e > 0)
          if (overwrite)
            vals{e} = v;
          endif
        else
          n += 1;
          keys{n} = k;
          vals{n} = v;
          p = lookup (hashes, h);
          hashes = [hashes(1:p); h; hashes(p+1:end)];
          order = [order(1:p); n; order(p+1:end)];
        endif
      endfor
      this.keys_ = keys(1:n);
      this.values_ = vals(1:n);
      this.hashes_ = hashes;
      this.order_ = order;
    endfunction

  endmethods

  methods (Static, Hidden)

    ## A configured dictionary with no entries, for configureDictionary,
    ## which validates both type names.
    function d = configured (keyType, valueType)
      d = dictionary ();
      d.keyType_ = keyType;
      d.valueType_ = valueType;
      d.keyEmpty_ = emptyColumn (keyType);
      d.valueEmpty_ = emptyColumn (valueType);
    endfunction

    ## True for a key type keyHash can hash: a built-in type or a class with
    ## a keyHash method.
    function tf = hashable (type)
      tf = (any (strcmp (type, {'double', 'single', 'int8', 'int16', ...
                                'int32', 'int64', 'uint8', 'uint16', ...
                                'uint32', 'uint64', 'logical', 'char', ...
                                'cell', 'struct', 'function_handle'}))
            || any (strcmp (methods (type), 'keyHash')));
    endfunction

    ## A dictionary is a scalar object, so there is no empty one
    function d = empty (varargin)
      error ("dictionary.empty: a dictionary cannot be an empty array.");
    endfunction

  endmethods

endclassdef

## True for an array whose elements are keys or values one by one.  A table
## is one value whatever its size, and a dictionary or a function handle is
## always one; an object of another class is one when it is a single object.
function tf = isArray (x)
  if (istabular (x) || isa (x, 'dictionary') || is_function_handle (x))
    tf = false;
  elseif (isForeign (x))
    tf = builtin ('numel', x) != 1;
  else
    tf = true;
  endif
endfunction

## True for an object of a class other than the arrays of this package, whose
## own size and indexing may mean something else, as for 'timeseries'
function tf = isForeign (x)
  tf = (isobject (x) && ! (isa (x, 'string') || isa (x, 'datetime')
                           || isa (x, 'duration')
                           || isa (x, 'calendarDuration')
                           || isa (x, 'categorical') || isa (x, 'missing')));
endfunction

function sz = arraySize (x)
  if (! isArray (x))
    sz = [1, 1];
  elseif (isForeign (x))
    sz = builtin ('size', x);
  else
    sz = size (x);
  endif
endfunction

function x = element (x, i)
  if (! isArray (x))
    return;
  elseif (isForeign (x))
    x = builtin ('subsref', x, substruct ('()', {i}));
  else
    x = x(i);
  endif
endfunction

## Keys and values as stored: a character array as a string array of its
## rows, a sparse matrix as a full one.
function x = asStored (x)
  if (ischar (x))
    x = string (x);
  elseif (issparse (x))
    x = full (x);
  endif
endfunction

## An empty column of the class of X, keeping its time zone or categories
function e = emptyOf (x)
  if (istabular (x))
    e = x(zeros (0, 1), :);
  elseif (isArray (x) && ! isForeign (x))
    e = x(:);
    e = e(zeros (0, 1));
  else
    e = emptyColumn (class (x));
  endif
endfunction

## An empty column of the class TYPE, or [] for a class that has none
function e = emptyColumn (type)
  switch (type)
    case {'double', 'single', 'int8', 'int16', 'int32', 'int64', ...
          'uint8', 'uint16', 'uint32', 'uint64'}
      e = zeros (0, 1, type);
    case 'logical'
      e = false (0, 1);
    case 'cell'
      e = cell (0, 1);
    case 'struct'
      e = repmat (struct (), 0, 1);
    case {'function_handle', 'dictionary'}
      e = [];
    otherwise
      try
        e = feval (type);
        if (istabular (e))
          e = e(zeros (0, 1), :);
        else
          e = e(zeros (0, 1));
        endif
      catch
        e = [];
      end_try_catch
  endswitch
endfunction

## X converted to the class TYPE, its empty column EMPTY, as a WHAT ('key' or
## 'value') of a dictionary: by the function of the type's name, as in
## MATLAB, except where noted.
function [x, errmsg] = convert (x, type, empty, what)
  errmsg = '';
  if (issparse (x))
    x = full (x);
  endif
  if (strcmp (class (x), type))
    if (isa (x, 'datetime') && strcmp (what, 'key'))
      x = inZone (x, empty);
    endif
    return;
  endif
  bad = sprintf ("unable to use '%s' as %s of a dictionary of '%s' %ss.",
                 class (x), what, type, what);
  switch (type)
    case {'cell', 'struct'}
      ## A cell is never wrapped, as in MATLAB, and an object never becomes
      ## a structure of its private properties, as it does in MATLAB
      errmsg = bad;
      return;
    case 'logical'
      ## A character is true for a nonzero code, as in MATLAB
      if (ischar (x))
        x = double (x);
      endif
    case 'datetime'
      if (ischar (x) || iscellstr (x) || isa (x, 'string'))
        ## Text is read in the time zone of the dictionary, if any
        try
          x = datetime (x);
        catch
          errmsg = bad;
          return;
        end_try_catch
        if (isa (empty, 'datetime') && ! isempty (empty.TimeZone))
          x.TimeZone = empty.TimeZone;
        endif
        return;
      endif
    case 'table'
      ## Named as MATLAB names it, not after this function's variable; text
      ## is refused, as in MATLAB
      if (ischar (x))
        errmsg = bad;
        return;
      endif
      try
        x = table (x, 'VariableNames', {'Var1'});
      catch
        errmsg = bad;
      end_try_catch
      return;
  endswitch
  try
    x = feval (type, x);
  catch
    errmsg = bad;
    return;
  end_try_catch
  if (! strcmp (class (x), type))
    errmsg = bad;
  endif
endfunction

## The datetime X moved to the time zone of the datetime EMPTY, when both
## have one; a date without a zone is a different key from any with one.
function x = inZone (x, empty)
  if (isa (empty, 'datetime') && ! isempty (empty.TimeZone)
      && ! isempty (x.TimeZone))
    x.TimeZone = empty.TimeZone;
  endif
endfunction

## The entry holding the key K, with hash code H, or 0
function e = locate (hashes, order, keys, k, h)
  e = 0;
  p = lookup (hashes, h);
  while (p > 0 && hashes(p) == h)
    if (isequaln (keys{order(p)}, k))
      e = order(p);
      return;
    endif
    p -= 1;
  endwhile
endfunction

## The ITEMS, one key or value per cell, joined into a column of the class
## TYPE, its empty column EMPTY.
function [x, errmsg] = join (items, empty, type, what)
  x = [];
  errmsg = '';
  if (isempty (items) && strcmp (class (empty), type))
    x = empty;
    return;
  endif
  ## Tables of several rows would lose which rows belong to which key
  if (! (istabular (empty) && any (cellfun ('height', items) != 1)))
    try
      x = vertcat (empty, items{:});
      return;
    catch
    end_try_catch
  endif
  if (isscalar (items))
    x = items{1};
    return;
  endif
  errmsg = sprintf ("cannot join %s of type '%s'", what, type);
endfunction

## The text of one key or value X in a dictionary display: the contents of
## a cell in braces, a handle as written, and anything without a text of its
## own by its size and class.
function t = itemText (x)
  if (iscell (x))
    t = ['{', cellText(x{1}), '}'];
  elseif (is_function_handle (x))
    t = handleText (x);
  elseif (isnumeric (x) || islogical (x) || hasText (x))
    t = scalarText (x);
  else
    t = sizeClass (x);
  endif
endfunction

## The text of the contents X of a cell, as a cell array shows them
function t = cellText (x)
  if (ischar (x) && rows (x) == 1)
    t = oneLine (["'", x, "'"]);
  elseif (is_function_handle (x))
    t = handleText (x);
  elseif (isa (x, 'dictionary'))
    ## A dictionary is one object; indexing it would look up a key
    t = ['[', dispstrings(x){1}, ']'];
  elseif (! isempty (x) && isrow (x)
          && (isnumeric (x) || islogical (x) || hasText (x)))
    t = cell (1, columns (x));
    for i = 1:columns (x)
      t{i} = scalarText (x(i));
    endfor
    t = ['[', strjoin(t, ' '), ']'];
  else
    t = sizeClass (x);
  endif
endfunction

## The text of the scalar X: a number as Octave shows it, a string quoted on
## one line, and the other classes of this package by 'dispstrings'
function t = scalarText (x)
  if (isnumeric (x) || islogical (x))
    t = strtrim (disp (x));
  elseif (isa (x, 'string') && ! ismissing (x))
    t = oneLine (['"', char(x), '"']);
  else
    t = dispstrings (x){1};
  endif
endfunction

## The text T on one line, its breaks and tabs shown as a string array
## shows them
function t = oneLine (t)
  t = strrep (t, sprintf ("\r\n"), '↵');
  t = strrep (t, sprintf ("\n"), '↵');
  t = strrep (t, sprintf ("\t"), '→');
endfunction

## True for a class whose values 'dispstrings' writes as text
function tf = hasText (x)
  tf = (isa (x, 'string') || isa (x, 'datetime') || isa (x, 'duration')
        || isa (x, 'calendarDuration') || isa (x, 'categorical')
        || isa (x, 'missing') || isa (x, 'dictionary'));
endfunction

function t = handleText (f)
  t = func2str (f);
  if (t(1) != '@')
    t = ['@', t];
  endif
endfunction

function t = sizeClass (x)
  t = sprintf ('%dx', size (x));
  t = sprintf ('%s %s', t(1:end-1), class (x));
endfunction

## The number of characters in the UTF-8 text T, which has more bytes
function n = textWidth (t)
  n = numel (t) - nnz (t >= 128 & t < 192);
endfunction

## The file type named by TYPE, or told from the extension of FILENAME
function [errmsg, type] = fileKind (type, filename)
  errmsg = '';
  if (isempty (type))
    type = 'auto';
  endif
  if (! ((ischar (type) && rows (type) == 1)
         || (isa (type, 'string') && isscalar (type))))
    errmsg = "'FileType' must be a character vector or a string scalar.";
    return;
  endif
  type = lower (char (type));
  if (! any (strcmp (type, {'auto', 'json', 'ods'})))
    errmsg = sprintf ("invalid 'FileType' value '%s'.", char (type));
    return;
  endif
  if (strcmp (type, 'auto'))
    [~, ~, ext] = fileparts (filename);
    switch (lower (ext))
      case '.json'
        type = 'json';
      case {'.ods', '.fods'}
        type = 'ods';
      otherwise
        errmsg = strcat ("unable to tell the file type from the", ...
                         " extension; set 'FileType'.");
    endswitch
  endif
endfunction

## The JSON tree of the dictionary D: an object of its keys and values as
## __dictionary2json__ writes them, or an error message
function [tree, errmsg] = jsonObject (d)
  errmsg = '';
  tree = struct ('keys', {cell(0, 1)}, 'values', {cell(0, 1)});
  if (! isConfigured (d) || numEntries (d) == 0)
    return;
  endif
  [kt, vt] = types (d);
  kt = char (kt);
  vt = char (vt);
  if (! any (strcmp (vt, {'string', 'double', 'single', 'int8', 'uint8', ...
                          'int16', 'uint16', 'int32', 'uint32', 'int64', ...
                          'uint64', 'logical', 'datetime', 'duration', ...
                          'missing', 'dictionary', 'cell'})))
    errmsg = sprintf ("unable to write values of type '%s' to JSON.", vt);
    return;
  endif
  K = keys (d, 'cell');
  V = values (d, 'cell');
  for i = 1:numel (K)
    [K{i}, errmsg] = jsonKey (K{i}, kt);
    if (! isempty (errmsg))
      return;
    endif
  endfor
  for i = 1:numel (V)
    if (strcmp (vt, 'cell'))
      [V{i}, errmsg] = jsonContent (V{i}{1});
    else
      [V{i}, errmsg] = jsonContent (V{i});
    endif
    if (! isempty (errmsg))
      return;
    endif
  endfor
  tree = struct ('keys', {K}, 'values', {V});
endfunction

## The key K of the key type KT as __dictionary2json__ writes it
function [k, errmsg] = jsonKey (k, kt)
  errmsg = '';
  switch (kt)
    case 'string'
      if (ismissing (k))
        errmsg = "unable to write a missing string key to JSON.";
        return;
      endif
      k = char (k);
    case 'cell'
      k = k{1};
      if (! (ischar (k) && rows (k) <= 1))
        errmsg = "unable to write keys of type 'cell' to JSON.";
        return;
      endif
    case {'double', 'single', 'int8', 'uint8', 'int16', 'uint16', ...
          'int32', 'uint32', 'int64', 'uint64', 'logical'}
      if (iscomplex (k))
        errmsg = "unable to write complex values to JSON.";
      endif
    case {'datetime', 'duration'}
      k = char (k);
    otherwise
      errmsg = sprintf ("unable to write keys of type '%s' to JSON.", kt);
  endswitch
endfunction

## The value X, or the contents of a cell value, as __dictionary2json__
## writes it: text, numbers, logical values, null, arrays and objects
function [v, errmsg] = jsonContent (x)
  errmsg = '';
  v = [];
  null = repmat (struct (), 0, 0);
  if (isa (x, 'dictionary'))
    [v, errmsg] = jsonObject (x);
    return;
  endif
  if (isnumeric (x) && iscomplex (x))
    errmsg = "unable to write complex values to JSON.";
    return;
  endif
  if (ischar (x))
    if (rows (x) > 1)
      errmsg = "unable to write matrices to JSON.";
      return;
    endif
    v = x;
    return;
  endif
  isText = (isa (x, 'string') || isa (x, 'datetime') || isa (x, 'duration')
            || isa (x, 'missing'));
  if (! (isnumeric (x) || islogical (x) || iscell (x) || isText))
    errmsg = sprintf ("unable to write values of type '%s' to JSON.", ...
                      class (x));
    return;
  endif
  if (isempty (x))
    v = cell (0, 1);
    return;
  endif
  if (! isvector (x))
    errmsg = "unable to write matrices to JSON.";
    return;
  endif
  if (isnumeric (x) || islogical (x))
    v = x;
    return;
  endif
  ## Text and cells, one element at a time
  n = numel (x);
  c = cell (n, 1);
  for i = 1:n
    if (iscell (x))
      [c{i}, errmsg] = jsonContent (x{i});
      if (! isempty (errmsg))
        return;
      endif
    elseif (isa (x, 'missing') || ismissing (x(i)))
      c{i} = null;
    else
      c{i} = char (x(i));
    endif
  endfor
  if (n == 1 && ! iscell (x))
    v = c{1};
  else
    v = c;
  endif
endfunction

## Write the dictionary D of tables or timetables to the OpenDocument
## spreadsheet FILENAME, a sheet per entry named by its key
function errmsg = writeOds (d, filename)
  errmsg = '';
  if (! isConfigured (d) || numEntries (d) == 0)
    errmsg = "a dictionary with no entries has no sheets to write.";
    return;
  endif
  [kt, vt] = types (d);
  K = keys (d, 'cell');
  V = values (d, 'cell');
  for i = 1:numel (K)
    if (strcmp (kt, 'cell'))
      K{i} = K{i}{1};
    endif
    if (! ((ischar (K{i}) && rows (K{i}) <= 1)
           || (isa (K{i}, 'string') && ! ismissing (K{i}))))
      errmsg = "the keys must be text to name the sheets of an ODS file.";
      return;
    endif
    K{i} = char (K{i});
    if (strcmp (vt, 'cell'))
      V{i} = V{i}{1};
    endif
    if (! (isa (V{i}, 'table') || isa (V{i}, 'timetable')))
      errmsg = strcat ("only tables and timetables can be written to", ...
                       " ODS files.");
      return;
    endif
  endfor
  ## Fields named after the keys, each key kept as its sheet name
  f = matlab.lang.makeUniqueStrings (matlab.lang.makeValidName (K));
  s = struct ();
  for i = 1:numel (K)
    t = V{i};
    if (! strcmp (f{i}, K{i}))
      if (! any (strcmp (properties (t.Properties.CustomProperties), ...
                         'ActualSheetName')))
        t = addprop (t, 'ActualSheetName', 'table');
      endif
      t.Properties.CustomProperties.ActualSheetName = K{i};
    endif
    s.(f{i}) = t;
  endfor
  struct2ods (filename, s);
endfunction

## The methods '.' may call
function names = publicMethods ()
  names = {'types', 'isConfigured', 'numEntries', 'numel', 'length', ...
           'size', 'ndims', 'isempty', 'keys', 'values', 'isKey', ...
           'insert', 'lookup', 'remove', 'entries', 'isequal', 'isequaln', ...
           'keyHash', 'dispstrings', 'writedictionary', 'horzcat', ...
           'vertcat', 'cat'};
endfunction

## The one of OPTIONS that FORMAT names, in any case or by its first letters
function [format, errmsg] = formatName (format, options)
  errmsg = '';
  if (! ((ischar (format) && rows (format) <= 1)
         || (isa (format, 'string') && isscalar (format))))
    errmsg = "FORMAT must be a character vector or a string scalar.";
    return;
  endif
  format = char (format);
  match = ! isempty (format) & strncmpi (format, options, numel (format));
  if (nnz (match) != 1)
    errmsg = sprintf ("invalid format '%s'.", format);
    return;
  endif
  format = options{match};
endfunction

## 'pkg test' reads the BISTs from the file it is testing, so a class
## whose suite lives in inst/tests/ reads as carrying none.  This block
## answers for it and checks nothing itself.
%!test
%! ## The suite for this class is inst/tests/dictionary.m-tst.
