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
  ## or @code{@var{d}@{@var{key}@}(2) = 5}.  @code{end} and @qcode{':'} are
  ## not supported.
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

################################################################################
##                  ** Create and describe 'dictionary' **                    ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'dictionary'       'types'            'isConfigured'     'numEntries'      ##
## 'numel'            'length'           'size'             'ndims'           ##
## 'isempty'                                                                  ##
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
    ## its own, each cell one key.  An object of any class that is not an
    ## array type is one value, a @code{table} or @code{timetable} of any size
    ## included, where MATLAB accepts only a 1-by-1 one.
    ##
    ## @code{@var{d} = dictionary (@var{k1}, @var{v1}, @dots{}, @var{kN},
    ## @var{vN})} adds each pair of keys and values in turn, the first pair
    ## setting the key and value types and later pairs converted to them.
    ## Keys and values are converted as follows; any other class is refused.
    ##
    ## @multitable @columnfractions 0.3 0.7
    ## @headitem Type @tab Converted from
    ## @item @qcode{'string'} @tab character arrays (by rows), cell arrays of
    ## character vectors, numbers and logical values (as text),
    ## @code{datetime}, @code{missing}
    ## @item numeric @tab other numeric and logical values (rounded and
    ## saturated for integer types), character arrays (their character codes),
    ## @code{string} (as by @code{str2double}), @code{missing} (@code{NaN})
    ## @item @qcode{'logical'} @tab numbers other than @code{NaN}
    ## @item @qcode{'datetime'} @tab dates as text, read in the time zone of
    ## the dictionary; a @code{datetime} is moved to that zone
    ## @item @qcode{'categorical'} @tab @code{string}
    ## @end multitable
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
    ##
    ## @var{d} must be configured.
    ##
    ## @seealso{dictionary.values, dictionary.isKey, dictionary.numEntries}
    ## @end deftypefn
    function k = keys (this, format = 'uniform')
      scope = 'dictionary.keys';
      [format, errmsg] = formatName (format);
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
    ##
    ## @var{d} must be configured.
    ##
    ## @seealso{dictionary.keys, dictionary.numEntries}
    ## @end deftypefn
    function v = values (this, format = 'uniform')
      scope = 'dictionary.values';
      [format, errmsg] = formatName (format);
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
            val = subsasgn (subsref (this, s(1)), s(2:end), val);
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
            val = subsasgn (subsref (this, s(1)), s(2:end), val);
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

    ## The values of the keys in SUBS, the subscripts of '()' or, for BRACE,
    ## of '{}': one array of the shape of the keys, or the contents of the
    ## cells in a cell array.
    function [out, errmsg] = lookupValues (this, subs, brace)
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
      if (ischar (K) && strcmp (K, ':'))
        errmsg = "':' is not supported; use 'keys' or 'values'.";
        return;
      endif
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
        if (idx(i) == 0)
          if (n == 1)
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
      [out, errmsg] = join (this.values_(idx), this.valueEmpty_, ...
                            this.valueType_, 'values');
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
    ## dictionary from them when it has no types yet.
    function [this, errmsg] = addEntries (this, K, V)
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
          vals{e} = v;
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

  endmethods

endclassdef

## True for an array whose elements are keys or values one by one; any other
## object is a single key or value.
function tf = isArray (x)
  tf = (isnumeric (x) || islogical (x) || ischar (x) || iscell (x)
        || isstruct (x) || isa (x, 'string') || isa (x, 'datetime')
        || isa (x, 'duration') || isa (x, 'calendarDuration')
        || isa (x, 'categorical') || isa (x, 'missing'));
endfunction

function sz = arraySize (x)
  if (isArray (x))
    sz = size (x);
  else
    sz = [1, 1];
  endif
endfunction

function x = element (x, i)
  if (isArray (x))
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
  elseif (isArray (x))
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
## 'value') of a dictionary.
function [x, errmsg] = convert (x, type, empty, what)
  errmsg = '';
  if (issparse (x))
    x = full (x);
  endif
  if (strcmp (class (x), type))
    if (isa (x, 'datetime'))
      x = inZone (x, empty);
    endif
    return;
  endif
  switch (type)
    case 'string'
      if (ischar (x) || iscellstr (x) || isnumeric (x) || islogical (x)
          || isa (x, 'datetime') || isa (x, 'missing'))
        x = string (x);
        return;
      endif
    case {'double', 'single', 'int8', 'int16', 'int32', 'int64', ...
          'uint8', 'uint16', 'uint32', 'uint64'}
      if (isnumeric (x) || islogical (x))
        x = cast (x, type);
        return;
      elseif (ischar (x))
        x = cast (double (x), type);
        return;
      elseif (isa (x, 'string'))
        x = cast (reshape (str2double (cellstr (x)), size (x)), type);
        return;
      elseif (isa (x, 'missing'))
        x = cast (NaN (size (x)), type);
        return;
      endif
    case 'logical'
      if (isnumeric (x) && ! any (isnan (x(:))))
        x = logical (x);
        return;
      endif
    case 'datetime'
      if (ischar (x) || iscellstr (x) || isa (x, 'string'))
        x = inZone (datetime (x), empty);
        return;
      endif
    case 'categorical'
      if (isa (x, 'string'))
        x = categorical (x);
        return;
      endif
  endswitch
  errmsg = sprintf ("unable to use '%s' as %s of a dictionary of '%s' %ss.",
                    class (x), what, type, what);
endfunction

## The datetime X in the time zone of the datetime EMPTY, when both have one;
## a date without a zone is read in that of EMPTY.
function x = inZone (x, empty)
  if (isa (empty, 'datetime') && ! isempty (empty.TimeZone))
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

## The methods '.' may call
function names = publicMethods ()
  names = {'types', 'isConfigured', 'numEntries', 'numel', 'length', ...
           'size', 'ndims', 'isempty', 'keys', 'values', 'isKey', ...
           'horzcat', 'vertcat', 'cat'};
endfunction

## FORMAT, a format option of 'keys' and 'values', in lower case
function [format, errmsg] = formatName (format)
  errmsg = '';
  if (! ((ischar (format) && rows (format) <= 1)
         || (isa (format, 'string') && isscalar (format))))
    errmsg = "FORMAT must be a character vector or a string scalar.";
    return;
  endif
  format = lower (char (format));
  if (! any (strcmp (format, {'uniform', 'cell'})))
    errmsg = sprintf ("invalid format '%s'.", format);
  endif
endfunction

## 'pkg test' reads the BISTs from the file it is testing, so a class
## whose suite lives in inst/tests/ reads as carrying none.  This block
## answers for it and checks nothing itself.
%!test
%! ## The suite for this class is inst/tests/dictionary.m-tst.
