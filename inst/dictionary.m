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
  ## @seealso{configureDictionary, isdictionary, keyHash, keyMatch}
  ## @end deftp

  properties (Access = private)
    ## The key and value types, '' while unconfigured
    keyType_ = ''
    valueType_ = ''
    ## The hash code of every key, one per entry
    hashes_ = zeros (0, 1, 'uint64')
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
    ## @deftypefn {dictionary} {@var{d} =} dictionary ()
    ##
    ## Create a @code{dictionary} object.
    ##
    ## @code{@var{d} = dictionary ()} returns an unconfigured dictionary, with
    ## no entries and no key and value types.
    ##
    ## @seealso{configureDictionary, dictionary.isConfigured}
    ## @end deftypefn
    function this = dictionary (varargin)
      if (nargin == 0)
        return;
      endif
      if (mod (nargin, 2) != 0)
        error ("dictionary: each key must be followed by a value.");
      endif
      error ("dictionary: entries are not implemented yet.");
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
      n = numel (this.hashes_);
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
##                         ** Concatenation **                                ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'horzcat'          'vertcat'          'cat'                                ##
##                                                                            ##
################################################################################

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

  methods (Static, Hidden)

    ## A configured dictionary with no entries, for configureDictionary,
    ## which validates both type names.
    function d = configured (keyType, valueType)
      d = dictionary ();
      d.keyType_ = keyType;
      d.valueType_ = valueType;
    endfunction

  endmethods

endclassdef

## 'pkg test' reads the BISTs from the file it is testing, so a class
## whose suite lives in inst/tests/ reads as carrying none.  This block
## answers for it and checks nothing itself.
%!test
%! ## The suite for this class is inst/tests/dictionary.m-tst.
