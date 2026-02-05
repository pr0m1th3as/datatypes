## Copyright (C) 2024-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

classdef string
  ## -*- texinfo -*-
  ## @deftp {datatypes} string
  ##
  ## Array representing sequences of characters.
  ##
  ## A string array is an array, where each element stores a sequence of
  ## characters of arbitrary length.
  ##
  ## A string array can also have missing elements, which differ from a sequence
  ## of characters of zero length (the equivalent of an empty character vector).
  ##
  ## To enable existing functions to handle string arrays as if they were cell
  ## arrays of character vectors or character arrays, use the
  ## @code{convertCharsToStrings} function inside your code.  To enable
  ## functions working with string arrays to accept cell arrays of character
  ## vectors or character vectors as if they were string arrays or string
  ## scalars, use the @code{convertStringsToChars} function.
  ##
  ## @seealso{convertCharsToStrings, convertStringsToChars}
  ## @end deftp

  properties (SetAccess = private, Hidden)
    ## Text data
    strs = {''}
    ## Missing values flag
    isMissing = false
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ('%s =\n', in_name);
      endif
      __disp__ (this, 'string', in_name);
    endfunction

    ## Custom display
    function disp (this)
      __disp__ (this, 'string');
    endfunction

  endmethods

################################################################################
##                    ** Create String and Convert Type **                    ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'string'           'dispstrings'      'cellstr'          'cell'            ##
## 'char'             'double'                                                ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{str} =} string ()
    ## @deftypefnx {string} {@var{str} =} string (@var{in})
    ## @deftypefnx {string} {@var{str} =} string (@var{calendarDuration}, @qcode{'Format'}, @var{FMT})
    ## @deftypefnx {string} {@var{str} =} string (@var{duration}, @qcode{'Format'}, @var{FMT})
    ## @deftypefnx {string} {@var{str} =} string (@{@var{in1}, @dots{}, @var{inN}@})
    ##
    ## Create a new string array.
    ##
    ## @code{@var{str} = string ()} creates a scalar string array, whose element
    ## contains an empty character vector.
    ##
    ## @code{@var{str} = string (@var{in})} creates a string array of the same
    ## size as @var{in}, which is converted to string according to the following
    ## options:
    ##
    ## @itemize
    ## @item character arrays and cell arrays of character vectors are converted
    ## via the core @code{cellstr} function.
    ## @item numeric arrays are converted via the code @code{num2str} function.
    ## @item logical arrays are converted to either @qcode{false} or
    ## @qcode{true} character sequences.
    ## @item categorical arrays are converted via their @code{cellstr} method.
    ## @item datetime arrays are converted via their @code{dispstrings} method.
    ## @item calendarDuration arrays and duration arrays are converted via their
    ## respective @code{cellstr} methods, in which case an extra pair argument
    ## is supported to allow parsing to the respective method the appropriate
    ## display format.  See @qcode{calendarDuration} and @qcode{duration} for
    ## valid formats parsed through @var{FMT} to each class method.  Extra input
    ## arguments to the @code{string} constructor except for this case are
    ## ignored.
    ## @item missing arrays are converted to a string array of missing elements.
    ## @end itemize
    ##
    ## @code{@var{str} = string (@{@var{in1}, @dots{}, @var{inN}@})} creates a
    ## string array from a cell array, which may contain any combination of the
    ## aforementioned data types, provided that each cell element is compatible
    ## to a string scalar.  When using this syntax, calendarDuration arrays and
    ## duration arrays are converted via their @code{dispstrings} method, hence
    ## no extra format argument is meaningful.
    ##
    ## @seealso{calendarDuration, categorical, datetime, duration, missing}
    ## @end deftypefn
    function this = string (in, varargin)

      ## Return empty string
      if (nargin == 0)
        return;
      endif

      ## Handle string input first
      if (isa (in, "string"))
        this.strs = in.strs;
        this.isMissing = in.isMissing;
        return
      endif

      ## Handle all other valid cases
      if (isa (in, "categorical"))
        this.strs = cellstr (in);
        this.isMissing = isundefined (in);

      elseif (ischar (in))
        this.strs = cellstr (in);
        this.isMissing = false (size (this.strs));

      elseif (iscellstr (in))
        this.strs = in;
        this.isMissing = false (size (this.strs));

      elseif (iscell (in))
        sz = size (in);
        this.strs = repmat ({''}, sz);
        this.isMissing = false (sz);
        fcn = @(x) isscalar (x) | isempty (x) | (ischar (x) & isvector (x));
        all_scalar = all (cellfun (fcn, in));
        if (! all_scalar)
          error (strcat ("string: cell array must explicitly contain", ...
                         " scalar elements or character vectors."));
        endif
        is_numeric = cellfun (@isnumeric, in);
        is_logical = cellfun (@islogical, in);
        is_cellstr = cellfun (@iscellstr, in);
        is_charvec = cellfun (@ischar, in);
        is_strings = cellfun (@(x) isa (x, "string"), in);
        is_missing = cellfun (@(x) isa (x, "missing"), in);
        is_datetime = cellfun (@(x) isa (x, "datetime"), in);
        class_types = {"duration", "calendarDuration"};
        is_duration = cellfun (@(x) any (isa (x, class_types)), in);
        ## Check for unsupported classes
        all_support = all (is_numeric | is_logical | is_cellstr | is_charvec | ...
                           is_strings | is_missing | is_datetime | is_duration);
        if (! all_support)
          error ("string: cell array contains unsupported types.");
        endif
        ## Handle numeric elements first (including empty cells [])
        if (any (is_numeric(:)))
          tmpval = in(is_numeric);
          sz = size (tmpval);
          strs = repmat ({''}, sz);
          tf_m = false (sz);
          is_empty = cellfun (@isempty, tmpval);
          tf_m(is_empty) = true;
          tmpval = cell2mat (tmpval(! tf_m));
          strs(! tf_m) = arrayfun (@num2str, tmpval, "UniformOutput", false);
          is_nan = strcmp (strs, 'NaN');
          strs(is_nan) = {''};
          tf_m(is_nan) = true;
          this.strs(is_numeric) = strs;
          this.isMissing(is_numeric) = tf_m;
        endif
        ## Handle logical (no missing values here)
        if (any (is_logical(:)))
          tmpval = in(is_logical);
          sz = size (tmpval);
          strs = repmat ({'false'}, sz);
          tf_m = false (sz);
          is_true = logical (cell2mat (tmpval));
          strs(is_true) = {'true'};
          this.strs(is_logical) = strs;
          this.isMissing(is_logical) = tf_m;
        endif
        ## Handle cell arrays of character vectors
        if (any (is_cellstr(:)))
          this.strs(is_cellstr) = in{is_cellstr};
        endif
        ## Handle character vectors (including empty 0x0 char vectors '')
        if (any (is_charvec(:)))
          this.strs(is_charvec) = in(is_charvec);
        endif
        ## Handle strings
        if (any (is_strings(:)))
          this.strs(is_strings) = [in{is_strings}].strs;
          this.isMissing(is_strings) = [in{is_strings}].isMissing;
        endif
        ## Handle missing objects
        if (any (is_missing(:)))
          this.isMissing(is_missing) = true;
        endif
        ## Handle datetime objects
        if (any (is_datetime(:)))
          tmpval = in(is_datetime);
          sz = size (tmpval);
          strs = repmat ({''}, sz);
          tf_m = false (sz);
          is_nat = cellfun (@isnat, tmpval);
          tf_m(is_nat) = true;
          strs(! is_nat) = cellfun (@dispstrings, tmpval(! is_nat));
          this.strs(is_datetime) = strs;
          this.isMissing(is_datetime) = tf_m;
        endif
        ## Handle duration and calendarDuration objects
        if (any (is_duration(:)))
          tmpval = in(is_duration);
          sz = size (tmpval);
          strs = repmat ({''}, sz);
          tf_m = false (sz);
          is_nan = cellfun (@isnan, tmpval);
          tf_m(is_nan) = true;
          strs(! is_nan) = cellfun (@dispstrings, tmpval(! is_nan));
          this.strs(is_duration) = strs;
          this.isMissing(is_duration) = tf_m;
        endif

      elseif (isnumeric (in))
        is_nan = isnan (in);
        this.isMissing = is_nan;
        if (any (is_nan(:)))
          strs = repmat ({''}, size (in));
          strs(! is_nan) = arrayfun (@(x) {num2str(x)}, in(! is_nan));
          this.strs = strs;
        else
          this.strs = arrayfun (@(x) {num2str(x)}, in);
        endif

      elseif (islogical (in))
        sz = size (in);
        strs = repmat ({'false'}, sz);
        strs(in) = {'true'};
        this.strs = strs;
        this.isMissing = false (sz);

      elseif (isa (in, 'datetime'))
        is_nat = isnat (in);
        this.isMissing = is_nat;
        if (any (is_nat(:)))
          strs = repmat ({''}, size (in));
          strs(! is_nat) = dispstrings (in(! is_nat));
          this.strs = strs;
        else
          this.strs = dispstrings (in);
        endif

      elseif (isa (in, 'duration') || isa (in, 'calendarDuration'))
        is_nan = isnan (in);
        this.isMissing = is_nan;
        if (any (is_nan(:)))
          strs = repmat ({''}, size (in));
          strs(! is_nan) = cellstr (in(! is_nan), varargin{:});
          this.strs = strs;
        else
          this.strs = cellstr (in, varargin{:});
        endif

      elseif (isa (in, 'missing'))
        this.strs = repmat ({''}, size (in));
        this.isMissing = true (size (in));

      else
        error ("string: unsupported input type: '%s'", class (in));
      endif

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{cstr} =} dispstrings (@var{str})
    ##
    ## Get display formatted strings for each element of a string object.
    ##
    ## @code{@var{cstr} = dispstrings (@var{str})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## string object, @var{str}.  These character vectors will either be the
    ## string contents of the element, enclosed in @qcode{"..."}, and with CR/LF
    ## characters replaced with @qcode{'\r'} and @qcode{'\n'} escape sequences,
    ## or @qcode{<missing>} for missing values.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      cstr = strcat ({'"'}, this.strs, {'"'});
      cstr = strrep (cstr, sprintf ("\r"), '\r');
      cstr = strrep (cstr, sprintf ("\n"), '\n');
      cstr(this.isMissing) = "<missing>";
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{cstr} =} cellstr (@var{str})
    ##
    ## Convert string array to a cell array of character vectors.
    ##
    ## @code{@var{cstr} = cellstr (@var{str})} returns a cell array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## string @var{str}.  Both empty strings and missing values are returned as
    ## empty character vectors.
    ##
    ## @end deftypefn
    function cstr = cellstr (this)
      cstr = this.strs;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{c_arr} =} cell (@var{str})
    ##
    ## Convert string array to a cell array.
    ##
    ## @code{@var{c_arr} = cell (@var{str})} returns a cell array, @var{c_arr},
    ## which has the same size as the input string @var{str}.  All strings are
    ## converted to character vectors.  Empty strings are converted to
    ## @qcode{''} empty character vectors, while missing values are returned as
    ## @qcode{[]} empty numeric vectors.
    ##
    ## @end deftypefn
    function c_arr = cell (this)
      c_arr = this.strs;
      c_arr(this.isMissing) = [];
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{c_mat} =} char (@var{str})
    ##
    ## Convert string array to a character matrix.
    ##
    ## @code{@var{c_mat} = char (@var{str})} returns a character matrix,
    ## @var{c_mat}, which contains as many rows as the elements of the string.
    ## Both empty strings and missing values are returned as empty character
    ## vectors.
    ##
    ## @end deftypefn
    function c_mat = char (this)
      c_mat = char (this.strs);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{X} =} double (@var{str})
    ##
    ## Convert string array to a double array.
    ##
    ## @code{@var{X} = char (@var{str})} returns a double array, @var{X}, which
    ## has the same size as the input string @var{str}.  All elements in
    ## @var{str} that represent real or complex numbers are converted to
    ## equivalent double values.  Otherwise, @qcode{NaN} is returned.
    ##
    ## @end deftypefn
    function out = double (this)
      out = NaN (size (this));
      fcn = @(x) str2num (char (x));
      c_out = cellfun (fcn, this.strs, "UniformOutput", false);
      t_num = ! cellfun (@isempty, c_out);
      out(t_num) = cell2mat (c_out(t_num));
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'size'             'ndims'            'numel'            'strlength'       ##
## 'length'           'keyHash'                                               ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{sz} =} size (@var{str})
    ## @deftypefnx {string} {@var{dim_sz} =} size (@var{str}, @var{dim})
    ## @deftypefnx {string} {@var{dim_sz} =} size (@var{str}, @var{d1}, @var{d2}, @dots{})
    ## @deftypefnx {string} {[@var{rows}, @var{columns}, @dots{}, @var{dim_n_sz}] =} size (@dots{})
    ##
    ## Return the size of a string array.
    ##
    ## @code{@var{sz} = size (@var{str})} returns a row vector with the size
    ## (number of elements) of each dimension for the string array @var{str}.
    ##
    ## @code{@var{dim_sz} = size (@var{str}, @var{dim})} returns the size of
    ## the corresponding dimension specified in @var{dim}.  If @var{dim} is a
    ## vector, then @var{dim_sz} is a vector of the same length and with each
    ## element corresponding to a specified dimension.  Multiple dimensions may
    ## also be specified as separate arguments.
    ##
    ## With a single output argument, @code{size} returns a row vector.  When
    ## called with multiple output arguments, @code{size} returns the size of
    ## dimension N in the Nth argument.
    ##
    ## @end deftypefn
    function varargout = size (this, varargin)
      if (! isempty (varargin))
        sz = size (this.strs, varargin{:});
      else
        sz = size (this.strs);
      endif
      if (nargout == 0 || nargout == 1)
        varargout{1} = sz;
      elseif (numel (sz) != nargout)
        error (["string.size: nargout > 1 but does not", ...
                " match number of requested dimensions."]);
      else
        for i = 1:nargout
          varargout{i} = sz(i);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{out} =} ndims (@var{str})
    ##
    ## Number of dimensions in a string array.
    ##
    ## @code{@var{out} = ndims (@var{str})} returns the number of dimensions of
    ## the string array @var{D}.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = ndims (this.strs);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{out} =} numel (@var{str})
    ##
    ## Total number of elements in a string array.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = numel (this.strs);
      #out = 1
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{out} =} strlength (@var{str})
    ##
    ## Length of text in string arrays.
    ##
    ## @end deftypefn
    function out = strlength (this)
      out = NaN (size(this));
      fcn = @(x) __unicode_length__ (x);
      TF = ! this.isMissing;
      out(TF) = cell2mat (cellfun (fcn, this.strs(TF), "UniformOutput", false));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{N} =} length (@var{str})
    ##
    ## Length of a string vector.
    ##
    ## @code{@var{N} = length (@var{str})} returns the size of the longest
    ## dimension of the string array @var{str}, unless any of its dimensions
    ## has zero length, in which case @code{length (@var{D})} returns 0.
    ##
    ## @end deftypefn
    function N = length (this)
      if (isempty (this.strs))
        N = 0;
      else
        N = max (size (this.strs));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{hey} =} keyHash (@var{str})
    ##
    ## Generate a hash code for string array.
    ##
    ## @code{@var{h} = keyHash (@var{str})} generates a @qcode{uint64} scalar
    ## that represents the input array @var{str}.  @code{keyHash} utilizes the
    ## 64-bit FMV-1a variant of the Fowler-Noll-Vo non-cryptographic hash
    ## function.
    ##
    ## @code{@var{h} = keyHash (@var{str}), @var{base}} also generates a 64-bit
    ## hash code using @var{base} as the offset basis for the FNV-1a hash
    ## algorithm.  @var{base} must be a @qcode{uint64} integer type scalar.  Use
    ## this syntax to cascade @code{keyHash} on multiple objects for which a
    ## single hash code is required.
    ##
    ## Note that unlike MATLAB, this implementation does no use any random seed.
    ## As a result, @code{keyHash} will always generate the exact same hash key
    ## for any particular input across different workers and Octave sessions.
    ##
    ## @end deftypefn
    function key = keyHash (this, base = [])
      ## Initialize string with size and class name
      size_str = sprintf ('%dx', size (this.strs))(1:end-1);
      init_str = [size_str 'string'];
      if (base)
        if (! (isscalar (base) && isa (base, 'uint64')))
          error ("string.keyHash: BASE must be a UINT64 scalar.");
        endif
        key = __ckeyHash__(init_str, base);
      else
        key = __ckeyHash__(init_str);
      endif
      if (! isempty (this.strs))
        strs = [this.strs{:}];
        key = __ckeyHash__(strs, key);
        key = __nkeyHash__(this.isMissing(:), key);
      endif
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'contains'         'endsWith'         'matches'          'startsWith'      ##
## 'iscolumn'         'isempty'          'ismatrix'         'ismember'        ##
## 'ismissing'        'isrow'            'isscalar'         'issorted'        ##
## 'isstring'         'isvector'                                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} iscolumn (@var{str})
    ##
    ## Test if string array is a column vector.
    ##
    ## @var{TF} is @qcode{true}, if string array @var{str} is a column vector.
    ##
    ## @end deftypefn
    function TF = iscolumn (this)
      TF = iscolumn (this.isMissing);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} isempty (@var{str})
    ##
    ## Test if string array is empty.
    ##
    ## @var{TF} is @qcode{true}, if string array @var{str} is empty.
    ##
    ## @end deftypefn
    function TF = isempty(this)
      TF = isempty (this.isMissing);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} ismatrix (@var{str})
    ##
    ## Test if string array is a matrix.
    ##
    ## @var{TF} is @qcode{true}, if string array @var{str} is a matrix.
    ##
    ## @end deftypefn
    function TF = ismatrix (this)
      TF = ismatrix (this.isMissing);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} ismember (@var{A}, @var{B})
    ##
    ## Test for string elements in a set.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B})} returns a logical array
    ## @var{TF} of the same size as @var{A} containing @qcode{true} for each
    ## corresponding element of @var{A} that is in @var{B} and @qcode{false}
    ## otherwise.  Similarly to @qcode{NaN} values, @qcode{<missing>} elements
    ## are not equal with each other and always return @qcode{false}.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B}, @qcode{'rows'})} only
    ## applies to string matrices with the same number of columns, in which
    ## case the logical vector @var{TF} contains @qcode{true} for each row of
    ## @var{A} that is also a row in @var{B}.  @var{TF} has the same number of
    ## rows as @var{A}.
    ##
    ## @code{[@var{TF}, @var{index}] = ismember (@var{A}, @var{B})} also returns
    ## an index array of the same size as @var{A} containing the lowest index in
    ## @var{B} for each element of @var{A} that is a member of @var{B} and 0
    ## otherwise.  If the @qcode{'rows'} optional argument is used, then the
    ## returning index is a column vector with the same rows as @var{A} and it
    ## contains the lowest index in @var{B} for each row of @var{A} that is a
    ## member of @var{B} and 0 otherwise.
    ##
    ## @end deftypefn
    function [TF, index] = ismember (A, B, varargin)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      endif
      if (! isstring (A))
        error ("string.ismember: first input argument must be text.");
      endif
      if (! isstring (B))
        error ("string.ismember: second input argument must be text.");
      endif
      [TF, index] = ismember (A.strs, B.strs, varargin{:});
      TF(A.isMissing) = false;
      index(A.isMissing) = 0;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{TF} =} ismissing (@var{str})
    ## @deftypefnx {string} {@var{TF} =} ismissing (@var{str}, @var{indicator})
    ##
    ## Find missing elements in string array.
    ##
    ## @code{@var{TF} = ismissing (@var{str})} returns a logical array,
    ## @var{TF}, with any @qcode{true} values corresponding to missing elements
    ## in the input string array @var{str}.
    ##
    ## @code{@var{TF} = ismissing (@var{str}, @var{indicator})} also returns a
    ## logical array, @var{TF}, with any @qcode{true} values corresponding to
    ## elements in the input string array @var{str}, which are lexicographically
    ## equal to the values in @var{indicator}.
    ##
    ## @var{indicator} must be either a character vector or a string vector
    ## or a cell vector of character vectors.
    ##
    ## The output array @var{FT} has the same size as the input array @var{str}.
    ##
    ## @end deftypefn
    function TF = ismissing (this, varargin)
      if (nargin > 2)
        error ("string.ismissing: too many input arguments.");
      endif
      if (! isempty (varargin))
        indicator = varargin{1};
        TF = false (size (this));
        if (isvector (indicator))
          if (ischar (indicator))
            TF(this == indicator) = true;
          elseif (isstring (indicator))
            for i = 1:numel (indicator)
              str = indicator.strs(i);
              TF(this == str) = true;
            endfor
          elseif (iscellstr (indicator))
            for i = 1:numel (indicator)
              str = indicator(i);
              TF(this == str) = true;
            endfor
          else
            error ("string.ismissing: INDICATOR must be a text array.");
          endif
        else
          error ("string.ismissing: INDICATOR must be a vector.");
        endif
      else
        TF = this.isMissing;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} isrow (@var{str})
    ##
    ## Test if string array is a row vector.
    ##
    ## @var{TF} is @qcode{true}, if string array @var{str} is a row vector.
    ##
    ## @end deftypefn
    function TF = isrow (this)
      TF = isrow (this.isMissing);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} isscalar (@var{str})
    ##
    ## Test if string array is a scalar.
    ##
    ## @var{TF} is @qcode{true}, if string array @var{str} is a scalar.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = isscalar (this.isMissing);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} isstring (@var{str})
    ##
    ## Test for string array.
    ##
    ## @var{TF} is @qcode{true} for @code{string} inputs.
    ##
    ## @end deftypefn
    function TF = isstring (this)
      TF = true;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} isvector (@var{str})
    ##
    ## Test if string array is a vector.
    ##
    ## @var{TF} is @qcode{true}, if string array @var{str} is a vector.
    ##
    ## @end deftypefn
    function TF = isvector (this)
      TF = isvector (this.isMissing);
    endfunction

  endmethods

################################################################################
##                        ** Relational Operations **                         ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'eq'               'ge'               'gt'               'le'              ##
## 'lt'               'ne'               'strcmp'           'strcmpi'         ##
## 'strncmp'          'strncmpi'                                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Test for equality.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically equal to
    ## @var{B}.  If one input is a string array, the other input can be a string
    ## array, a character vector, or a cell array of character vectors.  This is
    ## equivalent to the @code{strcmp} function.
    ##
    ## @end deftypefn
    function TF = eq (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.eq: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A))
        A = repmat (A, size (B));
      elseif (isscalar (B))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.eq: inconsistent dimensions.");
      endif
      TF = strcmp (A.strs, B.strs);
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Test for greater than or equal to.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically greater
    ## than or equal to @var{B}.  If one input is a string array, the other
    ## input can be a string array, a character vector, or a cell array of
    ## character vectors.
    ##
    ## @end deftypefn
    function TF = ge (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.ge: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A) && ! isscalar (B))
        A = repmat (A, size (B));
      elseif (isscalar (B) && ! isscalar (A))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.ge: inconsistent dimensions.");
      endif
      out = sign_strings (A.strs, B.strs);
      TF = false (size (out));
      TF(out >= 0) = true;
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Test for greater than.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically greater
    ## than @var{B}.  If one input is a string array, the other input can be a
    ## string array, a character vector, or a cell array of character vectors.
    ##
    ## @end deftypefn
    function TF = gt (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.gt: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A) && ! isscalar (B))
        A = repmat (A, size (B));
      elseif (isscalar (B) && ! isscalar (A))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.gt: inconsistent dimensions.");
      endif
      out = sign_strings (A.strs, B.strs);
      TF = false (size (out));
      TF(out > 0) = true;
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Test for less than or equal to.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically less
    ## than or equal to @var{B}.  If one input is a string array, the other
    ## input can be a string array, a character vector, or a cell array of
    ## character vectors.
    ##
    ## @end deftypefn
    function TF = le (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.le: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A) && ! isscalar (B))
        A = repmat (A, size (B));
      elseif (isscalar (B) && ! isscalar (A))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.le: inconsistent dimensions.");
      endif
      out = sign_strings (A.strs, B.strs);
      TF = false (size (out));
      TF(out <= 0) = true;
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Test for less than.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically less
    ## than @var{B}.  If one input is a string array, the other input can be a
    ## string array, a character vector, or a cell array of character vectors.
    ##
    ## @end deftypefn
    function TF = lt (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.lt: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A) && ! isscalar (B))
        A = repmat (A, size (B));
      elseif (isscalar (B) && ! isscalar (A))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.lt: inconsistent dimensions.");
      endif
      out = sign_strings (A.strs, B.strs);
      TF = false (size (out));
      TF(out < 0) = true;
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} ne (@var{A}, @var{B})
    ##
    ## Test for inequality.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically not
    ## equal to @var{B}.  If one input is a string array, the other input can be
    ## a string array, a character vector, or a cell array of character vectors.
    ## @code{@var{TF} = ne (@var{A}, @var{B})} is equivalent to
    ## @code{@var{TF} = ! strcmp (@var{A}, @var{B})}.
    ##
    ## @end deftypefn
    function TF = ne (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.ne: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A))
        A = repmat (A, size (B));
      elseif (isscalar (B))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.ne: inconsistent dimensions.");
      endif
      TF = ! strcmp (A.strs, B.strs);
      TF(A.isMissing | B.isMissing) = true;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} strcmp (@var{A}, @var{B})
    ##
    ## Compare strings.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically equal to
    ## @var{B}.  If one input is a string array, the other input can be a string
    ## array, a character vector, or a cell array of character vectors.
    ##
    ## If either @var{A} or @var{B} is a string array or a cell array of
    ## character vectors, then a logical array @var{TF} of the same size is
    ## returned, containing the values described above for every member of the
    ## array.  In this case, the other argument may also be a string array or a
    ## cell array of character vectors (of the same size or scalar), or a
    ## character vector.
    ##
    ## @end deftypefn
    function TF = strcmp (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.strcmp: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A))
        A = repmat (A, size (B));
      elseif (isscalar (B))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.strcmp: inconsistent dimensions.");
      endif
      TF = strcmp (A.strs, B.strs);
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} strcmp (@var{A}, @var{B})
    ##
    ## Compare strings (case insensitive).
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically equal to
    ## @var{B}, disregarding case of alphabetic characters.  If one input is a
    ## string array, the other input can be a string array, a character vector,
    ## or a cell array of character vectors.
    ##
    ## If either @var{A} or @var{B} is a string array or a cell array of
    ## character vectors, then a logical array @var{TF} of the same size is
    ## returned, containing the values described above for every member of the
    ## array.  In this case, the other argument may also be a string array or a
    ## cell array of character vectors (of the same size or scalar), or a
    ## character vector.
    ##
    ## @end deftypefn
    function TF = strcmpi (A, B)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.strcmpi: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A))
        A = repmat (A, size (B));
      elseif (isscalar (B))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.strcmpi: inconsistent dimensions.");
      endif
      TF = strcmpi (A.strs, B.strs);
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} strncmp (@var{A}, @var{B}, @var{n})
    ##
    ## Compare first @var{n} characters of strings.
    ##
    ## @var{TF} is @qcode{true}, if the first @var{n} characters of strings
    ## @var{A} and @var{B} are lexicographically equal.  If one input is a
    ## string array, the other input can be a string array, a character vector,
    ## or a cell array of character vectors.
    ##
    ## If either @var{A} or @var{B} is a string array or a cell array of
    ## character vectors, then a logical array @var{TF} of the same size is
    ## returned, containing the values described above for every member of the
    ## array.  In this case, the other argument may also be a string array or a
    ## cell array of character vectors (of the same size or scalar), or a
    ## character vector.
    ##
    ## @end deftypefn
    function TF = strncmp (A, B, n)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.strncmp: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A))
        A = repmat (A, size (B));
      elseif (isscalar (B))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.strncmp: inconsistent dimensions.");
      endif
      TF = strncmp (A.strs, B.strs, n);
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} strncmp (@var{A}, @var{B}, @var{n})
    ##
    ## Compare first @var{n} characters of strings (case insensitive).
    ##
    ## @var{TF} is @qcode{true}, if the first @var{n} characters of strings
    ## @var{A} and @var{B} are lexicographically equal, disregarding case of
    ## alphabetic characters.  If one input is a string array, the other input
    ## can be a string array, a character vector, or a cell array of character
    ## vectors.
    ##
    ## If either @var{A} or @var{B} is a string array or a cell array of
    ## character vectors, then a logical array @var{TF} of the same size is
    ## returned, containing the values described above for every member of the
    ## array.  In this case, the other argument may also be a string array or a
    ## cell array of character vectors (of the same size or scalar), or a
    ## character vector.
    ##
    ## @end deftypefn
    function TF = strncmpi (A, B, n)
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.strncmpi: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      if (isscalar (A))
        A = repmat (A, size (B));
      elseif (isscalar (B))
        B = repmat (B, size (A));
      elseif (! isequal (size (A), size (B)))
        error ("string.strncmpi: inconsistent dimensions.");
      endif
      TF = strncmpi (A.strs, B.strs, n);
      TF(A.isMissing | B.isMissing) = false;
    endfunction

  endmethods

################################################################################
##                          ** String Operations **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'append'           'compose'          'erase'            'eraseBetween'    ##
## 'extract'          'extractAfter'     'extractBefore'    'extractBetween'  ##
## 'insertAfter'      'insertBefore'     'replace'          'replaceBetween'  ##
## 'reverse'          'sort'             'split'            'strcat           ##
## 'strip'            'pad'              'join'             'plus'            ##
## 'lower'            'upper'                                                 ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function out = compose (this, varargin)
      error ("string.compose: not implemented yet.");
    endfunction

    function out = eraseBetween (this, start, stop)
      error ("string.eraseBetween: not implemented yet.");
    endfunction

    function out = extract (this, pat)
      error ("string.extract: not implemented yet.");
    endfunction

    function out = extractAfter (this, pat)
      error ("string.extractAfter: not implemented yet.");
    endfunction

    function out = extractBefore (this, pat)
      error ("string.extractBefore: not implemented yet.");
    endfunction

    function out = extractBetween (this, start, stop)
      error ("string.extractBetween: not implemented yet.");
    endfunction

    function out = insertAfter (this, pat, new)
      error ("string.insertAfter: not implemented yet.");
    endfunction

    function out = insertBefore (this, pat, new)
      error ("string.insertBefore: not implemented yet.");
    endfunction

    function out = replace (this, pat, new)
      error ("string.replace: not implemented yet.");
    endfunction

    function out = replaceBetween (this, start, stop, new)
      error ("string.replaceBetween: not implemented yet.");
    endfunction

    function out = sort (this, varargin)
      error ("string.sort: not implemented yet.");
    endfunction

    function out = split (this, varargin)
      error ("string.split: not implemented yet.");
    endfunction

    function out = strip (this, varargin)
      error ("string.strip: not implemented yet.");
    endfunction

    function out = pad (this, varargin)
      error ("string.pad: not implemented yet.");
    endfunction

    function out = join (this, varargin)
      error ("string.join: not implemented yet.");
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} append (@var{str1}, @dots{}, @var{strN})
    ##
    ## Remove content from string array.
    ##
    ## @code{@var{newstr} = append (@var{str1}, @dots{}, @var{strN})} combines
    ## the text from each input argument, @var{str1}, @dots{}, @var{strN}),
    ## which must be either string arrays, cell arrays of character vectors, or
    ## character vectors or matrices.  All input arguments must be of compatible
    ## sizes.  Character vectors are treated as a single text element and
    ## character matrices are treated as a column of elements.  @code{apped}
    ## preserves any trailing white spaces, unlike the @code{strcat} function.
    ##
    ## @end deftypefn
    function out = append (varargin)
      ## Check input for valid types
      fcn = @(x) iscellstr (x) || ischar (x) || isa (x, 'string');
      dtypes = cellfun (fcn, varargin);
      if (! all (dtypes))
        error (strcat ("string.append: input arguments must be strindg", ...
                       " arrays, cell arrays of character vectors, or", ...
                       " character matrices."));
      endif
      ## Convert all inputs to string arrays
      ctypes = cellfun (@(x) ! isa (x, 'string'), varargin);
      if (any (ctypes))
        varargin(ctypes) = cellfun (@(x) string (char (x)), ...
                                    varargin(ctypes), "UniformOutput", false);
      endif
      ## Handle compatible dimensions
      out = varargin{1};
      in_sz = cellfun (@size, varargin, "UniformOutput", false);
      strArgs = cellfun (@(x) x.strs, varargin, 'UniformOutput', false);
      ismArgs = cellfun (@(x) x.isMissing, varargin, 'UniformOutput', false);
      if (isequal (in_sz{:}))
        out.strs = strcat (strArgs{:});
        out.isMissing = and (ismArgs{:});
      else
        try
          out.isMissing = and (ismArgs{:});
          szo = size (out.isMissing);
        catch
          error ("string.append: inputs have incompatible sizes.");
        end_try_catch
        for n = 1:numel (strArgs)
          strArg = strArgs{n};
          newsz = szo ./ size (strArg);
          strArgs{n} = repmat (strArg, newsz);
        endfor
        out.strs = strcat (strArgs{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} erase (@var{str}, @var{pat})
    ##
    ## Remove content from string array.
    ##
    ## @code{@var{newstr} = erase (@var{str}, @var{pat})} removes the
    ## occurences of @var{pat} from each element of the string array @var{str}.
    ## @var{newstr} is a string array of the same size as @var{str}.
    ##
    ## @end deftypefn
    function out = erase (this, pat)
      pat = char (pat);
      out = this;
      out.strs = strrep (this.strs, pat, '');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} reverse (@var{str})
    ##
    ## Reverse order of characters in string array.
    ##
    ## @code{@var{newstr} = reverse (@var{str})} reverses the order of the
    ## characters in every each element of the string array @var{str}.
    ## @var{newstr} is a string array of the same size as @var{str}.
    ##
    ## @end deftypefn
    function out = reverse (this)
      fu2n = @(x) typecast (unicode2native (x, 'UTF-32LE'), 'uint32');
      fn2u = @(x) native2unicode (typecast (x, 'uint8'), 'UTF-32LE');
      frev = @(x) x(end:-1:1);
      notempty = ! cellfun (@isempty, this.strs);
      out = this;
      code = cellfun (fu2n, this.strs(notempty), "UniformOutput", false);
      code = cellfun (frev, code, "UniformOutput", false);
      out.strs(notempty) = cellfun (fn2u, code, "UniformOutput", false);
      out.isMissing = this.isMissing;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} strcat (@var{str1}, @var{str2}, @dots{})
    ##
    ## Horizontal concatenation of texts in string array.
    ##
    ## @code{@var{newstr} = strcat (@var{str1}, @var{str2}, @dots{})} merges
    ## horizontally all the input arguments into a string array, as long as any
    ## of the input arguments is a string array.  All inputs must be of common
    ## size or scalars.  All inputs must be character vectors, cell arrays of
    ## character vectors, or string arrays.
    ##
    ## @end deftypefn
    function out = strcat (varargin)
      args = cell (size (varargin));
      for i = 1:numel (args)
        if (ischar (varargin{i}))
          args{i} = cellstr (varargin{i});
        elseif (isstring (varargin{i}))
          args{i} = cellstr (varargin{i});
        elseif (iscellstr (varargin{i}))
          args{i} = varargin{i};
        else
          error ("string.strcat: incompatible data type: '%s'", ...
                 class (varargin{i}));
        endif
      endfor
      out = string (strcat (args{:}));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} plys (@var{str1}, @var{str2})
    ##
    ## Append strings.
    ##
    ## @code{@var{newstr} = lower (@var{str})} is the equivalent of the syntax
    ## @code{@var{newstr} = @var{str1} + @var{str2}} and appends @var{str2} to
    ## @var{str1}.  Both input arguments must be string arrays of compatible
    ## size.
    ##
    ## @end deftypefn
    function out = plus (str1, str2)
      if (isa (str1, 'string') && isa (str2, 'string'))
        out = append (str1, str2);
      else
        error ("string.plus: both STR1 and STR2 must be string arrays.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} lower (@var{str})
    ##
    ## Convert contents of string array to lower case.
    ##
    ## @code{@var{newstr} = lower (@var{str})} converts all upper case
    ## characters in every element of the string array @var{str} to lower case.
    ## @var{newstr} is a string array of the same size as @var{str}.
    ##
    ## @end deftypefn
    function out = lower (this)
      out = this;
      out.strs = lower (this.strs);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} upper (@var{str})
    ##
    ## Convert contents of string array to upper case.
    ##
    ## @code{@var{newstr} = upper (@var{str})} converts all lower case
    ## characters in every element of the string array @var{str} to upper case.
    ## @var{newstr} is a string array of the same size as @var{str}.
    ##
    ## @end deftypefn
    function out = upper (this)
      out = this;
      out.strs = upper (this.strs);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{B} =} unique (@var{A})
    ## @deftypefnx {string} {@var{B} =} unique (@var{A}, @qcode{'rows'})
    ## @deftypefnx {string} {[@var{B}, @var{ixA}, @var{ixB}] =} unique (@dots{})
    ## @deftypefnx {string} {@dots{} =} unique (@dots{}, @var{order})
    ## @deftypefnx {string} {@dots{} =} unique (@dots{}, @var{occurence})
    ##
    ## Unique values in a string array.
    ##
    ## @code{@var{B} = unique (@var{A})} returns the unique values of the string
    ## array @var{A} in the string vector @var{B} sorted lexicographically.  If
    ## If @var{A} is a column vector, then @var{B} is also a column vector,
    ## otherwise @code{unique} returns a row vector.
    ##
    ## @code{@var{B} = unique (@var{A}, @qcode{'rows'})} returns the unique rows
    ## of the string matrix @var{A} in the string matrix @var{B} sorted in
    ## lexicographical order.
    ##
    ## @code{[@var{B}, @var{ixA}, @var{ixB}] = unique (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that
    ## @code{@var{B} = @var{A}(@var{ixA})} and
    ## @code{@var{A} = @var{B}(@var{ixB})}, unless the @qcode{'rows'} optional
    ## argument is given, in which case @code{@var{B} = @var{A}(@var{ixA},:)}
    ## and @code{@var{A} = @var{B}(@var{ixB},:)}.
    ##
    ## @code{@dots{} = unique (@dots{}, @var{order})} also specifies the order
    ## of the returned unique values.  @var{order} may be either
    ## @qcode{'sorted'}, which is the default behavior, or @qcode{'stable'}, in
    ## which case the unique values are returned in order of appearance.
    ##
    ## @code{@dots{} = unique (@dots{}, @var{occurence})} also specifies the
    ## which index is returned in @var{ixA}, where there are repeated values or
    ## rows (if opted) in the input categorical array.  @var{occurence} may be
    ## either @qcode{'first'}, which is the default and returns the index of the
    ## first occurence of each unique value, or @qcode{'last'}, in which case
    ## the last occurence of each unique value is returned.
    ##
    ## @end deftypefn
    function [B, ixA, ixB] = unique (A, varargin)
      ## Handle 'rows' option
      do_rows = false;
      if (! isempty (varargin))
        idx = strcmpi ('rows', varargin(:));
        if (any (idx))
          do_rows = true;
          varargin(idx) = [];
          if (ndims (A) != 2)
            error ("string.unique: 'rows' applies only to 2-D matrices.");
          endif
        endif
      endif
      ## Handle 'setOrder' and 'occurence' options
      opt = "sorted";
      if (! isempty (varargin))
        if (any (strcmp (varargin{1}, {"sorted", "stable", "first", "last"})))
          opt = varargin{1};
        else
          error ("string.unique: invalid option '%s'.", varargin{1});
        endif
      endif
      ## Find unique
      if (do_rows)
        is_nm = ! any (A.isMissing, 2);
        A = subset (A, is_nm, ':');
        [~, ixA, ixB] = __unique__ (A.strs, 'rows', opt);
        B = subset (A, ixA, ':');
        is_missing = ! is_nm;
        if (any (is_missing))
          w = size (A, 2);
          B = [B; repmat(missing, sum (is_missing), w)];
        endif
      else
        is_nm = ! A.isMissing;
        A = subset (A, is_nm);
        [~, ixA, ixB] = __unique__ (A.strs, opt);
        B = subset (A, ixA);
        is_missing = ! is_nm(:);
        if (any (is_missing))
          if (isrow (A))
            mstr = repmat (missing, 1, sum (is_missing));
            B = [B, mstr];
          else
            mstr = repmat (missing, sum (is_missing), 1);
            B = [B; mstr];
          endif
        endif
      endif
    endfunction

  endmethods

################################################################################
##                           ** Array Operations **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'cat'              'horzcat'          'vertcat'          'repmat'          ##
## 'repelem'          'repelems'         'reshape'          'circshift'       ##
## 'permute'          'ipermute'         'transpose'        'ctranspose'      ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{C} =} cat (@var{dim}, @var{A}, @var{B}, @dots{})
    ##
    ## Concatenate string arrays.
    ##
    ## @code{@var{C} = cat (@var{dim}, @var{A}, @var{B}, @dots{})} concatenates
    ## string arrays @var{A}, @var{B}, @dots{} along dimension @var{dim}.  All
    ## input arrays must have the same size except along the operating dimension
    ## @var{dim}.  Any of the input arrays may also be character matrixes, cell
    ## arrays of character vectors, numeric arrays, or logical arrays of
    ## compatible size.
    ##
    ## @end deftypefn
    function out = cat (dim, varargin)
      args = varargin;
      ## For categorical, datetime, and duration arrays being present in the
      ## input arguments, call their constructor and forward all input to their
      ## respective concatenation method.
      for i = 1:numel (args)
        if (iscategorical (args{i}))
          try
            cat_array = cellfun (@categorical, varargin, 'UniformOutput', false);
            out = [cat_array{:}];
            return;
          catch (err)
            error (err.message);
          end_try_catch
        elseif (isdatetime (args{i}))
          try
            dat_array = cellfun (@datetime, varargin, 'UniformOutput', false);
            out = [dat_array{:}];
            return;
          catch (err)
            error (err.message);
          end_try_catch
        elseif (isduration (args{i}))
          try
            dur_array = cellfun (@duration, varargin, 'UniformOutput', false);
            out = [dur_array{:}];
            return;
          catch (err)
            error (err.message);
          end_try_catch
        ## Grab everything else and try converting it a to string array
        elseif (! isa (args{i}, 'string'))
          try
            args(i) = string (args{i});
          catch
            error ("string.cat: cannot concatenate string arrays with 's'.", ...
                   class (args{i}));
          end_try_catch
        endif
      endfor
      ## Concatenate strings
      out = args{1};
      tmp = cellfun (@(obj) obj.strs, args, 'UniformOutput', false);
      out.strs = cat (dim, tmp{:});
      tmp = cellfun (@(obj) obj.isMissing, args, 'UniformOutput', false);
      out.isMissing = cat (dim, tmp{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{C} =} horzcat (@var{A}, @var{B}, @dots{})
    ##
    ## Horizontal concatenation of string arrays.
    ##
    ## @code{@var{C} = horzcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}, @var{B}, @dots{}]} and horizontally
    ## concatenates the string arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must have the same size except along the second dimension.  Any of
    ## the input arrays may also be character matrixes, cell arrays of character
    ## vectors, numeric arrays, or logical arrays of compatible size.
    ##
    ## @end deftypefn
    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{C} =} vertcat (@var{A}, @var{B}, @dots{})
    ##
    ## Vertical concatenation of string arrays.
    ##
    ## @code{@var{C} = vertcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}; @var{B}; @dots{}]} and vertically
    ## concatenates the string arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must have the same size except along the second dimension.  Any of
    ## the input arrays may also be character matrixes, cell arrays of character
    ## vectors, numeric arrays, or logical arrays of compatible size.
    ##
    ## @end deftypefn
    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{B} =} repmat (@var{A}, @var{n})
    ## @deftypefnx {string} {@var{B} =} repmat (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {string} {@var{B} =} repmat (@var{A}, @var{dimvec})
    ##
    ## Repeat copies of a string array.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{n})} returns a string array @var{B}
    ## containing @var{n} copies of the input string array @var{A} along every
    ## dimension of @var{A}.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{d1}, @dots{}, @var{dN})} returns an
    ## array @var{B} containing copies of @var{A} along the dimensions specified
    ## by the list of scalar integer values @var{d1}, @dots{}, @var{dN}, which
    ## specify how many copies of @var{A} are made in each dimension.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{dimvec})} is equivalent to the
    ## previous syntax with @code{@var{dimvec} = [@var{d1}, @dots{}, @var{dN}]}.
    ##
    ## @end deftypefn
    function this = repmat (this, varargin)
      this.strs = repmat (this.strs, varargin{:});
      this.isMissing = repmat (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{B} =} repelem (@var{A}, @var{n})
    ## @deftypefnx {string} {@var{B} =} repelem (@var{A}, @var{d1}, @dots{}, @var{dN})
    ##
    ## Repeat copies of string array elements.
    ##
    ## @code{@var{B} = repelem (@var{A}, @var{n})} returns a string vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a string vector.  If @var{n} is a scalar, each element of @var{A} is
    ## repeated @var{n} times along the non-singleton dimension of @var{A}.  If
    ## @var{n} is a vector, it must have the same elemnts as @var{A}, in which
    ## case it specifies the number of times to repeat each corresponding
    ## element of @var{A}.
    ##
    ## @code{@var{B} =} repelem (@var{A}, @var{d1}, @dots{}, @var{dN}} returns
    ## an array @var{B} with each element of @var{A} repeated according to the
    ## the list of input arguments @code{@var{d1}, @dots{}, @var{dN}} each
    ## corresponding to a different dimension @code{1:ndims (@var{A})} of the
    ## input array @var{A}.  @var{d1}, @dots{}, @var{dN} must be either scalars
    ## or vectors with the same length as the corresponding dimension of
    ## @var{A} containing non-negative integer values specifying the number of
    ## repetitions of each element along the corresponding dimension.
    ##
    ## @end deftypefn
    function this = repelem (this, varargin)
      this.strs = repelem (this.strs, varargin{:});
      this.isMissing = repelem (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{B} =} repelems (@var{A}, @var{R})
    ##
    ## Construct a vector of repeated elements from a string array.
    ##
    ## @code{@var{B} = repelems (@var{A}, @var{R})} returns a string vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a string vector.  @var{R} must be a @math{2xN} matrix of integers.
    ## Entries in the first row of @var{R} correspond to the linear indexing of
    ## the elements in @var{A} to be repeated.  The corresponding entries in the
    ## second row of @var{R} specify the repeat count of each element.
    ##
    ## @end deftypefn
    function this = repelems (this, R)
      this.strs = repelems (this.strs, R);
      this.isMissing = repelems (this.isMissing, R);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{B} =} reshape (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {string} {@var{B} =} reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})
    ## @deftypefnx {string} {@var{B} =} reshape (@var{A}, @var{dimvec})
    ##
    ## Repeat copies of string array elements.
    ##
    ## @code{@var{B} = reshape (@var{A}, @var{d1}, @dots{}, @var{dN})} returns
    ## a string array @var{B} with specified dimensions @var{d1}, @dots{},
    ## @var{dN}, whose elements are taken columnwise from the string array
    ## @var{A}.  The product of @var{d1}, @dots{}, @var{dN} must equal the total
    ## number of elements in @var{A}.
    ##
    ## @code{@var{B} = reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})} returns
    ## a string array @var{B} with one dimension unspecified which is calculated
    ## automatically so that the product of dimensions in @var{B} matches the
    ## total elements in @var{A}, which must be divisible the product of
    ## specified dimensions.  An empty matrix @qcode{([])} is used to flag the
    ## unspecified dimension.
    ##
    ## @end deftypefn
    function this = reshape (this, varargin)
      this.strs = reshape (this.strs, varargin{:});
      this.isMissing = reshape (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{B} =} circshift (@var{A}, @var{n})
    ## @deftypefnx {string} {@var{B} =} circshift (@var{A}, @var{n}, @var{dim})
    ##
    ## Circularly shift the elements in a string array.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n})} circularly shifts the
    ## elements of the string array @var{A} according to @var{n}.  If @var{n}
    ## is a nonzero integer scalar, then the elements of @var{A} are shifted by
    ## @var{n} elements along the first non-singleton dimension of @var{A}.  If
    ## @var{n} is a vector, it must not be longer that the number of dimensions
    ## of @var{A} with each value of @var{n} corresponding to a dimension in
    ## @var{A}.   The sign of the value(s) in @var{n} specify the direction in
    ## the elements of @var{A} are shifted.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n}, @var{dim})} circularly
    ## shifts the elements of the string array @var{A} along the dimension
    ## specified by @var{dim}.  In this case, @var{n} must be a scalar integer
    ## value.
    ##
    ## @end deftypefn
    function this = circshift (this, varargin)
      this.strs = circshift (this.strs, varargin{:});
      this.isMissing = circshift (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{B} =} permute (@var{A}, @var{dims})
    ##
    ## Generalized transpose for a string N-D array.
    ##
    ## @code{@var{B} = permute (@var{A}, @var{dims})} returns the generalized
    ## transpose of the string array @var{A} by rearranging its dimensions
    ## according to the permutation vector specified in @var{dims}.
    ##
    ## @var{dims} must index all the dimensions @code{1:ndims (@var{A})} of the
    ## input array @var{A}, in any order, but only once.  The @var{N}th
    ## dimension of @var{A} gets remapped to the dimension in @var{B} specified
    ## by @code{@var{dims}(@var{N})}.
    ##
    ## @end deftypefn
    function this = permute (this, varargin)
      this.strs = permute (this.strs, varargin{:});
      this.isMissing = permute (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{A} =} ipermute (@var{B}, @var{dims})
    ##
    ## Inverse of the generalized transpose for a string N-D array.
    ##
    ## @code{@var{A} = ipermute (@var{B}, @var{dims})} returns the inverse of
    ## the generalized transpose performed by the @code{permute} function.  The
    ## expression @code{ipermute (permute (@var{A}, @var{dims}), @var{dims})}
    ## returns the original array @var{A}.
    ##
    ## @var{dims} must index all the dimensions @code{1:ndims (@var{B})} of the
    ## input array @var{B}, in any order, but only once.  The dimension of
    ## @var{B} specified in @code{@var{dims}(@var{N})} gets remapped to the
    ## @var{N}th dimension of @var{A}.
    ##
    ## @end deftypefn
    function this = ipermute (this, varargin)
      this.strs = ipermute (this.strs, varargin{:});
      this.isMissing = ipermute (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{B} =} transpose (@var{A})
    ##
    ## Transpose a string matrix.
    ##
    ## @code{@var{B} = transpose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}.'} and returns the transpose of the string
    ## matrix @var{A}.
    ##
    ## @end deftypefn
    function this = transpose (this, varargin)
      this.strs = transpose (this.strs, varargin{:});
      this.isMissing = transpose (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{B} =} ctranspose (@var{A})
    ##
    ## Transpose a string matrix.
    ##
    ## @code{@var{B} = ctranspose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}'} and returns the transpose of the string matrix
    ## @var{A}.  For string arrays, @code{ctranspose} is identical to
    ## @code{transpose}.
    ##
    ## @end deftypefn
    function this = ctranspose (this, varargin)
      this.strs = ctranspose (this.strs, varargin{:});
      this.isMissing = ctranspose (this.isMissing, varargin{:});
    endfunction

  endmethods

################################################################################
##                  ** Reference and Assignment Operations **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'end'              'subsref'          'subsasgn'                           ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Overloaded end keyword
    function last_index = end (this, end_dim, ndim_obj)
      lastdim = ndims (this);
      if (end_dim == ndim_obj && ndim_obj == 1)
        last_index = prod (size (this));
      elseif (end_dim == ndim_obj && end_dim < lastdim)
        last_index = prod (size (this)(end_dim:lastdim));
      else
        last_index = size (this, end_dim);
      endif
    endfunction

    ## Class specific subscripted reference
    function varargout = subsref (this, s)

      chain_s = s(2:end);
      s = s(1);
      switch (s.type)
        case '()'
          out = this;
          out.strs = this.strs(s.subs{:});
          out.isMissing = this.isMissing(s.subs{:});

        case '{}'
          out = this.strs(s.subs{:});

        case '.'
          error (["string.subsasgn: '.' invalid indexing", ...
                  " for referencing values. Use '()' instead."]);
      endswitch

      ## Chained references
      if (! isempty (chain_s))
        out = subsref (out, chain_s);
      endif
      varargout{1} = out;

    endfunction

    ## Class specific subscripted assignment
    function this = subsasgn (this, s, val)

      ## Chained subscripts
      chain_s = s(2:end);
      s = s(1);
      if (! isempty (chain_s))
        rhs_in = subsref (this, s);
        rhs = subsasgn (rhs_in, chain_s, val);
      else
        rhs = val;
      endif

      switch (s.type)
        case '()'
          if (isempty (rhs))
            this.strs(s.subs{:}) = [];
            this.isMissing(s.subs{:}) =[];
            return;
          elseif (! isa (rhs, "string"))
            rhs = string (rhs);
          endif
          this.strs(s.subs{:}) = rhs.strs;
          this.isMissing(s.subs{:}) = rhs.isMissing;

        case '{}'
          if (! ischar (rhs) || ! isvector (rhs))
            error (["string.subsasgn: '{}' indexed assignment", ...
                    " requires a character vector."]);
          endif
          if (numel (this.strs(s.subs{:})) != 1)
            error (["string.subsasgn: '{}' indexing can", ...
                    " only be used for simple assignment."]);
          endif
          this.strs(s.subs{:}) = {rhs};
          this.isMissing(s.subs{:}) = false;

        case '.'
          error (["string.subsasgn: '.' invalid indexing for", ...
                  " assigning values. Use '()' or '{}' instead."]);
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.strs = this.strs(varargin{:});
      this.isMissing = this.isMissing(varargin{:});
    endfunction

  endmethods

endclassdef

function out = cmp_uint32 (Acode, Bcode)
  A_n = numel (Acode);
  B_n = numel (Bcode);
  len = min (A_n, B_n);
  A_d = double (Acode(1:len));
  B_d = double (Bcode(1:len));
  out = sign (A_d - B_d);
  out = out(find (out, 1));
  if (isempty (out))
    out = sign (A_n - B_n);
  endif
endfunction

function out = sign_strings (A, B)
  fcn = @(x) typecast (unicode2native (x, 'UTF-32LE')(1:4*numel (x)), 'uint32');
  Acode = cellfun (fcn, A, "UniformOutput", false);
  Acode(cellfun ('isempty', Acode)) = 0;
  Bcode = cellfun (fcn, B, "UniformOutput", false);
  Bcode(cellfun ('isempty', Bcode)) = 0;
  out = cellfun (@cmp_uint32, Acode, Bcode);
endfunction

## Test string constructor
%!test
%! str = string (["a";"b";"c"]);
%! assert (cellstr (str), {"a";"b";"c"});
%!test
%! str = string ({"a";"b";"c"});
%! assert (cellstr (str), {"a";"b";"c"});
%!test
%! str = string ({"a";"";"c"});
%! tfM = ismissing (str);
%! assert (cell (str), {"a";"";"c"});
%! assert (tfM, logical ([0; 0; 0]));
%!test
%! str = string ([1 2 3 NaN 5]);
%! tfM = ismissing (str);
%! assert (cellstr (str), {"1", "2", "3", "", "5"});
%! assert (tfM, logical ([0 0 0 1 0]));
%!test
%! str = string (duration ([3,4,5; NaN,NaN,NaN]));
%! tfM = ismissing (str);
%! assert (cellstr (str), {"03:04:05"; ""});
%! assert (tfM, logical ([0; 1]));
%!test
%! str = string (calendarDuration ([3,4,5; NaN,NaN,NaN]));
%! tfM = ismissing (str);
%! assert (cellstr (str), {"3y 4mo 5d"; ""});
%! assert (tfM, logical ([0; 1]));

%!error<string: cell array must explicitly contain scalar elements.> ...
%! string ({[1 2], false})
%!error<string: cell array contains unsupported types.> string ({"d", @(x)x});
%!error<string: unsupported input type: 'function_handle'> string (@(x)x);

## Test relational operations
%!assert (eq (string ("A"), string ("A")), true);
%!assert (eq (string ("A"), string ("b")), false);
%!assert (eq (string ("A"), {"A", "b"}), [true, false]);
%!assert (eq ({"A", "b"}, string ("A")), [true, false]);
%!assert (eq (string ({'A', 'b'}), 'A'), [true, false]);
%!assert (eq ('A', string ({"A", "b"})), [true, false]);
%!error <string.eq: comparison between 'string' and 'double' is not supported.> ...
%!       eq (string ("A"), 2)
%!error <string.eq: comparison between 'double' and 'string' is not supported.> ...
%!       NaN == string ("A")
%!error <string.eq: inconsistent dimensions.> ...
%! eq (string ({"A","B"}), string ({"A";"B"}))
%!assert (ge (string ("A"), string ("A")), true);
%!assert (ge (string ("A"), string ("b")), false);
%!assert (ge (string ("b"), {"A", "b"}), [true, true]);
%!assert (ge ({"A", "b"}, string ("b")), [false, true]);
%!assert (ge (string ({'A', 'b'}), 'A'), [true, true]);
%!assert (ge ('A', string ({"A", "b"})), [true, false]);
%!error <string.ge: comparison between 'string' and 'double' is not supported.> ...
%!       ge (string ("A"), 2)
%!error <string.ge: comparison between 'double' and 'string' is not supported.> ...
%!       NaN >= string ("A")
%!error <string.ge: inconsistent dimensions.> ...
%! ge (string ({"A","B"}), string ({"A";"B"}))
%!assert (gt (string ("A"), string ("A")), false);
%!assert (gt (string ("A"), string ("b")), false);
%!assert (gt (string ("b"), {"A", "b"}), [true, false]);
%!assert (gt ({"A", "b"}, string ("b")), [false, false]);
%!assert (gt (string ({'A', 'b'}), 'A'), [false, true]);
%!assert (gt ('A', string ({"A", "b"})), [false, false]);
%!error <string.gt: comparison between 'string' and 'double' is not supported.> ...
%!       gt (string ("A"), 2)
%!error <string.gt: comparison between 'double' and 'string' is not supported.> ...
%!       NaN > string ("A")
%!error <string.gt: inconsistent dimensions.> ...
%! gt (string ({"A","B"}), string ({"A";"B"}))
%!assert (le (string ("A"), string ("A")), true);
%!assert (le (string ("A"), string ("b")), true);
%!assert (le (string ("b"), {"A", "b"}), [false, true]);
%!assert (le ({"A", "b"}, string ("b")), [true, true]);
%!assert (le (string ({'A', 'b'}), 'A'), [true, false]);
%!assert (le ('A', string ({"A", "b"})), [true, true]);
%!error <string.le: comparison between 'string' and 'double' is not supported.> ...
%!       le (string ("A"), 2)
%!error <string.le: comparison between 'double' and 'string' is not supported.> ...
%!       NaN <= string ("A")
%!error <string.le: inconsistent dimensions.> ...
%! le (string ({"A","B"}), string ({"A";"B"}))
%!assert (lt (string ("A"), string ("A")), false);
%!assert (lt (string ("A"), string ("b")), true);
%!assert (lt (string ("b"), {"A", "b"}), [false, false]);
%!assert (lt ({"A", "b"}, string ("b")), [true, false]);
%!assert (lt (string ({'A', 'b'}), 'A'), [false, false]);
%!assert (lt ('A', string ({"A", "b"})), [false, true]);
%!error <string.lt: comparison between 'string' and 'double' is not supported.> ...
%!       lt (string ("A"), 2)
%!error <string.lt: comparison between 'double' and 'string' is not supported.> ...
%!       NaN < string ("A")
%!error <string.lt: inconsistent dimensions.> ...
%! lt (string ({"A","B"}), string ({"A";"B"}))
%!assert (ne (string ("A"), string ("A")), false);
%!assert (ne (string ("A"), string ("b")), true);
%!assert (ne (string ("A"), {"A", "b"}), [false, true]);
%!assert (ne ({"A", "b"}, string ("A")), [false, true]);
%!assert (ne (string ({'A', 'b'}), 'A'), [false, true]);
%!assert (ne ('A', string ({"A", "b"})), [false, true]);
%!error <string.ne: comparison between 'string' and 'double' is not supported.> ...
%!       ne (string ("A"), 2)
%!error <string.ne: comparison between 'double' and 'string' is not supported.> ...
%!       NaN != string ("A")
%!error <string.ne: inconsistent dimensions.> ...
%! ne (string ({"A","B"}), string ({"A";"B"}))
%!assert (strcmp (string ("A"), string ("A")), true);
%!assert (strcmp (string ("A"), string ("b")), false);
%!assert (strcmp (string ("A"), {"A", "b"}), [true, false]);
%!assert (strcmp ({"A", "b"}, string ("A")), [true, false]);
%!assert (strcmp (string ({'A', 'b'}), 'A'), [true, false]);
%!assert (strcmp ('A', string ({"A", "b"})), [true, false]);
%!error <string.strcmp: comparison between 'string' and 'double' is not supported.> ...
%!       strcmp (string ("A"), 2)
%!error <string.strcmp: comparison between 'double' and 'string' is not supported.> ...
%!       strcmp (2, string ("A"))
%!error <string.strcmp: inconsistent dimensions.> ...
%! strcmp (string ({"A","B"}), string ({"A";"B"}))
%!assert (strcmpi (string ("A"), string ("a")), true);
%!assert (strcmpi (string ("A"), string ("b")), false);
%!assert (strcmpi (string ("a"), {"A", "b"}), [true, false]);
%!assert (strcmpi ({"A", "b"}, string ("a")), [true, false]);
%!assert (strcmpi (string ({'a', 'b'}), 'A'), [true, false]);
%!assert (strcmpi ('A', string ({"a", "b"})), [true, false]);
%!error <string.strcmpi: comparison between 'string' and 'double' is not supported.> ...
%!       strcmpi (string ("A"), 2)
%!error <string.strcmpi: comparison between 'double' and 'string' is not supported.> ...
%!       strcmpi (2, string ("A"))
%!error <string.strcmpi: inconsistent dimensions.> ...
%! strcmpi (string ({"A","B"}), string ({"a";"b"}))
%!assert (strncmp (string ("ASDFG"), "ASDER", 3), true);
%!assert (strncmp (string ("ASDFG"), "ASDER", 4), false);
%!assert (strncmp (string ("ASDFG"), {"ASDER","ASFGH"}, 3), [true, false]);
%!assert (strncmp (string ("ASDFG"), {"ASDER","ASFGH"}, 2), [true, true]);
%!error <string.strncmp: comparison between 'string' and 'double' is not supported.> ...
%!       strncmp (string ("A"), 2)
%!error <string.strncmp: comparison between 'double' and 'string' is not supported.> ...
%!       strncmp (2, string ("A"))
%!error <string.strncmp: inconsistent dimensions.> ...
%! strncmp (string ({"A","B"}), string ({"a";"b"}), 1)
%!assert (strncmpi (string ("asDFG"), "ASDER", 3), true);
%!assert (strncmpi (string ("ASDFG"), "asDER", 4), false);
%!assert (strncmpi (string ("asdfg"), {"ASDER","ASFGH"}, 3), [true, false]);
%!assert (strncmpi (string ("asdfg"), {"ASDER","ASFGH"}, 2), [true, true]);
%!error <string.strncmpi: comparison between 'string' and 'double' is not supported.> ...
%!       strncmpi (string ("A"), 2)
%!error <string.strncmpi: comparison between 'double' and 'string' is not supported.> ...
%!       strncmpi (2, string ("A"))
%!error <string.strncmpi: inconsistent dimensions.> ...
%! strncmpi (string ({"A","B"}), string ({"a";"b"}), 1)
