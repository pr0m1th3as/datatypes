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
    ## @item character arrays are converted so that each row becomes a string
    ## element, with any trailing whitespace preserved; cell arrays of character
    ## vectors are stored as-is.
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

      ## Handle empty input
      if (isempty (in) && ! ischar (in))
        sz = size (in);
        this.strs = repmat ({''}, sz);
        this.isMissing = false (sz);
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
        ## Convert each row to a string element with 'num2cell' rather than
        ## 'cellstr', which would deblank and silently drop trailing whitespace
        ## (MATLAB's 'string' preserves it).  An empty char array yields a single
        ## empty string element, matching the no-argument constructor.
        if (isempty (in))
          this.strs = {''};
        elseif (ndims (in) > 2)
          sz = size (in);
          nr = prod (sz([1,3:end]));
          nc = sz(2);
          in = reshape (in, nr, nc);
          in = num2cell (in, 2);
          sz(2) = [];
          this.strs = reshape (in, sz);
        else
          this.strs = num2cell (in, 2);
        endif
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
    ## string contents of each corresponding element or @qcode{<missing>} for
    ## missing values.
    ##
    ## Composed string elements, i.e. double quoted strings, are translated so
    ## that any special characters are represented by their corresponding
    ## escaped character sequence, unless the input string, @var{str}, is a
    ## scalar, in which case text retains its original composition but newlines
    ## are prepadded with four white space characters for aligned display.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      cstr = strcat ({'"'}, this.strs, {'"'});
      if (! isscalar (this))
        cstr = strrep (cstr, sprintf ("\r\n"), '↵');
        cstr = strrep (cstr, sprintf ("\n"), '↵');
        cstr = strrep (cstr, sprintf ("\t"), '→');
      else
        cstr = strrep (cstr, sprintf ("\n"), sprintf ("\n%s", '    '));
      endif
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
    ## which has the same size as the input string @var{str}.  All elements in
    ## @var{str} that represent real or complex numbers are converted to
    ## equivalent double values, whereas all other non-missing elements are
    ## converted to character vectors.  Zero-length strings are converted to
    ## @qcode{''} empty character vectors, while missing values are returned as
    ## @qcode{[]} empty numeric vectors.
    ##
    ## @end deftypefn
    function c_arr = cell (this)
      ## Convert numbers first
      X = double (this);
      c_arr = num2cell (X);
      ## Grab everything else
      is_text = isnan (X) & ! this.isMissing;
      c_arr(is_text) = this.strs(is_text);
      c_arr(this.isMissing) = {[]};
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
      c_mat = char (this.strs{:});
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
## 'size'             'ndims'            'numel'            'length'          ##
## 'strlength'        'count'            'keyHash'                            ##
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
    ## @deftypefn {string} {@var{out} =} strlength (@var{str})
    ##
    ## Length of text in string arrays.
    ##
    ## @end deftypefn
    function out = strlength (this)
      out = NaN (size (this));
      fcn = @(x) __unicode_length__ (x);
      TF = ! this.isMissing;
      out(TF) = cell2mat (cellfun (fcn, this.strs(TF), "UniformOutput", false));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{out} =} count (@var{str}, @var{pattern})
    ## @deftypefnx {string} {@var{out} =} count (@var{str}, @var{pattern}, @qcode{'IgnoreCase'}, @qcode{true})
    ##
    ## Count occurences of pattern in string array.
    ##
    ## @code{@var{out} = count (@var{str}, @var{pattern})} returns a numerical
    ## array @var{out} of the same size as @var{str} containing the number of
    ## occurences of @var{pattern} in each corresponding element of @var{str}.
    ##
    ## @code{@var{out} = count (@var{str}, @var{pattern}, @qcode{'IgnoreCase'},
    ## @qcode{true})} ignores case when identifying occurences of @var{pattern}.
    ##
    ## @end deftypefn
    function out = count (this, pattern, varargin)
      ## Check pattern
      if (nargin < 2)
        error ("string.count: PATTERN is required.");
      elseif (ischar (pattern) || isstring (pattern))
        pattern = cellstr (pattern);
      elseif (! iscellstr (pattern))
        error (strcat ("string.count: PATTERN much be a character vector,", ...
                       " a string array, or cell array of character vectors."));
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'IgnoreCase'};
      dfValues = {false};
      [IgnoreCase, arg] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! (islogical (IgnoreCase) && isscalar (IgnoreCase)))
        error ("string.count: 'IgnoreCase' must be a logical scalar.");
      elseif (! isempty (arg))
        error ("string.count: unrecognized input argument.");
      endif

      ## Accumulate the number of occurences of each pattern in each element
      out = zeros (size (this));
      vid = ! this.isMissing;
      str = this.strs(vid);
      if (IgnoreCase)
        for i = 1:numel (pattern)
          out(vid) += cellfun ('numel', strfind (lower (str), lower (pattern{i})));
        endfor
      else
        for i = 1:numel (pattern)
          out(vid) += cellfun ('numel', strfind (str, pattern{i}));
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{hey} =} keyHash (@var{str})
    ##
    ## Generate a hash code for string array.
    ##
    ## @code{@var{h} = keyHash (@var{str})} generates a @qcode{uint64} scalar
    ## that represents the input array @var{str}.  @code{keyHash} utilizes the
    ## 64-bit FNV-1a variant of the Fowler-Noll-Vo non-cryptographic hash
    ## function.
    ##
    ## @code{@var{h} = keyHash (@var{str}), @var{base}} also generates a 64-bit
    ## hash code using @var{base} as the offset basis for the FNV-1a hash
    ## algorithm.  @var{base} must be a @qcode{uint64} integer type scalar.  Use
    ## this syntax to cascade @code{keyHash} on multiple objects for which a
    ## single hash code is required.
    ##
    ## Note that unlike MATLAB, this implementation does not use any random seed.
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
      ## Compute hash with underlying string array values
      strs = [this.strs{:}];
      key = __ckeyHash__(strs, key);
      key = __nkeyHash__(this.isMissing(:), key);
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'contains'         'endsWith'         'startsWith'       'matches'         ##
## 'iscolumn'         'isempty'          'ismatrix'         'ismember'        ##
## 'ismissing'        'isrow'            'isscalar'         'issorted'        ##
## 'isstring'         'isvector'                                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{TF} =} contains (@var{str}, @var{pattern})
    ## @deftypefnx {string} {@var{TF} =} contains (@var{str}, @var{pattern}, @qcode{'IgnoreCase'}, @qcode{true})
    ##
    ## Test if strings contain pattern.
    ##
    ## @code{@var{TF} = contains (@var{str}, @var{pattern})} returns a logical
    ## array @var{TF} of the same size as @var{A} containing @qcode{true} for
    ## each corresponding element of @var{str} that contains the specified
    ## @var{pattern} and @qcode{false} otherwise.  Similarly to @qcode{NaN}
    ## values, @qcode{<missing>} elements do not match any pattern and always
    ## return @qcode{false}.
    ##
    ## @code{@var{TF} = contains (@var{str}, @var{pattern}, @qcode{'IgnoreCase'},
    ## @qcode{true})} ignores case when determining if @var{str} ends with
    ## @var{pattern}.
    ##
    ## @end deftypefn
    function TF = contains (this, pattern, varargin)
      ## Check pattern
      if (nargin < 2)
        error ("string.contains: PATTERN is required.");
      elseif (ischar (pattern) || isstring (pattern))
        pattern = cellstr (pattern);
      elseif (! iscellstr (pattern))
        error (strcat ("string.contains: PATTERN much be a character vector,", ...
                       " a string array, or cell array of character vectors."));
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'IgnoreCase'};
      dfValues = {false};
      [IgnoreCase, arg] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! (islogical (IgnoreCase) && isscalar (IgnoreCase)))
        error ("string.contains: 'IgnoreCase' must be a logical scalar.");
      elseif (! isempty (arg))
        error ("string.contains: unrecognized input argument.");
      endif

      ## Check for the occurence of each pattern in the nonmissing elements of
      ## the input string array and boolean OR the results
      TF = false (size (this));
      vid = ! this.isMissing;
      str = this.strs(vid);
      if (IgnoreCase)
        for i = 1:numel (pattern)
          idx = strfind (lower (str), lower (pattern{i}));
          TF(vid) |= ! cellfun ('isempty', idx);
        endfor
      else
        for i = 1:numel (pattern)
          idx = strfind (str, pattern{i});
          TF(vid) |= ! cellfun ('isempty', idx);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{TF} =} endsWith (@var{str}, @var{pattern})
    ## @deftypefnx {string} {@var{TF} =} endsWith (@var{str}, @var{pattern}, @qcode{'IgnoreCase'}, @qcode{true})
    ##
    ## Test if strings end with pattern.
    ##
    ## @code{@var{TF} = endsWith (@var{str}, @var{pattern})} returns a logical
    ## array @var{TF} of the same size as @var{A} containing @qcode{true} for
    ## each corresponding element of @var{str} that ends with the specified
    ## @var{pattern} and @qcode{false} otherwise.  Similarly to @qcode{NaN}
    ## values, @qcode{<missing>} elements do not match any pattern and always
    ## return @qcode{false}.
    ##
    ## @code{@var{TF} = endsWith (@var{str}, @var{pattern}, @qcode{'IgnoreCase'},
    ## @qcode{true})} ignores case when determining if @var{str} ends with
    ## @var{pattern}.
    ##
    ## @end deftypefn
    function TF = endsWith (this, pattern, varargin)
      ## Check pattern
      if (nargin < 2)
        error ("string.endsWith: PATTERN is required.");
      elseif (ischar (pattern) || isstring (pattern))
        pattern = cellstr (pattern);
      elseif (! iscellstr (pattern))
        error (strcat ("string.endsWith: PATTERN much be a character vector,", ...
                       " a string array, or cell array of character vectors."));
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'IgnoreCase'};
      dfValues = {false};
      [IgnoreCase, arg] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! (islogical (IgnoreCase) && isscalar (IgnoreCase)))
        error ("string.endsWith: 'IgnoreCase' must be a logical scalar.");
      elseif (! isempty (arg))
        error ("string.endsWith: unrecognized input argument.");
      endif

      ## Reverse str and pattern
      str = cellfun ('flip', this.strs, "UniformOutput", false);
      pattern = cellfun ('flip', cellstr (pattern), "UniformOutput", false);

      ## For each pattern, trim all elements of the input string array to the
      ## length of the pattern and compare strings according to IgnoreCase
      TF = false (size (this));
      vid = ! this.isMissing;
      if (IgnoreCase)
        for i = 1:numel (pattern)
          TF(vid) |= strncmpi (str(vid), pattern{i}, length (pattern{i}));
        endfor
      else
        for i = 1:numel (pattern)
          TF(vid) |= strncmp (str(vid), pattern{i}, length (pattern{i}));
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{TF} =} startsWith (@var{str}, @var{pattern})
    ## @deftypefnx {string} {@var{TF} =} startsWith (@var{str}, @var{pattern}, @qcode{'IgnoreCase'}, @qcode{true})
    ##
    ## Test if strings start with pattern.
    ##
    ## @code{@var{TF} = startsWith (@var{str}, @var{pattern})} returns a logical
    ## array @var{TF} of the same size as @var{A} containing @qcode{true} for
    ## each corresponding element of @var{str} that starts with the specified
    ## @var{pattern} and @qcode{false} otherwise.  Similarly to @qcode{NaN}
    ## values, @qcode{<missing>} elements do not match any pattern and always
    ## return @qcode{false}.
    ##
    ## @code{@var{TF} = startsWith (@var{str}, @var{pattern}, @qcode{'IgnoreCase'},
    ## @qcode{true})} ignores case when determining if @var{str} starts with
    ## @var{pattern}.
    ##
    ## @end deftypefn
    function TF = startsWith (this, pattern, varargin)
      ## Check pattern
      if (nargin < 2)
        error ("string.startsWith: PATTERN is required.");
      elseif (ischar (pattern) || isstring (pattern))
        pattern = cellstr (pattern);
      elseif (! iscellstr (pattern))
        error (strcat ("string.startsWith: PATTERN much be a character vector,", ...
                       " a string array, or cell array of character vectors."));
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'IgnoreCase'};
      dfValues = {false};
      [IgnoreCase, arg] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! (islogical (IgnoreCase) && isscalar (IgnoreCase)))
        error ("string.startsWith: 'IgnoreCase' must be a logical scalar.");
      elseif (! isempty (arg))
        error ("string.startsWith: unrecognized input argument.");
      endif

      ## For each pattern, trim all elements of the input string array to the
      ## length of the pattern and compare strings according to IgnoreCase
      TF = false (size (this));
      vid = ! this.isMissing;
      str = this.strs(vid);
      if (IgnoreCase)
        for i = 1:numel (pattern)
          TF(vid) |= strncmpi (str, pattern{i}, length (pattern{i}));
        endfor
      else
        for i = 1:numel (pattern)
          TF(vid) |= strncmp (str, pattern{i}, length (pattern{i}));
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{TF} =} matches (@var{str}, @var{pattern})
    ## @deftypefnx {string} {@var{TF} =} matches (@var{str}, @var{pattern}, @qcode{'IgnoreCase'}, @qcode{true})
    ##
    ## Test if strings match pattern.
    ##
    ## @code{@var{TF} = matches (@var{str}, @var{pattern})} returns a logical
    ## array @var{TF} of the same size as @var{A} containing @qcode{true} for
    ## each corresponding element of @var{str} that matches the specified
    ## @var{pattern} and @qcode{false} otherwise.  Similarly to @qcode{NaN}
    ## values, @qcode{<missing>} elements do not match any pattern and always
    ## return @qcode{false}.
    ##
    ## @code{@var{TF} = matches (@var{str}, @var{pattern}, @qcode{'IgnoreCase'},
    ## @qcode{true})} ignores case when determining if @var{str} starts with
    ## @var{pattern}.
    ##
    ## @end deftypefn
    function TF = matches (this, pattern, varargin)
      ## Check pattern
      if (nargin < 2)
        error ("string.matches: PATTERN is required.");
      elseif (ischar (pattern) || isstring (pattern))
        pattern = cellstr (pattern);
      elseif (! iscellstr (pattern))
        error (strcat ("string.matches: PATTERN much be a character vector,", ...
                       " a string array, or cell array of character vectors."));
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'IgnoreCase'};
      dfValues = {false};
      [IgnoreCase, arg] = parsePairedArguments (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! (islogical (IgnoreCase) && isscalar (IgnoreCase)))
        error ("string.matches: 'IgnoreCase' must be a logical scalar.");
      elseif (! isempty (arg))
        error ("string.matches: unrecognized input argument.");
      endif

      ## Check for the matching of each pattern in the nonmissing elements of
      ## the input string array and boolean OR the results
      TF = false (size (this));
      vid = ! this.isMissing;
      str = this.strs(vid);
      if (IgnoreCase)
        for i = 1:numel (pattern)
          TF(vid) |= strcmpi (str, pattern{i});
        endfor
      else
        for i = 1:numel (pattern)
          TF(vid) |= strcmp (str, pattern{i});
        endfor
      endif
    endfunction

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
    function TF = isempty (this)
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
    ## @deftypefn  {string} {@var{TF} =} ismember (@var{A}, @var{B})
    ## @deftypefnx {string} {@var{TF} =} ismember (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {string} {[@var{TF}, @var{index}] =} ismember (@dots{})
    ## @deftypefnx {string} {[@var{TF}, @var{index}] =} ismember (@dots{}, @qcode{'legacy'})
    ##
    ## Find string elements in a set.
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
    ## member of @var{B} and 0 otherwise.  If the @qcode{'legacy'} optional
    ## argument is specified, then the highest index of matched elements is
    ## returned.  Unless multiple matches exist, the @qcode{'legacy'} option has
    ## no effect on the returned @var{index}.
    ##
    ## @end deftypefn
    function varargout = ismember (A, B, varargin)
      if (iscategorical (B))
        A = categorical (A);
        if (nargout > 1)
          [varargout{1}, varargout{2}] = ismember (A, B, varargin{:});
        else
          varargout{1} = ismember (A, B, varargin{:});
        endif
        return;
      endif
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
      if (nargin > 2 && any (strcmp (varargin, 'rows')))
        if (columns (A) != columns (B))
          error (strcat ("string.ismember: A and B must have the same", ...
                         " number of columns with the 'rows' option."));
        endif
      endif
      ## Handle empty input array
      if (isempty (A) || isempty (B))
        sz = size (A);
        varargout{1} = false (sz);
        if (nargout > 1)
          varargout{2} = zeros (sz);
        endif
      else
        if (nargout > 1)
          [TF, index] = __ismember__ (A.strs, B.strs, varargin{:});
          TF(A.isMissing) = false;
          index(A.isMissing) = 0;
          varargout{1} = TF;
          varargout{2} = index;
        else
          TF = __ismember__ (A.strs, B.strs, varargin{:});
          TF(A.isMissing) = false;
          varargout{1} = TF;
        endif
      endif
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
    ## The output array @var{TF} has the same size as the input array @var{str}.
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
            for idx = 1:numel (indicator)
              TF(this == indicator.strs(idx)) = true;
            endfor
          elseif (iscellstr (indicator))
            for idx = 1:numel (indicator)
              TF(this == indicator(idx)) = true;
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
      ## Overload methods for certain data types
      if (any (isa (B, {'categorical', 'duration'})))
        TF = eq (cellstr (A), B);
        return;
      endif
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.eq: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      szA = size (A);
      szB = size (B);
      if (isscalar (A))
        A = repmat (A, szB);
      elseif (isscalar (B))
        B = repmat (B, szA);
      elseif (! isequal (szA, szB))
        ## Force size dim vectors to equal length
        ndA = numel (szA);
        ndB = numel (szB);
        if (ndA > ndB)
          szB = [szB, ones(1, ndA - ndB)];
        elseif (ndB > ndA)
          szA = [szA, ones(1, ndB - ndA)];
        endif
        ## Check for compatible dimensions
        A_1 = szA != 1 & szA != szB;
        B_1 = szB != 1 & szA != szB;
        A_B = A_1 & B_1;
        if (any (A_B))
          error ("string.eq: incompatible dimensions.");
        endif
        ## Expand arrays to same size
        eqAB = szA == szB;
        szA(eqAB) = 1;
        szB(eqAB) = 1;
        A = repmat (A, szB);
        B = repmat (B, szA);
      endif
      TF = strcmp (A.strs, B.strs);
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} ge (@var{A}, @var{B})
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
      ## Overload methods for certain data types
      if (any (isa (B, {'categorical', 'duration'})))
        TF = ge (cellstr (A), B);
        return;
      endif
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.ge: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      szA = size (A);
      szB = size (B);
      if (isscalar (A))
        A = repmat (A, szB);
      elseif (isscalar (B))
        B = repmat (B, szA);
      elseif (! isequal (szA, szB))
        ## Force size dim vectors to equal length
        ndA = numel (szA);
        ndB = numel (szB);
        if (ndA > ndB)
          szB = [szB, ones(1, ndA - ndB)];
        elseif (ndB > ndA)
          szA = [szA, ones(1, ndB - ndA)];
        endif
        ## Check for compatible dimensions
        A_1 = szA != 1 & szA != szB;
        B_1 = szB != 1 & szA != szB;
        A_B = A_1 & B_1;
        if (any (A_B))
          error ("string.ge: incompatible dimensions.");
        endif
        ## Expand arrays to same size
        eqAB = szA == szB;
        szA(eqAB) = 1;
        szB(eqAB) = 1;
        A = repmat (A, szB);
        B = repmat (B, szA);
      endif
      out = sign_strings (A.strs, B.strs);
      TF = false (size (out));
      TF(out >= 0) = true;
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} gt (@var{A}, @var{B})
    ##
    ## Test for greater than.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically greater
    ## than @var{B}.  If one input is a string array, the other input can be a
    ## string array, a character vector, or a cell array of character vectors.
    ##
    ## @end deftypefn
    function TF = gt (A, B)
      ## Overload methods for certain data types
      if (any (isa (B, {'categorical', 'duration'})))
        TF = gt (cellstr (A), B);
        return;
      endif
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.gt: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      szA = size (A);
      szB = size (B);
      if (isscalar (A))
        A = repmat (A, szB);
      elseif (isscalar (B))
        B = repmat (B, szA);
      elseif (! isequal (szA, szB))
        ## Force size dim vectors to equal length
        ndA = numel (szA);
        ndB = numel (szB);
        if (ndA > ndB)
          szB = [szB, ones(1, ndA - ndB)];
        elseif (ndB > ndA)
          szA = [szA, ones(1, ndB - ndA)];
        endif
        ## Check for compatible dimensions
        A_1 = szA != 1 & szA != szB;
        B_1 = szB != 1 & szA != szB;
        A_B = A_1 & B_1;
        if (any (A_B))
          error ("string.gt: incompatible dimensions.");
        endif
        ## Expand arrays to same size
        eqAB = szA == szB;
        szA(eqAB) = 1;
        szB(eqAB) = 1;
        A = repmat (A, szB);
        B = repmat (B, szA);
      endif
      out = sign_strings (A.strs, B.strs);
      TF = false (size (out));
      TF(out > 0) = true;
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} le (@var{A}, @var{B})
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
      ## Overload methods for certain data types
      if (any (isa (B, {'categorical', 'duration'})))
        TF = le (cellstr (A), B);
        return;
      endif
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.le: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      szA = size (A);
      szB = size (B);
      if (isscalar (A))
        A = repmat (A, szB);
      elseif (isscalar (B))
        B = repmat (B, szA);
      elseif (! isequal (szA, szB))
        ## Force size dim vectors to equal length
        ndA = numel (szA);
        ndB = numel (szB);
        if (ndA > ndB)
          szB = [szB, ones(1, ndA - ndB)];
        elseif (ndB > ndA)
          szA = [szA, ones(1, ndB - ndA)];
        endif
        ## Check for compatible dimensions
        A_1 = szA != 1 & szA != szB;
        B_1 = szB != 1 & szA != szB;
        A_B = A_1 & B_1;
        if (any (A_B))
          error ("string.le: incompatible dimensions.");
        endif
        ## Expand arrays to same size
        eqAB = szA == szB;
        szA(eqAB) = 1;
        szB(eqAB) = 1;
        A = repmat (A, szB);
        B = repmat (B, szA);
      endif
      out = sign_strings (A.strs, B.strs);
      TF = false (size (out));
      TF(out <= 0) = true;
      TF(A.isMissing | B.isMissing) = false;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{TF} =} lt (@var{A}, @var{B})
    ##
    ## Test for less than.
    ##
    ## @var{TF} is @qcode{true}, if string @var{A} is lexicographically less
    ## than @var{B}.  If one input is a string array, the other input can be a
    ## string array, a character vector, or a cell array of character vectors.
    ##
    ## @end deftypefn
    function TF = lt (A, B)
      ## Overload methods for certain data types
      if (any (isa (B, {'categorical', 'duration'})))
        TF = lt (cellstr (A), B);
        return;
      endif
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.lt: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      szA = size (A);
      szB = size (B);
      if (isscalar (A))
        A = repmat (A, szB);
      elseif (isscalar (B))
        B = repmat (B, szA);
      elseif (! isequal (szA, szB))
        ## Force size dim vectors to equal length
        ndA = numel (szA);
        ndB = numel (szB);
        if (ndA > ndB)
          szB = [szB, ones(1, ndA - ndB)];
        elseif (ndB > ndA)
          szA = [szA, ones(1, ndB - ndA)];
        endif
        ## Check for compatible dimensions
        A_1 = szA != 1 & szA != szB;
        B_1 = szB != 1 & szA != szB;
        A_B = A_1 & B_1;
        if (any (A_B))
          error ("string.lt: incompatible dimensions.");
        endif
        ## Expand arrays to same size
        eqAB = szA == szB;
        szA(eqAB) = 1;
        szB(eqAB) = 1;
        A = repmat (A, szB);
        B = repmat (B, szA);
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
      ## Overload methods for certain data types
      if (any (isa (B, {'categorical', 'duration'})))
        TF = ne (cellstr (A), B);
        return;
      endif
      if (iscellstr (A) || ischar (A))
        A = string (A);
      elseif (iscellstr (B) || ischar (B))
        B = string (B);
      elseif (! isa (A, 'string') || ! isa (B, 'string'))
        error (strcat ("string.ne: comparison between '%s' and '%s'", ...
                       " is not supported."), class (A), class (B));
      endif
      szA = size (A);
      szB = size (B);
      if (isscalar (A))
        A = repmat (A, szB);
      elseif (isscalar (B))
        B = repmat (B, szA);
      elseif (! isequal (szA, szB))
        ## Force size dim vectors to equal length
        ndA = numel (szA);
        ndB = numel (szB);
        if (ndA > ndB)
          szB = [szB, ones(1, ndA - ndB)];
        elseif (ndB > ndA)
          szA = [szA, ones(1, ndB - ndA)];
        endif
        ## Check for compatible dimensions
        A_1 = szA != 1 & szA != szB;
        B_1 = szB != 1 & szA != szB;
        A_B = A_1 & B_1;
        if (any (A_B))
          error ("string.ne: incompatible dimensions.");
        endif
        ## Expand arrays to same size
        eqAB = szA == szB;
        szA(eqAB) = 1;
        szB(eqAB) = 1;
        A = repmat (A, szB);
        B = repmat (B, szA);
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
    ## @deftypefn {string} {@var{TF} =} strcmpi (@var{A}, @var{B})
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
    ## @deftypefn {string} {@var{TF} =} strncmpi (@var{A}, @var{B}, @var{n})
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
## 'reverse'          'sort'             'split'            'splitlines'      ##
## 'strcat'           'strip'            'pad'              'join'            ##
## 'plus'             'lower'            'upper'            'unique'          ##
##                                                                            ##
################################################################################

  methods (Hidden)

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

    function out = splitlines (this, varargin)
      error ("string.splitlines: not implemented yet.");
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
    ## Combine string arrays.
    ##
    ## @code{@var{newstr} = append (@var{str1}, @dots{}, @var{strN})} combines
    ## the text from each input argument, @var{str1}, @dots{}, @var{strN}),
    ## which must be either string arrays, cell arrays of character vectors, or
    ## character vectors or matrices.  All input arguments must be of compatible
    ## sizes.  Character vectors are treated as a single text element and
    ## character matrices are treated as a column of elements.  @code{append}
    ## preserves any trailing white spaces, unlike the @code{strcat} function.
    ##
    ## @end deftypefn
    function out = append (varargin)
      ## Check input for valid types
      fcn = @(x) iscellstr (x) || ischar (x) || isa (x, 'string');
      dtypes = cellfun (fcn, varargin);
      if (! all (dtypes))
        error (strcat ("string.append: input arguments must be string", ...
                       " arrays, cell arrays of character vectors, or", ...
                       " character matrices."));
      endif
      ## Convert all inputs to string arrays
      ctypes = cellfun (@(x) ! isa (x, 'string'), varargin);
      if (any (ctypes))
        varargin(ctypes) = cellfun (@(x) string (char (x)), ...
                                    varargin(ctypes), "UniformOutput", false);
      endif
      ## A single argument is returned unchanged
      out = varargin{1};
      if (numel (varargin) == 1)
        return;
      endif
      ## Handle compatible dimensions.  An element is missing whenever any input
      ## contributes a missing value at that position, since a missing string
      ## propagates like NaN through concatenation.
      in_sz = cellfun (@size, varargin, "UniformOutput", false);
      strArgs = cellfun (@(x) x.strs, varargin, 'UniformOutput', false);
      ismArgs = cellfun (@(x) x.isMissing, varargin, 'UniformOutput', false);
      if (isequal (in_sz{:}))
        out.strs = strcat (strArgs{:});
        out.isMissing = or (ismArgs{:});
      else
        try
          out.isMissing = or (ismArgs{:});
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
      ## Missing elements carry no text, as elsewhere in the class
      out.strs(out.isMissing) = {''};
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{str} =} compose (@var{formatSpec}, @var{A})
    ## @deftypefnx {string} {@var{str} =} compose (@var{formatSpec}, @var{A1}, @dots{}, @var{AN})
    ## @deftypefnx {string} {@var{str} =} compose (@var{txt})
    ##
    ## Format data into a string array or translate escape-character sequences.
    ##
    ## @code{@var{str} = compose (@var{formatSpec}, @var{A})} formats the data
    ## in the array @var{A} according to the formatting operators in
    ## @var{formatSpec}, which must be a string scalar or character vector, and
    ## returns the result in the string array @var{str}.  The formatting
    ## operators are the same as those accepted by the @code{sprintf} function.
    ## Unlike @code{sprintf}, which returns a single character vector,
    ## @code{compose} returns a string array whose elements correspond to the
    ## rows of @var{A}.
    ##
    ## @code{compose} applies @var{formatSpec} to each row of @var{A} so that
    ## @var{str} has the same number of rows as @var{A}.  The size of @var{str}
    ## is further determined as follows:
    ##
    ## @itemize
    ## @item If the number of columns in @var{A} exceeds the number of
    ## formatting operators in @var{formatSpec}, then @var{formatSpec} is
    ## applied repeatedly along each row of @var{A}, adding columns to @var{str}.
    ## @item If the number of columns in @var{A} is less than the number of
    ## formatting operators, then the operators left without a corresponding
    ## value appear unchanged in @var{str}.
    ## @item If @var{A} has zero columns, then @var{str} has the same size as
    ## @var{A} and no formatting operators are applied.
    ## @end itemize
    ##
    ## @code{@var{str} = compose (@var{formatSpec}, @var{A1}, @dots{}, @var{AN})}
    ## formats the data from the arrays @var{A1}, @dots{}, @var{AN}.  The
    ## formatting operators are assigned to the input arrays in order: once an
    ## operator has consumed a value from an input array, it becomes unavailable
    ## to the following arrays.  All input arrays must be of compatible sizes.
    ##
    ## @code{@var{str} = compose (@var{txt})} translates escape-character
    ## sequences, such as @qcode{'\n'} and @qcode{'\t'}, in @var{txt} and
    ## returns the result in @var{str}, which has the same size as @var{txt}.
    ## Any formatting operators in @var{txt} are left unchanged.
    ##
    ## In all syntaxes, escape-character sequences appearing in literal text are
    ## translated and each @qcode{'%%'} literal is converted to a single
    ## @qcode{'%'} character, following the same rules as the @code{sprintf}
    ## function.  The only difference from @code{sprintf} is that a formatting
    ## operator left without a corresponding value is emitted unchanged rather
    ## than dropped.
    ##
    ## @end deftypefn
    function out = compose (this, varargin)

      ## Escape-sequence translation syntax: compose (TXT)
      if (nargin == 1)
        out = this;
        cstr = this.strs;
        for k = 1:numel (cstr)
          if (this.isMissing(k))
            continue;
          endif
          cstr{k} = compose_apply (compose_tokenize (cstr{k}), {});
        endfor
        out.strs = cstr;
        return;
      endif

      ## Formatting syntax: compose (FORMATSPEC, A1, ..., AN)
      if (! isscalar (this))
        error (strcat ("string.compose: FORMATSPEC must be a", ...
                       " string scalar or a character vector."));
      endif
      if (this.isMissing)
        error ("string.compose: FORMATSPEC cannot be a missing value.");
      endif
      tok = compose_tokenize (this.strs{1});

      ## Count the values consumed by one full pass of FORMATSPEC
      vpa = 0;
      for t = 1:numel (tok)
        if (strcmp (tok{t}.type, 'op'))
          vpa += tok{t}.nval;
        endif
      endfor

      ## Normalize each input array to a 2-D cell matrix of values
      N = numel (varargin);
      C = cell (1, N);
      nrows = ones (1, N);
      for n = 1:N
        A = varargin{n};
        if (ischar (A))
          Cn = cellstr (A);
        elseif (isa (A, 'string'))
          Cn = cellstr (A);
        elseif (isnumeric (A) || islogical (A))
          Cn = num2cell (A);
        else
          error ("string.compose: unsupported input type: '%s'.", class (A));
        endif
        Cn = reshape (Cn, size (Cn, 1), []);
        C{n} = Cn;
        nrows(n) = size (Cn, 1);
      endfor

      ## Determine the common number of rows (singletons expand)
      R = max (nrows);
      for n = 1:N
        rn = size (C{n}, 1);
        if (rn == R)
          continue;
        elseif (rn == 1)
          C{n} = repmat (C{n}, R, 1);
        else
          error ("string.compose: input arrays must be of compatible sizes.");
        endif
      endfor

      ## Combine all values per row, in column order across input arrays
      vals = [C{:}];
      V = columns (vals);

      ## With zero data columns no formatting is applied (size preserved)
      if (V == 0)
        out = string (cell (R, 0));
        return;
      endif

      ## Number of times FORMATSPEC is applied per row
      if (vpa == 0)
        nApp = 1;
      else
        nApp = max (1, ceil (V / vpa));
      endif

      ## Apply FORMATSPEC, consuming VPA values at a time
      outc = cell (R, nApp);
      for r = 1:R
        for a = 1:nApp
          lo = (a - 1) * vpa + 1;
          if (vpa == 0 || lo > V)
            slice = {};
          else
            slice = vals(r, lo:min (a * vpa, V));
          endif
          outc{r,a} = compose_apply (tok, slice);
        endfor
      endfor

      out = string (outc);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {string} {@var{newstr} =} erase (@var{str}, @var{match})
    ##
    ## Remove content from string array.
    ##
    ## @code{@var{newstr} = erase (@var{str}, @var{match})} removes the
    ## occurrences of @var{match} from each element of the string array
    ## @var{str}.  @var{match} can be a string array, a character vector, or a
    ## cell array of character vectors.  When @var{match} contains more than one
    ## piece of text, every occurrence of every element of @var{match} is
    ## removed.  @var{newstr} is a string array of the same size as @var{str};
    ## the size of @var{match} need not match the size of @var{str}.  Missing
    ## values in @var{str} are preserved.
    ##
    ## @end deftypefn
    function out = erase (this, match)
      if (isa (match, 'string'))
        pats = cellstr (match);
      elseif (ischar (match) || iscellstr (match))
        ## Route char/cellstr through the constructor, which keeps trailing
        ## whitespace that bare 'cellstr' would deblank
        pats = cellstr (string (match));
      else
        error (strcat ("string.erase: MATCH must be a string array, a", ...
                       " character vector, or a cell array of character", ...
                       " vectors."));
      endif
      out = this;
      for i = 1:numel (pats)
        out.strs = strrep (out.strs, pats{i}, '');
      endfor
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{newstr} =} eraseBetween (@var{str}, @var{startPat}, @var{endPat})
    ## @deftypefnx {string} {@var{newstr} =} eraseBetween (@var{str}, @var{startPos}, @var{endPos})
    ## @deftypefnx {string} {@var{newstr} =} eraseBetween (@dots{}, @qcode{"Boundaries"}, @var{bounds})
    ##
    ## Erase content between start and end boundaries.
    ##
    ## @code{@var{newstr} = eraseBetween (@var{str}, @var{startPat}, @var{endPat})}
    ## removes from each element of the string array @var{str} the text that
    ## occurs between the substrings @var{startPat} and @var{endPat}, keeping the
    ## boundary substrings themselves.  @var{startPat} and @var{endPat} can be
    ## string arrays, character vectors, or cell arrays of character vectors.
    ## For each element, the first occurrence of @var{startPat} is matched and
    ## then the first occurrence of @var{endPat} that begins after it; if either
    ## boundary is not found, the element is returned unchanged.
    ##
    ## @code{@var{newstr} = eraseBetween (@var{str}, @var{startPos}, @var{endPos})}
    ## removes the text between the character positions @var{startPos} and
    ## @var{endPos}, inclusive of the characters at those positions.
    ## @var{startPos} and @var{endPos} must be positive integers with
    ## @var{startPos} not exceeding @var{endPos} and both within the length of
    ## the corresponding element of @var{str}.
    ##
    ## @code{@var{newstr} = eraseBetween (@dots{}, @qcode{"Boundaries"}, @var{bounds})}
    ## specifies whether the boundaries are included in or excluded from the
    ## erased text.  @var{bounds} can be either @qcode{"inclusive"} or
    ## @qcode{"exclusive"}.  When boundaries are given as substrings, the default
    ## is @qcode{"exclusive"} and the boundary substrings are preserved; when
    ## given as positions, the default is @qcode{"inclusive"} and the characters
    ## at those positions are erased.
    ##
    ## @var{startPat}/@var{endPat} and @var{startPos}/@var{endPos} must either be
    ## scalars, applied to every element of @var{str}, or be of the same size as
    ## @var{str} and applied element-wise.  @var{newstr} is a string array of the
    ## same size as @var{str}.  Missing values in @var{str} are preserved.
    ##
    ## @end deftypefn
    function out = eraseBetween (this, start, stop, varargin)
      if (nargin < 3)
        error ("string.eraseBetween: not enough input arguments.");
      endif

      ## Position boundaries are numeric, pattern boundaries are text; the two
      ## modes cannot be mixed and have different default 'Boundaries' values.
      istxt = @(x) ischar (x) || iscellstr (x) || isa (x, 'string');
      if (isnumeric (start) && isnumeric (stop))
        posMode = true;
        dfBounds = 'inclusive';
      elseif (istxt (start) && istxt (stop))
        posMode = false;
        dfBounds = 'exclusive';
      else
        error (strcat ("string.eraseBetween: START and STOP must be either", ...
                       " both numeric positions or both text patterns."));
      endif

      ## Parse the optional 'Boundaries' Name/Value pair
      [bounds, rem] = parsePairedArguments ({'Boundaries'}, {dfBounds}, varargin);
      if (! isempty (rem))
        error ("string.eraseBetween: invalid optional arguments.");
      endif
      if (isa (bounds, 'string'))
        bounds = char (bounds);
      endif
      if (! ischar (bounds) || ! any (strcmpi (bounds, {'inclusive', 'exclusive'})))
        error (strcat ("string.eraseBetween: BOUNDARIES must be", ...
                       " 'inclusive' or 'exclusive'."));
      endif
      inclusive = strcmpi (bounds, 'inclusive');

      ## Normalize text boundaries to cell arrays of character vectors.  Route
      ## char/cellstr through the constructor, which keeps trailing whitespace
      ## that bare 'cellstr' would deblank.
      if (! posMode)
        if (isa (start, 'string'))
          start = cellstr (start);
        else
          start = cellstr (string (start));
        endif
        if (isa (stop, 'string'))
          stop = cellstr (stop);
        else
          stop = cellstr (string (stop));
        endif
      endif

      ## Broadcast scalar boundaries; otherwise sizes must match STR
      sz = size (this.strs);
      [start, errmsg] = eb_expand (start, sz, 'START');
      if (! isempty (errmsg))
        error ("string.eraseBetween: %s", errmsg);
      endif
      [stop, errmsg] = eb_expand (stop, sz, 'STOP');
      if (! isempty (errmsg))
        error ("string.eraseBetween: %s", errmsg);
      endif

      out = this;
      cstr = this.strs;
      for k = 1:numel (cstr)
        if (this.isMissing(k))
          continue;
        endif
        if (posMode)
          cstr{k} = eb_between (cstr{k}, start(k), stop(k), true, inclusive);
        else
          cstr{k} = eb_between (cstr{k}, start{k}, stop{k}, false, inclusive);
        endif
      endfor
      out.strs = cstr;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{newstr} =} extract (@var{str}, @var{pat})
    ## @deftypefnx {string} {@var{newstr} =} extract (@var{str}, @var{pos})
    ##
    ## Extract substrings from a string array.
    ##
    ## @code{@var{newstr} = extract (@var{str}, @var{pat})} returns the
    ## substrings of @var{str} that match @var{pat}.  @var{pat} can be a string
    ## array, a character vector, or a cell array of character vectors; when it
    ## contains more than one piece of text, any of them may match.  Within each
    ## element of @var{str} the matches are found left to right and do not
    ## overlap; where alternatives match at the same position, the first listed
    ## in @var{pat} is taken.
    ##
    ## The matches of an element occupy a row of @var{newstr}, so the matches run
    ## along the second dimension and every element of @var{str} must yield the
    ## same number of matches.  For a string scalar with @var{n} matches,
    ## @var{newstr} is @code{1x@var{n}}; for a non-scalar @var{str}, @var{newstr}
    ## has one row per element (taken in column-major order) and one column per
    ## match.  Elements with no match, including missing values, are treated as
    ## having zero matches.
    ##
    ## @code{@var{newstr} = extract (@var{str}, @var{pos})} returns the single
    ## character located at position @var{pos} in each element of @var{str}.
    ## @var{pos} must be a positive integer that is either a scalar, applied to
    ## every element, or the same size as @var{str}, applied element-wise.  In
    ## this syntax @var{newstr} has the same size as @var{str} and missing values
    ## are preserved.
    ##
    ## @end deftypefn
    function out = extract (this, pat)
      if (nargin < 2)
        error ("string.extract: not enough input arguments.");
      endif

      ## Numeric POS selects a single character per element; text PAT extracts
      ## substring matches.
      if (isnumeric (pat))
        pos = pat;
        sz = size (this.strs);
        if (isscalar (pos))
          pos = repmat (pos, sz);
        elseif (! isequal (size (pos), sz))
          error ("string.extract: POS must be scalar or the same size as STR.");
        endif
        if (any (pos(:) != fix (pos(:))) || any (pos(:) < 1))
          error ("string.extract: POS must be a positive integer.");
        endif
        out = this;
        cstr = this.strs;
        for k = 1:numel (cstr)
          if (this.isMissing(k))
            continue;
          endif
          cp = str2cp (cstr{k});
          if (pos(k) > numel (cp))
            error ("string.extract: POS exceeds the length of the string.");
          endif
          cstr{k} = cp2str (cp(pos(k)));
        endfor
        out.strs = cstr;
        return;
      endif

      ## Text pattern form
      if (isa (pat, 'string'))
        pats = cellstr (pat);
      elseif (ischar (pat) || iscellstr (pat))
        ## Route char/cellstr through the constructor, which keeps trailing
        ## whitespace that bare 'cellstr' would deblank
        pats = cellstr (string (pat));
      else
        error (strcat ("string.extract: PAT must be a string array, a", ...
                       " character vector, or a cell array of character", ...
                       " vectors."));
      endif

      ## Collect the matches of each element (missing elements yield none)
      ne = numel (this.strs);
      matches = cell (ne, 1);
      counts = zeros (ne, 1);
      for k = 1:ne
        if (this.isMissing(k))
          matches{k} = {};
        else
          matches{k} = extract_matches (this.strs{k}, pats);
        endif
        counts(k) = numel (matches{k});
      endfor

      ## Matches run along the columns, so every element must match the same
      ## number of times to form a rectangular string array.
      if (ne > 0 && any (counts != counts(1)))
        error (strcat ("string.extract: all elements of STR must have the", ...
                       " same number of matches."));
      endif
      ncol = 0;
      if (ne > 0)
        ncol = counts(1);
      endif
      outc = cell (ne, ncol);
      for e = 1:ne
        outc(e,:) = matches{e};
      endfor
      out = string (outc);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{newstr} =} extractAfter (@var{str}, @var{pat})
    ## @deftypefnx {string} {@var{newstr} =} extractAfter (@var{str}, @var{pos})
    ##
    ## Extract the substring after a position or pattern.
    ##
    ## @code{@var{newstr} = extractAfter (@var{str}, @var{pat})} returns, for
    ## each element of @var{str}, the part of the text that follows the first
    ## occurrence of @var{pat}, excluding @var{pat} itself.  @var{pat} can be a
    ## string array, a character vector, or a cell array of character vectors,
    ## and must either be a scalar, applied to every element of @var{str}, or be
    ## of the same size as @var{str} and applied element-wise.  If @var{pat} is
    ## not found in an element, the corresponding element of @var{newstr} is a
    ## missing value.
    ##
    ## @code{@var{newstr} = extractAfter (@var{str}, @var{pos})} returns the part
    ## of each element of @var{str} that follows the character position
    ## @var{pos}, that is, from @code{@var{pos}+1} to the end.  @var{pos} must be
    ## a positive integer that is either a scalar or the same size as @var{str}.
    ##
    ## @var{newstr} is a string array of the same size as @var{str}.  Missing
    ## values in @var{str} are preserved.
    ##
    ## @end deftypefn
    function out = extractAfter (this, pat)
      if (nargin < 2)
        error ("string.extractAfter: not enough input arguments.");
      endif
      [out, errmsg] = extract_side (this, pat, true);
      if (! isempty (errmsg))
        error ("string.extractAfter: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{newstr} =} extractBefore (@var{str}, @var{pat})
    ## @deftypefnx {string} {@var{newstr} =} extractBefore (@var{str}, @var{pos})
    ##
    ## Extract the substring before a position or pattern.
    ##
    ## @code{@var{newstr} = extractBefore (@var{str}, @var{pat})} returns, for
    ## each element of @var{str}, the part of the text that precedes the first
    ## occurrence of @var{pat}, excluding @var{pat} itself.  @var{pat} can be a
    ## string array, a character vector, or a cell array of character vectors,
    ## and must either be a scalar, applied to every element of @var{str}, or be
    ## of the same size as @var{str} and applied element-wise.  If @var{pat} is
    ## not found in an element, the corresponding element of @var{newstr} is a
    ## missing value.
    ##
    ## @code{@var{newstr} = extractBefore (@var{str}, @var{pos})} returns the
    ## part of each element of @var{str} that precedes the character position
    ## @var{pos}, that is, from the start up to @code{@var{pos}-1}.  @var{pos}
    ## must be a positive integer that is either a scalar or the same size as
    ## @var{str}.
    ##
    ## @var{newstr} is a string array of the same size as @var{str}.  Missing
    ## values in @var{str} are preserved.
    ##
    ## @end deftypefn
    function out = extractBefore (this, pat)
      if (nargin < 2)
        error ("string.extractBefore: not enough input arguments.");
      endif
      [out, errmsg] = extract_side (this, pat, false);
      if (! isempty (errmsg))
        error ("string.extractBefore: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{newstr} =} extractBetween (@var{str}, @var{startPat}, @var{endPat})
    ## @deftypefnx {string} {@var{newstr} =} extractBetween (@var{str}, @var{startPos}, @var{endPos})
    ## @deftypefnx {string} {@var{newstr} =} extractBetween (@dots{}, @qcode{"Boundaries"}, @var{bounds})
    ##
    ## Extract the substrings between start and end boundaries.
    ##
    ## @code{@var{newstr} = extractBetween (@var{str}, @var{startPat}, @var{endPat})}
    ## returns the text that occurs between the substrings @var{startPat} and
    ## @var{endPat} in each element of @var{str}.  @var{startPat} and
    ## @var{endPat} can be string arrays, character vectors, or cell arrays of
    ## character vectors.  Within each element the boundary pairs are matched
    ## from left to right and do not overlap, so an element may yield several
    ## substrings; these run along the second dimension, and every element of
    ## @var{str} must yield the same number of matches.  For a string scalar with
    ## @var{n} matches, @var{newstr} is @code{1x@var{n}}; for a non-scalar
    ## @var{str}, @var{newstr} has one row per element (taken in column-major
    ## order) and one column per match.  Elements with no match, including
    ## missing values, are treated as having zero matches.
    ##
    ## @code{@var{newstr} = extractBetween (@var{str}, @var{startPos}, @var{endPos})}
    ## returns the substring between the character positions @var{startPos} and
    ## @var{endPos}, inclusive of the characters at those positions.  This syntax
    ## extracts a single substring per element, so @var{newstr} has the same size
    ## as @var{str}.
    ##
    ## @code{@var{newstr} = extractBetween (@dots{}, @qcode{"Boundaries"}, @var{bounds})}
    ## specifies whether the boundaries are included in or excluded from the
    ## extracted text.  @var{bounds} can be either @qcode{"inclusive"} or
    ## @qcode{"exclusive"}.  When boundaries are given as substrings, the default
    ## is @qcode{"exclusive"} and the boundary substrings are not included; when
    ## given as positions, the default is @qcode{"inclusive"} and the characters
    ## at those positions are included.
    ##
    ## @var{startPat}/@var{endPat} and @var{startPos}/@var{endPos} must either be
    ## scalars, applied to every element of @var{str}, or be of the same size as
    ## @var{str}.  Missing values in @var{str} are preserved.
    ##
    ## @end deftypefn
    function out = extractBetween (this, start, stop, varargin)
      if (nargin < 3)
        error ("string.extractBetween: not enough input arguments.");
      endif

      istxt = @(x) ischar (x) || iscellstr (x) || isa (x, 'string');
      if (isnumeric (start) && isnumeric (stop))
        posMode = true;
        dfBounds = 'inclusive';
      elseif (istxt (start) && istxt (stop))
        posMode = false;
        dfBounds = 'exclusive';
      else
        error (strcat ("string.extractBetween: START and STOP must be", ...
                       " either both numeric positions or both text", ...
                       " patterns."));
      endif

      [bounds, rem] = parsePairedArguments ({'Boundaries'}, {dfBounds}, varargin);
      if (! isempty (rem))
        error ("string.extractBetween: invalid optional arguments.");
      endif
      if (isa (bounds, 'string'))
        bounds = char (bounds);
      endif
      if (! ischar (bounds) || ! any (strcmpi (bounds, {'inclusive', 'exclusive'})))
        error (strcat ("string.extractBetween: BOUNDARIES must be", ...
                       " 'inclusive' or 'exclusive'."));
      endif
      inclusive = strcmpi (bounds, 'inclusive');

      sz = size (this.strs);
      ne = numel (this.strs);

      if (posMode)
        [start, errmsg] = eb_expand (start, sz, 'START');
        if (! isempty (errmsg))
          error ("string.extractBetween: %s", errmsg);
        endif
        [stop, errmsg] = eb_expand (stop, sz, 'STOP');
        if (! isempty (errmsg))
          error ("string.extractBetween: %s", errmsg);
        endif
        ## One span per element, so the output matches the size of STR
        out = this;
        cstr = this.strs;
        for k = 1:ne
          if (this.isMissing(k))
            continue;
          endif
          cstr{k} = eb_span (cstr{k}, start(k), stop(k), inclusive);
        endfor
        out.strs = cstr;
        return;
      endif

      ## Pattern mode: each element may contribute several substrings.  Route
      ## char/cellstr through the constructor, which keeps trailing whitespace
      ## that bare 'cellstr' would deblank.
      if (isa (start, 'string'))
        start = cellstr (start);
      else
        start = cellstr (string (start));
      endif
      if (isa (stop, 'string'))
        stop = cellstr (stop);
      else
        stop = cellstr (string (stop));
      endif
      [start, errmsg] = eb_expand (start, sz, 'START');
      if (! isempty (errmsg))
        error ("string.extractBetween: %s", errmsg);
      endif
      [stop, errmsg] = eb_expand (stop, sz, 'STOP');
      if (! isempty (errmsg))
        error ("string.extractBetween: %s", errmsg);
      endif

      matches = cell (ne, 1);
      counts = zeros (ne, 1);
      for k = 1:ne
        if (this.isMissing(k))
          matches{k} = {};
        else
          matches{k} = extract_between_matches (this.strs{k}, start{k}, ...
                                                stop{k}, inclusive);
        endif
        counts(k) = numel (matches{k});
      endfor
      if (ne > 0 && any (counts != counts(1)))
        error (strcat ("string.extractBetween: all elements of STR must", ...
                       " have the same number of matches."));
      endif
      ncol = 0;
      if (ne > 0)
        ncol = counts(1);
      endif
      outc = cell (ne, ncol);
      for e = 1:ne
        outc(e,:) = matches{e};
      endfor
      out = string (outc);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{newstr} =} insertAfter (@var{str}, @var{pat}, @var{newtext})
    ## @deftypefnx {string} {@var{newstr} =} insertAfter (@var{str}, @var{pos}, @var{newtext})
    ##
    ## Insert text after a pattern or position.
    ##
    ## @code{@var{newstr} = insertAfter (@var{str}, @var{pat}, @var{newtext})}
    ## inserts the text @var{newtext} into each element of @var{str} after every
    ## non-overlapping occurrence of the substring @var{pat}.  @var{pat} can be a
    ## string array, a character vector, or a cell array of character vectors,
    ## and must either be a scalar, applied to every element of @var{str}, or be
    ## of the same size as @var{str} and applied element-wise.  If @var{pat} is
    ## not found in an element, that element is returned unchanged.
    ##
    ## @code{@var{newstr} = insertAfter (@var{str}, @var{pos}, @var{newtext})}
    ## inserts @var{newtext} after the character position @var{pos}, that is,
    ## between the characters at positions @var{pos} and @code{@var{pos}+1}.
    ## @var{pos} must be a positive integer not exceeding the length of the
    ## corresponding element of @var{str}, and must be either a scalar or the
    ## same size as @var{str}.
    ##
    ## @var{newtext} can be a string array, a character vector, or a cell array
    ## of character vectors, and must be either a scalar, inserted at every
    ## position, or of the same size as @var{str}.  @var{newstr} is a string
    ## array of the same size as @var{str}.  Missing values in @var{str} are
    ## preserved, and a missing value in @var{newtext} makes the corresponding
    ## element of @var{newstr} missing.
    ##
    ## @end deftypefn
    function out = insertAfter (this, pat, new)
      if (nargin < 3)
        error ("string.insertAfter: not enough input arguments.");
      endif
      [out, errmsg] = insert_side (this, pat, new, true);
      if (! isempty (errmsg))
        error ("string.insertAfter: %s", errmsg);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {string} {@var{newstr} =} insertBefore (@var{str}, @var{pat}, @var{newtext})
    ## @deftypefnx {string} {@var{newstr} =} insertBefore (@var{str}, @var{pos}, @var{newtext})
    ##
    ## Insert text before a pattern or position.
    ##
    ## @code{@var{newstr} = insertBefore (@var{str}, @var{pat}, @var{newtext})}
    ## inserts the text @var{newtext} into each element of @var{str} before every
    ## non-overlapping occurrence of the substring @var{pat}.  @var{pat} can be a
    ## string array, a character vector, or a cell array of character vectors,
    ## and must either be a scalar, applied to every element of @var{str}, or be
    ## of the same size as @var{str} and applied element-wise.  If @var{pat} is
    ## not found in an element, that element is returned unchanged.
    ##
    ## @code{@var{newstr} = insertBefore (@var{str}, @var{pos}, @var{newtext})}
    ## inserts @var{newtext} before the character position @var{pos}, that is,
    ## between the characters at positions @code{@var{pos}-1} and @var{pos}.
    ## @var{pos} must be a positive integer not exceeding the length of the
    ## corresponding element of @var{str}, and must be either a scalar or the
    ## same size as @var{str}.
    ##
    ## @var{newtext} can be a string array, a character vector, or a cell array
    ## of character vectors, and must be either a scalar, inserted at every
    ## position, or of the same size as @var{str}.  @var{newstr} is a string
    ## array of the same size as @var{str}.  Missing values in @var{str} are
    ## preserved, and a missing value in @var{newtext} makes the corresponding
    ## element of @var{newstr} missing.
    ##
    ## @end deftypefn
    function out = insertBefore (this, pat, new)
      if (nargin < 3)
        error ("string.insertBefore: not enough input arguments.");
      endif
      [out, errmsg] = insert_side (this, pat, new, false);
      if (! isempty (errmsg))
        error ("string.insertBefore: %s", errmsg);
      endif
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
    ## @deftypefn {string} {@var{newstr} =} plus (@var{str1}, @var{str2})
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
    ## @deftypefnx {string} {@dots{} =} unique (@dots{}, @var{occurrence})
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
    ## @code{@dots{} = unique (@dots{}, @var{occurrence})} also specifies the
    ## which index is returned in @var{ixA}, where there are repeated values or
    ## rows (if opted) in the input categorical array.  @var{occurrence} may be
    ## either @qcode{'first'}, which is the default and returns the index of the
    ## first occurrence of each unique value, or @qcode{'last'}, in which case
    ## the last occurrence of each unique value is returned.
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
      ## Handle 'setOrder' and 'occurrence' options
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
      ## For categorical, datetime, and duration arrays being present in the
      ## input arguments, call their constructor for the first input array and
      ## forward all input to their respective concatenation method.
      is_datetime = cellfun ('isdatetime', varargin);
      is_duration = cellfun ('isduration', varargin);
      if (any (cellfun ('iscategorical', varargin)))
        varargin{1} = categorical (varargin{1});
        out = cat (dim, varargin{:});
        return;
      elseif (any (is_datetime))
        idx = find (is_datetime, 1);
        tmp = varargin{idx};
        varargin{1} = datetime (varargin{1}, 'Format', tmp.Format);
        out = cat (dim, varargin{:});
        return;
      elseif (any (cellfun ('isduration', varargin)))
        idx = find (is_duration, 1);
        tmp = varargin{idx};
        varargin{1} = duration (varargin{1}, 'Format', tmp.Format);
        out = cat (dim, varargin{:});
        return;
      endif
      ## For everything else, try converting it a to string array
      idx = find (cellfun (@(x) ! isstring (x), varargin));
      if (! isempty (idx))
        for i = idx
          varargin{i} = string (varargin{i});
        endfor
      endif
      ## Concatenate strings
      out = string;
      tmp = cellfun (@(obj) obj.strs, varargin, 'UniformOutput', false);
      out.strs = cat (dim, tmp{:});
      tmp = cellfun (@(obj) obj.isMissing, varargin, 'UniformOutput', false);
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
    ## @var{n} is a vector, it must have the same elements as @var{A}, in which
    ## case it specifies the number of times to repeat each corresponding
    ## element of @var{A}.
    ##
    ## @code{@var{B} = repelem (@var{A}, @var{d1}, @dots{}, @var{dN})} returns
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
    ## Reshape string array.
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

################################################################################
##        ** Overloaded methods for duration and categorical classes **       ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'colon'            'linspace'         'intersect'        'setdiff'         ##
## 'setxor'           'union'            'isequal'          'isequaln'        ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Overload colon for duration support
    function R = colon (varargin)
      ## Get properties from first duration input
      idx = find (cellfun ('isduration', varargin), 1);
      if (isempty (idx))
        error ("string.colon: unsupported input types.");
      endif
      A = varargin{idx};
      ## Convert first input (string) to duration
      varargin{1} = duration (varargin{1}, 'Format', A.Format);
      ## Call duration overloaded method
      R = colon (varargin{:});
    endfunction

    ## Overload linspace for duration support
    function R = linspace (A, B, n = 100)
      ## Check for duration input
      if (! isduration (B))
        error ("string.linspace: unsupported input types.");
      endif
      ## Convert first input (string) to duration
      A = duration (A, 'Format', B.Format);
      ## Call duration overloaded method
      R = linspace (A, B, n);
    endfunction

    ## Overload intersect for duration or categorical support
    function [C, ixA, ixB] = intersect (A, B, varargin)
      ## Check for duration input
      if (! (isduration (B) || iscategorical (B)))
        error ("string.intersect: unsupported input types.");
      endif
      ## Convert first input (string) to cellstr
      A = cellstr (A);
      ## Call overloaded method
      [C, ixA, ixB] = intersect (A, B, varargin{:});
    endfunction

    ## Overload setdiff for duration or categorical support
    function [C, index] = setdiff (A, B, varargin)
      ## Check for duration input
      if (! (isduration (B) || iscategorical (B)))
        error ("string.setdiff: unsupported input types.");
      endif
      ## Convert first input (string) to cellstr
      A = cellstr (A);
      ## Call duration overloaded method
      [C, index] = setdiff (A, B, varargin{:});
    endfunction

    ## Overload setxor for duration or categorical support
    function [C, ixA, ixB] = setxor (A, B, varargin)
      ## Check for duration input
      if (! (isduration (B) || iscategorical (B)))
        error ("string.setxor: unsupported input types.");
      endif
      ## Convert first input (string) to cellstr
      A = cellstr (A);
      ## Call duration overloaded method
      [C, ixA, ixB] = setxor (A, B, varargin{:});
    endfunction

    ## Overload union for duration or categorical support
    function [C, ixA, ixB] = union (A, B, varargin)
      ## Check for duration input
      if (! (isduration (B) || iscategorical (B)))
        error ("string.union: unsupported input types.");
      endif
      ## Convert first input (string) to cellstr
      A = cellstr (A);
      ## Call duration overloaded method
      [C, ixA, ixB] = union (A, B, varargin{:});
    endfunction

    ## Overload isequal for categorical support
    function TF = isequal (varargin)
      ## Check for categorical input
      idx = find (cellfun ('iscategorical', varargin), 1);
      if (isempty (idx))
        if (any (cellfun (@(x) ! isa (x, 'string'), varargin)))
          error ("string.isequal: unsupported input types.");
        endif
        tmp1 = cellfun (@(x) x.strs, varargin, 'UniformOutput', false);
        tmp2 = cellfun (@(x) x.isMissing, varargin, 'UniformOutput', false);
        if (any (cellfun (@(x) any (x, 'all'), tmp2)))
          TF = false;
        else
          TF = isequal (tmp1{:}) && isequal (tmp2{:});
        endif
      else
        ## Convert first input (string) to categorical
        varargin{1} = categorical (varargin{1});
        ## Call categorical overloaded method
        TF = isequal (varargin{:});
      endif
    endfunction

    ## Overload isequaln for categorical support
    function TF = isequaln (varargin)
      ## Check for categorical input
      idx = find (cellfun ('iscategorical', varargin), 1);
      if (isempty (idx))
        if (any (cellfun (@(x) ! isa (x, 'string'), varargin)))
          error ("string.isequaln: unsupported input types.");
        endif
        tmp1 = cellfun (@(x) x.strs, varargin, 'UniformOutput', false);
        tmp2 = cellfun (@(x) x.isMissing, varargin, 'UniformOutput', false);
        TF = isequal (tmp1{:}) && isequal (tmp2{:});
      else
        ## Convert first input (string) to categorical
        varargin{1} = categorical (varargin{1});
        ## Call categorical overloaded method
        TF = isequaln (varargin{:});
      endif
    endfunction

  endmethods

  methods (Access = private)

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.strs = this.strs(varargin{:});
      this.isMissing = this.isMissing(varargin{:});
    endfunction

    ## Shared implementation of extractAfter (AFTER true) and extractBefore
    ## (AFTER false).  PAT may be numeric positions or text boundaries; an
    ## unmatched text boundary yields a missing value.  On invalid input returns
    ## a non-empty ERRMSG describing the fault, leaving OUT unchanged, so the
    ## calling method can emit the error under its own name.
    function [out, errmsg] = extract_side (this, pat, after)
      out = this;
      errmsg = '';
      cstr = this.strs;
      isMiss = this.isMissing;
      sz = size (cstr);

      if (isnumeric (pat))
        [pos, errmsg] = eb_expand (pat, sz, 'POS');
        if (! isempty (errmsg))
          return;
        endif
        if (any (pos(:) != fix (pos(:))) || any (pos(:) < 1))
          errmsg = "POS must be a positive integer.";
          return;
        endif
        for k = 1:numel (cstr)
          if (isMiss(k))
            continue;
          endif
          cp = str2cp (cstr{k});
          if (pos(k) > numel (cp))
            errmsg = "POS exceeds the length of the string.";
            return;
          endif
          if (after)
            cstr{k} = cp2str (cp((pos(k) + 1):end));
          else
            cstr{k} = cp2str (cp(1:(pos(k) - 1)));
          endif
        endfor
      else
        if (isa (pat, 'string'))
          pat = cellstr (pat);
        elseif (ischar (pat) || iscellstr (pat))
          ## Route char/cellstr through the constructor, which keeps trailing
          ## whitespace that bare 'cellstr' would deblank
          pat = cellstr (string (pat));
        else
          errmsg = strcat ("PAT must be a string array, a character", ...
                           " vector, or a cell array of character vectors.");
          return;
        endif
        [pat, errmsg] = eb_expand (pat, sz, 'PAT');
        if (! isempty (errmsg))
          return;
        endif
        for k = 1:numel (cstr)
          if (isMiss(k))
            continue;
          endif
          s = cstr{k};
          p = pat{k};
          i = [];
          if (! isempty (p))
            i = strfind (s, p);
          endif
          if (isempty (i))
            cstr{k} = '';            # PAT not found -> missing
            isMiss(k) = true;
            continue;
          endif
          i = i(1);
          if (after)
            cstr{k} = s((i + numel (p)):end);
          else
            cstr{k} = s(1:(i - 1));
          endif
        endfor
      endif

      out.strs = cstr;
      out.isMissing = isMiss;
    endfunction

    ## Shared implementation of insertAfter (AFTER true) and insertBefore
    ## (AFTER false).  PAT may be numeric positions or text boundaries; text
    ## boundaries are matched at every non-overlapping occurrence (an unmatched
    ## boundary leaves the element unchanged).  NEW is the text to insert,
    ## scalar or the same size as STR; a missing NEW makes the corresponding
    ## element missing.  On invalid input returns a non-empty ERRMSG, leaving
    ## OUT unchanged, so the calling method can emit the error under its own name.
    function [out, errmsg] = insert_side (this, pat, new, after)
      out = this;
      cstr = this.strs;
      isMiss = this.isMissing;
      sz = size (cstr);
      [newc, newMiss, errmsg] = norm_new (new, sz);
      if (! isempty (errmsg))
        return;
      endif

      if (isnumeric (pat))
        [pos, errmsg] = eb_expand (pat, sz, 'POS');
        if (! isempty (errmsg))
          return;
        endif
        if (any (pos(:) != fix (pos(:))) || any (pos(:) < 1))
          errmsg = "POS must be a positive integer.";
          return;
        endif
        for k = 1:numel (cstr)
          if (isMiss(k))
            continue;
          endif
          if (newMiss(k))
            cstr{k} = '';
            isMiss(k) = true;
            continue;
          endif
          cp = str2cp (cstr{k});
          if (pos(k) > numel (cp))
            errmsg = "POS exceeds the length of the string.";
            return;
          endif
          ncp = str2cp (newc{k});
          if (after)
            cstr{k} = cp2str ([cp(1:pos(k)), ncp, cp((pos(k) + 1):end)]);
          else
            cstr{k} = cp2str ([cp(1:(pos(k) - 1)), ncp, cp(pos(k):end)]);
          endif
        endfor
      else
        if (isa (pat, 'string'))
          pat = cellstr (pat);
        elseif (ischar (pat) || iscellstr (pat))
          ## Route char/cellstr through the constructor, which keeps trailing
          ## whitespace that bare 'cellstr' would deblank
          pat = cellstr (string (pat));
        else
          errmsg = strcat ("PAT must be a string array, a character", ...
                           " vector, or a cell array of character vectors.");
          return;
        endif
        [pat, errmsg] = eb_expand (pat, sz, 'PAT');
        if (! isempty (errmsg))
          return;
        endif
        for k = 1:numel (cstr)
          if (isMiss(k))
            continue;
          endif
          if (newMiss(k))
            cstr{k} = '';
            isMiss(k) = true;
            continue;
          endif
          p = pat{k};
          if (isempty (p))
            continue;                # an empty boundary matches nothing
          endif
          if (after)
            cstr{k} = strrep (cstr{k}, p, [p, newc{k}]);
          else
            cstr{k} = strrep (cstr{k}, p, [newc{k}, p]);
          endif
        endfor
      endif

      out.strs = cstr;
      out.isMissing = isMiss;
    endfunction

  endmethods

endclassdef

## Broadcast a scalar boundary argument to the size of STR, or verify that it
## already matches.  Works for both numeric arrays and cell arrays of patterns.
## On a size mismatch returns a non-empty ERRMSG naming the offending argument;
## the calling method emits the error under its own name.
function [arg, errmsg] = eb_expand (arg, sz, name)
  errmsg = '';
  if (numel (arg) == 1)
    arg = repmat (arg, sz);
  elseif (! isequal (size (arg), sz))
    errmsg = sprintf ("%s must be scalar or the same size as STR.", name);
  endif
endfunction

## Erase the content of a single character vector S between two boundaries.
## When ISPOS is true, A and B are numeric character positions; otherwise they
## are the start and end boundary substrings.  INCLUSIVE selects whether the
## boundaries themselves are erased.
function s = eb_between (s, a, b, isPos, inclusive)
  if (isPos)
    cp = str2cp (s);
    n = numel (cp);
    if (a != fix (a) || b != fix (b) || a < 1 || a > b || b > n)
      error ("string.eraseBetween: position indices out of range.");
    endif
    if (inclusive)
      cp(a:b) = [];
    elseif (a + 1 <= b - 1)
      cp((a + 1):(b - 1)) = [];
    endif
    s = cp2str (cp);
  else
    if (isempty (a) || isempty (b))
      return;                       # an empty boundary matches nothing
    endif
    i = strfind (s, a);
    if (isempty (i))
      return;                       # start boundary not found
    endif
    i = i(1);
    aEnd = i + numel (a) - 1;
    j = strfind (s, b);
    j = j(j >= aEnd + 1);           # end boundary must follow the start match
    if (isempty (j))
      return;                       # end boundary not found
    endif
    j = j(1);
    if (inclusive)
      s(i:(j + numel (b) - 1)) = [];
    elseif (aEnd + 1 <= j - 1)
      s((aEnd + 1):(j - 1)) = [];
    endif
  endif
endfunction

## Normalize the inserted-text argument NEW to a cell array of character vectors
## NEWC plus a logical missing mask NEWMISS, each broadcast to the size SZ of
## STR.  Mirrors the boundary-broadcasting rules of the insert/extract family.
## On invalid input returns a non-empty ERRMSG; the calling method emits the
## error under its own name.
function [newc, newMiss, errmsg] = norm_new (new, sz)
  newc = {};
  newMiss = [];
  errmsg = '';
  if (isa (new, 'string'))
    newc = cellstr (new);
    newMiss = ismissing (new);
  elseif (ischar (new) || iscellstr (new))
    ## Route char/cellstr through the constructor, which keeps trailing
    ## whitespace that bare 'cellstr' would deblank
    newc = cellstr (string (new));
    newMiss = false (size (newc));
  else
    errmsg = strcat ("NEW must be a string array, a character", ...
                     " vector, or a cell array of character vectors.");
    return;
  endif
  [newc, errmsg] = eb_expand (newc, sz, 'NEW');
  if (! isempty (errmsg))
    return;
  endif
  newMiss = eb_expand (newMiss, sz, 'NEW');
endfunction

## Extract a single span of S between positions A and B.  INCLUSIVE selects
## whether the characters at A and B are kept.
function s = eb_span (s, a, b, inclusive)
  cp = str2cp (s);
  n = numel (cp);
  if (a != fix (a) || b != fix (b) || a < 1 || a > b || b > n)
    error ("string.extractBetween: position indices out of range.");
  endif
  if (inclusive)
    s = cp2str (cp(a:b));
  elseif (a + 1 <= b - 1)
    s = cp2str (cp((a + 1):(b - 1)));
  else
    s = '';
  endif
endfunction

## Collect the non-overlapping substrings of S that lie between successive START
## and END boundary substrings, scanning left to right.  INCLUSIVE selects
## whether the boundary substrings themselves are included.
function m = extract_between_matches (s, sp, ep, inclusive)
  m = {};
  if (isempty (sp) || isempty (ep))
    return;
  endif
  i = 1;
  n = numel (s);
  while (i <= n)
    si = strfind (s(i:end), sp);
    if (isempty (si))
      break;
    endif
    si = i + si(1) - 1;                   # absolute start of START match
    aEnd = si + numel (sp) - 1;
    ei = strfind (s((aEnd + 1):end), ep);
    if (isempty (ei))
      break;
    endif
    ei = aEnd + ei(1);                    # absolute start of END match
    if (inclusive)
      m{end+1} = s(si:(ei + numel (ep) - 1));
    elseif (aEnd + 1 <= ei - 1)
      m{end+1} = s((aEnd + 1):(ei - 1));
    else
      m{end+1} = '';
    endif
    i = ei + numel (ep);                  # resume past the END match
  endwhile
endfunction

## Convert a character vector to/from a row of Unicode code points (uint32), so
## that character positions index whole characters rather than UTF-8 bytes.
function cp = str2cp (s)
  cp = typecast (unicode2native (s, 'UTF-32LE'), 'uint32');
endfunction

function s = cp2str (cp)
  if (isempty (cp))
    s = '';
  else
    s = native2unicode (typecast (uint32 (cp), 'uint8'), 'UTF-32LE');
  endif
endfunction

## Collect the non-overlapping matches of PATS in the character vector S,
## scanning left to right.  At each position the patterns are tried in order and
## the first that matches is taken; the scan then resumes past that match.
function m = extract_matches (s, pats)
  m = {};
  i = 1;
  n = numel (s);
  while (i <= n)
    hit = 0;
    for p = 1:numel (pats)
      L = numel (pats{p});
      if (L > 0 && i + L - 1 <= n && strncmp (s(i:i+L-1), pats{p}, L))
        hit = L;
        m{end+1} = pats{p};
        break;
      endif
    endfor
    if (hit > 0)
      i += hit;
    else
      i += 1;
    endif
  endwhile
endfunction

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

## Split a format specifier into an ordered list of literal and operator tokens.
## Each token is a scalar struct with the fields:
##   'type' : 'lit' for literal text or 'op' for a conversion operator
##   'text' : the corresponding piece of text from the format specifier
##   'nval' : number of values an operator consumes (1, plus 1 per '*'), or 0
## The '%%' literal is collapsed to a single '%' and kept as a 'lit' token.
function tok = compose_tokenize (fmt)
  pat = '%%|%[-+ #0]*(?:\d+|\*)?(?:\.(?:\d+|\*))?[diouxXeEfgGcs]';
  [mt, sp] = regexp (fmt, pat, 'match', 'split');
  tok = {};
  for k = 1:numel (mt)
    if (! isempty (sp{k}))
      tok{end+1} = struct ('type', 'lit', 'text', sp{k}, 'nval', 0);
    endif
    if (strcmp (mt{k}, '%%'))
      tok{end+1} = struct ('type', 'lit', 'text', '%', 'nval', 0);
    else
      nstar = numel (strfind (mt{k}, '*'));
      tok{end+1} = struct ('type', 'op', 'text', mt{k}, 'nval', 1 + nstar);
    endif
  endfor
  if (! isempty (sp{end}))
    tok{end+1} = struct ('type', 'lit', 'text', sp{end}, 'nval', 0);
  endif
endfunction

## Format a single string from a token list and a cell of input values.  Each
## operator consumes its values in order; operators left without a corresponding
## value are emitted unchanged.  Literal text has its escape sequences translated
## following the same rules as the 'sprintf' function.
function out = compose_apply (tok, vals)
  parts = cell (1, numel (tok));
  vi = 1;
  nv = numel (vals);
  for t = 1:numel (tok)
    if (strcmp (tok{t}.type, 'op') && vi + tok{t}.nval - 1 <= nv)
      args = vals(vi:vi + tok{t}.nval - 1);
      parts{t} = sprintf (tok{t}.text, args{:});
      vi += tok{t}.nval;
    elseif (strcmp (tok{t}.type, 'op'))
      parts{t} = tok{t}.text;
    elseif (strcmp (tok{t}.text, '%'))
      parts{t} = '%';
    else
      parts{t} = do_string_escapes (tok{t}.text);
    endif
  endfor
  out = ['', parts{:}];
endfunction
