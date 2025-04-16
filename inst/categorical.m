## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
##
## This file is part of the statistics package for GNU Octave.
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

classdef categorical
  ## -*- texinfo -*-
  ## @deftp {Class} categorical
  ##
  ## Array representing categorical data.
  ##
  ## A @code{categorical} array represents an array of values that correspond
  ## to a finite set of discrete categories, which can be either ordinal (having
  ## a mathematical ordering) or nominal.  It is an efficient way to define
  ## groups of rows in a table or to other types of variables.
  ##
  ## Each @code{categorical} array stores the list of categories as a cell array
  ## of character vectors and a numeric array of @qcode{uint16} type as indices
  ## to the categories.  The categorical array may also store elements of
  ## undefined categorical values, which represent the absense of a given
  ## category and correspond to the @qcode{NaN} value for numeric arrays or in
  ## general to the missing value for other data types.
  ##
  ## @code{categorical} arrays do not have any public properties, which can be
  ## indexed by using dot notation similarily to structures.  However, there are
  ## several methods which can be used to modify their categories once they are
  ## constructed.
  ##
  ## @end deftp

  properties (SetAccess = private, Hidden)
    ## Category Names
    cats = {}
    ## Indices to categories
    code = uint16 ([])
    ## <undefined> elements
    isMissing = []
    ## Flag for ordinal categories
    isOrdinal = false
    ## Flag for protecting category list
    isProtected = false
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ('%s =\n', in_name);
      endif
      __disp__ (this, 'categorical', in_name);
    endfunction

    ## Custom display
    function disp (this)
      __disp__ (this, 'categorical');
    endfunction

  endmethods

################################################################################
##                ** Create and convert 'categorical' type **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'categorical'      'dispstrings'      'cellstr'          'double'          ##
## 'single'           'int64'            'int32'            'int16'           ##
## 'int8'             'uint64'           'uint32'           'uint16'          ##
## 'uint8'                                                                    ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} categorical (@var{A})
    ## @deftypefnx {categorical} {@var{C} =} categorical (@var{A}, @var{valueset})
    ## @deftypefnx {categorical} {@var{C} =} categorical (@var{A}, @var{valueset}, @var{catnames})
    ## @deftypefnx {categorical} {@var{C} =} categorical (dots{}, @var{Name}, @var{Value})
    ##
    ## Create a new array of categorical values.
    ##
    ## @var{A} is the array of values to convert to categoricals.
    ##
    ## @var{valueset} is the set of all values from which @var{A} is drawn.
    ## If omitted, it defaults to the unique values in @var{A}.
    ##
    ## @var{catnames} is a list of category names corresponding to
    ## @var{valueset}. If omitted, it defaults to @var{valueset}, converted
    ## to strings.
    ##
    ## @var{Ordinal} is a logical indicating whether the category values in
    ## @var{C} have a numeric ordering relationship. Defaults to false.
    ##
    ## @var{Protected} indicates whether @var{C} should be protected, which
    ## prevents the addition of new categories to the array. Defaults to
    ## false.
    ##
    ## @code{@var{C} = categorical ()} returns an empty categorical array.
    ##
    ## @seealso{categories, discretize, iscategorical}
    ## @end deftypefn
    function this = categorical (x, varargin)

      ## Return an empty categorical object
      if (nargin == 0)
        return;
      endif

      ## Handle undefined array (all x is NaN)
      if (isnumeric (x))
        if (all (isnan (x(:))))
          this.code = uint16 (zeros (size (x)));
          this.isMissing = true (size (x));
          return;
        endif
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {'Ordinal', 'Protected'};
      dfValues = {false, false};
      [Ordinal, Protected, args] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! ismember (Ordinal, [0, 1]))
        error (["categorical: 'Ordinal' variable indicator", ...
                " must be either false (0) or true (1)."]);
      endif
      if (! ismember (Protected, [0, 1]))
        error (["categorical: 'Protected' categories indicator", ...
                " must be either false (0) or true (1)."]);
      endif
      if (Ordinal)
        this.isOrdinal = true;
        this.isProtected = true;
      elseif (Protected)
        this.isProtected = true;
      endif

      ## Handle categorical input first
      if (isa (x, 'categorical'))
        this.cats = x.cats;
        this.code = x.code;
        this.isMissing = x.isMissing;
        return
      endif

      ## Handle input arguments
      index2cat = [];
      ## Input Array (x)
      classx = class (x);
      if (iscellstr (x))
        classx = 'cellstr';
      endif
      vtype = {'datetime', 'duration', 'logical', 'string'};
      if (! iscellstr (x) && ! isnumeric (x) && ! any (isa (x, vtype)))
        error ("categorical: invalid input type for X.");
      endif
      ## Categories (valueset)
      if (numel (args) > 0)
        valueset = args{1};
        classv = class (valueset);
        if (iscellstr (valueset))
          classv = 'cellstr';
        endif
        if (strcmp (classx, 'cellstr'))
          if (! (iscellstr (valueset) || isa (valueset, 'string')))
            error ("categorical: incompatible types of VALUESET and X.");
          endif
        elseif (! isequal (classx, classv))
          error ("categorical: types of VALUESET and X do not match.");
        endif
        if (numel (unique (valueset)) < numel (valueset))
          error ("categorical: VALUESET contains non-unique values.");
        endif
        valueset = valueset(:);
        ## Category names (catnames)
        if (numel (args) > 1)
          catnames = args{2};
          if (! (iscellstr (catnames) || isa (catnames, 'string')))
            error ("categorical: invalid type of CATNAMES.");
          endif
          catnames = cellstr (catnames);
          if (numel (valueset) != numel (catnames))
            error ("categorical: CATNAMES and VALUESET lengths do not match.");
          endif
          if (any (cellfun (@isempty, catnames)))
            error ("categorical: CATNAMES contains empty or missing strings.");
          endif
          catnames = catnames(:);
          ## Create index vector to categories
          index2cat = __grp2idx__ (catnames);
        else
          ## Check valueset for missing or empty elements
          if (strcmp (classv, 'cellstr'))
            if (any (cellfun (@isempty, valueset)))
              error (["categorical: VALUESET cannot contain empty", ...
                      " text, unless CATNAMES are specified."]);
            endif
            catnames = valueset;
          elseif (strcmp (classv, 'string'))
            if (any (cellfun (@(x) isempty (x) || ismissing (x), valueset)))
              error (["categorical: VALUESET cannot contain empty or", ...
                      " missing text, unless CATNAMES are specified."]);
            endif
            catnames = cellstr (valueset);
          elseif (isnumeric (valueset))
            catnames = arrayfun (@num2str, valueset, "UniformOutput", false);
          elseif (strcmp (classv, 'logical'))
            if (all (valueset))
              catnames = {'true'};
            elseif (! all (valueset))
              catnames = {'false'};
            else
              catnames = {'false'; 'true'};
            endif
          else # datetime or duration
            catnames = dispstrings (valueset);
          endif
        endif

      else
        ## Resolve valueset and catnames from input array
        if (isnumeric (x))
          valueset = unique (x);
          valueset = valueset(! isnan (valueset));
          catnames = arrayfun (@num2str, valueset, "UniformOutput", false);
        elseif (strcmp (classx, 'logical'))
          valueset = unique (x);
          if (all (valueset))
            catnames = {'true'};
          elseif (! all (valueset))
            catnames = {'false'};
          else
            catnames = {'false'; 'true'};
          endif
        elseif (strcmp (classx, 'cellstr'))
          valueset = unique (x);  # does not remove empty
          valueset = valueset(! cellfun (@isempty, valueset));
          catnames = valueset;
        elseif (strcmp (classx, 'string'))
          valueset = unique (x);  # removes missing, but not empty
          valueset = valueset(! cellfun (@isempty, cellstr (valueset)));
          catnames = cellstr (valueset);
        elseif (strcmp (classx, 'datetime'))
          valueset = unique (x);  # does not remove NaT
          fcn = @(x) strcmp (x, 'NaT');
          valueset = valueset(! cellfun (fcn, dispstrings (valueset)));
          catnames = dispstrings (valueset);
        elseif (strcmp (classx, 'duration'))
          valueset = unique (x);  # does not remove NaN
          fcn = @(x) strcmp (x, 'NaN');
          valueset = valueset(! cellfun (fcn, dispstrings (valueset)));
          catnames = dispstrings (valueset);
        endif
      endif

      ## Associate input array with valueset
      [tf, loc] = ismember (x, valueset);
      maxc = intmax ('uint16');
      if (any (loc > maxc))
        error (["categorical: too many categories; categorical supports up", ...
                " to %d categories; this input has %d."], maxc, max (loc));
      endif

      ## Reassociate to user defined categories (only when regrouping required)
      if (! isempty (index2cat) && numel (index2cat) != unique (index2cat))
        valueset = 1:numel (valueset);
        fcn = @ (x) index2cat(find (valueset == x));
        loc(tf) = arrayfun (fcn, loc(tf));
      endif

      ## Add constructor properties
      this.code = uint16 (loc);
      this.isMissing = ! tf;
      this.cats = catnames;

    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{cstr} =} dispstrings (@var{C})
    ##
    ## Get display formatted strings for each element of a categorical array.
    ##
    ## @code{@var{cstr} = dispstrings (@var{C})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## categorical @var{C}.
    ##
    ## @end deftypefn
    function cstr = dispstrings (this)
      cstr = cell (size (this));
      ix = this.code(! this.isMissing);
      cstr(! this.isMissing) = this.cats(ix);
      cstr(this.isMissing) = {'<undefined>'};
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{cstr} =} cellstr (@var{C})
    ##
    ## Convert categorical array to a cell array of character vectors.
    ##
    ## @code{@var{cstr} = cellstr (@var{C})} returns a cellstr array of
    ## character vectors, @var{cstr}, which has the same size as the input
    ## categorical @var{C}.
    ##
    ## @end deftypefn
    function cstr = cellstr (this)
      cstr = dispstrings (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} double (@var{C})
    ##
    ## Convert categorical array to a double array.
    ##
    ## @code{@var{out} = double (@var{C})} returns a double array indexing the
    ## categories in @var{C}.  Categorical elements of undefined category are
    ## returned as @code{NaN}.
    ##
    ## @end deftypefn
    function out = double (this)
      out = double (this.code);
      out(out == 0) = NaN;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} single (@var{C})
    ##
    ## Convert categorical array to a single array.
    ##
    ## @code{@var{out} = single (@var{C})} returns a single array indexing the
    ## categories in @var{C}.  Categorical elements of undefined category are
    ## returned as @code{NaN}.
    ##
    ## @end deftypefn
    function out = single (this)
      out = single (this.code);
      out(out == 0) = NaN;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} int64 (@var{C})
    ##
    ## Convert categorical array to a int64 array.
    ##
    ## @code{@var{out} = int64 (@var{C})} returns a @qcode{int64} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.
    ##
    ## @end deftypefn
    function out = int64 (this)
      out = int64 (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} int32 (@var{C})
    ##
    ## Convert categorical array to a int32 array.
    ##
    ## @code{@var{out} = int32 (@var{C})} returns a @qcode{int32} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.
    ##
    ## @end deftypefn
    function out = int32 (this)
      out = int32 (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} int16 (@var{C})
    ##
    ## Convert categorical array to a int16 array.
    ##
    ## @code{@var{out} = int16 (@var{C})} returns a @qcode{int16} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.  Note that the returned category
    ## indices saturate to @qcode{intmax ('int16')}, which is 32767.
    ##
    ## @end deftypefn
    function out = int16 (this)
      out = int16 (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} int8 (@var{C})
    ##
    ## Convert categorical array to a int8 array.
    ##
    ## @code{@var{out} = int8 (@var{C})} returns a @qcode{int8} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.  Note that the returned category
    ## indices saturate to @qcode{intmax ('int8')}, which is 127.
    ##
    ## @end deftypefn
    function out = int8 (this)
      out = int8 (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} uint64 (@var{C})
    ##
    ## Convert categorical array to a uint64 array.
    ##
    ## @code{@var{out} = uint64 (@var{C})} returns a @qcode{uint64} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.
    ##
    ## @end deftypefn
    function out = uint64 (this)
      out = uint64 (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} uint32 (@var{C})
    ##
    ## Convert categorical array to a uint32 array.
    ##
    ## @code{@var{out} = uint32 (@var{C})} returns a @qcode{uint32} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.
    ##
    ## @end deftypefn
    function out = uint32 (this)
      out = uint32 (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} uint16 (@var{C})
    ##
    ## Convert categorical array to a uint16 array.
    ##
    ## @code{@var{out} = uint16 (@var{C})} returns a @qcode{uint16} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.
    ##
    ## @end deftypefn
    function out = uint16 (this)
      out = uint16 (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} uint8 (@var{C})
    ##
    ## Convert categorical array to a int8 array.
    ##
    ## @code{@var{out} = uint8 (@var{C})} returns a @qcode{uint8} array
    ## indexing the categories in @var{C}.  Categorical elements of undefined
    ## category are returned as @qcode{0}.  Note that the returned category
    ## indices saturate to @qcode{intmax ('uint8')}, which is 255.
    ##
    ## @end deftypefn
    function out = uint8 (this)
      out = uint8 (this.code);
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'summary'          'categories'       'size'             'ndims'           ##
## 'numel'                                                                    ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {} summary (@var{C})
    ##
    ## Display summary of categorical array.
    ##
    ## @code{summary (@var{C})} displays the number of elements in the
    ## categorical array @var{C} that are equal to each category of @var{C}.
    ## Any undefined elements in @var{C} are summed together and displayed as
    ## @qcode{<undefined>}.
    ##
    ## @end deftypefn
    function summary (this)
      ## Get number of elements per category
      cats = this.cats';
      cols = numel (cats);
      nums = arrayfun (@(x) sum (this.code(:) == x), 1:cols);
      ## Check for undefined elements
      undefined = sum (this.code == 0);
      if (undefined > 0)
        cats = [cats, {'<undefined>'}];
        nums = [nums, undefined];
      endif
      ## Merge categories and number of elements into a cellstr array
      cstr = [cats; num2cell(nums)];
      dispcellmatrix (cstr);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{cstr} =} categories (@var{C})
    ##
    ## List of categories in categorical array.
    ##
    ## @code{@var{cstr} = categories (@var{C})} returns a cell array of
    ## character vectors with the names of the categories in @var{C}.
    ##
    ## @end deftypefn
    function cstr = categories (this)
      cstr = this.cats(:);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{sz} =} size (@var{C})
    ## @deftypefnx {categorical} {@var{dim_sz} =} size (@var{C}, @var{dim})
    ## @deftypefnx {categorical} {@var{dim_sz} =} size (@var{C}, @var{d1}, @var{d2}, @dots{})
    ## @deftypefnx {categorical} {[@var{rows}, @var{columns}, @dots{}, @var{dim_n_sz}] =} size (@dots{})
    ##
    ## Size of a categorical array.
    ##
    ## @code{@var{sz} = size (@var{C})} returns a row vector with the size
    ## (number of elements) of each dimension for the calendar duration array
    ## @var{C}.
    ##
    ## @code{@var{dim_sz} = size (@var{C}, @var{dim})} returns the size of
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
        sz = size (this.code, varargin{:});
      else
        sz = size (this.code);
      endif
      if (nargout == 0 || nargout == 1)
        varargout{1} = sz;
      elseif (numel (sz) != nargout)
        error (["categorical.size: nargout > 1 but does", ...
                " not match number of requested dimensions."]);
      else
        for i = 1:nargout
          varargout{i} = sz(i);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} ndims (@var{calD})
    ##
    ## Number of dimensions in a categorical array.
    ##
    ## @code{@var{out} = ndims (@var{calD})} returns the number of dimensions
    ## of the calendar duration array @var{calD}.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = ndims (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} numel (@var{calD})
    ##
    ## Total number of elements in a categorical array.
    ##
    ## For compatibility reasons with Octave's OOP interface and @code{subsasgn}
    ## behavior, categorical's @code{numel} is defined to always return 1.
    ##
    ## @end deftypefn
    function out = numel (this, varargin)
      out = 1;
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'iscategory'       'iscolumn'         'isempty'          'isequal'         ##
## 'isequaln'         'ismatrix'         'ismember'         'isordinal'       ##
## 'isprotected'      'isrow'            'isscalar'         'issorted'        ##
## 'issortedrows'     'isundefined'      'isvector'                           ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} iscategory (@var{C}, @var{catnames})
    ##
    ## Test for categories in a categorical array.
    ##
    ## @code{@var{TF} = iscategory (@var{C}, @var{catnames})} returns a logical
    ## array @var{TF} of the same size as @var{catnames} containing @qcode{true}
    ## for each corresponding element of @var{catnames} that is a category in
    ## categorical array @var{C} and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = iscategory (this, catnames)
      catnames = cellstr (catnames);
      TF = ismember (catnames, this.cats);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} iscolumn (@var{C})
    ##
    ## Return true if categorical array is a column vector.
    ##
    ## @end deftypefn
    function TF = iscolumn (this)
      TF = iscolumn (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isempty (@var{C})
    ##
    ## Return true if categorical array is empty.
    ##
    ## @end deftypefn
    function TF = isempty (this)
      TF = isempty (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isequal (@var{C1}, @var{C2}, @dots{})
    ##
    ## Return true if categorical arrays are equal.
    ##
    ## @end deftypefn
    function TF = isequal (varargin)
      args = varargin;
      [args{:}] = promote (varargin{:});
      ## If any categorical value is ordinal, all must be
      is_ordinal = cellfun (@isordinal, args);
      if (all (is_ordinal))
        ## Check that all categorical arrays have the same categories
        ## and they are in the same order
        cats = cellfun (@(x) categories (x), args, 'UniformOutput', false);
        if (! isequal (cats{:}))
          TF = false;
        endif
        fieldArgs = cellfun (@(x) x.code, args, 'UniformOutput', false);
        TF = isequal (fieldArgs{:});
      elseif (any (is_ordinal))
        TF = false;
      else
        ## Compare the category names of each element
        fieldArgs = cellfun (@(x) x.cats(x.code), args, 'UniformOutput', false);
        TF = isequal (fieldArgs{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isequaln (@var{C1}, @var{C2}, @dots{})
    ##
    ## Return true if categorical arrays are equal under the additional
    ## assumption that @qcode{NaN == NaN}.
    ##
    ## @end deftypefn
    function TF = isequaln (varargin)
      args = varargin;
      [args{:}] = promote (varargin{:});
      ## If any categorical value is ordinal, all must be
      is_ordinal = cellfun (@isordinal, args);
      if (all (is_ordinal))
        ## Check that all categorical arrays have the same categories
        ## and they are in the same order
        cats = cellfun (@(x) categories (x), args, 'UniformOutput', false);
        if (! isequal (cats{:}))
          TF = false;
        endif
        fieldArgs = cellfun (@(x) x.code, args, 'UniformOutput', false);
        TF = isequaln (fieldArgs{:});
      elseif (any (is_ordinal))
        TF = false;
      else
        ## Compare the category names of each element
        fieldArgs = cellfun (@(x) x.cats(x.code), args, 'UniformOutput', false);
        TF = isequaln (fieldArgs{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} ismatrix (@var{C})
    ##
    ## Return true if categorical array is a 2-D array.
    ##
    ## @end deftypefn
    function TF = ismatrix (this)
      TF = ismatrix (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{TF} =} ismember (@var{A}, @var{B})
    ## @deftypefnx {categorical} {@var{TF} =} ismember (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {categorical} {[@var{TF}, @var{index}] =} ismember (@dots{})
    ##
    ## Test for categorical elements in a set.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B})} returns a logical array
    ## @var{TF} of the same size as @var{A} containing @qcode{true} for each
    ## corresponding element of @var{A} that is in @var{B} and @qcode{false}
    ## otherwise.  If @var{A} and @var{B} are both ordinal, they must both have
    ## the same ordered set of categories.  If neither @var{A} nor @var{B} are
    ## ordinal, then this restriction is relaxed and comparison is performed
    ## using the category names. Comparison between an ordinal and an unordered
    ## categorical array is not allowed.  @var{A} or @var{B} may also be a
    ## string array or a cell array of character vectors containing one or
    ## multiple category names to compare against.
    ##
    ## @code{@var{TF} = ismember (@var{A}, @var{B}, @qcode{'rows'})} only
    ## applies to categorical matrices with the same number of columns, in which
    ## case the logical vector @var{TF} contains @qcode{true} for each row of
    ## @var{A} that is also a row in @var{B}.  @var{TF} has the size number of
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
      ## Either A or B contain category name(s)
      if ((isa (A, 'string') || iscellstr (A)) && isa (B, 'categorical'))
        if (numel (varargin) > 0)
          error (["categorical.ismember: cannot use 'rows' when", ...
                  " testing against category names."]);
        endif
        A = cellstr (A);
        [TF, index] = ismember (A, categories (B));
        return;
      elseif ((isa (B, 'string') || iscellstr (B)) && isa (A, 'categorical'))
        if (numel (varargin) > 0)
          error (["categorical.ismember: cannot use 'rows' when", ...
                  " testing against category names."]);
        endif
        B = cellstr (B);
        [TF, index] = ismember (B, categories (A));
        return;
      endif
      if (numel (varargin) > 0)
        if (! ismatrix (A) || ! ismatrix (B) || size (A, 2) != size (b, 2))
          error (["categorical.ismember: cannot use 'rows' unless both", ...
                  " A and B are matrices with the same number of columns."]);
        endif
      endif
      ## Both ordinal
      if (isordinal (A) && isordinal (B))
        if (! isequal (categories (A), categories (B)))
          error (["categorical.ismember: ordinal categorical arrays", ...
                  " must have the same ordered set of categories."]);
        endif
        [TF, idx] = ismember (double (A), double (B), varargin{:});
      elseif (isordinal (A) || isordinal (B))
        error ("categorical.ismember: both categorical arrays nust be ordinal.");
      else
        ## Compare the category names of each element
        cats_A = A.cats(A.code);
        cats_B = B.cats(B.code);
        [TF, index] = ismember (cats_A, cats_B, varargin{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isordinal (@var{C})
    ##
    ## Test if categorical array is ordinal.
    ##
    ## @end deftypefn
    function TF = isordinal (this)
      TF = this.isOrdinal;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isprotected (@var{C})
    ##
    ## Test if categorical array is protected.
    ##
    ## @end deftypefn
    function TF = isprotected (this)
      TF = this.isProtected;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isrow (@var{C})
    ##
    ## Return true if categorical array is a row vector.
    ##
    ## @end deftypefn
    function TF = isrow (this)
      TF = isrow (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isscalar (@var{C})
    ##
    ## Return true if categorical array is a scalar.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = isscalar (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} isundefined (@var{C})
    ##
    ## Test for undefined elements in categorical array.
    ##
    ## @code{@var{TF} = isundefined (@var{C})} returns a logical array @var{TF}
    ## of the same size as @var{C} containing @qcode{true} for each
    ## corresponding element of @var{C} that does not have a value from one of
    ## the categories in @var{C} and @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isundefined (this)
      TF = this.isMissing;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isvector (@var{C})
    ##
    ## Return true if categorical array is a vector.
    ##
    ## @end deftypefn
    function TF = isvector (this)
      TF = isvector (this.code);
    endfunction

  endmethods

################################################################################
##                        ** Category Operations **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'addcats'          'mergecats'        'removecats'       'renamecats'      ##
## 'reordercats'      'setcats'          'times'                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} addcats (@var{A}, @var{newcats})
    ## @deftypefnx {categorical} {@var{B} =} addcats (@dots{}, @qcode{'After'}, @var{catname})
    ## @deftypefnx {categorical} {@var{B} =} addcats (@dots{}, @qcode{'Before'}, @var{catname})
    ##
    ## Add categories to categorical array.
    ##
    ## @code{@var{B} = addcats (@var{A}, @var{newcats})} appends new categories
    ## specified in @var{newcats} to the categorical array @var{A} at the end of
    ## any existing categories.  The output categorical array @var{B} does not
    ## contain elements that belong to the newly added categories.
    ##
    ## @code{@var{B} = addcats (@dots{}, @qcode{'After'}, @var{catname})} adds
    ## the categories after the existing category specified by @var{catname}.
    ##
    ## @code{@var{B} = addcats (@dots{}, @qcode{'Before'}, @var{catname})} adds
    ## the categories before the existing category specified by @var{catname}.
    ##
    ## @end deftypefn
    function B = addcats (A, newcats, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("categorical:addcats: too few input arguments.");
      elseif (isempty (newcats))
        error ("categorical:addcats: NEWCATS cannot be empty.");
      endif

      ## New catnames must be unique and non-existing
      newcats = cellstr (newcats);
      if (! isequal (newcats, unique (newcats)))
        error ("categorical:addcats: duplicate category names in NEWCATS.");
      endif
      TF = ismember (newcats, A.cats);
      if (any (TF))
        error ("categorical:addcats: new category names already present.");
      endif

      ## Parse optional Name-Value paired arguments
      optNames = {"After", "Before"};
      dfValues = {[], []};
      [After, Before] = pairedArgs (optNames, dfValues, varargin(:));

      ## Check optional Name-Value paired arguments
      if (! isempty (After) && ! isempty (Before))
        error (["categorical.addcats: cannot use both", ...
                " 'After' and 'Before' options."]);
      endif

      ## Add categories
      if (! isempty (After))
        maxcode = numel (A.cats);
        idxcode = find (strcmp (After, A.cats));
        if (isempty (idxcode))
          error ("categorical:addcats: 'After' indexes a non-existing category.");
        elseif (idxcode == maxcode)
          B = A;
          B.cats = [B.cats; newcats];
        else
          B = A;
          B.cats = [B.cats; newcats];
          idx_add = idxcode+1:maxcode;
          n_cats = numel (newcats);
          for i = numel (idx_add):-1:1
            B.code(B.code == idx_add(i)) += n_cats;
          endfor
        endif
      elseif (! isempty (Before))
        maxcode = numel (A.cats);
        idxcode = find (strcmp (After, A.cats));
        if (isempty (idxcode))
          error ("categorical:addcats: 'Before' indexes a non-existing category.");
        elseif (idxcode == maxcode)
          B = A;
          B.cats = [B.cats; newcats];
        else
          B = A;
          B.cats = [B.cats; newcats];
          idx_add = idxcode:maxcode;
          n_cats = numel (newcats);
          for i = numel (idx_add):-1:1
            B.code(B.code == idx_add(i)) += n_cats;
          endfor
        endif
      else
        B = A;
        B.cats = [B.cats; newcats];
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} mergecats (@var{A}, @var{oldcats})
    ## @deftypefnx {categorical} {@var{B} =} mergecats (@var{A}, @var{oldcats}, @var{newcat})
    ##
    ## Merge categories in categorical array.
    ##
    ## @code{@var{B} = mergecats (@var{A}, @var{oldcats})} merges two or more
    ## categories specified by @var{oldcats} into a single category with the
    ## same name as @qcode{@var{oldcats}(1)}.  In case of ordinal categorical
    ## arrays, the categories listed in @var{oldcats} must be in consecutive
    ## order.  All elements of @var{A} corresponding to the categories listed in
    ## @var{oldcats} are re-indexed to correspond to @qcode{@var{oldcats}(1)} in
    ## @var{B}.
    ##
    ## @code{@var{B} = mergecats (@var{A}, @var{oldcats}, @var{newcat})} merges
    ## the categories listed in @var{oldcats} into a single new category named
    ## as specififed by @var{newcat}.
    ##
    ## @end deftypefn
    function B = mergecats (A, oldcats, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("categorical:mergecats: too few input arguments.");
      elseif (isempty (oldcats))
        error ("categorical:mergecats: OLDCATS cannot be empty.");
      endif
      oldcats = cellstr (oldcats);
      if (nargin < 3)
        newcat = oldcats{1};
      else
        newcat = cellstr (varargin{1});
        if (cellfun ('isempty', newcat))
          error ("categorical:mergecats: blank new category name.");
        endif
      endif
      ## Keep old cat names that reference existing categories, ignore the rest
      [TF, index] = ismember (oldcats, A.cats);
      if (! any (TF))
        B = A;
        return;
      endif
      index(! TF) = [];
      ## Only consecutive categories can be merged in ordinal arrays
      if (A.isOrdinal && any (diff (index) != 1))
        error (["categorical.mergecats: only consecutive categories", ...
                " can be merged in ordinal categorical arrays."]);
      endif

      ## Merge categories
      TF = ismember (A.code, index);
      B = A;
      B.code(TF) = index(1);
      B.cats(index(1)) = newcat;
      remidx = index(2:end);
      B.cats(remidx) = [];
      B.code(! TF) -= numel (remidx);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} removecats (@var{A})
    ## @deftypefnx {categorical} {@var{B} =} removecats (@var{A}, @var{oldcats})
    ##
    ## Remove categories from categorical array.
    ##
    ## @code{@var{B} = removecats (@var{A})} removes all unused categories from
    ## categorical array @var{A}.  The output categorical array @var{B} has the
    ## same size and values as @var{A}, but potentially fewer categories.
    ##
    ## @code{@var{B} = removecats (@var{A}, @var{oldcats})} removes the
    ## categories specified by @var{oldcats}.  The elements of @var{B} that
    ## correspond to the removed categories are undefined.
    ##
    ## @end deftypefn
    function B = removecats (A, varargin)
      ## Remove unused categories
      if (nargin == 1)
        usedcodes = __unique__ (A.code(:));
        usedcodes(usedcodes == 0) = [];
        remidx = ! ismember (1:numel(A.cats), usedcodes);
        B = A;
        B.cats(remidx) = [];
      elseif (isempty (varargin))
        error ("categorical:removecats: OLDCATS cannot be empty.");
      else
        oldcats = cellstr (varargin{1});
        ## Keep old cat names that reference existing categories, ignore the rest
        [TF, remidx] = ismember (oldcats, A.cats);
        if (! any (TF))
          B = A;
          return;
        endif
        remidx(! TF) = [];
        [TF] = ismember (A.code, remidx);
        B = A;
        B.code(TF) = 0;
        B.cats(remidx) = [];
        B.isMissing = B.isMissing | TF;
        remcodes = unique (B.code(! TF)(:));
        if (isscalar (remcodes))
          B.code(B.code > remcodes) -= 1;
          return;
        endif
        ## If remcodes are consecutive, then we only need to subtract once
        ## the number of elements in remcodes from each code that is more
        ## than max(remcodes), otherwise we need to do this repeatedly for
        ## consecutive subset in remcodes
        idx = find (diff (remcodes) > 1);
        if (isempty (idx))  # all removed elements are consecutive
          B.code(B.code > max (remcodes)) -= numel (remcodes);
          return;
        else
          while (numel (remcodes) > 0)
            if (numel (idx) > 0)
              remvec = remcodes(1:idx(1));
              remcodes(1:idx(1)) = [];
              idx = idx - idx(1);
              idx(1) = [];
            else
              remvec = remcodes;
              remcodes = [];
            endif
            B.code(B.code > max (remvec)) -= numel (remvec);
          endwhile
        endif
      endif;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} renamecats (@var{A}, @var{newnames})
    ## @deftypefnx {categorical} {@var{B} =} renamecats (@var{A}, @var{oldnames}, @var{newnames})
    ##
    ## Rename categories in categorical array.
    ##
    ## Renames some or all of the categories in @var{obj}, without changing
    ## any of its values.
    ##
    ## @end deftypefn
    function B = renamecats (A, varargin)
      ## Check input arguments
      if (nargin < 2)
        error ("categorical:renamecats: too few input arguments.");
      endif
      if (nargin == 2)
        oldnames = A.cats;
        newnames = cellstr (varargin{1});
        if (numel (oldnames) != numel (newnames))
          error (["categorical:renamecats: NEWNAMES must equal the", ...
                  " number of existing categories in input array."]);
        endif
      else
        oldnames = cellstr (varargin{1});
        newnames = cellstr (varargin{2});
        if (numel (oldnames) != numel (newnames))
          error (["categorical:renamecats: OLDNAMES and NEWNAMES", ...
                  " must have the same number of elements."]);
        endif
      endif
      [TF, index] = ismember (oldnames, A.cats);
      if (! all (TF))
        error (["categorical.renamecats: OLDNAMES must be", ...
                " a subset of existing categories."])
      endif
      B = this;
      B.cats(index) = newnames;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} reordercats (@var{A})
    ## @deftypefnx {categorical} {@var{B} =} reordercats (@var{A}, @var{neworder})
    ##
    ## Reorder categories in categorical array.
    ##
    ## @code{@var{B} = reordercats (@var{A})} reorders the categories of @var{A}
    ## in alphanumeric order.
    ##
    ## @code{@var{B} = reordercats (@var{A}, @var{neworder})} reorders the
    ## categories of @var{A} according to the order specified by @var{neworder},
    ## which must be a cell array of character vectors or a string array with
    ## the same set of values as the existing categories in @var{A}.
    ##
    ## @end deftypefn
    function B = reordercats (A, varargin)
      if (nargin == 1)
        neworder = sort (A.cats);
      else
        neworder = cellstr (varargin{1});
        if (! all (ismember (neworder, A.cats)))
          error (["categorical.reordercats: NEWORDER must contain", ...
                  " the same set with the existing categories."]);
        endif
      endif
      [~, newidx] = ismember (A.cats, neworder);
      B = A;
      B.code(! B.isMissing) = newidx(A.code(! A.isMissing));
      B.cats = neworder(:);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{B} =} setcats (@var{A}, @var{newcats})
    ##
    ## Set categories in categorical array.
    ##
    ## @code{@var{B} = setcats (@var{A}, @var{newcats})} sets categories in a
    ## categorical array according to the elements of the imput array and the
    ## categories specified by @var{newcats}.
    ##
    ## @itemize
    ## @item Any element of @var{A} that corresponds to a category listed in
    ## @var{newcats} is copied to @var{B} with the same categorical value.
    ## @item Any categories of @var{A} not listed in @var{newcats} are not
    ## copied to @var{B} and the corresponding elements of @var{B} are
    ## undefined.
    ## @item New categories listed in @var{newcats} that are not present in
    ## @var{A} are added in @var{B}, but without any elements equal to these
    ## new categories.
    ## @end itemize
    ##
    ## @end deftypefn
    function B = setcats (A, newcats)
      ## Check input arguments
      if (nargin < 2)
        error ("categorical:setcats: too few input arguments.");
      endif
      newcats = cellstr (newcats);
      B = A;
      [TF, index] = ismember (A.cats, newcats);
      ## Remove and reorder categories
      B = removecats (A, A.cats(! TF));
      [TF, index] = ismember (newcats, B.cats);
      index(! TF) = [];
      B = reordercats (B, B.cats(index));
      [TF, index] = ismember (newcats, B.cats);
      index(! TF) = [];
      ## For each new category added before previously existing categories
      ## we need to increase the code indexing respectively
      for i = 1:numel (TF)
        if (! TF(i))
          B.code(B.code >= i) += 1;
        endif
      endfor
      B.cats = newcats(:);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{C} =} times (@var{A}, @var{B})
    ##
    ## Combine categorical arrays.
    ##
    ## @code{@var{C} = times (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{A} .* @var{B}} and returns a categorical array whose
    ## categories are the Cartesian product of the categories in @var{A} and
    ## @var{B} and each element is indexed to a new category which is the
    ## combination of the categories of the corresponding elements in @var{A}
    ## and @var{B}.
    ##
    ## @var{A} and @var{B} must be of common size or scalars.
    ##
    ## @end deftypefn
    function C = times (A, B)
      ## Check input arguments
      if (nargin < 2)
        error ("categorical:times: too few input arguments.");
      endif
      newcats = {};
      for i = 1:numel (A.cats)
        for j = 1:numel (B.cats)
          newcats = [newcats; strjoin({A.cats{i}, B.cats{j}})];
        endfor
      endfor
      C = addcats (categorical, newcats);
      C.code = A.code .* B.code;
      C.isMissing = A.isMissing | B.isMissing;
    endfunction

  endmethods

################################################################################
##                        ** Relational Operations **                         ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'eq'               'ge'               'gt'               'le'              ##
## 'lt'               'ne'                                                    ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function TF = eq (A, B)
      TF = isequal (A, B);
    endfunction

    function TF = ge (A, B)
      if (iscellstr (B) || isa (B, 'string') || ischar (B))
        if (! A.isOrdinal)
          error ("categorical.ge: categorical array is not ordinal.");
        endif
        B = cellstr (B);
        if (! isscalar (B))
          error ("categorical.ge: incompatible size for relational comparison.");
        endif
        code = find (ismember (categories (A), B));
        if (isempty (code))
          error ("categorical.ge: category does not exist in array.");
        endif
        TF = double (A) >= code;
      elseif (iscellstr (A) || isa (A, 'string') || ischar (A))
        TF = B <= A;
      elseif (! A.isOrdinal || ! B.isOrdinal)
        error (["categorical.ge: relational comparison is not", ...
                " allowed for non-ordinal categorical arrays."]);
      else
        TF = double (A) >= double (B);
      endif
    endfunction

    function TF = gt (A, B)
      if (iscellstr (B) || isa (B, 'string') || ischar (B))
        if (! A.isOrdinal)
          error ("categorical.lt: categorical array is not ordinal.");
        endif
        B = cellstr (B);
        if (! isscalar (B))
          error ("categorical.lt: incompatible size for relational comparison.");
        endif
        code = find (ismember (categories (A), B));
        if (isempty (code))
          error ("categorical.lt: category does not exist in array.");
        endif
        TF = double (A) > code;
      elseif (iscellstr (A) || isa (A, 'string') || ischar (A))
        TF = B < A;
      elseif (! A.isOrdinal || ! B.isOrdinal)
        error (["categorical.lt: relational comparison is not", ...
                " allowed for non-ordinal categorical arrays."]);
      else
        TF = double (A) > double (B);
      endif
    endfunction

    function TF = le (A, B)
      if (iscellstr (B) || isa (B, 'string') || ischar (B))
        if (! A.isOrdinal)
          error ("categorical.le: categorical array is not ordinal.");
        endif
        B = cellstr (B);
        if (! isscalar (B))
          error ("categorical.le: incompatible size for relational comparison.");
        endif
        code = find (ismember (categories (A), B));
        if (isempty (code))
          error ("categorical.le: category does not exist in array.");
        endif
        TF = double (A) <= code;
      elseif (iscellstr (A) || isa (A, 'string') || ischar (A))
        TF = B >= A;
      elseif (! A.isOrdinal || ! B.isOrdinal)
        error (["categorical.le: relational comparison is not", ...
                " allowed for non-ordinal categorical arrays."]);
      else
        TF = double (A) <= double (B);
      endif
    endfunction

    function TF = lt (A, B)
      if (iscellstr (B) || isa (B, 'string') || ischar (B))
        if (! A.isOrdinal)
          error ("categorical.lt: categorical array is not ordinal.");
        endif
        B = cellstr (B);
        if (! isscalar (B))
          error ("categorical.lt: incompatible size for relational comparison.");
        endif
        code = find (ismember (categories (A), B));
        if (isempty (code))
          error ("categorical.lt: category does not exist in array.");
        endif
        TF = double (A) < code;
      elseif (iscellstr (A) || isa (A, 'string') || ischar (A))
        TF = B > A;
      elseif (! A.isOrdinal || ! B.isOrdinal)
        error (["categorical.lt: relational comparison is not", ...
                " allowed for non-ordinal categorical arrays."]);
      else
        TF = double (A) < double (B);
      endif
    endfunction

    function TF = ne (A, B)
      TF = ! isequal (A, B);
    endfunction

  endmethods

################################################################################
##                   ** Sort, Filter, and Set Operations **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'sort'             'sortrows'         'unique'           'intersect'       ##
## 'setdiff'          'setxor'           'union'                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    function [B, index] = sort (A, varargin)
      B = A;
      code = double (A);
      [code, index] = sort (code, varargin{:});
      B.code = code;
      B.isMissing = isnan (code);
    endfunction

    function [B, index] = sortrows (A, varargin)
      if (ndims (A) != 2)
        error ("categorical.sortrows: A must be a 2-D matrix.");
      endif
      col_dir = false;
      if (numel (varargin) > 0)
        col = varargin{1};
        if (isnumeric (col))
          if (! isvector (col) || fix (col) != col)
            error ("categorical.sortrows: COL must be a vector of integers.");
          endif
        elseif ((ischar (col) && isvector (col)) ||
                (isscalar (col) && isa (col, 'string')))
          col = cellstr (col);
          if (strcmpi (col, 'ascend'))
            col = [1:size(A, 2)];
          elseif (strcmpi (col, 'descend'))
            col = -[1:size(A, 2)];
          else
            error (strjoing (["categorical.sortrows: DIRECTION can", ...
                              "be either 'ascend' or 'descend'."]));
          endif
        else
          error ("categorical.sortrows: invalid value for COL argument.");
        endif
        col_dir = true;
      endif
      if (numel (varargin) > 1)
        direction = cellstr (varargin{2});
        if (! all (ismember (direction, {'ascend', 'descend'})))
          error ("categorical.sortrows: invalid value for DIRECTION argument.");
        endif
        if (isscalar (direction) && strcmpi (direction, 'ascend'))
          col = abs (col);
        elseif (isscalar (direction) && strcmpi (direction, 'descend'))
          col = - abs (col);
        else
          if (numel (direction) != numel (col))
            error (strjoin (["categorical.sortrows: DIRECTION", ...
                             "does not match COL argument."]));
          endif
          col = abs (col);
          idx = strcmpi (direction, 'descend');
          col(idx) = - col(idx);
        endif
      endif
      B = A;
      code = double (A);
      if (col_dir)
        [B.code, index] = sortrows (code, col);
      else
        [B.code, index] = sortrows (code);
      endif
    endfunction

    function [B, ixA, ixB] = unique (A, varargin)
      ## Handle 'rows' option
      do_rows = false;
      if (! isempty (varargin))
        idx = strcmpi ('rows', varargin(:));
        if (any (idx))
          do_rows = true;
          varargin(idx) = [];
          if (ndims (A) != 2)
            error ("categorical.unique: 'rows' applies only to 2-D matrices.");
          endif
        endif
      endif
      ## Handle 'setOrder' and 'occurence' options
      opt = "sorted";
      if (! isempty (varargin))
        if (any (strcmp (varargin{1}, {"sorted", "stable", "first", "last"})))
          opt = varargin{1};
        else
          error ("categorical.unique: invalid option '%s'.", varargin{1});
        endif
      endif
      ## Find unique
      code = double (A);
      if (do_rows)
        [~, ixA, ixB] = __unique__ (code, 'rows', opt);
        B = subset (A, ixA, ':');
      else
        [~, ixA, ixB] = __unique__ (code, opt);
        B = subset (A, ixA);
      endif
    endfunction

    function [out, ixa, ixb] = intersect (A, B, varargin)
      error ("categorical.intersect: not implemented yet.");
    endfunction

    function [out, ixa, ixb] = setdiff (A, B, varargin)
      error ("categorical.setdiff: not implemented yet.");
    endfunction

    function [out, ixa, ixb] = setxor (A, B, varargin)
      error ("categorical.setxor: not implemented yet.");
    endfunction

    function [out, ixa, ixb] = union (A, B, varargin)
      error ("categorical.union: not implemented yet.");
    endfunction

  endmethods

################################################################################
##                           ** Array Operations **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'cat'              'horzcat'          'vertcat'          'repmat'          ##
## 'reshape'          'circshift'        'permute'          'ipermute'        ##
## 'transpose'        'ctranspose'                                            ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function out = cat (dim, varargin)
      args = varargin;
      [args{:}] = promote (varargin{:});
      ## Check that all dimensions except DIM are equal
      if (fix (dim) != dim | dim < 1)
        error ("categorical.cat: DIM must be a valid dimension.");
      endif
      sz_dim = cellfun (@(x) size (x), args, "UniformOutput", false);
      n_dims = cellfun (@(x) ndims (x), sz_dim, "UniformOutput", false);
      if (! isequal (n_dims{:}))
        error ("categorical.cat: dimensions mismatch.");
      elseif (dim <= n_dims{1})
        idx = 1:n_dims{1};
        idx(dim) = [];
        rem_dims = cellfun (@(x) (x(idx)), sz_dim, "UniformOutput", false);
        if (! isequal (rem_dims{:}))
          error ("categorical.cat: dimensions mismatch.");
        endif
      endif
      ## If any categorical value is ordinal, all must be
      is_ordinal = cellfun (@isordinal, args);
      if (all (is_ordinal))
        ## Check that all categorical arrays have the same categories
        ## and they are in the same order
        cats = cellfun (@(x) categories (x), args, 'UniformOutput', false);
        if (! isequal (cats{:}))
          error (["categorical.cat: cannot concatenate ordinal categorical", ...
                  " arrays unless they have the same ordered set of categories."]);
        endif
        out = args{1};
        fieldArgs = cellfun (@(x) x.code, args, 'UniformOutput', false);
        out.code = cat (dim, fieldArgs{:});
        fieldArgs = cellfun (@(x) x.isMissing, args, 'UniformOutput', false);
        out.isMissing = cat (dim, fieldArgs{:});
        return;
      elseif (any (is_ordinal))
        error (["categorical.cat: cannot concatenate ordinal", ...
                " with non-ordinal categorical arrays."]);
      endif
      ## If any categorical array is protected, all must have the same categories
      is_protected = cellfun (@isprotected, args);
      if (any (is_protected))
        ## Check that all categorical arrays have the same categories
        ## but they are not necessarily in the same order
        cats = cellfun (@(x) categories (x), args, 'UniformOutput', false);
        if (! all (ismember (cats{:})))
          error (["categorical.cat: cannot concatenate protected categorical", ...
                  " arrays that do not have the same set of categories."]);
        endif
        out = args{1};
        out.isProtected = true; # returning array must also be protected
        ## Go through remaining categorical arrays and reorder codes accordingly
        idx = cell (1, numel(cats{1}));
        for i = 2:numel (args)
          for j = 1:numel (out.cats)
            new_code = find (strcmp (args{i}.cats, out.cats(j)));
            idx{j} = args{i}.code == new_code;
          endfor
          for j = 1:numel (out.cats)
            args{i}.code(idx{j}) = j;
          endfor
        endfor
        fieldArgs = cellfun (@(x) x.code, args, 'UniformOutput', false);
        out.code = cat (dim, fieldArgs{:});
        fieldArgs = cellfun (@(x) x.isMissing, args, 'UniformOutput', false);
        out.isMissing = cat (dim, fieldArgs{:});
        return;
      endif
      ## No constrains, add new categories as necessary and bump code indexing
      ## to reflect the changes in category list of concatenated output array
      out = args{1};
      for i = 2:numel (args)
        ncats = numel (categories (args{i}));
        idx = cell (1, ncats);
        newcat = {};
        n_code = [];
        for j = 1:ncats
          new_code = find (strcmp (out.cats, args{i}.cats(j)));
          if (! isempty (new_code))
            idx{j} = args{i}.code == new_code;
            n_code = [n_code, j];
          else
            idx{j} = args{i}.code == j;
            n_code = [n_code, numel(out.cats)+1];
            newcat = [newcat, args{i}.cats(j)];
          endif
        endfor
        out.cats = [out.cats, newcat];
        for j = 1:ncats
          args{i}.code(idx{j}) = n_code(j);
        endfor
        fieldArgs = cellfun (@(x) x.code, args, 'UniformOutput', false);
        out.code = cat (dim, fieldArgs{:});
        fieldArgs = cellfun (@(x) x.isMissing, args, 'UniformOutput', false);
        out.isMissing = cat (dim, fieldArgs{:});
      endfor
    endfunction

    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    function this = repmat (this, varargin)
      this.code = repmat (this.code, varargin{:});
      this.isMissing = repmat (this.isMissing, varargin{:});
    endfunction

    function this = reshape (this, varargin)
      this.code = reshape (this.code, varargin{:});
      this.isMissing = reshape (this.isMissing, varargin{:});
    endfunction

    function this = circshift (this, varargin)
      this.code = circshift (this.code, varargin{:});
      this.isMissing = circshift (this.isMissing, varargin{:});
    endfunction

    function this = permute (this, order)
      this.code = permute (this.code, order);
      this.isMissing = permute (this.isMissing, order);
    endfunction

    function this = ipermute (this, order)
      this.code = ipermute (this.code, order);
      this.isMissing = ipermute (this.isMissing, order);
    endfunction

    function this = transpose (this)
      this.code = transpose (this.code);
      this.isMissing = transpose (this.isMissing);
    endfunction

    function this = ctranspose (this)
      this.code = ctranspose (this.code);
      this.isMissing = ctranspose (this.isMissing);
    endfunction

  endmethods

################################################################################
##                  ** Reference and Assignment Operations **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'subsref'          'subsasgn'                                              ##
##                                                                            ##
################################################################################

  methods (Hidden)

    ## Class specific subscripted reference
    function varargout = subsref (this, s)

      chain_s = s(2:end);
      s = s(1);
      switch (s.type)
        case '()'
          out = this;
          out.code = this.code(s.subs{:});
          out.isMissing = this.isMissing(s.subs{:});

        case '{}'
          error (["categorical.subsref: '{}' invalid indexing", ...
                  " for referencing values. Use '()' instead."]);

        case '.'
          error (["categorical.subsref: '.' invalid indexing", ...
                  " for referencing field of non-structure array."]);
      endswitch

      ## Chained references
      if (! isempty (chain_s))
        out = subsref (out, chain_s);
      endif
      varargout{1} = out;

    endfunction

    ## Class specific subscripted assignment
    function this = subsasgn (this, s, val)

      if (numel (s) > 1)
        error ("categorical.subsasgn: chained subscripts not allowed.");
      endif
      switch s.type
        case '()'
          if (iscellstr (val) || ischar (val) ||
                                 any (isa (val, {'missing', 'string'})))
            val = promote (val);
            if (isordinal (this))
              val.isOrdinal = true;
            elseif (isprotected (this))
              val.isProtected = true;
            endif
          elseif (! isa (val, 'categorical'))
            error (["categorical.subsasgn: assignment value must be a", ...
                    " categorical array or text representing categories."]);
          endif
          ## After this point VAL is categorical array
          ## If any categorical array is ordinal, all must be
          if (isordinal (this) && isordinal (val))
            ## Check that all categorical arrays have the same categories
            ## and they are in the same order
            cats = cellfun (@(x) categories (x), args, 'UniformOutput', false);
            if (! isequal (categories (this), categories (val)))
              error (["categorical.subsasgn: cannot assign value to ordinal", ...
                      " categorical array unless they have the same ordered", ...
                      " set of categories."]);
            endif
            this.code(s.subs{:}) = val.code;
            this.isMissing(s.subs{:}) = val.isMissing;
            return;
          elseif (isordinal (this))
            error (["categorical.subsasgn: cannot assign unordered", ...
                    " categorical array to ordinal categorical array."]);
          elseif (isordinal (val))
            error (["categorical.subsasgn: cannot assign ordinal", ...
                    " categorical array to unordered categorical array."]);
          endif
          ## If protected, all must have the same categories
          if (isprotected (this) || isprotected (val))
            ## Check that all categorical arrays have the same categories
            ## but they are not necessarily in the same order
            if (! all (ismember (this.cats, val.cats)))
              error (["categorical.subsasgn: cannot asssign to protected", ...
                      " categorical array new categories."]);
            endif
            ## Reorder codes accordingly
            idx = cell (1, numel (this.cats));
            for j = 1:numel (this.cats)
              new_code = find (strcmp (val.cats, this.cats(j)));
              idx{j} = val.code == new_code;
            endfor
            for j = 1:numel (this.cats)
              val.code(idx{j}) = j;
            endfor
            this.code(s.subs{:}) = val.code;
            this.isMissing(s.subs{:}) = val.isMissing;
            return;
          endif
          ## No constrains, add new categories as necessary and bump code indexing
          ## to reflect the changes in category list of assigned categorical array
          ncats = numel (categories (val));
          idx = cell (1, ncats);
          newcat = {};
          n_code = [];
          for j = 1:ncats
            new_code = find (strcmp (this.cats, val.cats(j)));
            if (! isempty (new_code))
              idx{j} = val.code == new_code;
              n_code = [n_code, j];
            else
              idx{j} = val.code == j;
              n_code = [n_code, numel(this.cats)+1];
              newcat = [newcat, val.cats(j)];
            endif
          endfor
          this.cats = [this.cats, newcat];
          for j = 1:ncats
            val.code(idx{j}) = n_code(j);
          endfor
          this.code(s.subs{:}) = val.code;
          this.isMissing(s.subs{:}) = val.isMissing;

        case '{}'
          error (["categorical.subsasgn: '{}' invalid indexing", ...
                  " for assigning values. Use '()' instead."]);

        case '.'
          error (["categorical.subsasgn: '.' invalid indexing", ...
                  " for assigning field of non-structure array."]);
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Promote text arrays to categorical objects
    function varargout = promote (varargin)
      for i = 1:numel (varargin)
        val = varargin{i};
        if (isa (val, "categorical"))
          varargout{i} = val;
        elseif (iscellstr (val) || isa (val, "string") || ischar (val))
          val = cellstr (val);
          varargout{i} = categorical (val);
        elseif (isa (val, 'missing'))
          varargout{i} = categorical (nan (size (missing)));
        else
          error ("categorical: invalid input to constructor.");
        endif
      endfor
    endfunction

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.cats = this.cats(varargin{:});
      this.code = this.code(varargin{:});
    endfunction

  endmethods

endclassdef

## Custom function for displaying categorical summary
function dispcellmatrix (C)
  sz = terminal_size ();
  cols = sz(2) - 4;
  colgap = "     ";
  dispstr = {};
  optLens = [];
  for iCol = 1:size (C, 2)
    [outstr, optLen] = mixedcell2str (C(:, iCol));
    dispstr = [dispstr, outstr];
    optLens = [optLens, optLen];
  endfor
  if (sum (optLens + 6) <= cols) # all columns fit in terminal size
    rowSpat = "";
    for iCol = 1:size (C, 2)
      rowSpat = [rowSpat, sprintf("%%-%ds", optLens(iCol)), colgap];
    endfor
    for iRow = 1:size (C, 1)
      strrow = sprintf (rowSpat, dispstr{iRow,:});
      fprintf ("    %s\n", strrow);
    endfor
    fprintf ("\n");
  else  # we need to split rows
    optLen_cs = cumsum (optLens + 6);
    startCol = 1;
    while (! isempty (find (optLen_cs > cols)))
      stopCol = find (optLen_cs > cols, 1) - 1;
      rowSpat = "";
      for iCol = 1:stopCol
        rowSpat = [rowSpat, sprintf("%%-%ds", optLens(iCol)), colgap];
      endfor
      optLens(1:iCol) = [];
      optLen_cs = cumsum (optLens + 6);
      stopCol = stopCol + startCol - 1;
      fprintf ("Columns %d through %d:\n\n", startCol, stopCol);
      for iRow = 1:size (C, 1)
        strrow = sprintf (rowSpat, dispstr{iRow,[startCol:stopCol]});
        fprintf ("    %s\n", strrow);
      endfor
      fprintf ("\n");
      startCol = stopCol + 1;
    endwhile
    if (! isempty (optLens))
      for iCol = 1:length (optLens)
        rowSpat = [rowSpat, sprintf("%%-%ds", optLens(iCol)), colgap];
      endfor
      stopCol = startCol + iCol - 1;
      if (startCol == stopCol)
        fprintf ("Column %d:\n\n", startCol);
      else
        fprintf ("Columns %d through %d:\n\n", startCol, stopCol);
      endif
      for iRow = 1:size (C, 1)
        strrow = sprintf (rowSpat, dispstr{iRow,[startCol:stopCol]});
        fprintf ("    %s\n", strrow);
      endfor
      fprintf ("\n");
    endif
  endif
endfunction

## Custom function to convert a mixed cell array to cellstr array
function [dispstr, optLen]  = mixedcell2str (data)
  dispstr = cell (size (data));
  ## Preallocate indexes to avoid truncation when last elements are 0
  is_char = logical (zeros (size (data)));
  is_numb = is_char;

  ## Index numeric scalar and character row vector
  ve = cell2mat (cellfun (@(x) size (x,1), data, "UniformOutput", false)) == 1;

  ## Catch 'char' row vectors
  is_char(ve) = cellfun ('ischar', data(ve));
  sf = @(x) sprintf ("%s", x);
  dispstr(is_char) = cellfun (sf, data(is_char), "UniformOutput", false);

  ## Catch 'numeric' scalars
  is_numb(ve) = cellfun ('isnumeric', data(ve));
  sf = @(x) sprintf ("%d", x);
  dispstr(is_numb) = cellfun (sf, data(is_numb), "UniformOutput", false);

  ## Get optimal length
  optLen = max (cellfun (@length, dispstr));

  ## Pad data according to optimal length
  Ra_wB = sprintf("%%-%ds", optLen);
  fcn = @(x) sprintf (Ra_wB, x);
  dispstr(is_numb) = cellfun (fcn, dispstr(is_numb), "UniformOutput", false);

  La_wB = sprintf("%%+%ds", optLen);
  fcn = @(x) sprintf (La_wB, x);
  dispstr(is_char) = cellfun (fcn, dispstr(is_char), "UniformOutput", false);
endfunction
