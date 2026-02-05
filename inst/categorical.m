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

classdef categorical
  ## -*- texinfo -*-
  ## @deftp {datatypes} categorical
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
    isMissing = logical ([])
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
## 'categorical'      'dispstrings'      'cellstr'          'char'            ##
## 'double'           'single'           'int64'            'int32'           ##
## 'int16'            'int8'             'uint64'           'uint32'          ##
## 'uint16'           'uint8'                                                 ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} categorical (@var{A})
    ## @deftypefnx {categorical} {@var{C} =} categorical (@var{A}, @var{valueset})
    ## @deftypefnx {categorical} {@var{C} =} categorical (@var{A}, @var{valueset}, @var{catnames})
    ## @deftypefnx {categorical} {@var{C} =} categorical (@dots{}, @var{Name}, @var{Value})
    ##
    ## Create a new array of categorical values.
    ##
    ## @code{@var{C} = categorical (@var{A})} creates a categorical array
    ## @var{C} from the input array @var{A}, which can be numeric, logical,
    ## datetime, duration, string, or cell array of character vectors.  Input
    ## @var{A} can also be another categorical array.  The categories in @var{C}
    ## the sorted unique values from the input array @var{A}.  When the input
    ## array is string or cell array of character vectors, any leading or
    ## trailing white spaces are removed.  Missing values in the input array
    ## correspond to @qcode{<undefined>} elements in the created categorical
    ## array.  By default, there is no category for undefined values in the
    ## output array.
    ##
    ## @code{@var{C} = categorical (@var{A}, @var{valueset})} creates a
    ## categorical array from input @var{A} with the categories specified in
    ## @var{valueset}, which must be a vector of unique values.  The data type
    ## of input array @var{A} and @var{valueset} must be the same, unless they
    ## are string or cell arrays of character vectors, in which case they can be
    ## used interchangeably.  Similarly to intput array @var{A} any leading or
    ## trailing white spaces are removed, if @var{valueset} is a string or cell
    ## array of character vectors.
    ##
    ## @code{@var{C} = categorical (@var{A}, @var{valueset}, @var{catnames})}
    ## creates a categorical array from input @var{A} with the categories
    ## specified in @var{valueset} and named after the corresponding values in
    ## @var{catnames}, which must be specified either as a string array or a
    ## cell array of character vectors.  If omitted, @code{categorical} uses
    ## the cellstring representation of @var{valueset} to name the specified
    ## category names.  @var{catnames} must not contain any missing values, it
    ## may have duplicate names, and it must have the same number of elements as
    ## @var{valueset}.
    ##
    ## @code{@var{C} = categorical (@dots{}, @var{Name}, @var{Value})} further
    ## specifies additional parameters for creating categorical array @var{C}.
    ##
    ## @itemize
    ## @item @qcode{"Ordinal"} must be a logical scalar specifying that the
    ## categories in @var{C} have a numeric ordering relationship.  By default,
    ## it is @qcode{false} and @code{categorical} creates a non-ordinal array.
    ## The elements of unordered categorical arrays can only be compared for
    ## equality.  Any other relational operator cannot be used.  Setting
    ## @qcode{"Ordinal"} to @qcode{true} results in a categorical array with
    ## mathematically orderred categories.  The ordering goes from smallest to
    ## largest according to the order in @var{valueset} or the order or
    ## appearance in input array @var{A}, when @var{valueset} is not specified,
    ## in which case the unique values in @{A} are not sorted in order to set
    ## the categories.  Ordinal categorical arrays allow for relational
    ## operators such as @qcode{>=, >, <=, <}, as well as statistical operations
    ## such as @code{min}, @code{max}, and @code{median}.
    ##
    ## @item @qcode{"Protected"} must be a logical scalar specifying that the
    ## categories in @var{C} are protected.  By default, it is @qcode{false} for
    ## unordered categorical arrays and it is always @qcode{true} for ordinal
    ## categorical arrays.  Setting @qcode{"Protected"} to @qcode{true} prevents
    ## from assigning new values that do not correspond to existing categories.
    ## When @qcode{false}, assigning new values to the array automatically
    ## updates the categories. Hence, categorical arrays with differenct sets of
    ## categories can be combined/merged into a new array with set operations.
    ## @end itemize
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
        error (strcat ("categorical: 'Ordinal' variable indicator", ...
                       " must be either false (0) or true (1)."));
      endif
      if (! ismember (Protected, [0, 1]))
        error (strcat ("categorical: 'Protected' categories indicator", ...
                       " must be either false (0) or true (1)."));
      endif
      opt = "sorted";
      if (Ordinal)
        this.isOrdinal = true;
        this.isProtected = true;
        opt = "stable";
      elseif (Protected)
        this.isProtected = true;
      endif

      ## Handle categorical input first
      if (isa (x, 'categorical'))
        this.cats = x.cats;
        this.code = x.code;
        this.isMissing = x.isMissing;
        ## Set new categories if valueset argument is provided
        if (numel (args) > 0)
          valueset = cellstr (string (args{1}));
          this = setcats (this, valueset);
        endif
        ## Set new category names if catname argument is provided
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
          this.cats = catnames(:);
          ## Create index vector to categories
          index2cat = __grp2idx__ (catnames(:));
          ## Reassociate to user defined categories (only when required)
          if (! isempty (index2cat) && numel (index2cat) != unique (index2cat))
            valueset = 1:numel (valueset);
            fcn = @ (x) index2cat(find (valueset == x));
            tf = ! this.isMissing;
            this.code(tf) = uint16 (arrayfun (fcn, this.code(tf)));
          endif
        endif
        return;
      endif

      ## Handle input arguments
      index2cat = [];
      isnanvset = false;
      ## Input Array (x)
      if (iscellstr (x))
        classx = 'cellstr';
        isnanx = cellfun (@(x) isempty (strtrim (x)), x);
      elseif (isnumeric (x))
        classx = 'numeric';
        isnanx = isnan (x);
      elseif (isa (x, 'datetime'))
        classx = 'datetime';
        isnanx = isnat (x);
      elseif (isa (x, 'duration'))
        classx = 'duration';
        isnanx = isnan (x);
      elseif (islogical (x))
        classx = 'logical';
        isnanx = isnan (x);
      elseif (isa (x, 'string'))
        classx = 'string';
        isnanx = ismissing (x);
      else
        error ("categorical: invalid input type for X.");
      endif
      ## Categories (valueset)
      if (numel (args) > 0)
        valueset = args{1}(:);
        if (iscellstr (valueset))
          classv = 'cellstr';
        elseif (isnumeric (valueset))
          classv = 'numeric';
        else
          classv = class (valueset);
        endif
        if (any (strcmp (classx, {'cellstr', 'string'})))
          if (! (iscellstr (valueset) || isa (valueset, 'string')))
            error ("categorical: incompatible types of VALUESET and X.");
          endif
          isnanvset = ismissing (valueset);
        elseif (isequal (classx, classv))
          if (strcmp (classv, 'datetime'))
            isnanvset = isnat (valueset);
          else  # numeric, duration,
            isnanvset = isnan (valueset);
          endif
        else
          error ("categorical: types of VALUESET and X do not match.");
        endif
        if (sum (isnanvset) > 1)
          error ("categorical: VALUESET contains multiple missing values.");
        endif
        if (numel (unique (valueset)) < numel (valueset))
          error ("categorical: VALUESET contains non-unique values.");
        endif
        ## Category names (catnames)
        if (numel (args) > 1)
          catnames = args{2}(:);
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
          ## Create index vector to categories
          index2cat = __grp2idx__ (catnames);
        else
          ## Check valueset for missing or empty elements
          if (any (strcmp (classv, {'cellstr', 'string'})))
            catnames = cellstr (valueset);
            if (any (cellfun (@(x) isempty (strtrim (x)), valueset)))
              error (strcat ("categorical: VALUESET cannot contain empty or", ...
                             " missing text, unless CATNAMES are specified."));
            endif
          elseif (isnumeric (valueset))
            if (any (isnan (valueset)))
              error (strcat ("categorical: numeric VALUESET cannot contain", ...
                             " NaN values, unless CATNAMES are specified."));
            endif
            catnames = arrayfun (@num2str, valueset, "UniformOutput", false);
          elseif (strcmp (classv, 'logical'))
            if (all (valueset))
              catnames = {'true'};
            elseif (! any (valueset))
              catnames = {'false'};
            else
              catnames = {'false'; 'true'};
            endif
          elseif (strcmp (classv, 'datetime'))
            if (any (isnat (valueset)))
              error (strcat ("categorical: datetime VALUESET cannot contain", ...
                             " NaT values, unless CATNAMES are specified."));
            endif
            catnames = dispstrings (valueset);
          else  # duration
            if (any (isnan (valueset)))
              error (strcat ("categorical: duration VALUESET cannot contain", ...
                             " NaN values, unless CATNAMES are specified."));
            endif
            catnames = dispstrings (valueset);
          endif
        endif

      else
        ## Resolve valueset and catnames from input array
        if (strcmp (classx, 'numeric'))
          valueset = unique (x, opt);
          valueset = valueset(! isnan (valueset));
          catnames = arrayfun (@num2str, valueset, "UniformOutput", false);
        elseif (strcmp (classx, 'logical'))
          valueset = unique (x, opt);
          if (all (valueset))
            catnames = {'true'};
          elseif (! any (valueset))
            catnames = {'false'};
          else
            catnames = {'false'; 'true'};
          endif
        elseif (strcmp (classx, 'cellstr'))
          valueset = unique (x, opt);  # does not remove empty
          valueset = valueset(! cellfun (@isempty, valueset));
          catnames = valueset;
        elseif (strcmp (classx, 'string'))
          valueset = unique (x, opt);  # removes missing, but not empty
          valueset = valueset(! cellfun (@isempty, cellstr (valueset)));
          catnames = cellstr (valueset);
        elseif (strcmp (classx, 'datetime'))
          valueset = unique (x, opt);  # does not remove NaT
          fcn = @(x) strcmp (x, 'NaT');
          valueset = valueset(! cellfun (fcn, dispstrings (valueset)));
          catnames = dispstrings (valueset);
        elseif (strcmp (classx, 'duration'))
          valueset = unique (x, opt);  # does not remove NaN
          fcn = @(x) strcmp (x, 'NaN');
          valueset = valueset(! cellfun (fcn, dispstrings (valueset)));
          catnames = dispstrings (valueset);
        endif
      endif

      ## Associate input array with valueset
      [tf, loc] = ismember (x, valueset);
      maxc = intmax ('uint16');
      maxl = max (loc(:)) + sum (isnanvset);
      if (maxl > maxc)
        error (strcat ("categorical: too many categories; categorical", ...
                       " supports up to %d categories; this input has", ...
                       " %d."), maxc, maxl);
      endif

      ## If missing value in valueset is associated with a category name
      ## remove missing values in X from 'tf' and assign the index of the
      ## valueset that corresponds to the missing value to 'loc'
      if (any (isnanvset))
        tf = tf | isnanx;
        loc(find (isnanx)) = find (isnanvset);
      endif

      ## Reassociate to user defined categories (only when regrouping required)
      if (! isempty (index2cat))
        if (numel (index2cat) != unique (index2cat) || any (isnanvset))
          valueset = 1:numel (valueset);
          fcn = @ (x) index2cat(find (valueset == x));
          loc(tf) = arrayfun (fcn, loc(tf));
        endif
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
    ## @deftypefn {categorical} {@var{CM} =} char (@var{C})
    ##
    ## Convert categorical array to a 2-D character matrix.
    ##
    ## @code{@var{CM} = char (@var{C})} returns a character matrix @var{CM},
    ## which contains @code{numel (@var{C})} rows and each row contains the
    ## category name for the corresponding element of @code{@var{C}(:)}.
    ##
    ## @end deftypefn
    function CM = char (this)
      cstr = dispstrings (this);
      CM = char (cstr(:));
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
## 'summary'          'categories'       'countcats'        'length'          ##
## 'size'             'ndims'            'numel'            'keyHash'         ##
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
    ## @deftypefn  {categorical} {@var{C} =} countcats (@var{A})
    ## @deftypefnx {categorical} {@var{C} =} countcats (@var{A}, @var{dim})
    ##
    ## Count occurences of categories in a categorical array.
    ##
    ## @code{@var{C} = countcats (@var{A})} returns the number of elements for
    ## each category in @var{A}.  If @var{A} is a vector, @var{C} is also a
    ## vector with one element for each category in @var{A}.  If @var{A} is a
    ## matrix, @var{C} is a matrix with each column containing the category
    ## counts from each column of @var{A}.  For multidimensional arrays,
    ## @code{countcats} operates along the first non-singleton dimension.
    ##
    ## @code{@var{C} = countcats (@var{A}, @var{dim})} aperates along the
    ## dimension @var{dim}.
    ##
    ## @end deftypefn
    function C = countcats (A, dim = [])
      nc = numel (A.cats);
      if (isempty (dim))
        C = histc (A.code, [1:nc]);
      else
        if (! isscalar (dim) || fix (dim) != dim || dim < 0)
          error ("categorical.countcats: DIM must be a positive integer.");
        endif
        if (dim <= ndims (A))
          C = histc (A.code, [1:nc], dim);
        else
          C = repmat (zeros (size (A)), [ones(1, dim - 1), nc]);
          totnum = prod (size (A));
          linvec = 1:totnum;
          for i = 1:nc
            offset = (i - 1) * totnum;
            C(linvec + offset) = A.code == i;
          endfor
        endif
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{N} =} length (@var{C})
    ##
    ## Length of a categorical vector.
    ##
    ## @code{@var{N} = length (@var{C})} returns the size of the longest
    ## dimension of the categorical array @var{C}, unless any of its dimensions
    ## has zero length, in which case @code{length (@var{C})} returns 0.
    ##
    ## @end deftypefn
    function N = length (this)
      if (isempty (this.code))
        N = 0;
      else
        N = max (size (this.code));
      endif
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
    ## (number of elements) of each dimension for the categorical array @var{C}.
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
        error (strcat ("categorical.size: nargout > 1 but does", ...
                       " not match number of requested dimensions."));
      else
        for i = 1:nargout
          varargout{i} = sz(i);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} ndims (@var{C})
    ##
    ## Number of dimensions in a categorical array.
    ##
    ## @code{@var{out} = ndims (@var{C})} returns the number of dimensions of
    ## the categorical array @var{C}.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = ndims (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{out} =} numel (@var{C})
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

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{hey} =} keyHash (@var{C})
    ## @deftypefnx {categorical} {@var{hey} =} keyHash (@var{C}, @var{base})
    ##
    ## Generate a hash code for a categorical array.
    ##
    ## @code{@var{h} = keyHash (@var{C})} generates a @qcode{uint64} scalar that
    ## represents the input array @var{C}.  @code{keyHash} utilizes the 64-bit
    ## FMV-1a variant of the Fowler-Noll-Vo non-cryptographic hash function.
    ##
    ## @code{@var{h} = keyHash (@var{C}), @var{base}} also generates a 64-bit
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
      ## Initialize string with size, class name, flags, and categories
      size_str = sprintf ('%dx', size (this.code))(1:end-1);
      flag_str = sprintf ('-o%d-p%d:', this.isOrdinal, this.isProtected);
      cats_str = sprintf ('%s', this.cats{:});
      init_str = [size_str 'categorical' flag_str cats_str];
      if (isempty (this.code))
        if (base)
          if (! (isscalar (base) && isa (base, 'uint64')))
            error ("categorical.keyHash: BASE must be a UINT64 scalar.");
          endif
          key = __ckeyHash__(init_str, base);
        else
          key = __ckeyHash__(init_str);
        endif
      else
        cats = [this.cats(:); '<undefined>'];
        code = this.code(:);
        code(code == 0) = max (code) + 1;
        cstr = [cats{code}];
        if (base)
          if (! (isscalar (base) && isa (base, 'uint64')))
            error ("categorical.keyHash: BASE must be a UINT64 scalar.");
          endif
          key = __ckeyHash__([init_str cstr], base);
        else
          key = __ckeyHash__([init_str cstr]);
        endif
        key = __nkeyHash__(this.isMissing(:), key);
      endif
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'iscategory'       'iscolumn'         'isempty'          'isequal'         ##
## 'isequaln'         'ismatrix'         'ismember'         'ismissing'       ##
## 'isordinal'        'isprotected'      'isrow'            'isscalar'        ##
## 'issorted'         'issortedrows'     'isundefined'      'isvector'        ##
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
    ## @code{@var{TF} = iscolumn (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is a column
    ## vector, and @qcode{false} otherwise.  A column vector is a 2-D array for
    ## which @code{size (@var{X})} returns @code{[@var{N}, 1]} with non-negative
    ## @var{N}.
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
    ## @code{@var{TF} = isempty (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is empty, and
    ## @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function TF = isempty (this)
      TF = isempty (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{TF} =} isequal (@var{C1}, @var{C2})
    ## @deftypefnx {categorical} {@var{TF} =} isequal (@var{C1}, @var{C2}, @dots{})
    ##
    ## Return true if categorical arrays are equal.
    ##
    ## @code{@var{TF} = isequal (@var{C1}, @var{C2})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the categorical arrays @var{C1} and
    ## @var{C2} contain the same values, and @qcode{false} otherwise.  Either
    ## @var{C1} or @var{C2} may also be a string array, a missing object array,
    ## a character vector, or a cell array of character vectors, which will be
    ## promoted to a categorical array prior to comparison.
    ##
    ## If categorical arrays @var{C1} and @var{C2} are ordinal, they must have
    ## the same set and ordering of categories.  If neither are ordinal, the
    ## category names of each pair of elements are compared.  Hence, they do not
    ## need to have the same set of categories.
    ##
    ## @code{@var{TF} = isequal (@var{C1}, @var{C2}, @dots{})} returns a logical
    ## scalar @var{TF}, which is @qcode{true}, if all input arguments are equal,
    ## and @qcode{false} otherwise.
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
          return;
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
    ## Return true if categorical arrays are equal under the assumption that
    ## undefined elements are equal.
    ##
    ## @code{@var{TF} = isequaln (@var{C1}, @var{C2})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the categorical arrays @var{C1} and
    ## @var{C2} contain the same values or corresponding undefined elements, and
    ## @qcode{false} otherwise.  Either @var{C1} or @var{C2} may also be a
    ## string array, a missing object array, a character vector, or a cell array
    ## of character vectors, which will be promoted to a categorical array prior
    ## to comparison.
    ##
    ## If categorical arrays @var{C1} and @var{C2} are ordinal, they must have
    ## the same set and ordering of categories.  If neither are ordinal, the
    ## category names of each pair of elements are compared.  Hence, they do not
    ## need to have the same set of categories.
    ##
    ## @code{@var{TF} = isequaln (@var{C1}, @var{C2}, @dots{})} returns a
    ## logical scalar @var{TF}, which is @qcode{true}, if all input arguments
    ## are equal, and @qcode{false} otherwise.
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
          return;
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
    ## @code{@var{TF} = ismatrix (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is a matrix, and
    ## @qcode{false} otherwise.  A matrix is an array of any type where
    ## @code{ndims (@var{X}) == 2} and for which @code{size (@var{X})} returns
    ## @code{[@var{H}, @var{W}]} with non-negative @var{H} and @var{W}.
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
      ## Either A or B contain category name(s)
      if ((isa (A, 'string') || iscellstr (A)) && isa (B, 'categorical'))
        if (numel (varargin) > 0)
          error (strcat ("categorical.ismember: cannot use 'rows'", ...
                         " when testing against category names."));
        endif
        A = cellstr (A);
        [TF, index] = ismember (A, categories (B));
        return;
      elseif ((isa (B, 'string') || iscellstr (B)) && isa (A, 'categorical'))
        if (numel (varargin) > 0)
          error (strcat ("categorical.ismember: cannot use 'rows'", ...
                         " when testing against category names."));
        endif
        B = cellstr (B);
        [TF, index] = ismember (B, categories (A));
        return;
      endif
      if (numel (varargin) > 0)
        if (! ismatrix (A) || ! ismatrix (B) || size (A, 2) != size (B, 2))
          error (strcat ("categorical.ismember: cannot use 'rows' unless", ...
                         " both A and B are matrices with the same number", ...
                         " of columns."));
        endif
      endif
      ## Both ordinal
      if (isordinal (A) && isordinal (B))
        if (! isequal (categories (A), categories (B)))
          error (strcat ("categorical.ismember: ordinal categorical arrays", ...
                         " must have the same ordered set of categories."));
        endif
        [TF, idx] = ismember (double (A), double (B), varargin{:});
      elseif (isordinal (A) || isordinal (B))
        error ("categorical.ismember: both categorical arrays nust be ordinal.");
      else
        ## Compare the category names of each element (except undefined)
        A_idx = A.code != 0;
        B_idx = B.code != 0;
        cats_A = cell (size (A.code));
        cats_B = cell (size (B.code));
        cats_A(A_idx) = A.cats(A.code(A_idx));
        cats_A(! A_idx) = {''};
        cats_B(B_idx) = B.cats(B.code(B_idx));
        cats_B(! B_idx) = {''};
        [TF, index] = ismember (cats_A, cats_B, varargin{:});
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{out} =} ismissing (@var{C})
    ## @deftypefnx {categorical} {@var{out} =} ismissing (@var{C}, @var{indicator})
    ##
    ## Find missing elements in categorical array.
    ##
    ## @code{@var{TF} = ismissing (@var{C})} returns a logical array @var{TF}
    ## of the same size as @var{C} containing @qcode{true} for each
    ## corresponding element of @var{C} that does not have a value from one of
    ## the categories in @var{C} and @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = ismissing (@var{C}, @var{indicator})} also returns a
    ## logical array @var{TF} containing @qcode{true} for each corresponding
    ## element of @var{C} that does not have a value from one of the categories
    ## specified in @var{indicator} and @qcode{false} otherwise.
    ##
    ## @var{indicator} must be either a categorical array or a character vector
    ## or a string vector or a cell vector of character vectors.  When the
    ## @var{indicator} contains text representation, the comparison is based on
    ## lexicographical equality to the category names of @var{C}.
    ##
    ## @end deftypefn
    function TF = ismissing (this, varargin)
      if (nargin > 2)
        error ("categorical.ismissing: too many input arguments.");
      endif
      if (! isempty (varargin))
        indicator = varargin{1};
        TF = false (size (this));
        if (isvector (indicator))
          if (ischar (indicator))
            TF(this == indicator) = true;
          elseif (isstring (indicator))
            for i = 1:numel (indicator)
              cat = indicator.strs(i);
              TF(this == cat) = true;
            endfor
          elseif (iscellstr (indicator))
            for i = 1:numel (indicator)
              cat = indicator(i);
              TF(this == cat) = true;
            endfor
          elseif (isa (indicator, 'categorical'))
            for i = 1:length (indicator)
              idx = indicator.code(i);
              if (idx)
                cat = indicator.cats{idx};
                TF(this == cat) = true;
              else  # code = 0 corresponds to undefined categories
                TF(this.isMissing) = true;
              endif
            endfor
          else
            error ("categorical.ismissing: INDICATOR must be a text array.");
          endif
        else
          error ("categorical.ismissing: INDICATOR must be a vector.");
        endif
      else
        TF = this.isMissing;
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} isordinal (@var{C})
    ##
    ## Test if categorical array is ordinal.
    ##
    ## @code{@var{TF} = isordinal (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is ordinal, and
    ## @qcode{false} otherwise.
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
    ## @code{@var{TF} = isprotected (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is protected, and
    ## @qcode{false} otherwise.
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
    ## @code{@var{TF} = isrow (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is a row vector,
    ## and @qcode{false} otherwise.  A row vector is a 2-D array for which
    ## @code{size (@var{X})} returns @code{[1, @var{N}]} with non-negative
    ## @var{N}.
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
    ## @code{@var{TF} = isscalar (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is also a scalar,
    ## and @qcode{false} otherwise.  A scalar is a single element object for
    ## which @code{size (@var{X})} returns @code{[1, 1]}.
    ##
    ## @end deftypefn
    function TF = isscalar (this)
      TF = isscalar (this.code);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{TF} =} issorted (@var{C})
    ## @deftypefnx {categorical} {@var{TF} =} issorted (@var{C}, @var{dim})
    ## @deftypefnx {categorical} {@var{TF} =} issorted (@var{C}, @var{direction})
    ## @deftypefnx {categorical} {@var{TF} =} issorted (@var{C}, @var{dim}, @var{direction})
    ## @deftypefnx {categorical} {@var{TF} =} issorted (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ##
    ## Return true if categorical array is sorted.
    ##
    ## @code{@var{TF} = issorted (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true}, if the categorical array @var{C} is sorted in
    ## ascending order, and @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = issorted (@var{C}, @var{dim})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the categorical array @var{C} is
    ## sorted in ascending order along the dimension @var{dim}, and
    ## @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = issorted (@var{A}, @var{direction})} returns a logical
    ## scalar @var{TF}, which is @qcode{true}, if the categorical array @var{C}
    ## is sorted in the direction specified by @var{direction}, and
    ## @qcode{false} otherwise.  @var{direction} can be any of the following
    ## options:
    ##
    ## @itemize
    ## @item @qcode{'ascend'}, which is the default, checks is elements are in
    ## ascending order.
    ## @item @qcode{'descend'} checks if elements are in descending order.
    ## @item @qcode{'monotonic'} checks if elements are either in ascending or
    ## descending order.
    ## @item @qcode{'strictascend'} checks if elements are in ascending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictdescend'} checks if elements are in descending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictmonotonic'} checks if elements are either in
    ## ascending or descending order and there are no duplicate or undefined
    ## elements.
    ## @end itemize
    ##
    ## @code{@var{TF} = issorted (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where missing elements (@qcode{<undefined>}) are placed with
    ## any of the following options specified in @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = issorted (this, varargin)
      ## Single input argument
      if (nargin == 1)
        TF = isequal (this, sort (this));
        return;
      endif

      ## Get direction
      fcn = @(x) ischar (x) && ! strcmpi (x, 'MissingPlacement');
      cid = cellfun (fcn, varargin);
      if (any (cid))
        direction = varargin{cid};
        ## Check for type of direction
        valid = {'ascend', 'descend', 'monotonic', 'strictascend', ...
                 'strictdescend', 'strictmonotonic'};
        if (! ismember (direction, valid_direction))
          error ("categorical.issorted: invalid DIRECTION value.");
        endif
        switch (direction)
          case {'ascend', 'descend'}
            TF = isequal (this, sort (this, varargin(:)));

          case {'strictascend', 'strictdescend'}
            ## Check for missing values first (fast)
            if (any (this.isMissing(:)))
              TF = false;
              return;
            endif
            varargin{cid} = strrep (direction, 'strict', '');
            sorted = unique (sort (this, varargin{:}), 'stable');
            TF = isequal (this, sorted);

          case 'monotonic'
            ## Check for either ascending or descending
            varargin{cid} = 'ascend';
            TF = isequal (this, sort (this, varargin{:}));
            if (TF)
              return;
            endif
            varargin{cid} = 'descend';
            TF = isequal (this, sort (this, varargin{:}));

          case 'strictmonotonic'
            ## Check missing values first (fast)
            if (any (this.isMissing(:)))
              TF = false;
              return;
            endif
            ## Check for either ascending or descending
            varargin{cid} = 'ascend';
            sorted = unique (sort (this, varargin{:}), 'stable');
            TF = isequal (this, sorted);
            if (TF)
              return;
            endif
            varargin{cid} = 'descend';
            sorted = unique (sort (this, varargin{:}), 'stable');
            TF = isequal (this, sorted);
        endswitch
      else
        ## No DIRECTION input argument
        TF = isequal (this, sort (this, varargin{:}));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{TF} =} issortedrows (@var{C})
    ## @deftypefnx {categorical} {@var{TF} =} issortedrows (@var{C}, @var{col})
    ## @deftypefnx {categorical} {@var{TF} =} issortedrows (@var{C}, @var{direction})
    ## @deftypefnx {categorical} {@var{TF} =} issortedrows (@var{C}, @var{col}, @var{direction})
    ## @deftypefnx {categorical} {@var{TF} =} issortedrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ##
    ## Return true if categorical matrix rows are sorted.
    ##
    ## @code{@var{TF} = issortedrows (@var{C})} returns a logical scalar
    ## @var{TF}, which is @qcode{true}, if the rows in the 2-D categorical array
    ## @var{C} are sorted in ascending order, and @qcode{false} otherwise.
    ##
    ## @code{@var{TF} = issortedrows (@var{C}, @var{col})} returns a logical
    ## scalar @var{TF}, which is @qcode{true}, if the categorical array @var{C}
    ## is sorted according to the columns specified by the vector @var{col}, and
    ## @qcode{false} otherwise.  @var{col} must explicitly contain non-zero
    ## integers whose absolute values index  existing columns in @var{A}.
    ## Positive elements sort the corresponding columns in ascending order,
    ## while negative elements sort the corresponding columns in descending
    ## order.
    ##
    ## @code{@var{TF} = issortedrows (@var{C}, @var{direction})} checks if the
    ## rows in @var{C} are sorted according to the specified direction, which
    ## can be any of the following options:
    ##
    ## @itemize
    ## @item @qcode{'ascend'}, which is the default, checks is elements are in
    ## ascending order.
    ## @item @qcode{'descend'} checks if elements are in descending order.
    ## @item @qcode{'monotonic'} checks if elements are either in ascending or
    ## descending order.
    ## @item @qcode{'strictascend'} checks if elements are in ascending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictdescend'} checks if elements are in descending order
    ## and there are no duplicate or undefined elements.
    ## @item @qcode{'strictmonotonic'} checks if elements are either in
    ## ascending or descending order and there are no duplicate or undefined
    ## elements.
    ## @end itemize
    ##
    ## Alternatively, @var{direction} can be a cell array array of character
    ## vectors specifying the sorting direction for each individual column of
    ## @var{A}, in which case the number of elements in @var{direction} must
    ## equal the number of columns in @var{A}.
    ##
    ## @code{@var{B} = issortedrows (@var{A}, @var{col}, @var{direction})}
    ## checks if the rows in the categorical array @var{A} are sorted according
    ## to the columns specified in @var{col} using the corresponding sorting
    ## direction specified in @var{direction}.  In this case, the sign of the
    ## values in @var{col} is ignored.  @var{col} and @var{direction} must have
    ## the same length, but not necessarily the same number of elements as the
    ## columns in @var{A}.
    ##
    ## @code{@var{TF} = issorted (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where missing elements (@qcode{<undefined>}) are placed with
    ## any of the following options specified in @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @end deftypefn
    function TF = issortedrows (this, varargin)
      ## Single input argument
      if (nargin == 1)
        TF = isequal (this, sortrows (this));
        return;
      endif

      ## Get direction
      opt_out_arg = 'MissingPlacement';
      fcn = @(x) (ischar (x) && ! strcmpi (x, opt_out_arg)) || iscellstr (x);
      cid = cellfun (fcn, varargin);
      if (any (cid))
        if (sum (cid) > 1)
          error ("categorical.issortedrows: invalid input arguments.");
        endif
        direction = cellstr (varargin{cid});

        ## Check for valid type of direction
        valid = {'ascend', 'descend', 'monotonic', 'strictascend', ...
                 'strictdescend', 'strictmonotonic'};
        if (! all (cellfun (@(x) ismember (x, valid), direction)))
          error ("categorical.issortedrows: invalid DIRECTION value.");
        endif

        ## Handle non-strict modes first
        simple_types = {'ascend', 'descend', 'monotonic'};
        if (all (cellfun (@(x) ismember (x, {'ascend', 'descend'}), direction)))
          TF = isequal (this, sortrows (this, varargin(:)));
          return;
        endif
        if (all (cellfun (@(x) ismember (x, simple_types), direction)))
          idx = strcmp (direction, 'monotonic');
          direction{idx} = 'ascend';
          varargin{cid} = direction;
          TF = isequal (this, sortrows (this, varargin(:)));
          if (TF)
            return;
          endif
          direction{idx} = 'descend';
          varargin{cid} = direction;
          TF = isequal (this, sort (this, varargin{:}));
          return;
        endif

        ## Handle strict modes
        strict_types = {'strictascend', 'strictdescend', 'strictmonotonic'};
        idx = cellfun (@(x) ismember (x, strict_types), direction);

        ## Get COL vector (if given) from input arguments and find
        ## the corresponding columns for which strict modes apply.
        col = cellfun (@isnumeric, varargin);
        if (col)   # COL is available
          col = varargin{col};
          if (numel (direction) != numel (col))
            error ("categorical.issortedrows: COL and DIRECTION mismatch.");
          endif
          strict_idx = abs (col(idx));  # remove negative numbers (if any)
        else       # only DIRECTION is available
          strict_idx = idx;
        endif

        ## Check for missing values first (fast)
        if (any (any (this.isMissing(:,strict_idx))))
          TF = false;
          return;
        endif

        ## Change strict modes to simple modes
        direction = strrep (direction, 'strict', '');
        varargin{cid} = direction;

        ## Operate with simple modes and check strictness on selected columns
        if (all (cellfun (@(x) ismember (x, {'ascend', 'descend'}), direction)))
          sorted = sortrows (this, varargin{:});
          ## Test 'strict' columns for unique rows
          tmpcol = subset (sorted, ':', strict_idx);
          [~, ix] = unique (tmpcol, 'rows', 'stable');
          sorted = subset (sorted, ix, ':');
          TF = isequal (this, sorted);

        else  # 'monotonic' mode also exists
          idx = strcmp (direction, 'monotonic');
          direction{idx} = 'ascend';
          varargin{cid} = direction;
          sorted = sortrows (this, varargin{:});
          ## Test 'strct' columns for unique rows
          tmpcol = subset (sorted, ':', strict_idx);
          [~, ix] = unique (tmpcol, 'rows', 'stable');
          sorted = subset (sorted, ix, ':');
          TF = isequal (this, sorted);
          if (TF)
            return;
          endif
          direction{idx} = 'descend';
          varargin{cid} = direction;
          sorted = sortrows (this, varargin{:});
          ## Test 'strct' columns for unique rows
          tmpcol = subset (sorted, ':', strict_idx);
          [~, ix] = unique (tmpcol, 'rows', 'stable');
          sorted = subset (sorted, ix, ':');
          TF = isequal (this, sorted);
        endif
      else
        ## No DIRECTION input argument
        TF = isequal (this, sortrows (this, varargin{:}));
      endif
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
    ## @qcode{<undefined>} is the equivalent of @qcode{NaN} in numeric arrays.
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
    ## @code{@var{TF} = isvector (@var{C})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the categorical array @var{C} is a vector and
    ## @qcode{false} otherwise.  A vector is a 2-D array for which one of the
    ## dimensions is equal to 1 (either @math{1xN} or @math{Nx1}).  By
    ## definition, a scalar is also a vector.
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
    ## @var{catname} must be either a character vector, a cellstr scalar or a
    ## string scalar.  @var{newcats} may be a cell array of character vectors or
    ## any type of array that can be converted to a cell array of character
    ## vectors with the @code{cellstr} function, as long as it does contain any
    ## duplicate names and does not reference an existing category in @var{A}.
    ##
    ## @end deftypefn
    function B = addcats (A, newcats, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("categorical:addcats: too few input arguments.");
      elseif (isempty (newcats))
        error ("categorical:addcats: NEWCATS cannot be empty.");
      endif

      ## Convert to cellstring
      try
        newcats = cellstr (newcats);
      catch
        error ("categorical:addcats: NEWCATS cannot be converted to cellstr.");
      end_try_catch

      ## New catnames must be unique and non-existing
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
        error (strcat (["categorical.addcats: cannot use both", ...
                        " 'After' and 'Before' options."]));
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
    ## @var{newcat} must be either a character vector, a cellstr scalar or a
    ## string scalar.  @var{oldcats} may be a cell array of character vectors or
    ## any type of array that can be converted to a cell array of character
    ## vectors with the @code{cellstr} function.  Any names in @var{oldcats}
    ## that do not reference an existing category are ignored.
    ##
    ## @end deftypefn
    function B = mergecats (A, oldcats, varargin)

      ## Check input arguments
      if (nargin < 2)
        error ("categorical:mergecats: too few input arguments.");
      elseif (isempty (oldcats))
        error ("categorical:mergecats: OLDCATS cannot be empty.");
      endif

      ## Convert to cellstring
      try
        oldcats = cellstr (oldcats);
      catch
        error ("categorical:mergecats: NEWCATS cannot be converted to cellstr.");
      end_try_catch

      ## Check for optional third argument
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
        error (strcat ("categorical.mergecats: only consecutive categories", ...
                       " can be merged in ordinal categorical arrays."));
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
    ## @var{oldcats} may be a cell array of character vectors or any type of
    ## array that can be converted to a cell array of character vectors with the
    ## @code{cellstr} function.  Any names in @var{oldcats} that do not
    ## reference an existing category are ignored.
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
        ## Convert to cellstring
        try
          oldcats = cellstr (varargin{1});
        catch
          error (strcat ("categorical:removecats: OLDCATS", ...
                         " cannot be converted to cellstr."));
        end_try_catch

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
    ## @code{@var{B} = renamecats (@var{A}, @var{newnames})} renames all the
    ## categories in @var{A}, without changing any of its values, with the names
    ## specified in @var{newnames}.  @var{newnames} may be a cell array of
    ## character vectors or any type of array that can be converted to a cell
    ## array of character vectors with the @code{cellstr} function, as long as
    ## it has the same number of elements as the categories in @var{A}.
    ##
    ## @code{@var{B} = renamecats (@var{A}, @var{oldnames}, @var{newnames})}
    ## renames the categories of @var{A} specified in @var{oldnames} with the
    ## names specified in @var{newnames}.  Both @var{oldnames} and
    ## @var{newnames} may be a cell arrays of character vectors or any type of
    ## array that can be converted to a cell array of character vectors with the
    ## @code{cellstr} function, as long as they have the same number of
    ## elements.  @var{oldnames} must specify a subset of existing categories in
    ## @var{A}.
    ##
    ## @end deftypefn
    function B = renamecats (A, varargin)
      ## Check input arguments
      if (nargin < 2)
        error ("categorical:renamecats: too few input arguments.");
      endif

      ## Process input arguments
      if (nargin == 2)
        oldnames = A.cats;
        ## Convert to cellstring
        try
          newnames = cellstr (varargin{1});
        catch
          error (strcat ("categorical:renamecats: NEWNAMES", ...
                         " cannot be converted to cellstr."));
        end_try_catch
        if (numel (oldnames) != numel (newnames))
          error (strcat ("categorical:renamecats: NEWNAMES must equal the", ...
                         " number of existing categories in input array."));
        endif
      else
        ## Convert to cellstring
        try
          oldnames = cellstr (varargin{1});
        catch
          error (strcat ("categorical:renamecats: OLDNAMES", ...
                         " cannot be converted to cellstr."));
        end_try_catch
        try
          newnames = cellstr (varargin{2});
        catch
          error (strcat ("categorical:renamecats: NEWNAMES", ...
                         " cannot be converted to cellstr."));
        end_try_catch
        if (numel (oldnames) != numel (newnames))
          error (strcat ("categorical:renamecats: OLDNAMES and NEWNAMES", ...
                         " must have the same number of elements."));
        endif
      endif

      ## Find and rename existing categories
      [TF, index] = ismember (oldnames, A.cats);
      if (! all (TF))
        error (strcat ("categorical.renamecats: OLDNAMES must be", ...
                       " a subset of existing categories."));
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
    ## which may be a cell array of character vectors or any type of array that
    ## can be converted to a cell array of character vectors with the
    ## @code{cellstr} function as long as it contains the same set with the
    ## existing categories in @var{A}.
    ##
    ## @end deftypefn
    function B = reordercats (A, varargin)
      if (nargin == 1)
        neworder = sort (A.cats);
      else
        ## Convert to cellstring
        try
          neworder = cellstr (varargin{1});
        catch
          error (strcat ("categorical:reordercats: NEWORDER", ...
                         " cannot be converted to cellstr."));
        end_try_catch
        if (! all (ismember (neworder, A.cats)))
          error ("categorical.reordercats: NEWORDER must contain", ...
                 " the same set with the existing categories.");
        endif
      endif
      ## Reorder
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
    ## @code{@var{B} = setcats (@var{A}, @var{newcats})} sets categories in the
    ## categorical array @var{B} according to the elements of the input array
    ## @var{A} and the categories specified by @var{newcats} according to the
    ## following rules:
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
    ## @var{newcats} may be a cell array of character vectors or any type of
    ## array that can be converted to a cell array of character vectors with the
    ## @code{cellstr} function.
    ##
    ## @end deftypefn
    function B = setcats (A, newcats)
      ## Check input arguments
      if (nargin < 2)
        error ("categorical:setcats: too few input arguments.");
      endif

      ## Convert to cellstring
      try
        newcats = cellstr (newcats);
      catch
        error ("categorical:setcats: NEWCATS cannot be converted to cellstr.");
      end_try_catch
      ## Find existing categories
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
    ## @code{@var{C} = @var{A} .* @var{B}} and returns a categorical array whose
    ## categories are the Cartesian product of the categories in @var{A} and
    ## @var{B} and each element is indexed to a new category which is the
    ## combination of the categories of the corresponding elements in @var{A}
    ## and @var{B}.
    ##
    ## @var{A} and @var{B} must be of common size or scalar categorical arrays.
    ##
    ## @end deftypefn
    function C = times (A, B)
      ## Check input arguments
      if (nargin < 2)
        error ("categorical:times: too few input arguments.");
      endif
      if (! isa (A, 'categorical') || ! isa (B, 'categorical'))
        error ("categorical:times: A and B must be categorical arrays.");
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

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Equality for categorical arrays.
    ##
    ## @code{@var{TF} = eq (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} == @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{true}
    ## where the corresponding elements of @var{A} and @var{B} are equal and set
    ## to @qcode{false} where they are not.  @var{A} and @var{B} must be size
    ## compatible, which translates to they can be the same size, one can be
    ## scalar, or for every dimension, their dimension sizes must be equal or
    ## one of them must be 1.
    ##
    ## If categorical arrays @var{A} and @var{B} are ordinal, they must have
    ## the same set and ordering of categories.  If neither are ordinal, the
    ## category names of each pair of elements are compared.  Hence, they do
    ## not need to have the same set of categories.
    ##
    ## One of the input arguments can also be a character vector, a cellstr
    ## scalar or a string scalar as long as the other is a categorical array.
    ## In this case, a logical array of the same size as the categorical array
    ## is returned in which every element is tested for equality by comparing
    ## its category with that specified by the string argument.
    ##
    ## Undefined elements always return @qcode{false}, since they are not
    ## comparable to any other categorical values including other undefined
    ## elements.
    ##
    ## @end deftypefn
    function TF = eq (A, B)
      if (iscellstr (B) || isa (B, 'string') || ischar (B))
        B = cellstr (B);
        if (! isscalar (B))
          error ("categorical.eq: incompatible size for comparison.");
        endif
        TF = strcmp (B, cellstr (A));
      elseif (iscellstr (A) || isa (A, 'string') || ischar (A))
        A = cellstr (A);
        if (! isscalar (A))
          error ("categorical.eq: incompatible size for comparison.");
        endif
        TF = strcmp (A, cellstr (B));
      elseif (iscategorical (A) && iscategorical (B))
        if (A.isOrdinal && B.isOrdinal)
          ## Check that both categorical arrays have the same categories
          ## and they are in the same order
          cats = cellfun (@(x) categories (x), {A, B}, 'UniformOutput', false);
          if (! isequal (cats{:}))
            error (strcat ("categorical.eq: comparison between ordinal", ...
                           " arrays requires that both have the same", ...
                           " categories, which must be ordered in the", ...
                           " same way."));
          endif
          codes = cellfun (@(x) x.code, {A, B}, 'UniformOutput', false);
          TF = codes{1} == codes{2};
        elseif (A.isOrdinal || B.isOrdinal)
          error (strcat ("categorical.eq: cannot compare a categorical", ...
                         " array that is ordinal with one that is not."));
        else
          TF = strcmp (cellstr (A), cellstr (B));
        endif
      else
        error (strcat ("categorical.eq: comparison is not defined between", ...
                       " '%s' and '%s' arrays."), class (A), class (B));
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} ge (@var{A}, @var{B})
    ##
    ## Greater than or equal to for ordinal categorical arrays.
    ##
    ## @code{@var{TF} = ge (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} >= @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{true}
    ## where the corresponding elements of @var{A} are greater than or equal to
    ## @var{B} and set to @qcode{false} where they are not.  @var{A} and @var{B}
    ## must be size compatible, which translates to they can be the same size,
    ## one can be scalar, or for every dimension, their dimension sizes must be
    ## equal or one of them must be 1.
    ##
    ## If categorical arrays @var{A} and @var{B} are both ordinal, they must
    ## have the same set and ordering of categories.  Unordered categorical
    ## arrays cannot be compared for greater than or equal to inequality.
    ##
    ## One of the input arguments can also be a character vector, a cellstr
    ## scalar or a string scalar as long as the other is a categorical array.
    ## In this case, a logical array of the same size as the categorical array
    ## is returned in which every element is tested for greater than or equal to
    ## inequality by comparing its category with that specified by the string
    ## argument.
    ##
    ## Undefined elements always return @qcode{false}, since they are not
    ## comparable to any other categorical values including other undefined
    ## elements.
    ##
    ## @end deftypefn
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
        error (strcat ("categorical.ge: relational comparison is not", ...
                       " allowed for non-ordinal categorical arrays."));
      else
        TF = double (A) >= double (B);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} gt (@var{A}, @var{B})
    ##
    ## Greater than for ordinal categorical arrays.
    ##
    ## @code{@var{TF} = gt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} > @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{true}
    ## where the corresponding elements of @var{A} are greater than @var{B} and
    ## set to @qcode{false} where they are not.  @var{A} and @var{B} must be
    ## size compatible, which translates to they can be the same size, one can
    ## be scalar, or for every dimension, their dimension sizes must be equal or
    ## one of them must be 1.
    ##
    ## If categorical arrays @var{A} and @var{B} are both ordinal, they must
    ## have the same set and ordering of categories.  Unordered categorical
    ## arrays cannot be compared for greater than inequality.
    ##
    ## One of the input arguments can also be a character vector, a cellstr
    ## scalar or a string scalar as long as the other is a categorical array.
    ## In this case, a logical array of the same size as the categorical array
    ## is returned in which every element is tested for greater than inequality
    ## by comparing its category with that specified by the string argument.
    ##
    ## Undefined elements always return @qcode{false}, since they are not
    ## comparable to any other categorical values including other undefined
    ## elements.
    ##
    ## @end deftypefn
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
        error (strcat ("categorical.lt: relational comparison is not", ...
                       " allowed for non-ordinal categorical arrays."));
      else
        TF = double (A) > double (B);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} le (@var{A}, @var{B})
    ##
    ## Less than or equal to for ordinal categorical arrays.
    ##
    ## @code{@var{TF} = le (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} <= @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{true}
    ## where the corresponding elements of @var{A} are less than or equal to
    ## @var{B} and set to @qcode{false} where they are not.  @var{A} and @var{B}
    ## must be size compatible, which translates to they can be the same size,
    ## one can be scalar, or for every dimension, their dimension sizes must be
    ## equal or one of them must be 1.
    ##
    ## If categorical arrays @var{A} and @var{B} are both ordinal, they must
    ## have the same set and ordering of categories.  Unordered categorical
    ## arrays cannot be compared for less than or equal to inequality.
    ##
    ## One of the input arguments can also be a character vector, a cellstr
    ## scalar or a string scalar as long as the other is a categorical array.
    ## In this case, a logical array of the same size as the categorical array
    ## is returned in which every element is tested for less than or equal to
    ## inequality by comparing its category with that specified by the string
    ## argument.
    ##
    ## Undefined elements always return @qcode{false}, since they are not
    ## comparable to any other categorical values including other undefined
    ## elements.
    ##
    ## @end deftypefn
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
        error (strcat ("categorical.le: relational comparison is not", ...
                       " allowed for non-ordinal categorical arrays."));
      else
        TF = double (A) <= double (B);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} lt (@var{A}, @var{B})
    ##
    ## Less than for ordinal categorical arrays.
    ##
    ## @code{@var{TF} = lt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} < @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{true}
    ## where the corresponding elements of @var{A} are less than @var{B} and
    ## set to @qcode{false} where they are not.  @var{A} and @var{B} must be
    ## size compatible, which translates to they can be the same size, one can
    ## be scalar, or for every dimension, their dimension sizes must be equal or
    ## one of them must be 1.
    ##
    ## If categorical arrays @var{A} and @var{B} are both ordinal, they must
    ## have the same set and ordering of categories.  Unordered categorical
    ## arrays cannot be compared for less than inequality.
    ##
    ## One of the input arguments can also be a character vector, a cellstr
    ## scalar or a string scalar as long as the other is a categorical array.
    ## In this case, a logical array of the same size as the categorical array
    ## is returned in which every element is tested for less than inequality
    ## by comparing its category with that specified by the string argument.
    ##
    ## Undefined elements always return @qcode{false}, since they are not
    ## comparable to any other categorical values including other undefined
    ## elements.
    ##
    ## @end deftypefn
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
        error (strcat ("categorical.lt: relational comparison is not", ...
                       " allowed for non-ordinal categorical arrays."));
      else
        TF = double (A) < double (B);
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{TF} =} ne (@var{A}, @var{B})
    ##
    ## Not equal for categorical arrays.
    ##
    ## @code{@var{TF} = ne (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} != @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{true}
    ## where the corresponding elements of @var{A} and @var{B} are not equal and
    ## set to @qcode{false} where they are equal.  @var{A} and @var{B} must be
    ## size compatible, which translates to they can be the same size, one can
    ## be scalar, or for every dimension, their dimension sizes must be equal or
    ## one of them must be 1.
    ##
    ## If categorical arrays @var{A} and @var{B} are ordinal, they must have
    ## the same set and ordering of categories.  If neither are ordinal, the
    ## category names of each pair of elements are compared.  Hence, they do
    ## not need to have the same set of categories.
    ##
    ## One of the input arguments can also be a character vector, a cellstr
    ## scalar or a string scalar as long as the other is a categorical array.
    ## In this case, a logical array of the same size as the categorical array
    ## is returned in which every element is tested for inequality by comparing
    ## its category with that specified by the string argument.
    ##
    ## Undefined elements always return @qcode{true}, since they are not
    ## comparable to any other categorical values including other undefined
    ## elements.
    ##
    ## @end deftypefn
    function TF = ne (A, B)
      TF = ! eq (A, B);
    endfunction

  endmethods

################################################################################
##                        ** Arithmetic Operations **                         ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'min'              'mink'             'max'              'maxk'            ##
## 'median'           'mode'             'histcounts'                         ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} min (@var{A})
    ## @deftypefnx {categorical} {[@var{C}, @var{index}] =} min (@var{A})
    ## @deftypefnx {categorical} {@var{C} =} min (@var{A}, @qcode{[]}, @var{dim})
    ## @deftypefnx {categorical} {@var{C} =} min (@var{A}, @qcode{[]}, @var{vecdim})
    ## @deftypefnx {categorical} {@var{C} =} min (@var{A}, @qcode{[]}, @qcode{'all'})
    ## @deftypefnx {categorical} {[@var{C}, @var{index}] =} min (@var{A}, @qcode{[]}, @dots{})
    ## @deftypefnx {categorical} {@var{C} =} min (@var{A}, @var{B})
    ## @deftypefnx {categorical} {[@dots{}] =} min (@dots{}, @var{missingflag})
    ##
    ## Smallest element in an ordinal categorical array.
    ##
    ## @code{@var{C} = min (@var{A})} returns the smallest element in ordinal
    ## categorical vector @var{A}.  If @var{A} is a matrix, @code{min (@var{A})}
    ## returns a row vector with the smallest element from each column.  For
    ## multidimensional arrays, @code{min (@var{A})} operates along the first
    ## non-singleton dimension.
    ##
    ## @code{[@var{C}, @var{index}] = min (@var{A})} also returns the indices of
    ## the minimum values in @var{index}, which has the same size as @var{C}.
    ## When the operating dimension contains more than one minimal elements, the
    ## index of the first one is returned.
    ##
    ## @code{@var{C} = min (@var{A}, @qcode{[]}, @var{dim})} operates along the
    ## dimension specified by @var{dim}.
    ##
    ## @code{@var{C} = min (@var{A}, @qcode{[]}, @var{vecdim})} operates on all
    ## the elements contained in the dimensions specified by @var{vecdim}, which
    ## must be a numeric vector of non-repeating positive integers.  Any values
    ## in @var{vecdim} indexing dimensions larger that the actual array @var{A}
    ## are ignored.
    ##
    ## @code{@var{C} = min (@var{A}, @qcode{[]}, @qcode{'all'})} operates on all
    ## dimensions and returns the smallest element in @var{A}.
    ##
    ## @code{[@var{C}, @var{index}] = min (@var{A}, @qcode{[]}, @dots{})} also
    ## returns the indices of the minimum values in @var{index}, using any of
    ## the previous syntaxes.
    ##
    ## @code{@var{C} = min (@var{A}, @var{B})} returns an ordinal categorical
    ## array @var{C} with the smallest elements from @var{A} and @var{B}, which
    ## both must be ordinal categorical arrays of compatible sizes with the same
    ## set and ordering of categories.  Compatible size means that @var{A} and
    ## @var{B} can be the same size, one can be scalar, or for every dimension,
    ## their dimension sizes must be equal or one of them must be 1.
    ##
    ## @code{[@dots{}] = min (@dots{}, @var{missingflag})} specifies how to
    ## handle undefined elements in any of the previous syntaxes.
    ## @var{missingflag} must be a character vector or a string scalar with one
    ## of the following values:
    ##
    ## @itemize
    ## @item @qcode{'omitundefined'}, which is the default, ignores all
    ## undefined elements and returns the minimum of the remaining elements.  If
    ## all elements along the operating dimension are undefined, then it returns
    ## an undefined element.  @qcode{'omitnan'} may also be used as equivalent
    ## to @qcode{'omitundefined'}.
    ## @item @qcode{'includeundefined'} returns an undefined element if there
    ## any undefined elements along the operating dimension.
    ## @qcode{'includenan'} may also be used as equivalent to
    ## @qcode{'includeundefined'}.
    ## @end itemize
    ##
    ## @end deftypefn
    function [C, index] = min (A, B = [], varargin)
      ## Check for ordinal categorical array
      if (! A.isOrdinal)
        error ("categorical.min: categorical array A is not ordinal.");
      endif
      if (numel (varargin) > 2)
        error ("categorical.min: too many input arguments.");
      endif
      ## Get missing flag
      omitflag = true;
      if (numel (varargin) > 0)
        if (ischar (varargin{end}) || isa (varargin{end}, 'string'))
          if (any (strcmpi (varargin{end}, {'includeundefined', 'includenan'})))
            omitflag = false;
            varargin(end) = [];
          elseif (any (strcmpi (varargin{end}, {'omitundefined', 'omitnan'})))
            omitflag = true;
            varargin(end) = [];
          elseif (! strcmpi (varargin{end}, 'all'))
            error ("categorical.min: invalid missing flag.");
          endif
        endif
      endif
      ## Grab dim, vecdim, 'all'
      if (isempty (varargin))
        dim = [];
      else
        dim = varargin{1};
      endif
      ## Create output array
      C = A;
      ## Minimum of two arrays
      if (! isempty (B))
        ## No second output allowed
        if (nargout > 1)
          error (strcat ("categorical.min: a second output is", ...
                         " not supported with this syntax."));
        endif
        ## Check for ordinal categorical array
        if (! isa (B, 'categorical'))
          error ("categorical.min: array B must be categorical.");
        endif
        if (! B.isOrdinal)
          error ("categorical.min: categorical array B is not ordinal.");
        endif
        ## Check for same categories (including their order)
        Acats = categories (A);
        Bcats = categories (B);
        if (! isequal (Acats, Bcats))
          error (strcat ("categorical.min: categorical arrays must have", ...
                         " the same set of categories, including thei order."));
        endif
        ## Process codes and missing values
        A_d = double (A);
        B_d = double (B);
        if (omitflag)
          C_d = __nanmin__ (A_d, B_d);
        else
          C_d = __nanmin__ (A_d, B_d, true);
        endif
      ## Minimum of one array
      else
        A_d = double (A);
        if (omitflag)
          [C_d, index] = __nanmin__ (A_d, [], varargin{:});
        else
          [C_d, index] = __nanmin__ (A_d, [], varargin{:}, true);
        endif
      endif
      ## Fix missing codes
      C.isMissing = isnan (C_d);
      C.code = uint16 (C_d);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} mink (@var{A}, @var{K})
    ## @deftypefnx {categorical} {@var{B} =} mink (@var{A}, @var{K}, @var{dim})
    ## @deftypefnx {categorical} {[@var{B}, @var{index}] =} mink (@dots{})
    ##
    ## Smallest K categories from an ordinal categorical array.
    ##
    ## @code{@var{B} = mink (@var{A}, @var{K})} returns the @var{K} smallest
    ## categories in categorical vector @var{A}.  If @var{A} is a matrix,
    ## @code{mink (@var{A})} returns the @var{K} smallest categories from each
    ## column.  For multidimensional arrays, @code{mink (@var{A})} returns the
    ## @var{K} smallest categories along the first non-singleton dimension.
    ##
    ## @code{@var{B} = mink (@var{A}, @var{K}, @var{dim})} returns the @var{K}
    ## smallest elements in categorical array  @var{A} along the dimension
    ## specified by @var{dim}.
    ##
    ## @code{[@var{B}, @var{index}] = mink (@dots{})} also returns the indices
    ## of the @var{K} smallest elements in @var{index}, using any of the
    ## previous syntaxes.
    ##
    ## @end deftypefn
    function [B, index] = mink (A, K, dim = [])
      ## Check input arguments
      if (! A.isOrdinal)
        error ("categorical.mink: categorical array A is not ordinal.");
      endif
      if (! (isscalar (K) && fix (K) == K && K > 0))
        error ("categorical.mink: K must be a positive integer scalar.");
      endif
      if (isempty (dim))
        dim = find (size (A) > 1)(1);
      endif
      ## Calculate subscript vector
      idx = repmat ({':'}, 1, ndims (A));
      idx{dim} = 1:K;
      ## Sort array and keep K elements along DIM
      [B, index] = sort (A, dim);
      B = subset (B, idx{:});
      index = index(1:K);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} max (@var{A})
    ## @deftypefnx {categorical} {[@var{C}, @var{index}] =} max (@var{A})
    ## @deftypefnx {categorical} {@var{C} =} max (@var{A}, @qcode{[]}, @var{dim})
    ## @deftypefnx {categorical} {@var{C} =} max (@var{A}, @qcode{[]}, @var{vecdim})
    ## @deftypefnx {categorical} {@var{C} =} max (@var{A}, @qcode{[]}, @qcode{'all'})
    ## @deftypefnx {categorical} {[@var{C}, @var{index}] =} max (@var{A}, @qcode{[]}, @dots{})
    ## @deftypefnx {categorical} {@var{C} =} max (@var{A}, @var{B})
    ## @deftypefnx {categorical} {[@dots{}] =} max (@dots{}, @var{missingflag})
    ##
    ## Largest element in an ordinal categorical array.
    ##
    ## @code{@var{C} = max (@var{A})} returns the largest element in ordinal
    ## categorical vector @var{A}.  If @var{A} is a matrix, @code{max (@var{A})}
    ## returns a row vector with the largest element from each column.  For
    ## multidimensional arrays, @code{max (@var{A})} operates along the first
    ## non-singleton dimension.
    ##
    ## @code{[@var{C}, @var{index}] = max (@var{A})} also returns the indices of
    ## the maximum values in @var{index}, which has the same size as @var{C}.
    ## When the operating dimension contains more than one maximal elements, the
    ## index of the first one is returned.
    ##
    ## @code{@var{C} = max (@var{A}, @qcode{[]}, @var{dim})} operates along the
    ## dimension specified by @var{dim}.
    ##
    ## @code{@var{C} = max (@var{A}, @qcode{[]}, @var{vecdim})} operates on all
    ## the elements contained in the dimensions specified by @var{vecdim}, which
    ## must be a numeric vector of non-repeating positive integers.  Any values
    ## in @var{vecdim} indexing dimensions larger that the actual array @var{A}
    ## are ignored.
    ##
    ## @code{@var{C} = max (@var{A}, @qcode{[]}, @qcode{'all'})} operates on all
    ## dimensions and returns the largest element in @var{A}.
    ##
    ## @code{[@var{C}, @var{index}] = max (@var{A}, @qcode{[]}, @dots{})} also
    ## returns the indices of the maximum values in @var{index}, using any of
    ## the previous syntaxes.
    ##
    ## @code{@var{C} = max (@var{A}, @var{B})} returns an ordinal categorical
    ## array @var{C} with the largest elements from @var{A} and @var{B}, which
    ## both must be ordinal categorical arrays of compatible sizes with the same
    ## set and ordering of categories.  Compatible size means that @var{A} and
    ## @var{B} can be the same size, one can be scalar, or for every dimension,
    ## their dimension sizes must be equal or one of them must be 1.
    ##
    ## @code{[@dots{}] = max (@dots{}, @var{missingflag})} specifies how to
    ## handle undefined elements in any of the previous syntaxes.
    ## @var{missingflag} must be a character vector or a string scalar with one
    ## of the following values:
    ##
    ## @itemize
    ## @item @qcode{'omitundefined'}, which is the default, ignores all
    ## undefined elements and returns the maximum of the remaining elements.  If
    ## all elements along the operating dimension are undefined, then it returns
    ## an undefined element.  @qcode{'omitnan'} may also be used as equivalent
    ## to @qcode{'omitundefined'}.
    ## @item @qcode{'includeundefined'} returns an undefined element if there
    ## any undefined elements along the operating dimension.
    ## @qcode{'includenan'} may also be used as equivalent to
    ## @qcode{'includeundefined'}.
    ## @end itemize
    ##
    ## @end deftypefn
    function [C, index] = max (A, B = [], varargin)
      ## Check for ordinal categorical array
      if (! A.isOrdinal)
        error ("categorical.max: categorical array A is not ordinal.");
      endif
      if (numel (varargin) > 2)
        error ("categorical.max: too many input arguments.");
      endif
      ## Get missing flag
      omitflag = true;
      if (numel (varargin) > 0)
        if (ischar (varargin{end}) || isa (varargin{end}, 'string'))
          if (any (strcmpi (varargin{end}, {'includeundefined', 'includenan'})))
            omitflag = false;
            varargin(end) = [];
          elseif (any (strcmpi (varargin{end}, {'omitundefined', 'omitnan'})))
            omitflag = true;
            varargin(end) = [];
          elseif (! strcmpi (varargin{end}, 'all'))
            error ("categorical.max: invalid missing flag.");
          endif
        endif
      endif
      ## Grab dim, vecdim, 'all'
      if (isempty (varargin))
        dim = [];
      else
        dim = varargin{1};
      endif
      ## Create output array
      C = A;
      ## Minimum of two arrays
      if (! isempty (B))
        ## No second output allowed
        if (nargout > 1)
          error (strcat ("categorical.min: a second output is", ...
                         " not supported with this syntax."));
        endif
        ## Check for ordinal categorical array
        if (! isa (B, 'categorical'))
          error ("categorical.max: array B must be categorical.");
        endif
        if (! B.isOrdinal)
          error ("categorical.max: categorical array B is not ordinal.");
        endif
        ## Check for same categories (including their order)
        Acats = categories (A);
        Bcats = categories (B);
        if (! isequal (Acats, Bcats))
          error (strcat ("categorical.max: categorical arrays must have", ...
                         " the same set of categories, including thei order."));
        endif
        ## Process codes and missing values
        A_d = double (A);
        B_d = double (B);
        if (omitflag)
          C_d = __nanmax__ (A_d, B_d);
        else
          C_d = __nanmax__ (A_d, B_d, true);
        endif
      ## Minimum of one array
      else
        A_d = double (A);
        if (omitflag)
          [C_d, index] = __nanmax__ (A_d, [], varargin{:});
        else
          [C_d, index] = __nanmax__ (A_d, [], varargin{:}, true);
        endif
      endif
      ## Fix missing codes
      C.isMissing = isnan (C_d);
      C.code = uint16 (C_d);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{B} =} maxk (@var{A}, @var{K})
    ## @deftypefnx {categorical} {@var{B} =} maxk (@var{A}, @var{K}, @var{dim})
    ## @deftypefnx {categorical} {[@var{B}, @var{index}] =} maxk (@dots{})
    ##
    ## Largest K categories from an ordinal categorical array.
    ##
    ## @code{@var{B} = maxk (@var{A}, @var{K})} returns the @var{K} largest
    ## categories in categorical vector @var{A}.  If @var{A} is a matrix,
    ## @code{maxk (@var{A})} returns the @var{K} largest categories from each
    ## column.  For multidimensional arrays, @code{maxk (@var{A})} returns the
    ## @var{K} largest categories along the first non-singleton dimension.
    ##
    ## @code{@var{B} = maxk (@var{A}, @var{K}, @var{dim})} returns the @var{K}
    ## largest elements in categorical array  @var{A} along the dimension
    ## specified by @var{dim}.
    ##
    ## @code{[@var{B}, @var{index}] = maxk (@dots{})} also returns the indices
    ## of the @var{K} largest elements in @var{index}, using any of the
    ## previous syntaxes.
    ##
    ## @end deftypefn
    function [B, index] = maxk (A, K, dim = [])
      ## Check input arguments
      if (! A.isOrdinal)
        error ("categorical.maxk: categorical array A is not ordinal.");
      endif
      if (! (isscalar (K) && fix (K) == K && K > 0))
        error ("categorical.maxk: K must be a positive integer scalar.");
      endif
      if (isempty (dim))
        dim = find (size (A) > 1)(1);
      endif
      ## Calculate subscript vector
      idx = repmat ({':'}, 1, ndims (A));
      idx{dim} = 1:K;
      ## Sort array and keep K elements along DIM
      [B, index] = sort (A, dim, 'descend', 'MissingPlacement', 'last');
      B = subset (B, idx{:});
      index = index(1:K);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} median (@var{A})
    ## @deftypefnx {categorical} {@var{B} =} median (@var{A}, @var{dim})
    ## @deftypefnx {categorical} {@var{B} =} median (@var{A}, @var{vecdim})
    ## @deftypefnx {categorical} {@var{B} =} median (@var{A}, @qcode{'all'})
    ## @deftypefnx {categorical} {@var{B} =} median (@dots{}, @var{missingflag})
    ##
    ## Median value of an ordinal categorical array.
    ##
    ## @code{@var{B} = median (@var{A})} returns the median of the elements in
    ## ordinal categorical vector @var{A}.  If @var{A} is a matrix,
    ## @code{median (@var{A})} returns a row vector with the median element
    ## from each column.  For multidimensional arrays, @code{median (@var{A})}
    ## operates along the first non-singleton dimension.  @var{B} is also
    ## ordinal with the same ordered categories as @var{A}.  For even number of
    ## elements along the operating dimension, the returned median value is
    ## either the midway category between the two middle elements or the larger
    ## of the two categories midway between the two middle elements.
    ##
    ## @code{@var{B} = median (@var{A}, @var{dim})} operates along the dimension
    ## specified by @var{dim}.
    ##
    ## @code{@var{B} = median (@var{A}, @var{vecdim})} operates on all the
    ## elements contained in the dimensions specified by @var{vecdim}, which
    ## must be a numeric vector of non-repeating positive integers.  Any values
    ## in @var{vecdim} indexing dimensions larger that the actual array @var{A}
    ## are ignored.
    ##
    ## @code{@var{C} = median (@var{A}, @qcode{[]}, @qcode{'all'})} operates on
    ## all dimensions and returns the median element in @var{A}.
    ##
    ## @code{@var{C} = median (@dots{}, @var{missingflag})} specifies how to
    ## handle undefined elements in any of the previous syntaxes.
    ## @var{missingflag} must be a character vector or a string scalar with one
    ## of the following values:
    ##
    ## @itemize
    ## @item @qcode{'omitundefined'} ignores all undefined elements and returns
    ## the median of the remaining elements.  If all elements along the
    ## operating dimension are undefined, then it returns an undefined element.
    ## @qcode{'omitnan'} may also be used as equivalent to
    ## @qcode{'omitundefined'}.
    ## @item @qcode{'includeundefined'}, which is the default, returns an
    ## undefined element if there any undefined elements along the operating
    ## dimension. @qcode{'includenan'} may also be used as equivalent to
    ## @qcode{'includeundefined'}.
    ## @end itemize
    ##
    ## @end deftypefn
    function B = median (A, varargin)
      ## Check for ordinal categorical array
      if (! A.isOrdinal)
        error ("categorical.median: categorical array A is not ordinal.");
      endif
      if (numel (varargin) > 2)
        error ("categorical.median: too many input arguments.");
      endif
      ## Get missing flag
      omitflag = 'omitnan';
      if (numel (varargin) > 0)
        if (ischar (varargin{end}) || isa (varargin{end}, 'string'))
          if (any (strcmpi (varargin{end}, {'includeundefined', 'includenan'})))
            omitflag = 'includenan';
            varargin(end) = [];
          elseif (any (strcmpi (varargin{end}, {'omitundefined', 'omitnan'})))
            omitflag = 'omitnan';
            varargin(end) = [];
          elseif (! strcmpi (varargin{end}, 'all'))
            error ("categorical.max: invalid missing flag.");
          endif
        endif
      endif
      ## Grab dim, vecdim, 'all' before computing median value
      if (isempty (varargin))
        B_d = median (double (A), omitflag);
      else
        B_d = median (double (A), varargin{1}, omitflag);
      endif
      ## Save median to output array
      B = A;
      B.isMissing = isnan (B_d);
      B.code = uint16 (B_d);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{M} =} mode (@var{A})
    ## @deftypefnx {categorical} {[@var{M}, @var{F}] =} mode (@var{A})
    ## @deftypefnx {categorical} {[@var{M}, @var{F}, @var{C}] =} mode (@var{A})
    ## @deftypefnx {categorical} {[@dots{}] =} mode (@var{A}, @var{dim})
    ## @deftypefnx {categorical} {[@dots{}] =} mode (@var{A}, @var{vecdim})
    ## @deftypefnx {categorical} {[@dots{}] =} mode (@var{A}, @qcode{'all'})
    ##
    ## Most frequent element in a categorical array.
    ##
    ## @code{@var{M} = mode (@var{A})} returns the most frequent element in the
    ## categorical vector @var{A}.  If @var{A} is a matrix,
    ## @code{mode (@var{A})} returns a row vector with the most frequent element
    ## from each column.  For multidimensional arrays, @code{mode (@var{A})}
    ## operates along the first non-singleton dimension.  @var{B} is also a
    ## categorical array with the same categories as @var{A}.  For multiple
    ## elements with the same maximum frequency along the operating dimension,
    ## the element from the category that occurs first in @var{A} is returned.
    ##
    ## @code{[@var{M}, @var{F}] = mode (@var{A})} also returns a numeric array
    ## @var{F}, which has the same size as @var{M} and it contains the number of
    ## occurences of each corresponding element of @var{M}.
    ##
    ## @code{[@var{M}, @var{F}, @var{C}] = mode (@var{A})} also returns a cell
    ## array @var{C}, which has the same size as @var{M} and each element is a
    ## sorted categorical vector of all the values with the same maximum
    ## frequency of the corresponding element of @var{M}.
    ##
    ## @code{@var{B} = median (@var{A}, @var{dim})} operates along the dimension
    ## specified by @var{dim}.
    ##
    ## @code{@var{B} = median (@var{A}, @var{vecdim})} operates on all the
    ## elements contained in the dimensions specified by @var{vecdim}, which
    ## must be a numeric vector of non-repeating positive integers.  Any values
    ## in @var{vecdim} indexing dimensions larger that the actual array @var{A}
    ## are ignored.
    ##
    ## @code{@var{C} = median (@var{A}, @qcode{[]}, @qcode{'all'})} operates on
    ## all dimensions and returns the most frequent element in @var{A}.
    ##
    ## @end deftypefn
    function [M, F, C] = mode (A, dim = [])
      ## Check for ordinal categorical array
      if (! A.isOrdinal)
        error ("categorical.mode: categorical array A is not ordinal.");
      endif

      ## Handle 'all' option first
      if (strcmpi (dim, 'all'))
        A = subset (A, ':');
        dim = [];
      endif

      ## Simple case, only one input argument
      if (isempty (dim))
        [m, F, c] = mode (double (A));
        M = A;
        M.code = uint16 (m);
        M.isMissing = isnan (m);
        C = cell (size (c));
        for i = 1:numel (c)
          tmp = A;
          tmp.code = uint16 (c{i});
          tmp.isMissing = isnan (c{i});
          C(i) = tmp;
        endfor

      elseif (isscalar (dim)) # DIM
        if (dim < 1 || fix (dim) != dim)
          error ("categorical.mode: DIM must be a positive integer.");
        endif
        [m, F, c] = mode (double (A), dim);
        M = A;
        M.code = uint16 (m);
        M.isMissing = isnan (m);
        C = cell (size (c));
        for i = 1:numel (c)
          tmp = A;
          tmp.code = uint16 (c{i});
          tmp.isMissing = isnan (c{i});
          C(i) = tmp;
        endfor

      elseif (isvector (dim)) # VECDIM
        vecdim = sort (dim);
        if (! all (diff (vecdim)))
           error (strcat ("categorical.mode: VECDIM must contain", ...
                           " non-repeating positive integers."));
        endif

        ## Ignore dimensions in VECDIM larger than actual array
        vecdim(find (vecdim > ndims (A))) = [];

        ## Special case, no dimensions left in VECDIM, return input
        if (isempty (vecdim))
          M = A;
          sz = size (A);
          F = ones (sz);
          C = cell (sz);
          for i = 1:prod (sz)
            C{i} = subset (A, i);
          endfor

        else
          ## Calculate permutation vector
          szx = size (A);
          remdims = 1:ndims (A);      # All dimensions
          remdims(vecdim) = [];       # Delete dimensions specified by vecdim
          nremd = numel (remdims);

          ## If all dimensions are given, it is equivalent to 'all' flag
          if (nremd == 0)
            A = subset (A, ':');
            [m, F, c] = mode (double (A));
            M = A;
            M.code = uint16 (m);
            M.isMissing = isnan (m);
            C = cell (size (c));
            for i = 1:numel (c)
              tmp = A;
              tmp.code = uint16 (c{i});
              tmp.isMissing = isnan (c{i});
              C(i) = tmp;
            endfor

          else
            ## Permute to push vecdims to back
            perm = [remdims, vecdim];
            A = permute (A, perm);

            ## Reshape to squash all vecdims in final dimension
            sznew = [szx(remdims), prod(szx(vecdim))];
            A = reshape (A, sznew);

            ## Calculate mode on final dimension
            dim = nremd + 1;
            [m, F, c] = mode (double (A), dim);

            ## Inverse permute back to correct dimensions
            m = ipermute (m, perm);
            F = ipermute (F, perm);
            c = ipermute (c, perm);

            M = A;
            M.code = uint16 (m);
            M.isMissing = isnan (m);
            C = cell (size (c));
            for i = 1:numel (c)
              tmp = A;
              tmp.code = uint16 (c{i});
              tmp.isMissing = isnan (c{i});
              C(i) = tmp;
            endfor
          endif
        endif
      else
        error ("categorical.mode: invalid second input argument.");
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{N} =} histcounts (@var{A})
    ## @deftypefnx {categorical} {@var{N} =} histcounts (@var{A}, @var{cats})
    ## @deftypefnx {categorical} {@var{N} =} histcounts (@dots{}, @qcode{'Normalization'}, @var{normtype})
    ## @deftypefnx {categorical} {[@var{N}, @var{cats}] =} histcounts (@dots{})
    ##
    ## Histogram bin counts of a categorical array.
    ##
    ## @code{@var{N} = histcounts (@var{A})} returns a numeric vector @var{N}
    ## with the number of elements of each category in @var{A}.  @var{A} can be
    ## a categorical array of any dimensions, but it is converted internally to
    ## a single column vector.
    ##
    ## @code{@var{N} = histcounts (@var{A}, @var{cats})} returns the number of
    ## elements only for the categories of @var{A} specified in @var{cats},
    ## which may be a categorical array, a string array, or a cell array of
    ## character vectors, as long as it specifies unique existing categories in
    ## @var{A}.
    ##
    ## @code{@var{N} = histcounts (@dots{}, @qcode{'Normalization'},
    ## @var{normtype})} specifies how to normalize the histogram values returned
    ## in @var{N} with any of the following options specified in @var{normtype}:
    ##
    ## @itemize
    ## @item @qcode{'count'}, which is the default, returns the number of
    ## elements in each category.
    ## @item @qcode{'countdensity'} is the same as @qcode{'count'}, since the
    ## bin width in categorical arrays is always equal to 1.
    ## @item @qcode{'probability'} returns the number of elements in each
    ## category relative to the total number of elements in @var{A}.
    ## @item @qcode{'pdf'} is the same as @qcode{'probability'}, since the bin
    ## width in categorical arrays is always equal to 1.
    ## @item @qcode{'cumcount'} returns the cumulative number of elements in
    ## each category and all previous categories.
    ## @item @qcode{'cdf'} returns the cumulative number of elements in
    ## each category and all previous categories relative to the total number of
    ## elements in @var{A}.
    ## @end itemize
    ##
    ## @code{[@var{N}, @var{cats}] = histcounts (@dots{})} also returns the
    ## corresponding categories of @var{A} for each count in @var{N}.
    ## @var{cats} is a cell array of character vectors with the same size as
    ## @var{N}.
    ##
    ## @end deftypefn
    function [N, cats] = histcounts (A, varargin)
      ## Parse and validate optional Name-Value paired argument
      optNames = {'Normalization'};
      dfValues = {'count'};
      [normtype, args] = pairedArgs (optNames, dfValues, varargin(:));
      vnt = {'count', 'countdensity', 'probability', 'pdf', 'cumcount', 'cdf'};
      if (! ismember (normtype, vnt))
        error ("categorical.histcounts: invalid 'Normalization' type.");
      endif

      ## Check for selected categories
      if (! isempty (args))
        cats = args{1};
        if (isa (cats, 'categorical'))
          cats = categories (cats);
        elseif (isa (cats, 'string'))
          cats = cellstr (cats);
        elseif (! iscellstr (cats))
          error (strcat ("categorical.histcounts: invalid", ...
                         " type for CATS input argument."));
        endif
        if (! all (ismember (cats, A.cats)))
          error (strcat ("categorical.histcounts: input argument", ...
                         " CATS references non-existing categories in A."));
        endif
      else
        cats = A.cats;
      endif
      ## Force cats to row vector
      cats = cats(:)';

      ## Count elements in selected categories
      codes = A.code(:);
      ccats = find (ismember (A.cats, cats));
      ncats = numel (ccats);
      N = zeros (1, ncats)
      for i = 1:ncats
        N(i) = sum (codes == ccats(i));
      endfor

      ## Apply normalization scheme
      if (any (strcmp (normtype, {'probability', 'pdf'})))
        N = N ./ numel (codes);
      elseif (strcmp (normtype, 'cumcount'))
        N = cumsum (N);
      elseif (strcmp (normtype, 'cdf'))
        N = cumsum (N ./ numel (codes));
      endif
    endfunction

  endmethods

################################################################################
##                   ** Sort, Filter, and Set Operations **                   ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'sort'             'sortrows'         'topkrows'         'unique'          ##
## 'intersect'        'setdiff'          'setxor'           'union'           ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} sort (@var{A})
    ## @deftypefnx {categorical} {@var{B} =} sort (@var{A}, @var{dim})
    ## @deftypefnx {categorical} {@var{B} =} sort (@var{A}, @var{direction})
    ## @deftypefnx {categorical} {@var{B} =} sort (@var{A}, @var{dim}, @var{direction})
    ## @deftypefnx {categorical} {@var{B} =} sort (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ## @deftypefnx {categorical} {[@var{B}, @var{index}] =} sort (@var{A}, @dots{})
    ##
    ## Sort elements in a categorical array.
    ##
    ## @code{@var{B} = sort (@var{A})} sorts the categorical array @var{A} in
    ## ascending order.  The sorted array @var{B} has the same categories as
    ## @var{A}.  If @var{A} is a matrix, @code{sort (@var{A})} sorts each column
    ## of @var{A} in ascending order.  For multidimensional arrays,
    ## @code{mode (@var{A})} sorts along the first non-singleton dimension.
    ##
    ## @code{@var{B} = sort (@var{A}, @var{dim})} sorts along the dimension
    ## specified by @var{dim}.
    ##
    ## @code{@var{B} = sort (@var{A}, @var{direction})} also specifies the
    ## sorting direction, which can be either @qcode{'ascend'} (default) or
    ## @qcode{'descend'}.
    ##
    ## @code{@var{B} = sort (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where to place the missing elements (@qcode{<undefined>})
    ## returned in @var{B} with any of the following options specified in
    ## @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @code{[@var{B}, @var{index}] = sort (@var{A}, @dots{})} also returns a
    ## sorting index containing the original indices of the elements in the
    ## sorted array.
    ##
    ## @itemize
    ## @item If @var{A} is a vector, then @var{index} contains the original
    ## linear indices of the elements in the sorted vector @var{B} such that
    ## @code{@var{B} = @var{A}(@var{index})}.
    ## @item If @var{A} is an @math{MxN} matrix and @qcode{@var{dim} = 1}, then
    ## @var{index} contains the original row indices of the elements in the
    ## sorted vector @var{B} such that for @qcode{j = 1:N},
    ## @code{@var{B}(:,j) = @var{A}(@var{index}(:,j),j)}.
    ## @end itemize
    ##
    ## @end deftypefn
    function [B, index] = sort (A, varargin)
      ## Parse and validate optional 'MissingPlacement' paired argument
      optNames = {'MissingPlacement'};
      dfValues = {'auto'};
      [MP, args] = pairedArgs (optNames, dfValues, varargin(:));
      if (! ismember (MP, {'auto', 'first', 'last'}))
        error ("categorical.sort: invalid value for 'MissingPlacement'.");
      endif

      ## Get direction
      cid = cellfun (@ischar, args);
      if (any (cid))
        dir = args{cid};
      else
        dir = 'ascend';
      endif

      ## Since codes are positive integers and 0 is used for undefined elements,
      ## we assume A.code == 0 for NaN.  However, in ascending order NaNs go to
      ## the end, while 0s at the very top and vice versa.  So we have to
      ## mitigate this in tandem with the 'MissingPlacement' option, by setting
      ## 0s to max(code) + 1 for certain combinations of options.
      code = A.code;
      is_nan = code == 0;
      if (any (is_nan(:)))
        if ((strcmp (dir, 'ascend') && any (strcmp (MP, {'auto', 'last'}))) ||
            (strcmp (dir, 'descend') && any (strcmp (MP, {'auto', 'first'}))))
          ## Get new value for missing elements code
          nan_code = max (code(:)) + 1;
          code(is_nan) = nan_code;
          ## Sort values
          [code, index] = sort (code, args{:});
          ## Get indices of missing values and change them back to 0
          is_nan = code == nan_code;
          code(is_nan) = 0;
        else
          ## ## Sort values without swaping code for undefined elements
          [code, index] = sort (code, args{:});
          is_nan = code == 0;
        endif
      else
        ## Sort values with no undefined elements
        [code, index] = sort (code, args{:});
      endif

      ## Populate output categorical array
      B = A;
      B.code = uint16 (code);
      B.isMissing = is_nan;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} sortrows (@var{A})
    ## @deftypefnx {categorical} {@var{B} =} sortrows (@var{A}, @var{col})
    ## @deftypefnx {categorical} {@var{B} =} sortrows (@var{A}, @var{direction})
    ## @deftypefnx {categorical} {@var{B} =} sortrows (@var{A}, @var{col}, @var{direction})
    ## @deftypefnx {categorical} {@var{B} =} sortrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})
    ## @deftypefnx {categorical} {[@var{B}, @var{index}] =} sortrows (@var{A}, @dots{})
    ##
    ## Sort rows in a categorical array.
    ##
    ## @code{@var{B} = sortrows (@var{A})} sorts the rows of the 2-D categorical
    ## array @var{A} in ascending order.  The sorted array @var{B} has the same
    ## categories as @var{A}.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{col})} sorts @var{A} according to
    ## to the columns specified by the numeric vector @var{col}, which must
    ## explicitly contain non-zero integers whose absolute values index existing
    ## columns in @var{A}.  Positive elements sort the corresponding columns in
    ## ascending order, while negative elements sort the corresponding columns
    ## in descending order.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{direction})} also specifies the
    ## sorting direction, which can be either @qcode{'ascend'} (default) or
    ## @qcode{'descend'} applying to all columns in @var{A}.  Alternatively,
    ## @var{direction} can be a cell array array of character vectors specifying
    ## the sorting direction for each individual column of @var{A}, in which
    ## case the number of elements in @var{direction} must equal the number of
    ## columns in @var{A}.
    ##
    ## @code{@var{B} = sortrows (@var{A}, @var{col}, @var{direction})} sorts the
    ## categorical array @var{A} according to the columns specified in @var{col}
    ## using the corresponding sorting direction specified in @var{direction}.
    ## In this case, the sign of the values in @var{col} is ignored.  @var{col}
    ## and @var{direction} must have the same length, but not necessarily the
    ## same number of elements as the columns in @var{A}.
    ##
    ## @code{@var{B} = sortrows (@dots{}, @qcode{'MissingPlacement'}, @var{MP})}
    ## specifies where to place the missing elements (@qcode{<undefined>})
    ## returned in @var{B} with any of the following options specified in
    ## @var{MP}:
    ##
    ## @itemize
    ## @item @qcode{'auto'}, which is the default, places missing elements last
    ## for ascending sort and first for descending sort.
    ## @item @qcode{'first'} places missing elements first.
    ## @item @qcode{'last'} places missing elements last.
    ## @end itemize
    ##
    ## @code{[@var{B}, @var{index}] = sortrows (@var{A}, @dots{})} also returns
    ## an index vector containing the original row indices of @var{A} in the
    ## sorted matrix @var{B} such that @code{@var{B} = @var{A}(@var{index},:)}.
    ##
    ## @end deftypefn
    function [B, index] = sortrows (A, varargin)
      if (ndims (A) != 2)
        error ("categorical.sortrows: A must be a 2-D matrix.");
      endif

      ## Parse and validate optional 'MissingPlacement' paired argument
      optNames = {'MissingPlacement'};
      dfValues = {'auto'};
      [MP, args] = pairedArgs (optNames, dfValues, varargin(:));
      if (! ismember (MP, {'auto', 'first', 'last'}))
        error ("categorical.sort: invalid value for 'MissingPlacement'.");
      endif

      ## Parse COL / DIRECTION input
      nc = size (A, 2);
      col = [1:nc];  # default ascending direction
      dir_flag = false;
      if (numel (args) > 2)
        error ("categorical.sortrows: too many input arguments.");
      endif
      if (numel (args) > 0)
        col = args{1};
        if (isnumeric (col))
          if (! isvector (col) || fix (col) != col)
            error (strcat ("categorical.sortrows: COL must be a vector", ...
                           " of nonzero integers indexing columns in A."));
          endif
          if (max (abs (col)) > nc)
            error ("categorical.sortrows: COL indexes non-existing column.");
          endif
        elseif (isvector (col) && (ischar (col) || iscellstr (col) ||
                                   isa (col, 'string')))
          direction = cellstr (col);
          if (! all (ismember (direction, {'ascend', 'descend'})))
            error (strcat ("categorical.sortrows: DIRECTION input must", ...
                           " contain either 'ascend' or 'descend' values."));
          endif
          ## Apply scalar expansion
          if (isscalar (direction))
            direction = repmat (direction, 1, nc);
          endif
          if (numel (direction) != nc)
            error (strcat ("categorical.sortrows: DIRECTION", ...
                           " does not match the columns in A."));
          endif
          ## Assign DIRECTION to COL
          col = [1:nc];
          idx = strcmp (direction, 'descend');
          col(idx) = - col(idx);
          dir_flag = true;
        else
          error ("categorical.sortrows: invalid type for COL argument.");
        endif
      endif
      if (numel (args) > 1)
        if (dir_flag)
          error ("categorical.sortrows: invalid third input argument.");
        endif
        if ((isvector (args{2}) && ischar (args{2})) || isa (args{2}, 'string'))
          direction = cellstr (args{2});
        elseif (isvector (args{2}) && iscellstr (args{2}))
          direction = args{2};
        else
          error ("categorical.sortrows: invalid type for DIRECTION argument.");
        endif
        if (! all (ismember (direction, {'ascend', 'descend'})))
          error (strcat ("categorical.sortrows: DIRECTION input must", ...
                         " contain either 'ascend' or 'descend' values."));
        endif
        ## Assign DIRECTION to COL
        if (isscalar (direction) && strcmp (direction, 'ascend'))
          col = abs (col);
        elseif (isscalar (direction) && strcmp (direction, 'descend'))
          col = - abs (col);
        else
          if (numel (direction) != numel (col))
            error (strcat ("categorical.sortrows: DIRECTION", ...
                           " does not match the elements in COL."));
          endif
          col = abs (col);
          idx = strcmp (direction, 'descend');
          col(idx) = - col(idx);
        endif
      endif

      ## Since codes are positive integers and 0 is used for undefined elements,
      ## we assume A.code == 0 for NaN.  However, in ascending order NaNs go to
      ## the end, while 0s at the very top and vice versa.  So we have to
      ## mitigate this in tandem with the 'MissingPlacement' option, by setting
      ## 0s to max(code) + 1 for certain combinations of options.
      code = A.code;
      is_nan = code == 0;
      if (any (is_nan(:)))
        ## Get new value for missing elements code
        nan_code = max (code(:)) + 1;
        asc_cols = [];
        des_cols = [];
        if (any (strcmp (MP, {'auto', 'last'})))
          ## Change codes only in ascending columns
          asc_cols = col(col > 0);
          if (! isempty (asc_cols))
            asc_code = code(:, asc_cols);
            a_is_nan = asc_code == 0;
            asc_code(a_is_nan) = nan_code;
            code(:, asc_cols) = asc_code;
          endif
        endif
        if (any (strcmp (MP, {'auto', 'first'})))
          ## Change codes only in descending columns
          des_cols = abs (col(col < 0));
          if (! isempty (des_cols))
            des_code = code(:, des_cols);
            d_is_nan = des_code == 0;
            des_code(d_is_nan) = nan_code;
            code(:, des_cols) = des_code;
          endif
        endif
        ## Sort values
        [code, index] = sortrows (code, col);
        ## Get indices of missing values and change them back to 0
        adcols = [asc_cols, des_cols];
        if (! isempty (adcols))
          adcode = code(:, [asc_cols, des_cols]);
          is_nan = adcode == nan_code;
          adcode(is_nan) = 0;
          code(:, [asc_cols, des_cols]) = adcode;
        endif
        ## Re-index missing values after sorting
        is_nan = code == 0;
      else
        ## Sort values with no undefined elements
        [code, index] = sortrows (code, col);
      endif

      ## Populate output categorical array
      B = A;
      B.code = uint16 (code);
      B.isMissing = is_nan;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} topkrows (@var{A}, @var{K})
    ## @deftypefnx {categorical} {@var{B} =} topkrows (@var{A}, @var{K}, @var{col})
    ## @deftypefnx {categorical} {@var{B} =} topkrows (@var{A}, @var{K}, @var{direction})
    ## @deftypefnx {categorical} {@var{B} =} topkrows (@var{A}, @var{K}, @var{col}, @var{direction})
    ##
    ## Top K sorted rows of categorical array.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K})} returns the top @var{K} rows
    ## of the 2-D categorical array @var{A} sorted in descending order as a
    ## group.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K}, @var{col})} returns the top
    ## @var{K} rows of the 2-D categorical array @var{A} sorted according to the
    ## columns specified by the numeric vector @var{col}, which must explicitly
    ## contain non-zero integers whose absolute values index existing columns in
    ## @var{A}.  Positive elements sort the corresponding columns in ascending
    ## order, while negative elements sort the corresponding columns in
    ## descending order.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K}, @var{direction})} returns the
    ## top @var{K} rows of the 2-D categorical array @var{A} sorted according to
    ## @var{direction}, which can be either @qcode{'ascend'} (default) or
    ## @qcode{'descend'} applying to all columns in @var{A}.  Alternatively,
    ## @var{direction} can be a cell array array of character vectors specifying
    ## the sorting direction for each individual column of @var{A}, in which
    ## case the number of elements in @var{direction} must equal the number of
    ## columns in @var{A}.
    ##
    ## @code{@var{B} = topkrows (@var{A}, @var{K}, @var{col}, @var{direction})}
    ## returns the top @var{K} rows of the 2-D categorical array @var{A} sorted
    ## according to the columns specified in @var{col} using the corresponding
    ## sorting direction specified in @var{direction}.  In this case, the sign
    ## of the values in @var{col} is ignored.  @var{col} and @var{direction}
    ## must have the same length, but not necessarily the same number of
    ## elements as the columns in @var{A}.
    ##
    ## @end deftypefn
    function B = topkrows (A, K, varargin)
      ## Check input argument
      if (! (isscalar (K) && fix (K) == K && K > 0))
        error ("categorical.topkrows: K must be a positive integer scalar.");
      endif
      ## Sort rows
      if (numel (varargin) == 0)
        B = sortrows (A, 'descend');
      else
        B = sortrows (A, varargin{:});
      endif
      ## Return top K rows
      B = subset (B, 1:K, ':');
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} unique (@var{A})
    ## @deftypefnx {categorical} {@var{B} =} unique (@var{A}, @qcode{'rows'})
    ## @deftypefnx {categorical} {[@var{B}, @var{ixA}, @var{ixB}] =} unique (@dots{})
    ## @deftypefnx {categorical} {@dots{} =} unique (@dots{}, @var{order})
    ## @deftypefnx {categorical} {@dots{} =} unique (@dots{}, @var{occurence})
    ##
    ## Unique values in a categorical array.
    ##
    ## @code{@var{B} = unique (@var{A})} returns the unique values of the
    ## categorical array @var{A} in the categorical vector @var{B} sorted
    ## according to the order of categories in @var{A}.  @var{B} retains the
    ## same categories as @var{A}.  If @var{A} is a column vector, then @var{B}
    ## is also a column vector, otherwise @code{unique} returns a row vector.
    ##
    ## @code{@var{B} = unique (@var{A}, @qcode{'rows'})} returns the unique rows
    ## of the categorical matrix @var{A} in the categorical matrix @var{B}
    ## sorted  according to the order of categories in @var{A}.  @var{B} retains
    ## the same categories as @var{A}.
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
            error ("categorical.unique: 'rows' applies only to 2-D matrices.");
          endif
        endif
      endif
      ## Handle 'setOrder' and 'occurence' options
      opt = "sorted";
      if (! isempty (varargin))
        if (any (strcmp (varargin{1}, {'sorted', 'stable', 'first', 'last'})))
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

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} intersect (@var{A}, @var{B})
    ## @deftypefnx {categorical} {@var{C} =} intersect (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {categorical} {[@var{C}, @var{ixA}, @var{ixB}] =} intersect (@dots{})
    ## @deftypefnx {categorical} {@dots{} =} intersect (@dots{}, @var{order})
    ##
    ## Set intersection of two categorical arrays.
    ##
    ## @code{@var{C} = intersect (@var{A}, @var{B})} returns the unique common
    ## values of the categorical arrays @var{A} and @var{B}.  Either @var{A} or
    ## @var{B} input arguments may be a character vector, a string array, or a
    ## cell array of character vectors, which is promoted to a categorical array
    ## prior to set intersection.  If both @var{A} and @var{B} are row vectors,
    ## then @var{C} is also a row vector, otherwise @code{intersect} returns a
    ## column vector.
    ##
    ## If categorical arrays @var{A} and @var{B} are ordinal, they must have
    ## the same set and ordering of categories, which is transfered to @var{C}.
    ## If neither are ordinal, the category names of each pair of elements are
    ## compared (they do not need to have the same set of categories) in which
    ## case the categories in @var{C} are the sorted union of the categories in
    ## @var{A} and @var{B}.
    ##
    ## @code{@var{C} = intersect (@var{A}, @var{B}, @qcode{'rows'}} returns the
    ## unique common rows of the categorical matrices @var{A} and @var{B}, which
    ## must have the same number of columns.  By default, the rows in
    ## categorical matrix @var{C} are in sorted order.
    ##
    ## @code{[@var{C}, @var{ixA}, @var{ixB}] = intersect (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that
    ## @code{@var{C} = @var{A}(@var{ixA})} and
    ## @code{@var{C} = @var{B}(@var{ixB})}, unless the @qcode{'rows'} optional
    ## argument is given, in which case @code{@var{C} = @var{A}(@var{ixA},:)}
    ## and @code{@var{C} = @var{B}(@var{ixB},:)}.
    ##
    ## @code{@dots{} = intersect (@dots{}, @var{order})} also specifies the
    ## order of the returned unique values.  @var{order} may be either
    ## @qcode{'sorted'}, which is the default behavior, or @qcode{'stable'},
    ## in which case the unique values are returned in order of appearance.
    ##
    ## @end deftypefn
    function [C, ixA, ixB] = intersect (A, B, varargin)
      [C, ixA, ixB] = setop (A, B, 'intersect', varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} setdiff (@var{A}, @var{B})
    ## @deftypefnx {categorical} {@var{C} =} setdiff (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {categorical} {[@var{C}, @var{ixA}] =} setdiff (@dots{})
    ## @deftypefnx {categorical} {@dots{} =} setdiff (@dots{}, @var{order})
    ##
    ## Set difference of two categorical arrays.
    ##
    ## @code{@var{C} = setdiff (@var{A}, @var{B})} returns the unique common
    ## values of the categorical arrays @var{A} and @var{B}.  Either @var{A} or
    ## @var{B} input arguments may be a character vector, a string array, or a
    ## cell array of character vectors, which is promoted to a categorical array
    ## prior to set difference.  If both @var{A} and @var{B} are row vectors,
    ## then @var{C} is also a row vector, otherwise @code{intersect} returns a
    ## column vector.
    ##
    ## If categorical arrays @var{A} and @var{B} are ordinal, they must have
    ## the same set and ordering of categories, which is transfered to @var{C}.
    ## If neither are ordinal, the category names of each pair of elements are
    ## compared (they do not need to have the same set of categories) in which
    ## case the categories in @var{C} are the sorted union of the categories in
    ## @var{A} and @var{B}.
    ##
    ## @code{@var{C} = setdiff (@var{A}, @var{B}, @qcode{'rows'}} returns the
    ## unique common rows of the categorical matrices @var{A} and @var{B}, which
    ## must have the same number of columns.  By default, the rows in
    ## categorical matrix @var{C} are in sorted order.
    ##
    ## @code{[@var{C}, @var{ixA}] = setdiff (@dots{})} also returns the index
    ## vector @var{ixA} such that @code{@var{C} = @var{A}(@var{ixA})}, unless
    ## the @qcode{'rows'} optional argument is given, in which case
    ## @code{@var{C} = @var{A}(@var{ixA},:)}.
    ##
    ## @code{@dots{} = setdiff (@dots{}, @var{order})} also specifies the
    ## order of the returned unique values.  @var{order} may be either
    ## @qcode{'sorted'}, which is the default behavior, or @qcode{'stable'},
    ## in which case the unique values are returned in order of appearance.
    ##
    ## @end deftypefn
    function [C, ixA] = setdiff (A, B, varargin)
      [C, ixA] = setop (A, B, 'setdiff', varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} setxor (@var{A}, @var{B})
    ## @deftypefnx {categorical} {@var{C} =} setxor (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {categorical} {[@var{C}, @var{ixA}, @var{ixB}] =} setxor (@dots{})
    ## @deftypefnx {categorical} {@dots{} =} setxor (@dots{}, @var{order})
    ##
    ## Set exclusive-or of two categorical arrays.
    ##
    ## @code{@var{C} = setxor (@var{A}, @var{B})} returns the unique common
    ## values of the categorical arrays @var{A} and @var{B}.  Either @var{A} or
    ## @var{B} input arguments may be a character vector, a string array, or a
    ## cell array of character vectors, which is promoted to a categorical array
    ## prior to set exclusive-or.  If both @var{A} and @var{B} are row vectors,
    ## then @var{C} is also a row vector, otherwise @code{setxor} returns a
    ## column vector.
    ##
    ## If categorical arrays @var{A} and @var{B} are ordinal, they must have
    ## the same set and ordering of categories, which is transfered to @var{C}.
    ## If neither are ordinal, the category names of each pair of elements are
    ## compared (they do not need to have the same set of categories) in which
    ## case the categories in @var{C} are the sorted union of the categories in
    ## @var{A} and @var{B}.
    ##
    ## @code{@var{C} = setxor (@var{A}, @var{B}, @qcode{'rows'}} returns the
    ## unique common rows of the categorical matrices @var{A} and @var{B}, which
    ## must have the same number of columns.  By default, the rows in
    ## categorical matrix @var{C} are in sorted order.
    ##
    ## @code{[@var{C}, @var{ixA}, @var{ixB}] = setxor (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that
    ## @code{@var{C} = @var{A}(@var{ixA})} and
    ## @code{@var{C} = @var{B}(@var{ixB})}, unless the @qcode{'rows'} optional
    ## argument is given, in which case @code{@var{C} = @var{A}(@var{ixA},:)}
    ## and @code{@var{C} = @var{B}(@var{ixB},:)}.
    ##
    ## @code{@dots{} = setxor (@dots{}, @var{order})} also specifies the
    ## order of the returned unique values.  @var{order} may be either
    ## @qcode{'sorted'}, which is the default behavior, or @qcode{'stable'},
    ## in which case the unique values are returned in order of appearance.
    ##
    ## @end deftypefn
    function [C, ixA, ixB] = setxor (A, B, varargin)
      [C, ixA, ixB] = setop (A, B, 'setxor', varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{C} =} union (@var{A}, @var{B})
    ## @deftypefnx {categorical} {@var{C} =} union (@var{A}, @var{B}, @qcode{'rows'})
    ## @deftypefnx {categorical} {[@var{C}, @var{ixA}, @var{ixB}] =} union (@dots{})
    ## @deftypefnx {categorical} {@dots{} =} union (@dots{}, @var{order})
    ##
    ## Set union of two categorical arrays.
    ##
    ## @code{@var{C} = union (@var{A}, @var{B})} returns the unique common
    ## values of the categorical arrays @var{A} and @var{B}.  Either @var{A} or
    ## @var{B} input arguments may be a character vector, a string array, or a
    ## cell array of character vectors, which is promoted to a categorical array
    ## prior to set exclusive-or.  If both @var{A} and @var{B} are row vectors,
    ## then @var{C} is also a row vector, otherwise @code{union} returns a
    ## column vector.
    ##
    ## If categorical arrays @var{A} and @var{B} are ordinal, they must have
    ## the same set and ordering of categories, which is transfered to @var{C}.
    ## If neither are ordinal, the category names of each pair of elements are
    ## compared (they do not need to have the same set of categories) in which
    ## case the categories in @var{C} are the sorted union of the categories in
    ## @var{A} and @var{B}.
    ##
    ## @code{@var{C} = union (@var{A}, @var{B}, @qcode{'rows'}} returns the
    ## unique common rows of the categorical matrices @var{A} and @var{B}, which
    ## must have the same number of columns.  By default, the rows in
    ## categorical matrix @var{C} are in sorted order.
    ##
    ## @code{[@var{C}, @var{ixA}, @var{ixB}] = union (@dots{})} also returns
    ## index vectors @var{ixA} and @var{ixB} such that
    ## @code{@var{C} = @var{A}(@var{ixA})} and
    ## @code{@var{C} = @var{B}(@var{ixB})}, unless the @qcode{'rows'} optional
    ## argument is given, in which case @code{@var{C} = @var{A}(@var{ixA},:)}
    ## and @code{@var{C} = @var{B}(@var{ixB},:)}.
    ##
    ## @code{@dots{} = union (@dots{}, @var{order})} also specifies the
    ## order of the returned unique values.  @var{order} may be either
    ## @qcode{'sorted'}, which is the default behavior, or @qcode{'stable'},
    ## in which case the unique values are returned in order of appearance.
    ##
    ## @end deftypefn
    function [C, ixA, ixB] = union (A, B, varargin)
      [C, ixA, ixB] = setop (A, B, 'union', varargin{:});
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
    ## @deftypefn {categorical} {@var{C} =} cat (@var{dim}, @var{A}, @var{B}, @dots{})
    ##
    ## Concatenate categorical arrays.
    ##
    ## @code{@var{C} = cat (@var{dim}, @var{A}, @var{B}, @dots{})} concatenates
    ## categorical arrays @var{A}, @var{B}, @dots{} along dimension @var{dim}.
    ## All input arrays must have the same size except along the operating
    ## dimension @var{dim}.  Any of the input arrays may also be string arrays
    ## or cell arrays of character vectors of compatible size.
    ##
    ## If any input array is an ordinal categorical array, then all inputs must
    ## be ordinal categorical arrays with the same set and ordering of
    ## categories.  In this case, @var{C} is also an ordinal categorical array
    ## with the same set and ordering of categories.  If none of the input
    ## arrays are ordinal, then they do not need to have the same set of
    ## categories.  In this case, categorical array @var{C} contains the union
    ## of the categories from all input arrays.  Protected categorical arrays
    ## can only be concatenated with other arrays that have the same set of
    ## categories but not necessarily in the same order.
    ##
    ## @end deftypefn
    function out = cat (dim, varargin)
      ## Remove empty inputs
      varargin(cellfun ('isempty', varargin)) = [];
      if (numel (varargin) == 1)
        out = varargin{1};
        return;
      endif
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
          error (strcat ("categorical.cat: cannot concatenate ordinal", ...
                         " categorical arrays unless they have the same", ...
                         " ordered set of categories."));
        endif
        out = args{1};
        fieldArgs = cellfun (@(x) x.code, args, 'UniformOutput', false);
        out.code = cat (dim, fieldArgs{:});
        fieldArgs = cellfun (@(x) x.isMissing, args, 'UniformOutput', false);
        out.isMissing = cat (dim, fieldArgs{:});
        return;
      elseif (any (is_ordinal))
        error (strcat ("categorical.cat: cannot concatenate ordinal", ...
                       " with non-ordinal categorical arrays."));
      endif
      ## If any categorical array is protected, all must have the same categories
      is_protected = cellfun (@isprotected, args);
      if (any (is_protected))
        ## Check that all categorical arrays have the same categories
        ## but they are not necessarily in the same order
        cats = cellfun (@(x) categories (x), args, 'UniformOutput', false);
        if (! all (ismember (cats{:})))
          error (strcat ("categorical.cat: cannot concatenate protected", ...
                         " categorical arrays that do not have the same", ...
                         " set of categories."));
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

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{C} =} horzcat (@var{A}, @var{B}, @dots{})
    ##
    ## Horizontal concatenation of categorical arrays.
    ##
    ## @code{@var{C} = horzcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}, @var{B}, @dots{}]} and horizontally
    ## concatenates the categorical arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must have the same size except along the second dimension.  Any of
    ## the input arrays may also be string arrays or cell arrays of character
    ## vectors of compatible size.
    ##
    ## If any input array is an ordinal categorical array, then all inputs must
    ## be ordinal categorical arrays with the same set and ordering of
    ## categories.  In this case, @var{C} is also an ordinal categorical array
    ## with the same set and ordering of categories.  If none of the input
    ## arrays are ordinal, then they do not need to have the same set of
    ## categories.  In this case, categorical array @var{C} contains the union
    ## of the categories from all input arrays.  Protected categorical arrays
    ## can only be concatenated with other arrays that have the same set of
    ## categories but not necessarily in the same order.
    ##
    ## @end deftypefn
    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{C} =} vertcat (@var{A}, @var{B}, @dots{})
    ##
    ## Vertical concatenation of categorical arrays.
    ##
    ## @code{@var{C} = vertcat (@var{A}, @var{B}, @dots{}} is the equivalent of
    ## the syntax @code{@var{B} = [@var{A}; @var{B}; @dots{}]} and vertically
    ## concatenates the categorical arrays @var{A}, @var{B}, @dots{}.  All input
    ## arrays must have the same size except along the first dimension.  Any of
    ## the input arrays may also be string arrays or cell arrays of character
    ## vectors of compatible size.
    ##
    ## If any input array is an ordinal categorical array, then all inputs must
    ## be ordinal categorical arrays with the same set and ordering of
    ## categories.  In this case, @var{C} is also an ordinal categorical array
    ## with the same set and ordering of categories.  If none of the input
    ## arrays are ordinal, then they do not need to have the same set of
    ## categories.  In this case, categorical array @var{C} contains the union
    ## of the categories from all input arrays.  Protected categorical arrays
    ## can only be concatenated with other arrays that have the same set of
    ## categories but not necessarily in the same order.
    ##
    ## @end deftypefn
    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} repmat (@var{A}, @var{n})
    ## @deftypefnx {categorical} {@var{B} =} repmat (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {categorical} {@var{B} =} repmat (@var{A}, @var{dimvec})
    ##
    ## Repeat copies of a categorical array.
    ##
    ## @code{@var{B} = repmat (@var{A}, @var{n})} returns a categorical array
    ## @var{B} containing @var{n} copies of the input categorical array @var{A}
    ## along every dimension of @var{A}.
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
      this.code = repmat (this.code, varargin{:});
      this.isMissing = repmat (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} repelem (@var{A}, @var{n})
    ## @deftypefnx {categorical} {@var{B} =} repelem (@var{A}, @var{d1}, @dots{}, @var{dN})
    ##
    ## Repeat copies of categorical array elements.
    ##
    ## @code{@var{B} = repelem (@var{A}, @var{n})} returns a categorical vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a categorical vector.  If @var{n} is a scalar, each element of @var{A} is
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
      this.code = repelem (this.code, varargin{:});
      this.isMissing = repelem (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{B} =} repelems (@var{A}, @var{R})
    ##
    ## Construct a vector of repeated elements from a categorical array.
    ##
    ## @code{@var{B} = repelems (@var{A}, @var{R})} returns a categorical vector
    ## @var{B} containing repeated elements of the input @var{A}, which must be
    ## a categorical vector.  @var{R} must be a @math{2xN} matrix of integers.
    ## Entries in the first row of @var{R} correspond to the linear indexing of
    ## the elements in @var{A} to be repeated.  The corresponding entries in the
    ## second row of @var{R} specify the repeat count of each element.
    ##
    ## @end deftypefn
    function this = repelems (this, R)
      this.code = repelems (this.code, R);
      this.isMissing = repelems (this.isMissing, R);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} reshape (@var{A}, @var{d1}, @dots{}, @var{dN})
    ## @deftypefnx {categorical} {@var{B} =} reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})
    ## @deftypefnx {categorical} {@var{B} =} reshape (@var{A}, @var{dimvec})
    ##
    ## Repeat copies of categorical array elements.
    ##
    ## @code{@var{B} = reshape (@var{A}, @var{d1}, @dots{}, @var{dN})} returns a
    ## categorical array @var{B} with specified dimensions @var{d1}, @dots{},
    ## @var{dN}, whose elements are taken columnwise from the categorical array
    ## @var{A}.  The product of @var{d1}, @dots{}, @var{dN} must equal the total
    ## number of elements in @var{A}.
    ##
    ## @code{@var{B} = reshape (@var{A}, @dots{}, @qcode{[]}, @dots{})} returns
    ## a categorical array @var{B} with one dimension unspecified which is
    ## calculated automatically so that the product of dimensions in @var{B}
    ## matches the total elements in @var{A}, which must be divisible the
    ## product of specified dimensions.  An empty matrix @qcode{([])} is used to
    ## flag the unspecified dimension.
    ##
    ## @end deftypefn
    function this = reshape (this, varargin)
      this.code = reshape (this.code, varargin{:});
      this.isMissing = reshape (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {categorical} {@var{B} =} circshift (@var{A}, @var{n})
    ## @deftypefnx {categorical} {@var{B} =} circshift (@var{A}, @var{n}, @var{dim})
    ##
    ## Circularly shift the elements in a categorical array.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n})} circularly shifts the
    ## elements of the categorical array @var{A} according to @var{n}.  If
    ## @var{n} is a nonzero integer scalar, then the elements of @var{A} are
    ## shifted by @var{n} elements along the first non-singleton dimension of
    ## @var{A}.  If @var{n} is a vector, it must not be longer that the number
    ## of dimensions of @var{A} with each value of @var{n} corresponding to a
    ## dimension in @var{A}.   The sign of the value(s) in @var{n} specify the
    ## direction in the elements of @var{A} are shifted.
    ##
    ## @code{@var{B} = circshift (@var{A}, @var{n}, @var{dim})} circularly
    ## shifts the elements of the categorical array @var{A} along the dimension
    ## specified by @var{dim}.  In this case, @var{n} must be a scalar integer
    ## value.
    ##
    ## @end deftypefn
    function this = circshift (this, varargin)
      this.code = circshift (this.code, varargin{:});
      this.isMissing = circshift (this.isMissing, varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{B} =} permute (@var{A}, @var{dims})
    ##
    ## Generalized transpose for a categorical N-D array.
    ##
    ## @code{@var{B} = permute (@var{A}, @var{dims})} returns the generalized
    ## transpose of the categorical array @var{A} by rearranging its dimensions
    ## according to the permutation vector specified in @var{dims}.
    ##
    ## @var{dims} must index all the dimensions @code{1:ndims (@var{A})} of the
    ## input array @var{A}, in any order, but only once.  The @var{N}th
    ## dimension of @var{A} gets remapped to the dimension in @var{B} specified
    ## by @code{@var{dims}(@var{N})}.
    ##
    ## @end deftypefn
    function this = permute (this, dims)
      this.code = permute (this.code, dims);
      this.isMissing = permute (this.isMissing, dims);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{A} =} ipermute (@var{B}, @var{dims})
    ##
    ## Inverse of the generalized transpose for a categorical N-D array.
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
    function this = ipermute (this, dims)
      this.code = ipermute (this.code, dims);
      this.isMissing = ipermute (this.isMissing, dims);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{B} =} transpose (@var{A})
    ##
    ## Transpose a categorical matrix.
    ##
    ## @code{@var{B} = transpose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}.'} and returns the transpose of the categorical
    ## matrix @var{A}.
    ##
    ## @end deftypefn
    function this = transpose (this)
      if (ndims (this) != 2)
        error ("categorical.transpose: not defined for N-D arrays.");
      endif
      this.code = transpose (this.code);
      this.isMissing = transpose (this.isMissing);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {categorical} {@var{B} =} ctranspose (@var{A})
    ##
    ## Transpose a categorical matrix.
    ##
    ## @code{@var{B} = ctranspose (@var{A})} is the equivalent of the syntax
    ## @code{@var{B} = @var{A}'} and returns the transpose of the categorical
    ## matrix @var{A}.  For categorical arrays, @code{ctranspose} is identical
    ## to @code{transpose}.
    ##
    ## @end deftypefn
    function this = ctranspose (this)
      if (ndims (this) != 2)
        error ("categorical.ctranspose: not defined for N-D arrays.");
      endif
      this.code = ctranspose (this.code);
      this.isMissing = ctranspose (this.isMissing);
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

    ## Overload 'end' keyword
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
          out.code = this.code(s.subs{:});
          out.isMissing = this.isMissing(s.subs{:});

        case '{}'
          error (strcat ("categorical.subsref: '{}' invalid indexing", ...
                         " for referencing values. Use '()' instead."));

        case '.'
          error (strcat ("categorical.subsref: '.' invalid indexing", ...
                         " for referencing field of non-structure array."));
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
          if (isempty (val))
            this.code(s.subs{:}) = [];
            this.isMissing(s.subs{:}) = [];
            return;
          elseif (iscellstr (val) || ischar (val) || isnumeric (val) ||
              islogical (val) || any (isa (val, {'missing', 'string'})))
            val = promote (val);
            if (isordinal (this))
              val.isOrdinal = true;
            elseif (isprotected (this))
              val.isProtected = true;
            endif
          elseif (! isa (val, 'categorical'))
            error (strcat ("categorical.subsasgn: assignment value must", ...
                           " be a categorical array, numeric, logical or", ...
                           "  text representing categories."));
          endif
          ## After this point VAL is categorical array
          ## If any categorical array is ordinal, all must be
          if (isordinal (this) && isordinal (val))
            ## Check that all categorical arrays have the same categories
            ## and they are in the same order
            if (! isequal (categories (this), categories (val)))
              error (strcat ("categorical.subsasgn: cannot assign value", ...
                             " to ordinal categorical array unless they", ...
                             " have the same ordered set of categories."));
            endif
            this.code(s.subs{:}) = val.code;
            this.isMissing(s.subs{:}) = val.isMissing;
            return;
          elseif (isordinal (this))
            error (strcat ("categorical.subsasgn: cannot assign unordered", ...
                           " categorical array to ordinal categorical array."));
          elseif (isordinal (val))
            error (strcat ("categorical.subsasgn: cannot assign ordinal", ...
                           " categorical array to unordered categorical", ...
                           " array."));
          endif
          ## If protected, all must have the same categories
          if (isprotected (this) || isprotected (val))
            ## Check that all categorical arrays have the same categories
            ## but they are not necessarily in the same order
            if (! all (ismember (this.cats, val.cats)))
              error (strcat ("categorical.subsasgn: cannot asssign to", ...
                             " protected categorical array new categories."));
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
          error (strcat ("categorical.subsasgn: '{}' invalid indexing", ...
                         " for assigning values. Use '()' instead."));

        case '.'
          error (strcat ("categorical.subsasgn: '.' invalid indexing", ...
                         " for assigning field of non-structure array."));
      endswitch

    endfunction

  endmethods

  methods (Access = private)

    ## Return a subset of the array
    function this = subset (this, varargin)
      this = this;
      this.code = this.code(varargin{:});
      this.isMissing = this.isMissing(varargin{:});
    endfunction

    ## Common function for set operations
    function [C, ixA, ixB] = setop (A, B, fname, varargin)
      if (ischar (A) || iscellstr (A) || isa (A, 'string'))
        A = categorical (A);
      elseif (ischar (B) || iscellstr (B) || isa (B, 'string'))
        B = categorical (B);
      endif
      if (! isa (A, 'categorical') || ! isa (B, 'categorical'))
        C = sprintf ("categorical.%s: invalid type input arrays.", fname);
        return;
      endif
      if (xor (A.isOrdinal, B.isOrdinal))
        C = sprintf (strcat ("categorical.%s: if A is", ...
                             " ordinal, B must be ordinal."), fname);
        return;
      endif
      C = A;
      if (A.isOrdinal && B.isOrdinal)
        if (! isequal (A.cats, B.cats))
          C = sprintf (strcat ("categorical.%s: ordinal arrays must", ...
                               " have the same set of categories in", ...
                               " the same order."), fname);
          return;
        endif
        ## For ordinal arrays, operate directly on codes
        Acodes = double (A);
        Bcodes = double (B);
      else
        ## For unordered arrays, operate on categorical values
        allcats = unique ([A.cats(:); B.cats(:)]);
        A_cats_idx = cellfun (@(x) find (ismember (allcats, x)), A.cats);
        Acodes = nan (size (A.code));
        for i = 1:numel (A_cats_idx)
          Acodes(A.code == i) = A_cats_idx(i);
        endfor
        B_cats_idx = cellfun (@(x) find (ismember (allcats, x)), B.cats);
        Bcodes = nan (size (B.code));
        for i = 1:numel (B_cats_idx)
          Bcodes(B.code == i) = B_cats_idx(i);
        endfor
        C.cats = allcats;
      endif
      ## Apply set operation
      switch (fname)
        case 'intersect'
          [code, ixA, ixB] = intersect (Acodes, Bcodes, varargin{:});
        case 'setdiff'
          [code, ixA] = setdiff (Acodes, Bcodes, varargin{:});
        case 'setxor'
          [code, ixA, ixB] = setxor (Acodes, Bcodes, varargin{:});
        case 'union'
          [code, ixA, ixB] = union (Acodes, Bcodes, varargin{:});
      endswitch
      ## Add codes and missing arrays
      C.code = uint16 (code);
      C.isMissing = C.code == 0;
    endfunction

  endmethods

endclassdef

## Promote text arrays to categorical objects
function varargout = promote (varargin)
  for i = 1:numel (varargin)
    val = varargin{i};
    if (isa (val, "categorical"))
      varargout{i} = val;
    elseif (iscellstr (val) || isa (val, "string") || ischar (val))
      val = cellstr (val);
      varargout{i} = categorical (val);
    elseif (isnumeric (val) || islogical (val))
      varargout{i} = categorical (val);
    elseif (isa (val, 'missing'))
      varargout{i} = categorical (nan (size (missing)));
    else
      error ("categorical: invalid input to constructor.");
    endif
  endfor
endfunction

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
