## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

classdef missing
  ## -*- texinfo -*-
  ## @deftp {datatypes} missing
  ##
  ## Array of missing values.
  ##
  ## A special class to represent missing data to other data types.
  ##
  ## @end deftp

  properties (SetAccess = private, Hidden)
    data = NaN
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ('%s =\n', in_name);
      endif
      __disp__ (this, 'missing', in_name);
    endfunction

    ## Custom display
    function disp (this)
      __disp__ (this, 'missing');
    endfunction

  endmethods

################################################################################
##                ** Create and convert 'categorical' type **                 ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'categorical'      'dispstrings'      'cellstr'          'double'          ##
## 'single'           'calendarDuration' 'categorical'      'datetime'        ##
## 'duration'                                                                 ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{M} =} missing ()
    ##
    ## Create missing values.
    ##
    ## @code{missing} always returns a scalar missing value.  Use @code{repmat}
    ## to expand a scalar missing value to a missing array of desired
    ## dimensions.
    ##
    ## @end deftypefn
    function this = missing ()
      if (nargin > 0 )
        error ("missing: too many input arguments.");
      endif
    endfunction

  endmethods

  methods (Hidden)

    function cstr = dispstrings (this)
      cstr = repmat ({'<missing>'}, size (this));
    endfunction

    function cstr = cellstr (this)
      cstr = dispstrings (this);
    endfunction

    function out = double (this)
      out = this.data;
    endfunction

    function out = single (this)
      out = single (this.data);
    endfunction

    function out = calendarDuration (this)
      out = calendarDuration (NaN (size (this)), NaN, NaN);
    endfunction

    function out = categorical (this)
      out = categorical (NaN (size (this)));
    endfunction

    function out = datetime (this)
      out = NaT (size (this));
    endfunction

    function out = duration (this)
      out = duration (NaN (size (this)));
    endfunction

  endmethods

################################################################################
##                         ** Summary Information **                          ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'size'             'ndims'            'numel'                              ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {missing} {@var{sz} =} size (@var{M})
    ## @deftypefnx {missing} {@var{dim_sz} =} size (@var{M}, @var{dim})
    ## @deftypefnx {missing} {@var{dim_sz} =} size (@var{M}, @var{d1}, @var{d2}, @dots{})
    ## @deftypefnx {missing} {[@var{rows}, @var{columns}, @dots{}, @var{dim_n_sz}] =} size (@dots{})
    ##
    ## Return the size of a missing array.
    ##
    ## @code{@var{sz} = size (@var{M})} returns a row vector with the size
    ## (number of elements) of each dimension for the missing array @var{M}.
    ##
    ## @code{@var{dim_sz} = size (@var{M}, @var{dim})} returns the size of
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
        sz = size (this.data, varargin{:});
      else
        sz = size (this.data);
      endif
      if (nargout == 0 || nargout == 1)
        varargout{1} = sz;
      elseif (numel (sz) != nargout)
        error (["missing.size: nargout > 1 but does not", ...
                " match number of requested dimensions."]);
      else
        for i = 1:nargout
          varargout{i} = sz(i);
        endfor
      endif
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{out} =} ndims (@var{M})
    ##
    ## Number of dimensions in a missing array.
    ##
    ## @code{@var{out} = ndims (@var{M})} returns the number of dimensions of
    ## the missing array @var{M}.
    ##
    ## @end deftypefn
    function out = ndims (this)
      out = ndims (this.data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{out} =} numel (@var{M})
    ##
    ## Total number of elements in a missing array.
    ##
    ## @code{@var{out} = numel (@var{M})} returns the number of elements in the
    ## missing array @var{M}.
    ##
    ## @end deftypefn
    function out = numel (this)
      out = numel (this.data);
    endfunction

  endmethods

################################################################################
##                          ** Query Operations **                            ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'iscolumn'         'isempty'          'ismatrix'         'ismissing'       ##
## 'isrow'            'isscalar'         'isvector'                           ##
##                                                                            ##
################################################################################

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} iscolumn (@var{M})
    ##
    ## Return true if missing array is a column vector.
    ##
    ## @code{@var{TF} = iscolumn (@var{M})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the missing array @var{M} is a column vector
    ## and @qcode{false} otherwise.  A column vector is a 2-D array for which
    ## @code{size (@var{X})} returns @code{[@var{N}, 1]} with non-negative
    ## @var{N}.
    ##
    ## @end deftypefn
    function out = iscolumn (this)
      out = iscolumn (this.data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} isempty (@var{M})
    ##
    ## Return true if missing array is empty.
    ##
    ## @code{@var{TF} = isempty (@var{M})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the missing array @var{M} is empty and
    ## @qcode{false} otherwise.
    ##
    ## @end deftypefn
    function out = isempty (this)
      out = isempty (this.data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} ismatrix (@var{M})
    ##
    ## Return true if missing array is a 2-D array.
    ##
    ## @code{@var{TF} = ismatrix (@var{M})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the missing array @var{M} is a matrix and
    ## @qcode{false} otherwise.  A matrix is an array of any type where
    ## @code{ndims (@var{X}) == 2} and for which @code{size (@var{X})} returns
    ## @code{[@var{H}, @var{W}]} with non-negative @var{H} and @var{W}.
    ##
    ## @end deftypefn
    function out = ismatrix (this)
      out = ismatrix (this.data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{out} =} ismissing (@var{M})
    ##
    ## Return true for each element in missing array.
    ##
    ## @code{@var{TF} = ismissing (@var{M})} returns a logical array @var{TF}
    ## of the same size as @var{M} containing @qcode{true} in every element.
    ##
    ## @end deftypefn
    function out = ismissing (this)
      out = true (size (this));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} isrow (@var{M})
    ##
    ## Return true if missing array is a row vector.
    ##
    ## @code{@var{TF} = isrow (@var{M})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the missing array @var{M} is a row vector
    ## and @qcode{false} otherwise.  A row vector is a 2-D array for which
    ## @code{size (@var{X})} returns @code{[1, @var{N}]} with non-negative
    ## @var{N}.
    ##
    ## @end deftypefn
    function out = isrow (this)
      out = isrow (this.data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} isscalar (@var{M})
    ##
    ## Return true if missing array is a scalar.
    ##
    ## @code{@var{TF} = isscalar (@var{M})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the missing array @var{M} is also a scalar
    ## and @qcode{false} otherwise.  A scalar is a single element object for
    ## which @code{size (@var{X})} returns @code{[1, 1]}.
    ##
    ## @end deftypefn
    function out = isscalar (this)
      out = isscalar (this.data);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} isvector (@var{M})
    ##
    ## Return true if missing array is a vector.
    ##
    ## @code{@var{TF} = isvector (@var{M})} returns a logical scalar @var{TF},
    ## which is @qcode{true} if the missing array @var{M} is a vector and
    ## @qcode{false} otherwise.  A vector is a 2-D array for which one of the
    ## dimensions is equal to 1 (either @math{1xN} or @math{Nx1}).  By
    ## definition, a scalar is also a vector.
    ##
    ## @end deftypefn
    function out = isvector (this)
      out = isvector (this.data);
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
    ## @deftypefn {missing} {@var{TF} =} eq (@var{A}, @var{B})
    ##
    ## Equality for missing arrays.
    ##
    ## @code{@var{TF} = eq (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} == @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{false}.
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.
    ##
    ## One of the input arguments may also be any type of array.  Any comparison
    ## with missing arrays always returns @qcode{false}.
    ##
    ## @end deftypefn
    function TF = eq (A, B)
      A = nan (size (A));
      B = nan (size (B));
      try
        TF = A == B;
      catch
        error ("missing.eq: arrays have incompatible sizes.");
      end_try_catch
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} ge (@var{A}, @var{B})
    ##
    ## Greater than or equal to for missing arrays.
    ##
    ## @code{@var{TF} = ge (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} >= @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{false}.
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.
    ##
    ## One of the input arguments may also be any type of array.  Any comparison
    ## with missing arrays always returns @qcode{false}.
    ##
    ## @end deftypefn
    function out = ge (A, B)
      A = nan (size (A));
      B = nan (size (B));
      try
        TF = A >= B;
      catch
        error ("missing.ge: arrays have incompatible sizes.");
      end_try_catch
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} gt (@var{A}, @var{B})
    ##
    ## Greater than for missing arrays.
    ##
    ## @code{@var{TF} = gt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} > @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{false}.
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.
    ##
    ## One of the input arguments may also be any type of array.  Any comparison
    ## with missing arrays always returns @qcode{false}.
    ##
    ## @end deftypefn
    function out = gt (A, B)
      A = nan (size (A));
      B = nan (size (B));
      try
        TF = A > B;
      catch
        error ("missing.gt: arrays have incompatible sizes.");
      end_try_catch
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} le (@var{A}, @var{B})
    ##
    ## Less than or equal to for missing arrays.
    ##
    ## @code{@var{TF} = le (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} <= @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{false}.
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.
    ##
    ## One of the input arguments may also be any type of array.  Any comparison
    ## with missing arrays always returns @qcode{false}.
    ##
    ## @end deftypefn
    function out = le (A, B)
      A = nan (size (A));
      B = nan (size (B));
      try
        TF = A <= B;
      catch
        error ("missing.le: arrays have incompatible sizes.");
      end_try_catch
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} lt (@var{A}, @var{B})
    ##
    ## Less than for missing arrays.
    ##
    ## @code{@var{TF} = lt (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} < @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{false}.
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.
    ##
    ## One of the input arguments may also be any type of array.  Any comparison
    ## with missing arrays always returns @qcode{false}.
    ##
    ## @end deftypefn
    function out = lt (A, B)
      A = nan (size (A));
      B = nan (size (B));
      try
        TF = A < B;
      catch
        error ("missing.lt: arrays have incompatible sizes.");
      end_try_catch
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {missing} {@var{TF} =} ne (@var{A}, @var{B})
    ##
    ## Not equal for missing arrays.
    ##
    ## @code{@var{TF} = ne (@var{A}, @var{B})} is the equivalent of the syntax
    ## @code{@var{TF} = @var{A} != @var{B}} and returns a logical array of the
    ## same size as the largest input with its elements set to @qcode{false}.
    ## @var{A} and @var{B} must be size compatible, which translates to they can
    ## be the same size, one can be scalar, or for every dimension, their
    ## dimension sizes must be equal or one of them must be 1.
    ##
    ## One of the input arguments may also be any type of array.  Any comparison
    ## with missing arrays always returns @qcode{false}.
    ##
    ## @end deftypefn
    function out = ne (A, B)
      A = nan (size (A));
      B = nan (size (B));
      try
        TF = A != B;
      catch
        error ("missing.ne: arrays have incompatible sizes.");
      end_try_catch
    endfunction

  endmethods

################################################################################
##                           ** Array Operations **                           ##
################################################################################
##                             Available Methods                              ##
##                                                                            ##
## 'cat'              'horzcat'          'vertcat'          'repmat'          ##
## 'reshape'          'squeeze'          'transpose'        'ctranspose'      ##
##                                                                            ##
################################################################################

  methods (Hidden)

    function out = cat (dim, varargin)
      datatype = [];
      for i = 1:numel (varargin)
        if (isa (varargin{i}, 'missing'))
          continue
        elseif (isa (varargin{i}, 'calendarDuration'))
          datatype = 'calendarDuration';
        elseif (iscellstr (varargin{i}))
          datatype = 'cellstr';
        elseif (isa (varargin{i}, 'categorical'))
          datatype = 'categorical';
        elseif (isa (varargin{i}, 'datetime'))
          datatype = 'datetime';
        elseif (isa (varargin{i}, 'double'))
          datatype = 'double';
        elseif (isa (varargin{i}, 'duration'))
          datatype = 'duration';
        elseif (isa (varargin{i}, 'single'))
          datatype = 'single';
        elseif (isa (varargin{i}, 'string'))
          datatype = 'string';
        else
          error ("missing.cat: cannot convert 'missing' to '%s' type.", ...
                 class (varargin{i}));
        endif
      endfor
      args = varargin;
      for i = 1:numel (varargin)
        if (isa (varargin{i}, 'missing'))
          switch datatype
            case 'calendarDuration'
              args{i} = calendarDuration (NaN (size (varargin{i})), NaN, NaN);
            case 'cellstr'
              args{i} = repmat({''}, size (varargin{i}));
            case 'categorical'
              args{i} = categorical (NaN (size (varargin{i})));
            case 'datetime'
              args{i} = NaT (size (varargin{i}));
            case 'double'
              args{i} = NaN (size (varargin{i}));
            case 'single'
              args{i} = single (NaN (size (varargin{i})));
            case 'string'
              args{i} = string (NaN (size (varargin{i})));
          endswitch
        endif
      endfor
      out = cat (dim, args{:});
    endfunction

    function out = horzcat (varargin)
      out = cat (2, varargin{:});
    endfunction

    function out = vertcat (varargin)
      out = cat (1, varargin{:});
    endfunction

    function this = repmat (this, varargin)
      this.data = repmat (this.data, varargin{:});
    endfunction

    function this = reshape (this, varargin)
      this.data = reshape (this.data, varargin{:});
    endfunction

    function this = squeeze (this, varargin)
      this.data = squeeze (this.data, varargin{:});
    endfunction

    function this = ctranspose (this, varargin)
      this.data = ctranspose (this.data, varargin{:});
    endfunction

    function this = transpose (this, varargin)
      this.data = transpose (this.data, varargin{:});
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
          out.data = this.data(s.subs{:});

        case '{}'
          error (["missing.subsref: '{}' invalid indexing", ...
                  " for referencing values. Use '()' instead."]);

        case '.'
          error (["missing.subsref: '.' invalid indexing", ...
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

      if (numel (s) > 1)
        error ("missing.subsasgn: chained subscripts not allowed.");
      endif
      switch (s.type)
        case '()'
          if (isa (val, "missing"))
            this.data(s.subs{:}) = val.data;
          elseif (isempty (val))
            this.data(s.subs{:}) = [];
          else
            error ("missing.subsasgn: unable to perform assignment.");
          endif

        case '{}'
          error (["missing.subsasgn: '{}' invalid indexing", ...
                  " for assigning values. Use '()' instead."]);

        case '.'
          error (["missing.subsasgn: '.' invalid indexing", ...
                  " for assigning values. Use '()' instead."]);
      endswitch

    endfunction

  endmethods

endclassdef
