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
  ## @deftp {Class} missing
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
    ## @deftypefn {missing} missing
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

    function out = ndims (this)
      out = ndims (this.data);
    endfunction

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

    function out = iscolumn (this)
      out = iscolumn (this.data);
    endfunction

    function out = isempty (this)
      out = isempty (this.data);
    endfunction

    function out = ismatrix (this)
      out = ismatrix (this.data);
    endfunction

    function out = ismissing (this)
      out = true (size (this));
    endfunction

    function out = isrow (this)
      out = isrow (this.data);
    endfunction

    function out = isscalar (this)
      out = isscalar (this.data);
    endfunction

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

    function TF = eq (A, B)
      if (isscalar (A))
        TF = false (size (B));
      elseif (isscalar (B))
        TF = false (size (A));
      elseif (isequal (size (A), size (B)))
        TF = false (size (A));
      else
        error ("missing.eq: arrays have incompatible sizes.");
      endif
    endfunction

    function out = ge (A, B)
      if (isscalar (A))
        TF = false (size (B));
      elseif (isscalar (B))
        TF = false (size (A));
      elseif (isequal (size (A), size (B)))
        TF = false (size (A));
      else
        error ("missing.ge: arrays have incompatible sizes.");
      endif
    endfunction

    function out = gt (A, B)
      if (isscalar (A))
        TF = false (size (B));
      elseif (isscalar (B))
        TF = false (size (A));
      elseif (isequal (size (A), size (B)))
        TF = false (size (A));
      else
        error ("missing.gt: arrays have incompatible sizes.");
      endif
    endfunction

    function out = le (A, B)
      if (isscalar (A))
        TF = false (size (B));
      elseif (isscalar (B))
        TF = false (size (A));
      elseif (isequal (size (A), size (B)))
        TF = false (size (A));
      else
        error ("missing.le: arrays have incompatible sizes.");
      endif
    endfunction

    function out = lt (A, B)
      if (isscalar (A))
        TF = false (size (B));
      elseif (isscalar (B))
        TF = false (size (A));
      elseif (isequal (size (A), size (B)))
        TF = false (size (A));
      else
        error ("missing.lt: arrays have incompatible sizes.");
      endif
    endfunction

    function out = ne (A, B)
      if (isscalar (A))
        TF = true (size (B));
      elseif (isscalar (B))
        TF = true (size (A));
      elseif (isequal (size (A), size (B)))
        TF = true (size (A));
      else
        error ("missing.ne: arrays have incompatible sizes.");
      endif
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
