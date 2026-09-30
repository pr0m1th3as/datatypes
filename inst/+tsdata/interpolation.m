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
## @deftp {datatypes} tsdata.interpolation
##
## The interpolation method of a @code{timeseries}.
##
## A @code{tsdata.interpolation} object names how a @code{timeseries} is
## evaluated between its samples, and holds the function that does it.  It is
## what @qcode{@var{ts}.DataInfo.Interpolation} returns, and it is the value
## @code{setinterpmethod} sets.
##
## Two methods are built in, @qcode{'linear'} and @qcode{'zoh'} (zero-order
## hold, which keeps each value until the next sample), and any function
## handle may be given instead.
##
## @end deftp
classdef interpolation

  properties (Dependent)
    ## -*- texinfo -*-
    ## @deftp {tsdata.interpolation} {property} Fhandle
    ##
    ## The interpolating function.
    ##
    ## A function handle called as
    ## @code{@var{newData} = @var{fh} (@var{newTime}, @var{oldTime},
    ## @var{oldData})}, as in MATLAB: @var{newTime} and @var{oldTime} are
    ## column vectors, and @var{oldData} and @var{newData} have time along
    ## their first dimension, a sample per row.  It is @code{[]} for an object
    ## that names no method.  Assigning a function handle sets @qcode{Name} to
    ## @qcode{'myFuncHandle'}.
    ##
    ## MATLAB stores this property in several shapes, among them a cell
    ## holding the handle and, for a built-in method, a cell holding a handle
    ## and a function name; here it is always the handle itself.
    ##
    ## @end deftp
    Fhandle

    ## -*- texinfo -*-
    ## @deftp {tsdata.interpolation} {property} Name
    ##
    ## The name of the interpolation method.
    ##
    ## One of @qcode{'linear'}, @qcode{'zoh'} or @qcode{'myFuncHandle'}, or
    ## @qcode{''} for an object that names no method.  Assigning
    ## @qcode{'linear'} or @qcode{'zoh'} also sets @qcode{Fhandle} to the
    ## built-in function; @qcode{'myFuncHandle'} is set by assigning a function
    ## handle to @qcode{Fhandle}, not by name.  MATLAB accepts any name and
    ## leaves @qcode{Fhandle} as it was, so the two can disagree there.
    ##
    ## @end deftp
    Name
  endproperties

  properties (Access = private)
    fh = []
    name = ''
  endproperties

  methods

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.interpolation} {@var{ip} =} tsdata.interpolation ()
    ## @deftypefnx {tsdata.interpolation} {@var{ip} =} tsdata.interpolation (@var{name})
    ## @deftypefnx {tsdata.interpolation} {@var{ip} =} tsdata.interpolation (@var{fh})
    ##
    ## Create an interpolation method object.
    ##
    ## @code{@var{ip} = tsdata.interpolation ()} returns an object that names
    ## no method, with @qcode{Name} @qcode{''} and @qcode{Fhandle} @code{[]}.
    ##
    ## @code{@var{ip} = tsdata.interpolation (@var{name})} returns the built-in
    ## method @var{name}, @qcode{'linear'} or @qcode{'zoh'}, given as a
    ## character vector or a string scalar.
    ##
    ## @code{@var{ip} = tsdata.interpolation (@var{fh})} returns a method
    ## evaluated by the function handle @var{fh}, called as
    ## @code{@var{newData} = @var{fh} (@var{newTime}, @var{oldTime},
    ## @var{oldData})}, and names it @qcode{'myFuncHandle'}.
    ##
    ## A cell array is refused, where MATLAB returns an empty object for it
    ## without an error.
    ##
    ## @seealso{timeseries, tsdata.datametadata}
    ## @end deftypefn
    function this = interpolation (method)
      if (nargin == 0)
        return;
      endif
      if (isa (method, 'function_handle'))
        this.Fhandle = method;
      elseif ((ischar (method) && isrow (method))
              || (isstring (method) && isscalar (method)))
        this.Name = method;
      else
        error (strcat ("tsdata.interpolation: METHOD must be 'linear',", ...
                       " 'zoh' or a function handle."));
      endif
    endfunction

    function val = get.Fhandle (this)
      val = this.fh;
    endfunction

    function this = set.Fhandle (this, val)
      if (isa (val, 'function_handle') && isscalar (val))
        this.fh = val;
        this.name = 'myFuncHandle';
      elseif (isnumeric (val) && isempty (val))
        this.fh = [];
        this.name = '';
      else
        error (strcat ("tsdata.interpolation: 'Fhandle' must be a", ...
                       " function handle or empty."));
      endif
    endfunction

    function val = get.Name (this)
      val = this.name;
    endfunction

    function this = set.Name (this, val)
      if (isstring (val) && isscalar (val))
        val = char (val);
      endif
      if (! (ischar (val) && (isrow (val) || isempty (val))))
        error ("tsdata.interpolation: 'Name' must be a character vector.");
      endif
      switch (val)
        case 'linear'
          this.fh = @(nt, ot, od) tsdata.interpolation.builtin (nt, ot, od, ...
                                                                'linear');
        case 'zoh'
          this.fh = @(nt, ot, od) tsdata.interpolation.builtin (nt, ot, od, ...
                                                                'zoh');
        case 'myFuncHandle'
          if (! strcmp (this.name, 'myFuncHandle'))
            error (strcat ("tsdata.interpolation: 'Name' is set to", ...
                           " 'myFuncHandle' by assigning a function", ...
                           " handle to 'Fhandle'."));
          endif
        case ''
          this.fh = [];
        otherwise
          error (strcat ("tsdata.interpolation: 'Name' must be", ...
                         " 'linear', 'zoh' or 'myFuncHandle'."));
      endswitch
      this.name = val;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.interpolation} {@var{S} =} get (@var{ip})
    ## @deftypefnx {tsdata.interpolation} {@var{value} =} get (@var{ip}, @var{name})
    ## @deftypefnx {tsdata.interpolation} {@var{values} =} get (@var{ip}, @var{names})
    ##
    ## Return property values.
    ##
    ## @code{@var{S} = get (@var{ip})} returns a structure of every
    ## property of @var{ip}.  @code{@var{value} = get (@var{ip},
    ## @var{name})} returns the property @var{name}, a character vector or a
    ## string scalar matched in any case, and @code{@var{values} = get
    ## (@var{ip}, @var{names})} a row cell array of the properties named in
    ## the cell array @var{names}.  For an array, one @var{name} gives a cell
    ## array of its size, and no name, or several, a cell array with a row
    ## per object and a column per property.
    ##
    ## @seealso{tsdata.interpolation.set, timeseries.get}
    ## @end deftypefn
    function out = get (this, varargin)
      out = timeseries.propertyGet (this, 'tsdata.interpolation.get', ...
                                    varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.interpolation} {} set (@var{ip}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.interpolation} {@var{ip2} =} set (@var{ip}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.interpolation} {@var{S} =} set (@var{ip})
    ##
    ## Set property values.
    ##
    ## @code{set (@var{ip}, @var{name}, @var{value}, @dots{})} sets each
    ## property @var{name}, matched in any case, to its @var{value}, in the
    ## order given, in the variable @var{ip} of the caller, which must
    ## therefore be a variable.  Metadata held by a @code{timeseries} is set
    ## with dot assignment instead, as
    ## @code{@var{ts}.TimeInfo.Units = 'days'}.
    ##
    ## @code{@var{ip2} = set (@var{ip}, @var{name}, @var{value}, @dots{})}
    ## returns the modified object and leaves @var{ip} as it was.
    ## @code{@var{S} = set (@var{ip})} returns what @code{get (@var{ip})}
    ## does.
    ##
    ## @seealso{tsdata.interpolation.get, timeseries.set}
    ## @end deftypefn
    function varargout = set (this, varargin)
      if (nargin == 1)
        varargout{1} = get (this);
        return;
      endif
      this = timeseries.propertySet (this, 'tsdata.interpolation.set', ...
                                     varargin{:});
      if (nargout > 0)
        varargout{1} = this;
        return;
      endif
      ## With no output the caller's own variable is set, as in MATLAB
      name = inputname (1);
      if (isempty (name))
        error (strcat ("tsdata.interpolation.set: with no output argument,", ...
                       " IP must be a variable; set anything else", ...
                       " with dot assignment."));
      endif
      assignin ('caller', name, this);
    endfunction

  endmethods

  methods (Static, Hidden)

    ## The built-in methods, called as a user function is: OT the old times
    ## and NT the new, both columns, OD the old data with a sample per row of
    ## its first dimension.  Each data column is interpolated from its own
    ## samples that are not NaN, as MATLAB does; repeated old times are
    ## right-continuous.  'linear' interpolates; 'zoh' holds the last sample
    ## at or before each new time.  Past either end the result is NaN.
    function nd = builtin (nt, ot, od, method)
      sz = size (od);
      od = reshape (double (od), sz(1), []);
      nt = nt(:);
      ot = ot(:);
      nd = NaN (numel (nt), columns (od));
      for j = 1:columns (od)
        ok = ! isnan (od(:,j));
        t = ot(ok);
        v = od(ok,j);
        if (isempty (t))
          continue;
        endif
        if (strcmp (method, 'zoh'))
          for i = 1:numel (nt)
            k = find (t <= nt(i), 1, 'last');
            if (! isempty (k) && nt(i) <= t(end))
              nd(i,j) = v(k);
            endif
          endfor
        elseif (all (t == t(1)))
          ## Repeated times are right-continuous: the last sample there
          nd(nt == t(1),j) = v(end);
        else
          nd(:,j) = interp1 (t, v, nt, 'linear', NaN);
        endif
      endfor
      nd = reshape (nd, [numel(nt), sz(2:end)]);
    endfunction

  endmethods

  methods (Hidden)

    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    function disp (this)
      if (isempty (this.fh))
        fhText = '[]';
      else
        fhText = ['@', func2str(this.fh)];
        ## 'func2str' already prefixes an anonymous function with '@'
        if (strncmp (fhText, '@@', 2))
          fhText = fhText(2:end);
        endif
      endif
      fprintf ("\n  interpolation with properties:\n\n");
      fprintf ("    Fhandle: %s\n", fhText);
      fprintf ("       Name: '%s'\n\n", this.name);
    endfunction

  endmethods

endclassdef

## Test construction and the built-in methods
%!test
%! ip = tsdata.interpolation ();
%! assert_equal (ip.Name, '');
%! assert_equal (ip.Fhandle, []);
%!test
%! ip = tsdata.interpolation ('linear');
%! assert_equal (ip.Name, 'linear');
%! assert_equal (ip.Fhandle (1, [0; 2], [0; 10]), 5);
%!test
%! ip = tsdata.interpolation ('zoh');
%! assert_equal (ip.Name, 'zoh');
%! assert_equal (ip.Fhandle (1, [0; 2], [0; 10]), 0);
%!test
%! ip = tsdata.interpolation (string ('zoh'));
%! assert_equal (ip.Name, 'zoh');
%!test
%! ip = tsdata.interpolation ('linear');
%! assert_equal (ip.Fhandle (1, [0; 2], [0, 4; 10, 8]), [5, 6]);
%!test
%! ip = tsdata.interpolation ('linear');
%! assert_equal (isna (ip.Fhandle (3, [0; 2], [0; 10])), false);
%! assert_equal (ip.Fhandle (3, [0; 2], [0; 10]), NaN);
## Test a function handle names the method 'myFuncHandle'
%!test
%! ip = tsdata.interpolation (@(nt, ot, od) 2 * nt);
%! assert_equal (ip.Name, 'myFuncHandle');
%! assert_equal (ip.Fhandle (3, [0; 1], [0; 1]), 6);
%!test
%! ip = tsdata.interpolation (@sin);
%! assert_equal (ip.Fhandle, @sin);
## Test assigning 'Name' re-derives 'Fhandle'
%!test
%! ip = tsdata.interpolation ('linear');
%! ip.Name = 'zoh';
%! assert_equal (ip.Fhandle (1, [0; 2], [0; 10]), 0);
%!test
%! ip = tsdata.interpolation (@(t, d, tn) tn);
%! ip.Name = 'linear';
%! assert_equal (ip.Fhandle (1, [0; 2], [0; 10]), 5);
%!test
%! ip = tsdata.interpolation ('linear');
%! ip.Name = '';
%! assert_equal (ip.Fhandle, []);
%!test
%! ip = tsdata.interpolation (@(t, d, tn) tn);
%! ip.Name = 'myFuncHandle';
%! assert_equal (ip.Name, 'myFuncHandle');
%!test
%! ip = tsdata.interpolation ();
%! ip.Name = string ('linear');
%! assert_equal (ip.Name, 'linear');
## Test assigning 'Fhandle' sets 'Name'
%!test
%! ip = tsdata.interpolation ('zoh');
%! ip.Fhandle = @(t, d, tn) tn;
%! assert_equal (ip.Name, 'myFuncHandle');
%!test
%! ip = tsdata.interpolation ('zoh');
%! ip.Fhandle = [];
%! assert_equal (ip.Name, '');
## Test value semantics
%!test
%! a = tsdata.interpolation ('linear');
%! b = a;
%! b.Name = 'zoh';
%! assert_equal (a.Name, 'linear');
## Test 'get'
%!test
%! S = get (tsdata.interpolation ('linear'));
%! assert_equal (fieldnames (S), {'Fhandle'; 'Name'});
%!test
%! assert_equal (get (tsdata.interpolation ('linear'), 'Name'), 'linear');
%! assert_equal (get (tsdata.interpolation ('linear'), 'name'), 'linear');
%!test
%! assert_equal (get (tsdata.interpolation ('linear'), {'Name'}), {'linear'});
## Test 'set' with no output sets the caller's variable
%!test
%! obj = tsdata.interpolation ('linear');
%! set (obj, 'Name', 'zoh');
%! assert_equal (obj.Name, 'zoh');
%!test
%! obj = tsdata.interpolation ('linear');
%! set (obj, 'NAME', 'zoh');
%! assert_equal (obj.Name, 'zoh');
## Test 'set' with an output leaves its input as it was
%!test
%! obj = tsdata.interpolation ('linear');
%! obj2 = set (obj, 'Name', 'zoh');
%! assert_equal (obj2.Name, 'zoh');
%! assert_equal (obj.Name, 'linear');
%!test
%! obj = tsdata.interpolation ('linear');
%! assert_equal (fieldnames (set (obj)), fieldnames (get (obj)));
## Test the built-in methods take MATLAB's arguments and skip NaN samples
%!test
%! ip = tsdata.interpolation ('linear');
%! assert_equal (ip.Fhandle ([0.5; 1.5; 2.5], [0; 1; 2; 3], [1; NaN; 3; 4]), ...
%!               [1.5; 2.5; 3.5]);
%!test
%! ip = tsdata.interpolation ('zoh');
%! assert_equal (ip.Fhandle ([0.5; 1.5; 2.5], [0; 1; 2; 3], [1; NaN; 3; 4]), ...
%!               [1; 1; 3]);
## Test repeated old times are right-continuous
%!test
%! ip = tsdata.interpolation ('linear');
%! assert_equal (ip.Fhandle ([0.9; 1; 1.1], [0; 1; 1; 2], [1; 2; 3; 4]), ...
%!               [1.9; 3; 3.1], 1e-14);
%!test
%! ip = tsdata.interpolation ('zoh');
%! assert_equal (ip.Fhandle ([0.9; 1; 1.1], [0; 1; 1; 2], [1; 2; 3; 4]), ...
%!               [1; 3; 3]);
## Test past either end gives NaN, for 'zoh' too
%!test
%! ip = tsdata.interpolation ('zoh');
%! nd = ip.Fhandle ([-1; 0.5; 4; 5], (0:4)', [10; 20; 40; 80; 160]);
%! assert_equal (nd, [NaN; 10; 160; NaN]);
%!test
%! ip = tsdata.interpolation ('linear');
%! od = permute (reshape (1:12, 2, 2, 3), [3, 1, 2]);
%! nd = ip.Fhandle ([0.5; 1.5], [0; 1; 2], od);
%! assert_equal (size (nd), [2, 2, 2]);
%! assert_equal (squeeze (nd(1,:,:)), [3, 5; 4, 6]);

%!error <tsdata.interpolation: METHOD must be 'linear', 'zoh' or a function handle.> ...
%! tsdata.interpolation ({@sin})
%!error <tsdata.interpolation: METHOD must be 'linear', 'zoh' or a function handle.> ...
%! tsdata.interpolation (5)
%!error <tsdata.interpolation: 'Name' must be 'linear', 'zoh' or 'myFuncHandle'.> ...
%! tsdata.interpolation ('cubic')
%!error <tsdata.interpolation: 'Name' must be 'linear', 'zoh' or 'myFuncHandle'.> ...
%! tsdata.interpolation ('Linear')
%!error <tsdata.interpolation: 'Name' must be a character vector.> ...
%! setfield (tsdata.interpolation (), 'Name', 5)
%!error <tsdata.interpolation: 'Name' is set to 'myFuncHandle' by assigning a function handle to 'Fhandle'.> ...
%! setfield (tsdata.interpolation ('linear'), 'Name', 'myFuncHandle')
%!error <tsdata.interpolation: 'Fhandle' must be a function handle or empty.> ...
%! setfield (tsdata.interpolation (), 'Fhandle', 'zoh')
%!error <tsdata.interpolation: 'Fhandle' must be a function handle or empty.> ...
%! setfield (tsdata.interpolation (), 'Fhandle', {@sin})
%!error <tsdata.interpolation.get: too many input arguments.> ...
%! get (tsdata.interpolation ('linear'), 'Name', 'Name')
%!error <tsdata.interpolation.get: unknown property: 'Bogus'> get (tsdata.interpolation ('linear'), 'Bogus')
%!error <tsdata.interpolation.get: NAME must be a character vector or a cell array of character vectors.> ...
%! get (tsdata.interpolation ('linear'), 5)
%!error <tsdata.interpolation.set: with no output argument, IP must be a variable; set anything else with dot assignment.> ...
%! set (tsdata.interpolation ('linear'), 'Name', 'zoh')
%!error <tsdata.interpolation.set: name-value arguments must be in pairs.> ...
%! obj = tsdata.interpolation ('linear'); set (obj, 'Name');
%!error <tsdata.interpolation.set: unknown property: 'Bogus'> ...
%! obj = tsdata.interpolation ('linear'); set (obj, 'Bogus', 1);
%!error <tsdata.interpolation.set: NAME must be a character vector.> ...
%! obj = tsdata.interpolation ('linear'); set (obj, 5, 1);
