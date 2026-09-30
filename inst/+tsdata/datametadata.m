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
## @deftp {datatypes} tsdata.datametadata
##
## The data metadata of a @code{timeseries}.
##
## A @code{tsdata.datametadata} object describes the samples of a
## @code{timeseries}: their units, how the series is evaluated between them,
## and any data the user attaches.  It is what
## @qcode{@var{ts}.DataInfo} returns.
##
## @end deftp
classdef datametadata

  properties
    ## -*- texinfo -*-
    ## @deftp {tsdata.datametadata} {property} Units
    ##
    ## The units of the data.
    ##
    ## A character vector, @qcode{''} by default.  It is free text: nothing
    ## checks it against the data or converts the data with it.
    ##
    ## @end deftp
    Units = ''

    ## -*- texinfo -*-
    ## @deftp {tsdata.datametadata} {property} Interpolation
    ##
    ## The interpolation method.
    ##
    ## A @code{tsdata.interpolation} object, @qcode{'linear'} by default.
    ## A method name or a function handle may be assigned, and is converted.
    ## MATLAB starts a @code{tsdata.datametadata} created on its own with
    ## @code{[]} here, and a @code{timeseries} with @qcode{'linear'}; here
    ## both start with @qcode{'linear'}.
    ##
    ## @end deftp
    Interpolation = tsdata.interpolation ('linear')

    ## -*- texinfo -*-
    ## @deftp {tsdata.datametadata} {property} UserData
    ##
    ## Any data the user attaches, @code{[]} by default.
    ##
    ## @end deftp
    UserData = []

    ## -*- texinfo -*-
    ## @deftp {tsdata.datametadata} {property} InterpretSingleRowDataAs3D
    ##
    ## Stored for compatibility, with no effect.
    ##
    ## A logical scalar, @code{false} by default.  In MATLAB it affects only
    ## @code{addsample} given a row of data, where @code{true} gives
    ## two-dimensional data whose time runs along its last dimension while the
    ## sample holds the whole row, a state no other method can read
    ## consistently.  Here the orientation of the data always follows its
    ## dimensions, as @qcode{IsTimeFirst} of @code{timeseries} describes.
    ##
    ## @end deftp
    InterpretSingleRowDataAs3D = false
  endproperties

  methods

    ## -*- texinfo -*-
    ## @deftypefn {tsdata.datametadata} {@var{di} =} tsdata.datametadata ()
    ##
    ## Create a data metadata object.
    ##
    ## @code{@var{di} = tsdata.datametadata ()} returns an object with
    ## @qcode{Units} @qcode{''}, @qcode{Interpolation} @qcode{'linear'},
    ## @qcode{UserData} @code{[]} and @qcode{InterpretSingleRowDataAs3D}
    ## @code{false}.  A @code{timeseries} creates its own; this is needed
    ## only to assign a whole new one to @qcode{@var{ts}.DataInfo}.
    ##
    ## @seealso{timeseries, tsdata.interpolation}
    ## @end deftypefn
    function this = datametadata ()
    endfunction

    function this = set.Units (this, val)
      this.Units = checkText (val, 'Units');
    endfunction

    function this = set.Interpolation (this, val)
      if (isa (val, 'tsdata.interpolation') && ! isscalar (val))
        error (strcat ("tsdata.datametadata: 'Interpolation' must be a", ...
                       " single tsdata.interpolation object."));
      endif
      if (! isa (val, 'tsdata.interpolation'))
        if (! (isa (val, 'function_handle')
               || (ischar (val) && isrow (val))
               || (isstring (val) && isscalar (val))))
          error (strcat ("tsdata.datametadata: 'Interpolation' must be a", ...
                         " tsdata.interpolation object, a method name or a", ...
                         " function handle."));
        endif
        val = tsdata.interpolation (val);
      endif
      this.Interpolation = val;
    endfunction

    function this = set.InterpretSingleRowDataAs3D (this, val)
      if (! ((islogical (val) || isnumeric (val)) && isscalar (val)
             && any (val == [0, 1])))
        error (strcat ("tsdata.datametadata: 'InterpretSingleRowDataAs3D'", ...
                       " must be a logical scalar."));
      endif
      this.InterpretSingleRowDataAs3D = logical (val);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.datametadata} {@var{S} =} get (@var{di})
    ## @deftypefnx {tsdata.datametadata} {@var{value} =} get (@var{di}, @var{name})
    ## @deftypefnx {tsdata.datametadata} {@var{values} =} get (@var{di}, @var{names})
    ##
    ## Return property values.
    ##
    ## @code{@var{S} = get (@var{di})} returns a structure of every
    ## property of @var{di}.  @code{@var{value} = get (@var{di},
    ## @var{name})} returns the property @var{name}, a character vector or a
    ## string scalar matched in any case, and @code{@var{values} = get
    ## (@var{di}, @var{names})} a row cell array of the properties named in
    ## the cell array @var{names}.  For an array, one @var{name} gives a cell
    ## array of its size, and no name, or several, a cell array with a row
    ## per object and a column per property.
    ##
    ## @seealso{tsdata.datametadata.set, timeseries.get}
    ## @end deftypefn
    function out = get (this, varargin)
      out = timeseries.propertyGet (this, 'tsdata.datametadata.get', ...
                                    varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.datametadata} {} set (@var{di}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.datametadata} {@var{di2} =} set (@var{di}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.datametadata} {@var{S} =} set (@var{di})
    ##
    ## Set property values.
    ##
    ## @code{set (@var{di}, @var{name}, @var{value}, @dots{})} sets each
    ## property @var{name}, matched in any case, to its @var{value}, in the
    ## order given, in the variable @var{di} of the caller, which must
    ## therefore be a variable.  Metadata held by a @code{timeseries} is set
    ## with dot assignment instead, as
    ## @code{@var{ts}.TimeInfo.Units = 'days'}.
    ##
    ## @code{@var{di2} = set (@var{di}, @var{name}, @var{value}, @dots{})}
    ## returns the modified object and leaves @var{di} as it was.
    ## @code{@var{S} = set (@var{di})} returns what @code{get (@var{di})}
    ## does.
    ##
    ## @seealso{tsdata.datametadata.get, timeseries.set}
    ## @end deftypefn
    function varargout = set (this, varargin)
      if (nargin == 1)
        varargout{1} = get (this);
        return;
      endif
      this = timeseries.propertySet (this, 'tsdata.datametadata.set', ...
                                     varargin{:});
      if (nargout > 0)
        varargout{1} = this;
        return;
      endif
      ## With no output the caller's own variable is set, as in MATLAB
      name = inputname (1);
      if (isempty (name))
        error (strcat ("tsdata.datametadata.set: with no output argument,", ...
                       " DI must be a variable; set anything else", ...
                       " with dot assignment."));
      endif
      assignin ('caller', name, this);
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
      fprintf ("\n  datametadata with properties:\n\n");
      names = {'Units', 'Interpolation', 'UserData', ...
               'InterpretSingleRowDataAs3D'};
      for i = 1:numel (names)
        fprintf ("%30s: %s\n", names{i}, ...
                 timeseries.dispValue (this.(names{i})));
      endfor
      fprintf ("\n");
    endfunction

  endmethods

endclassdef

## Text property values are character vectors; a string scalar is converted.
function val = checkText (val, name)
  if (isstring (val) && isscalar (val))
    val = char (val);
  endif
  if (! (ischar (val) && (isrow (val) || isempty (val))))
    error ("tsdata.datametadata: '%s' must be a character vector.", name);
  endif
  if (isempty (val))
    val = '';
  endif
endfunction

## Test the defaults
%!test
%! di = tsdata.datametadata ();
%! assert_equal (di.Units, '');
%! assert_equal (class (di.Interpolation), 'tsdata.interpolation');
%! assert_equal (di.Interpolation.Name, 'linear');
%! assert_equal (di.UserData, []);
%! assert_equal (di.InterpretSingleRowDataAs3D, false);
%!test
%! assert_equal (properties (tsdata.datametadata ()), {'Units'; ...
%!               'Interpolation'; 'UserData'; 'InterpretSingleRowDataAs3D'});
## Test assignment
%!test
%! di = tsdata.datametadata ();
%! di.Units = 'volts';
%! assert_equal (di.Units, 'volts');
%!test
%! di = tsdata.datametadata ();
%! di.Units = string ('volts');
%! assert_equal (di.Units, 'volts');
%!test
%! di = tsdata.datametadata ();
%! di.Units = 'volts';
%! di.Units = string ('');
%! assert_equal (di.Units, '');
%!test
%! di = tsdata.datametadata ();
%! di.Interpolation = 'zoh';
%! assert_equal (di.Interpolation.Name, 'zoh');
%!test
%! di = tsdata.datametadata ();
%! di.Interpolation = tsdata.interpolation ('zoh');
%! assert_equal (di.Interpolation.Name, 'zoh');
%!test
%! di = tsdata.datametadata ();
%! di.Interpolation = @(t, d, tn) tn;
%! assert_equal (di.Interpolation.Name, 'myFuncHandle');
%!test
%! di = tsdata.datametadata ();
%! di.Interpolation.Name = 'zoh';
%! assert_equal (di.Interpolation.Name, 'zoh');
%!test
%! di = tsdata.datametadata ();
%! di.UserData = {1, 'a'};
%! assert_equal (di.UserData, {1, 'a'});
%!test
%! di = tsdata.datametadata ();
%! di.InterpretSingleRowDataAs3D = 1;
%! assert_equal (di.InterpretSingleRowDataAs3D, true);
## Test value semantics
%!test
%! a = tsdata.datametadata ();
%! b = a;
%! b.Units = 'volts';
%! assert_equal (a.Units, '');
## Test 'get'
%!test
%! S = get (tsdata.datametadata ());
%! assert_equal (fieldnames (S), {'Units'; 'Interpolation'; 'UserData'; ...
%!               'InterpretSingleRowDataAs3D'});
%!test
%! assert_equal (get (tsdata.datametadata (), 'Units'), '');
%! assert_equal (get (tsdata.datametadata (), 'units'), '');
%!test
%! assert_equal (get (tsdata.datametadata (), {'Units'}), {''});
## Test 'set' with no output sets the caller's variable
%!test
%! obj = tsdata.datametadata ();
%! set (obj, 'Units', 'volts');
%! assert_equal (obj.Units, 'volts');
%!test
%! obj = tsdata.datametadata ();
%! set (obj, 'UNITS', 'volts');
%! assert_equal (obj.Units, 'volts');
## Test 'set' with an output leaves its input as it was
%!test
%! obj = tsdata.datametadata ();
%! obj2 = set (obj, 'Units', 'volts');
%! assert_equal (obj2.Units, 'volts');
%! assert_equal (obj.Units, '');
%!test
%! obj = tsdata.datametadata ();
%! assert_equal (fieldnames (set (obj)), fieldnames (get (obj)));

%!error <tsdata.datametadata: 'Units' must be a character vector.> ...
%! setfield (tsdata.datametadata (), 'Units', 5)
%!error <tsdata.datametadata: 'Units' must be a character vector.> ...
%! setfield (tsdata.datametadata (), 'Units', ['ab'; 'cd'])
%!error <tsdata.datametadata: 'Interpolation' must be a single tsdata.interpolation object.> ...
%! setfield (tsdata.datametadata (), 'Interpolation', ...
%!           [tsdata.interpolation('linear'), tsdata.interpolation('zoh')])
%!error <tsdata.datametadata: 'Interpolation' must be a tsdata.interpolation object, a method name or a function handle.> ...
%! setfield (tsdata.datametadata (), 'Interpolation', 5)
%!error <tsdata.interpolation: 'Name' must be 'linear', 'zoh' or 'myFuncHandle'.> ...
%! setfield (tsdata.datametadata (), 'Interpolation', 'cubic')
%!error <tsdata.datametadata: 'InterpretSingleRowDataAs3D' must be a logical scalar.> ...
%! setfield (tsdata.datametadata (), 'InterpretSingleRowDataAs3D', 'x')
%!error <tsdata.datametadata: 'InterpretSingleRowDataAs3D' must be a logical scalar.> ...
%! setfield (tsdata.datametadata (), 'InterpretSingleRowDataAs3D', 2)
%!error <tsdata.datametadata: 'InterpretSingleRowDataAs3D' must be a logical scalar.> ...
%! setfield (tsdata.datametadata (), 'InterpretSingleRowDataAs3D', ...
%!           [true, false])
%!error <tsdata.datametadata.get: too many input arguments.> ...
%! get (tsdata.datametadata (), 'Units', 'Units')
%!error <tsdata.datametadata.get: unknown property: 'Bogus'> get (tsdata.datametadata (), 'Bogus')
%!error <tsdata.datametadata.get: NAME must be a character vector or a cell array of character vectors.> ...
%! get (tsdata.datametadata (), 5)
%!error <tsdata.datametadata.set: with no output argument, DI must be a variable; set anything else with dot assignment.> ...
%! set (tsdata.datametadata (), 'Units', 'volts')
%!error <tsdata.datametadata.set: name-value arguments must be in pairs.> ...
%! obj = tsdata.datametadata (); set (obj, 'Units');
%!error <tsdata.datametadata.set: unknown property: 'Bogus'> ...
%! obj = tsdata.datametadata (); set (obj, 'Bogus', 1);
%!error <tsdata.datametadata.set: NAME must be a character vector.> ...
%! obj = tsdata.datametadata (); set (obj, 5, 1);
