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
## @deftp {datatypes} tsdata.qualmetadata
##
## The quality metadata of a @code{timeseries}.
##
## A @code{tsdata.qualmetadata} object lists the quality codes a
## @code{timeseries} may carry in its @qcode{Quality} property, and a
## description of each.  It is what @qcode{@var{ts}.QualityInfo} returns.
## Code @code{@var{Code}(@var{k})} is described by
## @code{@var{Description}@{@var{k}@}}.
##
## @end deftp
classdef qualmetadata

  properties
    ## -*- texinfo -*-
    ## @deftp {tsdata.qualmetadata} {property} Code
    ##
    ## The quality codes.
    ##
    ## A vector of distinct integers from -128 to 127, stored as
    ## @code{double}, @code{[]} by default.  MATLAB accepts any number here,
    ## repeated codes included.
    ##
    ## @end deftp
    Code = []

    ## -*- texinfo -*-
    ## @deftp {tsdata.qualmetadata} {property} Description
    ##
    ## The description of each quality code.
    ##
    ## A cell array of character vectors, @code{[]} by default.  A character
    ## vector or a string array is converted.
    ##
    ## @end deftp
    Description = []

    ## -*- texinfo -*-
    ## @deftp {tsdata.qualmetadata} {property} UserData
    ##
    ## Any data the user attaches, @code{[]} by default.
    ##
    ## @end deftp
    UserData = []
  endproperties

  methods

    ## -*- texinfo -*-
    ## @deftypefn {tsdata.qualmetadata} {@var{qi} =} tsdata.qualmetadata ()
    ##
    ## Create a quality metadata object.
    ##
    ## @code{@var{qi} = tsdata.qualmetadata ()} returns an object with
    ## @qcode{Code}, @qcode{Description} and @qcode{UserData} all @code{[]}.
    ## A @code{timeseries} creates its own; this is needed only to assign a
    ## whole new one to @qcode{@var{ts}.QualityInfo}.
    ##
    ## @seealso{timeseries}
    ## @end deftypefn
    function this = qualmetadata ()
    endfunction

    function this = set.Code (this, val)
      if (isempty (val) && (isnumeric (val) || islogical (val)))
        this.Code = [];
        return;
      endif
      if (! (isnumeric (val) && isreal (val) && isvector (val)
             && all (val == fix (val)) && all (val >= -128 & val <= 127)))
        error (strcat ("tsdata.qualmetadata: 'Code' must be a vector of", ...
                       " integers from -128 to 127."));
      endif
      if (numel (unique (val)) != numel (val))
        error ("tsdata.qualmetadata: 'Code' must not repeat a code.");
      endif
      this.Code = double (val);
    endfunction

    function this = set.Description (this, val)
      if (isempty (val) && ! iscellstr (val) && ! ischar (val))
        this.Description = [];
        return;
      endif
      if (isstring (val))
        val = cellstr (val);
      elseif (ischar (val) && (isrow (val) || isempty (val)))
        val = {val};
      endif
      if (! (iscellstr (val) && (isvector (val) || isempty (val))))
        error (strcat ("tsdata.qualmetadata: 'Description' must be a cell", ...
                       " array of character vectors."));
      endif
      this.Description = val;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.qualmetadata} {@var{S} =} get (@var{qi})
    ## @deftypefnx {tsdata.qualmetadata} {@var{value} =} get (@var{qi}, @var{name})
    ## @deftypefnx {tsdata.qualmetadata} {@var{values} =} get (@var{qi}, @var{names})
    ##
    ## Return property values.
    ##
    ## @code{@var{S} = get (@var{qi})} returns a structure of every
    ## property of @var{qi}.  @code{@var{value} = get (@var{qi},
    ## @var{name})} returns the property @var{name}, a character vector or a
    ## string scalar matched in any case, and @code{@var{values} = get
    ## (@var{qi}, @var{names})} a row cell array of the properties named in
    ## the cell array @var{names}.  For an array, one @var{name} gives a cell
    ## array of its size.
    ##
    ## @seealso{tsdata.qualmetadata.set, timeseries.get}
    ## @end deftypefn
    function out = get (this, varargin)
      out = timeseries.propertyGet (this, 'tsdata.qualmetadata.get', ...
                                    varargin{:});
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn  {tsdata.qualmetadata} {} set (@var{qi}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.qualmetadata} {@var{qi2} =} set (@var{qi}, @var{name}, @var{value}, @dots{})
    ## @deftypefnx {tsdata.qualmetadata} {@var{S} =} set (@var{qi})
    ##
    ## Set property values.
    ##
    ## @code{set (@var{qi}, @var{name}, @var{value}, @dots{})} sets each
    ## property @var{name}, matched in any case, to its @var{value}, in the
    ## order given, in the variable @var{qi} of the caller, which must
    ## therefore be a variable.  Metadata held by a @code{timeseries} is set
    ## with dot assignment instead, as
    ## @code{@var{ts}.TimeInfo.Units = 'days'}.
    ##
    ## @code{@var{qi2} = set (@var{qi}, @var{name}, @var{value}, @dots{})}
    ## returns the modified object and leaves @var{qi} as it was.
    ## @code{@var{S} = set (@var{qi})} returns what @code{get (@var{qi})}
    ## does.
    ##
    ## @seealso{tsdata.qualmetadata.get, timeseries.set}
    ## @end deftypefn
    function varargout = set (this, varargin)
      if (nargin == 1)
        varargout{1} = get (this);
        return;
      endif
      this = timeseries.propertySet (this, 'tsdata.qualmetadata.set', ...
                                     varargin{:});
      if (nargout > 0)
        varargout{1} = this;
        return;
      endif
      ## With no output the caller's own variable is set, as in MATLAB
      name = inputname (1);
      if (isempty (name))
        error (strcat ("tsdata.qualmetadata.set: with no output argument,", ...
                       " QI must be a variable; set anything else", ...
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
      fprintf ("\n  qualmetadata with properties:\n\n");
      names = {'Code', 'Description', 'UserData'};
      for i = 1:numel (names)
        fprintf ("%15s: %s\n", names{i}, ...
                 timeseries.dispValue (this.(names{i})));
      endfor
      fprintf ("\n");
    endfunction

  endmethods

endclassdef

## Test the defaults
%!test
%! qi = tsdata.qualmetadata ();
%! assert_equal (qi.Code, []);
%! assert_equal (qi.Description, []);
%! assert_equal (qi.UserData, []);
%!test
%! assert_equal (properties (tsdata.qualmetadata ()), ...
%!               {'Code'; 'Description'; 'UserData'});
## Test 'Code'
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Code = [0, 1];
%! assert_equal (qi.Code, [0, 1]);
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Code = [0; 1];
%! assert_equal (qi.Code, [0; 1]);
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Code = int8 ([-128, 127]);
%! assert_equal (qi.Code, [-128, 127]);
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Code = [0, 1];
%! qi.Code = [];
%! assert_equal (qi.Code, []);
## Test 'Description'
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Description = {'good', 'bad'};
%! assert_equal (qi.Description, {'good', 'bad'});
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Description = 'good';
%! assert_equal (qi.Description, {'good'});
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Description = string ({'good', 'bad'});
%! assert_equal (qi.Description, {'good', 'bad'});
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Description = {'good'};
%! qi.Description = [];
%! assert_equal (qi.Description, []);
## Test 'Code' and 'Description' may differ in length while being assigned
%!test
%! qi = tsdata.qualmetadata ();
%! qi.Code = [0, 1];
%! assert_equal (qi.Description, []);
%! qi.Description = {'good', 'bad'};
%! assert_equal (numel (qi.Description), 2);
## Test 'UserData'
%!test
%! qi = tsdata.qualmetadata ();
%! qi.UserData = struct ('a', 1);
%! assert_equal (qi.UserData, struct ('a', 1));
## Test 'get'
%!test
%! S = get (tsdata.qualmetadata ());
%! assert_equal (fieldnames (S), {'Code'; 'Description'; 'UserData'});
%!test
%! assert_equal (get (tsdata.qualmetadata (), 'Code'), []);
%! assert_equal (get (tsdata.qualmetadata (), 'code'), []);
%!test
%! assert_equal (get (tsdata.qualmetadata (), {'Code'}), {[]});
## Test 'set' with no output sets the caller's variable
%!test
%! obj = tsdata.qualmetadata ();
%! set (obj, 'Code', [0, 1]);
%! assert_equal (obj.Code, [0, 1]);
%!test
%! obj = tsdata.qualmetadata ();
%! set (obj, 'CODE', [0, 1]);
%! assert_equal (obj.Code, [0, 1]);
## Test 'set' with an output leaves its input as it was
%!test
%! obj = tsdata.qualmetadata ();
%! obj2 = set (obj, 'Code', [0, 1]);
%! assert_equal (obj2.Code, [0, 1]);
%! assert_equal (obj.Code, []);
%!test
%! obj = tsdata.qualmetadata ();
%! assert_equal (fieldnames (set (obj)), fieldnames (get (obj)));

%!error <tsdata.qualmetadata: 'Code' must be a vector of integers from -128 to 127.> ...
%! setfield (tsdata.qualmetadata (), 'Code', [0.5, 1])
%!error <tsdata.qualmetadata: 'Code' must be a vector of integers from -128 to 127.> ...
%! setfield (tsdata.qualmetadata (), 'Code', [200, 1])
%!error <tsdata.qualmetadata: 'Code' must be a vector of integers from -128 to 127.> ...
%! setfield (tsdata.qualmetadata (), 'Code', 'ab')
%!error <tsdata.qualmetadata: 'Code' must be a vector of integers from -128 to 127.> ...
%! setfield (tsdata.qualmetadata (), 'Code', [0, 1; 2, 3])
%!error <tsdata.qualmetadata: 'Code' must be a vector of integers from -128 to 127.> ...
%! setfield (tsdata.qualmetadata (), 'Code', [1i, 1])
%!error <tsdata.qualmetadata: 'Code' must not repeat a code.> ...
%! setfield (tsdata.qualmetadata (), 'Code', [0, 0])
%!error <tsdata.qualmetadata: 'Description' must be a cell array of character vectors.> ...
%! setfield (tsdata.qualmetadata (), 'Description', 5)
%!error <tsdata.qualmetadata: 'Description' must be a cell array of character vectors.> ...
%! setfield (tsdata.qualmetadata (), 'Description', {1, 'a'})
%!error <tsdata.qualmetadata: 'Description' must be a cell array of character vectors.> ...
%! setfield (tsdata.qualmetadata (), 'Description', {'a', 'b'; 'c', 'd'})
%!error <tsdata.qualmetadata.get: too many input arguments.> ...
%! get (tsdata.qualmetadata (), 'Code', 'Code')
%!error <tsdata.qualmetadata.get: unknown property: 'Bogus'> get (tsdata.qualmetadata (), 'Bogus')
%!error <tsdata.qualmetadata.get: NAME must be a character vector or a cell array of character vectors.> ...
%! get (tsdata.qualmetadata (), 5)
%!error <tsdata.qualmetadata.set: with no output argument, QI must be a variable; set anything else with dot assignment.> ...
%! set (tsdata.qualmetadata (), 'Code', [0, 1])
%!error <tsdata.qualmetadata.set: name-value arguments must be in pairs.> ...
%! obj = tsdata.qualmetadata (); set (obj, 'Code');
%!error <tsdata.qualmetadata.set: unknown property: 'Bogus'> ...
%! obj = tsdata.qualmetadata (); set (obj, 'Bogus', 1);
%!error <tsdata.qualmetadata.set: NAME must be a character vector.> ...
%! obj = tsdata.qualmetadata (); set (obj, 5, 1);
