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

classdef vartype
  ## -*- texinfo -*-
  ## @deftp {datatypes} vartype
  ##
  ## Subscript into a table by variable type.
  ##
  ## A utility class that facilitates subscripting table variables according to
  ## their data type.
  ##
  ## @seealso{table}
  ## @end deftp

  properties (SetAccess = private, Hidden)
    ## Type of data
    type
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    ## Custom display
    function disp (this)
      fprintf ("\n  table vartype subscript:\n\n");
      fprintf ("    Select table variables matching the type '%s'\n\n", ...
               this.type);
    endfunction

  endmethods

  methods  (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {vartype} {@var{S} =} vartype (@var{type})
    ##
    ## Create a subscript into table by variable type.
    ##
    ## @code{@var{S} = vartype (@var{type})} creates a subscript to select table
    ## variables of a specified type.  The input argument, @var{type}, must be a
    ## character vector or a string scalar that specifies any type that is
    ## accepted by the @code{isa} function, such as @code{numeric},
    ## @code{logical}, @code{integer}, @code{string}, @code{categorical}, etc.
    ## It can also be @code{cellstr} to select variables that contain cell
    ## arrays of character vectors or @code{numeric} to select variables with
    ## numeric values.
    ##
    ## @end deftypefn
    function this = vartype (type)
      if (! ((isvector (type) && ischar (type)) || isa (type, "string")))
        error (["vartype: TYPE  must be either a character", ...
                " vector or a string scalar."]);
      endif
      this.type = char (cellstr (type));
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {vartype} {@var{TF} =} varMatch (@var{obj}, @var{value})
    ##
    ## Match a @qcode{vartype} object to a variable value.
    ##
    ## @code{@var{TF} = varMatch (@var{obj}, @var{value})} compares the class
    ## type of the variable in @var{value} with the predifined class type in the
    ## @qcode{vartype} object.  If they are equal, @var{TF} is @qcode{true},
    ## otherwise @var{TF} is @qcode{false}.
    ##
    ## @end deftypefn

    function TF = varMatch (this, varVal)
      if (isequal (this.type, 'cellstr'))
        TF = iscellstr (varVal);
      elseif (isequal (this.type, 'numeric'))
        TF = isnumeric (varVal);
      else
        TF = isa (varVal, this.type);
      endif
    endfunction

  endmethods

endclassdef

## Test output
%!test
%! S = vartype ('cellstr');
%! assert (isa (S, "vartype"), true);
%!test
%! S = vartype ('cellstr');
%! assert (S.varMatch ({2343}), false);
%! assert (S.varMatch ({'as'}), true);
%!test
%! S = vartype ('string');
%! assert (S.varMatch (string ('as')), true);
%! assert (S.varMatch ({'as'}), false);
%!test
%! S = vartype ('single');
%! assert (S.varMatch (34.5), false);
%! assert (S.varMatch (single (34.5)), true);
%!test
%! S = vartype ('numeric');
%! assert (S.varMatch (int8 (34)), true);
%! assert (S.varMatch (single (34.5)), true);
%!test
%! S = vartype ('duration');
%! assert (S.varMatch (int8 (34)), false);
%! assert (S.varMatch (calweeks (3)), false);
%! assert (S.varMatch (hours (12)), true);
%!test
%! S = vartype ('calendarDuration');
%! assert (S.varMatch ('char'), false);
%! assert (S.varMatch (calweeks (3)), true);
%! assert (S.varMatch (hours (12)), false);

## Test input validation
%!error <vartype: TYPE  must be either a character vector or a string scalar.> ...
%! vartype (3)
