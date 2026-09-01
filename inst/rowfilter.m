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

classdef rowfilter
  ## -*- texinfo -*-
  ## @deftp {datatypes} rowfilter
  ##
  ## Subscript into a table or timetable by a condition on its variables.
  ##
  ## A utility class that builds a condition out of comparisons written
  ## against variable names, and selects the rows satisfying it.  The
  ## condition is written before the rows are looked at, so the same filter
  ## may be used on any table naming the same variables.
  ##
  ## @example
  ## @group
  ## rf = rowfilter (tbl);
  ## tbl(rf.Height > 180 & rf.Age < 40, :)
  ## @end group
  ## @end example
  ##
  ## @seealso{timerange, withtol, table, timetable}
  ## @end deftp

  properties (SetAccess = private, Hidden)
    ## The names a condition may be written against
    varNames
    ## The variable a comparison is waiting for, empty once one is made
    pending
    ## The condition, as a function of the table it will be applied to
    condition
    ## The condition as written, for the display
    text
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
      if (isempty (this.condition))
        fprintf ("  rowfilter with no constraint\n\n");
      else
        fprintf ("  rowfilter with constraint:\n\n    %s\n\n", this.text);
      endif
      fprintf ("  VariableNames: %s\n\n", strjoin (this.varNames, ", "));
    endfunction

    ## Class specific subscripted reference.  Every '.' name is a variable of
    ## the table the filter will be applied to, as in MATLAB, so the filter
    ## has no readable properties of its own.
    function varargout = subsref (this, s)
      chain_s = s(2:end);
      s = s(1);
      if (! strcmp (s.type, '.'))
        error ("rowfilter.subsref: only '.' indexing is supported.");
      endif
      name = s.subs;
      if (isstring (name) && isscalar (name))
        name = char (name);
      endif
      if (! (ischar (name) && isrow (name)))
        error (strcat ("rowfilter.subsref: '.' index argument must be a", ...
                       " character vector or a string scalar."));
      endif
      if (! any (strcmp (this.varNames, name)))
        error (strcat ("rowfilter: no variable named '%s' to filter on;", ...
                       " use one of: %s"), name, ...
               strjoin (this.varNames, ", "));
      endif
      out = this;
      out.pending = name;
      if (! isempty (chain_s))
        out = subsref (out, chain_s);
      endif
      varargout{1} = out;
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn  {rowfilter} {@var{rf} =} rowfilter (@var{tbl})
    ## @deftypefnx {rowfilter} {@var{rf} =} rowfilter (@var{varNames})
    ##
    ## Create a row filter.
    ##
    ## @code{@var{rf} = rowfilter (@var{tbl})} creates a filter that may be
    ## written against the variables of @var{tbl}, a table or a timetable.
    ## For a timetable the row times may be filtered on too, under the name
    ## of the row dimension.
    ##
    ## @code{@var{rf} = rowfilter (@var{varNames})} creates one against the
    ## named variables without a table to take them from, so that a filter
    ## may be written before the data it will be applied to exists.
    ##
    ## A filter carries no condition until one is written against it, and a
    ## condition is a comparison of a variable with a value, optionally
    ## combined with @code{&}, @code{|} and @code{~}.
    ##
    ## @seealso{timerange, withtol, table, timetable}
    ## @end deftypefn
    function this = rowfilter (arg)

      if (nargin != 1)
        print_usage ();
      endif
      if (istabular (arg))
        props = arg.Properties;
        names = props.VariableNames;
        if (istimetable (arg))
          names = [props.DimensionNames(1), names];
        endif
      elseif (ischar (arg) || iscellstr (arg) || isa (arg, 'string'))
        names = cellstr (arg);
        names = names(:)';
        if (any (cellfun (@isempty, names)))
          error ("rowfilter: variable names must be nonempty.");
        endif
      else
        error (strcat ("rowfilter: input must be a table, a timetable, or", ...
                       " a list of variable names."));
      endif
      this.varNames = names;
      this.pending = '';
      this.condition = [];
      this.text = '';
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {rowfilter} {@var{ix} =} rowIndices (@var{rf}, @var{tbl})
    ##
    ## Return the positions of the rows of @var{tbl} the filter selects.
    ##
    ## @end deftypefn
    function ix = rowIndices (this, tbl)
      if (isempty (this.condition))
        error (strcat ("rowfilter: the filter carries no condition;", ...
                       " compare one of its variables with a value first."));
      endif
      mask = this.condition (tbl);
      if (! (islogical (mask) && numel (mask) == height (tbl)))
        error (strcat ("rowfilter: the condition did not yield one true", ...
                       " or false value for each row."));
      endif
      ix = find (mask(:));
    endfunction

  endmethods

  methods (Static, Hidden)

    ## Build the condition for one comparison.  Only one side may be the
    ## filter: comparing two of them says nothing.  A value on the left is
    ## the same condition read the other way round, so the operator is
    ## mirrored and the condition still reads as the variable first.
    ##
    ## This lives in the class because the filter overloads '.', so reading
    ## its own fields is direct only inside a method.
    function out = compare (a, b, sym, fcn, mirror)
      if (isa (a, 'rowfilter') && isa (b, 'rowfilter'))
        error (strcat ("rowfilter: '%s' compares a variable with a", ...
                       " value, not two filters."), sym);
      endif
      swapped = ! isa (a, 'rowfilter');
      if (swapped)
        rf = b;
        val = a;
      else
        rf = a;
        val = b;
      endif
      if (isempty (rf.pending))
        error (strcat ("rowfilter: '%s' needs a variable to compare; name", ...
                       " one with '.' first."), sym);
      endif
      name = rf.pending;
      out = rf;
      out.pending = '';
      if (swapped)
        out.condition = @(t) fcn (val, t.(name));
        out.text = sprintf ("%s %s %s", name, mirror, valuestr (val));
      else
        out.condition = @(t) fcn (t.(name), val);
        out.text = sprintf ("%s %s %s", name, sym, valuestr (val));
      endif
    endfunction

    function [rf, ca, cb] = combination (a, b, op)
      if (! (isa (a, 'rowfilter') && isa (b, 'rowfilter')))
        error (strcat ("rowfilter: '%s' combines two filters."), op);
      endif
      if (isempty (a.condition) || isempty (b.condition))
        error (strcat ("rowfilter: '%s' combines two conditions; compare a", ...
                       " variable with a value on both sides first."), op);
      endif
      rf = a;
      rf.varNames = unique ([a.varNames, b.varNames], 'stable');
      ca = a.condition;
      cb = b.condition;
    endfunction

  endmethods

  methods (Hidden)

    function out = eq (a, b)
      out = rowfilter.compare (a, b, '==', @eq, '==');
    endfunction

    function out = ne (a, b)
      out = rowfilter.compare (a, b, '~=', @ne, '~=');
    endfunction

    function out = lt (a, b)
      out = rowfilter.compare (a, b, '<', @lt, '>');
    endfunction

    function out = le (a, b)
      out = rowfilter.compare (a, b, '<=', @le, '>=');
    endfunction

    function out = gt (a, b)
      out = rowfilter.compare (a, b, '>', @gt, '<');
    endfunction

    function out = ge (a, b)
      out = rowfilter.compare (a, b, '>=', @ge, '<=');
    endfunction

    function out = and (a, b)
      [out, ca, cb] = rowfilter.combination (a, b, '&');
      out.condition = @(t) ca (t) & cb (t);
      out.text = sprintf ("(%s) & (%s)", a.text, b.text);
    endfunction

    function out = or (a, b)
      [out, ca, cb] = rowfilter.combination (a, b, '|');
      out.condition = @(t) ca (t) | cb (t);
      out.text = sprintf ("(%s) | (%s)", a.text, b.text);
    endfunction

    function out = not (a)
      if (isempty (a.condition))
        error (strcat ("rowfilter: '~' needs a condition to negate;", ...
                       " compare a variable with a value first."));
      endif
      out = a;
      ca = a.condition;
      out.condition = @(t) ! ca (t);
      out.text = sprintf ("~(%s)", a.text);
    endfunction

  endmethods

endclassdef

## A compared-against value as text, for the display.
function s = valuestr (val)
  if (ischar (val))
    s = sprintf ("'%s'", val);
  elseif (isnumeric (val) && isscalar (val))
    s = num2str (val);
  elseif (islogical (val) && isscalar (val))
    s = mat2str (val);
  else
    s = sprintf ("<%s>", class (val));
  endif
endfunction

## Test a comparison selects the rows satisfying it
%!test
%! T = table ((1:6)', 'VariableNames', {'A'});
%! assert_equal (T(rowfilter (T).A > 2, :).A, [3; 4; 5; 6]);
%! assert_equal (T(rowfilter (T).A == 3, :).A, 3);
%! assert_equal (T(rowfilter (T).A ~= 3, :).A, [1; 2; 4; 5; 6]);
%! assert_equal (T(rowfilter (T).A <= 2, :).A, [1; 2]);

## Test the conditions combine
%!test
%! T = table ((1:6)', 'VariableNames', {'A'});
%! rf = rowfilter (T);
%! assert_equal (T(rf.A > 2 & rf.A < 5, :).A, [3; 4]);
%! assert_equal (T(rf.A < 2 | rf.A > 5, :).A, [1; 6]);
%! assert_equal (T(~ (rf.A > 2), :).A, [1; 2]);

## Test a value on the left of the comparison
%!test
%! T = table ((1:6)', 'VariableNames', {'A'});
%! assert_equal (T(2 < rowfilter (T).A, :).A, [3; 4; 5; 6]);

## Test a categorical variable compares against a category name
%!test
%! T = table ((1:6)', categorical ({'x';'y';'x';'y';'x';'y'}), ...
%!            'VariableNames', {'A', 'G'});
%! assert_equal (T(rowfilter (T).G == 'x', :).A, [1; 3; 5]);

## Test a timetable filters on its row times by the row dimension name
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! V = timetable (tv, (1:6)', 'VariableNames', {'A'});
%! assert_equal (V(rowfilter (V).tv > tv(3), :).A, [4; 5; 6]);

## Test a filter built from names alone
%!test
%! T = table ((1:6)', 'VariableNames', {'A'});
%! rf = rowfilter ({'A'});
%! assert_equal (T(rf.A > 4, :).A, [5; 6]);
%! assert_equal (class (rowfilter (["A" "G"])), 'rowfilter');

## Test a partly written filter is still a filter
%!test
%! T = table ((1:6)', 'VariableNames', {'A'});
%! assert_equal (class (rowfilter (T)), 'rowfilter');
%! assert_equal (class (rowfilter (T).A), 'rowfilter');
%! assert_equal (class (rowfilter (T).A > 2), 'rowfilter');

%!error <rowfilter: no variable named 'Nope' to filter on; use one of: A> ...
%! rowfilter (table ((1:3)', 'VariableNames', {'A'})).Nope;
%!error <rowfilter: input must be a table, a timetable, or a list of variable names.> ...
%! rowfilter (5);
%!error <rowfilter: variable names must be nonempty.> rowfilter ({''});
## The message of a comparison naming no variable, tested through '==' since
## a '%!error' pattern ends at the first '>' and cannot carry one.
%!error <rowfilter: '==' needs a variable to compare; name one with '.' first.> ...
%! rowfilter (table ((1:3)', 'VariableNames', {'A'})) == 2;
%!error <rowfilter: '==' compares a variable with a value, not two filters.> ...
%! rf = rowfilter (table ((1:3)', 'VariableNames', {'A'})); rf.A == rf.A;
%!error <rowfilter: '&' combines two conditions; compare a variable with a value on both sides first.> ...
%! rf = rowfilter (table ((1:3)', 'VariableNames', {'A'})); rf.A & rf.A;
%!error <rowfilter: '~' needs a condition to negate; compare a variable with a value first.> ...
%! ~ rowfilter (table ((1:3)', 'VariableNames', {'A'}));
%!error <rowfilter: the filter carries no condition; compare one of its variables with a value first.> ...
%! T = table ((1:3)', 'VariableNames', {'A'}); T(rowfilter (T), :);
%!error <rowfilter.subsref: only '.' indexing is supported.> ...
%! rf = rowfilter (table ((1:3)', 'VariableNames', {'A'})); rf(1);
