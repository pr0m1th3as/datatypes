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

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{tbl} =} array2table (@var{A})
## @deftypefnx {datatypes} {@var{tbl} =} array2table (@var{A}, @var{Name}, @var{Value})
##
## Convert an array to a table.
##
## @code{@var{tbl} = array2table (@var{A})} converts the 2-D matrix @var{A} to
## the table @var{tbl}, where each column of @var{A} becomes a variable in
## @var{tbl}.
##
## @var{A} can be any type of array supported by @code{table}, including a cell
## array, as long they are constraint to 2 dimensions.  However, in the case of
## a cell array @code{array2table} does not extract the contents of its cells,
## resulting to a table with each variable being a column of cells.  Use
## @code{cell2table} if you wat to create a table from the contents of the cells
## in @var{A}.
##
## @code{@var{tbl} = array2table (@var{A}, @var{Name}, @var{Value})} specifies
## optional parameters for creating the table @var{tbl} with the following
## Name-Value paired arguments.
##
## @multitable @columnfractions 0.23 0.02 0.75
## @headitem @var{Name} @tab @tab @var{Value}
##
## @item @qcode{'VariableNames'} @tab @tab A cell array of character vectors or
## a string array defining the variable names of @var{tbl}.  The names must be
## valid variable names and unique.
##
## @item @qcode{'RowNames'} @tab @tab A cell array of character vectors or
## a string array defining the row names of @var{tbl}.  The names must be unique
## but not necessarily valid variable names.
##
## @item @qcode{'DimensionNames'} @tab @tab A cell array of character vectors or
## a string array defining the dimension names of @var{tbl}.  The names must be
## unique and not in conflict with variable names.  By default, dimension names
## are @qcode{'Row', 'Variables'}.
## @end multitable
##
## Alternatively, you may specify any of the aforementioned paired arguments as
## @code{@var{Name} = @var{Value}}.
##
## @seealso{cell2table, struct2table, table}
## @end deftypefn
function tbl = array2table (A, varargin)

  ## Check input is a matrix
  if (ndims (A) > 2)
    error ("array2table: input array must be a 2-D array.");
  endif

  ## Parse optional Name-Value paired arguments
  optNames = {'VariableNames', 'RowNames', 'DimensionNames'};
  dfValues = {{}, {}, {}};
  newPairs = {};
  for ii = numel (varargin):-1:1
    tmpi = strsplit (inputname (ii+1, false));
    if (numel (tmpi) > 1)
      idx = find (strcmpi (tmpi{1}, optNames));
      if (strcmp (tmpi{2}, '=') && any (idx))
        newPairs = [newPairs, optNames{idx}, varargin(ii)];
        varargin(ii) = [];
      endif
    endif
  endfor
  args = [varargin, newPairs];
  [varNames, rowNames, dimNames, args] = parsePairedArguments ...
                                         (optNames, dfValues, args);

  ## Split columns into separate input data arguments for table
  varN = size (A, 2);
  varValues = cell (1, varN);
  for ix = 1:varN
    varValues{ix} = A(:,ix);
  endfor

  ## Handle variable names
  if (! isempty (varNames))
    if (numel (varNames) != varN)
      error ("array2table: 'VariableNames' must match the columns in input array.");
    endif
  else
    varName = inputname(1);
    if (isempty (varName))
      varName = 'Var';
    endif
    varNames = cell (1, varN);
    for ix = 1:varN
      varNames{ix} = sprintf ('%s%d', varName, ix);
    endfor
  endif
  optArgs = {'VariableNames', varNames};

  ## Handle remaining paired arguments
  if (! isempty (rowNames))
    if (numel (rowNames) != size (A, 1))
      error ("array2table: 'RowNames' must match the rows in input array.");
    endif
    optArgs = [optArgs {'RowNames', rowNames}];
  endif
  if (! isempty (dimNames))
    if (numel (dimNames) != 2)
      error ("array2table: 'DimensionNames' must be a two-element vector.");
    endif
    optArgs = [optArgs {'DimensionNames', dimNames}];
  endif

  ## Construct table
  tbl = table (varValues{:}, optArgs{:});

endfunction

%!test
%! A = [1, 2; 3, 4];
%! tbl = array2table (A);
%! assert (tbl.A1, [1; 3]);
%! assert (tbl.A2, [2; 4]);
%! assert (size (A), size (tbl));
%!test
%! tbl = array2table ([1, 2; 3, 4]);
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, [2; 4]);
%! assert (size (tbl), [2, 2]);
%!test
%! tbl = array2table ([1, 2; 3, 4], 'VariableNames', {'A', 'B'});
%! assert (tbl.A, [1; 3]);
%! assert (tbl.B, [2; 4]);
%! assert (isa (tbl.A, "double"), true);
%!test
%! tbl = array2table ([1, 2; 3, 4], VariableNames = {'A', 'B'});
%! assert (tbl.A, [1; 3]);
%! assert (tbl.B, [2; 4]);
%! assert (isa (tbl.A, "double"), true);
%!test
%! tbl = array2table ([1, 2; 3, 4], "RowNames", {'A', 'B'});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, [2; 4]);
%! assert (tbl.Properties.RowNames, {'A'; 'B'});
%! assert (class (tbl('A', :)), 'table');
%! assert (tbl{'A', :}, [1, 2]);
%!test
%! tbl = array2table ([1, 2; 3, 4], RowNames = {'A', 'B'});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, [2; 4]);
%! assert (tbl.Properties.RowNames, {'A'; 'B'});
%! assert (class (tbl('A', :)), 'table');
%! assert (tbl{'B', :}, [3, 4]);
%!test
%! tbl = array2table ([1, 2; 3, 4], string ('DimensionNames'), {'A', 'B'});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, [2; 4]);
%! assert (tbl.A, {});
%! assert (tbl.B, [1, 2; 3, 4]);
%!test
%! tbl = array2table ([1, 2; 3, 4], "RowNames", {'A', 'B'}, DimensionNames = {'A', 'B'});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, [2; 4]);
%! assert (tbl.A, {'A'; 'B'});
%! assert (tbl.B, [1, 2; 3, 4]);

%!error<array2table: input array must be a 2-D array.> ...
%! array2table (ones (3, 3, 3));
%!error<array2table: 'VariableNames' must match the columns in input array.> ...
%! array2table ([1; 2; 3], 'VariableNames', {'A', 'B'});
%!error<array2table: 'RowNames' must match the rows in input array.> ...
%! array2table ([1; 2; 3], 'RowNames', {'A', 'B'});
%!error<array2table: 'DimensionNames' must be a two-element vector.> ...
%! array2table ([1; 2; 3], 'DimensionNames', {'A', 'B', 'C'});
