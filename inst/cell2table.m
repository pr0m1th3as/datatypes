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
## @deftypefn  {datatypes} {@var{tbl} =} cell2table (@var{C})
## @deftypefnx {datatypes} {@var{tbl} =} cell2table (@var{C}, @var{Name}, @var{Value})
##
## Convert a cell array to a table.
##
## @code{@var{tbl} = cell2table (@var{C})} converts the 2-D cell array @var{C}
## to the table @var{tbl}, where the contents of each column of @var{C} becomes
## a variable in @var{tbl}.  The contents of each collumn are concatenated into
## their common data type (i.e. if a column of @var{C} contains explicitly
## @qcode{double} numbers, then the corresponding variable in @var{tbl} is of
## the same type), otherwise they are added as a column of cells.
##
## @code{@var{tbl} = array2table (@var{C}, @var{Name}, @var{Value})} specifies
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
## @seealso{array2table, struct2table, table}
## @end deftypefn
function tbl = cell2table (C, varargin)

  ## Check input is a matrix
  if (ndims (C) > 2)
    error ("cell2table: input array must be a 2-D cell array.");
  endif
  if (! iscell (C))
    error ("cell2table: input array must be a cell array.");
  endif

  ## Parse optional Name-Value paired arguments
  optNames = {'VariableNames', 'RowNames', 'DimensionNames'};
  dfValues = {{}, {}, {}};
  [varNames, rowNames, dimNames, args] = parsePairedArguments ...
                                         (optNames, dfValues, varargin);

  ## Split columns into separate input data arguments for table
  varN = size (C, 2);
  varValues = cell (1, varN);
  for ix = 1:varN
    tmp = C(:,ix);
    if (iscellstr (tmp))
      varValues{ix} = tmp;
    else
      ## Try to concatenate into array
      try
        warning ('off', 'Octave:num-to-str');
        newArray = cat (1, tmp{:});
        warning ('on', 'Octave:num-to-str');
        if (size (newArray, 1) == size (tmp, 1))
          varValues{ix} = newArray;
          continue;
        else
          error ();
        endif
      catch
        varValues{ix} = tmp;
      end_try_catch
    endif
  endfor

  ## Handle variable names
  if (! isempty (varNames))
    if (numel (varNames) != varN)
      error ("cell2table: 'VariableNames' must match the columns in input cell.");
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
    if (numel (rowNames) != size (C, 1))
      error ("cell2table: 'RowNames' must match the rows in input cell.");
    endif
    optArgs = [optArgs {'RowNames', rowNames}];
  endif
  if (! isempty (dimNames))
    if (numel (dimNames) != 2)
      error ("cell2table: 'DimensionNames' must be a two-element vector.");
    endif
    optArgs = [optArgs {'DimensionNames', dimNames}];
  endif

  ## Construct table
  tbl = table (varValues{:}, optArgs{:});

endfunction

%!test
%! C = {1, 2; 3, 4};
%! tbl = cell2table (C);
%! assert (tbl.C1, [1; 3]);
%! assert (tbl.C2, [2; 4]);
%! assert (size (C), size (tbl));
%! assert (isa (tbl.C1, 'double'), true);
%!test
%! tbl = cell2table ({1, 2; 3, 4});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, [2; 4]);
%! assert (size (tbl), [2, 2]);
%! assert (isa (tbl.Var1, 'double'), true);
%!test
%! tbl = cell2table ({1, ''; 3, 4}, 'VariableNames', {'A', 'B'});
%! assert (tbl.A, [1; 3]);
%! assert (tbl.B, {''; 4});
%! assert (isa (tbl.A, 'double'), true);
%! assert (isa (tbl.B, 'double'), false);
%!test
%! tbl = cell2table ({1, ''; 3, 4}, "RowNames", {'A', 'B'});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, {''; 4});
%! assert (tbl.Properties.RowNames, {'A'; 'B'});
%! assert (class (tbl('A', :)), 'table');
%! assert (tbl{'A', :}, {1, ''});
%!test
%! tbl = cell2table ({1, ''; 3, 4}, string ('DimensionNames'), {'A', 'B'});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, {""; 4});
%! assert (tbl.A, {});
%! assert (tbl.B, {1, ''; 3, 4});
%!test
%! tbl = cell2table ({1, ''; 3, 4}, "RowNames", {'A', 'B'}, "DimensionNames", {'A', 'B'});
%! assert (tbl.Var1, [1; 3]);
%! assert (tbl.Var2, {''; 4});
%! assert (tbl.A, {'A'; 'B'});
%! assert (tbl.B, {1, ''; 3, 4});
%!test
%! tbl = cell2table ({1, string(''); 3, 4}, RowNames = {'R1', 'R2'}, "DimensionNames", {'A', 'B'});
%! assert (class (tbl('R1', :)), 'table');
%! assert (class (tbl{'R1', :}), 'string');
%! assert (cellstr (tbl{'R1', :}), {'1', ''});
%! assert (tbl.A, {'R1'; 'R2'});
%! assert (class (tbl.B), 'string');
%! assert (cellstr (tbl.B), {'1', ''; '3', '4'});

%!error<cell2table: input array must be a 2-D cell array.> ...
%! cell2table (cell (3, 3, 3));
%!error<cell2table: input array must be a cell array.> cell2table ([3; 3; 3]);
%!error<cell2table: 'VariableNames' must match the columns in input cell.> ...
%! cell2table ({1; 2; 3}, 'VariableNames', {'A', 'B'});
%!error<cell2table: 'RowNames' must match the rows in input cell.> ...
%! cell2table ({1; 2; 3}, 'RowNames', {'A', 'B'});
%!error<cell2table: 'DimensionNames' must be a two-element vector.> ...
%! cell2table ({1; 2; 3}, 'DimensionNames', {'A', 'B', 'C'});
