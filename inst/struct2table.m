## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
##
## This file is part of the statistics package for GNU Octave.
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
## @deftypefn  {datatypes} {@var{tbl} =} struct2table (@var{S})
## @deftypefnx {datatypes} {@var{tbl} =} struct2table (@var{S}, @var{Name}, @var{Value})
##
## Convert a cell array to a table.
##
## @code{@var{tbl} = struct2table (@var{S})} converts a structure array @var{S}
## to the table @var{tbl}, where each field of the input structure becomes a
## variable in the output table.  For a scalar structure with @math{N} fields,
## all of which have @math{M} rows, or an @math{Mx1} or @math{1xM} structure
## array with @math{N} fields, the output is an @math{MxN} table.
##
## @code{@var{tbl} = array2table (@var{S}, @var{Name}, @var{Value})} specifies
## optional parameters for creating the table @var{tbl} with the following
## Name-Value paired arguments.
##
## @multitable @columnfractions 0.23 0.02 0.75
## @headitem @var{Name} @tab @tab @var{Value}
##
## @item @qcode{'AsArray'} @tab @tab A logical scalar specifying whether to
## treat a scalar input as a structure array, which allows the fields containing
## data of different sizes.
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
## @seealso{array2table, cell2table, table}
## @end deftypefn
function tbl = struct2table (S, varargin)

  ## Check input is a struct scalar of vector
  if (! isstruct (S))
    error ("struct2table: input array must be a structure.");
  endif

  ## Handle default value for AsArray
  if (isscalar (S))
    default = false;
  else
    default = true;
  endif

  ## Parse optional Name-Value paired arguments
  optNames = {'AsArray', 'RowNames', 'DimensionNames'};
  dfValues = {default, {}, {"Row", "Variables"}};
  [AsArray, rowNames, dimNames, args] = pairedArgs (optNames, dfValues, ...
                                                    varargin(:));

  ## Get variable names from structure fields
  varNames = fieldnames (S);
  optArgs = {'VariableNames', varNames};

  ## Get variable data from structure
  varN = numel (varNames);
  varValues = cell (varN, 1);
  if (AsArray)
    for i = 1:varN
      try
        varValues{i} = cat (1, S(:).(varNames{i}));
      catch
        varValues{i} = cat (1, {S(:).(varNames{i})})';
      end_try_catch
    endfor
    nrows = cellfun (@rows, varValues);
    if (! all (ismember (nrows, numel (S))))
      for i = 1:varN
        varValues{i} = cat (1, {S(:).(varNames{i})})';
      endfor
    endif
  else
    vals = struct2cell (S);
    for i = 1:varN
      try
        varValues{i} = cat (1, vals{i,1,:});
      catch
        varValues{i} = cat (1, vals(i,1,:));
      end_try_catch
    endfor
    nrows = cellfun (@rows, varValues);
    if (! all (ismember (nrows, nrows(1))))
      error ("struct2table: fields have different rows. Use 'AsArray' option.");
    endif
  endif

  ## Handle remaining paired arguments
  if (! isempty (rowNames))
    if (numel (rowNames) != nrows)
      error ("struct2table: 'RowNames' must match the rows in input cell.");
    endif
    optArgs = [optArgs {'RowNames', rowNames}];
  endif
  if (! isempty (dimNames))
    if (numel (dimNames) != 2)
      error ("struct2table: 'DimensionNames' must be a two-element vector.");
    endif
    optArgs = [optArgs {'DimensionNames', dimNames}];
  endif

  ## Construct table
  tbl = table (varValues{:}, optArgs{:});

endfunction

%!test
%! S(1).A = [1, 2];
%! S(2).A = [3, 4];
%! S(1).B = 5;
%! S(2).B = 6;
%! tbl = struct2table(S);
%! assert (tbl.A, [1, 2; 3, 4]);
%! assert (tbl.B, [5; 6]);
%! assert (numel (S), rows (tbl));
%!test
%! S(1).A = [1, 2];
%! S(2).A = [3, 4, 5, 6];
%! S(1).B = 7;
%! S(2).B = 8;
%! tbl = struct2table(S);
%! assert (isa (tbl.A, 'cell'), true);
%! assert (isa (tbl.B, 'double'), true);
%! assert (numel (S), rows (tbl));
%! assert (tbl.A(1){1}, [1, 2]);
%! assert (tbl.A(2){1}, [3, 4, 5, 6]);
%! assert (tbl.B, [7; 8]);
%!test
%! S.A = [1; 2; 3];
%! S.B = [4, 5; 6, 7; 8, 9];
%! tbl = struct2table(S);
%! assert (isa (tbl.A, 'double'), true);
%! assert (isa (tbl.B, 'double'), true);
%! assert (tbl.A, [1; 2; 3]);
%! assert (tbl.B, [4, 5; 6, 7; 8, 9]);
%!test
%! tbl = struct2table (struct ('A', 1, 'B', [1; 2]), 'AsArray', true);
%! assert (iscell (tbl.A), true);
%! assert (iscell (tbl.B), true);
%! assert (size (tbl.B{1}), [2, 1]);

%!error<struct2table: input array must be a structure.> struct2table ({1});
%!error<struct2table: fields have different rows. Use 'AsArray' option.> ...
%! struct2table (struct ('A', 1, 'B', [1; 2]));
%!error<struct2table: 'RowNames' must match the rows in input cell.> ...
%! struct2table (struct ('A', {'a';, 'b'}, 'B', [1; 2]), 'RowNames', 'q');
%!error<struct2table: 'DimensionNames' must be a two-element vector.> ...
%! struct2table (struct ('A', {'a';, 'b'}, 'B', [1; 2]), 'DimensionNames', 'q');
