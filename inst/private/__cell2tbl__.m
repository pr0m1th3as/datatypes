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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{tbl} =} __cell2tbl__ (@var{C}, @var{T}, @var{N}, @var{D}, @var{U}, @var{RowNames}, @var{leaf})
## @deftypefnx {datatypes} {@var{tbl} =} __cell2tbl__ (@var{C}, @var{T}, @var{N}, @var{D}, @var{U}, @var{RowNames}, @var{leaf}, @var{A})
##
## Rebuild a table from the flat data and header blocks of a house I/O file.
##
## @var{C} is the data block, one column per written column, and @var{T},
## @var{N}, @var{D} and @var{U} are the variable-type, name, description and
## unit blocks, each with one row per nesting level.  Columns sharing a name are
## one multicolumn variable; a variable whose type row goes deeper is a nested
## table or a structure and is rebuilt by recursion.
##
## @var{leaf} converts one variable's columns to its value and is called as
## @code{@var{leaf} (@var{varC}, @var{varA}, @var{typestr})}.  @var{A} is an
## optional companion block sliced by column alongside @var{C}, for a reader
## that carries per-cell information of its own, such as the ODS value types; it
## defaults to empty.
##
## @end deftypefn

function tbl = __cell2tbl__ (C, T, N, D, U, RowNames, leaf, A = {})

  ## Get names, number, and positions of top level variables
  [varNames, ii, varIdx] = __unique__ (N(1,:), 'stable');
  varlen = numel (ii);
  varValues = cell (1, varlen);
  ## No nested table or structure
  if (size (T, 1) == 1)
    for ix = 1:varlen
      colIdx = varIdx == ix;
      varC = C(:,colIdx);
      varA = slice_companion (A, colIdx);
      varValues{ix} = leaf (varC, varA, T{ii(ix)});
    endfor
  ## Table contains nested tables or structures
  else
    ## For each top level variable search for nested tables or structures
    for ix = 1:varlen
      colIdx = varIdx == ix;
      varC = C(:,colIdx);
      varA = slice_companion (A, colIdx);
      varN = N(:,colIdx);
      varT = T(:,colIdx);
      ## No nested table or structure in this variable
      if (all (__ismissing__ (varT(2,:))))
        varValues{ix} = leaf (varC, varA, varT{1});
      ## Check for structure
      elseif (all (strcmp (varT(1,:), 'struct')))
        varValues{ix} = cell2struct (varC, varN(2,:), 2);
      ## Check for table
      elseif (all (strcmp (varT(1,:), 'table')))
        ## Pass the nested descriptions/units (rows below the top-level one)
        ## down so the recursive call restores them on the inner table.
        if (isempty (D))
          varD = [];
        else
          varD = D(2:end,colIdx);
        endif
        if (isempty (U))
          varU = [];
        else
          varU = U(2:end,colIdx);
        endif
        varValues{ix} = __cell2tbl__ (varC, varT(2:end,:), varN(2:end,:), ...
                                      varD, varU, [], leaf, varA);
      endif
    endfor
  endif
  ## Create table
  if (isempty (RowNames))
    tbl = table (varValues{:}, 'VariableNames', varNames);
  else
    tbl = table (varValues{:}, 'VariableNames', varNames, 'RowNames', RowNames);
  endif
  ## Restore variable descriptions and units, when present.  The first header
  ## row holds each top-level variable's metadata (deeper rows belong to nested
  ## tables and are restored by the recursive calls above).  Multicolumn
  ## variables repeat the entry across split columns, so take the first column
  ## belonging to each variable.
  if (! isempty (D))
    tbl.Properties.VariableDescriptions = D(1,ii);
  endif
  if (! isempty (U))
    tbl.Properties.VariableUnits = U(1,ii);
  endif
endfunction

## A companion block is sliced by the same columns as the data block; a reader
## that has none passes an empty one through untouched.
function varA = slice_companion (A, colIdx)
  if (isempty (A))
    varA = {};
  else
    varA = A(:,colIdx);
  endif
endfunction
