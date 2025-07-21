## Copyright (C) 2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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
## @deftypefn  {datatypes} {@var{tbl} =} csv2table (@var{file})
## @deftypefnx {datatypes} {@var{tbl} =} csv2table (@var{file}, @var{Name}, @var{Value})
##
## Load a CSV file into a table.
##
##
##
## @seealso{array2table, struct2table, table}
## @end deftypefn
function tbl = csv2table (name, varargin)

  ## Check first input is a character vector or a string scalar
  if (ischar (name) && isvector (name))
    C = __csv2table__ (name);
  elseif (isa  (name, 'string') && isscalar (name))
    C = __csv2table__ (char (name));
  else
    error ("csv2table: NAME must be a character vector or a string scalar.");
  endif
  if (ischar (C))
    error ("csv2table: %s.", C);
  endif

  ## Get first comment line (as saved by 'table2csv' method)
  H_key = strsplit (C{1,1});
  H_txt = strjoin (H_key([1,2,4,5,7,8,10,11,13]));
  txt = '# varTypes rows; varNames rows; varDescriptions rows; varUnits rows.';
  if (strcmpi (H_txt, txt))
    Trows = str2num (H_key{3});
    Nrows = str2num (H_key{6});
    Drows = str2num (H_key{9});
    Urows = str2num (H_key{12});
    C(1,:) = [];  # remove comment line
  else
    Trows = 0;
    Nrows = 0;
    Drows = 0;
    Urows = 0;
  endif

  ## Handle CSV file with vartype header (as saved by table2cev method)
  if (Trows > 0)
    Hrows = Trows + Nrows + Drows + Urows;

    ## Check for RowNames
    if (strcmp (C{1,1}, 'RowNames') && isempty (C{Trows+1,1}))
      RowNames = C([Hrows+1:end],1);
      C(:,1) = [];      # remove row names
    else
      RowNames = {};
    endif

    ## Split cell into headers and data
    T = C(1:Trows,:);     # variable types
    C(1:Trows,:) = [];
    N = C(1:Nrows,:);     # variable names
    C(1:Nrows,:) = [];
    if (Drows)
      D = C(1:Drows,:);   # variable descriptions
      C(1:Drows,:) = [];
    else
      D = {};
    endif
    if (Urows)
      U = C(1:Urows,:);   # variable units
      C(1:Urows,:) = [];
    else
      U = {};
    endif
    ## after this point C contains only data

    ## Construct table
    tbl = cell2tbl (C, T, N, D, U, RowNames);
  endif

  ## Parse optional Name-Value paired arguments
  optNames = {'NumHeaderLines', 'VariableNames', 'ReadVariableNames', ...
              'VariableNamingRule', 'VariableNamesLine', 'VariableTypes', ...
              'VariableUnitsLine', 'VariableDescriptionsLine', ...
              'ReadRowNames', 'RowNamesColumn', 'TextType', ...
              'DatetimeType', 'DurationType', 'HexType'};
  dfValues = {0, {}, true, 'modify', 1, {}, 0, 0, true, 1, 'char', ...
              'datetime', 'duration', 'auto'};
  [numHeaderLines, varNames, readVarNames, varNamingRule, varNamesLine, ...
   varTypes, varUnitsLine, varDescrLine, readRowNames, rowNamesColumn, ...
   textType, datetimeType, durationTypes, hexType, args] = pairedArgs ...
                                            (optNames, dfValues, varargin(:));

  ## Apply optional argmuments to CSV files with vartype header
  if (Trows > 0)
    if (numHeaderLines)
      tbl(1:numHeaderLines,:) = [];
    endif
    if (! isempty (varNames))
      tbl = tbl(:,varNames);
    endif
    if (! readRowNames)
      tbl.Properties.RowNames = [];
    endif
    return
  endif

  ## Handle generic CSV file

endfunction

function varValue = cell2var (C, T)
  ## Numeric vartypes
  numvartype = {'double', 'single', 'int8', 'uint8', 'int16', ...
                'uint16', 'int32', 'uint32', 'int64', 'uint64'};
  if (strcmp (T, "cell"))
    varValue = C;
  elseif (strcmp (T, "logical"))
    varValue = logical (cell2mat (C));
  elseif (ismember (T, numvartype))
    varValue = cast (cell2mat (C), T);
  elseif (strcmp (T, "calendarDuration"))
    warning ("csv2table: 'calendarDuration' strings are not converted.");
    varValue = C;
  elseif (strcmp (T, "categorical"))
    warning ("csv2table: 'categorical' strings are not converted.");
    varValue = C;
  elseif (strcmp (T, "datetime"))
    varValue = datetime (C);
  elseif (strcmp (T, "duration"))
    varValue = duration (C);
  elseif (strcmp (T, "string"))
    varValue = string (C);
  endif
endfunction

function tbl = cell2tbl (C, T, N, D, U, RowNames);
  ## Get names, number, and positions of top level variables
  [varNames, ii, varIdx] = __unique__ (N(1,:), 'stable');
  varlen = numel (ii);
  varValues = cell (1, varlen);
  ## No nested table or structure
  if (size (T, 1) == 1)
    for ix = 1:varlen
      colIdx = varIdx == ix;
      varC = C(:,colIdx);
      varValues{ix} = cell2var (varC, T{ii(ix)});
    endfor
  ## Table contains nested tables or structures
  else
    ## For each top level variable search for nested tables or structures
    for ix = 1:varlen
      colIdx = varIdx == ix;
      varC = C(:,colIdx);
      varN = N(:,colIdx);
      varT = T(:,colIdx);
      ## No nested table or structure in this variable
      if (all (__ismissing__ (varT(2,:))))
        varValues{ix} = cell2var (varC, varT{1});
      ## Check for structure
      elseif (all (strcmp (varT(1,:), 'structure')))
        varValues{ix} = cell2struct (varC, varN(2,:), 2);
      ## Check for table
      elseif (all (strcmp (varT(1,:), 'table')))
        varValues{ix} = cell2tbl (varC, varT(2:end,:), varN(2:end,:), [], [], []);
      endif
    endfor
  endif
  ## Create table
  if (isempty (RowNames))
    tbl = table (varValues{:}, 'VariableNames', varNames);
  else
    tbl = table (varValues{:}, 'VariableNames', varNames, 'RowNames', RowNames);
  endif
endfunction

