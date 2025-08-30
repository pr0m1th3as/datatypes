## Copyright (C) 2024-2025 Andreas Bertsatos <abertsatos@biol.uoa.gr>
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

function __disp__ (C, name = 'ans')
  if (isempty (C))
    fprintf ('  0x0 empty cell array\n\n');
    fprintf ('    {0x0 double}\n\n');
  elseif (ismatrix (C))
    fprintf ('  %dx%d cell array\n\n', size (C));
    dispcellmatrix (C);
  else
    str = strjoin (repmat ({'%d'}, 1, ndims (C)), 'x');
    str = sprintf ('\n  %s cell array\n\n', str);
    fprintf (str, size (C));
    ## Handle each page separately
    sz = size (C);
    high_sz = sz(3:end);
    high_ixs = {};
    for i = 1:numel (high_sz)
      high_ixs{i} = [1:high_sz(i)]';
    endfor
    page_ixs = combvec (high_ixs);
    for ix = 1:size (page_ixs, 1)
      p_ix = page_ixs(ix,:);
      pagestr = sprintf (strjoin(repmat ({'%d'}, 1, numel (p_ix)), ':'), p_ix);
      fprintf ('%s(:,:,%s) = \n\n', name, pagestr);
      page_C = C(:,:,p_ix);
      dispcellmatrix (page_C);
    endfor
  endif
endfunction

function dispcellmatrix (C)
  sz = terminal_size ();
  cols = sz(2) - 4;
  colgap = "    ";
  dispstr = {};
  optLens = [];
  for iCol = 1:size (C, 2)
    [outstr, optLen] = mixedcell2str (C(:, iCol), cols);
    dispstr = [dispstr, outstr];
    optLens = [optLens, optLen];
  endfor
  if (sum (optLens + 6) <= cols) # all columns fit in terminal size
    rowSpat = "";
    for iCol = 1:size (C, 2)
      rowSpat = [rowSpat, sprintf("%%-%ds", optLens(iCol)), colgap];
    endfor
    for iRow = 1:size (C, 1)
      strrow = sprintf (rowSpat, dispstr{iRow,:});
      fprintf ("    %s\n", strrow);
    endfor
    fprintf ("\n");
  else  # we need to split rows
    optLen_cs = cumsum (optLens + 6);
    startCol = 1;
    while (! isempty (find (optLen_cs > cols)))
      stopCol = find (optLen_cs > cols, 1) - 1;
      ## Just in case a single column exceeds terminal size
      if (stopCol == 0)
        stopCol = 1;
      endif
      rowSpat = "";
      for iCol = 1:stopCol
        rowSpat = [rowSpat, sprintf("%%-%ds", optLens(iCol)), colgap];
      endfor
      optLens(1:iCol) = [];
      optLen_cs = cumsum (optLens + 6);
      stopCol = stopCol + startCol - 1;
      fprintf ("Columns %d through %d:\n\n", startCol, stopCol);
      for iRow = 1:size (C, 1)
        strrow = sprintf (rowSpat, dispstr{iRow,[startCol:stopCol]});
        fprintf ("    %s\n", strrow);
      endfor
      fprintf ("\n");
      startCol = stopCol + 1;
    endwhile
    if (! isempty (optLens))
      for iCol = 1:length (optLens)
        rowSpat = [rowSpat, sprintf("%%-%ds", optLens(iCol)), colgap];
      endfor
      stopCol = startCol + iCol - 1;
      if (startCol == stopCol)
        fprintf ("Column %d:\n\n", startCol);
      else
        fprintf ("Columns %d through %d:\n\n", startCol, stopCol);
      endif
      for iRow = 1:size (C, 1)
        strrow = sprintf (rowSpat, dispstr{iRow,[startCol:stopCol]});
        fprintf ("    %s\n", strrow);
      endfor
      fprintf ("\n");
    endif
  endif
endfunction

## Special function to convert a mixed cell array to cellstr array
## that keeps MATLAB like formatting for each type of element
function [dispstr, optLen]  = mixedcell2str (data, cols)
  dispstr = cell (size (data));
  ## Preallocate indexes to avoid truncation when last elements are 0
  is_char = logical (zeros (size (data)));
  is_bool = is_char;
  is_numeric = is_char;
  is_object = is_char;
  is_struct = is_char;
  has_method = is_char;
  no_method = is_char;

  ## Nested cells are printed by size
  is_cell = cellfun ('iscell', data);
  sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                                   ' cell']), size (x));
  dispstr(is_cell) = cellfun (sf, data(is_cell), "UniformOutput", false);

  ## Empty cells are printed as doubles
  is_empty = cellfun (@isempty, data);
  sf = @(x) sprintf ("0x0 %s", class (x));
  dispstr(is_empty) = cellfun (sf, data(is_empty), "UniformOutput", false);

  ## Index remaining scalar and row vector elements
  ve = cell2mat (cellfun (@(x) size (x,1), data, "UniformOutput", false)) == 1;
  ve = ve & ! (is_cell | is_empty);
  ## Index everything else
  me = cell2mat (cellfun (@(x) size (x,1), data, "UniformOutput", false)) != 1;
  me = me & ! (is_cell | is_empty);

  ## Catch 'char' scalars or row vectors
  is_char(ve) = cellfun ('ischar', data(ve));
  sf = @(x) sprintf ("'%s'", x);
  dispstr(is_char) = cellfun (sf, data(is_char), "UniformOutput", false);

  ## Catch 'logical' scalars or row vectors
  is_bool(ve) = cellfun ('islogical', data(ve));
  sf = @(x) sprintf ("%s", strtrim (sprintf ("%d ", x)));
  dispstr(is_bool) = cellfun (sf, data(is_bool), "UniformOutput", false);

  ## Catch 'numeric' scalars or row vectors
  is_numeric(ve) = cellfun ('isnumeric', data(ve));
  sf = @(x) sprintf ("%s", strtrim (sprintf ("%g ", x)));
  dispstr(is_numeric) = cellfun (sf, data(is_numeric), "UniformOutput", false);

  ## Catch 'object' scalars or row vectors
  is_object(ve) = cellfun ('isobject', data(ve));
  ## Handle objects with dispstring method available
  f = @(x) ismethod (x, 'dispstrings');
  has_method(is_object) = cellfun (f, data(is_object));
  sf = @(x) sprintf ("%s", strjoin (dispstrings (x), '    '));
  dispstr(has_method) = cellfun (sf, data(has_method), "UniformOutput", false);
  ## Handle objects without
  no_method(is_object) = ! cellfun (f, data(is_object));
  sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                             ' %s']), size (x), class (x));
  dispstr(no_method) = cellfun (sf, data(no_method), "UniformOutput", false);

  ## Catch scalar elements or row vectors of 'struct' type
  is_struct(ve) = cellfun ('isstruct', data(ve));
  sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                             ' struct']), size (x));
  dispstr(is_struct) = cellfun (sf, data(is_struct), "UniformOutput", false);

  ## Catch remaining elements containing matrices or arrays of any type
  sf = @(x) sprintf (strcat ([strjoin(repmat ({'%d'}, 1, ndims (x)), 'x'), ...
                             ' %s']), size (x), class (x));
  dispstr(me) = cellfun (sf, data(me), "UniformOutput", false);

  ## Index numerical and logical values to right alignment
  pad_B = is_numeric | is_bool;     # pad before: sprintf("{%%+%ds}"
  ## Index array types for bracketing
  brackets = pad_B | has_method;

  ## Get optimal length
  if (all (brackets))
    optLen = max (cellfun (@length, dispstr(brackets))) + 2;
  elseif (any (brackets))
    optLen1 = max (cellfun (@length, dispstr(brackets))) + 2;
    optLen2 = max (cellfun (@length, dispstr(! brackets)));
    optLen = max (optLen1, optLen2);
  else
    optLen = max (cellfun (@length, dispstr(! brackets)));
  endif

  ## Make sure a single column does not exceed terminal size
  if (optLen > cols)
    for i = 1:sum (is_char)
      hm_idx = find (is_char, i);
      sf = @(x) sprintf ("'%s ... '", x(1:cols-10));
      dispstr(hm_idx) = cellfun (sf, data(hm_idx), "UniformOutput", false);
    endfor
    for i = 1:sum (is_bool)
      hm_idx = find (is_bool, i);
      do_idx = find (cumsum (cellfun ('length', strsplit (dispstr{hm_idx})) ...
                             + 1) > cols - 10, 1) - 1;
      sf = @(x) sprintf ("%s ... ", strtrim (sprintf ("%d ", x(1:do_idx))));
      dispstr(hm_idx) = cellfun (sf, data(hm_idx), "UniformOutput", false);
    endfor
    for i = 1:sum (is_numeric)
      hm_idx = find (is_numeric, i);
      do_idx = find (cumsum (cellfun ('length', strsplit (dispstr{hm_idx})) ...
                             + 1) > cols - 8, 1) - 1;
      sf = @(x) sprintf ("%s ... ", strtrim (sprintf ("%g ", x(1:do_idx))));
      dispstr(hm_idx) = cellfun (sf, data(hm_idx), "UniformOutput", false);
    endfor
    for i = 1:sum (has_method)
      hm_idx = find (has_method, i);
      do_idx = find (cumsum (cellfun ('length', strsplit (dispstr{hm_idx})) ...
                             + 4) > cols - 6, 1) - 1;
      sf = @(x) sprintf ("%s ... ", strjoin (dispstrings (x(1:do_idx)), '    '));
      dispstr(hm_idx) = cellfun (sf, data(hm_idx), "UniformOutput", false);
    endfor
    ## Recalculate optimal length
    if (all (brackets))
      optLen = max (cellfun (@length, dispstr(brackets))) + 2;
    elseif (any (brackets))
      optLen1 = max (cellfun (@length, dispstr(brackets))) + 2;
      optLen2 = max (cellfun (@length, dispstr(! brackets)));
      optLen = max (optLen1, optLen2);
    else
      optLen = max (cellfun (@length, dispstr(! brackets)));
    endif
  endif

  ## Pad data according to optimal length
  ## numeric and logical is right aligned, everything else is left aligned
  Ra_wB = sprintf("{[%%+%ds]}", optLen - 2);
  fcn = @(x) sprintf (Ra_wB, x);
  idx = pad_B & brackets;
  dispstr(idx) = cellfun (fcn, dispstr(idx), "UniformOutput", false);

  La_wB = sprintf("{[%%-%ds]}", optLen - 2);
  fcn = @(x) sprintf (La_wB, x);
  idx = ! pad_B & brackets;
  dispstr(idx) = cellfun (fcn, dispstr(idx), "UniformOutput", false);

  La_nB = sprintf("{%%-%ds}", optLen);
  fcn = @(x) sprintf (La_nB, x);
  idx = (! pad_B & ! brackets) | me;
  dispstr(idx) = cellfun (fcn, dispstr(idx), "UniformOutput", false);
endfunction

function out = combvec (vecs)
  switch (numel (vecs))
    case 1
      out = vecs{1}(:);
    case 2
      a = vecs{1}(:);
      b = vecs{2}(:);
      out = repmat (a, numel (b), 2);
      i_comb = 1;
      for i_a = 1:numel (a)
        for i_b = 1:numel (b)
          out(i_comb,:) = [a(i_a), b(i_b)];
          i_comb = i_comb + 1;
        endfor
      endfor
    otherwise
      out = [];
      a = vecs{1}(:);
      rest = vecs(2:end);
      rest_combs = combvec (rest);
      for i = 1:numel (a)
        out = [out; [repmat(a(i), [size(rest_combs,1), 1]), rest_combs]];
      endfor
  endswitch
endfunction
