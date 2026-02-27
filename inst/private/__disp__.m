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
## this program; if not, see <http://www.gnu.org/licens

## -*- texinfo -*-
## @deftypefn  {private} {} __disp__ (@var{obj}, @var{class})
## @deftypefnx {private} {} __disp__ (@var{obj}, @var{class}, @var{varname})
##
## Display arrays of datatype objects.
##
## @end deftypefn

function __disp__ (this, datatype, name = 'ans')
  if (isempty (this))
    str = strjoin (repmat ({'%d'}, 1, ndims (this)), 'x');
    str = sprintf ('\n  %s empty %s array\n\n', str, datatype);
    fprintf (str, size (this));
  elseif (isscalar (this))
    fprintf ('  %s\n\n', datatype);
    fprintf ('   %s\n', dispstrings (this){:});
  elseif (ismatrix (this))
    fprintf ('  %dx%d %s array\n\n', size (this), datatype);
    dispcstrmatrix (dispstrings (this));
  else
    str = strjoin (repmat ({'%d'}, 1, ndims (this)), 'x');
    str = sprintf ('\n  %s %s array\n\n', str, datatype);
    fprintf (str, size (this));
    ## Handle each page separately
    sz = size (this);
    high_sz = sz(3:end);
    high_ixs = {};
    for i = 1:numel (high_sz)
      high_ixs{i} = [1:high_sz(i)]';
    endfor
    page_ixs = combvec (high_ixs);
    idx.type = '()';
    for ix = 1:size (page_ixs, 1)
      p_ix = page_ixs(ix,:);
      idx.subs = {":", ":", num2cell(p_ix){:}};
      pagestr = sprintf (strjoin(repmat ({'%d'}, 1, numel (p_ix)), ':'), p_ix);
      fprintf ('%s(:,:,%s) = \n\n', name, pagestr);
      page_this = subsref (this, idx);
      dispcstrmatrix (dispstrings (page_this));
    endfor
  endif
endfunction

function dispcstrmatrix (cstr)
  sz = terminal_size ();
  cols = sz(2) - 4;
  colgap = "    ";
  optLens = [];
  for iCol = 1:size (cstr, 2)
    optLen = max (cellfun ('length', cstr(:, iCol)));
    optLens = [optLens, optLen];
  endfor
  if (sum (optLens + 4) <= cols) # all columns fit in terminal size
    rowSpat = "";
    for iCol = 1:size (cstr, 2)
      rowSpat = [rowSpat, sprintf("%%+%ds", optLens(iCol)), colgap];
    endfor
    for iRow = 1:size (cstr, 1)
      strrow = sprintf (rowSpat, cstr{iRow,:});
      fprintf ("    %s\n", strrow);
    endfor
    fprintf ("\n");
  else  # we need to split rows
    optLen_cs = cumsum (optLens + 6);
    startCol = 1;
    while (! isempty (find (optLen_cs > cols)))
      stopCol = find (optLen_cs > cols, 1) - 1;
      rowSpat = "";
      for iCol = 1:stopCol
        rowSpat = [rowSpat, sprintf("%%-%ds", optLens(iCol)), colgap];
      endfor
      optLens(1:iCol) = [];
      optLen_cs = cumsum (optLens + 6);
      stopCol = stopCol + startCol - 1;
      fprintf ("Columns %d through %d:\n\n", startCol, stopCol);
      for iRow = 1:size (cstr, 1)
        strrow = sprintf (rowSpat, cstr{iRow,[startCol:stopCol]});
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
      for iRow = 1:size (cstr, 1)
        strrow = sprintf (rowSpat, cstr{iRow,[startCol:stopCol]});
        fprintf ("    %s\n", strrow);
      endfor
      fprintf ("\n");
    endif
  endif
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
