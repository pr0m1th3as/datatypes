## Copyright (C) 2024-2026 Andreas Bertsatos <abertsatos@biol.uoa.gr>
##
## This file is part of the datatypes package for GNU Octave.
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of the
## License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, see
## <http:##www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {private} {[@var{g}, @var{gn}, @var{gl}] =} __grp2idx__ (@var{s})
##
## Get index for group variables.
##
## @end deftypefn

function [g, gn, gl] = __grp2idx__ (s)
  if (nargin != 1)
    print_usage ();
  endif

  s_was_char = false;
  if (ischar (s))
    s_was_char = true;
    s = cellstr (s);
  elseif (! isvector (s))
    error ("grp2idx: S must be a vector, cell array of strings, or char matrix.");
  endif

  [gl, I, g] = unique (s(:));
  ## Fix order in here, since unique does not support this yet
  if (iscellstr (s))
    I = sort(I);
    for i = 1:length (gl)
      gl_s(i) = gl(g(I(i)));
      idx(i,:) = (g == g(I(i)));
    endfor
    for i = 1:length (gl)
      g(idx(i,:)) = i;
    endfor
    gl = gl_s;
    gl = gl';
  else
    I = sort(I);
    for i = 1:length (gl)
      gl_s(i) = gl(g(I(i)));
      idx(i,:) = (g == g(I(i)));
    endfor
    for i = 1:length (gl)
      g(idx(i,:)) = i;
    endfor
    gl = gl_s;
    gl = gl';
  endif

  ## Handle NaNs and empty strings
  if (iscellstr (s))
    empties = cellfun (@isempty, s);
    if (any (empties))
      g(empties) = NaN;
      while (min (g) > 1)
        g--;
      endwhile
    endif
    empties = cellfun (@isempty, gl);
    if (any (empties))
      gl(empties) = [];
    endif
  else
    ## This works fine because NaN come at the end after sorting, we don't
    ## have to worry about change on the indices.
    g(isnan (s)) = NaN;
    gl(isnan (gl)) = [];
  endif

  if (nargout > 1)
    if (iscellstr (gl))
      gn = gl;
    elseif (iscell (gl))
      gn = cellfun (@num2str, gl, "UniformOutput", false);
    else
      gn = arrayfun (@num2str, gl, "UniformOutput", false);
    endif
  endif

  if (nargout > 2 && s_was_char)
    gl = char (gl);
  endif

endfunction
