## Copyright (C) 1995-2023 The Octave Project Developers
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

## -*- texinfo -*-
## @deftypefn  {private} {@var{TF} =} __ismissing__ (@var{A})
## @deftypefnx {private} {@var{TF} =} __ismissing__ (@var{A}, @var{indicator})
##
## Find missing data in a numeric or string array.
##
## @end deftypefn

function TF = __ismissing__ (A, indicator)

  ## Check "indicator"
  if (nargin != 2)
     indicator = [];
  endif

  ## If A is an array of cell strings and indicator just a string,
  ## convert indicator to a cell string with one element
  if (iscellstr (A) && ischar (indicator) && ! iscellstr (indicator))
    indicator = {indicator};
  endif

  if ((! isempty (indicator)) &&
      ((isnumeric (A) && ! (isnumeric (indicator) || islogical (indicator))) ||
       (ischar (A) && ! ischar (indicator)) ||
       (iscellstr (A) && ! iscellstr (indicator))))
    error ("ismissing: 'indicator' and 'A' must have the same data type.");
  endif

  ## Main logic
  if (isempty (indicator))

    if (isnumeric (A))
      ## numeric matrix: just find the NaNs
      ## integer types have no missing value, but isnan will return false
      TF = isnan (A);

    elseif (iscellstr (A))
      ## cell strings - find empty cells
      TF = cellfun ('isempty', A);

    elseif (ischar (A))
      ## char matrix: find the white spaces
      TF = isspace (A);

    else
      ## no missing type defined, return false
      TF = false (size (A));
    endif

  else
    ## Indicator specified for missing data
    TF = false (size (A));
    if (isnumeric(A) || islogical (A))
      for iter = 1:numel (indicator)
        if (isnan (indicator(iter)))
          TF(isnan(A)) = true;
        else
          TF(A == indicator(iter)) = true;
        endif
      endfor
    elseif (ischar (A))
      for iter = 1:numel (indicator)
        TF(A == indicator(iter)) = true;
      endfor
    elseif (iscellstr (A))
      for iter = 1:numel (indicator)
        TF(strcmp (A, indicator(iter))) = true;
      endfor
    endif
  endif
endfunction
