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
## @deftypefn  {datatypes} {@var{B} =} convertStringsToChars (@var{A})
## @deftypefnx {datatypes} {[@var{B1}, @dots{}, @var{Bn}] =} convertStringsToChars (@var{A1}, @dots{}, @var{An})
##
## Convert string arrays to a character arrays, where applicable.
##
## @code{@var{B} = convertStringsToChars (@var{A})} converts @var{A} to a
## character vector, if @var{A} is a string scalar, or to a cell array of
## character vectors, if @var{A} is a string array.  Otherwise, @var{A} is
## returned unaltered.
##
## @code{[@var{B1}, @dots{}, @var{Bn}] = convertStringsToChars (@var{A1},
## @dots{}, @var{An})} converts any of the input arguments that are of string
## type to character vectors or to cell array of character vectors, or leaves
## them unaltered.
##
## @seealso{convertCharsToStrings, string}
## @end deftypefn
function varargout = convertStringsToChars (varargin)
  for i = 1:numel (varargin)
    if (isa (varargin{i}, 'string'))
      if (isscalar (varargin{i}))
        varargout{i} = char (varargin{i});
      else
        varargout{i} = cellstr (varargin{i});
      endif
    else
      varargout{i} = varargin{i};
    endif
  endfor
endfunction

%!test
%! A = 'text';
%! B = [1, 2, 3];
%! C = string ("test");
%! [A1, B1, C1] = convertStringsToChars (A, B, C);
%! assert (A, A1);
%! assert (B, B1);
%! assert (ischar (C1), true);
%!test
%! A = string ({'asd', 'ert'; 'xcv', 'dfg'});
%! A1 = convertStringsToChars (A);
%! assert (isequal (size (A), size (A1)));
%! assert (iscellstr (A1), true);
%!test
%! A = categorical (NaN);
%! B = duration;
%! C = 'text';
%! [A1, B1, C1] = convertStringsToChars (A, B, C);
%! assert (iscategorical (A1), true);
%! assert (isduration (B1), true);
%! assert (ischar (C1), true);
