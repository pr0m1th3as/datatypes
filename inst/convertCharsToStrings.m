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
## @deftypefn  {datatypes} {@var{B} =} convertCharsToStrings (@var{A})
## @deftypefnx {datatypes} {[@var{B1}, @dots{}, @var{Bn}] =} convertCharsToStrings (@var{A1}, @dots{}, @var{An})
##
## Convert character arrays to a string arrays, where applicable.
##
## @code{@var{B} = convertCharsToStrings (@var{A})} converts @var{A} to a string
## scalar, if @var{A} is a character vector or array, or to a string array, if
## @var{A} is a cell array of character vectors.  Otherwise, @var{A} is returned
## unaltered.
##
## @code{[@var{B1}, @dots{}, @var{Bn}] = convertCharsToStrings (@var{A1},
## @dots{}, @var{An})} converts any of the input arguments that are either
## character arrays or cell arrays of character vectors to string scalars or
## string arrays, respectively.  Otherwise, @code{convertCharsToStrings} returns
## the input arguments unaltered.
##
## @seealso{convertStringsToChars, string}
## @end deftypefn
function varargout = convertCharsToStrings (varargin)
  for i = 1:numel (varargin)
    if (ischar (varargin{i}))
      varargout{i} = string (varargin{i}(:)');
    elseif (iscellstr (varargin{i}))
      varargout{i} = string (varargin{i});
    else
      varargout{i} = varargin{i};
    endif
  endfor
endfunction

%!test
%! A = 'text';
%! B = [1, 2, 3];
%! C = string ("test");
%! [A1, B1, C1] = convertCharsToStrings (A, B, C);
%! assert (isstring (A1), true);
%! assert (B, B1);
%! assert (C == C1, true);
%!test
%! A = {'asd', 'ert'; 'xcv', 'dfg'};
%! A1 = convertCharsToStrings (A);
%! assert (isequal (size (A), size (A1)));
%! assert (isstring (A1), true);
%!test
%! A = categorical (NaN);
%! B = duration;
%! C = 'text';
%! [A1, B1, C1] = convertCharsToStrings (A, B, C);
%! assert (iscategorical (A1), true);
%! assert (isduration (B1), true);
%! assert (isstring (C1), true);
%!test
%! A = ['e','r';'v','b'];
%! A1 = convertCharsToStrings (A);
%! assert (isscalar (A1), true);
%! assert (strcmp (char (A1), 'evrb'), true);
