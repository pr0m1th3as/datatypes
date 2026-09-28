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
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {datatypes} {@var{str} =} strings
## @deftypefnx {datatypes} {@var{str} =} strings (@var{n})
## @deftypefnx {datatypes} {@var{str} =} strings (@var{sz1}, @dots{}, @var{szN})
## @deftypefnx {datatypes} {@var{str} =} strings (@var{sz})
##
## Create a string array of strings with no characters.
##
## @code{@var{str} = strings} returns a string scalar with no characters,
## @qcode{""}.
##
## @code{@var{str} = strings (@var{n})} returns an @math{N*N} string array
## whose every element has no characters.
##
## @code{@var{str} = strings (@var{sz1}, @dots{}, @var{szN})} returns a string
## array of size @var{sz1}-by-@dots{}-by-@var{szN}, and
## @code{@var{str} = strings (@var{sz})} takes the same sizes as the row vector
## @var{sz}.
##
## A size may be numeric or logical, and must be a real, finite integer.  A
## negative size counts as zero, as does an empty size argument, and an empty
## size vector gives a @math{0*0} array.
##
## The elements are strings with no characters, not missing strings, so
## @code{ismissing} is false and @code{strlength} is zero for every one of
## them.  @code{string (cell (@var{sz}))} gives missing strings instead.
##
## @seealso{string, string.empty}
## @end deftypefn
function str = strings (varargin)

  if (nargin == 0)
    sz = [1, 1];
  elseif (nargin == 1)
    sz = varargin{1};
    errmsg = size_errmsg (sz);
    if (! isempty (errmsg))
      error ("strings: %s", errmsg);
    endif
    if (isempty (sz))
      sz = [0, 0];
    elseif (isscalar (sz))
      sz = [sz, sz];
    elseif (! isrow (sz))
      error ("strings: size vector must be a row vector.");
    endif
  else
    for i = 1:nargin
      errmsg = size_errmsg (varargin{i});
      if (! isempty (errmsg))
        error ("strings: %s", errmsg);
      endif
      if (numel (varargin{i}) > 1)
        error (strcat ("strings: each size must be a scalar when more than", ...
                       " one is given."));
      endif
    endfor
    ## An empty size argument counts as zero, as in MATLAB
    sz = cellfun (@(x) sum (double (x)), varargin);
  endif
  sz = max (double (sz), 0);

  str = string (repmat ({''}, sz));

endfunction

## The problem with the size argument SZ, or empty when there is none.
function errmsg = size_errmsg (sz)
  errmsg = '';
  if (! (isnumeric (sz) || islogical (sz)))
    errmsg = "size must be numeric or logical.";
  elseif (! isreal (sz))
    errmsg = "size must be real.";
  elseif (! all (isfinite (sz(:))))
    errmsg = "size must be finite.";
  elseif (any (sz(:) != fix (sz(:))))
    errmsg = "size must be an integer.";
  endif
endfunction

%!test
%! str = strings;
%! assert_equal (size (str), [1, 1]);
%! assert_equal (ismissing (str), false);
%! assert_equal (strlength (str), 0);
%!assert_equal (size (strings (3)), [3, 3])
%!assert_equal (size (strings (2, 3)), [2, 3])
%!assert_equal (size (strings ([2, 3])), [2, 3])
%!assert_equal (size (strings (2, 3, 4)), [2, 3, 4])
%!assert_equal (size (strings ([2, 3, 4])), [2, 3, 4])
%!assert_equal (size (strings (2, 3, 1, 1)), [2, 3])
%!assert_equal (size (strings ([2, 3, 0])), [2, 3, 0])
%!assert_equal (size (strings (0)), [0, 0])
%!assert_equal (size (strings (0, 3)), [0, 3])
%!assert_equal (size (strings (-1)), [0, 0])
%!assert_equal (size (strings (2, -1)), [2, 0])
%!assert_equal (size (strings ([])), [0, 0])
%!assert_equal (size (strings (zeros (1, 0))), [0, 0])
%!assert_equal (size (strings (2, [])), [2, 0])
%!assert_equal (size (strings (int8 (2))), [2, 2])
%!assert_equal (size (strings (uint64 (2), 3)), [2, 3])
%!assert_equal (size (strings (true)), [1, 1])
%!assert_equal (class (strings (2)), 'string')
%!assert_equal (ismissing (strings (2, 3)), false (2, 3))
%!assert_equal (strlength (strings (2, 3)), zeros (2, 3))
%!assert_equal (cellstr (strings (1, 2)), {'', ''})

%!error <strings: size must be an integer.> strings (2.5)
%!error <strings: size must be numeric or logical.> strings ('2')
%!error <strings: size must be numeric or logical.> strings (string ('2'))
%!error <strings: size must be numeric or logical.> strings (2, 'like', 'a')
%!error <strings: size must be finite.> strings (NaN)
%!error <strings: size must be finite.> strings (Inf)
%!error <strings: size must be real.> strings (1 + 2i)
%!error <strings: size vector must be a row vector.> strings ([2; 3])
%!error <strings: each size must be a scalar when more than one is given.> ...
%! strings (1, [2, 3])
