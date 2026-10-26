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
## @deftypefn {datatypes} {@var{TF} =} isdictionary (@var{X})
##
## True if input is a @code{dictionary} object, false otherwise.
##
## @code{@var{TF} = isdictionary (@var{X})} always returns a logical scalar,
## for a configured or an unconfigured dictionary alike.  MATLAB has no such
## function; it is provided as @code{istable} and @code{istimeseries} are for
## their classes.
##
## @seealso{dictionary, isa}
## @end deftypefn
function TF = isdictionary (x)
  TF = isa (x, 'dictionary');
endfunction

%!demo
%! ## `isdictionary` is true for a dictionary, configured or not, and false
%! ## for anything else, a cell array holding a dictionary included.
%!
%! isdictionary (dictionary ())
%! isdictionary (dictionary ('a', 1))
%! isdictionary (struct ('a', 1))
%! isdictionary ({dictionary()})

%!assert_equal (isdictionary (dictionary ()), true)
%!assert_equal (isdictionary (configureDictionary ('string', 'double')), true)
%!assert_equal (isdictionary (struct ()), false)
%!assert_equal (isdictionary ({dictionary()}), false)
%!assert_equal (isdictionary ([1, 2, 3]), false)
%!assert_equal (isdictionary ('dictionary'), false)
