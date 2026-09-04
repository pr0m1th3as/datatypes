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
## @deftypefn  {datatypes} {[@var{base}, @var{fmt}] =} __typefmt__ (@var{T})
##
## Split a variable-type string into its type and its display format.
##
## A @code{datetime} or @code{duration} variable written by @code{table2csv} or
## @code{table2ods} carries its @qcode{Format} in the variable-type row, after a
## @qcode{"|"}, so that the format survives the round trip.  @var{base} is the
## part before the separator, which still carries a @code{datetime}'s
## @qcode{TimeZone}, and @var{fmt} is the part after it, empty when the string
## carries no format.
##
## @end deftypefn

function [base, fmt] = __typefmt__ (T)

  base = T;
  fmt = '';
  k = index (T, '|');
  if (k > 0)
    base = T(1:k-1);
    fmt = T(k+1:end);
  endif

endfunction
