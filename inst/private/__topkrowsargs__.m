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
## @deftypefn {private} {[@var{col}, @var{direction}, @var{MP}, @var{errmsg}] =} __topkrowsargs__ (@var{K}, @var{ncols}, @var{args})
##
## Validate the arguments of the @code{topkrows} methods.
##
## This is the shared argument handling of the @code{topkrows} methods of the
## @code{categorical}, @code{datetime}, and @code{duration} classes.  @var{K} is
## the number of rows asked for, @var{ncols} the number of columns of the array
## being ranked, and @var{args} the cell array of the caller's optional input
## arguments.
##
## @var{col} is the list of sort columns, empty when none was given, and
## @var{direction} is either a character vector or a cell array of them.  The
## default direction is @qcode{'descend'}: these are the top rows, not the first
## ones, which is where @code{topkrows} parts company with @code{sortrows}.
##
## @var{MP} is the missing placement, defaulting to @qcode{'last'}.  MATLAB has
## no such option in @code{topkrows} and always ranks as @qcode{'last'} does, so
## the default reproduces it and naming the option is an Octave extension.
##
## No error is raised here.  Instead @var{errmsg} returns the message body,
## which is empty on success, and the caller is expected to raise the error
## under its own class and method name.
##
## @end deftypefn

function [col, direction, MP, errmsg] = __topkrowsargs__ (K, ncols, args)

  col = [];
  direction = 'descend';
  MP = 'last';
  errmsg = '';

  ## Check K
  if (! (isnumeric (K) && isscalar (K) && isreal (K) && isfinite (K) &&
         fix (K) == K && K >= 0))
    errmsg = "K must be a nonnegative integer scalar.";
    return;
  endif

  if (numel (args) > 0)
    [args{:}] = convertStringsToChars (args{:});
  endif

  ## Take 'MissingPlacement' out before the positional arguments are read, so
  ## that naming it is reported as itself rather than as one argument too many.
  mid = find (cellfun (@(x) ischar (x) && ...
                       strcmpi (x, 'MissingPlacement'), args));
  if (! isempty (mid))
    if (mid(1) == numel (args))
      errmsg = "'MissingPlacement' requires a value.";
      return;
    endif
    MP = args{mid(1)+1};
    if (! (ischar (MP) && isrow (MP)) ||
        ! any (strcmpi (MP, {'auto', 'first', 'last'})))
      errmsg = "invalid value for 'MissingPlacement'.";
      return;
    endif
    args([mid(1), mid(1)+1]) = [];
  endif

  if (numel (args) > 2)
    errmsg = "too many input arguments.";
    return;
  endif

  ## Split the remaining arguments into COL and DIRECTION
  for i = 1:numel (args)
    if (isnumeric (args{i}))
      col = args{i};
    elseif (ischar (args{i}) || iscellstr (args{i}))
      direction = args{i};
    else
      errmsg = strcat ("optional arguments must be a column list or a", ...
                       " sorting direction.");
      return;
    endif
  endfor

  ## Unlike 'sortrows', COL must be positive: a negative column does not select
  ## a direction here, as MATLAB also refuses it.
  if (! isempty (col))
    if (! (isvector (col) && isreal (col) && all (isfinite (col)) &&
           all (fix (col) == col) && all (col > 0) && all (col <= ncols)))
      errmsg = strcat ("COL must contain positive integers indexing", ...
                       " existing columns in A.");
      return;
    endif
  endif

  if (ischar (direction))
    dirlist = {direction};
  else
    dirlist = direction;
  endif
  if (! all (cellfun (@(d) ischar (d) && ...
                      any (strcmpi (d, {'ascend', 'descend'})), dirlist)))
    errmsg = "DIRECTION must be 'ascend' or 'descend'.";
    return;
  endif

endfunction
