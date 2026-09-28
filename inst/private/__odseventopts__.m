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
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {datatypes} {[@var{args}, @var{errmsg}] =} __odseventopts__ (@var{x}, @var{varNames})
##
## The @code{eventtable} constructor options one cross-reference carries.
##
## @var{x} is one element of the struct array @code{__odscrossrefs__} returns.
## @var{args} is a cell of Name-Value pairs naming whichever of the three
## variable designations the reference carries, and is empty when it carries
## none.
##
## @var{varNames} are the variables of the sheet read as the event table.
## @var{errmsg} is the body of a complaint for the caller to raise under its
## own name when a designation names none of them, and is empty otherwise.
##
## @end deftypefn

function [args, errmsg] = __odseventopts__ (x, varNames)

  args = {};
  if (! isempty (x.labels))
    args = [args, {'EventLabelsVariable', x.labels}];
  endif
  if (! isempty (x.lengths))
    args = [args, {'EventLengthsVariable', x.lengths}];
  endif
  if (! isempty (x.ends))
    args = [args, {'EventEndsVariable', x.ends}];
  endif

  errmsg = '';
  named = args(2:2:end);
  bad = find (! ismember (named, varNames), 1);
  if (! isempty (bad))
    errmsg = sprintf (strcat ("sheet '%s' is named as the event table of", ...
                              " sheet '%s' but has no variable '%s'."), ...
                      x.to, x.from, named{bad});
  endif

endfunction
