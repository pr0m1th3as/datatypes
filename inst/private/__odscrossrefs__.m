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
## @deftypefn  {datatypes} {@var{x} =} __odscrossrefs__ (@var{preamble})
## @deftypefnx {datatypes} {[@var{x}, @var{errmsg}] =} __odscrossrefs__ (@var{preamble}, @var{names})
##
## The event cross-references a workbook's metadata preamble carries.
##
## @var{preamble} is the block of rows above the first @qcode{## Sheet:} marker
## of the hidden metadata sheet.  Each line reading
## @qcode{## Events crossref:} names, in the cells after it, the sheet holding
## a timetable, the sheet holding that timetable's event table, and the event
## table's three variable designations, any of which may be blank.
##
## @var{x} is a struct array with fields @qcode{from}, @qcode{to},
## @qcode{labels}, @qcode{lengths} and @qcode{ends}, empty when the preamble
## carries no such line.
##
## Given the workbook's data sheet names @var{names}, @var{errmsg} is the body
## of a complaint for the caller to raise under its own name when a reference
## starts from a sheet the file does not have, or names as an event table a
## sheet that carries one of its own; it is empty otherwise.
##
## Each value goes in a cell of its own rather than into one string because a
## sheet name may contain spaces.  A line whose keyword is not recognised is
## skipped, so that a file written by a later version is read rather than
## refused.
##
## @end deftypefn

function [x, errmsg] = __odscrossrefs__ (preamble, names = {})

  x = struct ('from', {}, 'to', {}, 'labels', {}, 'lengths', {}, 'ends', {});
  errmsg = '';
  if (isempty (preamble) || columns (preamble) < 3)
    return;
  endif
  key = '## Events crossref:';
  for r = 1:rows (preamble)
    if (! (ischar (preamble{r,1}) && strcmp (strtrim (preamble{r,1}), key)))
      continue;
    endif
    a = cell_text (preamble, r, 2);
    b = cell_text (preamble, r, 3);
    if (isempty (a) || isempty (b))
      continue;
    endif
    x(end+1) = struct ('from', a, 'to', b, ...
                       'labels', cell_text (preamble, r, 4), ...
                       'lengths', cell_text (preamble, r, 5), ...
                       'ends', cell_text (preamble, r, 6));
  endfor

  if (nargin < 2)
    return;
  endif
  for i = 1:numel (x)
    if (! any (strcmp (x(i).from, names)))
      errmsg = sprintf (strcat ("sheet '%s' is said to have its events on", ...
                                " sheet '%s', but the file has no sheet", ...
                                " '%s'."), x(i).from, x(i).to, x(i).from);
      return;
    endif
    if (any (strcmp (x(i).to, {x.from})))
      errmsg = sprintf (strcat ("sheet '%s' is named as an event table and", ...
                                " carries one of its own; an event table", ...
                                " cannot carry an event table."), x(i).to);
      return;
    endif
  endfor

endfunction

## One cell of the preamble as a character vector, empty when the column is
## not there at all or holds anything but text.
function s = cell_text (preamble, r, c)
  s = '';
  if (columns (preamble) >= c && ischar (preamble{r,c}))
    s = preamble{r,c};
  endif
endfunction
