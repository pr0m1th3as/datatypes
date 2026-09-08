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
## @deftypefn {datatypes} {@var{tt} =} __odsattach__ (@var{tt}, @var{file}, @var{x}, @var{names}, @var{caller})
##
## Attach the event table one cross-reference names to the timetable it names.
##
## @var{x} is one element of the struct array @code{__odscrossrefs__} returns,
## @var{names} the workbook's data sheet names, and @var{caller} the name the
## complaint is raised under.  The referenced sheet is read, rebuilt as an
## @code{eventtable} under the three designations the reference carries, and
## assigned to @var{tt}.
##
## The check is per attachment and not per sheet: one sheet may be referenced
## by several timetables, and a sheet valid for one of them can be invalid for
## another.
##
## @end deftypefn

function tt = __odsattach__ (tt, file, x, names, caller)

  if (! any (strcmp (x.to, names)))
    error (strcat ("%s: the event table of sheet '%s' is said to be on", ...
                   " sheet '%s', which the file does not have."), ...
           caller, x.from, x.to);
  endif
  [etbl, rtn] = ods2table (file, 'Sheet', x.to);
  if (isempty (rtn))
    error (strcat ("%s: sheet '%s' is named as the event table of sheet", ...
                   " '%s' but holds no row times."), caller, x.to, x.from);
  endif
  ev = table2timetable (etbl, 'RowTimes', rtn);
  evClass = class (ev.Properties.RowTimes);
  ttClass = class (tt.Properties.RowTimes);
  if (! strcmp (evClass, ttClass))
    error (strcat ("%s: the event table on sheet '%s' has %s row times", ...
                   " where sheet '%s' has %s ones."), caller, x.to, ...
           evClass, x.from, ttClass);
  endif
  args = __odseventopts__ (x);
  tt.Properties.Events = eventtable (ev, args{:});

endfunction
