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
## @deftypefn {datatypes} {@var{s} =} __mergesheet__ (@var{s}, @var{obj}, @var{sheet}, @var{writeMode})
##
## Merge one tabular object into the struct of sheets a workbook was read into.
##
## @var{s} is the struct @code{ods2struct} returned, @var{obj} the object being
## written, @var{sheet} the sheet name it is to occupy, and @var{writeMode}
## either @qcode{'append'} or a replacing mode.  It is a private helper of
## @code{table.table2ods} and @code{timetable.timetable2ods}, which reach it
## when the file already exists and its other sheets have to survive.
##
## It lives here rather than on @code{tabular} because a dot access to
## @qcode{Properties} inside a method does not go through @code{subsref}, which
## is what synthesises that property.
##
## @end deftypefn

function s = __mergesheet__ (s, T, sheet, writeMode)
  ## Find the field whose sheet name matches, which is its
  ## 'ActualSheetName' where it has one and its field name otherwise.
  fields = fieldnames (s);
  targetField = '';
  for i = 1:numel (fields)
    fsheet = fields{i};
    cp = s.(fields{i}).Properties.CustomProperties;
    if (isfield (cp, 'ActualSheetName') && ! isempty (cp.ActualSheetName))
      fsheet = cp.ActualSheetName;
    endif
    if (strcmp (fsheet, sheet))
      targetField = fields{i};
      break;
    endif
  endfor
  ## Select mode
  if (strcmp (writeMode, 'append') && ! isempty (targetField))
    ## Append the rows; vertcat errors if the variables do not agree.
    combined = [s.(targetField); T];
    s.(targetField) = copy_actual_sheet_name (combined, ...
                                                      s.(targetField));
  elseif (! isempty (targetField))
    ## Replace the sheet, keeping its resolved name.
    s.(targetField) = copy_actual_sheet_name (T, s.(targetField));
  else
    ## A new sheet: canonicalise SHEET to a unique field name and stash the
    ## original name when it had to change.
    fn = matlab.lang.makeValidName (sheet);
    base = fn;
    j = 1;
    while (isfield (s, fn))
      fn = sprintf ("%s_%d", base, j);
      j += 1;
    endwhile
    if (! strcmp (fn, sheet))
      T = addprop (T, 'ActualSheetName', 'table');
      T.Properties.CustomProperties.ActualSheetName = sheet;
    endif
    s.(fn) = T;
  endif
endfunction

## Copy the 'ActualSheetName' custom property from SRC onto T, where SRC
## carries one.
function T = copy_actual_sheet_name (T, src)
  cp = src.Properties.CustomProperties;
  if (isfield (cp, 'ActualSheetName') && ! isempty (cp.ActualSheetName))
    tcp = T.Properties.CustomProperties;
    if (! isfield (tcp, 'ActualSheetName'))
      T = addprop (T, 'ActualSheetName', 'table');
    endif
    T.Properties.CustomProperties.ActualSheetName = cp.ActualSheetName;
  endif
endfunction