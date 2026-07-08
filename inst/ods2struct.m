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
## @deftypefn {datatypes} {@var{s} =} ods2struct (@var{filename})
##
## Read every sheet of an OpenDocument spreadsheet into a scalar structure.
##
## @code{@var{s} = ods2struct (@var{filename})} reads each data sheet of the
## OpenDocument spreadsheet named by @var{filename} into a @code{table} and
## returns a scalar structure with one field per sheet, in sheet order.  Both
## the compressed @qcode{.ods} and the flat @qcode{.fods} formats are read.
## Each sheet is reconstructed exactly as by @code{ods2table}; it is the inverse
## of @code{struct2ods}.
##
## A sheet name that is not a valid structure field name is canonicalised with
## @code{matlab.lang.makeValidName} (and made unique if two sheet names collide).
## Whenever the field name differs from the sheet name, the original sheet name
## is stored on that field's table as the @qcode{'ActualSheetName'} custom
## property, so a subsequent @code{struct2ods} restores the exact sheet name.
##
## @seealso{struct2ods, ods2table, table2ods, readtable}
## @end deftypefn

function s = ods2struct (filename)

  if (nargin != 1)
    print_usage ();
  endif
  if (! (ischar (filename) || iscellstr (filename) || isa (filename, 'string')))
    error ("ods2struct: FILENAME must be a character vector, cellstr, or string.");
  endif
  file = char (cellstr (filename));

  ## Enumerate the data sheet names (the first output doubles as an error probe).
  [data, ~, ~, names] = __ods2table__ (file);
  if (ischar (data))
    error ("ods2struct: %s", data);
  endif

  s = struct ();
  usedFields = {};
  for k = 1:numel (names)
    sn = names{k};
    R = ods2table (file, 'Sheet', sn);
    fn = matlab.lang.makeValidName (sn);
    ## Make the field name unique if canonicalisation collided with an earlier
    ## one; the ActualSheetName property below preserves the true sheet name.
    base = fn;
    j = 1;
    while (any (strcmp (fn, usedFields)))
      fn = sprintf ('%s_%d', base, j);
      j += 1;
    endwhile
    usedFields{end+1} = fn;
    ## Stash the original sheet name whenever the field name had to change.
    if (! strcmp (fn, sn))
      R = addprop (R, 'ActualSheetName', 'table');
      R.Properties.CustomProperties.ActualSheetName = sn;
    endif
    s.(fn) = R;
  endfor

endfunction

%!test  # round-trip a multi-sheet workbook written by struct2ods
%! s.alpha = table ([1; 2; 3], {'a'; 'b'; 'c'}, 'VariableNames', {'x', 'y'});
%! s.beta = table ([10.5; 20.5], 'VariableNames', {'v'});
%! fn = [tempname() '.ods'];
%! unwind_protect
%!   struct2ods (fn, s);
%!   r = ods2struct (fn);
%!   assert_equal (fieldnames (r), {'alpha'; 'beta'});
%!   assert_equal (r.alpha.x, [1; 2; 3]);
%!   assert_equal (r.beta.v, [10.5; 20.5]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!test  # an odd sheet name is canonicalised and stashed in ActualSheetName
%! T = table ([7; 8], 'VariableNames', {'v'});
%! T = addprop (T, 'ActualSheetName', 'table');
%! T.Properties.CustomProperties.ActualSheetName = 'My Sheet!';
%! s.only = T;
%! fn = [tempname() '.fods'];
%! unwind_protect
%!   struct2ods (fn, s);
%!   r = ods2struct (fn);
%!   f = fieldnames (r);
%!   assert_equal (numel (f), 1);
%!   assert_equal (isvarname (f{1}), true);
%!   assert_equal (r.(f{1}).Properties.CustomProperties.ActualSheetName, ...
%!                 'My Sheet!');
%!   assert_equal (r.(f{1}).v, [7; 8]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!error <ods2struct: FILENAME must be a character vector, cellstr, or string.> ...
%! ods2struct (42)
%!error <ods2struct: cannot read> ...
%! ods2struct ([tempname() '.ods'])
