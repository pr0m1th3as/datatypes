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
## @deftypefn {datatypes} {} struct2ods (@var{filename}, @var{s})
##
## Write a scalar structure of tables to a multi-sheet OpenDocument spreadsheet.
##
## @code{struct2ods (@var{filename}, @var{s})} writes each field of the scalar
## structure @var{s} to its own sheet in the OpenDocument spreadsheet named by
## @var{filename}.  Every field of @var{s} must hold a @code{table}; the field
## name becomes the sheet name.  Both the compressed @qcode{.ods} and the flat
## @qcode{.fods} formats are supported, selected by the file extension.
##
## Full type fidelity is preserved through a hidden, sectioned
## @qcode{__datatypes_meta__} sheet, exactly as for the single-table
## @code{table2ods} method; the workbook round-trips through @code{ods2struct}.
##
## A field whose table carries an @qcode{'ActualSheetName'} custom property
## (see @code{addprop}) uses that value as the sheet name instead of the field
## name, which lets a sheet name that is not a valid identifier (for example
## @qcode{'Sales 2024'}) round-trip.  Sheet names must be non-empty and must not
## contain any of the characters @qcode{[ ] * ? : / @backslashchar{}}, and the
## resolved names must be unique.
##
## @seealso{ods2struct, table2ods, ods2table, writetable}
## @end deftypefn

function struct2ods (filename, s)

  if (nargin != 2)
    print_usage ();
  endif
  if (! ((ischar (filename) && isvector (filename)) ...
         || iscellstr (filename) || isa (filename, 'string')))
    error (strcat ("struct2ods: FILENAME must be a character vector,", ...
                   " cellstr, or string."));
  endif
  file = char (cellstr (filename));
  [~, ~, ext] = fileparts (file);
  if (strcmpi (ext, '.fods'))
    is_flat = true;
  elseif (strcmpi (ext, '.ods'))
    is_flat = false;
  else
    error (strcat ("struct2ods: FILENAME must have a '.ods' or '.fods'", ...
                   " extension."));
  endif

  if (! (isstruct (s) && isscalar (s)))
    error ("struct2ods: S must be a scalar structure.");
  endif
  fields = fieldnames (s);
  if (isempty (fields))
    error ("struct2ods: S must have at least one field.");
  endif

  K = numel (fields);
  names = cell (1, K);
  datas = cell (1, K);
  vtypes = cell (1, K);
  metablocks = cell (1, K);
  for k = 1:K
    T = s.(fields{k});
    if (! isa (T, 'table'))
      error ("struct2ods: field '%s' is not a table.", fields{k});
    endif
    ## Resolve the sheet name: an 'ActualSheetName' custom property wins over
    ## the field name, so non-identifier sheet names can round-trip.
    sheetName = fields{k};
    cp = T.Properties.CustomProperties;
    if (isstruct (cp) && isfield (cp, 'ActualSheetName') ...
        && ! isempty (cp.ActualSheetName))
      sheetName = char (cp.ActualSheetName);
    endif
    if (! (ischar (sheetName) && isrow (sheetName) && ! isempty (sheetName)))
      error (strcat ("struct2ods: sheet name for field '%s' must be a", ...
                     " non-empty character vector."), fields{k});
    endif
    if (any (ismember (sheetName, '[]*?:/\')))
      error (strcat ("struct2ods: sheet name '%s' contains an invalid", ...
                     " character ([ ] * ? : / \\)."), sheetName);
    endif
    if (any (strcmp (sheetName, names(1:k-1))))
      error ("struct2ods: duplicate sheet name '%s'.", sheetName);
    endif
    names{k} = sheetName;
    [datas{k}, vtypes{k}, metablocks{k}] = __ods_parts__ (T, 'struct2ods');
  endfor

  ## Assemble the sectioned metadata grid: each table's metadata block preceded
  ## by a "## Sheet: <name>" marker row, all padded to a common width.
  sections = cell (1, K);
  maxcols = 0;
  for k = 1:K
    mb = metablocks{k};
    marker = [{['## Sheet: ' names{k}]}, repmat({''}, 1, max (0, columns (mb) - 1))];
    sections{k} = [marker; mb];
    maxcols = max (maxcols, columns (sections{k}));
  endfor
  metagrid = cell (0, maxcols);
  for k = 1:K
    sec = sections{k};
    if (columns (sec) < maxcols)
      sec = [sec, repmat({''}, rows (sec), maxcols - columns (sec))];
    endif
    metagrid = [metagrid; sec];
  endfor

  opts = struct ();
  opts.sheets = struct ('name', names, 'data', datas, 'vtype', vtypes);
  opts.meta = metagrid;
  msg = __table2ods__ (file, {}, {}, {}, is_flat, opts);
  if (! isequal (msg, 0))
    error ("struct2ods: %s", msg);
  endif

endfunction

%!demo
%! ## `struct2ods` writes a whole workbook at once: each field of a scalar struct
%! ## of tables becomes its own sheet, and the field name becomes the sheet name.
%!
%! wb.Patients = table ({'Li'; 'Diaz'}, [38; 40], 'VariableNames', {'Name', 'Age'});
%! wb.Visits = table ([1; 2; 3], 'VariableNames', {'Visit'});
%! filename = fullfile (tempdir (), 'clinic.ods');
%! struct2ods (filename, wb);
%!
%! ## The two fields are now two sheets, recoverable with `ods2struct`.
%! fieldnames (ods2struct (filename))
%!
%! delete (filename);

%!test  # round-trip a two-table workbook through ods2struct
%! s.alpha = table ([1; 2; 3], {'a'; 'b'; 'c'}, 'VariableNames', {'x', 'y'});
%! s.beta = table ([10.5; 20.5], 'VariableNames', {'v'});
%! fn = [tempname() '.ods'];
%! unwind_protect
%!   struct2ods (fn, s);
%!   r = ods2struct (fn);
%!   assert_equal (sort (fieldnames (r)), {'alpha'; 'beta'});
%!   assert_equal (r.alpha.x, [1; 2; 3]);
%!   assert_equal (r.alpha.y, {'a'; 'b'; 'c'});
%!   assert_equal (r.beta.v, [10.5; 20.5]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!test  # native types (datetime, duration, int) round-trip per sheet
%! s.one = table (datetime (2024, 1, [1; 2]), int32 ([5; 6]), ...
%!                'VariableNames', {'d', 'n'});
%! s.two = table (seconds ([30; 90; 120]), 'VariableNames', {'t'});
%! fn = [tempname() '.fods'];
%! unwind_protect
%!   struct2ods (fn, s);
%!   r = ods2struct (fn);
%!   assert_equal (class (r.one.d), 'datetime');
%!   assert_equal (class (r.one.n), 'int32');
%!   assert_equal (r.one.n, int32 ([5; 6]));
%!   assert_equal (class (r.two.t), 'duration');
%!   assert_equal (isequaln (seconds (r.two.t), [30; 90; 120]), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!test  # a non-identifier sheet name round-trips via ActualSheetName
%! T = table ([1; 2], 'VariableNames', {'v'});
%! T = addprop (T, 'ActualSheetName', 'table');
%! T.Properties.CustomProperties.ActualSheetName = 'Sales 2024';
%! s.sheet1 = T;
%! fn = [tempname() '.ods'];
%! unwind_protect
%!   struct2ods (fn, s);
%!   names = __ods2table__ (fn);  # smoke: file is readable
%!   r = ods2struct (fn);
%!   fn2 = fieldnames (r);
%!   assert_equal (numel (fn2), 1);
%!   cp = r.(fn2{1}).Properties.CustomProperties;
%!   assert_equal (cp.ActualSheetName, 'Sales 2024');
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!error <struct2ods: FILENAME must have a '.ods' or '.fods' extension.> ...
%! struct2ods ('bad.txt', struct ('a', table (1)))
%!error <struct2ods: S must be a scalar structure.> ...
%! struct2ods ([tempname() '.ods'], struct ('a', {table(1), table(2)}))
%!error <struct2ods: field 'b' is not a table.> ...
%! struct2ods ([tempname() '.ods'], struct ('a', table (1), 'b', 5))
%!error <struct2ods: S must have at least one field.> ...
%! struct2ods ([tempname() '.ods'], struct ())
