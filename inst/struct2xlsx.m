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
## @deftypefn {datatypes} {} struct2xlsx (@var{filename}, @var{s})
##
## Write a scalar structure of tables to a multi-sheet Excel workbook.
##
## @code{struct2xlsx (@var{filename}, @var{s})} writes each field of the scalar
## structure @var{s} to its own sheet in the Office Open XML workbook named by
## @var{filename} (@qcode{.xlsx} or @qcode{.xlsm}).  Every field must hold a
## @code{table}; the field name becomes the sheet name.
##
## This is the Excel counterpart of @code{struct2ods}.  Like @code{writetable},
## it writes the MATLAB-interoperable format (a variable-name header row followed
## by the data, with no hidden type metadata); read it back with
## @code{xlsx2struct}.  A field whose table carries an @qcode{'ActualSheetName'}
## custom property uses that value as the sheet name instead of the field name.
## Sheet names must be non-empty, at most 31 characters, and must not contain any
## of the characters @qcode{[ ] * ? : / @backslashchar{}}; the resolved names
## must be unique.
##
## @seealso{xlsx2struct, struct2ods, writetable, readtable}
## @end deftypefn

function struct2xlsx (filename, s)

  if (nargin != 2)
    print_usage ();
  endif
  if (! ((ischar (filename) && isvector (filename)) ...
         || iscellstr (filename) || isa (filename, 'string')))
    error (strcat ("struct2xlsx: FILENAME must be a character vector,", ...
                   " cellstr, or string."));
  endif
  file = char (cellstr (filename));
  [~, ~, ext] = fileparts (file);
  if (! any (strcmpi (ext, {'.xlsx', '.xlsm'})))
    error ("struct2xlsx: FILENAME must have a '.xlsx' or '.xlsm' extension.");
  endif

  if (! (isstruct (s) && isscalar (s)))
    error ("struct2xlsx: S must be a scalar structure.");
  endif
  fields = fieldnames (s);
  if (isempty (fields))
    error ("struct2xlsx: S must have at least one field.");
  endif

  K = numel (fields);
  names = cell (1, K);
  datas = cell (1, K);
  vtypes = cell (1, K);
  headers = cell (1, K);
  for k = 1:K
    T = s.(fields{k});
    if (! isa (T, 'table'))
      error ("struct2xlsx: field '%s' is not a table.", fields{k});
    endif
    sheetName = fields{k};
    cp = T.Properties.CustomProperties;
    if (isstruct (cp) && isfield (cp, 'ActualSheetName') ...
        && ! isempty (cp.ActualSheetName))
      sheetName = char (cp.ActualSheetName);
    endif
    if (! (ischar (sheetName) && isrow (sheetName) && ! isempty (sheetName)))
      error (strcat ("struct2xlsx: sheet name for field '%s' must be a", ...
                     " non-empty character vector."), fields{k});
    endif
    if (numel (sheetName) > 31)
      error (strcat ("struct2xlsx: sheet name '%s' exceeds the 31-character", ...
                     " Excel limit."), sheetName);
    endif
    if (any (ismember (sheetName, '[]*?:/\')))
      error (strcat ("struct2xlsx: sheet name '%s' contains an invalid", ...
                     " character ([ ] * ? : / \\)."), sheetName);
    endif
    if (any (strcmp (sheetName, names(1:k-1))))
      error ("struct2xlsx: duplicate sheet name '%s'.", sheetName);
    endif
    names{k} = sheetName;
    [headers{k}, datas{k}, vtypes{k}] = __interop_parts__ (T, 'struct2xlsx');
  endfor

  opts = struct ();
  opts.sheets = struct ('name', names, 'data', datas, 'vtype', vtypes, ...
                        'header', headers);
  opts.macro = strcmpi (ext, '.xlsm');
  msg = __table2xlsx__ (file, {}, {}, opts);
  if (! isequal (msg, 0))
    error ("struct2xlsx: %s", msg);
  endif

endfunction

%!demo
%! ## `struct2xlsx` is the Excel counterpart of `struct2ods`: each field of a
%! ## scalar struct of tables is written as its own worksheet in an `.xlsx` file.
%!
%! wb.Patients = table ({'Li'; 'Diaz'}, [38; 40], 'VariableNames', {'Name', 'Age'});
%! wb.Visits = table ([1; 2; 3], 'VariableNames', {'Visit'});
%! filename = fullfile (tempdir (), 'clinic.xlsx');
%! struct2xlsx (filename, wb);
%!
%! ## Read it back with `xlsx2struct` to recover the same field names.
%! fieldnames (xlsx2struct (filename))
%!
%! delete (filename);

%!test  # round-trip a two-table workbook through xlsx2struct
%! s.alpha = table ([1; 2; 3], {'a'; 'b'; 'c'}, 'VariableNames', {'x', 'y'});
%! s.beta = table ([10.5; 20.5], 'VariableNames', {'v'});
%! fn = [tempname() '.xlsx'];
%! unwind_protect
%!   struct2xlsx (fn, s);
%!   r = xlsx2struct (fn);
%!   assert_equal (fieldnames (r), {'alpha'; 'beta'});
%!   assert_equal (r.alpha.x, [1; 2; 3]);
%!   assert_equal (r.alpha.y, {'a'; 'b'; 'c'});
%!   assert_equal (r.beta.v, [10.5; 20.5]);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!test  # native datetime/duration round-trip per sheet
%! s.one = table (datetime (2024, 1, [1; 2]), 'VariableNames', {'d'});
%! s.two = table (seconds ([30; 90]), 'VariableNames', {'t'});
%! fn = [tempname() '.xlsx'];
%! unwind_protect
%!   struct2xlsx (fn, s);
%!   r = xlsx2struct (fn);
%!   assert_equal (class (r.one.d), 'datetime');
%!   assert_equal (isequaln (datevec (r.one.d), datevec (s.one.d)), true);
%!   assert_equal (class (r.two.t), 'duration');
%!   assert_equal (isequaln (seconds (r.two.t), [30; 90]), true);
%! unwind_protect_cleanup
%!   delete (fn);
%! end_unwind_protect

%!error <struct2xlsx: FILENAME must have a '.xlsx' or '.xlsm' extension.> ...
%! struct2xlsx ('bad.ods', struct ('a', table (1)))
%!error <struct2xlsx: S must be a scalar structure.> ...
%! struct2xlsx ([tempname() '.xlsx'], struct ('a', {table(1), table(2)}))
%!error <struct2xlsx: field 'b' is not a table.> ...
%! struct2xlsx ([tempname() '.xlsx'], struct ('a', table (1), 'b', 5))
