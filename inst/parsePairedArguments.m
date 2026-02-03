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
## this program; if not, see <http://www.gnu.org/licens

## -*- texinfo -*-
## @deftypefn  {datatypes} {[@var{optarg_1}, @dots{}, @var{optarg_N}] =} @
## parsePairedArguments (@var{optarg_names}, @var{default_values}, @var{arg_list})
## @deftypefnx {datatypes} {[@var{optarg_1}, @dots{}, @var{optarg_N}, @var{rem_args}] =} @
## parsePairedArguments (@var{optarg_names}, @var{default_values}, @var{arg_list})
##
## Parse optional paired arguments from variable argument list.
##
## @code{parsePairedArguments} parses the optional paired arguments specified
## by @var{optarg_names} from the variable input argument list, @var{arg_list}.
## Any @var{optarg_names} that are not found in @var{arg_list} are returned with
## their default value specified by @var{default_values}, which must be a cell
## array with the same number of elements as @var{optarg_names}.
##
## @var{optarg_names} must be a cell array of character vectors or a string
## array with the same number of elements as the number of output arguments
## specified as @code{[@var{optarg_1}, @dots{}, @var{optarg_N}]}, while an extra
## output argument, @var{rem_args}, may be specified for the remaining input
## arguments in @var{arg_list} that were not specified by @var{optarg_names}.
##
## Each property name specified by @var{optarg_names} is case insensitive.
##
## If you wish to specify optional arguments using the assignment syntax as in
## @code{@var{Name_1} = @var{Value_1}, @dots{}, @var{Name_N} = @var{Value_N}},
## then you should adapt the code from the following example prior to calling
## @code{parsePairedArguments}.
##
## @example
## ## Declare optional property Names and their default Values
## optNames = @{'A', 'B', 'C'@};
## dfValues = @{1, 2, 3@};
##
## ## Expand specified properties using assignment syntax into Name-Value pairs
## newPairs = @{@};
## for ii = numel (varargin):-1:1
##   tmpi = strsplit (inputname (ii+1, false));
##   if (numel (tmpi) > 1)
##     idx = find (strcmpi (tmpi@{1@}, optNames));
##     if (strcmp (tmpi@{2@}, '=') && any (idx))
##       ## Append Name, Value paired argument
##       newPairs = [newPairs, optNames@{idx@}, varargin(ii)];
##       varargin(ii) = [];
##     endif
##   endif
## endfor
## args = [varargin, newPairs];
##
## ## Parse optional Name-Value paired arguments
## [varNames, rowNames, dimNames, args] = parsePairedArguments ...
##                                        (optNames, dfValues, args);
## @end example
##
## @end deftypefn

function [varargout] = parsePairedArguments (optNames, dfValues, args)

  ## Input validation
  if (nargin != 3)
    error ("parsePairedArguments: invalid number of input arguments.");
  endif
  if (isa (optNames, 'string'))
    optNames = cellstr (optNames);
  endif
  if (! iscellstr (optNames))
    error (strcat ("parsePairedArguments: OPTARG_NAMES must be a cell", ...
                   " array of character vectors or a string array."));
  endif
  if (! iscell (dfValues))
    error ("parsePairedArguments: DFVALUES must be a cell array.");
  endif
  optN = numel (optNames);
  if (optN != numel (dfValues))
    error ("parsePairedArguments: OPTARG_NAMES mismatches DFVALUES.");
  endif
  if (nargout < optN || nargout > optN + 1)
    error ("parsePairedArguments: inconsistent number of output arguments.");
  endif
  if (! iscell (args))
    error ("parsePairedArguments: ARG_LIST must be a cell array.");
  endif

  ## Search through all input arguments for Name/Value pairs
  foundNames = [];
  nargs = numel (args);
  for ii = nargs-1:-1:1
    tmp_arg = args{ii};
    if (isstring (tmp_arg))
      tmp_arg = char (tmp_arg);
    endif
    if (ischar (tmp_arg))
      idx = strcmpi (tmp_arg, optNames);
      if (any (idx))
        idx = find (idx);
        varargout{idx} = args{ii+1};
        foundNames = [foundNames, idx];
        args(ii:ii+1) = [];
      endif
    endif
  endfor

  ## Find optNames that were not in args and add defaults
  allNames = 1:numel (optNames);
  notfound = ! ismember (allNames, foundNames);
  defNames = optNames(notfound);
  for ii = 1:numel (defNames)
    idx = find (strcmpi (defNames{ii}, optNames));
    varargout{idx} = dfValues{idx};
  endfor

  ## Append remaining input arguments in varargout
  if (nargout > optN)
    idx = optN + 1;
    varargout{idx} = args(:);
  endif

endfunction

%!shared optNames, dfValues
%! optNames = {'A', 'B', 'C'};
%! dfValues = {{3}, [1, 2], 'text'};
%!test
%! [a, b, c] = parsePairedArguments (optNames, dfValues, {'A', 5});
%! assert (a, 5);
%! assert (b, [1, 2]);
%! assert (c, 'text');
%!test
%! [a, b, c, args] = parsePairedArguments (optNames, dfValues, {4, 'D', 5});
%! assert (numel (args), 3);
%! assert (args{2}, 'D');
%! assert (a, {3});
%! assert (b, [1, 2]);
%! assert (c, 'text');
%!test
%! [a, b, c] = parsePairedArguments (optNames, dfValues, {'A', 5, 'b', 1, 'C', 'test'});
%! assert (a, 5);
%! assert (b, 1);
%! assert (c, 'test');
%!test
%! [a, b, c, args] = parsePairedArguments (optNames, dfValues, {1});
%! assert (args, {1});
%! assert (a, {3});
%! assert (b, [1, 2]);
%! assert (c, 'text');

%!error <parsePairedArguments: invalid number of input arguments.> ...
%! parsePairedArguments (optNames, dfValues)
%!error <parsePairedArguments: OPTARG_NAMES must be a cell array of character vectors or a string array.> ...
%! parsePairedArguments ([1, 2, 3], dfValues, {1})
%!error <parsePairedArguments: DFVALUES must be a cell array.> ...
%! parsePairedArguments (optNames, [1, 2, 3], {1})
%!error <parsePairedArguments: OPTARG_NAMES mismatches DFVALUES.> ...
%! parsePairedArguments (optNames(1:2), dfValues, {1})
%!error <parsePairedArguments: inconsistent number of output arguments.> ...
%! [a, b] = parsePairedArguments (optNames, dfValues, {1})
%!error <parsePairedArguments: inconsistent number of output arguments.> ...
%! [a, b, c, d, e] = parsePairedArguments (optNames, dfValues, {1})
%!error <parsePairedArguments: ARG_LIST must be a cell array.> ...
%! [a, b, c, e] = parsePairedArguments (optNames, dfValues, 1)
