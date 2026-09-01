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
## @deftp {datatypes.tabular} {} CustomProperties
##
## The custom metadata of a @code{table} or a @code{timetable}.
##
## It holds the properties added with @code{addprop} and removed with
## @code{rmprop}, and is reached as
## @qcode{@var{tbl}.Properties.CustomProperties.@var{name}}.  A single class
## serves both tabular classes, as in MATLAB.  It cannot be constructed
## directly.
##
## @end deftp
classdef CustomProperties

  properties (Access = private)
    Values = struct ()
    Types = struct ()
  endproperties

  methods (Access = {?datatypes.tabular.TableProperties})

    function this = CustomProperties (s, types)
      if (nargin > 0 && isstruct (s))
        this.Values = s;
      endif
      if (nargin > 1 && isstruct (types))
        this.Types = types;
      endif
      ## Every property carries its type under its own name.  A set that does
      ## not match means a table wrote one container and not the other, and
      ## every read of 'Properties' passes through here, so it is caught at
      ## the first look rather than answering with the wrong type.
      if (! isequal (sort (fieldnames (this.Values)), ...
                     sort (fieldnames (this.Types))))
        error (strcat ("datatypes.tabular.CustomProperties: the custom", ...
                       " properties and their types do not match."));
      endif
    endfunction

  endmethods

  methods (Access = {?table})

    ## Return the stored values and the type of each, so that a table can
    ## take the whole store in one assignment.  Both are structs keyed by the
    ## property names.
    function [vals, types] = unpack (this)
      vals = this.Values;
      types = this.Types;
    endfunction

  endmethods

  methods

    ## -*- texinfo -*-
    ## @deftypefn {datatypes.tabular.CustomProperties} {@var{names} =} fieldnames (@var{obj})
    ##
    ## Return the names of the custom properties that are set.
    ##
    ## @end deftypefn
    function names = fieldnames (this)
      names = fieldnames (this.Values);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datatypes.tabular.CustomProperties} {@var{names} =} properties (@var{obj})
    ##
    ## Return the names of the custom properties that are set.
    ##
    ## The names are data rather than properties the class declares, so
    ## @code{properties} answers from the set that is stored, exactly as
    ## @code{fieldnames} does.
    ##
    ## @end deftypefn
    function names = properties (this)
      names = fieldnames (this);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datatypes.tabular.CustomProperties} {@var{tf} =} isfield (@var{obj}, @var{name})
    ##
    ## Return true when a custom property of that name is set.
    ##
    ## @var{name} may be a character vector or a cellstr, in which case a
    ## logical array of the same size is returned, as for a struct.
    ##
    ## @end deftypefn
    function tf = isfield (this, name)
      tf = isfield (this.Values, name);
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {datatypes.tabular.CustomProperties} {@var{tf} =} isempty (@var{obj})
    ##
    ## Return true when no custom property is set.
    ##
    ## @end deftypefn
    function tf = isempty (this)
      tf = isempty (fieldnames (this.Values));
    endfunction

  endmethods

  methods (Hidden)

    function varargout = subsref (this, s)
      if (! strcmp (s(1).type, '.'))
        error (strcat ("datatypes.tabular.CustomProperties: only '.'", ...
                       " indexing is supported."));
      endif
      name = s(1).subs;
      ## A property name may be given as a string scalar, as in MATLAB.
      if (isstring (name) && isscalar (name))
        name = char (name);
      endif
      if (! (ischar (name) && isrow (name)))
        error (strcat ("datatypes.tabular.CustomProperties: '.' index", ...
                       " argument must be a character vector or a", ...
                       " string scalar."));
      endif
      if (! isfield (this.Values, name))
        error (strcat ("datatypes.tabular.CustomProperties: there is no", ...
                       " custom property named '%s'."), name);
      endif
      out = this.Values.(name);
      if (numel (s) > 1)
        out = subsref (out, s(2:end));
      endif
      varargout{1} = out;
    endfunction

    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    function disp (this)
      names = fieldnames (this.Values);
      if (isempty (names))
        fprintf ("  No custom properties are set.\n");
        fprintf (strcat ("  Use 'addprop' and 'rmprop' methods to modify", ...
                         " CustomProperties.\n"));
        return;
      endif
      fprintf ("\n  CustomProperties with properties:\n\n");
      for i = 1:numel (names)
        fprintf ("%+24s: %s\n", names{i}, ...
                 datatypes.tabular.TabularProperties.formatValue ( ...
                   this.Values.(names{i})));
      endfor
    endfunction

  endmethods

endclassdef
