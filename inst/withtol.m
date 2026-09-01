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

classdef withtol
  ## -*- texinfo -*-
  ## @deftp {datatypes} withtol
  ##
  ## Subscript into a timetable by times matched within a tolerance.
  ##
  ## A utility class that selects the rows of a timetable whose times fall
  ## within a tolerance of the times asked for.  It exists because a row time
  ## is rarely known to the last microsecond, so asking for one exactly would
  ## usually select nothing.
  ##
  ## @seealso{timerange, timetable}
  ## @end deftp

  properties (SetAccess = private, Hidden)
    ## The times to match
    subscriptTimes
    ## Half-width of the window around each of them
    tolerance
  endproperties

  methods (Hidden)

    ## Custom display
    function display (this)
      in_name = inputname (1);
      if (! isempty (in_name))
        fprintf ("%s =\n", in_name);
      endif
      disp (this);
    endfunction

    ## Custom display
    function disp (this)
      fprintf ("  withtol subscript: %d time(s), tolerance %s\n\n", ...
               numel (this.subscriptTimes), cellstr (this.tolerance){1});
    endfunction

  endmethods

  methods (Access = public)

    ## -*- texinfo -*-
    ## @deftypefn {withtol} {@var{wt} =} withtol (@var{subscriptTimes}, @var{tol})
    ##
    ## Create a tolerant time subscript.
    ##
    ## @code{@var{wt} = withtol (@var{subscriptTimes}, @var{tol})} creates a
    ## subscript selecting the rows of a timetable whose times are within
    ## @var{tol} of one of @var{subscriptTimes}.  The window is closed: a row
    ## exactly @var{tol} away is selected.
    ##
    ## @var{subscriptTimes} may be a @code{datetime} or @code{duration}
    ## vector, or text that reads as one.  @var{tol} must be a nonnegative
    ## @code{duration} scalar; a bare number is not accepted, there being
    ## nothing to say what unit it is in.
    ##
    ## The tolerance must be less than half the smallest gap between the
    ## times asked for, so that their windows cannot overlap and no row can
    ## be selected twice.
    ##
    ## @seealso{timerange, timetable}
    ## @end deftypefn
    function this = withtol (subscriptTimes, tol)

      if (nargin != 2)
        print_usage ();
      endif
      if (! (isduration (tol) && isscalar (tol)))
        error (strcat ("withtol: TOL must be a duration scalar; a number", ...
                       " says nothing about the unit it is in."));
      endif
      if (! (seconds (tol) >= 0))
        error ("withtol: TOL must be nonnegative.");
      endif
      if (isa (subscriptTimes, 'string') || ischar (subscriptTimes)
          || iscellstr (subscriptTimes))
        subscriptTimes = cellstr (subscriptTimes);
      elseif (! (isdatetime (subscriptTimes) || isduration (subscriptTimes)))
        error (strcat ("withtol: SUBSCRIPTTIMES must be a datetime or", ...
                       " duration vector, or text naming times."));
      endif
      ## Windows that overlap could select a row twice, so the tolerance is
      ## bounded by half the closest the asked-for times come to each other.
      if (! iscellstr (subscriptTimes) && numel (subscriptTimes) > 1)
        gaps = abs (diff (sort (subscriptTimes(:))));
        least = min (seconds (gaps));
        if (! (seconds (tol) < least / 2))
          error (strcat ("withtol: TOL must be less than half the", ...
                         " smallest gap between the subscript times, %s,", ...
                         " or a row could be selected twice."), ...
                 cellstr (gaps(1) - gaps(1) + seconds (least / 2)){1});
        endif
      endif
      this.subscriptTimes = subscriptTimes;
      this.tolerance = tol;
    endfunction

    ## -*- texinfo -*-
    ## @deftypefn {withtol} {@var{ix} =} rowIndices (@var{wt}, @var{rowTimes})
    ##
    ## Return the positions in @var{rowTimes} the subscript selects.
    ##
    ## The positions come back grouped by the time that matched them, in the
    ## order those times were asked for, rather than in the order the rows
    ## are held.
    ##
    ## @end deftypefn
    function ix = rowIndices (this, rowTimes)
      subs = this.subscriptTimes;
      if (iscellstr (subs))
        if (isdatetime (rowTimes))
          subs = datetime (subs);
          subs.TimeZone = rowTimes.TimeZone;
        else
          subs = duration (subs);
        endif
      elseif (! strcmp (class (subs), class (rowTimes)))
        error (strcat ("withtol: a timetable with %s row times cannot be", ...
                       " subscripted with %s times."), ...
               class (rowTimes), class (subs));
      endif
      ix = [];
      for i = 1:numel (subs)
        gap = abs (seconds (rowTimes - subs(i)));
        ix = [ix; find(gap <= seconds (this.tolerance))];
      endfor
    endfunction

  endmethods

endclassdef

## Test the tolerance window is closed at both ends
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (withtol (tv(3), hours (1)), tv), [2; 3; 4]);
%! assert_equal (rowIndices (withtol (tv(3), hours (1) - seconds (1)), tv), 3);
%! assert_equal (rowIndices (withtol (tv(3), seconds (0)), tv), 3);

## Test the rows come back in the order the times were asked for
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (withtol (tv([2 5]), minutes (1)), tv), [2; 5]);
%! assert_equal (rowIndices (withtol (tv([5 2]), minutes (1)), tv), [5; 2]);

## Test a time that matches nothing selects nothing
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (withtol (tv(1) - hours (9), minutes (1)), tv), ...
%!               zeros (0, 1));

## Test text times and duration row times
%!test
%! tv = datetime (2024, 1, 1) + hours (0:5)';
%! assert_equal (rowIndices (withtol ('01-Jan-2024 02:00:00', ...
%!                                    minutes (1)), tv), 3);
%! dv = hours (0:5)';
%! assert_equal (rowIndices (withtol (dv(3), minutes (1)), dv), 3);

%!error <withtol: TOL must be a duration scalar; a number says nothing about the unit it is in.> ...
%! withtol (datetime (2024, 1, 1), 1);
%!error <withtol: TOL must be nonnegative.> ...
%! withtol (datetime (2024, 1, 1), hours (-1));
%!error <withtol: TOL must be less than half the smallest gap between the subscript times, 00:30:00, or a row could be selected twice.> ...
%! tv = datetime (2024, 1, 1) + hours (0:5)'; withtol (tv([3 4]), hours (1));
%!error <withtol: SUBSCRIPTTIMES must be a datetime or duration vector, or text naming times.> ...
%! withtol (1, hours (1));
%!error <withtol: a timetable with duration row times cannot be subscripted with datetime times.> ...
%! rowIndices (withtol (datetime (2024, 1, 1), hours (1)), hours (0:2)');
