%!demo
%! ## `eq` (`==`) tests durations for equality; equal spans compare equal even when
%! ## built from different units.
%!
%! hours (1) == minutes (60)
%!
%! ## Element-wise against a scalar duration.
%! minutes ([30, 60, 90]) == hours (1)
