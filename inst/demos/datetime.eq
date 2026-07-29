%!demo
%! ## Datetimes compare with `==` element-wise.  Comparison is by absolute
%! ## instant, so zoned datetimes compare correctly even across different zones.
%!
%! datetime (2024, 3, 9) == datetime (2024, 3, [8, 9, 10])
