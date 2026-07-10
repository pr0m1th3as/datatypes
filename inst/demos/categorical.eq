%!demo
%! ## `eq` (the `==` operator) compares a categorical array element by element,
%! ## either to a single category name or to another categorical array.  Equality
%! ## works for every categorical — nominal or ordinal.
%!
%! C = categorical ({'M'; 'S'; 'L'; 'M'})
%!
%! ## Which elements equal a given category?
%! C == 'M'
%!
%! ## Element-wise against a second array.
%! C == categorical ({'M'; 'M'; 'L'; 'S'})
