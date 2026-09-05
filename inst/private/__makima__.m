## Copyright (C) 2025-2026 Avanish Salunke <avanishsalunke16@gmail.com>
##
## This file is part of the datatypes package for GNU Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING. If not,
## see <http://www.gnu.org/licenses/>.

## Copied from the statistics package's 'makima' so that this package's
## interpolating methods can offer it: core Octave's 'interp1' has no
## 'makima'.  It goes when the function reaches core, and nothing here is
## meant to diverge from the original in the meantime.

## -*- texinfo -*-
## @deftypefn  {private} {@var{yi} =} __makima__ (@var{x}, @var{y}, @var{xq})
## @deftypefnx {private} {@var{yi} =} __makima__ (@var{y}, @var{xq})
## @deftypefnx {private} {@var{yi} =} __makima__ (@dots{}, @qcode{'extrap'})
##
## Compute the 1-D Modified Akima piecewise cubic Hermite interpolant of
## sample data @var{x} and @var{y}.
##
## The Modified Akima (MAKIMA) algorithm generates a shape-preserving 
## piecewise cubic interpolant. It differs from standard splines by avoiding
## excessive local undulations and overshoots, and it connects collinear points
## (flat regions) with straight lines. It is particularly well-suited for
## oscillatory data where @code{pchip} might aggressively flatten local
## extrema.
##
## The sample points @var{x} must be a vector of unique values. If @var{x}
## is not sorted, the function will automatically sort it and rearrange
## @var{y} accordingly.
##
## The sample values @var{y} can be a scalar, vector, or an N-dimensional
## array. If @var{y} is an N-dimensional array, the interpolation is
## performed along its last dimension, which must have the same length as
## @var{x}. Complex values for @var{y} are supported.
##
## If query points @var{xq} are provided, the function evaluates the 
## interpolant and returns the interpolated values @var{yi}. By default,
## @code{__makima__} uses the boundary polynomials to extrapolate for points 
## outside the range of @var{x}. The optional string argument @qcode{'extrap'} 
## is accepted for compatibility with other interpolation functions.
##
## If only @var{x} and @var{y} are provided, the function returns a 
## piecewise polynomial structure @var{pp} that represents the interpolant.
## This structure can be evaluated later at specific query points using
## @code{ppval}.
##
## Evaluating the interpolant at query points outside the domain of @var{x}
## automatically extrapolates using the boundary polynomials.
##
## @seealso{interp1, pchip, spline}
##
## This is an internal helper; do NOT call it directly.
##
## @end deftypefn

function yi = __makima__ (x, y, xq, varargin)
  if (nargin < 2 || nargin > 4)
    error ("__makima__: invalid number of inputs");
  endif

  if (nargin == 4 && ! strcmpi (varargin{1}, 'extrap'))
    error ("__makima__: unknown option '%s'", varargin{1});
  endif

  return_pp = (nargin == 2);

  if (! return_pp)
    size_xq = size (xq);
  endif

  x = x(:);
  n = numel (x);

  is_y_vector = isvector (y);
  size_y = size (y);

  if (is_y_vector)
    if (numel (y) != n)
      error (strcat ("__makima__: the number of sample points X, %d, is", ...
                     " incompatible with the number of values Y, %d."), ...
             n, numel (y));
    endif
    y = y(:);
    dim_y = 1;
    nc = 1;
  else
    dim_y = ndims (y);
    if (size_y(dim_y) != n)
      error (strcat ("__makima__: the number of sample points X, %d, is", ...
                     " incompatible with the number of values Y, %d."), ...
             n, size_y(dim_y));
    endif

    ## permute the interpolation dimension to be the first
    perm_order = 1:numel (size_y);
    perm_order(dim_y) = 1;
    perm_order(1) = dim_y;

    y = permute (y, perm_order);
    nc = numel (y) / n;
    y = reshape (y, n, nc);
  endif

  if (iscomplex (y))
    if (return_pp)
      ## Build complex pp struct
      pp_real = __makima__ (x, real (y));
      pp_imag = __makima__ (x, imag (y));
      yi = pp_real;
      yi.coefs = pp_real.coefs + 1i * pp_imag.coefs;
    else
      yi = __makima__ (x, real (y), xq, varargin{:}) ...
           + 1i * __makima__ (x, imag (y), xq, varargin{:});
    endif
    return;
  endif

  if (! return_pp)
    xqv = xq(:);
    nq = numel (xqv);
  endif

  if (n < 2)
    error ("__makima__: the first two inputs must have at least two elements.");
  endif

  if (! issorted (x))
    [x, sort_idx] = sort (x);
    y = y(sort_idx, :);
  endif

  math_done = false;
  if (n == 2)
    if (return_pp)
      ## Linear coefficients for 2-point pp struct
      coefs = zeros (nc, 4, class (y));
      coefs(:, 3) = (y(2, :).' - y(1, :).') ./ (x(2) - x(1));
      coefs(:, 4) = y(1, :).';
      
      if (is_y_vector)
        dim_out = 1;
      else
        dim_out = size_y;
        dim_out(dim_y) = [];
      endif
      yi = mkpp (x.', coefs, dim_out);
      return;
    else
      yi = interp1 (x, y, xqv, 'linear', 'extrap');
      yi = reshape (yi, [nq, nc]); 
      math_done = true;
    endif
  endif

  if (! math_done)
    dx = diff (x);
  
    if (any (dx <= 0))
      error ("__makima__: the sample points x must be unique.");
    endif
  dy = diff (y);
  m = dy ./ dx;

  m_0  = 2 * m(1, :) - m(2, :);
  m_m1 = 2 * m_0     - m(1, :);
  m_n  = 2 * m(end, :) - m(end-1, :);
  m_n1 = 2 * m_n       - m(end, :);

  m_ext = [m_m1; m_0; m; m_n; m_n1];

  d = zeros (n, nc, class (y));
  k_idx = (1 : n)';

  s_im2 = m_ext(k_idx    , :);
  s_im1 = m_ext(k_idx + 1, :);
  s_i   = m_ext(k_idx + 2, :);
  s_ip1 = m_ext(k_idx + 3, :);

  w1 = abs (s_ip1 - s_i)   + abs (s_ip1 + s_i)   / 2;
  w2 = abs (s_im1 - s_im2) + abs (s_im1 + s_im2) / 2;

  W = w1 + w2;

  numer = (w1 .* s_im1 + w2 .* s_i);
  denom = max (W, eps);
  d = numer ./ denom;

  zero_mask = (W == 0);
  if (any (zero_mask(:)))
    fallback = (s_im1 + s_i) / 2;
    d(zero_mask) = fallback(zero_mask);
  endif

  if (return_pp)
    hseg = dx;
    delta = m;
    
    d0 = d(1:end-1, :);
    d1 = d(2:end, :);
    y0 = y(1:end-1, :);
    
    c3 = (d0 + d1 - 2*delta) ./ (hseg .* hseg);
    c2 = (3*delta - 2*d0 - d1) ./ hseg;
    c1 = d0;
    c0 = y0;
    
    c3_t = c3.';
    c2_t = c2.';
    c1_t = c1.';
    c0_t = c0.';
    
    coefs = [c3_t(:), c2_t(:), c1_t(:), c0_t(:)];
    
    if (is_y_vector)
      dim_out = 1;
    else
      dim_out = size_y;
      dim_out(dim_y) = [];
      if (isempty (dim_out))
        dim_out = 1;
      endif
    endif

    yi = mkpp (x.', coefs, dim_out);
    return;
  endif

  yi = NaN (nq, nc, class (y));

  if (nq > 0)
    idx = lookup (x, xqv);

    idx(idx >= n) = n - 1;
    idx(idx == 0) = 1;

    x_left = x(idx);
    hseg = dx(idx);

    s = xqv - x_left;

    for c = 1:nc
      y0 = y(idx, c);
      y1 = y(idx + 1, c);
      d0 = d(idx, c);
      d1 = d(idx + 1, c);

      delta = (y1 - y0) ./ hseg;
      c2 = (3*delta - 2*d0 - d1) ./ hseg;
      c3 = (d0 + d1 - 2*delta) ./ (hseg.^2);

      yi(:, c) = y0 + s .* (d0 + s .* (c2 + s .* c3));
    endfor

  endif

  endif

  if (! return_pp)
    if (is_y_vector)
      yi = reshape (yi, size_xq);
    else
      out_shape = size_y;
      out_shape(dim_y) = nq;
      
      yi = reshape (yi, out_shape(perm_order));
      yi = ipermute (yi, perm_order);
      
      ## only append size_xq if multi-dimensional array is given
      if (! isvector (xq))
        final_shape = size_y;
        final_shape(dim_y) = [];
        final_shape = [final_shape, size_xq];
        
        yi = reshape (yi, final_shape);
      endif
    endif
  endif

endfunction
