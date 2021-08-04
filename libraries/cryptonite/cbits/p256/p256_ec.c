/*
 * Copyright 2013 The Android Open Source Project
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Google Inc. nor the names of its contributors may
 *       be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY Google Inc. ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL Google Inc. BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// This is an implementation of the P256 elliptic curve group. It's written to
// be portable and still constant-time.
//
// WARNING: Implementing these functions in a constant-time manner is far from
//          obvious. Be careful when touching this code.
//
// See http://www.imperialviolet.org/2010/12/04/ecc.html ([1]) for background.

#include "p256/p256_gf.h"


/* Field element operations: */

/* felem_inv calculates |out| = |in|^{-1}
 *
 * Based on Fermat's Little Theorem:
 *   a^p = a (mod p)
 *   a^{p-1} = 1 (mod p)
 *   a^{p-2} = a^{-1} (mod p)
 */
static void felem_inv(felem out, const felem in) {
  felem ftmp, ftmp2;
  /* each e_I will hold |in|^{2^I - 1} */
  felem e2, e4, e8, e16, e32, e64;
  unsigned i;

  felem_square(ftmp, in); /* 2^1 */
  felem_mul(ftmp, in, ftmp); /* 2^2 - 2^0 */
  felem_assign(e2, ftmp);
  felem_square(ftmp, ftmp); /* 2^3 - 2^1 */
  felem_square(ftmp, ftmp); /* 2^4 - 2^2 */
  felem_mul(ftmp, ftmp, e2); /* 2^4 - 2^0 */
  felem_assign(e4, ftmp);
  felem_square(ftmp, ftmp); /* 2^5 - 2^1 */
  felem_square(ftmp, ftmp); /* 2^6 - 2^2 */
  felem_square(ftmp, ftmp); /* 2^7 - 2^3 */
  felem_square(ftmp, ftmp); /* 2^8 - 2^4 */
  felem_mul(ftmp, ftmp, e4); /* 2^8 - 2^0 */
  felem_assign(e8, ftmp);
  for (i = 0; i < 8; i++) {
    felem_square(ftmp, ftmp);
  } /* 2^16 - 2^8 */
  felem_mul(ftmp, ftmp, e8); /* 2^16 - 2^0 */
  felem_assign(e16, ftmp);
  for (i = 0; i < 16; i++) {
    felem_square(ftmp, ftmp);
  } /* 2^32 - 2^16 */
  felem_mul(ftmp, ftmp, e16); /* 2^32 - 2^0 */
  felem_assign(e32, ftmp);
  for (i = 0; i < 32; i++) {
    felem_square(ftmp, ftmp);
  } /* 2^64 - 2^32 */
  felem_assign(e64, ftmp);
  felem_mul(ftmp, ftmp, in); /* 2^64 - 2^32 + 2^0 */
  for (i = 0; i < 192; i++) {
    felem_square(ftmp, ftmp);
  } /* 2^256 - 2^224 + 2^192 */

  felem_mul(ftmp2, e64, e32); /* 2^64 - 2^0 */
  for (i = 0; i < 16; i++) {
    felem_square(ftmp2, ftmp2);
  } /* 2^80 - 2^16 */
  felem_mul(ftmp2, ftmp2, e16); /* 2^80 - 2^0 */
  for (i = 0; i < 8; i++) {
    felem_square(ftmp2, ftmp2);
  } /* 2^88 - 2^8 */
  felem_mul(ftmp2, ftmp2, e8); /* 2^88 - 2^0 */
  for (i = 0; i < 4; i++) {
    felem_square(ftmp2, ftmp2);
  } /* 2^92 - 2^4 */
  felem_mul(ftmp2, ftmp2, e4); /* 2^92 - 2^0 */
  felem_square(ftmp2, ftmp2); /* 2^93 - 2^1 */
  felem_square(ftmp2, ftmp2); /* 2^94 - 2^2 */
  felem_mul(ftmp2, ftmp2, e2); /* 2^94 - 2^0 */
  felem_square(ftmp2, ftmp2); /* 2^95 - 2^1 */
  felem_square(ftmp2, ftmp2); /* 2^96 - 2^2 */
  felem_mul(ftmp2, ftmp2, in); /* 2^96 - 3 */

  felem_mul(out, ftmp2, ftmp); /* 2^256 - 2^224 + 2^192 + 2^96 - 3 */
}


/* Group operations:
 *
 * Elements of the elliptic curve group are represented in Jacobian
 * coordinates: (x, y, z). An affine point (x', y') is x'=x/z**2, y'=y/z**3 in
 * Jacobian form. */

/* point_double sets {x_out,y_out,z_out} = 2*{x,y,z}.
 *
 * See http://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#doubling-dbl-2009-l */
static void point_double(felem x_out, felem y_out, felem z_out, const felem x,
                         const felem y, const felem z) {
  felem delta, gamma, alpha, beta, tmp, tmp2;

  felem_square(delta, z);
  felem_square(gamma, y);
  felem_mul(beta, x, gamma);

  felem_sum(tmp, x, delta);
  felem_diff(tmp2, x, delta);
  felem_mul(alpha, tmp, tmp2);
  felem_scalar_3(alpha);

  felem_sum(tmp, y, z);
  felem_square(tmp, tmp);
  felem_diff(tmp, tmp, gamma);
  felem_diff(z_out, tmp, delta);

  felem_scalar_4(beta);
  felem_square(x_out, alpha);
  felem_diff(x_out, x_out, beta);
  felem_diff(x_out, x_out, beta);

  felem_diff(tmp, beta, x_out);
  felem_mul(tmp, alpha, tmp);
  felem_square(tmp2, gamma);
  felem_scalar_8(tmp2);
  felem_diff(y_out, tmp, tmp2);
}

/* point_add_mixed sets {x_out,y_out,z_out} = {x1,y1,z1} + {x2,y2,1}.
 * (i.e. the second point is affine.)
 *
 * See http://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-2007-bl
 *
 * Note that this function does not handle P+P, infinity+P nor P+infinity
 * correctly. */
static void point_add_mixed(felem x_out, felem y_out, felem z_out,
                            const felem x1, const felem y1, const felem z1,
                            const felem x2, const felem y2) {
  felem z1z1, z1z1z1, s2, u2, h, i, j, r, rr, v, tmp;

  felem_square(z1z1, z1);
  felem_sum(tmp, z1, z1);

  felem_mul(u2, x2, z1z1);
  felem_mul(z1z1z1, z1, z1z1);
  felem_mul(s2, y2, z1z1z1);
  felem_diff(h, u2, x1);
  felem_sum(i, h, h);
  felem_square(i, i);
  felem_mul(j, h, i);
  felem_diff(r, s2, y1);
  felem_sum(r, r, r);
  felem_mul(v, x1, i);

  felem_mul(z_out, tmp, h);
  felem_square(rr, r);
  felem_diff(x_out, rr, j);
  felem_diff(x_out, x_out, v);
  felem_diff(x_out, x_out, v);

  felem_diff(tmp, v, x_out);
  felem_mul(y_out, tmp, r);
  felem_mul(tmp, y1, j);
  felem_diff(y_out, y_out, tmp);
  felem_diff(y_out, y_out, tmp);
}

/* point_add sets {x_out,y_out,z_out} = {x1,y1,z1} + {x2,y2,z2}.
 *
 * See http://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-2007-bl
 *
 * Note that this function does not handle P+P, infinity+P nor P+infinity
 * correctly. */
static void point_add(felem x_out, felem y_out, felem z_out, const felem x1,
                      const felem y1, const felem z1, const felem x2,
                      const felem y2, const felem z2) {
  felem z1z1, z1z1z1, z2z2, z2z2z2, s1, s2, u1, u2, h, i, j, r, rr, v, tmp;

  felem_square(z1z1, z1);
  felem_square(z2z2, z2);
  felem_mul(u1, x1, z2z2);

  felem_sum(tmp, z1, z2);
  felem_square(tmp, tmp);
  felem_diff(tmp, tmp, z1z1);
  felem_diff(tmp, tmp, z2z2);

  felem_mul(z2z2z2, z2, z2z2);
  felem_mul(s1, y1, z2z2z2);

  felem_mul(u2, x2, z1z1);
  felem_mul(z1z1z1, z1, z1z1);
  felem_mul(s2, y2, z1z1z1);
  felem_diff(h, u2, u1);
  felem_sum(i, h, h);
  felem_square(i, i);
  felem_mul(j, h, i);
  felem_diff(r, s2, s1);
  felem_sum(r, r, r);
  felem_mul(v, u1, i);

  felem_mul(z_out, tmp, h);
  felem_square(rr, r);
  felem_diff(x_out, rr, j);
  felem_diff(x_out, x_out, v);
  felem_diff(x_out, x_out, v);

  felem_diff(tmp, v, x_out);
  felem_mul(y_out, tmp, r);
  felem_mul(tmp, s1, j);
  felem_diff(y_out, y_out, tmp);
  felem_diff(y_out, y_out, tmp);
}

/* point_add_or_double_vartime sets {x_out,y_out,z_out} = {x1,y1,z1} +
 *                                                        {x2,y2,z2}.
 *
 * See http://www.hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-2007-bl
 *
 * This function handles the case where {x1,y1,z1}={x2,y2,z2}. */
static void point_add_or_double_vartime(
    felem x_out, felem y_out, felem z_out, const felem x1, const felem y1,
    const felem z1, const felem x2, const felem y2, const felem z2) {
  felem z1z1, z1z1z1, z2z2, z2z2z2, s1, s2, u1, u2, h, i, j, r, rr, v, tmp;
  char x_equal, y_equal;

  felem_square(z1z1, z1);
  felem_square(z2z2, z2);
  felem_mul(u1, x1, z2z2);

  felem_sum(tmp, z1, z2);
  felem_square(tmp, tmp);
  felem_diff(tmp, tmp, z1z1);
  felem_diff(tmp, tmp, z2z2);

  felem_mul(z2z2z2, z2, z2z2);
  felem_mul(s1, y1, z2z2z2);

  felem_mul(u2, x2, z1z1);
  felem_mul(z1z1z1, z1, z1z1);
  felem_mul(s2, y2, z1z1z1);
  felem_diff(h, u2, u1);
  x_equal = felem_is_zero_vartime(h);
  felem_sum(i, h, h);
  felem_square(i, i);
  felem_mul(j, h, i);
  felem_diff(r, s2, s1);
  y_equal = felem_is_zero_vartime(r);
  if (x_equal && y_equal) {
    point_double(x_out, y_out, z_out, x1, y1, z1);
    return;
  }
  felem_sum(r, r, r);
  felem_mul(v, u1, i);

  felem_mul(z_out, tmp, h);
  felem_square(rr, r);
  felem_diff(x_out, rr, j);
  felem_diff(x_out, x_out, v);
  felem_diff(x_out, x_out, v);

  felem_diff(tmp, v, x_out);
  felem_mul(y_out, tmp, r);
  felem_mul(tmp, s1, j);
  felem_diff(y_out, y_out, tmp);
  felem_diff(y_out, y_out, tmp);
}

/* copy_conditional sets out=in if mask = -1 in constant time.
 *
 * On entry: mask is either 0 or -1. */
static void copy_conditional(felem out, const felem in, limb mask) {
  int i;

  for (i = 0; i < NLIMBS; i++) {
    const limb tmp = mask & (in[i] ^ out[i]);
    out[i] ^= tmp;
  }
}

/* select_affine_point sets {out_x,out_y} to the index'th entry of table.
 * On entry: index < 16, table[0] must be zero. */
static void select_affine_point(felem out_x, felem out_y, const limb* table,
                                limb index) {
  limb i, j;

  memset(out_x, 0, sizeof(felem));
  memset(out_y, 0, sizeof(felem));

  for (i = 1; i < 16; i++) {
    limb mask = i ^ index;
    mask |= mask >> 2;
    mask |= mask >> 1;
    mask &= 1;
    mask--;
    for (j = 0; j < NLIMBS; j++, table++) {
      out_x[j] |= *table & mask;
    }
    for (j = 0; j < NLIMBS; j++, table++) {
      out_y[j] |= *table & mask;
    }
  }
}

/* select_jacobian_point sets {out_x,out_y,out_z} to the index'th entry of
 * table. On entry: index < 16, table[0] must be zero. */
static void select_jacobian_point(felem out_x, felem out_y, felem out_z,
                                  const limb* table, limb index) {
  limb i, j;

  memset(out_x, 0, sizeof(felem));
  memset(out_y, 0, sizeof(felem));
  memset(out_z, 0, sizeof(felem));

  /* The implicit value at index 0 is all zero. We don't need to perform that
   * iteration of the loop because we already set out_* to zero. */
  table += 3 * NLIMBS;

  // Hit all entries to obscure cache profiling.
  for (i = 1; i < 16; i++) {
    limb mask = i ^ index;
    mask |= mask >> 2;
    mask |= mask >> 1;
    mask &= 1;
    mask--;
    for (j = 0; j < NLIMBS; j++, table++) {
      out_x[j] |= *table & mask;
    }
    for (j = 0; j < NLIMBS; j++, table++) {
      out_y[j] |= *table & mask;
    }
    for (j = 0; j < NLIMBS; j++, table++) {
      out_z[j] |= *table & mask;
    }
  }
}

/* scalar_base_mult sets {nx,ny,nz} = scalar*G where scalar is a little-endian
 * number. Note that the value of scalar must be less than the order of the
 * group. */
static void scalar_base_mult(felem nx, felem ny, felem nz,
                             const cryptonite_p256_int* scalar) {
  int i, j;
  limb n_is_infinity_mask = -1, p_is_noninfinite_mask, mask;
  u32 table_offset;

  felem px, py;
  felem tx, ty, tz;

  memset(nx, 0, sizeof(felem));
  memset(ny, 0, sizeof(felem));
  memset(nz, 0, sizeof(felem));

  /* The loop adds bits at positions 0, 64, 128 and 192, followed by
   * positions 32,96,160 and 224 and does this 32 times. */
  for (i = 0; i < 32; i++) {
    if (i) {
      point_double(nx, ny, nz, nx, ny, nz);
    }
    table_offset = 0;
    for (j = 0; j <= 32; j += 32) {
      char bit0 = cryptonite_p256_get_bit(scalar, 31 - i + j);
      char bit1 = cryptonite_p256_get_bit(scalar, 95 - i + j);
      char bit2 = cryptonite_p256_get_bit(scalar, 159 - i + j);
      char bit3 = cryptonite_p256_get_bit(scalar, 223 - i + j);
      limb index = bit0 | (bit1 << 1) | (bit2 << 2) | (bit3 << 3);

      select_affine_point(px, py, kPrecomputed + table_offset, index);
      table_offset += 30 * NLIMBS;

      /* Since scalar is less than the order of the group, we know that
       * {nx,ny,nz} != {px,py,1}, unless both are zero, which we handle
       * below. */
      point_add_mixed(tx, ty, tz, nx, ny, nz, px, py);
      /* The result of point_add_mixed is incorrect if {nx,ny,nz} is zero
       * (a.k.a.  the point at infinity). We handle that situation by
       * copying the point from the table. */
      copy_conditional(nx, px, n_is_infinity_mask);
      copy_conditional(ny, py, n_is_infinity_mask);
      copy_conditional(nz, kOne, n_is_infinity_mask);

      /* Equally, the result is also wrong if the point from the table is
       * zero, which happens when the index is zero. We handle that by
       * only copying from {tx,ty,tz} to {nx,ny,nz} if index != 0. */
      p_is_noninfinite_mask = NON_ZERO_TO_ALL_ONES(index);
      mask = p_is_noninfinite_mask & ~n_is_infinity_mask;
      copy_conditional(nx, tx, mask);
      copy_conditional(ny, ty, mask);
      copy_conditional(nz, tz, mask);
      /* If p was not zero, then n is now non-zero. */
      n_is_infinity_mask &= ~p_is_noninfinite_mask;
    }
  }
}

/* point_to_affine converts a Jacobian point to an affine point. If the input
 * is the point at infinity then it returns (0, 0) in constant time. */
static void point_to_affine(felem x_out, felem y_out, const felem nx,
                            const felem ny, const felem nz) {
  felem z_inv, z_inv_sq;
  felem_inv(z_inv, nz);
  felem_square(z_inv_sq, z_inv);
  felem_mul(x_out, nx, z_inv_sq);
  felem_mul(z_inv, z_inv, z_inv_sq);
  felem_mul(y_out, ny, z_inv);
}

/* scalar_base_mult sets {nx,ny,nz} = scalar*{x,y}. */
static void scalar_mult(felem nx, felem ny, felem nz, const felem x,
                        const felem y, const cryptonite_p256_int* scalar) {
  int i;
  felem px, py, pz, tx, ty, tz;
  felem precomp[16][3];
  limb n_is_infinity_mask, index, p_is_noninfinite_mask, mask;

  /* We precompute 0,1,2,... times {x,y}. */
  memset(precomp, 0, sizeof(felem) * 3);
  memcpy(&precomp[1][0], x, sizeof(felem));
  memcpy(&precomp[1][1], y, sizeof(felem));
  memcpy(&precomp[1][2], kOne, sizeof(felem));

  for (i = 2; i < 16; i += 2) {
    point_double(precomp[i][0], precomp[i][1], precomp[i][2],
                 precomp[i / 2][0], precomp[i / 2][1], precomp[i / 2][2]);

    point_add_mixed(precomp[i + 1][0], precomp[i + 1][1], precomp[i + 1][2],
                    precomp[i][0], precomp[i][1], precomp[i][2], x, y);
  }

  memset(nx, 0, sizeof(felem));
  memset(ny, 0, sizeof(felem));
  memset(nz, 0, sizeof(felem));
  n_is_infinity_mask = -1;

  /* We add in a window of four bits each iteration and do this 64 times. */
  for (i = 0; i < 256; i += 4) {
    if (i) {
      point_double(nx, ny, nz, nx, ny, nz);
      point_double(nx, ny, nz, nx, ny, nz);
      point_double(nx, ny, nz, nx, ny, nz);
      point_double(nx, ny, nz, nx, ny, nz);
    }

    index = (cryptonite_p256_get_bit(scalar, 255 - i - 0) << 3) |
            (cryptonite_p256_get_bit(scalar, 255 - i - 1) << 2) |
            (cryptonite_p256_get_bit(scalar, 255 - i - 2) << 1) |
            cryptonite_p256_get_bit(scalar, 255 - i - 3);

    /* See the comments in scalar_base_mult about handling infinities. */
    select_jacobian_point(px, py, pz, precomp[0][0], index);
    point_add(tx, ty, tz, nx, ny, nz, px, py, pz);
    copy_conditional(nx, px, n_is_infinity_mask);
    copy_conditional(ny, py, n_is_infinity_mask);
    copy_conditional(nz, pz, n_is_infinity_mask);

    p_is_noninfinite_mask = NON_ZERO_TO_ALL_ONES(index);
    mask = p_is_noninfinite_mask & ~n_is_infinity_mask;

    copy_conditional(nx, tx, mask);
    copy_conditional(ny, ty, mask);
    copy_conditional(nz, tz, mask);
    n_is_infinity_mask &= ~p_is_noninfinite_mask;
  }
}

/* cryptonite_p256_base_point_mul sets {out_x,out_y} = nG, where n is < the
 * order of the group. */
void cryptonite_p256_base_point_mul(const cryptonite_p256_int* n, cryptonite_p256_int* out_x, cryptonite_p256_int* out_y) {
  felem x, y, z;

  scalar_base_mult(x, y, z, n);

  {
    felem x_affine, y_affine;

    point_to_affine(x_affine, y_affine, x, y, z);
    from_montgomery(out_x, x_affine);
    from_montgomery(out_y, y_affine);
  }
}

/* cryptonite_p256_points_mul_vartime sets {out_x,out_y} = n1*G + n2*{in_x,in_y}, where
 * n1 and n2 are < the order of the group.
 *
 * As indicated by the name, this function operates in variable time. This
 * is safe because it's used for signature validation which doesn't deal
 * with secrets. */
void cryptonite_p256_points_mul_vartime(
    const cryptonite_p256_int* n1, const cryptonite_p256_int* n2, const cryptonite_p256_int* in_x,
    const cryptonite_p256_int* in_y, cryptonite_p256_int* out_x, cryptonite_p256_int* out_y) {
  felem x1, y1, z1, x2, y2, z2, px, py;

  /* If both scalars are zero, then the result is the point at infinity. */
  if (cryptonite_p256_is_zero(n1) != 0 && cryptonite_p256_is_zero(n2) != 0) {
    cryptonite_p256_clear(out_x);
    cryptonite_p256_clear(out_y);
    return;
  }

  to_montgomery(px, in_x);
  to_montgomery(py, in_y);
  scalar_base_mult(x1, y1, z1, n1);
  scalar_mult(x2, y2, z2, px, py, n2);

  if (cryptonite_p256_is_zero(n2) != 0) {
    /* If n2 == 0, then {x2,y2,z2} is zero and the result is just
         * {x1,y1,z1}. */
  } else if (cryptonite_p256_is_zero(n1) != 0) {
    /* If n1 == 0, then {x1,y1,z1} is zero and the result is just
         * {x2,y2,z2}. */
    memcpy(x1, x2, sizeof(x2));
    memcpy(y1, y2, sizeof(y2));
    memcpy(z1, z2, sizeof(z2));
  } else {
    /* This function handles the case where {x1,y1,z1} == {x2,y2,z2}. */
    point_add_or_double_vartime(x1, y1, z1, x1, y1, z1, x2, y2, z2);
  }

  point_to_affine(px, py, x1, y1, z1);
  from_montgomery(out_x, px);
  from_montgomery(out_y, py);
}

/* this function is not part of the original source
   add 2 points together. so far untested.
   probably vartime, as it use point_add_or_double_vartime
 */
void cryptonite_p256e_point_add(
    const cryptonite_p256_int *in_x1, const cryptonite_p256_int *in_y1,
    const cryptonite_p256_int *in_x2, const cryptonite_p256_int *in_y2,
    cryptonite_p256_int *out_x, cryptonite_p256_int *out_y)
{
    felem x, y, z, px1, py1, px2, py2;

    to_montgomery(px1, in_x1);
    to_montgomery(py1, in_y1);
    to_montgomery(px2, in_x2);
    to_montgomery(py2, in_y2);

    point_add_or_double_vartime(x, y, z, px1, py1, kOne, px2, py2, kOne);

    point_to_affine(px1, py1, x, y, z);
    from_montgomery(out_x, px1);
    from_montgomery(out_y, py1);
}

/* this function is not part of the original source
   negate a point, i.e. (out_x, out_y) = (in_x, -in_y)
 */
void cryptonite_p256e_point_negate(
    const cryptonite_p256_int *in_x, const cryptonite_p256_int *in_y,
    cryptonite_p256_int *out_x, cryptonite_p256_int *out_y)
{
    memcpy(out_x, in_x, P256_NBYTES);
    cryptonite_p256_sub(&cryptonite_SECP256r1_p, in_y, out_y);
}

/* this function is not part of the original source
   cryptonite_p256e_point_mul sets {out_x,out_y} = n*{in_x,in_y}, where
   n is < the order of the group.
 */
void cryptonite_p256e_point_mul(const cryptonite_p256_int* n,
    const cryptonite_p256_int* in_x, const cryptonite_p256_int* in_y,
    cryptonite_p256_int* out_x, cryptonite_p256_int* out_y) {
  felem x, y, z, px, py;

  to_montgomery(px, in_x);
  to_montgomery(py, in_y);
  scalar_mult(x, y, z, px, py, n);
  point_to_affine(px, py, x, y, z);
  from_montgomery(out_x, px);
  from_montgomery(out_y, py);
}
