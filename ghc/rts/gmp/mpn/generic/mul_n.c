/* mpn_mul_n and helper function -- Multiply/square natural numbers.

   THE HELPER FUNCTIONS IN THIS FILE (meaning everything except mpn_mul_n)
   ARE INTERNAL FUNCTIONS WITH MUTABLE INTERFACES.  IT IS ONLY SAFE TO REACH
   THEM THROUGH DOCUMENTED INTERFACES.  IN FACT, IT IS ALMOST GUARANTEED
   THAT THEY'LL CHANGE OR DISAPPEAR IN A FUTURE GNU MP RELEASE.


Copyright (C) 1991, 1993, 1994, 1996, 1997, 1998, 1999, 2000 Free Software
Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA. */

#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"


/* Multiplicative inverse of 3, modulo 2^BITS_PER_MP_LIMB.
   0xAAAAAAAB for 32 bits, 0xAAAAAAAAAAAAAAAB for 64 bits. */
#define INVERSE_3      ((MP_LIMB_T_MAX / 3) * 2 + 1)

#if !defined (__alpha) && !defined (__mips)
/* For all other machines, we want to call mpn functions for the compund
   operations instead of open-coding them.  */
#define USE_MORE_MPN
#endif

/*== Function declarations =================================================*/

static void evaluate3 _PROTO ((mp_ptr, mp_ptr, mp_ptr,
                               mp_ptr, mp_ptr, mp_ptr,
                               mp_srcptr, mp_srcptr, mp_srcptr,
                               mp_size_t, mp_size_t));
static void interpolate3 _PROTO ((mp_srcptr,
                                  mp_ptr, mp_ptr, mp_ptr,
                                  mp_srcptr,
                                  mp_ptr, mp_ptr, mp_ptr,
                                  mp_size_t, mp_size_t));
static mp_limb_t add2Times _PROTO ((mp_ptr, mp_srcptr, mp_srcptr, mp_size_t));


/*-- mpn_kara_mul_n ---------------------------------------------------------------*/

/* Multiplies using 3 half-sized mults and so on recursively.
 * p[0..2*n-1] := product of a[0..n-1] and b[0..n-1].
 * No overlap of p[...] with a[...] or b[...].
 * ws is workspace.
 */

void
#if __STDC__
mpn_kara_mul_n (mp_ptr p, mp_srcptr a, mp_srcptr b, mp_size_t n, mp_ptr ws)
#else
mpn_kara_mul_n(p, a, b, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_srcptr b;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t i, sign, w, w0, w1;
  mp_size_t n2;
  mp_srcptr x, y;

  n2 = n >> 1;
  ASSERT (n2 > 0);

  if (n & 1)
    {
      /* Odd length. */
      mp_size_t n1, n3, nm1;

      n3 = n - n2;

      sign = 0;
      w = a[n2];
      if (w != 0)
	w -= mpn_sub_n (p, a, a + n3, n2);
      else
	{
	  i = n2;
	  do
	    {
	      --i;
	      w0 = a[i];
	      w1 = a[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = a + n3;
	      y = a;
	      sign = 1;
	    }
	  else
	    {
	      x = a;
	      y = a + n3;
	    }
	  mpn_sub_n (p, x, y, n2);
	}
      p[n2] = w;

      w = b[n2];
      if (w != 0)
	w -= mpn_sub_n (p + n3, b, b + n3, n2);
      else
	{
	  i = n2;
	  do 
	    {
	      --i;
	      w0 = b[i]; 
	      w1 = b[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = b + n3;
	      y = b;
	      sign ^= 1;
	    }
	  else
	    {
	      x = b;
	      y = b + n3;
	    }
	  mpn_sub_n (p + n3, x, y, n2);
	}
      p[n] = w;

      n1 = n + 1;
      if (n2 < KARATSUBA_MUL_THRESHOLD)
	{
	  if (n3 < KARATSUBA_MUL_THRESHOLD)
	    {
	      mpn_mul_basecase (ws, p, n3, p + n3, n3);
	      mpn_mul_basecase (p, a, n3, b, n3);
	    }
	  else
	    {
	      mpn_kara_mul_n (ws, p, p + n3, n3, ws + n1);
	      mpn_kara_mul_n (p, a, b, n3, ws + n1);
	    }
	  mpn_mul_basecase (p + n1, a + n3, n2, b + n3, n2);
	}
      else
	{
	  mpn_kara_mul_n (ws, p, p + n3, n3, ws + n1);
	  mpn_kara_mul_n (p, a, b, n3, ws + n1);
	  mpn_kara_mul_n (p + n1, a + n3, b + n3, n2, ws + n1);
	}

      if (sign)
	mpn_add_n (ws, p, ws, n1);
      else
	mpn_sub_n (ws, p, ws, n1);

      nm1 = n - 1;
      if (mpn_add_n (ws, p + n1, ws, nm1))
	{
	  mp_limb_t x = ws[nm1] + 1;
	  ws[nm1] = x;
	  if (x == 0)
	    ++ws[n];
	}
      if (mpn_add_n (p + n3, p + n3, ws, n1))
	{
	  mp_limb_t x;
	  i = n1 + n3;
	  do
	    {
	      x = p[i] + 1;
	      p[i] = x;
	      ++i;
	    } while (x == 0);
	}
    }
  else
    {
      /* Even length. */
      mp_limb_t t;

      i = n2;
      do
	{
	  --i;
	  w0 = a[i];
	  w1 = a[n2+i];
	}
      while (w0 == w1 && i != 0);
      sign = 0;
      if (w0 < w1)
	{
	  x = a + n2;
	  y = a;
	  sign = 1;
	}
      else
	{
	  x = a;
	  y = a + n2;
	}
      mpn_sub_n (p, x, y, n2);

      i = n2;
      do 
	{
	  --i;
	  w0 = b[i];
	  w1 = b[n2+i];
	}
      while (w0 == w1 && i != 0);
      if (w0 < w1)
	{
	  x = b + n2;
	  y = b;
	  sign ^= 1;
	}
      else
	{
	  x = b;
	  y = b + n2;
	}
      mpn_sub_n (p + n2, x, y, n2);

      /* Pointwise products. */
      if (n2 < KARATSUBA_MUL_THRESHOLD)
	{
	  mpn_mul_basecase (ws, p, n2, p + n2, n2);
	  mpn_mul_basecase (p, a, n2, b, n2);
	  mpn_mul_basecase (p + n, a + n2, n2, b + n2, n2);
	}
      else
	{
	  mpn_kara_mul_n (ws, p, p + n2, n2, ws + n);
	  mpn_kara_mul_n (p, a, b, n2, ws + n);
	  mpn_kara_mul_n (p + n, a + n2, b + n2, n2, ws + n);
	}

      /* Interpolate. */
      if (sign)
	w = mpn_add_n (ws, p, ws, n);
      else
	w = -mpn_sub_n (ws, p, ws, n);
      w += mpn_add_n (ws, p + n, ws, n);
      w += mpn_add_n (p + n2, p + n2, ws, n);
      /* TO DO: could put "if (w) { ... }" here.
       * Less work but badly predicted branch.
       * No measurable difference in speed on Alpha.
       */
      i = n + n2;
      t = p[i] + w;
      p[i] = t;
      if (t < w)
	{
	  do
	    {
	      ++i;
	      w = p[i] + 1;
	      p[i] = w;
	    }
	  while (w == 0);
	}
    }
}

void
#if __STDC__
mpn_kara_sqr_n (mp_ptr p, mp_srcptr a, mp_size_t n, mp_ptr ws)
#else
mpn_kara_sqr_n (p, a, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t i, sign, w, w0, w1;
  mp_size_t n2;
  mp_srcptr x, y;

  n2 = n >> 1;
  ASSERT (n2 > 0);

  if (n & 1)
    {
      /* Odd length. */
      mp_size_t n1, n3, nm1;

      n3 = n - n2;

      sign = 0;
      w = a[n2];
      if (w != 0)
	w -= mpn_sub_n (p, a, a + n3, n2);
      else
	{
	  i = n2;
	  do
	    {
	      --i;
	      w0 = a[i];
	      w1 = a[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = a + n3;
	      y = a;
	      sign = 1;
	    }
	  else
	    {
	      x = a;
	      y = a + n3;
	    }
	  mpn_sub_n (p, x, y, n2);
	}
      p[n2] = w;

      w = a[n2];
      if (w != 0)
	w -= mpn_sub_n (p + n3, a, a + n3, n2);
      else
	{
	  i = n2;
	  do 
	    {
	      --i;
	      w0 = a[i]; 
	      w1 = a[n3+i];
	    }
	  while (w0 == w1 && i != 0);
	  if (w0 < w1)
	    {
	      x = a + n3;
	      y = a;
	      sign ^= 1;
	    }
	  else
	    {
	      x = a;
	      y = a + n3;
	    }
	  mpn_sub_n (p + n3, x, y, n2);
	}
      p[n] = w;

      n1 = n + 1;
      if (n2 < KARATSUBA_SQR_THRESHOLD)
	{
	  if (n3 < KARATSUBA_SQR_THRESHOLD)
	    {
	      mpn_sqr_basecase (ws, p, n3);
	      mpn_sqr_basecase (p, a, n3);
	    }
	  else
	    {
	      mpn_kara_sqr_n (ws, p, n3, ws + n1);
	      mpn_kara_sqr_n (p, a, n3, ws + n1);
	    }
	  mpn_sqr_basecase (p + n1, a + n3, n2);
	}
      else
	{
	  mpn_kara_sqr_n (ws, p, n3, ws + n1);
	  mpn_kara_sqr_n (p, a, n3, ws + n1);
	  mpn_kara_sqr_n (p + n1, a + n3, n2, ws + n1);
	}

      if (sign)
	mpn_add_n (ws, p, ws, n1);
      else
	mpn_sub_n (ws, p, ws, n1);

      nm1 = n - 1;
      if (mpn_add_n (ws, p + n1, ws, nm1))
	{
	  mp_limb_t x = ws[nm1] + 1;
	  ws[nm1] = x;
	  if (x == 0)
	    ++ws[n];
	}
      if (mpn_add_n (p + n3, p + n3, ws, n1))
	{
	  mp_limb_t x;
	  i = n1 + n3;
	  do
	    {
	      x = p[i] + 1;
	      p[i] = x;
	      ++i;
	    } while (x == 0);
	}
    }
  else
    {
      /* Even length. */
      mp_limb_t t;

      i = n2;
      do
	{
	  --i;
	  w0 = a[i];
	  w1 = a[n2+i];
	}
      while (w0 == w1 && i != 0);
      sign = 0;
      if (w0 < w1)
	{
	  x = a + n2;
	  y = a;
	  sign = 1;
	}
      else
	{
	  x = a;
	  y = a + n2;
	}
      mpn_sub_n (p, x, y, n2);

      i = n2;
      do 
	{
	  --i;
	  w0 = a[i];
	  w1 = a[n2+i];
	}
      while (w0 == w1 && i != 0);
      if (w0 < w1)
	{
	  x = a + n2;
	  y = a;
	  sign ^= 1;
	}
      else
	{
	  x = a;
	  y = a + n2;
	}
      mpn_sub_n (p + n2, x, y, n2);

      /* Pointwise products. */
      if (n2 < KARATSUBA_SQR_THRESHOLD)
	{
	  mpn_sqr_basecase (ws, p, n2);
	  mpn_sqr_basecase (p, a, n2);
	  mpn_sqr_basecase (p + n, a + n2, n2);
	}
      else
	{
	  mpn_kara_sqr_n (ws, p, n2, ws + n);
	  mpn_kara_sqr_n (p, a, n2, ws + n);
	  mpn_kara_sqr_n (p + n, a + n2, n2, ws + n);
	}

      /* Interpolate. */
      if (sign)
	w = mpn_add_n (ws, p, ws, n);
      else
	w = -mpn_sub_n (ws, p, ws, n);
      w += mpn_add_n (ws, p + n, ws, n);
      w += mpn_add_n (p + n2, p + n2, ws, n);
      /* TO DO: could put "if (w) { ... }" here.
       * Less work but badly predicted branch.
       * No measurable difference in speed on Alpha.
       */
      i = n + n2;
      t = p[i] + w;
      p[i] = t;
      if (t < w)
	{
	  do
	    {
	      ++i;
	      w = p[i] + 1;
	      p[i] = w;
	    }
	  while (w == 0);
	}
    }
}

/*-- add2Times -------------------------------------------------------------*/

/* z[] = x[] + 2 * y[]
   Note that z and x might point to the same vectors. */
#ifdef USE_MORE_MPN
static inline mp_limb_t
#if __STDC__
add2Times (mp_ptr z, mp_srcptr x, mp_srcptr y, mp_size_t n)
#else
add2Times (z, x, y, n)
     mp_ptr    z;
     mp_srcptr x;
     mp_srcptr y;
     mp_size_t n;
#endif
{
  mp_ptr t;
  mp_limb_t c;
  TMP_DECL (marker);
  TMP_MARK (marker);
  t = (mp_ptr) TMP_ALLOC (n * BYTES_PER_MP_LIMB);
  c = mpn_lshift (t, y, n, 1);
  c += mpn_add_n (z, x, t, n);
  TMP_FREE (marker);
  return c;
}
#else

static mp_limb_t
#if __STDC__
add2Times (mp_ptr z, mp_srcptr x, mp_srcptr y, mp_size_t n)
#else
add2Times (z, x, y, n)
     mp_ptr    z;
     mp_srcptr x;
     mp_srcptr y;
     mp_size_t n;
#endif
{
  mp_limb_t c, v, w;

  ASSERT (n > 0);
  v = *x; w = *y;
  c = w >> (BITS_PER_MP_LIMB - 1);
  w <<= 1;
  v += w;
  c += v < w;
  *z = v;
  ++x; ++y; ++z;
  while (--n)
    {
      v = *x;
      w = *y;
      v += c;
      c = v < c;
      c += w >> (BITS_PER_MP_LIMB - 1);
      w <<= 1;
      v += w;
      c += v < w;
      *z = v;
      ++x; ++y; ++z;
    }

  return c;
}
#endif

/*-- evaluate3 -------------------------------------------------------------*/

/* Evaluates:
 *   ph := 4*A+2*B+C
 *   p1 := A+B+C
 *   p2 := A+2*B+4*C
 * where:
 *   ph[], p1[], p2[], A[] and B[] all have length len,
 *   C[] has length len2 with len-len2 = 0, 1 or 2.
 * Returns top words (overflow) at pth, pt1 and pt2 respectively.
 */
#ifdef USE_MORE_MPN
static void
#if __STDC__
evaluate3 (mp_ptr ph, mp_ptr p1, mp_ptr p2, mp_ptr pth, mp_ptr pt1, mp_ptr pt2,
	   mp_srcptr A, mp_srcptr B, mp_srcptr C, mp_size_t len, mp_size_t len2)
#else
evaluate3 (ph, p1, p2, pth, pt1, pt2,
           A, B, C, len, len2)
     mp_ptr    ph;
     mp_ptr    p1;
     mp_ptr    p2;
     mp_ptr    pth;
     mp_ptr    pt1;
     mp_ptr    pt2;
     mp_srcptr A;
     mp_srcptr B;
     mp_srcptr C;
     mp_size_t len;
     mp_size_t len2;
#endif
{
  mp_limb_t c, d, e;
  
  ASSERT (len - len2 <= 2);

  e = mpn_lshift (p1, B, len, 1);

  c = mpn_lshift (ph, A, len, 2);
  c += e + mpn_add_n (ph, ph, p1, len);
  d = mpn_add_n (ph, ph, C, len2);
  if (len2 == len) c += d; else c += mpn_add_1 (ph + len2, ph + len2, len-len2, d);
  ASSERT (c < 7);
  *pth = c;

  c = mpn_lshift (p2, C, len2, 2);
#if 1
  if (len2 != len) { p2[len-1] = 0; p2[len2] = c; c = 0; }
  c += e + mpn_add_n (p2, p2, p1, len);
#else
  d = mpn_add_n (p2, p2, p1, len2);
  c += d;
  if (len2 != len) c = mpn_add_1 (p2+len2, p1+len2, len-len2, c);
  c += e;
#endif
  c += mpn_add_n (p2, p2, A, len);
  ASSERT (c < 7);
  *pt2 = c;

  c = mpn_add_n (p1, A, B, len);
  d = mpn_add_n (p1, p1, C, len2);
  if (len2 == len) c += d;
  else c += mpn_add_1 (p1+len2, p1+len2, len-len2, d);
  ASSERT (c < 3);
  *pt1 = c;

}

#else

static void
#if __STDC__
evaluate3 (mp_ptr ph, mp_ptr p1, mp_ptr p2, mp_ptr pth, mp_ptr pt1, mp_ptr pt2,
	   mp_srcptr A, mp_srcptr B, mp_srcptr C, mp_size_t l, mp_size_t ls)
#else
evaluate3 (ph, p1, p2, pth, pt1, pt2,
           A, B, C, l, ls)
     mp_ptr    ph;
     mp_ptr    p1;
     mp_ptr    p2;
     mp_ptr    pth;
     mp_ptr    pt1;
     mp_ptr    pt2;
     mp_srcptr A;
     mp_srcptr B;
     mp_srcptr C;
     mp_size_t l;
     mp_size_t ls;
#endif
{
  mp_limb_t a,b,c, i, t, th,t1,t2, vh,v1,v2;

  ASSERT (l - ls <= 2);

  th = t1 = t2 = 0;
  for (i = 0; i < l; ++i)
    {
      a = *A;
      b = *B;
      c = i < ls ? *C : 0;

      /* TO DO: choose one of the following alternatives. */
#if 0
      t = a << 2;
      vh = th + t;
      th = vh < t;
      th += a >> (BITS_PER_MP_LIMB - 2);
      t = b << 1;
      vh += t;
      th += vh < t;
      th += b >> (BITS_PER_MP_LIMB - 1);
      vh += c;
      th += vh < c;
#else
      vh = th + c;
      th = vh < c;
      t = b << 1;
      vh += t;
      th += vh < t;
      th += b >> (BITS_PER_MP_LIMB - 1);
      t = a << 2;
      vh += t;
      th += vh < t;
      th += a >> (BITS_PER_MP_LIMB - 2);
#endif

      v1 = t1 + a;
      t1 = v1 < a;
      v1 += b;
      t1 += v1 < b;
      v1 += c;
      t1 += v1 < c;

      v2 = t2 + a;
      t2 = v2 < a;
      t = b << 1;
      v2 += t;
      t2 += v2 < t;
      t2 += b >> (BITS_PER_MP_LIMB - 1);
      t = c << 2;
      v2 += t;
      t2 += v2 < t;
      t2 += c >> (BITS_PER_MP_LIMB - 2);

      *ph = vh;
      *p1 = v1;
      *p2 = v2;

      ++A; ++B; ++C;
      ++ph; ++p1; ++p2;
    }

  ASSERT (th < 7);
  ASSERT (t1 < 3);
  ASSERT (t2 < 7);

  *pth = th;
  *pt1 = t1;
  *pt2 = t2;
}
#endif


/*-- interpolate3 ----------------------------------------------------------*/

/* Interpolates B, C, D (in-place) from:
 *   16*A+8*B+4*C+2*D+E
 *   A+B+C+D+E
 *   A+2*B+4*C+8*D+16*E
 * where:
 *   A[], B[], C[] and D[] all have length l,
 *   E[] has length ls with l-ls = 0, 2 or 4.
 *
 * Reads top words (from earlier overflow) from ptb, ptc and ptd,
 * and returns new top words there.
 */

#ifdef USE_MORE_MPN
static void
#if __STDC__
interpolate3 (mp_srcptr A, mp_ptr B, mp_ptr C, mp_ptr D, mp_srcptr E,
              mp_ptr ptb, mp_ptr ptc, mp_ptr ptd, mp_size_t len, mp_size_t len2)
#else
interpolate3 (A, B, C, D, E,
              ptb, ptc, ptd, len, len2)
     mp_srcptr A;
     mp_ptr    B;
     mp_ptr    C;
     mp_ptr    D;
     mp_srcptr E;
     mp_ptr    ptb;
     mp_ptr    ptc;
     mp_ptr    ptd;
     mp_size_t len;
     mp_size_t len2;
#endif
{
  mp_ptr ws;
  mp_limb_t t, tb,tc,td;
  TMP_DECL (marker);
  TMP_MARK (marker);

  ASSERT (len - len2 == 0 || len - len2 == 2 || len - len2 == 4);

  /* Let x1, x2, x3 be the values to interpolate.  We have:
   *         b = 16*a + 8*x1 + 4*x2 + 2*x3 +    e
   *         c =    a +   x1 +   x2 +   x3 +    e
   *         d =    a + 2*x1 + 4*x2 + 8*x3 + 16*e
   */

  ws = (mp_ptr) TMP_ALLOC (len * BYTES_PER_MP_LIMB);

  tb = *ptb; tc = *ptc; td = *ptd;


  /* b := b - 16*a -    e
   * c := c -    a -    e
   * d := d -    a - 16*e
   */

  t = mpn_lshift (ws, A, len, 4);
  tb -= t + mpn_sub_n (B, B, ws, len);
  t = mpn_sub_n (B, B, E, len2);
  if (len2 == len) tb -= t;
  else tb -= mpn_sub_1 (B+len2, B+len2, len-len2, t);

  tc -= mpn_sub_n (C, C, A, len);
  t = mpn_sub_n (C, C, E, len2);
  if (len2 == len) tc -= t;
  else tc -= mpn_sub_1 (C+len2, C+len2, len-len2, t);

  t = mpn_lshift (ws, E, len2, 4);
  t += mpn_add_n (ws, ws, A, len2);
#if 1
  if (len2 != len) t = mpn_add_1 (ws+len2, A+len2, len-len2, t);
  td -= t + mpn_sub_n (D, D, ws, len);
#else
  t += mpn_sub_n (D, D, ws, len2);
  if (len2 != len) {
    t = mpn_sub_1 (D+len2, D+len2, len-len2, t);
    t += mpn_sub_n (D+len2, D+len2, A+len2, len-len2);
  } /* end if/else */
  td -= t;
#endif


  /* b, d := b + d, b - d */

#ifdef HAVE_MPN_ADD_SUB_N
  /* #error TO DO ... */
#else
  t = tb + td + mpn_add_n (ws, B, D, len);  
  td = tb - td - mpn_sub_n (D, B, D, len);
  tb = t;
  MPN_COPY (B, ws, len);
#endif
  
  /* b := b-8*c */
  t = 8 * tc + mpn_lshift (ws, C, len, 3);
  tb -= t + mpn_sub_n (B, B, ws, len);

  /* c := 2*c - b */
  tc = 2 * tc + mpn_lshift (C, C, len, 1);
  tc -= tb + mpn_sub_n (C, C, B, len);

  /* d := d/3 */
  td = (td - mpn_divexact_by3 (D, D, len)) * INVERSE_3;

  /* b, d := b + d, b - d */
#ifdef HAVE_MPN_ADD_SUB_N
  /* #error TO DO ... */
#else
  t = tb + td + mpn_add_n (ws, B, D, len);  
  td = tb - td - mpn_sub_n (D, B, D, len);
  tb = t;
  MPN_COPY (B, ws, len);
#endif

      /* Now:
       *	 b = 4*x1
       *	 c = 2*x2
       *	 d = 4*x3
       */

  ASSERT(!(*B & 3));
  mpn_rshift (B, B, len, 2);
  B[len-1] |= tb<<(BITS_PER_MP_LIMB-2);
  ASSERT((long)tb >= 0);
  tb >>= 2;

  ASSERT(!(*C & 1));
  mpn_rshift (C, C, len, 1);
  C[len-1] |= tc<<(BITS_PER_MP_LIMB-1);
  ASSERT((long)tc >= 0);
  tc >>= 1;

  ASSERT(!(*D & 3));
  mpn_rshift (D, D, len, 2);
  D[len-1] |= td<<(BITS_PER_MP_LIMB-2);
  ASSERT((long)td >= 0);
  td >>= 2;

#if WANT_ASSERT
  ASSERT (tb < 2);
  if (len == len2)
    {
      ASSERT (tc < 3);
      ASSERT (td < 2);
    }
  else
    {
      ASSERT (tc < 2);
      ASSERT (!td);
    }
#endif

  *ptb = tb;
  *ptc = tc;
  *ptd = td;

  TMP_FREE (marker);
}

#else

static void
#if __STDC__
interpolate3 (mp_srcptr A, mp_ptr B, mp_ptr C, mp_ptr D, mp_srcptr E,
	      mp_ptr ptb, mp_ptr ptc, mp_ptr ptd, mp_size_t l, mp_size_t ls)
#else
interpolate3 (A, B, C, D, E,
              ptb, ptc, ptd, l, ls)
     mp_srcptr A;
     mp_ptr    B;
     mp_ptr    C;
     mp_ptr    D;
     mp_srcptr E;
     mp_ptr    ptb;
     mp_ptr    ptc;
     mp_ptr    ptd;
     mp_size_t l;
     mp_size_t ls;
#endif
{
  mp_limb_t a,b,c,d,e,t, i, sb,sc,sd, ob,oc,od;
  const mp_limb_t maskOffHalf = (~(mp_limb_t) 0) << (BITS_PER_MP_LIMB >> 1);

#if WANT_ASSERT
  t = l - ls;
  ASSERT (t == 0 || t == 2 || t == 4);
#endif

  sb = sc = sd = 0;
  for (i = 0; i < l; ++i)
    {
      mp_limb_t tb, tc, td, tt;

      a = *A;
      b = *B;
      c = *C;
      d = *D;
      e = i < ls ? *E : 0;

      /* Let x1, x2, x3 be the values to interpolate.  We have:
       *	 b = 16*a + 8*x1 + 4*x2 + 2*x3 +    e
       *	 c =	a +   x1 +   x2 +   x3 +    e
       *	 d =	a + 2*x1 + 4*x2 + 8*x3 + 16*e
       */

      /* b := b - 16*a -    e
       * c := c -    a -    e
       * d := d -    a - 16*e
       */
      t = a << 4;
      tb = -(a >> (BITS_PER_MP_LIMB - 4)) - (b < t);
      b -= t;
      tb -= b < e;
      b -= e;
      tc = -(c < a);
      c -= a;
      tc -= c < e;
      c -= e;
      td = -(d < a);
      d -= a;
      t = e << 4;
      td = td - (e >> (BITS_PER_MP_LIMB - 4)) - (d < t);
      d -= t;

      /* b, d := b + d, b - d */
      t = b + d;
      tt = tb + td + (t < b);
      td = tb - td - (b < d);
      d = b - d;
      b = t;
      tb = tt;

      /* b := b-8*c */
      t = c << 3;
      tb = tb - (tc << 3) - (c >> (BITS_PER_MP_LIMB - 3)) - (b < t);
      b -= t;

      /* c := 2*c - b */
      t = c << 1;
      tc = (tc << 1) + (c >> (BITS_PER_MP_LIMB - 1)) - tb - (t < b);
      c = t - b;

      /* d := d/3 */
      d *= INVERSE_3;
      td = td - (d >> (BITS_PER_MP_LIMB - 1)) - (d*3 < d);
      td *= INVERSE_3;

      /* b, d := b + d, b - d */
      t = b + d;
      tt = tb + td + (t < b);
      td = tb - td - (b < d);
      d = b - d;
      b = t;
      tb = tt;

      /* Now:
       *	 b = 4*x1
       *	 c = 2*x2
       *	 d = 4*x3
       */

      /* sb has period 2. */
      b += sb;
      tb += b < sb;
      sb &= maskOffHalf;
      sb |= sb >> (BITS_PER_MP_LIMB >> 1);
      sb += tb;

      /* sc has period 1. */
      c += sc;
      tc += c < sc;
      /* TO DO: choose one of the following alternatives. */
#if 1
      sc = (mp_limb_t)((long)sc >> (BITS_PER_MP_LIMB - 1));
      sc += tc;
#else
      sc = tc - ((long)sc < 0L);
#endif

      /* sd has period 2. */
      d += sd;
      td += d < sd;
      sd &= maskOffHalf;
      sd |= sd >> (BITS_PER_MP_LIMB >> 1);
      sd += td;

      if (i != 0)
	{
	  B[-1] = ob | b << (BITS_PER_MP_LIMB - 2);
	  C[-1] = oc | c << (BITS_PER_MP_LIMB - 1);
	  D[-1] = od | d << (BITS_PER_MP_LIMB - 2);
	}
      ob = b >> 2;
      oc = c >> 1;
      od = d >> 2;

      ++A; ++B; ++C; ++D; ++E;
    }

  /* Handle top words. */
  b = *ptb;
  c = *ptc;
  d = *ptd;

  t = b + d;
  d = b - d;
  b = t;
  b -= c << 3;
  c = (c << 1) - b;
  d *= INVERSE_3;
  t = b + d;
  d = b - d;
  b = t;

  b += sb;
  c += sc;
  d += sd;

  B[-1] = ob | b << (BITS_PER_MP_LIMB - 2);
  C[-1] = oc | c << (BITS_PER_MP_LIMB - 1);
  D[-1] = od | d << (BITS_PER_MP_LIMB - 2);

  b >>= 2;
  c >>= 1;
  d >>= 2;

#if WANT_ASSERT
  ASSERT (b < 2);
  if (l == ls)
    {
      ASSERT (c < 3);
      ASSERT (d < 2);
    }
  else
    {
      ASSERT (c < 2);
      ASSERT (!d);
    }
#endif

  *ptb = b;
  *ptc = c;
  *ptd = d;
}
#endif


/*-- mpn_toom3_mul_n --------------------------------------------------------------*/

/* Multiplies using 5 mults of one third size and so on recursively.
 * p[0..2*n-1] := product of a[0..n-1] and b[0..n-1].
 * No overlap of p[...] with a[...] or b[...].
 * ws is workspace.
 */

/* TO DO: If TOOM3_MUL_THRESHOLD is much bigger than KARATSUBA_MUL_THRESHOLD then the
 *        recursion in mpn_toom3_mul_n() will always bottom out with mpn_kara_mul_n()
 *        because the "n < KARATSUBA_MUL_THRESHOLD" test here will always be false.
 */

#define TOOM3_MUL_REC(p, a, b, n, ws) \
  do {								\
    if (n < KARATSUBA_MUL_THRESHOLD)				\
      mpn_mul_basecase (p, a, n, b, n);				\
    else if (n < TOOM3_MUL_THRESHOLD)				\
      mpn_kara_mul_n (p, a, b, n, ws);				\
    else							\
      mpn_toom3_mul_n (p, a, b, n, ws);				\
  } while (0)

void
#if __STDC__
mpn_toom3_mul_n (mp_ptr p, mp_srcptr a, mp_srcptr b, mp_size_t n, mp_ptr ws)
#else
mpn_toom3_mul_n (p, a, b, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_srcptr b;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t cB,cC,cD, dB,dC,dD, tB,tC,tD;
  mp_limb_t *A,*B,*C,*D,*E, *W;
  mp_size_t l,l2,l3,l4,l5,ls;

  /* Break n words into chunks of size l, l and ls.
   * n = 3*k   => l = k,   ls = k
   * n = 3*k+1 => l = k+1, ls = k-1
   * n = 3*k+2 => l = k+1, ls = k
   */
  {
    mp_limb_t m;

    ASSERT (n >= TOOM3_MUL_THRESHOLD);
    l = ls = n / 3;
    m = n - l * 3;
    if (m != 0)
      ++l;
    if (m == 1)
      --ls;

    l2 = l * 2;
    l3 = l * 3;
    l4 = l * 4;
    l5 = l * 5;
    A = p;
    B = ws;
    C = p + l2;
    D = ws + l2;
    E = p + l4;
    W = ws + l4;
  }

  /** First stage: evaluation at points 0, 1/2, 1, 2, oo. **/
  evaluate3 (A, B, C, &cB, &cC, &cD, a, a + l, a + l2, l, ls);
  evaluate3 (A + l, B + l, C + l, &dB, &dC, &dD, b, b + l, b + l2, l, ls);

  /** Second stage: pointwise multiplies. **/
  TOOM3_MUL_REC(D, C, C + l, l, W);
  tD = cD*dD;
  if (cD) tD += mpn_addmul_1 (D + l, C + l, l, cD);
  if (dD) tD += mpn_addmul_1 (D + l, C, l, dD);
  ASSERT (tD < 49);
  TOOM3_MUL_REC(C, B, B + l, l, W);
  tC = cC*dC;
  /* TO DO: choose one of the following alternatives. */
#if 0
  if (cC) tC += mpn_addmul_1 (C + l, B + l, l, cC);
  if (dC) tC += mpn_addmul_1 (C + l, B, l, dC);
#else
  if (cC)
    {
      if (cC == 1) tC += mpn_add_n (C + l, C + l, B + l, l);
      else tC += add2Times (C + l, C + l, B + l, l);
    }
  if (dC)
    {
      if (dC == 1) tC += mpn_add_n (C + l, C + l, B, l);
      else tC += add2Times (C + l, C + l, B, l);
    }
#endif
  ASSERT (tC < 9);
  TOOM3_MUL_REC(B, A, A + l, l, W);
  tB = cB*dB;
  if (cB) tB += mpn_addmul_1 (B + l, A + l, l, cB);
  if (dB) tB += mpn_addmul_1 (B + l, A, l, dB);
  ASSERT (tB < 49);
  TOOM3_MUL_REC(A, a, b, l, W);
  TOOM3_MUL_REC(E, a + l2, b + l2, ls, W);

  /** Third stage: interpolation. **/
  interpolate3 (A, B, C, D, E, &tB, &tC, &tD, l2, ls << 1);

  /** Final stage: add up the coefficients. **/
  {
    mp_limb_t i, x, y;
    tB += mpn_add_n (p + l, p + l, B, l2);
    tD += mpn_add_n (p + l3, p + l3, D, l2);
    mpn_incr_u (p + l3, tB);
    mpn_incr_u (p + l4, tC);
    mpn_incr_u (p + l5, tD);
  }
}

/*-- mpn_toom3_sqr_n --------------------------------------------------------------*/

/* Like previous function but for squaring */

#define TOOM3_SQR_REC(p, a, n, ws) \
  do {								\
    if (n < KARATSUBA_SQR_THRESHOLD)				\
      mpn_sqr_basecase (p, a, n);				\
    else if (n < TOOM3_SQR_THRESHOLD)				\
      mpn_kara_sqr_n (p, a, n, ws);				\
    else							\
      mpn_toom3_sqr_n (p, a, n, ws);				\
  } while (0)

void
#if __STDC__
mpn_toom3_sqr_n (mp_ptr p, mp_srcptr a, mp_size_t n, mp_ptr ws)
#else
mpn_toom3_sqr_n (p, a, n, ws)
     mp_ptr    p;
     mp_srcptr a;
     mp_size_t n;
     mp_ptr    ws;
#endif
{
  mp_limb_t cB,cC,cD, tB,tC,tD;
  mp_limb_t *A,*B,*C,*D,*E, *W;
  mp_size_t l,l2,l3,l4,l5,ls;

  /* Break n words into chunks of size l, l and ls.
   * n = 3*k   => l = k,   ls = k
   * n = 3*k+1 => l = k+1, ls = k-1
   * n = 3*k+2 => l = k+1, ls = k
   */
  {
    mp_limb_t m;

    ASSERT (n >= TOOM3_MUL_THRESHOLD);
    l = ls = n / 3;
    m = n - l * 3;
    if (m != 0)
      ++l;
    if (m == 1)
      --ls;

    l2 = l * 2;
    l3 = l * 3;
    l4 = l * 4;
    l5 = l * 5;
    A = p;
    B = ws;
    C = p + l2;
    D = ws + l2;
    E = p + l4;
    W = ws + l4;
  }

  /** First stage: evaluation at points 0, 1/2, 1, 2, oo. **/
  evaluate3 (A, B, C, &cB, &cC, &cD, a, a + l, a + l2, l, ls);

  /** Second stage: pointwise multiplies. **/
  TOOM3_SQR_REC(D, C, l, W);
  tD = cD*cD;
  if (cD) tD += mpn_addmul_1 (D + l, C, l, 2*cD);
  ASSERT (tD < 49);
  TOOM3_SQR_REC(C, B, l, W);
  tC = cC*cC;
  /* TO DO: choose one of the following alternatives. */
#if 0
  if (cC) tC += mpn_addmul_1 (C + l, B, l, 2*cC);
#else
  if (cC >= 1)
    {
      tC += add2Times (C + l, C + l, B, l);
      if (cC == 2)
        tC += add2Times (C + l, C + l, B, l);
    }
#endif
  ASSERT (tC < 9);
  TOOM3_SQR_REC(B, A, l, W);
  tB = cB*cB;
  if (cB) tB += mpn_addmul_1 (B + l, A, l, 2*cB);
  ASSERT (tB < 49);
  TOOM3_SQR_REC(A, a, l, W);
  TOOM3_SQR_REC(E, a + l2, ls, W);

  /** Third stage: interpolation. **/
  interpolate3 (A, B, C, D, E, &tB, &tC, &tD, l2, ls << 1);

  /** Final stage: add up the coefficients. **/
  {
    mp_limb_t i, x, y;
    tB += mpn_add_n (p + l, p + l, B, l2);
    tD += mpn_add_n (p + l3, p + l3, D, l2);
    mpn_incr_u (p + l3, tB);
    mpn_incr_u (p + l4, tC);
    mpn_incr_u (p + l5, tD);
  }
}

void
#if __STDC__
mpn_mul_n (mp_ptr p, mp_srcptr a, mp_srcptr b, mp_size_t n)
#else
mpn_mul_n (p, a, b, n)
     mp_ptr    p;
     mp_srcptr a;
     mp_srcptr b;
     mp_size_t n;
#endif
{
  if (n < KARATSUBA_MUL_THRESHOLD)
    mpn_mul_basecase (p, a, n, b, n);
  else if (n < TOOM3_MUL_THRESHOLD)
    {
      /* Allocate workspace of fixed size on stack: fast! */
#if TUNE_PROGRAM_BUILD
      mp_limb_t ws[2 * (TOOM3_MUL_THRESHOLD_LIMIT-1) + 2 * BITS_PER_MP_LIMB];
#else
      mp_limb_t ws[2 * (TOOM3_MUL_THRESHOLD-1) + 2 * BITS_PER_MP_LIMB];
#endif
      mpn_kara_mul_n (p, a, b, n, ws);
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else if (n < FFT_MUL_THRESHOLD)
#else
  else
#endif
    {
      /* Use workspace of unknown size in heap, as stack space may
       * be limited.  Since n is at least TOOM3_MUL_THRESHOLD, the
       * multiplication will take much longer than malloc()/free().  */
      mp_limb_t wsLen, *ws;
      wsLen = 2 * n + 3 * BITS_PER_MP_LIMB;
      ws = (mp_ptr) (*_mp_allocate_func) ((size_t) wsLen * sizeof (mp_limb_t));
      mpn_toom3_mul_n (p, a, b, n, ws);
      (*_mp_free_func) (ws, (size_t) wsLen * sizeof (mp_limb_t));
    }
#if WANT_FFT || TUNE_PROGRAM_BUILD
  else
    {
      mpn_mul_fft_full (p, a, n, b, n);      
    }
#endif
}
