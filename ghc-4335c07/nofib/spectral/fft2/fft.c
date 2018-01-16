/* 
From: Matt Donadio <donadio@ges33.mds.lmco.com>

Simon L. Peyton Jones writes:
 > If anyone has a C version, or is better with floats than me, it would
 > be delightful to either declare GHC2.04 correct, or fix the bug if there is
 > one.

This is the FFT that I generally use.  It's basically a direct
translation of the psuedocode from Cormen, Leiserson, and Rivest's
_Introduction_to_Algorithms_ (which is actually a lot better then the
examples I have seen in my DSP books).  I also tested it a while ago
against the an IEEE test and saw no problems.

I'll try to take a look at the nofib results when I get home tonight,
but I think I only have ghc-2.03.

Matt Donadio <donadio@mds.lmco.com>
Lockheed Martin Management & Data Systems

*/


/*
 * $Id: fft.c,v 1.2 2001/05/27 17:37:59 sof Exp $
 *
 * $Log: fft.c,v $
 * Revision 1.2  2001/05/27 17:37:59  sof
 * basic mingw headers doesn't define M_PI; make fft.c cope
 *
 * Revision 1.1  1997/07/27 01:03:52  sof
 * For reference, C version of FFT
 *
 * Revision 1.1  1996/10/24 15:41:00  donadio
 * Initial revision
 *
 */

static char RCSid[] = "$Id: fft.c,v 1.2 2001/05/27 17:37:59 sof Exp $";

/*
 * fft() - a straightforward implementation of a decimation in time
 *         FFT algorithm.  No fancy micro-twiddling of C code.  Let
 *         the compiler do that for you.
 *
 * input  - Pointer to the input vector.
 * output - Pointer to where you want the output, ie this is not an
 *          in-place algorithm.
 * n      - The number of points in the input vector.  Must be a power
 *          of two.
 * dir    - Direction :  1 = forward
 *                      -1 = reverse
 * 
 * See Cormen, Leiserson, and Rivest's _Introduction_to_Algorithms_,
 * Chapter 32 for more details.
 * 
 * If you need to speed this up, rewrite bit_reverse_copy() and try changing
 * the local variables with complex type into two variables with double type
 * Some uses of const may help, too.  YMMV.
 *
 * Matt Donadio - m.p.donadio@ieee.org
 *
 */

#include <math.h>

/* It's not there on mingw systems [05/01 - sof] */
#ifndef M_PI
#define M_PI		3.14159265358979323846
#endif

typedef struct {
  double r;
  double i;
} complex;

static void bit_reverse_copy(complex *, complex *, int);

void fft(complex * input, complex * output, unsigned int n, int dir)
{
  unsigned int s, j, k;
  unsigned int lgn;
  unsigned int m;
  complex Wm, W;
  complex t, u;

  bit_reverse_copy(input, output, n);

  for (lgn = 0; n >> lgn > 1; lgn++);

  for (s = 1; s <= lgn; s++) {
    m = 1 << s;
    Wm.r = cos(2.0 * M_PI / (double) m);
    Wm.i = sin(-dir * 2.0 * M_PI / (double) m);
    W.r = 1.0;
    W.i = 0.0;
    for (j = 0; j <= m / 2 - 1; j++) {
      for (k = j; k <= n - 1; k += m) {
	t.r = W.r * (output + k + m / 2)->r - W.i * (output + k + m / 2)->i;
	t.i = W.r * (output + k + m / 2)->i + W.i * (output + k + m / 2)->r;
	u.r = (output + k)->r;
	u.i = (output + k)->i;
	(output + k)->r = u.r + t.r;
	(output + k)->i = u.i + t.i;
	(output + k + m / 2)->r = u.r - t.r;
	(output + k + m / 2)->i = u.i - t.i;
      }
      t.r = W.r * Wm.r - W.i * Wm.i;
      t.i = W.r * Wm.i + W.i * Wm.r;
      W.r = t.r;
      W.i = t.i;
    }
  }

  if (dir == -1) {
    for (j = 0; j < n; j++) {
      (output + j)->r /= (double) n;
      (output + j)->i /= (double) n;
    }
  }
}

/*
 * this could probably be a lot better...
 */

static void bit_reverse_copy(complex * input, complex * output, int n)
{
  unsigned int i, j, k;
  int lgn;

  for (lgn = 0; n >> lgn > 1; lgn++);

  for (i = 0; i < n; i++) {
    k = 0;
    for (j = 0; j < lgn; j++) {
      if ((i & (1 << j)) == (1 << j)) {
	k += 1 << (lgn - 1 - j);
      }
    }
    (output + i)->r = (input + k)->r;
    (output + i)->i = (input + k)->i;
  }
}
