/* An implementation in GMP of Scho"nhage's fast multiplication algorithm
   modulo 2^N+1, by Paul Zimmermann, INRIA Lorraine, February 1998.

   THE CONTENTS OF THIS FILE ARE FOR INTERNAL USE AND THE FUNCTIONS HAVE
   MUTABLE INTERFACES.  IT IS ONLY SAFE TO REACH THEM THROUGH DOCUMENTED
   INTERFACES.  IT IS ALMOST GUARANTEED THAT THEY'LL CHANGE OR DISAPPEAR IN
   A FUTURE GNU MP RELEASE.

Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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


/* References:

   Schnelle Multiplikation grosser Zahlen, by Arnold Scho"nhage and Volker
   Strassen, Computing 7, p. 281-292, 1971.

   Asymptotically fast algorithms for the numerical multiplication
   and division of polynomials with complex coefficients, by Arnold Scho"nhage,
   Computer Algebra, EUROCAM'82, LNCS 144, p. 3-15, 1982.

   Tapes versus Pointers, a study in implementing fast algorithms,
   by Arnold Scho"nhage, Bulletin of the EATCS, 30, p. 23-32, 1986.

   See also http://www.loria.fr/~zimmerma/bignum


   Future:

   K==2 isn't needed in the current uses of this code and the bits specific
   for that could be dropped.

   It might be possible to avoid a small number of MPN_COPYs by using a
   rotating temporary or two.

   Multiplications of unequal sized operands can be done with this code, but
   it needs a tighter test for identifying squaring (same sizes as well as
   same pointers).  */


#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"


/* Change this to "#define TRACE(x) x" for some traces. */
#define TRACE(x)



FFT_TABLE_ATTRS mp_size_t mpn_fft_table[2][MPN_FFT_TABLE_SIZE] = {
  FFT_MUL_TABLE,
  FFT_SQR_TABLE
};


static void mpn_mul_fft_internal
_PROTO ((mp_limb_t *op, mp_srcptr n, mp_srcptr m, mp_size_t pl,
         int k, int K,
         mp_limb_t **Ap, mp_limb_t **Bp,
         mp_limb_t *A, mp_limb_t *B,
         mp_size_t nprime, mp_size_t l, mp_size_t Mp, int **_fft_l,
         mp_limb_t *T, int rec));


/* Find the best k to use for a mod 2^(n*BITS_PER_MP_LIMB)+1 FFT.
   sqr==0 if for a multiply, sqr==1 for a square */
int
#if __STDC__
mpn_fft_best_k (mp_size_t n, int sqr)
#else
mpn_fft_best_k (n, sqr)
     mp_size_t n;
     int       sqr;
#endif
{
  mp_size_t  t;
  int        i;

  for (i = 0; mpn_fft_table[sqr][i] != 0; i++)
    if (n < mpn_fft_table[sqr][i])
      return i + FFT_FIRST_K;

  /* treat 4*last as one further entry */
  if (i == 0 || n < 4*mpn_fft_table[sqr][i-1])
    return i + FFT_FIRST_K;
  else
    return i + FFT_FIRST_K + 1;
}


/* Returns smallest possible number of limbs >= pl for a fft of size 2^k.
   FIXME: Is this simply pl rounded up to the next multiple of 2^k ?  */

mp_size_t
#if __STDC__
mpn_fft_next_size (mp_size_t pl, int k)
#else
mpn_fft_next_size (pl, k)
     mp_size_t pl;
     int       k;
#endif
{
  mp_size_t N, M;
  int       K;

  /*  if (k==0) k = mpn_fft_best_k (pl, sqr); */
  N = pl*BITS_PER_MP_LIMB;
  K = 1<<k;
  if (N%K) N=(N/K+1)*K;
  M = N/K;
  if (M%BITS_PER_MP_LIMB) N=((M/BITS_PER_MP_LIMB)+1)*BITS_PER_MP_LIMB*K;
  return (N/BITS_PER_MP_LIMB);
}


static void
#if __STDC__
mpn_fft_initl(int **l, int k)
#else
mpn_fft_initl(l, k)
     int  **l;
     int  k;
#endif
{
    int i,j,K;

    l[0][0] = 0;
    for (i=1,K=2;i<=k;i++,K*=2) {
	for (j=0;j<K/2;j++) {
	    l[i][j] = 2*l[i-1][j];
	    l[i][K/2+j] = 1+l[i][j];
	}
    }
}


/* a <- -a mod 2^(n*BITS_PER_MP_LIMB)+1 */
static void
#if __STDC__
mpn_fft_neg_modF(mp_limb_t *ap, mp_size_t n)
#else
mpn_fft_neg_modF(ap, n)
     mp_limb_t *ap;
     mp_size_t n;
#endif
{
  mp_limb_t c;

  c = ap[n]+2;
  mpn_com_n (ap, ap, n);
  ap[n]=0; mpn_incr_u(ap, c);
}


/* a <- a*2^e mod 2^(n*BITS_PER_MP_LIMB)+1 */
static void
#if __STDC__
mpn_fft_mul_2exp_modF(mp_limb_t *ap, int e, mp_size_t n, mp_limb_t *tp)
#else
mpn_fft_mul_2exp_modF(ap, e, n, tp)
     mp_limb_t *ap;
     int e;
     mp_size_t n;
     mp_limb_t *tp;
#endif
{
  int d, sh, i; mp_limb_t cc;

  d = e%(n*BITS_PER_MP_LIMB); /* 2^e = (+/-) 2^d */
  sh = d % BITS_PER_MP_LIMB;
  if (sh) mpn_lshift(tp, ap, n+1, sh); /* no carry here */
  else MPN_COPY(tp, ap, n+1);
  d /= BITS_PER_MP_LIMB; /* now shift of d limbs to the left */
 if (d) { 
   /* ap[d..n-1] = tp[0..n-d-1], ap[0..d-1] = -tp[n-d..n-1] */
   /* mpn_xor would be more efficient here */
   for (i=d-1;i>=0;i--) ap[i] = ~tp[n-d+i];
   cc = 1-mpn_add_1(ap, ap, d, 1);
   if (cc) cc=mpn_sub_1(ap+d, tp, n-d, 1);
   else MPN_COPY(ap+d, tp, n-d);
   if (cc+=mpn_sub_1(ap+d, ap+d, n-d, tp[n]))
     ap[n]=mpn_add_1(ap, ap, n, cc);
   else ap[n]=0;
  }
  else if ((ap[n]=mpn_sub_1(ap, tp, n, tp[n]))) {
    ap[n]=mpn_add_1(ap, ap, n, 1);
  }
  if ((e/(n*BITS_PER_MP_LIMB))%2) mpn_fft_neg_modF(ap, n);
}


/* a <- a+b mod 2^(n*BITS_PER_MP_LIMB)+1 */
static void
#if __STDC__
mpn_fft_add_modF (mp_limb_t *ap, mp_limb_t *bp, int n)
#else
mpn_fft_add_modF (ap, bp, n)
     mp_limb_t *ap,*bp;
     int n;
#endif
{
  mp_limb_t c;

  c = ap[n] + bp[n] + mpn_add_n(ap, ap, bp, n);
  if (c>1) c -= 1+mpn_sub_1(ap,ap,n,1);
  ap[n]=c;
}


/* input: A[0] ... A[inc*(K-1)] are residues mod 2^N+1 where
          N=n*BITS_PER_MP_LIMB 
          2^omega is a primitive root mod 2^N+1
   output: A[inc*l[k][i]] <- \sum (2^omega)^(ij) A[inc*j] mod 2^N+1 */

static void
#if __STDC__
mpn_fft_fft_sqr (mp_limb_t **Ap, mp_size_t K, int **ll,
                 mp_size_t omega, mp_size_t n, mp_size_t inc, mp_limb_t *tp)
#else
mpn_fft_fft_sqr(Ap,K,ll,omega,n,inc,tp)
mp_limb_t **Ap,*tp;
mp_size_t K,omega,n,inc;
int       **ll;
#endif
{
  if (K==2) {
#ifdef ADDSUB
      if (mpn_addsub_n(Ap[0], Ap[inc], Ap[0], Ap[inc], n+1) & 1)
#else
      MPN_COPY(tp, Ap[0], n+1);
      mpn_add_n(Ap[0], Ap[0], Ap[inc],n+1);
      if (mpn_sub_n(Ap[inc], tp, Ap[inc],n+1))
#endif
      	Ap[inc][n] = mpn_add_1(Ap[inc], Ap[inc], n, 1);
    }
    else {
      int       j, inc2=2*inc;
      int       *lk = *ll;
      mp_limb_t *tmp;
      TMP_DECL(marker);

      TMP_MARK(marker);
      tmp = TMP_ALLOC_LIMBS (n+1);
	mpn_fft_fft_sqr(Ap, K/2,ll-1,2*omega,n,inc2, tp);
	mpn_fft_fft_sqr(Ap+inc, K/2,ll-1,2*omega,n,inc2, tp);
	/* A[2*j*inc]   <- A[2*j*inc] + omega^l[k][2*j*inc] A[(2j+1)inc]
	   A[(2j+1)inc] <- A[2*j*inc] + omega^l[k][(2j+1)inc] A[(2j+1)inc] */
	for (j=0;j<K/2;j++,lk+=2,Ap+=2*inc) {
	  MPN_COPY(tp, Ap[inc], n+1);
	  mpn_fft_mul_2exp_modF(Ap[inc], lk[1]*omega, n, tmp);
	  mpn_fft_add_modF(Ap[inc], Ap[0], n);
	  mpn_fft_mul_2exp_modF(tp,lk[0]*omega, n, tmp);
	  mpn_fft_add_modF(Ap[0], tp, n);
	}
        TMP_FREE(marker);
    }
}


/* input: A[0] ... A[inc*(K-1)] are residues mod 2^N+1 where
          N=n*BITS_PER_MP_LIMB 
         2^omega is a primitive root mod 2^N+1 
   output: A[inc*l[k][i]] <- \sum (2^omega)^(ij) A[inc*j] mod 2^N+1 */

static void
#if __STDC__
mpn_fft_fft (mp_limb_t **Ap, mp_limb_t **Bp, mp_size_t K, int **ll,
             mp_size_t omega, mp_size_t n, mp_size_t inc, mp_limb_t *tp)
#else
mpn_fft_fft(Ap,Bp,K,ll,omega,n,inc,tp)
     mp_limb_t **Ap,**Bp,*tp;
     mp_size_t K,omega,n,inc;
     int       **ll;
#endif
{
  if (K==2) {
#ifdef ADDSUB
      if (mpn_addsub_n(Ap[0], Ap[inc], Ap[0], Ap[inc], n+1) & 1)
#else
      MPN_COPY(tp, Ap[0], n+1);
      mpn_add_n(Ap[0], Ap[0], Ap[inc],n+1);
      if (mpn_sub_n(Ap[inc], tp, Ap[inc],n+1))
#endif
      	Ap[inc][n] = mpn_add_1(Ap[inc], Ap[inc], n, 1);
#ifdef ADDSUB
      if (mpn_addsub_n(Bp[0], Bp[inc], Bp[0], Bp[inc], n+1) & 1)
#else
      MPN_COPY(tp, Bp[0], n+1);
      mpn_add_n(Bp[0], Bp[0], Bp[inc],n+1);
      if (mpn_sub_n(Bp[inc], tp, Bp[inc],n+1))
#endif
      	Bp[inc][n] = mpn_add_1(Bp[inc], Bp[inc], n, 1);
    }
    else {
	int       j, inc2=2*inc;
        int       *lk=*ll;
        mp_limb_t *tmp;
	TMP_DECL(marker);

	TMP_MARK(marker);
	tmp = TMP_ALLOC_LIMBS (n+1);
	mpn_fft_fft(Ap, Bp, K/2,ll-1,2*omega,n,inc2, tp);
	mpn_fft_fft(Ap+inc, Bp+inc, K/2,ll-1,2*omega,n,inc2, tp);
	/* A[2*j*inc]   <- A[2*j*inc] + omega^l[k][2*j*inc] A[(2j+1)inc]
	   A[(2j+1)inc] <- A[2*j*inc] + omega^l[k][(2j+1)inc] A[(2j+1)inc] */
	for (j=0;j<K/2;j++,lk+=2,Ap+=2*inc,Bp+=2*inc) {
	  MPN_COPY(tp, Ap[inc], n+1);
	  mpn_fft_mul_2exp_modF(Ap[inc], lk[1]*omega, n, tmp);
	  mpn_fft_add_modF(Ap[inc], Ap[0], n);
	  mpn_fft_mul_2exp_modF(tp,lk[0]*omega, n, tmp);
	  mpn_fft_add_modF(Ap[0], tp, n);
	  MPN_COPY(tp, Bp[inc], n+1);
	  mpn_fft_mul_2exp_modF(Bp[inc], lk[1]*omega, n, tmp);
	  mpn_fft_add_modF(Bp[inc], Bp[0], n);
	  mpn_fft_mul_2exp_modF(tp,lk[0]*omega, n, tmp);
	  mpn_fft_add_modF(Bp[0], tp, n);
	}
	TMP_FREE(marker);
    }
}


/* a[i] <- a[i]*b[i] mod 2^(n*BITS_PER_MP_LIMB)+1 for 0 <= i < K */
static void
#if __STDC__
mpn_fft_mul_modF_K (mp_limb_t **ap, mp_limb_t **bp, mp_size_t n, int K) 
#else
mpn_fft_mul_modF_K(ap, bp, n, K) 
     mp_limb_t **ap, **bp;
     mp_size_t n;
     int       K;
#endif
{
  int  i;
  int  sqr = (ap == bp);
  TMP_DECL(marker);
  
  TMP_MARK(marker); 

  if (n >= (sqr ? FFT_MODF_SQR_THRESHOLD : FFT_MODF_MUL_THRESHOLD)) {
    int k, K2,nprime2,Nprime2,M2,maxLK,l,Mp2;
    int       **_fft_l;
    mp_limb_t **Ap,**Bp,*A,*B,*T;

    k = mpn_fft_best_k (n, sqr);
    K2 = 1<<k;
    maxLK = (K2>BITS_PER_MP_LIMB) ? K2 : BITS_PER_MP_LIMB;
    M2 = n*BITS_PER_MP_LIMB/K2;
    l = n/K2;
    Nprime2 = ((2*M2+k+2+maxLK)/maxLK)*maxLK; /* ceil((2*M2+k+3)/maxLK)*maxLK*/
    nprime2 = Nprime2/BITS_PER_MP_LIMB;
    Mp2 = Nprime2/K2;

    Ap = TMP_ALLOC_MP_PTRS (K2);
    Bp = TMP_ALLOC_MP_PTRS (K2);
    A = TMP_ALLOC_LIMBS (2*K2*(nprime2+1));
    T = TMP_ALLOC_LIMBS (nprime2+1);
    B = A + K2*(nprime2+1);
    _fft_l = TMP_ALLOC_TYPE (k+1, int*);
    for (i=0;i<=k;i++)
      _fft_l[i] = TMP_ALLOC_TYPE (1<<i, int);
    mpn_fft_initl(_fft_l, k);

    TRACE (printf("recurse: %dx%d limbs -> %d times %dx%d (%1.2f)\n", n,
                  n, K2, nprime2, nprime2, 2.0*(double)n/nprime2/K2));

    for (i=0;i<K;i++,ap++,bp++)
      mpn_mul_fft_internal(*ap, *ap, *bp, n, k, K2, Ap, Bp, A, B, nprime2,
	 l, Mp2, _fft_l, T, 1);
  }
  else {
     mp_limb_t *a, *b, cc, *tp, *tpn; int n2=2*n;
     tp = TMP_ALLOC_LIMBS (n2);
     tpn = tp+n;
     TRACE (printf ("  mpn_mul_n %d of %d limbs\n", K, n));
     for (i=0;i<K;i++) {
        a = *ap++; b=*bp++;
        if (sqr)
          mpn_sqr_n(tp, a, n);
        else
          mpn_mul_n(tp, b, a, n);
	if (a[n]) cc=mpn_add_n(tpn, tpn, b, n); else cc=0;
	if (b[n]) cc += mpn_add_n(tpn, tpn, a, n) + a[n];
	if (cc) {
          cc = mpn_add_1(tp, tp, n2, cc);
          ASSERT_NOCARRY (mpn_add_1(tp, tp, n2, cc));
        }
	a[n] = mpn_sub_n(a, tp, tpn, n) && mpn_add_1(a, a, n, 1); 
     }
  }
  TMP_FREE(marker); 
}


/* input: A^[l[k][0]] A^[l[k][1]] ... A^[l[k][K-1]]
   output: K*A[0] K*A[K-1] ... K*A[1] */

static void
#if __STDC__
mpn_fft_fftinv (mp_limb_t **Ap, int K, mp_size_t omega, mp_size_t n,
                mp_limb_t *tp)
#else
mpn_fft_fftinv(Ap,K,omega,n,tp)
     mp_limb_t **Ap, *tp;
     int       K;
     mp_size_t omega, n;
#endif
{
    if (K==2) {
#ifdef ADDSUB
      if (mpn_addsub_n(Ap[0], Ap[1], Ap[0], Ap[1], n+1) & 1)
#else
      MPN_COPY(tp, Ap[0], n+1);
      mpn_add_n(Ap[0], Ap[0], Ap[1], n+1);
      if (mpn_sub_n(Ap[1], tp, Ap[1], n+1))
#endif
        Ap[1][n] = mpn_add_1(Ap[1], Ap[1], n, 1);
    }
    else {
	int j, K2=K/2; mp_limb_t **Bp=Ap+K2, *tmp; 
	TMP_DECL(marker);

	TMP_MARK(marker);
	tmp = TMP_ALLOC_LIMBS (n+1);
	mpn_fft_fftinv(Ap, K2, 2*omega, n, tp);
	mpn_fft_fftinv(Bp, K2, 2*omega, n, tp);
	/* A[j]     <- A[j] + omega^j A[j+K/2]
	   A[j+K/2] <- A[j] + omega^(j+K/2) A[j+K/2] */
        for (j=0;j<K2;j++,Ap++,Bp++) {
	  MPN_COPY(tp, Bp[0], n+1);
	  mpn_fft_mul_2exp_modF(Bp[0], (j+K2)*omega, n, tmp);
	  mpn_fft_add_modF(Bp[0], Ap[0], n);
	  mpn_fft_mul_2exp_modF(tp, j*omega, n, tmp);
	  mpn_fft_add_modF(Ap[0], tp, n);
	}
	TMP_FREE(marker);
    }
}


/* A <- A/2^k mod 2^(n*BITS_PER_MP_LIMB)+1 */
static void
#if __STDC__
mpn_fft_div_2exp_modF (mp_limb_t *ap, int k, mp_size_t n, mp_limb_t *tp)
#else
mpn_fft_div_2exp_modF(ap,k,n,tp)
     mp_limb_t *ap,*tp;
     int       k;
     mp_size_t n;
#endif
{
    int i;
    
    i = 2*n*BITS_PER_MP_LIMB;
    i = (i-k) % i;
    mpn_fft_mul_2exp_modF(ap,i,n,tp); 
    /* 1/2^k = 2^(2nL-k) mod 2^(n*BITS_PER_MP_LIMB)+1 */
    /* normalize so that A < 2^(n*BITS_PER_MP_LIMB)+1 */
    if (ap[n]==1) {
      for (i=0;i<n && ap[i]==0;i++);
      if (i<n) {
	ap[n]=0;
	mpn_sub_1(ap, ap, n, 1);
      }
    }
}


/* R <- A mod 2^(n*BITS_PER_MP_LIMB)+1, n<=an<=3*n */
static void
#if __STDC__
mpn_fft_norm_modF(mp_limb_t *rp, mp_limb_t *ap, mp_size_t n, mp_size_t an) 
#else
mpn_fft_norm_modF(rp, ap, n, an)
     mp_limb_t *rp;
     mp_limb_t *ap;
     mp_size_t n;
     mp_size_t an;
#endif
{
  mp_size_t l;

   if (an>2*n) {
     l = n;
     rp[n] = mpn_add_1(rp+an-2*n, ap+an-2*n, 3*n-an, 
		       mpn_add_n(rp,ap,ap+2*n,an-2*n));
   }
   else {
     l = an-n;
     MPN_COPY(rp, ap, n);
     rp[n]=0;
   }
   if (mpn_sub_n(rp,rp,ap+n,l)) {
     if (mpn_sub_1(rp+l,rp+l,n+1-l,1))
       rp[n]=mpn_add_1(rp,rp,n,1); 
   }
}


static void
#if __STDC__
mpn_mul_fft_internal(mp_limb_t *op, mp_srcptr n, mp_srcptr m, mp_size_t pl,
                     int k, int K,
                     mp_limb_t **Ap, mp_limb_t **Bp,
                     mp_limb_t *A, mp_limb_t *B,
                     mp_size_t nprime, mp_size_t l, mp_size_t Mp,
                     int **_fft_l,
                     mp_limb_t *T, int rec)
#else
mpn_mul_fft_internal(op,n,m,pl,k,K,Ap,Bp,A,B,nprime,l,Mp,_fft_l,T,rec)
     mp_limb_t *op;
     mp_srcptr n, m;
     mp_limb_t **Ap,**Bp,*A,*B,*T;
     mp_size_t pl,nprime;
     int       **_fft_l;
     int       k,K,l,Mp,rec;
#endif
{
  int       i, sqr, pla, lo, sh, j;
  mp_limb_t *p;

    sqr = (n==m);

    TRACE (printf ("pl=%d k=%d K=%d np=%d l=%d Mp=%d rec=%d sqr=%d\n",
                   pl,k,K,nprime,l,Mp,rec,sqr));

    /* decomposition of inputs into arrays Ap[i] and Bp[i] */
    if (rec) for (i=0;i<K;i++) {
      Ap[i] = A+i*(nprime+1); Bp[i] = B+i*(nprime+1);
      /* store the next M bits of n into A[i] */
      /* supposes that M is a multiple of BITS_PER_MP_LIMB */
      MPN_COPY(Ap[i], n, l); n+=l; MPN_ZERO(Ap[i]+l, nprime+1-l);
      /* set most significant bits of n and m (important in recursive calls) */
      if (i==K-1) Ap[i][l]=n[0];
      mpn_fft_mul_2exp_modF(Ap[i], i*Mp, nprime, T);
      if (!sqr) {
	MPN_COPY(Bp[i], m, l); m+=l; MPN_ZERO(Bp[i]+l, nprime+1-l);
	if (i==K-1) Bp[i][l]=m[0];
	mpn_fft_mul_2exp_modF(Bp[i], i*Mp, nprime, T);
      }
    }

    /* direct fft's */
    if (sqr) mpn_fft_fft_sqr(Ap,K,_fft_l+k,2*Mp,nprime,1, T);
    else mpn_fft_fft(Ap,Bp,K,_fft_l+k,2*Mp,nprime,1, T);

    /* term to term multiplications */
    mpn_fft_mul_modF_K(Ap, (sqr) ? Ap : Bp, nprime, K);

    /* inverse fft's */
    mpn_fft_fftinv(Ap, K, 2*Mp, nprime, T);

    /* division of terms after inverse fft */
    for (i=0;i<K;i++) mpn_fft_div_2exp_modF(Ap[i],k+((K-i)%K)*Mp,nprime, T);

    /* addition of terms in result p */
    MPN_ZERO(T,nprime+1); 
    pla = l*(K-1)+nprime+1; /* number of required limbs for p */
    p = B; /* B has K*(n'+1) limbs, which is >= pla, i.e. enough */
    MPN_ZERO(p, pla);
    sqr=0; /* will accumulate the (signed) carry at p[pla] */
    for (i=K-1,lo=l*i+nprime,sh=l*i;i>=0;i--,lo-=l,sh-=l) {
        mp_ptr n = p+sh;
	j = (K-i)%K;
	if (mpn_add_n(n,n,Ap[j],nprime+1))
	  sqr += mpn_add_1(n+nprime+1,n+nprime+1,pla-sh-nprime-1,1);
	T[2*l]=i+1; /* T = (i+1)*2^(2*M) */
	if (mpn_cmp(Ap[j],T,nprime+1)>0) { /* subtract 2^N'+1 */
	  sqr -= mpn_sub_1(n,n,pla-sh,1);
	  sqr -= mpn_sub_1(p+lo,p+lo,pla-lo,1);
	}
    }
    if (sqr==-1) {
      if ((sqr=mpn_add_1(p+pla-pl,p+pla-pl,pl,1))) {
	/* p[pla-pl]...p[pla-1] are all zero */
        mpn_sub_1(p+pla-pl-1,p+pla-pl-1,pl+1,1);
	mpn_sub_1(p+pla-1,p+pla-1,1,1);
      }
    }
    else if (sqr==1) {
	    if (pla>=2*pl)
	      while ((sqr=mpn_add_1(p+pla-2*pl,p+pla-2*pl,2*pl,sqr)));
	    else {
	      sqr = mpn_sub_1(p+pla-pl,p+pla-pl,pl,sqr);
              ASSERT (sqr == 0);
	    }
    }
    else
      ASSERT (sqr == 0);

    /* here p < 2^(2M) [K 2^(M(K-1)) + (K-1) 2^(M(K-2)) + ... ]
              < K 2^(2M) [2^(M(K-1)) + 2^(M(K-2)) + ... ]
	      < K 2^(2M) 2^(M(K-1))*2 = 2^(M*K+M+k+1) */
    mpn_fft_norm_modF(op,p,pl,pla);
}


/* op <- n*m mod 2^N+1 with fft of size 2^k where N=pl*BITS_PER_MP_LIMB
   n and m have respectively nl and ml limbs
   op must have space for pl+1 limbs
   One must have pl = mpn_fft_next_size(pl, k).
*/

void
#if __STDC__
mpn_mul_fft (mp_ptr op, mp_size_t pl,
             mp_srcptr n, mp_size_t nl,
             mp_srcptr m, mp_size_t ml,
             int k)
#else
mpn_mul_fft (op, pl, n, nl, m, ml, k)
     mp_ptr    op;
     mp_size_t pl;
     mp_srcptr n;
     mp_size_t nl;
     mp_srcptr m;
     mp_size_t ml;
     int k;
#endif
{
    int        K,maxLK,i,j;
    mp_size_t  N,Nprime,nprime,M,Mp,l;
    mp_limb_t  **Ap,**Bp,*A,*T,*B;
    int        **_fft_l;
    int        sqr = (n==m && nl==ml);
    TMP_DECL(marker);

    TRACE (printf ("\nmpn_mul_fft pl=%ld nl=%ld ml=%ld k=%d\n",
                   pl, nl, ml, k));
    ASSERT_ALWAYS (mpn_fft_next_size(pl, k) == pl);

    TMP_MARK(marker);
    N = pl*BITS_PER_MP_LIMB;
    _fft_l = TMP_ALLOC_TYPE (k+1, int*);
    for (i=0;i<=k;i++)
      _fft_l[i] = TMP_ALLOC_TYPE (1<<i, int);
    mpn_fft_initl(_fft_l, k);
    K = 1<<k;
    M = N/K;	/* N = 2^k M */
    l = M/BITS_PER_MP_LIMB;
    maxLK = (K>BITS_PER_MP_LIMB) ? K : BITS_PER_MP_LIMB;

    Nprime = ((2*M+k+2+maxLK)/maxLK)*maxLK; /* ceil((2*M+k+3)/maxLK)*maxLK; */
    nprime = Nprime/BITS_PER_MP_LIMB; 
    TRACE (printf ("N=%d K=%d, M=%d, l=%d, maxLK=%d, Np=%d, np=%d\n",
                   N, K, M, l, maxLK, Nprime, nprime));
    if (nprime >= (sqr ? FFT_MODF_SQR_THRESHOLD : FFT_MODF_MUL_THRESHOLD)) {
      maxLK = (1<<mpn_fft_best_k(nprime,n==m))*BITS_PER_MP_LIMB;
      if (Nprime % maxLK) {
	Nprime=((Nprime/maxLK)+1)*maxLK;
	nprime = Nprime/BITS_PER_MP_LIMB;
      }
      TRACE (printf ("new maxLK=%d, Np=%d, np=%d\n", maxLK, Nprime, nprime));
    }

    T = TMP_ALLOC_LIMBS (nprime+1);
    Mp = Nprime/K;

    TRACE (printf("%dx%d limbs -> %d times %dx%d limbs (%1.2f)\n",
                  pl,pl,K,nprime,nprime,2.0*(double)N/Nprime/K);
           printf("   temp space %ld\n", 2*K*(nprime+1)));

    A = _MP_ALLOCATE_FUNC_LIMBS (2*K*(nprime+1));
    B = A+K*(nprime+1);
    Ap = TMP_ALLOC_MP_PTRS (K); 
    Bp = TMP_ALLOC_MP_PTRS (K); 
    /* special decomposition for main call */
    for (i=0;i<K;i++) {
      Ap[i] = A+i*(nprime+1); Bp[i] = B+i*(nprime+1);
      /* store the next M bits of n into A[i] */
      /* supposes that M is a multiple of BITS_PER_MP_LIMB */
      if (nl>0) {
	j = (nl>=l) ? l : nl; /* limbs to store in Ap[i] */
	MPN_COPY(Ap[i], n, j); n+=l; MPN_ZERO(Ap[i]+j, nprime+1-j);
	mpn_fft_mul_2exp_modF(Ap[i], i*Mp, nprime, T);
      }
      else MPN_ZERO(Ap[i], nprime+1);
      nl -= l;
      if (n!=m) {
	if (ml>0) {
	  j = (ml>=l) ? l : ml; /* limbs to store in Bp[i] */
	  MPN_COPY(Bp[i], m, j); m+=l; MPN_ZERO(Bp[i]+j, nprime+1-j);
	  mpn_fft_mul_2exp_modF(Bp[i], i*Mp, nprime, T);
	}
	else MPN_ZERO(Bp[i], nprime+1);
      }
      ml -= l;
    }
    mpn_mul_fft_internal(op,n,m,pl,k,K,Ap,Bp,A,B,nprime,l,Mp,_fft_l,T,0);
    TMP_FREE(marker);
    _MP_FREE_FUNC_LIMBS (A, 2*K*(nprime+1));
}


#if WANT_ASSERT
static int
#if __STDC__
mpn_zero_p (mp_ptr p, mp_size_t n)
#else
     mpn_zero_p (p, n)
     mp_ptr p;
     mp_size_t n;
#endif
{
  mp_size_t i;

  for (i = 0; i < n; i++)
    {
      if (p[i] != 0)
        return 0;
    }

  return 1;
}
#endif


/* Multiply {n,nl}*{m,ml} and write the result to {op,nl+ml}.

   FIXME: Duplicating the result like this is wasteful, do something better
   perhaps at the norm_modF stage above. */

void
#if __STDC__
mpn_mul_fft_full (mp_ptr op,
                  mp_srcptr n, mp_size_t nl,
                  mp_srcptr m, mp_size_t ml)
#else
mpn_mul_fft_full (op, n, nl, m, ml)
     mp_ptr    op;
     mp_srcptr n;
     mp_size_t nl;
     mp_srcptr m;
     mp_size_t ml;
#endif
{
  mp_ptr     pad_op;
  mp_size_t  pl;
  int        k;
  int        sqr = (n==m && nl==ml);

  k = mpn_fft_best_k (nl+ml, sqr);
  pl = mpn_fft_next_size (nl+ml, k);

  TRACE (printf ("mpn_mul_fft_full nl=%ld ml=%ld -> pl=%ld k=%d\n",
                 nl, ml, pl, k));

  pad_op = _MP_ALLOCATE_FUNC_LIMBS (pl+1);
  mpn_mul_fft (pad_op, pl, n, nl, m, ml, k);

  ASSERT (mpn_zero_p (pad_op+nl+ml, pl+1-(nl+ml)));
  MPN_COPY (op, pad_op, nl+ml);

  _MP_FREE_FUNC_LIMBS (pad_op, pl+1);
}
