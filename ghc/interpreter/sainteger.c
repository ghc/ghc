
/* --------------------------------------------------------------------------
 * Yet another implementation of Integer
 *
 * Copyright (c) Glasgow University, 1999.
 * All rights reserved. See NOTICE for details and conditions of use etc...
 * ------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#include "sainteger.h"


/* --------------------------------------------------------------------------
 * Local fns
 * ------------------------------------------------------------------------*/

typedef unsigned char uchar;
typedef unsigned short ush;


static int maxused_add ( B*, B* );
static int maxused_sub ( B*, B* );
static int maxused_mul ( B*, B* );
static int maxused_qrm ( B*, B* );
static int maxused_neg ( B* );

static int  ucmp ( B*, B* );
static void uadd ( B*, B*, B* );
static void usub ( B*, B*, B* );
static void umul ( B*, B*, B* );
static void uqrm ( B*, B*, B*, B* );

/*#define DEBUG_SAINTEGER*/
/*#define DEBUG_SAINTEGER_UQRM*/


#ifdef DEBUG_SAINTEGER
#define myassert(zzzz) assert(zzzz)
#else
#define myassert(zzzz) /* */
#endif


/* --------------------------------------------------------------------------
 * Basics
 * ------------------------------------------------------------------------*/

void pp ( B* x )
{
   int i;
   printf ( "sign=%2d  used=%d  size=%d   ", x->sign, x->used, x->size );
   for (i = x->used-1; i >= 0; i--)
      printf ( "%2x ", (int)(x->stuff[i]) );
   printf ( "\n" );
}


static int sane ( B* x )
{
   int i;

   if (x->sign == 0 && x->used != 0) return 0;
   if (x->sign != -1 && x->sign != 0 && x->sign != 1) return 0;

   if (x->used < 0) return 0;
   if (x->size < 0) return 0;
   if (x->used > x->size) return 0;
   if (x->used == 0) return 1;
   if (x->stuff[x->used-1] == 0) return 0;
   for (i = 0; i < x->used; i++)
      if (x->stuff[i] >= B_BASE) return 0;
   return 1;
}


int is_sane ( B* x )
{
   return sane(x);
}


static void u_renormalise ( B* b )
{
   while (b->used > 0 && b->stuff[b->used-1] == 0) b->used--;
   if (b->used == 0) b->sign = 0; else b->sign = 1;
}


void do_renormalise ( B* b )
{
   while (b->used > 0 && b->stuff[b->used-1] == 0) b->used--;
   if (b->used == 0) b->sign = 0;
}

/* --------------------------------------------------------------------------
 * Size of things
 * ------------------------------------------------------------------------*/

static int maxused_add ( B* x, B* y )
{
   myassert(sane(x));
   myassert(sane(y));
   return 1 + (x->used > y->used ? x->used : y->used);
}

static int maxused_sub ( B* x, B* y )
{
   myassert(sane(x));
   myassert(sane(y));
   return 1 + (x->used > y->used ? x->used : y->used);
}

static int maxused_mul ( B* x, B* y )
{
   myassert(sane(x));
   myassert(sane(y));
   return x->used + y->used;
}

static int maxused_qrm ( B* x, B* y )
{
   myassert(sane(x));
   myassert(sane(y));
   return (x->used > y->used ? x->used : y->used);
}

static int maxused_neg ( B* x )
{
   myassert(sane(x));
   return x->used;
}


/* quick, safe approx */
static int maxused_fromInt ( int sizeof_int )
{
   if (B_BASE == 256)  return     sizeof_int;
   if (B_BASE >= 16)   return 2 * sizeof_int;
   if (B_BASE >= 4)    return 4 * sizeof_int;
   /* (B_BASE >= 2) */ return 8 * sizeof_int;
}

/* ditto */
static int maxused_fromStr ( char* str )
{
   int nd = 0;
   if (*str == '-') str++;
   while (isdigit((int)(*str))) { str++; nd++; };

   if (B_BASE >= 100) return ((nd+1) / 2);
   if (B_BASE >= 10)  return nd;
   /* (B_BASE >= 2)*/ return 4 * nd;
}


int size_add ( B* x, B* y )
{
   return sizeof(B) + maxused_add(x,y);
}

int size_sub ( B* x, B* y )
{ 
   return sizeof(B) + maxused_sub(x,y); 
}

int size_mul ( B* x, B* y )
{
   return sizeof(B) + maxused_mul(x,y); 
}

int size_qrm ( B* x, B* y )
{
   return sizeof(B) + maxused_qrm(x,y); 
}

int size_neg ( B* x )
{
   return sizeof(B) + maxused_neg(x); 
}

int size_fromInt ( void )
{
   int sizeof_int = sizeof(int);
   return sizeof(B) + maxused_fromInt ( sizeof_int );
}

int size_fromWord ( void )
{
   int sizeof_word = sizeof(unsigned int);
   return sizeof(B) + maxused_fromInt ( sizeof_word );
}

int size_fromStr ( char* str )
{
   return sizeof(B) + maxused_fromStr ( str );
}

int size_fltmantissa ( void )
{
   return sizeof(B) + sizeof(float);
}

int size_dblmantissa ( void )
{
   return sizeof(B) + sizeof(double);
}


/* --------------------------------------------------------------------------
 * Conversions
 * ------------------------------------------------------------------------*/

void do_fromInt  ( int n, int sizeRes, B* res )
{
 
   res->size = sizeRes - sizeof(B);
   res->sign = res->used = 0;
   if (n == 0) { myassert(sane(res)); return; };
   if (n < 0) res->sign = -1; else res->sign = 1;
   if (n < 0) n = -n;

   while (n != 0) {
      res->stuff[res->used] = (uchar)(n % B_BASE);
      n /= B_BASE;
      res->used++;
   }
   myassert(sane(res));
}

void do_fromWord  ( unsigned int n, int sizeRes, B* res )
{
 
   res->size = sizeRes - sizeof(B);
   res->sign = res->used = 0;
   if (n == 0) { myassert(sane(res)); return; };
   res->sign = 1;

   while (n != 0) {
      res->stuff[res->used] = (uchar)(n % B_BASE);
      n /= B_BASE;
      res->used++;
   }
   myassert(sane(res));
}

/* NOTE: This only works currectly if B_BASE >= 10 */
void do_fromStr ( char* str, int sizeRes, B* res )
{
   int sign, d, t, j, carry;

   res->size = sizeRes - sizeof(B);
   res->sign = res->used = 0;
   sign = 1;
   if (*str == '-') { str++; sign = -1; };

   while (isdigit((int)(*str))) {

      /* multiply res by 10 */
      carry = 0;
      for (j = 0; j < res->used; j++) {
         t = 10 * res->stuff[j] + carry;
         res->stuff[j] = t % B_BASE;
         carry = t / B_BASE;
      }
      myassert(carry < B_BASE);
      if (carry > 0)
         res->stuff[res->used++] = carry;

      /* add a digit on */
      d = *str - '0';
      str++;

      carry = d;
      for (j = 0; j < res->used; j++) {
         carry += res->stuff[j];
         res->stuff[j] = carry % B_BASE;
         carry /= B_BASE;
         if (carry == 0) break;
      }
      if (carry > 0)
         res->stuff[res->used++] = carry;
   }

   res->sign = sign;
   myassert(sane(res));
}

int do_toInt ( B* x )
{
   int i, d, res;
   if (x->sign == 0) return 0;
   res = 0;
   for (i = x->used-1; i >= 0; i--) {
      d = x->stuff[i];
      res = res * B_BASE + d;
   }
   if (x->sign < 0) res = -res;
   return res;
}

unsigned int do_toWord ( B* x )
{
   int i, d;
   unsigned int res;
   if (x->sign == 0) return 0;
   res = 0;
   for (i = x->used-1; i >= 0; i--) {
      d = x->stuff[i];
      res = res * B_BASE + d;
   }
   return res;
}

float do_toFloat ( B* x )
{
   int i, d;
   float res;
   if (x->sign == 0) return 0.0;
   res = 0.0;
   for (i = x->used-1; i >= 0; i--) {
      d = x->stuff[i];
      res = res * B_BASE_FLT + d;
   }
   if (x->sign < 0) res = -res;
   return res;
}

double do_toDouble ( B* x )
{
   int i, d;
   double res;
   if (x->sign == 0) return 0.0;
   res = 0.0;
   for (i = x->used-1; i >= 0; i--) {
      d = x->stuff[i];
      res = res * B_BASE_FLT + d;
   }
   if (x->sign < 0) res = -res;
   return res;
}


/* --------------------------------------------------------------------------
 * Signed ops
 * ------------------------------------------------------------------------*/

/* A helper for signed + and -.  sdiff(x,y) ignores the signs of x and y
   sets p to the signed value abs(x)-abs(y).
*/
static void sdiff ( B* x, B* y, B* res )
{
   int t;
   myassert(sane(x));
   myassert(sane(y));
   myassert(res->size == maxused_sub(x,y));
   t = ucmp(x,y);
   if (t == 0) { res->sign = res->used = 0; return; }
   if (t == -1) {
      /* x < y */
      usub(y,x,res);
      res->sign = -1;
   } else {
      /* x > y */
      usub(x,y,res);
      res->sign = 1;
   }
   myassert(sane(res));
}

int do_getsign ( B* x )
{
   myassert(sane(x));
   return x->sign;
}

void do_neg ( B* x, int sizeRes, B* res )
{
   int i;
   myassert(sane(x));
   res->size = sizeRes - sizeof(B);
   res->used = x->used;
   for (i = 0; i < x->used; i++) 
      res->stuff[i] = x->stuff[i];
   res->sign = - (x->sign);
}

void do_add ( B* x, B* y, int sizeRes, B* res )
{
   myassert(sane(x));
   myassert(sane(y));
   res->size = sizeRes - sizeof(B);
   res->used = res->sign = 0;

   if ( (x->sign >= 0 && y->sign >= 0) ||
        (x->sign < 0  && y->sign < 0)) {
      /* same sign; add magnitude and clone sign */
      uadd(x,y,res);
      if (x->sign < 0 && res->sign != 0) res->sign = -1;
   } 
   else 
   /* signs differ; employ sdiff */
   if (x->sign >= 0 && y->sign < 0) {
      sdiff(x,y,res);      
   } else {
      myassert(x->sign < 0 && y->sign >= 0);
      sdiff(y,x,res);
   }
   myassert(sane(res));
}

void do_sub ( B* x, B* y, int sizeRes, B* res )
{
   myassert(sane(x));
   myassert(sane(y));
   res->size = sizeRes - sizeof(B);
   res->used = res->sign = 0;

   if ( (x->sign >= 0 && y->sign < 0) ||
        (x->sign < 0  && y->sign >= 0)) {
      /* opposite signs; add magnitudes and clone sign of x */
      uadd(x,y,res);
      myassert(res->sign != 0);
      if (x->sign < 0) res->sign = -1;
   } 
   else
   /* signs are the same; employ sdiff */
   if (x->sign >= 0 && y->sign >= 0) {
      sdiff(x,y,res);
   } else {
      myassert(x->sign < 0 && y->sign < 0);
      sdiff(y,x,res);
   }
   myassert(sane(res));
}


void do_mul ( B* x, B* y, int sizeRes, B* res )
{
   myassert(sane(x));
   myassert(sane(y));
   res->size = sizeRes - sizeof(B);
   res->used = res->sign = 0;

   if (x->sign == 0 || y->sign == 0) {
      res->sign = res->used = 0;
      myassert(sane(res));
      return;
   }
   umul(x,y,res);
   if (x->sign != y->sign) res->sign = -1;
   myassert(sane(res));
}


void do_qrm ( B* x, B* y, int sizeRes, B* q, B* r )
{
   myassert(sane(x));
   myassert(sane(y));

   q->size = r->size = sizeRes - sizeof(B);
   q->used = r->used = q->sign = r->sign = 0;

   if (y->sign == 0) {
      fprintf(stderr, "do_qrm: division by zero -- exiting now!\n");
      exit(1);
      return;
   }

   if (x->sign == 0) {
      q->used = r->used = q->sign = r->sign = 0;
      myassert(sane(q)); myassert(sane(r));
      return;
   }

   uqrm ( x, y, q, r );
   if (x->sign != y->sign && q->sign != 0) q->sign = -1;   
   if (x->sign == -1 && r->sign != 0) r->sign = -1;

   myassert(sane(q)); myassert(sane(r));
}

int do_cmp ( B* x, B* y )
{
   if (!sane(x)) 
      pp(x);
   myassert(sane(x));
   myassert(sane(y));
   if (x->sign < y->sign) return -1;
   if (x->sign > y->sign) return 1;
   myassert(x->sign == y->sign);
   if (x->sign == 0) return 0;
   if (x->sign == 1) return ucmp(x,y); else return ucmp(y,x);
}


/* --------------------------------------------------------------------------
 * Unsigned ops
 * ------------------------------------------------------------------------*/

static int ucmp ( B* x, B* y )
{
   int i;
   myassert(sane(x));
   myassert(sane(y));
   if (x->used < y->used) return -1;
   if (x->used > y->used) return 1;
   for (i = x->used-1; i >= 0; i--) {
      if (x->stuff[i] < y->stuff[i]) return -1;
      if (x->stuff[i] > y->stuff[i]) return 1;
   }
   return 0;  
}



static void uadd ( B* x, B* y, B* res )
{
   int c, i, t, n;
   B* longer;

   myassert(sane(x));
   myassert(sane(y));
   myassert (res->size == maxused_add(x,y));
   res->used = res->size;
   res->stuff[res->used-1] = 0;

   if (x->used > y->used) {
      n = y->used;
      longer = x;
   } else {
      n = x->used;
      longer = y;
   }

   c = 0;
   for (i = 0; i < n; i++) {
      t = x->stuff[i] + y->stuff[i] + c;
      if (t >= B_BASE) {
         res->stuff[i] = t-B_BASE;
         c = 1;
      } else {
         res->stuff[i] = t;
         c = 0;
      }
   }

   for (i = n; i < longer->used; i++) {
      t = longer->stuff[i] + c;
      if (t >= B_BASE) {
         res->stuff[i] = t-B_BASE;
      } else {
         res->stuff[i] = t;
         c = 0;
      }
   }
   if (c > 0) {
      myassert(res->used == longer->used+1);
      res->stuff[longer->used] = c;
   }

   u_renormalise(res);
   myassert(sane(res));
}


static void usub ( B* x, B* y, B* res )
{
   int b, i, t;
   myassert(sane(x));
   myassert(sane(y));
   myassert (x->used >= y->used);
   myassert (res->size == maxused_sub(x,y));

   b = 0;
   for (i = 0; i < y->used; i++) {
      t = x->stuff[i] - y->stuff[i] - b;
      if (t < 0) {
         res->stuff[i] = t + B_BASE;
         b = 1;
      } else {
         res->stuff[i] = t;
         b = 0;
      }
   }

   for (i = y->used; i < x->used; i++) {
      t = x->stuff[i] - b;
      if (t < 0) {
         res->stuff[i] = t + B_BASE;
      } else {
         res->stuff[i] = t;
         b = 0;
      }
   }
   myassert (b == 0);

   res->used = x->used;
   u_renormalise(res);
   myassert(sane(res));
}


void umul ( B* x, B* y, B* res )
{
   int i, j, carry;

   myassert(sane(x));
   myassert(sane(y));
   myassert(res->size == maxused_mul(x,y));

   for (j = 0; j < y->used; j++) res->stuff[j] = 0;

   for (i = 0; i < x->used; i++) {
      carry = 0;
      for (j = 0; j < y->used; j++) {
         carry += res->stuff[i+j] + x->stuff[i]*y->stuff[j];
         res->stuff[i+j] = carry % B_BASE;
         carry /= B_BASE;
         myassert (carry < B_BASE);
      }
      res->stuff[i+y->used] = carry;
   }

   res->used = x->used+y->used;
   u_renormalise(res);
   myassert(sane(res));
}


static void uqrm ( B* dend, B* isor, B* dres, B* mres )
{
   int i, j, t, vh, toolarge, delta, carry, scaleup;
   uchar *dend_stuff, *isor_stuff, *tmp;

   myassert(sane(isor));
   myassert(sane(dend));
   myassert(isor->used > 0);  // against division by zero

   myassert(dres->size == maxused_qrm(isor,dend));
   myassert(mres->size == maxused_qrm(isor,dend));

   if (dend->used < isor->used) {
      // Result of division must be zero, since dividend has
      // fewer digits than the divisor.  Remainder is the
      // original dividend.
      dres->used = 0;
      mres->used = dend->used;
      for (j = 0; j < mres->used; j++) mres->stuff[j] = dend->stuff[j];
      u_renormalise(dres); u_renormalise(mres);
      myassert(sane(dres));
      myassert(sane(mres));
      return;
   }

   if (isor->used == 1) {

      // Simple case; divisor is a single digit
      carry = 0;
      for (j = dend->used-1; j >= 0; j--) {
         carry += dend->stuff[j];
         dres->stuff[j] = carry/isor->stuff[0];
         carry = B_BASE*(carry%isor->stuff[0]);
      }
      carry /= B_BASE;
      dres->used = dend->used;
      u_renormalise(dres);

      // Remainder is the final carry value
      mres->used = 0;
      if (carry > 0) {
         mres->used = 1;
         mres->stuff[0] = carry;
      }
      u_renormalise(dres); u_renormalise(mres);
      myassert(sane(dres));
      myassert(sane(mres));
      return;

   } else {

      // Complex case: both dividend and divisor have two or more digits.
      myassert(isor->used >= 2);
      myassert(dend->used >= 2);

      // Allocate space for a copy of both dividend and divisor, since we 
      // need to mess with them.  Also allocate tmp as a place to hold
      // values of the form   quotient_digit * divisor.
      dend_stuff = malloc ( sizeof(uchar)*(dend->used+1) );
      isor_stuff = malloc ( sizeof(uchar)*isor->used     );
      tmp        = malloc ( sizeof(uchar)*(isor->used+1) );
      myassert (dend_stuff && isor_stuff && tmp);
      
      // Calculate a scaling-up factor, and multiply both divisor and 
      // dividend by it.  Doing this reduces the number of corrections
      // needed to the quotient-digit-estimates made in the loop below,
      // and thus speeds up division, but is not actually needed to
      // get the correct results.  The scaleup factor should not increase
      // the number of digits needed to represent either the divisor
      // (since the factor is derived from it) or the dividend (since
      // we already gave it a new leading zero).
      scaleup = B_BASE / (1 + isor->stuff[isor->used-1]);
      myassert (1 <= scaleup && scaleup <= B_BASE/2);

      if (scaleup == 1) {
         // Don't bother to multiply; just copy.
         for (j = 0; j < dend->used; j++) dend_stuff[j] = dend->stuff[j];
         for (j = 0; j < isor->used; j++) isor_stuff[j] = isor->stuff[j];

         // Extend dividend with leading zero.
         dend_stuff[dend->used] = tmp[isor->used] = 0;

      } else {
         carry = 0;
         for (j = 0; j < isor->used; j++) {
            t = scaleup * isor->stuff[j] + carry;
            isor_stuff[j] = t % B_BASE;
            carry = t / B_BASE;
         }
         myassert (carry == 0);

         carry = 0;
         for (j = 0; j < dend->used; j++) {
            t = scaleup * dend->stuff[j] + carry;
            dend_stuff[j] = t % B_BASE;
            carry = t / B_BASE;
         }
         dend_stuff[dend->used] = carry;
         tmp[isor->used] = 0;
      }

      // For each quotient digit ...
      for (i = dend->used; i >= isor->used; i--) {
         myassert (i-2 >= 0);
         myassert (i <= dend->used);
         myassert (isor->used >= 2);

#if DEBUG_SAINTEGER_UQRM
	 printf("\n---------\nqdigit %d\n", i );
	 printf("dend_stuff is "); 
         for (j = dend->used; j>= 0; j--) printf("%d ",dend_stuff[j]);
	 printf("\n");
#endif
	 // Make a guess vh of the quotient digit
         vh = (B_BASE*B_BASE*dend_stuff[i] + B_BASE*dend_stuff[i-1] + dend_stuff[i-2])
              /
              (B_BASE*isor_stuff[isor->used-1] + isor_stuff[isor->used-2]);
         if (vh > B_BASE-1) vh = B_BASE-1;
#if DEBUG_SAINTEGER_UQRM
	 printf("guess formed from %d %d %d   %d %d\n", 
                 dend_stuff[i], dend_stuff[i-1] , dend_stuff[i-2], 
                 isor_stuff[isor->used-1], isor_stuff[isor->used-2]);
	 printf("guess is %d\n", vh );
#endif
         // Check if vh is too large (by 1).  Calculate vh * isor into tmp
         // and see if it exceeds the same length prefix of dend.  If so, 
         // vh needs to be decremented.
         carry = 0;
         for (j = 0; j < isor->used; j++) {
            t = vh * isor_stuff[j] + carry;
            tmp[j] = t % B_BASE;
            carry = t / B_BASE;
         }
         tmp[isor->used] = carry;
         delta = i - isor->used;
#if DEBUG_SAINTEGER_UQRM
	 printf("final carry is %d\n", carry);
	 printf("vh * isor is " );
         for (j = isor->used; j >=0; j--) printf("%d ",tmp[j]);printf("\n");
	 printf("delta = %d\n", delta );
#endif
         toolarge = 0;
         for (j = isor->used; j >= 0; j--) {
#if DEBUG_SAINTEGER_UQRM
            printf ( "(%d,%d)  ", (int)(tmp[j]), (int)(dend_stuff[j+delta]) );
#endif
            if (tmp[j] > dend_stuff[j+delta]) {toolarge=1; break;};
            if (tmp[j] < dend_stuff[j+delta]) break;
	 }

         // If we did guess too large, decrement vh and subtract a copy of
         // isor from tmp.  This had better not go negative!
         if (toolarge) {
#if DEBUG_SAINTEGER_UQRM
	    printf ( "guess too large\n" );
#endif
            vh--;
            carry = 0;
            for (j = 0; j < isor->used; j++) {
               if (carry + isor_stuff[j] > tmp[j]) {
                  tmp[j] = (B_BASE + tmp[j]) - isor_stuff[j] - carry;
                  carry = 1;
               } else {
                  tmp[j] = tmp[j] - isor_stuff[j] - carry;
                  carry = 0;
               }
            }
	    //if (carry > 0) {pp(isor);pp(dend);};
            //myassert(carry == 0);
            if (carry > 0) {
               myassert(tmp[isor->used] > 0);
               tmp[isor->used]--;
            }
#if DEBUG_SAINTEGER_UQRM
	    printf("after adjustment of tmp ");
            for (j = isor->used; j >=0; j--) printf("%d ",tmp[j]);
            printf("\n");
#endif
	 }

         // Now vh really is the i'th quotient digit.  
         // Subtract (tmp << delta) from
         // the dividend.
         carry = 0;
         for (j = 0; j <= isor->used; j++) {
            if (carry + tmp[j] > dend_stuff[j+delta]) {
               dend_stuff[j+delta] = (B_BASE+dend_stuff[j+delta]) - tmp[j] - carry;
               carry = 1;
            } else {
               dend_stuff[j+delta] = dend_stuff[j+delta] - tmp[j] - carry;
               carry = 0;
            }
         }
         myassert(carry==0);

#if DEBUG_SAINTEGER_UQRM
         printf("after final sub ");
         for(j=dend->used; j>=0; j--) printf("%d ", dend_stuff[j]);
         printf("\n");
#endif

         // park vh in the result array
#if DEBUG_SAINTEGER_UDIV
         printf("[%d] <- %d\n", i-isor->used, vh );
#endif
         dres->stuff[i-isor->used] = vh;
      }
   }

   // Now we've got all the quotient digits.  Zap leading zeroes.
   dres->used = dend->used - isor->used + 1;
   u_renormalise(dres);
   myassert(sane(dres));

   // The remainder is in dend_stuff.  Copy, divide by the original scaling 
   // factor, and zap leading zeroes.
   mres->used = dend->used;
   for (j = 0; j < dend->used; j++) mres->stuff[j] = dend_stuff[j];
   u_renormalise(mres);
   myassert(sane(mres));

   if (scaleup > 1) {
      carry = 0;
      for (j = mres->used-1; j >= 0; j--) {
         carry += mres->stuff[j];
         mres->stuff[j] = carry/scaleup;
         carry = B_BASE*(carry%scaleup);
      }
      myassert (carry == 0);
      u_renormalise(mres);
      myassert(sane(mres));   
   }

   free(tmp);
   free(isor_stuff);
   free(dend_stuff);
}


/* --------------------------------------------------------------------------
 * Test framework
 * ------------------------------------------------------------------------*/

#if 0
int main ( int argc, char** argv )
{
   int i, j, t, k, m;
   B *bi, *bj, *bk, *bm;

   for (i = -10007; i <= 10007; i++) {
      printf ( "i = %d\n", i );

      t = size_fromInt(); bi = malloc(t); myassert(bi); 
      do_fromInt(i, t, bi);

      t = do_toInt(bi); myassert(i == t);

      for (j = -10007; j <= 10007; j++) {

         t = size_fromInt(); bj = malloc(t); myassert(bj); 
         do_fromInt(j, t, bj);

         t = do_toInt(bj); myassert(j == t);

         if (1) {
            t = size_add(bi,bj); bk = malloc(t); myassert(bk);
            do_add(bi,bj,t,bk);
            k = do_toInt(bk);
            if (i+j != k) {
               pp(bi); pp(bj); pp(bk);
               myassert(i+j == k);
            }
            free(bk);
         }

         if (1) {
            t = size_sub(bi,bj); bk = malloc(t); myassert(bk);
            do_sub(bi,bj,t,bk);
            k = do_toInt(bk); 
            if (i-j != k) {
               pp(bi); pp(bj); pp(bk);
               myassert(i-j == k);
            }
            free(bk);
         }

         if (1) {
            t = size_mul(bi,bj); bk = malloc(t); myassert(bk);
            do_mul(bi,bj,t,bk);
            k = do_toInt(bk); 
            if (i*j != k) {
               pp(bi); pp(bj); pp(bk);
               myassert(i*j == k);
            }
            free(bk);
         }

         if (j != 0) {
            t = size_qrm(bi,bj); 
            bk = malloc(t); myassert(bk); 
            bm = malloc(t); myassert(bm);
            do_qrm(bi,bj,t,bk,bm);
            k = do_toInt(bk);
            m = do_toInt(bm);
            myassert(k == i/j);
            myassert(m == i%j);
            free(bk); free(bm);
         }

         free(bj);
      }
      free(bi); 

   }
   printf("done\n");
   return 0;
}
#endif

#if 0
int main ( int argc, char** argv )
{
   B *a, *b, *c, *d, *e;
   a = fromInt(1); b=fromInt(9); pp(a); pp(b);
   c = mkB( maxused_uqrm(a,b) );
   d = mkB( maxused_uqrm(a,b) );
   e = mkB( maxused_uadd(a,b) );
   uadd(a,b,e); pp(e);
   //uqrm(a,b,c,d); pp(c); pp(d);

   return 0;
}
#endif

/*-------------------------------------------------------------------------*/
