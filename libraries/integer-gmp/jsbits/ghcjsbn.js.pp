/*
   GHCJS bignum library for integer-gmp package

   uses JavaScript arrays for big numbers
   some algorithms and code based on JSBN by Tom Wu

   Copyright Luite Stegeman 2016
 */

#include <ghcjs/rts.h>

// #define GHCJSBN_TRACE_INTEGER 1
// #define GHCJSBN_ASSERT_INTEGER 1

// bits per limb
#define GHCJSBN_BITS 28
#define GHCJSBN_MASK 0xfffffff
#define GHCJSBN_DV   0x10000000

// BI_FP = 52
// BI_FP - GHCJSBN_BITS
#define GHCJSBN_F1 24
// 2*GHCJSBN_BITS - BI_FP
#define GHCJSBN_F2 4
// 2 ^ BI_FP
#define GHCJSBN_FV 4503599627370496

// values for the Haskell Ordering enum
#define GHCJSBN_LT 0
#define GHCJSBN_EQ 1
#define GHCJSBN_GT 2

var h$ghcjsbn_zero_i     = MK_INTEGER_S(0);
var h$ghcjsbn_one_i      = MK_INTEGER_S(1);
var h$ghcjsbn_negOne_i   = MK_INTEGER_S(-1);
var h$ghcjsbn_null_b     = [-1];
var h$ghcjsbn_zero_b     = [0];
var h$ghcjsbn_one_b      = [1, 1];
var h$ghcjsbn_two31_b    = [2, 0, 8];
var h$ghcjsbn_czero_b    = [2, 268435455, 15];
var h$ghcjsbn_two31_i    = MK_INTEGER_Jp(h$ghcjsbn_two31_b);
var h$ghcjsbn_negTwo31_i = MK_INTEGER_S(-2147483648);

/******************************************************************************

 Types used here:
   - b BigNat:  array of limbs (each a number of GHCJSBN_BITS bits)
   - s Int:     small integer in range -2^31 .. 2^31-1
   - w Word:    small integer in range 0 .. 2^32-1,
                  values greater than 2^31-1 are stored as negative numbers
   - i Integer: Haskell Integer heap object, see invariants

 Integer invariants:
   - BigNat arrays do not have leading zeroes
   - Jp > S > Jn
   - S range: -2^31 .. 2^31-1 (-2147483648 .. 2147483647)

 ******************************************************************************/

#ifdef GHCJSBN_ASSERT_INTEGER
#define ASSERTVALID_I(i, msg) h$ghcjsbn_assertValid_i(i, msg)
#define ASSERTVALID_B(d, msg) h$ghcjsbn_assertValid_b(d, msg)
#define ASSERTVALID_S(s, msg) h$ghcjsbn_assertValid_s(s, msg)
#define ASSERTVALID_W(w, msg) h$ghcjsbn_assertValid_w(w, msg)
#define ASSERTVALID_D(d, msg) h$ghcjsbn_assertValid_d(d, msg)

// checks that the S,Jn,Jp constructor invariants hold
function h$ghcjsbn_assertValid_i(b, msg) {
  var sd, d, neg, i, n;
  // check global constants for unwanted mutations
  if(h$ghcjsbn_zero_b.length !== 1 || h$ghcjsbn_zero_b[0] !== 0) {
    throw new Error("zero_b mutated");
  }
  if(h$ghcjsbn_one_b.length !== 2 || h$ghcjsbn_one_b[0] !== 1 || h$ghcjsbn_one_b[1] !== 1) {
    throw new Error("one_b mutated");
  }
  if(IS_INTEGER_S(b)) {
    sd = INTEGER_S_DATA(b);
    if(typeof sd !== 'number')
      throw new Error("invalid small integer: not a number");
    if((sd|0) !== sd)
      throw new Error("invalid small integer: not a small int");
  } else {
    if(IS_INTEGER_Jp(b)) {
      neg = false;
    } else if(IS_INTEGER_Jn(b)) {
      neg = true;
    } else {
      throw new Error("invalid integer: unexpected constructor");
    }
    d = INTEGER_J_DATA(b);
    ASSERTVALID_B(d, "assertValid_i");
    if(d[0] < 2)
      throw new Error("invalid big integer: array too short");
    if(d[0] === 2) {
      if((d[2] >> (31-GHCJSBN_BITS)) === 0 ||
         (neg && d[2] === 0x20 && d[1] === 0))
        throw new Error("invalid big integer: in smallint range");
    }
    // everything ok
  }
}

// checks invariant for big number
function h$ghcjsbn_assertValid_b(d, msg) {
  var i, n;
  if(!Array.isArray(d))
    throw new Error("invalid big integer: not an array");
#ifdef GHCJSBN_TRACE_INTEGER
  var jb = h$ghcjsbn_tmp_toJSBN(d);
  if(msg) h$log("BigNat: " + msg + ": [" + d.join(",") + "] (" + jb.toString() + ")");
#endif
  if(typeof d[0] !== 'number' || d[0] > (d.length-1))
    throw new Error("invalid big integer: incorrect number of limbs");
  if(d[0] > 0 && d[d[0]] === 0)
    throw new Error("invalid big integer: leading zero");
  for(i = 1; i <= d[0]; i++) {
    n = d[i];
    if(typeof n !== 'number')
      throw new Error("invalid big integer: limb is not a number");
    if((n & GHCJSBN_MASK) !== n)
      throw new Error("invalid big integer: limb out of range");
  }
}

function h$ghcjsbn_assertValid_s(s, msg) {
  if(typeof s !== 'number')
    throw new Error("invalid int: not a number");
#ifdef GHCJSBN_TRACE_INTEGER
  if(msg) h$log("Int: " + msg + ": " + s);
#endif
  if((s|0) !== s)
    throw new Error("invalid int: not in smallint range");
}

function h$ghcjsbn_assertValid_w(w, msg) {
  if(typeof w !== 'number')
    throw new Error("invalid word: not a number");
#ifdef GHCJSBN_TRACE_INTEGER
  if(msg) h$log("Word: " + msg + ": " + w);
#endif
  if((w|0) !== w)
    throw new Error("invalid word: not in smallint range");
}

function h$ghcjsbn_assertValid_d(d, msg) {
  if(typeof d !== 'number')
    throw new Error("invalid double: not a number");
#ifdef GHCJSBN_TRACE_INTEGER
  if(msg) h$log("Double: " + msg + " : " + d);
#endif
}

#else
#define ASSERTVALID_I(i, msg)
#define ASSERTVALID_B(b, msg)
#define ASSERTVALID_S(s, msg)
#define ASSERTVALID_W(w, msg)
#define ASSERTVALID_D(d, msg)
#endif

/******************************************************************************/

///////////////////////////////////////////////////////////////////////////////
// the ghcjsbn_r functions operate on the raw array data directly
///////////////////////////////////////////////////////////////////////////////

#define GHCJS_SMALLPRIMES_MAX 1008

var h$ghcjsbn_smallPrimes =
 [   2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31,  37,  41,  43,  47
 ,  53,  59,  61,  67,  71,  73,  79,  83,  89,  97, 101, 103, 107, 109, 113
 , 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197
 , 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281
 , 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379
 , 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463
 , 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571
 , 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659
 , 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761
 , 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863
 , 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977
 , 983, 991, 997
 ];

var h$ghcjsbn_smallPrimesM = null;

function h$ghcjsbn_getSmallPrimesM() {
  var a, i;
  if(h$ghcjsbn_smallPrimesM === null) {
    a = [];
    for(i = 0; i < GHCJS_SMALLPRIMES_MAX; i++) {
      a[i] = false;
    }
    for(i = h$ghcjsbn_smallPrimes.length - 1; i >= 0; i--) {
      a[h$ghcjsbn_smallPrimes[i]] = true;
    }
    h$ghcjsbn_smallPrimesM = a;
  }
  return h$ghcjsbn_smallPrimesM;
}


// Int -> Int -> Bool
// fixme: seed
function h$ghcjsbn_isPrime_s(s, rounds) {
  if(s < 2 || (s > 2 && ((s&1) === 1))) return false;
  if(s <= GHCJS_SMALLPRIMES_MAX) {
    return h$ghcjsbn_getSmallPrimesM()[s];
  }
  throw new Error("isPrime_s");
}


// fixme: seed
function h$ghcjsbn_random_b(min, max) {
  if(h$ghcjsbn_cmp_bb(min, max) >= 0) {
    return min;
  }
  var range = h$ghcjsbn_sub_bb(max, min);
  var size = 4 + Math.ceil(h$ghcjsbn_nbits_b(range) / 16);
  var r = h$ghcjsbn_zero_b;
  for(var i=0;i<size;i++) {
    var rnd = Math.floor(Math.random()*65536);
    r = h$ghcjsbn_or_bb(h$ghcsbn_shl_b(r, 16), h$ghcjsbn_mkBigNat_w(rnd));
  }
  return h$ghcjsbn_add_bb(h$ghcjsbn_rem_bb(r, range), min);
}

function h$ghcjsbn_testPrime_b(n, rounds) {
  var r = 0, d = n;
  var nm1 = h$ghcjsbn_sub_bw(n,1);
  var two = h$ghcjsbn_mkBigNat_w(2);
  while(h$ghcjsbn_testBit_b(d, 0)) {
    d = h$ghcjsbn_shr_b(d, 1);
    r++;
  }
  for(var round = 0; round < rounds; round++) {
    var a = h$ghcjsbn_random_b(two, nm1);
    var x = h$ghcjsbn_modPow_bbb(a, d, n);
    if(h$ghcjsbn_eq_bw(x,1) || h$ghcjsbn_eq_bb(x,nm1)) continue;
    var found = false;
    for(var rr=0;rr<r;rr++) {
      x = h$ghcjsbn_modPow_bbb(x, two, n);
      if(h$ghcjsbn_eq_bb(x, nm1)) {
        found = true;
        break;
      }
    }
    if(!found) return 0;
  }
  return 1;
}

function h$ghcjsbn_testPrime_w(w, rounds) {
  return h$ghcjsbn_testPrime_b(h$ghcjsbn_mkBigNat_w(w), rounds);
}

function h$integer_gmp_next_prime1(w) {
  var r = h$ghcjsbn_nextPrime_b(h$ghcjsbn_mkBigNat_w(w));
  return h$ghcjsbn_toWord_b(r);
}

function h$ghcjsbn_nextPrime_b(bn) {
  var rounds = 64;
  if(h$ghcjsbn_eq_bw(bn, 2)) {
    return h$ghcjsbn_mkBigNat_w(3);
  }
  var i = bn;
  do {
    i = h$ghcjsbn_add_bw(i, 2);
  } while(!h$ghcjsbn_testPrime_b(i, rounds));
  return i;
}

// BigNat -> BigNat -> Bool
/*
function h$ghcjsbn_eq_bb(b1, b2) {
  ASSERTVALID_B(b1, "eq_bb b1");
  ASSERTVALID_B(b2, "eq_bb b2");
  var l1 = b1.length, l2 = b2.length;
  if(l1 !== l2) return false;
  while(--l1 >= 0) {
    if(b1[l1] !== b2[l1]) return false;
  }
  return true;
}
*/

// BigNat -> BigNat -> Int (Ordering: LT,EQ,GT)
function h$ghcjsbn_cmp_bb(b1, b2) {
  ASSERTVALID_B(b1, "cmp_bb b1");
  ASSERTVALID_B(b2, "cmp_bb b2");
  var l1 = b1[0], l2 = b2[0], d1, d2;
  if(l1 === l2) {
    while(--l1 >= 0) {
      d1 = b1[l1+1];
      d2 = b2[l1+1];
      if(d1 !== d2) return d1 < d2 ? GHCJSBN_LT : GHCJSBN_GT;
    }
    return GHCJSBN_EQ;
  } else {
    return l1 > l2 ? GHCJSBN_GT : GHCJSBN_LT;
  }
}

// fixed size tmp, these should not grow
var h$ghcjsbn_tmp_2a = [0, 0, 0];
var h$ghcjsbn_tmp_2b = [0, 0, 0];

// this is variable size scratch space
var h$ghcjsbn_tmp_a = [0, 0, 0, 0, 0, 0, 0, 0];
var h$ghcjsbn_tmp_b = [0, 0, 0, 0, 0, 0, 0, 0];

// b - w :: BigNat -> Word -> BigNat

function h$ghcjsbn_sub_bw(b, w) {
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  return h$ghcjsbn_sub_bb(b, a);
}

// b - s :: BigNat -> Int -> BigNat
// returns new BigNat, nullBigNat in case of underflow
// returns size of t
function h$ghcjsbn_sub_bs(b, s) {
  ASSERTVALID_B(b, "sub_bs");
  ASSERTVALID_S(s, "sub_bs");
  var a, ms, r;
  if(s < 0) {
    if(s === -2147483648) {
      r = h$ghcjsbn_add_bb(b, h$ghcjsbn_two31_b);
    } else {
      a = h$ghcjsn_tmp_2a;
      h$ghcjsbn_toBigNat_s(a, -s);
      r = h$ghcjsbn_add_bb(b, a);
    }
  } else {
    a = h$ghcjsn_tmp_2a;
    h$ghcjsbn_toBigNat_s(a, s);
    r = h$ghcjsbn_sub_bb(b, a);
  }
  ASSERTVALID_B(r, "sub_bs result");
  return r;
}

// t = b + w :: BigNat -> BigNat -> Word -> Int
// returns size of t
function h$ghcjsbn_add_bw(b, w) {
  ASSERTVALID_B(b, "add_bw");
  ASSERTVALID_W(w, "add_bw");
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  return h$ghcjsbn_add_bb(b, a);
}

// t = b + s :: BigNat -> BigNat -> Int -> Int
// returns size of t, nullBigNat in case of underflow
function h$ghcjsbn_add_bs(b, s) {
  ASSERTVALID_B(b, "add_bs");
  ASSERTVALID_S(s, "add_bs");
  var a, ms, r;
  if(s < 0) {
    if(s === -2147483648) {
      r = h$ghcjsbn_sub_bb(b, h$ghcjsbn_two31_r);
    } else {
      ms = -s;
      a = h$ghcjsbn_tmp_2a;
      h$ghcjsbn_toBigNat_s(a, ms);
      r = h$ghcjsbn_sub(b, a);
    }
  } else {
    a = h$ghcjsbn_tmp_2a;
    h$ghcjsbn_toBigNat_s(a, s);
    r = h$ghcjsbn_add_bb(b, a);
  }
  ASSERTVALID_B(r, "add_bs result");
  return r;
}

// t = b1 + b2 :: BigNat -> BigNat -> BigNat -> Int
// returns size of t
function h$ghcjsbn_add_bb(b1, b2) {
  ASSERTVALID_B(b1, "add_bb b1");
  ASSERTVALID_B(b2, "add_bb b2");
  var i, c = 0, l1 = b1[0], l2 = b2[0], t = [0];
  var bl, lmin, lmax;
  if(l1 <= l2) {
    lmin = l1;
    lmax = l2;
    bl = b2;
  } else {
    lmin = l2;
    lmax = l1;
    bl = b1;
  }
  for(i=1;i<=lmin;i++) {
    c += b1[i] + b2[i];
    t[i] = c & GHCJSBN_MASK;
    c >>= GHCJSBN_BITS;
  }
  for(i=lmin+1;i<=lmax;i++) {
    c += bl[i];
    t[i] = c & GHCJSBN_MASK;
    c >>= GHCJSBN_BITS;
  }
  if(c !== 0) t[++lmax] = c;
  t[0] = lmax;
  ASSERTVALID_B(t, "add_bb result");
  return t;
}

// b1 += b2 :: BigNat -> BigNat -> Int
// returns new size of b1
function h$ghcjsbn_addTo_bb(b1, b2) {
  ASSERTVALID_B(b1, "addTo_bb b1");
  ASSERTVALID_B(b2, "addTo_bb b2");
  var i, c = 0, l1 = b1[0], l2 = b2[0];
  if(l2 > l1) {
    for(i = l1 + 1; i <= l2; i++) {
      b1[i] = 0;
    }
    l1 = l2;
  }
  for(i = 1; i <= l2; i++) {
    c += b1[i] + b2[i];
    b1[i] = c & GHCJSBN_MASK;
    c >>= GHCJSBN_BITS;
  }
  // propagate carry as long as needed
  for(i = l2 + 1; c !== 0 && i <= l1; i++) {
    c += b1[i];
    b1[i] = c & GHCJSBN_MASK;
    c >>= GHCJSBN_BITS;
  }
  if(c !== 0) {
    b1[l1] = c;
    b1[0] = l1+1;
  } else {
    b1[0] = l1;
  }
  ASSERTVALID_B(b1, "addTo_bb result");
}

// b1 - b2 :: BigNat -> BigNat -> BigNat
// returns a new BigNat, nullBigNat in case of underflow
function h$ghcjsbn_sub_bb(b1, b2) {
  ASSERTVALID_B(b1, "sub_bb b1");
  ASSERTVALID_B(b2, "sub_bb b2");
  if(h$ghcjsbn_cmp_bb(b1,b2) === GHCJSBN_LT) {
    return [];
  } else {
    var i, c = 0, l1 = b1[0], l2 = b2[0], t = [0];
    for(i = 1; i <= l2; i++) {
      c += b1[i] - b2[i];
      t[i] = c & GHCJSBN_MASK;
      c >>= GHCJSBN_BITS;
    }
    for(i = l2 + 1; i <= l1; i++) {
      c += b1[i];
      t[i] = c & GHCJSBN_MASK;
      c >>= GHCJSBN_BITS;
    }
    while(l1 > 0 && t[l1] === 0) l1--;
    t[0] = l1;
    ASSERTVALID_B(t, "sub_bb result");
    return t;
  }
}

// b1 -= b2 :: BigNat -> BigNat -> Int
// returns size of t, b1 must be >= b2
function h$ghcjsbn_subTo_bb(b1, b2) {
  ASSERTVALID_B(b1, "subTo_bb b1");
  ASSERTVALID_B(b2, "subTo_bb b2");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(h$ghcjsbn_cmp_bb(b1, b2) === GHCJSBN_LT) {
    throw new Error("h$ghcjsbn_subTo_bb assertion failed: b1 >= b2");
  }
#endif
  var i, c = 0, l1 = b1[0], l2 = b2[0];
  for(i = 1; i <= l2; i++) {
    c += b1[i] - b2[i];
    b1[i] = c & GHCJSBN_MASK;
    c >>= GHCJSBN_BITS;
  }
  for(i = l2 + 1; c !== 0 && i <= l1; i++) {
    c += b1[i];
    b1[i] = c & GHCJSBN_MASK;
    c >>= GHCJSBN_BITS;
  }
  while(l1 > 0 && b1[l1] === 0) l1--;
  b1[0] = l1;
  ASSERTVALID_B(b1, "subTo_bb result");
}

// t = b1 / b2, BigNat -> BigNat -> BigNat -> Int (returns size of t)
/* function h$ghcjsbn_div_bb(t, b1, b2) {

}

// t = b1 % b2, BigNat -> BigNat -> BigNat -> Int (returns size of t)
function h$ghcjsbn_mod_bb(t, b1, b2) {

}

// b % s, BigNat -> Int -> Int
function h$ghcjsbn_mod_bs(b, s) {

}
*/
// BigNat -> Integer (nonnegative, known length)
/*
function h$ghcjsbn_wrap_pl(b, l) {
  var lb;
  if(l === 0) {
    return MK_INTEGER_S(0);
  } else if(l === 1) {
    return MK_INTEGER_S(b[0]);
  } else if(l === 2 && (b[1] >> (31 - GHCJSBN_BITS)) === 0) {
    return MK_INTEGER_S((b[1] << GHCJSBN_BITS)|b[0]);
  } else {
    lb = b.length - l;
    while(lb-- > 0) b.pop();
    return MK_INTEGER_Jp(b);
  }
}
*/
// BigNat -> Integer (nonnegative)
function h$ghcjsbn_wrap_p(b) {
  var l = b[0];
  if(l === 0) {
    return MK_INTEGER_S(0);
  } else if(l === 1) {
    return MK_INTEGER_S(b[1]);
  } else if(l === 2 && (b[2] >> (31 - GHCJSBN_BITS)) === 0) {
    return MK_INTEGER_S((b[2] << GHCJSBN_BITS)|b[1]);
  } else {
    return MK_INTEGER_Jp(b);
  }
}
/*
function h$ghcjsbn_wrap_nl(b, l) {
  var lb;
  if(l === 0) {
    return MK_INTEGER_S(0);
  } else if(l === 1) {
    return MK_INTEGER_S(-b[0]);
  } else if(l === 2 &&
            ((b[1] >> (31 - GHCJSN_BITS)) === 0 ||
             (b[1] === (1 << (31 - GHCJSBN_BITS)) && b[0] === 0))) {
    return MK_INTEGER_S((-b[1]-b[0])|0);
  } else {
    lb = b.length - l;
    while(lb-- > 0) b.pop();
    return MK_INTEGER_Jn(b);
  }
}
*/
// BigNat -> Integer (nonnegative)
function h$ghcjsbn_wrap_n(b) {
  var l = b[0];
  if(l === 0) {
    return MK_INTEGER_S(0);
  } else if(l === 1) {
    return MK_INTEGER_S(-b[1]);
  } else if(l === 2 &&
            ((b[2] >> (31 - GHCJSN_BITS)) === 0 ||
             (b[2] === (1 << (31 - GHCJSBN_BITS)) && b[1] === 0))) {
    return MK_INTEGER_S((-b[2]-b[1])|0);
  } else {
    return MK_INTEGER_Jn(b);
  }
}

// b1 *= b2 :: BigNat -> BigNat -> IO ()
function h$ghcjsbn_mulTo_bb(b1, b2) {
  ASSERTVALID_B(b1, "mulTo_bb b1");
  ASSERTVALID_B(b2, "mulTo_bb b2");
  var t = h$ghcjsbn_mul_bb(b1, b2);
  h$ghcjsbn_copy(b1, t);
  ASSERTVALID_B(b1, "mulTo_bb result");
}

// b1 * b2 ::  BigNat -> BigNat -> BigNat
function h$ghcjsbn_mul_bb(b1, b2) {
  ASSERTVALID_B(b1, "mul_bb b1");
  ASSERTVALID_B(b2, "mul_bb b2");
  var l1 = b1[0], l2 = b2[0];
/*  if(l1 > 50 && l2 > 50) {
    return h$ghcjsbn_mul_karatsuba_bb(b1, b2);
  } fixme update this */
  var n = l1 + l2, i, t = [0];
  for(i = 1; i <= n; i++) t[i] = 0;
  if(l1 > l2) {
    for(i = 0; i < l2; i++) {
      t[i + l1 + 1] = h$ghcjsbn_mul_limb(0, b1, b2[i+1], t, i, 0, l1);
    }
  } else {
    for(i = 0; i < l1; i++) {
      t[i + l2 + 1] = h$ghcjsbn_mul_limb(0, b2, b1[i+1], t, i, 0, l2);
    }
  }
  for(i = l1 + l2; i > 0 && t[i] === 0; i--);
  t[0] = i;
  ASSERTVALID_B(t, "mul_bb result");
  return t;
}

function h$ghcjsbn_mul_bw(b, w) {
  ASSERTVALID_B(b, "mul_bw");
  ASSERTVALID_W(w, "mul_bw");
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
  var t = h$ghcjsbn_mul_bb(b, a);
  ASSERTVALID_B(t, "mul_bw result");
  return t;
}


// karatzuba multiplication for long numbers
function h$ghcjsbn_mul_karatsuba_bb(t, b1, b2) {
  throw new Error("not yet updated");
  var l1 = b1.length, l2 = b2.length;
  var i, b  = (l1 < l2 ? l1 : l2) >> 1;
  var x0 = [b], x1 = [l1-b], y0 = [b], y1 = [l2-b];
  for(i = 1; i <= b; i++) {
    x0[i] = b1[i];
    y0[i] = b2[i];
  }
  for(i = b + 1; i <= l1; i++) x1[i - b] = b1[i];
  for(i = b + 1; i <= l2; i++) y1[i - b] = b2[i];
  var z0 = h$ghcjsbn_mul_bb(x0, y0), z1, z2 = h$ghcjsbn_mul_bb(x1, y1);

  // compute z1 = (x1 + x0)(y1 + y0) - z2 - z0
  // (reusing x0 and y0 for (x1 + x0) and (y1 + y0))
  h$ghcjsbn_addTo_bb(x0, x1);
  h$ghcjsbn_addTo_bb(y0, x1);
  z1 = h$ghcjsbn_mul_bb(x0, y0);
  h$ghcjsbn_subTo_bb(z1, z2);
  h$ghcjsbn_subTo_bb(z1, z0);
  // store shifted z2 in t
  // fixme this looks wrong
  for(i = 0; i < 2*b; i++) t[i] = 0;
  l2 = z2.length;
  for(i = 0; i < l2; i++) t[i+2*b] = z2[i];
  // compute shifted z1s = z1 * B
  var z1s = [];
  l1 = z1.length;
  for(i = 0; i < b; i++)  z1s[i]   = 0;
  for(i = 0; i < l1; i++) z1s[i+b] = z1[i];
  // add the results so that t = z2 * (2*B) + z1 * B + z0
  h$ghcjsbn_addTo_bb(t, z1s);
  h$ghcjsbn_addTo_bb(t, z0);
  return t;
}

// from JSBN am3
// w_j += (x*b_i) ?
/* c = carry?
   n = iterations?
 */
#if(GHCJSBN_BITS == 28)
function h$ghcjsbn_mul_limb(i,b,x,w,j,c,n) {
  // ASSERTVALID_B(b, "mul_limb b");
  // ASSERTVALID_B(w, "mul_limb w");
  var xl = x & 0x3fff, xh = x >> 14;
  while(--n >= 0) {
    var l = b[++i]   &  0x3fff;
    var h = b[i] >> 14;
    var m = xh * l + h * xl;
    l = xl *l + ((m & 0x3fff) << 14) + w[++j] + c;
    c = (l >> 28) + (m >> 14) + xh * h;
    // h$log("mul_limb: c: " + c + " l: " + l + " xh: " + xh + " h: " + h);
    w[j] = l & 0xfffffff;
  }
  return c;
}
#else
#error "no limb multiplication routine for specified GHCJSBN_BITS"
#endif

// q = b1 / b2, r = b1 % b2 :: BigNat -> BigNat -> BigNat -> BigNat -> Int
// b2 must be > 0
// returns length of r
// d is normalized before return

/*
   algorithm:
 y = 0?
 nsh = number of leading zeroes in most significant word
 pm = positive modulus
 pt = positive divident
 y = tmp, shifted modulus
 r = shifted divident
 ys = length of y
 y0 = biggest limb of y
 yt = new estimated length of y?
 */

function h$ghcjsbn_quotRem_bb(q, r, b1, b2) {
  ASSERTVALID_B(b1, "quotRem_bb b1");
  ASSERTVALID_B(b2, "quotRem_bb b2");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(h$ghcjsbn_cmp_bw(b2, 0) !== GHCJSBN_GT) {
    throw new Error("h$ghcjsbn_quotRem_bb: operand not positive");
  }
  #endif
  if(q === null) q = h$ghcjsbn_tmp_a;
  if(r === null) r = h$ghcjsbn_tmp_b;
  var l1 = b1[0], l2 = b2[0], nsh, y = [];
  if(l1 === 0) {
    q[0] = 0;
    r[0] = 0;
    return;
  }
  if(h$ghcjsbn_cmp_bb(b1,b2) === GHCJSBN_LT) {
    q[0] = 0;
    h$ghcjsbn_copy(r, b1);
    return;
  }
  nsh = GHCJSBN_BITS-h$ghcjsbn_nbits_s(b2[l2]);
  ASSERTVALID_S(nsh, "quotRem_bb nsh");
  if(nsh !== 0) {
    h$ghcjsbn_shlTo_b(y, b2, nsh);
    h$ghcjsbn_shlTo_b(r, b1, nsh);
  } else {
    h$ghcjsbn_copy(y, b2);
    h$ghcjsbn_copy(r, b1);
  }
  ASSERTVALID_B(y, "quotRem_bb y_0");
  ASSERTVALID_B(r, "quotRem_bb r_0");
  var ys = y[0], y0 = y[ys];
  var yt = y0*(1<<GHCJSBN_F1)+((ys>1)?y[ys-1]>>GHCJSBN_F2:0);
  var d1 = GHCJSBN_FV/yt, d2 = (1<<GHCJSBN_F1)/yt, e = 1 << GHCJSBN_F2;
  var i = r[0], j = i-ys, t = q;
  h$ghcjsbn_shlTo_limbs_b(t,y,j);
  // h$log("rt1: " + i);
  // h$log("[" + r.join(",") + "] [" + t.join(",") + "]");
  if(h$ghcjsbn_cmp_bb(r, t) !== GHCJSBN_LT) {
    r[r[0]+1] = 1;
    r[0] += 1;
    // h$log("rt1a: " + r[0]);
    h$ghcjsbn_subTo_bb(r, t);
  }
  // h$log("rt2: " + r[0]);
  // h$log("y0: " + y0 + " yt: " + yt + " d1: " + d1 + " d2: " + d2 + " e: " + e);
  h$ghcjsbn_shlTo_limbs_b(t, h$ghcjsbn_one_b, ys);
  y = h$ghcjsbn_sub_bb(t, y);
  while(y.length <= ys) y[y.length] = 0; // fixme? no looks ok
  while(--j >= 0) {
    // Estimate quotient digit
    var qd = (r[(--i)+1]===y0)?GHCJSBN_MASK:Math.floor(r[i+1]*d1+(r[i]+e)*d2);
    // h$log("i: " + i + " j: " + j + " qd: " + qd + " rdi: " + r[i+1] + " ys: " + ys);
    // h$log("yd: [" + y.join(',') + "] rd: [" + r.join(',') + "]");
    var am = h$ghcjsbn_mul_limb(0, y, qd, r, j, 0, ys);
    // h$log("am: " + am);
    if((r[i+1] += am) < qd) {
    // if((r[i+1] += h$ghcjsbn_mul_limb(0, y, qd, r, j, 0, ys)) < qd) {
      h$ghcjsbn_shlTo_limbs_b(t, y, j);
      h$ghcjsbn_subTo_bb(r, t);
      // h$log("0. rdi: " + r[i+1] + " qd: " + qd);
      while(r[i+1] < --qd) {
        // h$log("1. rdi: " + r[i+1] + " qd: " + qd);
        h$ghcjsbn_subTo_bb(r, t);
      }
    }
  }
  ASSERTVALID_B(r, "intermediate r");
  h$ghcjsbn_shrTo_limbs_b(q, r, ys);
  r[0] = ys;
  while(r[r[0]] === 0 && r[0] > 0 && r[0]--);
  if(nsh !== 0) {
    var r0 = [];
    h$ghcjsbn_copy(r0, r);
    h$ghcjsbn_shrTo_b(r, r0, nsh);
  }
  ASSERTVALID_B(q, "quotRem_bb result q");
  ASSERTVALID_B(r, "quotRem_bb result r");
}

// b % w , q = b / w :: BigNat -> BigNat -> Word -> Word
function h$ghcjsbn_quotRem_bw(q, b, w) {
  ASSERTVALID_B(b, "quotRem_bw");
  ASSERTVALID_W(w, "quotRem_bw");
  var a = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(a, w);
/*  if(w === 0) {
    a[0] = 0;
  } else if(w > 0 && w <= GHCJSBN_MASK) {
    a[0] = 1;
    a[1] = w;
  } else {
    a[0] = 2;
    a[1] = w   & GHCJSBN_MASK;
    a[2] = w >>> GHCJSBN_BITS;
  } */
  var r = [];
  h$ghcjsbn_quotRem_bb(q, r, b, a);
  return h$ghcjsbn_toWord_b(r);
}

// BigNat -> JSBN
// assumes same number of bits
function h$ghcjsbn_tmp_toJSBN(b) {
  var j = new BigInteger(), bl = b[0], i;
  for(i = 0; i < bl; i++) j.data[i] = b[i+1];
  j.s = 0;
  j.t = bl;
  return j;
/*  ASSERTVALID_B(b, "toJSBN");
  var j0 = new BigInteger();
  var j1 = new BigInteger();
  var j2 = new BigInteger();
  for(var i = b[0]; i > 0; i--) {
    h$log("i: " + b[i]);
    j2.fromString('' + b[i]);
    j0.lShiftTo(28, j1);
    j1.addTo(j2, j0);
  }
  return j0; */
}

// b = fromJSBN(j) :: BigNat -> JSBN -> Int
// returns length
function h$ghcjsbn_tmp_fromJSBN(b, j) {
  var bl = j.t, i;
  for(i = 0; i < bl; i++) {
    b[i] = j.data[i];
  }
  return bl;
}


// function h$ghcjsbn_divMod_bs(d

// t = b1 % b2 :: BigNat -> BigNat -> BigNat
function h$ghcjsbn_rem_bb(b1, b2) {
  ASSERTVALID_B(b1, "rem_bb b1");
  ASSERTVALID_B(b2, "rem_bb b2");
  var t1 = [], t2 = [];
  h$ghcjsbn_quotRem_bb(t1, t2, b1, b2);
  ASSERTVALID_B(t2, "rem_bb result");
  return t2;
}

// b1 % s :: BigNat -> Word -> Word
function h$ghcjsbn_rem_bw(b, w) {
  ASSERTVALID_B(b, "rem_bw");
  ASSERTVALID_W(w, "rem_bw");
  //  var t1 = [];
  var r = h$ghcjsbn_quotRem_bw([] /* t1 */, b, w);
  ASSERTVALID_W(r, "rem_bw result");
  return r;
//  var a = h$ghcjsbn_tmp_2a;
//  h$ghcjsbn_toBigNat_w(a, w);
//  a[1] = w   & GHCJSBN_MASK;
//  a[2] = w >>> GHCJSBN_BITS;
//  var t1 = []; // , t2 = h$ghcjsbn_tmp_2b;
//  return h$ghcjsbn_quotRem_bw(t1, /* t2 , */ b, a);
//  return t[1] | (t[2] << GHCJSBN_BITS);
}

// b1 / b2 :: BigNat -> BigNat -> BigNat
function h$ghcjsbn_quot_bb(b1, b2) {
  ASSERTVALID_B(b1, "quot_bb b1");
  ASSERTVALID_B(b2, "quot_bb b2");
  var t1 = [], t2 = [];
  h$ghcjsbn_quotRem_bb(t1, t2, b1, b2);
  ASSERTVALID_B(t1, "quot_bb result");
  return t1;
}
/*
// b / s :: BigNat -> Int -> BigNat
function h$ghcjsbn_div_bs(b, w) {
  ASSERTVALID_B(b, "div_bs");
  ASSERTVALID_S(s, "div_bs");
#ifdef GHCJS_ASSERT_INTEGER
  if(s <= 0) {
    throw new Error("h$ghcjsbn_div_bs: divisor must be positive");
  }
#endif
  var a = h$ghcjsbn_tmp_2a;
  a[0] = s &  GHCJSBN_MASK;
  a[1] = s >> GHCJSBN_BITS;
  return h$ghcjsbn_div_bb(t, b, a);
}
*/
// t = b % w :: BigNat -> BigNat -> Word -> Int
// returns length of t
/*
function h$ghcjsbn_div_bw(t, b, w) {
  ASSERTVALID_B(b, "div_bw");
  ASSWRTVALID_W(w, "div_bw");
  var a = h$ghcjsbn_tmp_2a;
 a[0] = w   & GHCJSBN_MASK;
 a[1] = w >>> GHCJSBN_BITS;
  return h$ghcjsbn_div_bb(t, b, a);
}
*/
// b ^ 2 :: BigNat -> BigNat
function h$ghcjsbn_sqr_b(b) {
  ASSERTVALID_B(b, "sqr_b");
  var l = b[0], n = 2 * l, i, c, t = [0];
  for(i = 1; i <= n; i++) t[i] = 0;
  for(i = 0; i < l - 1; i++) {
    c = h$ghcjsbn_mul_limb(i, b, b[i+1],t,2*i,0,1);
    if((t[i + l + 1] += h$ghcjsbn_mul_limb(i+1, b, 2*b[i+1], t, 2*i+1, c, l - i - 1)) >= GHCJSBN_DV) {
      t[i + l + 1] -= GHCJSBN_DV;
      t[i + l + 2]  =  1;
    }
  }
  if(n > 0) t[n] += h$ghcjsbn_mul_limb(i, b, b[i+1], t, 2*i, 0, 1);
  if(t[n] === 0) n--;
  t[0] = n;
  ASSERTVALID_B(t, "sqr_b result");
  return t;
}

// b1 ^ b2 :: BigNat -> BigNat -> BigNat
// returns size of t
function h$ghcjsbn_pow_bb(b1, b2) {
  ASSERTVALID_B(b1, "pow_bb b1");
  ASSERTVALID_B(b2, "pow_bb b2");
  var i, sq = b1, t = [1,1];
  var bits = h$ghcjsbn_nbits_b(b2);
  for(i = 0; i < bits; i++) {
    if(h$ghcjsbn_testBit_b(b2, i)) {
      h$ghcjsbn_mulTo_bb(t, sq);
    }
    sq = h$ghcjsbn_sqr_b(sq);
  }
  return t;
}

// t = b ^ s :: BigNat -> Word -> BigNat
function h$ghcjsbn_pow_bw(b, w) {
  ASSERTVALID_B(b, "pow_bw");
  ASSERTVALID_W(w, "pow_bw");
  var i, sq = b, t = [1,1];
  while(w) {
    if(w&1) h$ghcjsbn_mulTo_bb(t, sq);
    w >>>= 1;
    if(w) {
      sq = h$ghcjsbn_sqr_b(sq);
    }
  }
  ASSERTVALID_B(t, "pow_bw result");
  return t;
}

// w1 ^ w2 :: Word -> Word -> BigNat
function h$ghcjsbn_pow_ww(w1, w2) {
  ASSERTVALID_S(w1, "pow_ww w1");
  ASSERTVALID_S(w2, "pow_ww w2");
  var b = h$ghcjsbn_tmp_2a;
  h$ghcjsbn_toBigNat_w(b, w1);
  var t = h$ghcjsbn_pow_bw(b, w2);
  ASSERTVALID_B(t, "pow_ww result");
  return t;
}

// (b ^ s1) % s2 :: BigNat -> BigNat -> BigNat -> BigNat
function h$ghcjsbn_modPow_bbb(b, e, m) {
  var r = h$ghcjsbn_one_b, b = h$ghcjsbn_rem_bb(b, m);
  while(!h$ghcjsbn_eq_bw(e, 0)) {
    if(h$ghcjsbn_testBit_b(e, 0)) {
      r = h$ghcjsbn_rem_bb(h$ghcjsbn_mul_bb(r, b), m);
    }
    e = h$ghcjsbn_shr_b(e, 1);
    b = h$ghcjsbn_rem_bb(h$ghcjsbn_mul_bb(b, b), m);
  }
  return r;
}

// (b ^ s1) % s2 :: BigNat -> Int -> Int -> Int
function h$ghcjsbn_modPow_bss(b, e, m) {
  return h$ghcjsbn_modPow_sss(h$ghcjsbn_rem_bw(b, m), e, m);
}
// (s1 ^ s2) % s3 :: Int -> Int -> Int -> Int
function h$ghcjsbn_modPow_sss(b, e, m) {
  return h$integer_gmp_powm_word(b, e, m);
}

function h$ghcjsbn_modular_inverse(a, n) {
    var t = h$ghcjsbn_zero_b, newt = h$ghcjsbn_one_b;
    var r = n, newr = a;
    while(!h$ghcjsbn_cmp_bw(newr, 0)) {
      var quotient = h$ghcjsbn_div_bb(r, newr);
      // fixme this goes wrong because it can go below 0
      var tmp = newt;
      newt = h$ghcjsbn_sub_bb(t, h$ghcjsbn_mul_bb(quotient, newt));
      t = tmp;
      tmp = newr;
      newr = h$ghcjsbn_sub_bb(r, h$ghcjsbn_mul_bb(quotient, newr));
      r = tmp;
    }
    if(h$ghcjsbn_cmp_bw(r, 1) > 0) return a; // fixme signal not invertible
    // if t < 0 then t := t + n
    return t;
}

function h$ghcjsbn_powModSBigNat(bpos, base, epos, exp, m) {
  var newBase = bpos ? base : h$ghcjsbn_sub_bb(m, base);
  var newExp = epos ? exp : h$ghcjsbn_modular_inverse(exp, m);
  return h$ghcjsbn_modPow_bbb(newBase, newExp, m);
}

function h$integer_gmp_powm_word(b, e, m) {
  var r = 1, b = b % m;
  while(e !== 0) {
    if(e % 2 === 1) r = (r * b) % m;
    e = e >>> 1;
    b = (b * b) % m;
  }
  return r;
}

// r = gcd(b1,b2) BigNat -> BigNat -> BigNat
function h$ghcjsbn_gcd_bb(b1, b2) {
  ASSERTVALID_B(b1, "gcd_bb b1");
  ASSERTVALID_B(b2, "gcd_bb b2");
  var r;
  if(h$ghcjsbn_cmp_bb(b1, b2) === GHCJSBN_GT) {
    r  = b1;
    b1 = b2;
    b2 = r;
  }
  while(b1[0] > 0) {
    r = h$ghcjsbn_rem_bb(b2, b1);
    b2 = b1;
    b1 = r;
  }
  ASSERTVALID_B(b2, "gcd_bb result");
  return b2;
}
// gcd(b,s) :: BigNat -> Int -> Int
function h$ghcjsbn_gcd_bs(b, s) {
  throw new Error("h$ghcjsbn_gcd_bs not implemented");
}

// gcd(s1,s2) :: Int -> Int -> Int
function h$ghcjsbn_gcd_ss(s1, s2) {
  ASSERTVALID_S(s1, "gcd_ss s1");
  ASSERTVALID_S(s2, "gcd_ss s2");
  var a, b, r;
  a = s1 < 0 ? -s1 : s1;
  b = s2 < 0 ? -s2 : s2;
  if(b < a) {
    r = a;
    a = b;
    b = r;
  }
  while(a !== 0) {
    r = b % a;
    b = a;
    a = r;
  }
  ASSERTVALID_S(b, "gcd_ss result");
  return b;
}

// gcd(w1,w2) :: Word -> Word -> Word
// fixme negatives are probably wrong here
function h$ghcjsbn_gcd_ww(w1, w2) {
  ASSERTVALID_W(w1, "gcd_ww w1");
  ASSERTVALID_W(w2, "gcd_ww w2");
  var a, b, r;
  a = w1 < 0 ? (w1 + 4294967296) : w1;
  b = w2 < 0 ? (w2 + 4294967296) : w2;
  if(b < a) {
    r = a;
    a = b;
    b = r;
  }
  while(a !== 0) {
    r = b % a;
    b = a;
    a = r;
  }
  b = b|0;
  ASSERTVALID_W(b, "gcd_ww result");
  return b;
}

function h$ghcjsbn_gcd_bw(b, w) {
  ASSERTVALID_B(b, "gcd_bw");
  ASSERTVALID_W(w, "gcd_bw");
  var q = [], r = h$ghcjsbn_quotRem_bw(q, b, w);
  ASSERTVALID_W(r, "gcd_bw r");
  if(r === 0) {
    return b[0] === 0 ? 0 : w;
  } else {
    return h$ghcjsbn_gcd_ww(r, w);
  }
}

// b >> s :: BigNat -> Int -> BigNat
function h$ghcjsbn_shr_b(b, s) {
  ASSERTVALID_B(b, "shr_b");
  ASSERTVALID_S(s, "shr_b");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(s < 0) throw new Error("h$ghcjsbn_shr_b: negative operand");
#endif
  var i, v1, v2, l = b[0], sl  = (s / GHCJSBN_BITS)|0, t = [0];
  l -= sl;
  if(l <= 0) {
    t[0] = 0;
  } else {
    var sb1 = s % GHCJSBN_BITS, sb2 = GHCJSBN_BITS - sb1, m = (1<<sb1)-1;
    var c = b[sl + 1] >> sb1, v;
    for(i = 1; i < l; i++) {
      v = b[i + sl + 1];
      t[i] = ((v&m) << sb2)|c;
      c = v >> sb1;
    }
    if(c !== 0) {
      t[l] = c;
      t[0] = l;
    } else {
      t[0] = l - 1;
    }
  }
  ASSERTVALID_B(t, "shr_b result");
  return t;
}

// t = b >> s :: BigNat -> BigNat -> Int -> IO ()
function h$ghcjsbn_shrTo_b(t, b, s) {
  ASSERTVALID_B(b, "shrTo_b");
  ASSERTVALID_S(s, "shrTo_b");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(s < 0) throw new Error("h$ghcjsbn_shrTo_b: negative operand");
#endif
  var i, v1, v2, l = b[0], sl  = (s / GHCJSBN_BITS)|0;
  t[0] = 0;
  l -= sl;
  if(l <= 0) {
    t[0] = 0;
  } else {
    var sb1 = s % GHCJSBN_BITS, sb2 = GHCJSBN_BITS - sb1, m = (1<<sb1)-1;
    var c = b[sl + 1] >> sb1, v;
    for(i = 1; i < l; i++) {
      v = b[i + sl + 1];
      t[i] = ((v&m) << sb2)|c;
      c = v >> sb1;
    }
    if(c !== 0) {
      t[l] = c;
      t[0] = l;
    } else {
      t[0] = l - 1;
    }
  }
  ASSERTVALID_B(t, "shrTo_b result");
}

function h$ghcjsbn_shr_neg_b(b, s) {
  if(s === 0) return b;
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_zero_b) === GHCJSBN_EQ) return b;
  var t = h$ghcjsbn_sub_bw(b, 1);
  t = h$ghcjsbn_shr_b(t, s);
  return h$ghcjsbn_add_bw(t, 1);
}

// b << s :: BigNat -> Int -> BigNat
function h$ghcjsbn_shl_b(b, s) {
  ASSERTVALID_B(b, "shl_b");
  ASSERTVALID_S(s, "shl_b");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(s < 0) throw new Error("h$ghcjsbn_shl_b: negative operand");
#endif
  var sl = (s / GHCJSBN_BITS)|0;
  var sb1 = s % GHCJSBN_BITS, sb2 = GHCJSBN_BITS - sb1;
  // mask wrong
  var l = b[0];
  if(l === 0) return h$ghcjsbn_zero_b;
  var c = 0, i, v, m = (1 <<sb1) - 1, t = [0];
  for(i = 1; i <= sl; i++) {
    t[i] = 0;
  }
  for(i = 1; i <= l; i++) {
    v = b[i];
    t[i + sl] = ((v << sb1) & GHCJSBN_MASK) | c;
    c = v >> sb2;
  }
  if(c !== 0) {
    t[l+sl+1] = c;
    t[0] = l + sl + 1;
  } else {
    t[0] = l + sl;
  }
  ASSERTVALID_B(t, "shl_b result");
  return t;
}

// t = b << s :: BigNat -> BigNat -> Int -> IO ()
function h$ghcjsbn_shlTo_b(t, b, s) {
  ASSERTVALID_B(b, "shlTo_b");
  ASSERTVALID_S(s, "shlTo_b");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(s < 0) throw new Error("h$ghcjsbn_shlTo_b: negative operand");
#endif
  var sl = (s / GHCJSBN_BITS)|0;
  var sb1 = s % GHCJSBN_BITS, sb2 = GHCJSBN_BITS - sb1;
  // mask wrong
  var l = b[0], c = 0, i, v, m = (1 <<sb1) - 1;
  t[0] = 0;
  for(i = 1; i <= sl; i++) {
    t[i] = 0;
  }
  for(i = 1; i <= l; i++) {
    v = b[i];
    t[i + sl] = ((v << sb1) & GHCJSBN_MASK) | c;
    c = v >> sb2;
  }
  if(c !== 0) {
    t[l+sl+1] = c;
    t[0] = l + sl + 1;
  } else {
    t[0] = l + sl;
  }
  ASSERTVALID_B(t, "shlTo_b result");
}


// t = b >> (GHCJSBN_BITS * s) :: BigNat -> BigNat -> Int
function h$ghcjsbn_shrTo_limbs_b(t, b, s) {
  ASSERTVALID_B(b, "shrTo_limbs_b");
  ASSERTVALID_S(s, "shrTo_limbs_b");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(s < 0) throw new Error("h$ghcjsbn_shrTo_limbs_b: negative operand");
#endif
  var l = b[0], l1 = l - s, i;
  if(l1 < 1) {
    t[0] = 0;
  } else {
    t[0] = l1;
    for(i = 1; i <= l1; i++) t[i] = b[i+s];
  }
  ASSERTVALID_B(t, "shrTo_limbs_b result");
}

// t = b << (GHCJSBN_BITS * s) :: BigNat -> BigNat -> Int
function h$ghcjsbn_shlTo_limbs_b(t, b, s) {
  ASSERTVALID_B(b, "shlTo_limbs_b");
  ASSERTVALID_S(s, "shlTo_limbs_b");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(s < 0) throw new Error("h$ghcjsbn_shlTo_limbs_b: negative operand");
#endif
  var l = b[0], l1 = l + s, i;
  if(l === 0) {
    t[0] = 0;
  } else {
    t[0] = l1;
    for(i = 1; i <= s; i++)  t[i] = 0;
    for(i = s+1; i <= l1; i++) t[i] = b[i-s];
  }
  ASSERTVALID_B(t, "shlTo_limbs_b result");
}

function h$ghcjsbn_nbits_b(b) {
  ASSERTVALID_B(b, "nbits_b");
  var l = b[0], c = 0, s, t;
  if(l === 0) {
    return 0;
  } else {
    var r =  ((l-1)*GHCJSBN_BITS) + h$ghcjsbn_nbits_s(b[l]);
    ASSERTVALID_S(r, "nbits_b result");
    return r;
  }
}

function h$ghcjsbn_nbits_s(s) {
  ASSERTVALID_S(s, "nbits_s");
  var c = 1, t;
  if((t = s >>> 16) != 0) { s = t; c += 16; }
  if((t = s >>  8)  != 0) { s = t; c += 8; }
  if((t = s >>  4)  != 0) { s = t; c += 4; }
  if((t = s >>  2)  != 0) { s = t; c += 2; }
  if((t = s >>  1)  != 0) { s = t; c += 1; }
  ASSERTVALID_S(c, "nbits_s result");
  return c;
}

// BigNat -> Word -> String
function h$ghcjsbn_showBase(b, base) {
  ASSERTVALID_B(b, "showBase");
  ASSERTVALID_S(base, "showBase");
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_zero_b) === GHCJSBN_EQ) {
    return "0";
  } else {
    return h$ghcjsbn_showBase_rec(b, base, Math.log(base), 0);
  }
}

function h$ghcjsbn_showBase_rec(b, base, logBase, pad) {
  var bits = h$ghcjsbn_nbits_b(b), r;
  // h$log("[" + b.join(",") + "] bits: " + bits);
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b) === GHCJSBN_LT) {
    // convert short numbers to int and show in base
    var ti = h$ghcjsbn_toInt_b(b);
    // h$log("############# got base limb: " + ti);
    r = ti === 0 ? "" : ti.toString(base);
  } else {
    // divide and conquer for long numbers
    var digits = Math.floor(bits * 0.6931471805599453 / logBase);
    var d2 = Math.round(digits/2), p, q = [], r = [];
    p = h$ghcjsbn_pow_ww(base, d2);
    h$ghcjsbn_quotRem_bb(q, r, b, p);
    r = h$ghcjsbn_showBase_rec(q, base, logBase, 0) +
        h$ghcjsbn_showBase_rec(r, base, logBase, d2);
  }
  var rl = r.length;
  if(rl < pad) {
    while(rl <= pad-8) { r = "00000000" + r; rl += 8; }
    switch(pad-rl) {
    case 1: r = "0" + r; break;
    case 2: r = "00" + r; break;
    case 3: r = "000" + r; break;
    case 4: r = "0000" + r; break;
    case 5: r = "00000" + r; break;
    case 6: r = "000000" + r; break;
    case 7: r = "0000000" + r; break;
    }
  }
  return r;
}

// BigNat -> String (decimal)
function h$ghcjsbn_show(b) {
  throw new Error("show not implemented");
  // digits =
}

// BigNat -> String
function h$ghcjsbn_showHex(b) {
  throw new Error("showHex not implemented");
}

// s = b[l - 1];

// normalize a number to length l by stripping unused leading digits
/*
function h$ghcjsbn_normalize(b, l) {
  var d = b.length - l;
  while(d--) b.pop();
}

// normalize a number by stripping leading zeroes
function h$ghcjsbn_normalize0(b) {
  var l = b.length;
  while(b[--l] === 0) b.pop();
}
*/
// t = b :: BigNat -> BigNat -> Int, returns length of t
function h$ghcjsbn_copy(t, b) {
  ASSERTVALID_B(b, "copy");
  var l = b[0];
  for(var i = 0; i <= l; i++) {
    t[i] = b[i];
  }
  return l;
}

// BigNat -> Int -> Bool
// test if bit n is set in b (least significant bit is 0)
function h$ghcjsbn_testBit_b(b, n) {
  ASSERTVALID_B(b, "testBit_b");
  ASSERTVALID_S(n, "testBit_b");
  var limb = (n / GHCJSBN_BITS)|0;
  if(limb >= b[0]) {
    return false;
  } else {
    var bit = n - (GHCJSBN_BITS * limb);
    return (b[limb+1] & (1 << bit)) !== 0;
  }
}

function h$ghcjsbn_popCount_b(b) {
  ASSERTVALID_B(b, "popCount_b");
  var c = 0, l = b[0];
  while(l > 0) {
    c += h$popCnt32(b[l--]);
  }
  return c;
}

// t = b1 ^ b2 :: BigNat -> BigNat -> BigNat -> Int
// returns length of t
function h$ghcjsbn_xor_bb(b1, b2) {
  ASSERTVALID_B(b1, "xor_bb b1");
  ASSERTVALID_B(b2, "xor_bb b2");
  var i, lmin, lmax, blmax, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    lmin  = l1;
    lmax  = l2;
    blmax = b2;
  } else {
    lmin  = l2;
    lmax  = l1;
    blmax = b1;
  }
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] ^ b2[i];
  }
  for(i = lmin + 1; i <= lmax; i++) {
    t[i] = blmax[i];
  }
  while(lmax > 0 && t[lmax] === 0) lmax--;
  t[0] = lmax;
  ASSERTVALID_B(t, "xor_bb result");
  return t;
}

// b1 | b2 :: BigNat -> BigNat -> BigNat
function h$ghcjsbn_or_bb(b1, b2) {
  ASSERTVALID_B(b1, "or_bb b1");
  ASSERTVALID_B(b2, "or_bb b2");
  var i, lmin, lmax, blmax, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    lmin  = l1;
    lmax  = l2;
    blmax = b2;
  } else {
    lmin  = l2;
    lmax  = l1;
    blmax = b1;
  }
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] | b2[i];
  }
  for(i = lmin + 1; i <= lmax; i++) {
    t[i] = blmax[i];
  }
  t[0] = lmax;
  ASSERTVALID_B(t, "or_bb result");
  return t;
}

// b1 & b2 :: BigNat -> BigNat -> BigNat
function h$ghcjsbn_and_bb(b1, b2) {
  ASSERTVALID_B(b1, "and_bb b1");
  ASSERTVALID_B(b2, "and_bb b2");
  var i, lmin, l1 = b1[0], l2 = b2[0], t = [0];
  lmin = l1 <= l2 ? l1 : l2;
  for(i = 1; i <= lmin; i++) {
    t[i] = b1[i] & b2[i];
  }
  while(lmin > 0 && t[lmin] === 0) lmin--;
  t[0] = lmin;
  ASSERTVALID_B(t, "and_bb result");
  return t;
}

// b1 & (~b2) :: BigNat -> BigNat -> BigNat
// fixme is this one correct?
function h$ghcjsbn_andn_bb(b1, b2) {
  ASSERTVALID_B(b1, "andn_bb b1");
  ASSERTVALID_B(b2, "andn_bb b2");
  var i, lmin, l1 = b1[0], l2 = b2[0], t = [0];
  if(l1 <= l2) {
    for(i = 0; i <= l1; i++) t[i] = b1[i] & (~b2[i]);
  } else {
    for(i = 0;    i <= l2; i++) t[i] = b1[i] & (~b2[i]);
    for(i = l2+1; i <= l1; i++) t[i] = b1[i];
  }
  while(l1 > 0 && t[l1] === 0) l1--;
  t[0] = l1;
  ASSERTVALID_B(t, "andn_bb result");
  return t;
}

function h$ghcjsbn_toInt_b(b) {
  ASSERTVALID_B(b, "toInt_b");
  var bl = b[0], r;
  if(bl >= 2) {
    r = (b[2] << GHCJSBN_BITS) | b[1];
  } else if(bl === 1) {
    r = b[1];
  } else {
    r = 0;
  }
  ASSERTVALID_S(r, "toInt_b result");
  return r;
}

function h$ghcjsbn_toWord_b(b) {
  ASSERTVALID_B(b, "toWord_b");
  var bl = b[0], w;
  if(bl >= 2) {
    w = (b[2] << GHCJSBN_BITS) | b[1];
  } else if(bl === 1) {
    w = b[1];
  } else {
    w = 0;
  }
  ASSERTVALID_W(w, "toWord_b result");
  return w;
}

var h$integer_bigNatToWord64 = h$ghcjsbn_toWord64_b;
var h$integer_word64ToBigNat = h$ghcjsbn_mkBigNat_ww; // fixme?

#if GHCJSBN_BITS == 28
function h$ghcjsbn_toWord64_b(b) {
  ASSERTVALID_B(b, "toWord64_b");
  var len = b[0], w1, w2;
  if(len < 2) {
    w2 = 0;
    w1 = (len === 1) ? b[1] : 0;
  } else {
    w1 = b[1] | (b[2] << 28);
    if(len === 2) {
      w2 = b[2] >>> 4;
    } else {
      w2 = (b[2] >>> 4) | (b[3] << 24);
    }
  }
  ASSERTVALID_W(w2, "toWord64_b result w2");
  ASSERTVALID_W(w1, "toWord64_b result w1");
  RETURN_UBX_TUP2(w2, w1);
}
#else
#error "no toWord64_b implementation for GHCJSBN_BITS"
#endif

// BigNat -> Int -> IO ()
function h$ghcjsbn_toBigNat_s(b, s) {
  ASSERTVALID_S(s, "toBigNat_s");
#ifdef GHCJSBN_ASSERT_INTEGER
  if(s < 0) {
    throw new Error("h$ghcjsbn_toBigNat_s: negative operand");
  }
#endif
  if(s === 0) {
    b[0] = 0;
  } else if(s <= GHCJSBN_MASK) {
    b[0] = 1;
    b[1] = s;
  } else {
    b[0] = 2;
    b[1] = s & GHCJSBN_MASK;
    b[2] = s >> GHCJSBN_MASK;
  }
  ASSERTVALID_B(b, "toBigNat_s result");
}

// BigNat -> Word -> IO ()
function h$ghcjsbn_toBigNat_w(b, w) {
  ASSERTVALID_W(w, "toBigNat_w");
  if(w === 0) {
    b[0] = 0;
  } else if(w > 0 && w <= GHCJSBN_MASK) {
    b[0] = 1;
    b[1] = w;
  } else {
    b[0] = 2;
    b[1] = w & GHCJSBN_MASK;
    b[2] = w >>> GHCJSBN_BITS;
  }
  ASSERTVALID_B(b, "toBigNat_w result");
}

function h$ghcjsbn_mkBigNat_w(w) {
  ASSERTVALID_W(w, "mkBigNat_w");
  var r;
  if(w === 0) r = h$ghcjsbn_zero_b;
  else if(w === 1) r = h$ghcjsbn_one_b;
  else if(w > 0 && w <= GHCJSBN_MASK) r = [1,w];
  else r = [2, w & GHCJSBN_MASK, w >>> GHCJSBN_BITS];
  ASSERTVALID_B(r, "mkBigNat_w result");
  // ASSERTVALID_B(h$ghcjsbn_zero_b, "mkBigNat_w zero");
  return r;
}

#if GHCJSBN_BITS == 28
function h$ghcjsbn_mkBigNat_ww(hw, lw) {
  ASSERTVALID_W(hw, "mkBigNat_ww hw");
  ASSERTVALID_W(lw, "mkBigNat_ww lw");
  var r;
  if(hw === 0) r = h$ghcjsbn_mkBigNat_w(lw);
  else {
    var w1 = lw & GHCJSBN_MASK;
    var w2 = (lw >>> GHCJSBN_BITS) | ((hw << 4) & GHCJSBN_MASK);
    var w3 = hw >>> 24;
    if(w3 === 0) {
      r = [2, w1, w2];
    } else {
      r = [3, w1, w2, w3];
    }
  }
  ASSERTVALID_B(r, "mkBigNat_ww result");
  return r;
}


// fixme remove after reboot
var h$ghcjsbn_toBigNat_ww = h$ghcjsbn_mkBigNat_ww;

/* fixme re-enable after reboot
function h$ghcjsbn_toBigNat_ww(b, hw, lw) {
  ASSERTVALID_W(hw, "toBigNat_ww hw");
  ASSERTVALID_W(lw, "toBigNat_ww lw");
  if(hw === 0) h$ghcjsbn_toBigNat_w(b, lw);
  else {
    var w1 = lw & GHCJSBN_MASK;
    var w2 = (lw >>> GHCJSBN_BITS) | ((hw << 4) & GHCJSBN_MASK);
    var w3 = hw >>> 24;
    if(w3 === 0) {
      r[0] = 2;
      r[1] = w1;
      r[2] = w2;
    } else {
      r[0] = 3;
      r[1] = w1;
      r[2] = w2;
      r[3] = w3;
    }
  }
}
*/
#else
#error "no mkBigNat_ww implementation for specified GHCJSBN_BITS"
#endif

// fixme remove later
var h$integer_mkInteger = h$ghcjsbn_mkInteger;

#if GHCJSBN_BITS == 28
function h$ghcjsbn_mkInteger(nonNeg, xs) {
  // fixme write proper optimized version
  var r = [0], s = 0, t;
  while(IS_CONS(xs)) {
    t = h$ghcjsbn_shl_b(h$ghcjsbn_mkBigNat_w(UNWRAP_NUMBER(CONS_HEAD(xs))), s);
    h$ghcjsbn_addTo_bb(r, t);
    s += 31;
    xs = CONS_TAIL(xs);
  }
  if(nonNeg) {
    if(h$ghcjsbn_cmp_bb(r, h$ghcjsbn_two31_b) === GHCJSBN_LT) {
      return MK_INTEGER_S(h$ghcjsbn_toInt_b(r));
    } else {
      return MK_INTEGER_Jp(r);
    }
  } else {
    var c = h$ghcjsbn_cmp_bb(r, h$ghcjsbn_two31_b);
    if(c === GHCJSBN_GT) {
      return MK_INTEGER_Jn(r);
    } else if(c === GHCJSBN_EQ) {
      return h$ghcjsbn_negTwo31_i;
    } else {
      return MK_INTEGER_S(-h$ghcjsbn_toInt_b(r));
    }
  }
/*  var r = h$ghcjsbn_mkBigNat_w(0), l = 0, s = 0, y, t;
  while(IS_CONS(xs)) {
    l++;
    y  = UNWRAP_NUMBER(CONS_HEAD(xs));
    r[++l] = (y << s | c) & GHCJSBN_MASK;
    c  = y >>> s;
    xs = CONS_TAIL(xs);
    s  += 3;
    l++;
    if(s > GHCJSBN_BITS) {
      s  -= GHCJSBN_BITS;
      r[++l] = c & GHCJSBN_MASK;
      c >>= GHCJSBN_BITS;
    }
  }
  if(c !== 0) r[++l] =
  while(
  if(l === 0) {
    return MK_INTEGER_S(0);
  } else if(l === 1) {

  } else if(l === 2) {

  } */
}
#else
error "no mkInteger implementation for specified GHCJSBN_BITS"
#endif

// BigNat -> Word -> Int (Ordering)
function h$ghcjsbn_cmp_bw(b, w) {
  ASSERTVALID_B(b, "cmp_bw");
  ASSERTVALID_W(w, "cmp_bw");
  var w1 = w & GHCJSBN_MASK, w2 = w >>> GHCJSBN_BITS, bl = b[0];
  if(w2 === 0) {
    if(bl === 0) {
      return w1 > 0 ? GHCJSBN_LT : GHCJSBN_EQ;
    } else if(bl === 1) {
      var bw = b[1];
      return bw > w1 ? GHCJSBN_GT : (bw === w1 ? GHCJSBN_EQ : GHCJSBN_LT);
    } else {
      return GHCJSBN_GT;
    }
  } else {
    if(bl < 2) {
      return GHCJSBN_LT;
    } else if(bl > 2) {
      return GHCJSBN_GT;
    } else {
      var bw1 = b[1], bw2 = b[2];
      return (bw2 > w2) ? GHCJSBN_GT
                        : (bw2 < w2 ? GHCJSBN_LT
                                    : (bw1 > w1 ? GHCJSBN_GT
                                                : (bw1 < w1 ? GHCJSBN_LT
                                                            : GHCJSBN_EQ)));
    }
  }
}

/*
function h$ghcjsbn_gt_bw(b, w) {
  var r = h$ghcjsbn_gt_bw0(b,w);
  h$log("gt_bw result: " + r);
  return r;
}
*/

function h$ghcjsbn_gt_bw(b, w) {
  ASSERTVALID_B(b, "gt_bw");
  ASSERTVALID_W(w, "gt_bw");
  var bl = b[0];
  if(bl > 2) return true;
  else if(bl === 0) return false;
  else if(bl === 1) return w >= 0 && b[1] > w;
  else { // bl === 2
    var wh = w >>> GHCJSBN_BITS, wl = w & GHCJSBN_MASK, b2 = b[2];
    // var r = (wh > b2 || ((wh === b2) && wl > b[1]));
    // h$log("r: " + r + " " + wh + " " + wl + " " );
    return (b2 > wh || ((wh === b2) && b[1] > wl));
  }
}

// BigNat -> BigNat -> Bool
function h$ghcjsbn_eq_bb(b1, b2) {
  ASSERTVALID_B(b1, "eq_bb");
  ASSERTVALID_B(b2, "eq_bb");
  var bl1 = b1[0], bl2 = b2[0];
  if(bl1 !== bl2) {
    return false;
  } else {
    for(var i = bl1; i >= 1; i--) {
      var bw1 = b1[i], bw2 = b2[i];
      if(bw1 !== bw2) return false;
    }
  }
  return true; // GHCJSBN_EQ;
}

// BigNat -> BigNat -> Bool
function h$ghcjsbn_neq_bb(b1, b2) {
  ASSERTVALID_B(b1, "neq_bb");
  ASSERTVALID_B(b2, "neq_bb");
  var bl1 = b1[0], bl2 = b2[0];
  if(bl1 !== bl2) {
    return true;
  } else {
    for(var i = bl1; i >= 1; i--) {
      var bw1 = b1[i], bw2 = b2[i];
      if(bw1 !== bw2) return true;
    }
  }
  return false;
}

// BigNat -> BigNat -> Bool
/*
function h$ghcjsbn_eq_bw(b, w) {
  var r = h$ghcjsbn_eq_bw0(b, w);
  return r;
}
*/
function h$ghcjsbn_eq_bw(b, w) {
  ASSERTVALID_B(b, "eq_bw");
  ASSERTVALID_W(w, "eq_bw");
  var w1 = w & GHCJSBN_MASK, w2 = w >>> GHCJSBN_BITS, bl = b[0];
  if(w2 === 0) {
    if(w1 === 0) {
      return bl === 0;
    } else {
      return bl === 1 && b[1] === w;
    }
  } else {
    return bl === 2 && b[1] === w1 && b[2] === w2;
  }
}

// BigNat -> Bool
function h$ghcjsbn_isZero_b(b) {
  ASSERTVALID_B(b, "isZero_b");
  return b[0] === 0;
}

// BigNat -> Int
function h$ghcjsbn_isNull_b(b) {
  return b[0] === -1;
}

// 1 << n
function h$ghcjsbn_bitBigNat(n) {
#ifdef GHCJSBN_ASSERT_INTEGER
  if(n < 0) {
    throw new Error("bitBigNat: argument must be positive");
  }
#endif
  if(n === 0) {
    r = h$ghcjsbn_one_b;
  } else if(n < GHCJSBN_BITS) {
    r = [1, 1 << n];
  } else {
    var l = (n / GHCJSBN_BITS)|0;
    var r = [l+1];
    for(var i = 1; i<= l; i++) r[i] = 0;
    r[l+1] = 1 << (n - (GHCJSBN_BITS * l));
  }
  ASSERTVALID_B(r, "bitBigNat result");
  return r;
}


// Integer -> Int
// assumes argument is strictly positive
function h$ghcjsbn_integerLog2(i) {
  ASSERTVALID_I(i, "integerLog2");
#ifdef GHCJSBN_ASSERT_INTEGER
/*  if(h$ghcjsbn_cmp_ii(i, h$ghcjsbn_zero_i) !== GHCJSBN_GT) {
    throw new Error("integerLog2: argument must be positive");
  } */
#endif
  if(IS_INTEGER_S(i)) {
    return h$ghcjsbn_nbits_s(INTEGER_S_DATA(i))-1;
  } else {
    return h$ghcjsbn_nbits_b(INTEGER_J_DATA(i))-1;
  }
}

// Integer -> Int
// returns negation of result if integer is exactly a power of two
function h$ghcjsbn_integerLog2IsPowerOf2(i) {
  ASSERTVALID_I(i, "integerLog2IsPowerOf2");
#ifdef GHCJSBN_ASSERT_INTEGER
/*  if(h$ghcjbn_cmp_ii(i, h$ghcjsbn_zero_i) !== GHCJSBN_GT) {
    throw new Error("integerLog2IsPowerOf2: argument must be positive");
  } */
#endif
  var nb;
  if(IS_INTEGER_S(i)) {
    var sd = INTEGER_S_DATA(i);
    ASSERTVALID_S(sd, "integerLog2IsPowerOf2 sd");
    nb = h$ghcjsbn_nbits_s(sd);
    return ((sd === 1 << nb) ? -nb : nb);
  } else {
    var bd = INTEGER_J_DATA(i);
    ASSERTVALID_B(bd, "integerLog2IsPowerOf2 bd");
    nb = h$ghcjsbn_nbits_b(bd);
    var i, bl = (nb / GHCJSBN_BITS) | 0, lb = nb - GHCJSBN_BITS * bl, l = bd[bl+1];
    if(l !== (1 << lb)) return nb;
    for(i = bl; i >= 1; i--) {
      if(bd[i] !== 0) return nb;
    }
    return -nb;
  }
}

// BigNat? -> Int
function h$ghcjsbn_isValid_b(b) {
  if(!Array.isArray(b)) return 0;
  if(b.length < 1) return 0;
  var bl = b[0], w;
  if(b.length < (bl+1)) return 0;
  for(var i = 0; i <= bl; i++) {
    w = b[i];
    if(typeof w !== 'number' || (w & GHCJSBN_MASK) !== w) return 0;
  }
  return 1;
}

// BigNat -> Integer
function h$ghcjsbn_toInteger_b(b) {
  ASSERTVALID_B(b, "toInteger_b");
  if(h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b) === GHCJSBN_LT) {
    return MK_INTEGER_S(h$ghcjsbn_toInt_b(b));
  } else {
    return MK_INTEGER_Jp(b);
  }
}

// BigNat -> Integer
function h$ghcjsbn_toNegInteger_b(b) {
  ASSERTVALID_B(b, "toNegInteger_b");
  var c = h$ghcjsbn_cmp_bb(b, h$ghcjsbn_two31_b);
  if(c === GHCJSBN_LT) {
    return MK_INTEGER_S(-h$ghcjsbn_toInt_b(b));
  } else if(c === GHCJSBN_EQ) {
    return h$ghcjsbn_negTwo31_i;
  } else {
    return MK_INTEGER_Jn(b);
  }
}

// BigNat? -> Int
// (can be called with invalid bignat)
function h$ghcjsbn_sizeof_b(b) {
  if(b.length < 1) return 0;
  return (h$ghcjsbn_nbits_b(b)+31)>>5;
}

#if GHCJSBN_BITS == 28
// extract a word from a BigNat
function h$ghcjsbn_index_b(b, w) {
  var limb = (32 * w / 28)|0;
  var l    = b[0];
  var l1   = limb < l ? b[limb+1] : 0;
  var l2   = (limb+1) < l ? b[limb+2] : 0;
  var ws   = 4 * (w % 7);
  return (l1 >>> ws) | (l2 << 28 - ws);
}
#else
#error "ghcjsbn_index_b not implemented for current number of bits per limb";
#endif

function h$ghcjsbn_byteArrayToBigNat(ba, len) {
  return h$ghcjsbn_importBigNatFromByteArray(ba, 0, len, 0);
}

function h$ghcjsbn_importBigNatFromAddr(a_d, a_o, len, msbf) {
  var r = h$ghcjsbn_zero_b;
  for(var i=0;i<len;i++) {
    var off = msbf ? i : (len-i-1);
    var val = a_d.u8[off];
    var r = h$ghcjsbn_or_bb(h$ghcjsbn_shl_b(r, 8), h$ghcjsbn_mkBigNat_w(val));
  }
  return r;
}

function h$ghcjsbn_importBigNatFromByteArray(ba, ofs, len, msbf) {
  return h$ghcjsbn_importBigNatFromAddr(ba, ofs, len, msbf);
}

// ByteArray# -> Addr# -> Int# -> IO Word
function h$ghcjsbn_exportToAddr_b(bn, a_d, a_o, msbf) {
  var bytes = h$ghcjsbn_sizeInBase_b(bn, 256);
  for(var i=0;i<bytes;i++) {
    var b = h$ghcjsbn_toWord_b(bn) & 255;
    var off = msbf ? bytes-i-1 : i;
    bn = h$ghcjsbn_shr_b(bn, 8);
    a_d.u8[a_o+off] = b;
  }
  return bytes;
}

// Word# -> Addr# -> Int# -> IO Word
function h$ghcjsbn_exportToAddr_w(w, a_d, a_o, msbf) {
  return h$ghcjsbn_exportToAddr_b(h$ghcjsbn_mkBigNat_w(w), a_d, a_o, msbf);
}

//////////////////////////////////////////////////////////////////////////////
// fixme move to primop places later

var h$integer_int64ToInteger = h$ghcjsbn_toInteger_s64;

function h$ghcjsbn_toInteger_s64(s_a, s_b) {
  ASSERTVALID_S(s_a, "toInteger_s64 s_a");
  ASSERTVALID_S(s_b, "toInteger_s64 s_b");
  if(s_a === 0) {
    if(s_b >= 0) {
      return MK_INTEGER_S(s_b);
    } else {
      return MK_INTEGER_Jp(h$ghcjsbn_mkBigNat_w(s_b));
    }
  } else if(s_a === -1) {
    if(s_b < 0) {
      return MK_INTEGER_S(s_b);
    } else if(s_b === 0) {
      return MK_INTEGER_Jn(h$ghcjsbn_mkBigNat_ww(1,0));
    } else {
      return MK_INTEGER_Jn(h$ghcjsbn_mkBigNat_w(((~s_b)+1)|0));
    }
  } else if(s_a > 0) {
    return MK_INTEGER_Jp(h$ghcjsbn_mkBigNat_ww(s_a, s_b));
  } else {
    if(s_b === 0) { // zero should be correct!
      return MK_INTEGER_Jn(h$ghcjsbn_mkBigNat_ww(((~s_a)+1)|0, 0));
    } else {
      return MK_INTEGER_Jn(h$ghcjsbn_mkBigNat_ww((~s_a)|0, ((~s_b)+1)|0));
    }
    /*
     if(s_b === 0) { // zero should be correct!
      return MK_INTEGER_Jn(h$ghcjsbn_mkBigNat_ww(((~s_a)+1)|0, 0));
    } else {
      return MK_INTEGER_Jn(h$ghcjsbn_mkBigNat_ww(~s_a, ((~s_b)+1)|0));
    } */
  }
}

function h$decodeDoubleInt64(d) {
  ASSERTVALID_D(d, "DoubleDecode_Int64");
  if(isNaN(d)) {
    // RETURN_UBX_TUP4(null, -1572864, 0, 972);
    RETURN_UBX_TUP3(972, -1572864, 0);
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) { // denormal or zero
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  // negate mantissa for negative input
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
      // ret2 = 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  // prim ubx tup returns don't return the first value!
  RETURN_UBX_TUP3(ret3,ret1,ret2);
}

// fixme remove this once rebooted
function h$primop_DoubleDecode_Int64Op(d) {
  ASSERTVALID_D(d, "DoubleDecode_Int64");
  if(isNaN(d)) {
    // RETURN_UBX_TUP4(null, -1572864, 0, 972);
    RETURN_UBX_TUP4(null, -1572864, 0, 972);
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) { // denormal or zero
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  // negate mantissa for negative input
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
      // ret2 = 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  // prim ubx tup returns don't return the first value!
  RETURN_UBX_TUP4(null,ret1,ret2,ret3);
}

function h$ghcjsbn_encodeDouble_b(pos, b, e) {
  ASSERTVALID_B(b, "encodeDouble_b");
  ASSERTVALID_S(e, "encodeDouble_b");
  if(e >= 972) {
    return pos ? Infinity : -Infinity;
  }
  var ls = 1, bl = b[0], i, r = b[bl], mul = 1 << GHCJSBN_BITS, rmul = 1/mul, s = 1;
  for(i = bl-1; i >= 1; i--) {
/*    if(e > GHCJSBN_BITS) {
      e -= GHCJSBN_BITS;
      s *= rmul;
      r  = r + s * b[i];
    } else { */
      r = r * mul + s * b[i];
//    }
  }
  // h$log("remaning exp: " + e);
  if(e > 600) {
    r = r * Math.pow(2, e-600) * Math.pow(2,600);
  } else if(e < -600) {
    r = r * Math.pow(2, e+600) * Math.pow(2,-600);
  } else {
    r = r * Math.pow(2, e);
  }
  ASSERTVALID_D(r, "encodeDouble_b result");
  return pos ? r : -r;
}

function h$ghcjsbn_toDouble_b(nonNeg, b) {
  return h$ghcjsbn_encodeDouble_b(nonNeg, b, 0);
}

// fixme
var h$ghcjsbn_encodeDouble_i = h$ghcjsbn_encodeDouble_s;

function h$ghcjsbn_encodeDouble_s(m, e) {
  ASSERTVALID_S(m, "encodeDouble_s m");
  ASSERTVALID_S(e, "encodeDouble_s e");
  var r =  m * Math.pow(2, e);
  ASSERTVALID_D(r, "encodeDouble_s result");
  return r;
}

function h$ghcjsbn_sizeInBase_b(bn, base) {
  if(h$ghcjsbn_eq_bb(bn, h$ghcjsbn_zero_b)) return 1;
  var bits = h$ghcjsbn_nbits_b(bn);
  var r;
  if(h$popCnt32(base) === 1) {
    // exact result for powers of two
    var factor = Math.round(Math.log(base)/Math.log(2));
    r = Math.ceil(bits/factor);
  } else {
    r = Math.ceil(bits*Math.log(2)/Math.log(base));
  }
  return r;
}
function h$integer_gmp_mpn_sizeinbase1(w, base) {
  return h$ghcjsbn_sizeInBase_b(h$ghcjsbn_mkBigNat_w(w), base);
}
