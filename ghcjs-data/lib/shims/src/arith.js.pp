#include <ghcjs/rts.h>

// #define GHCJS_TRACE_ARITH 1

#ifdef GHCJS_TRACE_ARITH
function h$logArith() { h$log.apply(h$log,arguments); }
#define TRACE_ARITH(args...) h$logArith(args)
#else
#define TRACE_ARITH(args...)
#endif

function h$hs_eqWord64(a1,a2,b1,b2) {
  return (a1===b1 && a2===b2) ? 1 : 0;
}

function h$hs_neWord64(a1,a2,b1,b2) {
  return (a1 !== b1 || a2 !== b2) ? 1 : 0;
}

function h$hs_word64ToWord(a1,a2) {
  return a2;
}

function h$hs_wordToWord64(w) {
  RETURN_UBX_TUP2(0, w);
}

function h$hs_intToInt64(a) {
  RETURN_UBX_TUP2((a < 0) ? -1 : 0, a);
}

function h$hs_int64ToWord64(a1,a2) {
  RETURN_UBX_TUP2(a1, a2);
}

function h$hs_word64ToInt64(a1,a2) {
  RETURN_UBX_TUP2(a1, a2);
}

function h$hs_int64ToInt(a1,a2) {
  return a2;
}

function h$hs_negateInt64(a1,a2) {
  var c = goog.math.Long.fromBits(a2,a1).negate();
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_not64(a1,a2) {
  RETURN_UBX_TUP2(~a1, ~a2);
}

function h$hs_xor64(a1,a2,b1,b2) {
  RETURN_UBX_TUP2(a1 ^ b1, a2 ^ b2);
}

function h$hs_and64(a1,a2,b1,b2) {
  RETURN_UBX_TUP2(a1 & b1, a2 & b2);
}

function h$hs_or64(a1,a2,b1,b2) {
  RETURN_UBX_TUP2(a1 | b1, a2 | b2);
}

function h$hs_eqInt64(a1,a2,b1,b2) {
  return (a1 === b1 && a2 === b2) ? 1 : 0;
}

function h$hs_neInt64(a1,a2,b1,b2) {
  return (a1 !== b1 || a2 !== b2) ? 1 : 0;
}

function h$hs_leInt64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s < b2s || (a2s === b2s && ((a2&1) <= (b2&1)))) ? 1 : 0;
  } else {
    return (a1 < b1) ? 1 : 0;
  }
}

function h$hs_ltInt64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s < b2s || (a2s === b2s && ((a2&1) < (b2&1)))) ? 1 : 0;
  } else {
    return (a1 < b1) ? 1 : 0;
  }
}

function h$hs_geInt64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s > b2s || (a2s === b2s && ((a2&1) >= (b2&1)))) ? 1 : 0;
  } else {
    return (a1 > b1) ? 1 : 0;
  }
}

function h$hs_gtInt64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s > b2s || (a2s === b2s && ((a2&1) > (b2&1)))) ? 1 : 0;
  } else {
    return (a1 > b1) ? 1 : 0;
  }
}

function h$hs_quotWord64(a1,a2,b1,b2) {
  // var a = h$ghcjsbn_mkBigNat_ww(a1,a2); // bigFromWord64(a1,a2);
  // var b = h$ghcjsbn_mkBigNat_ww(b1,b2); // bigFromWord64(b1,b2);
  var q = h$ghcjsbn_quot_bb(h$ghcjsbn_mkBigNat_ww(a1,a2),
                            h$ghcjsbn_mkBigNat_ww(b1,b2));
  return h$ghcjsbn_toWord64_b(q); // this should return the tuple
  //RETURN_UBX_TUP2(h$ghcjsbn_toWord_b(h$ghcjsbn_shr_b(q, 32))
  //  a.divide(b);
  // RETURN_UBX_TUP2(c.shiftRight(32).intValue(), c.intValue());
}

function h$hs_timesInt64(a1,a2,b1,b2) {
  var c = goog.math.Long.fromBits(a2,a1).multiply(goog.math.Long.fromBits(b2,b1));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_quotInt64(a1,a2,b1,b2) {
  var c = goog.math.Long.fromBits(a2,a1).div(goog.math.Long.fromBits(b2,b1));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_remInt64(a1,a2,b1,b2) {
  var c = goog.math.Long.fromBits(a2,a1).modulo(goog.math.Long.fromBits(b2,b1));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_plusInt64(a1,a2,b1,b2) {
  var c = goog.math.Long.fromBits(a2,a1).add(goog.math.Long.fromBits(b2,b1));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_minusInt64(a1,a2,b1,b2) {
  var c = goog.math.Long.fromBits(a2,a1).subtract(goog.math.Long.fromBits(b2,b1));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_leWord64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s < b2s || (a2s === b2s && ((a2&1) <= (b2&1)))) ? 1 : 0;
  } else {
    var a1s = a1 >>> 1;
    var b1s = b1 >>> 1;
    return (a1s < b1s || (a1s === b1s && ((a1&1) <= (b1&1)))) ? 1 : 0;
  }
}

function h$hs_ltWord64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s < b2s || (a2s === b2s && ((a2&1) < (b2&1)))) ? 1 : 0;
  } else {
    var a1s = a1 >>> 1;
    var b1s = b1 >>> 1;
    return (a1s < b1s || (a1s === b1s && ((a1&1) < (b1&1)))) ? 1 : 0;
  }
}

function h$hs_geWord64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s > b2s || (a2s === b2s && ((a2&1) >= (b2&1)))) ? 1 : 0;
  } else {
    var a1s = a1 >>> 1;
    var b1s = b1 >>> 1;
    return (a1s > b1s || (a1s === b1s && ((a1&1) >= (b1&1)))) ? 1 : 0;
  }
}

function h$hs_gtWord64(a1,a2,b1,b2) {
  if(a1 === b1) {
    var a2s = a2 >>> 1;
    var b2s = b2 >>> 1;
    return (a2s > b2s || (a2s === b2s && ((a2&1) > (b2&1)))) ? 1 : 0;
  } else {
    var a1s = a1 >>> 1;
    var b1s = b1 >>> 1;
    return (a1s > b1s || (a1s === b1s && ((a1&1) > (b1&1)))) ? 1 : 0;
  }
}

function h$hs_remWord64(a1,a2,b1,b2) {
  /* var a = h$bigFromWord64(a1,a2);
     var b = h$bigFromWord64(b1,b2);
     var c = a.mod(b); */
  var r = h$ghcjsbn_rem_bb(h$ghcjsbn_mkBigNat_ww(a1,a2)
                           ,h$ghcjsbn_mkBigNat_ww(b1,b2));
  return h$ghcjsbn_toWord64_b(r);
  // RETURN_UBX_TUP2(c.shiftRight(32).intValue(), c.intValue());
}

function h$hs_uncheckedIShiftL64(a1,a2,n) {
  TRACE_ARITH("hs_uncheckedIShiftL64 " + a1 + " " + a2 + " " + n);
  var num = new goog.math.Long(a2,a1).shiftLeft(n);
  TRACE_ARITH("hs_uncheckedIShiftL64 result " + num.getHighBits() + " " + num.getLowBits());
  RETURN_UBX_TUP2(num.getHighBits(), num.getLowBits());
}

function h$hs_uncheckedIShiftRA64(a1,a2,n) {
  TRACE_ARITH("hs_uncheckedShiftRA64 " + a1 + " " + a2 + " " + n);
  var num = new goog.math.Long(a2,a1).shiftRight(n);
  RETURN_UBX_TUP2(num.getHighBits(), num.getLowBits());
}

// always nonnegative n?
function h$hs_uncheckedShiftL64(a1,a2,n) {
  TRACE_ARITH("hs_uncheckedShiftL64 " + a1 + " " + a2 + " " + n);
  n &= 63;
  TRACE_ARITH("hs_uncheckedShiftL64 n " + n);
  if(n == 0) {
    TRACE_ARITH("hs_uncheckedShiftL64 zero");
    RETURN_UBX_TUP2(a1, a2);
  } else if(n < 32) {
    TRACE_ARITH("hs_uncheckedShiftL64 sm32");
    RETURN_UBX_TUP2((a1 << n) | (a2 >>> (32-n)), a2 << n);
  } else {
    TRACE_ARITH("hs_uncheckedShiftL64 result " + ((a2 << (n-32))|0) + " " + 0);
    RETURN_UBX_TUP2(((a2 << (n-32))|0), 0);
  }
}

function h$hs_uncheckedShiftRL64(a1,a2,n) {
  TRACE_ARITH("hs_uncheckedShiftRL64 " + a1 + " " + a2 + " " + n);
  n &= 63;
  if(n == 0) {
    RETURN_UBX_TUP2(a1, a2);
  } else if(n < 32) {
    RETURN_UBX_TUP2(a1 >>> n, (a2 >>> n ) | (a1 << (32-n)));
  } else {
    RETURN_UBX_TUP2(0, (a1 >>> (n-32))|0);
  }
}

// fixme this function appears to deoptimize a lot due to smallint overflows
function h$imul_shim(a, b) {
    var ah  = (a >>> 16) & 0xffff;
    var al = a & 0xffff;
    var bh  = (b >>> 16) & 0xffff;
    var bl = b & 0xffff;
    // the shift by 0 fixes the sign on the high part
    // the final |0 converts the unsigned value into a signed value
    return (((al * bl)|0) + (((ah * bl + al * bh) << 16) >>> 0)|0);
}

var h$mulInt32 = Math.imul ? Math.imul : h$imul_shim;

// function h$mulInt32(a,b) {
//  return goog.math.Long.fromInt(a).multiply(goog.math.Long.fromInt(b)).getLowBits();
// }
// var hs_mulInt32 = h$mulInt32;

function h$mulWord32(a,b) {
  return goog.math.Long.fromBits(a,0).multiply(goog.math.Long.fromBits(b,0)).getLowBits();
}

function h$mul2Word32(a,b) {
  var c = goog.math.Long.fromBits(a,0).multiply(goog.math.Long.fromBits(b,0));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$quotWord32(a,b) {
  return goog.math.Long.fromBits(a,0).div(goog.math.Long.fromBits(b,0)).getLowBits();
}

function h$remWord32(a,b) {
  return goog.math.Long.fromBits(a,0).modulo(goog.math.Long.fromBits(b,0)).getLowBits();
}

function h$quotRem2Word32(a1,a2,b) {
/*  var a = h$bigFromWord64(a1,a2);
  var b = h$bigFromWord(b);
 var d = a.divide(b); */
  /* var a = h$ghcjsbn_mkBigNat_ww(a1,a2);
  var b = h$ghcjsbn_mkBigNat_w(b); */
  var q = [], r = [];
  h$ghcjsbn_quotRem_bb(q,r,h$ghcjsbn_mkBigNat_ww(a1,a2),h$ghcjsbn_mkBigNat_w(b));
  RETURN_UBX_TUP2(h$ghcjsbn_toWord_b(q), h$ghcjsbn_toWord_b(r));
  // RETURN_UBX_TUP2(d.intValue(), a.subtract(b.multiply(d)).intValue());
}

function h$wordAdd2(a,b) {
  var c = goog.math.Long.fromBits(a,0).add(goog.math.Long.fromBits(b,0));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

// this does an unsigned shift, is that ok?
function h$uncheckedShiftRL64(a1,a2,n) {
  if(n < 0) throw "unexpected right shift";
  n &= 63;
  if(n == 0) {
    RETURN_UBX_TUP2(a1, a2);
  } else if(n < 32) {
    RETURN_UBX_TUP2((a1 >>> n), (a2 >>> n) | (a1 << (32 - n)));
  } else {
    RETURN_UBX_TUP2(0, (a2 >>> (n - 32))|0);
  }
}

function h$isDoubleNegativeZero(d) {
  TRACE_ARITH("isDoubleNegativeZero: " + d);
  return (d===0 && (1/d) === -Infinity) ? 1 : 0;
}

function h$isFloatNegativeZero(d) {
  TRACE_ARITH("isFloatNegativeZero: " + d);
  return (d===0 && (1/d) === -Infinity) ? 1 : 0;
}

function h$isDoubleInfinite(d) {
  return (d === Number.NEGATIVE_INFINITY || d === Number.POSITIVE_INFINITY) ? 1 : 0;
}

function h$isFloatInfinite(d) {
  return (d === Number.NEGATIVE_INFINITY || d === Number.POSITIVE_INFINITY) ? 1 : 0;
}

function h$isFloatFinite(d) {
  return (d !== Number.NEGATIVE_INFINITY && d !== Number.POSITIVE_INFINITY && !isNaN(d)) ? 1 : 0;
}

function h$isDoubleFinite(d) {
  return (d !== Number.NEGATIVE_INFINITY && d !== Number.POSITIVE_INFINITY && !isNaN(d)) ? 1 : 0;
}

function h$isDoubleNaN(d) {
  return (isNaN(d)) ? 1 : 0;
}

function h$isFloatNaN(d) {
  return (isNaN(d)) ? 1 : 0;
}

function h$isDoubleDenormalized(d) {
  return (d !== 0 && Math.abs(d) < 2.2250738585072014e-308) ? 1 : 0;
}

function h$isFloatDenormalized(d) {
  return (d !== 0 && Math.abs(d) < 2.2250738585072014e-308) ? 1 : 0;
}

var h$convertBuffer = new ArrayBuffer(8);
var h$convertDouble = new Float64Array(h$convertBuffer);
var h$convertFloat  = new Float32Array(h$convertBuffer);
var h$convertInt    = new Int32Array(h$convertBuffer);

// use direct inspection through typed array for decoding floating point numbers if this test gives
// the expected answer. fixme: does this test catch all non-ieee or weird endianness situations?
h$convertFloat[0] = 0.75;
// h$convertFloat[0] = 1/0; // to force using fallbacks
var h$decodeFloatInt   = h$convertInt[0] === 1061158912 ? h$decodeFloatIntArray   : h$decodeFloatIntFallback;
var h$decodeDouble2Int = h$convertInt[0] === 1061158912 ? h$decodeDouble2IntArray : h$decodeDouble2IntFallback;

function h$decodeFloatIntArray(d) {
    TRACE_ARITH("decodeFloatIntArray: " + d);
    if(isNaN(d)) {
        RETURN_UBX_TUP2(-12582912, 105);
    }
    h$convertFloat[0] = d;
    var i = h$convertInt[0];
    var exp = (i >> 23) & 0xff;
    var sgn = 2 * (i >> 31) + 1;
    var s   = i&8388607;
    if(exp === 0) { // zero or denormal
        if(s === 0) {
            TRACE_ARITH("decodeFloatIntArray s: 0 e: 0");
            RETURN_UBX_TUP2(0, 0);
        } else {
            h$convertFloat[0] = d*8388608;
            i = h$convertInt[0];
            TRACE_ARITH("decodeFloatIntArray s: " + (sgn * (i&8388607)) +  " e: " + ((i&2139095040) >> 23) - 173);
            RETURN_UBX_TUP2(sgn*(i&8388607), ((i&2139095040) >> 23) - 173)
        }
    } else {
      TRACE_ARITH("decodeFloatIntArray s: " + (sgn * (s|8388608)) +  " e: " + (exp-150));
      RETURN_UBX_TUP2(sgn * (s|8388608), exp - 150);
    }
}

function h$decodeFloatIntFallback(d) {
    TRACE_ARITH("decodeFloatIntFallback: " + d);
    if(isNaN(d)) {
      RETURN_UBX_TUP2(-12582912, 105);
    }
    var ret0, ret1;
    CALL_UBX_TUP2(ret0, ret1, h$integer_cmm_decodeDoublezhFallback(d));
    var exponent = ret0 + 29;
    var significand = ret1.shiftRight(28).add(h$bigOne).shiftRight(1).intValue();
    if(exponent > 105) {
      exponent = 105;
      significand = d > 0 ? 8388608 : -8388608;
    } else if(exponent < -151 || significand === 0) {
      significand = 0;
      exponent = 0;
    }
    TRACE_ARITH("decodeFloatIntFallback s: " + significand + " e: " + exponent);
    RETURN_UBX_TUP2(significand, exponent);
}

function h$decodeDouble2IntArray(d) {
    TRACE_ARITH("decodeDouble2IntArray: " + d);
    if(isNaN(d)) {
	RETURN_UBX_TUP4(1, -1572864, 0, 972);
    }
    h$convertDouble[0] = d;
  TRACE_ARITH("decodeDouble2IntArray binary: " + h$convertInt[0].toString(2) + " " + h$convertInt[1].toString(2));
    var i1 = h$convertInt[1];
    var ret1, ret2 = h$convertInt[0], ret3;
    var exp = (i1&2146435072)>>>20;
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
    TRACE_ARITH("decodeDouble2IntArray: exp: " + ret3 + " significand: " + ret1 + " " + ret2);
    RETURN_UBX_TUP4(i1<0?-1:1,ret1,ret2,ret3);
}

function h$decodeDouble2IntFallback(d) {
    TRACE_ARITH("decodeDouble2IntFallback: " + d);
    if(isNaN(d)) {
	RETURN_UBX_TUP4(1,-1572864,0,972);
    }
    var exponent, significand;
    CALL_UBX_TUP2(exponent, significand, h$integer_cmm_decodeDoublezhFallback(d));
    var sign = d<0?-1:1;
    var s = significand.abs();
    var ret1 = s.shiftRight(32).intValue();
    var ret2 = s.intValue();
    var ret3 = exponent;
    TRACE_ARITH("decodeDouble2IntFallback: exp: " + ret3 + " significand: " + ret1 + " " + ret2);
    RETURN_UBX_TUP4(sign, ret1, ret2, ret3);
}

// round .5 to nearest even number
function h$rintDouble(a) {
  var rounda = Math.round(a);
  if(a >= 0) {
    if(a%1===0.5 && rounda%2===1) { // tie
      return rounda-1;
    } else {
      return rounda;
    }
  } else {
    if(a%1===-0.5 && rounda%2===-1) { // tie
      return rounda-1;
    } else {
      return rounda;
    }
  }
}
var h$rintFloat = h$rintDouble;

function h$acos(d) { return Math.acos(d); }
function h$acosf(f) { return Math.acos(f); }

function h$asin(d) { return Math.asin(d); }
function h$asinf(f) { return Math.asin(f); }

function h$atan(d) { return Math.atan(d); }
function h$atanf(f) { return Math.atan(f); }

function h$atan2(x,y) { return Math.atan2(x,y); }
function h$atan2f(x,y) { return Math.atan2(x,y); }

function h$cos(d) { return Math.cos(d); }
function h$cosf(f) { return Math.cos(f); }

function h$sin(d) { return Math.sin(d); }
function h$sinf(f) { return Math.sin(f); }

function h$tan(d) { return Math.tan(d); }
function h$tanf(f) { return Math.tan(f); }

function h$cosh(d) { return (Math.exp(d)+Math.exp(-d))/2; }
function h$coshf(f) { return h$cosh(f); }

function h$sinh(d) { return (Math.exp(d)-Math.exp(-d))/2; }
function h$sinhf(f) { return h$sinh(f); }

function h$tanh(d) { return (Math.exp(2*d)-1)/(Math.exp(2*d)+1); }
function h$tanhf(f) { return h$tanh(f); }

var h$popCntTab =
   [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8];

function h$popCnt32(x) {
   return h$popCntTab[x&0xFF] +
          h$popCntTab[(x>>>8)&0xFF] +
          h$popCntTab[(x>>>16)&0xFF] +
          h$popCntTab[(x>>>24)&0xFF];
}

function h$popCnt64(x1,x2) {
   return h$popCntTab[x1&0xFF] +
          h$popCntTab[(x1>>>8)&0xFF] +
          h$popCntTab[(x1>>>16)&0xFF] +
          h$popCntTab[(x1>>>24)&0xFF] +
          h$popCntTab[x2&0xFF] +
          h$popCntTab[(x2>>>8)&0xFF] +
          h$popCntTab[(x2>>>16)&0xFF] +
          h$popCntTab[(x2>>>24)&0xFF];
}

function h$bswap64(x1,x2) {
  RETURN_UBX_TUP2((x2 >>> 24) | (x2 << 24) | ((x2 & 0xFF00) << 8) | ((x2 & 0xFF0000) >> 8)
                 ,(x1 >>> 24) | (x1 << 24) | ((x1 & 0xFF00) << 8) | ((x1 & 0xFF0000) >> 8));
}

var h$clz32 = Math.clz32 || function(x) {
    if (x < 0)   return 0;
    if (x === 0) return 32;
    return 31 - ((Math.log(x) / Math.LN2) | 0);
}
function h$clz8(x) {
    return h$clz32(x&255)-24;
}
function h$clz16(x) {
    return h$clz32(x&65535)-16;
}

function h$clz64(x1,x2) {
    return (x1 === 0) ? 32 + h$clz32(x2) : h$clz32(x1);
}

var h$ctz32tbl = [32,0,1,26,2,23,27,0,3,16,24,30,28,11,0,13,4,7,17,0,25,22,31,15,29,10,12,6,0,21,14,9,5,20,8,19,18,0,0,0,0,0,31];
function h$ctz32(x) {
    return h$ctz32tbl[((x&-x)%37)&63];
}
function h$ctz16(x) {
    return h$ctz32(x|65536);
}
function h$ctz8(x) {
    return h$ctz32(x|256);
}
function h$ctz64(x1,x2) {
    return (x2 === 0) ? 32 + h$ctz32(x1) : h$ctz32(x2);
}

var h$fround            = null;
var h$truncateFloat_buf = null;
if(typeof Math.fround === 'function') {
  h$fround = function(f) {
    TRACE_ARITH("fround (native): " + f);
    return Math.fround(f);
  }
} else {
  h$fround = function(f) {
    TRACE_ARITH("fround (buffer): " + f);
    if(!h$truncateFloat_buf) h$truncateFloat_buf = new Float32Array(1);
    h$truncateFloat_buf[0] = f;
    return h$truncateFloat_buf[0];
  }
}
