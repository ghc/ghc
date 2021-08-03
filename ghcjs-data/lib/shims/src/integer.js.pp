#include <ghcjs/rts.h>

/* 
   Integer and integer-gmp support
   partial GMP emulation

   note: sign behaves different from real gmp sign,
         value is always zero, don't use it for comparisons
*/

#ifdef GHCJS_TRACE_INTEGER
function h$logInteger() { h$log.apply(h$log,arguments); }
#define TRACE_INTEGER(args...) h$logInteger(args)
#else
#define TRACE_INTEGER(args...)
#endif

var h$bigZero = h$nbv(0);
var h$bigOne = h$nbv(1);
var h$bigCache = [];
(function() {
  for(var i=0;i<=100;i++) {
    h$bigCache.push(h$nbv(i));
  }
})();

// convert a value to a BigInt
function h$bigFromInt(v) {
  TRACE_INTEGER("h$bigFromInt: " + v);
  var v0 = v|0;
  if(v0 >= 0) {
    if(v0 <= 100) {
      return h$bigCache[v0];
    } else if(v0 < 268435456) { // 67108864) { // guaranteed to fit in one digit
      return h$nbv(v0);
    }
    var r1 = h$nbv(v0 >>> 16);
    var r2 = h$nbi();
    r1.lShiftTo(16,r2);
    r1.fromInt(v0 & 0xffff);
    var r3 = r1.or(r2);
    TRACE_INTEGER("h$bigFromInt result: " + r3.toString());
    return r3;
  } else {
    v0 = -v0;
    if(v0 < 268435456) { // 67108864) {
      return h$nbv(v0).negate();
    }
    var r1 = h$nbv(v0 >>> 16);
    var r2 = h$nbi();
    r1.lShiftTo(16,r2);
    r1.fromInt(v0 & 0xffff);
    var r3 = r1.or(r2);
    BigInteger.ZERO.subTo(r3,r2);
    TRACE_INTEGER("h$bigFromInt result: " + r2.toString());
    return r2;
  }
}

function h$bigFromWord(v) {
  var v0 = v|0;
  if(v0 >= 0) {
    if(v0 <= 100) {
      return h$bigCache[v0];
    } else if(v0 < 268435456) { // 67108864) { // guaranteed to fit in one digit
      return h$nbv(v0);
    }
  }
  var r1 = h$nbv(v0 >>> 16);
  var r2 = h$nbv(0);
  r1.lShiftTo(16,r2);
  r1.fromInt(v0 & 0xffff);
  return r1.or(r2);
}

function h$bigFromInt64(v1,v2) {
  TRACE_INTEGER("h$bigFromInt64: " + v1 + " " + v2);
  var v10 = v1|0;
  var v20 = v2|0;
  var r = new BigInteger([ v10 >>  24, (v10 & 0xff0000) >> 16, (v10 & 0xff00) >> 8, v10 & 0xff
                         , v20 >>> 24, (v20 & 0xff0000) >> 16, (v20 & 0xff00) >> 8, v20 & 0xff
                         ]);
  TRACE_INTEGER("h$bigFromInt64 result: " + r.toString());
  return r;
}

function h$bigFromWord64(v1,v2) {
  TRACE_INTEGER("h$bigFromWord64: " + v1 + " " + v2);
  var v10 = v1|0;
  var v20 = v2|0;
  var arr = [ 0, v10 >>> 24, (v10 & 0xff0000) >> 16, (v10 & 0xff00) >> 8, v10 & 0xff
                         , v20 >>> 24, (v20 & 0xff0000) >> 16, (v20 & 0xff00) >> 8, v20 & 0xff
                         ];
  TRACE_INTEGER(arr);
  var r = new BigInteger([ 0, v10 >>> 24, (v10 & 0xff0000) >> 16, (v10 & 0xff00) >> 8, v10 & 0xff
                         , v20 >>> 24, (v20 & 0xff0000) >> 16, (v20 & 0xff00) >> 8, v20 & 0xff
                         ]);
  TRACE_INTEGER("h$bigFromWord64 result: " + r.toString());
  return r;
}

function h$bigFromNumber(n) {
  var ra = [];
  var s = 0;
  if(n < 0) {
    n = -n;
    s = -1;
  }
  var b = 1;
  while(n >= b) {
    ra.unshift((n/b)&0xff);
    b *= 256;
  }
  ra.unshift(s);
  return new BigInteger(ra);
}

function h$encodeNumber(big,e) {
  var m = Math.pow(2,e);
  if(m === Infinity) {
    switch(big.signum()) {
      case 1: return Infinity;
      case 0: return 0;
      default: return -Infinity;
    }
  }
  var b = big.toByteArray();
  var l = b.length;
  var r = 0;
  TRACE_INTEGER("h$encodeNumber", b);
  for(var i=l-1;i>=1;i--) {
  TRACE_INTEGER("h$encodeNumber i: " + i + " b[i] " + b[i]);
    r += m * Math.pow(2,(l-i-1)*8) * (b[i] & 0xff);
    TRACE_INTEGER(r);
  }
  // last one signed
  if(b[0] != 0) {
    r += m * Math.pow(2,(l-1)*8) * b[0];
  }
  TRACE_INTEGER("h$encodeNumber result: " + r);
  return r;
}

function h$integer_cmm_cmpIntegerzh(sa, abits, sb, bbits) {
  TRACE_INTEGER("cmpInteger: " + abits + " " + bbits);
  var c = abits.compareTo(bbits);
  return c == 0 ? 0 : c > 0 ? 1 : -1;
}

function h$integer_cmm_cmpIntegerIntzh(sa, abits, b) {
  TRACE_INTEGER("cmpIntegerInt: " + abits + " " + b);
  var c = abits.compareTo(h$bigFromInt(b));
  return c == 0 ? 0 : c > 0 ? 1 : -1;
}

function h$integer_cmm_plusIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("plusInteger: " + abits + " " + bbits);
    return abits.add(bbits);
}

function h$integer_cmm_plusIntegerIntzh(sa, abits, b) {
  TRACE_INTEGER("plusIntegerInt: " + abits + " " + b);
  return abits.add(h$bigFromInt(b));
}

function h$integer_cmm_minusIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("minusInteger: " + abits + " " + bbits);
    return abits.subtract(bbits);
}

function h$integer_cmm_minusIntegerIntzh(sa, abits, b) {
   TRACE_INTEGER("minusIntegerInt: " + abits + " " + b);
   return abits.subtract(h$bigFromInt(b));
}

function h$integer_cmm_timesIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("timesInteger: " + abits + " " + bbits);
    return abits.multiply(bbits);
}

function h$integer_cmm_timesIntegerIntzh(sa, abits, b) {
  TRACE_INTEGER("timesIntegerInt: " + abits + " " + b);
  return abits.multiply(h$bigFromInt(b));
}

// fixme make more efficient, divideRemainder
function h$integer_cmm_quotRemIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("quotRemInteger: " + abits + " " + bbits);
    var q = abits.divide(bbits);
    TRACE_INTEGER("quotRemInteger q: " + q.toString());
    var r = abits.subtract(q.multiply(bbits));
    TRACE_INTEGER("quotRemInteger r: " + r.toString());
    RETURN_UBX_TUP2(q, r);
}

function h$integer_cmm_quotRemIntegerWordzh(sa, abits, b) {
    var bbits = h$bigFromWord(b);
    TRACE_INTEGER("quotRemIntegerWord: " + abits + " " + b);
    var q = abits.divide(bbits);
    RETURN_UBX_TUP2(q, abits.subtract(q.multiply(bbits)));
}

function h$integer_cmm_quotIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("quotInteger: " + abits + " " + bbits);
    return abits.divide(bbits);
}

function h$integer_cmm_quotIntegerWordzh(sa, abits, b) {
    TRACE_INTEGER("quotIntegerWord: " + abits + " " + b);
    return abits.divide(h$bigFromWord(b));
}

function h$integer_cmm_remIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("remInteger: " + abits + " " + bbits);
    return abits.subtract(bbits.multiply(abits.divide(bbits)));
}

function h$integer_cmm_remIntegerWordzh(sa, abits, b) {
    TRACE_INTEGER("remIntegerWord: " + abits + " " + b);
    var bbits = h$bigFromWord(b);
    return abits.subtract(bbits.multiply(abits.divide(bbits)));
}

function h$integer_cmm_divModIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("divModInteger: " + abits + " " + bbits);
    var d = abits.divide(bbits);
    var m = abits.subtract(d.multiply(bbits));
    TRACE_INTEGER("signums: " + abits.signum() + " " + bbits.signum() + " " + m.signum());
    if(abits.signum()!==bbits.signum() && m.signum() !== 0) {
        d = d.subtract(h$bigOne);
        m.addTo(bbits, m);
    }
    RETURN_UBX_TUP2(d, m);
}

function h$integer_cmm_divModIntegerWordzh(sa, abits, b) {
    TRACE_INTEGER("divModIntegerWord: " + abits + " " + b);
    return h$integer_cmm_divModIntegerzh(sa, abits, 0, h$bigFromWord(b));
}

function h$integer_cmm_divIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("divInteger "  + abits + " " + bbits);
    var d = abits.divide(bbits);
    var m = abits.subtract(d.multiply(bbits));
    TRACE_INTEGER("signums: " + abits.signum() + " " + bbits.signum() + " " + m.signum());
    if(abits.signum()!==bbits.signum() && m.signum() !== 0) {
        TRACE_INTEGER("subtracting");
        d = d.subtract(h$bigOne);
    }
    TRACE_INTEGER("divInteger result "  + d);
    return d;
}

function h$integer_cmm_divIntegerWordzh(sa, abits, b) {
    TRACE_INTEGER("divIntegerWord "  + abits + " " + b);
    return h$integer_cmm_divIntegerzh(sa, abits, 0, h$bigFromWord(b));
}

function h$integer_cmm_modIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("modInteger "  + abits + " " + bbits);
    var d = abits.divide(bbits);
    var m = abits.subtract(d.multiply(bbits));
    if(abits.signum()!==bbits.signum() && m.signum() !== 0) {
        m.addTo(bbits, m);
    }
    return m;
}

function h$integer_cmm_modIntegerWordzh(sa, abits, b) {
    TRACE_INTEGER("modIntegerWord "  + abits + " " + b);
    return h$integer_cmm_modIntegerzh(sa, abits, 0, h$bigFromWord(b));
}

function h$integer_cmm_divExactIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("divExactInteger "  + abits + " " + bbits);
    return abits.divide(bbits);
}

function h$integer_cmm_divExactIntegerWordzh(sa, abits, b) {
    TRACE_INTEGER("divExactIntegerWord "  + abits + " " + b);
    return abits.divide(h$bigFromWord(b));
}

function h$gcd(a, b) {
    var x = a.abs();
    var y = b.abs();
    var big, small;
    if(x.compareTo(y) < 0) {
        small = x;
        big = y;
    } else {
        small = y;
        big = x;
    }
    while(small.signum() !== 0) {
        var q = big.divide(small);
        var r = big.subtract(q.multiply(small));
        big = small;
        small = r;
    }
    return big;
}

function h$integer_cmm_gcdIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("gcdInteger "  + abits + " " + bbits);
    return h$gcd(abits, bbits);
}

function h$integer_cmm_gcdIntegerIntzh(sa, abits, b) {
    TRACE_INTEGER("gcdIntegerInt "  + abits + " " + b);
    var r = h$gcd(abits, h$bigFromInt(b));
    return r.intValue();
}

function h$integer_cmm_gcdIntzh(a, b) {
        var x = a<0 ? -a : a;
        var y = b<0 ? -b : b;
        var big, small;
        if(x<y) {
            small = x;
            big = y;
        } else {
            small = y;
            big = x;
        }
        while(small!==0) {
            var r = big % small;
            big = small;
            small = r;
        }
        return big;
}

function h$integer_cmm_powIntegerzh(sa, abits, b) {
    TRACE_INTEGER("powInteger "  + abits + " " + b);
    if(b >= 0) {
      return abits.pow(b);
    } else {
      return abits.pow(b + 2147483648);
    }
}

// (a ^ b) % c
function h$integer_cmm_powModIntegerzh(sa, abits, sb, bbits, sc, cbits) {
    TRACE_INTEGER("powModInteger "  + abits + " " + bbits + " " + cbits);
    return abits.modPow(bbits, cbits);
}

// warning, there is no protection against side-channel attacks here
function h$integer_cmm_powModSecIntegerzh(sa, abits, sb, bbits, sc, cbits) {
    TRACE_INTEGER("powModSecInteger "  + abits + " " + bbits + " " + cbits);
    return h$integer_cmm_powModIntegerzh(sa, abits, sb, bbits, sc, cbits);
}

function h$integer_cmm_recipModIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("recipModInteger "  + abits + " " + bbits);
    return abits.modInverse(bbits);
}

function h$integer_cmm_nextPrimeIntegerzh(sa, abits) {
    TRACE_INTEGER("nextPrimeInteger "  + abits);
    var n = abits.add(BigInteger.ONE);
    while(true) {
      if(n.isProbablePrime(50)) return n;
      n.addTo(BigInteger.ONE, n);
    }
}

function h$integer_cmm_testPrimeIntegerzh(sa, abits, b) {
    TRACE_INTEGER("testPrimeInteger "  + abits + " " + b);
    return abits.isProbablePrime(b) ? 1 : 0;
}

function h$integer_cmm_sizeInBasezh(sa, abits, b) {
    TRACE_INTEGER("sizeInBase " + abits);
    return Math.ceil(abits.bitLength() * Math.log(2) / Math.log(b));
}

var h$oneOverLog2 = 1 / Math.log(2);

function h$integer_cmm_decodeDoublezh(x) {
    TRACE_INTEGER("integer_cmm_decodeDouble " + x);
    var sgn, ret1, ret2, ret3;
    CALL_UBX_TUP4(sgn, ret1, ret2, ret3, h$decodeDouble2Int(x));
    var b   = h$bigFromInt(ret1).shiftLeft(32).add(h$bigFromWord(ret2));
    ret1 = (!isNaN(x) && sgn < 0) ? b.negate() : b;
    // var ret3 = h$ret3;
    TRACE_INTEGER("integer_cmm_decodeDouble s: " + ret1 + " e: " + ret3);
    RETURN_UBX_TUP2(ret3, ret1);
}

function h$integer_cmm_decodeDoublezhFallback(x) {
    TRACE_INTEGER("integer_cmm_decodeDouble fallback " + x);
    if(isNaN(x)) {
      RETURN_UBX_TUP2(972, h$bigFromInt(3).shiftLeft(51).negate());
    }
    if( x < 0 ) {
        var result, ret1;
        CALL_UBX_TUP2(result, ret1, h$integer_cmm_decodeDoublezh(-x));
        RETURN_UBX_TUP2(result, ret1.negate());
    }
    if(x === Number.POSITIVE_INFINITY) {
        RETURN_UBX_TUP2(972, h$bigOne.shiftLeft(52));
    }
    var exponent = (Math.floor(Math.log(x) * h$oneOverLog2)-52)|0;
    var n;
    // prevent overflow
    if(exponent < -1000) {
      n = x * Math.pow(2,-exponent-128) * Math.pow(2,128);
    } else if(exponent > 900) {
      n = x * Math.pow(2,-exponent+128) * Math.pow(2,-128);
    } else {
      n = x * Math.pow(2,-exponent);
    }
    // fixup precision, do we also need the other way (exponent++) ?
    if(Math.abs(n - Math.floor(n) - 0.5) < 0.0001) {
      exponent--;
      n *= 2;
    }
    var ret1 = h$bigFromNumber(n);
    TRACE_INTEGER("integer_cmm_decodeDoubleFallback s: " + h$ret1 + " e: " + exponent);
    RETURN_UBX_TUP2(exponent, ret1);
}

function h$integer_cmm_int2Integerzh(i) {
    TRACE_INTEGER("int2Integer "  + i);
    RETURN_UBX_TUP2(0, h$bigFromInt(i));
}

function h$integer_cmm_word2Integerzh(i) {
    TRACE_INTEGER("word2Integer "  + i);
    RETURN_UBX_TUP2(0, h$bigFromWord(i));
}

function h$integer_cmm_andIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("andInteger "  + abits + " " + bbits);
    return abits.and(bbits);
}

function h$integer_cmm_orIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("orInteger "  + abits + " " + bbits);
    return abits.or(bbits);
}

function h$integer_cmm_xorIntegerzh(sa, abits, sb, bbits) {
    TRACE_INTEGER("xorInteger "  + abits + " " + bbits);
    return abits.xor(bbits);
}

function h$integer_cmm_testBitIntegerzh(sa, abits, bit) {
    return abits.testBit(bit)?1:0;
}

function h$integer_cmm_mul2ExpIntegerzh(sa, abits, b) {
    TRACE_INTEGER("mul2ExpInteger "  + abits + " " + b);
    return abits.shiftLeft(b);
}

function h$integer_cmm_fdivQ2ExpIntegerzh(sa, abits, b) {
    TRACE_INTEGER("fdivQ2ExpInteger "  + abits + " " + b);
    return abits.shiftRight(b);
}

function h$integer_cmm_complementIntegerzh(sa, abits) {
    TRACE_INTEGER("complementInteger "  + abits);
    return abits.not();
}

function h$integer_cmm_int64ToIntegerzh(a0, a1) {
    TRACE_INTEGER("int64ToInteger "  + a0 + " " + a1);
    RETURN_UBX_TUP2(0, h$bigFromInt64(a0,a1));
}

function h$integer_cmm_word64ToIntegerzh(a0, a1) {
    TRACE_INTEGER("word64ToInteger "  + a0 + " " + a1);
    RETURN_UBX_TUP2(0, h$bigFromWord64(a0,a1))
}

function h$hs_integerToInt64(as, abits) {
    TRACE_INTEGER("integerToInt64 "  + abits);
    RETURN_UBX_TUP2(abits.shiftRight(32).intValue(), abits.intValue());
}

function h$hs_integerToWord64(as, abits) {
    TRACE_INTEGER("integerToWord64 "  + abits);
    RETURN_UBX_TUP2(abits.shiftRight(32).intValue(), abits.intValue());
}

function h$integer_cmm_integer2Intzh(as, abits) {
   TRACE_INTEGER("integer2Int "  + abits);
   return abits.intValue();
}

function h$integer_cbits_encodeDouble(as,abits,e) {
    TRACE_INTEGER("encodeDouble "  + abits + " " + e);
   return h$encodeNumber(abits,e);
}

function h$integer_cbits_encodeFloat(as,abits,e) {
    TRACE_INTEGER("integerToInt64 "  + abits + " " + e);
   return h$encodeNumber(abits,e);
}

function h$__int_encodeDouble(i,e) {
   return i * Math.pow(2,e);
}

function h$__int_encodeFloat(i,e) {
   return i * Math.pow(2,e);
}

function h$integer_wordLog2(w) {
    TRACE_INTEGER("integer_wordLog2 " + w);
    return 31 - h$clz32(w);
}

function h$integer_integerLog2(i) {
    TRACE_INTEGER("integer_integerLog2 " + i + " -> " + (i.bitLength()-1));
    return i.bitLength()-1;
}

function h$integer_integerLog2IsPowerOf2(i) {
    TRACE_INTEGER("integer_integerLog2IsPowerOf2 " + i);
    var b = i.bitLength();
    var ret1 = (b === 0 || i.getLowestSetBit() !== b) ? 1 : 0;
    TRACE_INTEGER("integer_integerLog2IsPowerOf2 result" + ret1 + " " + (b-1));
    RETURN_UBX_TUP2(b-1, ret1);
}

function h$integer_intLog2IsPowerOf2(i) {
    TRACE_INTEGER("integer_intLog2IsPowerOf2 " + i);
    var l = 31 - h$clz32(i);
    var ret1 = (i !== (1 << l)) ? 1 : 0;
    TRACE_INTEGER("integer_intLog2IsPowerOf2 result " + ret1 + " " + l);
    RETURN_UBX_TUP2(l, ret1);
}

function h$integer_roundingMode(i,j) {
    TRACE_INTEGER("integer_roundingMode");
    return 1; // round to even, is that correct?
}

function h$integer_smartJ(i) {
    TRACE_INTEGER("integer_smartJ");
    if(i.bitLength() >= 32) return MK_INTEGER_J(i);
    return MK_INTEGER_S(i.intValue()|0);
}

function h$integer_mpzToInteger(i) {
    TRACE_INTEGER("integer_mpzToInteger");
    if(typeof i === 'number') return MK_INTEGER_S(i);
    return h$integer_smartJ(i);
}

var h$integer_negTwoThirtyOne = MK_INTEGER_J(h$bigFromInt(-2147483648).negate());
function h$integer_mpzNeg(i) {
    TRACE_INTEGER("integer_mpzNeg: " + i + " " + typeof i);
    if(typeof i === 'number') {
	return (i === -2147483648) ? h$integer_negTwoThirtyOne : -i;
    }
    return i.negate();
}

function h$integer_absInteger(i) {
    TRACE_INTEGER("integer_absInteger");
    return i.abs();
}

function h$integer_negateInteger(i) {
    TRACE_INTEGER("integer_negateInteger: " + i + " -> " + i.negate());
    return i.negate();
}
