// #define GHCJS_TRACE_ARITH 1

#ifdef GHCJS_TRACE_ARITH
function h$logArith() { h$log.apply(h$log,arguments); }
#define TRACE_ARITH(args...) h$logArith(args)
#else
#define TRACE_ARITH(args...)
#endif

#define UN(x) ((x)>>>0)

function h$hs_leInt64(h1,l1,h2,l2) {
  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s < l2s || (l1s === l2s && ((l1&1) <= (l2&1)))) ? 1 : 0;
  } else {
    return (h1 < h2) ? 1 : 0;
  }
}

function h$hs_ltInt64(h1,l1,h2,l2) {
  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s < l2s || (l1s === l2s && ((l1&1) < (l2&1)))) ? 1 : 0;
  } else {
    return (h1 < h2) ? 1 : 0;
  }
}

function h$hs_geInt64(h1,l1,h2,l2) {
  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s > l2s || (l1s === l2s && ((l1&1) >= (l2&1)))) ? 1 : 0;
  } else {
    return (h1 > h2) ? 1 : 0;
  }
}

function h$hs_gtInt64(h1,l1,h2,l2) {
  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s > l2s || (l1s === l2s && ((l1&1) > (l2&1)))) ? 1 : 0;
  } else {
    return (h1 > h2) ? 1 : 0;
  }
}

function h$hs_quotWord64(h1,l1,h2,l2) {
  TRACE_ARITH("quotWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);
  // algorithm adapted from Hacker's Delight p198

  // if divisor > numerator, just return 0
  if ((h2 > h1) || (h2 === h1 && l2 > l1)) {
    RETURN_UBX_TUP2(0,0);
  }

  if (h2 === 0) {
    if (h1 < l2) {
      var ql = h$quotRem2Word32(h1,l1,l2);
      RETURN_UBX_TUP2(0,ql);
    }
    else {
      var qh = h$quotRem2Word32(0,h1,l2);
      var rh = h$ret1; // remainder
      var ql = h$quotRem2Word32(rh,l1,l2);
      RETURN_UBX_TUP2(qh,ql);
    }
  }
  else {
    var n = Math.clz32(h2);
    // normalize divisor (MSB = 1)
    var dh = UN((h2 << n) | (l2 >>> (32-n)));
    // shift numerator 1 bit right (MSB = 0)
    var nh = h1 >>> 1;
    var nl = UN((h1 << 31) | (l1 >>> 1));
    // compute quotient estimation
    var q1 = h$quotRem2Word32(nh,nl,dh);
    // undo normalization and division of numerator by 2
    var q0 = q1 >>> (31 - n);
    if (q0 !== 0) {
      q0 = UN(q0 - 1);
    }
    // q0 might be too small by 1. q0*arg2 doesn't overflow
    var q0vh = h$hs_timesWord64(h2,l2,0,q0);
    var q0vl = h$ret1;
    var sh = h$hs_minusWord64(h1,l1,q0vh,q0vl);
    var sl = h$ret1;
    if ((sh > h2) || (sh === h2 && sl >= l2)) {
      q0 = UN(q0 + 1);
    }
    RETURN_UBX_TUP2(0,q0);
  }
}

function h$hs_remWord64(h1,l1,h2,l2) {
  TRACE_ARITH("remWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);
  var qh = h$hs_quotWord64(h1,l1,h2,l2);
  var ql = h$ret1;
  var qvh = h$hs_timesWord64(h2,l2,qh,ql);
  var qvl = h$ret1;
  return h$hs_minusWord64(h1,l1,qvh,qvl);
}

function h$hs_timesWord64(h1,l1,h2,l2) {
  TRACE_ARITH("timesWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);
  var rl = UN(l1 * l2);
  var rh = UN(UN(l2 * h1) + UN(l1 * h2));
  RETURN_UBX_TUP2(rh,rl);
}

function h$hs_minusWord64(h1,l1,h2,l2) {
  TRACE_ARITH("minusWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);
  var b  = l2 > l1 ? 1 : 0
  var rl = UN(l1 - l2);
  var rh = UN(UN(h2 - h1) - b);
  RETURN_UBX_TUP2(rh,rl);
}

function h$hs_plusWord64(h1,l1,h2,l2) {
  TRACE_ARITH("plusWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);
  var c1 = (l1 & 0x80000000) >>> 31;
  var c2 = (l2 & 0x80000000) >>> 31;
  var rl = UN(l1 & 0x7FFFFFFF) + UN(l1 & 0x7FFFFFFF);
  var cr = (rl & 0x80000000) >>> 31;
  var rh = UN(h1+h2);
  var c  = UN(c1+c2+cr);
  rl = UN(rl + UN(c << 31));
  rh = UN(rh + (c >>> 1));
  RETURN_UBX_TUP2(rh,rl);
}

function h$hs_timesInt64(h1,l1,h2,l2) {
  TRACE_ARITH("timesInt64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  // check for 0 and 1 operands
  if (h1 === 0) {
    if (l1 === 0) {
      RETURN_UBX_TUP2(0,0);
    }
    if (l1 === 1) {
      RETURN_UBX_TUP2(h2,l2);
    }
  }
  if (h2 === 0) {
    if (l2 === 0) {
      RETURN_UBX_TUP2(0,0);
    }
    if (l2 === 1) {
      RETURN_UBX_TUP2(h1,l1);
    }
  }

  var a48 = h1 >>> 16;
  var a32 = h1 & 0xFFFF;
  var a16 = l1 >>> 16;
  var a00 = l1 & 0xFFFF;

  var b48 = h2 >>> 16;
  var b32 = h2 & 0xFFFF;
  var b16 = l2 >>> 16;
  var b00 = l2 & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  RETURN_UBX_TUP2((c48 << 16) | c32, (c16 << 16) | c00);
}

function h$hs_quotInt64(h1,l1,h2,l2) {
  TRACE_ARITH("quotInt64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  throw "hs_quotInt64 not implemented yet";
  //var c = goog.math.Long.fromBits(l1,h1).div(goog.math.Long.fromBits(l2,h2));
  //RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_remInt64(h1,l1,h2,l2) {
  TRACE_ARITH("remInt64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  throw "hs_remInt64 not implemented yet";
  var c = goog.math.Long.fromBits(l1,h1).modulo(goog.math.Long.fromBits(l2,h2));
  RETURN_UBX_TUP2(c.getHighBits(), c.getLowBits());
}

function h$hs_plusInt64(h1,l1,h2,l2) {
  TRACE_ARITH("plusInt64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  const a48 = h1 >>> 16;
  const a32 = h1 & 0xFFFF;
  const a16 = l1 >>> 16;
  const a00 = l1 & 0xFFFF;

  const b48 = h2 >>> 16;
  const b32 = h2 & 0xFFFF;
  const b16 = l2 >>> 16;
  const b00 = l2 & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  RETURN_UBX_TUP2((c48 << 16) | c32, (c16 << 16) | c00);
}

function h$hs_minusInt64(h1,l1,h2,l2) {
  TRACE_ARITH("minusInt64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  // negate arg2 and adds it
  const nl2 = (~l2 + 1)    | 0;
  const nh2 = (~h2 + !nl2) | 0;
  h$hs_plusInt64(h1,l1,nh2,nl2);
}

function h$hs_leWord64(h1,l1,h2,l2) {
  TRACE_ARITH("leWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s < l2s || (l1s === l2s && ((l1&1) <= (l2&1)))) ? 1 : 0;
  } else {
    var h1s = h1 >>> 1;
    var h2s = h2 >>> 1;
    return (h1s < h2s || (h1s === h2s && ((h1&1) <= (h2&1)))) ? 1 : 0;
  }
}

function h$hs_ltWord64(h1,l1,h2,l2) {
  TRACE_ARITH("ltWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s < l2s || (l1s === l2s && ((l1&1) < (l2&1)))) ? 1 : 0;
  } else {
    var h1s = h1 >>> 1;
    var h2s = h2 >>> 1;
    return (h1s < h2s || (h1s === h2s && ((h1&1) < (h2&1)))) ? 1 : 0;
  }
}

function h$hs_geWord64(h1,l1,h2,l2) {
  TRACE_ARITH("geWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s > l2s || (l1s === l2s && ((l1&1) >= (l2&1)))) ? 1 : 0;
  } else {
    var h1s = h1 >>> 1;
    var h2s = h2 >>> 1;
    return (h1s > h2s || (h1s === h2s && ((h1&1) >= (h2&1)))) ? 1 : 0;
  }
}

function h$hs_gtWord64(h1,l1,h2,l2) {
  TRACE_ARITH("gtWord64: " + h1 + " " + l1 + " " + h2 + " " + l2);

  if(h1 === h2) {
    var l1s = l1 >>> 1;
    var l2s = l2 >>> 1;
    return (l1s > l2s || (l1s === l2s && ((l1&1) > (l2&1)))) ? 1 : 0;
  } else {
    var h1s = h1 >>> 1;
    var h2s = h2 >>> 1;
    return (h1s > h2s || (h1s === h2s && ((h1&1) > (h2&1)))) ? 1 : 0;
  }
}

function h$hs_uncheckedShiftLWord64(h,l,n) {
  TRACE_ARITH("uncheckedShiftLWord64: " + h + " " + l + " " + n);

  n &= 63;
  if (n == 0) {
    RETURN_UBX_TUP2(h,l);
  } else if (n === 32) {
    RETURN_UBX_TUP2(l,0);
  } else if (n < 32) {
    RETURN_UBX_TUP2(UN((h << n) | (l >>> (32 - n))), UN(l << n));
  } else {
    RETURN_UBX_TUP2(UN(l << (n - 32)), 0);
  }
}

function h$hs_uncheckedShiftRWord64(h,l,n) {
  TRACE_ARITH("uncheckedShiftRWord64 " + h + " " + l + " " + n);

  n &= 63;
  if(n == 0) {
    RETURN_UBX_TUP2(h, l);
  } else if(n === 32) {
    RETURN_UBX_TUP2(0, h);
  } else if(n < 32) {
    RETURN_UBX_TUP2(h >>> n, UN((l >>> n ) | (h << (32-n))));
  } else {
    RETURN_UBX_TUP2(0, (h >>> (n-32)));
  }
}

function h$hs_uncheckedShiftLLInt64(h,l,n) {
  TRACE_ARITH("uncheckedShiftLLInt64: " + h + " " + l + " " + n);

  n &= 63;
  if (n == 0) {
    RETURN_UBX_TUP2(h,l);
  } else if (n === 32) {
    RETURN_UBX_TUP2(l|0,0);
  } else if (n < 32) {
    RETURN_UBX_TUP2((h << n) | (l >>> (32 - n)), UN(l << n));
  } else {
    RETURN_UBX_TUP2(l << (n - 32), 0);
  }
}

function h$hs_uncheckedShiftRAInt64(h,l,n) {
  TRACE_ARITH("uncheckedShiftRAInt64: " + h + " " + l + " " + n);

  n &= 63;
  if (n == 0) {
    RETURN_UBX_TUP2(h,l);
  } else if (n < 32) {
    RETURN_UBX_TUP2(h >> n, UN((l >>> n) | (h << (32 - n))));
  } else {
    RETURN_UBX_TUP2(h >= 0 ? 0 : -1, UN(h >> (n - 32)));
  }
}

function h$hs_uncheckedShiftRLInt64(h,l,n) {
  TRACE_ARITH("uncheckedShiftRLInt64 " + h + " " + l + " " + n);

  n &= 63;
  if(n == 0) {
    RETURN_UBX_TUP2(h, l);
  } else if(n == 32) {
    RETURN_UBX_TUP2(0, h);
  } else if(n < 32) {
    RETURN_UBX_TUP2(h >>> n, UN((l >>> n) | (h << (32-n))));
  } else {
    RETURN_UBX_TUP2(0, (h >>> (n-32)));
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

// Compute product of two Ints. Returns (nh,ch,cl)
// where (ch,cl) are the two parts of the 64-bit result
// and nh is 0 if ch can be safely dropped (i.e. it's a sign-extension of cl).
function h$hs_timesInt2(a,b) {
  TRACE_ARITH("timesInt2 " + a + " " + b);

  // adapted from Hacker's Delight (p174)

  // check for 0 and 1 operands
  if (a === 0) {
    RETURN_UBX_TUP3(0,0,0);
  }
  if (b === 0) {
    RETURN_UBX_TUP3(0,0,0);
  }
  if (a === 1) {
    RETURN_UBX_TUP3(0,b<0?(-1):0,b);
  }
  if (b === 1) {
    RETURN_UBX_TUP3(0,a<0?(-1):0,a);
  }

  var cl = h$mulInt32(a,b);

  var ha = a >> 16;
  var la = a & 0xFFFF;

  var hb = b >> 16;
  var lb = b & 0xFFFF;

  var w0 = la * lb;
  var t  = (h$mulInt32(ha,lb) + (w0 >>> 16))|0;
  var w1 = t & 0xFFFF;
  var w2 = t >> 16;
  w1 = (h$mulInt32(la,hb) + w1)|0;

  var ch = ((h$mulInt32(ha,hb) + w2)|0 + (w1 >> 16))|0;
  var nh = ((ch === 0 && cl >= 0) || (ch === -1 && cl < 0)) ? 0 : 1

  TRACE_ARITH("timesInt2 results:" + nh + " " + ch + " " + cl);
  RETURN_UBX_TUP3(nh, ch, cl);
}


function h$mulWord32(l1,l2) {
  TRACE_ARITH("mulWord32 " + l1 + " " + l2);

  // check for 0 and 1 operands
  if (l1 === 0) {
    return 0;
  }
  if (l1 === 1) {
    return l2;
  }
  if (l2 === 0) {
    return 0;
  }
  if (l2 === 1) {
    return l1;
  }

  var a16 = l1 >>> 16;
  var a00 = l1 & 0xFFFF;

  var b16 = l2 >>> 16;
  var b00 = l2 & 0xFFFF;

  var c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c16 &= 0xFFFF;
  return ((c16 << 16) | c00);
}

function h$mul2Word32(l1,l2) {
  TRACE_ARITH("mul2Word32 " + l1 + " " + l2);

  // check for 0 and 1 operands
  if (l1 === 0) {
    RETURN_UBX_TUP2(0,0);
  }
  if (l1 === 1) {
    RETURN_UBX_TUP2(0,l2);
  }
  if (l2 === 0) {
    RETURN_UBX_TUP2(0,0);
  }
  if (l2 === 1) {
    RETURN_UBX_TUP2(0,l1);
  }

  var a16 = l1 >>> 16;
  var a00 = l1 & 0xFFFF;

  var b16 = l2 >>> 16;
  var b00 = l2 & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 &= 0xFFFF;
  RETURN_UBX_TUP2(UN((c48 << 16) | c32), UN((c16 << 16) | c00));
}

function h$quotWord32(n,d) {
  TRACE_ARITH("quotWord32 " + n + " " + d);

  // from Hacker's Delight book (p 192)
  // adapted for JavaScript
  var t = d >> 31;
  var n2 = n & ~t;
  var q = ((n2 >>> 1) / d) << 1;
  var r = (n - h$mulWord32(q,d)) >>> 0;
  var c = UN(r) >= UN(d);
  return (q + (c ? 1 : 0)) >>> 0;
}

function h$remWord32(n,d) {
  TRACE_ARITH("remWord32 " + n + " " + d);

  var t = d >> 31;
  var n2 = n & ~t;
  var q = ((n2 >>> 1) / d) << 1;
  var r = (n - h$mulWord32(q,d)) >>> 0;
  var c = UN(r) >= UN(d);
  return UN(r - (c ? d : 0));
}

function h$quotRemWord32(n,d) {
  TRACE_ARITH("quotRemWord32 " + n + " " + d);

  var t = d >> 31;
  var n2 = n & ~t;
  var q = ((n2 >>> 1) / d) << 1;
  var r = UN(n - h$mulWord32(q,d));
  var c = UN(r) >= UN(d);
  var rq = UN(q + (c ? 1 : 0));
  var rr = UN(r - (c ? d : 0));

  TRACE_ARITH("quotRemWord32 results: " + rq + " " + rr);

  RETURN_UBX_TUP2(rq,rr);
}

function h$quotRem2Word32(nh,nl,d) {
  TRACE_ARITH("quotRem2Word32 " + nh + " " + nl + " " + d);

  if (nh === 0) {
    return h$quotRemWord32(nl,d);
  }

  // from Hacker's Delight book (p196)

  nh = UN(nh);
  nl = UN(nl);
  d  = UN(d);

  if (nh >= d) {
    // WordQuotRem2Op requires that high word < divisor
    throw "h$quotRem2Word32: unexpected high word > divisor: high word=" + nh + ", divisor=" + d;
  }

  if (d === 0) {
    // FIXME: raise Haskell exception
    throw "h$quotRem2Word32: division by zero";
  }

  var s = Math.clz32(d); // 0 <= s <= 31
  d = UN(d << s);        // normalize divisor
  var dh = d >>> 16;     // break divisor up into two 16-bit digits
  var dl = d & 0xFFFF;

  //TRACE_ARITH("quotRem2Word32 s " + s);
  //TRACE_ARITH("quotRem2Word32 normalized d " + d + " " + dh + " " + dl);

  // shift dividend left too
  var un32 = UN((nh << s) | ((nl >>> (32-s)) & ((-s) >> 31)));
  var un10 = UN(nl << s);

  var un1 = un10 >>> 16;    // break lower part of the divisor into two 16-bit digits
  var un0 = un10 & 0xFFFF;

  //TRACE_ARITH("quotRem2Word32 uns " + un32 + " " + un10 + " " + un1 + " " + un0);

  var q1 = UN(un32 / dh);       // compute first quotient digit q1
  var rhat = UN(un32 - h$mulWord32(q1,dh));

  //TRACE_ARITH("quotRem2Word32 q1 rhat " + q1 + " " + rhat);

  while (q1 > 0xFFFF || h$mulWord32(q1,dl) > (UN(UN(rhat << 16) | un1))) {
    q1   = UN(q1 - 1);
    rhat = UN(rhat + dh);
    if (rhat > 0xFFFF) break;
  }

  //TRACE_ARITH("quotRem2Word32 q1' rhat' " + q1 + " " + rhat);

  var un21 = UN(UN(UN(un32 << 16) | un1) - UN(q1*d));

  //TRACE_ARITH("quotRem2Word32 un21 " + un21);

  var q0 = UN(un21 / dh);     // compute second quotient digit q0
  rhat = UN(un21 - h$mulWord32(q0,dh));

  //TRACE_ARITH("quotRem2Word32 q0 rhat " + q0 + " " + rhat);

  while (q0 > 0xFFFF || UN(q0*dl) > UN(UN(rhat << 16) + un0)) {
    q0   = UN(q0 - 1);
    rhat = UN(rhat + dh);
    if (rhat > 0xFFFF) break;
  }

  //TRACE_ARITH("quotRem2Word32 q0' rhat' " + q0 + " " + rhat);

  var rq = UN(q1 << 16 | q0);
  var rr = (UN(UN(un21 << 16) | un0) - h$mulWord32(q0,d)) >>> s;

  TRACE_ARITH("quotRem2Word32 results: " + rq + " " + rr);

  RETURN_UBX_TUP2(rq,rr);
}

function h$wordAdd2(a,b) {
  TRACE_ARITH("wordAdd2 " + a + " " + b);

  const a16 = a >>> 16;
  const a00 = a & 0xFFFF;

  const b16 = b >>> 16;
  const b00 = b & 0xFFFF;

  var c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  RETURN_UBX_TUP2(c32, (c16 << 16) | c00);
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

function h$decodeDoubleInt64(d) {
  TRACE_ARITH("decodeDoubleInt64: " + d);
  if(isNaN(d)) {
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
