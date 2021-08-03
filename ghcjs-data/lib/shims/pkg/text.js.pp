#include <ghcjs/rts.h>

function h$_hs_text_memcpy(dst_v,dst_v_zero,dst_o2,src_v,src_o_zero,src_o2,n) {
  return h$memcpy(dst_v,2*dst_o2,src_v,2*src_o2,2*n);
}

function h$_hs_text_memcmp(a_v,a_o_zero,a_o2,b_v,b_o_zero,b_o2,n) {
  return h$memcmp(a_v,2*a_o2,b_v,2*b_o2,2*n);
}

// decoder below adapted from cbits/cbits.c in the text package

#define TEXT_UTF8_ACCEPT 0
#define TEXT_UTF8_REJECT 12

var h$_text_utf8d =
   [
  /*
   * The first part of the table maps bytes to character classes that
   * to reduce the size of the transition table and create bitmasks.
   */
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  /*
   * The second part is a transition table that maps a combination of
   * a state of the automaton and a character class to a state.
   */
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12];

/*
 * A best-effort decoder. Runs until it hits either end of input or
 * the start of an invalid byte sequence.
 *
 * At exit, updates *destoff with the next offset to write to, and
 * returns the next source offset to read from.
 */

function h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero
                                         , destoff_v, destoff_o
                                         , src_v, src_o
                                         , src_end_v, src_end_o
                                         , s
                                         ) {
  if(src_v === null || src_end_v === null) {
    RETURN_UBX_TUP2(null, src_end_o);
  }
  var dsto = destoff_v.dv.getUint32(destoff_o,true) << 1;
  var srco = src_o;
  var state     = s.state;
  var codepoint = s.codepoint;
  var ddv = dest_v.dv;
  var sdv = src_v.dv;

  function decode(b) {
    var type = h$_text_utf8d[b];
    codepoint = (state !== TEXT_UTF8_ACCEPT) ?
      (b & 0x3f) | (codepoint << 6) :
      (0xff >>> type) & b;
    state = h$_text_utf8d[256 + state + type];
    return state;
  }

  while (srco < src_end_o) {
    if(decode(sdv.getUint8(srco++)) !== TEXT_UTF8_ACCEPT) {
      if(state !== TEXT_UTF8_REJECT) {
        continue;
      } else {
        break;
      }
    }
    if (codepoint <= 0xffff) {
      ddv.setUint16(dsto,codepoint,true);
      dsto += 2;
    } else {
      ddv.setUint16(dsto,(0xD7C0 + (codepoint >>> 10)),true);
      ddv.setUint16(dsto+2,(0xDC00 + (codepoint & 0x3FF)),true);
      dsto += 4;
    }
    s.last = srco;
  }

  s.state = state;
  s.codepoint = codepoint;
  destoff_v.dv.setUint32(destoff_o,dsto>>1,true);
  RETURN_UBX_TUP2(src_v, s.last);
}

function h$_hs_text_decode_utf8_state( dest_v, dest_o_zero
                                     , destoff_v, destoff_o
                                     , src_v, src_o
                                     , srcend_v, srcend_o
                                     , codepoint0_v, codepoint0_o
                                     , state0_v, state0_o
                                     ) {
  var s = { state:     state0_v.dv.getUint32(state0_o, true)
          , codepoint: codepoint0_v.dv.getUint32(codepoint0_o, true)
          , last: src_o
          };
  var ret, ret1;
  CALL_UBX_TUP2( ret
               , ret1
               , h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero
						 , destoff_v, destoff_o
						 , src_v.arr[src_o][0], src_v.arr[src_o][1]
						 , srcend_v, srcend_o
						 , s
						 ));
  src_v.arr[src_o][1] = s.last;
  state0_v.dv.setUint32(state0_o, s.state, true);
  codepoint0_v.dv.setUint32(codepoint0_o, s.codepoint, true);
  RETURN_UBX_TUP2(ret, ret1);
}

function h$_hs_text_decode_utf8( dest_v, dest_o_zero
                               , destoff_v, destoff_o
                               , src_v, src_o
                               , srcend_v, srcend_o
                               ) {
  /* Back up if we have an incomplete or invalid encoding */
  var s = { state: TEXT_UTF8_ACCEPT
          , codepoint: 0
          , last: src_o
          };
  var ret, ret1;
  CALL_UBX_TUP2( ret
	       , ret1
	       , h$_hs_text_decode_utf8_internal ( dest_v, dest_o_zero
                                                 , destoff_v, destoff_o
                                                 , src_v, src_o
                                                 , srcend_v, srcend_o
                                                 , s
                                                 ));
  RETURN_UBX_TUP2(ret, ret1);
}


/*
 * The ISO 8859-1 (aka latin-1) code points correspond exactly to the first 256 unicode
 * code-points, therefore we can trivially convert from a latin-1 encoded bytestring to
 * an UTF16 array
 */
function h$_hs_text_decode_latin1(dest_d, dest_o_zero, src_d, src_o, srcend_d, srcend_o) {
  var p = src_o;
  var d = 0;
  var su8 = src_d.u8;
  var su3 = src_d.u3;
  var du1 = dest_d.u1;

  // consume unaligned prefix
  while(p != srcend_o && p & 3) {
    du1[d++] = su8[p++];
  }

  // iterate over 32-bit aligned loads
  if(su3) {
    while (p < srcend_o - 3) {
      var w = su3[p>>2];
      du1[d++] = w          & 0xff;
      du1[d++] = (w >>> 8)  & 0xff;
      du1[d++] = (w >>> 16) & 0xff;
      du1[d++] = (w >>> 32) & 0xff;
      p += 4;
    }
  }

  // handle unaligned suffix
  while (p != srcend_o)
    du1[d++] = su8[p++];
}

function h$_hs_text_encode_utf8(destp_v, destp_o, src_v, src_o_zero, srcoff, srclen) {
  var dest_v = destp_v.arr[destp_o][0];
  var dest_o = destp_v.arr[destp_o][1];
  var src    = srcoff;
  var dest   = dest_o;
  var srcend = src + srclen;
  var srcu1  = src_v.u1;
  if(!srcu1) throw "h$_hs_text_encode_utf8: invalid alignment for source";
  var srcu3  = src_v.u3;
  var destu8 = dest_v.u8;
  while(src < srcend) {
    // run of (aligned) ascii characters
    while(srcu3 && !(src & 1) && srcend - src >= 2) {
      var w = srcu3[src>>1];
      if(w & 0xFF80FF80) break;
      destu8[dest++] = w & 0xFFFF;
      destu8[dest++] = w >>> 16;
      src += 2;
    }
    while(src < srcend) {
      var w = srcu1[src++];
      if(w <= 0x7F) {
        destu8[dest++] = w;
        break; // go back to a stream of ASCII
      } else if(w <= 0x7FF) {
        destu8[dest++] = (w >> 6) | 0xC0;
        destu8[dest++] = (w & 0x3f) | 0x80;
      } else if(w < 0xD800 || w > 0xDBFF) {
        destu8[dest++] = (w >>> 12) | 0xE0;
        destu8[dest++] = ((w >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (w & 0x3F) | 0x80;
      } else {
        var c = ((w - 0xD800) << 10) + (srcu1[src++] - 0xDC00) + 0x10000;
        destu8[dest++] = (c >>> 18) | 0xF0;
        destu8[dest++] = ((c >> 12) & 0x3F) | 0x80;
        destu8[dest++] = ((c >> 6) & 0x3F) | 0x80;
        destu8[dest++] = (c & 0x3F) | 0x80;
      }
    }
  }
  destp_v.arr[destp_o][1] = dest;
}
