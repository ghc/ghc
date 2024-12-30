//#OPTIONS: CPP

// these aren't added to the CAFs, so the list stays in mem indefinitely, is that a problem?
#ifdef GHCJS_PROF
function h$strt(str, cc) { return MK_LAZY_CC(function() { return h$toHsString(str, cc); }, cc); }
function h$strta(str, cc) { return MK_LAZY_CC(function() { return h$toHsStringA(str, cc); }, cc); }
function h$strtb(arr, cc) { return MK_LAZY_CC(function() { return h$toHsStringMU8(arr, cc); }, cc); }
#else
function h$strt(str) { return MK_LAZY(function() { return h$toHsString(str); }); }
function h$strta(str) { return MK_LAZY(function() { return h$toHsStringA(str); }); }
function h$strtb(arr) { return MK_LAZY(function() { return h$toHsStringMU8(arr); }); }
#endif

// unpack strings without thunks
#ifdef GHCJS_PROF
function h$ustra(str, cc) { return h$toHsStringA(str, cc); }
function h$ustr(str, cc) { return h$toHsString(str, cc); }     // utf8 string, string argument
function h$urstra(arr, cc) { return h$toHsList(arr, cc); }     // ascii string, array of codepoints argument
function h$urstr(arr, cc) { return h$toHsStringMU8(arr, cc); } // utf8 string, array of bytes argumnt
#else
function h$ustra(str) { return h$toHsStringA(str); }
function h$ustr(str) { return h$toHsString(str); }
function h$urstra(arr) { return h$toHsList(arr); }
function h$urstr(arr) { return h$toHsStringMU8(arr); }
#endif

function h$caseMapping(x) {
    return (x%2)?-((x+1)>>1):(x>>1);
}

var h$toUpper = null;
function h$u_towupper(ch) {
    if(h$toUpper == null) { h$toUpper = h$decodeMapping(h$toUpperMapping, h$caseMapping); }
    return ch+(h$toUpper[ch]|0);
}

var h$toLower = null;
function h$u_towlower(ch) {
    if(h$toLower == null) { h$toLower = h$decodeMapping(h$toLowerMapping, h$caseMapping); }
    return ch+(h$toLower[ch]|0);
}

var h$toTitle = null;
function h$u_towtitle(ch) {
    if(h$toTitle == null) { h$toTitle = h$decodeMapping(h$toTitleMapping, h$caseMapping); }
    return ch+(h$toTitle[ch]|0);
}

var h$alpha = null;
function h$u_iswalpha(a) {
    if(h$alpha == null) { h$alpha = h$decodeRLE(h$alphaRanges); }
    return h$alpha[a]|0;
}

var h$alnum = null;
function h$u_iswalnum(a) {
  if(h$alnum == null) { h$alnum = h$decodeRLE(h$alnumRanges); }
  return h$alnum[a] == 1 ? 1 : 0;
}

// var h$spaceChars = [9,10,11,12,13,32,160,5760,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288];
function h$isSpace(a) {
    if(a<5760) return a===32||(a>=9&&a<=13)||a===160;
    return (a>=8192&&a<=8202)||a===5760||a===8239||a===8287||a===12288;
}

function h$u_iswspace(a) {
    return h$isSpace(a)?1:0;
}

var h$lower = null;
function h$u_iswlower(a) {
    if(h$lower == null) { h$lower = h$decodeRLE(h$lowerRanges); }
    if(a < 0x30000) return h$lower[a]|0;
    if(a < 0xE0000) return 0;
    return h$lower[a-0xB0000]|0;
}

var h$upper = null;
function h$u_iswupper(a) {
    if(h$upper == null) { h$upper = h$decodeRLE(h$upperRanges); }
    if(a < 0x30000) return h$upper[a]|0;
    if(a < 0xE0000) return 0;
    return h$upper[a-0xB0000]|0;
}


var h$cntrlChars = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159];
var h$cntrl = null;
function h$u_iswcntrl(a) {
    if(h$cntrl === null) {
        h$cntrl = [];
        for(var i=0;i<=159;i++) h$cntrl[i] = (h$cntrlChars.indexOf(i) !== -1) ? 1 : 0;
    }
    return a <= 159 ? h$cntrl[a] : 0;
}

var h$print = null;
function h$u_iswprint(a) {
    if(h$print == null) { h$print = h$decodeRLE(h$printRanges); }
    if(a < 0x30000) return h$print[a]|0;
    if(a < 0xE0000) return 0;
    return h$print[a-0xB0000]|0;
}

// decode a packed string (Compactor encoding method) to an array of numbers
function h$decodePacked(s) {
    function f(o) {
        var c = s.charCodeAt(o);
        return c<34?c-32:c<92?c-33:c-34;
    }
    var r=[], i=0;
    while(i < s.length) {
        var c = s.charCodeAt(i);
        if(c < 124) r.push(f(i++));
        else if(c === 124) {
            i += 3; r.push(90+90*f(i-2)+f(i-1));
        } else if(c === 125) {
            i += 4;
            r.push(8190+8100*f(i-3)+90*f(i-2)+f(i-1));
        } else throw ("h$decodePacked: invalid: " + c);
    }
    return r;
}

// decode string with encoded character ranges
function h$decodeRLE(str) {
    var r = [], x = 0, i = 0, j = 0, v, k, a = h$decodePacked(str);
    while(i < a.length) {
        v = a[i++];
        if(v === 0) { // alternating
            k = a[i++];
            while(k--) {
                r[j++] = x;
                r[j++] = 1-x;
            }
        } else {
            if(v <= 2) {
                k = (a[i]<<16)+a[i+1];
                i+=2;
            } else k = (v-1)>>1;
            if(v%2) {
                r[j++] = x;
                x = 1-x;
            }
            while(k--) r[j++] = x;
            x = 1-x;
        }
    }
    r.shift();
    return r;
}

function h$decodeMapping(str, f) {
    var r = [], i = 0, j = 0, k, v, v2, a = h$decodePacked(str);
    while(i < a.length) {
        v = a[i++];
        if(v === 0) { // alternating
            k  = a[i];
            v  = f(a[i+1]);
            v2 = f(a[i+2]);
            while(k--) {
                r[j++] = v;
                r[j++] = v2;
            }
            i+=3;
        } else {
            if(v === 2) {
                k = (a[i] << 16) + a[i+1];
                v = a[i+2];
                i += 3;
            } else if(v%2) {
                k = 1;
                v = v>>1;
            } else {
                k = (v>>1)-1;
                v = a[i++];
            }
            v = f(v);
            while(k--) r[j++] = v;
        }
    }
    return r;
}

var h$unicodeCat = null;
function h$u_gencat(a) {
    if(h$unicodeCat == null) h$unicodeCat = h$decodeMapping(h$catMapping, function(x) { return x; });
    // private use
    if(a >= 0xE000 && a <= 0xF8FF || a >= 0xF0000 & a <= 0xFFFFD || a >= 0x100000 && a <= 0x10FFFD) return 28;
    var c = a < 0x30000 ? (h$unicodeCat[a]|0) :
        (a < 0xE0000 ? 0 : (h$unicodeCat[a-0xB0000]|0));
    return c?c-1:29;
}

function h$localeEncoding() {
   // h$log("### localeEncoding");
   RETURN_UBX_TUP2(h$encodeUtf8("UTF-8"), 0); // offset 0
}

function h$wcwidth(wch) {
  return 1; // XXX: add real implementation
}

function h$rawStringData(str) {
    var v = h$newByteArray(str.length+1);
    var u8 = v.u8;
    for(var i=0;i<str.length;i++) {
       u8[i] = str[i];
    }
    u8[str.length] = 0;
    return v;
}

// encode a javascript string to a zero terminated utf8 byte array
function h$encodeUtf8(str) {
  return h$encodeUtf8Internal(str, false, false);
}

// encode a string constant
function h$encodeModifiedUtf8(str) {
  return h$encodeUtf8Internal(str, true, false);
}

// encode a packed string
// since \0 is used to separate strings (and a common occurrence)
// we add the following mapping:
//   - \0  -> \cz\0
//   - \cz -> \cz\cz
//
// decoding to bytes, the following is produced:
//   - \cz\0  -> C0 80
//   - \cz\cz -> 1A
//
// additionally, for dealing with raw binary data we have an escape sequence
// to pack base64 encoded runs:
//
//   - \cz\xNN -> followed by NN-0x1f (31 decimal) bytes of base64 encoded
//                data. supported range: 0x20 .. 0x9f (1-128 bytes data)
//
function h$encodePackedUtf8(str) {
  return h$encodeUtf8Internal(str, false, true);
}

// modified: encode \0     -> 192 128
// packed:   encode \cz\cz -> 26
//                  \cz\0  -> 192 128
function h$encodeUtf8Internal(str, modified, packed) {
  var i, j, c, low, b64bytes, b64chars;
  function base64val(cc) {
    if(cc >= 65 && cc <= 90) return cc - 65; // A-Z
    if(cc >= 97 && cc <= 122) return cc - 71; // a-z
    if(cc >= 48 && cc <= 57) return cc + 4; // 0-9
    if(cc === 43) return 62; // +
    if(cc === 47) return 63; // /
    if(cc === 61) return 0; // = (treat padding as zero)
    throw new Error("invalid base64 value: " + cc);
  }
  var n = 0;
  var czescape = false;
  for(i=0;i<str.length;i++) {
    // non-BMP encoded as surrogate pair in JavaScript string, get actual codepoint
    var c = str.charCodeAt(i);
    if (0xD800 <= c && c <= 0xDBFF) {
      low = str.charCodeAt(i+1);
      c = ((c - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
      i++;
    }
    if(czescape) {
      if(c === 26) { // \cz\cz -> 26
        n+=1;
      } else if(c === 0) { // \cz\0 -> 192 128
        n+=2
      } else if(c >= 0x20 && c <= 0x9f) {
        b64bytes = c - 0x1f; // number of bytes in base64 encoded run
        b64chars = ((b64bytes + 2) / 3) << 2;
        n += b64bytes;
        i += b64chars;
      } else {
        throw new Error("invalid cz escaped character: " + c);
      }
      czescape = false;
    } else {
      if(c === 26 && packed) {
        czescape = true;
      } else if(c === 0 && modified) {
        n+=2;
      } else if(c <= 0x7F) {
        n++;
      } else if(c <= 0x7FF) {
        n+=2;
      } else if(c <= 0xFFFF) {
        n+=3;
      } else if(c <= 0x1FFFFF) {
        n+=4;
      } else if(c <= 0x3FFFFFF) {
        n+=5;
      } else {
        n+=6;
      }
    }
  }
  var v = h$newByteArray(n+1);
  var u8 = v.u8;
  n = 0;
  for(i=0;i<str.length;i++) {
    c = str.charCodeAt(i);
    // non-BMP encoded as surrogate pair in JavaScript string, get actual codepoint
    if (0xD800 <= c && c <= 0xDBFF) {
      low = str.charCodeAt(i+1);
      c = ((c - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
      i++;
    }
//    h$log("### encoding char " + c + " to UTF-8: " + String.fromCodePoint(c));
    if(packed && !czescape && c === 26) {
      czescape = true;
    } else if(c === 0 && (modified || czescape)) {
      u8[n]   = 192;
      u8[n+1] = 128;
      n+=2;
      czescape = false;
    } else if(czescape) {
      if(c >= 0x20 && c <= 0x9f) {
        b64bytes = c - 0x1f;
        while(b64bytes > 0) {
          var c1 = base64val(str.charCodeAt(i+1)),
              c2 = base64val(str.charCodeAt(i+2)),
              c3 = base64val(str.charCodeAt(i+3)),
              c4 = base64val(str.charCodeAt(i+4));
          i+=4;
          u8[n] = (c1<<2)|(c2>>4);
          n++;
          if(b64bytes >= 2) {
            u8[n] = ((c2&0xf)<<4)|(c3 >> 2);
            n++;
          }
          if(b64bytes >= 3) {
            u8[n] = ((c3&0x3)<<6)|c4;
            n++;
          }
          b64bytes -= 3;
        }
      } else {
        u8[n] = c;
        n++;
      }
      czescape = false;
    } else if(c <= 0x7F) {
      u8[n] = c;
      n++;
    } else if(c <= 0x7FF) {
      u8[n] = (c >> 6) | 0xC0;
      u8[n+1] = (c & 0x3F) | 0x80;
      n+=2;
    } else if(c <= 0xFFFF) {
      u8[n]   = (c >> 12) | 0xE0;
      u8[n+1] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+2] = (c & 0x3F) | 0x80;
      n+=3;
    } else if(c <= 0x1FFFFF) {
      u8[n]   = (c >> 18) | 0xF0;
      u8[n+1] = ((c >> 12) & 0x3F) | 0x80;
      u8[n+2] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+3] = (c & 0x3F) | 0x80;
      n+=4;
    } else if(c <= 0x3FFFFFF) {
      u8[n]   = (c >> 24) | 0xF8;
      u8[n+1] = ((c >> 18) & 0x3F) | 0x80;
      u8[n+2] = ((c >> 12) & 0x3F) | 0x80;
      u8[n+3] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+4] = (c & 0x3F) | 0x80;
      n+=5;
    } else {
      u8[n]   = (c >>> 30) | 0xFC;
      u8[n+1] = ((c >> 24) & 0x3F) | 0x80;
      u8[n+2] = ((c >> 18) & 0x3F) | 0x80;
      u8[n+3] = ((c >> 12) & 0x3F) | 0x80;
      u8[n+4] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+5] = (c & 0x3F) | 0x80;
      n+=6;
    }
  }
  u8[v.len-1] = 0; // terminator
//  h$log("### encodeUtf8: " + str);
//  h$log(v);
  return v;
}


// encode a javascript string to a zero terminated utf16 byte array
function h$encodeUtf16(str) {
  var n = 0;
  var i;
  for(i=0;i<str.length;i++) {
    var c = str.charCodeAt(i);
    if(c <= 0xFFFF) {
      n += 2;
    } else {
      n += 4;
    }
  }
  var v = h$newByteArray(n+1);
  var dv = v.dv;
  n = 0;
  for(i=0;i<str.length;i++) {
    var c = str.charCodeAt(i);
    if(c <= 0xFFFF) {
      dv.setUint16(n, c, true);
      n+=2;
    } else {
      var c0 = c - 0x10000;
      dv.setUint16(n,   c0 >> 10, true);
      dv.setUint16(n+2, c0 & 0x3FF, true);
      n+=4;
    }
  }
  dv.setUint8(v.len-1,0);  // terminator
  return v;
}


/*
function h$encodeUtf16(str) {
  var b = new DataView(new ArrayBuffer(str.length * 2));
  for(var i=str.length-1;i>=0;i--) {
    b.setUint16(i<<1, str.charCodeAt(i));
  }
  h$ret1 = 0;
  return b;
}
var h$eU16 = h$encodeUtf16;

function h$decodeUtf16(v,start) {
  return h$decodeUtf16(v, v.byteLength - start, start);
}

function h$decodeUtf16z(v,start) {
  var len = v.byteLength - start;
  for(var i=start;i<l;i+=2) {
    if(v.getUint16(i) === 0) {
      len = i;
      break;
    }
  }
  return h$decodeUtf16l(v,l,start)
}
*/

function h$decodeUtf16l(v, byteLen, start) {
  // perhaps we can apply it with an Uint16Array view, but that might give us endianness problems
  var arr = [];
  for(var i=0;i<byteLen;i+=2) {
    arr[i>>1] = v.dv.getUint16(i+start,true);
  }
  return h$charCodeArrayToString(arr);
}
var h$dU16 = h$decodeUtf16;

// decode a buffer with UTF-8 chars to a JS string
// stop at the first zero
function h$decodeUtf8z(v,start) {
  var n = start;
  var max = v.len;
  while(n < max) {
    if(v.u8[n] === 0) { break; }
    n++;
  }
  return h$decodeUtf8(v,n,start);
}

// decode a buffer with Utf8 chars to a JS string
// invalid characters are ignored
function h$decodeUtf8(v,n0,start) {
//  h$log("### decodeUtf8");
//  h$log(v);
  var n = n0 || v.len;
  var arr = [];
  var i = start || 0;
  var code;
  var u8 = v.u8;
//  h$log("### decoding, starting at:  " + i);
  while(i < n) {
    var c = u8[i];
    while((c & 0xC0) === 0x80) {
      c = u8[++i];
    }
//    h$log("### lead char: " + c);
    if((c & 0x80) === 0) {
      code = (c & 0x7F);
      i++;
    } else if((c & 0xE0) === 0xC0) {
      code = ( ((c & 0x1F) << 6)
             | (u8[i+1] & 0x3F)
             );
      i+=2;
    } else if((c & 0xF0) === 0xE0) {
      code = ( ((c & 0x0F) << 12)
             | ((u8[i+1] & 0x3F) << 6)
             | (u8[i+2] & 0x3F)
             );
      i+=3;
    } else if ((c & 0xF8) === 0xF0) {
      code = ( ((c & 0x07) << 18)
             | ((u8[i+1] & 0x3F) << 12)
             | ((u8[i+2] & 0x3F) <<  6)
             | (u8[i+3] & 0x3F)
             );
      i+=4;
    } else if((c & 0xFC) === 0xF8) {
      code = ( ((c & 0x03) << 24)
             | ((u8[i+1] & 0x3F) << 18)
             | ((u8[i+2] & 0x3F) << 12)
             | ((u8[i+3] & 0x3F) <<  6)
             | (u8[i+4] & 0x3F)
             );
      i+=5;
    } else {
      code = ( ((c & 0x01) << 30)
             | ((u8[i+1] & 0x3F) << 24)
             | ((u8[i+2] & 0x3F) << 18)
             | ((u8[i+3] & 0x3F) << 12)
             | ((u8[i+4] & 0x3F) <<  6)
             | (u8[i+5] & 0x3F)
             );
      i+=6;
    }
    // h$log("### decoded codePoint: " + code + " - " + String.fromCharCode(code)); // String.fromCodePoint(code));
    // need to deal with surrogate pairs
    if(code > 0xFFFF) {
      var offset = code - 0x10000;
      arr.push(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF));
    } else {
      arr.push(code);
    }
  }
  return h$charCodeArrayToString(arr);
}

// fixme what if terminator, then we read past end
function h$decodeUtf16(v) {
  var n = v.len;
  var arr = [];
  var dv = v.dv;
  for(var i=0;i<n;i+=2) {
    arr.push(dv.getUint16(i,true));
  }
  return h$charCodeArrayToString(arr);
}

function h$charCodeArrayToString(arr) {
    if(arr.length <= 60000) {
	return String.fromCharCode.apply(this, arr);
    }
    var r = '';
    for(var i=0;i<arr.length;i+=60000) {
	r += String.fromCharCode.apply(this, arr.slice(i, i+60000));
    }
    return r;
}

function h$hs_iconv_open(to,to_off,from,from_off) {
#ifndef GHCJS_BROWSER
  h$setErrno("EINVAL"); // no encodings supported
#endif
  return -1;
//  var fromStr = decodeUtf8(from, from_off);
//  var toStr = decodeUtf8(to, to_off);
//  h$log("#### hs_iconv_open: " + fromStr + " -> " + toStr);
//  return 1; // fixme?
}

function h$hs_iconv_close(iconv) {
  return 0;
}

// convert JavaScript String to a Haskell String
#ifdef GHCJS_PROF
function h$toHsString(str, cc) {
#else
function h$toHsString(str) {
#endif
  if(typeof str !== 'string') return HS_NIL;
  var i = str.length - 1;
  var r = HS_NIL;
  while(i>=0) {
    // Used at h$appendToHsString as well
    var cp = str.charCodeAt(i);
    if(cp >= 0xDC00 && cp <= 0xDFFF && i > 0) {
      --i;
      cp = (cp - 0xDC00) + (str.charCodeAt(i) - 0xD800) * 1024 + 0x10000;
    }
    r = MK_CONS_CC(cp, r, cc);
    --i;
  }
  return r;
}

// string must have been completely forced first
function h$fromHsString(str) {
    var xs = '';
    while(IS_CONS(str)) {
	var h = CONS_HEAD(str);
	xs += String.fromCodePoint(UNWRAP_NUMBER(h));
        str = CONS_TAIL(str);
    }
    return xs;
}

// list of JSVal to array, list must have been completely forced first
function h$fromHsListJSVal(xs) {
    var arr = [];
    while(IS_CONS(xs)) {
        arr.push(JSVAL_VAL(CONS_HEAD(xs)));
        xs = CONS_TAIL(xs);
    }
    return arr;
}

// ascii only version of the above
#ifdef GHCJS_PROF
function h$toHsStringA(str, cc) {
#else
function h$toHsStringA(str) {
#endif
    if(typeof str !== 'string') return HS_NIL;
    var i = str.length - 1;
    var r = HS_NIL;
    while(i>=0) {
	r = MK_CONS_CC(str.charCodeAt(i), r, cc);
	--i;
    }
    return r;
}

// unpack utf8 string, append to existing Haskell string
#ifdef GHCJS_PROF
function h$appendToHsString(str, appendTo, cc) {
#else
function h$appendToHsString(str, appendTo) {
#endif
  var i = str.length - 1;
  // we need to make an updatable thunk here
  // if we embed the given closure in a CONS cell.
  // (#24495)
  var r = i == 0 ? appendTo : MK_UPD_THUNK(appendTo);
  while(i>=0) {
    // Copied from h$toHsString
    var cp = str.charCodeAt(i);
    if(cp >= 0xDC00 && cp <= 0xDFFF && i > 0) {
      --i;
      cp = (cp - 0xDC00) + (str.charCodeAt(i) - 0xD800) * 1024 + 0x10000;
    }
    r = MK_CONS_CC(cp, r, cc);
    --i;
  }
  return r;
}

// convert array with modified UTF-8 encoded text
#ifdef GHCJS_PROF
function h$toHsStringMU8(arr, cc) {
#else
function h$toHsStringMU8(arr) {
#endif
    var i = arr.length - 1, accept = false, b, n = 0, cp = 0, r = HS_NIL;
    while(i >= 0) {
        b = arr[i];
        if(!(b & 128)) {
            cp = b;
            accept = true;
        } else if((b & 192) === 128) {
            cp += (b & 32) * Math.pow(64, n)
        } else {
            cp += (b&((1<<(6-n))-1)) * Math.pow(64, n);
            accept = true;
        }
        if(accept) {
            r  = MK_CONS_CC(cp, r, cc);
            cp = 0
            n  = 0;
        } else {
            n++;
        }
        accept = false;
        i--;
    }
    return r;
}

#ifdef GHCJS_PROF
function h$toHsList(arr, cc) {
#else
function h$toHsList(arr) {
#endif
  var r = HS_NIL;
  for(var i=arr.length-1;i>=0;i--) {
    r = MK_CONS_CC(arr[i], r, cc);
  }
  return r;
}

// array of JS values to Haskell list of JSVal
#ifdef GHCJS_PROF
function h$toHsListJSVal(arr, cc) {
#else
function h$toHsListJSVal(arr) {
#endif
    var r = HS_NIL;
    for(var i=arr.length-1;i>=0;i--) {
	r = MK_CONS_CC(MK_JSVAL(arr[i]), r, cc);
    }
    return r;
}

// unpack ascii string, append to existing Haskell string
#ifdef GHCJS_PROF
function h$appendToHsStringA(str, appendTo, cc) {
#else
function h$appendToHsStringA(str, appendTo) {
#endif
  var i = str.length - 1;
  // we need to make an updatable thunk here
  // if we embed the given closure in a CONS cell.
  // (#24495)
  var r = i == 0 ? appendTo : MK_UPD_THUNK(appendTo);
  while(i>=0) {
    r = MK_CONS_CC(str.charCodeAt(i), r, cc);
    --i;
  }
  return r;
}

// throw e wrapped in a GHCJS.Prim.JSException  in the current thread
function h$throwJSException(e) {
  // create a JSException object and  wrap it in a SomeException
  // adding the Exception dictionary
  var strVal;
  if(typeof e === 'string') {
    strVal = e;
  } else if(e instanceof Error) {
    strVal = e.toString() + '\n' + e.stack;
  } else {
    strVal = "" + e;
  }
  var someE = MK_SOMEEXCEPTION(HS_JSEXCEPTION_EXCEPTION,
                               MK_JSEXCEPTION(MK_JSVAL(e), h$toHsString(strVal))
                              );
  return h$throw(someE, true);
}
