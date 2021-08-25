function h$hsprimitive_memcpy(dst_d, dst_o, doff, src_d, src_o, soff, len) {
  return h$primitive_memmove(dst_d, dst_o, doff, src_d, src_o, soff, len);
}

function h$hsprimitive_memmove(dst_d, dst_o, doff, src_d, src_o, soff, len) {
  if(len === 0) return;
  var du8 = dst_d.u8, su8 = src_d.u8;
  for(var i=len-1;i>=0;i--) {
    du8[dst_o+doff+i] = su8[src_o+soff+i];
  }
}

#define MEMSETADDR(TYPE, SIZE, PROP) \
function h$hsprimitive_memset_ ## TYPE (p_d, p_o, off, n, x) { \
  var start = (p_o >> SIZE) + off;                          \
  if(n > 0) { \
    if(p_d.PROP.fill) p_d.PROP.fill(x, start, start + n); \
    else for(var i=start; i<start+n; i++) p_d.PROP[i] = x; \
  } \
}

#define MEMSETBA(TYPE, PROP) \
function h$hsprimitive_memsetba_ ## TYPE (p_d, off, n, x) { \
  if(n > 0) { \
    if(p_d.PROP.fill) p_d.PROP.fill(x, off, off + n); \
    else for(var i=off; i<off+n; i++) p_d.PROP[i] = x; \
  } \
}

MEMSETBA(Word8,  u8)
MEMSETBA(Word16, u1)
MEMSETBA(Word32, i3)
MEMSETBA(Word,   i3)
MEMSETBA(Float,  f3)
MEMSETBA(Double, f6)
MEMSETBA(Char,   i3)

MEMSETADDR(Word8,  0, u8)
MEMSETADDR(Word16, 1, u1)
MEMSETADDR(Word32, 2, i3)
MEMSETADDR(Word,   2, i3)
MEMSETADDR(Float,  2, f3)
MEMSETADDR(Double, 3, f6)
MEMSETADDR(Char,   2, i3)

function h$hsprimitive_memsetba_Word64(p_d, off, n, x_1, x_2) {
  h$hsprimitive_memset_Word64(p_d, 0, off, n, x_1, x_2);
}

function h$hsprimitive_memset_Word64(p_d, p_o, off, n, x_1, x_2) {
  var start = (p_o >> 3) + off;
  if(n > 0) {
    var pi3 = p_d.i3;
    for(var i = 0; i < n; i++) {
      var o = (start + i) << 1;
      pi3[o]   = x_1;
      pi3[o+1] = x_2;
    }
  }
}

function h$hsprimitive_memset_Ptr(p_d, p_o, off, n, x_1, x_2) {
  if(n > 0) {
    if(!p_d.arr) p_d.arr = [];
    var a = p_d.arr;
    for(var i = 0; i < n; i++) {
      a[p_o + ((off + i) << 2)] = [x_1, x_2];
    }
  }
}
