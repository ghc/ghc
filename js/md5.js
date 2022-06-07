
function h$MD5Init(ctx, ctx_off) {
  if(!ctx.arr) { ctx.arr = []; }
  ctx.arr[ctx_off] = new goog.crypt.Md5();
}
var h$__hsbase_MD5Init = h$MD5Init;

function h$MD5Update(ctx, ctx_off, data, data_off, len) {
  var arr = new Uint8Array(data.buf, data_off);
  ctx.arr[ctx_off].update(arr, len);
}
var h$__hsbase_MD5Update = h$MD5Update;

function h$MD5Final(dst, dst_off, ctx, ctx_off) {
  var digest = ctx.arr[ctx_off].digest();
  for(var i=0;i<16;i++) {
    dst.u8[dst_off+i] = digest[i];
  }
}
var h$__hsbase_MD5Final = h$MD5Final;
