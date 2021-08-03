function h$get_current_timezone_seconds(t, pdst_v, pdst_o, pname_v, pname_o) {
    var d      = new Date(t * 1000);
    var now    = new Date();
    var jan    = new Date(now.getFullYear(),0,1);
    var jul    = new Date(now.getFullYear(),6,1);
    var stdOff = Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
    var isDst  = d.getTimezoneOffset() < stdOff;
    var tzo    = d.getTimezoneOffset();
    pdst_v.dv.setInt32(pdst_o, isDst ? 1 : 0, true);
    if(!pname_v.arr) pname_v.arr = [];
    var offstr = tzo < 0 ? ('+' + (tzo/-60)) : ('' + (tzo/-60));
    pname_v.arr[pname_o] = [h$encodeUtf8("UTC" + offstr), 0];
    return (-60*tzo)|0;
}

function h$clock_gettime(when, p_d, p_o) {
  /* XXX: guess if we have to write 64 bit values:

            alloca is often used and will give us 16 bytes
            if timespec contains two 64 bit values

          but we really should fix this by not having hsc2hs values
          from the build system leak here
   */
  var is64 = p_d.i3.length == 4 && p_o == 0;
  var o  = p_o >> 2,
      t  = Date.now ? Date.now() : new Date().getTime(),
      tf = Math.floor(t / 1000),
      tn = 1000000 * (t - (1000 * tf));
  if(is64) {
    p_d.i3[o]   = tf|0;
    p_d.i3[o+1] = 0;
    p_d.i3[o+2] = tn|0;
    p_d.i3[o+3] = 0;
  } else {
    p_d.i3[o]   = tf|0;
    p_d.i3[o+1] = tn|0;
  }
  return 0;
}

function h$clock_getres(when, p_d, p_o) {
  return 0; // XXX: not implemented
}
