function h$clock_gettime(when, p_d, p_o) {
  var is64 = p_d.i3.length == 4 && p_o == 0;
  var o  = p_o >> 2;
  var t  = Date.now ? Date.now() : new Date().getTime();
  var tf = Math.floor(t / 1000);
  var tn = 1000000 * (t - (1000 * tf));

  if (is64) {
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
