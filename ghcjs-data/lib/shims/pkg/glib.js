
function h$g_object_ref(p, p_2) {
  h$ret1 = p_2;
  return p;
};
function h$g_free(p, p_2) {
};
function h$gtk2hs_g_object_unref_from_mainloop(o, o_2) {
};
function h$gtk2hs_closure_new(f, f_2) {
  h$ret1 = f_2;
  return f;
};
var h$g_known_types = [];
function h$g_get_type(c) {
  var n;
  for(n=0; n != h$g_known_types.length; n++) {
    if(h$g_known_types[n] == c)
      return n;
  }
  h$g_known_types[n] = c;
  return n;
};
function h$g_type_check_instance_is_a(o, x, t) {
  return o instanceof h$g_known_types[t] ? 1 : 0;
};
function h$g_idle_add_full(priority, f, f_2, data, data_2, notify, notify_2) {
  setTimeout(function() {
    h$run(h$c2(h$ap1_e, f.arr[0], h$mkPtr(data, data_2)));
  }, 0);
  return 1;
};
function h$gdk_threads_enter() {
};
function h$gdk_threads_leave() {
};

