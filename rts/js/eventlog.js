// default eventlog writer: does nothing
var h$event_log_writer = (a,o) => {return;}

// redirect the eventlog to stderr
function h$eventlogToStderr() {
  h$event_log_writer = (a,o) => h$errorMsg(h$decodeUtf8z(a,o));
}

function h$traceEvent(ev_v,ev_o) {
  h$event_log_writer(ev_v,ev_o);
}

function h$traceMarker(ev_v,ev_o) {
  h$event_log_writer(ev_v,ev_o);
}

function h$flushEventlog(cap_a,cap_o) {
}
