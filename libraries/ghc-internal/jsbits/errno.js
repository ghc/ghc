//#OPTIONS: CPP

#include "HsBaseConfig.h"

#ifdef GHCJS_TRACE_ERRNO
function h$logErrno() { h$log.apply(h$log,arguments); }
#define TRACE_ERRNO(args...) h$logErrno(args)
#else
#define TRACE_ERRNO(args...)
#endif

var h$errno = 0;

function h$__hscore_get_errno() {
  TRACE_ERRNO("hscore_get_errno: " + h$errno);
  return h$errno;
}

function h$unsupported(status, c) {
    h$errno = 12456;
    if(c) c(status);
    return status;
}

function h$base_strerror(err) {
    if(err === 12456) {
	RETURN_UBX_TUP2(h$encodeUtf8("operation unsupported on this platform"), 0);
    }
#ifdef GHCJS_BROWSER
    RETURN_UBX_TUP2(h$encodeUtf8("unknown error"), 0);
#else
    RETURN_UBX_TUP2(h$encodeUtf8(h$errorStrs[err] || "unknown error"), 0);
#endif
}

#ifndef GHCJS_BROWSER
function h$setErrno(e) {
  TRACE_ERRNO("setErrno: " + e);
  var es = e.toString();
  var getErr = function() {
      if(es.indexOf('ENOTDIR') !== -1)      return CONST_ENOTDIR;
      if(es.indexOf('EISDIR') !== -1)       return CONST_EISDIR;
      if(es.indexOf('ENOENT') !== -1)       return CONST_ENOENT;
      if(es.indexOf('EEXIST') !== -1)       return CONST_EEXIST;
      if(es.indexOf('ENETUNREACH') !== -1)  return CONST_EINVAL; // fixme
      if(es.indexOf('EPERM') !== -1)        return CONST_EPERM;
      if(es.indexOf('EMFILE') !== -1)       return CONST_EMFILE;
      if(es.indexOf('EPIPE') !== -1)        return CONST_EPIPE;
      if(es.indexOf('EAGAIN') !== -1)       return CONST_EAGAIN;
      if(es.indexOf('EINVAL') !== -1)       return CONST_EINVAL;
      if(es.indexOf('ESPIPE') !== -1)       return CONST_ESPIPE;
      if(es.indexOf('EBADF') !== -1)        return CONST_EBADF;
      if(es.indexOf('ENOSPC') !== -1)       return CONST_ENOSPC;
      if(es.indexOf('EACCES') !== -1)       return CONST_EACCES;
      if(es.indexOf('EXDEV') !== -1)        return CONST_EXDEV;
      if(es.indexOf('Bad argument') !== -1) return CONST_ENOENT; // fixme?
      throw ("setErrno not yet implemented for: " + e);

  }
  h$errno = getErr();
}

var h$errorStrs =  { CONST_E2BIG:   "Argument list too long"
                   , CONST_EACCES:  "Permission denied"
                   , CONST_EINVAL:  "Invalid argument"
                   , CONST_EBADF:   "Bad file descriptor"
                   , CONST_ENOTDIR: "Not a directory"
                   , CONST_EISDIR:  "Illegal operation on a directory"
                   , CONST_ENOENT:  "No such file or directory"
                   , CONST_EPERM:   "Operation not permitted"
                   , CONST_EEXIST:  "File exists"
                   , CONST_EMFILE:  "Too many open files"
                   , CONST_EPIPE:   "Broken pipe"
                   , CONST_EAGAIN:  "Resource temporarily unavailable"
                   , CONST_ESPIPE:  "Illegal seek"
                   , CONST_EXDEV:   "Cross-device link" // See https://en.cppreference.com/w/cpp/error/errno_macros
                   }

function h$handleErrno(r_err, f) {
  try {
    return f();
  } catch(e) {
    h$setErrno(e);
    return r_err;
  }
}

function h$handleErrnoS(r_err, r_success, f) {
  try {
    f();
    return r_success;
  } catch(e) {
    h$setErrno(e);
    return r_err;
  }
}

function h$handleErrnoC(err, r_err, r_success, c) {
    if(err) {
        h$setErrno(err);
        c(r_err);
    } else {
        c(r_success);
    }
}
#endif
