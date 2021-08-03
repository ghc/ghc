#include "HsBaseConfig.h"
#include <ghcjs/rts.h>

// #define GHCJS_TRACE_IO 1

#ifdef GHCJS_TRACE_IO
function h$logIO() { h$log.apply(h$log, arguments); }
#define TRACE_IO(args...) h$logIO(args)
#else
#define TRACE_IO(args...)
#endif

function h$base_access(file, file_off, mode, c) {
    TRACE_IO("base_access");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(fd, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                c(mode & fs.mode); // fixme is this ok?
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$base_chmod(file, file_off, mode, c) {
    TRACE_IO("base_chmod");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.chmod(h$decodeUtf8z(file, file_off), mode, function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$base_close(fd, c) {
    TRACE_IO("base_close fd: " + fd);
    var fdo = h$base_fds[fd];
    if(fdo) {
        delete h$base_fds[fd];
        if(--fdo.refs < 1) {
          TRACE_IO("base_close: closing underlying fd");
          if(fdo.close) {
            fdo.close(fd, fdo, c);
          } else {
            c(0);
          }
        } else {
          TRACE_IO("base_close: remaining references, not closing underlying fd");
          c(0);
        }
    } else {
        TRACE_IO("base_close: file descriptor not found, already closed?");
        h$errno = CONST_EINVAL;
        c(-1);
    }
}

function h$base_dup(fd, c) {
    // h$log("h$base_dup al: " + arguments.length);
    h$base_dup2(fd, h$base_fdN--, c);
}

function h$base_dup2(fd, new_fd, c) {
   TRACE_IO("base_dup2 " + fd + " " + new_fd);
   // if(new_fd >= 0 && new_fd <= 2) {

   // }
   // h$log("h$base_dup2 al: " + arguments.length);
   // if(fd >= 0 && fd < 2) {
  //   h$errno = CONST_EINVAL;
  //   c(-1);
     // fixme make sure it can't be called again!
  //   return;
   // } // && new_fd )

    /* Fixme:
         The two descriptors do not share file descriptor flags
        (the close-on-exec flag).  The close-on-exec flag
        (FD_CLOEXEC; see fcntl(2)) for the duplicate descriptor is off.
     */
    var fdo = h$base_fds[fd];
    if(!fdo) {
      TRACE_IO("file descriptor not found");
      h$errno = CONST_EINVAL;
      c(-1);
    } else {
      var new_fdo = h$base_fds[new_fd];
      function f() {
        h$base_fds[new_fd] = fdo;
        fdo.refs++;
        c(new_fd);
      }
      if(new_fdo) {
        TRACE_IO("closing existing fd");
        h$base_close(new_fd, f);
      } else {
        f();
      }  // h$new_fdo.close();
    }
}

function h$base_fstat(fd, stat, stat_off, c) {
    TRACE_IO("base_stat");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.fstat(fd, function(err, fs) {
            if(err) {
                h$handlErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$base_isatty(fd) {
    TRACE_IO("base_isatty " + fd);
    //  return 1; // fixme debug
    var fdo = h$base_fds[fd];
    if(fdo && typeof fdo.isatty !== 'undefined') {
      if(typeof fdo.isatty === 'function') return fdo.isatty() ? 1 : 0;
      return fdo.isatty ? 1 : 0;
    }
    return 0;
}

function h$base_lseek(fd, pos_1, pos_2, whence, c) {
    TRACE_IO("base_lseek");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var p = goog.math.Long.fromBits(pos_2, pos_1), p1;
        var o = h$base_fds[fd];
        if(!o) {
            h$errno = CONST_BADF;
            c(-1,-1);
        } else {
            switch(whence) {
            case 0: /* SET */
                o.pos = p.toNumber();
                c(p.getHighBits(), p.getLowBits());
                break;
            case 1: /* CUR */
                o.pos += p.toNumber();
                p1 = goog.math.Long.fromNumber(o.pos);
                c(p1.getHighBits(), p1.getLowBits());
                break;
            case 2: /* END */
                h$fs.fstat(fd, function(err, fs) {
                    if(err) {
                        h$setErrno(err);
                        c(-1,-1);
                    } else {
                        o.pos = fs.size + p.toNumber();
                        p1 = goog.math.Long.fromNumber(o.pos);
                        c(p1.getHighBits(), p1.getLowBits());
                    }
                });
                break;
            default:
                h$errno = CONST_EINVAL;
                c(-1,-1);
            }
        }
    } else {
#endif
        h$unsupported();
        c(-1, -1);
#ifndef GHCJS_BROWSER
    }
#endif
}

function h$base_lstat(file, file_off, stat, stat_off, c) {
    TRACE_IO("base_lstat");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.lstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}
function h$base_open(file, file_off, how, mode, c) {
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var flags, off;
        var fp   = h$decodeUtf8z(file, file_off);
        TRACE_IO("base_open: " + fp);
        var acc  = how & h$base_o_accmode;
        // passing a number lets node.js use it directly as the flags (undocumented)
        if(acc === h$base_o_rdonly) {
            flags = h$processConstants['fs']['O_RDONLY'];
        } else if(acc === h$base_o_wronly) {
            flags = h$processConstants['fs']['O_WRONLY'];
        } else { // r+w
            flags = h$processConstants['fs']['O_RDWR'];
        }
        off = (how & h$base_o_append) ? -1 : 0;
        flags = flags | ((how & h$base_o_trunc)  ? h$processConstants['fs']['O_TRUNC']  : 0)
                      | ((how & h$base_o_creat)  ? h$processConstants['fs']['O_CREAT']  : 0)
                      | ((how & h$base_o_excl)   ? h$processConstants['fs']['O_EXCL']   : 0)
                      | ((how & h$base_o_append) ? h$processConstants['fs']['O_APPEND'] : 0);
        h$fs.open(fp, flags, mode, function(err, fd) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var f = function(p) {
                    h$base_fds[fd] = { read:  h$base_readFile
                                     , write: h$base_writeFile
                                     , close: h$base_closeFile
                                     , fd:    fd
                                     , pos:   p
                                     , refs:  1
                                     };
                    TRACE_IO("base_open: " + fp + " -> " + fd);
                    c(fd);
                }
                if(off === -1) {
                    h$fs.stat(fp, function(err, fs) {
                        if(err) h$handleErrnoC(err, -1, 0, c); else f(fs.size);
                    });
                } else {
                    f(0);
                }
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}
function h$base_read(fd, buf, buf_off, n, c) {
    TRACE_IO("base_read: " + fd);
    var fdo = h$base_fds[fd];
    if(fdo && fdo.read) {
        fdo.read(fd, fdo, buf, buf_off, n, c);
    } else {
        h$fs.read(fd, buf.u8, buf_off, n, null, function(err, bytesRead, buf0) {
            h$handleErrnoC(err, -1, bytesRead, c);
        });
    }
}
function h$base_stat(file, file_off, stat, stat_off, c) {
    TRACE_IO("base_stat");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handlErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}
function h$base_umask(mode) {
    TRACE_IO("base_umask: " + mode);
#ifndef GHCJS_BROWSER
    if(h$isNode) return process.umask(mode);
#endif
    return 0;
}

function h$base_write(fd, buf, buf_off, n, c) {
    TRACE_IO("base_write: " + fd);
    var fdo = h$base_fds[fd];
    if(fdo && fdo.write) {
        fdo.write(fd, fdo, buf, buf_off, n, c);
    } else {
        h$fs.write(fd, buf.u8, buf_off, n, function(err, bytesWritten, buf0) {
            h$handleErrnoC(err, -1, bytesWritten, c);
        });
    }
}

function h$base_ftruncate(fd, pos_1, pos_2, c) {
    TRACE_IO("base_ftruncate");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.ftruncate(fd, goog.math.Long.fromBits(pos_2, pos_1).toNumber(), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
#endif
        h$unsupported(-1, c);
}
function h$base_unlink(file, file_off, c) {
    TRACE_IO("base_unlink");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.unlink(h$decodeUtf8z(file, file_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
#endif
        h$unsupported(-1, c);
}
function h$base_getpid() {
    TRACE_IO("base_getpid");
#ifndef GHCJS_BROWSER
    if(h$isNode) return process.pid;
#endif
    return 0;
}
function h$base_link(file1, file1_off, file2, file2_off, c) {
    TRACE_IO("base_link");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.link(h$decodeUtf8z(file1, file1_off), h$decodeUtf8z(file2, file2_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
#endif
        h$unsupported(-1, c);
}
function h$base_mkfifo(file, file_off, mode, c) {
    throw "h$base_mkfifo";
}
function h$base_sigemptyset(sigset, sigset_off) {
    return 0;
    // throw "h$base_sigemptyset";
}
function h$base_sigaddset(sigset, sigset_off, sig) {
    return 0;
    // throw "h$base_sigaddset";
}
function h$base_sigprocmask(sig, sigset1, sigset1_off, sigset2, sigset2_off) {
    return 0;
    // throw "h$base_sigprocmask";
}
function h$base_tcgetattr(attr, termios, termios_off) {
    return 0;
}
function h$base_tcsetattr(attr, val, termios, termios_off) {
    return 0;
}
function h$base_utime(file, file_off, timbuf, timbuf_off, c) {
    TRACE_IO("base_utime");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.fstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, 0, -1, c); // fixme
            } else {
                var atime = goog.math.Long.fromNumber(fs.atime.getTime());
                var mtime = goog.math.Long.fromNumber(fs.mtime.getTime());
                var ctime = goog.math.Long.fromNumber(fs.ctime.getTime());
                timbuf.i3[0] = atime.getHighBits();
                timbuf.i3[1] = atime.getLowBits();
                timbuf.i3[2] = mtime.getHighBits();
                timbuf.i3[3] = mtime.getLowBits();
                timbuf.i3[4] = ctime.getHighBits();
                timbuf.i3[5] = ctime.getLowBits();
                c(0);
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}
function h$base_waitpid(pid, stat, stat_off, options, c) {
    throw "h$base_waitpid";
}
/** @const */ var h$base_o_rdonly   = 0x00000;
/** @const */ var h$base_o_wronly   = 0x00001;
/** @const */ var h$base_o_rdwr     = 0x00002;
/** @const */ var h$base_o_accmode  = 0x00003;
/** @const */ var h$base_o_append   = 0x00008;
/** @const */ var h$base_o_creat    = 0x00200;
/** @const */ var h$base_o_trunc    = 0x00400;
/** @const */ var h$base_o_excl     = 0x00800;
/** @const */ var h$base_o_noctty   = 0x20000;
/** @const */ var h$base_o_nonblock = 0x00004;
/** @const */ var h$base_o_binary   = 0x00000;

function h$base_c_s_isreg(mode) {
    return 1;
}
function h$base_c_s_ischr(mode) {
    return 0;
}
function h$base_c_s_isblk(mode) {
    return 0;
}
function h$base_c_s_isdir(mode) {
    return 0; // fixme
}
function h$base_c_s_isfifo(mode) {
    return 0;
}

#ifndef GHCJS_BROWSER
function h$base_fillStat(fs, b, off) {
    if(off%4) throw "h$base_fillStat: not aligned";
    var o = off>>2;
    b.i3[o+0] = fs.mode;
    var s = goog.math.Long.fromNumber(fs.size);
    b.i3[o+1] = s.getHighBits();
    b.i3[o+2] = s.getLowBits();
    b.i3[o+3] = 0; // fixme
    b.i3[o+4] = 0; // fixme
    b.i3[o+5] = fs.dev;
    var i = goog.math.Long.fromNumber(fs.ino);
    b.i3[o+6] = i.getHighBits();
    b.i3[o+7] = i.getLowBits();
    b.i3[o+8] = fs.uid;
    b.i3[o+9] = fs.gid;
}
#endif

// [mode,size1,size2,mtime1,mtime2,dev,ino1,ino2,uid,gid] all 32 bit
/** @const */ var h$base_sizeof_stat = 40;

function h$base_st_mtime(stat, stat_off) {
    RETURN_UBX_TUP2(stat.i3[(stat_off>>2)+3], stat.i3[(stat_off>>2)+4]);
}

function h$base_st_size(stat, stat_off) {
    RETURN_UBX_TUP2(stat.i3[(stat_off>>2)+1], stat.i3[(stat_off>>2)+2]);
}

function h$base_st_mode(stat, stat_off) {
    return stat.i3[stat_off>>2];
}

function h$base_st_dev(stat, stat_off) {
    return stat.i3[(stat_off>>2)+5];
}

function h$base_st_ino(stat, stat_off) {
    RETURN_UBX_TUP2(stat.i3[(stat_off>>2)+6], stat.i3[(stat_off>>2)+7]);
}

/** @const */ var h$base_echo            = 1;
/** @const */ var h$base_tcsanow         = 2;
/** @const */ var h$base_icanon          = 4;
/** @const */ var h$base_vmin            = 8;
/** @const */ var h$base_vtime           = 16;
/** @const */ var h$base_sigttou         = 0;
/** @const */ var h$base_sig_block       = 0;
/** @const */ var h$base_sig_setmask     = 0;
/** @const */ var h$base_f_getfl         = 0;
/** @const */ var h$base_f_setfl         = 0;
/** @const */ var h$base_f_setfd         = 0;
/** @const */ var h$base_fd_cloexec      = 0;
/** @const */ var h$base_sizeof_termios  = 4;
/** @const */ var h$base_sizeof_sigset_t = 4;

function h$base_lflag(termios, termios_off) {
    return 0;
}

function h$base_poke_lflag(termios, termios_off, flag) {
    return 0;
}

function h$base_ptr_c_cc(termios, termios_off) {
    RETURN_UBX_TUP2(h$newByteArray(8), 0);
}

/** @const */ var h$base_default_buffer_size = 32768;

function h$base_c_s_issock(mode) {
    return 0; // fixme
}

/** @const */ var h$base_SEEK_SET = 0;
/** @const */ var h$base_SEEK_CUR = 1;
/** @const */ var h$base_SEEK_END = 2;

function h$base_set_saved_termios(a, b, c) {
    RETURN_UBX_TUP2(null, 0);
}

function h$base_get_saved_termios(r) {
    RETURN_UBX_TUP2(null, 0);
}

// fixme
function h$lockFile(fd, dev, ino, for_writing) {
    TRACE_IO("lockFile:" + fd);
    return 0;
}
function h$unlockFile(fd) {
    TRACE_IO("unlockFile:" + fd);
    return 0;
}



// engine-dependent setup
var h$base_readStdin , h$base_writeStderr, h$base_writeStdout;
var h$base_isattyStdin = false, h$base_isattyStdout = false, h$base_isattyStderr = false;
var h$base_closeStdin = null, h$base_closeStderr = null, h$base_closeStdout = null;
var h$base_readFile,  h$base_writeFile,   h$base_closeFile;
#ifndef GHCJS_BROWSER
var h$base_stdin_waiting = new h$Queue();
var h$base_stdin_chunk   = { buf: null
                           , pos: 0
                           , processing: false
                           };
var h$base_stdin_eof     = false;
var h$base_process_stdin = function() {
    var c = h$base_stdin_chunk;
    var q = h$base_stdin_waiting;
    if(!q.length() || c.processing) return;
    c.processing = true;
    if(!c.buf) { c.pos = 0; c.buf = process.stdin.read(); }
    while(c.buf && q.length()) {
        var x = q.dequeue();
        var n = Math.min(c.buf.length - c.pos, x.n);
        for(var i=0;i<n;i++) {
            x.buf.u8[i+x.off] = c.buf[c.pos+i];
        }
        c.pos += n;
        x.c(n);
        if(c.pos >= c.buf.length) c.buf = null;
        if(!c.buf && q.length()) { c.pos = 0; c.buf = process.stdin.read(); }
    }
    while(h$base_stdin_eof && q.length()) q.dequeue().c(0);
    c.processing = false;
}

if(h$isNode) {
    h$base_closeFile = function(fd, fdo, c) {
        TRACE_IO("base_closeFile: " + fd + " (" + fdo.fd + ")");
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        h$fs.close(real_fd, function(err) {
            delete h$base_fds[fd];
            h$handleErrnoC(err, -1, 0, c);
        });
    }

    h$base_readFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        TRACE_IO("base_readFile: " + fd + " (" + fdo.fd + ") " + pos + " " + buf_offset + " " + n);
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        h$fs.read(real_fd, Buffer.alloc(n), 0, n, pos, function(err, bytesRead, nbuf) {
            if(err) {
                h$setErrno(err);
                c(-1);
            } else {
                for(var i=bytesRead-1;i>=0;i--) buf.u8[buf_offset+i] = nbuf[i];
                if(typeof fdo.pos === 'number') fdo.pos += bytesRead;
                c(bytesRead);
            }
        });
    }

    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        TRACE_IO("read stdin");
        h$base_stdin_waiting.enqueue({buf: buf, off: buf_offset, n: n, c: c});
        h$base_process_stdin();
    }

    h$base_closeStdin = function(fd, fdo, c) {
        TRACE_IO("close stdin");
        // process.stdin.close(); fixme
        c(0);
    }

    h$base_writeFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        TRACE_IO("base_writeFile: " + fd + " (" + fdo.fd + ") " + pos + " " + buf_offset + " " + n);
        var nbuf = Buffer.alloc(n);
        for(var i=0;i<n;i++) nbuf[i] = buf.u8[i+buf_offset];
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        if(typeof fdo.pos === 'number') fdo.pos += n;
        h$fs.write(real_fd, nbuf, 0, n, pos, function(err, bytesWritten) {
            TRACE_IO("written file: " + fd + " (" + fdo.fd + ")");
            if(err) {
                h$setErrno(err);
                if(typeof fdo.pos === 'number') fdo.pos -= n;
                if(h$errno === CONST_EAGAIN)
                    setTimeout(function() { h$base_writeFile(fd, fdo, buf, buf_offset, n, c); }, 20);
                else c(-1);
            } else {
                if(typeof fdo.pos === 'number') fdo.pos += bytesWritten - n;
                c(bytesWritten);
            }
        });
    }

    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        TRACE_IO("write stdout");
        h$base_writeFile(1, fdo, buf, buf_offset, n, c);
    }

    h$base_closeStdout = function(fd, fdo, c) {
        TRACE_IO("close stdout");
	      // not actually closed, fixme?
        c(0);
    }

    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        TRACE_IO("write stderr");
        h$base_writeFile(2, fdo, buf, buf_offset, n, c);
    }

    h$base_closeStderr = function(fd, fdo, c) {
        TRACE_IO("close stderr");
	// not actually closed, fixme?
        c(0);
    }

    process.stdin.on('readable', h$base_process_stdin);
    process.stdin.on('end', function() { h$base_stdin_eof = true; h$base_process_stdin(); });

    h$base_isattyStdin  = function() { return process.stdin.isTTY;  };
    h$base_isattyStdout = function() { return process.stdout.isTTY; };
    h$base_isattyStderr = function() { return process.stderr.isTTY; };

} else if (h$isJsShell) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        putstr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        printErr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
} else if(h$isJsCore) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
	c(0);
    }
    var h$base_stdoutLeftover = { f: print, val: null };
    var h$base_stderrLeftover = { f: debug, val: null };
    var h$base_writeWithLeftover = function(buf, n, buf_offset, c, lo) {
	var lines = h$decodeUtf8(buf, n, buf_offset).split(/\r?\n/);
	if(lines.length === 1) {
	    if(lines[0].length) {
		if(lo.val !== null) lo.val += lines[0];
		else lo.val = lines[0];
	    }
	} else {
            lo.f(((lo.val !== null) ? lo.val : '') + lines[0]);
	    for(var i=1;i<lines.length-1;i++) lo.f(lines[i]);
	    if(lines[lines.length-1].length) lo.val = lines[lines.length-1];
	    else lo.val = null;
	}
	c(n);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
	h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stdoutLeftover);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
	// writing to stderr not supported, write to stdout
	h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stderrLeftover);
    }
} else { // browser / fallback
#endif
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
#ifndef GHCJS_BROWSER
}
#endif

var h$base_stdin_fd  =
  { read:   h$base_readStdin
  , close:  h$base_closeStdin
  , isatty: h$base_isattyStdin
  , refs:   1
  };
var h$base_stdout_fd =
  { write:  h$base_writeStdout
  , close:  h$base_closeStdout
  , isatty: h$base_isattyStdout
  , refs:   1
  };
var h$base_stderr_fd =
  { write:  h$base_writeStderr
  , close:  h$base_closeStderr
  , isatty: h$base_isattyStderr
  , refs:   1
  };

var h$base_fdN = -2; // negative file descriptors are 'virtual', -1 is already used to indicated error
var h$base_fds = [h$base_stdin_fd, h$base_stdout_fd, h$base_stderr_fd];

function h$shutdownHaskellAndExit(code, fast) {
#ifndef GHCJS_BROWSER
#ifdef GHCJS_LOG_BUFFER
    if(h$isNode) console.log(h$logBuffer);
    if(h$isJsShell || h$isJsCore) print(h$logBuffer);
#endif
#endif
    h$exitProcess(code);
}

// RAND_MAX = 32767
function h$rand() {
  return (32768 * Math.random()) & 32767;
}

// SIGUSR1, SIGTERM, SIGINT, SIGPIPE, SIGHUP, SIGTERM, SIGINT
// SIGBREAK, SIGWINCH, SIGKILL, SIGSTOP, SIGBUS, SIGFPE
// SIGSEGV, SIGILL

// returns old action code
function h$stg_sig_install(sigNo, actionCode, sigSet_d, sigSet_o) {
  // XXX dummy implementation
  return 0;
}
