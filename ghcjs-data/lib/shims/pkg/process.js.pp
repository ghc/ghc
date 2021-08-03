#include "HsBaseConfig.h"

// #ifdef GHCJS_NODE
// only works on node.js

#ifdef GHCJS_TRACE_PROCESS
function h$logProcess() { h$log.apply(h$log,arguments); }
#define TRACE_PROCESS(args...) h$logProcess(args)
#else
#define TRACE_PROCESS(args...)
#endif

#ifndef GHCJS_BROWSER
// one-dir pipe
function h$process_pipeFd(pipe, write) {
    var fdN = h$base_fdN--, fd = {};
    h$base_fds[fdN] = fd;
    TRACE_PROCESS("pipe " + fdN + " opened, writable: " + write);
    if(write) {
        fd.err   = null;
        fd.waiting = new h$Queue();
        fd.close = function(fd, fdo, c) { delete h$base_fds[fd]; pipe.end(); c(0); };
        fd.refs = 1;
        pipe.on('error', function(err) {
            fd.err = err;
        });
        fd.write = function(fd, fdo, buf, buf_offset, n, c)  {
            TRACE_PROCESS("pipe " + fd + " write: " + n);
            if(fdo.err) {
                h$setErrno(fdo.err);
                c(-1);
            }
            var u8 = buf.u8;
            var nbuf = Buffer.alloc(n);
            // can this be made more efficient?
            for(var k=0;k<n;k++) nbuf[k] = u8[buf_offset+k];
            var r = pipe.write(nbuf, function() {
                TRACE_PROCESS("pipe " + fd + " flushed");
                c(n);
            });
            TRACE_PROCESS("pipe write: " + fd + " result: " + r);
        }
    } else {
        fd.close      = function(fd, fdo, c) { delete h$base_fds[fd]; c(0); }
        fd.refs       = 1;
        fd.waiting    = new h$Queue();
        fd.chunk      = { buf: null, pos: 0, processing: false };
        fd.eof        = false;
        fd.err        = null;

        // this is a workaround for some versions of node.js incorrectly flushing streams,
        // leading to data loss when processes exit quickly. see GHCJS #453
        pipe.on('data', function(buf) {
            if(fd.chunk.buf) {
                fd.chunk.buf = Buffer.concat([fd.chunk.buf, buf]);
            } else {
                fd.chunk.buf = buf;
            }
            h$process_process_pipe(fd, pipe);
        });
        pipe.pause();
        // end workaround

        pipe.on('readable', function() {
            TRACE_PROCESS("pipe " + fdN + " readable");
            h$process_process_pipe(fd, pipe);
        });
        pipe.on('end', function() {
            TRACE_PROCESS("pipe " + fdN + " eof");
            fd.eof = true;
            h$process_process_pipe(fd, pipe);
        });
        pipe.on('error', function(err) {
            fd.err = err;
            h$process_process_pipe(fd, pipe);
        });
        fd.read = function(fd, fdo, buf, buf_offset, n, c) {
            TRACE_PROCESS("pipe " + fd + " read: " + n + " " + fdo.chunk.buf);
            fdo.waiting.enqueue({buf: buf, off: buf_offset, n: n, c: c});
            h$process_process_pipe(fdo, pipe);
        }
        // fixme
        // fd.write = function(fd, fdo, buf, buf_offset, n, c) { c(0); }
    }
    TRACE_PROCESS("created pipe, fd: " + fdN);
    return fdN;
}

function h$process_process_pipe(fd, pipe) {
    var c = fd.chunk;
    var q = fd.waiting;
    TRACE_PROCESS("processing pipe, queue: " + q.length());
    if(!q.length() || c.processing) return;
    c.processing = true;
    while(fd.err && q.length()) { h$setErrno(fd.err); q.dequeue().c(-1); } // global errno is risky here
    if(!c.buf) { c.pos = 0; c.buf = pipe.read(); }
    while(c.buf && q.length()) {
        var x = q.dequeue();
        var n = Math.min(c.buf.length - c.pos, x.n);
        for(var i=0;i<n;i++) {
            x.buf.u8[i+x.off] = c.buf[c.pos+i];
        }
        c.pos += n;
        x.c(n);
        if(c.pos >= c.buf.length) c.buf = null;
        if(!c.buf && q.length()) { c.pos = 0; c.buf = pipe.read(); }
    }
    while(fd.eof && q.length()) q.dequeue().c(0);
    TRACE_PROCESS("done processing pipe, remaining queue: " + q.length());
    c.processing = false;
}
#endif /* GHCJS_BROWSER */
function h$process_runInteractiveProcess( cmd, args, workingDir, env
                                        , stdin_fd, stdout_fd, stderr_fd
                                        , closeHandles, createGroup, delegateCtlC) {
    TRACE_PROCESS("runInteractiveProcess");
    TRACE_PROCESS("cmd: " + cmd + " args: " + args.join(' '));
    TRACE_PROCESS("workingDir: " + workingDir + " env: " + env);
    TRACE_PROCESS("stdin: " + stdin_fd + " stdout: " + stdout_fd + " stderr: " + stderr_fd);

#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var stdin_p, stdout_p, stderr_p;

        if(stdin_fd === -1) {
            stdin_p = 'pipe';
        } else if(stdin_fd === 0) {
            stdin_p = process.stdin;
        } else {
            throw "runInteractiveProcess: custom stdin unsupported";
        }

        if(stdout_fd === -1) {
            stdout_p = 'pipe';
        } else if(stdout_fd === 1) {
            stdout_p = process.stdout;
        } else {
            throw "runInteractiveProcess: custom stdout unsupported";
        }

        if(stderr_fd === -1) {
            stderr_p = 'pipe'
        } else if(stderr_fd === 2) {
            stderr_p = process.stderr;
        } else {
            throw "runInteractiveProcess: custom stderr unsupported";
        }

        var options = { detached: createGroup
                        , stdio: [stdin_p, stdout_p, stderr_p]
                      };
        if(workingDir !== null) options.cwd = workingDir;
        if(env !== null) {
            var envObj = {};
            for(var i=0;i<env.length;i+=2) envObj[env[i]] = env[i+1];
            if(process.env['GHCJS_BOOTING']) envObj['GHCJS_BOOTING']=1;
            if(process.env['GHCJS_BOOTING1']) envObj['GHCJS_BOOTING1']=1;
            TRACE_PROCESS("environment: " + h$collectProps(envObj));
            options.env = envObj;
        }

        var procObj;
        var child;
	// node.js on Windows x86 sometimes throw an EBADF exception when process.stdin is invalid,
	// retry with ignored stdin when this happens
	try {
	    child = h$child.spawn(cmd, args, options);
	} catch(e) {
	    if(e.toString().indexOf('EBADF') !== -1 && options.stdio[0] === process.stdin) {
		options.stdio[0] = 'ignore';
		child = h$child.spawn(cmd, args, options);
	    } else {
		throw e;
	    }
	}
        child.on('exit', function(code, sig) {
            TRACE_PROCESS("process finished: " + code + " " + sig);
            procObj.exit = code;
            for(var i=0;i<procObj.waiters.length;i++) {
                procObj.waiters[i](code);
            }
        });

        // fixme this leaks
        procObj = { pid: h$nProc
                    , fds: [ stdin_fd  === -1 ? h$process_pipeFd(child.stdio[0], true)  : 0
                             , stdout_fd === -1 ? h$process_pipeFd(child.stdio[1], false) : 1
                             , stderr_fd === -1 ? h$process_pipeFd(child.stdio[2], false) : 2
                           ]
                    , exit: null
                    , waiters : []
                    , child: child
                  };
        h$procs[h$nProc++] = procObj;

        return procObj;
    } else
#endif
        // fixme we need an IOError not a JSException
        throw "$process_runInteractiveProcess: unsupported";
}

#ifndef GHCJS_BROWSER
var h$nProc = 1;
var h$procs = [];
#endif

// return the thing to run as an array, first element the process, rest the args
// null if no interpreter can be found
function h$process_commandToProcess(cmd, args) {
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        TRACE_PROCESS("commandToProcess: " + cmd + " " + args);
        if(process.platform === 'win32') {
            if(args === null) { // shellcmd
                var com = process.env['COMSPEC'];
                if(!com) {
                    com = h$directory_findExecutables("cmd.exe");
                    if(com.length) {
                        com = cmd[0];
                    } else {
                        com = h$directory_findExecutables("command.com");
                        if(!com.length) return null;
                        com = com[0];
                    }
                }
                // fixme need to escape stuff
                return [com, com + " /c " + args];
            } else {
                // fixme need to escape stuff
                var r = [cmd];
                r = r.concat(args);
                return r;
            }
        } else {  // non-windows
            if(args === null) { // shellcmd
                return ["/bin/sh", "-c", cmd];
            } else {
                var r = [cmd];
                r = r.concat(args);
                return r;
            }
        }
    } else
#endif
        // fixme we need an IOError not a JSException
        throw "process_commandToProcess: unsupported";
}

function h$process_terminateProcess(pid) {
    TRACE_PROCESS("terminateProcess: " + pid);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var p = h$procs[pid];
        p.child.kill();
    }
#endif
    return 0; // fixme error status?
}

function h$process_getProcessExitCode(pid, code_d, code_o) {
    TRACE_PROCESS("getProcessExitCode: " + pid);
#ifdef GHCJS_BROWSER
    return 0;
#else
    var p = h$procs[pid];
    if(p.exit === null) return 0;
    code_d.i3[code_o] = p.exit;
    return 1;
#endif
}

function h$process_waitForProcess(pid, code_d, code_o, c) {
    TRACE_PROCESS("waitForProcess: " + pid);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var p = h$procs[pid];
        if(p.exit !== null) {
            h$process_getProcessExitCode(pid, code_d, code_o);
            c(0);
        } else {
            p.waiters.push(function(code) {
		code_d.i3[code_o] = code;
		c(0);
	    });
        }
    } else
#endif
        h$unsupported(-1, c);
}


