/*
  Template Haskell communication

  reads messages from stdin, sends over stderr
  (Haskell stderr stream is redirected to stdout)

  messages are prefixed with the following data:
    - UInt32BE: message length
    - UInt32BE: message target:
                  0: control message from compiler to TH server
                  n: response to request n
*/

var h$THfs = require('fs');

require('jsdom-global')();

// set this to true to record each message and the received JS code to a file
// you can then replay the TH session later with 'node thrunner.js replay'
var h$THRecord = // true ||
      !!process.env['GHCJS_RECORD_TH'];

var h$THReplay = process.argv.length > 0 && process.argv[process.argv.length-1] === 'replay';

var h$TH = { nWaiters: 0
           , waiters: {}
           , data: []
           , dataLen: 0
           , requestId: 1
           , loadedSymbol: null
           , sendRequest: h$sendRequest
           , awaitMessage: h$awaitMessage
           , sendMessage: h$sendMessage
           , loadCode: h$loadCode
           , loadCodeStr: h$loadCodeStr
           , bufSize: h$bufSize
           , getMemoryUsage: h$getMemoryUsage
           };

global.h$TH = h$TH;
global.require = require;
global.module = module;

// start listening
function h$initTH() {
    process.stdin.setEncoding('utf8');
    process.stderr.setEncoding('binary');
    process.on('uncaughtException', function(err) { console.log(err); });
    h$awaitMessageRaw(0, h$loadInitialCode);
    var leftover = null;
    process.stdin.on('readable', function() {
        while(true) {
            var str = process.stdin.read();
            if(str) {
                // save incomplete hex pair if needed
                str = str.toString();
                if(leftover) str = leftover + str;
                str = str.replace(/\s/gm, '');
                if(str.length % 2) {
                  leftover = str.slice(str.length-1);
                  str = str.slice(0,str.length-1);
                } else {
                  leftover = null;
                }
                var buf = Buffer.from(str, 'hex');

                // make sure the first 8 bytes into data[0]
                // otherwise delay copying the buffers until a complete message
                // has been received
                if(h$TH.data.length < 1 || h$TH.data[0].length >= 8) {
                  h$TH.data.push(buf);
                } else {
                  h$TH.data[0] = Buffer.concat([h$TH.data[0], buf]);
                }
                h$TH.dataLen += buf.length;
                h$processQueue();
            } else {
                return;
            }
        }
    });
    process.stdin.on('close', function() { process.exit(0); });
}

function h$getMemoryUsage() {
  var m = process.memoryUsage();
  // return m.rss;
  return (m.heapTotal + m.external)|0;
}

var h$THMessageN = 0;
function h$processQueue() {
    while(h$TH.nWaiters > 0 && h$TH.data && h$TH.dataLen >= 8) {
        // if we have at least 8 bytes, they are all in data[0]
        var msgLength = h$TH.data[0].readUInt32BE(0);
        var msgTarget = h$TH.data[0].readUInt32BE(4);
        var msgBytes  = msgLength + 8;
        if(h$TH.dataLen >= msgBytes && h$TH.waiters[msgTarget]) {
            var bb         = Buffer.concat(h$TH.data);
            var w          = h$TH.waiters[msgTarget]
            var msgPayload = bb.slice(8, msgBytes);

            h$TH.data = [bb.slice(msgBytes)];
            h$TH.dataLen -= msgBytes;
            delete h$TH.waiters[msgTarget];
            h$TH.nWaiters--;
            if(h$THRecord && !h$THReplay)
                h$THfs.writeFileSync("thmessage." + (++h$THMessageN) + ".dat", msgPayload);
            w(msgPayload);
        } else {
            return;
        }
    }
}

function h$sendRequest(bs, offset, len, c) {
    var req = h$TH.requestId++;
    h$sendMessage(bs, offset, len, req, function() {});
    h$awaitMessage(req, c);
}

function h$sendMessage(bs, offset, len, req, c) {
    var msg = len === -1 ? Buffer.from(bs.u8.subarray(offset))
                         : Buffer.from(bs.u8.subarray(offset, len));
    var hdr = Buffer.alloc(8);
    hdr.writeUInt32BE(msg.length, 0);
    hdr.writeUInt32BE(req, 4);
    process.stderr.write(Buffer.concat([hdr, msg]), 'binary', function() { c(); });
}

function h$awaitMessage(req, c) {
    h$awaitMessageRaw(req, function(buf) {
        c(h$THWrapBuffer(h$BufferToArrayBuffer(buf),false),0);
    });
}

var h$THReplayMessageN = 0;
function h$awaitMessageRaw(req, c) {
    if(h$TH.waiters[req]) throw ("h$awaitMessage: already waiting for " + req);
    if(h$THReplay) {
        try {
            c(h$THfs.readFileSync('thmessage.' + (++h$THReplayMessageN) + '.dat'));
            return;
        } catch(e) { }
    }
    h$TH.nWaiters++;
    h$TH.waiters[req] = c;
    h$processQueue();
}

function h$bufSize(buf, buf_offset) {
    if(buf === null) return 0;
    return buf.len;
}

// load the RTS and start the server
function h$loadInitialCode(buf) {
    var code = buf.toString('utf8');
    h$loadCodeStr(code, true);
    // don't allow Haskell to read from stdin
    h$base_stdin_fd.read = function(fd, fdo, buf, buf_offset, n, c) { c(0); }
    // redirect Haskell's stderr to stdout since we use stderr to communicate
    h$base_stderr_fd.write = h$base_stdout_fd.write;
    h$main(h$ghcjszmthZCGHCJSziPrimziTHziEvalzirunTHServer);

}

var h$THCodeN = 0;
function h$loadCodeStr(str, isFirst) {
    if(h$THReplay) {
        try {
            str = h$THfs.readFileSync("thcode." + (++h$THCodeN) + ".js").toString('utf8');
        } catch(e) { }
    } else if(h$THRecord) {
        h$THfs.writeFileSync("thcode." + (++h$THCodeN) + ".js", str);
    }
    eval.call(null, str);
}

// load additional code and run the initializers for it, the code should
// assign the h$TH.loadedSymbol variable
function h$loadCode(buf, off, len) {
    h$TH.loadedSymbol = null;
    var str = Buffer.from(buf.u8).toString('utf8',off, off+len);
    h$TH.loadCodeStr(str, false);
    // h$runInitStatic();
    if(h$TH.loadedSymbol === null) throw "h$loadCode: error loading code"
    return h$TH.loadedSymbol;
}

function h$BufferToArrayBuffer(buf) {
    if(buf.toArrayBuffer) return buf.toArrayBuffer();
    return new Uint8Array(buf).buffer;
}

// copied from src/mem.js
function h$THWrapBuffer(buf, unalignedOk, offset, length) {
    if(!unalignedOk && offset && offset % 8 !== 0) {
        throw ("h$THWrapBuffer: offset not aligned:" + offset);
    }
    if(!buf || !(buf instanceof ArrayBuffer))
        throw "h$THWrapBuffer: not an ArrayBuffer"
    if(!offset) { offset = 0; }
    if(!length || length < 0) { length = buf.byteLength - offset; }
    // console.log("wrapping buf: " + length + " " + offset);
    return { buf: buf
             , len: length
             , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)
             , u8: new Uint8Array(buf, offset, length)
             , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)
             , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)
             , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)
             , dv: new DataView(buf, offset, length)
           };
}

h$initTH();
