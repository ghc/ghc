/*
  GHC JS Interpreter

  See Note [The JS interpreter] in GHC.Runtime.Interpreter.JS

  Read commands on stdin (ending with \n):
    LOAD foo.js             : load foo.js file
    RUN_SERVER ghci_unit_id : run ghci_unit_id:GHCi.Server.defaultServer

  Once the Haskell server is started with RUN_SERVER, the JS server no longer
  reads commands on stdin. Everything must go through the Haskell server (which
  uses pipes for communication)
*/

var h$THfs = require('fs');
var h$THvm = require('vm');

function h$debug_log(s) {
  // uncomment the following line to enable some debug messages
  // console.log("[JS interpreter] " + s);
}

// load and exec JS file
function h$loadJS(path) {
  h$debug_log("Loading file: " + path);
  var data = h$THfs.readFileSync(path);
  const script = new h$THvm.Script(data);
  script.runInThisContext();
}

// Lookup a static closure by its name
function h$lookupClosure(v) {
  h$debug_log("Looking up closure: " + v);
  const r = eval(v);
  h$debug_log("  -> Result: " + r);
  if (!r) return 0;
  // a RemoteRef is just the offset of a stable pointer
  return h$makeStablePtr(r);
}

// give access to these functions to the dynamically linked code
globalThis.h$loadJS = h$loadJS;
globalThis.h$lookupClosure = h$lookupClosure;
global.require = require;
global.module  = module;


function h$initInterp() {
  h$debug_log("Welcome to GHC's JS interpreter");

  function stdin_end() {
    h$debug_log('GHC disconnected: goodbye.');
    process.exit(1);
  };

  // read until we find '\n'
  // Accumulate bytes in "bytes" array
  let bytes = [];
  let decoder = new TextDecoder('utf8');

  function stdin_readable() {
    // read until we find '\n'
    while (null !== (bs = process.stdin.read(1))) {
      let b = bs[0];
      switch(b) {
        case 10: // `\n` found. `bytes` must contain a command
          let cmd = decoder.decode(new Uint8Array(bytes));
          bytes = [];
          // we only supports 2 commands: LOAD, RUN_SERVER
          if (cmd.startsWith("LOAD ")) {
            h$loadJS(cmd.slice(5));
          }
          else if (cmd.startsWith("RUN_SERVER ")) {
            let uid = cmd.slice(11);
            let root = eval("h$" + uid + "ZCGHCiziServerzidefaultServer");
            // remove listeners
            process.stdin.removeListener('end',      stdin_end);
            process.stdin.removeListener('readable', stdin_readable);
            // run the server
            h$debug_log("Run server");
            h$main(root);
            // break the loop
            return;
          }
          else {
            console.log("[JS interpreter] Invalid command received: " + cmd);
            process.exit(1);
          }
          break;
        default:
          bytes.push(b);
      }
    }
  };

  // read commands on STDIN
  process.stdin.on('end',      stdin_end);
  process.stdin.on('readable', stdin_readable);
}

h$initInterp();
