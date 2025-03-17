#!/usr/bin/env -S node --disable-warning=ExperimentalWarning --max-old-space-size=65536 --no-turbo-fast-api-calls --wasm-lazy-validation

// Note [The Wasm Dynamic Linker]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// This nodejs script implements the wasm dynamic linker to support
// Template Haskell & ghci in the GHC wasm backend. It loads wasm
// shared libraries, resolves symbols and runs code on demand,
// communicating with the host GHC via the standard iserv protocol.
// Below are questions/answers to elaborate the introduction:
//
// *** What works right now and what doesn't work yet?
//
// loadDLL & bytecode interpreter work. Template Haskell & ghci work.
// Profiled dynamic code works. Compiled code and bytecode can all be
// loaded, though the side effects are constrained to what's supported
// by wasi preview1: we map the full host filesystem into wasm cause
// yolo, but things like processes and sockets don't work.
//
// JSFFI is unsupported in bytecode yet. So in ghci you can't type in
// code that contains JSFFI declarations, though you can invoke
// compiled code that uses JSFFI.
//
// loadArchive/loadObj etc are unsupported and will stay that way. The
// only form of compiled code that can be loaded is wasm shared
// library. There's no code unloading logic. The retain_cafs flag is
// ignored and revertCAFs is a no-op.
//
// ghc -j doesn't work yet (#25285).
//
// *** What are implications to end users?
//
// Even if you intend to compile fully static wasm modules, you must
// compile everything with -dynamic-too to ensure shared libraries are
// present, otherwise TH doesn't work. In cabal, this is achieved by
// setting `shared: True` in the global cabal config (or under a
// `package *` stanza in your `cabal.project`). You also need to set
// `library-for-ghci: False` since that's unsupported.
//
// *** Why not extend the RTS linker in C like every other new
// platform?
//
// Aside from all the pain of binary manipulation in C, what you can
// do in C on wasm is fairly limited: for instance, you can't manage
// executable memory regions at all. So you need a lot of back and
// forth between C and JS host, totally not worth the extra effort
// just for the sake of making the original C RTS linker interface
// partially work.
//
// *** What kind of wasm shared library can be loaded? What features
// work to what extent?
//
// We support .so files produced by wasm-ld --shared which conforms to
// https://github.com/WebAssembly/tool-conventions/blob/f44d6c526a06a19eec59003a924e475f57f5a6a1/DynamicLinking.md.
// All .so files in the wasm32-wasi sysroot as well as those produced
// by ghc can be loaded.
//
// For simplicity, we don't have any special treatment for weak
// symbols. Any unresolved symbol at link-time will not produce an
// error, they will only trigger an error when they're used at
// run-time and the data/function definition has not been realized by
// then.
//
// There's no dlopen/dlclose etc exposed to the C/C++ world, the
// interfaces here are directly called by JSFFI imports in ghci.
// There's no so unloading logic yet, but it would be fairly easy to
// add once we need it.
//
// No fancy stuff like LD_PRELOAD, LD_LIBRARY_PATH etc.
//
// *** How does GHC interact with the wasm dynamic linker?
//
// dyld.mjs is tracked as a build dependency and installed in GHC
// libdir with executable perms. When GHC targets wasm and needs to
// start iserv, it starts dyld.mjs and manage the process lifecycle
// through the entire GHC session. nodejs location is not tracked and
// must be present in PATH.
//
// GHC passes the libHSghci*.so location via command line, so dyld.mjs
// will load it as well as all dependent so files, then start the
// default iserv implementation in the ghci library and read/write
// binary messages. nodejs receives pipe file descriptors from GHC,
// though uvwasi doesn't support preopening them as wasi virtual file
// descriptors, therefore we hook a few wasi syscalls and designate
// our own preopen file descriptors for IPC logic.
//
// dyld.mjs inherits default stdin/stdout/stderr from GHC and that's
// how ghci works. Like native external interpreter, you can use the
// -opti GHC flag to pass process arguments, like RTS flags or -opti-v
// to dump the iserv messages.

import { JSValManager, setImmediate } from "./prelude.mjs";
import { parseRecord, parseSections } from "./post-link.mjs";

// Make a consumer callback from a buffer. See Parser class
// constructor comments for what a consumer is.
function makeBufferConsumer(buf) {
  return (len) => {
    if (len > buf.length) {
      throw new Error("not enough bytes");
    }

    const r = buf.subarray(0, len);
    buf = buf.subarray(len);
    return r;
  };
}

// Make a consumer callback from a ReadableStreamDefaultReader.
function makeStreamConsumer(reader) {
  let buf = new Uint8Array();

  return async (len) => {
    while (buf.length < len) {
      const { done, value } = await reader.read();
      if (done) {
        throw new Error("not enough bytes");
      }
      if (buf.length === 0) {
        buf = value;
        continue;
      }
      const tmp = new Uint8Array(buf.length + value.length);
      tmp.set(buf, 0);
      tmp.set(value, buf.length);
      buf = tmp;
    }

    const r = buf.subarray(0, len);
    buf = buf.subarray(len);
    return r;
  };
}

// A simple binary parser
class Parser {
  #cb;
  #consumed = 0;
  #limit;

  // cb is a consumer callback that returns a buffer with exact N
  // bytes for await cb(N). limit indicates how many bytes the Parser
  // may consume at most; it's optional and only used by eof().
  constructor(cb, limit) {
    this.#cb = cb;
    this.#limit = limit;
  }

  eof() {
    return this.#consumed >= this.#limit;
  }

  async skip(len) {
    await this.#cb(len);
    this.#consumed += len;
  }

  async readUInt8() {
    const r = (await this.#cb(1))[0];
    this.#consumed += 1;
    return r;
  }

  async readULEB128() {
    let acc = 0n,
      shift = 0n;
    while (true) {
      const byte = await this.readUInt8();
      acc |= BigInt(byte & 0x7f) << shift;
      shift += 7n;
      if (byte >> 7 === 0) {
        break;
      }
    }
    return Number(acc);
  }

  async readBuffer() {
    const len = await this.readULEB128();
    const r = await this.#cb(len);
    this.#consumed += len;
    return r;
  }

  async readString() {
    return new TextDecoder("utf-8", { fatal: true }).decode(
      await this.readBuffer()
    );
  }
}

// Parse the dylink.0 section of a wasm module
async function parseDyLink0(reader) {
  const p0 = new Parser(makeStreamConsumer(reader));
  // magic, version
  await p0.skip(8);
  // section id
  console.assert((await p0.readUInt8()) === 0);
  const p1_buf = await p0.readBuffer();
  const p1 = new Parser(makeBufferConsumer(p1_buf), p1_buf.length);
  // custom section name
  console.assert((await p1.readString()) === "dylink.0");

  const r = { neededSos: [], exportInfo: [], importInfo: [] };
  while (!p1.eof()) {
    const subsection_type = await p1.readUInt8();
    const p2_buf = await p1.readBuffer();
    const p2 = new Parser(makeBufferConsumer(p2_buf), p2_buf.length);
    switch (subsection_type) {
      case 1: {
        // WASM_DYLINK_MEM_INFO
        r.memSize = await p2.readULEB128();
        r.memP2Align = await p2.readULEB128();
        r.tableSize = await p2.readULEB128();
        r.tableP2Align = await p2.readULEB128();
        break;
      }
      case 2: {
        // WASM_DYLINK_NEEDED
        //
        // There may be duplicate entries. Not a big deal to not
        // dedupe, but why not.
        const n = await p2.readULEB128();
        const acc = new Set();
        for (let i = 0; i < n; ++i) {
          acc.add(await p2.readString());
        }
        r.neededSos = [...acc];
        break;
      }
      case 3: {
        // WASM_DYLINK_EXPORT_INFO
        //
        // Not actually used yet, kept for completeness in case of
        // future usage.
        const n = await p2.readULEB128();
        for (let i = 0; i < n; ++i) {
          const name = await p2.readString();
          const flags = await p2.readULEB128();
          r.exportInfo.push({ name, flags });
        }
        break;
      }
      case 4: {
        // WASM_DYLINK_IMPORT_INFO
        //
        // Same.
        const n = await p2.readULEB128();
        for (let i = 0; i < n; ++i) {
          const module = await p2.readString();
          const name = await p2.readString();
          const flags = await p2.readULEB128();
          r.importInfo.push({ module, name, flags });
        }
        break;
      }
      default: {
        throw new Error(`unknown subsection type ${subsection_type}`);
      }
    }
  }

  return r;
}

// Formats a server.address() result to a URL origin with correct
// handling for IPv6 hostname
function originFromServerAddress({ address, family, port }) {
  const hostname = family === "IPv6" ? `[${address}]` : address;
  return `http://${hostname}:${port}`;
}

// Browser/node portable code stays above this watermark.
const isNode = Boolean(globalThis?.process?.versions?.node);

// Too cumbersome to only import at use sites. Too troublesome to
// factor out browser-only/node-only logic into different modules. For
// now, just make these global let bindings optionally initialized if
// isNode and be careful to not use them in browser-only logic.
let fs, http, path, require, stream, wasi, ws;

if (isNode) {
  require = (await import("node:module")).createRequire(import.meta.url);

  fs = require("fs");
  http = require("http");
  path = require("path");
  stream = require("stream");
  wasi = require("wasi");

  // Optional npm dependencies loaded via NODE_PATH
  try {
    ws = require("ws");
  } catch {}
} else {
  wasi = await import(
    "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js"
  );
}

// A subset of dyld logic that can only be run in the host node
// process and has full access to local filesystem
class DyLDHost {
  // Deduped absolute paths of directories where we lookup .so files
  #rpaths = new Set();

  constructor() {
    // Inherited pipe file descriptors from GHC
    const out_fd = Number.parseInt(process.argv[4]),
      in_fd = Number.parseInt(process.argv[5]);

    this.readStream = stream.Readable.toWeb(
      fs.createReadStream(undefined, { fd: in_fd })
    );
    this.writeStream = stream.Writable.toWeb(
      fs.createWriteStream(undefined, { fd: out_fd })
    );
  }

  close() {}

  installSignalHandlers(cb) {
    process.on("SIGINT", cb);
    process.on("SIGQUIT", cb);
  }

  // removeLibrarySearchPath is a no-op in ghci. If you have a use
  // case where it's actually needed, I would like to hear..
  async addLibrarySearchPath(p) {
    this.#rpaths.add(path.resolve(p));
    return null;
  }

  // f can be either just soname or an absolute path, will be
  // canonicalized and checked for file existence here. Throws if
  // non-existent.
  async findSystemLibrary(f) {
    if (path.isAbsolute(f)) {
      await fs.promises.access(f, fs.promises.constants.R_OK);
      return f;
    }
    const r = (
      await Promise.allSettled(
        [...this.#rpaths].map(async (p) => {
          const r = path.resolve(p, f);
          await fs.promises.access(r, fs.promises.constants.R_OK);
          return r;
        })
      )
    ).find(({ status }) => status === "fulfilled");
    console.assert(
      r,
      `findSystemLibrary(${f}): not found in ${[...this.#rpaths]}`
    );
    return r.value;
  }

  // returns a Response for a .so absolute path
  async fetchWasm(p) {
    return new Response(stream.Readable.toWeb(fs.createReadStream(p)), {
      headers: { "Content-Type": "application/wasm" },
    });
  }
}

// Fulfill the same functionality as DyLDHost by doing fetch() calls
// to respective RPC endpoints of a host http server. Also manages
// WebSocket connections back to host.
export class DyLDRPC {
  #origin;
  #wsPipe;
  #wsSig;

  constructor({ origin }) {
    this.#origin = origin;

    const ws_url = this.#origin.replace("http://", "ws://");

    this.#wsPipe = new WebSocket(ws_url, "pipe");
    this.#wsPipe.binaryType = "arraybuffer";

    this.readStream = new ReadableStream({
      start: (controller) => {
        this.#wsPipe.addEventListener("message", (ev) =>
          controller.enqueue(new Uint8Array(ev.data))
        );
        this.#wsPipe.addEventListener("error", (ev) => controller.error(ev));
        this.#wsPipe.addEventListener("close", () => controller.close());
      },
    });

    this.writeStream = new WritableStream({
      start: (controller) => {
        this.#wsPipe.addEventListener("error", (ev) => controller.error(ev));
      },
      write: (buf) => this.#wsPipe.send(buf),
    });

    this.#wsSig = new WebSocket(ws_url, "sig");
    this.#wsSig.binaryType = "arraybuffer";

    this.opened = Promise.all(
      [this.#wsPipe, this.#wsSig].map(
        (ws) =>
          new Promise((res, rej) => {
            ws.addEventListener("open", res);
            ws.addEventListener("error", rej);
          })
      )
    );
  }

  close() {
    this.#wsPipe.close();
    this.#wsSig.close();
  }

  async #rpc(endpoint, ...args) {
    const r = await fetch(`${this.#origin}/rpc/${endpoint}`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(args),
    });
    if (!r.ok) {
      throw new Error(await r.text());
    }
    return r.json();
  }

  installSignalHandlers(cb) {
    this.#wsSig.addEventListener("message", cb);
  }

  async addLibrarySearchPath(p) {
    return this.#rpc("addLibrarySearchPath", p);
  }

  async findSystemLibrary(f) {
    return this.#rpc("findSystemLibrary", f);
  }

  async fetchWasm(p) {
    return fetch(`${this.#origin}/fs${p}`);
  }
}

// Actual implementation of endpoints used by DyLDRPC
class DyLDRPCServer {
  #dyldHost = new DyLDHost();
  #server;
  #wss;

  constructor({ host, port, dyldPath, libdir, ghciSoPath, args }) {
    this.#server = http.createServer(async (req, res) => {
      const origin = originFromServerAddress(await this.listening);

      res.setHeader("Access-Control-Allow-Origin", "*");
      res.setHeader("Access-Control-Allow-Headers", "*");

      if (req.method === "OPTIONS") {
        res.writeHead(204);
        res.end();
        return;
      }

      if (req.url === "/main.html") {
        res.writeHead(200, {
          "Content-Type": "text/html",
        });
        res.end(
          `
<!DOCTYPE html>
<title>wasm ghci</title>
<script type="module" src="./main.js"></script>
`
        );
        return;
      }

      if (req.url === "/main.js") {
        res.writeHead(200, {
          "Content-Type": "application/javascript",
        });
        res.end(
          `
import { DyLDRPC, main } from "./fs${dyldPath}";
const args = ${JSON.stringify({ libdir, ghciSoPath, args })};
args.rpc = new DyLDRPC({origin: "${origin}"});
args.rpc.opened.then(() => main(args));
`
        );
        return;
      }

      if (req.url.startsWith("/fs")) {
        const p = req.url.replace("/fs", "");

        res.setHeader(
          "Content-Type",
          {
            ".mjs": "application/javascript",
            ".so": "application/wasm",
          }[path.extname(p)] || "application/octet-stream"
        );

        const buf = Buffer.from(await fs.promises.readFile(p));
        const etag = `sha512-${Buffer.from(
          await crypto.subtle.digest("SHA-512", buf)
        ).toString("base64")}`;

        res.setHeader("ETag", etag);

        if (req.headers["if-none-match"] === etag) {
          res.writeHead(304);
          res.end();
          return;
        }

        res.writeHead(200);
        res.end(buf);
        return;
      }

      if (req.url.startsWith("/rpc")) {
        const endpoint = req.url.replace("/rpc/", "");

        let body = "";
        for await (const chunk of req) {
          body += chunk;
        }

        res.writeHead(200, {
          "Content-Type": "application/json",
        });
        res.end(
          JSON.stringify(await this.#dyldHost[endpoint](...JSON.parse(body)))
        );
        return;
      }

      res.writeHead(404, {
        "Content-Type": "text/plain",
      });
      res.end("not found");
    });

    this.closed = new Promise((res) => this.#server.on("close", res));

    this.#wss = new ws.WebSocketServer({ server: this.#server });
    this.#wss.on("connection", (ws) => {
      ws.addEventListener("error", () => {
        this.#wss.close();
        this.#server.close();
      });

      ws.addEventListener("close", () => {
        this.#wss.close();
        this.#server.close();
      });

      if (ws.protocol === "pipe") {
        (async () => {
          for await (const buf of this.#dyldHost.readStream) {
            ws.send(buf);
          }
        })();
        const writer = this.#dyldHost.writeStream.getWriter();
        ws.addEventListener("message", (ev) =>
          writer.write(new Uint8Array(ev.data))
        );
        return;
      }

      if (ws.protocol === "sig") {
        this.#dyldHost.installSignalHandlers(() => ws.send(new Uint8Array(0)));
        return;
      }

      throw new Error(`unknown protocol ${ws.protocol}`);
    });

    this.listening = new Promise((res) =>
      this.#server.listen({ host, port }, () => res(this.#server.address()))
    );
  }
}

// The real stuff
class DyLD {
  // Wasm page size.
  static #pageSize = 0x10000;

  // Placeholder value of GOT.mem addresses that must be imported
  // first and later modified to be the correct relocated pointer.
  // This value is 0xffffffff subtracts one page, so hopefully any
  // memory access near this address will trap immediately.
  //
  // In JS API i32 is signed, hence this layer of redirection.
  static #poison = (0xffffffff - DyLD.#pageSize) | 0;

  // When processing exports, skip the following ones since they're
  // generated by wasm-ld.
  static #ldGeneratedExportNames = new Set([
    "_initialize",
    "__wasm_apply_data_relocs",
    "__wasm_apply_global_relocs",
    "__wasm_call_ctors",
  ]);

  // Handles RPC logic back to host in a browser, or just do plain
  // function calls in node
  #rpc;

  // The WASI instance to provide wasi imports, shared across all wasm
  // instances
  #wasi;

  // Wasm memory & table
  #memory = new WebAssembly.Memory({ initial: 1 });
  #table = new WebAssembly.Table({ element: "anyfunc", initial: 1 });

  // __stack_pointer
  #sp = new WebAssembly.Global(
    {
      value: "i32",
      mutable: true,
    },
    DyLD.#pageSize
  );

  // The JSVal manager
  #jsvalManager = new JSValManager();

  // sonames of loaded sos.
  //
  // Note that "soname" is just xxx.so as in file path, not actually
  // parsed from a section in .so file. wasm-ld does accept
  // --soname=<value>, but it just writes the module name to the name
  // section, which can be stripped by wasm-opt and such. We do not
  // rely on the name section at all.
  //
  // Invariant: soname is globally unique!
  #loadedSos = new Set();

  // Mapping from export names to export funcs. It's also passed as
  // __exports in JSFFI code, hence the "memory" special field.
  exportFuncs = { memory: this.#memory };

  // The FinalizationRegistry used by JSFFI.
  #finalizationRegistry = new FinalizationRegistry((sp) =>
    this.exportFuncs.rts_freeStablePtr(sp)
  );

  // The GOT.func table.
  #gotFunc = {};

  // The GOT.mem table. By wasm dylink convention, a wasm global
  // exported by .so is always assumed to be a GOT.mem entry, not a
  // re-exported actual wasm global.
  #gotMem = {};

  // Global STG registers
  #regs = {};

  constructor({ args, rpc }) {
    this.#rpc = rpc;

    if (isNode) {
      this.#wasi = new wasi.WASI({
        version: "preview1",
        args,
        env: { PATH: "", PWD: process.cwd() },
        preopens: { "/": "/" },
      });
    } else {
      this.#wasi = new wasi.WASI(
        args,
        [],
        [
          new wasi.OpenFile(
            new wasi.File(new Uint8Array(), { readonly: true })
          ),
          wasi.ConsoleStdout.lineBuffered((msg) =>
            console.log(`[WASI stdout] ${msg}`)
          ),
          wasi.ConsoleStdout.lineBuffered((msg) =>
            console.warn(`[WASI stderr] ${msg}`)
          ),
        ],
        { debug: false }
      );
    }

    // Keep this in sync with rts/wasm/Wasm.S!
    for (let i = 1; i <= 10; ++i) {
      this.#regs[`__R${i}`] = new WebAssembly.Global({
        value: "i32",
        mutable: true,
      });
    }

    for (let i = 1; i <= 6; ++i) {
      this.#regs[`__F${i}`] = new WebAssembly.Global({
        value: "f32",
        mutable: true,
      });
    }

    for (let i = 1; i <= 6; ++i) {
      this.#regs[`__D${i}`] = new WebAssembly.Global({
        value: "f64",
        mutable: true,
      });
    }

    this.#regs.__L1 = new WebAssembly.Global({ value: "i64", mutable: true });

    for (const k of ["__Sp", "__SpLim", "__Hp", "__HpLim"]) {
      this.#regs[k] = new WebAssembly.Global({ value: "i32", mutable: true });
    }
  }

  async addLibrarySearchPath(p) {
    return this.#rpc.addLibrarySearchPath(p);
  }

  async findSystemLibrary(f) {
    return this.#rpc.findSystemLibrary(f);
  }

  // When we do loadDLL, we first perform "downsweep" which return a
  // toposorted array of dependencies up to itself, then sequentially
  // load the downsweep result.
  //
  // The rationale of a separate downsweep phase, instead of a simple
  // recursive loadDLL function is: V8 delegates async
  // WebAssembly.compile to a background worker thread pool. To
  // maintain consistent internal linker state, we *must* load each so
  // file sequentially, but it's okay to kick off compilation asap,
  // store the Promise in downsweep result and await for the actual
  // WebAssembly.Module in loadDLL logic. This way we can harness some
  // background parallelism.
  async #downsweep(p) {
    const toks = p.split("/");

    const soname = toks[toks.length - 1];

    if (this.#loadedSos.has(soname)) {
      return [];
    }

    // Do this before loading dependencies to break potential cycles.
    this.#loadedSos.add(soname);

    if (p.startsWith("/")) {
      // GHC may attempt to load libghc_tmp_2.so that needs
      // libghc_tmp_1.so in a temporary directory without adding that
      // directory via addLibrarySearchPath
      toks.pop();
      await this.addLibrarySearchPath(toks.join("/"));
    } else {
      p = await this.findSystemLibrary(p);
    }

    const resp = await this.#rpc.fetchWasm(p);
    const resp2 = resp.clone();
    const modp = WebAssembly.compileStreaming(resp);
    // Parse dylink.0 from the raw buffer, not via
    // WebAssembly.Module.customSections(). This should return asap
    // without waiting for rest of the wasm module binary data.
    const r = await parseDyLink0(resp2.body.getReader());
    r.modp = modp;
    r.soname = soname;
    let acc = [];
    for (const dep of r.neededSos) {
      acc.push(...(await this.#downsweep(dep)));
    }
    acc.push(r);
    return acc;
  }

  // The real stuff
  async loadDLL(p) {
    for (const {
      memSize,
      memP2Align,
      tableSize,
      tableP2Align,
      modp,
      soname,
    } of await this.#downsweep(p)) {
      const import_obj = {
        wasi_snapshot_preview1: this.#wasi.wasiImport,
        env: {
          memory: this.#memory,
          __indirect_function_table: this.#table,
          __stack_pointer: this.#sp,
          ...this.exportFuncs,
        },
        regs: this.#regs,
        // Keep this in sync with post-link.mjs!
        ghc_wasm_jsffi: {
          newJSVal: (v) => this.#jsvalManager.newJSVal(v),
          getJSVal: (k) => this.#jsvalManager.getJSVal(k),
          freeJSVal: (k) => this.#jsvalManager.freeJSVal(k),
          scheduleWork: () => setImmediate(this.exportFuncs.rts_schedulerLoop),
        },
        "GOT.mem": this.#gotMem,
        "GOT.func": this.#gotFunc,
      };

      // __memory_base & __table_base, different for each .so
      let memory_base;
      let table_base = this.#table.grow(tableSize);
      console.assert(tableP2Align === 0);

      // libc.so is always the first one to be ever loaded and has VIP
      // treatment. It will never be unloaded even if we support
      // unloading in the future. Nor do we support multiple libc.so
      // in the same address space.
      if (soname === "libc.so") {
        // Starting from 0x0: one page of C stack, then global data
        // segments of libc.so, then one page space between
        // __heap_base/__heap_end so that dlmalloc can initialize
        // global state. wasm-ld aligns __heap_base to page sized so
        // we follow suit.
        console.assert(memP2Align <= Math.log2(DyLD.#pageSize));
        memory_base = DyLD.#pageSize;
        const data_pages = Math.ceil(memSize / DyLD.#pageSize);
        this.#memory.grow(data_pages + 1);

        this.#gotMem.__heap_base = new WebAssembly.Global(
          { value: "i32", mutable: true },
          DyLD.#pageSize * (1 + data_pages)
        );
        this.#gotMem.__heap_end = new WebAssembly.Global(
          { value: "i32", mutable: true },
          DyLD.#pageSize * (1 + data_pages + 1)
        );
      } else {
        // TODO: this would also be __dso_handle@GOT, in case we
        // implement so unloading logic in the future.
        memory_base = this.exportFuncs.aligned_alloc(1 << memP2Align, memSize);
      }

      import_obj.env.__memory_base = new WebAssembly.Global(
        { value: "i32", mutable: false },
        memory_base
      );
      import_obj.env.__table_base = new WebAssembly.Global(
        { value: "i32", mutable: false },
        table_base
      );

      const mod = await modp;

      // Fulfill the ghc_wasm_jsffi imports. Use new Function()
      // instead of eval() to prevent bindings in this local scope to
      // be accessed by JSFFI code snippets. See Note [Variable passing in JSFFI]
      // for what's going on here.
      Object.assign(
        import_obj.ghc_wasm_jsffi,
        new Function(
          "__exports",
          "__ghc_wasm_jsffi_dyld",
          "__ghc_wasm_jsffi_finalization_registry",
          "return {".concat(
            ...parseSections(mod).map(
              (rec) => `${rec[0]}: ${parseRecord(rec)}, `
            ),
            "};"
          )
        )(this.exportFuncs, this, this.#finalizationRegistry)
      );

      // Fulfill the rest of the imports
      for (const { module, name, kind } of WebAssembly.Module.imports(mod)) {
        // Already there, no handling required
        if (import_obj[module] && import_obj[module][name]) {
          continue;
        }

        // Add a lazy function stub in env, but don't put it into
        // exportFuncs yet. This lazy binding is only effective for
        // the current so, since env is a transient object created on
        // the fly.
        if (module === "env" && kind === "function") {
          import_obj.env[name] = (...args) => {
            if (!this.exportFuncs[name]) {
              throw new WebAssembly.RuntimeError(
                `non-existent function ${name}`
              );
            }
            return this.exportFuncs[name](...args);
          };
          continue;
        }

        // Add a lazy GOT.mem entry with poison value, in the hope
        // that if they're used before being resolved with real
        // addresses, a memory trap will be triggered immediately.
        if (module === "GOT.mem" && kind === "global") {
          this.#gotMem[name] = new WebAssembly.Global(
            { value: "i32", mutable: true },
            DyLD.#poison
          );
          continue;
        }

        // Missing entry in GOT.func table, could be already defined
        // or not
        if (module === "GOT.func" && kind === "global") {
          // A dependency has exported the function, just create the
          // entry on the fly
          if (this.exportFuncs[name]) {
            this.#gotFunc[name] = new WebAssembly.Global(
              { value: "i32", mutable: true },
              this.#table.grow(1, this.exportFuncs[name])
            );
            continue;
          }

          // Can't find this function, so poison it like GOT.mem.
          // TODO: when wasm type reflection is widely available in
          // browsers, use the WebAssembly.Function constructor to
          // dynamically create a stub function that does better error
          // reporting
          this.#gotFunc[name] = new WebAssembly.Global(
            { value: "i32", mutable: true },
            DyLD.#poison
          );
          continue;
        }

        throw new Error(
          `cannot handle import ${module}.${name} with kind ${kind}`
        );
      }

      // Fingers crossed...
      const instance = await WebAssembly.instantiate(mod, import_obj);

      // Process the exports
      for (const k in instance.exports) {
        // Boring stuff
        if (DyLD.#ldGeneratedExportNames.has(k)) {
          continue;
        }

        // Invariant: each function symbol can be defined only once.
        // This is incorrect for weak symbols which are allowed to
        // appear multiple times but this is sufficient in practice.
        console.assert(
          !this.exportFuncs[k],
          `duplicate symbol ${k} when loading ${soname}`
        );

        const v = instance.exports[k];

        if (typeof v === "function") {
          this.exportFuncs[k] = v;
          // If there's a lazy GOT.func entry, put the function in the
          // table and fulfill the entry. Otherwise no need to do
          // anything, if it's required later a GOT.func entry will be
          // created on demand.
          if (this.#gotFunc[k]) {
            // ghc-prim/ghc-internal may export functions imported by
            // rts
            console.assert(this.#gotFunc[k].value === DyLD.#poison);
            this.#table.set(this.#gotFunc[k].value, v);
          }
          continue;
        }

        // It's a GOT.mem entry
        if (v instanceof WebAssembly.Global) {
          const addr = v.value + memory_base;
          if (this.#gotMem[k]) {
            console.assert(this.#gotMem[k].value === DyLD.#poison);
            this.#gotMem[k].value = addr;
          } else {
            this.#gotMem[k] = new WebAssembly.Global(
              { value: "i32", mutable: true },
              addr
            );
          }
          continue;
        }

        throw new Error(`cannot handle export ${k} ${v}`);
      }

      // We call wasi.initialize when loading libc.so, then reuse the
      // wasi instance globally. When loading later .so files, just
      // manually invoke _initialize().
      if (soname === "libc.so") {
        instance.exports.__wasm_apply_data_relocs();
        // wasm-ld forbits --export-memory with --shared, I don't know
        // why but this is sufficient to make things work
        this.#wasi.initialize({
          exports: {
            memory: this.#memory,
            _initialize: instance.exports._initialize,
          },
        });
        continue;
      }

      const init = () => {
        // See
        // https://gitlab.haskell.org/haskell-wasm/llvm-project/-/blob/release/20.x/lld/wasm/Writer.cpp#L1450,
        // __wasm_apply_data_relocs is now optional so only call it if
        // it exists (we know for sure it exists for libc.so though).
        // There's also __wasm_init_memory (not relevant yet, we don't
        // use passive segments) & __wasm_apply_global_relocs but
        // those are included in the start function and should have
        // been called upon instantiation, see
        // Writer::createStartFunction().
        if (instance.exports.__wasm_apply_data_relocs) {
          instance.exports.__wasm_apply_data_relocs();
        }

        instance.exports._initialize();
      };

      // rts init must be deferred until ghc-internal symbols are
      // exported. We hard code this hack for now.
      if (/libHSrts-\d+(\.\d+)*/i.test(soname)) {
        this.rts_init = init;
        continue;
      }
      if (/libHSghc-internal-\d+(\.\d+)*/i.test(soname)) {
        this.rts_init();
        delete this.rts_init;
      }
      init();
    }
  }

  lookupSymbol(sym) {
    if (this.#gotMem[sym] && this.#gotMem[sym].value !== DyLD.#poison) {
      return this.#gotMem[sym].value;
    }
    if (this.#gotFunc[sym] && this.#gotFunc[sym].value !== DyLD.#poison) {
      return this.#gotFunc[sym].value;
    }
    // Not in GOT.func yet, create the entry on demand
    if (this.exportFuncs[sym]) {
      console.assert(!this.#gotFunc[sym]);
      const addr = this.#table.grow(1, this.exportFuncs[sym]);
      this.#gotFunc[sym] = new WebAssembly.Global(
        { value: "i32", mutable: true },
        addr
      );
      return addr;
    }
    return 0;
  }
}

export async function main({ rpc, libdir, ghciSoPath, args }) {
  try {
    const dyld = new DyLD({
      args: ["dyld.so", ...args],
      rpc,
    });
    await dyld.addLibrarySearchPath(libdir);
    await dyld.loadDLL(ghciSoPath);

    const reader = rpc.readStream.getReader();
    const writer = rpc.writeStream.getWriter();

    const cb_sig = (cb) => {
      rpc.installSignalHandlers(cb);
    };

    const cb_recv = async () => {
      const { done, value } = await reader.read();
      if (done) {
        throw new Error("not enough bytes");
      }
      return value;
    };
    const cb_send = (buf) => {
      writer.write(new Uint8Array(buf));
    };

    await dyld.exportFuncs.defaultServer(cb_sig, cb_recv, cb_send);
  } finally {
    rpc.close();
  }
}

(async () => {
  if (!isNode) {
    return;
  }

  const libdir = process.argv[2];
  const ghciSoPath = process.argv[3];
  const args = process.argv.slice(6);

  if (!process.env.GHCI_BROWSER) {
    const rpc = new DyLDHost();
    await main({
      rpc,
      libdir,
      ghciSoPath,
      args,
    });
    return;
  }

  if (!ws) {
    throw new Error(
      "Please install ws and ensure it's available via NODE_PATH"
    );
  }

  const server = new DyLDRPCServer({
    host: process.env.GHCI_BROWSER_HOST || "127.0.0.1",
    port: process.env.GHCI_BROWSER_PORT || 0,
    dyldPath: import.meta.filename,
    libdir,
    ghciSoPath,
    args,
  });
  const origin = originFromServerAddress(await server.listening);

  // https://pptr.dev/api/puppeteer.consolemessage
  // https://playwright.dev/docs/api/class-consolemessage
  const on_console_msg = (msg) => {
    switch (msg.type()) {
      case "error":
      case "warn":
      case "warning":
      case "trace":
      case "assert": {
        console.error(msg.text());
        break;
      }
      default: {
        console.log(msg.text());
        break;
      }
    }
  };

  if (process.env.GHCI_BROWSER_PUPPETEER_LAUNCH_OPTS) {
    let puppeteer;
    try {
      puppeteer = require("puppeteer");
    } catch {
      puppeteer = require("puppeteer-core");
    }

    // https://pptr.dev/api/puppeteer.puppeteernode.launch
    const browser = await puppeteer.launch(
      JSON.parse(process.env.GHCI_BROWSER_PUPPETEER_LAUNCH_OPTS)
    );
    try {
      const page = await browser.newPage();

      // https://pptr.dev/api/puppeteer.pageevent
      page.on("console", on_console_msg);
      page.on("error", (err) => console.error(err));
      page.on("pageerror", (err) => console.error(err));

      await page.goto(`${origin}/main.html`);
      await server.closed;
      return;
    } finally {
      await browser.close();
    }
  }

  if (process.env.GHCI_BROWSER_PLAYWRIGHT_BROWSER_TYPE) {
    let playwright;
    try {
      playwright = require("playwright");
    } catch {
      playwright = require("playwright-core");
    }

    // https://playwright.dev/docs/api/class-browsertype#browser-type-launch
    const browser = await playwright[
      process.env.GHCI_BROWSER_PLAYWRIGHT_BROWSER_TYPE
    ].launch(
      process.env.GHCI_BROWSER_PLAYWRIGHT_LAUNCH_OPTS
        ? JSON.parse(process.env.GHCI_BROWSER_PLAYWRIGHT_LAUNCH_OPTS)
        : {}
    );
    try {
      const page = await browser.newPage();

      // https://playwright.dev/docs/api/class-page#events
      page.on("console", on_console_msg);
      page.on("pageerror", (err) => console.error(err));

      await page.goto(`${origin}/main.html`);
      await server.closed;
      return;
    } finally {
      await browser.close();
    }
  }

  console.log(
    `Open ${origin}/main.html or import ${origin}/main.js to boot ghci`
  );
})();
