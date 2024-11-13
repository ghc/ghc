#!/usr/bin/env -S node --disable-warning=ExperimentalWarning --experimental-wasm-type-reflection --max-old-space-size=8192 --no-turbo-fast-api-calls --wasm-lazy-validation

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

import assert from "node:assert/strict";
import fs from "node:fs/promises";
import fsSync from "node:fs";
import path from "node:path";
import { WASI } from "node:wasi";
import { JSValManager } from "./prelude.mjs";
import { parseRecord, parseSections } from "./post-link.mjs";

// A simple binary parser
class Parser {
  #buf;
  #offset = 0;

  constructor(buf) {
    this.#buf = buf;
  }

  eof() {
    return this.#offset === this.#buf.length;
  }

  skip(len) {
    this.#offset += len;
    assert(this.#offset <= this.#buf.length);
  }

  readUInt8() {
    const r = this.#buf[this.#offset];
    this.#offset += 1;
    return r;
  }

  readULEB128() {
    let acc = 0n,
      shift = 0n;
    while (true) {
      const byte = this.readUInt8();
      acc |= BigInt(byte & 0x7f) << shift;
      shift += 7n;
      if (byte >> 7 === 0) {
        break;
      }
    }
    return Number(acc);
  }

  readBuffer() {
    const len = this.readULEB128();
    const r = this.#buf.subarray(this.#offset, this.#offset + len);
    this.#offset += len;
    assert(this.#offset <= this.#buf.length);
    return r;
  }

  readString() {
    return new TextDecoder("utf-8", { fatal: true }).decode(this.readBuffer());
  }
}

// Parse the dylink.0 section of a wasm module
function parseDyLink0(buf) {
  const p0 = new Parser(buf);
  // magic, version
  p0.skip(8);
  // section id
  assert(p0.readUInt8() === 0);
  const p1 = new Parser(p0.readBuffer());
  // custom section name
  assert(p1.readString() === "dylink.0");

  const r = { neededSos: [], exportInfo: [], importInfo: [] };
  while (!p1.eof()) {
    const subsection_type = p1.readUInt8();
    const p2 = new Parser(p1.readBuffer());
    switch (subsection_type) {
      case 1: {
        // WASM_DYLINK_MEM_INFO
        r.memSize = p2.readULEB128();
        r.memP2Align = p2.readULEB128();
        r.tableSize = p2.readULEB128();
        r.tableP2Align = p2.readULEB128();
        break;
      }
      case 2: {
        // WASM_DYLINK_NEEDED
        //
        // There may be duplicate entries. Not a big deal to not
        // dedupe, but why not.
        const n = p2.readULEB128();
        const acc = new Set();
        for (let i = 0; i < n; ++i) {
          acc.add(p2.readString());
        }
        r.neededSos = [...acc];
        break;
      }
      case 3: {
        // WASM_DYLINK_EXPORT_INFO
        //
        // Not actually used yet, kept for completeness in case of
        // future usage.
        const n = p2.readULEB128();
        for (let i = 0; i < n; ++i) {
          const name = p2.readString();
          const flags = p2.readULEB128();
          r.exportInfo.push({ name, flags });
        }
        break;
      }
      case 4: {
        // WASM_DYLINK_IMPORT_INFO
        //
        // Same.
        const n = p2.readULEB128();
        for (let i = 0; i < n; ++i) {
          const module = p2.readString();
          const name = p2.readString();
          const flags = p2.readULEB128();
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
  static #poison = new WebAssembly.Global(
    { value: "i32", mutable: false },
    0xffffffff - DyLD.#pageSize
  ).value;

  // When processing exports, skip the following ones since they're
  // generated by wasm-ld.
  static #ldGeneratedExportNames = new Set([
    "_initialize",
    "__wasm_apply_data_relocs",
    "__wasm_apply_global_relocs",
    "__wasm_call_ctors",
  ]);

  // Virtual file descriptors designated for IPC logic and passed to
  // iserv main. uvwasi doesn't support preopening host file
  // descriptors as wasi file descriptors so we designate them and
  // hook certain wasi syscalls on them, so that the pipe file
  // descriptors passed from GHC can be used to communicate with the
  // wasm side.
  static read_fd = 0x7ffffffe;
  static write_fd = 0x7fffffff;

  // The WASI instance to provide wasi imports, shared across all wasm
  // instances
  #wasi;

  // The actual wasi_snapshot_preview1 import object, after hooking
  // the wasi syscalls provided by uvwasi.
  #wasiImport;

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

  // Deduped absolute paths of directories where we lookup .so files
  #rpaths = new Set();

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

  constructor({ args, out_fd, in_fd }) {
    this.#wasi = new WASI({
      version: "preview1",
      args,
      env: { PATH: "", PWD: process.cwd() },
      preopens: { "/": "/" },
    });

    this.#wasiImport = {};

    // https://gitlab.haskell.org/ghc/wasi-libc/-/blob/master/libc-bottom-half/headers/public/wasi/api.h
    for (const k in this.#wasi.wasiImport) {
      switch (k) {
        case "fd_fdstat_get": {
          this.#wasiImport[k] = (fd, retptr0) => {
            switch (fd) {
              case DyLD.read_fd: {
                const fdstat = new DataView(this.#memory.buffer, retptr0, 24);
                fdstat.setUint8(0, 6); // __wasi_filetype_t fs_filetype;
                fdstat.setUint16(2, 0, true); //  __wasi_fdflags_t fs_flags;
                fdstat.setBigUint64(8, (1n << 1n) | (1n << 21n), true); // __wasi_rights_t fs_rights_base;
                fdstat.setBigUint64(16, (1n << 1n) | (1n << 21n), true); // __wasi_rights_t fs_rights_inheriting;
                return 0;
              }
              case DyLD.write_fd: {
                const fdstat = new DataView(this.#memory.buffer, retptr0, 24);
                fdstat.setUint8(0, 6); // __wasi_filetype_t fs_filetype;
                fdstat.setUint16(2, 0, true); //  __wasi_fdflags_t fs_flags;
                fdstat.setBigUint64(8, (1n << 6n) | (1n << 21n), true); // __wasi_rights_t fs_rights_base;
                fdstat.setBigUint64(16, (1n << 1n) | (1n << 21n), true); // __wasi_rights_t fs_rights_inheriting;
                return 0;
              }
              default: {
                return this.#wasi.wasiImport[k](fd, retptr0);
              }
            }
          };
          break;
        }

        case "fd_filestat_get": {
          this.#wasiImport[k] = (fd, retptr0) => {
            switch (fd) {
              case DyLD.read_fd: {
                const filestat = new DataView(this.#memory.buffer, retptr0, 64);
                filestat.setBigUint64(0, 109n, true); // __wasi_device_t dev;
                filestat.setBigUint64(8, BigInt(DyLD.read_fd), true); // __wasi_inode_t ino;
                filestat.setUint8(16, 6); // __wasi_filetype_t filetype;
                filestat.setBigUint64(24, 1n, true); // __wasi_linkcount_t nlink;
                filestat.setBigUint64(32, 0n, true); // __wasi_filesize_t size;
                filestat.setBigUint64(40, 0n, true); // __wasi_timestamp_t atim;
                filestat.setBigUint64(48, 0n, true); // __wasi_timestamp_t mtim;
                filestat.setBigUint64(56, 0n, true); // __wasi_timestamp_t ctim;
                return 0;
              }
              case DyLD.write_fd: {
                const filestat = new DataView(this.#memory.buffer, retptr0, 64);
                filestat.setBigUint64(0, 109n, true); // __wasi_device_t dev;
                filestat.setBigUint64(8, BigInt(DyLD.read_fd), true); // __wasi_inode_t ino;
                filestat.setUint8(16, 6); // __wasi_filetype_t filetype;
                filestat.setBigUint64(24, 1n, true); // __wasi_linkcount_t nlink;
                filestat.setBigUint64(32, 0n, true); // __wasi_filesize_t size;
                filestat.setBigUint64(40, 0n, true); // __wasi_timestamp_t atim;
                filestat.setBigUint64(48, 0n, true); // __wasi_timestamp_t mtim;
                filestat.setBigUint64(56, 0n, true); // __wasi_timestamp_t ctim;
                return 0;
              }
              default: {
                return this.#wasi.wasiImport[k](fd, retptr0);
              }
            }
          };
          break;
        }

        case "fd_read": {
          this.#wasiImport[k] = (fd, iovs, iovs_len, retptr0) => {
            switch (fd) {
              case DyLD.read_fd: {
                assert(iovs_len === 1);
                const iov = new DataView(this.#memory.buffer, iovs, 8);
                const buf = iov.getUint32(0, true),
                  buf_len = iov.getUint32(4, true);
                const bytes_read = fsSync.readSync(
                  in_fd,
                  new Uint8Array(this.#memory.buffer, buf, buf_len)
                );
                new DataView(this.#memory.buffer, retptr0, 4).setUint32(
                  0,
                  bytes_read,
                  true
                );
                return 0;
              }
              default: {
                return this.#wasi.wasiImport[k](fd, iovs, iovs_len, retptr0);
              }
            }
          };
          break;
        }

        case "fd_write": {
          this.#wasiImport[k] = (fd, iovs, iovs_len, retptr0) => {
            switch (fd) {
              case DyLD.write_fd: {
                assert(iovs_len === 1);
                const iov = new DataView(this.#memory.buffer, iovs, 8);
                const buf = iov.getUint32(0, true),
                  buf_len = iov.getUint32(4, true);
                const bytes_written = fsSync.writeSync(
                  out_fd,
                  new Uint8Array(this.#memory.buffer, buf, buf_len)
                );
                new DataView(this.#memory.buffer, retptr0, 4).setUint32(
                  0,
                  bytes_written,
                  true
                );
                return 0;
              }
              default: {
                return this.#wasi.wasiImport[k](fd, iovs, iovs_len, retptr0);
              }
            }
          };
          break;
        }

        default: {
          this.#wasiImport[k] = (...args) => this.#wasi.wasiImport[k](...args);
          break;
        }
      }
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

  // removeLibrarySearchPath is a no-op in ghci. If you have a use
  // case where it's actually needed, I would like to hear..
  addLibrarySearchPath(p) {
    this.#rpaths.add(path.resolve(p));
  }

  // f can be either just soname or an absolute path, will be
  // canonicalized and checked for file existence here. Throws if
  // non-existent.
  async findSystemLibrary(f) {
    if (path.isAbsolute(f)) {
      await fs.access(f, fs.constants.R_OK);
      return f;
    }
    const r = (
      await Promise.allSettled(
        [...this.#rpaths].map(async (p) => {
          const r = path.resolve(p, f);
          await fs.access(r, fs.constants.R_OK);
          return r;
        })
      )
    ).find(({ status }) => status === "fulfilled");
    assert(r, `findSystemLibrary(${f}): not found in ${[...this.#rpaths]}`);
    return r.value;
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
    const soname = path.basename(p);

    if (this.#loadedSos.has(soname)) {
      return [];
    }

    // Do this before loading dependencies to break potential cycles.
    this.#loadedSos.add(soname);

    if (path.isAbsolute(p)) {
      // GHC may attempt to load libghc_tmp_2.so that needs
      // libghc_tmp_1.so in a temporary directory without adding that
      // directory via addLibrarySearchPath
      this.addLibrarySearchPath(path.dirname(p));
    } else {
      p = await this.findSystemLibrary(p);
    }

    const buf = await fs.readFile(p);
    const modp = WebAssembly.compile(buf);
    // Parse dylink.0 from the raw buffer, not via
    // WebAssembly.Module.customSections(). At this point we only care
    // about WASM_DYLINK_NEEDED, but might as well do the rest of the
    // parsing anyway.
    const r = parseDyLink0(buf);
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
        wasi_snapshot_preview1: this.#wasiImport,
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
      assert(tableP2Align === 0);

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
        assert(memP2Align <= Math.log2(DyLD.#pageSize));
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
      // be accessed by JSFFI code snippets.
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

          // For lazy GOT.func entries we can do better than poison:
          // insert a stub in the table, so we at least get an error
          // message that includes the missing function's name, not a
          // mysterious table trap. The function type is Cmm function
          // type as a best effort guess, if there's a type mismatch
          // then call_indirect would trap.
          //
          // Also set a __poison field since we can't compare value
          // against DyLD.#poison.
          this.#gotFunc[name] = new WebAssembly.Global(
            { value: "i32", mutable: true },
            this.#table.grow(
              1,
              new WebAssembly.Function(
                { parameters: [], results: ["i32"] },
                () => {
                  throw new WebAssembly.RuntimeError(
                    `non-existent function ${name}`
                  );
                }
              )
            )
          );
          this.#gotFunc[name].__poison = true;
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
        assert(
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
            assert(this.#gotFunc[k].__poison);
            delete this.#gotFunc[k].__poison;
            this.#table.set(this.#gotFunc[k].value, v);
          }
          continue;
        }

        // It's a GOT.mem entry
        if (v instanceof WebAssembly.Global) {
          const addr = v.value + memory_base;
          if (this.#gotMem[k]) {
            assert(this.#gotMem[k].value === DyLD.#poison);
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
        // https://github.com/llvm/llvm-project/blob/llvmorg-19.1.1/lld/wasm/Writer.cpp#L1430,
        // there's also __wasm_init_memory (not relevant yet, we don't
        // use passive segments) & __wasm_apply_global_relocs but
        // those are included in the start function and should have
        // been called upon instantiation.
        instance.exports.__wasm_apply_data_relocs();

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
    if (this.#gotFunc[sym] && !this.#gotFunc[sym].__poison) {
      return this.#gotFunc[sym].value;
    }
    // Not in GOT.func yet, create the entry on demand
    if (this.exportFuncs[sym]) {
      assert(!this.#gotFunc[sym]);
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

function isMain() {
  return import.meta.filename === process.argv[1];
}

if (isMain()) {
  // sysroot libdir that contains libc.so etc
  const libdir = process.argv[2],
    ghci_so_path = process.argv[3];

  // Inherited pipe file descriptors from GHC
  const out_fd = Number.parseInt(process.argv[4]),
    in_fd = Number.parseInt(process.argv[5]);

  const dyld = new DyLD({
    args: ["dyld.so", DyLD.write_fd, DyLD.read_fd, ...process.argv.slice(6)],
    out_fd,
    in_fd,
  });
  dyld.addLibrarySearchPath(libdir);
  await dyld.loadDLL(ghci_so_path);

  await dyld.exportFuncs.defaultServer();
}
