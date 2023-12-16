#!/usr/bin/env -S node --expose-gc

import fs from "node:fs/promises";
import path from "node:path";
import { WASI } from "wasi";
import { postLink } from "./post-link.mjs";

// The ESM code returned by post-linker doesn't need to be written to
// a temporary file first. It can be directly imported from a
// base64-encoded data URL.

function jsToDataURL(src) {
  return `data:text/javascript;base64,${Buffer.from(src).toString("base64")}`;
}

async function evalJSModuleDefault(src) {
  return (await import(jsToDataURL(src))).default;
}

const wasm_path = path.resolve(process.argv[2]);
const js_path = path.join(
  path.dirname(wasm_path),
  `${path.basename(wasm_path, ".wasm")}.mjs`
);

const wasm_module = await WebAssembly.compile(await fs.readFile(wasm_path));
const js_stub_src = await postLink(wasm_module);
const wasm_import_factory = await evalJSModuleDefault(js_stub_src);

// Yes, you can pass +RTS and other command line flags to Haskell via
// "./test-runner.mjs foo.wasm +RTS ..."
const wasi = new WASI({
  version: "preview1",
  args: process.argv.slice(2),
  env: { PATH: "", PWD: process.cwd() },
  preopens: { "/": "/" },
});

// Poor man's tying-the-knot
let __exports = {};

const wasm_instance = await WebAssembly.instantiate(wasm_module, {
  ghc_wasm_jsffi: wasm_import_factory(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});

// Do this immediately before you _initialize()
Object.assign(__exports, wasm_instance.exports);

// This calls _initialize(). Other wasi implementations may differ,
// always check their doc/src to be sure
wasi.initialize(wasm_instance);

const k = (await import(js_path)).default;
await k(__exports);
