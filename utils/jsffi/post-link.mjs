#!/usr/bin/env -S node

// This is the post-linker program that processes a wasm module with
// ghc_wasm_jsffi custom section and outputs an ESM module that
// exports a function to generate the ghc_wasm_jsffi wasm imports. It
// has a simple CLI interface: "./post-link.mjs -i foo.wasm -o
// foo.js", as well as an exported postLink function that takes a
// WebAssembly.Module object and returns the ESM module content.

import fs from "node:fs/promises";
import path from "node:path";
import util from "node:util";

// Each record in the ghc_wasm_jsffi custom section are 3
// NUL-terminated strings: name, binder, body. We try to parse the
// body as an expression and fallback to statements, and return the
// completely parsed arrow function source.
export function parseRecord([name, binder, body]) {
  for (const src of [`${binder} => (${body})`, `${binder} => {${body}}`]) {
    try {
      new Function(`return ${src};`);
      return src;
    } catch (_) {}
  }
  throw new Error(`parseRecord ${name} ${binder} ${body}`);
}

// Parse ghc_wasm_jsffi custom sections in a WebAssembly.Module object
// and return an array of records.
export function parseSections(mod) {
  const recs = [];
  const dec = new TextDecoder("utf-8", { fatal: true });
  const importNames = new Set(
    WebAssembly.Module.imports(mod)
      .filter((i) => i.module === "ghc_wasm_jsffi")
      .map((i) => i.name)
  );
  for (const buf of WebAssembly.Module.customSections(mod, "ghc_wasm_jsffi")) {
    const ba = new Uint8Array(buf);
    let strs = [];
    for (let l = 0, r; l < ba.length; l = r + 1) {
      r = ba.indexOf(0, l);
      strs.push(dec.decode(ba.subarray(l, r)));
      if (strs.length === 3) {
        if (importNames.has(strs[0])) {
          recs.push(strs);
        }
        strs = [];
      }
    }
  }
  return recs;
}

export async function postLink(mod) {
  let src = (
    await fs.readFile(path.join(import.meta.dirname, "prelude.mjs"), {
      encoding: "utf-8",
    })
  ).replaceAll("export ", ""); // we only use it as code template, don't export stuff

  // Keep this in sync with dyld.mjs!
  src = `${src}\nexport default (__exports) => {`;
  src = `${src}\nconst __ghc_wasm_jsffi_jsval_manager = new JSValManager();`;
  src = `${src}\nconst __ghc_wasm_jsffi_finalization_registry = globalThis.FinalizationRegistry ? new FinalizationRegistry(sp => __exports.rts_freeStablePtr(sp)) : { register: () => {}, unregister: () => true };`;
  src = `${src}\nreturn {`;
  src = `${src}\nnewJSVal: (v) => __ghc_wasm_jsffi_jsval_manager.newJSVal(v),`;
  src = `${src}\ngetJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.getJSVal(k),`;
  src = `${src}\nfreeJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.freeJSVal(k),`;
  src = `${src}\nscheduleWork: () => setImmediate(__exports.rts_schedulerLoop),`;
  for (const rec of parseSections(mod)) {
    src = `${src}\n${rec[0]}: ${parseRecord(rec)},`;
  }
  return `${src}\n};\n};\n`;
}

function isMain() {
  return import.meta.filename === process.argv[1];
}

async function main() {
  const { input, output } = util.parseArgs({
    options: {
      input: {
        type: "string",
        short: "i",
      },
      output: {
        type: "string",
        short: "o",
      },
    },
  }).values;

  await fs.writeFile(
    output,
    await postLink(await WebAssembly.compile(await fs.readFile(input)))
  );
}

if (isMain()) {
  await main();
}
