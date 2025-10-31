# The Haskell playground browser test

This directory contains the `playground001` test, which builds a fully
client side Haskell playground in the browser, then runs a
puppeteer-based test to actually interpret a Haskell program in a
headless browser.

## Headless testing

`playground001` is tested in GHC CI. To test it locally, first ensure
you've set up the latest
[`ghc-wasm-meta`](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
toolchain and sourced the `~/.ghc-wasm/env` script, so the right
`node` with the right pre-installed libraries are used. Additionally,
you need to install latest Firefox and:

```sh
export FIREFOX_LAUNCH_OPTS='{"browser":"firefox","executablePath":"/usr/bin/firefox"}'`
```

Or on macOS:

```sh
export FIREFOX_LAUNCH_OPTS='{"browser":"firefox","executablePath":"/Applications/Firefox.app/Contents/MacOS/firefox"}'
```

Without `FIREFOX_LAUNCH_OPTS`, `playground001` is skipped.

It's possible to test against Chrome as well, the
[`playground001.js`](./playground001.js) test driver doesn't assume
anything Firefox-specific, it just takes the
[`puppeteer.launch`](https://pptr.dev/api/puppeteer.puppeteernode.launch)
options as JSON passed via command line.

`playground001` works on latest versions of Firefox/Chrome/Safari.

## Manual testing

The simplest way to build the playground manually and run it in a
browser tab is to test it once with `--only=playground001
--keep-test-files` passed to Hadrian, then you can find the temporary
directory containing [`index.html`](./index.html), `rootfs.tar.zst`
etc, then fire up a dev web server and load it.

Additionally, you can build the playground in tree without invoking
the GHC testsuite. Just build GHC with the wasm target first, then
copy `utils/jsffi/*.mjs` here and run
[`./playground001.sh`](./playground001.sh) script. You need to set
`TEST_CC` to the path of `wasm32-wasi-clang` and `TEST_HC` to the path
of `wasm32-wasi-ghc`, that's it.

## Customized Haskell playground

You may want to build a customized Haskell playground that uses GHC
API to interpret Haskell code with custom packages, here are some tips
to get started:

- Read the code in this directory and figure out how `playground001`
  itself works.
- [`./playground001.sh`](./playground001.sh) can be used as a basis to
  write your own build/test script.

You don't need to read the full `dyld.mjs` script. The user-facing
things that are relevant to the playground use case are:

- `export class DyLDBrowserHost`: it is the `rpc` object required when
  calling `main`. You need to pass `stdout`/`stderr` callbacks to
  write each line of stdout/stderr, as well as a `rootfs` object that
  represents an in-memory vfs containing the shared libraries to load.
- `export async function main`: it eventually returns a `DyLD` object
  that can be used like `await
  dyld.exportFuncs.myExportedHaskellFunc(js_foo, js_bar)` to invoke
  your exported Haskell function.

Check the source code of [`index.html`](./index.html) and cross
reference [`playground001.hs`](./playground001.hs) for the example of
how they are used.

The `rootfs` object is a
[`PreopenDirectory`](https://github.com/haskell-wasm/browser_wasi_shim/blob/master/src/fs_mem.ts)
object in the
[`browser_wasi_shim`](https://github.com/haskell-wasm/browser_wasi_shim)
library. The Haskell playground needs a complex vfs containing many
files (shared libraries, interface files, package databases, etc), so
to speed things up, the whole vfs is compressed into a
`rootfs.tar.zst` archive, then that archive is extracted using
[`bsdtar-wasm`](https://github.com/haskell-wasm/bsdtar-wasm).

You don't need to read the source code of `browser_wasi_shim`; you can
simply paste and adapt the relevant code snippet in
[`index.html`](./index.html) to create the right `rootfs` object from
a tarball.

The main concern is what do you need to pack into `rootfs.tar.zst`.
For `playground001`, it contains:

- `/tmp/clib`: the C/C++ shared libraries
- `/tmp/hslib/lib`: the GHC libdir
- `/tmp/libplayground001.so`: the main shared library to start loading
  that exports `myMain`

You can read [`./playground001.sh`](./playground001.sh) to figure out
the details of how I prepare `rootfs.tar.zst` and trim unneeded files
to minimize the tarball size.

There are multiple possible ways to install third-party packages in
the playground:

- Start from a `wasm32-wasi-ghc` installation, use `wasm32-wasi-cabal
  v1-install --global` to install everything to the global package
  database. In theory this is the simplest way, though I haven't tried
  it myself and it's unclear to what extent do `v1` commands work
  these days.
- Use default nix-style installation, then package the cabal store and
  `dist-newstyle` directories into `rootfs.tar.zst`, and pass the
  right package database flags when calling GHC API.

Note that cabal built packages are not relocatable! So things will
break if you build them at a host location and then package into a
different absolute path into the rootfs, keep this in mind.

If you have any difficulties, you're welcome to the [Haskell
Wasm](https://matrix.to/#/#haskell.wasm:matrix.org) matrix room for
community support.
