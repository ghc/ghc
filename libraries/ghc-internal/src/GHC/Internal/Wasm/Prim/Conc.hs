{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Internal.Wasm.Prim.Conc (
  threadDelay
) where

import GHC.Internal.Base
import GHC.Internal.IO.Unsafe
import GHC.Internal.Stable

{-

Note [threadDelay on wasm]
~~~~~~~~~~~~~~~~~~~~~~~~~~

When you compile Haskell to wasm32-wasi and use wasmtime to run it,
threadDelay works, because the poll_oneoff syscall is properly
implemented, so the awaitEvent() logic in the single-threaded RTS
works. However, given constraints of the browser environment, wasi
implementations in pure JavaScript tend to not implement poll_oneoff
at all, so we can't pretend the old posix style polling works at all.

Since we can do JSFFI async imports now, it's super easy to implement
threadDelay using setTimeout() in JavaScript, and an implementation
has indeed been added in GHC.Internal.Wasm.Prim.Conc.Internal. But how do we
make the user-facing GHC.Internal.Conc.IO.threadDelay switch to it in browser
environments and fall back to the stg_delay# implementation otherwise?

A first step is the isJSFFIUsed magic boolean which fetches a C
boolean global variable. If JSFFI is used, then JSFFI.o would be
included by wasm-ld, and the ctor there will handle RTS initialization
as well as setting that boolean to true. This all takes place before
actual Haskell evaluation, so you can use isJSFFIUsed in Haskell to
observe whether the current project makes use of JSFFI (aka targets a
JavaScript host like browsers) at all.

However, merely referring to the right threadDelay implementation
based on the isJSFFIUsed guard is not enough! wasm-ld will
transitively include the version that uses JSFFI in the final module
as well as a bunch of other JSFFI related stuff, so the resulting wasm
module will now unconditionally contain non-wasi imports :( This
violates our principle of "don't pay for JavaScript when you don't use
it". So we need a bit of dependency injection here.

We use a global stable pointer variable in C to point to the actual
JSFFI-based threadDelay function closure. When JSFFI is not used, it
defaults to NULL, but it's okay since it'll not be used at runtime.
When JSFFI is used, the ctor in JSFFI.o will inject the right
closure's stable pointer into that variable, which will be
dereferenced here the first time threadDelay is called. This way, we
can ensure the JSFFI based threadDelay is used in browsers, while not
contaminating standalone wasm32-wasi modules with JSFFI stuff.

And yes, it's safe to free the stable pointer here, once the function
closure has been fetched. This allows the CAF to be garbage collected
when user code no longer uses threadDelay.

-}

{-# OPAQUE threadDelay #-}
threadDelay :: Int -> IO ()
threadDelay = unsafeDupablePerformIO $ do
  f <- deRefStablePtr rts_threadDelay_sp
  freeStablePtr rts_threadDelay_sp
  pure f

foreign import ccall unsafe "rts_threadDelay_sp"
  rts_threadDelay_sp :: StablePtr (Int -> IO ())
