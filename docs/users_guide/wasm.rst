.. _wasm:

Using the GHC WebAssembly backend
=================================

.. _wasm-clarify:

What does the WebAssembly “backend” mean
----------------------------------------

In order to compile Haskell to wasm, you need a custom GHC build that
targets wasm. There isn't a GHC option which allows you to use a
stock GHC installed via ``ghcup`` or ``stack`` to generate wasm. That’s
because GHC is still a single-target compiler, so each GHC build is only
capable of compiling code that runs on a single architecture and operating
system.

So, the term GHC wasm backend isn’t the same sense as the
unregisterised/LLVM/NCG backends. It merely describes GHC’s support as a
cross compiler that targets wasm, and more specifically,
``wasm32-wasi``.

The generated wasm module makes use of a few post-MVP extensions that
are supported by default in latest releases of
Chrome/Firefox/Safari/`wasmtime <https://wasmtime.dev>`__. The wasm
module uses `WASI <https://wasi.dev>`__ as the system call layer, so
it’s supported by any wasm engine that implements WASI (including
browsers, which can provide the WASI layer via JavaScript).

.. _wasm-setup:

Setting up the GHC wasm backend
-------------------------------

The wasm backend is still a tech preview and not included in the
official bindists yet. If you are using x86_64-linux, you can follow the
“getting started” subsections in
`ghc-wasm-meta <https://gitlab.haskell.org/ghc/ghc-wasm-meta>`__ to
quickly set up the GHC wasm backend using the nightly artifacts.

It’s also possible to build the GHC wasm backend manually, if your host
system is one of {x86_64,aarch64}-{linux,darwin}. Refer to the
``ghc-wasm-meta`` readme for detailed instructions.

.. _wasm-compile:

Using the GHC wasm backend to compile & link code
-------------------------------------------------

Once the GHC wasm backend is set up, you can use it to compile and link
code. The compiler executables follow the cross compiler linking
convention, so you need to call ``wasm32-wasi-ghc``,
``wasm32-wasi-ghc-pkg`` and ``wasm32-wasi-hsc2hs`` instead of ``ghc``,
``ghc-pkg`` and ``hsc2hs``.

You can also use the ``--with-compiler=``, ``--with-hc-pkg=`` and
``--with-hsc2hs`` flags of ``cabal`` to build cabal projects. The
``wasm32-wasi-cabal`` wrapper script set up by the ``ghc-wasm-meta``
installer does this automatically for you, but using flags manually also
works with stock ``cabal`` installations. When ``cabal`` builds an
executable component, that executable will be built as a wasm module,
and you can use ``cabal list-bin exe:foo`` to find the wasm module’s
location in the build directory.

.. _wasm-run:

Running the GHC wasm backend’s output
-------------------------------------

Once you have a wasm module, you can run it with a dedicated wasm engine
like ``wasmtime``, or inside the browsers.

To run it with ``wasmtime``, you can simply do:

.. code:: sh

   $ wasmtime run foo.wasm

Just like native executables, you can pass command line arguments, and
also RTS options, as long as it’s built with ``-rtsopts``:

.. code:: sh

   $ wasmtime run foo.wasm --bar +RTS --nonmoving-gc -RTS

You can also mount some host directory into it:

.. code:: sh

   $ wasmtime run --mapdir /::$PWD foo.wasm

As long as the filesystem capability is provided, in addition to
filesystem I/O in Haskell code, you can use the RTS eventlog and
profiling functionality, then inspect the report files:

.. code:: sh

   $ wasmtime run --mapdir /::$PWD foo.wasm +RTS -hc -l -RTS

.. _wasm-ghci:

Using GHCi with the wasm backend
--------------------------------

The GHC wasm backend supports the GHCi feature via a nodejs ``dyld``
script that provides RTS linker functionality and bootstraps the
external interpreter. This script is installed into the
``wasm32-wasi-ghc --print-libdir`` location and will be automatically
launched by GHC when evaluating Template Haskell splices or starting
GHCi, though you do need a recent ``node`` available in ``PATH``.

You can launch a GHCi session with ``wasm32-wasi-ghci`` or
``wasm32-wasi-ghc --interactive``. All existing GHCi features work with
wasm, including the GHCi debugger. You can also use JavaScript FFI
detailed in the next section; by default, the JavaScript FFI has access
to the nodejs global namespace since it runs in nodejs after all.

Additionally, the wasm backend’s GHCi supports the browser mode to
allow live-coding the frontend using GHCi. This requires the `ws
<https://www.npmjs.com/package/ws>`__ library to be installed and
passed to the ``dyld`` script via the `NODE_PATH
<https://nodejs.org/api/modules.html>`__ environment variable. Suppose
your wasm backend installation is supplied by ``ghc-wasm-meta``, the
right ``node`` installation and ``NODE_PATH`` with all the optional
npm dependencies are automatically provided out of the box.

To get started with the browser mode, set the ``GHCI_BROWSER``
environment variable:

::

   $ export GHCI_BROWSER=1
   $ wasm32-wasi-ghc --interactive
   GHCi, version 9.13.20250320: https://www.haskell.org/ghc/  :? for help
   Open http://127.0.0.1:37517/main.html or import http://127.0.0.1:37517/main.js to boot ghci

At this point, the GHCi session is frozen. The ``dyld`` script acts as a
broker between the host GHC process and the in-browser external
interpreter; it starts an HTTP server that serves ``main.js``, an ES6
module that connects back to the HTTP server and finishes the rest of
external interpreter bootstrap process. The ``dyld`` HTTP server allows
`CORS <https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/CORS>`__
requests from any origin, meaning it’s possible to use F12 devtools
console to import ``main.js`` and use GHCi to debug other websites, but
the simplest way to get started is simply opening ``main.html`` in the
browser.

After a few seconds, the GHCi session shall be unfrozen and available
for use. There are a few important points to keep in mind when using the
GHCi browser mode:

- All the code runs in the browser, and the JavaScript FFI only has
  access to the browser global namespace, not the nodejs one. There is
  no escape hatch to invoke any function on the nodejs side.
- Likewise, the browser side has no access to the host filesystem. You
  can use ``:load`` etc in the GHCi prompt to load modules just fine,
  but attempting to do a ``readFile`` will fail, there is no file for
  you to open in the browser-side virtual filesystem.
- Template Haskell splices are also evaluated in the browser. If your
  splices require side-effects like reading files then they will fail to
  evaluate; to workaround it, compile modules containing such splices to
  object code first and load object code instead.
- By default, ``stdout``/``stderr`` doesn’t write back to the GHCi
  prompt, the messages are written to the F12 devtools console in a
  line-buffered manner.

There are other options that can be specified as environment variables:

- ``GHCI_BROWSER_HOST``: specify the host address that the ``dyld`` HTTP
  server should bind to, supports IPv4/IPv6. Defaults to ``127.0.0.1``.
  Be careful when changing it and exposing the ``dyld`` HTTP server to
  other networks, some endpoints of the server allow downloading files
  from the host filesystem!
- ``GHCI_BROWSER_PORT``: specify the port that the ``dyld`` HTTP server
  should listen on. Defaults to a random idle port.
- ``GHCI_BROWSER_REDIRECT_WASI_CONSOLE``: if set to ``1``, the wasi
  stdout/stderr output messages are redirected back to the host GHCi
  terminal instead of outputing to the F12 devtools console. The main
  intended use case is mobile browsers which likely don’t have F12
  devtools readily available. Also note that this only redirects wasi
  console messages, not ``console.log`` invocations in the browser.

For testing purposes, there is also support for using
`Puppeteer <https://pptr.dev>`__ or
`Playwright <https://playwright.dev>`__ to automatically launch a
headless browser and load ``main.html``. Like ``ws``, the relevant npm
dependencies need to be supplied via ``NODE_PATH``, either
``puppeteer``/``puppeteer-core`` or ``playwright``/``playwright-core``,
then the following options can be used:

- ``GHCI_BROWSER_PUPPETEER_LAUNCH_OPTS``: JSON-formatted arguments to
  be passed to `puppeteer.launch()
  <https://pptr.dev/api/puppeteer.puppeteernode.launch>`__.
- ``GHCI_BROWSER_PLAYWRIGHT_BROWSER_TYPE``: one of
  ``chromium``/``firefox``/``webkit``, the kind of browser to be
  launched by ``playwright``.
- ``GHCI_BROWSER_PLAYWRIGHT_LAUNCH_OPTS``: optional, JSON-formatted
  arguments to be passed to `browser.launch()
  <https://playwright.dev/docs/api/class-browsertype#browser-type-launch>`__.

.. _wasm-jsffi:

JavaScript FFI in the wasm backend
----------------------------------

The GHC wasm backend supports the JavaScript FFI feature. For Haskell
projects that are meant to be run in JavaScript environments like
browsers or nodejs, the JavaScript FFI enables:

-  Calling JavaScript from Haskell via foreign imports and vice versa
   via foreign exports.
-  Representing JavaScript values as first-class garbage collected
   Haskell values on the Haskell heap.
-  Blocking for asynchronous JavaScript computation in a single Haskell
   thread without blocking the entire runtime.
-  Not paying for JavaScript when not using it, the same toolchain still
   generates self-contained ``wasm32-wasi`` modules by default.

The JavaScript FFI as implemented in the GHC wasm backend is pioneered
by the asterius project and heavily inspired by GHCJS, the predecessor
of the GHC JavaScript backend. Despite some similarities, it still
differs from the GHC JavaScript backend’s implementation significantly.
The rest of this guide is a canonical reference for the GHC wasm
backend’s JavaScript FFI, which we’ll now abbreviate as JSFFI.

.. _wasm-jsffi-types:

Marshalable types and ``JSVal``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

JSFFI supports all boxed marshalable foreign types in C FFI:

-  ``Bool``
-  ``Char``
-  ``Int`` / ``Word``
-  ``Int8`` / ``Int16`` / ``Int32`` / ``Int64``
-  ``Word8`` / ``Word16`` / ``Word32`` / ``Word64``
-  ``Ptr`` / ``FunPtr`` / ``StablePtr``
-  ``Float`` / ``Double``

The above types and their ``newtype``\ s can be used as argument/result
types in JSFFI. Some caveats to keep in mind:

-  ``Bool`` is marshaled to ``0`` / ``1`` instead of ``false`` /
   ``true`` in JavaScript. This is affected by implementation details of
   JSFFI, which is layered on top of C FFI and shares some
   characteristics of C FFI. It should be fine in most cases, since
   implicit conversion to ``boolean`` happens when it’s used as a
   boolean. It’s also fine to pass a JavaScript ``boolean`` into
   Haskell, since it’ll be implicitly converted to a number first.
-  Likewise, ``Char`` is marshaled to 32-bit integer that represents its
   Unicode code point. Do not pass a single character JavaScript
   ``string`` as ``Char``, since implicit conversion to number results
   in ``NaN``! If you absolutely need to use ``Char`` as a JSFFI
   argument/result type, you’re in charge of handling ``Char``\ s as
   code points. Most likely you only need to marshal between Haskell
   ``String`` or ``Text`` and JavaScript ``string``\ s, for which there
   already exist conversion functions.
-  64-bit integer types are marshaled to JavaScript ``bigint``\ s. In
   JavaScript, mixing ``bigint`` and regular numbers in arithmetic
   results in type errors, so keep this in mind. As for ``Int`` /
   ``Word``, they are 32-bit since the GHC wasm backend is based on
   ``wasm32`` .
-  JSFFI doesn’t support unboxed foreign types like ``Int#``,
   ``ByteArray#``, etc, even when ``UnliftedFFITypes`` is enabled.

In addition to the above types, JSFFI supports the ``JSVal`` type and
its ``newtype``\ s as argument/result types. ``JSVal`` is defined in
``GHC.Wasm.Prim`` in ``ghc-experimental``, which represents an opaque
reference to a JavaScript value.

``JSVal``\ s are first-class Haskell values on the Haskell heap. You can
get them via foreign import results or foreign export arguments, store
them in Haskell data structures and pass them between
Haskell/JavaScript. They are garbage-collected by the GHC RTS:

-  There can be multiple ``JSVal``\ s that point to the same JavaScript
   value. As long as there’s at least one ``JSVal`` still alive on the
   Haskell heap, that JavaScript value will still be alive on the
   JavaScript heap.
-  If there’s no longer any live ``JSVal`` that points to the JavaScript
   value, then after Haskell garbage collection, the runtime no longer
   retain any reference to it, allowing the JavaScript runtime to
   eventually garbage collect it as well.

In addition to garbage collection, ``GHC.Wasm.Prim`` also exports
``freeJSVal :: JSVal -> IO ()``, allowing the user to drop the
JavaScript reference from the runtime eagerly. You’re encouraged to make
use of ``freeJSVal`` when you’re sure about a ``JSVal``\ ’s lifetime,
especially for the temporary ``JSVal``\ s. This will help reducing the
memory footprint at runtime.

Note that ``freeJSVal`` is idempotent and it’s safe to call it more
than once. After it’s called, any subsequent usage of that ``JSVal``
by passing to the JavaScript side results in a runtime panic.

.. _wasm-jsffi-import:

Foreign imports
~~~~~~~~~~~~~~~

One can embed a JavaScript code snippet in a foreign import declaration
and call that piece of JavaScript code by calling the foreign import
function:

.. code:: haskell

   import GHC.Wasm.Prim

   foreign import javascript unsafe "console.log($1)"
     js_print :: JSString -> IO ()

   foreign import javascript unsafe "typeof $1 === 'object'"
     js_is_obj :: JSVal -> Bool

   foreign import javascript unsafe "let acc = 1; for (let i = 1; i <= $1; ++i) acc *= i; return acc;"
     js_fac :: Word -> Word

A JSFFI import code snippet can be either a single JavaScript expression
or a series of JavaScript statements as function body, in which case you
can use ``return`` to return the import result value. The import code
snippet has access to:

-  The import argument values, bound to arguments ``$1``, ``$2``, etc.
-  The ``__export`` binding, which contain all wasm module exports. For
   instance, you could use ``__exports.memory`` to access the
   ``WebAssembly.Memory`` object and use it to copy blobs between the
   Haskell/JavaScript side. The ``memory`` export exists by default.
-  The full Web API that exists in the JavaScript global scope.

There are two kinds of JSFFI imports: synchronous/asynchronous imports.
``unsafe`` indicates synchronous imports, which has the following
caveats:

- The calling thread as well as the entire runtime blocks on waiting for
  the import result.
- If the JavaScript code throws, the runtime crashes with the same
  error. A JavaScript exception cannot be handled as a Haskell exception
  here, so you need to use a JavaScript ``catch`` explicitly shall the
  need arise.
- Unlike ``unsafe`` C imports, re-entrance is actually supported, the
  imported JavaScript code can call into Haskell again, provided that
  Haskell function is exported as a synchronous one.

When a JSFFI import is marked as ``safe`` / ``interruptible`` or lacks
safety annotation, then it’s treated as an asynchronous import. The
asynchronous JSFFI imports combine the Haskell concurrency model and the
JavaScript event loop, allowing Haskell code to work with async
JavaScript computation without blocking the entire runtime.

.. code:: haskell

   import Control.Exception

   foreign import javascript safe "new Promise(res => setTimeout(res, $1))"
     js_sleep :: Int -> IO ()

   sleep :: Int -> IO ()
   sleep t = evaluate =<< js_sleep t

   foreign import javascript safe "const r = await fetch($1); return r.text();"
     js_fetch :: JSString -> IO JSString

Asynchronous import code is wrapped in async JavaScript functions,
therefore ``await`` is also supported. Async JavaScript functions always
return ``Promise``\ s, and you can also explicitly create and return a
``Promise`` that resolves to the final result of the async computation.

When an asynchronous JSFFI import is called, the Haskell function
returns immediately once the async JavaScript function returns a
``Promise``. The value returned by the Haskell function is a thunk. When
the thunk is evaluated later, the evaluating thread is suspended by the
runtime, and resumed when the ``Promise`` actually resolves or rejects.

Compared to synchronous JSFFI imports, asynchronous JSFFI imports have
the following notable pros/cons:

- Waiting for the result only blocks a single Haskell thread, other
  threads can still make progress and garbage collection may still
  happen.
- If the ``Promise`` rejects, Haskell code can catch JavaScript errors
  as ``JSException``\ s.
- It has higher overhead than synchronous JSFFI imports.

Using thunks to encapsulate ``Promise`` result allows cheaper
concurrency without even needing to fork Haskell threads just for
waiting for a bunch of async calls to return. Just like lazy I/O, the
convenience comes with caveat, you need to take some care to force the
result thunk before closing the underlying resource. And even if the
result type is ``()``, it’s still a thunk that needs to be explicitly
forced to ensure the ``Promise`` has actually resolved, so you likely
need to write a worker/wrapper function pair for cases like ``sleep``.

There’s also a special kind of JSFFI import that allow converting a
callable ``JSVal`` to a Haskell function:

.. code:: haskell

   type Logger = JSString -> IO ()

   type JSFunction = JSVal

   foreign import javascript unsafe "s => console.log(s)"
     js_logger :: JSFunction

   foreign import javascript unsafe "dynamic"
     js_logger_to_hs :: JSFunction -> Logger

Much like ``foreign import ccall "dynamic"`` which wraps a C function
pointer as a Haskell function, ``foreign import javascript "dynamic"``
wraps a ``JSVal`` that represent a JavaScript function as a Haskell
function. The returned Haskell function retains the reference to that
``JSVal``, and the ``unsafe`` / ``safe`` annotation indicates whether
that JavaScript function is synchronous or asynchronous.

Of course, without ``foreign import javascript "dynamic"``, one could
still easily implement similar functionality:

.. code:: haskell

   foreign import javascript unsafe "$1($2)"
     js_logger_to_hs :: JSFunction -> JSString -> IO ()

And that’s how it’s implemented under the hood. It’s handled as a JSFFI
import with an auto-generated code snippet that calls the first
argument, passing the rest of arguments.

.. _wasm-jsffi-export:

Foreign exports
~~~~~~~~~~~~~~~

One can use ``foreign export javascript`` to export a top-level Haskell
binding as a wasm module export which can be called in JavaScript:

.. code:: haskell

   foreign export javascript "my_fib"
     fib :: Word -> Word

Give ``fib :: Word -> Word``, the above declaration exports ``fib`` as
``my_fib``. It is a wasm module export function without any JavaScript
wrapper, and as long as the wasm instance is properly initialized, you
can call ``await instance.exports.my_fib(10)`` to invoke the exported
Haskell function and get the result.

JSFFI exports are asynchronous by default. Calling an async export
return a ``Promise`` in JavaScript that needs to be ``await``\ ed for
the real result. If the Haskell function throws, the ``Promise`` is
rejected with a ``WebAssembly.RuntimeError``, and the ``message`` field
contains a JavaScript string of the Haskell exception.

Additionally, sync exports are also supported by using ``"my_fib sync"``
instead of ``"my_fib"``. With ``sync`` added alongside export function
name, the JavaScript function would return the result synchronously. For
the time being, sync exports don’t support propagating uncaught Haskell
exception to a JavaScript exception at the call site yet.

Above is the static flavor of JSFFI exports. It’s also possible to
export a dynamically created Haskell function closure as a JavaScript
function and obtain its ``JSVal``:

.. code:: haskell

   type BinOp a = a -> a -> a

   foreign import javascript "wrapper"
     js_func_from_hs :: BinOp Int -> IO JSVal

This is also much like ``foreign import ccall "wrapper"``, which wraps a
Haskell function closure as a C function pointer. Note that ``unsafe`` /
``safe`` annotation is ignored here, since the ``JSVal`` that represent
the exported function is always returned synchronously. Likewise, you
can use ``"wrapper sync"`` instead of ``"wrapper"`` to indicate the
returned JavaScript function is sync instead of async.

The ``JSVal`` callbacks created by dynamic JSFFI exports can be passed
to the rest of JavaScript world to be invoked later. But wait, didn’t we
say earlier that ``JSVal``\ s are garbage collected? Isn’t a
use-after-free trap waiting ahead of the road, when the ``JSVal`` is
collected in Haskell but the JavaScript callback is invoked later?

So, normal ``JSVal``\ s created by JSFFI import results or JSFFI export
arguments only manage a single kind of resource: the JavaScript value it
refers to. But ``JSVal``\ s created by dynamic JSFFI exports manage two
kinds of resources: the JavaScript callback it refers to, as well as a
stable pointer that retains the Haskell function closure. If this
``JSVal`` is garbage collected, the Haskell runtime no longer retains
the JavaScript callback, but the JavaScript side may still hold that
callback and intends to call it later, so the Haskell function closure
is still retained by default.

Still, the runtime can gradually drop these retainers by using
``FinalizationRegistry`` to invoke the finalizers to free the underlying
stable pointers once the JavaScript callbacks are recycled.

One last corner case is cyclic reference between the two heaps: if a
JavaScript callback is retained only by ``JSVal`` and that ``JSVal`` is
retained only by a Haskell function closure that gets exported, this
creates a cyclic reference that can’t be automatically recycled. This is
a fundamental limit of the GHC wasm backend today since the Haskell heap
lives in the linear memory, distinct from the host JavaScript heap, and
coordination between two heaps is always a non-trivial challenge.
However, one can still use ``freeJSVal`` to break the cycle. When
``freeJSVal`` is applied to a ``JSVal`` that represents a JavaScript
callback created by a dynamic JSFFI export, both kinds of resources are
freed at once: the JavaScript callback, as well as the Haskell function
closure.

.. _wasm-jsffi-flag:

Detect whether JSFFI is being used
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you’re writing a Haskell library, you may want to detect whether the
final linked module involves JSFFI logic, or is it still a
self-contained ``wasm32-wasi`` module. One obvious reason is ``wasi``
implementations in JavaScript environments are often incomplete and lack
certain features (e.g. the ``poll_oneoff`` syscall), so it makes sense
to dispatch code.

``GHC.Wasm.Prim`` exports ``isJSFFIUsed :: Bool`` that can be used for
this purpose. As long as there’s a single JSFFI import/export or
anything involving ``JSVal`` linked into the final wasm module, it
will be ``True``. It is ``False`` if and only if the user code has
absolutely no transitive dependency on anything related to JSFFI, in
which case the linked wasm module will be a self-contained
``wasm32-wasi`` module. If something compiles fine with the GHC wasm
backend before JSFFI feature is merged, but ``isJSFFIUsed`` is still
``True``, then it’s definitely a bug.

.. _wasm-jsffi-async-exception:

Interaction with async exception
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a thread is blocked waiting for an async JSFFI import call to
return, it can be interrupted by a Haskell async exception, with some
caveats:

-  The async exception would not magically cancel the ``Promise``. In
   general, JavaScript ``Promise``\ s aren’t cancelable anyway. There do
   exist third-party ``Promise`` libraries to provide a “cancel”
   interface, which only guarantees all ``.then()`` continuations
   registered on that ``Promise`` will no longer be invoked. For
   simplicity of implementation, we aren’t using those for the time
   being.
-  Normally, ``throwTo`` would block until the async exception has been
   delivered. In the case of JSFFI, ``throwTo`` would always return
   successfully immediately, while the target thread is still left in a
   suspended state. The target thread will only be waken up when the
   ``Promise`` actually resolves or rejects, though the ``Promise``
   result will be discarded at that point.

The current way async exceptions are handled in JSFFI is subject to
change though. Ideally, once the exception is delivered, the target
thread can be waken up immediately and continue execution, and the
pending ``Promise`` will drop reference to that thread and no longer
invoke any continuations.

.. _wasm-jsffi-cffi:

Interaction with C FFI
~~~~~~~~~~~~~~~~~~~~~~

User code can take advantage of JSFFI and C FFI together, and make use
of third party C/C++ code as long as they work on ``wasm32-wasi``.
However, there is an important limitation to keep in mind when it comes
to the interaction between JSFFI and C FFI:

A Haskell thread cannot force an async JSFFI import thunk when it
represents a Haskell function exported via C FFI. Doing so would throw
``WouldBlockException``.

For example, suppose we’re using the Haskell bindings of a certain C
library, and some of the C functions expect callers to pass a C function
pointer as the callback argument. Yes, we can use
``foreign import ccall "wrapper"`` to wrap a Haskell function closure
and pass it to that C function. The wrapped Haskell function can even
call sync JSFFI imports, but it cannot call an async JSFFI import and
block on the result.

The other direction is fine. Regardless of whether a C import is
``unsafe`` or ``safe``, it can be called in a Haskell thread that
represents a Haskell function exported to JavaScript. Do keep in mind
that we’re using the single-threaded runtime at the moment, so other
than supporting re-entrancy, ``safe`` C calls don’t offer extra
advantage than ``unsafe``.

As mentioned before, JSFFI is layered on top of C FFI under the hood,
and they share the same C symbol namespace. In most cases, the JSFFI
related symbols are auto-generated so they can’t collide, but for each
``foreign export javascript "my_func"``, there will be a ``my_func``
externally visible C symbol, so you need to take a bit of care not to
duplicate symbol with the C side.

.. _wasm-jsffi-jsapi:

The JavaScript API
~~~~~~~~~~~~~~~~~~

When linking a wasm module that makes use of JSFFI, correct link-time
arguments must be passed to GHC and this needs to be adjusted on a
per-project basis:

.. code:: haskell

   ghc -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=my_func

Why?

Consider the case ``ghc hello.hs`` where ``hello.hs`` is just a good old
``main = putStrLn "hello world"``. By default, ghc exports ``main`` as a
C function, compiles and links a little C stub file that contain the
actual ``main`` that initializes the runtime and call the Haskell
``main``, and ``clang`` would link everything as a ``wasm32-wasi``
command module.

Conceptually, a ``wasm32-wasi`` command module is like an executable,
with a single entry point and meant to be invoked only once and then
exits. But certain link-time arguments can tell ``clang`` to target a
``wasm32-wasi`` reactor module instead. A reactor module is a bit like a
shared library: it has internal functions as well as user-defined entry
points, and once it’s initialized, the entry points can be called as
many times as the user wants.

Given the nature of JSFFI, if a project uses JSFFI, then it surely is
meant to target a ``wasm32-wasi`` reactor module. And there’s no
sensible default entry point, not even ``main``, you need to explicitly
pass the export names you need via link-time arguments, otherwise those
exports will be absent from the resulting wasm module due to linker dead
code elimination.

Now, suppose a wasm module has already been linked and it makes use of
JSFFI. This wasm module will contain ``ghc_wasm_jsffi`` custom
sections, and the section payloads include information like JSFFI
import code snippets and function arities. The next step is calling a
small post-linker script, which will parse the wasm module and emit a
JavaScript module. The `post-linker
<https://gitlab.haskell.org/ghc/ghc/-/blob/master/utils/jsffi/post-link.mjs>`_
is written in nodejs, though the resulting JavaScript module has
nothing nodejs specific and work in browsers as well.

.. code:: sh

   $ $(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i hello.wasm -o hello.js

The generated JavaScript module contains a default export which is a
function. The function takes an ``__exports`` argument and generates the
``ghc_wasm_jsffi`` wasm imports. There is knot-tying going on here: only
after a wasm module is instantiated, you can access its exports, but the
``ghc_wasm_jsffi`` imports required for instantiation need access to the
exports! JavaScript is not a lazy language, but we can achieve
knot-tying less elegantly by using mutation:

.. code:: javascript

   let __exports = {};

   const { instance } = await WebAssembly.instantiateStreaming(
     fetch(wasm_url),
     {
       ghc_wasm_jsffi: (await import(js_url)).default(__exports),
       wasi_snapshot_preview1: ...
     }
   );

   Object.assign(__exports, wasm_instance.exports);

This way, the ``ghc_wasm_jsffi`` imports will have access to all exports
of the wasm instance.

After the wasm instance is created, initialization needs to be done:

.. code:: javascript

   wasi.initialize(wasm_instance);

The ``wasm32-wasi`` reactor module ABI defines the ``_initialize``
export function, which is auto generated at link time and it must be
called exactly once before any other wasm exports can be called. The
correct way to call it depends on the wasi implementation provider in
JavaScript.

Finally, in JavaScript, you can use ``await __exports.my_func()`` to
call your exported ``my_func`` function and get its result, pass
arguments, do error handling, etc etc.
