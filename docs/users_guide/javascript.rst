.. _ffi-javascript:

FFI and the JavaScript Backend
==============================

.. index::
   single: FFI and the JavaScript Backend

GHC's JavaScript backend supports its own calling convention for
JavaScript-specific foreign imports. Any unapplied function is
supported, including function names. Commonly, JavaScript foreign
imports are written as an unapplied JavaScript `arrow function
<https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions>`_,
but ``function`` keyword anonymous functions are also supported.

By treating an import string as an unapplied function, arbitrary
JavaScript can be included in an import, so a simple example might
look like:

.. code-block:: haskell

  foreign import javascript "((x,y) => { return x + y; })"
    js_add :: Int -> Int -> Int

.. _`JavaScript FFI Types`:

JavaScript FFI Types
--------------------

Some types are able to be used directly in the type signatures of foreign
exports, without conversion to a ``JSVal``. We saw in the first example
that ``Int`` is one of these.

There are a number of supported types that can be passed directly in this
way, and they act as primitives within GHC's JavaScript RTS. This is in
comparison to data structures that are implemented in Haskell, such as
``String`` - being a list, this doesn't have a primitive JavaScript implementation,
and isn't equivalent to a JavaScript string.

The following types are supported in this way:

* ``Int``, including ``Int32`` and other sized numerical values.
* ``Int64``, and other 64 bit numbers are passed as two variables to the function,
  where the first includes the sign and the higher bits
* ``Bool``
* ``Char``
* ``Any``
* ``ByteArray#``
* ``Double`` and ``Float``
* ``MVar#``, and other RTS objects
* Unboxed tuples (e.g. ``(# a, b #)``) can appear in the return type, and are
  constructed in JavaScript using macros such as ``RETURN_UBX_TUP2(x, y)``.

As in the C FFI, types in the JavaScript FFI can't be type checked against the foreign code, so
the following example would compile successfully - despite `5` not being a valid JavaScript value
for the Haskell `Bool` type:

.. code-block:: haskell

  foreign import javascript "((x) => { return 5; })"
    type_error :: Bool -> Bool

Interruptible calling convention
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A foreign import can be declared with the `interruptible` calling convention:

.. code-block:: haskell

  foreign import javascript interruptible "((i,cont) => { ...; return cont(17); })"
    foo :: Int -> IO Int

In this case the javascript function will be passed one additional argument in
the final position (noted `cont` in the example): it is a continuation
function that must be called from JavaScript to return a value from the foreign
call and to resume the execution of the Haskell code.

JSVal
^^^^^

The JavaScript backend has a concept of an untyped 'plain' JavaScript
value, under the guise of the type ``JSVal``. Values having this type
are mostly opaque to Haskell codes: you can think of `JSVal` as a data type whose
data constructors aren't exposed. Its main use case is to pass opaque
JavaScript values from one FFI call to another.

Nevertheless the module ``GHC.JS.Prim`` from ``base`` contains functions for
working with foreign ``JSVal`` objects. Currently, it provides the following
conversions:

* ``Int`` <-> ``JSVal`` (``toJSInt``, ``fromJSInt``)
* ``String`` <-> ``JSVal`` (``toJSString``, ``fromJSString``)
* ``[JSVal]`` <-> ``JSVal`` (``toJSArray``, ``fromJSArray``)

It also contains functions for working with objects:

* ``jsNull :: JSVal`` - the JavaScript ``null``
* ``isNull :: JSVal -> Bool`` - test for the JavaScript ``null``
* ``isUndefined :: JSVal -> Bool`` - test for the JavaScript ``undefined``
* ``getProp :: JSVal -> String -> JSVal`` - object field access


JavaScript Callbacks
^^^^^^^^^^^^^^^^^^^^

The JavaScript execution model is based around callback functions, and
GHC's JavaScript backend implements these as a type in order to support
useful browser programs, and programs interacting with JavaScript libraries.

The module ``GHC.JS.Foreign.Callback`` in ``base`` defines the type ``Callback a``,
as well as several functions to construct callbacks from Haskell functions
of up to three ``JSVal`` arguments. Unlike a regular function, a ``Callback``
function is passed in the FFI as a plain JavaScript function - enabling us to call
these functions from within JavaScript:

.. code-block:: haskell

  foreign import javascript "((f) => { f('Example!'); })"
    callback_example :: Callback (JSVal -> IO ()) -> IO ()

  printJSValAsString :: JSVal -> IO ()
  printJSValAsString = putStrLn . fromJSString

  main :: IO ()
  main = do
    printJS <- syncCallback1 ThrowWouldBlock printJSValAsString
    callback_example printJS
    releaseCallback printJS

This example will call our ``printJSValAsString`` function, via JavaScript,
with the JavaScript string ``Example!`` as an argument. On the last line,
the callback memory is freed. Since there's no way for the Haskell JS runtime
to know if a function is still being referenced by JavaScript code, the memory
must be manually released when no longer needed.

On the first line of ``main``, we see where the ``Callback`` is actually
created, by ``syncCallback1``. ``syncCallback`` has versions up to three,
including a zero-argument version with no suffix. To use callbacks with more
than three pieces of data, it's recommended to package data into JavaScript
objects or arrays as required.

There are three categories of functions that create callbacks, with the
arity-1 type signatures shown here for demonstration:

* ``syncCallback1 :: OnBlocked -> (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))``:
  Synchronous callbacks that don't return a value. These take an additional
  ``data OnBlocked = ThrowWouldBlock | ContinueAsync`` argument for use in the
  case that the thread becomes blocked on e.g. an ``MVar`` transaction.

* ``syncCallback1' :: (JSVal -> IO JSVal) -> IO (Callback (JSVal -> IO JSVal))``:
  Synchronous callbacks that return a value. Because of the return value, there
  is no possibility of continuing asynchronously, so no ``OnBlocked`` argument
  is taken.

* ``asyncCallback1 :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))``:
  Asynchronous callbacks that immediately start in a new thread. Cannot return a
  value.

There is no checking that the passed arguments match the callback, so the
following example compiles and correctly prints 10, despite the argument being
passed as an ``Int`` to a ``Callback`` that accepts a ``JSVal``:

.. code-block:: haskell

  foreign import javascript "((f,x) => { return f(x); })"
    apply_int :: Callback (JSVal -> IO JSVal) -> Int -> IO Int

  main :: IO ()
  main = do
    add3 <- syncCallback1' (return . (+3))
    print =<< apply_int add3 7
    releaseCallback add3

Callbacks as Foreign Exports
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

JavaScript callbacks allow for a sort of FFI exports via FFI imports. To do
this, a global JavaScript variable is set, and that global variable can then
be called from use cases that access plain JavaScript functions - such as
interactive HTML elements. This would look like:

.. code-block:: haskell

  foreign import javascript "((f) => { globalF = f })"
    setF :: Callback (JSVal -> IO ()) -> IO ()

  main :: IO ()
  main = do
    log <- syncCallback1 ThrowWouldBlock (print . fromJSString)
    setF log
    -- don't releaseCallback log


.. code-block:: html

  <button onClick="globalF('Button pressed!")>Example</button>

We have to make sure not to use ``releaseCallback`` on any functions that
are to be available in HTML, because we want these functions to be in
memory indefinitely.

Writing Replacement Implementations for Libraries with C FFI Functions
----------------------------------------------------------------------

Many libraries make use of C FFI functions to accomplish low-level or
performance sensitive operations - known as ``cbits`` and often kept in
a folder with this name. For such a library to support the JavaScript
backend, the ``cbits`` must have replacement implementations.

In principle, it is possible for the JavaScript backend to automatically
compile ``cbits`` using Emscripten, but this requires wrappers to convert
data between the JS backend's RTS data format, and the format expected by
Emscripten-compiled functions. Since C functions are often used where
performance is more critical, there's potential for the data conversions
to negate this purpose.

Instead, it is more effective for a library to provide an alternate
implementation for functions using the C FFI - either by providing direct
one-to-one replacement JavaScript functions, or by using C preprocessor
directives to replace C FFI imports with some combination of JS FFI imports
and pure-Haskell implementation.

Direct Implementation of C FFI Imports in JavaScript as ``jsbits``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When the JavaScript backend generates code for a C FFI import, it will call
the function named in the import string, prepended by ``h$`` - so the imported
C function ``open`` will look for the JavaScript function ``h$open``. No verification
is done to ensure that these functions are actually implemented in the linked
JavaScript files, so there can be runtime errors when a missing JavaScript
function is called.

Based on this, implementing a C function in JavaScript is a matter of providing
a function of the correct shape (based on the C FFI import type signature) in
any of the linked JavaScript sources. External JavaScript sources are linked
by either providing them as an argument to GHC, or listing them in the ``js-sources``
field of the cabal file - in which case it would usually be inside a predicate to
detect the ``javascript`` architecture, such as:

.. code-block:: cabal

  library

    if arch(javascript)
      js-sources:
        jsbits/example.js

Note that ``js-sources`` requires Cabal 3.10 to be used with library targets, and
Cabal 3.12 to be used with executable targets.

The shape required of the JavaScript function will depend on the particular
C types used:

* primitives, such as ``CInt`` will map directly to a single JavaScript argument
  using JavaScript primitives. In the case of ``CInt``, this will be a JavaScript
  number. Note that in the case of return values, a JavaScript number will usually
  need to be rounded or cast back to an integral value in cases where mathematical
  operations are used

* pointer values, including ``CString``, are passed as an unboxed ``(ptr, offset)``
  pair. For arguments, being unboxed will mean these are passed as two top-level
  arguments to the function. For return values, unboxed values should be returned
  from JavaScript functions by using a special C preprocessor macro,
  ``RETURN_UBX_TUP2(ptr, offset)``

* ``CString``, in addition to the above pointer handling, will need to be decoded
  and encoded to convert them between character arrays and JavaScript strings.

* other RTS primitive types are discussed previously in `JavaScript FFI Types`_.

As an example, let's consider the implementation of ``getcwd``:

.. code-block:: haskell

  -- unix:System.Posix.Directory

  foreign import ccall unsafe "getcwd" c_getcwd :: Ptr CChar -> CSize -> IO (Ptr CChar)

.. code-block:: javascript

  // libraries/base/jsbits/base.js

  //#OPTIONS: CPP

  function h$getcwd(buf, off, buf_size) {
    try {
      var cwd = h$encodeUtf8(process.cwd());
      if (buf_size < cwd.len && buf_size !== 0) {
        h$setErrno("ERANGE");
        RETURN_UBX_TUP2(null, 0);
      } else if (buf !== null) {
        h$copyMutableByteArray(cwd, 0, buf, off, cwd.len);
        RETURN_UBX_TUP2(buf, off);
      } else if (buf_size === 0) {
        RETURN_UBX_TUP2(cwd, 0);
      } else {
        var out = h$newByteArray(buf_size);
        h$copyMutableByteArray(cwd, 0, out, off, cwd.len);
      }
    } catch (e) {
      h$setErrno(e);
      RETURN_UBX_TUP2(null, 0);
    }
  }

Here, the C function ``getcwd`` maps to the JavaScript function ``h$getcwd``, which
exists in a ``.js`` file within ``base``'s ``jsbits`` subdirectory. ``h$getcwd``
expects a ``CString`` (passed as the equivalent ``Ptr CChar``) and a
``CSize`` argument. This results in three arguments to the JavaScript function - two
for the string's pointer and offset, and one for the size, which will be passed as a
JavaScript number.

Next, the JavaScript ``h$getcwd`` function demonstrates several details:

* In the try clause, the ``cwd`` value is first accessed using a NodeJS-provided method.
  This value is immediately encoded using ``h$encodeUtf8``, which is provided by the
  JavaScript backend. This function will only return the pointer for the encoded value,
  and the offset will always be 0

* Next, we select one of several cases - based on the specification of the C function
  that we're trying to immitate

* In the first case where the given buffer size is too small, but not zero, the function
  must set the ``ERANGE`` error code, which we do here with ``h$setErrno``, and return
  ``null``. As we saw in the function arguments, pointers are passed as a ``(ptr, offset)``
  pair - meaning ``null`` is represented by returning the unboxed pair ``(null, 0)``

* In the second case where there is enough space in ``buf`` to successfully copy the
  bytes, we do so using ``h$copyMutableByteArray`` - a function supplied by GHC's JavaScript
  RTS

* In the third case where ``buf_size`` is 0, this indicates in the C function's specification
  that we can allocate a new buffer of the appropriate size to return. We already have
  this in the form of the previously encoded ``cwd``, so we can just return it, along
  with the 0 offset

* In the last case where ``buf`` is null, and ``buf_size`` is large enough, we allocate a
  new buffer, this time with ``buf_size`` bytes of space using ``h$newByteArray``, and
  we again perform a mutable copy

* To use C preprocessor macros in linked JavaScript files, the file must open with the
  ``//#OPTIONS: CPP`` line, as is shown towards the start of this snippet

* If an error occurs, the catch clause will pass it to ``h$setErrno`` and return the
  ``(null, 0)`` pointer and offset pair - which is a behaviour expected by the C function
  in the error case.

Writing JavaScript Functions to be NodeJS and Browser Aware
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the above example of implementing ``getcwd``, the function we use in the JavaScript
implementation is from NodeJS, and the behaviour doesn't make sense to implement in a
browser. Therefore, the actual implementation will include a C preprocessor condition
to check if we're compiling for the browser, in which case ``h$unsupported(-1)`` will
be called. There can be multiple non-browser JavaScript runtimes, so we'll also have
to check at runtime to make sure that NodeJS is in use.

.. code-block:: javascript

  function h$getcwd(buf, off, buf_size) {
  #ifndef GHCJS_BROWSER
    if (h$isNode()) {
      try {
        var cwd = h$encodeUtf8(process.cwd());
        if (buf_size < cwd.len && buf_size !== 0) {
          h$setErrno("ERANGE");
          return (null, 0);
        } else if (buf !== null) {
          h$copyMutableByteArray(cwd, 0, buf, off, cwd.len);
          RETURN_UBX_TUP2(buf, off);
        } else if (buf_size === 0) {
          RETURN_UBX_TUP2(cwd, 0);
        } else {
          var out = h$newByteArray(buf_size);
          h$copyMutableByteArray(cwd, 0, out, off, cwd.len);
        }
      } catch (e) {
        h$setErrno(e);
        RETURN_UBX_TUP2(null, 0);
      }
    } else
  #endif
      h$unsupported();
      RETURN_UBX_TUP2(null, 0);
  }

Replacing C FFI Imports with Pure Haskell and JavaScript
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Instead of providing a direct JavaScript implementation for each C FFI import, we can
instead use the C preprocessor to conditionally remove these C imports (and possibly
use sites as well). Then, some combination of JavaScript FFI imports and Haskell
implementation can be added instead. As in the direct implementation section, any
linked JavaScript files should usually be in a ``if arch(javascript)`` condition in
the cabal file.

As an example of a mixed Haskell and JavaScript implementation replacing a C
implementation, consider ``base:GHC.Clock``:

.. code-block:: haskell

  #if defined(javascript_HOST_ARCH)
  getMonotonicTimeNSec :: IO Word64
  getMonotonicTimeNSec = do
    w <- getMonotonicTimeMSec
    return (floor w * 1000000)

  foreign import javascript unsafe "performance.now"
    getMonotonicTimeMSec :: IO Double

  #else
  foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicTimeNSec :: IO Word64
  #endif

Here, the ``getMonotonicTimeNSec`` C FFI import is replaced by the JavaScript FFI
import ``getMonotonicTimeMSec``, which imports the standard JavaScript function
``performance.now``. However, because this JavaScript implementation
returns the time as a ``Double`` of floating point milliseconds, it must be wrapped
by a Haskell function to extract the integral value that's expected.

In this case, the choice of using a mixed Haskell and JavaScript replacement
implementation was caused by the limitation of clocks being system calls. In a lot
of cases, C functions are used for similar system-level functionality. In such
cases, it's recommended to import the required system functions from standard
JavaScript libraries (or from the runtime, as was required for ``getcwd``), and
use Haskell wrapper functions to convert the imported functions to the appropriate
format.

In other cases, C functions are used for performance. For these cases, pure-Haskell
implementations are the preferred first step for compatibility with the JavaScript
backend since it would be more future-proof against changes to the RTS data format.
Depending on the use case, compiler-optimised JS code might be hard to complete with
using hand-written JavaScript. Generally, the most likely performance gains from
hand-written JavaScript come from functions with data that stays as JavaScript
primitive types for a long time, especially strings. For this, ``JSVal`` allows
values to be passed between ``Haskell`` and ``JavaScript`` without a marshalling
penalty.


Linking with C sources
----------------------

GHC supports compiling C sources into JavaScript (using Emscripten) and linking
them with the rest of the JavaScript code (generated from Haskell codes and from
the RTS).

C functions compiled with Emscripten get a "_" prepended to their name in
JavaScript. For example, C "malloc" becomes "_malloc" in JavaScript.

EMCC pragmas
^^^^^^^^^^^^

By default the EMCC linker drops code considered dead and it has no way to know
which code is alive due to some call from Haskell or from a JavaScript wrapper.
As such, you must explicitly add some pragmas at the top of one of your `.js`
files to indicate which functions are alive:

```
//#OPTIONS:EMCC:EXPORTED_RUNTIME_METHODS=foo,bar
```

Enable methods `foo` and `bar` from the Emscripten runtime system. This is used
for methods such as `ccall`, `cwrap`, `addFunction`, `removeFunction`... that
are described in Emscripten documentation.

```
//#OPTIONS:EMCC:EXPORTED_FUNCTIONS=_foo,_bar
```

Enable C functions `foo` and `bar` to be exported respectively as `_foo` and
`_bar` (`_` prepended). This is used for C library functions (e.g. `_malloc`,
`_free`, etc.) and for the C code compiled with your project (e.g.
`_sqlite3_open` and others for the `sqlite` C library).

You can use both pragmas as many times as you want. Ultimately all the entries
end up in sets of functions passed to the Emscripten linker via
`-sEXPORTED_RUNTIME_METHODS` and `-sEXPORTED_FUNCTIONS` (which can only be
passed once; the latter argument overrides the former ones).


```
//#OPTIONS:EMCC:EXTRA=-foo,-bar
```

This pragma allows additional options to be passed to Emscripten if need be. We
already pass:

- `-sSINGLE_FILE=1`: required to create a single `.js` file as artefact
  (otherwise `.wasm` files corresponding to C codes need to be present in the
  current working directory when invoking the resulting `.js` file).

- `-sALLOW_TABLE_GROWTH`: required to support `addFunction`

- `-sEXPORTED_RUNTIME_METHODS` and `-sEXPORTED_FUNCTIONS`: see above.

Be careful because some extra arguments may break the build in unsuspected ways.

Wrappers
^^^^^^^^

The JavaScript backend doesn't generate wrappers for foreign imports to call
directly into the compiled C code. I.e. given the following foreign import:

```haskell
foreign import ccall "foo" foo :: ...
```

The JavaScript backend will replace calls to `foo` with calls to the JavaScript
function `h$foo`. It's still up to the programmer to call `_foo` or not from `h$foo`
on a case by case basis. If `h$foo` calls the generated from C function
`_foo`, then we say that `h$foo` is a wrapper function. These wrapper functions
are used to marshal arguments and returned values between the JS heap and the
Emscripten heap.

On one hand, GHC's JavaScript backend creates a different array of bytes per
allocation (in order to make use of the garbage collector of the JavaScript
engine). On the other hand, Emscripten's C heap consists in a single array of
bytes. To call C functions converted to JavaScript that have pointer arguments,
wrapper functions have to:

1. allocate some buffer in the Emscripten heap (using `_malloc`) to get a valid
   Emscripten pointer
2. copy the bytes from the JS object to the buffer in the Emscripten heap
3. use the Emscripten pointer to make the call to the C function
4. optionally copy back the bytes from the Emscripten heap if the call may have
   changed the contents of the buffer
5. free the Emscripten buffer (with `_free`)

GHC's JavaScript rts provides helper functions for this in `rts/js/mem.js`. See
`h$copyFromHeap`, `h$copyToHeap`, `h$initHeapBuffer`, etc.

Callbacks
^^^^^^^^^

Some C functions take function pointers as arguments (e.g. callbacks). This is
supported by the JavaScript backend but requires some work from the wrapper
functions.

1. On the Haskell side it is possible to create a pointer to an Haskell function
   (a `FunPtr`) by using a "wrapper" foreign import. See the documentation of
   `base:Foreign.Ptr.FunPtr`.
2. This `FunPtr` can be passed to a JavaScript wrapper function. However it's
   implemented as a `StablePtr` and needs to be converted into a function
   pointer that Emscripten understands. This can be done with
   `h$registerFunPtrOnHeap`.
3. When a callback is no longer needed, it can be freed with
   `h$unregisterFunPtrFromHeap`.

Note that in some circumstances you may not want to register an Haskell function
directly as a callback. It is perfectly possible to register/free regular JavaScript
function as Emscripten functions using `Module.addFunction` and `Module.removeFunction`.
That's what the helper functions mentioned above do.

