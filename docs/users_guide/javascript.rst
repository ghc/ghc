.. _ffi-javascript:

FFI and the JavaScript Backend
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

JSVal
~~~~~

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

JavaScript FFI Types
~~~~~~~~~~~~~~~~~~~~

Some types are able to be used directly in the type signatures of foreign
exports, without conversion to a ``JSVal``. We saw in the first example
that ``Int`` is one of these.

The supported types are those with primitive JavaScript representations
that match the Haskell type. This means types such as the Haskell ``String``
type aren't supported directly, because they're lists - which don't have
a primitive JavaScript representation, and so are incompatible with each
other.

The following types are supported in this way:

* ``Int``
* ``Bool``
* ``Char``

As in the C FFI, types in the JavaScript FFI can't be type checked against the foreign code, so
the following example would compile successfully - despite `5` not being a valid JavaScript value
for the Haskell `Bool` type:

.. code-block:: haskell

  foreign import javascript "((x) => { return 5; })"
    type_error :: Bool -> Bool

JavaScript Callbacks
~~~~~~~~~~~~~~~~~~~~

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

* ``syncCallback1 :: (JSVal -> IO ()) -> OnBlocked -> IO (Callback (JSVal -> IO ()))``:
  Synchronous callbacks that don't return a value. These take an additional
  ``data OnBlocked = ThrowWouldBlock | ContinueAsync`` argument for use in the
  case that the thread becomes blocked on e.g. an ``MVar`` transaction.

* ``syncCallback' :: (JSVal -> IO JSVal) -> IO (Callback (JSVal -> IO ()))``:
  Synchronous callbacks that return a value. Because of the return value, there
  is no possibility of continuing asynchronously, so no ``OnBlocked`` argument
  is taken.

* ``asyncCallback :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))``:
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

