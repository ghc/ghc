.. _ffi:

Foreign function interface (FFI)
================================

.. index::
   single: Foreign function interface
   single: interfacing with native code

.. extension:: ForeignFunctionInterface
    :shortdesc: Enable foreign function interface.

    :since: 6.8.1

    Allow use of the Haskell foreign function interface.

GHC (mostly) conforms to the Haskell Foreign Function Interface, whose
definition is part of the Haskell Report on
`http://www.haskell.org/ <http://www.haskell.org/>`__.

FFI support is enabled by default, but can be enabled or disabled
explicitly with the :extension:`ForeignFunctionInterface` flag.

GHC implements a number of GHC-specific extensions to the FFI Chapter of the
Haskell 2010 Report. These extensions are described in :ref:`ffi-ghcexts`, but
please note that programs using these features are not portable. Hence, these
features should be avoided where possible.

The FFI libraries are documented in the accompanying  library
documentation; see for example the :base-ref:`Foreign.` module.

GHC differences to the FFI Chapter
----------------------------------

Guaranteed call safety
~~~~~~~~~~~~~~~~~~~~~~

The Haskell 2010 Report specifies that ``safe`` FFI calls must allow foreign
calls to safely call into Haskell code. In practice, this means that the
garbage collector must be able to run while these calls are in progress,
moving heap-allocated Haskell values around arbitrarily.

This greatly constrains library authors since it implies that it is not safe to
pass any heap object reference to a ``safe`` foreign function call.  For
instance, it is often desirable to pass an unpinned ``ByteArray#``\s directly
to native code to avoid making an otherwise-unnecessary copy. However, this can
only be done safely if the array is guaranteed not to be moved by the garbage
collector in the middle of the call.

The Chapter does *not* require implementations to refrain from doing the
same for ``unsafe`` calls, so strictly Haskell 2010-conforming programs
cannot pass heap-allocated references to ``unsafe`` FFI calls either.

In previous releases, GHC would take advantage of the freedom afforded by the
Chapter by performing ``safe`` foreign calls in place of ``unsafe`` calls in
the bytecode interpreter. This meant that some packages which worked when
compiled would fail under GHCi (e.g. :ghc-ticket:`13730`).

However, since version 8.4 this is no longer the case: GHC **guarantees** that
garbage collection will never occur during an ``unsafe`` call, even in the
bytecode interpreter, and further guarantees that ``unsafe`` calls will be
performed in the calling thread.


.. _ffi-ghcexts:

GHC extensions to the FFI Chapter
---------------------------------

The FFI features that are described in this section are specific to GHC.
Your code will not be portable to other compilers if you use them.

Unboxed types
~~~~~~~~~~~~~

The following unboxed types may be used as basic foreign types (see FFI
Chapter, Section 8.6): ``Int#``, ``Word#``, ``Char#``, ``Float#``,
``Double#``, ``Addr#``, ``StablePtr# a``, ``MutableByteArray#``,
``ForeignObj#``, and ``ByteArray#``.

.. _ffi-newtype-io:

Newtype wrapping of the IO monad
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The FFI spec requires the IO monad to appear in various places, but it
can sometimes be convenient to wrap the IO monad in a ``newtype``, thus: ::

      newtype MyIO a = MIO (IO a)

(A reason for doing so might be to prevent the programmer from calling
arbitrary IO procedures in some part of the program.)

The Haskell FFI already specifies that arguments and results of foreign
imports and exports will be automatically unwrapped if they are newtypes
(Section 3.2 of the FFI addendum). GHC extends the FFI by automatically
unwrapping any newtypes that wrap the IO monad itself. More precisely,
wherever the FFI specification requires an ``IO`` type, GHC will accept any
newtype-wrapping of an ``IO`` type. For example, these declarations are
OK: ::

       foreign import foo :: Int -> MyIO Int
       foreign import "dynamic" baz :: (Int -> MyIO Int) -> CInt -> MyIO Int

.. _ffi-prim:

Primitive imports
~~~~~~~~~~~~~~~~~

GHC extends the FFI with an additional calling convention ``prim``,
e.g.: ::

       foreign import prim "foo" foo :: ByteArray# -> (# Int#, Int# #)

This is used to import functions written in Cmm code that follow an
internal GHC calling convention. The arguments and results must be
unboxed types, except that an argument may be of type ``Any`` (by way of
``unsafeCoerce#``) and the result type is allowed to be an unboxed tuple
or the type ``Any``.

This feature is not intended for use outside of the core libraries that
come with GHC. For more details see the
:ghc-wiki:`GHC developer wiki <Commentary/PrimOps>`.

.. _ffi-interruptible:

Interruptible foreign calls
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: InterruptibleFFI
    :shortdesc: Enable interruptible FFI.

    :since: 7.2.1

This concerns the interaction of foreign calls with
``Control.Concurrent.throwTo``. Normally when the target of a
``throwTo`` is involved in a foreign call, the exception is not raised
until the call returns, and in the meantime the caller is blocked. This
can result in unresponsiveness, which is particularly undesirable in the
case of user interrupt (e.g. Control-C). The default behaviour when a
Control-C signal is received (``SIGINT`` on Unix) is to raise the
``UserInterrupt`` exception in the main thread; if the main thread is
blocked in a foreign call at the time, then the program will not respond
to the user interrupt.

The problem is that it is not possible in general to interrupt a foreign
call safely. However, GHC does provide a way to interrupt blocking
system calls which works for most system calls on both Unix and Windows.
When the ``InterruptibleFFI`` extension is enabled, a foreign call can
be annotated with ``interruptible`` instead of ``safe`` or ``unsafe``: ::

    foreign import ccall interruptible
       "sleep" sleepBlock :: CUint -> IO CUint

``interruptible`` behaves exactly as ``safe``, except that when a
``throwTo`` is directed at a thread in an interruptible foreign call, an
OS-specific mechanism will be used to attempt to cause the foreign call
to return:

Unix systems
    The thread making the foreign call is sent a ``SIGPIPE`` signal
    using ``pthread_kill()``. This is usually enough to cause a blocking
    system call to return with ``EINTR`` (GHC by default installs an
    empty signal handler for ``SIGPIPE``, to override the default
    behaviour which is to terminate the process immediately).

Windows systems
    [Vista and later only] The RTS calls the Win32 function
    ``CancelSynchronousIo``, which will cause a blocking I/O operation
    to return with the error ``ERROR_OPERATION_ABORTED``.

If the system call is successfully interrupted, it will return to
Haskell whereupon the exception can be raised. Be especially careful
when using ``interruptible`` that the caller of the foreign function is
prepared to deal with the consequences of the call being interrupted; on
Unix it is good practice to check for ``EINTR`` always, but on Windows
it is not typically necessary to handle ``ERROR_OPERATION_ABORTED``.

.. _ffi-capi:

The CAPI calling convention
~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: CApiFFI
    :shortdesc: Enable the CAPI calling convention.

    :since: 7.10.1

The ``CApiFFI`` extension allows a calling convention of ``capi`` to be
used in foreign declarations, e.g. ::

    foreign import capi "header.h f" f :: CInt -> IO CInt

Rather than generating code to call ``f`` according to the platform's
ABI, we instead call ``f`` using the C API defined in the header
``header.h``. Thus ``f`` can be called even if it may be defined as a
CPP ``#define`` rather than a proper function.

When using ``capi``, it is also possible to import values, rather than
functions. For example, ::

    foreign import capi "pi.h value pi" c_pi :: CDouble

will work regardless of whether ``pi`` is defined as

.. code-block:: c

    const double pi = 3.14;

or with

.. code-block:: c

    #define pi 3.14

In order to tell GHC the C type that a Haskell type corresponds to when
it is used with the CAPI, a ``CTYPE`` pragma can be used on the type
definition. The header which defines the type can optionally also be
specified. The syntax looks like: ::

    data    {-# CTYPE "unistd.h" "useconds_t" #-} T = ...
    newtype {-# CTYPE            "useconds_t" #-} T = ...

``hs_thread_done()``
~~~~~~~~~~~~~~~~~~~~

.. code-block:: c

    void hs_thread_done(void);

GHC allocates a small amount of thread-local memory when a thread calls
a Haskell function via a ``foreign export``. This memory is not normally
freed until ``hs_exit()``; the memory is cached so that subsequent calls
into Haskell are fast. However, if your application is long-running and
repeatedly creates new threads that call into Haskell, you probably want
to arrange that this memory is freed in those threads that have finished
calling Haskell functions. To do this, call ``hs_thread_done()`` from
the thread whose memory you want to free.

Calling ``hs_thread_done()`` is entirely optional. You can call it as
often or as little as you like. It is safe to call it from a thread that
has never called any Haskell functions, or one that never will. If you
forget to call it, the worst that can happen is that some memory remains
allocated until ``hs_exit()`` is called. If you call it too often, the
worst that can happen is that the next call to a Haskell function incurs
some extra overhead.

.. _ffi-ghc:

Using the FFI with GHC
----------------------

The following sections also give some hints and tips on the use of the
foreign function interface in GHC.

.. _foreign-export-ghc:

Using ``foreign export`` and ``foreign import ccall "wrapper"`` with GHC
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: foreign export; with GHC

When GHC compiles a module (say ``M.hs``) which uses ``foreign export``
or ``foreign import "wrapper"``, it generates a ``M_stub.h`` for use by
C programs.

For a plain ``foreign export``, the file ``M_stub.h`` contains a C
prototype for the foreign exported function. For example, if we compile
the following module: ::

    module Foo where

    foreign export ccall foo :: Int -> IO Int

    foo :: Int -> IO Int
    foo n = return (length (f n))

    f :: Int -> [Int]
    f 0 = []
    f n = n:(f (n-1))

Then ``Foo_stub.h`` will contain something like this:

.. code-block:: c

    #include "HsFFI.h"
    extern HsInt foo(HsInt a0);

To invoke ``foo()`` from C, just ``#include "Foo_stub.h"`` and call
``foo()``.

The ``Foo_stub.h`` file can be redirected using the ``-stubdir`` option;
see :ref:`options-output`.

.. _using-own-main:

Using your own ``main()``
^^^^^^^^^^^^^^^^^^^^^^^^^

Normally, GHC's runtime system provides a ``main()``, which arranges to
invoke ``Main.main`` in the Haskell program. However, you might want to
link some Haskell code into a program which has a main function written
in another language, say C. In order to do this, you have to initialize
the Haskell runtime system explicitly.

Let's take the example from above, and invoke it from a standalone C
program. Here's the C code:

.. code-block:: c

    #include <stdio.h>
    #include "HsFFI.h"

    #ifdef __GLASGOW_HASKELL__
    #include "Foo_stub.h"
    #endif

    int main(int argc, char *argv[])
    {
      int i;

      hs_init(&argc, &argv);

      for (i = 0; i < 5; i++) {
        printf("%d\n", foo(2500));
      }

      hs_exit();
      return 0;
    }

We've surrounded the GHC-specific bits with
``#ifdef __GLASGOW_HASKELL__``; the rest of the code should be portable
across Haskell implementations that support the FFI standard.

The call to ``hs_init()`` initializes GHC's runtime system. Do NOT try
to invoke any Haskell functions before calling ``hs_init()``: bad things
will undoubtedly happen.

We pass references to ``argc`` and ``argv`` to ``hs_init()`` so that it
can separate out any arguments for the RTS (i.e. those arguments between
``+RTS...-RTS``).

After we've finished invoking our Haskell functions, we can call
``hs_exit()``, which terminates the RTS.

There can be multiple calls to ``hs_init()``, but each one should be matched by
one (and only one) call to ``hs_exit()``. The outermost ``hs_exit()`` will
actually de-initialise the system.  Note that currently GHC's runtime cannot
reliably re-initialise after this has happened; see :ref:`infelicities-ffi`.

.. note::
    When linking the final program, it is normally easiest to do the
    link using GHC, although this isn't essential. If you do use GHC, then
    don't forget the flag :ghc-flag:`-no-hs-main`, otherwise GHC
    will try to link to the ``Main`` Haskell module.

.. note::
    On Windows hs_init treats argv as UTF8-encoded. Passing other encodings
    might lead to unexpected results. Passing NULL as argv is valid but can
    lead to <unknown> showing up in error messages instead of the name of the
    executable.

To use ``+RTS`` flags with ``hs_init()``, we have to modify the example
slightly. By default, GHC's RTS will only accept "safe" ``+RTS`` flags (see
:ref:`options-linker`), and the :ghc-flag:`-rtsopts[=⟨none|some|all⟩]`
link-time flag overrides this. However, :ghc-flag:`-rtsopts[=⟨none|some|all⟩]`
has no effect when :ghc-flag:`-no-hs-main` is in use (and the same goes for
:ghc-flag:`-with-rtsopts=⟨opts⟩`). To set these options we have to call a
GHC-specific API instead of ``hs_init()``:

.. code-block:: c

    #include <stdio.h>
    #include "HsFFI.h"

    #ifdef __GLASGOW_HASKELL__
    #include "Foo_stub.h"
    #include "Rts.h"
    #endif

    int main(int argc, char *argv[])
    {
      int i;

    #if __GLASGOW_HASKELL__ >= 703
      {
          RtsConfig conf = defaultRtsConfig;
          conf.rts_opts_enabled = RtsOptsAll;
          hs_init_ghc(&argc, &argv, conf);
      }
    #else
      hs_init(&argc, &argv);
    #endif

      for (i = 0; i < 5; i++) {
        printf("%d\n", foo(2500));
      }

      hs_exit();
      return 0;
    }

Note two changes: we included ``Rts.h``, which defines the GHC-specific
external RTS interface, and we called ``hs_init_ghc()`` instead of
``hs_init()``, passing an argument of type ``RtsConfig``. ``RtsConfig``
is a struct with various fields that affect the behaviour of the runtime
system. Its definition is:

.. code-block:: c

    typedef struct {
        RtsOptsEnabledEnum rts_opts_enabled;
        const char *rts_opts;
    } RtsConfig;

    extern const RtsConfig defaultRtsConfig;

    typedef enum {
        RtsOptsNone,         // +RTS causes an error
        RtsOptsSafeOnly,     // safe RTS options allowed; others cause an error
        RtsOptsAll           // all RTS options allowed
      } RtsOptsEnabledEnum;

There is a default value ``defaultRtsConfig`` that should be used to
initialise variables of type ``RtsConfig``. More fields will undoubtedly
be added to ``RtsConfig`` in the future, so in order to keep your code
forwards-compatible it is best to initialise with ``defaultRtsConfig``
and then modify the required fields, as in the code sample above.

.. _ffi-library:

Making a Haskell library that can be called from foreign code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The scenario here is much like in :ref:`using-own-main`, except that the
aim is not to link a complete program, but to make a library from
Haskell code that can be deployed in the same way that you would deploy
a library of C code.

The main requirement here is that the runtime needs to be initialized
before any Haskell code can be called, so your library should provide
initialisation and deinitialisation entry points, implemented in C or
C++. For example:

.. code-block:: c

    #include <stdlib.h>
    #include "HsFFI.h"

    HsBool mylib_init(void){
      int argc = 2;
      char *argv[] = { "+RTS", "-A32m", NULL };
      char **pargv = argv;

      // Initialize Haskell runtime
      hs_init(&argc, &pargv);

      // do any other initialization here and
      // return false if there was a problem
      return HS_BOOL_TRUE;
    }

    void mylib_end(void){
      hs_exit();
    }

The initialisation routine, ``mylib_init``, calls ``hs_init()`` as
normal to initialise the Haskell runtime, and the corresponding
deinitialisation function ``mylib_end()`` calls ``hs_exit()`` to shut
down the runtime.

.. _glasgow-foreign-headers:

Using header files
~~~~~~~~~~~~~~~~~~

.. index::
   single: C calls, function headers

C functions are normally declared using prototypes in a C header file.
Earlier versions of GHC (6.8.3 and earlier) ``#include``\ d the header
file in the C source file generated from the Haskell code, and the C
compiler could therefore check that the C function being called via the
FFI was being called at the right type.

GHC no longer includes external header files when compiling via C, so
this checking is not performed. The change was made for compatibility
with the :ref:`native code generator <native-code-gen>` (:ghc-flag:`-fasm`) and to
comply strictly with the FFI specification, which requires that FFI calls are
not subject to macro expansion and other CPP conversions that may be applied
when using C header files. This approach also simplifies the inlining of foreign
calls across module and package boundaries: there's no need for the header file
to be available when compiling an inlined version of a foreign call, so the
compiler is free to inline foreign calls in any context.

The ``-#include`` option is now deprecated, and the ``include-files``
field in a Cabal package specification is ignored.

Memory Allocation
~~~~~~~~~~~~~~~~~

The FFI libraries provide several ways to allocate memory for use with
the FFI, and it isn't always clear which way is the best. This decision
may be affected by how efficient a particular kind of allocation is on a
given compiler/platform, so this section aims to shed some light on how
the different kinds of allocation perform with GHC.

``alloca``
    Useful for short-term allocation when the allocation is intended to
    scope over a given ``IO`` computation. This kind of allocation is
    commonly used when marshalling data to and from FFI functions.

    In GHC, ``alloca`` is implemented using ``MutableByteArray#``, so
    allocation and deallocation are fast: much faster than C's
    ``malloc/free``, but not quite as fast as stack allocation in C. Use
    ``alloca`` whenever you can.

``mallocForeignPtr``
    Useful for longer-term allocation which requires garbage collection.
    If you intend to store the pointer to the memory in a foreign data
    structure, then ``mallocForeignPtr`` is *not* a good choice,
    however.

    In GHC, ``mallocForeignPtr`` is also implemented using
    ``MutableByteArray#``. Although the memory is pointed to by a
    ``ForeignPtr``, there are no actual finalizers involved (unless you
    add one with ``addForeignPtrFinalizer``), and the deallocation is
    done using GC, so ``mallocForeignPtr`` is normally very cheap.

``malloc/free``
    If all else fails, then you need to resort to ``Foreign.malloc`` and
    ``Foreign.free``. These are just wrappers around the C functions of
    the same name, and their efficiency will depend ultimately on the
    implementations of these functions in your platform's C library. We
    usually find ``malloc`` and ``free`` to be significantly slower than
    the other forms of allocation above.

``Foreign.Marshal.Pool``
    Pools are currently implemented using ``malloc/free``, so while they
    might be a more convenient way to structure your memory allocation
    than using one of the other forms of allocation, they won't be any
    more efficient. We do plan to provide an improved-performance
    implementation of Pools in the future, however.

.. _ffi-threads:

Multi-threading and the FFI
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to use the FFI in a multi-threaded setting, you must use the
:ghc-flag:`-threaded` option (see :ref:`options-linker`).

Foreign imports and multi-threading
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When you call a ``foreign import``\ ed function that is annotated as
``safe`` (the default), and the program was linked using :ghc-flag:`-threaded`,
then the call will run concurrently with other running Haskell threads.
If the program was linked without :ghc-flag:`-threaded`, then the other Haskell
threads will be blocked until the call returns.

This means that if you need to make a foreign call to a function that
takes a long time or blocks indefinitely, then you should mark it
``safe`` and use :ghc-flag:`-threaded`. Some library functions make such calls
internally; their documentation should indicate when this is the case.

If you are making foreign calls from multiple Haskell threads and using
:ghc-flag:`-threaded`, make sure that the foreign code you are calling is
thread-safe. In particularly, some GUI libraries are not thread-safe and
require that the caller only invokes GUI methods from a single thread.
If this is the case, you may need to restrict your GUI operations to a
single Haskell thread, and possibly also use a bound thread (see
:ref:`haskell-threads-and-os-threads`).

Note that foreign calls made by different Haskell threads may execute in
*parallel*, even when the ``+RTS -N`` flag is not being used
(:ref:`parallel-options`). The :rts-flag:`-N ⟨x⟩` flag controls parallel
execution of Haskell threads, but there may be an arbitrary number of
foreign calls in progress at any one time, regardless of the ``+RTS -N``
value.

If a call is annotated as ``interruptible`` and the program was
multithreaded, the call may be interrupted in the event that the Haskell
thread receives an exception. The mechanism by which the interrupt
occurs is platform dependent, but is intended to cause blocking system
calls to return immediately with an interrupted error code. The
underlying operating system thread is not to be destroyed. See
:ref:`ffi-interruptible` for more details.

.. _haskell-threads-and-os-threads:

The relationship between Haskell threads and OS threads
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Normally there is no fixed relationship between Haskell threads and OS
threads. This means that when you make a foreign call, that call may
take place in an unspecified OS thread. Furthermore, there is no
guarantee that multiple calls made by one Haskell thread will be made by
the same OS thread.

This usually isn't a problem, and it allows the GHC runtime system to
make efficient use of OS thread resources. However, there are cases
where it is useful to have more control over which OS thread is used,
for example when calling foreign code that makes use of thread-local
state. For cases like this, we provide *bound threads*, which are
Haskell threads tied to a particular OS thread. For information on bound
threads, see the documentation for the :base-ref:`Control.Concurrent.` module.

Foreign exports and multi-threading
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When the program is linked with :ghc-flag:`-threaded`, then you may invoke
``foreign export``\ ed functions from multiple OS threads concurrently.
The runtime system must be initialised as usual by calling
``hs_init()``, and this call must complete before invoking any
``foreign export``\ ed functions.

.. _hs-exit:

On the use of ``hs_exit()``
^^^^^^^^^^^^^^^^^^^^^^^^^^^

``hs_exit()`` normally causes the termination of any running Haskell
threads in the system, and when ``hs_exit()`` returns, there will be no
more Haskell threads running. The runtime will then shut down the system
in an orderly way, generating profiling output and statistics if
necessary, and freeing all the memory it owns.

It isn't always possible to terminate a Haskell thread forcibly: for
example, the thread might be currently executing a foreign call, and we
have no way to force the foreign call to complete. What's more, the
runtime must assume that in the worst case the Haskell code and runtime
are about to be removed from memory (e.g. if this is a
:ref:`Windows DLL <win32-dlls>`, ``hs_exit()`` is normally called before unloading
the DLL). So ``hs_exit()`` *must* wait until all outstanding foreign
calls return before it can return itself.

The upshot of this is that if you have Haskell threads that are blocked
in foreign calls, then ``hs_exit()`` may hang (or possibly busy-wait)
until the calls return. Therefore it's a good idea to make sure you
don't have any such threads in the system when calling ``hs_exit()``.
This includes any threads doing I/O, because I/O may (or may not,
depending on the type of I/O and the platform) be implemented using
blocking foreign calls.

The GHC runtime treats program exit as a special case, to avoid the need
to wait for blocked threads when a standalone executable exits. Since
the program and all its threads are about to terminate at the same time
that the code is removed from memory, it isn't necessary to ensure that
the threads have exited first.  If you want this fast and loose
version of ``hs_exit()``, you can call:

.. code-block:: c

   void hs_exit_nowait(void);

instead.  This is particularly useful if you have foreign libraries
that need to call ``hs_exit()`` at program exit (perhaps via a C++
destructor): in this case you should use ``hs_exit_nowait()``, because
the thread that called ``exit()`` and is running C++ destructors is in
a foreign call from Haskell that will never return, so ``hs_exit()``
would deadlock.

.. _hs_try_putmvar:

Waking up Haskell threads from C
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sometimes we want to be able to wake up a Haskell thread from some C
code.  For example, when using a callback-based C API, we register a C
callback and then we need to wait for the callback to run.

One way to do this is to create a ``foreign export`` that will do
whatever needs to be done to wake up the Haskell thread - perhaps
``putMVar`` - and then call this from our C callback.  There are a
couple of problems with this:

1. Calling a foreign export has a lot of overhead: it creates a
   complete new Haskell thread, for example.
2. The call may block for a long time if a GC is in progress.  We
   can't use this method if the C API we're calling doesn't allow
   blocking in the callback.

For these reasons GHC provides an external API to ``tryPutMVar``,
``hs_try_putmvar``, which you can use to cheaply and asynchronously
wake up a Haskell thread from C/C++.

.. code-block:: c

  void hs_try_putmvar (int capability, HsStablePtr sp);

The C call ``hs_try_putmvar(cap, mvar)`` is equivalent to the Haskell
call ``tryPutMVar mvar ()``, except that it is

* non-blocking: takes a bounded, short, amount of time

* asynchronous: the actual putMVar may be performed after the call
  returns (for example, if the RTS is currently garbage collecting).
  That's why ``hs_try_putmvar()`` doesn't return a result to say
  whether the put succeeded.  It is your responsibility to ensure that
  the ``MVar`` is empty; if it is full, ``hs_try_putmvar()`` will have
  no effect.

**Example**. Suppose we have a C/C++ function to call that will return and then
invoke a callback at some point in the future, passing us some data.
We want to wait in Haskell for the callback to be called, and retrieve
the data.  We can do it like this:

.. code-block:: haskell

     import GHC.Conc (newStablePtrPrimMVar, PrimMVar)

     makeExternalCall = mask_ $ do
       mvar <- newEmptyMVar
       sp <- newStablePtrPrimMVar mvar
       fp <- mallocForeignPtr
       withForeignPtr fp $ \presult -> do
         cap <- threadCapability =<< myThreadId
         scheduleCallback sp cap presult
         takeMVar mvar `onException`
           forkIO (do takeMVar mvar; touchForeignPtr fp)
         peek presult

     foreign import ccall "scheduleCallback"
         scheduleCallback :: StablePtr PrimMVar
                          -> Int
                          -> Ptr Result
                          -> IO ()

And inside ``scheduleCallback``, we create a callback that will in due
course store the result data in the ``Ptr Result``, and then call
``hs_try_putmvar()``.

There are a few things to note here.

* There's a special function to create the ``StablePtr``:
  ``newStablePtrPrimMVar``, because the RTS needs a ``StablePtr`` to
  the primitive ``MVar#`` object, and we can't create that directly.
  Do *not* just use ``newStablePtr`` on the ``MVar``: your program
  will crash.

* The ``StablePtr`` is freed by ``hs_try_putmvar()``.  This is because
  it would otherwise be difficult to arrange to free the ``StablePtr``
  reliably: we can't free it in Haskell, because if the ``takeMVar``
  is interrupted by an asynchronous exception, then the callback will
  fire at a later time.  We can't free it in C, because we don't know
  when to free it (not when ``hs_try_putmvar()`` returns, because that
  is an async call that uses the ``StablePtr`` at some time in the
  future).

* The ``mask_`` is to avoid asynchronous exceptions before the
  ``scheduleCallback`` call, which would leak the ``StablePtr``.

* We find out the current capability number and pass it to C.  This is
  passed back to ``hs_try_putmvar``, and helps the RTS to know which
  capability it should try to perform the ``tryPutMVar`` on.  If you
  don't care, you can pass ``-1`` for the capability to
  ``hs_try_putmvar``, and it will pick an arbitrary one.

  Picking the right capability will help avoid unnecessary context
  switches.  Ideally you should pass the capability that the thread
  that will be woken up last ran on, which you can find by calling
  ``threadCapability`` in Haskell.

* If you want to also pass some data back from the C callback to
  Haskell, this is best done by first allocating some memory in
  Haskell to receive the data, and passing the address to C, as we did
  in the above example.

* ``takeMVar`` can be interrupted by an asynchronous exception.  If
  this happens, the callback in C will still run at some point in the
  future, will still write the result, and will still call
  ``hs_try_putmvar()``.  Therefore we have to arrange that the memory
  for the result stays alive until the callback has run, so if an
  exception is thrown during ``takeMVar`` we fork another thread to
  wait for the callback and hold the memory alive using
  ``touchForeignPtr``.

For a fully working example, see
``testsuite/tests/concurrent/should_run/hs_try_putmvar001.hs`` in the
GHC source tree.

.. _ffi-floating-point:

Floating point and the FFI
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: Floating point; and the FFI

The standard C99 ``fenv.h`` header provides operations for inspecting
and modifying the state of the floating point unit. In particular, the
rounding mode used by floating point operations can be changed, and the
exception flags can be tested.

In Haskell, floating-point operations have pure types, and the
evaluation order is unspecified. So strictly speaking, since the
``fenv.h`` functions let you change the results of, or observe the
effects of floating point operations, use of ``fenv.h`` renders the
behaviour of floating-point operations anywhere in the program
undefined.

Having said that, we *can* document exactly what GHC does with respect
to the floating point state, so that if you really need to use
``fenv.h`` then you can do so with full knowledge of the pitfalls:

-  GHC completely ignores the floating-point environment, the runtime
   neither modifies nor reads it.

-  The floating-point environment is not saved over a normal thread
   context-switch. So if you modify the floating-point state in one
   thread, those changes may be visible in other threads. Furthermore,
   testing the exception state is not reliable, because a context switch
   may change it. If you need to modify or test the floating point state
   and use threads, then you must use bound threads
   (``Control.Concurrent.forkOS``), because a bound thread has its own
   OS thread, and OS threads do save and restore the floating-point
   state.

-  It is safe to modify the floating-point unit state temporarily during
   a foreign call, because foreign calls are never pre-empted by GHC.

