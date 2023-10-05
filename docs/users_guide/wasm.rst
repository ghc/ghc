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

To run the wasm module in the browsers, refer to the ``ghc-wasm-meta``
documentation for more details.
