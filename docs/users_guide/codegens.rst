.. _code-generators:

GHC Backends
============

.. index::
   single: GHC backends
   single: GHC code generators

GHC supports multiple backend code generators. This is the part of the
compiler responsible for taking the last intermediate representation
that GHC uses (a form called Cmm that is a simple, C like language) and
compiling it to executable code. The backends that GHC support are
described below.

.. _native-code-gen:

Native code Generator (``-fasm``)
---------------------------------

.. index::
   single: native code generator

The default backend for GHC. It is a native code generator, compiling
Cmm all the way to assembly code. It is the fastest backend and
generally produces good performance code. It has the best support for
compiling shared libraries. Select it with the ``-fasm`` flag.

.. _llvm-code-gen:

LLVM Code Generator (``-fllvm``)
--------------------------------

.. index::
   single: LLVM code generator

This is an alternative backend that uses the `LLVM <http://llvm.org>`__
compiler to produce executable code. It generally produces code as with
performance as good as the native code generator but for some cases can
produce much faster code. This is especially true for numeric, array
heavy code using packages like vector. The penalty is a significant
increase in compilation times. Select the LLVM backend with the
``-fllvm`` flag. Currently *LLVM 2.8* and later are supported.

You must install and have LLVM available on your ``PATH`` for the LLVM code
generator to work. Specifically GHC needs to be able to call the ``opt``
and ``llc`` tools. Secondly, if you are running Mac OS X with LLVM 3.0
or greater then you also need the `Clang c
compiler <http://clang.llvm.org>`__ compiler available on your ``PATH``.

To install LLVM and Clang:

-  *Linux*: Use your package management tool.

-  *Mac OS X*: Clang is included by default on recent OS X machines when
   XCode is installed (from ``10.6`` and later). LLVM is not included.
   In order to use the LLVM based code generator, you should install the
   `Homebrew <http://mxcl.github.com/homebrew/>`__ package manager for
   OS X. Alternatively you can download binaries for LLVM and Clang from
   `here <http://llvm.org/releases/download.html>`__.

-  *Windows*: You should download binaries for LLVM and clang from
   `here <http://llvm.org/releases/download.html>`__.

.. _c-code-gen:

C Code Generator (``-fvia-C``)
------------------------------

.. index::
   single: C code generator
   single: -fvia-C

This is the oldest code generator in GHC and is generally not included
any more having been deprecated around GHC 7.0. Select it with the
``-fvia-C`` flag.

The C code generator is only supported when GHC is built in
unregisterised mode, a mode where GHC produces "portable" C code as
output to facilitate porting GHC itself to a new platform. This mode
produces much slower code though so it's unlikely your version of GHC
was built this way. If it has then the native code generator probably
won't be available. You can check this information by calling
``ghc --info`` (see :ref:`ghc-info`).

.. _unreg:

Unregisterised compilation
--------------------------

.. index::
   single: unregisterised compilation

The term "unregisterised" really means "compile via vanilla C",
disabling some of the platform-specific tricks that GHC normally uses to
make programs go faster. When compiling unregisterised, GHC simply
generates a C file which is compiled via gcc.

When GHC is build in unregisterised mode only the LLVM and C code
generators will be available. The native code generator won't be. LLVM
usually offers a substantial performance benefit over the C backend in
unregisterised mode.

Unregisterised compilation can be useful when porting GHC to a new
machine, since it reduces the prerequisite tools to ``gcc``, ``as``, and
``ld`` and nothing more, and furthermore the amount of platform-specific
code that needs to be written in order to get unregisterised compilation
going is usually fairly small.

Unregisterised compilation cannot be selected at compile-time; you have
to build GHC with the appropriate options set. Consult the GHC Building
Guide for details.

You can check if your GHC is unregisterised by calling
``ghc --info`` (see :ref:`ghc-info`).
