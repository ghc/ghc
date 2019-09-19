====================================
GHC Bignum
====================================

Haskell language provides support for arbitrary-precision integers (see `Haskell
98 <https://www.haskell.org/onlinereport/basic.html#sect6.4 >`_ and `Haskell
2010 <https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1350006.4>`_
reports, section 6.4).

This document describes a newer implementation of this feature for the GHC
compiler.

This work is sponsored by IOHK (iohk.io).

Background
----------

GHC used to have two backends to provide arbitrary-precision integer support:
* ``integer-gmp``: based the `GNU MP Bignum library (GMP) <https://gmplib.org/>`_
* ``integer-simple``: a pure Haskell implementation

The two backends compare as follows:

+===============================+=========================+========================+
|                               | GMP                     | Simple                 |
+===============================+=========================+========================+
+ License                       | LGPL                    | BSD3                   |
+-------------------------------+-------------------------+------------------------+
+ Implementation                | External C library      | Pure Haskell           |
+-------------------------------+-------------------------+------------------------+
+ Performance                   | Very good               | Very bad               |
+-------------------------------+-------------------------+------------------------+

The choice between the two backends is tough. For performance we should always
use GMP because Simple's performance is several orders of magnitude poorer (see
figures later in this document). But on the other hand, building GHC with GMP is
complicated because the build system must find or build the external GMP C
library. Moreover, because of the LGPL license, static linking can't be used to
produce non LGPL programs. However static linking is required on some platforms
(e.g. `Windows
<https://gitlab.haskell.org/ghc/ghc/wikis/windows-dynamic-linking>`_).

Another issue is that the two backends use different internal representations
(one based on ByteArray# and another based on list of words).  It is not easy to
switch from one backend to another because GHC is aware of the backend used and
produces different code depending on the internal representation.

Similarly, because of the different internal representation, there are at least
`71 packages on Hackage <https://packdeps.haskellers.com/reverse/integer-gmp>`_
(among them some widely used packages such as ``bytestring`` and ``text``) that
explicitly depend on ``integer-gmp`` package or that provide a flag to depend
either on ``integer-gmp`` or ``integer-simple``.

ghc-bignum package
------------------

The current state with the two backends (GMP and Simple) isn't satisfying for
the reasons stated above. With the new ``ghc-bignum`` package, we aim to fix all
of them.

1. Provide a pure haskell implementation

Having a pure Haskell implementation with a BSD-like license is the ideal as it
can be easily compiled for any target GHC supports. For quite some time I
(Sylvain Henry) have been working on a pure Haskell implementation which has
been integrated into ``ghc-bignum``. It is much more performant than
``integer-simple`` (performance figures are given below).

2. Support platform specific arbitrary precision libraries

A pure Haskell can't really expect to beat low-level heavily tuned libraries
such as GMP. Hence ``ghc-bignum`` keeps the existing support for GMP.

Moreover ``ghc-bignum`` is designed to make it easier to add newer backends
based on other native libraries. There have been some proposals to switch from
GMP to another fast implementation that would be BSD-like licensed (e.g. `BSDNT
<https://github.com/wbhart/bsdnt>`_, `OpenSSL libcrypto integers
<https://github.com/ghc-proposals/ghc-proposals/pull/183>`_, etc.).

``ghc-bignum`` makes it easier to implementation new backends as the backend
code is called only in a few well specified cases (compared to the whole
Natural/Integer interface that had to be implemented before). Additionally every
backend can be tested against the same set of tests and benchmarks, and
newer backends can easily fallback to the Native backend to provide operations
that they don't provide.

3. Provide support for non-native target platforms

GHCJS and Asterius projects use GHC to compile Haskell code to JavaScript and
WebAssembly (respectively). The produced code needs to use JavaScript's BigInt
to achieve good performance. Similarly, GHC based compilers targeting .Net CLR
or Java's ByteCode would also need to use platform provided
arbitrary-precision numbers. To support this, ``ghc-bignum`` provides an ``ffi``
backend that calls FFI operations which are left unimplemented: the compiler
must replace these FFI calls with appropriate calls to the target library.

4. All the backends use the same representation

Many (most?) implementations of arbitrary precision numbers use an array of
words to store big numbers, with words stored in little-endian order. A
notorious exception is ``integer-simple`` which uses a Haskell list to store the
words, partly explaining its poor performance. ``ghc-bignum`` uses the same
representation with all the backends. This simplifies GHC as it doesn't have to
know which backend is used.

5. Backend selection via Cabal flags

Backend selection is done via flags of the ``ghc-bignum`` Cabal package. It
means that backend selection can be performed per project (e.g. by setting the
backend flag in your ``stack.yaml`` or ``cabal.project`` configuration files).
The same GHC can be used to build packages using different backends. Packages
needing to access the internal representation of arbitrary precision numbers
only have to depend on ``ghc-bignum``, whatever backend ends up being used.

6. Validation against native backend

When the ``check`` flag of the ``ghc-bignum`` package is set, all the
computations are done with both the selected backend and the native backend.
Both results are compared and an exception is raised if they are not the same.

Latest performance results
--------------------------

The following benchmarks use the performance of the native backend (pure Haskell
implementation) as a baseline. To compare with other backends (GMP, Simple), we
report the following numbers:

   time_with_other_backend / time_with_native_backend

E.g. a value of 50 means that the selected backend is 50x slower than Native; a
value of 0.25 means that the selected backend is 4x faster than Native (1/0.25=4).

* Platform is: Linux 5.3.7 on Intel(R) Core(TM) i7-9700K CPU @ 3.60GHz
* Simple is from a custom GHC 8.8.1 bindist (perf flavour)
* GMP is from GHC 8.6.5 (LTS-14.6 from Stackage)
* Sizes of the operands are given in Words

Summary:

* For the four operations (+,-,*,/):

   * Native is always faster than Simple except in 1 case: + for 8-1 (why?)

   * Simple's quotRem is so bad that in the four bench cases Native is at least 50x
     faster (193x faster in the 20-word / 20-word case)

   * Native is sometimes faster than GMP (e.g. 1.3x with `+` in 80-8 word case). But in
     general it is between 1-15x slower when inputs stay small.

* Native GCD implementation uses division recursively: the performance gets
  worse compared to GMP with the size of the inputs


+-------------------------+--------+--------+
| Operation               |  GMP   | Simple |
+-------------------------+--------+--------+
| 1-word    `+` 1-word    | 1.00   | 1.34   |
| 8-word    `+` 1-word    | 0.96   | 0.65   |
| 20-word   `+` 20-word   | 0.79   | 2.78   |
| 80-word   `+` 8-word    | 1.30   | 1.16   |
| 1024-word `+` 128-word  | 0.85   | 1.68   |
+-------------------------+--------+--------+

+-------------------------+--------+--------+
| Operation               |  GMP   | Simple |
+-------------------------+--------+--------+
| 1-word    `-` 1-word    | 1.10   | 2.31   |
| 8-word    `-` 1-word    | 0.99   | 1.01   |
| 20-word   `-` 20-word   | 0.33   | 4.45   |
| 80-word   `-` 8-word    | 1.00   | 1.88   |
| 1024-word `-` 128-word  | 0.61   | 3.05   |
+-------------------------+--------+--------+

+-------------------------+--------+--------+
| Operation               |  GMP   | Simple |
+-------------------------+--------+--------+
| 1-word    `*` 1-word    | 1.09   | 1.29   |
| 8-word    `*` 1-word    | 0.99   | 6.74   |
| 20-word   `*` 20-word   | 0.16   | 7.26   |
| 80-word   `*` 8-word    | 0.17   | 7.9    |
| 1024-word `*` 128-word  | 0.076  | 10.27  |
+-------------------------+--------+--------+

+------------------------------+--------+--------+
| Operation                    |  GMP   | Simple |
+------------------------------+--------+--------+
| 1-word    `quotRem` 1-word   | 1.14   | 56.02  |
| 8-word    `quotRem` 1-word   | 0.34   | 55.8   |
| 20-word   `quotRem` 20-word  | 0.38   | 193.57 |
| 80-word   `quotRem` 8-word   | 0.11   | 52.43  |
| 1024-word `quotRem` 128-word | 0.073  | 102.73 |
+------------------------------+--------+--------+

+--------------------------+--------+--------+
| Operation                |  GMP   | Simple |
+--------------------------+--------+--------+
| 1-word    `gcd` 1-word   | 0.20   | 120.44 |
| 8-word    `gcd` 1-word   | 0.20   | 87.72  |
| 20-word   `gcd` 20-word  | 0.017  | 86.56  |
| 80-word   `gcd` 8-word   | 0.037  | 36.22  |
| 1024-word `gcd` 128-word | 0.037  | 1103.6 |
+--------------------------+--------+--------+


Related links
-------------

* Modern Computer Arithmetic: https://members.loria.fr/PZimmermann/mca/pub226.html
* https://gitlab.haskell.org/ghc/ghc/issues/601
* https://gitlab.haskell.org/ghc/ghc/wikis/replacing-gmp-notes
* https://www.reddit.com/r/haskell/comments/2jt37u/ghc_uses_gmp_and_its_performance_is_nothing_short/
