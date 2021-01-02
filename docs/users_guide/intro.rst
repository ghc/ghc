.. _introduction-GHC:

Introduction
============

This is a guide to using the Glasgow Haskell Compiler (GHC): an
interactive and batch compilation system for the
`Haskell 2010 <http://www.haskell.org/>`__ language.

GHC has two main components: an interactive Haskell interpreter (also
known as GHCi), described in :ref:`ghci`, and a batch compiler,
described throughout :ref:`using-ghc`. In fact, GHC consists of a single
program which is just run with different options to provide either the
interactive or the batch system.

The batch compiler can be used alongside GHCi: compiled modules can be
loaded into an interactive session and used in the same way as
interpreted code, and in fact when using GHCi most of the library code
will be pre-compiled. This means you get the best of both worlds: fast
pre-compiled library code, and fast compile turnaround for the parts of
your program being actively developed.

GHC supports numerous language extensions, including concurrency, a
foreign function interface, exceptions, type system extensions such as
multi-parameter type classes, local universal and existential
quantification, functional dependencies, scoped type variables and
explicit unboxed types. These are all described in
:ref:`ghc-language-features`.

GHC has a comprehensive optimiser, so when you want to Really Go For It
(and you've got time to spare) GHC can produce pretty fast code.
Alternatively, the default option is to compile as fast as possible
while not making too much effort to optimise the generated code
(although GHC probably isn't what you'd describe as a fast compiler :-).

GHC's profiling system supports "cost centre stacks": a way of seeing
the profile of a Haskell program in a call-graph like structure. See
:ref:`profiling` for more details.

GHC comes with a number of libraries. These are described in separate
documentation.

.. _getting:

Obtaining GHC
-------------

Go to the `GHC home page <http://www.haskell.org/ghc/>`__ and follow the
"download" link to download GHC for your platform.

Alternatively, if you want to build GHC yourself, head on over to the
:ghc-wiki:`GHC Building Guide <building>` to find out how to get the sources,
and build it on your system. Note that GHC itself is written in Haskell, so you
will still need to install GHC in order to build it.

.. _mailing-lists-GHC:

Meta-information: Web sites, mailing lists, etc.
------------------------------------------------

.. index::
   single: mailing lists, Glasgow Haskell
   single: Glasgow Haskell mailing lists

On the World-Wide Web, there are several URLs of likely interest:

-  `GHC home page <http://www.haskell.org/ghc/>`__

-  `GHC Developers Home <https://gitlab.haskell.org/ghc/ghc>`__ (developer
   documentation, wiki, and bug tracker)

We run the following mailing lists about GHC. We encourage you to join,
as you feel is appropriate.

``glasgow-haskell-users``
    This list is for GHC users to chat among themselves. If you have a
    specific question about GHC, please check the
    `FAQ <http://www.haskell.org/haskellwiki/GHC/FAQ>`__ first.

    Subscribers can post to the list by sending their message to 
    glasgow-haskell-users@haskell.org. Further information can be found
    on the
    `Mailman page <http://www.haskell.org/mailman/listinfo/glasgow-haskell-users>`__.

``ghc-devs``
    The GHC developers hang out here. If you are working with the GHC API
    or have a question about GHC's implementation, feel free to chime in.

    Subscribers can post to the list by sending their message to 
    ghc-devs@haskell.org. Further information can be found on the
    `Mailman page <http://www.haskell.org/mailman/listinfo/ghc-devs>`__.

There are several other Haskell and GHC-related mailing lists served by
``www.haskell.org``. Go to http://www.haskell.org/mailman/listinfo/
for the full list.

.. _bug-reporting:

Reporting bugs in GHC
---------------------

.. index::
   single: bugs; reporting
   single: reporting bugs

Glasgow Haskell is a changing system so there are sure to be bugs in it.
If you find one, please see :ghc-wiki:`this wiki page <report-a-bug>` for
information on how to report it.

.. _version-numbering:

GHC version numbering policy
----------------------------

.. index::
   single: version, of ghc

As of GHC version 6.8, we have adopted the following policy for
numbering GHC versions:

    Stable branches are numbered ``x.y``, where ⟨y⟩ is *even*. Releases
    on the stable branch ``x.y`` are numbered ``x.y.z``, where ⟨z⟩ (>=
    1) is the patchlevel number. Patchlevels are bug-fix releases only,
    and never change the programmer interface to any system-supplied
    code. However, if you install a new patchlevel over an old one you
    will need to recompile any code that was compiled against the old
    libraries.

    The value of ``__GLASGOW_HASKELL__`` (see :ref:`c-pre-processor`)
    for a major release ``x.y.z`` is the integer ⟨xyy⟩ (if ⟨y⟩ is a
    single digit, then a leading zero is added, so for example in
    version 6.8.2 of GHC we would have ``__GLASGOW_HASKELL__==608``).

    .. index::
       single: __GLASGOW_HASKELL__

    We may make snapshot releases of the current stable branch
    `available for
    download <http://www.haskell.org/ghc/dist/latest/>`__, and the
    latest sources are available from
    :ghc-wiki:`the git repositories <repositories>`.

    Stable snapshot releases are named ``x.y.z.YYYYMMDD``. where
    ``YYYYMMDD`` is the date of the sources from which the snapshot was
    built, and ``x.y.z+1`` is the next release to be made on that
    branch. For example, ``6.8.1.20040225`` would be a snapshot of the
    ``6.8`` branch during the development of ``6.8.2``.

    Unstable snapshot releases are named ``x.y.YYYYMMDD``. where
    ``YYYYMMDD`` is the date of the sources from which the snapshot was
    built. For example, ``6.7.20040225`` would be a snapshot of the HEAD
    before the creation of the ``6.8`` branch.

    The value of ``__GLASGOW_HASKELL__`` for a snapshot release is the
    integer ⟨xyy⟩. You should never write any conditional code which
    tests for this value, however: since interfaces change on a
    day-to-day basis, and we don't have finer granularity in the values
    of ``__GLASGOW_HASKELL__``, you should only conditionally compile
    using predicates which test whether ``__GLASGOW_HASKELL__`` is equal
    to, later than, or earlier than a given major release.

The version number of your copy of GHC can be found by invoking ``ghc``
with the ``--version`` flag (see :ref:`options-help`).

The compiler version can be tested within compiled code with the
``MIN_VERSION_GLASGOW_HASKELL`` CPP macro (defined only when
:extension:`CPP` is used). See :ref:`standard-cpp-macros` for details.

.. _License:

The Glasgow Haskell Compiler License
------------------------------------

Copyright 2002 - 2007, The University Court of the University of
Glasgow. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

-  Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

-  Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

-  Neither name of the University nor the names of its contributors may
   be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
NO EVENT SHALL THE UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
