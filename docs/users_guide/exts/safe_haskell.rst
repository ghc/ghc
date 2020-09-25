.. _safe-haskell:

Safe Haskell
============

.. index::
   single: safe haskell

Safe Haskell is an extension to the Haskell language that is implemented
in GHC as of version 7.2. It allows for unsafe code to be securely
included in a trusted code base by restricting the features of GHC
Haskell the code is allowed to use. Put simply, it makes the types of
programs trustable.

While a primary use case of Safe Haskell is running untrusted code, Safe
Haskell doesn't provide this directly. Instead, Safe Haskell provides
strict type safety. Without Safe Haskell, GHC allows many exceptions to
the type system which can subvert any abstractions. By providing strict
type safety, Safe Haskell enables developers to build their own library
level sandbox mechanisms to run untrusted code.

While Safe Haskell is an extension, it actually runs in the background
for every compilation with GHC. It does this to track the type
violations of modules to infer their safety, even when they aren't
explicitly using Safe Haskell. Please refer to section
:ref:`safe-inference` for more details of this.

The design of Safe Haskell covers the following aspects:

- A :ref:`safe language <safe-language>` dialect of Haskell that provides
  stricter guarantees about the code. It allows types and module boundaries to
  be trusted.

- A *safe import* extension that specifies that the module being imported must
  be trusted.

- A definition of *trust* (or safety) and how it operates, along with ways of
  defining and changing the trust of modules and packages.

Safe Haskell, however, *does not offer* compilation safety. During
compilation time it is possible for arbitrary processes to be launched,
using for example the :ref:`custom pre-processor <pre-processor>` flag.
This can be manipulated to either compromise a user's system at
compilation time, or to modify the source code just before compilation
to try to alter Safe Haskell flags. This is discussed further in section
:ref:`safe-compilation`.

.. _safe-use-cases:

Uses of Safe Haskell
--------------------

.. index::
   single: safe haskell uses

Safe Haskell has been designed with two use cases in mind:

-  Enforcing strict type safety at compile time
-  Compiling and executing untrusted code

Strict type-safety (good style)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Haskell offers a powerful type system and separation of pure and effectual
functions through the ``IO`` monad. However, there are several loop holes in the
type system, the most obvious being the ``unsafePerformIO :: IO a -> a``
function. The safe language dialect of Safe Haskell disallows the use of such
functions. This can be useful restriction as it makes Haskell code easier to
analyse and reason about. It also codifies the existing culture in the Haskell
community of trying to avoid unsafe functions unless absolutely necessary. As
such, using the safe language (through the ``-XSafe`` flag) can be thought of as
a way of enforcing good style, similar to the function of ``-Wall``.

Building secure systems (restricted IO Monads)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: secure haskell

Systems such as information flow control security, capability based
security systems and DSLs for working with encrypted data.. etc can be
built in the Haskell language as a library. However they require
guarantees about the properties of Haskell that aren't true in general
due to the presence of functions like ``unsafePerformIO``. Safe Haskell
gives users enough guarantees about the type system to allow them to
build such secure systems.

As an example, let's define an interface for a plugin system where the
plugin authors are untrusted, possibly malicious third-parties. We do
this by restricting the plugin interface to pure functions or to a
restricted ``IO`` monad that we have defined. The restricted ``IO``
monad will only allow a safe subset of ``IO`` actions to be executed. We
define the plugin interface so that it requires the plugin module,
``Danger``, to export a single computation, ``Danger.runMe``, of type
``RIO ()``, where ``RIO`` is a monad defined as follows:

::

    -- While we use `Safe', the `Trustworthy' pragma would also be
    -- fine. We simply want to ensure that:
    -- 1) The module exports an interface that untrusted code can't
    --    abuse.
    -- 2) Untrusted code can import this module.
    --
    {-# LANGUAGE Safe #-}

    module RIO (RIO(), runRIO, rioReadFile, rioWriteFile) where

    -- Notice that symbol UnsafeRIO is not exported from this module!
    newtype RIO a = UnsafeRIO { runRIO :: IO a }

    instance Monad RIO where
        return = UnsafeRIO . return
        (UnsafeRIO m) >>= k = UnsafeRIO $ m >>= runRIO . k

    -- Returns True iff access is allowed to file name
    pathOK :: FilePath -> IO Bool
    pathOK file = {- Implement some policy based on file name -}

    rioReadFile :: FilePath -> RIO String
    rioReadFile file = UnsafeRIO $ do
        ok <- pathOK file
        if ok then readFile file else return ""

    rioWriteFile :: FilePath -> String -> RIO ()
    rioWriteFile file contents = UnsafeRIO $ do
        ok <- pathOK file
        if ok then writeFile file contents else return ()

We then compile the ``Danger`` plugin using the new Safe Haskell
``-XSafe`` flag:
::

    {-# LANGUAGE Safe #-}
    module Danger ( runMe ) where

    runMe :: RIO ()
    runMe = ...

Before going into the Safe Haskell details, let's point out some of the
reasons this security mechanism would fail without Safe Haskell:

- The design attempts to restrict the operations that ``Danger`` can perform by
  using types, specifically the ``RIO`` type wrapper around ``IO`` . The author
  of ``Danger`` can subvert this though by simply writing arbitrary ``IO``
  actions and using ``unsafePerformIO :: IO a -> a`` to execute them as pure
  functions.

- The design also relies on ``Danger`` not being able to access the
  ``UnsafeRIO`` constructor. Unfortunately Template Haskell can be used to
  subvert module boundaries and so could be used to gain access to this
  constructor.

- There is no way to place restrictions on the modules that ``Danger`` can
  import. This gives the author of ``Danger`` a very large attack surface,
  essentially any package currently installed on the system. Should any of
  these packages have a vulnerability, then the ``Danger`` module can exploit
  it.

Safe Haskell prevents all these attacks. This is done by compiling the
RIO module with the :extension:`Safe` or :extension:`Trustworthy` flag and compiling
``Danger`` with the :extension:`Safe` flag. We explain each below.

The use of :extension:`Safe` to compile ``Danger`` restricts the features of
Haskell that can be used to a `safe subset <#safe-language>`__. This
includes disallowing ``unsafePerformIO``, Template Haskell, pure FFI
functions, RULES and restricting the operation of Overlapping Instances.
The :extension:`Safe` flag also restricts the modules can be imported by
``Danger`` to only those that are considered trusted. Trusted modules
are those compiled with :extension:`Safe`, where GHC provides a mechanical
guarantee that the code is safe. Or those modules compiled with
:extension:`Trustworthy`, where the module author claims that the module is
Safe.

This is why the RIO module is compiled with :extension:`Safe` or
:extension:`Trustworthy`>, to allow the ``Danger`` module to import it. The
:extension:`Trustworthy` flag doesn't place any restrictions on the module like
:extension:`Safe` does (expect to restrict overlapping instances to `safe
overlapping instances <#safe-overlapping-instances>`__). Instead the
module author claims that while code may use unsafe features internally,
it only exposes an API that can used in a safe manner.

However, the unrestricted use of :extension:`Trustworthy` is a problem as an
arbitrary module can use it to mark themselves as trusted, yet
:extension:`Trustworthy` doesn't offer any guarantees about the module, unlike
:extension:`Safe`. To control the use of trustworthy modules it is recommended
to use the :ghc-flag:`-fpackage-trust` flag. This flag adds an extra requirement
to the trust check for trustworthy modules. It requires that for a
trustworthy modules to be considered trusted, and allowed to be used in
:extension:`Safe` compiled code, the client C compiling the code must tell GHC
that they trust the package the trustworthy module resides in. This is
essentially a way of for C to say, while this package contains
trustworthy modules that can be used by untrusted modules compiled with
:extension:`Safe`, I trust the author(s) of this package and trust the modules
only expose a safe API. The trust of a package can be changed at any
time, so if a vulnerability found in a package, C can declare that
package untrusted so that any future compilation against that package
would fail. For a more detailed overview of this mechanism see
:ref:`safe-trust`.

In the example, ``Danger`` can import module ``RIO`` because ``RIO`` is
compiled with :extension:`Safe`. Thus, ``Danger`` can make use of the
``rioReadFile`` and ``rioWriteFile`` functions to access permitted file
names. The main application then imports both ``RIO`` and ``Danger``. To
run the plugin, it calls ``RIO.runRIO Danger.runMe`` within the ``IO``
monad. The application is safe in the knowledge that the only ``IO`` to
ensue will be to files whose paths were approved by the ``pathOK`` test.

The Safe Haskell checks can be disabled for a module by passing the
:ghc-flag:`-fno-safe-haskell` flag. This is useful in particular when compiling
with source plugins as running a plugin marks the module as unsafe and can then
cause downstream modules to fail the safety checks.

.. _safe-language:

Safe Language
-------------

.. index::
   single: safe language

The Safe Haskell *safe language* (enabled by ``-XSafe``) guarantees the
following properties:

- *Referential transparency* — The types can be trusted. Any pure function, is
  guaranteed to be pure. Evaluating them is deterministic and won't cause any
  side effects. Functions in the ``IO`` monad are still allowed and behave as
  usual. So, for example, the ``unsafePerformIO :: IO a -> a`` function is
  disallowed in the safe language to enforce this property.

- *Module boundary control* — Only symbols that are publicly available through
  other module export lists can be accessed in the safe language. Values using
  data constructors not exported by the defining module, cannot be examined or
  created. As such, if a module ``M`` establishes some invariants through
  careful use of its export list, then code written in the safe language that
  imports ``M`` is guaranteed to respect those invariants.

- *Semantic consistency* — For any module that imports a module written in the
  safe language, expressions that compile both with and without the safe import
  have the same meaning in both cases. That is, importing a module written in
  the safe language cannot change the meaning of existing code that isn't
  dependent on that module. So, for example, there are some restrictions placed
  on the use of :ref:`OverlappingInstances <instance-overlap>`, as these can
  violate this property.

- *Strict subset* — The safe language is strictly a subset of Haskell as
  implemented by GHC. Any expression that compiles in the safe language has the
  same meaning as it does when compiled in normal Haskell.

These four properties guarantee that in the safe language you can trust
the types, can trust that module export lists are respected, and can
trust that code that successfully compiles has the same meaning as it
normally would.

To achieve these properties, in the safe language dialect we disable
completely the following features:

- :extension:`TemplateHaskell` — Can be used to gain access to constructors and
  abstract data types that weren't exported by a module, subverting module
  boundaries.

Furthermore, we restrict the following features:

- :extension:`ForeignFunctionInterface` — Foreign import declarations that
  import a function with a non-``IO`` type are disallowed.

- ``RULES`` — Rewrite rules defined in a module M compiled with
  :extension:`Safe` are dropped. Rules defined in Trustworthy modules that
  ``M`` imports are still valid and will fire as usual.

- :extension:`OverlappingInstances` — There is no restriction on the creation
  of overlapping instances, but we do restrict their use at a particular call
  site. This is a detailed restriction, please refer to :ref:`Safe Overlapping
  Instances <safe-overlapping-instances>` for details.

- :extension:`GeneralisedNewtypeDeriving` — GND is not allowed in the safe
  language. This is due to the ability of it to violate module boundaries when
  module authors forget to put nominal role annotations on their types as
  appropriate. For this reason, the ``Data.Coerce`` module is also considered
  unsafe. We are hoping to find a better solution here in the future.

- ``GHC.Generics`` — Hand crafted instances of the ``Generic`` type class are
  not allowed in Safe Haskell. Such instances aren't strictly unsafe, but
  there is an important invariant that a ``Generic`` instance should adhere to
  the structure of the data type for which the instance is defined, and
  allowing manually implemented ``Generic`` instances would break that
  invariant. Derived instances (through the :extension:`DeriveGeneric`
  extension) are still allowed. Note that the only allowed
  :ref:`deriving strategy <deriving-strategies>` for deriving ``Generic`` under
  Safe Haskell is ``stock``, as another strategy (e.g., ``anyclass``) would
  produce an instance that violates the invariant.

  Refer to the
  :ref:`generic programming <generic-programming>` section for more details.

.. _safe-overlapping-instances:

Safe Overlapping Instances
~~~~~~~~~~~~~~~~~~~~~~~~~~

Due to the semantic consistency guarantee of Safe Haskell, we must
restrict the function of overlapping instances. We don't restrict their
ability to be defined, as this is a global property and not something we
can determine by looking at a single module. Instead, when a module
calls a function belonging to a type-class, we check that the instance
resolution done is considered 'safe'. This check is enforced for modules
compiled with both ``-XSafe`` and ``-XTrustworthy``.

More specifically, consider the following modules:

::

            {-# LANGUAGE Safe #-}
            module Class (TC(..)) where
              class TC a where { op :: a -> String }

            {-# LANGUAGE Safe #-}
            module Dangerous (TC(..)) where
              import Class

              instance
                {-# OVERLAPS #-}
                TC [Int] where { op _ = "[Int]" }

            {-# LANGUAGE Safe #-}
            module TCB_Runner where
              import Class
              import Dangerous

              instance
                TC [a] where { op _ = "[a]" }

              f :: String
              f = op ([1,2,3,4] :: [Int])

Both module ``Class`` and module ``Dangerous`` will compile under :extension:`Safe`
without issue. However, in module ``TCB_Runner``, we must check if the call
to ``op`` in function ``f`` is safe.

What does it mean to be Safe? That importing a module compiled with
:extension:`Safe` shouldn't change the meaning of code that compiles fine
without importing the module. This is the Safe Haskell property known as
*semantic consistency*.

In our situation, module ``TCB_Runner`` compiles fine without importing
module ``Dangerous``. So when deciding which instance to use for the call to
``op``, if we determine the instance ``TC [Int]`` from module Dangerous
is the most specific, this is unsafe. This prevents code written by
third-parties we don't trust (which is compiled using ``-XSafe`` in Safe
Haskell) from changing the behaviour of our existing code.

Specifically, we apply the following rule to determine if a type-class
method call is *unsafe* when overlapping instances are involved:

-  Most specific instance, ``Ix``, defined in an ``-XSafe`` compiled module.
-  ``Ix`` is an orphan instance or a multi-parameter-type-class.
-  At least one overlapped instance, ``Iy``, is both:

   -  From a different module than ``Ix``
   -  ``Iy`` is not marked ``OVERLAPPABLE``

This is a slightly involved heuristic, but captures the situation of an
imported module ``N`` changing the behaviour of existing code. For example,
if the second condition isn't violated, then the module author ``M`` must
depend either on a type-class or type defined in ``N``.

When a particular type-class method call is considered unsafe due to
overlapping instances, and the module being compiled is using :extension:`Safe`
or :extension:`Trustworthy`, then compilation will fail. For :extension:`Unsafe`, no
restriction is applied, and for modules using safe inference, they will
be inferred unsafe.

.. _safe-imports:

Safe Imports
------------

.. index::
   single: safe imports

Safe Haskell enables a small extension to the usual import syntax of
Haskell, adding a ``safe`` keyword:

.. code-block:: none

    impdecl -> import [safe] [qualified] modid [as modid] [impspec]

When used, the module being imported with the safe keyword must be a
trusted module, otherwise a compilation error will occur. The safe
import extension is enabled by either of the ``-XSafe`` , ``-XTrustworthy`` , or
``-XUnsafe`` flags. When the ``-XSafe`` flag is used, the ``safe`` keyword is
allowed but meaningless, as every import is treated as a safe import.

.. _safe-trust:

Trust and Safe Haskell Modes
----------------------------

.. index::
   single: safe haskell trust
   single: trust

Safe Haskell introduces the following three language flags:

- :extension:`Safe` — Enables the safe language dialect, asking GHC to guarantee trust.
  The safe language dialect requires that all imports be trusted or a
  compilation error will occur. Safe Haskell will also infer this safety type
  for modules automatically when possible. Please refer to section
  :ref:`safe-inference` for more details of this.

- :extension:`Trustworthy` — Means that while this module may invoke unsafe functions
  internally, the module's author claims that it exports an API that can't be
  used in an unsafe way. This doesn't enable the safe language. It does however
  restrict the resolution of overlapping instances to only allow :ref:`safe
  overlapping instances <safe-overlapping-instances>`. The trust guarantee is
  provided by the module author, not GHC. An import statement with the ``safe``
  keyword results in a compilation error if the imported module is not trusted.
  An import statement without the keyword behaves as usual and can import any
  module whether trusted or not.

- :extension:`Unsafe` — Marks the module being compiled as unsafe so that modules
  compiled using :extension:`Safe` can't import it. You may want to explicitly mark a
  module unsafe when it exports internal constructors that can be used to
  violate invariants.

While these are flags, they also correspond to Safe Haskell module types
that a module can have. You can think of using these as declaring an
explicit contract (or type) that a module must have. If it is invalid,
then compilation will fail. GHC will also infer the correct type for
Safe Haskell, please refer to section :ref:`safe-inference` for more
details.

The procedure to check if a module is trusted or not depends on if the
:ghc-flag:`-fpackage-trust` flag is present. The check is similar in both cases
with the :ghc-flag:`-fpackage-trust` flag enabling an extra requirement for
trustworthy modules to be regarded as trusted.

Trust check (``-fpackage-trust`` disabled)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: trust check

A module ``M`` in a package ``P`` is trusted by a client C if and only if:

- Both of these hold:

   -  The module was compiled with :extension:`Safe`
   -  All of M's direct imports are trusted by C

- *or* all of these hold:

   -  The module was compiled with :extension:`Trustworthy`
   -  All of ``M``\'s direct *safe imports* are trusted by C

The above definition of trust has an issue. Any module can be compiled
with :extension:`Trustworthy` and it will be trusted. To control this, there is
an additional definition of package trust (enabled with the
:ghc-flag:`-fpackage-trust` flag). The point of package trust is to require that
the client C explicitly say which packages are allowed to contain
trustworthy modules. Trustworthy packages are only trusted if they
reside in a package trusted by C.

Trust check (``-fpackage-trust`` enabled)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: trust check
   single: -fpackage-trust

When the :ghc-flag:`-fpackage-trust` flag is enabled, whether or not a module is
trusted depends on if certain packages are trusted. Package trust is
determined by the client C invoking GHC (i.e. you).

Specifically, a package *P is trusted* when one of these hold:

-  C's package database records that ``P`` is trusted (and no command-line
   arguments override this)
-  C's command-line flags say to trust ``P`` regardless of what is recorded
   in the package database.

In either case, C is the only authority on package trust. It is up to
the client to decide which `packages they trust <#safe-package-trust>`__.

When the :ghc-flag:`-fpackage-trust` flag is used a *module M from package P is
trusted by a client C* if and only if:

-  Both of these hold:

   -  The module was compiled with :extension:`Safe`
   -  All of ``M``\'s direct imports are trusted by C

-  *or* all of these hold:

   -  The module was compiled with :extension:`Trustworthy`
   -  All of ``M``\'s direct safe imports are trusted by C
   -  Package ``P`` is trusted by C

For the first trust definition the trust guarantee is provided by GHC
through the restrictions imposed by the safe language. For the second
definition of trust, the guarantee is provided initially by the module
author. The client C then establishes that they trust the module author
by indicating they trust the package the module resides in. This trust
chain is required as GHC provides no guarantee for :extension:`Trustworthy`
compiled modules.

The reason there are two modes of checking trust is that the extra
requirement enabled by :ghc-flag:`-fpackage-trust` causes the design of Safe
Haskell to be invasive. Packages using Safe Haskell when the flag is
enabled may or may not compile depending on the state of trusted
packages on a user's machine. This is both fragile, and causes
compilation failures for everyone, even if they aren't trying to use any
of the guarantees provided by Safe Haskell. Disabling
:ghc-flag:`-fpackage-trust` by default and turning it into a flag makes Safe
Haskell an opt-in extension rather than an always on feature.

.. _safe-trust-example:

Example
~~~~~~~

::

    Package Wuggle:
        {-# LANGUAGE Safe #-}
        module Buggle where
            import Prelude
            f x = ...blah...

    Package P:
        {-# LANGUAGE Trustworthy #-}
        module M where
            import System.IO.Unsafe
            import safe Buggle

Suppose a client C decides to trust package ``P`` and package ``base``. Then
does C trust module ``M``? Well ``M`` is marked :extension:`Trustworthy`, so we don't
restrict the language. However, we still must check ``M``\'s imports:

- First, ``M`` imports ``System.IO.Unsafe``. This is an unsafe module, however
  ``M`` was compiled with :extension:`Trustworthy` , so ``P``\'s author takes
  responsibility for that import. ``C`` trusts ``P``\'s author, so this import
  is fine.

- Second, ``M`` safe imports ``Buggle``. For this import ``P``\'s author takes
  no responsibility for the safety, instead asking GHC to check whether
  ``Buggle`` is trusted by ``C``. Is it?

- ``Buggle``, is compiled with ``-XSafe``, so the code is machine-checked to be
  OK, but again under the assumption that all of ``Buggle``\'s imports are
  trusted by ``C``. We must recursively check all imports!

- Buggle only imports ``Prelude``, which is compiled with :extension:`Trustworthy`.
  ``Prelude`` resides in the ``base`` package, which ``C`` trusts, and (we'll
  assume) all of ``Prelude``\'s imports are trusted. So ``C`` trusts
  ``Prelude``, and so ``C`` also trusts Buggle. (While ``Prelude`` is typically
  imported implicitly, it still obeys the same rules outlined here).

Notice that C didn't need to trust package Wuggle; the machine checking
is enough. C only needs to trust packages that contain :extension:`Trustworthy`
modules.

.. _trustworthy-guarantees:

Trustworthy Requirements
~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: trustworthy

Module authors using the :extension:`Trustworthy` language extension for a
module ``M`` should ensure that ``M``\'s public API (the symbols exposed by its
export list) can't be used in an unsafe manner. This mean that symbols exported
should respect type safety and referential transparency.

.. _safe-package-trust:

Package Trust
~~~~~~~~~~~~~

.. index::
   single: package trust

Safe Haskell gives packages a new Boolean property, that of trust.
Several new options are available at the GHC command-line to specify the
trust property of packages:

.. ghc-flag:: -trust ⟨pkg⟩
    :shortdesc: Expose package ⟨pkg⟩ and set it to be trusted. See
        :ref:`safe-haskell`.
    :type: dynamic
    :category: packages

    Exposes package ⟨pkg⟩ if it was hidden and considers it a
    trusted package regardless of the package database.

.. ghc-flag:: -distrust ⟨pkg⟩
    :shortdesc: Expose package ⟨pkg⟩ and set it to be distrusted. See
        :ref:`safe-haskell`.
    :type: dynamic
    :category: packages

    Exposes package ⟨pkg⟩ if it was hidden and considers it
    an untrusted package regardless of the package database.

.. ghc-flag:: -distrust-all-packages
    :shortdesc: Distrust all packages by default. See :ref:`safe-haskell`.
    :type: dynamic
    :category: packages

    Considers all packages distrusted unless they are
    explicitly set to be trusted by subsequent command-line options.

To set a package's trust property in the package database please refer
to :ref:`packages`.

.. _safe-inference:

Safe Haskell Inference
----------------------

.. index::
   single: safe inference

In the case where a module is compiled without one of :extension:`Safe`,
:extension:`Trustworthy` or :extension:`Unsafe` being used, GHC will try to figure out
itself if the module can be considered safe. This safety inference will
never mark a module as trustworthy, only as either unsafe or as safe.
GHC uses a simple method to determine this for a module M: If M would
compile without error under the :extension:`Safe` flag, then M is marked as
safe. Otherwise, it is marked as unsafe.

When should you use Safe Haskell inference and when should you use an
explicit :extension:`Safe` flag? The later case should be used when you have a
hard requirement that the module be safe. This is most useful for the
:ref:`safe-use-cases` of Safe Haskell: running untrusted code. Safe
inference is meant to be used by ordinary Haskell programmers. Users who
probably don't care about Safe Haskell.

Haskell library authors have a choice. Most should just use Safe
inference. Assuming you avoid any unsafe features of the language then
your modules will be marked safe. Inferred vs. Explicit has the
following trade-offs:

- *Inferred* — This works well and adds no dependencies on the Safe Haskell type
  of any modules in other packages. It does mean that the Safe Haskell type of
  your own modules could change without warning if a dependency changes. One
  way to deal with this is through the use of :ref:`Safe Haskell warning flags
  <safe-flag-summary>` that will warn if GHC infers a Safe Haskell type
  different from expected.

- *Explicit* — This gives your library a stable Safe Haskell type that others
  can depend on. However, it will increase the chance of compilation failure
  when your package dependencies change.

.. _safe-flag-summary:

Safe Haskell Flag Summary
-------------------------

.. index::
   single: Safe Haskell flags

In summary, Safe Haskell consists of the following three language flags:

.. extension:: Safe
    :shortdesc: Enable the :ref:`Safe Haskell <safe-haskell>` Safe mode.

    :since: 7.2.1

    Restricts the module to the safe language. All of the module's
    direct imports must be trusted, but the module itself need not
    reside in a trusted package, because the compiler vouches for its
    trustworthiness. The "safe" keyword is allowed but meaningless in
    import statements, as regardless, every import is required to be
    safe.

    - *Module Trusted* — Yes
    - *Haskell Language* — Restricted to Safe Language
    - *Imported Modules* — All forced to be safe imports, all must be trusted.

.. extension:: Trustworthy
    :shortdesc: Enable the :ref:`Safe Haskell <safe-haskell>` Trustworthy mode.

    :since: 7.2.1

    This establishes that the module is trusted, but the guarantee is
    provided by the module's author. A client of this module then
    specifies that they trust the module author by specifying they trust
    the package containing the module. :extension:`Trustworthy` doesn't restrict the
    module to the safe language. It does however restrict the resolution of
    overlapping instances to only allow :ref:`safe overlapping instances
    <safe-overlapping-instances>`. It also allows the use of the safe import
    keyword.

    - *Module Trusted*  — Yes.
    - *Module Trusted*  (:ghc-flag:`-fpackage-trust` enabled) — Yes but only if the package
      the module resides in is also trusted.
    - *Haskell Language*  — Unrestricted, except only safe overlapping instances
      allowed.
    - *Imported Modules* — Under control of module author which ones must be
      trusted.

.. extension:: Unsafe
    :shortdesc: Enable :ref:`Safe Haskell <safe-haskell>` Unsafe mode.

    :since: 7.4.1

    Mark a module as unsafe so that it can't be imported by code
    compiled with :extension:`Safe`. Also enable the Safe Import extension so that a
    module can require
    a dependency to be trusted.

    - *Module Trusted* — No
    - *Haskell Language* — Unrestricted
    - *Imported Modules* — Under control of module author which ones must be
      trusted.

A flag to disable Safe Haskell checks:

.. ghc-flag:: -fno-safe-haskell
    :shortdesc: Disable :ref:`Safe Haskell <safe-haskell>`
    :type: dynamic

    This flag can be enabled to override any declared safety property of the
    module (Safe, Unsafe, Trustworthy) so compilation proceeds as if none of
    these flags were specified. This is particularly useful when compiling
    using plugins, which usually results in the compiled modules being marked
    as unsafe.

And one general flag:

.. ghc-flag:: -fpackage-trust
    :shortdesc: Enable :ref:`Safe Haskell <safe-haskell>` trusted package
        requirement for trustworthy modules.
    :type: dynamic
    :category: packages

    When enabled, turn on an extra check for a trustworthy module ``M``,
    requiring the package that ``M`` resides in be considered trusted, for ``M``
    to be considered trusted.

And five warning flags:

.. ghc-flag:: -Wunsafe
    :shortdesc: warn if the module being compiled is regarded to be unsafe.
        See :ref:`safe-haskell`
    :type: dynamic
    :reverse: -Wno-unsafe
    :category: warnings

    Issue a warning if the module being compiled is regarded to be
    unsafe. Should be used to check the safety type of modules when
    using safe inference.

.. ghc-flag:: -Wsafe
    :shortdesc: warn if the module being compiled is regarded to be safe.
    :type: dynamic
    :reverse: -Wno-safe
    :category: warnings

    Issue a warning if the module being compiled is regarded to be safe.
    Should be used to check the safety type of modules when using safe
    inference. If the module is explicitly marked as safe then no warning will
    be issued.

.. ghc-flag:: -Wtrustworthy-safe
    :shortdesc: warn if the module being compiled is marked as
        :extension:`Trustworthy` but it could instead be marked as
        :extension:`Safe`, a more informative bound.
    :type: dynamic
    :reverse: -Wno-safe
    :category: warnings

    Issue a warning if the module being compiled is marked as
    -XTrustworthy but it could instead be marked as
    -XSafe , a more informative bound. Can be used to detect once a Safe Haskell
    bound can be improved as dependencies are updated.

.. ghc-flag:: -Winferred-safe-imports
    :shortdesc: warn when an explicitly Safe Haskell module imports a Safe-Inferred one
    :type: dynamic
    :reverse: -Wno-inferred-safe-imports
    :category: warnings

    :since: 8.10.1

    .. index::
       single: safe haskell imports, warning

    The module ``A`` below is annotated to be explicitly ``Safe``, but it imports
    ``Safe-Inferred`` module. ::

        {-# LANGUAGE Safe #-}
        module A where

        import B (double)

        quad :: Int -> Int
        quad = double . double

    
        module B where

        double :: Int -> Int
        double n = n + n

    The inferred status is volatile: if an unsafe import is added to the module
    ``B``, it will cause compilation error of ``A``.  When
    :ghc-flag:`-Winferred-safe-imports` is enabled, the compiler will emit a
    warning about this.
    This option is off by default.

.. ghc-flag:: -Wmissing-safe-haskell-mode
    :shortdesc: warn when the Safe Haskell mode is not explicitly specified.
    :type: dynamic
    :reverse: -Wno-missing-safe-haskell-mode
    :category: warnings

    :since: 8.10.1

    .. index::
       single: safe haskell mode, missing

    The compiler will warn when none of  :extension:`Safe`,
    :extension:`Trustworthy` or :extension:`Unsafe` is specified.
    This option is off by default.

.. _safe-compilation:

Safe Compilation
----------------

.. index::
   single: safe compilation

GHC includes a variety of flags that allow arbitrary processes to be run
at compilation time. One such example is the
:ref:`custom pre-processor <pre-processor>` flag. Another is the ability of
Template Haskell to execute Haskell code at compilation time, including
IO actions. Safe Haskell *does not address this danger* (although,
Template Haskell is a disallowed feature).

Due to this, it is suggested that when compiling untrusted source code
that has had no manual inspection done, the following precautions be
taken:

-  Compile in a sandbox, such as a chroot or similar container
   technology. Or simply as a user with very reduced system access.

-  Compile untrusted code with the ``-XSafe``
   flag being specified on the command line. This will ensure that
   modifications to the source being compiled can't disable the use of
   the Safe Language as the command line flag takes precedence over a
   source level pragma.

-  Ensure that all untrusted code is imported as a
   :ref:`safe import <safe-imports>` and that the :ghc-flag:`-fpackage-trust`
   flag (see :ref:`flag <safe-package-trust>`) is used with packages from
   untrusted sources being marked as untrusted.

There is a more detailed discussion of the issues involved in
compilation safety and some potential solutions on the
:ghc-wiki:`GHC Wiki <safe-haskell/safe-compilation>`.

Additionally, the use of :ref:`annotations <annotation-pragmas>` is forbidden,
as that would allow bypassing Safe Haskell restrictions. See :ghc-ticket:`10826`
for details.
