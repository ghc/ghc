Quickstart
==========

Lets assume we have created a project directory and already have a
Haskell module or two.

Every project needs a name, we'll call this example "proglet".

.. highlight:: console

::

    $ cd proglet/
    $ ls
    Proglet.hs

It is assumed that (apart from external dependencies) all the files that
make up a package live under a common project root directory. This
simple example has all the project files in one directory, but most
packages will use one or more subdirectories.

To turn this into a Cabal package we need two extra files in the
project's root directory:

-  ``proglet.cabal``: containing package metadata and build information.

-  ``Setup.hs``: usually containing a few standardized lines of code,
   but can be customized if necessary.

We can create both files manually or we can use ``cabal init`` to create
them for us.

Using "cabal init"
------------------

The ``cabal init`` command is interactive. It asks us a number of
questions starting with the package name and version.

::

    $ cabal init
    Package name [default "proglet"]?
    Package version [default "0.1"]?
    ...

It also asks questions about various other bits of package metadata. For
a package that you never intend to distribute to others, these fields
can be left blank.

One of the important questions is whether the package contains a library
or an executable. Libraries are collections of Haskell modules that can
be re-used by other Haskell libraries and programs, while executables
are standalone programs.

::

    What does the package build:
       1) Library
       2) Executable
    Your choice?

For the moment these are the only choices. For more complex packages
(e.g. a library and multiple executables or test suites) the ``.cabal``
file can be edited afterwards.

Finally, ``cabal init`` creates the initial ``proglet.cabal`` and
``Setup.hs`` files, and depending on your choice of license, a
``LICENSE`` file as well.

::

    Generating LICENSE...
    Generating Setup.hs...
    Generating proglet.cabal...

    You may want to edit the .cabal file and add a Description field.

As this stage the ``proglet.cabal`` is not quite complete and before you
are able to build the package you will need to edit the file and add
some build information about the library or executable.

Editing the .cabal file
-----------------------

.. highlight:: cabal

Load up the ``.cabal`` file in a text editor. The first part of the
``.cabal`` file has the package metadata and towards the end of the file
you will find the :pkg-section:`executable` or :pkg-section:`library` section.

You will see that the fields that have yet to be filled in are commented
out. Cabal files use "``--``" Haskell-style comment syntax. (Note that
comments are only allowed on lines on their own. Trailing comments on
other lines are not allowed because they could be confused with program
options.)

If you selected earlier to create a library package then your ``.cabal``
file will have a section that looks like this:

::

    library
      exposed-modules:     Proglet
      -- other-modules:
      -- build-depends:

Alternatively, if you selected an executable then there will be a
section like:

::

    executable proglet
      -- main-is:
      -- other-modules:
      -- build-depends:

The build information fields listed (but commented out) are just the few
most important and common fields. There are many others that are covered
later in this chapter.

Most of the build information fields are the same between libraries and
executables. The difference is that libraries have a number of "exposed"
modules that make up the public interface of the library, while
executables have a file containing a ``Main`` module.

The name of a library always matches the name of the package, so it is
not specified in the library section. Executables often follow the name
of the package too, but this is not required and the name is given
explicitly.

Modules included in the package
-------------------------------

For a library, ``cabal init`` looks in the project directory for files
that look like Haskell modules and adds all the modules to the
:pkg-field:`library:exposed-modules` field. For modules that do not form part
of your package's public interface, you can move those modules to the
:pkg-field:`other-modules` field. Either way, all modules in the library need
to be listed.

For an executable, ``cabal init`` does not try to guess which file
contains your program's ``Main`` module. You will need to fill in the
:pkg-field:`executable:main-is` field with the file name of your program's
``Main`` module (including ``.hs`` or ``.lhs`` extension). Other modules
included in the executable should be listed in the :pkg-field:`other-modules`
field.

Modules imported from other packages
------------------------------------

While your library or executable may include a number of modules, it
almost certainly also imports a number of external modules from the
standard libraries or other pre-packaged libraries. (These other
libraries are of course just Cabal packages that contain a library.)

You have to list all of the library packages that your library or
executable imports modules from. Or to put it another way: you have to
list all the other packages that your package depends on.

For example, suppose the example ``Proglet`` module imports the module
``Data.Map``. The ``Data.Map`` module comes from the ``containers``
package, so we must list it:

::

    library
      exposed-modules:     Proglet
      other-modules:
      build-depends:       containers, base == 4.*

In addition, almost every package also depends on the ``base`` library
package because it exports the standard ``Prelude`` module plus other
basic modules like ``Data.List``.

You will notice that we have listed ``base == 4.*``. This gives a
constraint on the version of the base package that our package will work
with. The most common kinds of constraints are:

-  ``pkgname >= n``
-  ``pkgname ^>= n`` (since Cabal 2.0)
-  ``pkgname >= n && < m``
-  ``pkgname == n.*`` (since Cabal 1.6)

The last is just shorthand, for example ``base == 4.*`` means exactly
the same thing as ``base >= 4 && < 5``. Please refer to the documentation
on the :pkg-field:`build-depends` field for more information.

Building the package
--------------------

For simple packages that's it! We can now try configuring and building
the package:

.. code-block:: console

    $ cabal configure
    $ cabal build

Assuming those two steps worked then you can also install the package:

.. code-block:: console

    $ cabal install

For libraries this makes them available for use in GHCi or to be used by
other packages. For executables it installs the program so that you can
run it (though you may first need to adjust your system's ``$PATH``).

Next steps
----------

What we have covered so far should be enough for very simple packages
that you use on your own system.

The next few sections cover more details needed for more complex
packages and details needed for distributing packages to other people.

The previous chapter covers building and installing packages -- your own
packages or ones developed by other people.

Package concepts
================

Before diving into the details of writing packages it helps to
understand a bit about packages in the Haskell world and the particular
approach that Cabal takes.

The point of packages
---------------------

Packages are a mechanism for organising and distributing code. Packages
are particularly suited for "programming in the large", that is building
big systems by using and re-using code written by different people at
different times.

People organise code into packages based on functionality and
dependencies. Social factors are also important: most packages have a
single author, or a relatively small team of authors.

Packages are also used for distribution: the idea is that a package can
be created in one place and be moved to a different computer and be
usable in that different environment. There are a surprising number of
details that have to be got right for this to work, and a good package
system helps to simply this process and make it reliable.

Packages come in two main flavours: libraries of reusable code, and
complete programs. Libraries present a code interface, an API, while
programs can be run directly. In the Haskell world, library packages
expose a set of Haskell modules as their public interface. Cabal
packages can contain a library or executables or both.

Some programming languages have packages as a builtin language concept.
For example in Java, a package provides a local namespace for types and
other definitions. In the Haskell world, packages are not a part of the
language itself. Haskell programs consist of a number of modules, and
packages just provide a way to partition the modules into sets of
related functionality. Thus the choice of module names in Haskell is
still important, even when using packages.

Package names and versions
--------------------------

All packages have a name, e.g. "HUnit". Package names are assumed to be
unique. Cabal package names may contain letters, numbers and hyphens,
but not spaces and may also not contain a hyphened section consisting of
only numbers. The namespace for Cabal packages is flat, not
hierarchical.

Packages also have a version, e.g "1.1". This matches the typical way in
which packages are developed. Strictly speaking, each version of a
package is independent, but usually they are very similar. Cabal package
versions follow the conventional numeric style, consisting of a sequence
of digits such as "1.0.1" or "2.0". There are a range of common
conventions for "versioning" packages, that is giving some meaning to
the version number in terms of changes in the package, such as
e.g. `SemVer <http://semver.org>`__; however, for packages intended to be
distributed via Hackage Haskell's `Package Versioning Policy`_ applies
(see also the `PVP/SemVer FAQ section <https://pvp.haskell.org/faq/#semver>`__).

The combination of package name and version is called the *package ID*
and is written with a hyphen to separate the name and version, e.g.
"HUnit-1.1".

For Cabal packages, the combination of the package name and version
*uniquely* identifies each package. Or to put it another way: two
packages with the same name and version are considered to *be* the same.

Strictly speaking, the package ID only identifies each Cabal *source*
package; the same Cabal source package can be configured and built in
different ways. There is a separate installed package ID that uniquely
identifies each installed package instance. Most of the time however,
users need not be aware of this detail.

Kinds of package: Cabal vs GHC vs system
----------------------------------------

It can be slightly confusing at first because there are various
different notions of package floating around. Fortunately the details
are not very complicated.

Cabal packages
    Cabal packages are really source packages. That is they contain
    Haskell (and sometimes C) source code.

    Cabal packages can be compiled to produce GHC packages. They can
    also be translated into operating system packages.

GHC packages
    This is GHC's view on packages. GHC only cares about library
    packages, not executables. Library packages have to be registered
    with GHC for them to be available in GHCi or to be used when
    compiling other programs or packages.

    The low-level tool ``ghc-pkg`` is used to register GHC packages and
    to get information on what packages are currently registered.

    You never need to make GHC packages manually. When you build and
    install a Cabal package containing a library then it gets registered
    with GHC automatically.

    Haskell implementations other than GHC have essentially the same
    concept of registered packages. For the most part, Cabal hides the
    slight differences.

Operating system packages
    On operating systems like Linux and Mac OS X, the system has a
    specific notion of a package and there are tools for installing and
    managing packages.

    The Cabal package format is designed to allow Cabal packages to be
    translated, mostly-automatically, into operating system packages.
    They are usually translated 1:1, that is a single Cabal package
    becomes a single system package.

    It is also possible to make Windows installers from Cabal packages,
    though this is typically done for a program together with all of its
    library dependencies, rather than packaging each library separately.

Unit of distribution
--------------------

The Cabal package is the unit of distribution. What this means is that
each Cabal package can be distributed on its own in source or binary
form. Of course there may dependencies between packages, but there is
usually a degree of flexibility in which versions of packages can work
together so distributing them independently makes sense.

It is perhaps easiest to see what being "the unit of distribution"
means by contrast to an alternative approach. Many projects are made up
of several interdependent packages and during development these might
all be kept under one common directory tree and be built and tested
together. When it comes to distribution however, rather than
distributing them all together in a single tarball, it is required that
they each be distributed independently in their own tarballs.

Cabal's approach is to say that if you can specify a dependency on a
package then that package should be able to be distributed
independently. Or to put it the other way round, if you want to
distribute it as a single unit, then it should be a single package.

Explicit dependencies and automatic package management
------------------------------------------------------

Cabal takes the approach that all packages dependencies are specified
explicitly and specified in a declarative way. The point is to enable
automatic package management. This means tools like ``cabal`` can
resolve dependencies and install a package plus all of its dependencies
automatically. Alternatively, it is possible to mechanically (or mostly
mechanically) translate Cabal packages into system packages and let the
system package manager install dependencies automatically.

It is important to track dependencies accurately so that packages can
reliably be moved from one system to another system and still be able to
build it there. Cabal is therefore relatively strict about specifying
dependencies. For example Cabal's default build system will not even let
code build if it tries to import a module from a package that isn't
listed in the ``.cabal`` file, even if that package is actually
installed. This helps to ensure that there are no "untracked
dependencies" that could cause the code to fail to build on some other
system.

The explicit dependency approach is in contrast to the traditional
"./configure" approach where instead of specifying dependencies
declaratively, the ``./configure`` script checks if the dependencies are
present on the system. Some manual work is required to transform a
``./configure`` based package into a Linux distribution package (or
similar). This conversion work is usually done by people other than the
package author(s). The practical effect of this is that only the most
popular packages will benefit from automatic package management.
Instead, Cabal forces the original author to specify the dependencies
but the advantage is that every package can benefit from automatic
package management.

The "./configure" approach tends to encourage packages that adapt
themselves to the environment in which they are built, for example by
disabling optional features so that they can continue to work when a
particular dependency is not available. This approach makes sense in a
world where installing additional dependencies is a tiresome manual
process and so minimising dependencies is important. The automatic
package management view is that packages should just declare what they
need and the package manager will take responsibility for ensuring that
all the dependencies are installed.

Sometimes of course optional features and optional dependencies do make
sense. Cabal packages can have optional features and varying
dependencies. These conditional dependencies are still specified in a
declarative way however and remain compatible with automatic package
management. The need to remain compatible with automatic package
management means that Cabal's conditional dependencies system is a bit
less flexible than with the "./configure" approach.

Portability
-----------

One of the purposes of Cabal is to make it easier to build packages on
different platforms (operating systems and CPU architectures), with
different compiler versions and indeed even with different Haskell
implementations. (Yes, there are Haskell implementations other than
GHC!)

Cabal provides abstractions of features present in different Haskell
implementations and wherever possible it is best to take advantage of
these to increase portability. Where necessary however it is possible to
use specific features of specific implementations.

For example a package author can list in the package's ``.cabal`` what
language extensions the code uses. This allows Cabal to figure out if
the language extension is supported by the Haskell implementation that
the user picks. Additionally, certain language extensions such as
Template Haskell require special handling from the build system and by
listing the extension it provides the build system with enough
information to do the right thing.

Another similar example is linking with foreign libraries. Rather than
specifying GHC flags directly, the package author can list the libraries
that are needed and the build system will take care of using the right
flags for the compiler. Additionally this makes it easier for tools to
discover what system C libraries a package needs, which is useful for
tracking dependencies on system libraries (e.g. when translating into
Linux distribution packages).

In fact both of these examples fall into the category of explicitly
specifying dependencies. Not all dependencies are other Cabal packages.
Foreign libraries are clearly another kind of dependency. It's also
possible to think of language extensions as dependencies: the package
depends on a Haskell implementation that supports all those extensions.

Where compiler-specific options are needed however, there is an "escape
hatch" available. The developer can specify implementation-specific
options and more generally there is a configuration mechanism to
customise many aspects of how a package is built depending on the
Haskell implementation, the operating system, computer architecture and
user-specified configuration flags.

Developing packages
===================

The Cabal package is the unit of distribution. When installed, its
purpose is to make available:

-  One or more Haskell programs.

-  At most one library, exposing a number of Haskell modules.

However having both a library and executables in a package does not work
very well; if the executables depend on the library, they must
explicitly list all the modules they directly or indirectly import from
that library. Fortunately, starting with Cabal 1.8.0.4, executables can
also declare the package that they are in as a dependency, and Cabal
will treat them as if they were in another package that depended on the
library.

Internally, the package may consist of much more than a bunch of Haskell
modules: it may also have C source code and header files, source code
meant for preprocessing, documentation, test cases, auxiliary tools etc.

A package is identified by a globally-unique *package name*, which
consists of one or more alphanumeric words separated by hyphens. To
avoid ambiguity, each of these words should contain at least one letter.
Chaos will result if two distinct packages with the same name are
installed on the same system. A particular version of the package is
distinguished by a *version number*, consisting of a sequence of one or
more integers separated by dots. These can be combined to form a single
text string called the *package ID*, using a hyphen to separate the name
from the version, e.g. "``HUnit-1.1``".

.. Note::

   Packages are not part of the Haskell language; they simply
   populate the hierarchical space of module names. In GHC 6.6 and later a
   program may contain multiple modules with the same name if they come
   from separate packages; in all other current Haskell systems packages
   may not overlap in the modules they provide, including hidden modules.

Creating a package
------------------

Suppose you have a directory hierarchy containing the source files that
make up your package. You will need to add two more files to the root
directory of the package:

:file:`{package-name}.cabal`
    a Unicode UTF-8 text file containing a package description. For
    details of the syntax of this file, see the section on
    `package descriptions`_.

:file:`Setup.hs`
    a single-module Haskell program to perform various setup tasks (with
    the interface described in the section on :ref:`installing-packages`).
    This module should import only modules that will be present in all Haskell
    implementations, including modules of the Cabal library. The content of
    this file is determined by the :pkg-field:`build-type` setting in the
    ``.cabal`` file. In most cases it will be trivial, calling on the Cabal
    library to do most of the work.

Once you have these, you can create a source bundle of this directory
for distribution. Building of the package is discussed in the section on
:ref:`installing-packages`.

One of the purposes of Cabal is to make it easier to build a package
with different Haskell implementations. So it provides abstractions of
features present in different Haskell implementations and wherever
possible it is best to take advantage of these to increase portability.
Where necessary however it is possible to use specific features of
specific implementations. For example one of the pieces of information a
package author can put in the package's ``.cabal`` file is what language
extensions the code uses. This is far preferable to specifying flags for
a specific compiler as it allows Cabal to pick the right flags for the
Haskell implementation that the user picks. It also allows Cabal to
figure out if the language extension is even supported by the Haskell
implementation that the user picks. Where compiler-specific options are
needed however, there is an "escape hatch" available. The developer can
specify implementation-specific options and more generally there is a
configuration mechanism to customise many aspects of how a package is
built depending on the Haskell implementation, the Operating system,
computer architecture and user-specified configuration flags.

::

    name:     Foo
    version:  1.0

    library
      build-depends:   base >= 4 && < 5
      exposed-modules: Foo
      extensions:      ForeignFunctionInterface
      ghc-options:     -Wall
      if os(windows)
        build-depends: Win32 >= 2.1 && < 2.6

Example: A package containing a simple library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The HUnit package contains a file ``HUnit.cabal`` containing:

::

    name:           HUnit
    version:        1.1.1
    synopsis:       A unit testing framework for Haskell
    homepage:       http://hunit.sourceforge.net/
    category:       Testing
    author:         Dean Herington
    license:        BSD3
    license-file:   LICENSE
    cabal-version:  >= 1.10
    build-type:     Simple

    library
      build-depends:      base >= 2 && < 4
      exposed-modules:    Test.HUnit.Base, Test.HUnit.Lang,
                          Test.HUnit.Terminal, Test.HUnit.Text, Test.HUnit
      default-extensions: CPP

and the following ``Setup.hs``:

.. code-block:: haskell

    import Distribution.Simple
    main = defaultMain

Example: A package containing executable programs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    name:           TestPackage
    version:        0.0
    synopsis:       Small package with two programs
    author:         Angela Author
    license:        BSD3
    build-type:     Simple
    cabal-version:  >= 1.2

    executable program1
      build-depends:  HUnit >= 1.1.1 && < 1.2
      main-is:        Main.hs
      hs-source-dirs: prog1

    executable program2
      main-is:        Main.hs
      build-depends:  HUnit >= 1.1.1 && < 1.2
      hs-source-dirs: prog2
      other-modules:  Utils

with ``Setup.hs`` the same as above.

Example: A package containing a library and executable programs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

    name:            TestPackage
    version:         0.0
    synopsis:        Package with library and two programs
    license:         BSD3
    author:          Angela Author
    build-type:      Simple
    cabal-version:   >= 1.2

    library
      build-depends:   HUnit >= 1.1.1 && < 1.2
      exposed-modules: A, B, C

    executable program1
      main-is:         Main.hs
      hs-source-dirs:  prog1
      other-modules:   A, B

    executable program2
      main-is:         Main.hs
      hs-source-dirs:  prog2
      other-modules:   A, C, Utils

with ``Setup.hs`` the same as above. Note that any library modules
required (directly or indirectly) by an executable must be listed again.

The trivial setup script used in these examples uses the *simple build
infrastructure* provided by the Cabal library (see
`Distribution.Simple <../release/cabal-latest/doc/API/Cabal/Distribution-Simple.html>`__).
The simplicity lies in its interface rather that its implementation. It
automatically handles preprocessing with standard preprocessors, and
builds packages for all the Haskell implementations.

The simple build infrastructure can also handle packages where building
is governed by system-dependent parameters, if you specify a little more
(see the section on `system-dependent parameters`_).
A few packages require `more elaborate solutions <more complex packages>`_.

Package descriptions
--------------------

The package description file must have a name ending in "``.cabal``". It
must be a Unicode text file encoded using valid UTF-8. There must be
exactly one such file in the directory. The first part of the name is
usually the package name, and some of the tools that operate on Cabal
packages require this; specifically, Hackage rejects packages which
don't follow this rule.

In the package description file, lines whose first non-whitespace
characters are "``--``" are treated as comments and ignored.

This file should contain of a number global property descriptions and
several sections.

-  The `package properties`_ describe the package
   as a whole, such as name, license, author, etc.

-  Optionally, a number of *configuration flags* can be declared. These
   can be used to enable or disable certain features of a package. (see
   the section on `configurations`_).

-  The (optional) library section specifies the `library`_ properties and
   relevant `build information`_.

-  Following is an arbitrary number of executable sections which describe
   an executable program and relevant `build information`_.

Each section consists of a number of property descriptions in the form
of field/value pairs, with a syntax roughly like mail message headers.

-  Case is not significant in field names, but is significant in field
   values.

-  To continue a field value, indent the next line relative to the field
   name.

-  Field names may be indented, but all field values in the same section
   must use the same indentation.

-  Tabs are *not* allowed as indentation characters due to a missing
   standard interpretation of tab width.

-  To get a blank line in a field value, use an indented "``.``"

The syntax of the value depends on the field. Field types include:

*token*, *filename*, *directory*
    Either a sequence of one or more non-space non-comma characters, or
    a quoted string in Haskell 98 lexical syntax. The latter can be used
    for escaping whitespace, for example:
    ``ghc-options: -Wall "-with-rtsopts=-T -I1"``. Unless otherwise
    stated, relative filenames and directories are interpreted from the
    package root directory.
*freeform*, *URL*, *address*
    An arbitrary, uninterpreted string.
*identifier*
    A letter followed by zero or more alphanumerics or underscores.
*compiler*
    A compiler flavor (one of: ``GHC``, ``JHC``, ``UHC`` or ``LHC``)
    followed by a version range. For example, ``GHC ==6.10.3``, or
    ``LHC >=0.6 && <0.8``.

Modules and preprocessors
^^^^^^^^^^^^^^^^^^^^^^^^^

Haskell module names listed in the :pkg-field:`library:exposed-modules` and
:pkg-field:`library:other-modules` fields may correspond to Haskell source
files, i.e. with names ending in "``.hs``" or "``.lhs``", or to inputs for
various Haskell preprocessors. The simple build infrastructure understands the
extensions:

-  ``.gc`` (:hackage-pkg:`greencard`)
-  ``.chs`` (:hackage-pkg:`c2hs`)
-  ``.hsc`` (:hackage-pkg:`hsc2hs`)
-  ``.y`` and ``.ly`` (happy_)
-  ``.x`` (alex_)
-  ``.cpphs`` (cpphs_)

When building, Cabal will automatically run the appropriate preprocessor
and compile the Haskell module it produces. For the ``c2hs`` and
``hsc2hs`` preprocessors, Cabal will also automatically add, compile and
link any C sources generated by the preprocessor (produced by
``hsc2hs``'s ``#def`` feature or ``c2hs``'s auto-generated wrapper
functions). Dependencies on pre-processors are specified via the
:pkg-field:`build-tools` or :pkg-field:`build-tool-depends` fields.

Some fields take lists of values, which are optionally separated by
commas, except for the :pkg-field:`build-depends` field, where the commas are
mandatory.

Some fields are marked as required. All others are optional, and unless
otherwise specified have empty default values.

Package properties
^^^^^^^^^^^^^^^^^^

These fields may occur in the first top-level properties section and
describe the package as a whole:

.. pkg-field:: name: package-name (required)

    The unique name of the package, without the version number.

    As pointed out in the section on `package descriptions`_, some
    tools require the package-name specified for this field to match
    the package description's file-name :file:`{package-name}.cabal`.

.. pkg-field:: version: numbers (required)

    The package version number, usually consisting of a sequence of
    natural numbers separated by dots.

.. pkg-field:: cabal-version: >= x.y

    The version of the Cabal specification that this package description
    uses. The Cabal specification does slowly evolve, introducing new
    features and occasionally changing the meaning of existing features.
    By specifying which version of the spec you are using it enables
    programs which process the package description to know what syntax
    to expect and what each part means.

    For historical reasons this is always expressed using *>=* version
    range syntax. No other kinds of version range make sense, in
    particular upper bounds do not make sense. In future this field will
    specify just a version number, rather than a version range.

    The version number you specify will affect both compatibility and
    behaviour. Most tools (including the Cabal library and cabal
    program) understand a range of versions of the Cabal specification.
    Older tools will of course only work with older versions of the
    Cabal specification. Most of the time, tools that are too old will
    recognise this fact and produce a suitable error message.

    As for behaviour, new versions of the Cabal spec can change the
    meaning of existing syntax. This means if you want to take advantage
    of the new meaning or behaviour then you must specify the newer
    Cabal version. Tools are expected to use the meaning and behaviour
    appropriate to the version given in the package description.

    In particular, the syntax of package descriptions changed
    significantly with Cabal version 1.2 and the :pkg-field:`cabal-version`
    field is now required. Files written in the old syntax are still
    recognized, so if you require compatibility with very old Cabal
    versions then you may write your package description file using the
    old syntax. Please consult the user's guide of an older Cabal
    version for a description of that syntax.

.. pkg-field:: build-type: identifier

    :default: ``Custom``

    The type of build used by this package. Build types are the
    constructors of the
    `BuildType <../release/cabal-latest/doc/API/Cabal/Distribution-PackageDescription.html#t:BuildType>`__
    type, defaulting to ``Custom``.

    If the build type is anything other than ``Custom``, then the
    ``Setup.hs`` file *must* be exactly the standardized content
    discussed below. This is because in these cases, ``cabal`` will
    ignore the ``Setup.hs`` file completely, whereas other methods of
    package management, such as ``runhaskell Setup.hs [CMD]``, still
    rely on the ``Setup.hs`` file.

    For build type ``Simple``, the contents of ``Setup.hs`` must be:

    .. code-block:: haskell

        import Distribution.Simple
        main = defaultMain

    For build type ``Configure`` (see the section on `system-dependent
    parameters`_ below), the contents of
    ``Setup.hs`` must be:

    .. code-block:: haskell

        import Distribution.Simple
        main = defaultMainWithHooks autoconfUserHooks

    For build type ``Make`` (see the section on `more complex packages`_ below),
    the contents of ``Setup.hs`` must be:

    .. code-block:: haskell

        import Distribution.Make
        main = defaultMain

    For build type ``Custom``, the file ``Setup.hs`` can be customized,
    and will be used both by ``cabal`` and other tools.

    For most packages, the build type ``Simple`` is sufficient.

.. pkg-field:: license: identifier

    :default: ``AllRightsReserved``

    The type of license under which this package is distributed. License
    names are the constants of the
    `License <../release/cabal-latest/doc/API/Cabal/Distribution-License.html#t:License>`__
    type.

.. pkg-field:: license-file: filename
.. pkg-field:: license-files: filename list

    The name of a file(s) containing the precise copyright license for
    this package. The license file(s) will be installed with the
    package.

    If you have multiple license files then use the :pkg-field:`license-files`
    field instead of (or in addition to) the :pkg-field:`license-file` field.

.. pkg-field:: copyright: freeform

    The content of a copyright notice, typically the name of the holder
    of the copyright on the package and the year(s) from which copyright
    is claimed. For example::

      copyright: (c) 2006-2007 Joe Bloggs

.. pkg-field:: author: freeform

    The original author of the package.

    Remember that ``.cabal`` files are Unicode, using the UTF-8
    encoding.

.. pkg-field:: maintainer: address

    The current maintainer or maintainers of the package. This is an
    e-mail address to which users should send bug reports, feature
    requests and patches.

.. pkg-field:: stability: freeform

    The stability level of the package, e.g. ``alpha``,
    ``experimental``, ``provisional``, ``stable``.

.. pkg-field:: homepage: URL

    The package homepage.

.. pkg-field:: bug-reports: URL

    The URL where users should direct bug reports. This would normally
    be either:

    -  A ``mailto:`` URL, e.g. for a person or a mailing list.

    -  An ``http:`` (or ``https:``) URL for an online bug tracking
       system.

    For example Cabal itself uses a web-based bug tracking system

    ::

        bug-reports: https://github.com/haskell/cabal/issues

.. pkg-field:: package-url: URL

    The location of a source bundle for the package. The distribution
    should be a Cabal package.

.. pkg-field:: synopsis: freeform

    A very short description of the package, for use in a table of
    packages. This is your headline, so keep it short (one line) but as
    informative as possible. Save space by not including the package
    name or saying it's written in Haskell.

.. pkg-field:: description: freeform

    Description of the package. This may be several paragraphs, and
    should be aimed at a Haskell programmer who has never heard of your
    package before.

    For library packages, this field is used as prologue text by
    :ref:`setup-haddock` and thus may contain the same markup as Haddock_
    documentation comments.

.. pkg-field:: category: freeform

    A classification category for future use by the package catalogue
    Hackage_. These categories have not
    yet been specified, but the upper levels of the module hierarchy
    make a good start.

.. pkg-field:: tested-with: compiler list

    A list of compilers and versions against which the package has been
    tested (or at least built).

.. pkg-field:: data-files: filename list

    A list of files to be installed for run-time use by the package.
    This is useful for packages that use a large amount of static data,
    such as tables of values or code templates. Cabal provides a way to
    `find these files at run-time <accessing data files from package code>`_.

    A limited form of ``*`` wildcards in file names, for example
    ``data-files: images/*.png`` matches all the ``.png`` files in the
    ``images`` directory.

    The limitation is that ``*`` wildcards are only allowed in place of
    the file name, not in the directory name or file extension. In
    particular, wildcards do not include directories contents
    recursively. Furthermore, if a wildcard is used it must be used with
    an extension, so ``data-files: data/*`` is not allowed. When
    matching a wildcard plus extension, a file's full extension must
    match exactly, so ``*.gz`` matches ``foo.gz`` but not
    ``foo.tar.gz``. A wildcard that does not match any files is an
    error.

    The reason for providing only a very limited form of wildcard is to
    concisely express the common case of a large number of related files
    of the same file type without making it too easy to accidentally
    include unwanted files.

.. pkg-field:: data-dir: directory

    The directory where Cabal looks for data files to install, relative
    to the source directory. By default, Cabal will look in the source
    directory itself.

.. pkg-field:: extra-source-files: filename list

    A list of additional files to be included in source distributions
    built with :ref:`setup-sdist`. As with :pkg-field:`data-files` it can use
    a limited form of ``*`` wildcards in file names.

.. pkg-field:: extra-doc-files: filename list

    A list of additional files to be included in source distributions,
    and also copied to the html directory when Haddock documentation is
    generated. As with :pkg-field:`data-files` it can use a limited form of
    ``*`` wildcards in file names.

.. pkg-field:: extra-tmp-files: filename list

    A list of additional files or directories to be removed by
    :ref:`setup-clean`. These  would typically be additional files created by
    additional hooks, such as the scheme described in the section on
    `system-dependent parameters`_

Library
^^^^^^^

.. pkg-section:: library
    :synopsis: Library build information.

    Build information for libraries. There can be only one library in a
    package, and it's name is the same as package name set by global
    :pkg-field:`name` field.

The library section should contain the following fields:

.. pkg-field:: exposed-modules: identifier list

    :required: if this package contains a library

    A list of modules added by this package.

.. pkg-field:: virtual-modules: identifier list

    A list of virtual modules provided by this package.  Virtual modules
    are modules without a source file.  See for example the ``GHC.Prim``
    module from the ``ghc-prim`` package.  Modules listed here will not be
    built, but still end up in the list of ``exposed-modules`` in the
    installed package info when the package is registered in the package
    database.

.. pkg-field:: exposed: boolean

    :default: ``True``

    Some Haskell compilers (notably GHC) support the notion of packages
    being "exposed" or "hidden" which means the modules they provide can
    be easily imported without always having to specify which package
    they come from. However this only works effectively if the modules
    provided by all exposed packages do not overlap (otherwise a module
    import would be ambiguous).

    Almost all new libraries use hierarchical module names that do not
    clash, so it is very uncommon to have to use this field. However it
    may be necessary to set ``exposed: False`` for some old libraries
    that use a flat module namespace or where it is known that the
    exposed modules would clash with other common modules.

.. pkg-field:: reexported-modules: exportlist

    Supported only in GHC 7.10 and later. A list of modules to
    *reexport* from this package. The syntax of this field is
    ``orig-pkg:Name as NewName`` to reexport module ``Name`` from
    ``orig-pkg`` with the new name ``NewName``. We also support
    abbreviated versions of the syntax: if you omit ``as NewName``,
    we'll reexport without renaming; if you omit ``orig-pkg``, then we
    will automatically figure out which package to reexport from, if
    it's unambiguous.

    Reexported modules are useful for compatibility shims when a package
    has been split into multiple packages, and they have the useful
    property that if a package provides a module, and another package
    reexports it under the same name, these are not considered a
    conflict (as would be the case with a stub module.) They can also be
    used to resolve name conflicts.

The library section may also contain build information fields (see the
section on `build information`_).

Cabal 2.0 and later support "internal libraries", which are extra named
libraries (as opposed to the usual unnamed library section). For
example, suppose that your test suite needs access to some internal
modules in your library, which you do not otherwise want to export. You
could put these modules in an internal library, which the main library
and the test suite :pkg-field:`build-depends` upon. Then your Cabal file might
look something like this:

::

    name:           foo
    version:        1.0
    license:        BSD3
    cabal-version:  >= 1.23
    build-type:     Simple

    library foo-internal
        exposed-modules: Foo.Internal
        -- NOTE: no explicit constraints on base needed
        --       as they're inherited from the 'library' stanza
        build-depends: base

    library
        exposed-modules: Foo.Public
        build-depends: foo-internal, base >= 4.3 && < 5

    test-suite test-foo
        type:       exitcode-stdio-1.0
        main-is:    test-foo.hs
        -- NOTE: no constraints on 'foo-internal' as same-package
        --       dependencies implicitly refer to the same package instance
        build-depends: foo-internal, base

Internal libraries are also useful for packages that define multiple
executables, but do not define a publically accessible library. Internal
libraries are only visible internally in the package (so they can only
be added to the :pkg-field:`build-depends` of same-package libraries,
executables, test suites, etc.) Internal libraries locally shadow any
packages which have the same name (so don't name an internal library
with the same name as an external dependency.)

Opening an interpreter session
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

While developing a package, it is often useful to make its code
available inside an interpreter session. This can be done with the
``repl`` command:

.. code-block:: console

    $ cabal repl

The name comes from the acronym
`REPL <http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop>`__,
which stands for "read-eval-print-loop". By default ``cabal repl`` loads
the first component in a package. If the package contains several named
components, the name can be given as an argument to ``repl``. The name
can be also optionally prefixed with the component's type for
disambiguation purposes. Example:

.. code-block:: console

    $ cabal repl foo
    $ cabal repl exe:foo
    $ cabal repl test:bar
    $ cabal repl bench:baz

Freezing dependency versions
""""""""""""""""""""""""""""

If a package is built in several different environments, such as a
development environment, a staging environment and a production
environment, it may be necessary or desirable to ensure that the same
dependency versions are selected in each environment. This can be done
with the ``freeze`` command:

.. code-block:: console

    $ cabal freeze

The command writes the selected version for all dependencies to the
``cabal.config`` file. All environments which share this file will use
the dependency versions specified in it.

Generating dependency version bounds
""""""""""""""""""""""""""""""""""""

Cabal also has the ability to suggest dependency version bounds that
conform to `Package Versioning Policy`_, which is
a recommended versioning system for publicly released Cabal packages.
This is done by running the ``gen-bounds`` command:

.. code-block:: console

    $ cabal gen-bounds

For example, given the following dependencies specified in
:pkg-field:`build-depends`:

::

    build-depends:
      foo == 0.5.2
      bar == 1.1

``gen-bounds`` will suggest changing them to the following:

::

    build-depends:
      foo >= 0.5.2 && < 0.6
      bar >= 1.1 && < 1.2

Listing outdated dependency version bounds
""""""""""""""""""""""""""""""""""""""""""

Manually updating dependency version bounds in a ``.cabal`` file or a
freeze file can be tedious, especially when there's a lot of
dependencies. The ``cabal outdated`` command is designed to help with
that. It will print a list of packages for which there is a new
version on Hackage that is outside the version bound specified in the
``build-depends`` field. The ``outdated`` command can also be
configured to act on the freeze file (both old- and new-style) and
ignore major (or all) version bumps on Hackage for a subset of
dependencies.

The following flags are supported by the ``outdated`` command:

``--freeze-file``
    Read dependency version bounds from the freeze file (``cabal.config``)
    instead of the package description file (``$PACKAGENAME.cabal``).
``--new-freeze-file``
    Read dependency version bounds from the new-style freeze file
    (``cabal.project.freeze``) instead of the package description file.
``--simple-output``
    Print only the names of outdated dependencies, one per line.
``--exit-code``
    Exit with a non-zero exit code when there are outdated dependencies.
``-q, --quiet``
    Don't print any output. Implies ``-v0`` and ``--exit-code``.
``--ignore`` *PACKAGENAMES*
    Don't warn about outdated dependency version bounds for the packages in this
    list.
``--minor`` *[PACKAGENAMES]*
    Ignore major version bumps for these packages. E.g. if there's a version 2.0
    of a package ``pkg`` on Hackage and the freeze file specifies the constraint
    ``pkg == 1.9``, ``cabal outdated --freeze --minor=pkg`` will only consider
    the ``pkg`` outdated when there's a version of ``pkg`` on Hackage satisfying
    ``pkg > 1.9 && < 2.0``. ``--minor`` can also be used without arguments, in
    that case major version bumps are ignored for all packages.

Examples:

.. code-block:: console

    $ cd /some/package
    $ cabal outdated
    Outdated dependencies:
    haskell-src-exts <1.17 (latest: 1.19.1)
    language-javascript <0.6 (latest: 0.6.0.9)
    unix ==2.7.2.0 (latest: 2.7.2.1)

    $ cabal outdated --simple-output
    haskell-src-exts
    language-javascript
    unix

    $ cabal outdated --ignore=haskell-src-exts
    Outdated dependencies:
    language-javascript <0.6 (latest: 0.6.0.9)
    unix ==2.7.2.0 (latest: 2.7.2.1)

    $ cabal outdated --ignore=haskell-src-exts,language-javascript,unix
    All dependencies are up to date.

    $ cabal outdated --ignore=haskell-src-exts,language-javascript,unix -q
    $ echo $?
    0

    $ cd /some/other/package
    $ cabal outdated --freeze-file
    Outdated dependencies:
    HTTP ==4000.3.3 (latest: 4000.3.4)
    HUnit ==1.3.1.1 (latest: 1.5.0.0)

    $ cabal outdated --freeze-file --ignore=HTTP --minor=HUnit
    Outdated dependencies:
    HUnit ==1.3.1.1 (latest: 1.3.1.2)


Executables
^^^^^^^^^^^

.. pkg-section:: executable name
    :synopsis: Exectuable build info section.

    Executable sections (if present) describe executable programs contained
    in the package and must have an argument after the section label, which
    defines the name of the executable. This is a freeform argument but may
    not contain spaces.

The executable may be described using the following fields, as well as
build information fields (see the section on `build information`_).

.. pkg-field:: main-is: filename (required)

    The name of the ``.hs`` or ``.lhs`` file containing the ``Main``
    module. Note that it is the ``.hs`` filename that must be listed,
    even if that file is generated using a preprocessor. The source file
    must be relative to one of the directories listed in
    :pkg-field:`hs-source-dirs`.

.. pkg-field:: scope: token
    :since: 2.0

    Whether the executable is ``public`` (default) or ``private``, i.e. meant to
    be run by other programs rather than the user. Private executables are
    installed into `$libexecdir/$libexecsubdir`.

Running executables
"""""""""""""""""""

You can have Cabal build and run your executables by using the ``run``
command:

.. code-block:: console

    $ cabal run EXECUTABLE [-- EXECUTABLE_FLAGS]

This command will configure, build and run the executable
``EXECUTABLE``. The double dash separator is required to distinguish
executable flags from ``run``'s own flags. If there is only one
executable defined in the whole package, the executable's name can be
omitted. See the output of ``cabal help run`` for a list of options you
can pass to ``cabal run``.

Test suites
^^^^^^^^^^^

.. pkg-section:: test-suite name
    :synopsis: Test suite build information.

    Test suite sections (if present) describe package test suites and must
    have an argument after the section label, which defines the name of the
    test suite. This is a freeform argument, but may not contain spaces. It
    should be unique among the names of the package's other test suites, the
    package's executables, and the package itself. Using test suite sections
    requires at least Cabal version 1.9.2.

The test suite may be described using the following fields, as well as
build information fields (see the section on `build information`_).

.. pkg-field:: type: interface (required)

    The interface type and version of the test suite. Cabal supports two
    test suite interfaces, called ``exitcode-stdio-1.0`` and
    ``detailed-0.9``. Each of these types may require or disallow other
    fields as described below.

Test suites using the ``exitcode-stdio-1.0`` interface are executables
that indicate test failure with a non-zero exit code when run; they may
provide human-readable log information through the standard output and
error channels. The ``exitcode-stdio-1.0`` type requires the ``main-is``
field.

.. pkg-field:: main-is: filename
    :synopsis: Module containing tests main function.

    :required: ``exitcode-stdio-1.0``
    :disallowed: ``detailed-0.9``

    The name of the ``.hs`` or ``.lhs`` file containing the ``Main``
    module. Note that it is the ``.hs`` filename that must be listed,
    even if that file is generated using a preprocessor. The source file
    must be relative to one of the directories listed in
    :pkg-field:`hs-source-dirs`. This field is analogous to the ``main-is`` field
    of an executable section.

Test suites using the ``detailed-0.9`` interface are modules exporting
the symbol ``tests :: IO [Test]``. The ``Test`` type is exported by the
module ``Distribution.TestSuite`` provided by Cabal. For more details,
see the example below.

The ``detailed-0.9`` interface allows Cabal and other test agents to
inspect a test suite's results case by case, producing detailed human-
and machine-readable log files. The ``detailed-0.9`` interface requires
the :pkg-field:`test-module` field.

.. pkg-field:: test-module: identifier

    :required: ``detailed-0.9``
    :disallowed: ``exitcode-stdio-1.0``

    The module exporting the ``tests`` symbol.

Example: Package using ``exitcode-stdio-1.0`` interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

The example package description and executable source file below
demonstrate the use of the ``exitcode-stdio-1.0`` interface.

.. code-block:: cabal
    :caption: foo.cabal

    Name:           foo
    Version:        1.0
    License:        BSD3
    Cabal-Version:  >= 1.9.2
    Build-Type:     Simple

    Test-Suite test-foo
        type:       exitcode-stdio-1.0
        main-is:    test-foo.hs
        build-depends: base >= 4 && < 5

.. code-block:: haskell
    :caption: test-foo.hs

    module Main where

    import System.Exit (exitFailure)

    main = do
        putStrLn "This test always fails!"
        exitFailure

Example: Package using ``detailed-0.9`` interface
"""""""""""""""""""""""""""""""""""""""""""""""""

The example package description and test module source file below
demonstrate the use of the ``detailed-0.9`` interface. The test module
also develops a simple implementation of the interface set by
``Distribution.TestSuite``, but in actual usage the implementation would
be provided by the library that provides the testing facility.

.. code-block:: cabal
    :caption: bar.cabal

    Name:           bar
    Version:        1.0
    License:        BSD3
    Cabal-Version:  >= 1.9.2
    Build-Type:     Simple

    Test-Suite test-bar
        type:       detailed-0.9
        test-module: Bar
        build-depends: base >= 4 && < 5, Cabal >= 1.9.2 && < 2


.. code-block:: haskell
    :caption: Bar.hs

    module Bar ( tests ) where

    import Distribution.TestSuite

    tests :: IO [Test]
    tests = return [ Test succeeds, Test fails ]
      where
        succeeds = TestInstance
            { run = return $ Finished Pass
            , name = "succeeds"
            , tags = []
            , options = []
            , setOption = \_ _ -> Right succeeds
            }
        fails = TestInstance
            { run = return $ Finished $ Fail "Always fails!"
            , name = "fails"
            , tags = []
            , options = []
            , setOption = \_ _ -> Right fails
            }

Running test suites
"""""""""""""""""""

You can have Cabal run your test suites using its built-in test runner:

::

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test

See the output of ``cabal help test`` for a list of options you can pass
to ``cabal test``.

Benchmarks
^^^^^^^^^^

.. pkg-section:: benchmark name
    :since: 1.9.2
    :synopsis: Benchmark build information.

    Benchmark sections (if present) describe benchmarks contained in the
    package and must have an argument after the section label, which defines
    the name of the benchmark. This is a freeform argument, but may not
    contain spaces. It should be unique among the names of the package's
    other benchmarks, the package's test suites, the package's executables,
    and the package itself. Using benchmark sections requires at least Cabal
    version 1.9.2.

The benchmark may be described using the following fields, as well as
build information fields (see the section on `build information`_).

.. pkg-field:: type: interface (required)

    The interface type and version of the benchmark. At the moment Cabal
    only support one benchmark interface, called ``exitcode-stdio-1.0``.

Benchmarks using the ``exitcode-stdio-1.0`` interface are executables
that indicate failure to run the benchmark with a non-zero exit code
when run; they may provide human-readable information through the
standard output and error channels.

.. pkg-field:: main-is: filename

    :required: ``exitcode-stdio-1.0``

    The name of the ``.hs`` or ``.lhs`` file containing the ``Main``
    module. Note that it is the ``.hs`` filename that must be listed,
    even if that file is generated using a preprocessor. The source file
    must be relative to one of the directories listed in
    :pkg-field:`hs-source-dirs`. This field is analogous to the ``main-is``
    field of an executable section.

Example: Package using ``exitcode-stdio-1.0`` interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

The example package description and executable source file below
demonstrate the use of the ``exitcode-stdio-1.0`` interface.

.. code-block:: cabal
    :caption: foo.cabal
    :name: foo-bench.cabal

    Name:           foo
    Version:        1.0
    License:        BSD3
    Cabal-Version:  >= 1.9.2
    Build-Type:     Simple

    Benchmark bench-foo
        type:       exitcode-stdio-1.0
        main-is:    bench-foo.hs
        build-depends: base >= 4 && < 5, time >= 1.1 && < 1.7

.. code-block:: haskell
    :caption: bench-foo.hs

    {-# LANGUAGE BangPatterns #-}
    module Main where

    import Data.Time.Clock

    fib 0 = 1
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    main = do
        start <- getCurrentTime
        let !r = fib 20
        end <- getCurrentTime
        putStrLn $ "fib 20 took " ++ show (diffUTCTime end start)

Running benchmarks
""""""""""""""""""

You can have Cabal run your benchmark using its built-in benchmark
runner:

::

    $ cabal configure --enable-benchmarks
    $ cabal build
    $ cabal bench

See the output of ``cabal help bench`` for a list of options you can
pass to ``cabal bench``.

Foreign libraries
^^^^^^^^^^^^^^^^^

Foreign libraries are system libraries intended to be linked against
programs written in C or other "foreign" languages. They
come in two primary flavours: dynamic libraries (``.so`` files on Linux,
``.dylib`` files on OSX, ``.dll`` files on Windows, etc.) are linked against
executables when the executable is run (or even lazily during
execution), while static libraries (``.a`` files on Linux/OSX, ``.lib``
files on Windows) get linked against the executable at compile time.

Foreign libraries only work with GHC 7.8 and later.

A typical stanza for a foreign library looks like

::

    foreign-library myforeignlib
      type:                native-shared
      lib-version-info:    6:3:2

      if os(Windows)
        options: standalone
        mod-def-file: MyForeignLib.def

      other-modules:       MyForeignLib.SomeModule
                           MyForeignLib.SomeOtherModule
      build-depends:       base >=4.7 && <4.9
      hs-source-dirs:      src
      c-sources:           csrc/MyForeignLibWrapper.c
      default-language:    Haskell2010


.. pkg-section:: foreign-library name
    :since: 2.0
    :synopsis: Foriegn library build information.

    Build information for `foreign libraries`_.

.. pkg-field:: type: foreign library type

   Cabal recognizes ``native-static`` and ``native-shared`` here, although
   we currently only support building `native-shared` libraries.

.. pkg-field:: options: foreign library option list

   Options for building the foreign library, typically specific to the
   specified type of foreign library. Currently we only support
   ``standalone`` here. A standalone dynamic library is one that does not
   have any dependencies on other (Haskell) shared libraries; without
   the ``standalone`` option the generated library would have dependencies
   on the Haskell runtime library (``libHSrts``), the base library
   (``libHSbase``), etc. Currently, ``standalone`` *must* be used on Windows
   and *must not* be used on any other platform.

.. pkg-field:: mod-def-file: filename

   This option can only be used when creating dynamic Windows libraries
   (that is, when using ``native-shared`` and the ``os`` is ``Windows``). If
   used, it must be a path to a *module definition file*. The details of
   module definition files are beyond the scope of this document; see the
   `GHC <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/win32-dlls.html>`_
   manual for some details and some further pointers.

.. pkg-field:: lib-version-info: current:revision:age

   This field is currently only used on Linux.

   This field specifies a Libtool-style version-info field that sets
   an appropriate ABI version for the foreign library. Note that the
   three numbers specified in this field do not directly specify the
   actual ABI version: ``6:3:2`` results in library version ``4.2.3``.

   With this field set, the SONAME of the library is set, and symlinks
   are installed.

   How you should bump this field on an ABI change depends on the
   breakage you introduce:

   -  Programs using the previous version may use the new version as
      drop-in replacement, and programs using the new version can also
      work with the previous one. In other words, no recompiling nor
      relinking is needed. In this case, bump ``revision`` only, don't
      touch current nor age.
   -  Programs using the previous version may use the new version as
      drop-in replacement, but programs using the new version may use
      APIs not present in the previous one. In other words, a program
      linking against the new version may fail with "unresolved
      symbols" if linking against the old version at runtime: set
      revision to 0, bump current and age.
   -  Programs may need to be changed, recompiled, and relinked in
      order to use the new version. Bump current, set revision and age
      to 0.

   Also refer to the Libtool documentation on the version-info field.

.. pkg-field:: lib-version-linux: version

   This field is only used on Linux.

   Specifies the library ABI version directly for foreign libraries
   built on Linux: so specifying ``4.2.3`` causes a library
   ``libfoo.so.4.2.3`` to be built with SONAME ``libfoo.so.4``, and
   appropriate symlinks ``libfoo.so.4`` and ``libfoo.so`` to be
   installed.

Note that typically foreign libraries should export a way to initialize
and shutdown the Haskell runtime. In the example above, this is done by
the ``csrc/MyForeignLibWrapper.c`` file, which might look something like

.. code-block:: c

    #include <stdlib.h>
    #include "HsFFI.h"

    HsBool myForeignLibInit(void){
      int argc = 2;
      char *argv[] = { "+RTS", "-A32m", NULL };
      char **pargv = argv;

      // Initialize Haskell runtime
      hs_init(&argc, &pargv);

      // do any other initialization here and
      // return false if there was a problem
      return HS_BOOL_TRUE;
    }

    void myForeignLibExit(void){
      hs_exit();
    }

With modern ghc regular libraries are installed in directories that contain
package keys. This isn't usually a problem because the package gets registered
in ghc's package DB and so we can figure out what the location of the library
is. Foreign libraries however don't get registered, which means that we'd have
to have a way of finding out where a platform library got installed (other than by
searching the ``lib/`` directory). Instead, we install foreign libraries in
``~/.cabal/lib``, much like we install executables in ``~/.cabal/bin``.

Build information
^^^^^^^^^^^^^^^^^
.. pkg-section:: None

The following fields may be optionally present in a library, executable,
test suite or benchmark section, and give information for the building
of the corresponding library or executable. See also the sections on
`system-dependent parameters`_ and `configurations`_ for a way to supply
system-dependent values for these fields.

.. pkg-field:: build-depends: package list

    A list of packages needed to build this one. Each package can be
    annotated with a version constraint.

    Version constraints use the operators ``==, >=, >, <, <=`` and a
    version number. Multiple constraints can be combined using ``&&`` or
    ``||``. If no version constraint is specified, any version is
    assumed to be acceptable. For example:

    ::

        library
          build-depends:
            base >= 2,
            foo >= 1.2.3 && < 1.3,
            bar

    Dependencies like ``foo >= 1.2.3 && < 1.3`` turn out to be very
    common because it is recommended practise for package versions to
    correspond to API versions (see PVP_).

    Since Cabal 1.6, there is a special wildcard syntax to help with
    such ranges

    ::

        build-depends: foo ==1.2.*

    It is only syntactic sugar. It is exactly equivalent to
    ``foo >= 1.2 && < 1.3``.

    .. Warning::

       A potential pitfall of the wildcard syntax is that the
       constraint ``nats == 1.0.*`` doesn't match the release
       ``nats-1`` because the version ``1`` is lexicographically less
       than ``1.0``. This is not an issue with the caret-operator
       ``^>=`` described below.

    Starting with Cabal 2.0, there's a new syntactic sugar to express
    PVP_-style
    major upper bounds conveniently, and is inspired by similar
    syntactic sugar found in other language ecosystems where it's often
    called the "Caret" operator:

    ::

        build-depends:
          foo ^>= 1.2.3.4,
          bar ^>= 1

    This allows to express the intent that this packages requires
    versions of ``foo`` and ``bar`` which are semantically compatible
    to ``foo-1.2.3.4`` and ``bar-1`` respectively. This subtle but important
    difference in signaling allows tooling to treat *"hard"* ``<``-style
    and *"weak"* ``^>=``-style upper bounds differently. For instance,
    :option:`--allow-newer`'s ``^``-modifier allows to relax only *"weak"*
    ``^>=``-style bounds while leaving ``<``-bounds unaffected.

    Ignoring the signaling intent, the equivalences are

    - ``^>= x`` == ``>= x && < x.1``
    - ``^>= x.y`` == ``>= x.y && < x.(y+1)``
    - ``^>= x.y.z`` == ``>= x.y.z && < x.(y+1)``
    - ``^>= x.y.z.u`` == ``>= x.y.z.u && < x.(y+1)``
    - etc.

    Consequently, the example declaration above is equivalent to

    ::

        build-depends:
          foo >= 1.2.3.4 && < 1.3,
          bar >= 1 && < 1.1

    .. Note::

       Prior to Cabal 1.8, ``build-depends`` specified in each
       section were global to all sections. This was unintentional, but
       some packages were written to depend on it, so if you need your
       :pkg-field:`build-depends` to be local to each section, you must specify
       at least ``Cabal-Version: >= 1.8`` in your ``.cabal`` file.

    .. Note::

       Cabal 1.20 experimentally supported module thinning and
       renaming in ``build-depends``; however, this support has since been
       removed and should not be used.

.. pkg-field:: other-modules: identifier list

    A list of modules used by the component but not exposed to users.
    For a library component, these would be hidden modules of the
    library. For an executable, these would be auxiliary modules to be
    linked with the file named in the ``main-is`` field.

    .. Note::

       Every module in the package *must* be listed in one of
       :pkg-field:`other-modules`, :pkg-field:`library:exposed-modules` or
       :pkg-field:`executable:main-is` fields.

.. pkg-field:: hs-source-dirs: directory list

    :default: ``.``

    Root directories for the module hierarchy.

    For backwards compatibility, the old variant ``hs-source-dir`` is
    also recognized.

.. pkg-field:: default-extensions: identifier list

    A list of Haskell extensions used by every module. These determine
    corresponding compiler options enabled for all files. Extension
    names are the constructors of the
    `Extension <../release/cabal-latest/doc/API/Cabal/Language-Haskell-Extension.html#t:Extension>`__
    type. For example, ``CPP`` specifies that Haskell source files are
    to be preprocessed with a C preprocessor.

.. pkg-field:: other-extensions: identifier list

    A list of Haskell extensions used by some (but not necessarily all)
    modules. From GHC version 6.6 onward, these may be specified by
    placing a ``LANGUAGE`` pragma in the source files affected e.g.

    .. code-block:: haskell

        {-# LANGUAGE CPP, MultiParamTypeClasses #-}

    In Cabal-1.24 the dependency solver will use this and
    :pkg-field:`default-extensions` information. Cabal prior to 1.24 will abort
    compilation if the current compiler doesn't provide the extensions.

    If you use some extensions conditionally, using CPP or conditional
    module lists, it is good to replicate the condition in
    :pkg-field:`other-extensions` declarations:

    ::

        other-extensions: CPP
        if impl(ghc >= 7.5)
          other-extensions: PolyKinds

    You could also omit the conditionally used extensions, as they are
    for information only, but it is recommended to replicate them in
    :pkg-field:`other-extensions` declarations.

.. pkg-field:: extensions: identifier list
   :deprecated:

   Deprecated in favor of :pkg-field:`default-extensions`.

.. pkg-field:: build-tool-depends: package:executable list
    :since: 2.0

    A list of Haskell programs needed to build this component.
    Each is specified by the package containing the executable and the name of the executable itself, separated by a colon, and optionally followed by a version bound.
    It is fine for the package to be the current one, in which case this is termed an *internal*, rather than *external* executable dependency.

    External dependencies can (and should) contain a version bound like conventional :pkg-field:`build-depends` dependencies.
    Internal deps should not contain a version bound, as they will be always resolved within the same configuration of the package in the build plan.
    Specifically, version bounds that include the package's version will be warned for being extraneous, and version bounds that exclude the package's version will raise an error for being impossible to follow.

    Cabal can make sure that specified programs are built and on the ``PATH`` before building the component in question.
    It will always do so for internal dependencies, and also do so for external dependencies when using Nix-style local builds.

    :pkg-field:`build-tool-depends` was added in Cabal 2.0, and it will
    be ignored (with a warning) with old versions of Cabal.  See
    :pkg-field:`build-tools` for more information about backwards
    compatibility.

.. pkg-field:: build-tools: program list
    :deprecated:

    Deprecated in favor of :pkg-field:`build-tool-depends`, but :ref:`see below for backwards compatibility information <buildtoolsbc>`.

    A list of Haskell programs needed to build this component.
    Each may be followed by an optional version bound.
    Confusingly, each program in the list either refer to one of three things:

      1. Another executables in the same package (supported since Cabal 1.12)

      2. Tool name contained in Cabal's :ref:`hard-coded set of common tools <buildtoolsmap>`

      3. A pre-built executable that should already be on the ``PATH``
         (supported since Cabal 2.0)

    These cases are listed in order of priority:
    an executable in the package will override any of the hard-coded packages with the same name,
    and a hard-coded package will override any executable on the ``PATH``.

    In the first two cases, the list entry is desugared into a :pkg-field:`build-tool-depends` entry.
    In the first case, the entry is desugared into a :pkg-field:`build-tool-depends` entry by prefixing with ``$pkg:``.
    In the second case, it is desugared by looking up the package and executable name in a hard-coded table.
    In either case, the optional version bound is passed through unchanged.
    Refer to the documentation for :pkg-field:`build-tool-depends` to understand the desugared field's meaning, along with restrictions on version bounds.

    .. _buildtoolsbc:

    **Backward Compatiblity**

    Although this field is deprecated in favor of :pkg-field:`build-tool-depends`, there are some situations where you may prefer to use :pkg-field:`build-tools` in cases (1) and (2), as it is supported by more versions of Cabal.
    In case (3), :pkg-field:`build-tool-depends` is better for backwards-compatibility, as it will be ignored by old versions of Cabal; if you add the executable to :pkg-field:`build-tools`, a setup script built against old Cabal will choke.
    If an old version of Cabal is used, an end-user will have to manually arrange for the requested executable to be in your ``PATH``.

    .. _buildtoolsmap:

    **Set of Known Tool Names**

    Identifiers specified in :pkg-field:`build-tools` are desugared into their respective equivalent :pkg-field:`build-tool-depends` form according to the table below. Consequently, a legacy specification such as::

        build-tools: alex >= 3.2.1 && < 3.3, happy >= 1.19.5 && < 1.20

    is simply desugared into the equivalent specification::

        build-tool-depends: alex:alex >= 3.2.1 && < 3.3, happy:happy >= 1.19.5 && < 1.20

    +--------------------------+-----------------------------------+-----------------+
    | :pkg-field:`build-tools` | desugared                         | Note            |
    | identifier               | :pkg-field:`build-tool-depends`   |                 |
    |                          | identifier                        |                 |
    +==========================+===================================+=================+
    | ``alex``                 | ``alex:alex``                     |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``c2hs``                 | ``c2hs:c2hs``                     |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``cpphs``                | ``cpphs:cpphs``                   |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``greencard``            | ``greencard:greencard``           |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``haddock``              | ``haddock:haddock``               |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``happy``                | ``happy:happy``                   |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``hsc2hs``               | ``hsc2hs:hsc2hs``                 |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``hscolour``             | ``hscolour:hscolour``             |                 |
    +--------------------------+-----------------------------------+-----------------+
    | ``hspec-discover``       | ``hspec-discover:hspec-discover`` | since Cabal 2.0 |
    +--------------------------+-----------------------------------+-----------------+

    This built-in set can be programmatically extended via ``Custom`` setup scripts; this, however, is of limited use since the Cabal solver cannot access information injected by ``Custom`` setup scripts.

.. pkg-field:: buildable: boolean

    :default: ``True``

    Is the component buildable? Like some of the other fields below,
    this field is more useful with the slightly more elaborate form of
    the simple build infrastructure described in the section on
    `system-dependent parameters`_.

.. pkg-field:: ghc-options: token list

    Additional options for GHC. You can often achieve the same effect
    using the :pkg-field:`extensions` field, which is preferred.

    Options required only by one module may be specified by placing an
    ``OPTIONS_GHC`` pragma in the source file affected.

    As with many other fields, whitespace can be escaped by using
    Haskell string syntax. Example:
    ``ghc-options: -Wcompat "-with-rtsopts=-T -I1" -Wall``.

.. pkg-field:: ghc-prof-options: token list

    Additional options for GHC when the package is built with profiling
    enabled.

    Note that as of Cabal-1.24, the default profiling detail level
    defaults to ``exported-functions`` for libraries and
    ``toplevel-functions`` for executables. For GHC these correspond to
    the flags ``-fprof-auto-exported`` and ``-fprof-auto-top``. Prior to
    Cabal-1.24 the level defaulted to ``none``. These levels can be
    adjusted by the person building the package with the
    ``--profiling-detail`` and ``--library-profiling-detail`` flags.

    It is typically better for the person building the package to pick
    the profiling detail level rather than for the package author. So
    unless you have special needs it is probably better not to specify
    any of the GHC ``-fprof-auto*`` flags here. However if you wish to
    override the profiling detail level, you can do so using the
    :pkg-field:`ghc-prof-options` field: use ``-fno-prof-auto`` or one of the
    other ``-fprof-auto*`` flags.

.. pkg-field:: ghc-shared-options: token list

    Additional options for GHC when the package is built as shared
    library. The options specified via this field are combined with the
    ones specified via :pkg-field:`ghc-options`, and are passed to GHC during
    both the compile and link phases.

.. pkg-field:: includes: filename list

    A list of header files to be included in any compilations via C.
    This field applies to both header files that are already installed
    on the system and to those coming with the package to be installed.
    The former files should be found in absolute paths, while the latter
    files should be found in paths relative to the top of the source
    tree or relative to one of the directories listed in
    :pkg-field:`include-dirs`.

    These files typically contain function prototypes for foreign
    imports used by the package. This is in contrast to
    :pkg-field:`install-includes`, which lists header files that are intended
    to be exposed to other packages that transitively depend on this
    library.

.. pkg-field:: install-includes: filename list

    A list of header files from this package to be installed into
    ``$libdir/includes`` when the package is installed. Files listed in
    :pkg-field:`install-includes` should be found in relative to the top of the
    source tree or relative to one of the directories listed in
    :pkg-field:`include-dirs`.

    :pkg-field:`install-includes` is typically used to name header files that
    contain prototypes for foreign imports used in Haskell code in this
    package, for which the C implementations are also provided with the
    package. For example, here is a ``.cabal`` file for a hypothetical
    ``bindings-clib`` package that bundles the C source code for ``clib``::

        include-dirs:     cbits
        c-sources:        clib.c
        install-includes: clib.h

    Now any package that depends (directly or transitively) on the
    ``bindings-clib`` library can use ``clib.h``.

    Note that in order for files listed in :pkg-field:`install-includes` to be
    usable when compiling the package itself, they need to be listed in
    the :pkg-field:`includes` field as well.

.. pkg-field:: include-dirs: directory list

    A list of directories to search for header files, when preprocessing
    with ``c2hs``, ``hsc2hs``, ``cpphs`` or the C preprocessor, and also
    when compiling via C. Directories can be absolute paths (e.g., for
    system directories) or paths that are relative to the top of the
    source tree. Cabal looks in these directories when attempting to
    locate files listed in :pkg-field:`includes` and
    :pkg-field:`install-includes`.
 
.. pkg-field:: c-sources: filename list

    A list of C source files to be compiled and linked with the Haskell
    files.

.. pkg-field:: cxx-sources: filename list

    A list of C++ source files to be compiled and linked with the Haskell
    files. Useful for segregating C and C++ sources when supplying different
    command-line arguments to the compiler via the :pkg-field:`cc-options`
    and the :pkg-field:`cxx-options` fields. The files listed in the
    :pkg-field:`cxx-sources` can reference files listed in the
    :pkg-field:`c-sources` field and vice-versa. The object files will be linked
    appropriately.
    
.. pkg-field:: asm-sources: filename list

    A list of assembly source files to be compiled and linked with the
    Haskell files.

.. pkg-field:: cmm-sources: filename list

    A list of C-- source files to be compiled and linked with the Haskell
    files.

.. pkg-field:: js-sources: filename list

    A list of JavaScript source files to be linked with the Haskell
    files (only for JavaScript targets).

.. pkg-field:: extra-libraries: token list

    A list of extra libraries to link with.

.. pkg-field:: extra-ghci-libraries: token list

    A list of extra libraries to be used instead of 'extra-libraries'
    when the package is loaded with GHCi.

.. pkg-field:: extra-bundled-libraries: token list

   A list of libraries that are supposed to be copied from the build
   directory alongside the produced haskell libraries.  Note that you
   are under the obligation to produce those lirbaries in the build
   directory (e.g. via a custom setup).  Libraries listed here will
   be included when ``copy``-ing packages and be listed in the
   ``hs-libraries`` of the package configuration.

.. pkg-field:: extra-lib-dirs: directory list

    A list of directories to search for libraries.

.. pkg-field:: cc-options: token list

    Command-line arguments to be passed to the C compiler. Since the
    arguments are compiler-dependent, this field is more useful with the
    setup described in the section on `system-dependent parameters`_.

.. pkg-field:: cpp-options: token list

    Command-line arguments for pre-processing Haskell code. Applies to
    haskell source and other pre-processed Haskell source like .hsc
    .chs. Does not apply to C code, that's what cc-options is for.

.. pkg-field:: cxx-options: token list

    Command-line arguments to be passed to the compiler when compiling
    C++ code. The C++ sources to which these command-line arguments
    should be applied can be specified with the :pkg-field:`cxx-sources`
    field. Command-line options for C and C++ can be passed separately to
    the compiler when compiling both C and C++ sources by segregating the C
    and C++ sources with the :pkg-field:`c-sources` and
    :pkg-field:`cxx-sources` fields respectively, and providing different
    command-line arguments with the :pkg-field:`cc-options` and the
    :pkg-field:`cxx-options` fields.

.. pkg-field:: ld-options: token list

    Command-line arguments to be passed to the linker. Since the
    arguments are compiler-dependent, this field is more useful with the
    setup described in the section on `system-dependent parameters`_.

.. pkg-field:: pkgconfig-depends: package list

    A list of
    `pkg-config <http://www.freedesktop.org/wiki/Software/pkg-config/>`__
    packages, needed to build this package. They can be annotated with
    versions, e.g. ``gtk+-2.0 >= 2.10, cairo >= 1.0``. If no version
    constraint is specified, any version is assumed to be acceptable.
    Cabal uses ``pkg-config`` to find if the packages are available on
    the system and to find the extra compilation and linker options
    needed to use the packages.

    If you need to bind to a C library that supports ``pkg-config`` (use
    ``pkg-config --list-all`` to find out if it is supported) then it is
    much preferable to use this field rather than hard code options into
    the other fields.

.. pkg-field:: frameworks: token list

    On Darwin/MacOS X, a list of frameworks to link to. See Apple's
    developer documentation for more details on frameworks. This entry
    is ignored on all other platforms.

.. pkg-field:: extra-frameworks-dirs: directory list

    On Darwin/MacOS X, a list of directories to search for frameworks.
    This entry is ignored on all other platforms.

Configurations
^^^^^^^^^^^^^^

Library and executable sections may include conditional blocks, which
test for various system parameters and configuration flags. The flags
mechanism is rather generic, but most of the time a flag represents
certain feature, that can be switched on or off by the package user.
Here is an example package description file using configurations:

Example: A package containing a library and executable programs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

::

    Name: Test1
    Version: 0.0.1
    Cabal-Version: >= 1.2
    License: BSD3
    Author:  Jane Doe
    Synopsis: Test package to test configurations
    Category: Example

    Flag Debug
      Description: Enable debug support
      Default:     False

    Flag WebFrontend
      Description: Include API for web frontend.
      -- Cabal checks if the configuration is possible, first
      -- with this flag set to True and if not it tries with False

    Library
      Build-Depends:   base
      Exposed-Modules: Testing.Test1
      Extensions:      CPP

      if flag(debug)
        GHC-Options: -DDEBUG
        if !os(windows)
          CC-Options: "-DDEBUG"
        else
          CC-Options: "-DNDEBUG"

      if flag(webfrontend)
        Build-Depends: cgi > 0.42
        Other-Modules: Testing.WebStuff

    Executable test1
      Main-is: T1.hs
      Other-Modules: Testing.Test1
      Build-Depends: base

      if flag(debug)
        CC-Options: "-DDEBUG"
        GHC-Options: -DDEBUG

Layout
""""""

Flags, conditionals, library and executable sections use layout to
indicate structure. This is very similar to the Haskell layout rule.
Entries in a section have to all be indented to the same level which
must be more than the section header. Tabs are not allowed to be used
for indentation.

As an alternative to using layout you can also use explicit braces
``{}``. In this case the indentation of entries in a section does not
matter, though different fields within a block must be on different
lines. Here is a bit of the above example again, using braces:

Example: Using explicit braces rather than indentation for layout
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

::

    Name: Test1
    Version: 0.0.1
    Cabal-Version: >= 1.2
    License: BSD3
    Author:  Jane Doe
    Synopsis: Test package to test configurations
    Category: Example

    Flag Debug {
      Description: Enable debug support
      Default:     False
    }

    Library {
      Build-Depends:   base
      Exposed-Modules: Testing.Test1
      Extensions:      CPP
      if flag(debug) {
        GHC-Options: -DDEBUG
        if !os(windows) {
          CC-Options: "-DDEBUG"
        } else {
          CC-Options: "-DNDEBUG"
        }
      }
    }

Configuration Flags
"""""""""""""""""""

.. pkg-section:: flag name
   :synopsis: Flag declaration.

   Flag section declares a flag which can be used in `conditional blocks`_.

Flag names are case-insensitive and must match ``[[:alnum:]_][[:alnum:]_-]*``
regular expression.

.. note::

    Hackage accepts ASCII-only flags, ``[a-zA-Z0-9_][a-zA-Z0-9_-]*`` regexp.

.. pkg-field:: description: freeform

    The description of this flag.

.. pkg-field:: default: boolean

    :default: ``True``

    The default value of this flag.

    .. note::

      This value may be `overridden in several
      ways <installing-packages.html#controlling-flag-assignments>`__. The
      rationale for having flags default to True is that users usually
      want new features as soon as they are available. Flags representing
      features that are not (yet) recommended for most users (such as
      experimental features or debugging support) should therefore
      explicitly override the default to False.

.. pkg-field:: manual: boolean

    :default: ``False``

    By default, Cabal will first try to satisfy dependencies with the
    default flag value and then, if that is not possible, with the
    negated value. However, if the flag is manual, then the default
    value (which can be overridden by commandline flags) will be used.

Conditional Blocks
^^^^^^^^^^^^^^^^^^

Conditional blocks may appear anywhere inside a library or executable
section. They have to follow rather strict formatting rules. Conditional
blocks must always be of the shape

::

      if condition
         property-descriptions-or-conditionals

or

::

      if condition
           property-descriptions-or-conditionals
      else
           property-descriptions-or-conditionals

Note that the ``if`` and the condition have to be all on the same line.

Since Cabal 2.2 conditional blocks support ``elif`` construct.

::

      if condition1
           property-descriptions-or-conditionals
      elif condition2
           property-descriptions-or-conditionals
      else
           property-descriptions-or-conditionals

Conditions
""""""""""

Conditions can be formed using boolean tests and the boolean operators
``||`` (disjunction / logical "or"), ``&&`` (conjunction / logical
"and"), or ``!`` (negation / logical "not"). The unary ``!`` takes
highest precedence, ``||`` takes lowest. Precedence levels may be
overridden through the use of parentheses. For example,
``os(darwin) && !arch(i386) || os(freebsd)`` is equivalent to
``(os(darwin) && !(arch(i386))) || os(freebsd)``.

The following tests are currently supported.

:samp:`os({name})`
    Tests if the current operating system is *name*. The argument is
    tested against ``System.Info.os`` on the target system. There is
    unfortunately some disagreement between Haskell implementations
    about the standard values of ``System.Info.os``. Cabal canonicalises
    it so that in particular ``os(windows)`` works on all
    implementations. If the canonicalised os names match, this test
    evaluates to true, otherwise false. The match is case-insensitive.
:samp:`arch({name})`
    Tests if the current architecture is *name*. The argument is matched
    against ``System.Info.arch`` on the target system. If the arch names
    match, this test evaluates to true, otherwise false. The match is
    case-insensitive.
:samp:`impl({compiler})`
    Tests for the configured Haskell implementation. An optional version
    constraint may be specified (for example ``impl(ghc >= 6.6.1)``). If
    the configured implementation is of the right type and matches the
    version constraint, then this evaluates to true, otherwise false.
    The match is case-insensitive.

    Note that including a version constraint in an ``impl`` test causes
    it to check for two properties:

    -  The current compiler has the specified name, and

    -  The compiler's version satisfied the specified version constraint

    As a result, ``!impl(ghc >= x.y.z)`` is not entirely equivalent to
    ``impl(ghc < x.y.z)``. The test ``!impl(ghc >= x.y.z)`` checks that:

    -  The current compiler is not GHC, or

    -  The version of GHC is earlier than version x.y.z.

:samp:`flag({name})`
    Evaluates to the current assignment of the flag of the given name.
    Flag names are case insensitive. Testing for flags that have not
    been introduced with a flag section is an error.
``true``
    Constant value true.
``false``
    Constant value false.

Resolution of Conditions and Flags
""""""""""""""""""""""""""""""""""

If a package descriptions specifies configuration flags the package user
can `control these in several
ways <installing-packages.html#controlling-flag-assignments>`__. If the
user does not fix the value of a flag, Cabal will try to find a flag
assignment in the following way.

-  For each flag specified, it will assign its default value, evaluate
   all conditions with this flag assignment, and check if all
   dependencies can be satisfied. If this check succeeded, the package
   will be configured with those flag assignments.

-  If dependencies were missing, the last flag (as by the order in which
   the flags were introduced in the package description) is tried with
   its alternative value and so on. This continues until either an
   assignment is found where all dependencies can be satisfied, or all
   possible flag assignments have been tried.

To put it another way, Cabal does a complete backtracking search to find
a satisfiable package configuration. It is only the dependencies
specified in the :pkg-field:`build-depends` field in conditional blocks that
determine if a particular flag assignment is satisfiable
(:pkg-field:`build-tools` are not considered). The order of the declaration and
the default value of the flags determines the search order. Flags
overridden on the command line fix the assignment of that flag, so no
backtracking will be tried for that flag.

If no suitable flag assignment could be found, the configuration phase
will fail and a list of missing dependencies will be printed. Note that
this resolution process is exponential in the worst case (i.e., in the
case where dependencies cannot be satisfied). There are some
optimizations applied internally, but the overall complexity remains
unchanged.

Meaning of field values when using conditionals
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

During the configuration phase, a flag assignment is chosen, all
conditionals are evaluated, and the package description is combined into
a flat package descriptions. If the same field both inside a conditional
and outside then they are combined using the following rules.

-  Boolean fields are combined using conjunction (logical "and").

-  List fields are combined by appending the inner items to the outer
   items, for example

   ::

       other-extensions: CPP
       if impl(ghc)
         other-extensions: MultiParamTypeClasses

   when compiled using GHC will be combined to

   ::

       other-extensions: CPP, MultiParamTypeClasses

   Similarly, if two conditional sections appear at the same nesting
   level, properties specified in the latter will come after properties
   specified in the former.

-  All other fields must not be specified in ambiguous ways. For example

   ::

       Main-is: Main.hs
       if flag(useothermain)
         Main-is: OtherMain.hs

   will lead to an error. Instead use

   ::

       if flag(useothermain)
         Main-is: OtherMain.hs
       else
         Main-is: Main.hs

Source Repositories
^^^^^^^^^^^^^^^^^^^

.. pkg-section:: source-repository

It is often useful to be able to specify a source revision control
repository for a package. Cabal lets you specifying this information in
a relatively structured form which enables other tools to interpret and
make effective use of the information. For example the information
should be sufficient for an automatic tool to checkout the sources.

Cabal supports specifying different information for various common
source control systems. Obviously not all automated tools will support
all source control systems.

Cabal supports specifying repositories for different use cases. By
declaring which case we mean automated tools can be more useful. There
are currently two kinds defined:

-  The ``head`` kind refers to the latest development branch of the
   package. This may be used for example to track activity of a project
   or as an indication to outside developers what sources to get for
   making new contributions.

-  The ``this`` kind refers to the branch and tag of a repository that
   contains the sources for this version or release of a package. For
   most source control systems this involves specifying a tag, id or
   hash of some form and perhaps a branch. The purpose is to be able to
   reconstruct the sources corresponding to a particular package
   version. This might be used to indicate what sources to get if
   someone needs to fix a bug in an older branch that is no longer an
   active head branch.

You can specify one kind or the other or both. As an example here are
the repositories for the Cabal library. Note that the ``this`` kind of
repository specifies a tag.

::

    source-repository head
      type:     darcs
      location: http://darcs.haskell.org/cabal/

    source-repository this
      type:     darcs
      location: http://darcs.haskell.org/cabal-branches/cabal-1.6/
      tag:      1.6.1

The exact fields are as follows:

.. pkg-field:: type: token

    The name of the source control system used for this repository. The
    currently recognised types are:

    -  ``darcs``
    -  ``git``
    -  ``svn``
    -  ``cvs``
    -  ``mercurial`` (or alias ``hg``)
    -  ``bazaar`` (or alias ``bzr``)
    -  ``arch``
    -  ``monotone``

    This field is required.

.. pkg-field:: location: URL

    The location of the repository. The exact form of this field depends
    on the repository type. For example:

    -  for darcs: ``http://code.haskell.org/foo/``
    -  for git: ``git://github.com/foo/bar.git``
    -  for CVS: ``anoncvs@cvs.foo.org:/cvs``

    This field is required.

.. pkg-field:: module: token

    CVS requires a named module, as each CVS server can host multiple
    named repositories.

    This field is required for the CVS repository type and should not be
    used otherwise.

.. pkg-field:: branch: token

    Many source control systems support the notion of a branch, as a
    distinct concept from having repositories in separate locations. For
    example CVS, SVN and git use branches while for darcs uses different
    locations for different branches. If you need to specify a branch to
    identify a your repository then specify it in this field.

    This field is optional.

.. pkg-field:: tag: token

    A tag identifies a particular state of a source repository. The tag
    can be used with a ``this`` repository kind to identify the state of
    a repository corresponding to a particular package version or
    release. The exact form of the tag depends on the repository type.

    This field is required for the ``this`` repository kind.

.. pkg-field:: subdir: directory

    Some projects put the sources for multiple packages under a single
    source repository. This field lets you specify the relative path
    from the root of the repository to the top directory for the
    package, i.e. the directory containing the package's ``.cabal``
    file.

    This field is optional. It default to empty which corresponds to the
    root directory of the repository.

Downloading a package's source
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The ``cabal get`` command allows to access a package's source code -
either by unpacking a tarball downloaded from Hackage (the default) or
by checking out a working copy from the package's source repository.

::

    $ cabal get [FLAGS] PACKAGES

The ``get`` command supports the following options:

``-d --destdir`` *PATH*
    Where to place the package source, defaults to (a subdirectory of)
    the current directory.
``-s --source-repository`` *[head\|this\|...]*
    Fork the package's source repository using the appropriate version
    control system. The optional argument allows to choose a specific
    repository kind.
``--index-state`` *[HEAD\|@<unix-timestamp>\|<iso8601-utc-timestamp>]*
    Use source package index state as it existed at a previous time. Accepts
    unix-timestamps (e.g. ``@1474732068``), ISO8601 UTC timestamps (e.g.
    ``2016-09-24T17:47:48Z``), or ``HEAD`` (default).
    This determines which package versions are available as well as which
    ``.cabal`` file revision is selected (unless ``--pristine`` is used).
``--pristine``
    Unpack the original pristine tarball, rather than updating the
    ``.cabal`` file with the latest revision from the package archive.

Custom setup scripts
--------------------

Since Cabal 1.24, custom ``Setup.hs`` are required to accurately track
their dependencies by declaring them in the ``.cabal`` file rather than
rely on dependencies being implicitly in scope.  Please refer
`this article <https://www.well-typed.com/blog/2015/07/cabal-setup-deps/>`__
for more details.

Declaring a ``custom-setup`` stanza also enables the generation of
``MIN_VERSION_package_(A,B,C)`` CPP macros for the Setup component.

.. pkg-section:: custom-setup
   :synopsis: Custom Setup.hs build information.
   :since: 1.24

   The optional :pkg-section:`custom-setup` stanza contains information needed
   for the compilation of custom ``Setup.hs`` scripts,

::

    custom-setup
      setup-depends:
        base  >= 4.5 && < 4.11,
        Cabal >= 1.14 && < 1.25

.. pkg-field:: setup-depends: package list
    :since: 1.24

    The dependencies needed to compile ``Setup.hs``. See the
    :pkg-field:`build-depends` field for a description of the syntax expected by
    this field.

Backward compatibility and ``custom-setup``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Versions prior to Cabal 1.24 don't recognise ``custom-setup`` stanzas,
and will behave agnostic to them (except for warning about an unknown
section). Consequently, versions prior to Cabal 1.24 can't ensure the
declared dependencies ``setup-depends`` are in scope, and instead
whatever is registered in the current package database environment
will become eligible (and resolved by the compiler) for the
``Setup.hs`` module.

The availability of the
``MIN_VERSION_package_(A,B,C)`` CPP macros
inside ``Setup.hs`` scripts depends on the condition that either

- a ``custom-setup`` section has been declared (or ``cabal new-build`` is being
  used which injects an implicit hard-coded ``custom-setup`` stanza if it's missing), or
- GHC 8.0 or later is used (which natively injects package version CPP macros)

Consequently, if you need to write backward compatible ``Setup.hs``
scripts using CPP, you should declare a ``custom-setup`` stanza and
use the pattern below:

.. code-block:: haskell

    {-# LANGUAGE CPP #-}
    import Distribution.Simple

    #if defined(MIN_VERSION_Cabal)
    -- version macros are available and can be used as usual
    # if MIN_VERSION_Cabal(a,b,c)
    -- code specific to lib:Cabal >= a.b.c
    # else
    -- code specific to lib:Cabal < a.b.c
    # endif
    #else
    # warning Enabling heuristic fall-back. Please upgrade cabal-install to 1.24 or later if Setup.hs fails to compile.

    -- package version macros not available; except for exotic environments,
    -- you can heuristically assume that lib:Cabal's version is correlated
    -- with __GLASGOW_HASKELL__, and specifically since we can assume that
    -- GHC < 8.0, we can assume that lib:Cabal is version 1.22 or older.
    #endif

    main = ...

The simplified (heuristic) CPP pattern shown below is useful if all you need
is to distinguish ``Cabal < 2.0`` from ``Cabal >= 2.0``.

.. code-block:: haskell

    {-# LANGUAGE CPP #-}
    import Distribution.Simple

    #if !defined(MIN_VERSION_Cabal)
    # define MIN_VERSION_Cabal(a,b,c) 0
    #endif

    #if MIN_VERSION_Cabal(2,0,0)
    -- code for lib:Cabal >= 2.0
    #else
    -- code for lib:Cabal < 2.0
    #endif

    main = ...



Autogenerated modules
---------------------

Modules that are built automatically at setup, created with a custom
setup script, must appear on :pkg-field:`other-modules` for the library,
executable, test-suite or benchmark stanzas or also on
:pkg-field:`library:exposed-modules` for libraries to be used, but are not
really on the package when distributed. This makes commands like sdist fail
because the file is not found.

These special modules must appear again on the :pkg-field:`autogen-modules`
field of the stanza that is using it, besides :pkg-field:`other-modules` or
:pkg-field:`library:exposed-modules`. With this there is no need to create
complex build hooks for this poweruser case.

.. pkg-field:: autogen-modules: module list
   :since: 2.0

   .. TODO: document autogen-modules field

Right now :pkg-field:`executable:main-is` modules are not supported on
:pkg-field:`autogen-modules`.

::

    Library
        default-language: Haskell2010
        build-depends: base
        exposed-modules:
            MyLibrary
            MyLibHelperModule
        other-modules:
            MyLibModule
        autogen-modules:
            MyLibHelperModule

    Executable Exe
        default-language: Haskell2010
        main-is: Dummy.hs
        build-depends: base
        other-modules:
            MyExeModule
            MyExeHelperModule
        autogen-modules:
            MyExeHelperModule

Accessing data files from package code
--------------------------------------

The placement on the target system of files listed in
the :pkg-field:`data-files` field varies between systems, and in some cases
one can even move packages around after installation (see `prefix
independence <installing-packages.html#prefix-independence>`__). To
enable packages to find these files in a portable way, Cabal generates a
module called :file:`Paths_{pkgname}` (with any hyphens in *pkgname*
replaced by underscores) during building, so that it may be imported by
modules of the package. This module defines a function

.. code-block:: haskell

    getDataFileName :: FilePath -> IO FilePath

If the argument is a filename listed in the :pkg-field:`data-files` field, the
result is the name of the corresponding file on the system on which the
program is running.

.. Note::

   If you decide to import the :file:`Paths_{pkgname}` module then it
   *must* be listed in the :pkg-field:`other-modules` field just like any other
   module in your package and on :pkg-field:`autogen-modules` as the file is
   autogenerated.

The :file:`Paths_{pkgname}` module is not platform independent, as any
other autogenerated module, so it does not get included in the source
tarballs generated by ``sdist``.

The :file:`Paths_{pkgname}` module also includes some other useful
functions and values, which record the version of the package and some
other directories which the package has been configured to be installed
into (e.g. data files live in ``getDataDir``):

.. code-block:: haskell

    version :: Version

    getBinDir :: IO FilePath
    getLibDir :: IO FilePath
    getDynLibDir :: IO FilePath
    getDataDir :: IO FilePath
    getLibexecDir :: IO FilePath
    getSysconfDir :: IO FilePath

The actual location of all these directories can be individually
overridden at runtime using environment variables of the form
``pkg_name_var``, where ``pkg_name`` is the name of the package with all
hyphens converted into underscores, and ``var`` is either ``bindir``,
``libdir``, ``dynlibdir``, ``datadir``, ``libexedir`` or ``sysconfdir``. For example,
the configured data directory for ``pretty-show`` is controlled with the
``pretty_show_datadir`` environment variable.

Accessing the package version
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The aforementioned auto generated :file:`Paths_{pkgname}` module also
exports the constant ``version ::``
`Version <http://hackage.haskell.org/package/base/docs/Data-Version.html>`__
which is defined as the version of your package as specified in the
``version`` field.

System-dependent parameters
---------------------------

For some packages, especially those interfacing with C libraries,
implementation details and the build procedure depend on the build
environment. The ``build-type`` ``Configure`` can be used to handle many
such situations. In this case, ``Setup.hs`` should be:

.. code-block:: haskell

    import Distribution.Simple
    main = defaultMainWithHooks autoconfUserHooks

Most packages, however, would probably do better using the ``Simple``
build type and `configurations`_.

The :pkg-field:`build-type` ``Configure`` differs from ``Simple`` in two ways:

-  The package root directory must contain a shell script called
   ``configure``. The configure step will run the script. This
   ``configure`` script may be produced by
   `autoconf <http://www.gnu.org/software/autoconf/>`__ or may be
   hand-written. The ``configure`` script typically discovers
   information about the system and records it for later steps, e.g. by
   generating system-dependent header files for inclusion in C source
   files and preprocessed Haskell source files. (Clearly this won't work
   for Windows without MSYS or Cygwin: other ideas are needed.)

-  If the package root directory contains a file called
   *package*\ ``.buildinfo`` after the configuration step, subsequent
   steps will read it to obtain additional settings for `build
   information`_ fields,to be merged with the ones
   given in the ``.cabal`` file. In particular, this file may be
   generated by the ``configure`` script mentioned above, allowing these
   settings to vary depending on the build environment.

The build information file should have the following structure:

    *buildinfo*

    ``executable:`` *name* *buildinfo*

    ``executable:`` *name* *buildinfo* ...

where each *buildinfo* consists of settings of fields listed in the
section on `build information`_. The first one (if
present) relates to the library, while each of the others relate to the
named executable. (The names must match the package description, but you
don't have to have entries for all of them.)

Neither of these files is required. If they are absent, this setup
script is equivalent to ``defaultMain``.

Example: Using autoconf
^^^^^^^^^^^^^^^^^^^^^^^

This example is for people familiar with the
`autoconf <http://www.gnu.org/software/autoconf/>`__ tools.

In the X11 package, the file ``configure.ac`` contains:

.. code-block:: shell

    AC_INIT([Haskell X11 package], [1.1], [libraries@haskell.org], [X11])

    # Safety check: Ensure that we are in the correct source directory.
    AC_CONFIG_SRCDIR([X11.cabal])

    # Header file to place defines in
    AC_CONFIG_HEADERS([include/HsX11Config.h])

    # Check for X11 include paths and libraries
    AC_PATH_XTRA
    AC_TRY_CPP([#include <X11/Xlib.h>],,[no_x=yes])

    # Build the package if we found X11 stuff
    if test "$no_x" = yes
    then BUILD_PACKAGE_BOOL=False
    else BUILD_PACKAGE_BOOL=True
    fi
    AC_SUBST([BUILD_PACKAGE_BOOL])

    AC_CONFIG_FILES([X11.buildinfo])
    AC_OUTPUT

Then the setup script will run the ``configure`` script, which checks
for the presence of the X11 libraries and substitutes for variables in
the file ``X11.buildinfo.in``:

::

    buildable: @BUILD_PACKAGE_BOOL@
    cc-options: @X_CFLAGS@
    ld-options: @X_LIBS@

This generates a file ``X11.buildinfo`` supplying the parameters needed
by later stages:

::

    buildable: True
    cc-options:  -I/usr/X11R6/include
    ld-options:  -L/usr/X11R6/lib

The ``configure`` script also generates a header file
``include/HsX11Config.h`` containing C preprocessor defines recording
the results of various tests. This file may be included by C source
files and preprocessed Haskell source files in the package.

.. Note::

   Packages using these features will also need to list additional
   files such as ``configure``, templates for ``.buildinfo`` files, files
   named only in ``.buildinfo`` files, header files and so on in the
   :pkg-field:`extra-source-files` field to ensure that they are included in
   source distributions. They should also list files and directories generated
   by ``configure`` in the :pkg-field:`extra-tmp-files` field to ensure that
   they are removed by ``setup clean``.

Quite often the files generated by ``configure`` need to be listed
somewhere in the package description (for example, in the
:pkg-field:`install-includes` field). However, we usually don't want generated
files to be included in the source tarball. The solution is again
provided by the ``.buildinfo`` file. In the above example, the following
line should be added to ``X11.buildinfo``:

::

    install-includes: HsX11Config.h

In this way, the generated ``HsX11Config.h`` file won't be included in
the source tarball in addition to ``HsX11Config.h.in``, but it will be
copied to the right location during the install process. Packages that
use custom ``Setup.hs`` scripts can update the necessary fields
programmatically instead of using the ``.buildinfo`` file.

Conditional compilation
-----------------------

Sometimes you want to write code that works with more than one version
of a dependency. You can specify a range of versions for the dependency
in the :pkg-field:`build-depends`, but how do you then write the code that can
use different versions of the API?

Haskell lets you preprocess your code using the C preprocessor (either
the real C preprocessor, or ``cpphs``). To enable this, add
``extensions: CPP`` to your package description. When using CPP, Cabal
provides some pre-defined macros to let you test the version of
dependent packages; for example, suppose your package works with either
version 3 or version 4 of the ``base`` package, you could select the
available version in your Haskell modules like this:

.. code-block:: cpp

    #if MIN_VERSION_base(4,0,0)
    ... code that works with base-4 ...
    #else
    ... code that works with base-3 ...
    #endif

In general, Cabal supplies a macro
``MIN_VERSION_``\ *``package``*\ ``_(A,B,C)`` for each package depended
on via :pkg-field:`build-depends`. This macro is true if the actual version of
the package in use is greater than or equal to ``A.B.C`` (using the
conventional ordering on version numbers, which is lexicographic on the
sequence, but numeric on each component, so for example 1.2.0 is greater
than 1.0.3).

Since version 1.20, the ``MIN_TOOL_VERSION_``\ *``tool``*
family of macros lets you condition on the version of build tools used to
build the program (e.g. ``hsc2hs``).

Since version 1.24, the macro ``CURRENT_COMPONENT_ID``, which
expands to the string of the component identifier that uniquely
identifies this component.  Furthermore, if the package is a library,
the macro ``CURRENT_PACKAGE_KEY`` records the identifier that was passed
to GHC for use in symbols and for type equality.

Since version 2.0, the macro ``CURRENT_PACKAGE_VERSION`` expands
to the string version number of the current package.

Cabal places the definitions of these macros into an
automatically-generated header file, which is included when
preprocessing Haskell source code by passing options to the C
preprocessor.

Cabal also allows to detect when the source code is being used for
generating documentation. The ``__HADDOCK_VERSION__`` macro is defined
only when compiling via Haddock_
instead of a normal Haskell compiler. The value of the
``__HADDOCK_VERSION__`` macro is defined as ``A*1000 + B*10 + C``, where
``A.B.C`` is the Haddock version. This can be useful for working around
bugs in Haddock or generating prettier documentation in some special
cases.

More complex packages
---------------------

For packages that don't fit the simple schemes described above, you have
a few options:

-  By using the :pkg-field:`build-type` ``Custom``, you can supply your own
   ``Setup.hs`` file, and customize the simple build infrastructure
   using *hooks*. These allow you to perform additional actions before
   and after each command is run, and also to specify additional
   preprocessors. A typical ``Setup.hs`` may look like this:

   .. code-block:: haskell

       import Distribution.Simple
       main = defaultMainWithHooks simpleUserHooks { postHaddock = posthaddock }

       posthaddock args flags desc info = ....

   See ``UserHooks`` in
   `Distribution.Simple <../release/cabal-latest/doc/API/Cabal/Distribution-Simple.html>`__
   for the details, but note that this interface is experimental, and
   likely to change in future releases.

   If you use a custom ``Setup.hs`` file you should strongly consider
   adding a :pkg-section:`custom-setup` stanza with a
   :pkg-field:`custom-setup:setup-depends` field to ensure that your setup
   script does not break with future dependency versions.

-  You could delegate all the work to ``make``, though this is unlikely
   to be very portable. Cabal supports this with the :pkg-field:`build-type`
   ``Make`` and a trivial setup library
   `Distribution.Make <../release/cabal-latest/doc/API/Cabal/Distribution-Make.html>`__,
   which simply parses the command line arguments and invokes ``make``.
   Here ``Setup.hs`` should look like this:

   .. code-block:: haskell

       import Distribution.Make
       main = defaultMain

   The root directory of the package should contain a ``configure``
   script, and, after that has run, a ``Makefile`` with a default target
   that builds the package, plus targets ``install``, ``register``,
   ``unregister``, ``clean``, ``dist`` and ``docs``. Some options to
   commands are passed through as follows:

   -  The ``--with-hc-pkg``, ``--prefix``, ``--bindir``, ``--libdir``,
      ``--dynlibdir``, ``--datadir``, ``--libexecdir`` and ``--sysconfdir`` options to
      the ``configure`` command are passed on to the ``configure``
      script. In addition the value of the ``--with-compiler`` option is
      passed in a ``--with-hc`` option and all options specified with
      ``--configure-option=`` are passed on.

   -  The ``--destdir`` option to the ``copy`` command becomes a setting
      of a ``destdir`` variable on the invocation of ``make copy``. The
      supplied ``Makefile`` should provide a ``copy`` target, which will
      probably look like this:

      .. code-block:: make

          copy :
                  $(MAKE) install prefix=$(destdir)/$(prefix) \
                                  bindir=$(destdir)/$(bindir) \
                                  libdir=$(destdir)/$(libdir) \
                                  dynlibdir=$(destdir)/$(dynlibdir) \
                                  datadir=$(destdir)/$(datadir) \
                                  libexecdir=$(destdir)/$(libexecdir) \
                                  sysconfdir=$(destdir)/$(sysconfdir) \

-  Finally, with the :pkg-field:`build-type` ``Custom``, you can also write your
   own setup script from scratch. It must conform to the interface
   described in the section on `building and installing
   packages <installing-packages.html>`__, and you may use the Cabal
   library for all or part of the work. One option is to copy the source
   of ``Distribution.Simple``, and alter it for your needs. Good luck.


.. include:: references.inc
