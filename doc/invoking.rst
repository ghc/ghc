Invoking Haddock
================

Haddock is invoked from the command line, like so:

.. code-block:: none

    haddock [option ...] file ...

Where each ``file`` is a filename containing a Haskell source module (.hs)
or a Literate Haskell source module (.lhs) or just a module name.

All the modules specified on the command line will be processed
together. When one module refers to an entity in another module being
processed, the documentation will link directly to that entity.

Entities that cannot be found, for example because they are in a module
that isn't being processed as part of the current batch, simply won't be
hyperlinked in the generated documentation. Haddock will emit warnings
listing all the identifiers it couldn't resolve.

The modules should *not* be mutually recursive, as Haddock don't like
swimming in circles.

Note that while older version would fail on invalid markup, this is
considered a bug in the new versions. If you ever get failed parsing
message, please report it.

You must also specify an option for the output format. Currently only
the :option:`--html` option for HTML, the :option:`--hoogle` option for
outputting Hoogle data, and the :option:`--latex` option are functional.

The packaging tool
`Cabal <http://www.haskell.org/ghc/docs/latest/html/Cabal/index.html>`__
has Haddock support, and is often used instead of invoking Haddock
directly.

The following options are available:

.. option:: -B <dir>

    Tell GHC that its lib directory is dir. Can be used to override
    the default path.

.. option:: -o <dir>
            --odir=<dir>

    Generate files into dir instead of the current directory.

.. option:: -l <dir>
            --lib=<dir>

    Use Haddock auxiliary files (themes, javascript, etc...) in dir.

.. option:: -i <file>
            --read-interface=<file>
            -i <docpath>,<file>
            --read-interface=<docpath>,<file>
            -i <docpath>,<srcpath>,<file>
            --read-interface=<docpath>,<srcpath>,<file>

    Read the interface file in file, which must have been produced by
    running Haddock with the :option:`--dump-interface` option. The interface
    describes a set of modules whose HTML documentation is located in
    docpath (which may be a relative pathname). The docpath is optional,
    and defaults to “.”. The srcpath is optional but has no default
    value.

    This option allows Haddock to produce separate sets of documentation
    with hyperlinks between them. The docpath is used to direct
    hyperlinks to point to the right files; so make sure you don't move
    the HTML files later or these links will break. Using a relative
    docpath means that a documentation subtree can still be moved around
    without breaking links.

    Similarly to docpath, srcpath is used generate cross-package
    hyperlinks but within sources rendered with :option:`--hyperlinked-source`
    option.

    Multiple :option:`--read-interface` options may be given.

.. option:: -D <file>
            --dump-interface=<file>

    Produce an interface file [1]_ in the file file. An interface file
    contains information Haddock needs to produce more documentation
    that refers to the modules currently being processed - see the
    :option:`--read-interface` option for more details. The interface file is
    in a binary format; don't try to read it.

.. option:: --show-interface=<file>

    Dumps a binary interface file to stdout in a human readable fashion.
    Uses json as output format.

.. [1]
   Haddock interface files are not the same as Haskell interface files,
   I just couldn't think of a better name.

.. option:: --html, -h

    Generate documentation in HTML format. Several files will be
    generated into the current directory (or the specified directory if
    the :option:`-o` option is given), including the following:

    ``module.html``; ``mini_module.html``
        An HTML page for each module, and a "mini" page for each used
        when viewing their synopsis.

    ``index.html``
        The top level page of the documentation: lists the modules
        available, using indentation to represent the hierarchy if the
        modules are hierarchical.

    ``doc-index.html``; ``doc-index-X.html``
        The alphabetic index, possibly split into multiple pages if big
        enough.

    ``some.css``; ``etc...``
        Files needed for the themes used. Specify your themes using the
        :option:`--theme` option.

    ``haddock-util.js``
        Some JavaScript utilities used to implement some of the dynamic
        features like collapsible sections.

.. option:: --mathjax

    Specify a custom URL for a mathjax-compatible JS script. By default,
    this is set to `MathJax
    <https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML>`_.

.. option:: --latex

    Generate documentation in LaTeX format. Several files will be
    generated into the current directory (or the specified directory if
    the :option:`-o` option is given), including the following:

    ``package.tex``
        The top-level LaTeX source file; to format the documentation
        into PDF you might run something like this: ::

            $ pdflatex package.tex

    ``haddock.sty``
        The default style. The file contains definitions for various
        macros used in the LaTeX sources generated by Haddock; to change
        the way the formatted output looks, you might want to override
        these by specifying your own style with the :option:`--latex-style`
        option.

    ``module.tex``
        The LaTeX documentation for each module.

.. option:: --latex-style=<style>

    This option lets you override the default style used by the LaTeX
    generated by the :option:`--latex` option. Normally Haddock puts a
    standard ``haddock.sty`` in the output directory, and includes the
    command ``\usepackage{haddock}`` in the LaTeX source. If this option
    is given, then ``haddock.sty`` is not generated, and the command is
    instead ``\usepackage{style}``.

.. option:: --hoogle

    Generate an index file for the
    `Hoogle <http://hackage.haskell.org/package/hoogle>`_ search engine.
    One text file will be generated into the current directory (or the
    specified directory if the :option:`-o` is given). Note that
    the :option:`--package-name` is required.

    Since the output is intended to be parsed by Hoogle, some conventions
    need to be upheld:

      * Every entity should span exactly one line. ::

            newtype ReaderT r (m :: * -> *) a :: * -> (* -> *) -> * -> *
          
        The one exception to this rule is classes. The body of a class
        is split up with one class member per line, an opening brace on
        the line of the header, and a closing brace on a new line after
        the class. ::

            class Foo a where {
                foo :: a -> a -> Baz a;
                type family Baz a;
                type Baz a = [(a, a)];
            }
      
      * Entites that are exported only indirectly (for instance data
        constructors visible via a ``ReaderT(..)`` export) have their names
        wrapped in square brackets. ::

            [ReaderT] :: (r -> m a) -> ReaderT r m a
            [runReaderT] :: ReaderT r m a -> r -> m a


.. option:: --hyperlinked-source

    Generate hyperlinked source code (as HTML web page). All rendered
    files will be put into ``src/`` subfolder of output directory.

    Usually, this should be used in combination with :option:`--html` option -
    generated documentation will then contain references to appropriate
    code fragments. Previously, this behaviour could be achieved by
    generating sources using external tool and specifying
    :option:`--source-base`, :option:`--source-module`, :option:`--source-entity` and
    related options. Note that these flags are ignored once
    :option:`--hyperlinked-source` is set.

    In order to make cross-package source hyperlinking possible,
    appropriate source paths have to be set up when providing interface
    files using :option:`--read-interface` option.

.. option:: --source-css=<style>

    Use custom CSS file for sources rendered by the
    :option:`--hyperlinked-source` option. If no custom style file is
    provided, Haddock will use default one.

.. option:: -S, --docbook

    Reserved for future use (output documentation in DocBook XML
    format).

.. option:: --base-url=<url>

    Base url for static assets (eg. css, javascript, json files etc.).
    When present, static assets are not copied.  This option is useful
    when creating documentation for multiple packages, it allows to have
    a single copy of static assets served from the given url.

.. option:: --source-base=<url>
            --source-module=<url>
            --source-entity=<url>
            --source-entity-line=<url>

    Include links to the source files in the generated documentation.
    Use the :option:`--source-base` option to add a source code link in the
    header bar of the contents and index pages. Use the
    :option:`--source-module` to add a source code link in the header bar of
    each module page. Use the :option:`--source-entity` option to add a source
    code link next to the documentation for every value and type in each
    module. :option:`--source-entity-line` is a flag that gets used for
    entities that need to link to an exact source location rather than a
    name, eg. since they were defined inside a Template Haskell splice.

    In each case URL is the base URL where the source files can be
    found. For the per-module and per-entity URLs, the following
    substitutions are made within the string URL:

    -  The string ``%M`` or ``%{MODULE}`` is replaced by the module
       name. Note that for the per-entity URLs this is the name of the
       *exporting* module.

    -  The string ``%F`` or ``%{FILE}`` is replaced by the original
       source file name. Note that for the per-entity URLs this is the
       name of the *defining* module.

    -  The string ``%N`` or ``%{NAME}`` is replaced by the name of the
       exported value or type. This is only valid for the
       :option:`--source-entity` option.

    -  The string ``%K`` or ``%{KIND}`` is replaced by a flag indicating
       whether the exported name is a value ``v`` or a type
       ``t``. This is only valid for the :option:`--source-entity` option.

    -  The string ``%L`` or ``%{LINE}`` is replaced by the number of the
       line where the exported value or type is defined. This is only
       valid for the :option:`--source-entity` option.

    -  The string ``%%`` is replaced by ``%``.

    For example, if your sources are online under some directory, you
    would say ``haddock --source-base=url/ --source-module=url/%F``

    If you have html versions of your sources online with anchors for
    each type and function name, you would say
    ``haddock --source-base=url/ --source-module=url/%M.html --source-entity=url/%M.html#%N``

    For the ``%{MODULE}`` substitution you may want to replace the
    ``.`` character in the module names with some other character
    (some web servers are known to get confused by multiple ``.``
    characters in a file name). To replace it with a character c use
    ``%{MODULE/./c}``.

    Similarly, for the ``%{FILE}`` substitution you may want to replace
    the ``/`` character in the file names with some other character
    (especially for links to colourised entity source code with a shared
    css file). To replace it with a character c use ``%{FILE///c}``/

    One example of a tool that can generate syntax-highlighted HTML from
    your source code, complete with anchors suitable for use from
    haddock, is
    `hscolour <http://www.cs.york.ac.uk/fp/darcs/hscolour>`__.

.. option:: -s <url>
            --source=<url>

    Deprecated aliases for :option:`--source-module`

.. option:: --comments-base=<url>
            --comments-module=<url>
            --comments-entity=<url>

    documentation. This feature would typically be used in conjunction
    with a Wiki system.

    Use the :option:`--comments-base` option to add a user comments link in
    the header bar of the contents and index pages. Use the
    :option:`--comments-module` to add a user comments link in the header bar
    of each module page. Use the :option:`--comments-entity` option to add a
    comments link next to the documentation for every value and type in
    each module.

    In each case URL is the base URL where the corresponding comments
    page can be found. For the per-module and per-entity URLs the same
    substitutions are made as with the :option:`--source-module` and
    :option:`--source-entity` options above.

    For example, if you want to link the contents page to a wiki page,
    and every module to subpages, you would say
    ``haddock --comments-base=url --comments-module=url/%M``

    If your Wiki system doesn't like the ``.`` character in Haskell
    module names, you can replace it with a different character. For
    example to replace the ``.`` characters with ``_`` use
    ``haddock --comments-base=url --comments-module=url/%{MODULE/./_}``.
    Similarly, you can replace the ``/`` in a file name (may be useful for
    entity comments, but probably not).

.. option:: --theme=<path>

    Specify a theme to be used for HTML (:option:`--html`) documentation. If
    given multiple times then the pages will use the first theme given
    by default, and have alternate style sheets for the others. The
    reader can switch between themes with browsers that support
    alternate style sheets, or with the "Style" menu that gets added
    when the page is loaded. If no themes are specified, then just the
    default built-in theme ("Linuwial") is used.

    The path parameter can be one of:

    -  A *directory*: The base name of the directory becomes the name of
       the theme. The directory must contain exactly one ``some.css``
       file. Other files, usually image files, will be copied, along
       with the ``some.css`` file, into the generated output directory.

    -  A *CSS file*: The base name of the file becomes the name of the
       theme.

    -  The *name* of a built-in theme ("Linuwial", "Ocean", or "Classic").

.. option:: --built-in-themes

    Includes the built-in themes ("Linuwial", "Ocean", and "Classic"). Can be
    combined with :option:`--theme`. Note that order matters: The first
    specified theme will be the default.

.. option:: --use-unicode

    Enable use of Unicode characters in HTML output.

.. option:: -c <file>
            --css=<file>

    Deprecated aliases for :option:`--theme`

.. option:: -p <file>
            --prologue=<file>

    Specify a file containing documentation which is placed on the main
    contents page under the heading “Description”. The file is parsed as
    a normal Haddock doc comment (but the comment markers are not
    required).

.. option:: -t <title>
            --title=<title>

    Use title as the page heading for each page in the
    documentation.This will normally be the name of the library being
    documented.

    The title should be a plain string (no markup please!).

.. option:: --package-name=<name>

    Specify the name of the package being documented.

.. option:: --package-version=<version>

    Specify the version of the package being documented.

.. option:: -q <mode>
            --qual=<mode>

    Specify how identifiers are qualified.

    mode should be one of

    -  ``none`` (default): don't qualify any identifiers

    -  ``full``: always qualify identifiers completely

    -  ``local``: only qualify identifiers that are not part of the module

    -  ``relative``: like local, but strip name of the module from
       qualifications of identifiers in submodules

    Example: If you generate documentation for module A, then the
    identifiers A.x, A.B.y and C.z are qualified as follows.

    -  none: x, y, z

    -  full: A.x, A.B.y, C.z

    -  local: x, A.B.y, C.z

    -  relative: x, B.y, C.z

.. option:: --since-qual=<mode>

    Specify how ``@since`` annotations are qualified.

    mode should be one of

    -  ``always`` (default): always qualify ``@since`` annotations with
       a package name and version

    -  ``only-external``: only qualify ``@since`` annotations with a
       package name and version when they do not come from the current
       package

.. option:: -?
            --help

    Display help and exit.

.. option:: -V
            --version

    Output version information and exit.

.. option:: --ghc-version

    Output the version of GHC which Haddock expects to find at :option:-B
    and exit.

.. option:: --print-ghc-path

    Output the path to the GHC (which Haddock computes based on :option:-B)
    and exit.

.. option:: --print-ghc-libdir

    Output the path to the GHC ``lib`` directory (which Haddock computes
    based on :option:-B) and exit.

.. option:: -v
            --verbose

    Increase verbosity. Currently this will cause Haddock to emit some
    extra warnings, in particular about modules which were imported but
    it had no information about (this is often quite normal; for example
    when there is no information about the ``Prelude``).

.. option:: --use-contents=<url>
            --use-index=<url>

    When generating HTML, do not generate an index. Instead, redirect
    the Contents and/or Index link on each page to URL. This option is
    intended for use in conjunction with :option:`--gen-contents` and/or
    :option:`--gen-index` for generating a separate contents and/or index
    covering multiple libraries.

.. option:: --gen-contents
            --gen-index

    Generate an HTML contents and/or index containing entries pulled
    from all the specified interfaces (interfaces are specified using
    :option:`-i` or :option:`--read-interface`). This is used to generate a single
    contents and/or index for multiple sets of Haddock documentation.

.. option:: --ignore-all-exports

    Causes Haddock to behave as if every module has the
    ``ignore-exports`` attribute (:ref:`module-attrs`). This might be useful for
    generating implementation documentation rather than interface
    documentation, for example.

.. option:: --hide <module>

    Causes Haddock to behave as if module module has the ``hide``
    attribute. (:ref:`module-attrs`).

.. option:: --show <module>

    Causes Haddock to behave as if module module does not have the ``hide``
    attribute. (:ref:`module-attrs`).

.. option:: --show-all

    Causes Haddock to behave as if no modules have the ``hide`` attribute.
    (:ref:`module-attrs`).

.. option:: --show-extensions <module>

    Causes Haddock to behave as if module module has the
    ``show-extensions`` attribute. (:ref:`module-attrs`).

.. option:: --optghc=<option>

    Pass option to GHC. Note that there is a double dash there, unlike
    for GHC.

.. option:: -w
            --no-warnings

    Turn off all warnings.

.. option:: --interface-version

    Prints out the version of the binary Haddock interface files that
    this version of Haddock generates.

.. option:: --compatible-interface-versions

    Prints out space-separated versions of binary Haddock interface
    files that this version of Haddock is compatible with.

.. option:: --bypass-interface-version-check

    **DANGEROUS** Causes Haddock to ignore the interface versions of
    binary Haddock interface files. This can make Haddock crash during
    deserialization of interface files.

.. option:: --no-tmp-comp-dir

    Do not use a temporary directory for reading and writing compilation
    output files (``.o``, ``.hi``, and stub files). Instead, use the
    present directory or another directory that you have explicitly told
    GHC to use via the :option:`--optghc` flag.

    This flag can be used to avoid recompilation if compilation files
    already exist. Compilation files are produced when Haddock has to
    process modules that make use of Template Haskell, in which case
    Haddock compiles the modules using the GHC API.

.. option:: --print-missing-docs

    Print extra information about any undocumented entities.

Using literate or pre-processed source
--------------------------------------

Since Haddock uses GHC internally, both plain and literate Haskell
sources are accepted without the need for the user to do anything. To
use the C pre-processor, however, the user must pass the ``-cpp``
option to GHC using :option:`--optghc`.
