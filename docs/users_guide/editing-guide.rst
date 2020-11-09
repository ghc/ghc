Care and feeding of your GHC User's Guide
=========================================

The GHC User's Guide is the primary reference documentation
for the Glasgow Haskell Compiler. Even more than this, it at times serves (for
better or for worse) as a de-facto language standard, being the sole
non-academic reference for many widely used language extensions.

Since GHC 8.0, the User's Guide is authored in `ReStructuredText
<https://en.wikipedia.org/wiki/ReStructuredText>`__ (or ReST or RST, for short)
a rich but light-weight mark-up language aimed at producing documentation. The
`Sphinx <http://sphinx-doc.org/>`__ tool is used to produce the final PDF and
HTML documentation.

This document (also written in ReST) serves as a brief introduction to ReST and to
document the conventions used in the User's Guide. This document is *not* intended
to be a thorough guide to ReST. For this see the resources referenced
`below <#references>`__.

Basics
------

Unicode characters are allowed in the document.

The basic syntax works largely as one would expect. For instance,

.. code-block:: rest

    This is a paragraph containing a few sentences of text. Purple turtles walk
    through green fields of lofty maize. Lorem ipsum dolor sit amet, consectetur
    adipiscing elit. Some lists,

    1. This is a list item

       a. Followed by a sub-item
       b. And another!
       c. Now with ``a bit of code`` and some *emphasis*.

    2. Back to the first list

    Or perhaps you are more of a bullet list person,

    * Foo
    * Fizzle

      - Bar
      - Blah

    Or perhaps a definition list is in order,

    *Chelonii*
        The taxonomic order consisting of modern turtles
    *Meiolaniidae*
        The taxonomic order of an extinct variety of herbivorous turtles.

Note the blank lines between a list item and its sub-items. Sub-items should be
on the same indentation level as the content of their parent items. Also note
that no whitespace is necessary or desirable before the bullet or item number
(lest the list be indented unnecessarily).

The above would be rendered as,

    This is a paragraph containing a few sentences of text. Purple turtles walk
    through green fields of lofty maize. Lorem ipsum dolor sit amet, consectetur
    adipiscing elit. Some lists,

    1. This is a list item

       a. Followed by a sub-item
       b. And another!
       c. Now with ``a bit of code`` and some *emphasis*.

    2. Back to the first list

    Or perhaps you are more of a bullet list person,

    * Foo
    * Fizzle

      - Bar
      - Blah

    Or perhaps a definition list is in order,

    *Chelonii*
        The taxonomic order consisting of modern turtles
    *Meiolaniidae*
        The taxonomic order of an extinct variety of herbivorous turtles.


Headings
~~~~~~~~

While ReST can accommodate a wide range of heading styles, we have standardized
on this convention in the User's Guide,

.. code-block:: rest

    Header level 1
    ==============

    Header level 2
    --------------

    Header level 3
    ~~~~~~~~~~~~~~

    Header level 4
    ^^^^^^^^^^^^^^


Formatting code
~~~~~~~~~~~~~~~

Haskell
^^^^^^^

Code snippets can be included as both inline and block elements. Inline
code is denoted with double-backticks whereas block of code are introduced
by ending a paragraph with double-colons and indentation,

.. code-block:: rest

    The ``fib`` function is defined as, ::

        fib :: Integer -> Integer
        fib 1 = 1
        fib n = n * fib (n - 1)

Which would be rendered as,

    The ``fib`` function is defined as, ::

        fib :: Integer -> Integer
        fib 1 = 1
        fib n = n * fib (n - 1)

Other languages
^^^^^^^^^^^^^^^

Double-colon blocks are syntax-highlighted as Haskell by default. To avoid this
use a
``.. code-block`` `directive
<http://sphinx-doc.org/markup/code.html#directive-code-block>`__ with explicit
language designation,

.. code-block:: rest

    This is a simple shell script,

    .. code-block:: sh

        #!/bin/bash
        echo "Hello World!"


Links
~~~~~

Within the User's Guide
^^^^^^^^^^^^^^^^^^^^^^^

Frequently we want to give a name to a section so it can be referred to
from other points in the document,

.. code-block:: rest

    .. _options-platform:

    Platform-specific Flags
    -----------------------

    There are lots of platform-specific flags.

    Some other section
    -------------------

    GHC supports a variety of :ref:`x86 specific features <options-platform>`.

    See :ref:`options-platform` for details.


To GHC resources
^^^^^^^^^^^^^^^^

There are special macros for conveniently linking to GHC
Wiki articles and tickets,

.. code-block:: rest

    See :ghc-wiki:`commentary/compiler/demand` for details on demand analysis.

    See the :ghc-wiki:`coding style <commentary/coding-style>` for guidelines.

    See the :ghc-ticket:`123` for further discussion.

    See the :ghc-ticket:`this bug <123>` for what happens when this fails.


To external resources
^^^^^^^^^^^^^^^^^^^^^

External links can be written in either of these ways,

.. code-block:: rest

    See the `GHC Wiki <https://gitlab.haskell.org/ghc/ghc/wikis>`_ for details.

    See the `GHC Wiki`_ for details.

    .. _GHC Wiki: https://gitlab.haskell.org/ghc/ghc/wikis


To core library Haddock documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is often useful to be able to refer to the Haddock documentation of the
libraries shipped with GHC. The users guide's build system provides
commands for referring to documentation for the following core GHC packages,

* ``base``: ``:base-ref:``
* ``cabal``: ``:cabal-ref:``
* ``ghc-prim``: ``:ghc-prim-ref:``

These are defined in :file:`docs/users_guide/ghc_config.py.in`.

For instance,

.. code-block:: rest

    See the documentation for :base-ref:`Control.Applicative.`
    for details.

Math
^^^^

You can insert type-set equations using ``:math:``. For instance,

.. code-block:: rest

    Fick's law of diffusion, :math:`J = -D \frac{d \varphi}{d x}`, ...

will render as,

    Fick's law of diffusion, :math:`J = -D \frac{d \varphi}{d x}`, ...


Index entries
~~~~~~~~~~~~~

Index entries can be included anywhere in the document as a block element.
They look like,

.. code-block:: rest

    Here is some discussion on the Strict Haskell extension.

    .. index::
        single: strict haskell
        single: language extensions; StrictData

This would produce two entries in the index referring to the "Strict Haskell"
section. One would be a simple "strict haskell" heading whereas the other would
be a "StrictData" subheading under "language extensions".

Sadly it is not possible to use inline elements (e.g. monotype inlines) inside
index headings.

Citations
---------

Citations can be marked-up like this,

.. code-block:: rest

    See the original paper [Doe2008]_

    .. [Doe2008] John Doe and Leslie Conway.
                 "This is the title of our paper" (2008)


Admonitions
-----------

`Admonitions`_ are block elements used to draw the readers attention to a point.
They should not be over-used for the sake of readability but they can be quite
effective in separating and drawing attention to points of importance,

.. code-block:: rest

    .. important::

        Be friendly and supportive to your fellow contributors.

Would be rendered as,

    .. important::

        Be friendly and supportive to your fellow contributors.

There are a number of admonitions types,

.. hlist::
    :columns: 3

    * attention
    * caution
    * danger
    * error
    * hint
    * important
    * note
    * tip
    * warning


.. _Admonitions: http://docutils.sourceforge.net/docs/ref/rst/directives.html#admonitions

Documenting command-line options and GHCi commands
--------------------------------------------------

:file:`conf.py` defines a few Sphinx object types for GHCi commands
(``ghci-cmd``), :program:`ghc` command-line options (``ghc-flag``), and runtime
:system options (``rts-flag``),

Command-line options
~~~~~~~~~~~~~~~~~~~~

The ``ghc-flag`` and ``rts-flag`` roles/directives can be used to document
command-line arguments to the :program:`ghc` executable and runtime system,
respectively. For instance,

.. code-block:: rest

    .. rts-flag:: -C ⟨seconds⟩

       :since: 8.2
       :default: 20 milliseconds

       Sets the context switch interval to ⟨s⟩ seconds.

Will be rendered as,

    .. rts-flag:: -C ⟨seconds⟩
       :noindex:

       :since: 8.2
       :default: 20 milliseconds

       Sets the context switch interval to ⟨s⟩ seconds.

and will have an associated index entry generated automatically.

The ``ghc-flag`` directive requires a few extra parameters to be passed.
This extra information is used to generate the :ref:`flag-reference` and the
man page. A ``ghc-flag`` directive looks like this,

.. code-block:: rest

    .. ghc-flag:: -fasm
        :shortdesc: Use the native code generator
        :type: dynamic
        :reverse: -fllvm
        :category: codegen

        Regular description...

When rendered, the extra parameters will be hidden, and the data stored for
later use. For more details, see the Sphinx extension ``flags.py``.

Note that, as in Style Conventions below, we use ``⟨⟩`` instead of
less-than/greater-than signs. To reference a ``ghc-flag`` or ``rts-flag``, you
must match the definition exactly, including the arguments. A quick way to find
the exact names and special characters is,

.. code-block:: sh

    $ git grep -- "flag:: -o "

which will generate the appropriate,

.. code-block:: none

    separate_compilation.rst:.. ghc-flag:: -o ⟨file⟩

GHCi commands
~~~~~~~~~~~~~

The ``ghci-cmd`` role and directive can be used to document GHCi directives. For
instance, we can describe the GHCi ``:module`` command,

.. code-block:: rest

    .. ghci-cmd:: :module; [*]⟨file⟩

        Load a module

which will be rendered as,

    .. ghci-cmd:: :module; [*]⟨file⟩
        :noindex:

        Load a module

And later refer to it by just the command name, ``:module``,

.. code-block:: rest

    The GHCi :ghci-cmd:`:load` and :ghci-cmd:`:module` commands are used
    to modify the modules in scope.

Like command-line options, GHCi commands will have associated index entries
generated automatically.

Style Conventions
-----------------

When describing user commands and the like it is common to need to denote
user-substitutable tokens. In this document we use the convention, ``⟨subst⟩``
(note that these are angle brackets, ``U+27E8`` and ``U+27E9``, not
less-than/greater-than signs).


ReST reference materials
------------------------

* `Sphinx ReST Primer`_: A great place to start.
* `Sphinx extensions`_: How Sphinx extends ReST
* `ReST reference`_: When you really need the details.
* `Directives reference`_

.. _Sphinx ReST Primer: http://sphinx-doc.org/rest.html
.. _ReST reference: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _Sphinx extensions: http://sphinx-doc.org/markup/index.html
.. _Directives reference: http://docutils.sourceforge.net/docs/ref/rst/directives.html#code
