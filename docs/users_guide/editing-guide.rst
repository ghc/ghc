Care and feeding of your GHC Users Guide
========================================

The GHC User's Guide is the primary reference documentation
for the Glasgow Haskell Compiler. Even more than this, it at times serves (for
better or for worse) as a de-facto language standard, being the sole
non-academic reference for many widely used language extensions.

Since GHC 8.0, the User's Guide is authored in `ReStructuredText
<https://en.wikipedia.org/wiki/ReStructuredText>`__ (or ReST or RST, for short)
a rich but light-weight mark-up language aimed at producing documentation. The
`Sphinx <http://sphinx-doc.org/>`__ tool is used to produce the final PDF and
HTML documentation.

This document (also written in ReST) serves as a brief introducion to ReST and to
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

While ReST can accomodate a wide range of heading styles, we have standardized
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

Within the Users Guide
^^^^^^^^^^^^^^^^^^^^^^

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


To GHC Trac resources
^^^^^^^^^^^^^^^^^^^^^

There are special macros for conveniently linking to GHC Trac
Wiki articles and tickets,

.. code-block:: rest

    See :ghc-wiki:`Commentary/Latedmd` for details on demand analysis.

    See the :ghc-wiki:`coding style <Commentary/CodingStyle>` for guidelines.

    See the :ghc-ticket:`123` for further discussion.

    See the :ghc-ticket:`this bug <123>` for what happens when this fails.


To external resources
^^^^^^^^^^^^^^^^^^^^^

External links can be written in either of these ways,

.. code-block:: rest

    See the `GHC Wiki <http://ghc.haskell.org/wiki>`_ for details.

    See the `GHC Wiki`_ for details.

    .. _GHC Wiki: http://ghc.haskell.org/wiki


To core library Haddock documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is often useful to be able to refer to the Haddock documention of the
libraries shipped with GHC. The users guide's build system provides
commands for referring to documentation for the following core GHC packages,

* ``base``: ``:base-ref:``
* ``cabal``: ``:cabal-ref:``
* ``ghc-prim``: ``:ghc-prim-ref:``

For instance,

.. code-block:: rest

    See the documentation for :base-ref:`Control.Applicative <Control-Applicative.html>`
    for details.


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

Style Conventions
-----------------

When describing user commands and the like it is common to need to denote
user-substitutable tokens. In this document we use the convention, ``⟨subst⟩``
(note that these are angle brackets, ``U+27E8`` and ``U+27E9``, not
less-than/greater-than signs).


.. _references:

GHC command-line options reference
----------------------------------

The tabular nature of GHC flags reference (``flags.rst``) makes it very
difficult to maintain as ReST. For this reason it is generated by
``utils/mkUserGuidePart``. Any command-line options added to GHC should
be added to the appropriate file in ``utils/mkUserGuidePart/Options``.


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
