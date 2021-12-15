Documentation and Markup
========================

Haddock understands special documentation annotations in the Haskell
source file and propagates these into the generated documentation. The
annotations are purely optional: if there are no annotations, Haddock
will just generate documentation that contains the type signatures, data
type declarations, and class declarations exported by each of the
modules being processed.

.. _top-level-declaration:

Documenting a Top-Level Declaration
-----------------------------------

The simplest example of a documentation annotation is for documenting
any top-level declaration (function type signature, type declaration, or
class declaration). For example, if the source file contains the
following type signature: ::

    square :: Int -> Int
    square x = x * x

Then we can document it like this: ::

    -- |The 'square' function squares an integer.
    square :: Int -> Int
    square x = x * x

The ``-- |`` syntax begins a documentation annotation, which applies
to the *following* declaration in the source file. Note that the
annotation is just a comment in Haskell — it will be ignored by the
Haskell compiler.

The declaration following a documentation annotation should be one of
the following:

-  A type signature for a top-level function,

-  A definition for a top-level function with no type signature,

-  A ``data`` declaration,

-  A ``pattern`` declaration,

-  A ``newtype`` declaration,

-  A ``type`` declaration

-  A ``class`` declaration,

-  An ``instance`` declaration,

-  A ``data family`` or ``type family`` declaration, or

-  A ``data instance`` or ``type instance`` declaration.

If the annotation is followed by a different kind of declaration, it
will probably be ignored by Haddock.

Some people like to write their documentation *after* the declaration;
this is possible in Haddock too: ::

    square :: Int -> Int
    -- ^The 'square' function squares an integer.
    square x = x * x

Since Haddock uses the GHC API internally, it can infer types for
top-level functions without type signatures. However, you're
encouraged to add explicit type signatures for all top-level
functions, to make your source code more readable for your users, and
at times to avoid GHC inferring overly general type signatures that
are less helpful to your users.

Documentation annotations may span several lines; the annotation
continues until the first non-comment line in the source file. For
example: ::

    -- |The 'square' function squares an integer.
    -- It takes one argument, of type 'Int'.
    square :: Int -> Int
    square x = x * x

You can also use Haskell's nested-comment style for documentation
annotations, which is sometimes more convenient when using multi-line
comments: ::

    {-|
      The 'square' function squares an integer.
      It takes one argument, of type 'Int'.
    -}
    square :: Int -> Int
    square x = x * x

Documenting Parts of a Declaration
----------------------------------

In addition to documenting the whole declaration, in some cases we can
also document individual parts of the declaration.

Class Methods
~~~~~~~~~~~~~

Class methods are documented in the same way as top level type
signatures, by using either the ``-- |`` or ``-- ^`` annotations: ::

    class C a where
       -- | This is the documentation for the 'f' method
       f :: a -> Int
       -- | This is the documentation for the 'g' method
       g :: Int -> a

Associated type and data families can also be annotated in this way.

Constructors and Record Fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constructors are documented like so: ::

    data T a b
      -- | This is the documentation for the 'C1' constructor
      = C1 a b
      -- | This is the documentation for the 'C2' constructor
      | C2 a b

or like this: ::

    data T a b
      = C1   -- ^ This is the documentation for the 'C1' constructor
          a  -- ^ This is the documentation for the argument of type 'a'
          b  -- ^ This is the documentation for the argument of type 'b'

There is one edge case that is handled differently: only one ``-- ^``
annotation occurring after the constructor and all its arguments is
applied to the constructor, not its last argument: ::

    data T a b
      = C1 a b  -- ^ This is the documentation for the 'C1' constructor
      | C2 a b  -- ^ This is the documentation for the 'C2' constructor

Record fields are documented using one of these styles: ::

    data R a b =
      C { -- | This is the documentation for the 'a' field
          a :: a,
          -- | This is the documentation for the 'b' field
          b :: b
        }

    data R a b =
      C { a :: a  -- ^ This is the documentation for the 'a' field
        , b :: b  -- ^ This is the documentation for the 'b' field
        }

Alternative layout styles are generally accepted by Haddock - for
example doc comments can appear before or after the comma in separated
lists such as the list of record fields above.

In cases where more than one constructor exports a field with the same
name, the documentation attached to the first occurrence of the field
will be used, even if a comment is not present. ::

    data T a = A { someField :: a -- ^ Doc for someField of A
                 }
             | B { someField :: a -- ^ Doc for someField of B
                 }

In the above example, all occurrences of ``someField`` in the
documentation are going to be documented with
``Doc for someField of A``. Note that Haddock versions 2.14.0 and before
would join up documentation of each field and render the result. The
reason for this seemingly weird behaviour is the fact that ``someField``
is actually the same (partial) function.

Deriving clauses
~~~~~~~~~~~~~~~~

Most instances are top-level, so can be documented as in
:ref:`top-level-declaration`. The exception to this is instance that are
come from a ``deriving`` clause on a datatype declaration. These can
the documented like this: ::

    data D a = L a | M
      deriving ( Eq   -- ^ @since 4.5
               , Ord  -- ^ default 'Ord' instance
               )

This also scales to the various GHC extensions for deriving: ::

    newtype T a = T a
      deriving          Show     -- ^ derivation of 'Show'
      deriving stock  ( Eq       -- ^ stock derivation of 'Eq'
                      , Foldable -- ^ stock derivation of 'Foldable'
                      )
      deriving newtype  Ord      -- ^ newtype derivation of 'Ord'
      deriving anyclass Read     -- ^ unsafe derivation of 'Read'
      deriving        ( Eq1      -- ^ deriving 'Eq1' via 'Identity'
                      , Ord1     -- ^ deriving 'Ord1' via 'Identity'
                      ) via Identity

Function Arguments
~~~~~~~~~~~~~~~~~~

Individual arguments to a function may be documented like this: ::

    f  :: Int      -- ^ The 'Int' argument
       -> Float    -- ^ The 'Float' argument
       -> IO ()    -- ^ The return value

Pattern synonyms, GADT-style data constructors, and class methods also
support this style of documentation.

.. _module-description:

The Module Description
----------------------

A module itself may be documented with multiple fields that can then be
displayed by the backend. In particular, the HTML backend displays all
the fields it currently knows about. We first show the most complete
module documentation example and then talk about the fields. ::

    {-|
    Module      : W
    Description : Short description
    Copyright   : (c) Some Person, 2013
                      Someone Else, 2014
    License     : GPL-3
    Maintainer  : sample@email.com
    Stability   : experimental
    Portability : POSIX

    Here is a longer description of this module, containing some
    commentary with @some markup@.
    -}
    module W where
    ...

All fields are optional but they must be in order if they do appear.
Multi-line fields are accepted but the consecutive lines have to start
indented more than their label. If your label is indented one space, as
is often the case with the ``--`` syntax, the consecutive lines have
to start at two spaces at the very least. For example, above we saw a
multiline ``Copyright`` field: ::

    {-|
    ...
    Copyright   : (c) Some Person, 2013
                      Someone Else, 2014
    ...
    -}

That could equivalently be written as: ::

    -- | ...
    -- Copyright:
    --  (c) Some Person, 2013
    --  Someone Else, 2014
    -- ...

or as: ::

    -- | ...
    -- Copyright: (c) Some Person, 2013
    --     Someone Else, 2014
    -- ...

but not as: ::

    -- | ...
    -- Copyright: (c) Some Person, 2013
    -- Someone Else, 2014
    -- ...

since the ``Someone`` needs to be indented more than the
``Copyright``.

Whether new lines and other formatting in multiline fields is
preserved depends on the field type. For example, new lines in the
``Copyright`` field are preserved, but new lines in the
``Description`` field are not; leading whitespace is not preserved in
either [#backend]_. Please note that we do not enforce the format for
any of the fields and the established formats are just a convention.

.. [#backend] Technically, whitespace and newlines in the
   ``Description`` field are preserved verbatim by the HTML backend,
   but because most browsers collapse whitespace in HTML, they don't
   render as such. But other backends may render this whitespace.

Fields of the Module Description
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``Module`` field specifies the current module name. Since the module
name can be inferred automatically from the source file, it doesn't
affect the output of any of the backends. But you might want to
include it for any other tools that might be parsing these comments
without the help of GHC.

The ``Description`` field accepts some short text which outlines the
general purpose of the module. If you're generating HTML, it will show
up next to the module link in the module index.

The ``Copyright``, ``License``, ``Maintainer`` and ``Stability`` fields should
be obvious. An alternative spelling for the ``License`` field is accepted
as ``Licence`` but the output will always prefer ``License``.

The ``Portability`` field has seen varied use by different library
authors. Some people put down things like operating system constraints
there while others put down which GHC extensions are used in the module.
Note that you might want to consider using the ``show-extensions`` module
flag for the latter (see :ref:`module-attrs`).

Finally, a module may contain a documentation comment before the
module header, in which case this comment is interpreted by Haddock as
an overall description of the module itself, and placed in a section
entitled ``Description`` in the documentation for the module. All the
usual Haddock :ref:`markup` is valid in this comment.

Controlling the Documentation Structure
---------------------------------------

Haddock produces interface documentation that lists only the entities
actually exported by the module. If there is no export list then all
entities defined by the module are exported.

The documentation for a module will
include *all* entities exported by that module, even if they were
re-exported from another module. The only exception is when Haddock can't
see the declaration for the re-exported entity, perhaps because it isn't
part of the batch of modules currently being processed.

To Haddock the export list has even more significance than just
specifying the entities to be included in the documentation. It also
specifies the *order* that entities will be listed in the generated
documentation. This leaves the programmer free to implement functions in
any order he/she pleases, and indeed in any *module* he/she pleases, but
still specify the order that the functions should be documented in the
export list. Indeed, many programmers already do this: the export list
is often used as a kind of ad-hoc interface documentation, with
headings, groups of functions, type signatures and declarations in
comments.

In the next section we give examples illustrating most of the
structural markup features. After the examples we go into more detail
explaining the related markup, namely :ref:`section-headings`,
:ref:`named-chunks`, and :ref:`re-exporting-entire-module`.

.. _structure-examples:

Documentation Structure Examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We now give several examples that produce similar results and
illustrate most of the structural markup features. The first two
examples use an export list, but the third example does not.

The first example, using an export list with :ref:`section-headings`
and inline section descriptions: ::

    module Image
      ( -- * Image importers
        --
        -- | There is a "smart" importer, 'readImage', that determines
        -- the image format from the file extension, and several
        -- "dumb" format-specific importers that decode the file as
        -- the specified type.
        readImage
      , readPngImage
      , readGifImage
      , ...
        -- * Image exporters
        -- ...
      ) where

    import Image.Types ( Image )

    -- | Read an image, guessing the format from the file name.
    readImage :: FilePath -> IO Image
    readImage = ...

    -- | Read a GIF.
    readGifImage :: FilePath -> IO Image
    readGifImage = ...

    -- | Read a PNG.
    readPngImage :: FilePath -> IO Image
    readPngImage = ...

    ...

Note that the order of the entities ``readPngImage`` and
``readGifImage`` in the export list is different from the order of the
actual declarations farther down; the order in the export list is the
order used in the generated docs. Also, the imported ``Image`` type
itself is not re-exported, so it will not be included in the rendered
docs (see :ref:`hyperlinking-re-exported`).

The second example, using an export list with a section description
defined elsewhere (the ``$imageImporters``; see :ref:`named-chunks`):
::

    module Image
      ( -- * Image importers
        --
        -- $imageImporters
        readImage
      , readPngImage
      , readGifImage
      , ...
        -- * Image exporters
        -- ...
      ) where

    import Image.Types ( Image )

    -- $imageImporters
    --
    -- There is a "smart" importer, 'readImage', that determines the
    -- image format from the file extension, and several "dumb"
    -- format-specific importers that decode the file as the specified
    -- type.

    -- | Read an image, guessing the format from the file name.
    readImage :: FilePath -> IO Image
    readImage = ...

    -- | Read a GIF.
    readGifImage :: FilePath -> IO Image
    readGifImage = ...

    -- | Read a PNG.
    readPngImage :: FilePath -> IO Image
    readPngImage = ...

    ...

This produces the same rendered docs as the first example, but the
source code itself is arguably more readable, since the documentation
for the group of importer functions is closer to their definitions.

The third example, without an export list: ::

    module Image where

    import Image.Types ( Image )

    -- * Image importers
    --
    -- $imageImporters
    --
    -- There is a "smart" importer, 'readImage', that determines the
    -- image format from the file extension, and several "dumb"
    -- format-specific importers that decode the file as the specified
    -- type.

    -- | Read an image, guessing the format from the file name.
    readImage :: FilePath -> IO Image
    readImage = ...

    -- | Read a GIF.
    readGifImage :: FilePath -> IO Image
    readGifImage = ...

    -- | Read a PNG.
    readPngImage :: FilePath -> IO Image
    readPngImage = ...

    ...

    -- * Image exporters
    -- ...

Note that the section headers (e.g. ``-- * Image importers``) now
appear in the module body itself, and that the section documentation
is still given using :ref:`named-chunks`. Unlike in the first example
when using an export list, the named chunk syntax ``$imageImporters``
*must* be used for the section documentation; attempting to use the
``-- | ...`` syntax to document the image importers here will wrongly
associate the documentation chunk with the next definition!

.. _section-headings:

Section Headings
~~~~~~~~~~~~~~~~

You can insert headings and sub-headings in the documentation by
including annotations at the appropriate point in the export list, or
in the module body directly when not using an export list.

For example: ::

    module Foo (
      -- * Classes
      C(..),
      -- * Types
      -- ** A data type
      T,
      -- ** A record
      R,
      -- * Some functions
      f, g
      ) where

Headings are introduced with the syntax ``-- *``, ``-- **`` and so
on, where the number of ``*``\ s indicates the level of the heading
(section, sub-section, sub-sub-section, etc.).

If you use section headings, then Haddock will generate a table of
contents at the top of the module documentation for you.

By default, when generating HTML documentation Haddock will create an
anchor to each section of the form ``#g:n``, where ``n`` is an integer
that might change as you add new section headings. If you want to
create stable links, you can add an explicit anchor (see
:ref:`anchors`) after the section heading: ::

  module Foo (
    -- * Classes #classes#
    C(..)
  ) where

This will create an HTML anchor ``#g:classes`` to the section.

The alternative style of placing the commas at the beginning of each
line is also supported, e.g.: ::

    module Foo (
      -- * Classes
        C(..)
      -- * Types
      -- ** A data type
      , T
      -- ** A record
      , R
      -- * Some functions
      , f
      , g
      ) where

When not using an export list, you may insert section headers in the
module body. Such section headers associate with all entities
declared up until the next section header. For example: ::

    module Foo where

    -- * Classes
    class C a where ...

    -- * Types
    -- ** A data type
    data T = ...

    -- ** A record
    data R = ...

    -- * Some functions
    f :: ...
    f = ...
    g :: ...
    g = ...

.. _re-exporting-entire-module:

Re-Exporting an Entire Module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Haskell allows you to re-export the entire contents of a module (or at
least, everything currently in scope that was imported from a given
module) by listing it in the export list: ::

    module A (
      module B,
      module C
     ) where

What will the Haddock-generated documentation for this module look like?
Well, it depends on how the modules ``B`` and ``C`` are imported. If
they are imported wholly and without any ``hiding`` qualifiers, then the
documentation will just contain a cross-reference to the documentation
for ``B`` and ``C``.

However, if the modules are not *completely* re-exported, for example:
::

    module A (
      module B,
      module C
     ) where

    import B hiding (f)
    import C (a, b)

then Haddock behaves as if the set of entities re-exported from ``B``
and ``C`` had been listed explicitly in the export list [#notImplemented]_.

.. Comment: was this ever implemented? Perhaps this part of the docs
   should just be removed until it is implemented?

.. [#notImplemented] This is not implemented at the time of writing
   (Haddock version 2.17.3 with GHC 8.0.2). At the moment, Haddock
   always inserts a module cross-reference.

The exception to this rule is when the re-exported module is declared
with the ``hide`` attribute (see :ref:`module-attrs`), in which
case the module is
never cross-referenced; the contents are always expanded in place in the
re-exporting module.

.. _named-chunks:

(Named) Chunks of Documentation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is often desirable to include a chunk of documentation which is not
attached to any particular Haskell declaration, for example, when
giving summary documentation for a group of related definitions (see
:ref:`structure-examples`). In addition to including such documentation
chunks at the top of the file, as part of the
:ref:`module-description`, you can also associate them with
:ref:`section-headings`.

There are several ways to associate documentation chunks with section
headings, depending on whether you are using an export list or not:

-  The documentation can be included in the export list directly, by
   preceding it with a ``-- |``. For example: ::

       module Foo (
          -- * A section heading

          -- | Some documentation not attached to a particular Haskell entity
          ...
        ) where

   In this case the chunk is not "named".

-  If the documentation is large and placing it inline in the export
   list might bloat the export list and obscure the structure, then it
   can be given a name and placed out of line in the body of the module.
   This is achieved with a special form of documentation annotation
   ``-- $``, which we call a *named chunk*: ::

       module Foo (
          -- * A section heading

          -- $doc
          ...
        ) where

       -- $doc
       -- Here is a large chunk of documentation which may be referred to by
       -- the name $doc.

   The documentation chunk is given a name of your choice (here
   ``doc``), which is the sequence of alphanumeric characters directly
   after the ``-- $``, and it may be referred to by the same name in
   the export list. Note that named chunks must come *after* any
   imports in the module body.

-  If you aren't using an export list, then your only choice is to use
   a named chunk with the ``-- $`` syntax. For example: ::

       module Foo where

       -- * A section heading
       --
       -- $doc
       -- Here is a large chunk of documentation which may be referred to by
       -- the name $doc.

   Just like with entity declarations when not using an export list,
   named chunks of documentation are associated with the preceding
   section header here, or with the implicit top-level documentation
   section if there is no preceding section header.

   **Warning**: the form used in the first bullet above, where the
   chunk is not named, *does not work* when you aren't using an
   export list. For example: ::

       module Foo where

       -- * A section heading
       --
       -- | Some documentation not attached to a particular Haskell entity

       -- | The fooifier.
       foo :: ...

   will result in ``Some documentation not ...`` being attached to the
   *next* entity declaration, here ``foo``, in addition to any other
   documentation that next entity already has!

.. _hyperlinking-re-exported:

Hyperlinking and Re-Exported Entities
-------------------------------------

When Haddock renders a type in the generated documentation, it
hyperlinks all the type constructors and class names in that type to
their respective definitions. But for a given type constructor or class
there may be several modules re-exporting it, and therefore several
modules whose documentation contains the definition of that type or
class (possibly including the current module!) so which one do we link
to?

Let's look at an example. Suppose we have three modules ``A``, ``B`` and
``C`` defined as follows: ::

    module A (T) where
    data T a = C a

    module B (f) where
    import A
    f :: T Int -> Int
    f (C i) = i

    module C (T, f) where
    import A
    import B

Module ``A`` exports a datatype ``T``. Module ``B`` imports ``A`` and
exports a function ``f`` whose type refers to ``T``. Also, both ``T``
and ``f`` are re-exported from module C.

Haddock takes the view that each entity has a *home* module; that is,
the module that the library designer would most like to direct the user
to, to find the documentation for that entity. So, Haddock makes all
links to an entity point to the home module. The one exception is when
the entity is also exported by the current module: Haddock makes a local
link if it can.

How is the home module for an entity determined? Haddock uses the
following rules:

-  If modules A and B both export the entity, and module A imports
   (directly or indirectly) module B, then B is preferred.

-  A module with the ``hide`` attribute is never chosen as the home.

-  A module with the ``not-home`` attribute is only chosen if there are
   no other modules to choose.

If multiple modules fit the criteria, then one is chosen at random. If
no modules fit the criteria (because the candidates are all hidden),
then Haddock will issue a warning for each reference to an entity
without a home.

In the example above, module ``A`` is chosen as the home for ``T``
because it does not import any other module that exports ``T``. The link
from ``f``'s type in module ``B`` will therefore point to ``A.T``.
However, ``C`` also exports ``T`` and ``f``, and the link from ``f``'s
type in ``C`` will therefore point locally to ``C.T``.

.. _module-attrs:

Module Attributes
-----------------

Certain attributes may be specified for each module which affect the
way that Haddock generates documentation for that module. Attributes are
specified in a comma-separated list in an
``{-# OPTIONS_HADDOCK ... #-}`` pragma at the top of the module, either
before or after the module description. For example: ::

    {-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

    -- |Module description
    module A where
    ...

The options and module description can be in either order.

The following attributes are currently understood by Haddock:

``hide``
    Omit this module from the generated documentation, but nevertheless
    propagate definitions and documentation from within this module to
    modules that re-export those definitions.

``prune``
    Omit definitions that have no documentation annotations from the
    generated documentation.

``ignore-exports``
    Ignore the export list. Generate documentation as if the module had
    no export list - i.e. all the top-level declarations are exported,
    and section headings may be given in the body of the module.

``not-home``
    Indicates that the current module should not be considered to be the
    home module for each entity it exports, unless that entity is not
    exported from any other module. See :ref:`hyperlinking-re-exported`
    for more details.

``show-extensions``
    Indicates that we should render the extensions used in this module
    in the resulting documentation. This will only render if the output
    format supports it. If Language is set, it will be shown as well and
    all the extensions implied by it won't. All enabled extensions will
    be rendered, including those implied by their more powerful
    versions.

.. _markup:

Markup
------

Haddock understands certain textual cues inside documentation
annotations that tell it how to render the documentation. The cues (or
“markup”) have been designed to be simple and mnemonic in ASCII so
the programmer doesn't have to deal with heavyweight annotations when
editing documentation comments.

Paragraphs
~~~~~~~~~~

One or more blank lines separates two paragraphs in a documentation
comment.

Special Characters
~~~~~~~~~~~~~~~~~~

The following characters have special meanings in documentation comments:
``\``, ``/``, ``'``, `````, ``"``, ``@``, ``<``, ``$``, ``#``. To insert a
literal occurrence of one of these special characters, precede it with a
backslash (``\``).

Additionally, the character ``>`` has a special meaning at the beginning
of a line, and the following characters have special meanings at the
beginning of a paragraph: ``*``, ``-``. These characters can also be
escaped using ``\``.

Furthermore, the character sequence ``>>>`` has a special meaning at the
beginning of a line. To escape it, just prefix the characters in the
sequence with a backslash.

Character References
~~~~~~~~~~~~~~~~~~~~

Although Haskell source files may contain any character from the Unicode
character set, the encoding of these characters as bytes varies between
systems. Consequently, only source files restricted to the ASCII character set
are portable. Other characters may be specified in character and string
literals using Haskell character escapes. To represent such characters
in documentation comments, Haddock supports SGML-style numeric character
references of the forms ``&#``\ D\ ``;`` and ``&#x``\ H\ ``;`` where D
and H are decimal and hexadecimal numbers denoting a code position in
Unicode (or ISO 10646). For example, the references ``&#x3BB;``,
``&#x3bb;`` and ``&#955;`` all represent the lower-case letter lambda.

Code Blocks
~~~~~~~~~~~

Displayed blocks of code are indicated by surrounding a paragraph with
``@...@`` or by preceding each line of a paragraph with ``>`` (we often
call these “bird tracks”). For example: ::

    -- | This documentation includes two blocks of code:
    --
    -- @
    --     f x = x + x
    -- @
    --
    -- >  g x = x * 42

There is an important difference between the two forms of code block: in
the bird-track form, the text to the right of the ‘\ ``>``\ ’ is
interpreted literally, whereas the ``@...@`` form interprets markup as
normal inside the code block. In particular, ``/`` is markup for italics,
and so e.g. ``@x / y / z@`` renders as ``x`` followed by italic
``y`` with no slashes, followed by ``z``.

Examples
~~~~~~~~

Haddock has markup support for examples of interaction with a
*read-eval-print loop (REPL)*. An example is introduced with ``>>>``
followed by an expression followed by zero or more result lines: ::

    -- | Two examples are given below:
    --
    -- >>> fib 10
    -- 55
    --
    -- >>> putStrLn "foo\nbar"
    -- foo
    -- bar

Result lines that only contain the string ``<BLANKLINE>`` are rendered
as blank lines in the generated documentation.

Properties
~~~~~~~~~~

Haddock provides markup for properties: ::

    -- | Addition is commutative:
    --
    -- prop> a + b = b + a

This allows third-party applications to extract and verify them.

Hyperlinked Identifiers
~~~~~~~~~~~~~~~~~~~~~~~

Referring to a Haskell identifier, whether it be a type, class,
constructor, or function, is done by surrounding it with a combination
of single quotes and backticks. For example: ::

    -- | This module defines the type 'T'.

```T``` is also ok. ``'T``` and ```T'`` are accepted but less common.

If there is an entity ``T`` in scope in the current module, then the
documentation will hyperlink the reference in the text to the definition
of ``T`` (if the output format supports hyperlinking, of course; in a
printed format it might instead insert a page reference to the
definition).

It is also possible to refer to entities that are not in scope in the
current module, by giving the full qualified name of the entity: ::

    -- | The identifier 'M.T' is not in scope

If ``M.T`` is not otherwise in scope, then Haddock will simply emit a
link pointing to the entity ``T`` exported from module ``M`` (without
checking to see whether either ``M`` or ``M.T`` exist).

Since values and types live in different namespaces in Haskell, it is possible
for a reference such as ``'X'`` to be ambiguous. In such a case, Haddock
defaults to pointing to the type. The ambiguity can be overcome by explicitly
specifying a namespace, by way of a ``v`` (for value) or ``t`` (for type)
immediately before the link: ::

    -- | An implicit reference to  'X', the type constructor
    --   An explicit reference to v'X', the data constructor
    --   An explicit reference to t'X', the type constructor
    data X = X

To make life easier for documentation writers, a quoted identifier is
only interpreted as such if the quotes surround a lexically valid
Haskell identifier. This means, for example, that it normally isn't
necessary to escape the single quote when used as an apostrophe: ::

    -- | I don't have to escape my apostrophes; great, isn't it?

Nothing special is needed to hyperlink identifiers which contain
apostrophes themselves: to hyperlink ``foo'`` one would simply type
``'foo''``. Hyperlinking operators works in exactly the same way. ::

    -- | A prefix operator @'(++)'@ and an infix identifier @'`elem`'@.

Emphasis, Bold and Monospaced Text
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Emphasis may be added by surrounding text with ``/.../``. Other markup
is valid inside emphasis. To have a forward slash inside of emphasis,
just escape it: ``/fo\/o/``

Bold (strong) text is indicated by surrounding it with ``__...__``.
Other markup is valid inside bold. For example, ``__/foo/__`` will make
the emphasised text ``foo`` bold. You don't have to escape a single
underscore if you need it bold:
``__This_text_with_underscores_is_bold__``.

Monospaced (or typewriter) text is indicated by surrounding it with
``@...@``. Other markup is valid inside a monospaced span: for example
``@'f' a b@`` will hyperlink the identifier ``f`` inside the code
fragment, but ``@__FILE__@`` will render ``FILE`` in bold with no
underscores, which may not be what you had in mind.

Linking to Modules
~~~~~~~~~~~~~~~~~~

Linking to a module is done by surrounding the module name with double
quotes: ::

    -- | This is a reference to the "Foo" module.

A basic check is done on the syntax of the header name to ensure that it
is valid before turning it into a link but unlike with identifiers,
whether the module is in scope isn't checked and will always be turned
into a link.

It is also possible to specify alternate text for the generated link
using syntax analogous to that used for URLs: ::

  -- | This is a reference to [the main module]("Module.Main").

Itemized and Enumerated Lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A bulleted item is represented by preceding a paragraph with either
“``*``” or “``-``”. A sequence of bulleted paragraphs is rendered as an
itemized list in the generated documentation, e.g.: ::

    -- | This is a bulleted list:
    --
    --     * first item
    --
    --     * second item

An enumerated list is similar, except each paragraph must be preceded by
either “``(n)``” or “``n.``” where n is any integer. e.g. ::

    -- | This is an enumerated list:
    --
    --     (1) first item
    --
    --     2. second item

Lists of the same type don't have to be separated by a newline: ::

    -- | This is an enumerated list:
    --
    --     (1) first item
    --     2. second item
    --
    -- This is a bulleted list:
    --
    --     * first item
    --     * second item

You can have more than one line of content in a list element: ::

    -- |
    -- * first item
    -- and more content for the first item
    -- * second item
    -- and more content for the second item

You can even nest whole paragraphs inside of list elements. The rules
are 4 spaces for each indentation level. You're required to use a
newline before such nested paragraphs: ::

    {-|
    * Beginning of list
    This belongs to the list above!

        > nested
        > bird
        > tracks

        * Next list
        More of the indented list.

            * Deeper

                @
                even code blocks work
                @

                * Deeper

                        1. Even deeper!
                        2. No newline separation even in indented lists.
    -}

The indentation of the first list item is honoured. That is, in the
following example the items are on the same level. Before Haddock
2.16.1, the second item would have been nested under the first item
which was unexpected. ::

    {-|
        * foo

        * bar
    -}

Definition Lists
~~~~~~~~~~~~~~~~

Definition lists are written as follows: ::

    -- | This is a definition list:
    --
    --   [@foo@]: The description of @foo@.
    --
    --   [@bar@]: The description of @bar@.

To produce output something like this:

``foo``
    The description of ``foo``.

``bar``
    The description of ``bar``.

Each paragraph should be preceded by the “definition term” enclosed in
square brackets and followed by a colon. Other markup operators may be
used freely within the definition term. You can escape ``]`` with a
backslash as usual.

Same rules about nesting and no newline separation as for bulleted and
numbered lists apply.

URLs
~~~~

A URL can be included in a documentation comment by surrounding it in
angle brackets, for example: ::

    <http://example.com>

If the output format supports it, the URL will be turned into a
hyperlink when rendered.

If Haddock sees something that looks like a URL (such as something
starting with ``http://`` or ``ssh://``) where the URL markup is valid,
it will automatically make it a hyperlink.

Links
~~~~~

Haddock supports Markdown syntax for inline links. A link consists of a
link text and a URL. The link text is enclosed in square brackets and
followed by the URL enclosed in regular parentheses, for example: ::

    [some link](http://example.com)

The link text is used as a description for the URL if the output
format supports it.

Hint: There's a `known issue <https://github.com/haskell/haddock/issues/774>`_ 
that any inline link at the beginning of a line within a multi-line comment 
isn't rendered correctly: ::
   
    {-| Some multi-line comment that has a
    [link](https://example.com) and a
    [reference link]: https://example.com
    -}
    
Adding a space or a word in front of such a link can be used as a workaround.   
   
Images
~~~~~~

Haddock supports Markdown syntax for inline images. This resembles the
syntax for links, but starts with an exclamation mark. An example looks
like this: ::

    ![image description](pathtoimage.png)

If the output format supports it, the image will be rendered inside the
documentation. The image description is used as replacement text and/or
an image title.

Mathematics / LaTeX
~~~~~~~~~~~~~~~~~~~

Haddock supports LaTeX syntax for rendering mathematical notation. The
delimiters are ``\[...\]`` for displayed mathematics and ``\(...\)``
for in-line mathematics. An example looks like this: ::

  \[
  f(a) = \frac{1}{2\pi i}\oint_\gamma \frac{f(z)}{z-a}\,\mathrm{d}z
  \]

If the output format supports it, the mathematics will be rendered
inside the documentation. For example, the HTML backend will display
the mathematics via `MathJax <https://www.mathjax.org>`__.

Grid Tables
~~~~~~~~~~~

Inspired by reSTs grid tables, Haddock supports a complete table representation
via grid-like "ASCII art". Grid tables are described with a visual grid made
up of the characters "-", "=", "|", and "+". The hyphen ("-") is used for
horizontal lines (row separators). The equals sign ("=") may be used to
separate optional header rows from the table body. The vertical bar ("|") is
used for vertical lines (column separators). The plus sign ("+") is used for
intersections of horizontal and vertical lines. ::

    -- | This is a grid table:
    --
    -- +------------------------+------------+----------+----------+
    -- | Header row, column 1   | Header 2   | Header 3 | Header 4 |
    -- | (header rows optional) |            |          |          |
    -- +========================+============+==========+==========+
    -- | body row 1, column 1   | column 2   | column 3 | column 4 |
    -- +------------------------+------------+----------+----------+
    -- | body row 2             | Cells may span columns.          |
    -- +------------------------+------------+---------------------+
    -- | body row 3             | Cells may  | \[                  |
    -- +------------------------+ span rows. | f(n) = \sum_{i=1}   |
    -- | body row 4             |            | \]                  |
    -- +------------------------+------------+---------------------+

.. _anchors:

Anchors
~~~~~~~

Sometimes it is useful to be able to link to a point in the
documentation which doesn't correspond to a particular entity. For that
purpose, we allow *anchors* to be included in a documentation comment.
The syntax is ``#label#``, where label is the name of the anchor. An
anchor is invisible in the generated documentation.

To link to an anchor from elsewhere, use the syntax ``"module#label"``
where module is the module name containing the anchor, and label is the
anchor label. The module does not have to be local, it can be imported
via an interface. Please note that in Haddock versions 2.13.x and
earlier, the syntax was ``"module\#label"``. It is considered deprecated
and will be removed in the future.

Headings
~~~~~~~~

Headings inside of comment documentation are possible by preceding them
with a number of ``=``\ s. From 1 to 6 are accepted. Extra ``=``\ s will
be treated as belonging to the text of the heading. Note that it's up to
the output format to decide how to render the different levels. ::

    -- |
    -- = Heading level 1 with some /emphasis/
    -- Something underneath the heading.
    --
    -- == /Subheading/
    -- More content.
    --
    -- === Subsubheading
    -- Even more content.

Note that while headings have to start on a new paragraph, we allow
paragraph-level content to follow these immediately. ::

    -- |
    -- = Heading level 1 with some __bold__
    -- Something underneath the heading.
    --
    -- == /Subheading/
    -- More content.
    --
    -- === Subsubheading
    -- >>> examples are only allowed at the start of paragraphs

As of 2.15.1, there's experimental (read: subject to change or get
removed) support for collapsible headers: simply wrap your existing
header title in underscores, as per bold syntax. The collapsible section
will stretch until the end of the comment or until a header of equal or
smaller number of ``=``\ s. ::

    -- |
    -- === __Examples:__
    -- >>> Some very long list of examples
    --
    -- ==== This still falls under the collapse
    -- Some specialised examples
    --
    -- === This is does not go into the collapsable section.
    -- More content.

Metadata
~~~~~~~~

Since Haddock 2.16.0, some support for embedding metadata in the
comments has started to appear. The use of such data aims to standardise
various community conventions in how such information is conveyed and to
provide uniform rendering.

Since
^^^^^

``@since`` annotation can be used to convey information about when the
function was introduced or when it has changed in a way significant to
the user. ``@since`` is a paragraph-level element. While multiple such
annotations are not an error, only the one to appear in the comment last
will be used. ``@since`` has to be followed with a version number, no
further description is currently allowed. The meaning of this feature is
subject to change in the future per user feedback. ::

    -- |
    -- Some comment
    --
    -- @since 1.2.3
