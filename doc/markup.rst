Documentation and Markup
========================

Haddock understands special documentation annotations in the Haskell
source file and propagates these into the generated documentation. The
annotations are purely optional: if there are no annotations, Haddock
will just generate documentation that contains the type signatures, data
type declarations, and class declarations exported by each of the
modules being processed.

Documenting a top-level declaration
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

-  A ``data`` declaration,

-  A ``newtype`` declaration,

-  A ``type`` declaration

-  A ``class`` declaration,

-  A ``data family`` or ``type family`` declaration, or

-  A ``data instance`` or ``type instance`` declaration.

If the annotation is followed by a different kind of declaration, it
will probably be ignored by Haddock.

Some people like to write their documentation *after* the declaration;
this is possible in Haddock too: ::

    square :: Int -> Int
    -- ^The 'square' function squares an integer.
    square x = x * x

Note that Haddock doesn't contain a Haskell type system — if you don't
write the type signature for a function, then Haddock can't tell what
its type is and it won't be included in the documentation.

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

Documenting parts of a declaration
----------------------------------

In addition to documenting the whole declaration, in some cases we can
also document individual parts of the declaration.

Class methods
~~~~~~~~~~~~~

Class methods are documented in the same way as top level type
signatures, by using either the ``-- |`` or ``-- ^`` annotations: ::

    class C a where
       -- | This is the documentation for the 'f' method
       f :: a -> Int
       -- | This is the documentation for the 'g' method
       g :: Int -> a

Constructors and record fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constructors are documented like so: ::

    data T a b
      -- | This is the documentation for the 'C1' constructor
      = C1 a b
      -- | This is the documentation for the 'C2' constructor
      | C2 a b

or like this: ::

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

In case that more than one constructor exports a field with the same
name, the documentation attached to the first occurence of the field
will be used, even if a comment is not present. ::

    data T a = A { someField :: a -- ^ Doc for someField of A
                 }
             | B { someField :: a -- ^ Doc for someField of B
                 }

In the above example, all occurences of ``someField`` in the
documentation are going to be documented with
``Doc for someField of A``. Note that Haddock versions 2.14.0 and before
would join up documentation of each field and render the result. The
reason for this seemingly weird behaviour is the fact that ``someField``
is actually the same (partial) function.

Function arguments
~~~~~~~~~~~~~~~~~~

Individual arguments to a function may be documented like this: ::

    f  :: Int      -- ^ The 'Int' argument
       -> Float    -- ^ The 'Float' argument
       -> IO ()    -- ^ The return value

The module description
----------------------

A module itself may be documented with multiple fields that can then be
displayed by the backend. In particular, the HTML backend displays all
the fields it currently knows about. We first show the most complete
module documentation example and then talk about the fields. ::

    {-|
    Module      : W
    Description : Short description
    Copyright   : (c) Some Guy, 2013
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

The “Module” field should be clear. It currently doesn't affect the
output of any of the backends but you might want to include it for human
information or for any other tools that might be parsing these comments
without the help of GHC.

The “Description” field accepts some short text which outlines the
general purpose of the module. If you're generating HTML, it will show
up next to the module link in the module index.

The “Copyright”, “License”, “Maintainer” and “Stability” fields should
be obvious. An alternative spelling for the “License” field is accepted
as “Licence” but the output will always prefer “License”.

The “Portability” field has seen varied use by different library
authors. Some people put down things like operating system constraints
there while others put down which GHC extensions are used in the module.
Note that you might want to consider using the “show-extensions” module
flag for the latter.

Finally, a module may contain a documentation comment before the module
header, in which case this comment is interpreted by Haddock as an
overall description of the module itself, and placed in a section
entitled “Description” in the documentation for the module. All usual
Haddock markup is valid in this comment.

All fields are optional but they must be in order if they do appear.
Multi-line fields are accepted but the consecutive lines have to start
indented more than their label. If your label is indented one space as
is often the case with “--” syntax, the consecutive lines have to start
at two spaces at the very least. Please note that we do not enforce the
format for any of the fields and the established formats are just a
convention.

Controlling the documentation structure
---------------------------------------

Haddock produces interface documentation that lists only the entities
actually exported by the module. The documentation for a module will
include *all* entities exported by that module, even if they were
re-exported by another module. The only exception is when Haddock can't
see the declaration for the re-exported entity, perhaps because it isn't
part of the batch of modules currently being processed.

However, to Haddock the export list has even more significance than just
specifying the entities to be included in the documentation. It also
specifies the *order* that entities will be listed in the generated
documentation. This leaves the programmer free to implement functions in
any order he/she pleases, and indeed in any *module* he/she pleases, but
still specify the order that the functions should be documented in the
export list. Indeed, many programmers already do this: the export list
is often used as a kind of ad-hoc interface documentation, with
headings, groups of functions, type signatures and declarations in
comments.

You can insert headings and sub-headings in the documentation by
including annotations at the appropriate point in the export list. For
example: ::

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

The alternative style of placing the commas at the beginning of each
line is also supported. e.g.: ::

    module Foo (
      -- * Classes
      , C(..)
      -- * Types
      -- ** A data type
      , T
      -- ** A record
      , R
      -- * Some functions
      , f
      , g
      ) where

Re-exporting an entire module
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
for ``B`` and ``C``. However, if the modules are not *completely*
re-exported, for example: ::

    module A (
      module B,
      module C
     ) where

    import B hiding (f)
    import C (a, b)

then Haddock behaves as if the set of entities re-exported from ``B``
and ``C`` had been listed explicitly in the export list [2]_.

.. [2]
   NOTE: this is not fully implemented at the time of writing (version
   0.2). At the moment, Haddock always inserts a cross-reference.

The exception to this rule is when the re-exported module is declared
with the ``hide`` attribute (:ref:`module-attrs`), in which case the module is
never cross-referenced; the contents are always expanded in place in the
re-exporting module.

Omitting the export list
~~~~~~~~~~~~~~~~~~~~~~~~

If there is no export list in the module, how does Haddock generate
documentation? Well, when the export list is omitted, e.g.: ::

    module Foo where

this is equivalent to an export list which mentions every entity defined
at the top level in this module, and Haddock treats it in the same way.
Furthermore, the generated documentation will retain the order in which
entities are defined in the module. In this special case the module body
may also include section headings (normally they would be ignored by
Haddock). ::

    module Foo where

    -- * This heading will now appear before foo.

    -- | Documentation for 'foo'.
    foo :: Integer
    foo = 5

Named chunks of documentation
-----------------------------

Occasionally it is desirable to include a chunk of documentation which
is not attached to any particular Haskell declaration. There are two
ways to do this:

-  The documentation can be included in the export list directly, e.g.: ::

       module Foo (
          -- * A section heading

          -- | Some documentation not attached to a particular Haskell entity
          ...
        ) where

-  If the documentation is large and placing it inline in the export
   list might bloat the export list and obscure the structure, then it
   can be given a name and placed out of line in the body of the module.
   This is achieved with a special form of documentation annotation
   “``-- $``”: ::

       module Foo (
          -- * A section heading

          -- $doc
          ...
        ) where

       -- $doc
       -- Here is a large chunk of documentation which may be referred to by
       -- the name $doc.

   The documentation chunk is given a name, which is the sequence of
   alphanumeric characters directly after the “``-- $``”, and it may be
   referred to by the same name in the export list.

Hyperlinking and re-exported entities
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

Certain attributes may be specified for each module which affects the
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

``hide`` ``hide``
    Omit this module from the generated documentation, but nevertheless
    propagate definitions and documentation from within this module to
    modules that re-export those definitions.

``hide`` ``prune``
    Omit definitions that have no documentation annotations from the
    generated documentation.

``ignore-exports`` ``ignore-exports``
    Ignore the export list. Generate documentation as if the module had
    no export list - i.e. all the top-level declarations are exported,
    and section headings may be given in the body of the module.

``not-home`` ``not-home``
    Indicates that the current module should not be considered to be the
    home module for each entity it exports, unless that entity is not
    exported from any other module. See ? for more details.

``show-extensions`` ``show-extensions``
    Indicates that we should render the extensions used in this module
    in the resulting documentation. This will only render if the output
    format supports it. If Language is set, it will be shown as well and
    all the extensions implied by it won't. All enabled extensions will
    be rendered, including those implied by their more powerful
    versions.

Markup
------

Haddock understands certain textual cues inside documentation
annotations that tell it how to render the documentation. The cues (or
“markup”) have been designed to be simple and mnemonic in ASCII so that
the programmer doesn't have to deal with heavyweight annotations when
editing documentation comments.

Paragraphs
~~~~~~~~~~

One or more blank lines separates two paragraphs in a documentation
comment.

Special characters
~~~~~~~~~~~~~~~~~~

The following characters have special meanings in documentation
comments: ``\\``, ``/``, ``'``, ``\```, ``"``, ``@``, ``<``. To insert a
literal occurrence of one of these special characters, precede it with a
backslash (``\\``).

Additionally, the character ``>`` has a special meaning at the beginning
of a line, and the following characters have special meanings at the
beginning of a paragraph: ``*``, ``-``. These characters can also be
escaped using ``\\``.

Furthermore, the character sequence ``>>>`` has a special meaning at the
beginning of a line. To escape it, just prefix the characters in the
sequence with a backslash.

Character references
~~~~~~~~~~~~~~~~~~~~

Although Haskell source files may contain any character from the Unicode
character set, the encoding of these characters as bytes varies between
systems, so that only source files restricted to the ASCII character set
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
normal inside the code block.

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
constructor, or function, is done by surrounding it with single quotes: ::

    -- | This module defines the type 'T'.

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

To make life easier for documentation writers, a quoted identifier is
only interpreted as such if the quotes surround a lexically valid
Haskell identifier. This means, for example, that it normally isn't
necessary to escape the single quote when used as an apostrophe: ::

    -- | I don't have to escape my apostrophes; great, isn't it?

Nothing special is needed to hyperlink identifiers which contain
apostrophes themselves: to hyperlink ``foo'`` one would simply type
``'foo''``. To hyperlink identifiers written in infix form, simply put
them in quotes as always: ``'`elem`'``.

For compatibility with other systems, the following alternative form of
markup is accepted [3]_: ```T'``.

.. [3]
   We chose not to use this as the primary markup for identifiers
   because strictly speaking the ````` character should not be used as a
   left quote, it is a grave accent.

Emphasis, Bold and Monospaced text
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
fragment.

Linking to modules
~~~~~~~~~~~~~~~~~~

Linking to a module is done by surrounding the module name with double
quotes: ::

    -- | This is a reference to the "Foo" module.

A basic check is done on the syntax of the header name to ensure that it
is valid before turning it into a link but unlike with identifiers,
whether the module is in scope isn't checked and will always be turned
into a link.

Itemized and Enumerated lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A bulleted item is represented by preceding a paragraph with either
“``*``” or “``-``”. A sequence of bulleted paragraphs is rendered as an
itemized list in the generated documentation, eg.: ::

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
newline before such nested paragraph: ::

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

Definition lists
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

The link text is used as a descriptive text for the URL, if the output
format supports it.

Images
~~~~~~

Haddock supports Markdown syntax for inline images. This resembles the
syntax for links, but starts with an exclamation mark. An example looks
like this: ::

    ![image description](pathtoimage.png)

If the output format supports it, the image will be rendered inside the
documentation. The image description is used as relpacement text and/or
image title.

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

Headings inside of comment documentation are possible be preceding them
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
function was introduced or when it has changed in the way significant to
the user. ``@since`` is a paragraph-level element. While multiple such
annotations are not an error, only the one to appear in the comment last
will be used. ``@since`` has to be followed with a version number, no
further description is currently allowed. The meaning of this feature is
subject to change in the future per user feedback. ::

    -- |
    -- Some comment
    --
    -- @since 1.2.3
