.. _syntax-stolen:

Summary of stolen syntax
------------------------

Turning on an option that enables special syntax *might* cause working
Haskell 98 code to fail to compile, perhaps because it uses a variable
name which has become a reserved word. This section lists the syntax
that is "stolen" by language extensions. We use notation and nonterminal
names from the Haskell 98 lexical syntax (see the Haskell 98 Report). We
only list syntax changes here that might affect existing working
programs (i.e. "stolen" syntax). Many of these extensions will also
enable new context-free syntax, but in all cases programs written to use
the new syntax would not be compilable without the option enabled.

There are two classes of special syntax:

-  New reserved words and symbols: character sequences which are no
   longer available for use as identifiers in the program.

-  Other special syntax: sequences of characters that have a different
   meaning when this particular option is turned on.

The following syntax is stolen:

``forall``
    .. index::
       single: forall

    Stolen (in types) by default (see :ref:`infelicities-lexical`). ``forall`` is
    a reserved keyword and never a type variable, in accordance with `GHC Proposal #43
    <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0043-forall-keyword.rst>`__.


``mdo``
    .. index::
       single: mdo

    Stolen by: :extension:`RecursiveDo`

``foreign``
    .. index::
       single: foreign

    Stolen by: :extension:`ForeignFunctionInterface`

``rec``, ``proc``, ``-<``, ``>-``, ``-<<``, ``>>-``, ``(|``, ``|)``
    .. index::
       single: proc

    Stolen by: :extension:`Arrows`

``?varid``
    .. index::
       single: implicit parameters

    Stolen by: :extension:`ImplicitParams`

``[|``, ``[e|``, ``[p|``, ``[d|``, ``[t|``, ``[||``, ``[e||``
    .. index::
       single: Quasi-quotes

    Stolen by: :extension:`QuasiQuotes`. Moreover, this introduces an ambiguity
    with list comprehension syntax. See the
    :ref:`discussion on quasi-quoting <quasi-quotes-list-comprehension-ambiguity>`
    for details.

``$(``, ``$$(``, ``$varid``, ``$$varid``
    .. index::
       single: Template Haskell

    Stolen by: :extension:`TemplateHaskell`

``[varid|``
    .. index::
       single: quasi-quotation

    Stolen by: :extension:`QuasiQuotes`

⟨varid⟩, ``#``\ ⟨char⟩, ``#``, ⟨string⟩, ``#``, ⟨integer⟩, ``#``, ⟨float⟩, ``#``, ⟨float⟩, ``##``
    Stolen by: :extension:`MagicHash`

``(#``, ``#)``
    Stolen by: :extension:`UnboxedTuples`

⟨varid⟩, ``!``, ⟨varid⟩
    Stolen by: :extension:`BangPatterns`

``pattern``
    Stolen by: :extension:`PatternSynonyms`

``static``
    Stolen by: :extension:`StaticPointers`


