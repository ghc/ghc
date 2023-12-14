.. _overloaded-labels:

Overloaded labels
-----------------

.. extension:: OverloadedLabels
    :shortdesc: Enable overloaded labels.

    :since: 8.0.1

    Enable use of the ``#foo`` overloaded label syntax.

GHC supports *overloaded labels*, a form of identifier whose interpretation may
depend both on its type and on its literal text.  When the
:extension:`OverloadedLabels` extension is enabled, an overloaded label can be written
with a prefix hash, for example ``#foo``.  The type of this expression is
``IsLabel "foo" a => a``.

The class ``IsLabel`` is defined as:

::

    class IsLabel (x :: Symbol) a where
      fromLabel :: a

This is rather similar to the class ``IsString`` (see
:ref:`overloaded-strings`), but with an additional type parameter that makes the
text of the label available as a type-level string (see
:ref:`type-level-literals`).  Note that ``fromLabel`` had an extra ``Proxy# x``
argument in GHC 8.0, but this was removed in GHC 8.2 as a type application (see
:ref:`visible-type-application`) can be used instead.

There are no predefined instances of this class.  It is not in scope by default,
but can be brought into scope by importing
:base-ref:`GHC.OverloadedLabels.`.  Unlike
``IsString``, there are no special defaulting rules for ``IsLabel``.

During typechecking, GHC will replace an occurrence of an overloaded label like
``#foo`` with ``fromLabel @"foo"``.  This will have some type ``alpha`` and
require the solution of a class constraint ``IsLabel "foo" alpha``.

The intention is for ``IsLabel`` to be used to support overloaded record fields
and perhaps anonymous records.  Thus, it may be given instances for base
datatypes (in particular ``(->)``) in the future.

If :extension:`RebindableSyntax` is enabled, overloaded
labels will be desugared using whatever ``fromLabel`` function is in scope,
rather than always using ``GHC.OverloadedLabels.fromLabel``.

When writing an overloaded label, there must be no space between the hash sign
and the following identifier.  The :extension:`MagicHash` extension makes use
of postfix hash signs; if :extension:`OverloadedLabels` and
:extension:`MagicHash` are both enabled then ``x#y`` means ``x# y``, but if
only :extension:`OverloadedLabels` is enabled then it means ``x #y``.  The
:extension:`UnboxedTuples` extension makes ``(#`` a single lexeme, so when
:extension:`UnboxedTuples` is enabled you must write a space between an opening
parenthesis and an overloaded label.  To avoid confusion, you are strongly
encouraged to put a space before the hash when using
:extension:`OverloadedLabels`.

When using :extension:`OverloadedLabels` (or other extensions that make use of
hash signs) in a ``.hsc`` file (see :ref:`hsc2hs`), the hash signs must be
doubled (write ``##foo`` instead of ``#foo``) to avoid them being treated as
``hsc2hs`` directives.

Here is an extension of the record access example in :ref:`type-level-literals`
showing how an overloaded label can be used as a record selector:

::

    {-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses,
                 FunctionalDependencies, FlexibleInstances,
                 OverloadedLabels, ScopedTypeVariables #-}

    import GHC.OverloadedLabels (IsLabel(..))
    import GHC.TypeLits (Symbol)

    data Label (l :: Symbol) = Get

    class Has a l b | a l -> b where
      from :: a -> Label l -> b

    data Point = Point Int Int deriving Show

    instance Has Point "x" Int where from (Point x _) _ = x
    instance Has Point "y" Int where from (Point _ y) _ = y

    instance Has a l b => IsLabel l (a -> b) where
      fromLabel x = from x (Get :: Label l)

    example = #x (Point 1 2)


Since GHC 9.6, any non-empty double quoted string can be used as a label. The
restriction that the label must be a valid identifier has also been lifted.

Examples of newly allowed syntax:

- Leading capital letters: `#Foo` equivalant to `getLabel @"Foo"`

- Numeric characters: `#3.14` equivalent to `getLabel @"3.14"`

- Arbitrary strings: `#"Hello, World!"` equivalent to `getLabel @"Hello, World!"`

Here is an example of the more permissive use of this extension, available since
GHC 9.6:

::

    {-# LANGUAGE DataKinds             #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE OverloadedLabels      #-}
    {-# LANGUAGE MagicHash             #-}

    import Data.Foldable (traverse_)
    import Data.Proxy (Proxy(..))
    import GHC.OverloadedLabels (IsLabel(..))
    import GHC.TypeLits (KnownSymbol, symbolVal)
    import GHC.Prim (Addr#)

    instance KnownSymbol symbol => IsLabel symbol String where
      fromLabel = symbolVal (Proxy :: Proxy symbol)

    (#) :: String -> Int -> String
    (#) _ i = show i

    f :: Addr# -> Int -> String
    f _ i = show i

    main :: IO ()
    main = traverse_ putStrLn
      [ #a
      , #number17
      , #do
      , #type
      , #Foo
      , #3
      , #199.4
      , #17a23b
      , #f'a'
      , #'a'
      , #'
      , #''notTHSplice
      , #...
      , #привет
      , #こんにちは
      , #"3"
      , #":"
      , #"Foo"
      , #"The quick brown fox"
      , #"\""
      , (++) #hello#world
      , (++) #"hello"#"world"
      , #"hello"# 1 -- equivalent to `(fromLabel @"hello") # 1`
      , f "hello"#2 -- equivalent to `f ("hello"# :: Addr#) 2`
      ]

See `GHC Proposal #170 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0170-unrestricted-overloadedlabels.rst>`__
for more details.
