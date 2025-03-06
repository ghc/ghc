.. _overloaded-strings:

Overloaded string literals
--------------------------

.. extension:: OverloadedStrings
    :shortdesc: Desugar string literals via ``IsString`` class.

    :since: 6.8.1

    Enable overloaded string literals (e.g. string literals desugared via the
    ``IsString`` class).

GHC supports *overloaded string literals*. Normally a string literal has
type ``String``, but with overloaded string literals enabled (with
:extension:`OverloadedStrings`) a string literal has type
``(IsString a) => a``.

This means that the usual string syntax can be used, e.g., for
``ByteString``, ``Text``, and other variations of string-like types.
String literals behave very much like integer literals, i.e., they can
be used in both expressions and patterns. If used in a pattern, the
literal will be replaced by an equality test, in the same way as an
integer literal is.

The class ``IsString`` is defined as: ::

    class IsString a where
        fromString :: String -> a

The only predefined instance is the obvious one to make strings work as
usual: ::

    instance IsString [Char] where
        fromString cs = cs

The class ``IsString`` is not in scope by default. If you want to
mention it explicitly (for example, to give an instance declaration for
it), you can import it from module ``Data.String``.

Enabling :extension:`OverloadedStrings` extends Haskell's defaulting mechanism
:ref:`class_defaulting` as follows:

  - Defaulting applies when all the unresolved constraints involve standard
    classes *or* ``IsString``; and at least one is a numeric class
    *or* ``IsString``.

  - `String` is added to the end of the standard list of types which are tried
     when doing type defaulting, and ``default`` declarations are extended to
     allow any type that is an instance of ``IsString``.

So, for example, the expression ``length "foo"`` will give rise to an
ambiguous use of ``IsString a0`` which, because of the above rules, will
default to ``String``.

Users may want to additionally enable :extension:`ExtendedDefaultRules`,
which relaxes the condition that a type variable which appears as the argument
of a non-unary or non-standard class cannot be defaulted.

Users wanting string literals to default to other string-like types – such as
``Text`` or ``ByteString`` – are encouraged to supply their own default
declarations, such as: ::

  default (Int, Double, Text)

or, using :extension:`NamedDefaults`: ::

  default IsString (Text)

A small example: ::

    {-# LANGUAGE OverloadedStrings #-}
    module Main where

    import Data.String( IsString(..) )

    newtype MyString = MyString String deriving (Eq, Show)
    instance IsString MyString where
        fromString = MyString

    greet :: MyString -> MyString
    greet "hello" = "world"
    greet other = other

    main = do
        print $ greet "hello"
        print $ greet "fool"

Note that deriving ``Eq`` is necessary for the pattern matching to work
since it gets translated into an equality comparison.
