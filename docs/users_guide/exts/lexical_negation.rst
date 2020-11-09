.. _lexical-negation:

Lexical negation
----------------

.. extension:: LexicalNegation
    :shortdesc: Use whitespace to determine whether the minus sign stands for
                negation or subtraction.

    :since: 9.0.1

    Detect if the minus sign stands for negation during lexical analysis by
    checking for the surrounding whitespace.

In Haskell 2010, the minus sign stands for negation when it has no left-hand
side. Consider ``x = - 5`` and ``y = 2 - 5``. In ``x``, there's no expression
between the ``=`` and ``-``, so the minus stands for negation, whereas in
``y``, there's ``2`` to the left of the minus, therefore it stands for
subtraction.

This leads to certain syntactic anomalies:

* ``(% x)`` is an operator section for any operator ``(%)`` except for ``(-)``.
  ``(- x)`` is negated ``x`` rather than the right operator section of
  subtraction. Consequently, it is impossible to write such a section, and
  users are advised to write ``(subtract x)`` instead.

* Negative numbers must be parenthesized when they appear in function argument
  position. ``f (-5)`` is correct, whereas ``f -5`` is parsed as ``(-) f 5``.

The latter issue is partly mitigated by :extension:`NegativeLiterals`. When it
is enabled, ``-5`` is parsed as negative 5 regardless of context, so ``f
-5`` works as expected. However, it only applies to literals, so ``f -x`` or
``f -(a*2)`` are still parsed as subtraction.

With :extension:`LexicalNegation`, both anomalies are resolved:

* ``(% x)`` is an operator section for any operator ``(%)``, no exceptions, as
  long as there's whitespace between ``%`` and ``x``.

* In ``f -x``, the ``-x`` is parsed as the negation of ``x`` for any
  syntactically atomic expression ``x`` (variable, literal, or parenthesized
  expression).

* The prefix ``-`` binds tighter than any infix operator. ``-a % b`` is parsed
  as ``(-a) % b`` regardless of the fixity of ``%``.

This means that ``(- x)`` is the right operator section of subtraction, whereas
``(-x)`` is the negation of ``x``. Note that these expressions will often have
different types (``(- x)`` might have type ``Int -> Int`` while ``(-x)`` will
have type ``Int``), and so users mistaking one for the other will likely get a
compile error.

Under :extension:`LexicalNegation`, negated literals are desugared without
``negate``. That is, ``-123`` stands for ``fromInteger (-123)`` rather than
``negate (fromInteger 123)``. This makes :extension:`LexicalNegation` a valid
replacement for :extension:`NegativeLiterals`.
