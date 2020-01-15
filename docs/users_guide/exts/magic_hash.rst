.. _magic-hash:

The magic hash
--------------

.. extension:: MagicHash
    :shortdesc: Allow ``#`` as a postfix modifier on identifiers.

    :since: 6.8.1

    Enables the use of the hash character (``#``) as an identifier suffix.

The language extension :extension:`MagicHash` allows ``#`` as a postfix modifier
to identifiers. Thus, ``x#`` is a valid variable, and ``T#`` is a valid type
constructor or data constructor.

The hash sign does not change semantics at all. We tend to use variable
names ending in "#" for unboxed values or types (e.g. ``Int#``), but
there is no requirement to do so; they are just plain ordinary
variables. Nor does the :extension:`MagicHash` extension bring anything into
scope. For example, to bring ``Int#`` into scope you must import
``GHC.Prim`` (see :ref:`primitives`); the :extension:`MagicHash` extension then
allows you to *refer* to the ``Int#`` that is now in scope. Note that
with this option, the meaning of ``x#y = 0`` is changed: it defines a
function ``x#`` taking a single argument ``y``; to define the operator
``#``, put a space: ``x # y = 0``.

The :extension:`MagicHash` also enables some new forms of literals (see
:ref:`glasgow-unboxed`):

-  ``'x'#`` has type ``Char#``

-  ``"foo"#`` has type ``Addr#``

-  ``3#`` has type ``Int#``. In general, any Haskell integer lexeme
   followed by a ``#`` is an ``Int#`` literal, e.g. ``-0x3A#`` as well as
   ``32#``.

-  ``3##`` has type ``Word#``. In general, any non-negative Haskell
   integer lexeme followed by ``##`` is a ``Word#``.

-  ``3.2#`` has type ``Float#``.

-  ``3.2##`` has type ``Double#``


