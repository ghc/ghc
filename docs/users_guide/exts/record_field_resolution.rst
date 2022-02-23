.. _record_field_resolution:

Record field name resolution 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A record field name `x` can be used in four contexts:

#. In a record *construction*: ``C{ x = 3 }``
#. In a record *update*: ``r{ x = 4 }``
#. In a record *pattern match*: ``case r of C{ x = value } -> …``
#. As a standalone record *selector*: ``x r``

In these four cases, here are the rules for field name resolution: 

#. An unqualified name "x" is unambiguous *if and only if* there is just one "x" in scope unqualified.

#. A qualified name "M.x" is unambiguous *if and only if* there is just one "M.x" in scope.

Those rules are amended by the following extensions:

* :extension:`DisambiguateRecordFields`: In record construction and pattern matching
  (``C{ x = …}``), an unqualified field name ``x`` is unambiguous if and only if the data constructor (``C``)
  has a field ``x``, and that field is in scope unqualified, or qualified as ``Q.x``, regardless of ``Q``.
  Similarly, in record construction and pattern matching, a qualified field name ``M.x`` is unambiguous if and only if the the data
  constructor (``C``) has a field ``x``, and that field is in scope qualified as ``M.x``.

  In record updates (``r{ x = 3 }``), the field name x is unambiguous if and only if there is just one field name x in scope unqualified.
  Similarly, the record update with a qualified field ``r{ M.x = 3 }`` is unambiguous if just one
  field name is in scope as ``M.x``.
  In both cases, non-field names are ignored.
* :extension:`DuplicateRecordFields`: This extension allows record updates if exactly
  one type has all the fields being updated, even if they are individually ambiguous according to the two rules for field name resolution above.
  
  For example::

    data S = MkS1 { x :: Int, y :: Bool }
           | MkS2 { x :: Int }

    data T = MkT1 { x :: Int, z :: Bool }
           | MkT2 { z :: Bool }

    f r = r{ x=3, y=True }

The only data type that has both ``x`` and ``y`` as fields is ``S``, so the field names ``x`` and ``y``
refer unambiguously to data type ``S``.

* :extension:`NoFieldSelectors`: This extension being enabled means that field selector names in scope will be ignored in an expression context.

  For example::

    data T = MkT { x :: Int }

    x :: String
    x = "Hello

    f = x

With ``NoFieldSelectors`` the ``x`` in ``f``'s right hand side refers to the ``x :: String``, not to the field ``x``.
