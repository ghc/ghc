.. _pattern-guards:

Pattern guards
--------------

.. extension:: PatternGuards
    :shortdesc: Allow pattern guards syntax.

    :since: 6.8.1
    :status: Disabled in :extension:`Haskell98`, enabled in :extension:`Haskell2010` and later.

    Haskell 2010 allows pattern match guards to contain `pattern guards
    <https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-460003.13>`__: ::
    
        case animal of
          Fish { name=nm }
            | "bob" <- nm  -> "it's Bob!"
          --  ^^^^^^^^^^^ a pattern guard

    This extension controls whether pattern guard syntax is allowed,
    independent of language edition.
