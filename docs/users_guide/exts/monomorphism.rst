.. _monomorphism:

Switching off the Monomorphism Restriction
------------------------------------------

.. extension:: MonomorphismRestriction
    :shortdesc: Apply the Haskell 2010 monomorphism restriction.

    :status: Enabled by default.
    :since: 6.8.1

    Apply the monomorphism restriction to bindings lacking explicit type
    signatures.

Haskell's monomorphism restriction (see `Section
4.5.5 <https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5>`__ of
the Haskell Report) can be completely switched off by
:extension:`NoMonomorphismRestriction`. Since GHC 7.8.1, the monomorphism
restriction is switched off by default in GHCi's interactive options
(see :ref:`ghci-interactive-options`).


