.. _relaxed-poly-rec:

Generalised typing of mutually recursive bindings
-------------------------------------------------

.. extension:: RelaxedPolyRec
    :shortdesc: Generalised typing of mutually recursive bindings.

    :since: 6.8.1

    :status: Included in :extension:`GHC2024`, :extension:`GHC2021`, :extension:`Haskell2010`

See :ref:`infelicities-recursive-groups` for a description of this extension.
This is a long-standing GHC extension. Around the time of GHC 7.6.3, this
extension became required as part of a typechecker refactoring.
The ``-XRelaxedPolyRec`` flag is now deprecated (since the feature is always
enabled) and may be removed at some future time.
