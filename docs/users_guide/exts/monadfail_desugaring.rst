.. _monadfail-desugaring:

New monadic failure desugaring mechanism
----------------------------------------

.. extension:: MonadFailDesugaring
    :shortdesc: Enable monadfail desugaring.

    :since: 8.0.1

    Use the ``MonadFail.fail`` instead of the legacy ``Monad.fail`` function
    when desugaring refutable patterns in ``do`` blocks.

The ``-XMonadFailDesugaring`` extension switches the desugaring of
``do``-blocks to use ``MonadFail.fail`` instead of ``Monad.fail``.

This extension is enabled by default since GHC 8.6.1, under the
`MonadFail Proposal (MFP)
<https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail>`__.

This extension is temporary, and will be deprecated in a future release.


