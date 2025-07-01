.. _doandifthenelse:

Do And If Then Else
===================

.. extension:: DoAndIfThenElse
    :shortdesc: Allow semicolons in ``if`` expressions.

    :since: 7.0.1

    :status: Included in :extension:`Haskell2010`

    Allow semicolons in ``if`` expressions.

Normally, a conditional is written like this: ``if cond then expr1 else expr2``. With the extension
:extension:`DoAndIfThenElse`, semicolons are allowed before the ``then`` and also before the ``else``, allowing
``if cond; then expr1; else expr2``. (You can also include either semicolon on its own.)

Allowing semicolons in the middle of a conditional is useful in connection with layout-controlled
blocks, like ``do``\ -blocks. This is because GHC invisibly inserts a semicolon between each line of a
layout-controlled block. Accordingly, with :extension:`DoAndIfThenElse`, we can write code like this ::

  f mb x y = do
    b <- mb
    if b
    then x
    else y

Without :extension:`DoAndIfThenElse`, the ``then`` and ``else`` lines would have to be indented with respect
to the rest of the lines in the ``do``\ -block.
