.. _assertions:

Assertions
==========

.. index::
   single: Assertions

If you want to make use of assertions in your standard Haskell code, you
could define a function like the following: ::

    assert :: Bool -> a -> a
    assert False x = error "assertion failed!"
    assert _     x = x

which works, but gives you back a less than useful error message -- an
assertion failed, but which and where?

One way out is to define an extended ``assert`` function which also
takes a descriptive string to include in the error message and perhaps
combine this with the use of a pre-processor which inserts the source
location where ``assert`` was used.

GHC offers a helping hand here, doing all of this for you. For every use
of ``assert`` in the user's source: ::

    kelvinToC :: Double -> Double
    kelvinToC k = assert (k >= 0.0) (k-273.15)

GHC will rewrite this to also include the source location where the
assertion was made, ::

    assert pred val ==> assertError "Main.hs|15" pred val

The rewrite is only performed by the compiler when it spots applications
of :base-ref:`Control.Exception.assert`, so you can still define and use your
own versions of ``assert``, should you so wish. If not, import
``Control.Exception`` to make use ``assert`` in your code.

.. index::
   pair: assertions; disabling

GHC ignores assertions when optimisation is turned on with the
:ghc-flag:`-O` flag. That is, expressions of the form ``assert pred e``
will be rewritten to ``e``. You can also disable assertions using the
:ghc-flag:`-fignore-asserts` option. The option
:ghc-flag:`-fno-ignore-asserts <-fignore-asserts>`
allows enabling assertions even when optimisation is turned on.

Assertion failures can be caught, see the documentation for the
:base-ref:`Control.Exception.` library for the details.


