.. _string-interpolation:

String interpolation
--------------------

.. extension:: StringInterpolation
    :shortdesc: Enable string interpolation.

    :since: 10.2

    Enable string interpolation.

The ``-XStringInterpolation`` extension allows writing interpolated strings of the form ``s"My name is ${name}"``. Values interpolated with ``${...}`` must have an ``Interpolate`` instance, found in ``Data.String.Experimental`` from ``ghc-experimental``.

By default, this will only ever produce ``String``. When :extension:`OverloadedStrings` is enabled, a ``fromString`` call is called on the result of the entire expression. However, if applicable, :extension:`QualifiedStrings` is recommended over :extension:`OverloadedStrings`, which allows more precise overloading.

:extension:`QualifiedStrings` has full control over the interpolation, allowing more exotic interpolations. A module ``M`` need only implement the following functions in order for ``M.s"..."`` to work:

* ``interpolateRaw``
* ``interpolateValue``
* ``interpolateAppend``
* ``interpolateEmpty``
* ``interpolateFinalize``

For example, ``Data.String.Interpolate.ShowS.Experimental`` from ``ghc-experimental`` allows using string interpolation to implement ``shows``:

::

    import Data.String.Interpolate.ShowS.Experimental qualified as ShowS

    instance Show a => Show (MyTree a) where
      showsPrec d (MyTree l v r) =
        showParen (d > 10) $
          ShowS.s"MyTree ${ShowS.P 11 l} ${v} ${ShowS.P 11 r  }"

This extension also works with :extension:`MultilineStrings` as one would expect. The multiline string is desugared to a single line string, then the string interpolation takes effect.

Laws
~~~~

If any of the following expressions typecheck, they should hold:

* ``Data.String.fromString "str" == s"str"``
* ``M."str" == M.s"str"``

Stability
~~~~~~~~~

It's possible the API will change before the interface is exported from ``base``. While the general interface should be mostly solidified, there is no guarantee that the API will be stable upon moving to ``base``.
