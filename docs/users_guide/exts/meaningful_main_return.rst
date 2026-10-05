.. _meaningful-main-return:

Meaningful main return values
-----------------------------

.. extension:: MeaningfulMainReturn
    :shortdesc: Restrict the result type of ``main`` and allow ``ExitCode``
                results to determine the program's exit code.

    :since: 10.2

    Constrain the result type of ``main`` to types which unify with either
    ``()``, ``Void``, or ``ExitCode``. For example, the following program is
    rejected: ::

      main :: IO Int
      main = pure 42

    Additionally, if it can be determined that ``main`` returns an ``ExitCode``,
    the value returned will be the program's exit code: ::

      {-# LANGUAGE MeaningfulMainReturn #-}
      import System.Exit

      main :: IO ExitCode
      main = pure (ExitFailure 42)

    The above behaves as if the user had written: ::

      {-# LANGUAGE NoMeaningfulMainReturn #-}
      import System.Exit

      main :: IO ()
      main = pure (ExitFailure 42) >>= exitWith

    If the result type of ``main`` can be unified with ``ExitCode`` but also
    ``()`` and/or ``Void``, no implicit ``exitWith`` is created. That is, the
    following program: ::

      main :: IO a
      main = putStrLn "foo" *> pure (error "never reached")

    will not try to evaluate the error to obtain an ``ExitCode`` value, and
    behave exactly as the same code would with this extension disabled.

    This extension is intended to become the default in the next GHC language
    edition.
