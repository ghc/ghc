.. _hascallstack:

HasCallStack
============

``GHC.Stack.HasCallStack`` is a lightweight method of obtaining a
partial call-stack at any point in the program.

A function can request its call-site with the ``HasCallStack`` constraint
and access it as a Haskell value by using ``callStack``.

One can then use functions from ``GHC.Stack`` to inspect or pretty
print (as is done in ``f`` below) the call stack. ::

   f :: HasCallStack => IO ()
   f = putStrLn (prettyCallStack callStack)

   g :: HasCallStack => IO ()
   g = f

Evaluating ``f`` directly shows a call stack with a single entry,
while evaluating ``g``, which also requests its call-site, shows
two entries, one for each computation "annotated" with
``HasCallStack``.

.. code-block:: none

   ghci> f
   CallStack (from HasCallStack):
     f, called at <interactive>:19:1 in interactive:Ghci1
   ghci> g
   CallStack (from HasCallStack):
     f, called at <interactive>:17:5 in main:Main
     g, called at <interactive>:20:1 in interactive:Ghci2

The ``error`` function from the Prelude supports printing the call stack that
led to the error in addition to the usual error message:

.. code-block:: none

   ghci> error "bad"
   *** Exception: bad
   CallStack (from HasCallStack):
     error, called at <interactive>:25:1 in interactive:Ghci5

The call stack here consists of a single entry, pinpointing the source
of the call to ``error``. However, by annotating several computations
with ``HasCallStack``, figuring out the exact circumstances and sequences
of calls that lead to a call to ``error`` becomes a lot easier, as demonstrated
with the simple example below. ::

   f :: HasCallStack => IO ()
   f = error "bad bad bad"

   g :: HasCallStack => IO ()
   g = f

   h :: HasCallStack => IO ()
   h = g

.. code-block:: none

   ghci> h
   *** Exception: bad bad bad
   CallStack (from HasCallStack):
     error, called at call-stack.hs:4:5 in main:Main
     f, called at call-stack.hs:7:5 in main:Main
     g, called at call-stack.hs:10:5 in main:Main
     h, called at <interactive>:28:1 in interactive:Ghci1

The ``CallStack`` will only extend as far as the types allow it, for
example ::

   myHead :: HasCallStack => [a] -> a
   myHead []     = errorWithCallStack "empty"
   myHead (x:xs) = x

   bad :: Int
   bad = myHead []

.. code-block:: none

   ghci> bad
   *** Exception: empty
   CallStack (from HasCallStack):
     errorWithCallStack, called at Bad.hs:8:15 in main:Bad
     myHead, called at Bad.hs:12:7 in main:Bad

includes the call-site of ``errorWithCallStack`` in ``myHead``, and of
``myHead`` in ``bad``, but not the call-site of ``bad`` at the GHCi
prompt.

GHC solves ``HasCallStack`` constraints in two steps:

1. If there is a ``CallStack`` in scope -- i.e. the enclosing definition
   has a ``HasCallStack`` constraint -- GHC will push the new call-site
   onto the existing ``CallStack``.

2. Otherwise GHC will solve the ``HasCallStack`` constraint for the
   singleton ``CallStack`` containing just the current call-site.

Importantly, GHC will **never** infer a ``HasCallStack`` constraint,
you must request it explicitly.

``CallStack`` is kept abstract, but GHC provides a function ::

   getCallStack :: CallStack -> [(String, SrcLoc)]

to access the individual call-sites in the stack. The ``String`` is the
name of the function that was called, and the ``SrcLoc`` provides the
package, module, and file name, as well as the line and column numbers.

``GHC.Stack`` additionally exports a function ``withFrozenCallStack`` that
allows users to freeze the current ``CallStack``, preventing any future push
operations from having an effect. This can be used by library authors
to prevent ``CallStack``\s from exposing unnecessary implementation
details. Consider the ``myHead`` example above, the ``errorWithCallStack`` line in
the printed stack is not particularly enlightening, so we might choose
to suppress it by freezing the ``CallStack`` that we pass to ``errorWithCallStack``. ::

   myHead :: HasCallStack => [a] -> a
   myHead []     = withFrozenCallStack (errorWithCallStack "empty")
   myHead (x:xs) = x

.. code-block:: none

   ghci> myHead []
   *** Exception: empty
   CallStack (from HasCallStack):
     myHead, called at Bad.hs:12:7 in main:Bad

**NOTE**: The intrepid user may notice that ``HasCallStack`` is just an
alias for an implicit parameter ``?callStack :: CallStack``. This is an
implementation detail and **should not** be considered part of the
``CallStack`` API, we may decide to change the implementation in the
future.

Compared with other sources of stack traces
-------------------------------------------

``HasCallStack`` does not interact with the RTS and does not require
compilation with ``-prof``. On the other hand, as the ``CallStack`` is
built up explicitly via the ``HasCallStack`` constraints, it will
generally not contain as much information as the simulated call-stacks
maintained by the RTS.

