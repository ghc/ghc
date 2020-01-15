.. _arrow-notation:

Arrow notation
==============

.. extension:: Arrows
    :shortdesc: Enable arrow notation extension

    :since: 6.8.1

    Enable arrow notation.

Arrows are a generalisation of monads introduced by John Hughes. For
more details, see

-  “Generalising Monads to Arrows”, John Hughes, in Science of Computer
   Programming 37, pp. 67–111, May 2000. The paper that introduced arrows:
   a friendly introduction, motivated with programming examples.

-  “\ `A New Notation for
   Arrows <http://www.soi.city.ac.uk/~ross/papers/notation.html>`__\ ”,
   Ross Paterson, in ICFP, Sep 2001. Introduced the notation described
   here.

-  “\ `Arrows and
   Computation <http://www.soi.city.ac.uk/~ross/papers/fop.html>`__\ ”,
   Ross Paterson, in The Fun of Programming, Palgrave, 2003.

-  “\ `Programming with
   Arrows <http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf>`__\ ”, John
   Hughes, in 5th International Summer School on Advanced Functional
   Programming, Lecture Notes in Computer Science vol. 3622, Springer,
   2004. This paper includes another introduction to the notation, with
   practical examples.

-  “\ `Type and Translation Rules for Arrow Notation in
   GHC <http://www.haskell.org/ghc/docs/papers/arrow-rules.pdf>`__\ ”,
   Ross Paterson and Simon Peyton Jones, September 16, 2004. A terse
   enumeration of the formal rules used (extracted from comments in the
   source code).

-  The arrows web page at
   ``http://www.haskell.org/arrows/`` <http://www.haskell.org/arrows/>`__.

With the :extension:`Arrows` extension, GHC supports the arrow notation described in
the second of these papers, translating it using combinators from the
:base-ref:`Control.Arrow.` module.
What follows is a brief introduction to the notation; it won't make much
sense unless you've read Hughes's paper.

The extension adds a new kind of expression for defining arrows:

.. code-block:: none

    exp10 ::= ...
           |  proc apat -> cmd

where ``proc`` is a new keyword. The variables of the pattern are bound
in the body of the ``proc``-expression, which is a new sort of thing
called a command. The syntax of commands is as follows:

.. code-block:: none

    cmd   ::= exp10 -<  exp
           |  exp10 -<< exp
           |  cmd0

with ⟨cmd⟩\ :sup:`0` up to ⟨cmd⟩\ :sup:`9` defined using infix operators
as for expressions, and

.. code-block:: none

    cmd10 ::= \ apat ... apat -> cmd
           |  let decls in cmd
           |  if exp then cmd else cmd
           |  case exp of { calts }
           |  do { cstmt ; ... cstmt ; cmd }
           |  fcmd

    fcmd  ::= fcmd aexp
           |  ( cmd )
           |  (| aexp cmd ... cmd |)

    cstmt ::= let decls
           |  pat <- cmd
           |  rec { cstmt ; ... cstmt [;] }
           |  cmd

where ⟨calts⟩ are like ⟨alts⟩ except that the bodies are commands
instead of expressions.

Commands produce values, but (like monadic computations) may yield more
than one value, or none, and may do other things as well. For the most
part, familiarity with monadic notation is a good guide to using
commands. However the values of expressions, even monadic ones, are
determined by the values of the variables they contain; this is not
necessarily the case for commands.

A simple example of the new notation is the expression ::

    proc x -> f -< x+1

We call this a procedure or arrow abstraction. As with a lambda
expression, the variable ``x`` is a new variable bound within the
``proc``-expression. It refers to the input to the arrow. In the above
example, ``-<`` is not an identifier but a new reserved symbol used for
building commands from an expression of arrow type and an expression to
be fed as input to that arrow. (The weird look will make more sense
later.) It may be read as analogue of application for arrows. The above
example is equivalent to the Haskell expression ::

    arr (\ x -> x+1) >>> f

That would make no sense if the expression to the left of ``-<``
involves the bound variable ``x``. More generally, the expression to the
left of ``-<`` may not involve any local variable, i.e. a variable bound
in the current arrow abstraction. For such a situation there is a
variant ``-<<``, as in ::

    proc x -> f x -<< x+1

which is equivalent to ::

    arr (\ x -> (f x, x+1)) >>> app

so in this case the arrow must belong to the ``ArrowApply`` class. Such
an arrow is equivalent to a monad, so if you're using this form you may
find a monadic formulation more convenient.

do-notation for commands
------------------------

Another form of command is a form of ``do``-notation. For example, you
can write ::

    proc x -> do
            y <- f -< x+1
            g -< 2*y
            let z = x+y
            t <- h -< x*z
            returnA -< t+z

You can read this much like ordinary ``do``-notation, but with commands
in place of monadic expressions. The first line sends the value of
``x+1`` as an input to the arrow ``f``, and matches its output against
``y``. In the next line, the output is discarded. The arrow ``returnA``
is defined in the :base-ref:`Control.Arrow.` module as ``arr id``. The above
example is treated as an abbreviation for ::

    arr (\ x -> (x, x)) >>>
            first (arr (\ x -> x+1) >>> f) >>>
            arr (\ (y, x) -> (y, (x, y))) >>>
            first (arr (\ y -> 2*y) >>> g) >>>
            arr snd >>>
            arr (\ (x, y) -> let z = x+y in ((x, z), z)) >>>
            first (arr (\ (x, z) -> x*z) >>> h) >>>
            arr (\ (t, z) -> t+z) >>>
            returnA

Note that variables not used later in the composition are projected out.
After simplification using rewrite rules (see :ref:`rewrite-rules`)
defined in the :base-ref:`Control.Arrow.` module, this reduces to ::

    arr (\ x -> (x+1, x)) >>>
            first f >>>
            arr (\ (y, x) -> (2*y, (x, y))) >>>
            first g >>>
            arr (\ (_, (x, y)) -> let z = x+y in (x*z, z)) >>>
            first h >>>
            arr (\ (t, z) -> t+z)

which is what you might have written by hand. With arrow notation, GHC
keeps track of all those tuples of variables for you.

Note that although the above translation suggests that ``let``-bound
variables like ``z`` must be monomorphic, the actual translation
produces Core, so polymorphic variables are allowed.

It's also possible to have mutually recursive bindings, using the new
``rec`` keyword, as in the following example: ::

    counter :: ArrowCircuit a => a Bool Int
    counter = proc reset -> do
            rec     output <- returnA -< if reset then 0 else next
                    next <- delay 0 -< output+1
            returnA -< output

The translation of such forms uses the ``loop`` combinator, so the arrow
concerned must belong to the ``ArrowLoop`` class.

Conditional commands
--------------------

In the previous example, we used a conditional expression to construct
the input for an arrow. Sometimes we want to conditionally execute
different commands, as in ::

    proc (x,y) ->
            if f x y
            then g -< x+1
            else h -< y+2

which is translated to ::

    arr (\ (x,y) -> if f x y then Left x else Right y) >>>
            (arr (\x -> x+1) >>> g) ||| (arr (\y -> y+2) >>> h)

Since the translation uses ``|||``, the arrow concerned must belong to
the ``ArrowChoice`` class.

There are also ``case`` commands, like ::

    case input of
        [] -> f -< ()
        [x] -> g -< x+1
        x1:x2:xs -> do
            y <- h -< (x1, x2)
            ys <- k -< xs
            returnA -< y:ys

The syntax is the same as for ``case`` expressions, except that the
bodies of the alternatives are commands rather than expressions. The
translation is similar to that of ``if`` commands.

Defining your own control structures
------------------------------------

As we're seen, arrow notation provides constructs, modelled on those for
expressions, for sequencing, value recursion and conditionals. But
suitable combinators, which you can define in ordinary Haskell, may also
be used to build new commands out of existing ones. The basic idea is
that a command defines an arrow from environments to values. These
environments assign values to the free local variables of the command.
Thus combinators that produce arrows from arrows may also be used to
build commands from commands. For example, the ``ArrowPlus`` class
includes a combinator ::

    ArrowPlus a => (<+>) :: a b c -> a b c -> a b c

so we can use it to build commands: ::

    expr' = proc x -> do
                    returnA -< x
            <+> do
                    symbol Plus -< ()
                    y <- term -< ()
                    expr' -< x + y
            <+> do
                    symbol Minus -< ()
                    y <- term -< ()
                    expr' -< x - y

(The ``do`` on the first line is needed to prevent the first ``<+> ...``
from being interpreted as part of the expression on the previous line.)
This is equivalent to ::

    expr' = (proc x -> returnA -< x)
            <+> (proc x -> do
                    symbol Plus -< ()
                    y <- term -< ()
                    expr' -< x + y)
            <+> (proc x -> do
                    symbol Minus -< ()
                    y <- term -< ()
                    expr' -< x - y)

We are actually using ``<+>`` here with the more specific type ::

    ArrowPlus a => (<+>) :: a (e,()) c -> a (e,()) c -> a (e,()) c

It is essential that this operator be polymorphic in ``e`` (representing
the environment input to the command and thence to its subcommands) and
satisfy the corresponding naturality property ::

    arr (first k) >>> (f <+> g) = (arr (first k) >>> f) <+> (arr (first k) >>> g)

at least for strict ``k``. (This should be automatic if you're not using
``seq``.) This ensures that environments seen by the subcommands are
environments of the whole command, and also allows the translation to
safely trim these environments. (The second component of the input pairs
can contain unnamed input values, as described in the next section.) The
operator must also not use any variable defined within the current arrow
abstraction.

We could define our own operator ::

    untilA :: ArrowChoice a => a (e,s) () -> a (e,s) Bool -> a (e,s) ()
    untilA body cond = proc x -> do
            b <- cond -< x
            if b then returnA -< ()
            else do
                    body -< x
                    untilA body cond -< x

and use it in the same way. Of course this infix syntax only makes sense
for binary operators; there is also a more general syntax involving
special brackets: ::

    proc x -> do
            y <- f -< x+1
            (|untilA (increment -< x+y) (within 0.5 -< x)|)

Primitive constructs
--------------------

Some operators will need to pass additional inputs to their subcommands.
For example, in an arrow type supporting exceptions, the operator that
attaches an exception handler will wish to pass the exception that
occurred to the handler. Such an operator might have a type ::

    handleA :: ... => a (e,s) c -> a (e,(Ex,s)) c -> a (e,s) c

where ``Ex`` is the type of exceptions handled. You could then use this
with arrow notation by writing a command ::

    body `handleA` \ ex -> handler

so that if an exception is raised in the command ``body``, the variable
``ex`` is bound to the value of the exception and the command
``handler``, which typically refers to ``ex``, is entered. Though the
syntax here looks like a functional lambda, we are talking about
commands, and something different is going on. The input to the arrow
represented by a command consists of values for the free local variables
in the command, plus a stack of anonymous values. In all the prior
examples, we made no assumptions about this stack. In the second
argument to ``handleA``, the value of the exception has been added to
the stack input to the handler. The command form of lambda merely gives
this value a name.

More concretely, the input to a command consists of a pair of an
environment and a stack. Each value on the stack is paired with the
remainder of the stack, with an empty stack being ``()``. So operators
like ``handleA`` that pass extra inputs to their subcommands can be
designed for use with the notation by placing the values on the stack
paired with the environment in this way. More precisely, the type of
each argument of the operator (and its result) should have the form ::

    a (e, (t1, ... (tn, ())...)) t

where ⟨e⟩ is a polymorphic variable (representing the environment) and
⟨ti⟩ are the types of the values on the stack, with ⟨t1⟩ being the
"top". The polymorphic variable ⟨e⟩ must not occur in ⟨a⟩, ⟨ti⟩ or ⟨t⟩.
However the arrows involved need not be the same. Here are some more
examples of suitable operators: ::

    bracketA :: ... => a (e,s) b -> a (e,(b,s)) c -> a (e,(c,s)) d -> a (e,s) d
    runReader :: ... => a (e,s) c -> a' (e,(State,s)) c
    runState :: ... => a (e,s) c -> a' (e,(State,s)) (c,State)

We can supply the extra input required by commands built with the last
two by applying them to ordinary expressions, as in ::

    proc x -> do
            s <- ...
            (|runReader (do { ... })|) s

which adds ``s`` to the stack of inputs to the command built using
``runReader``.

The command versions of lambda abstraction and application are analogous
to the expression versions. In particular, the beta and eta rules
describe equivalences of commands. These three features (operators,
lambda abstraction and application) are the core of the notation;
everything else can be built using them, though the results would be
somewhat clumsy. For example, we could simulate ``do``\-notation by
defining ::

    bind :: Arrow a => a (e,s) b -> a (e,(b,s)) c -> a (e,s) c
    u `bind` f = returnA &&& u >>> f

    bind_ :: Arrow a => a (e,s) b -> a (e,s) c -> a (e,s) c
    u `bind_` f = u `bind` (arr fst >>> f)

We could simulate ``if`` by defining ::

    cond :: ArrowChoice a => a (e,s) b -> a (e,s) b -> a (e,(Bool,s)) b
    cond f g = arr (\ (e,(b,s)) -> if b then Left (e,s) else Right (e,s)) >>> f ||| g

Differences with the paper
--------------------------

-  Instead of a single form of arrow application (arrow tail) with two
   translations, the implementation provides two forms ``-<``
   (first-order) and ``-<<`` (higher-order).

-  User-defined operators are flagged with banana brackets instead of a
   new ``form`` keyword.

-  In the paper and the previous implementation, values on the stack
   were paired to the right of the environment in a single argument, but
   now the environment and stack are separate arguments.

Portability
-----------

Although only GHC implements arrow notation directly, there is also a
preprocessor (available from the `arrows web
page <http://www.haskell.org/arrows/>`__) that translates arrow notation
into Haskell 98 for use with other Haskell systems. You would still want
to check arrow programs with GHC; tracing type errors in the
preprocessor output is not easy. Modules intended for both GHC and the
preprocessor must observe some additional restrictions:

-  The module must import :base-ref:`Control.Arrow.`.

-  The preprocessor cannot cope with other Haskell extensions. These
   would have to go in separate modules.

-  Because the preprocessor targets Haskell (rather than Core),
   ``let``\-bound variables are monomorphic.


