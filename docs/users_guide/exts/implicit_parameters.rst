.. _implicit-parameters:

Implicit parameters
===================

.. extension:: ImplicitParams
    :shortdesc: Enable Implicit Parameters.

    :since: 6.8.1

    Allow definition of functions expecting implicit parameters.

Implicit parameters are implemented as described in [Lewis2000]_ and enabled
with the option :extension:`ImplicitParams`. (Most of the following, still rather
incomplete, documentation is due to Jeff Lewis.)

.. [Lewis2000]
    "Implicit parameters: dynamic scoping with static types",
    J Lewis, MB Shields, E Meijer, J Launchbury,
    *27th ACM Symposium on Principles of Programming Languages (POPL'00)*,
    Boston, Jan 2000.

A variable is called *dynamically bound* when it is bound by the calling
context of a function and *statically bound* when bound by the callee's
context. In Haskell, all variables are statically bound. Dynamic binding
of variables is a notion that goes back to Lisp, but was later discarded
in more modern incarnations, such as Scheme. Dynamic binding can be very
confusing in an untyped language, and unfortunately, typed languages, in
particular Hindley-Milner typed languages like Haskell, only support
static scoping of variables.

However, by a simple extension to the type class system of Haskell, we
can support dynamic binding. Basically, we express the use of a
dynamically bound variable as a constraint on the type. These
constraints lead to types of the form ``(?x::t') => t``, which says
"this function uses a dynamically-bound variable ``?x`` of type ``t'``".
For example, the following expresses the type of a sort function,
implicitly parameterised by a comparison function named ``cmp``. ::

      sort :: (?cmp :: a -> a -> Bool) => [a] -> [a]

The dynamic binding constraints are just a new form of predicate in the
type class system.

An implicit parameter occurs in an expression using the special form
``?x``, where ``x`` is any valid identifier (e.g. ``ord ?x`` is a valid
expression). Use of this construct also introduces a new dynamic-binding
constraint in the type of the expression. For example, the following
definition shows how we can define an implicitly parameterised sort
function in terms of an explicitly parameterised ``sortBy`` function: ::

      sortBy :: (a -> a -> Bool) -> [a] -> [a]

      sort   :: (?cmp :: a -> a -> Bool) => [a] -> [a]
      sort    = sortBy ?cmp

Implicit-parameter type constraints
-----------------------------------

Dynamic binding constraints behave just like other type class
constraints in that they are automatically propagated. Thus, when a
function is used, its implicit parameters are inherited by the function
that called it. For example, our ``sort`` function might be used to pick
out the least value in a list: ::

      least   :: (?cmp :: a -> a -> Bool) => [a] -> a
      least xs = head (sort xs)

Without lifting a finger, the ``?cmp`` parameter is propagated to become
a parameter of ``least`` as well. With explicit parameters, the default
is that parameters must always be explicit propagated. With implicit
parameters, the default is to always propagate them.

An implicit-parameter type constraint differs from other type class
constraints in the following way: All uses of a particular implicit
parameter must have the same type. This means that the type of
``(?x, ?x)`` is ``(?x::a) => (a,a)``, and not
``(?x::a, ?x::b) => (a, b)``, as would be the case for type class
constraints.

You can't have an implicit parameter in the context of a class or
instance declaration. For example, both these declarations are illegal: ::

      class (?x::Int) => C a where ...
      instance (?x::a) => Foo [a] where ...

Reason: exactly which implicit parameter you pick up depends on exactly
where you invoke a function. But the "invocation" of instance
declarations is done behind the scenes by the compiler, so it's hard to
figure out exactly where it is done. Easiest thing is to outlaw the
offending types.

Implicit-parameter constraints do not cause ambiguity. For example,
consider: ::

       f :: (?x :: [a]) => Int -> Int
       f n = n + length ?x

       g :: (Read a, Show a) => String -> String
       g s = show (read s)

Here, ``g`` has an ambiguous type, and is rejected, but ``f`` is fine.
The binding for ``?x`` at ``f``\ 's call site is quite unambiguous, and
fixes the type ``a``.

Implicit-parameter bindings
---------------------------

An implicit parameter is *bound* using the standard ``let`` or ``where``
binding forms. For example, we define the ``min`` function by binding
``cmp``. ::

      min :: Ord a => [a] -> a
      min  = let ?cmp = (<=) in least

A group of implicit-parameter bindings may occur anywhere a normal group
of Haskell bindings can occur, except at top level. That is, they can
occur in a ``let`` (including in a list comprehension, or do-notation,
or pattern guards), or a ``where`` clause. Note the following points:

-  An implicit-parameter binding group must be a collection of simple
   bindings to implicit-style variables (no function-style bindings, and
   no type signatures); these bindings are neither polymorphic or
   recursive.

-  You may not mix implicit-parameter bindings with ordinary bindings in
   a single ``let`` expression; use two nested ``let``\ s instead. (In
   the case of ``where`` you are stuck, since you can't nest ``where``
   clauses.)

-  You may put multiple implicit-parameter bindings in a single binding
   group; but they are *not* treated as a mutually recursive group (as
   ordinary ``let`` bindings are). Instead they are treated as a
   non-recursive group, simultaneously binding all the implicit
   parameter. The bindings are not nested, and may be re-ordered without
   changing the meaning of the program. For example, consider: ::

         f t = let { ?x = t; ?y = ?x+(1::Int) } in ?x + ?y

   The use of ``?x`` in the binding for ``?y`` does not "see" the
   binding for ``?x``, so the type of ``f`` is ::

         f :: (?x::Int) => Int -> Int

Implicit parameters and polymorphic recursion
---------------------------------------------

Consider these two definitions: ::

      len1 :: [a] -> Int
      len1 xs = let ?acc = 0 in len_acc1 xs

      len_acc1 [] = ?acc
      len_acc1 (x:xs) = let ?acc = ?acc + (1::Int) in len_acc1 xs

      ------------

      len2 :: [a] -> Int
      len2 xs = let ?acc = 0 in len_acc2 xs

      len_acc2 :: (?acc :: Int) => [a] -> Int
      len_acc2 [] = ?acc
      len_acc2 (x:xs) = let ?acc = ?acc + (1::Int) in len_acc2 xs

The only difference between the two groups is that in the second group
``len_acc`` is given a type signature. In the former case, ``len_acc1``
is monomorphic in its own right-hand side, so the implicit parameter
``?acc`` is not passed to the recursive call. In the latter case,
because ``len_acc2`` has a type signature, the recursive call is made to
the *polymorphic* version, which takes ``?acc`` as an implicit
parameter. So we get the following results in GHCi:

.. code-block:: none

      Prog> len1 "hello"
      0
      Prog> len2 "hello"
      5

Adding a type signature dramatically changes the result! This is a
rather counter-intuitive phenomenon, worth watching out for.

Implicit parameters and monomorphism
------------------------------------

GHC applies the dreaded Monomorphism Restriction (section 4.5.5 of the
Haskell Report) to implicit parameters. For example, consider: ::

     f :: Int -> Int
     f v = let ?x = 0     in
           let y = ?x + v in
           let ?x = 5     in
           y

Since the binding for ``y`` falls under the Monomorphism Restriction it
is not generalised, so the type of ``y`` is simply ``Int``, not
``(?x::Int) => Int``. Hence, ``(f 9)`` returns result ``9``. If you add
a type signature for ``y``, then ``y`` will get type
``(?x::Int) => Int``, so the occurrence of ``y`` in the body of the
``let`` will see the inner binding of ``?x``, so ``(f 9)`` will return
``14``.


