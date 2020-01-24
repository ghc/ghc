.. _mono-local-binds:

Let-generalisation
------------------

.. extension:: MonoLocalBinds
    :shortdesc: Enable do not generalise local bindings.
        Implied by :extension:`TypeFamilies` and :extension:`GADTs`.

    :since: 6.12.1

    Infer less polymorphic types for local bindings by default.

An ML-style language usually generalises the type of any ``let``\-bound or
``where``\-bound variable, so that it is as polymorphic as possible. With the
extension :extension:`MonoLocalBinds` GHC implements a slightly more conservative
policy, using the following rules:

-  A variable is *closed* if and only if

   -  the variable is let-bound

   -  one of the following holds:

      -  the variable has an explicit type signature that has no free
         type variables, or

      -  its binding group is fully generalised (see next bullet)

-  A binding group is *fully generalised* if and only if

   -  each of its free variables is either imported or closed, and

   -  the binding is not affected by the monomorphism restriction
      (`Haskell Report, Section
      4.5.5 <http://www.haskell.org/onlinereport/decls.html#sect4.5.5>`__)

For example, consider ::

    f x = x + 1
    g x = let h y = f y * 2
              k z = z+x
          in  h x + k x

Here ``f`` is generalised because it has no free variables; and its
binding group is unaffected by the monomorphism restriction; and hence
``f`` is closed. The same reasoning applies to ``g``, except that it has
one closed free variable, namely ``f``. Similarly ``h`` is closed, *even
though it is not bound at top level*, because its only free variable
``f`` is closed. But ``k`` is not closed, because it mentions ``x``
which is not closed (because it is not let-bound).

Notice that a top-level binding that is affected by the monomorphism
restriction is not closed, and hence may in turn prevent generalisation
of bindings that mention it.

The rationale for this more conservative strategy is given in `the
papers <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf>`__
"Let should not be generalised" and "Modular type inference with local
assumptions", and a related `blog post
<https://www.haskell.org/ghc/blog/20100930-LetGeneralisationInGhc7.html>`__.

The extension :extension:`MonoLocalBinds` is implied by :extension:`TypeFamilies`
and :extension:`GADTs`. You can switch it off again with
:extension:`NoMonoLocalBinds <MonoLocalBinds>` but type inference becomes
less predictable if you do so. (Read the papers!)


