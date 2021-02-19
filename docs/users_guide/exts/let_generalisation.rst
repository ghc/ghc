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

-  A *binding group* is a strongly-connected component in the graph in which the nodes are let-bindings,
   and there is an edge from a let-binding B to the binding for each of the free variables in B's RHS.
   Before computing the strongly-connected components, any dependencies from variables with
   complete type signatures are removed.

-  With :extension:`MonoLocalBinds`, a binding group  is *generalised* if and only if

   - Each of its free variables (excluding the variables bound by the group itself)
     is either *imported* or *closed* (see next bullet), or

   - Any of its binders has a partial type signature (see :ref:`partial-type-signatures`).
     Adding a partial type signature ``f :: _``, (or, more generally, ``f :: _ => _``)
     provides a per-binding way to ask GHC to
     perform let-generalisation, even though :extension:`MonoLocalBinds` is on.

   NB: even if the binding is generalised, it may still be affected by the
   monomorphism restriction, which reduces generalisation
   (`Haskell Report, Section 4.5.5 <http://www.haskell.org/onlinereport/decls.html#sect4.5.5>`__)

-  A variable is *closed* if and only if

   -  the variable is let-bound, and

   -  one of the following holds:

      -  the variable has an explicit, complete (i.e. not partial) type signature
         that has no free type variables, or

      -  its binding group is fully generalised, so it has a closed type

      Note that a signature like ``f :: a -> a`` is equivalent to ``f :: forall a. a->a``,
      assuming ``a`` is not in scope, and hence is closed.

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


