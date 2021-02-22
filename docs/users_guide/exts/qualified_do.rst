.. _qualified-do-notation:

Qualified do-notation
-------------------------

.. index::
   single: Qualified do-notation
   single: do-notation; Qualified

.. extension:: QualifiedDo
    :shortdesc: Enable qualified do-notation desugaring.

    :since: 9.0.1

    Allow the use of qualified ``do`` notation.

``QualifiedDo`` enables qualifying a ``do`` block with a module name, to control which operations to use for
the monadic combinators that the ``do`` notation desugars to.
When ``-XQualifiedDo`` is enabled, you can *qualify* the ``do`` notation by writing ``modid.do``, where
``modid`` is a module name in scope: ::

    {-# LANGUAGE QualifiedDo #-}
    import qualified Some.Module.Monad as M

    action :: M.SomeType a
    action = M.do x <- u
                  res
                  M.return x

The additional module name (here ``M``) is called the qualifier of the do-expression.

The unqualified ``do`` syntax is convenient for writing monadic code, but
it only works for data types that provide an instance of the ``Monad`` type class.
There are other types which are "monad-like" but can't provide an instance of
``Monad`` (e.g. indexed monads, graded monads or relative monads), yet they could
still use the ``do`` syntax if it weren't hardwired to the methods of the ``Monad``
type class. ``-XQualifiedDo`` comes to make the do syntax customizable in this
respect.
It allows you to mix and match ``do`` blocks of different types with suitable
operations to use on each case: ::

  {-# LANGUAGE QualifiedDo #-}
  import qualified Control.Monad.Linear as L

  import MAC (label, box, runMAC)
  import qualified MAC as MAC

  f :: IO ()
  f = do
    x <- runMAC $           -- (Prelude.>>=)
                            --   (runMAC $
      MAC.do                --
        d <- label "y"      --     label "y" MAC.>>= \d ->
        box $               --
                            --       (box $
          L.do              --
            r <- L.f d      --         L.f d L.>>= \r ->
            L.g r           --         L.g r L.>>
            L.return r      --         L.return r
                            --       ) MAC.>>
        MAC.return d        --       (MAC.return d)
                            --   )
    print x                 --   (\x -> print x)

The semantics of ``do`` notation statements with ``-XQualifiedDo`` is as follows:

* The ``x <- u`` statement uses ``(M.>>=)`` ::

    M.do { x <- u; stmts }  =  u M.>>= \x -> M.do { stmts }

* The ``u`` statement uses ``(M.>>)`` ::

    M.do { u; stmts }  =  u M.>> M.do { stmts }

* The a ``pat <- u`` statement uses ``M.fail`` for the failing case,
  if such a case is needed ::

    M.do { pat <- u; stmts }  =  u M.>>= \case
      { pat -> M.do { stmts }
      ; _ -> M.fail "…"
      }

  If the pattern cannot fail, then we don't need to use ``M.fail``.  ::

    M.do { pat <- u; stmts }  =  u M.>>= \case pat -> M.do { stmts }

*  The desugaring of ``-XApplicativeDo`` uses ``M.fmap``, ``(M.<*>)``,
   and ``M.join`` (after the the applicative-do grouping has been performed) ::

    M.do { (x1 <- u1 | … | xn <- un); M.return e }  =
      (\x1 … xn -> e) `M.fmap` u1 M.<*> … M.<*> un

    M.do { (x1 <- u1 | … | xn <- un); stmts }  =
      M.join ((\x1 … xn -> M.do { stmts }) `M.fmap` u1 M.<*> … M.<*> un)

  Note that ``M.join`` is only needed if the final expression is not
  identifiably a ``return``. With ``-XQualifiedDo`` enabled, ``-XApplicativeDo``
  looks only for the qualified ``return``/``pure`` in a qualified do-block.

*  With ``-XRecursiveDo``, ``rec`` and ``mdo`` blocks use ``M.mfix`` and ``M.return``: ::

     M.do { rec { x1 <- u1; … ; xn <- un }; stmts }  =
       M.do
       { (x1, …, xn) <- M.mfix (\~(x1, …, xn) -> M.do { x1 <- u1; …; xn <- un; M.return (x1, …, xn)})
       ; stmts
       }

If a name ``M.op`` is required by the desugaring process (and only if it's required!) but the name is
not in scope, it is reported as an error.

The types of the operations picked for desugaring must produce an
expression which is accepted by the typechecker. But other than that,
there are no specific requirements on the types.

If no qualifier is specified with ``-XQualifiedDo`` enabled, it defaults to the operations defined in the Prelude, or, if
``-XRebindableSyntax`` is enabled, to whatever operations are in scope.

Note that the operations to be qualified must be in scope for QualifiedDo to work. I.e. ``import MAC (label)`` in the
example above would result in an error, since ``MAC.>>=`` and ``MAC.>>`` would not be in scope.

Examples
~~~~~~~~

``-XQualifiedDo`` does not affect ``return`` in the monadic ``do`` notation.  ::

  import qualified Some.Monad.M as M

  boolM :: (a -> M.M Bool) -> b -> b -> a -> M.M b
  boolM p a b x = M.do
      px <- p x     -- M.>>=
      if px then
        return b    -- Prelude.return
      else
        M.return a  -- M.return

``-XQualifiedDo`` does not affect explicit ``(>>=)`` in the monadic ``do`` notation.  ::

  import qualified Some.Monad.M as M
  import Data.Bool (bool)

  boolMM :: (a -> M.M Bool) -> M b -> M b -> a -> M.M b
  boolMM p ma mb x = M.do
      p x >>= bool ma mb   -- Prelude.>>=

Nested ``do`` blocks do not affect each other's meanings.  ::

  import qualified Some.Monad.M as M

  f :: M.M SomeType
  f = M.do
      x <- f1                 -- M.>>=
      f2 (do y <- g1          -- Prelude.>>=
             g2 x y)
    where
      f1 = ...
      f2 m = ...
      g1 = ...
      g2 x y = ...

The type of ``(>>=)`` can also be modified, as seen here for a graded monad: ::

  {-# LANGUAGE ConstraintKinds #-}
  {-# LANGUAGE PolyKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  module Control.Monad.Graded (GradedMonad(..)) where

  import Data.Kind (Constraint)

  class GradedMonad (m :: k -> * -> *) where
    type Unit m :: k
    type Plus m (i :: k) (j :: k) :: k
    type Inv  m (i :: k) (j :: k) :: Constraint
    (>>=) :: Inv m i j => m i a -> (a -> m j b) -> m (Plus m i j) b
    return :: a -> m (Unit m) a

  -----------------

  module M where

  import Control.Monad.Graded as Graded

  g :: GradedMonad m => a -> m SomeTypeIndex b
  g a = Graded.do
    b <- someGradedFunction a Graded.>>= someOtherGradedFunction
    c <- anotherGradedFunction b
    Graded.return c
