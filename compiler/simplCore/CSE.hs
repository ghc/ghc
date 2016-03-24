{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section{Common subexpression}
-}

{-# LANGUAGE CPP #-}

module CSE (cseProgram) where

#include "HsVersions.h"

import CoreSubst
import Var              ( Var )
import Id               ( Id, idType, idInlineActivation, zapIdOccInfo, zapIdUsageInfo )
import CoreUtils        ( mkAltExpr
                        , exprIsTrivial
                        , stripTicksE, stripTicksT, stripTicksTopE, mkTick, mkTicks )
import Type             ( tyConAppArgs )
import CoreSyn
import Outputable
import BasicTypes       ( isAlwaysActive )
import TrieMap

import Data.List

{-
                        Simple common sub-expression
                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we see
        x1 = C a b
        x2 = C x1 b
we build up a reverse mapping:   C a b  -> x1
                                 C x1 b -> x2
and apply that to the rest of the program.

When we then see
        y1 = C a b
        y2 = C y1 b
we replace the C a b with x1.  But then we *dont* want to
add   x1 -> y1  to the mapping.  Rather, we want the reverse, y1 -> x1
so that a subsequent binding
        y2 = C y1 b
will get transformed to C x1 b, and then to x2.

So we carry an extra var->var substitution which we apply *before* looking up in the
reverse mapping.


Note [Shadowing]
~~~~~~~~~~~~~~~~
We have to be careful about shadowing.
For example, consider
        f = \x -> let y = x+x in
                      h = \x -> x+x
                  in ...

Here we must *not* do CSE on the inner x+x!  The simplifier used to guarantee no
shadowing, but it doesn't any more (it proved too hard), so we clone as we go.
We can simply add clones to the substitution already described.

Note [Case binders 1]
~~~~~~~~~~~~~~~~~~~~~~
Consider

        f = \x -> case x of wild {
                        (a:as) -> case a of wild1 {
                                    (p,q) -> ...(wild1:as)...

Here, (wild1:as) is morally the same as (a:as) and hence equal to wild.
But that's not quite obvious.  In general we want to keep it as (wild1:as),
but for CSE purpose that's a bad idea.

So we add the binding (wild1 -> a) to the extra var->var mapping.
Notice this is exactly backwards to what the simplifier does, which is
to try to replaces uses of 'a' with uses of 'wild1'

Note [Case binders 2]
~~~~~~~~~~~~~~~~~~~~~~
Consider
        case (h x) of y -> ...(h x)...

We'd like to replace (h x) in the alternative, by y.  But because of
the preceding [Note: case binders 1], we only want to add the mapping
        scrutinee -> case binder
to the reverse CSE mapping if the scrutinee is a non-trivial expression.
(If the scrutinee is a simple variable we want to add the mapping
        case binder -> scrutinee
to the substitution

Note [CSE for INLINE and NOINLINE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are some subtle interactions of CSE with functions that the user
has marked as INLINE or NOINLINE. (Examples from Roman Leshchinskiy.)
Consider

        yes :: Int  {-# NOINLINE yes #-}
        yes = undefined

        no :: Int   {-# NOINLINE no #-}
        no = undefined

        foo :: Int -> Int -> Int  {-# NOINLINE foo #-}
        foo m n = n

        {-# RULES "foo/no" foo no = id #-}

        bar :: Int -> Int
        bar = foo yes

We do not expect the rule to fire.  But if we do CSE, then we risk
getting yes=no, and the rule does fire.  Actually, it won't because
NOINLINE means that 'yes' will never be inlined, not even if we have
yes=no.  So that's fine (now; perhaps in the olden days, yes=no would
have substituted even if 'yes' was NOINLINE).

But we do need to take care.  Consider

        {-# NOINLINE bar #-}
        bar = <rhs>     -- Same rhs as foo

        foo = <rhs>

If CSE produces
        foo = bar
then foo will never be inlined to <rhs> (when it should be, if <rhs>
is small).  The conclusion here is this:

   We should not add
       <rhs> :-> bar
  to the CSEnv if 'bar' has any constraints on when it can inline;
  that is, if its 'activation' not always active.  Otherwise we
  might replace <rhs> by 'bar', and then later be unable to see that it
  really was <rhs>.

Note that we do not (currently) do CSE on the unfolding stored inside
an Id, even if is a 'stable' unfolding.  That means that when an
unfolding happens, it is always faithful to what the stable unfolding
originally was.


Note [CSE for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  case f x of y { pat -> ...let y = f x in ... }
Then we can CSE the inner (f x) to y.  In fact 'case' is like a strict
let-binding, and we can use cseRhs for dealing with the scrutinee.

************************************************************************
*                                                                      *
\section{Common subexpression}
*                                                                      *
************************************************************************
-}

cseProgram :: CoreProgram -> CoreProgram
cseProgram binds = snd (mapAccumL cseBind emptyCSEnv binds)

cseBind :: CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind env (NonRec b e)
  = (env2, NonRec b'' e')
  where
    (env1, b') = addBinder env b
    (env2, (b'', e')) = cseRhs env1 (b',e)

cseBind env (Rec pairs)
  = (env2, Rec pairs')
  where
    (bs,es) = unzip pairs
    (env1, bs') = addRecBinders env bs
    (env2, pairs') = mapAccumL cseRhs env1 (bs' `zip` es)

cseRhs :: CSEnv -> (OutBndr, InExpr) -> (CSEnv, (OutBndr, OutExpr))
cseRhs env (id',rhs)
  = case lookupCSEnv env rhs'' of
        Nothing
          | always_active -> (extendCSEnv env rhs' id', (zapped_id, rhs'))
          | otherwise     -> (env,                      (id', rhs'))
        Just id
          | always_active -> (extendCSSubst env id' id_expr, (id', mkTicks ticks id_expr))
          | otherwise     -> (env,                           (id', mkTicks ticks id_expr))
          where
            id_expr = varToCoreExpr id  -- Could be a CoVar
          -- In the Just case, we have
          --        x = rhs
          --        ...
          --        x' = rhs
          -- We are replacing the second binding with x'=x
          -- and so must record that in the substitution so
          -- that subsequent uses of x' are replaced with x,
          -- See Trac #5996
  where
    zapped_id = zapIdUsageInfo id'
       -- Putting the Id into the environment makes it possible that
       -- it'll become shared more than it is now, which would
       -- invalidate (the usage part of) its demand info.  This caused
       -- Trac #100218.
       -- Easiest thing is to zap the usage info; subsequently
       -- performing late demand-analysis will restore it.  Don't zap
       -- the strictness info; it's not necessary to do so, and losing
       -- it is bad for performance if you don't do late demand
       -- analysis

    rhs' = cseExpr env rhs

    ticks = stripTicksT tickishFloatable rhs'
    rhs'' = stripTicksE tickishFloatable rhs'
    -- We don't want to lose the source notes when a common sub
    -- expression gets eliminated. Hence we push all (!) of them on
    -- top of the replaced sub-expression. This is probably not too
    -- useful in practice, but upholds our semantics.

    always_active = isAlwaysActive (idInlineActivation id')
         -- See Note [CSE for INLINE and NOINLINE]

tryForCSE :: CSEnv -> InExpr -> OutExpr
tryForCSE env expr
  | exprIsTrivial expr'                    = expr'       -- No point
  | Just smaller <- lookupCSEnv env expr'' = foldr mkTick (Var smaller) ticks
  | otherwise                              = expr'
  where
    expr' = cseExpr env expr
    expr'' = stripTicksE tickishFloatable expr'
    ticks = stripTicksT tickishFloatable expr'

cseExpr :: CSEnv -> InExpr -> OutExpr
cseExpr env (Type t)               = Type (substTy (csEnvSubst env) t)
cseExpr env (Coercion c)           = Coercion (substCo (csEnvSubst env) c)
cseExpr _   (Lit lit)              = Lit lit
cseExpr env (Var v)                = lookupSubst env v
cseExpr env (App f a)              = App (cseExpr env f) (tryForCSE env a)
cseExpr env (Tick t e)             = Tick t (cseExpr env e)
cseExpr env (Cast e co)            = Cast (cseExpr env e) (substCo (csEnvSubst env) co)
cseExpr env (Lam b e)              = let (env', b') = addBinder env b
                                     in Lam b' (cseExpr env' e)
cseExpr env (Let bind e)           = let (env', bind') = cseBind env bind
                                     in Let bind' (cseExpr env' e)
cseExpr env (Case scrut bndr ty alts) = Case scrut' bndr''' ty alts'
                          where
                                alts' = cseAlts env2 scrut' bndr bndr'' alts
                                (env1, bndr') = addBinder env bndr
                                bndr'' = zapIdOccInfo bndr'
                                -- The swizzling from Note [Case binders 2] may
                                -- cause a dead case binder to be alive, so we
                                -- play safe here and bring them all to life
                                (env2, (bndr''', scrut')) = cseRhs env1 (bndr'', scrut)
                                -- Note [CSE for case expressions]

cseAlts :: CSEnv -> OutExpr -> InBndr -> InBndr -> [InAlt] -> [OutAlt]

cseAlts env scrut' bndr bndr' alts
  = map cse_alt alts
  where
    scrut'' = stripTicksTopE tickishFloatable scrut'
    (con_target, alt_env)
        = case scrut'' of
            Var v' -> (v', extendCSSubst env bndr scrut'') -- See Note [Case binders 1]
                                                           -- map: bndr -> v'

            _      -> (bndr', extendCSEnv env scrut' bndr') -- See Note [Case binders 2]
                                                             -- map: scrut' -> bndr'

    arg_tys = tyConAppArgs (idType bndr)

    cse_alt (DataAlt con, args, rhs)
        | not (null args)
                -- Don't try CSE if there are no args; it just increases the number
                -- of live vars.  E.g.
                --      case x of { True -> ....True.... }
                -- Don't replace True by x!
                -- Hence the 'null args', which also deal with literals and DEFAULT
        = (DataAlt con, args', tryForCSE new_env rhs)
        where
          (env', args') = addBinders alt_env args
          new_env       = extendCSEnv env' con_expr con_target
          con_expr      = mkAltExpr (DataAlt con) args' arg_tys

    cse_alt (con, args, rhs)
        = (con, args', tryForCSE env' rhs)
        where
          (env', args') = addBinders alt_env args

{-
************************************************************************
*                                                                      *
\section{The CSE envt}
*                                                                      *
************************************************************************
-}

type InExpr  = CoreExpr         -- Pre-cloning
type InBndr  = CoreBndr
type InAlt   = CoreAlt

type OutExpr  = CoreExpr        -- Post-cloning
type OutBndr  = CoreBndr
type OutAlt   = CoreAlt

data CSEnv  = CS { cs_map    :: CoreMap (OutExpr, Id)   -- Key, value
                 , cs_subst  :: Subst }

emptyCSEnv :: CSEnv
emptyCSEnv = CS { cs_map = emptyCoreMap, cs_subst = emptySubst }

lookupCSEnv :: CSEnv -> OutExpr -> Maybe Id
lookupCSEnv (CS { cs_map = csmap }) expr
  = case lookupCoreMap csmap expr of
      Just (_,e) -> Just e
      Nothing    -> Nothing

extendCSEnv :: CSEnv -> OutExpr -> Id -> CSEnv
extendCSEnv cse expr id
  = cse { cs_map = extendCoreMap (cs_map cse) sexpr (sexpr,id) }
  where sexpr = stripTicksE tickishFloatable expr

csEnvSubst :: CSEnv -> Subst
csEnvSubst = cs_subst

lookupSubst :: CSEnv -> Id -> OutExpr
lookupSubst (CS { cs_subst = sub}) x = lookupIdSubst (text "CSE.lookupSubst") sub x

extendCSSubst :: CSEnv -> Id  -> CoreExpr -> CSEnv
extendCSSubst cse x rhs = cse { cs_subst = extendSubst (cs_subst cse) x rhs }

addBinder :: CSEnv -> Var -> (CSEnv, Var)
addBinder cse v = (cse { cs_subst = sub' }, v')
                where
                  (sub', v') = substBndr (cs_subst cse) v

addBinders :: CSEnv -> [Var] -> (CSEnv, [Var])
addBinders cse vs = (cse { cs_subst = sub' }, vs')
                where
                  (sub', vs') = substBndrs (cs_subst cse) vs

addRecBinders :: CSEnv -> [Id] -> (CSEnv, [Id])
addRecBinders cse vs = (cse { cs_subst = sub' }, vs')
                where
                  (sub', vs') = substRecBndrs (cs_subst cse) vs
