%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section{Common subexpression}

\begin{code}
{-# LANGUAGE CPP #-}

module CSE (cseProgram) where

#include "HsVersions.h"

import CoreSubst
import Var              ( Var )
import Id               ( Id, idType, idInlineActivation, zapIdOccInfo )
import CoreUtils        ( mkAltExpr
                        , exprIsTrivial)
import Type             ( tyConAppArgs )
import CoreSyn
import Outputable
import BasicTypes       ( isAlwaysActive )
import TrieMap

import Data.List
\end{code}


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
We are careful to do no CSE inside functions that the user has marked as
INLINE or NOINLINE.  In terms of Core, that means

        a) we do not do CSE inside an InlineRule

        b) we do not do CSE on the RHS of a binding b=e
           unless b's InlinePragma is AlwaysActive

Here's why (examples from Roman Leshchinskiy).  Consider

        yes :: Int
        {-# NOINLINE yes #-}
        yes = undefined

        no :: Int
        {-# NOINLINE no #-}
        no = undefined

        foo :: Int -> Int -> Int
        {-# NOINLINE foo #-}
        foo m n = n

        {-# RULES "foo/no" foo no = id #-}

        bar :: Int -> Int
        bar = foo yes

We do not expect the rule to fire.  But if we do CSE, then we get
yes=no, and the rule does fire.  Worse, whether we get yes=no or
no=yes depends on the order of the definitions.

In general, CSE should probably never touch things with INLINE pragmas
as this could lead to surprising results.  Consider

        {-# INLINE foo #-}
        foo = <rhs>

        {-# NOINLINE bar #-}
        bar = <rhs>     -- Same rhs as foo

If CSE produces
        foo = bar
then foo will never be inlined (when it should be); but if it produces
        bar = foo
bar will be inlined (when it should not be). Even if we remove INLINE foo,
we'd still like foo to be inlined if rhs is small. This won't happen
with foo = bar.

Not CSE-ing inside INLINE also solves an annoying bug in CSE. Consider
a worker/wrapper, in which the worker has turned into a single variable:
        $wf = h
        f = \x -> ...$wf...
Now CSE may transform to
        f = \x -> ...h...
But the WorkerInfo for f still says $wf, which is now dead!  This won't
happen now that we don't look inside INLINEs (which wrappers are).

Note [CSE for case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  case f x of y { pat -> ...let y = f x in ... }
Then we can CSE the inner (f x) to y.  In fact 'case' is like a strict
let-binding, and we can use cseRhs for dealing with the scrutinee.

%************************************************************************
%*                                                                      *
\section{Common subexpression}
%*                                                                      *
%************************************************************************

\begin{code}
cseProgram :: CoreProgram -> CoreProgram
cseProgram binds = cseBinds emptyCSEnv binds

cseBinds :: CSEnv -> [CoreBind] -> [CoreBind]
cseBinds _   []     = []
cseBinds env (b:bs) = (b':bs')
                    where
                      (env1, b') = cseBind  env  b
                      bs'        = cseBinds env1 bs

cseBind :: CSEnv -> CoreBind -> (CSEnv, CoreBind)
cseBind env (NonRec b e)
  = (env2, NonRec b' e')
  where
    (env1, b') = addBinder env b
    (env2, e') = cseRhs env1 (b',e)

cseBind env (Rec pairs)
  = (env2, Rec (bs' `zip` es'))
  where
    (bs,es) = unzip pairs
    (env1, bs') = addRecBinders env bs
    (env2, es') = mapAccumL cseRhs env1 (bs' `zip` es)

cseRhs :: CSEnv -> (OutBndr, InExpr) -> (CSEnv, OutExpr)
cseRhs env (id',rhs)
  = case lookupCSEnv env rhs' of
        Nothing -> (extendCSEnv env rhs' id', rhs')
        Just id -> (extendCSSubst env id' id, Var id)
          -- In the Just case, we have
          --        x = rhs
          --        ...
          --        x' = rhs
          -- We are replacing the second binding with x'=x
          -- and so must record that in the substitution so
          -- that subsequent uses of x' are replaced with x,
          -- See Trac #5996
  where
    rhs' | isAlwaysActive (idInlineActivation id') = cseExpr env rhs
         | otherwise                               = rhs
                -- See Note [CSE for INLINE and NOINLINE]

tryForCSE :: CSEnv -> InExpr -> OutExpr
tryForCSE env expr
  | exprIsTrivial expr'                   = expr'       -- No point
  | Just smaller <- lookupCSEnv env expr' = Var smaller
  | otherwise                             = expr'
  where
    expr' = cseExpr env expr

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
cseExpr env (Case scrut bndr ty alts) = Case scrut' bndr'' ty alts'
                          where
                                alts' = cseAlts env2 scrut' bndr bndr'' alts
                                (env1, bndr') = addBinder env bndr
                                bndr'' = zapIdOccInfo bndr'
                                -- The swizzling from Note [Case binders 2] may
                                -- cause a dead case binder to be alive, so we
                                -- play safe here and bring them all to life
                                (env2, scrut') = cseRhs env1 (bndr'', scrut)
                                -- Note [CSE for case expressions]

cseAlts :: CSEnv -> OutExpr -> InBndr -> InBndr -> [InAlt] -> [OutAlt]

cseAlts env scrut' bndr bndr' alts
  = map cse_alt alts
  where
    (con_target, alt_env)
        = case scrut' of
            Var v' -> (v',     extendCSSubst env bndr v')    -- See Note [Case binders 1]
                                                             -- map: bndr -> v'

            _      ->  (bndr', extendCSEnv env scrut' bndr') -- See Note [Case binders 2]
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
          new_env       = extendCSEnv env' (mkAltExpr (DataAlt con) args' arg_tys)
                                           con_target

    cse_alt (con, args, rhs)
        = (con, args', tryForCSE env' rhs)
        where
          (env', args') = addBinders alt_env args
\end{code}


%************************************************************************
%*                                                                      *
\section{The CSE envt}
%*                                                                      *
%************************************************************************

\begin{code}
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
  = cse { cs_map = extendCoreMap (cs_map cse) expr (expr,id) }

csEnvSubst :: CSEnv -> Subst
csEnvSubst = cs_subst

lookupSubst :: CSEnv -> Id -> OutExpr
lookupSubst (CS { cs_subst = sub}) x = lookupIdSubst (text "CSE.lookupSubst") sub x

extendCSSubst :: CSEnv -> Id  -> Id -> CSEnv
extendCSSubst cse x y = cse { cs_subst = extendIdSubst (cs_subst cse) x (Var y) }

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
\end{code}
