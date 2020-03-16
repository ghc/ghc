{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[LiberateCase]{Unroll recursion to allow evals to be lifted from a loop}
-}

{-# LANGUAGE CPP #-}
module GHC.Core.Op.LiberateCase ( liberateCase ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Driver.Session
import GHC.Core
import GHC.Core.Unfold  ( couldBeSmallEnoughToInline )
import TysWiredIn       ( unitDataConId )
import GHC.Types.Id
import GHC.Types.Var.Env
import Util             ( notNull )

{-
The liberate-case transformation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module walks over @Core@, and looks for @case@ on free variables.
The criterion is:
        if there is case on a free on the route to the recursive call,
        then the recursive call is replaced with an unfolding.

Example

   f = \ t -> case v of
                 V a b -> a : f t

=> the inner f is replaced.

   f = \ t -> case v of
                 V a b -> a : (letrec
                                f =  \ t -> case v of
                                               V a b -> a : f t
                               in f) t
(note the NEED for shadowing)

=> Simplify

  f = \ t -> case v of
                 V a b -> a : (letrec
                                f = \ t -> a : f t
                               in f t)

Better code, because 'a' is  free inside the inner letrec, rather
than needing projection from v.

Note that this deals with *free variables*.  SpecConstr deals with
*arguments* that are of known form.  E.g.

        last []     = error
        last (x:[]) = x
        last (x:xs) = last xs


Note [Scrutinee with cast]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
    f = \ t -> case (v `cast` co) of
                 V a b -> a : f t

Exactly the same optimisation (unrolling one call to f) will work here,
despite the cast.  See mk_alt_env in the Case branch of libCase.


To think about (Apr 94)
~~~~~~~~~~~~~~
Main worry: duplicating code excessively.  At the moment we duplicate
the entire binding group once at each recursive call.  But there may
be a group of recursive calls which share a common set of evaluated
free variables, in which case the duplication is a plain waste.

Another thing we could consider adding is some unfold-threshold thing,
so that we'll only duplicate if the size of the group rhss isn't too
big.

Data types
~~~~~~~~~~
The ``level'' of a binder tells how many
recursive defns lexically enclose the binding
A recursive defn "encloses" its RHS, not its
scope.  For example:
\begin{verbatim}
        letrec f = let g = ... in ...
        in
        let h = ...
        in ...
\end{verbatim}
Here, the level of @f@ is zero, the level of @g@ is one,
and the level of @h@ is zero (NB not one).


************************************************************************
*                                                                      *
         Top-level code
*                                                                      *
************************************************************************
-}

liberateCase :: DynFlags -> CoreProgram -> CoreProgram
liberateCase dflags binds = do_prog (initEnv dflags) binds
  where
    do_prog _   [] = []
    do_prog env (bind:binds) = bind' : do_prog env' binds
                             where
                               (env', bind') = libCaseBind env bind

{-
************************************************************************
*                                                                      *
         Main payload
*                                                                      *
************************************************************************

Bindings
~~~~~~~~
-}

libCaseBind :: LibCaseEnv -> CoreBind -> (LibCaseEnv, CoreBind)

libCaseBind env (NonRec binder rhs)
  = (addBinders env [binder], NonRec binder (libCase env rhs))

libCaseBind env (Rec pairs)
  = (env_body, Rec pairs')
  where
    binders = map fst pairs

    env_body = addBinders env binders

    pairs' = [(binder, libCase env_rhs rhs) | (binder,rhs) <- pairs]

        -- We extend the rec-env by binding each Id to its rhs, first
        -- processing the rhs with an *un-extended* environment, so
        -- that the same process doesn't occur for ever!
    env_rhs | is_dupable_bind = addRecBinds env dup_pairs
            | otherwise       = env

    dup_pairs = [ (localiseId binder, libCase env_body rhs)
                | (binder, rhs) <- pairs ]
        -- localiseID : see Note [Need to localiseId in libCaseBind]

    is_dupable_bind = small_enough && all ok_pair pairs

    -- Size: we are going to duplicate dup_pairs; to find their
    --       size, build a fake binding (let { dup_pairs } in (),
    --       and find the size of that
    -- See Note [Small enough]
    small_enough = case bombOutSize env of
                      Nothing   -> True   -- Infinity
                      Just size -> couldBeSmallEnoughToInline (lc_dflags env) size $
                                   Let (Rec dup_pairs) (Var unitDataConId)

    ok_pair (id,_)
        =  idArity id > 0       -- Note [Only functions!]
        && not (isDeadEndId id) -- Note [Not bottoming ids]

{- Note [Not bottoming Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do not specialise error-functions (this is unusual, but I once saw it,
(actually in Data.Typable.Internal)

Note [Only functions!]
~~~~~~~~~~~~~~~~~~~~~~
Consider the following code

       f = g (case v of V a b -> a : t f)

where g is expensive. If we aren't careful, liberate case will turn this into

       f = g (case v of
               V a b -> a : t (letrec f = g (case v of V a b -> a : f t)
                                in f)
             )

Yikes! We evaluate g twice. This leads to a O(2^n) explosion
if g calls back to the same code recursively.

Solution: make sure that we only do the liberate-case thing on *functions*

Note [Small enough]
~~~~~~~~~~~~~~~~~~~
Consider
  \fv. letrec
         f = \x. BIG...(case fv of { (a,b) -> ...g.. })...
         g = \y. SMALL...f...

Then we *can* in principle do liberate-case on 'g' (small RHS) but not
for 'f' (too big).  But doing so is not profitable, because duplicating
'g' at its call site in 'f' doesn't get rid of any cases.  So we just
ask for the whole group to be small enough.

Note [Need to localiseId in libCaseBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The call to localiseId is needed for two subtle reasons
(a)  Reset the export flags on the binders so
        that we don't get name clashes on exported things if the
        local binding floats out to top level.  This is most unlikely
        to happen, since the whole point concerns free variables.
        But resetting the export flag is right regardless.

(b)  Make the name an Internal one.  External Names should never be
        nested; if it were floated to the top level, we'd get a name
        clash at code generation time.

Expressions
~~~~~~~~~~~
-}

libCase :: LibCaseEnv
        -> CoreExpr
        -> CoreExpr

libCase env (Var v)             = libCaseApp env v []
libCase _   (Lit lit)           = Lit lit
libCase _   (Type ty)           = Type ty
libCase _   (Coercion co)       = Coercion co
libCase env e@(App {})          | let (fun, args) = collectArgs e
                                , Var v <- fun
                                = libCaseApp env v args
libCase env (App fun arg)       = App (libCase env fun) (libCase env arg)
libCase env (Tick tickish body) = Tick tickish (libCase env body)
libCase env (Cast e co)         = Cast (libCase env e) co

libCase env (Lam binder body)
  = Lam binder (libCase (addBinders env [binder]) body)

libCase env (Let bind body)
  = Let bind' (libCase env_body body)
  where
    (env_body, bind') = libCaseBind env bind

libCase env (Case scrut bndr ty alts)
  = Case (libCase env scrut) bndr ty (map (libCaseAlt env_alts) alts)
  where
    env_alts = addBinders (mk_alt_env scrut) [bndr]
    mk_alt_env (Var scrut_var) = addScrutedVar env scrut_var
    mk_alt_env (Cast scrut _)  = mk_alt_env scrut       -- Note [Scrutinee with cast]
    mk_alt_env _               = env

libCaseAlt :: LibCaseEnv -> (AltCon, [CoreBndr], CoreExpr)
                         -> (AltCon, [CoreBndr], CoreExpr)
libCaseAlt env (con,args,rhs) = (con, args, libCase (addBinders env args) rhs)

{-
Ids
~~~

To unfold, we can't just wrap the id itself in its binding if it's a join point:

  jump j a b c  =>  (joinrec j x y z = ... in jump j) a b c -- wrong!!!

Every jump must provide all arguments, so we have to be careful to wrap the
whole jump instead:

  jump j a b c  =>  joinrec j x y z = ... in jump j a b c -- right

-}

libCaseApp :: LibCaseEnv -> Id -> [CoreExpr] -> CoreExpr
libCaseApp env v args
  | Just the_bind <- lookupRecId env v  -- It's a use of a recursive thing
  , notNull free_scruts                 -- with free vars scrutinised in RHS
  = Let the_bind expr'

  | otherwise
  = expr'

  where
    rec_id_level = lookupLevel env v
    free_scruts  = freeScruts env rec_id_level
    expr'        = mkApps (Var v) (map (libCase env) args)

freeScruts :: LibCaseEnv
           -> LibCaseLevel      -- Level of the recursive Id
           -> [Id]              -- Ids that are scrutinised between the binding
                                -- of the recursive Id and here
freeScruts env rec_bind_lvl
  = [v | (v, scrut_bind_lvl, scrut_at_lvl) <- lc_scruts env
       , scrut_bind_lvl <= rec_bind_lvl
       , scrut_at_lvl > rec_bind_lvl]
        -- Note [When to specialise]
        -- Note [Avoiding fruitless liberate-case]

{-
Note [When to specialise]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f = \x. letrec g = \y. case x of
                           True  -> ... (f a) ...
                           False -> ... (g b) ...

We get the following levels
          f  0
          x  1
          g  1
          y  2

Then 'x' is being scrutinised at a deeper level than its binding, so
it's added to lc_sruts:  [(x,1)]

We do *not* want to specialise the call to 'f', because 'x' is not free
in 'f'.  So here the bind-level of 'x' (=1) is not <= the bind-level of 'f' (=0).

We *do* want to specialise the call to 'g', because 'x' is free in g.
Here the bind-level of 'x' (=1) is <= the bind-level of 'g' (=1).

Note [Avoiding fruitless liberate-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider also:
  f = \x. case top_lvl_thing of
                I# _ -> let g = \y. ... g ...
                        in ...

Here, top_lvl_thing is scrutinised at a level (1) deeper than its
binding site (0).  Nevertheless, we do NOT want to specialise the call
to 'g' because all the structure in its free variables is already
visible at the definition site for g.  Hence, when considering specialising
an occurrence of 'g', we want to check that there's a scruted-var v st

   a) v's binding site is *outside* g
   b) v's scrutinisation site is *inside* g


************************************************************************
*                                                                      *
        Utility functions
*                                                                      *
************************************************************************
-}

addBinders :: LibCaseEnv -> [CoreBndr] -> LibCaseEnv
addBinders env@(LibCaseEnv { lc_lvl = lvl, lc_lvl_env = lvl_env }) binders
  = env { lc_lvl_env = lvl_env' }
  where
    lvl_env' = extendVarEnvList lvl_env (binders `zip` repeat lvl)

addRecBinds :: LibCaseEnv -> [(Id,CoreExpr)] -> LibCaseEnv
addRecBinds env@(LibCaseEnv {lc_lvl = lvl, lc_lvl_env = lvl_env,
                             lc_rec_env = rec_env}) pairs
  = env { lc_lvl = lvl', lc_lvl_env = lvl_env', lc_rec_env = rec_env' }
  where
    lvl'     = lvl + 1
    lvl_env' = extendVarEnvList lvl_env [(binder,lvl) | (binder,_) <- pairs]
    rec_env' = extendVarEnvList rec_env [(binder, Rec pairs) | (binder,_) <- pairs]

addScrutedVar :: LibCaseEnv
              -> Id             -- This Id is being scrutinised by a case expression
              -> LibCaseEnv

addScrutedVar env@(LibCaseEnv { lc_lvl = lvl, lc_lvl_env = lvl_env,
                                lc_scruts = scruts }) scrut_var
  | bind_lvl < lvl
  = env { lc_scruts = scruts' }
        -- Add to scruts iff the scrut_var is being scrutinised at
        -- a deeper level than its defn

  | otherwise = env
  where
    scruts'  = (scrut_var, bind_lvl, lvl) : scruts
    bind_lvl = case lookupVarEnv lvl_env scrut_var of
                 Just lvl -> lvl
                 Nothing  -> topLevel

lookupRecId :: LibCaseEnv -> Id -> Maybe CoreBind
lookupRecId env id = lookupVarEnv (lc_rec_env env) id

lookupLevel :: LibCaseEnv -> Id -> LibCaseLevel
lookupLevel env id
  = case lookupVarEnv (lc_lvl_env env) id of
      Just lvl -> lvl
      Nothing  -> topLevel

{-
************************************************************************
*                                                                      *
         The environment
*                                                                      *
************************************************************************
-}

type LibCaseLevel = Int

topLevel :: LibCaseLevel
topLevel = 0

data LibCaseEnv
  = LibCaseEnv {
        lc_dflags :: DynFlags,

        lc_lvl :: LibCaseLevel, -- Current level
                -- The level is incremented when (and only when) going
                -- inside the RHS of a (sufficiently small) recursive
                -- function.

        lc_lvl_env :: IdEnv LibCaseLevel,
                -- Binds all non-top-level in-scope Ids (top-level and
                -- imported things have a level of zero)

        lc_rec_env :: IdEnv CoreBind,
                -- Binds *only* recursively defined ids, to their own
                -- binding group, and *only* in their own RHSs

        lc_scruts :: [(Id, LibCaseLevel, LibCaseLevel)]
                -- Each of these Ids was scrutinised by an enclosing
                -- case expression, at a level deeper than its binding
                -- level.
                --
                -- The first LibCaseLevel is the *binding level* of
                --   the scrutinised Id,
                -- The second is the level *at which it was scrutinised*.
                --   (see Note [Avoiding fruitless liberate-case])
                -- The former is a bit redundant, since you could always
                -- look it up in lc_lvl_env, but it's just cached here
                --
                -- The order is insignificant; it's a bag really
                --
                -- There's one element per scrutinisation;
                --    in principle the same Id may appear multiple times,
                --    although that'd be unusual:
                --       case x of { (a,b) -> ....(case x of ...) .. }
        }

initEnv :: DynFlags -> LibCaseEnv
initEnv dflags
  = LibCaseEnv { lc_dflags = dflags,
                 lc_lvl = 0,
                 lc_lvl_env = emptyVarEnv,
                 lc_rec_env = emptyVarEnv,
                 lc_scruts = [] }

-- Bomb-out size for deciding if
-- potential liberatees are too big.
-- (passed in from cmd-line args)
bombOutSize :: LibCaseEnv -> Maybe Int
bombOutSize = liberateCaseThreshold . lc_dflags
