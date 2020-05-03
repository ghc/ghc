{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE RankNTypes, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Typecheck arrow notation
module GHC.Tc.Gen.Arrow ( tcProc ) where

import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcLExpr, tcInferRho, tcSyntaxOp, tcCheckId, tcCheckExpr )

import GHC.Hs
import GHC.Tc.Gen.Match
import GHC.Tc.Utils.Zonk( hsLPatType )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Gen.Bind
import GHC.Tc.Gen.Pat
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Evidence
import GHC.Types.Id( mkLocalId )
import GHC.Tc.Utils.Instantiate
import GHC.Builtin.Types
import GHC.Types.Var.Set
import GHC.Builtin.Types.Arrows( mkArrowStackTupTy, mkArrowEnvTupTy )
import GHC.Builtin.Types.Prim
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Misc

import Control.Monad

{- Note [Arrow overview]
~~~~~~~~~~~~~~~~~~~~~~~~
Arrow notation (aka `proc` notation) is its own little sub-language
with its own quirky typechecking rules. Arrows generalize functions,
and `proc` expressions generalize lambdas. Their syntax is, naturally,
similar to that of lambdas, with the `proc` keyword taking the place
of the backslash:

  expr  ::= ....
         |  proc pat -> cmd        -- arrow abstraction

Arrows cannot, in general, be curried, so `proc` expressions only
accept a single argument pattern. More significantly, their bodies are
not expressions, but *commands*.

The grammar of commands is quite a lot like the grammar of
expressions, but more restrictive. The primitive building block is
arrow application, which applies an arrow to an argument:

  cmd   ::= exp1 -< exp2           -- arrow application (first order)
         |  exp1 -<< exp2          -- arrow application (higher order)

(The difference between -< and -<< is discussed in Note [The command
environment].) Most other commands closely resemble their expression
counterparts and are largely self-explanatory:

  cmd   ::= ....
         |  if exp then cmd1 else cmd2
         |  case exp of { pat1 -> cmd1; ...; patn -> cmdn }
         |  let pat = exp in cmd
         |  do { cstmt1; ...; cstmtn; cmd }

  cstmt ::= cmd
         |  pat <- cmd
         |  let pat = exp
         |  rec { cmd1; ...; cmdn }

The three remaining commands are more subtle:

  cmd   ::= ....
         |  cmd exp                   -- command application
         |  \pat -> cmd               -- command abstraction
         |  (| exp cmd1 ... cmdn |)   -- control operator

The purpose and meaning of these commands is discussed in Note [The
command stack].

Note [The command environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Although lambda expressions have a direct counterpart in core, proc
expressions do not---they are desugared into ordinary applications of
the Arrow class methods. This process essentially involves rewriting
the program into point-free form. For example, the expression

    proc x -> do { y -< f -< x; g -< y }

is desugared into simply `f >>> g`; the intermediate x and y variables
disappear! But this presents a problem: what if x or y appeared in one
of the arrow expressions itself, rather than as an argument?

    proc x -> do { y <- f x -< x; g -< y }

As before, we’d desugar this into `f x >>> g`... but now x is unbound!
So we reject this program as ill-formed. More precisely, we disallow
references to “arrow-local” variables on the left side of a “-<”. (In
fact, we don’t even consider them in scope.)

Which variables are “arrow-local”? All variables bound by the proc
argument pattern or any commands. This is quite restrictive, but it’s
also what makes arrows useful relative to monads: the structure of an
arrow computation doesn’t depend on the arrow’s input. Exceptions to
this restriction implement the ArrowApply class, which provides the
app method:

    app :: ArrowApply p => p (p a b, a) b

Using app, it turns out we can express the above example after all:

    arr (\x -> (f x, x)) >>> app >>> g

We don’t try to figure this out automatically (since that would make
lexical scope dependent on typing!), but the programmer can request it
explicitly using the -<< command:

    proc x -> do { y <- f x -<< x; g -< y }

To summarize: we treat arrow-local variables as out of scope on the
left side of “-<”, but in scope on the left side of “-<<”.

Note [The command stack]
~~~~~~~~~~~~~~~~~~~~~~~~
As discussed in Note [Arrow overview], the grammar of commands
includes “command application” and “command abstraction”:

  cmd   ::= ....
         |  cmd exp        -- command application
         |  \pat -> cmd    -- command abstraction

Note that these are distinct from arrow application (i.e. `f -< e` or
`f -<< e`) and arrow abstraction (i.e. proc expressions themselves).
What do they mean?

These commands manipulate the current *command stack*. Command
application pushes a value onto the stack, and command abstraction
pops one off. For example, the command

    (\x -> f -< x) e

is equivalent to simply `f -< e`. But how is this useful? Ordinary
lambda expressions are valuable because lambdas are first-class
values, but commands are syntactic forms, which can’t be passed around
at all! This is where so-called “arrow control operators” come in:

  cmd   ::= ....
         |  (| exp cmd1 ... cmdn |)   -- control operator

This syntax lifts functions on arrows into functions on *commands*.
For example, suppose the programmer defines a mapA operator,
an arrow analog to Control.Monad.mapM:

    mapA :: ArrowChoice p
         => p (e, a) b
         -> p (e, [a]) [b]

Using (| banana brackets |), mapA can be used in arrow notation as a
sort of “user-defined command”:

    proc (a, b) -> do
      xs <- f -< a
      (| mapA (\x -> g -< (x, b)) |) xs

Note that the lambda passed to mapA is a command, not an expression!
Also note that it refers to b in its body even though b is an
arrow-local variable (see Note [The command environment]).

The argument stack is the mechanism by which arrow control operators
can receive and supply values. When an arrow control operator is
applied, it is passed a tuple containing all the arguments currently
on the stack. Dually, when it passes a tuple to one of its argument
arrows, those values are pushed onto the stack.

From the programmer’s point of view, this process just magically
works, but the process by which we typecheck and desugar these
operators is quite subtle. See Note [Control operator typing] for the
gory details.

Note [Command typing]
~~~~~~~~~~~~~~~~~~~~~
Command typing judgments resemble expression typing judgments,
extended slightly to handle the command environment and the argument
stack. They take the following form:

    G|D |-a cmd :: s --> t      -- command typing

The above judgment should be read as “under contexts G and D, cmd
consumes an argument stack of type s and returns a value of type t in
the arrow a.” For example, given the definitions

    data Foo a b = ...
    instance Category Foo where { ... }
    instance Arrow Foo where { ... }
    foo :: Foo String Bool

we would expect the following judgment to hold:

    G|D |-Foo (foo -< "hello") :: '[] --> Bool

Some notable details of this notation:

  * G tracks the types of ordinary variables, while D tracks the types
    of arrow-local variables (see Note [The command environment]).

  * The long --> arrow is not itself a type, it’s just part of the
    syntax of the judgment.

  * s is a type-level list (a la DataKinds) containing the types of
    all values currently on the stack (see Note [The command stack]),
    which is important for typechecking control operators
    (see Note [Control operator typing]).

Note [Control operator typing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As alluded to in Note [The command stack], typechecking arrow control
operators is subtle. Consider the type of the mapA command:

    mapA :: ArrowChoice p
         => p (e, a) b
         -> p (e, [a]) [b]

If mapA is used as a control operator, we have two problems:

  1. The command passed to mapA is allowed to mention arrow-local
     variables, which means that somehow we have to make sure their
     values are passed to the desugared arrow. But mapA is an
     arbitrary function! How do we know how to get them there?

  2. When we call mapA, we pass it a value of type [a] and receive a
     value of type a on the stack. We determine this information from
     the tuples (e, a) and (e, [a]), but by what process? Those types
     might be unsolved metavariables when we get our hands on them.

We solve both problems with the help of a wired-in type family,
ArrowEnvTup, which morally has the following infinitely-large definition:

    newtype ArrowEnv env = ArrowEnv env

    type family ArrowEnvTup env stk = r | r -> env, stk where
      ArrowEnvTup env '[]           = ArrowEnv env
      ArrowEnvTup env '[t1]         = (ArrowEnv env, t1)
      ArrowEnvTup env '[t1, t2]     = (ArrowEnv env, t1, t2)
      ArrowEnvTup env '[t1, t2, t3] = (ArrowEnv env, t1, t2, t3)
      ...

When we typecheck a control operator command like

    G;D |-a (| op cmd1 ... cmdn |) :: s --> t

we check op against the following type:

    G |- op :: forall e. a1 (ArrowEnvTup e s1) t1
                      -> ...
                      -> an (ArrowEnvTup e sn) t2
                      -> a  (ArrowEnvTup e s)  t

There are several details about this worthy of note:

  1. The first parameter of ArrowEnvTup is the type of the command
     environment (see Note [The command environment]). Since it is
     universally quantified, we can be confident that op will simply
     pass it to each of its arguments unchanged. (The quantification
     also keeps the representation the desugarer uses for the
     environment opaque, so we don’t leak implementation details.)

  2. Normally, checking a type against a type family would lead to
     quite poor type inference, but ArrowEnvTup is injective! This
     means we’ll learn information about s1 through sn from both the
     argument commands and the type of op itself.

  3. As a final detail, note that the type of the arrow can vary in
     each argument to op. This might seem needlessly flexible, but it
     can really matter for operators like mapErrorA:

        -- | The analog to ExceptT for arrows.
        newtype ErrorA e p a b = ErrorA (p a (Either e b))

        mapErrorA :: ArrowChoice p
                  => (e1 -> e2)
                  -> ErrorA e1 p a b
                  -> ErrorA e2 p a b

     Most of the arrow type remains the same, but one of the type
     parameters changes, so forcing them to all be identical would be
     too restrictive.

This strategy works well in practice, though it is rather complicated.
The key trick is the use of the injective type family indexed over a
type-level list, which allows knowledge about the shape of the stack
to propagate bidirectionally, via the ordinary constraint-solving
process. See also Note [Arrow type families] in GHC.Builtin.Types.Arrows.

************************************************************************
*                                                                      *
                Proc
*                                                                      *
************************************************************************
-}

tcProc :: LPat GhcRn -> LHsCmdTop GhcRn         -- proc pat -> expr
       -> ExpRhoType                            -- Expected type of whole proc expression
       -> TcM (LPat GhcTc, LHsCmdTop GhcTcId, TcCoercion)

tcProc pat cmd exp_ty
  = newArrowScope $
    do  { exp_ty <- expTypeToType exp_ty  -- no higher-rank stuff with arrows
        ; (co, (exp_ty1, res_ty)) <- matchExpectedAppTy exp_ty
        ; (co1, (arr_ty, arg_ty)) <- matchExpectedAppTy exp_ty1
        ; let cmd_env = CmdEnv { cmd_arr = arr_ty }
        ; (pat', cmd') <- tcCheckPat ProcExpr pat arg_ty $
                          tcCmdTop cmd_env cmd (nilTy, res_ty)
        ; let res_co = mkTcTransCo co
                         (mkTcAppCo co1 (mkTcNomReflCo res_ty))
        ; return (pat', cmd', res_co) }

{-
************************************************************************
*                                                                      *
                Commands
*                                                                      *
************************************************************************
-}

-- See Note [Arrow overview]
type CmdType    = (CmdArgType, TcTauType)    -- cmd_type
type CmdArgType = TcTauType                  -- carg_type, a type-level list

data CmdEnv
  = CmdEnv {
        cmd_arr :: TcType -- arrow type constructor, of kind *->*->*
    }

mkCmdArrTy :: CmdEnv -> TcTauType -> TcTauType -> TcTauType
mkCmdArrTy env t1 t2 = mkAppTys (cmd_arr env) [t1, t2]

---------------------------------------
tcCmdTop :: CmdEnv
         -> LHsCmdTop GhcRn
         -> CmdType
         -> TcM (LHsCmdTop GhcTcId)

tcCmdTop env (L loc (HsCmdTop names cmd)) cmd_ty@(cmd_stk, res_ty)
  = setSrcSpan loc $
    do  { cmd'   <- tcCmd env cmd cmd_ty
        ; names' <- mapM (tcSyntaxName ProcOrigin (cmd_arr env)) names
        ; return (L loc $ HsCmdTop (CmdTopTc cmd_stk res_ty names') cmd') }

----------------------------------------
tcCmd  :: CmdEnv -> LHsCmd GhcRn -> CmdType -> TcM (LHsCmd GhcTcId)
        -- The main recursive function
tcCmd env (L loc cmd) res_ty
  = setSrcSpan loc $ do
        { cmd' <- tc_cmd env cmd res_ty
        ; return (L loc cmd') }

tc_cmd :: CmdEnv -> HsCmd GhcRn  -> CmdType -> TcM (HsCmd GhcTcId)
tc_cmd env (HsCmdPar x cmd) res_ty
  = do  { cmd' <- tcCmd env cmd res_ty
        ; return (HsCmdPar x cmd') }

tc_cmd env (HsCmdLet x (L l binds) (L body_loc body)) cmd_ty
  = do  { (binds', body') <- tcLocalBinds binds         $
                             setSrcSpan body_loc        $
                             tc_cmd env body cmd_ty
        ; return (HsCmdLet x (L l binds') (L body_loc body')) }

tc_cmd env in_cmd@(HsCmdCase x scrut matches) cmd_ty
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (scrut', scrut_ty) <- tcInferRho scrut
      matches' <- tcCmdMatches env cmd_ty scrut_ty matches
      return (HsCmdCase x scrut' matches')

tc_cmd env in_cmd@(HsCmdLamCase x matches) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (co, scrut_ty, stk_ty') <- matchConsTy stk_ty
      matches' <- tcCmdMatches env (stk_ty', res_ty) scrut_ty matches
      return (mkHsCmdWrap (mkWpCastN co) (HsCmdLamCase x matches'))

tc_cmd env (HsCmdIf x NoSyntaxExprRn pred b1 b2) cmd_ty -- Ordinary 'if'
  = do  { pred' <- tcLExpr pred (mkCheckExpType boolTy)
        ; b1'   <- tcCmd env b1 cmd_ty
        ; b2'   <- tcCmd env b2 cmd_ty
        ; return (HsCmdIf x NoSyntaxExprTc pred' b1' b2')
        }

tc_cmd env (HsCmdIf x fun@(SyntaxExprRn {}) pred b1 b2) cmd_ty -- Rebindable syntax for if
  = do  { pred_ty <- newOpenFlexiTyVarTy
        -- For arrows, need ifThenElse :: forall r. T -> r -> r -> r
        -- because we're going to apply it to the environment, not
        -- the return value.
        ; (_, [r_tv]) <- tcInstSkolTyVars [alphaTyVar]
        ; let r_ty = mkTyVarTy r_tv
        ; checkTc (not (r_tv `elemVarSet` tyCoVarsOfType pred_ty))
                  (text "Predicate type of `ifThenElse' depends on result type")
        ; (pred', fun')
            <- tcSyntaxOp IfOrigin fun (map synKnownType [pred_ty, r_ty, r_ty])
                                       (mkCheckExpType r_ty) $ \ _ ->
               tcLExpr pred (mkCheckExpType pred_ty)

        ; b1' <- tcCmd env b1 cmd_ty
        ; b2' <- tcCmd env b2 cmd_ty
        ; return (HsCmdIf x fun' pred' b1' b2')
        }

-------------------------------------------
--              Arrow application
--          (f -< a)   or   (f -<< a)
--
-- G   |- e1 :: a (ArrowStackTup (b ': s)) t
-- G,D |- e2 :: b
-- ----------------------------------------- [AppF]
-- G|D |-a e1 -< e2 :: s --> t
--
-- G,D |- e1 :: a (ArrowStackTup (b ': s)) t
-- G,D |- e2 :: b
-- a ∈ ArrowApply
-- ----------------------------------------- [AppH]
-- G|D |-a e1 -<< e2 :: s --> t

tc_cmd env cmd@(HsCmdArrApp _ fun arg ho_app lr) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt cmd) $
    do  { arg_ty <- select_arrow_scope newOpenFlexiTyVarTy

        ; let args_ty = mkArrowStackTupTy (consTy arg_ty stk_ty)
              fun_ty = mkCmdArrTy env args_ty res_ty
        ; fun' <- select_arrow_scope (tcLExpr fun (mkCheckExpType fun_ty))

        ; arg' <- tcLExpr arg (mkCheckExpType arg_ty)

        ; return (HsCmdArrApp (cmd_arr env) fun' arg' ho_app lr) }
  where
       -- Before type-checking f, use the environment of the enclosing
       -- proc for the (-<) case.
       -- Local bindings, inside the enclosing proc, are not in scope
       -- inside f.  In the higher-order case (-<<), they are.
       -- See Note [Escaping the arrow scope] in GHC.Tc.Types
    select_arrow_scope :: TcM a -> TcM a
    select_arrow_scope tc = case ho_app of
        HsHigherOrderApp -> tc
        HsFirstOrderApp  -> escapeArrowScope tc

-------------------------------------------
--              Command application
--
-- G|D |-a cmd :: (b ': s) --> t
-- G,D |-  e   :: b
-- ----------------------------- [AppC]
-- G|D |-a cmd e :: s --> t

tc_cmd env cmd@(HsCmdApp x fun arg) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt cmd) $
    do  { arg_ty <- newOpenFlexiTyVarTy
        ; fun'   <- tcCmd env fun (consTy arg_ty stk_ty, res_ty)
        ; arg'   <- tcLExpr arg (mkCheckExpType arg_ty)
        ; return (HsCmdApp x fun' arg') }

-------------------------------------------
--              Lambda
--
-- pat : b => D2
-- G|D1,D2 |-a cmd :: s --> t
-- -------------------------------------- [Abs]
-- G|D1 |-a \pat -> cmd :: (b ': s) --> t

tc_cmd env
       (HsCmdLam x (MG { mg_alts = L l [L mtch_loc
                                   (match@(Match { m_pats = pats, m_grhss = grhss }))],
                         mg_origin = origin }))
       (stk_ty, res_ty)
  = addErrCtxt (pprMatchInCtxt match) $
    do  { (co, arg_tys, stk_ty') <- matchConsTys n_pats stk_ty

                -- Check the patterns, and the GRHSs inside
        ; (pats', grhss') <- setSrcSpan mtch_loc                                 $
                             tcPats LambdaExpr pats (map mkCheckExpType arg_tys) $
                             tc_grhss grhss stk_ty' (mkCheckExpType res_ty)

        ; let match' = L mtch_loc (Match { m_ext = noExtField
                                         , m_ctxt = LambdaExpr, m_pats = pats'
                                         , m_grhss = grhss' })
              arg_tys = map hsLPatType pats'
              cmd' = HsCmdLam x (MG { mg_alts = L l [match']
                                    , mg_ext = MatchGroupTc arg_tys res_ty
                                    , mg_origin = origin })
        ; return (mkHsCmdWrap (mkWpCastN co) cmd') }
  where
    n_pats     = length pats
    match_ctxt = (LambdaExpr :: HsMatchContext GhcRn)    -- Maybe KappaExpr?
    pg_ctxt    = PatGuard match_ctxt

    tc_grhss (GRHSs x grhss (L l binds)) stk_ty res_ty
        = do { (binds', grhss') <- tcLocalBinds binds $
                                   mapM (wrapLocM (tc_grhs stk_ty res_ty)) grhss
             ; return (GRHSs x grhss' (L l binds')) }

    tc_grhs stk_ty res_ty (GRHS x guards body)
        = do { (guards', rhs') <- tcStmtsAndThen pg_ctxt tcGuardStmt guards res_ty $
                                  \ res_ty -> tcCmd env body
                                                (stk_ty, checkingExpType "tc_grhs" res_ty)
             ; return (GRHS x guards' rhs') }

-------------------------------------------
--              Do notation

tc_cmd env (HsCmdDo _ (L l stmts) ) (stk_ty, res_ty)
  = do  { co <- unifyType Nothing stk_ty nilTy -- no arguments allowed on the stack
        ; stmts' <- tcStmts ArrowExpr (tcArrDoStmt env) stmts res_ty
        ; return (mkHsCmdWrap (mkWpCastN co) (HsCmdDo res_ty (L l stmts'))) }


-----------------------------------------------------------------
--      Arrow control operators     (| e cmd1 ... cmdn |)
--
-- G |- e : forall w. a1 (ArrowEnvTup w s1) t1
--                            ...
--                 -> an (ArrowEnvTup w sn) tn
--                 -> a0 (ArrowEnvTup w s0) t0
-- G|D |-a1 cmd1 : s1 --> t1
-- ...
-- G|D |-an cmdn : sn --> tn
-- ------------------------------------------ [Op]
-- G|D |-a0 (| e cmd1 ... cmdn |) : s0 --> t0

tc_cmd env cmd@(HsCmdArrForm x expr f fixity cmd_args) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt cmd) $
    do  { (cmd_args', cmd_tys) <- mapAndUnzipM tc_cmd_arg cmd_args
                              -- We use alphaTyVar for 'w'
        ; let e_ty = mkInvForAllTy alphaTyVar $
                     mkVisFunTys cmd_tys $
                     mkCmdArrTy env (mkArrowEnvTupTy alphaTy stk_ty) res_ty
        ; expr' <- tcCheckExpr expr e_ty
        ; return (HsCmdArrForm x expr' f fixity cmd_args') }

  where
    tc_cmd_arg :: LHsCmdTop GhcRn -> TcM (LHsCmdTop GhcTcId, TcType)
    tc_cmd_arg cmd
       = do { arr_ty <- newFlexiTyVarTy arrowTyConKind
            ; stk_ty <- newFlexiTyVarTy (mkListTy liftedTypeKind)
            ; res_ty <- newFlexiTyVarTy liftedTypeKind
            ; let env' = env { cmd_arr = arr_ty }
            ; cmd' <- tcCmdTop env' cmd (stk_ty, res_ty)
            ; let arg_ty = mkArrowEnvTupTy alphaTy stk_ty
            ; return (cmd', mkCmdArrTy env' arg_ty res_ty) }

-----------------------------------------------------------------
--              Base case for illegal commands
-- This is where expressions that aren't commands get rejected

tc_cmd _ cmd _
  = failWithTc (vcat [text "The expression", nest 2 (ppr cmd),
                      text "was found where an arrow command was expected"])

-- | Typechecking for case command alternatives. Used for both
-- 'HsCmdCase' and 'HsCmdLamCase'.
tcCmdMatches :: CmdEnv
             -> CmdType
             -> TcType                           -- ^ type of the scrutinee
             -> MatchGroup GhcRn (LHsCmd GhcRn)  -- ^ case alternatives
             -> TcM (MatchGroup GhcTcId (LHsCmd GhcTcId))
tcCmdMatches env (stk_ty, res_ty) scrut_ty matches
  = tcMatchesCase match_ctxt scrut_ty matches (mkCheckExpType res_ty)
  where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = mc_body }
    mc_body body res_ty' = do { res_ty' <- expTypeToType res_ty'
                              ; tcCmd env body (stk_ty, res_ty') }

{-
************************************************************************
*                                                                      *
                Stmts
*                                                                      *
************************************************************************
-}

--------------------------------
--      Mdo-notation
-- The distinctive features here are
--      (a) RecStmts, and
--      (b) no rebindable syntax

tcArrDoStmt :: CmdEnv -> TcCmdStmtChecker
tcArrDoStmt env _ (LastStmt x rhs noret _) res_ty thing_inside
  = do  { rhs' <- tcCmd env rhs (nilTy, res_ty)
        ; thing <- thing_inside (panic "tcArrDoStmt")
        ; return (LastStmt x rhs' noret noSyntaxExpr, thing) }

tcArrDoStmt env _ (BodyStmt _ rhs _ _) res_ty thing_inside
  = do  { (rhs', elt_ty) <- tc_arr_rhs env rhs
        ; thing          <- thing_inside res_ty
        ; return (BodyStmt elt_ty rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcArrDoStmt env ctxt (BindStmt _ pat rhs) res_ty thing_inside
  = do  { (rhs', pat_ty) <- tc_arr_rhs env rhs
        ; (pat', thing)  <- tcCheckPat (StmtCtxt ctxt) pat pat_ty $
                            thing_inside res_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

tcArrDoStmt env ctxt (RecStmt { recS_stmts = stmts, recS_later_ids = later_names
                            , recS_rec_ids = rec_names }) res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- newFlexiTyVarTys (length tup_names) liftedTypeKind
        ; let tup_ids = zipWith mkLocalId tup_names tup_elt_tys
        ; tcExtendIdEnv tup_ids $ do
        { (stmts', tup_rets)
                <- tcStmtsAndThen ctxt (tcArrDoStmt env) stmts res_ty   $ \ _res_ty' ->
                        -- ToDo: res_ty not really right
                   zipWithM tcCheckId tup_names (map mkCheckExpType tup_elt_tys)

        ; thing <- thing_inside res_ty
                -- NB:  The rec_ids for the recursive things
                --      already scope over this part. This binding may shadow
                --      some of them with polymorphic things with the same Name
                --      (see note [RecStmt] in GHC.Hs.Expr)

        ; let rec_ids = takeList rec_names tup_ids
        ; later_ids <- tcLookupLocalIds later_names

        ; let rec_rets = takeList rec_names tup_rets
        ; let ret_table = zip tup_ids tup_rets
        ; let later_rets = [r | i <- later_ids, (j, r) <- ret_table, i == j]

        ; return (emptyRecStmtId { recS_stmts = stmts'
                                 , recS_later_ids = later_ids
                                 , recS_rec_ids = rec_ids
                                 , recS_ext = unitRecStmtTc
                                     { recS_later_rets = later_rets
                                     , recS_rec_rets = rec_rets
                                     , recS_ret_ty = res_ty} }, thing)
        }}

tcArrDoStmt _ _ stmt _ _
  = pprPanic "tcArrDoStmt: unexpected Stmt" (ppr stmt)

tc_arr_rhs :: CmdEnv -> LHsCmd GhcRn -> TcM (LHsCmd GhcTcId, TcType)
tc_arr_rhs env rhs = do { ty <- newFlexiTyVarTy liftedTypeKind
                        ; rhs' <- tcCmd env rhs (nilTy, ty)
                        ; return (rhs', ty) }

{-
************************************************************************
*                                                                      *
                Helpers
*                                                                      *
************************************************************************
-}

nilTy :: Type
nilTy = mkTyConApp promotedNilDataCon [liftedTypeKind]

consTy :: Type -> Type -> Type
consTy ty tys = mkTyConApp promotedConsDataCon [liftedTypeKind, ty, tys]

matchConsTy :: TcType -> TcM (TcCoercionN, TcType, TcType)
matchConsTy tys = do
  (co1, [k, ty, tys']) <- matchExpectedTyConApp promotedConsDataCon tys
  k_co <- unifyKind Nothing k liftedTypeKind
  let co2 = mkTcTyConAppCo Nominal promotedConsDataCon
              [k_co, mkTcNomReflCo ty, mkTcNomReflCo tys']
  pure (mkTcTransCo co1 co2, ty, tys')

-- | @'matchConsTys' n ty@ expects @ty@ to be a type-level list of the shape
-- > a0 ': a1 ': ... ': an ': ty'
-- and returns @[a0, a1, ..., an]@ and @ty'@ (plus evidence).
matchConsTys :: Int -> TcType -> TcM (TcCoercionN, [TcType], TcType)
matchConsTys 0 tys = pure (mkTcNomReflCo tys, [], tys)
matchConsTys n tys = do
  (co1, ty1, tys1) <- matchConsTy tys
  (co2, ty2, tys2) <- matchConsTys (n-1) tys1
  pure (mkTcTransCo co1 (mkNomConsCo (mkTcNomReflCo ty1) co2), ty1:ty2, tys2)

mkNomConsCo :: TcCoercionN -> TcCoercionN -> TcCoercionN
mkNomConsCo co1 co2 = mkTcTyConAppCo Nominal promotedConsDataCon
                        [mkTcNomReflCo liftedTypeKind, co1, co2]

arrowTyConKind :: Kind          --  *->*->*
arrowTyConKind = mkVisFunTys [liftedTypeKind, liftedTypeKind] liftedTypeKind

{-
************************************************************************
*                                                                      *
                Errors
*                                                                      *
************************************************************************
-}

cmdCtxt :: HsCmd GhcRn -> SDoc
cmdCtxt cmd = text "In the command:" <+> ppr cmd
