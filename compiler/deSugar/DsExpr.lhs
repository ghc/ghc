%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Desugaring exporessions.

\begin{code}
module DsExpr ( dsExpr, dsLExpr, dsLocalBinds, dsValBinds, dsLit ) where

#include "HsVersions.h"

import Match
import MatchLit
import DsBinds
import DsGRHSs
import DsListComp
import DsUtils
import DsArrows
import DsMonad
import Name
import NameEnv

#ifdef GHCI
        -- Template Haskell stuff iff bootstrapped
import DsMeta
#endif

import HsSyn

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types
import TcType
import TcEvidence
import TcRnMonad
import Type
import CoreSyn
import CoreUtils
import CoreFVs
import MkCore

import DynFlags
import CostCentre
import Id
import Module
import VarSet
import VarEnv
import DataCon
import TysWiredIn
import BasicTypes
import Maybes
import SrcLoc
import Util
import Bag
import Outputable
import FastString

import Control.Monad
\end{code}


%************************************************************************
%*                                                                      *
                dsLocalBinds, dsValBinds
%*                                                                      *
%************************************************************************

\begin{code}
dsLocalBinds :: HsLocalBinds Id -> CoreExpr -> DsM CoreExpr
dsLocalBinds EmptyLocalBinds    body = return body
dsLocalBinds (HsValBinds binds) body = dsValBinds binds body
dsLocalBinds (HsIPBinds binds)  body = dsIPBinds  binds body

-------------------------
dsValBinds :: HsValBinds Id -> CoreExpr -> DsM CoreExpr
dsValBinds (ValBindsOut binds _) body = foldrM ds_val_bind body binds
dsValBinds (ValBindsIn  _     _) _    = panic "dsValBinds ValBindsIn"

-------------------------
dsIPBinds :: HsIPBinds Id -> CoreExpr -> DsM CoreExpr
dsIPBinds (IPBinds ip_binds ev_binds) body
  = do  { ds_binds <- dsTcEvBinds ev_binds
        ; let inner = mkCoreLets ds_binds body
                -- The dict bindings may not be in 
                -- dependency order; hence Rec
        ; foldrM ds_ip_bind inner ip_binds }
  where
    ds_ip_bind (L _ (IPBind ~(Right n) e)) body
      = do e' <- dsLExpr e
           return (Let (NonRec n e') body)

-------------------------
ds_val_bind :: (RecFlag, LHsBinds Id) -> CoreExpr -> DsM CoreExpr
-- Special case for bindings which bind unlifted variables
-- We need to do a case right away, rather than building
-- a tuple and doing selections.
-- Silently ignore INLINE and SPECIALISE pragmas...
ds_val_bind (NonRecursive, hsbinds) body
  | [L loc bind] <- bagToList hsbinds,
        -- Non-recursive, non-overloaded bindings only come in ones
        -- ToDo: in some bizarre case it's conceivable that there
        --       could be dict binds in the 'binds'.  (See the notes
        --       below.  Then pattern-match would fail.  Urk.)
    strictMatchOnly bind
  = putSrcSpanDs loc (dsStrictBind bind body)

-- Ordinary case for bindings; none should be unlifted
ds_val_bind (_is_rec, binds) body
  = do  { prs <- dsLHsBinds binds
        ; ASSERT2( not (any (isUnLiftedType . idType . fst) prs), ppr _is_rec $$ ppr binds )
          case prs of
            [] -> return body
            _  -> return (Let (Rec prs) body) }
        -- Use a Rec regardless of is_rec. 
        -- Why? Because it allows the binds to be all
        -- mixed up, which is what happens in one rare case
        -- Namely, for an AbsBind with no tyvars and no dicts,
        --         but which does have dictionary bindings.
        -- See notes with TcSimplify.inferLoop [NO TYVARS]
        -- It turned out that wrapping a Rec here was the easiest solution
        --
        -- NB The previous case dealt with unlifted bindings, so we
        --    only have to deal with lifted ones now; so Rec is ok

------------------
dsStrictBind :: HsBind Id -> CoreExpr -> DsM CoreExpr
dsStrictBind (AbsBinds { abs_tvs = [], abs_ev_vars = []
               , abs_exports = exports
               , abs_ev_binds = ev_binds
               , abs_binds = binds }) body
  = do { let body1 = foldr bind_export body exports
             bind_export export b = bindNonRec (abe_poly export) (Var (abe_mono export)) b
       ; body2 <- foldlBagM (\body bind -> dsStrictBind (unLoc bind) body) 
                            body1 binds 
       ; ds_binds <- dsTcEvBinds ev_binds
       ; return (mkCoreLets ds_binds body2) }

dsStrictBind (FunBind { fun_id = L _ fun, fun_matches = matches, fun_co_fn = co_fn 
                      , fun_tick = tick, fun_infix = inf }) body
                -- Can't be a bang pattern (that looks like a PatBind)
                -- so must be simply unboxed
  = do { (args, rhs) <- matchWrapper (FunRhs (idName fun ) inf) matches
       ; MASSERT( null args ) -- Functions aren't lifted
       ; MASSERT( isIdHsWrapper co_fn )
       ; let rhs' = mkOptTickBox tick rhs
       ; return (bindNonRec fun rhs' body) }

dsStrictBind (PatBind {pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty }) body
  =     -- let C x# y# = rhs in body
        -- ==> case rhs of C x# y# -> body
    do { rhs <- dsGuarded grhss ty
       ; let upat = unLoc pat
             eqn = EqnInfo { eqn_pats = [upat], 
                             eqn_rhs = cantFailMatchResult body }
       ; var    <- selectMatchVar upat
       ; result <- matchEquations PatBindRhs [var] [eqn] (exprType body)
       ; return (bindNonRec var rhs result) }

dsStrictBind bind body = pprPanic "dsLet: unlifted" (ppr bind $$ ppr body)

----------------------
strictMatchOnly :: HsBind Id -> Bool
strictMatchOnly (AbsBinds { abs_binds = binds })
  = anyBag (strictMatchOnly . unLoc) binds
strictMatchOnly (PatBind { pat_lhs = lpat, pat_rhs_ty = ty })
  =  isUnLiftedType ty 
  || isBangLPat lpat   
  || any (isUnLiftedType . idType) (collectPatBinders lpat)
strictMatchOnly (FunBind { fun_id = L _ id })
  = isUnLiftedType (idType id)
strictMatchOnly _ = False -- I hope!  Checked immediately by caller in fact

\end{code}

%************************************************************************
%*                                                                      *
\subsection[DsExpr-vars-and-cons]{Variables, constructors, literals}
%*                                                                      *
%************************************************************************

\begin{code}
dsLExpr :: LHsExpr Id -> DsM CoreExpr

dsLExpr (L loc e) = putSrcSpanDs loc $ dsExpr e

dsExpr :: HsExpr Id -> DsM CoreExpr
dsExpr (HsPar e)              = dsLExpr e
dsExpr (ExprWithTySigOut e _) = dsLExpr e
dsExpr (HsVar var)            = return (varToCoreExpr var)   -- See Note [Desugaring vars]
dsExpr (HsIPVar _)            = panic "dsExpr: HsIPVar"
dsExpr (HsLit lit)            = dsLit lit
dsExpr (HsOverLit lit)        = dsOverLit lit

dsExpr (HsWrap co_fn e)
  = do { e' <- dsExpr e
       ; wrapped_e <- dsHsWrapper co_fn e'
       ; dflags <- getDynFlags
       ; warnAboutIdentities dflags e' (exprType wrapped_e)
       ; return wrapped_e }

dsExpr (NegApp expr neg_expr) 
  = App <$> dsExpr neg_expr <*> dsLExpr expr

dsExpr (HsLam a_Match)
  = uncurry mkLams <$> matchWrapper LambdaExpr a_Match

dsExpr (HsLamCase arg matches)
  = do { arg_var <- newSysLocalDs arg
       ; ([discrim_var], matching_code) <- matchWrapper CaseAlt matches
       ; return $ Lam arg_var $ bindNonRec discrim_var (Var arg_var) matching_code }

dsExpr (HsApp fun arg)
  = mkCoreAppDs <$> dsLExpr fun <*>  dsLExpr arg

dsExpr (HsUnboundVar _) = panic "dsExpr: HsUnboundVar"
\end{code}

Note [Desugaring vars]
~~~~~~~~~~~~~~~~~~~~~~
In one situation we can get a *coercion* variable in a HsVar, namely
the support method for an equality superclass:
   class (a~b) => C a b where ...
   instance (blah) => C (T a) (T b) where ..
Then we get
   $dfCT :: forall ab. blah => C (T a) (T b)
   $dfCT ab blah = MkC ($c$p1C a blah) ($cop a blah)

   $c$p1C :: forall ab. blah => (T a ~ T b)
   $c$p1C ab blah = let ...; g :: T a ~ T b = ... } in g

That 'g' in the 'in' part is an evidence variable, and when
converting to core it must become a CO.
   
Operator sections.  At first it looks as if we can convert
\begin{verbatim}
        (expr op)
\end{verbatim}
to
\begin{verbatim}
        \x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
        map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
        let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.

\begin{code}
dsExpr (OpApp e1 op _ e2)
  = -- for the type of y, we need the type of op's 2nd argument
    mkCoreAppsDs <$> dsLExpr op <*> mapM dsLExpr [e1, e2]
    
dsExpr (SectionL expr op)       -- Desugar (e !) to ((!) e)
  = mkCoreAppDs <$> dsLExpr op <*> dsLExpr expr

-- dsLExpr (SectionR op expr)   -- \ x -> op x expr
dsExpr (SectionR op expr) = do
    core_op <- dsLExpr op
    -- for the type of x, we need the type of op's 2nd argument
    let (x_ty:y_ty:_, _) = splitFunTys (exprType core_op)
        -- See comment with SectionL
    y_core <- dsLExpr expr
    x_id <- newSysLocalDs x_ty
    y_id <- newSysLocalDs y_ty
    return (bindNonRec y_id y_core $
            Lam x_id (mkCoreAppsDs core_op [Var x_id, Var y_id]))

dsExpr (ExplicitTuple tup_args boxity)
  = do { let go (lam_vars, args) (Missing ty)
                    -- For every missing expression, we need
                    -- another lambda in the desugaring.
               = do { lam_var <- newSysLocalDs ty
                    ; return (lam_var : lam_vars, Var lam_var : args) }
             go (lam_vars, args) (Present expr)
                    -- Expressions that are present don't generate
                    -- lambdas, just arguments.
               = do { core_expr <- dsLExpr expr
                    ; return (lam_vars, core_expr : args) }

       ; (lam_vars, args) <- foldM go ([], []) (reverse tup_args)
                -- The reverse is because foldM goes left-to-right

       ; return $ mkCoreLams lam_vars $ 
                  mkConApp (tupleCon (boxityNormalTupleSort boxity) (length tup_args))
                           (map (Type . exprType) args ++ args) }

dsExpr (HsSCC cc expr@(L loc _)) = do
    mod_name <- getModule
    count <- goptM Opt_ProfCountEntries
    uniq <- newUnique
    Tick (ProfNote (mkUserCC cc mod_name loc uniq) count True) <$> dsLExpr expr

dsExpr (HsCoreAnn _ expr)
  = dsLExpr expr

dsExpr (HsCase discrim matches)
  = do { core_discrim <- dsLExpr discrim
       ; ([discrim_var], matching_code) <- matchWrapper CaseAlt matches
       ; return (bindNonRec discrim_var core_discrim matching_code) }

-- Pepe: The binds are in scope in the body but NOT in the binding group
--       This is to avoid silliness in breakpoints
dsExpr (HsLet binds body) = do
    body' <- dsLExpr body
    dsLocalBinds binds body'

-- We need the `ListComp' form to use `deListComp' (rather than the "do" form)
-- because the interpretation of `stmts' depends on what sort of thing it is.
--
dsExpr (HsDo ListComp     stmts res_ty) = dsListComp stmts res_ty
dsExpr (HsDo PArrComp     stmts _)      = dsPArrComp (map unLoc stmts)
dsExpr (HsDo DoExpr       stmts _)      = dsDo stmts 
dsExpr (HsDo GhciStmtCtxt stmts _)      = dsDo stmts 
dsExpr (HsDo MDoExpr      stmts _)      = dsDo stmts 
dsExpr (HsDo MonadComp    stmts _)      = dsMonadComp stmts

dsExpr (HsIf mb_fun guard_expr then_expr else_expr)
  = do { pred <- dsLExpr guard_expr
       ; b1 <- dsLExpr then_expr
       ; b2 <- dsLExpr else_expr
       ; case mb_fun of
           Just fun -> do { core_fun <- dsExpr fun
                          ; return (mkCoreApps core_fun [pred,b1,b2]) }
           Nothing  -> return $ mkIfThenElse pred b1 b2 }

dsExpr (HsMultiIf res_ty alts)
  | null alts
  = mkErrorExpr

  | otherwise
  = do { match_result <- liftM (foldr1 combineMatchResults)
                               (mapM (dsGRHS IfAlt res_ty) alts)
       ; error_expr   <- mkErrorExpr
       ; extractMatchResult match_result error_expr }
  where
    mkErrorExpr = mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID res_ty
                               (ptext (sLit "multi-way if"))
\end{code}


\noindent
\underline{\bf Various data construction things}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
dsExpr (ExplicitList elt_ty wit xs) 
  = dsExplicitList elt_ty wit xs

-- We desugar [:x1, ..., xn:] as
--   singletonP x1 +:+ ... +:+ singletonP xn
--
dsExpr (ExplicitPArr ty []) = do
    emptyP <- dsDPHBuiltin emptyPVar
    return (Var emptyP `App` Type ty)
dsExpr (ExplicitPArr ty xs) = do
    singletonP <- dsDPHBuiltin singletonPVar
    appP       <- dsDPHBuiltin appPVar
    xs'        <- mapM dsLExpr xs
    return . foldr1 (binary appP) $ map (unary singletonP) xs'
  where
    unary  fn x   = mkApps (Var fn) [Type ty, x]
    binary fn x y = mkApps (Var fn) [Type ty, x, y]

dsExpr (ArithSeq expr witness seq)
  = case witness of
     Nothing -> dsArithSeq expr seq
     Just fl -> do { 
       ; fl' <- dsExpr fl
       ; newArithSeq <- dsArithSeq expr seq
       ; return (App fl' newArithSeq)}

dsExpr (PArrSeq expr (FromTo from to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, to]

dsExpr (PArrSeq expr (FromThenTo from thn to))
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn, to]

dsExpr (PArrSeq _ _)
  = panic "DsExpr.dsExpr: Infinite parallel array!"
    -- the parser shouldn't have generated it and the renamer and typechecker
    -- shouldn't have let it through
\end{code}

\noindent
\underline{\bf Record construction and update}
%              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For record construction we do this (assuming T has three arguments)
\begin{verbatim}
        T { op2 = e }
==>
        let err = /\a -> recConErr a 
        T (recConErr t1 "M.lhs/230/op1") 
          e 
          (recConErr t1 "M.lhs/230/op3")
\end{verbatim}
@recConErr@ then converts its arugment string into a proper message
before printing it as
\begin{verbatim}
        M.lhs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.

\begin{code}
dsExpr (RecordCon (L _ data_con_id) con_expr rbinds) = do
    con_expr' <- dsExpr con_expr
    let
        (arg_tys, _) = tcSplitFunTys (exprType con_expr')
        -- A newtype in the corner should be opaque; 
        -- hence TcType.tcSplitFunTys

        mk_arg (arg_ty, lbl)    -- Selector id has the field label as its name
          = case findField (rec_flds rbinds) lbl of
              (rhs:rhss) -> ASSERT( null rhss )
                            dsLExpr rhs
              []         -> mkErrorAppDs rEC_CON_ERROR_ID arg_ty (ppr lbl)
        unlabelled_bottom arg_ty = mkErrorAppDs rEC_CON_ERROR_ID arg_ty empty

        labels = dataConFieldLabels (idDataCon data_con_id)
        -- The data_con_id is guaranteed to be the wrapper id of the constructor
    
    con_args <- if null labels
                then mapM unlabelled_bottom arg_tys
                else mapM mk_arg (zipEqual "dsExpr:RecordCon" arg_tys labels)
    
    return (mkApps con_expr' con_args)
\end{code}

Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
        data T = T1 {op1, op2, op3 :: Int}
               | T2 {op4, op2 :: Int}
               | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
        r { op2 = e }
===>
        let op2 = e in
        case r of
          T1 op1 _ op3 -> T1 op1 op2 op3
          T2 op4 _     -> T2 op4 op2
          other        -> recUpdError "M.lhs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

Note [Update for GADTs]
~~~~~~~~~~~~~~~~~~~~~~~
Consider 
   data T a b where
     T1 { f1 :: a } :: T a Int

Then the wrapper function for T1 has type 
   $WT1 :: a -> T a Int
But if x::T a b, then
   x { f1 = v } :: T a b   (not T a Int!)
So we need to cast (T a Int) to (T a b).  Sigh.

\begin{code}
dsExpr expr@(RecordUpd record_expr (HsRecFields { rec_flds = fields })
                       cons_to_upd in_inst_tys out_inst_tys)
  | null fields
  = dsLExpr record_expr
  | otherwise
  = ASSERT2( notNull cons_to_upd, ppr expr )

    do  { record_expr' <- dsLExpr record_expr
        ; field_binds' <- mapM ds_field fields
        ; let upd_fld_env :: NameEnv Id -- Maps field name to the LocalId of the field binding
              upd_fld_env = mkNameEnv [(f,l) | (f,l,_) <- field_binds']

        -- It's important to generate the match with matchWrapper,
        -- and the right hand sides with applications of the wrapper Id
        -- so that everything works when we are doing fancy unboxing on the
        -- constructor aguments.
        ; alts <- mapM (mk_alt upd_fld_env) cons_to_upd
        ; ([discrim_var], matching_code) 
                <- matchWrapper RecUpd (MG { mg_alts = alts, mg_arg_tys = [in_ty], mg_res_ty = out_ty })

        ; return (add_field_binds field_binds' $
                  bindNonRec discrim_var record_expr' matching_code) }
  where
    ds_field :: HsRecField Id (LHsExpr Id) -> DsM (Name, Id, CoreExpr)
      -- Clone the Id in the HsRecField, because its Name is that
      -- of the record selector, and we must not make that a lcoal binder
      -- else we shadow other uses of the record selector
      -- Hence 'lcl_id'.  Cf Trac #2735
    ds_field rec_field = do { rhs <- dsLExpr (hsRecFieldArg rec_field)
                            ; let fld_id = unLoc (hsRecFieldId rec_field)
                            ; lcl_id <- newSysLocalDs (idType fld_id)
                            ; return (idName fld_id, lcl_id, rhs) }

    add_field_binds [] expr = expr
    add_field_binds ((_,b,r):bs) expr = bindNonRec b r (add_field_binds bs expr)

        -- Awkwardly, for families, the match goes 
        -- from instance type to family type
    tycon     = dataConTyCon (head cons_to_upd)
    in_ty     = mkTyConApp tycon in_inst_tys
    out_ty    = mkFamilyTyConApp tycon out_inst_tys

    mk_alt upd_fld_env con
      = do { let (univ_tvs, ex_tvs, eq_spec, 
                  theta, arg_tys, _) = dataConFullSig con
                 subst = mkTopTvSubst (univ_tvs `zip` in_inst_tys)

                -- I'm not bothering to clone the ex_tvs
           ; eqs_vars   <- mapM newPredVarDs (substTheta subst (eqSpecPreds eq_spec))
           ; theta_vars <- mapM newPredVarDs (substTheta subst theta)
           ; arg_ids    <- newSysLocalsDs (substTys subst arg_tys)
           ; let val_args = zipWithEqual "dsExpr:RecordUpd" mk_val_arg
                                         (dataConFieldLabels con) arg_ids
                 mk_val_arg field_name pat_arg_id 
                     = nlHsVar (lookupNameEnv upd_fld_env field_name `orElse` pat_arg_id)
                 inst_con = noLoc $ HsWrap wrap (HsVar (dataConWrapId con))
                        -- Reconstruct with the WrapId so that unpacking happens
                 wrap = mkWpEvVarApps theta_vars          <.>
                        mkWpTyApps    (mkTyVarTys ex_tvs) <.>
                        mkWpTyApps [ty | (tv, ty) <- univ_tvs `zip` out_inst_tys
                                       , not (tv `elemVarEnv` wrap_subst) ]
                 rhs = foldl (\a b -> nlHsApp a b) inst_con val_args

                        -- Tediously wrap the application in a cast
                        -- Note [Update for GADTs]
                 wrap_co = mkTcTyConAppCo tycon
                                [ lookup tv ty | (tv,ty) <- univ_tvs `zip` out_inst_tys ]
                 lookup univ_tv ty = case lookupVarEnv wrap_subst univ_tv of
                                        Just co' -> co'
                                        Nothing  -> mkTcReflCo ty
                 wrap_subst = mkVarEnv [ (tv, mkTcSymCo (mkTcCoVarCo eq_var))
                                       | ((tv,_),eq_var) <- eq_spec `zip` eqs_vars ]

                 pat = noLoc $ ConPatOut { pat_con = noLoc con, pat_tvs = ex_tvs
                                         , pat_dicts = eqs_vars ++ theta_vars
                                         , pat_binds = emptyTcEvBinds
                                         , pat_args = PrefixCon $ map nlVarPat arg_ids
                                         , pat_ty = in_ty }
           ; let wrapped_rhs | null eq_spec = rhs
                             | otherwise    = mkLHsWrap (WpCast wrap_co) rhs
           ; return (mkSimpleMatch [pat] wrapped_rhs) }

\end{code}

Here is where we desugar the Template Haskell brackets and escapes

\begin{code}
-- Template Haskell stuff

#ifdef GHCI
dsExpr (HsBracketOut x ps) = dsBracket x ps
#else
dsExpr (HsBracketOut _ _) = panic "dsExpr HsBracketOut"
#endif
dsExpr (HsSpliceE s)       = pprPanic "dsExpr:splice" (ppr s)

-- Arrow notation extension
dsExpr (HsProc pat cmd) = dsProcExpr pat cmd
\end{code}

Hpc Support 

\begin{code}
dsExpr (HsTick tickish e) = do
  e' <- dsLExpr e
  return (Tick tickish e')

-- There is a problem here. The then and else branches
-- have no free variables, so they are open to lifting.
-- We need someway of stopping this.
-- This will make no difference to binary coverage
-- (did you go here: YES or NO), but will effect accurate
-- tick counting.

dsExpr (HsBinTick ixT ixF e) = do
  e2 <- dsLExpr e
  do { ASSERT(exprType e2 `eqType` boolTy)
       mkBinaryTickBox ixT ixF e2
     }
\end{code}

\begin{code}

-- HsSyn constructs that just shouldn't be here:
dsExpr (ExprWithTySig {})  = panic "dsExpr:ExprWithTySig"
dsExpr (HsBracket     {})  = panic "dsExpr:HsBracket"
dsExpr (HsQuasiQuoteE {})  = panic "dsExpr:HsQuasiQuoteE"
dsExpr (HsArrApp      {})  = panic "dsExpr:HsArrApp"
dsExpr (HsArrForm     {})  = panic "dsExpr:HsArrForm"
dsExpr (HsTickPragma  {})  = panic "dsExpr:HsTickPragma"
dsExpr (EWildPat      {})  = panic "dsExpr:EWildPat"
dsExpr (EAsPat        {})  = panic "dsExpr:EAsPat"
dsExpr (EViewPat      {})  = panic "dsExpr:EViewPat"
dsExpr (ELazyPat      {})  = panic "dsExpr:ELazyPat"
dsExpr (HsType        {})  = panic "dsExpr:HsType"
dsExpr (HsDo          {})  = panic "dsExpr:HsDo"


findField :: [HsRecField Id arg] -> Name -> [arg]
findField rbinds lbl 
  = [rhs | HsRecField { hsRecFieldId = id, hsRecFieldArg = rhs } <- rbinds 
         , lbl == idName (unLoc id) ]
\end{code}

%--------------------------------------------------------------------

Note [Desugaring explicit lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Explicit lists are desugared in a cleverer way to prevent some
fruitless allocations.  Essentially, whenever we see a list literal
[x_1, ..., x_n] we:

1. Find the tail of the list that can be allocated statically (say
   [x_k, ..., x_n]) by later stages and ensure we desugar that
   normally: this makes sure that we don't cause a code size increase
   by having the cons in that expression fused (see later) and hence
   being unable to statically allocate any more

2. For the prefix of the list which cannot be allocated statically,
   say [x_1, ..., x_(k-1)], we turn it into an expression involving
   build so that if we find any foldrs over it it will fuse away
   entirely!
   
   So in this example we will desugar to:
   build (\c n -> x_1 `c` x_2 `c` .... `c` foldr c n [x_k, ..., x_n]
   
   If fusion fails to occur then build will get inlined and (since we
   defined a RULE for foldr (:) []) we will get back exactly the
   normal desugaring for an explicit list.

This optimisation can be worth a lot: up to 25% of the total
allocation in some nofib programs. Specifically

        Program           Size    Allocs   Runtime  CompTime
        rewrite          +0.0%    -26.3%      0.02     -1.8%
           ansi          -0.3%    -13.8%      0.00     +0.0%
           lift          +0.0%     -8.7%      0.00     -2.3%

Of course, if rules aren't turned on then there is pretty much no
point doing this fancy stuff, and it may even be harmful.

=======>  Note by SLPJ Dec 08.

I'm unconvinced that we should *ever* generate a build for an explicit
list.  See the comments in GHC.Base about the foldr/cons rule, which 
points out that (foldr k z [a,b,c]) may generate *much* less code than
(a `k` b `k` c `k` z).

Furthermore generating builds messes up the LHS of RULES. 
Example: the foldr/single rule in GHC.Base
   foldr k z [x] = ...
We do not want to generate a build invocation on the LHS of this RULE!

We fix this by disabling rules in rule LHSs, and testing that
flag here; see Note [Desugaring RULE left hand sides] in Desugar

To test this I've added a (static) flag -fsimple-list-literals, which
makes all list literals be generated via the simple route.  


\begin{code}
dsExplicitList :: PostTcType -> Maybe (SyntaxExpr Id) -> [LHsExpr Id] -> DsM CoreExpr
-- See Note [Desugaring explicit lists]
dsExplicitList elt_ty Nothing xs
  = do { dflags <- getDynFlags
       ; xs' <- mapM dsLExpr xs
       ; let (dynamic_prefix, static_suffix) = spanTail is_static xs'
       ; if gopt Opt_SimpleListLiterals dflags        -- -fsimple-list-literals
         || not (gopt Opt_EnableRewriteRules dflags)  -- Rewrite rules off
                -- Don't generate a build if there are no rules to eliminate it!
                -- See Note [Desugaring RULE left hand sides] in Desugar
         || null dynamic_prefix   -- Avoid build (\c n. foldr c n xs)!
         then return $ mkListExpr elt_ty xs'
         else mkBuildExpr elt_ty (mkSplitExplicitList dynamic_prefix static_suffix) }
  where
    is_static :: CoreExpr -> Bool
    is_static e = all is_static_var (varSetElems (exprFreeVars e))

    is_static_var :: Var -> Bool
    is_static_var v 
      | isId v = isExternalName (idName v)  -- Top-level things are given external names
      | otherwise = False                   -- Type variables

    mkSplitExplicitList prefix suffix (c, _) (n, n_ty)
      = do { let suffix' = mkListExpr elt_ty suffix
           ; folded_suffix <- mkFoldrExpr elt_ty n_ty (Var c) (Var n) suffix'
           ; return (foldr (App . App (Var c)) folded_suffix prefix) }

dsExplicitList elt_ty (Just fln) xs
  = do { fln' <- dsExpr fln
       ; list <- dsExplicitList elt_ty Nothing xs
       ; dflags <- getDynFlags
       ; return (App (App fln' (mkIntExprInt dflags (length xs))) list) }
       
spanTail :: (a -> Bool) -> [a] -> ([a], [a])
spanTail f xs = (reverse rejected, reverse satisfying)
    where (satisfying, rejected) = span f $ reverse xs
    
dsArithSeq :: PostTcExpr -> (ArithSeqInfo Id) -> DsM CoreExpr
dsArithSeq expr (From from)
  = App <$> dsExpr expr <*> dsLExpr from
dsArithSeq expr (FromTo from to)
  = do dflags <- getDynFlags
       warnAboutEmptyEnumerations dflags from Nothing to
       expr' <- dsExpr expr
       from' <- dsLExpr from
       to'   <- dsLExpr to
       return $ mkApps expr' [from', to']
dsArithSeq expr (FromThen from thn)
  = mkApps <$> dsExpr expr <*> mapM dsLExpr [from, thn]
dsArithSeq expr (FromThenTo from thn to)
  = do dflags <- getDynFlags
       warnAboutEmptyEnumerations dflags from (Just thn) to
       expr' <- dsExpr expr
       from' <- dsLExpr from
       thn'  <- dsLExpr thn
       to'   <- dsLExpr to
       return $ mkApps expr' [from', thn', to']
\end{code}

Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in DsListComp).  Basically does the translation given in the
Haskell 98 report:

\begin{code}
dsDo :: [ExprLStmt Id] -> DsM CoreExpr
dsDo stmts
  = goL stmts
  where
    goL [] = panic "dsDo"
    goL (L loc stmt:lstmts) = putSrcSpanDs loc (go loc stmt lstmts)
  
    go _ (LastStmt body _) stmts
      = ASSERT( null stmts ) dsLExpr body
        -- The 'return' op isn't used for 'do' expressions

    go _ (BodyStmt rhs then_expr _ _) stmts
      = do { rhs2 <- dsLExpr rhs
           ; warnDiscardedDoBindings rhs (exprType rhs2) 
           ; then_expr2 <- dsExpr then_expr
           ; rest <- goL stmts
           ; return (mkApps then_expr2 [rhs2, rest]) }
    
    go _ (LetStmt binds) stmts
      = do { rest <- goL stmts
           ; dsLocalBinds binds rest }

    go _ (BindStmt pat rhs bind_op fail_op) stmts
      = do  { body     <- goL stmts
            ; rhs'     <- dsLExpr rhs
            ; bind_op' <- dsExpr bind_op
            ; var   <- selectSimpleMatchVarL pat
            ; let bind_ty = exprType bind_op'   -- rhs -> (pat -> res1) -> res2
                  res1_ty = funResultTy (funArgTy (funResultTy bind_ty))
            ; match <- matchSinglePat (Var var) (StmtCtxt DoExpr) pat
                                      res1_ty (cantFailMatchResult body)
            ; match_code <- handle_failure pat match fail_op
            ; return (mkApps bind_op' [rhs', Lam var match_code]) }
    
    go loc (RecStmt { recS_stmts = rec_stmts, recS_later_ids = later_ids
                    , recS_rec_ids = rec_ids, recS_ret_fn = return_op
                    , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op
                    , recS_rec_rets = rec_rets, recS_ret_ty = body_ty }) stmts
      = goL (new_bind_stmt : stmts)  -- rec_ids can be empty; eg  rec { print 'x' }
      where
        new_bind_stmt = L loc $ BindStmt (mkBigLHsPatTup later_pats)
                                         mfix_app bind_op 
                                         noSyntaxExpr  -- Tuple cannot fail

        tup_ids      = rec_ids ++ filterOut (`elem` rec_ids) later_ids
        tup_ty       = mkBigCoreTupTy (map idType tup_ids) -- Deals with singleton case
        rec_tup_pats = map nlVarPat tup_ids
        later_pats   = rec_tup_pats
        rets         = map noLoc rec_rets
        mfix_app     = nlHsApp (noLoc mfix_op) mfix_arg
        mfix_arg     = noLoc $ HsLam (MG { mg_alts = [mkSimpleMatch [mfix_pat] body]
                                         , mg_arg_tys = [tup_ty], mg_res_ty = body_ty })
        mfix_pat     = noLoc $ LazyPat $ mkBigLHsPatTup rec_tup_pats
        body         = noLoc $ HsDo DoExpr (rec_stmts ++ [ret_stmt]) body_ty
        ret_app      = nlHsApp (noLoc return_op) (mkBigLHsTup rets)
        ret_stmt     = noLoc $ mkLastStmt ret_app
                     -- This LastStmt will be desugared with dsDo, 
                     -- which ignores the return_op in the LastStmt,
                     -- so we must apply the return_op explicitly 

    go _ (ParStmt   {}) _ = panic "dsDo ParStmt"
    go _ (TransStmt {}) _ = panic "dsDo TransStmt"

handle_failure :: LPat Id -> MatchResult -> SyntaxExpr Id -> DsM CoreExpr
    -- In a do expression, pattern-match failure just calls
    -- the monadic 'fail' rather than throwing an exception
handle_failure pat match fail_op
  | matchCanFail match
  = do { fail_op' <- dsExpr fail_op
       ; dflags <- getDynFlags
       ; fail_msg <- mkStringExpr (mk_fail_msg dflags pat)
       ; extractMatchResult match (App fail_op' fail_msg) }
  | otherwise
  = extractMatchResult match (error "It can't fail")

mk_fail_msg :: DynFlags -> Located e -> String
mk_fail_msg dflags pat = "Pattern match failure in do expression at " ++ 
                         showPpr dflags (getLoc pat)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Errors and contexts}
%*                                                                      *
%************************************************************************

\begin{code}
-- Warn about certain types of values discarded in monadic bindings (#3263)
warnDiscardedDoBindings :: LHsExpr Id -> Type -> DsM ()
warnDiscardedDoBindings rhs rhs_ty
  | Just (m_ty, elt_ty) <- tcSplitAppTy_maybe rhs_ty
  = do {  -- Warn about discarding non-() things in 'monadic' binding
       ; warn_unused <- woptM Opt_WarnUnusedDoBind
       ; if warn_unused && not (isUnitTy elt_ty)
         then warnDs (unusedMonadBind rhs elt_ty)
         else 
         -- Warn about discarding m a things in 'monadic' binding of the same type,
         -- but only if we didn't already warn due to Opt_WarnUnusedDoBind
    do { warn_wrong <- woptM Opt_WarnWrongDoBind
       ; case tcSplitAppTy_maybe elt_ty of
           Just (elt_m_ty, _) | warn_wrong, m_ty `eqType` elt_m_ty
                              -> warnDs (wrongMonadBind rhs elt_ty)
           _ -> return () } }

  | otherwise   -- RHS does have type of form (m ty), which is wierd
  = return ()   -- but at lesat this warning is irrelevant

unusedMonadBind :: LHsExpr Id -> Type -> SDoc
unusedMonadBind rhs elt_ty
  = ptext (sLit "A do-notation statement discarded a result of type") <+> ppr elt_ty <> dot $$
    ptext (sLit "Suppress this warning by saying \"_ <- ") <> ppr rhs <> ptext (sLit "\",") $$
    ptext (sLit "or by using the flag -fno-warn-unused-do-bind")

wrongMonadBind :: LHsExpr Id -> Type -> SDoc
wrongMonadBind rhs elt_ty
  = ptext (sLit "A do-notation statement discarded a result of type") <+> ppr elt_ty <> dot $$
    ptext (sLit "Suppress this warning by saying \"_ <- ") <> ppr rhs <> ptext (sLit "\",") $$
    ptext (sLit "or by using the flag -fno-warn-wrong-do-bind")
\end{code}
