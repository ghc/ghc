%
% (c) The University of Glasgow 2006
%

\begin{code}
{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TcEnv(
        TyThing(..), TcTyThing(..), TcId,

        -- Instance environment, and InstInfo type
        InstInfo(..), iDFunId, pprInstInfoDetails,
        simpleInstInfoClsTy, simpleInstInfoTy, simpleInstInfoTyCon, 
        InstBindings(..),

        -- Global environment
        tcExtendGlobalEnv, tcExtendGlobalEnvImplicit, setGlobalTypeEnv,
        tcExtendGlobalValEnv,
        tcLookupLocatedGlobal, tcLookupGlobal, 
        tcLookupField, tcLookupTyCon, tcLookupClass, tcLookupDataCon,
        tcLookupConLike,
        tcLookupLocatedGlobalId, tcLookupLocatedTyCon,
        tcLookupLocatedClass, tcLookupInstance, tcLookupAxiom,
        
        -- Local environment
        tcExtendKindEnv, tcExtendKindEnv2,
        tcExtendTyVarEnv, tcExtendTyVarEnv2, 
        tcExtendLetEnv,
        tcExtendIdEnv, tcExtendIdEnv1, tcExtendIdEnv2, 
        tcExtendIdBndrs, tcExtendGhciIdEnv,

        tcLookup, tcLookupLocated, tcLookupLocalIds, 
        tcLookupId, tcLookupTyVar, 
        tcLookupLcl_maybe, 
        getScopedTyVarBinds, getInLocalScope,
        wrongThingErr, pprBinders,

        tcExtendRecEnv,         -- For knot-tying

        -- Rules
         tcExtendRules,

        -- Defaults
        tcGetDefaultTys,

        -- Global type variables
        tcGetGlobalTyVars, zapLclTypeEnv,

        -- Template Haskell stuff
        checkWellStaged, tcMetaTy, thLevel, 
        topIdLvl, isBrackStage,

        -- New Ids
        newLocalName, newDFunName, newFamInstTyConName, newFamInstAxiomName,
        mkStableIdFromString, mkStableIdFromName,
        mkWrapperName
  ) where

#include "HsVersions.h"

import HsSyn
import IfaceEnv
import TcRnMonad
import TcMType
import TcType
import TcIface  
import PrelNames
import TysWiredIn
import Id
import IdInfo( IdDetails(VanillaId) )
import Var
import VarSet
import RdrName
import InstEnv
import DataCon
import ConLike
import TyCon
import CoAxiom
import TypeRep
import Class
import Name
import NameEnv
import VarEnv
import HscTypes
import DynFlags
import SrcLoc
import BasicTypes hiding( SuccessFlag(..) )
import Module
import Outputable
import Encoding
import FastString
import ListSetOps
import Util
import Maybes( MaybeErr(..) )
import Data.IORef
import Data.List
\end{code}


%************************************************************************
%*                                                                      *
%*                      tcLookupGlobal                                  *
%*                                                                      *
%************************************************************************

Using the Located versions (eg. tcLookupLocatedGlobal) is preferred,
unless you know that the SrcSpan in the monad is already set to the
span of the Name.

\begin{code}
tcLookupLocatedGlobal :: Located Name -> TcM TyThing
-- c.f. IfaceEnvEnv.tcIfaceGlobal
tcLookupLocatedGlobal name
  = addLocM tcLookupGlobal name

tcLookupGlobal :: Name -> TcM TyThing
-- The Name is almost always an ExternalName, but not always
-- In GHCi, we may make command-line bindings (ghci> let x = True)
-- that bind a GlobalId, but with an InternalName
tcLookupGlobal name
  = do  {    -- Try local envt
          env <- getGblEnv
        ; case lookupNameEnv (tcg_type_env env) name of {
                Just thing -> return thing ;
                Nothing    ->

                -- Should it have been in the local envt?
          if nameIsLocalOrFrom (tcg_mod env) name
          then notFound name  -- Internal names can happen in GHCi
          else

           -- Try home package table and external package table
    do  { mb_thing <- tcLookupImported_maybe name
        ; case mb_thing of
            Succeeded thing -> return thing
            Failed msg      -> failWithTc msg
        }}}

tcLookupField :: Name -> TcM Id         -- Returns the selector Id
tcLookupField name
  = tcLookupId name     -- Note [Record field lookup]

{- Note [Record field lookup]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~
You might think we should have tcLookupGlobal here, since record fields
are always top level.  But consider
        f = e { f = True }
Then the renamer (which does not keep track of what is a record selector
and what is not) will rename the definition thus
        f_7 = e { f_7 = True }
Now the type checker will find f_7 in the *local* type environment, not
the global (imported) one. It's wrong, of course, but we want to report a tidy
error, not in TcEnv.notFound.  -}

tcLookupDataCon :: Name -> TcM DataCon
tcLookupDataCon name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike (RealDataCon con) -> return con
        _                          -> wrongThingErr "data constructor" (AGlobal thing) name

tcLookupConLike :: Name -> TcM ConLike
tcLookupConLike name = do
    thing <- tcLookupGlobal name
    case thing of
        AConLike cl -> return cl
        _           -> wrongThingErr "constructor-like thing" (AGlobal thing) name

tcLookupClass :: Name -> TcM Class
tcLookupClass name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc | Just cls <- tyConClass_maybe tc -> return cls
        _                                           -> wrongThingErr "class" (AGlobal thing) name

tcLookupTyCon :: Name -> TcM TyCon
tcLookupTyCon name = do
    thing <- tcLookupGlobal name
    case thing of
        ATyCon tc -> return tc
        _         -> wrongThingErr "type constructor" (AGlobal thing) name

tcLookupAxiom :: Name -> TcM (CoAxiom Branched)
tcLookupAxiom name = do
    thing <- tcLookupGlobal name
    case thing of
        ACoAxiom ax -> return ax
        _           -> wrongThingErr "axiom" (AGlobal thing) name

tcLookupLocatedGlobalId :: Located Name -> TcM Id
tcLookupLocatedGlobalId = addLocM tcLookupId

tcLookupLocatedClass :: Located Name -> TcM Class
tcLookupLocatedClass = addLocM tcLookupClass

tcLookupLocatedTyCon :: Located Name -> TcM TyCon
tcLookupLocatedTyCon = addLocM tcLookupTyCon

-- Find the instance that exactly matches a type class application.  The class arguments must be precisely
-- the same as in the instance declaration (modulo renaming).
--
tcLookupInstance :: Class -> [Type] -> TcM ClsInst
tcLookupInstance cls tys
  = do { instEnv <- tcGetInstEnvs
       ; case lookupUniqueInstEnv instEnv cls tys of
           Left err             -> failWithTc $ ptext (sLit "Couldn't match instance:") <+> err 
           Right (inst, tys) 
             | uniqueTyVars tys -> return inst
             | otherwise        -> failWithTc errNotExact
       }
  where
    errNotExact = ptext (sLit "Not an exact match (i.e., some variables get instantiated)")
    
    uniqueTyVars tys = all isTyVarTy tys && hasNoDups (map extractTyVar tys)
      where
        extractTyVar (TyVarTy tv) = tv
        extractTyVar _            = panic "TcEnv.tcLookupInstance: extractTyVar"
    
    tcGetInstEnvs = do { eps <- getEps; env <- getGblEnv;
                       ; return (eps_inst_env eps, tcg_inst_env env) 
                       }
\end{code}

\begin{code}
instance MonadThings (IOEnv (Env TcGblEnv TcLclEnv)) where
    lookupThing = tcLookupGlobal
\end{code}

%************************************************************************
%*                                                                      *
                Extending the global environment
%*                                                                      *
%************************************************************************


\begin{code}
setGlobalTypeEnv :: TcGblEnv -> TypeEnv -> TcM TcGblEnv
-- Use this to update the global type env 
-- It updates both  * the normal tcg_type_env field
--                  * the tcg_type_env_var field seen by interface files
setGlobalTypeEnv tcg_env new_type_env
  = do  {     -- Sync the type-envt variable seen by interface files
           writeMutVar (tcg_type_env_var tcg_env) new_type_env
         ; return (tcg_env { tcg_type_env = new_type_env }) }


tcExtendGlobalEnvImplicit :: [TyThing] -> TcM r -> TcM r
  -- Extend the global environment with some TyThings that can be obtained
  -- via implicitTyThings from other entities in the environment.  Examples
  -- are dfuns, famInstTyCons, data cons, etc.
  -- These TyThings are not added to tcg_tcs.
tcExtendGlobalEnvImplicit things thing_inside
   = do { tcg_env <- getGblEnv
        ; let ge'  = extendTypeEnvList (tcg_type_env tcg_env) things
        ; tcg_env' <- setGlobalTypeEnv tcg_env ge'
        ; setGblEnv tcg_env' thing_inside }

tcExtendGlobalEnv :: [TyThing] -> TcM r -> TcM r
  -- Given a mixture of Ids, TyCons, Classes, all defined in the
  -- module being compiled, extend the global environment
tcExtendGlobalEnv things thing_inside
  = do { env <- getGblEnv
       ; let env' = env { tcg_tcs = [tc | ATyCon tc <- things] ++ tcg_tcs env,
                          tcg_patsyns = [ps | AConLike (PatSynCon ps) <- things] ++ tcg_patsyns env }
       ; setGblEnv env' $
            tcExtendGlobalEnvImplicit things thing_inside
       }

tcExtendGlobalValEnv :: [Id] -> TcM a -> TcM a
  -- Same deal as tcExtendGlobalEnv, but for Ids
tcExtendGlobalValEnv ids thing_inside 
  = tcExtendGlobalEnvImplicit [AnId id | id <- ids] thing_inside

tcExtendRecEnv :: [(Name,TyThing)] -> TcM r -> TcM r
-- Extend the global environments for the type/class knot tying game
-- Just like tcExtendGlobalEnv, except the argument is a list of pairs
tcExtendRecEnv gbl_stuff thing_inside
 = do  { tcg_env <- getGblEnv
       ; let ge' = extendNameEnvList (tcg_type_env tcg_env) gbl_stuff 
       ; tcg_env' <- setGlobalTypeEnv tcg_env ge'
       ; setGblEnv tcg_env' thing_inside }
\end{code}


%************************************************************************
%*                                                                      *
\subsection{The local environment}
%*                                                                      *
%************************************************************************

\begin{code}
tcLookupLocated :: Located Name -> TcM TcTyThing
tcLookupLocated = addLocM tcLookup

tcLookupLcl_maybe :: Name -> TcM (Maybe TcTyThing)
tcLookupLcl_maybe name
  = do { local_env <- getLclTypeEnv
       ; return (lookupNameEnv local_env name) }

tcLookup :: Name -> TcM TcTyThing
tcLookup name = do
    local_env <- getLclTypeEnv
    case lookupNameEnv local_env name of
        Just thing -> return thing
        Nothing    -> AGlobal <$> tcLookupGlobal name

tcLookupTyVar :: Name -> TcM TcTyVar
tcLookupTyVar name
  = do { thing <- tcLookup name
       ; case thing of
           ATyVar _ tv -> return tv
           _           -> pprPanic "tcLookupTyVar" (ppr name) }

tcLookupId :: Name -> TcM Id
-- Used when we aren't interested in the binding level, nor refinement. 
-- The "no refinement" part means that we return the un-refined Id regardless
-- 
-- The Id is never a DataCon. (Why does that matter? see TcExpr.tcId)
tcLookupId name = do
    thing <- tcLookup name
    case thing of
        ATcId { tct_id = id} -> return id
        AGlobal (AnId id)    -> return id
        _                    -> pprPanic "tcLookupId" (ppr name)

tcLookupLocalIds :: [Name] -> TcM [TcId]
-- We expect the variables to all be bound, and all at
-- the same level as the lookup.  Only used in one place...
tcLookupLocalIds ns 
  = do { env <- getLclEnv
       ; return (map (lookup (tcl_env env)) ns) }
  where
    lookup lenv name 
        = case lookupNameEnv lenv name of
                Just (ATcId { tct_id = id }) ->  id
                _ -> pprPanic "tcLookupLocalIds" (ppr name)

getInLocalScope :: TcM (Name -> Bool)
  -- Ids only
getInLocalScope = do { lcl_env <- getLclTypeEnv
                     ; return (`elemNameEnv` lcl_env) }
\end{code}

\begin{code}
tcExtendKindEnv2 :: [(Name, TcTyThing)] -> TcM r -> TcM r
-- Used only during kind checking, for TcThings that are
--      AThing or APromotionErr
-- No need to update the global tyvars, or tcl_th_bndrs, or tcl_rdr
tcExtendKindEnv2 things thing_inside
  = updLclEnv upd_env thing_inside
  where
    upd_env env = env { tcl_env = extendNameEnvList (tcl_env env) things }

tcExtendKindEnv :: [(Name, TcKind)] -> TcM r -> TcM r
tcExtendKindEnv name_kind_prs
  = tcExtendKindEnv2 [(n, AThing k) | (n,k) <- name_kind_prs]

-----------------------
-- Scoped type and kind variables
tcExtendTyVarEnv :: [TyVar] -> TcM r -> TcM r
tcExtendTyVarEnv tvs thing_inside
  = tcExtendTyVarEnv2 [(tyVarName tv, tv) | tv <- tvs] thing_inside

tcExtendTyVarEnv2 :: [(Name,TcTyVar)] -> TcM r -> TcM r
tcExtendTyVarEnv2 binds thing_inside
  = do { stage <- getStage
       ; tc_extend_local_env (NotTopLevel, thLevel stage)
                    [(name, ATyVar name tv) | (name, tv) <- binds] $
         do { env <- getLclEnv
            ; let env' = env { tcl_tidy = add_tidy_tvs (tcl_tidy env) }
            ; setLclEnv env' thing_inside }}
  where
    add_tidy_tvs env = foldl add env binds

    -- We initialise the "tidy-env", used for tidying types before printing,
    -- by building a reverse map from the in-scope type variables to the
    -- OccName that the programmer originally used for them
    add :: TidyEnv -> (Name, TcTyVar) -> TidyEnv
    add (env,subst) (name, tyvar)
        = case tidyOccName env (nameOccName name) of
            (env', occ') ->  (env', extendVarEnv subst tyvar tyvar')
                where
                  tyvar' = setTyVarName tyvar name'
                  name'  = tidyNameOcc name occ'

getScopedTyVarBinds :: TcM [(Name, TcTyVar)]
getScopedTyVarBinds
  = do  { lcl_env <- getLclEnv
        ; return [(name, tv) | ATyVar name tv <- nameEnvElts (tcl_env lcl_env)] }
\end{code}

Note [Initialising the type environment for GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcExtendGhciIdEnv extends the local type environemnt with GHCi
identifiers (from ic_tythings), bound earlier in the interaction.
They may have free type variables (RuntimeUnk things), and if we don't
register these free TyVars as global TyVars then the typechecker will
try to quantify over them and fall over in zonkQuantifiedTyVar.
So we must add any free TyVars to the typechecker's global
TyVar set.  That is most conveniently done here, using the local function
tcExtendLocalTypeEnv.

Note especially that

 * tcExtendGhciIdEnv extends the local type env, tcl_env
   That's important because some are not closed (ie have free tyvars)
   and the compiler assumes that the global type env (tcg_type_env) has
   no free tyvars.  Actually, only ones with Internal names can be non-closed
   so we jsut add those

 * The tct_closed flag depends on whether the thing has free (RuntimeUnk)
   type variables

 * It will also does tcExtendGlobalTyVars; this is important
   because of those RuntimeUnk variables

 * It does not extend the local RdrEnv (tcl_rdr), because the things are
   already in the GlobalRdrEnv.  Extending the local RdrEnv isn't terrible,
   but it means there is an entry for the same Name in both global and local
   RdrEnvs, and that lead to duplicate "perhpas you meant..." suggestions
   (e.g. T5564).

   We don't bother with the tcl_th_bndrs environment either.

 * NB: all these TcTyThings will be in the global type envt (tcg_type_env) as
       well.  We are just shadowing them here to deal with the global tyvar
       stuff.  That's why we can simply drop the External-Name ones; they
       will be found in the global envt

\begin{code}
tcExtendGhciIdEnv :: [TyThing] -> TcM a -> TcM a
-- Used to bind Ids for GHCi identifiers bound earlier in the user interaction
-- See Note [Initialising the type environment for GHCi]
tcExtendGhciIdEnv ids thing_inside
  = do { lcl_env <- tcExtendLocalTypeEnv tc_ty_things
       ; setLclEnv lcl_env thing_inside }
  where
    tc_ty_things =  [ (name, ATcId { tct_id     = id
                                   , tct_closed = is_top id })
                    | AnId id <- ids
                    , let name = idName id
                    , isInternalName name ]
    is_top id | isEmptyVarSet (tyVarsOfType (idType id)) = TopLevel
              | otherwise                                = NotTopLevel

tcExtendLetEnv :: TopLevelFlag -> TopLevelFlag -> [TcId] -> TcM a -> TcM a
-- Used for both top-level value bindings and and nested let/where-bindings
tcExtendLetEnv top_lvl closed ids thing_inside
  = do  { stage <- getStage
        ; tc_extend_local_env (top_lvl, thLevel stage)
                              [ (idName id, ATcId { tct_id = id
                                                  , tct_closed = closed })
                              | id <- ids] $
          tcExtendIdBndrs [TcIdBndr id top_lvl | id <- ids] thing_inside }

tcExtendIdEnv :: [TcId] -> TcM a -> TcM a
tcExtendIdEnv ids thing_inside 
  = tcExtendIdEnv2 [(idName id, id) | id <- ids] $
    tcExtendIdBndrs [TcIdBndr id NotTopLevel | id <- ids] 
    thing_inside

tcExtendIdEnv1 :: Name -> TcId -> TcM a -> TcM a
tcExtendIdEnv1 name id thing_inside 
  = tcExtendIdEnv2 [(name,id)] $
    tcExtendIdBndrs [TcIdBndr id NotTopLevel]
    thing_inside

tcExtendIdEnv2 :: [(Name,TcId)] -> TcM a -> TcM a
-- Do *not* extend the tcl_bndrs stack
-- The tct_closed flag really doesn't matter
-- Invariant: the TcIds are fully zonked (see tcExtendIdEnv above)
tcExtendIdEnv2 names_w_ids thing_inside
  = do  { stage <- getStage
        ; tc_extend_local_env (NotTopLevel, thLevel stage)
                              [ (name, ATcId { tct_id = id 
                                             , tct_closed = NotTopLevel })
                              | (name,id) <- names_w_ids] $
          thing_inside }

tcExtendIdBndrs :: [TcIdBinder] -> TcM a -> TcM a
tcExtendIdBndrs bndrs = updLclEnv (\env -> env { tcl_bndrs = bndrs ++ tcl_bndrs env })

tc_extend_local_env :: (TopLevelFlag, ThLevel) -> [(Name, TcTyThing)] -> TcM a -> TcM a
-- Precondition: the argument list extra_env has TcTyThings
--               that ATcId or ATyVar, but nothing else
--
-- Invariant: the ATcIds are fully zonked. Reasons:
--      (a) The kinds of the forall'd type variables are defaulted
--          (see Kind.defaultKind, done in zonkQuantifiedTyVar)
--      (b) There are no via-Indirect occurrences of the bound variables
--          in the types, because instantiation does not look through such things
--      (c) The call to tyVarsOfTypes is ok without looking through refs

tc_extend_local_env thlvl extra_env thing_inside
  = do  { traceTc "env2" (ppr extra_env)
        ; env1 <- tcExtendLocalTypeEnv extra_env
        ; let env2 = extend_local_env thlvl extra_env env1
        ; setLclEnv env2 thing_inside }
  where
    extend_local_env :: (TopLevelFlag, ThLevel) -> [(Name, TcTyThing)] -> TcLclEnv -> TcLclEnv
    -- Extend the local LocalRdrEnv and Template Haskell staging env simultaneously
    -- Reason for extending LocalRdrEnv: after running a TH splice we need
    -- to do renaming.
    extend_local_env thlvl pairs env@(TcLclEnv { tcl_rdr = rdr_env
                                               , tcl_th_bndrs = th_bndrs })
      = env { tcl_rdr      = extendLocalRdrEnvList rdr_env
                                [ n | (n, _) <- pairs, isInternalName n ]
                                -- The LocalRdrEnv contains only non-top-level names
                                -- (GlobalRdrEnv handles the top level)
            , tcl_th_bndrs = extendNameEnvList th_bndrs  -- We only track Ids in tcl_th_bndrs
                                 [(n, thlvl) | (n, ATcId {}) <- pairs] }

tcExtendLocalTypeEnv :: [(Name, TcTyThing)] -> TcM TcLclEnv
tcExtendLocalTypeEnv tc_ty_things
  | isEmptyVarSet extra_tvs
  = do { lcl_env@(TcLclEnv { tcl_env = lcl_type_env }) <- getLclEnv
       ; return (lcl_env { tcl_env = extendNameEnvList lcl_type_env tc_ty_things } ) }
  | otherwise
  = do { lcl_env@(TcLclEnv { tcl_env = lcl_type_env }) <- getLclEnv
       ; global_tvs <- readMutVar (tcl_tyvars lcl_env)
       ; new_g_var  <- newMutVar (global_tvs `unionVarSet` extra_tvs)
       ; return (lcl_env { tcl_tyvars = new_g_var
                         , tcl_env = extendNameEnvList lcl_type_env tc_ty_things } ) }
  where
    extra_tvs = foldr get_tvs emptyVarSet tc_ty_things

    get_tvs (_, ATcId { tct_id = id, tct_closed = closed }) tvs
      = case closed of
          TopLevel    -> ASSERT2( isEmptyVarSet (tyVarsOfType (idType id)), ppr id $$ ppr (idType id) )
                         tvs
          NotTopLevel -> tvs `unionVarSet` tyVarsOfType (idType id)

    get_tvs (_, ATyVar _ tv) tvs          -- See Note [Global TyVars]
      = tvs `unionVarSet` tyVarsOfType (tyVarKind tv) `extendVarSet` tv

    get_tvs (_, AThing k) tvs = tvs `unionVarSet` tyVarsOfType k

    get_tvs (_, AGlobal {})       tvs = tvs
    get_tvs (_, APromotionErr {}) tvs = tvs

        -- Note [Global TyVars]
        -- It's important to add the in-scope tyvars to the global tyvar set
        -- as well.  Consider
        --      f (_::r) = let g y = y::r in ...
        -- Here, g mustn't be generalised.  This is also important during
        -- class and instance decls, when we mustn't generalise the class tyvars
        -- when typechecking the methods.
        --
        -- Nor must we generalise g over any kind variables free in r's kind

zapLclTypeEnv :: TcM a -> TcM a
zapLclTypeEnv thing_inside
  = do { tvs_var <- newTcRef emptyVarSet
       ; let upd env = env { tcl_env = emptyNameEnv
                           , tcl_rdr = emptyLocalRdrEnv
                           , tcl_tyvars = tvs_var }
       ; updLclEnv upd thing_inside }
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Rules}
%*                                                                      *
%************************************************************************

\begin{code}
tcExtendRules :: [LRuleDecl Id] -> TcM a -> TcM a
        -- Just pop the new rules into the EPS and envt resp
        -- All the rules come from an interface file, not source
        -- Nevertheless, some may be for this module, if we read
        -- its interface instead of its source code
tcExtendRules lcl_rules thing_inside
 = do { env <- getGblEnv
      ; let
          env' = env { tcg_rules = lcl_rules ++ tcg_rules env }
      ; setGblEnv env' thing_inside }
\end{code}


%************************************************************************
%*                                                                      *
                Meta level
%*                                                                      *
%************************************************************************

\begin{code}
checkWellStaged :: SDoc         -- What the stage check is for
                -> ThLevel      -- Binding level (increases inside brackets)
                -> ThLevel      -- Use stage
                -> TcM ()       -- Fail if badly staged, adding an error
checkWellStaged pp_thing bind_lvl use_lvl
  | use_lvl >= bind_lvl         -- OK! Used later than bound
  = return ()                   -- E.g.  \x -> [| $(f x) |]

  | bind_lvl == outerLevel      -- GHC restriction on top level splices
  = stageRestrictionError pp_thing

  | otherwise                   -- Badly staged
  = failWithTc $                -- E.g.  \x -> $(f x)
    ptext (sLit "Stage error:") <+> pp_thing <+> 
        hsep   [ptext (sLit "is bound at stage") <+> ppr bind_lvl,
                ptext (sLit "but used at stage") <+> ppr use_lvl]

stageRestrictionError :: SDoc -> TcM a
stageRestrictionError pp_thing
  = failWithTc $ 
    sep [ ptext (sLit "GHC stage restriction:")
        , nest 2 (vcat [ pp_thing <+> ptext (sLit "is used in a top-level splice or annotation,")
                       , ptext (sLit "and must be imported, not defined locally")])]

topIdLvl :: Id -> ThLevel
-- Globals may either be imported, or may be from an earlier "chunk" 
-- (separated by declaration splices) of this module.  The former
--  *can* be used inside a top-level splice, but the latter cannot.
-- Hence we give the former impLevel, but the latter topLevel
-- E.g. this is bad:
--      x = [| foo |]
--      $( f x )
-- By the time we are prcessing the $(f x), the binding for "x" 
-- will be in the global env, not the local one.
topIdLvl id | isLocalId id = outerLevel
            | otherwise    = impLevel

tcMetaTy :: Name -> TcM Type
-- Given the name of a Template Haskell data type, 
-- return the type
-- E.g. given the name "Expr" return the type "Expr"
tcMetaTy tc_name = do
    t <- tcLookupTyCon tc_name
    return (mkTyConApp t [])

isBrackStage :: ThStage -> Bool
isBrackStage (Brack {}) = True
isBrackStage _other     = False
\end{code}


%************************************************************************
%*                                                                      *
                 getDefaultTys                                                                          
%*                                                                      *
%************************************************************************

\begin{code}
tcGetDefaultTys :: TcM ([Type], -- Default types
                        (Bool,  -- True <=> Use overloaded strings
                         Bool)) -- True <=> Use extended defaulting rules
tcGetDefaultTys
  = do  { dflags <- getDynFlags
        ; let ovl_strings = xopt Opt_OverloadedStrings dflags
              extended_defaults = xopt Opt_ExtendedDefaultRules dflags
                                        -- See also Trac #1974 
              flags = (ovl_strings, extended_defaults)
    
        ; mb_defaults <- getDeclaredDefaultTys
        ; case mb_defaults of {
           Just tys -> return (tys, flags) ;
                                -- User-supplied defaults
           Nothing  -> do

        -- No use-supplied default
        -- Use [Integer, Double], plus modifications
        { integer_ty <- tcMetaTy integerTyConName
        ; checkWiredInTyCon doubleTyCon
        ; string_ty <- tcMetaTy stringTyConName
        ; let deflt_tys = opt_deflt extended_defaults unitTy  -- Note [Default unitTy]
                          ++ [integer_ty, doubleTy]
                          ++ opt_deflt ovl_strings string_ty
        ; return (deflt_tys, flags) } } }
  where
    opt_deflt True  ty = [ty]
    opt_deflt False _  = []
\end{code}

Note [Default unitTy]
~~~~~~~~~~~~~~~~~~~~~
In interative mode (or with -XExtendedDefaultRules) we add () as the first type we
try when defaulting.  This has very little real impact, except in the following case.
Consider: 
        Text.Printf.printf "hello"
This has type (forall a. IO a); it prints "hello", and returns 'undefined'.  We don't
want the GHCi repl loop to try to print that 'undefined'.  The neatest thing is to
default the 'a' to (), rather than to Integer (which is what would otherwise happen;
and then GHCi doesn't attempt to print the ().  So in interactive mode, we add
() to the list of defaulting types.  See Trac #1200.


%************************************************************************
%*                                                                      *
\subsection{The InstInfo type}
%*                                                                      *
%************************************************************************

The InstInfo type summarises the information in an instance declaration

    instance c => k (t tvs) where b

It is used just for *local* instance decls (not ones from interface files).
But local instance decls includes
        - derived ones
        - generic ones
as well as explicit user written ones.

\begin{code}
data InstInfo a
  = InstInfo {
      iSpec   :: ClsInst,        -- Includes the dfun id.  Its forall'd type
      iBinds  :: InstBindings a   -- variables scope over the stuff in InstBindings!
    }

iDFunId :: InstInfo a -> DFunId
iDFunId info = instanceDFunId (iSpec info)

data InstBindings a
  = InstBindings
      { ib_binds :: (LHsBinds a)  -- Bindings for the instance methods
      , ib_pragmas :: [LSig a]    -- User pragmas recorded for generating 
                                  -- specialised instances
      , ib_extensions :: [ExtensionFlag] -- any extra extensions that should
                                         -- be enabled when type-checking this
                                         -- instance; needed for
                                         -- GeneralizedNewtypeDeriving
                      
      , ib_standalone_deriving :: Bool
           -- True <=> This code came from a standalone deriving clause
           --          Used only to improve error messages
      }

instance OutputableBndr a => Outputable (InstInfo a) where
    ppr = pprInstInfoDetails

pprInstInfoDetails :: OutputableBndr a => InstInfo a -> SDoc
pprInstInfoDetails info 
   = hang (pprInstanceHdr (iSpec info) <+> ptext (sLit "where"))
        2 (details (iBinds info))
  where
    details (InstBindings { ib_binds = b }) = pprLHsBinds b

simpleInstInfoClsTy :: InstInfo a -> (Class, Type)
simpleInstInfoClsTy info = case instanceHead (iSpec info) of
                           (_, cls, [ty]) -> (cls, ty)
                           _ -> panic "simpleInstInfoClsTy"

simpleInstInfoTy :: InstInfo a -> Type
simpleInstInfoTy info = snd (simpleInstInfoClsTy info)

simpleInstInfoTyCon :: InstInfo a -> TyCon
  -- Gets the type constructor for a simple instance declaration,
  -- i.e. one of the form       instance (...) => C (T a b c) where ...
simpleInstInfoTyCon inst = tcTyConAppTyCon (simpleInstInfoTy inst)
\end{code}

Make a name for the dict fun for an instance decl.  It's an *external*
name, like otber top-level names, and hence must be made with newGlobalBinder.

\begin{code}
newDFunName :: Class -> [Type] -> SrcSpan -> TcM Name
newDFunName clas tys loc
  = do  { is_boot <- tcIsHsBoot
        ; mod     <- getModule
        ; let info_string = occNameString (getOccName clas) ++ 
                            concatMap (occNameString.getDFunTyKey) tys
        ; dfun_occ <- chooseUniqueOccTc (mkDFunOcc info_string is_boot)
        ; newGlobalBinder mod dfun_occ loc }
\end{code}

Make a name for the representation tycon of a family instance.  It's an
*external* name, like other top-level names, and hence must be made with
newGlobalBinder.

\begin{code}
newFamInstTyConName :: Located Name -> [Type] -> TcM Name
newFamInstTyConName (L loc name) tys = mk_fam_inst_name id loc name [tys]

newFamInstAxiomName :: SrcSpan -> Name -> [CoAxBranch] -> TcM Name
newFamInstAxiomName loc name branches
  = mk_fam_inst_name mkInstTyCoOcc loc name (map coAxBranchLHS branches)

mk_fam_inst_name :: (OccName -> OccName) -> SrcSpan -> Name -> [[Type]] -> TcM Name
mk_fam_inst_name adaptOcc loc tc_name tyss
  = do  { mod   <- getModule
        ; let info_string = occNameString (getOccName tc_name) ++ 
                            intercalate "|" ty_strings
        ; occ   <- chooseUniqueOccTc (mkInstTyTcOcc info_string)
        ; newGlobalBinder mod (adaptOcc occ) loc }
  where
    ty_strings = map (concatMap (occNameString . getDFunTyKey)) tyss
\end{code}

Stable names used for foreign exports and annotations.
For stable names, the name must be unique (see #1533).  If the
same thing has several stable Ids based on it, the
top-level bindings generated must not have the same name.
Hence we create an External name (doesn't change), and we
append a Unique to the string right here.

\begin{code}
mkStableIdFromString :: String -> Type -> SrcSpan -> (OccName -> OccName) -> TcM TcId
mkStableIdFromString str sig_ty loc occ_wrapper = do
    uniq <- newUnique
    mod <- getModule
    name <- mkWrapperName "stable" str
    let occ = mkVarOccFS name :: OccName
        gnm = mkExternalName uniq mod (occ_wrapper occ) loc :: Name
        id  = mkExportedLocalId VanillaId gnm sig_ty :: Id
    return id

mkStableIdFromName :: Name -> Type -> SrcSpan -> (OccName -> OccName) -> TcM TcId
mkStableIdFromName nm = mkStableIdFromString (getOccString nm)
\end{code}

\begin{code}
mkWrapperName :: (MonadIO m, HasDynFlags m, HasModule m)
              => String -> String -> m FastString
mkWrapperName what nameBase
    = do dflags <- getDynFlags
         thisMod <- getModule
         let -- Note [Generating fresh names for ccall wrapper]
             wrapperRef = nextWrapperNum dflags
             pkg = packageIdString  (modulePackageId thisMod)
             mod = moduleNameString (moduleName      thisMod)
         wrapperNum <- liftIO $ atomicModifyIORef wrapperRef $ \mod_env ->
             let num = lookupWithDefaultModuleEnv mod_env 0 thisMod
                 mod_env' = extendModuleEnv mod_env thisMod (num+1)
             in (mod_env', num)
         let components = [what, show wrapperNum, pkg, mod, nameBase]
         return $ mkFastString $ zEncodeString $ intercalate ":" components

{-
Note [Generating fresh names for FFI wrappers]

We used to use a unique, rather than nextWrapperNum, to distinguish
between FFI wrapper functions. However, the wrapper names that we
generate are external names. This means that if a call to them ends up
in an unfolding, then we can't alpha-rename them, and thus if the
unique randomly changes from one compile to another then we get a
spurious ABI change (#4012).

The wrapper counter has to be per-module, not global, so that the number we end
up using is not dependent on the modules compiled before the current one.
-}
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Errors}
%*                                                                      *
%************************************************************************

\begin{code}
pprBinders :: [Name] -> SDoc
-- Used in error messages
-- Use quotes for a single one; they look a bit "busy" for several
pprBinders [bndr] = quotes (ppr bndr)
pprBinders bndrs  = pprWithCommas ppr bndrs

notFound :: Name -> TcM TyThing
notFound name 
  = do { lcl_env <- getLclEnv
       ; let stage = tcl_th_ctxt lcl_env
       ; case stage of   -- See Note [Out of scope might be a staging error]
           Splice {} -> stageRestrictionError (quotes (ppr name))
           _ -> failWithTc $
                vcat[ptext (sLit "GHC internal error:") <+> quotes (ppr name) <+> 
                     ptext (sLit "is not in scope during type checking, but it passed the renamer"),
                     ptext (sLit "tcl_env of environment:") <+> ppr (tcl_env lcl_env)]
                       -- Take case: printing the whole gbl env can
                       -- cause an infnite loop, in the case where we
                       -- are in the middle of a recursive TyCon/Class group;
                       -- so let's just not print it!  Getting a loop here is
                       -- very unhelpful, because it hides one compiler bug with another
       }

wrongThingErr :: String -> TcTyThing -> Name -> TcM a
wrongThingErr expected thing name
  = failWithTc (pprTcTyThingCategory thing <+> quotes (ppr name) <+> 
                ptext (sLit "used as a") <+> text expected)
\end{code}

Note [Out of scope might be a staging error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  x = 3
  data T = MkT $(foo x)

This is really a staging error, because we can't run code involving 'x'.
But in fact the type checker processes types first, so 'x' won't even be
in the type envt when we look for it in $(foo x).  So inside splices we
report something missing from the type env as a staging error.
See Trac #5752 and #5795.
