{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module TcTypeable(mkTypeableBinds) where


import BasicTypes ( Boxity(..), neverInlinePragma )
import TcBinds( addTypecheckedBinds )
import IfaceEnv( newGlobalBinder )
import TyCoRep( Type(..), TyLit(..) )
import TcEnv
import TcEvidence ( mkWpTyApps )
import TcRnMonad
import HscTypes ( lookupId )
import PrelNames
import TysPrim ( primTyCons )
import TysWiredIn ( tupleTyCon, sumTyCon, runtimeRepTyCon
                  , vecCountTyCon, vecElemTyCon
                  , nilDataCon, consDataCon )
import Name
import Id
import Type
import Kind ( isTYPEApp )
import TyCon
import DataCon
import Name ( getOccName )
import Module
import HsSyn
import DynFlags
import Bag
import Var ( TyVarBndr(..) )
import TrieMap
import Constants
import Fingerprint(Fingerprint(..), fingerprintString, fingerprintFingerprints)
import Outputable
import FastString ( FastString, mkFastString, fsLit )

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Maybe ( isJust )
import Data.Word( Word64 )

{- Note [Grand plan for Typeable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The overall plan is this:

1. Generate a binding for each module p:M
   (done in TcTypeable by mkModIdBindings)
       M.$trModule :: GHC.Types.Module
       M.$trModule = Module "p" "M"
   ("tr" is short for "type representation"; see GHC.Types)

   We might want to add the filename too.
   This can be used for the lightweight stack-tracing stuff too

   Record the Name M.$trModule in the tcg_tr_module field of TcGblEnv

2. Generate a binding for every data type declaration T in module M,
       M.$tcT :: GHC.Types.TyCon
       M.$tcT = TyCon ...fingerprint info...
                      $trModule
                      "T"
                      0#
                      kind_rep

   Here 0# is the number of arguments expected by the tycon to fully determine
   its kind. kind_rep is a value of type GHC.Types.KindRep, which gives a
   recipe for computing the kind of an instantiation of the tycon (see
   Note [Representing TyCon kinds: KindRep] later in this file for details).

   We define (in TyCon)

        type TyConRepName = Name

   to use for these M.$tcT "tycon rep names". Note that these must be
   treated as "never exported" names by Backpack (see
   Note [Handling never-exported TyThings under Backpack]). Consequently
   they get slightly special treatment in RnModIface.rnIfaceDecl.

3. Record the TyConRepName in T's TyCon, including for promoted
   data and type constructors, and kinds like * and #.

   The TyConRepName is not an "implicit Id".  It's more like a record
   selector: the TyCon knows its name but you have to go to the
   interface file to find its type, value, etc

4. Solve Typeable constraints.  This is done by a custom Typeable solver,
   currently in TcInteract, that use M.$tcT so solve (Typeable T).

There are many wrinkles:

* The timing of when we produce this bindings is rather important: they must be
  defined after the rest of the module has been typechecked since we need to be
  able to lookup Module and TyCon in the type environment and we may be
  currently compiling GHC.Types (where they are defined).

* GHC.Prim doesn't have any associated object code, so we need to put the
  representations for types defined in this module elsewhere. We chose this
  place to be GHC.Types. TcTypeable.mkPrimTypeableBinds is responsible for
  injecting the bindings for the GHC.Prim representions when compiling
  GHC.Types.

* TyCon.tyConRepModOcc is responsible for determining where to find
  the representation binding for a given type. This is where we handle
  the special case for GHC.Prim.

* To save space and reduce dependencies, we need use quite low-level
  representations for TyCon and Module.  See GHC.Types
  Note [Runtime representation of modules and tycons]

* The KindReps can unfortunately get quite large. Moreover, the simplifier will
  float out various pieces of them, resulting in numerous top-level bindings.
  Consequently we mark the KindRep bindings as noinline, ensuring that the
  float-outs don't make it into the interface file. This is important since
  there is generally little benefit to inlining KindReps and they would
  otherwise strongly affect compiler performance.

* In general there are lots of things of kind *, * -> *, and * -> * -> *. To
  reduce the number of bindings we need to produce, we generate their KindReps
  once in GHC.Types. These are referred to as "built-in" KindReps below.

* Even though KindReps aren't inlined, this scheme still has more of an effect on
  compilation time than I'd like. This is especially true in the case of
  families of type constructors (e.g. tuples and unboxed sums). The problem is
  particularly bad in the case of sums, since each arity-N tycon brings with it
  N promoted datacons, each with a KindRep whose size also scales with N.
  Consequently we currently simply don't allow sums to be Typeable.

  In general we might consider moving some or all of this generation logic back
  to the solver since the performance hit we take in doing this at
  type-definition time is non-trivial and Typeable isn't very widely used. This
  is discussed in #13261.

-}

-- | Generate the Typeable bindings for a module. This is the only
-- entry-point of this module and is invoked by the typechecker driver in
-- 'tcRnSrcDecls'.
--
-- See Note [Grand plan for Typeable] in TcTypeable.
mkTypeableBinds :: TcM TcGblEnv
mkTypeableBinds
  = do { -- Create a binding for $trModule.
         -- Do this before processing any data type declarations,
         -- which need tcg_tr_module to be initialised
       ; tcg_env <- mkModIdBindings
         -- Now we can generate the TyCon representations...
         -- First we handle the primitive TyCons if we are compiling GHC.Types
       ; (tcg_env, prim_todos) <- setGblEnv tcg_env mkPrimTypeableTodos

         -- Then we produce bindings for the user-defined types in this module.
       ; setGblEnv tcg_env $
    do { mod <- getModule
       ; let tycons = filter needs_typeable_binds (tcg_tcs tcg_env)
             mod_id = case tcg_tr_module tcg_env of  -- Should be set by now
                        Just mod_id -> mod_id
                        Nothing     -> pprPanic "tcMkTypeableBinds" (ppr tycons)
       ; traceTc "mkTypeableBinds" (ppr tycons)
       ; this_mod_todos <- todoForTyCons mod mod_id tycons
       ; mkTypeRepTodoBinds (this_mod_todos : prim_todos)
       } }
  where
    needs_typeable_binds tc
      | tc `elem` [runtimeRepTyCon, vecCountTyCon, vecElemTyCon]
      = False
      | otherwise =
          (not (isFamInstTyCon tc) && isAlgTyCon tc)
       || isDataFamilyTyCon tc
       || isClassTyCon tc


{- *********************************************************************
*                                                                      *
            Building top-level binding for $trModule
*                                                                      *
********************************************************************* -}

mkModIdBindings :: TcM TcGblEnv
mkModIdBindings
  = do { mod <- getModule
       ; loc <- getSrcSpanM
       ; mod_nm        <- newGlobalBinder mod (mkVarOcc "$trModule") loc
       ; trModuleTyCon <- tcLookupTyCon trModuleTyConName
       ; let mod_id = mkExportedVanillaId mod_nm (mkTyConApp trModuleTyCon [])
       ; mod_bind      <- mkVarBind mod_id <$> mkModIdRHS mod

       ; tcg_env <- tcExtendGlobalValEnv [mod_id] getGblEnv
       ; return (tcg_env { tcg_tr_module = Just mod_id }
                 `addTypecheckedBinds` [unitBag mod_bind]) }

mkModIdRHS :: Module -> TcM (LHsExpr GhcTc)
mkModIdRHS mod
  = do { trModuleDataCon <- tcLookupDataCon trModuleDataConName
       ; trNameLit <- mkTrNameLit
       ; return $ nlHsDataCon trModuleDataCon
                  `nlHsApp` trNameLit (unitIdFS (moduleUnitId mod))
                  `nlHsApp` trNameLit (moduleNameFS (moduleName mod))
       }

{- *********************************************************************
*                                                                      *
                Building type-representation bindings
*                                                                      *
********************************************************************* -}

-- | Information we need about a 'TyCon' to generate its representation. We
-- carry the 'Id' in order to share it between the generation of the @TyCon@ and
-- @KindRep@ bindings.
data TypeableTyCon
    = TypeableTyCon
      { tycon        :: !TyCon
      , tycon_rep_id :: !Id
      }

-- | A group of 'TyCon's in need of type-rep bindings.
data TypeRepTodo
    = TypeRepTodo
      { mod_rep_expr    :: LHsExpr GhcTc    -- ^ Module's typerep binding
      , pkg_fingerprint :: !Fingerprint     -- ^ Package name fingerprint
      , mod_fingerprint :: !Fingerprint     -- ^ Module name fingerprint
      , todo_tycons     :: [TypeableTyCon]
        -- ^ The 'TyCon's in need of bindings kinds
      }
    | ExportedKindRepsTodo [(Kind, Id)]
      -- ^ Build exported 'KindRep' bindings for the given set of kinds.

todoForTyCons :: Module -> Id -> [TyCon] -> TcM TypeRepTodo
todoForTyCons mod mod_id tycons = do
    trTyConTy <- mkTyConTy <$> tcLookupTyCon trTyConTyConName
    let mk_rep_id :: TyConRepName -> Id
        mk_rep_id rep_name = mkExportedVanillaId rep_name trTyConTy

    let typeable_tycons :: [TypeableTyCon]
        typeable_tycons =
            [ TypeableTyCon { tycon = tc''
                            , tycon_rep_id = mk_rep_id rep_name
                            }
            | tc     <- tycons
            , tc'    <- tc : tyConATs tc
              -- If the tycon itself isn't typeable then we needn't look
              -- at its promoted datacons as their kinds aren't Typeable
            , Just _ <- pure $ tyConRepName_maybe tc'
              -- We need type representations for any associated types
            , let promoted = map promoteDataCon (tyConDataCons tc')
            , tc''   <- tc' : promoted
            , Just rep_name <- pure $ tyConRepName_maybe tc''
            , typeIsTypeable $ dropForAlls $ tyConKind tc''
            ]
    return TypeRepTodo { mod_rep_expr    = nlHsVar mod_id
                       , pkg_fingerprint = pkg_fpr
                       , mod_fingerprint = mod_fpr
                       , todo_tycons     = typeable_tycons
                       }
  where
    mod_fpr = fingerprintString $ moduleNameString $ moduleName mod
    pkg_fpr = fingerprintString $ unitIdString $ moduleUnitId mod

todoForExportedKindReps :: [(Kind, Name)] -> TcM TypeRepTodo
todoForExportedKindReps kinds = do
    trKindRepTy <- mkTyConTy <$> tcLookupTyCon kindRepTyConName
    let mkId (k, name) = (k, mkExportedVanillaId name trKindRepTy)
    return $ ExportedKindRepsTodo $ map mkId kinds

-- | Generate TyCon bindings for a set of type constructors
mkTypeRepTodoBinds :: [TypeRepTodo] -> TcM TcGblEnv
mkTypeRepTodoBinds [] = getGblEnv
mkTypeRepTodoBinds todos
  = do { stuff <- collect_stuff

         -- First extend the type environment with all of the bindings
         -- which we are going to produce since we may need to refer to them
         -- while generating kind representations (namely, when we want to
         -- represent a TyConApp in a kind, we must be able to look up the
         -- TyCon associated with the applied type constructor).
       ; let produced_bndrs :: [Id]
             produced_bndrs = [ tycon_rep_id
                              | todo@(TypeRepTodo{}) <- todos
                              , TypeableTyCon {..} <- todo_tycons todo
                              ] ++
                              [ rep_id
                              | ExportedKindRepsTodo kinds <- todos
                              , (_, rep_id) <- kinds
                              ]
       ; gbl_env <- tcExtendGlobalValEnv produced_bndrs getGblEnv

       ; let mk_binds :: TypeRepTodo -> KindRepM [LHsBinds GhcTc]
             mk_binds todo@(TypeRepTodo {}) =
                 mapM (mkTyConRepBinds stuff todo) (todo_tycons todo)
             mk_binds (ExportedKindRepsTodo kinds) =
                 mkExportedKindReps stuff kinds >> return []

       ; (gbl_env, binds) <- setGblEnv gbl_env
                             $ runKindRepM (mapM mk_binds todos)
       ; return $ gbl_env `addTypecheckedBinds` concat binds }

-- | Generate bindings for the type representation of a wired-in 'TyCon's
-- defined by the virtual "GHC.Prim" module. This is where we inject the
-- representation bindings for these primitive types into "GHC.Types"
--
-- See Note [Grand plan for Typeable] in this module.
mkPrimTypeableTodos :: TcM (TcGblEnv, [TypeRepTodo])
mkPrimTypeableTodos
  = do { mod <- getModule
       ; if mod == gHC_TYPES
           then do { -- Build Module binding for GHC.Prim
                     trModuleTyCon <- tcLookupTyCon trModuleTyConName
                   ; let ghc_prim_module_id =
                             mkExportedVanillaId trGhcPrimModuleName
                                                 (mkTyConTy trModuleTyCon)

                   ; ghc_prim_module_bind <- mkVarBind ghc_prim_module_id
                                             <$> mkModIdRHS gHC_PRIM

                     -- Extend our environment with above
                   ; gbl_env <- tcExtendGlobalValEnv [ghc_prim_module_id]
                                                     getGblEnv
                   ; let gbl_env' = gbl_env `addTypecheckedBinds`
                                    [unitBag ghc_prim_module_bind]

                     -- Build TypeRepTodos for built-in KindReps
                   ; todo1 <- todoForExportedKindReps builtInKindReps
                     -- Build TypeRepTodos for types in GHC.Prim
                   ; todo2 <- todoForTyCons gHC_PRIM ghc_prim_module_id
                                            ghcPrimTypeableTyCons
                   ; return ( gbl_env' , [todo1, todo2])
                   }
           else do gbl_env <- getGblEnv
                   return (gbl_env, [])
       }

-- | This is the list of primitive 'TyCon's for which we must generate bindings
-- in "GHC.Types". This should include all types defined in "GHC.Prim".
--
-- The majority of the types we need here are contained in 'primTyCons'.
-- However, not all of them: in particular unboxed tuples are absent since we
-- don't want to include them in the original name cache. See
-- Note [Built-in syntax and the OrigNameCache] in IfaceEnv for more.
ghcPrimTypeableTyCons :: [TyCon]
ghcPrimTypeableTyCons = concat
    [ [ runtimeRepTyCon, vecCountTyCon, vecElemTyCon
      , funTyCon, tupleTyCon Unboxed 0 ]
    , map (tupleTyCon Unboxed) [2..mAX_TUPLE_SIZE]
    , map sumTyCon [2..mAX_SUM_SIZE]
    , primTyCons
    ]

data TypeableStuff
    = Stuff { dflags         :: DynFlags
            , trTyConDataCon :: DataCon         -- ^ of @TyCon@
            , trNameLit      :: FastString -> LHsExpr GhcTc
                                                -- ^ To construct @TrName@s
              -- The various TyCon and DataCons of KindRep
            , kindRepTyCon           :: TyCon
            , kindRepTyConAppDataCon :: DataCon
            , kindRepVarDataCon      :: DataCon
            , kindRepAppDataCon      :: DataCon
            , kindRepFunDataCon      :: DataCon
            , kindRepTYPEDataCon     :: DataCon
            , kindRepTypeLitSDataCon :: DataCon
            , typeLitSymbolDataCon   :: DataCon
            , typeLitNatDataCon      :: DataCon
            }

-- | Collect various tidbits which we'll need to generate TyCon representations.
collect_stuff :: TcM TypeableStuff
collect_stuff = do
    dflags <- getDynFlags
    trTyConDataCon         <- tcLookupDataCon trTyConDataConName
    kindRepTyCon           <- tcLookupTyCon   kindRepTyConName
    kindRepTyConAppDataCon <- tcLookupDataCon kindRepTyConAppDataConName
    kindRepVarDataCon      <- tcLookupDataCon kindRepVarDataConName
    kindRepAppDataCon      <- tcLookupDataCon kindRepAppDataConName
    kindRepFunDataCon      <- tcLookupDataCon kindRepFunDataConName
    kindRepTYPEDataCon     <- tcLookupDataCon kindRepTYPEDataConName
    kindRepTypeLitSDataCon <- tcLookupDataCon kindRepTypeLitSDataConName
    typeLitSymbolDataCon   <- tcLookupDataCon typeLitSymbolDataConName
    typeLitNatDataCon      <- tcLookupDataCon typeLitNatDataConName
    trNameLit              <- mkTrNameLit
    return Stuff {..}

-- | Lookup the necessary pieces to construct the @trNameLit@. We do this so we
-- can save the work of repeating lookups when constructing many TyCon
-- representations.
mkTrNameLit :: TcM (FastString -> LHsExpr GhcTc)
mkTrNameLit = do
    trNameSDataCon <- tcLookupDataCon trNameSDataConName
    let trNameLit :: FastString -> LHsExpr GhcTc
        trNameLit fs = nlHsPar $ nlHsDataCon trNameSDataCon
                       `nlHsApp` nlHsLit (mkHsStringPrimLit fs)
    return trNameLit

-- | Make Typeable bindings for the given 'TyCon'.
mkTyConRepBinds :: TypeableStuff -> TypeRepTodo
                -> TypeableTyCon -> KindRepM (LHsBinds GhcTc)
mkTyConRepBinds stuff@(Stuff {..}) todo (TypeableTyCon {..})
  = do -- Make a KindRep
       let (bndrs, kind) = splitForAllTyVarBndrs (tyConKind tycon)
       liftTc $ traceTc "mkTyConKindRepBinds"
                        (ppr tycon $$ ppr (tyConKind tycon) $$ ppr kind)
       let ctx = mkDeBruijnContext (map binderVar bndrs)
       kind_rep <- getKindRep stuff ctx kind

       -- Make the TyCon binding
       let tycon_rep_rhs = mkTyConRepTyConRHS stuff todo tycon kind_rep
           tycon_rep_bind = mkVarBind tycon_rep_id tycon_rep_rhs
       return $ unitBag tycon_rep_bind

-- | Here is where we define the set of Typeable types. These exclude type
-- families and polytypes.
tyConIsTypeable :: TyCon -> Bool
tyConIsTypeable tc =
       isJust (tyConRepName_maybe tc)
    && typeIsTypeable (dropForAlls $ tyConKind tc)
      -- Ensure that the kind of the TyCon, with its initial foralls removed,
      -- is representable (e.g. has no higher-rank polymorphism or type
      -- synonyms).

-- | Is a particular 'Type' representable by @Typeable@? Here we look for
-- polytypes and types containing casts (which may be, for instance, a type
-- family).
typeIsTypeable :: Type -> Bool
-- We handle types of the form (TYPE rep) specifically to avoid
-- looping on (tyConIsTypeable RuntimeRep)
typeIsTypeable ty
  | Just ty' <- coreView ty         = typeIsTypeable ty'
typeIsTypeable ty
  | Just _ <- isTYPEApp ty          = True
typeIsTypeable (TyVarTy _)          = True
typeIsTypeable (AppTy a b)          = typeIsTypeable a && typeIsTypeable b
typeIsTypeable (FunTy a b)          = typeIsTypeable a && typeIsTypeable b
typeIsTypeable (TyConApp tc args)   = tyConIsTypeable tc
                                   && all typeIsTypeable args
typeIsTypeable (ForAllTy{})         = False
typeIsTypeable (LitTy _)            = True
typeIsTypeable (CastTy{})           = False
typeIsTypeable (CoercionTy{})       = panic "typeIsTypeable(Coercion)"

-- | Maps kinds to 'KindRep' bindings. This binding may either be defined in
-- some other module (in which case the @Maybe (LHsExpr Id@ will be 'Nothing')
-- or a binding which we generated in the current module (in which case it will
-- be 'Just' the RHS of the binding).
type KindRepEnv = TypeMap (Id, Maybe (LHsExpr GhcTc))

-- | A monad within which we will generate 'KindRep's. Here we keep an
-- environment containing 'KindRep's which we've already generated so we can
-- re-use them opportunistically.
newtype KindRepM a = KindRepM { unKindRepM :: StateT KindRepEnv TcRn a }
                   deriving (Functor, Applicative, Monad)

liftTc :: TcRn a -> KindRepM a
liftTc = KindRepM . lift

-- | We generate @KindRep@s for a few common kinds in @GHC.Types@ so that they
-- can be reused across modules.
builtInKindReps :: [(Kind, Name)]
builtInKindReps =
    [ (star, starKindRepName)
    , (mkFunTy star star, starArrStarKindRepName)
    , (mkFunTys [star, star] star, starArrStarArrStarKindRepName)
    ]
  where
    star = liftedTypeKind

initialKindRepEnv :: TcRn KindRepEnv
initialKindRepEnv = foldlM add_kind_rep emptyTypeMap builtInKindReps
  where
    add_kind_rep acc (k,n) = do
        id <- tcLookupId n
        return $! extendTypeMap acc k (id, Nothing)

-- | Performed while compiling "GHC.Types" to generate the built-in 'KindRep's.
mkExportedKindReps :: TypeableStuff
                   -> [(Kind, Id)]  -- ^ the kinds to generate bindings for
                   -> KindRepM ()
mkExportedKindReps stuff@(Stuff {..}) = mapM_ kindrep_binding
  where
    empty_scope = mkDeBruijnContext []

    kindrep_binding :: (Kind, Id) -> KindRepM ()
    kindrep_binding (kind, rep_bndr) = do
        -- We build the binding manually here instead of using mkKindRepRhs
        -- since the latter would find the built-in 'KindRep's in the
        -- 'KindRepEnv' (by virtue of being in 'initialKindRepEnv').
        rhs <- mkKindRepRhs stuff empty_scope kind
        addKindRepBind empty_scope kind rep_bndr rhs

addKindRepBind :: CmEnv -> Kind -> Id -> LHsExpr GhcTc -> KindRepM ()
addKindRepBind in_scope k bndr rhs =
    KindRepM $ modify' $
    \env -> extendTypeMapWithScope env in_scope k (bndr, Just rhs)

-- | Run a 'KindRepM' and add the produced 'KindRep's to the typechecking
-- environment.
runKindRepM :: KindRepM a -> TcRn (TcGblEnv, a)
runKindRepM (KindRepM action) = do
    kindRepEnv <- initialKindRepEnv
    (res, reps_env) <- runStateT action kindRepEnv
    let rep_binds = foldTypeMap to_bind_pair [] reps_env
        to_bind_pair (bndr, Just rhs) rest = (bndr, rhs) : rest
        to_bind_pair (_, Nothing) rest = rest
    tcg_env <- tcExtendGlobalValEnv (map fst rep_binds) getGblEnv
    let binds = map (uncurry mkVarBind) rep_binds
        tcg_env' = tcg_env `addTypecheckedBinds` [listToBag binds]
    return (tcg_env', res)

-- | Produce or find a 'KindRep' for the given kind.
getKindRep :: TypeableStuff -> CmEnv  -- ^ in-scope kind variables
           -> Kind   -- ^ the kind we want a 'KindRep' for
           -> KindRepM (LHsExpr GhcTc)
getKindRep stuff@(Stuff {..}) in_scope = go
  where
    go :: Kind -> KindRepM (LHsExpr GhcTc)
    go = KindRepM . StateT . go'

    go' :: Kind -> KindRepEnv -> TcRn (LHsExpr GhcTc, KindRepEnv)
    go' k env
        -- Look through type synonyms
      | Just k' <- tcView k = go' k' env

        -- We've already generated the needed KindRep
      | Just (id, _) <- lookupTypeMapWithScope env in_scope k
      = return (nlHsVar id, env)

        -- We need to construct a new KindRep binding
      | otherwise
      = do -- Place a NOINLINE pragma on KindReps since they tend to be quite
           -- large and bloat interface files.
           rep_bndr <- (`setInlinePragma` neverInlinePragma)
                   <$> newSysLocalId (fsLit "$krep") (mkTyConTy kindRepTyCon)

           -- do we need to tie a knot here?
           flip runStateT env $ unKindRepM $ do
               rhs <- mkKindRepRhs stuff in_scope k
               addKindRepBind in_scope k rep_bndr rhs
               return $ nlHsVar rep_bndr

-- | Construct the right-hand-side of the 'KindRep' for the given 'Kind' and
-- in-scope kind variable set.
mkKindRepRhs :: TypeableStuff
             -> CmEnv       -- ^ in-scope kind variables
             -> Kind        -- ^ the kind we want a 'KindRep' for
             -> KindRepM (LHsExpr GhcTc) -- ^ RHS expression
mkKindRepRhs stuff@(Stuff {..}) in_scope = new_kind_rep
  where
    new_kind_rep k
        -- We handle TYPE separately to make it clear to consumers
        -- (e.g. serializers) that there is a loop here (as
        -- TYPE :: RuntimeRep -> TYPE 'LiftedRep)
      | Just rr <- isTYPEApp k
      = return $ nlHsDataCon kindRepTYPEDataCon `nlHsApp` nlHsDataCon rr

    new_kind_rep (TyVarTy v)
      | Just idx <- lookupCME in_scope v
      = return $ nlHsDataCon kindRepVarDataCon
                 `nlHsApp` nlHsIntLit (fromIntegral idx)
      | otherwise
      = pprPanic "mkTyConKindRepBinds.go(tyvar)" (ppr v)

    new_kind_rep (AppTy t1 t2)
      = do rep1 <- getKindRep stuff in_scope t1
           rep2 <- getKindRep stuff in_scope t2
           return $ nlHsDataCon kindRepAppDataCon
                    `nlHsApp` rep1 `nlHsApp` rep2

    new_kind_rep k@(TyConApp tc tys)
      | Just rep_name <- tyConRepName_maybe tc
      = do rep_id <- liftTc $ lookupId rep_name
           tys' <- mapM (getKindRep stuff in_scope) tys
           return $ nlHsDataCon kindRepTyConAppDataCon
                    `nlHsApp` nlHsVar rep_id
                    `nlHsApp` mkList (mkTyConTy kindRepTyCon) tys'
      | otherwise
      = pprPanic "mkTyConKindRepBinds(TyConApp)" (ppr tc $$ ppr k)

    new_kind_rep (ForAllTy (TvBndr var _) ty)
      = pprPanic "mkTyConKindRepBinds(ForAllTy)" (ppr var $$ ppr ty)

    new_kind_rep (FunTy t1 t2)
      = do rep1 <- getKindRep stuff in_scope t1
           rep2 <- getKindRep stuff in_scope t2
           return $ nlHsDataCon kindRepFunDataCon
                    `nlHsApp` rep1 `nlHsApp` rep2

    new_kind_rep (LitTy (NumTyLit n))
      = return $ nlHsDataCon kindRepTypeLitSDataCon
                 `nlHsApp` nlHsDataCon typeLitNatDataCon
                 `nlHsApp` nlHsLit (mkHsStringPrimLit $ mkFastString $ show n)

    new_kind_rep (LitTy (StrTyLit s))
      = return $ nlHsDataCon kindRepTypeLitSDataCon
                 `nlHsApp` nlHsDataCon typeLitSymbolDataCon
                 `nlHsApp` nlHsLit (mkHsStringPrimLit $ mkFastString $ show s)

    new_kind_rep (CastTy ty co)
      = pprPanic "mkTyConKindRepBinds.go(cast)" (ppr ty $$ ppr co)

    new_kind_rep (CoercionTy co)
      = pprPanic "mkTyConKindRepBinds.go(coercion)" (ppr co)

-- | Produce the right-hand-side of a @TyCon@ representation.
mkTyConRepTyConRHS :: TypeableStuff -> TypeRepTodo
                   -> TyCon      -- ^ the 'TyCon' we are producing a binding for
                   -> LHsExpr GhcTc -- ^ its 'KindRep'
                   -> LHsExpr GhcTc
mkTyConRepTyConRHS (Stuff {..}) todo tycon kind_rep
  =           nlHsDataCon trTyConDataCon
    `nlHsApp` nlHsLit (word64 dflags high)
    `nlHsApp` nlHsLit (word64 dflags low)
    `nlHsApp` mod_rep_expr todo
    `nlHsApp` trNameLit (mkFastString tycon_str)
    `nlHsApp` nlHsLit (int n_kind_vars)
    `nlHsApp` kind_rep
  where
    n_kind_vars = length $ filter isNamedTyConBinder (tyConBinders tycon)
    tycon_str = add_tick (occNameString (getOccName tycon))
    add_tick s | isPromotedDataCon tycon = '\'' : s
               | otherwise               = s

    -- This must match the computation done in
    -- Data.Typeable.Internal.mkTyConFingerprint.
    Fingerprint high low = fingerprintFingerprints [ pkg_fingerprint todo
                                                   , mod_fingerprint todo
                                                   , fingerprintString tycon_str
                                                   ]

    int :: Int -> HsLit GhcTc
    int n = HsIntPrim (sourceText $ show n) (toInteger n)

word64 :: DynFlags -> Word64 -> HsLit GhcTc
word64 dflags n
  | wORD_SIZE dflags == 4 = HsWord64Prim noSourceText (toInteger n)
  | otherwise             = HsWordPrim   noSourceText (toInteger n)

{-
Note [Representing TyCon kinds: KindRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One of the operations supported by Typeable is typeRepKind,

    typeRepKind :: TypeRep (a :: k) -> TypeRep k

Implementing this is a bit tricky for poly-kinded types like

    data Proxy (a :: k) :: Type
    -- Proxy :: forall k. k -> Type

The TypeRep encoding of `Proxy Type Int` looks like this:

    $tcProxy :: GHC.Types.TyCon
    $trInt   :: TypeRep Int
    $trType  :: TypeRep Type

    $trProxyType :: TypeRep (Proxy Type :: Type -> Type)
    $trProxyType = TrTyCon $tcProxy
                           [$trType]  -- kind variable instantiation

    $trProxy :: TypeRep (Proxy Type Int)
    $trProxy = TrApp $trProxyType $trInt

    $tkProxy :: GHC.Types.KindRep
    $tkProxy = KindRepFun (KindRepVar 0) (KindRepTyConApp $trType [])

Note how $trProxyType cannot use 'TrApp', because TypeRep cannot represent
polymorphic types.  So instead

 * $trProxyType uses 'TrTyCon' to apply Proxy to (the representations)
   of all its kind arguments. We can't represent a tycon that is
   applied to only some of its kind arguments.

 * In $tcProxy, the GHC.Types.TyCon structure for Proxy, we store a
   GHC.Types.KindRep, which represents the polymorphic kind of Proxy
       Proxy :: forall k. k->Type

 * A KindRep is just a recipe that we can instantiate with the
   argument kinds, using Data.Typeable.Internal.instantiateKindRep.

   Data.Typeable.Internal.typeRepKind uses instantiateKindRep

 * In a KindRep, the kind variables are represented by 0-indexed
   de Bruijn numbers:

    type KindBndr = Int   -- de Bruijn index

    data KindRep = KindRepTyConApp TyCon [KindRep]
                 | KindRepVar !KindBndr
                 | KindRepApp KindRep KindRep
                 | KindRepFun KindRep KindRep
                 ...
-}

mkList :: Type -> [LHsExpr GhcTc] -> LHsExpr GhcTc
mkList ty = foldr consApp (nilExpr ty)
  where
    cons = consExpr ty
    consApp :: LHsExpr GhcTc -> LHsExpr GhcTc -> LHsExpr GhcTc
    consApp x xs = cons `nlHsApp` x `nlHsApp` xs

    nilExpr :: Type -> LHsExpr GhcTc
    nilExpr ty = mkLHsWrap (mkWpTyApps [ty]) (nlHsDataCon nilDataCon)

    consExpr :: Type -> LHsExpr GhcTc
    consExpr ty = mkLHsWrap (mkWpTyApps [ty]) (nlHsDataCon consDataCon)
