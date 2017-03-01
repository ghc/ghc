{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999
-}

{-# LANGUAGE RecordWildCards #-}

module TcTypeable(mkTypeableBinds) where


import BasicTypes ( SourceText(..), Boxity(..), neverInlinePragma )
import TcBinds( addTypecheckedBinds )
import IfaceEnv( newGlobalBinder )
import TyCoRep( Type(..), TyLit(..) )
import TcEnv
import TcEvidence ( mkWpTyApps )
import TcRnMonad
import TcMType ( zonkTcType )
import HscTypes ( lookupId )
import PrelNames
import TysPrim ( primTyCons )
import TysWiredIn ( tupleTyCon, sumTyCon, runtimeRepTyCon
                  , vecCountTyCon, vecElemTyCon
                  , nilDataCon, consDataCon )
import Id
import Type
import Kind ( isTYPEApp )
import TyCon
import DataCon
import Name ( getOccName )
import OccName
import Module
import HsSyn
import DynFlags
import Bag
import Var ( TyVarBndr(..) )
import VarEnv
import Constants
import Fingerprint(Fingerprint(..), fingerprintString, fingerprintFingerprints)
import Outputable
import FastString ( FastString, mkFastString, fsLit )

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
   Note [Representing TyCon kinds] later in this file for details).

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

* Even KindReps aren't inlined this scheme still has more of an effect on
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
       ; mkTypeableTyConBinds (this_mod_todos : prim_todos)
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

mkModIdRHS :: Module -> TcM (LHsExpr Id)
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

-- | Information we need about a 'TyCon' to generate its representation.
data TypeableTyCon
    = TypeableTyCon
      { tycon        :: !TyCon
      , tycon_kind   :: !Kind
      , tycon_rep_id :: !Id
      }

-- | A group of 'TyCon's in need of type-rep bindings.
data TypeRepTodo
    = TypeRepTodo
      { mod_rep_expr    :: LHsExpr Id       -- ^ Module's typerep binding
      , pkg_fingerprint :: !Fingerprint     -- ^ Package name fingerprint
      , mod_fingerprint :: !Fingerprint     -- ^ Module name fingerprint
      , todo_tycons     :: [TypeableTyCon]
        -- ^ The 'TyCon's in need of bindings and their zonked kinds
      }

todoForTyCons :: Module -> Id -> [TyCon] -> TcM TypeRepTodo
todoForTyCons mod mod_id tycons = do
    trTyConTyCon <- tcLookupTyCon trTyConTyConName
    let mkRepId :: TyConRepName -> Id
        mkRepId rep_name = mkExportedVanillaId rep_name (mkTyConTy trTyConTyCon)

    tycons <- sequence
              [ do kind <- zonkTcType $ tyConKind tc''
                   return TypeableTyCon { tycon = tc''
                                        , tycon_kind = kind
                                        , tycon_rep_id = mkRepId rep_name
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
              ]
    let typeable_tycons = filter is_typeable tycons
        is_typeable (TypeableTyCon {..}) =
            --pprTrace "todoForTycons" (ppr tycon $$ ppr bare_kind $$ ppr is_typeable)
            (typeIsTypeable bare_kind)
          where bare_kind = dropForAlls tycon_kind
    return TypeRepTodo { mod_rep_expr    = nlHsVar mod_id
                       , pkg_fingerprint = pkg_fpr
                       , mod_fingerprint = mod_fpr
                       , todo_tycons     = typeable_tycons
                       }
  where
    mod_fpr = fingerprintString $ moduleNameString $ moduleName mod
    pkg_fpr = fingerprintString $ unitIdString $ moduleUnitId mod

-- | Generate TyCon bindings for a set of type constructors
mkTypeableTyConBinds :: [TypeRepTodo] -> TcM TcGblEnv
mkTypeableTyConBinds [] = getGblEnv
mkTypeableTyConBinds todos
  = do { stuff <- collect_stuff

         -- First extend the type environment with all of the bindings which we
         -- are going to produce since we may need to refer to them while
         -- generating the kind representations of other types.
       ; let tycon_rep_bndrs :: [Id]
             tycon_rep_bndrs = [ tycon_rep_id
                               | todo <- todos
                               , TypeableTyCon {..} <- todo_tycons todo
                               ]
       ; gbl_env <- tcExtendGlobalValEnv tycon_rep_bndrs getGblEnv

       ; setGblEnv gbl_env $ foldlM (mk_typeable_binds stuff) gbl_env todos }

-- | Make bindings for the type representations of a 'TyCon' and its
-- promoted constructors.
mk_typeable_binds :: TypeableStuff -> TcGblEnv -> TypeRepTodo -> TcM TcGblEnv
mk_typeable_binds stuff gbl_env todo
  = do pairs <- mapM (mkTyConRepBinds stuff todo) (todo_tycons todo)
       gbl_env <- tcExtendGlobalValEnv (map fst pairs) (return gbl_env)
       return $ gbl_env `addTypecheckedBinds` map snd pairs

-- | Generate bindings for the type representation of a wired-in 'TyCon's
-- defined by the virtual "GHC.Prim" module. This is where we inject the
-- representation bindings for these primitive types into "GHC.Types"
--
-- See Note [Grand plan for Typeable] in this module.
mkPrimTypeableTodos :: TcM (TcGblEnv, [TypeRepTodo])
mkPrimTypeableTodos
  = do { mod <- getModule
       ; if mod == gHC_TYPES
           then do { trModuleTyCon <- tcLookupTyCon trModuleTyConName
                   ; let ghc_prim_module_id =
                             mkExportedVanillaId trGhcPrimModuleName
                                                 (mkTyConTy trModuleTyCon)

                   ; ghc_prim_module_bind <- mkVarBind ghc_prim_module_id
                                             <$> mkModIdRHS gHC_PRIM

                   ; gbl_env <- tcExtendGlobalValEnv [ghc_prim_module_id] getGblEnv
                   ; let gbl_env' = gbl_env `addTypecheckedBinds`
                                    [unitBag ghc_prim_module_bind]
                   ; todo <- todoForTyCons gHC_PRIM ghc_prim_module_id
                                           ghcPrimTypeableTyCons
                   ; return (gbl_env', [todo])
                   }
           else do gbl_env <- getGblEnv
                   return (gbl_env, [])
       }
  where

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
      , funTyCon, tupleTyCon Unboxed 0]
    , map (tupleTyCon Unboxed) [2..mAX_TUPLE_SIZE]
    , map sumTyCon [2..mAX_SUM_SIZE]
    , primTyCons
    ]

data TypeableStuff
    = Stuff { dflags         :: DynFlags
            , trTyConDataCon :: DataCon         -- ^ of @TyCon@
            , trNameLit      :: FastString -> LHsExpr Id
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
mkTrNameLit :: TcM (FastString -> LHsExpr Id)
mkTrNameLit = do
    trNameSDataCon <- tcLookupDataCon trNameSDataConName
    let trNameLit :: FastString -> LHsExpr Id
        trNameLit fs = nlHsPar $ nlHsDataCon trNameSDataCon
                       `nlHsApp` nlHsLit (mkHsStringPrimLit fs)
    return trNameLit

-- | Make typeable bindings for the given 'TyCon'.
mkTyConRepBinds :: TypeableStuff -> TypeRepTodo
                -> TypeableTyCon -> TcRn (Id, LHsBinds Id)
mkTyConRepBinds stuff@(Stuff {..}) todo (TypeableTyCon {..})
  = do -- Place a NOINLINE pragma on KindReps since they tend to be quite large
       -- and bloat interface files.
       kind_rep_id <- (`setInlinePragma` neverInlinePragma)
                      <$> newSysLocalId (fsLit "$krep") (mkTyConTy kindRepTyCon)
       kind_rep <- mkTyConKindRep stuff tycon tycon_kind

       tycon_rep_rhs <- mkTyConRepTyConRHS stuff todo tycon kind_rep_id
       let tycon_rep_bind = mkVarBind tycon_rep_id tycon_rep_rhs
           kind_rep_bind = mkVarBind kind_rep_id kind_rep
       return (kind_rep_id, listToBag [tycon_rep_bind, kind_rep_bind])

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

-- | Produce the right-hand-side of a @TyCon@ representation.
mkTyConRepTyConRHS :: TypeableStuff -> TypeRepTodo
                   -> TyCon -> Id
                   -> TcRn (LHsExpr Id)
mkTyConRepTyConRHS (Stuff {..}) todo tycon kind_rep_id
  = do let rep_rhs = nlHsDataCon trTyConDataCon
                     `nlHsApp` nlHsLit (word64 dflags high)
                     `nlHsApp` nlHsLit (word64 dflags low)
                     `nlHsApp` mod_rep_expr todo
                     `nlHsApp` trNameLit (mkFastString tycon_str)
                     `nlHsApp` nlHsLit (int n_kind_vars)
                     `nlHsApp` nlHsVar kind_rep_id
       return rep_rhs
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

    int :: Int -> HsLit
    int n = HsIntPrim (SourceText $ show n) (toInteger n)

word64 :: DynFlags -> Word64 -> HsLit
word64 dflags n
  | wORD_SIZE dflags == 4 = HsWord64Prim NoSourceText (toInteger n)
  | otherwise             = HsWordPrim   NoSourceText (toInteger n)

{-
Note [Representing TyCon kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

One of the operations supported by Typeable is typeRepKind,

    typeRepKind :: TypeRep (a :: k) -> TypeRep k

Implementing this is a bit tricky. To see why let's consider the TypeRep
encoding of `Proxy Int` where

    data Proxy (a :: k) :: Type

which looks like,

    $tcProxy :: TyCon
    $trInt   :: TypeRep Int
    $trType  :: TypeRep Type

    $trProxyType :: TypeRep (Proxy :: Type -> Type)
    $trProxyType = TrTyCon $tcProxy
                           [$trType]  -- kind variable instantiation

    $trProxy :: TypeRep (Proxy Int)
    $trProxy = TrApp $trProxyType $trInt

Note how $trProxyType encodes only the kind variables of the TyCon
instantiation. To compute the kind (Proxy Int) we need to have a recipe to
compute the kind of a concrete instantiation of Proxy. We call this recipe a
KindRep and store it in the TyCon produced for Proxy,

    type KindBndr = Int   -- de Bruijn index

    data KindRep = KindRepTyConApp TyCon [KindRep]
                 | KindRepVar !KindBndr
                 | KindRepApp KindRep KindRep
                 | KindRepFun KindRep KindRep

The KindRep for Proxy would look like,

    $tkProxy :: KindRep
    $tkProxy = KindRepFun (KindRepVar 0) (KindRepTyConApp $trType [])


data Maybe a = Nothing | Just a

'Just :: a -> Maybe a

F :: forall k. k -> forall k'. k' -> Type
-}

-- | Produce a @KindRep@ expression for the kind of the given 'TyCon'.
mkTyConKindRep :: TypeableStuff -> TyCon -> Kind -> TcRn (LHsExpr Id)
mkTyConKindRep (Stuff {..}) tycon tycon_kind = do
    let (bndrs, kind) = splitForAllTyVarBndrs tycon_kind
        bndr_idxs = mkVarEnv $ (`zip` [0..]) $ map binderVar bndrs
    traceTc "mkTyConKindRepBinds"
             (ppr tycon $$ ppr tycon_kind $$ ppr kind $$ ppr bndr_idxs)
    go bndr_idxs kind
  where
    -- Compute RHS
    go :: VarEnv Int -> Kind -> TcRn (LHsExpr Id)
    go bndrs ty
      | Just ty' <- coreView ty
      = go bndrs ty'
    go bndrs (TyVarTy v)
      | Just idx <- lookupVarEnv bndrs v
      = return $ nlHsDataCon kindRepVarDataCon
                 `nlHsApp` nlHsIntLit (fromIntegral idx)
      | otherwise
      = pprPanic "mkTyConKindRepBinds.go(tyvar)" (ppr v $$ ppr bndrs)
    go bndrs (AppTy t1 t2)
      = do t1' <- go bndrs t1
           t2' <- go bndrs t2
           return $ nlHsDataCon kindRepAppDataCon
                    `nlHsApp` t1' `nlHsApp` t2'
    go _ ty | Just rr <- isTYPEApp ty
      = return $ nlHsDataCon kindRepTYPEDataCon `nlHsApp` nlHsDataCon rr
    go bndrs (TyConApp tc tys)
      | Just rep_name <- tyConRepName_maybe tc
      = do rep_id <- lookupId rep_name
           tys' <- mapM (go bndrs) tys
           return $ nlHsDataCon kindRepTyConAppDataCon
                    `nlHsApp` nlHsVar rep_id
                    `nlHsApp` mkList (mkTyConTy kindRepTyCon) tys'
      | otherwise
      = pprPanic "mkTyConKindRepBinds(TyConApp)"
                 (ppr tc $$ ppr tycon_kind)
    go _ (ForAllTy (TvBndr var _) ty)
      -- = let bndrs' = extendVarEnv (mapVarEnv (+1) bndrs) var 0 in go bndrs' ty
      = pprPanic "mkTyConKindRepBinds(ForAllTy)" (ppr var $$ ppr ty)
    go bndrs (FunTy t1 t2)
      = do t1' <- go bndrs t1
           t2' <- go bndrs t2
           return $ nlHsDataCon kindRepFunDataCon
                    `nlHsApp` t1' `nlHsApp` t2'
    go _ (LitTy (NumTyLit n))
      = return $ nlHsDataCon kindRepTypeLitSDataCon
                 `nlHsApp` nlHsDataCon typeLitNatDataCon
                 `nlHsApp` nlHsLit (mkHsStringPrimLit $ mkFastString $ show n)
    go _ (LitTy (StrTyLit s))
      = return $ nlHsDataCon kindRepTypeLitSDataCon
                 `nlHsApp` nlHsDataCon typeLitSymbolDataCon
                 `nlHsApp` nlHsLit (mkHsStringPrimLit $ mkFastString $ show s)
    go _ (CastTy ty co)
      = pprPanic "mkTyConKindRepBinds.go(cast)" (ppr ty $$ ppr co)
    go _ (CoercionTy co)
      = pprPanic "mkTyConKindRepBinds.go(coercion)" (ppr co)

    mkList :: Type -> [LHsExpr Id] -> LHsExpr Id
    mkList ty = foldr consApp (nilExpr ty)
      where
        cons = consExpr ty
        consApp :: LHsExpr Id -> LHsExpr Id -> LHsExpr Id
        consApp x xs = cons `nlHsApp` x `nlHsApp` xs

    nilExpr :: Type -> LHsExpr Id
    nilExpr ty = mkLHsWrap (mkWpTyApps [ty]) (nlHsDataCon nilDataCon)

    consExpr :: Type -> LHsExpr Id
    consExpr ty = mkLHsWrap (mkWpTyApps [ty]) (nlHsDataCon consDataCon)
