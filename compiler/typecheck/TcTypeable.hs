{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999
-}

module TcTypeable(
    mkTypeableBinds, mkPrimTypeableBinds, mkModIdBindings
  ) where


import TcBinds( addTypecheckedBinds )
import IfaceEnv( newGlobalBinder )
import TcEnv
import TcRnMonad
import PrelNames
import TysPrim ( primTyCons )
import TysWiredIn ( trModuleTyCon, trModuleDataCon, trTyConTyCon
                  , trTyConDataCon, trNameSDataCon )
import Id
import Type
import TyCon
import DataCon
import Name( getOccName )
import OccName
import Module
import HsSyn
import DynFlags
import Bag
import Fingerprint(Fingerprint(..), fingerprintString)
import Outputable
import Data.Word( Word64 )
import FastString ( FastString, mkFastString )

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
   We define (in TyCon)
      type TyConRepName = Name
   to use for these M.$tcT "tycon rep names".

3. Record the TyConRepName in T's TyCon, including for promoted
   data and type constructors, and kinds like * and #.

   The TyConRepName is not an "implicit Id".  It's more like a record
   selector: the TyCon knows its name but you have to go to the
   interface file to find its type, value, etc

4. Solve Typeable constraints.  This is done by a custom Typeable solver,
   currently in TcInteract, that use M.$tcT so solve (Typeable T).

There are many wrinkles:

* Since we generate $tcT for every data type T, the types TyCon and
  Module must be available right from the start; so they are wired in (and
  defined in ghc-prim:GHC.Types).

* GHC.Prim doesn't have any associated object code, so we need to put the
  representations for types defined in this module elsewhere. We put these
  in GHC.Types. TcTypeable.mkPrimTypeableBinds is responsible for injecting
  the bindings for the GHC.Prim representions when compiling GHC.Types.

* TyCon.tyConRepModOcc is responsible for determining where to find
  the representation binding for a given type. This is where we handle
  the special case for GHC.Prim.

* To save space and reduce dependencies, we need use quite low-level
  representations for TyCon and Module.  See GHC.Types
  Note [Runtime representation of modules and tycons]

-}

{- *********************************************************************
*                                                                      *
            Building top-level binding for $trModule
*                                                                      *
********************************************************************* -}

mkModIdBindings :: TcM TcGblEnv
mkModIdBindings
  = do { mod <- getModule
       ; loc <- getSrcSpanM
       ; mod_nm     <- newGlobalBinder mod (mkVarOcc "$trModule") loc
       ; let mod_id   = mkExportedVanillaId mod_nm
                                            (mkTyConApp trModuleTyCon [])
             mod_bind = mkVarBind mod_id (mkModIdRHS mod)

       ; tcg_env <- tcExtendGlobalValEnv [mod_id] getGblEnv
       ; return (tcg_env { tcg_tr_module = Just mod_id }
                 `addTypecheckedBinds` [unitBag mod_bind]) }

mkModIdRHS :: Module -> LHsExpr Id
mkModIdRHS mod
  = nlHsApps (dataConWrapId trModuleDataCon)
             [ trNameLit (unitIdFS (moduleUnitId mod))
             , trNameLit (moduleNameFS (moduleName mod)) ]

{- *********************************************************************
*                                                                      *
                Building type-representation bindings
*                                                                      *
********************************************************************* -}

mkTypeableBinds :: [TyCon] -> TcM TcGblEnv
mkTypeableBinds tycons
  = do { dflags <- getDynFlags
       ; gbl_env <- getGblEnv
       ; mod <- getModule
       ; let pkg_str  = unitIdString (moduleUnitId mod)
             mod_str  = moduleNameString (moduleName mod)
             mod_expr = case tcg_tr_module gbl_env of  -- Should be set by now
                           Just mod_id -> nlHsVar mod_id
                           Nothing     -> pprPanic "tcMkTypeableBinds" (ppr tycons)
             stuff    = (dflags, mod_expr, pkg_str, mod_str)
             all_tycons = [ tc' | tc <- tycons, tc' <- tc : tyConATs tc ]
                             -- We need type representations for any associated types
             tc_binds = map (mk_typeable_binds stuff) all_tycons
             tycon_rep_ids = foldr ((++) . collectHsBindsBinders) [] tc_binds

       ; gbl_env <- tcExtendGlobalValEnv tycon_rep_ids getGblEnv
       ; return (gbl_env `addTypecheckedBinds` tc_binds) }

-- | Generate bindings for the type representation of a wired-in TyCon defined
-- by the virtual "GHC.Prim" module. This is where we inject the representation
-- bindings for primitive types into "GHC.Types"
--
-- See Note [Grand plan for Typeable] in this module.
mkPrimTypeableBinds :: TcM TcGblEnv
mkPrimTypeableBinds
  = do { dflags <- getDynFlags
       ; mod <- getModule
       ; let prim_binds :: LHsBinds Id
             prim_binds
               | mod == gHC_TYPES = ghcPrimTypeableBinds dflags
               | otherwise        = emptyBag
             prim_rep_ids = collectHsBindsBinders prim_binds
       ; gbl_env <- tcExtendGlobalValEnv prim_rep_ids getGblEnv
       ; return (gbl_env `addTypecheckedBinds` [prim_binds]) }

-- | Generate bindings for the type representation of the wired-in TyCons defined
-- by the virtual "GHC.Prim" module. This differs from the usual
-- @mkTypeableBinds@ path in that here we need to lie to 'mk_typeable_binds'
-- about the module we are compiling (since we are currently compiling
-- "GHC.Types" yet are producing representations for types in "GHC.Prim").
--
-- See Note [Grand plan for Typeable] in this module.
ghcPrimTypeableBinds :: DynFlags -> LHsBinds Id
ghcPrimTypeableBinds dflags
  = ghc_prim_module_bind `unionBags` unionManyBags (map mkBind all_prim_tys)
  where
    all_prim_tys :: [TyCon]
    all_prim_tys = [ tc' | tc <- funTyCon : primTyCons
                         , tc' <- tc : tyConATs tc ]

    ghc_prim_module_id =
        mkExportedVanillaId trGhcPrimModuleName (mkTyConTy trModuleTyCon)
    ghc_prim_module_bind =
        unitBag $ mkVarBind ghc_prim_module_id (mkModIdRHS gHC_PRIM)

    stuff :: TypeableStuff
    stuff = (dflags, nlHsVar ghc_prim_module_id, "ghc-prim", "GHC.Prim")

    mkBind :: TyCon -> LHsBinds Id
    mkBind = mk_typeable_binds stuff

trNameLit :: FastString -> LHsExpr Id
trNameLit fs
  = nlHsApps (dataConWrapId trNameSDataCon) [nlHsLit (mkHsStringPrimLit fs)]

type TypeableStuff
  = ( DynFlags
    , LHsExpr Id  -- Of type GHC.Types.Module
    , String      -- Package name
    , String      -- Module name
    )

-- | Make bindings for the type representations of a 'TyCon' and its
-- promoted constructors.
mk_typeable_binds :: TypeableStuff -> TyCon -> LHsBinds Id
mk_typeable_binds stuff tycon
  = mkTyConRepBinds stuff tycon
    `unionBags`
    unionManyBags (map (mkTypeableDataConBinds stuff) (tyConDataCons tycon))

mkTyConRepBinds :: TypeableStuff -> TyCon -> LHsBinds Id
mkTyConRepBinds stuff tycon
  = case tyConRepName_maybe tycon of
      Just rep_name -> unitBag (mkVarBind rep_id rep_rhs)
         where
           rep_id  = mkExportedVanillaId rep_name (mkTyConApp trTyConTyCon [])
           rep_rhs = mkTyConRepRHS stuff tycon
      _ -> emptyBag

-- | Produce typeable binds for the promoted 'TyCon' of a data constructor
mkTypeableDataConBinds :: TypeableStuff -> DataCon -> LHsBinds Id
mkTypeableDataConBinds stuff dc
  = mkTyConRepBinds stuff (promoteDataCon dc)

mkTyConRepRHS :: TypeableStuff -> TyCon -> LHsExpr Id
mkTyConRepRHS (dflags, mod_expr, pkg_str, mod_str) tycon = rep_rhs
  where
    rep_rhs = nlHsApps (dataConWrapId trTyConDataCon)
                       [ nlHsLit (word64 high), nlHsLit (word64 low)
                       , mod_expr
                       , trNameLit (mkFastString tycon_str) ]

    tycon_str = add_tick (occNameString (getOccName tycon))
    add_tick s | isPromotedDataCon tycon = '\'' : s
               | otherwise               = s

    hashThis :: String
    hashThis = unwords [pkg_str, mod_str, tycon_str]

    Fingerprint high low = fingerprintString hashThis

    word64 :: Word64 -> HsLit
    word64 | wORD_SIZE dflags == 4 = \n -> HsWord64Prim (show n) (toInteger n)
           | otherwise             = \n -> HsWordPrim   (show n) (toInteger n)

