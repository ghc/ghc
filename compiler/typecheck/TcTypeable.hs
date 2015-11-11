{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1999
-}

module TcTypeable(
    mkTypeableBinds, mkModIdBindings
  ) where


import TcBinds( addTypecheckedBinds )
import IfaceEnv( newGlobalBinder )
import TcEnv
import TcRnMonad
import PrelNames( gHC_TYPES, trModuleDataConName, trTyConDataConName, trNameSDataConName )
import Id
import IdInfo( IdDetails(..) )
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

   The TyConRepNaem is not an "implicit Id".  It's more like a record
   selector: the TyCon knows its name but you have to go to the
   interface file to find its type, value, etc

4. Solve Typeable costraints.  This is done by a custom Typeable solver,
   currently in TcInteract, that use M.$tcT so solve (Typeable T).

There are many wrinkles:

* Since we generate $tcT for every data type T, the types TyCon and
  Module must be available right from the start; so they are defined
  in ghc-prim:GHC.Types

* To save space and reduce dependencies, we need use quite low-level
  representations for TyCon and Module.  See GHC.Types
  Note [Runtime representation of modules and tycons]

* It's hard to generate the TyCon/Module bindings when the types TyCon
  and Module aren't yet available; i.e. when compiling GHC.Types
  itself.  So we *don't* generate them for types in GHC.Types.  Instead
  we write them by hand in base:GHC.Typeable.Internal.

* To be able to define them by hand, they need to have user-writable
  names, thus
        tcBool    not $tcBool    for the type-rep TyCon for Bool
  Hence PrelNames.tyConRepModOcc

* Moreover for type constructors with special syntax, they need to have
  completely hand-crafted names
    lists    tcList         not $tc[]   for the type-rep TyCon for []
    kinds    tcLiftedKind   not $tc*    for the type-rep TyCon for *
  Hence PrelNames.mkSpecialTyConRepName, which takes an extra FastString
  to use for the TyConRepName

* Since listTyCon, boolTyCon etd are wired in, their TyConRepNames must
  be wired in as well.  For these wired-in TyCons we generate the
  TyConRepName's unique from that of the TyCon; see
  Unique.tyConRepNameUnique, dataConRepNameUnique.

-}

{- *********************************************************************
*                                                                      *
            Building top-level binding for $trModule
*                                                                      *
********************************************************************* -}

mkModIdBindings :: TcM TcGblEnv
mkModIdBindings
  = do { mod <- getModule
       ; if mod == gHC_TYPES
         then getGblEnv  -- Do not generate bindings for modules in GHC.Types
         else
    do { loc <- getSrcSpanM
       ; tr_mod_dc  <- tcLookupDataCon trModuleDataConName
       ; tr_name_dc <- tcLookupDataCon trNameSDataConName
       ; mod_nm     <- newGlobalBinder mod (mkVarOcc "$trModule") loc
       ; let mod_id   = mkExportedLocalId ReflectionId mod_nm
                                          (mkTyConApp (dataConTyCon tr_mod_dc) [])
             mod_bind = mkVarBind mod_id mod_rhs
             mod_rhs  = nlHsApps (dataConWrapId tr_mod_dc)
                           [ trNameLit tr_name_dc (unitIdFS (moduleUnitId mod))
                           , trNameLit tr_name_dc (moduleNameFS (moduleName mod)) ]

       ; tcg_env <- tcExtendGlobalValEnv [mod_id] getGblEnv
       ; return (tcg_env { tcg_tr_module = Just mod_id }
                 `addTypecheckedBinds` [unitBag mod_bind]) } }


{- *********************************************************************
*                                                                      *
                Building type-representation bindings
*                                                                      *
********************************************************************* -}

mkTypeableBinds :: [TyCon] -> TcM ([Id], [LHsBinds Id])
mkTypeableBinds tycons
  = do { dflags  <- getDynFlags
       ; gbl_env <- getGblEnv
       ; mod <- getModule
       ; if mod == gHC_TYPES
         then return ([], [])  -- Do not generate bindings for modules in GHC.Types
         else
    do { tr_datacon  <- tcLookupDataCon trTyConDataConName
       ; trn_datacon <- tcLookupDataCon trNameSDataConName
       ; let pkg_str  = unitIdString (moduleUnitId mod)
             mod_str  = moduleNameString (moduleName mod)
             mod_expr = case tcg_tr_module gbl_env of  -- Should be set by now
                           Just mod_id -> nlHsVar mod_id
                           Nothing     -> pprPanic "tcMkTypeableBinds" (ppr tycons)
             stuff    = (dflags, mod_expr, pkg_str, mod_str, tr_datacon, trn_datacon)
             tc_binds = map (mk_typeable_binds stuff) tycons
             tycon_rep_ids = foldr ((++) . collectHsBindsBinders) [] tc_binds
       ; return (tycon_rep_ids, tc_binds) } }

trNameLit :: DataCon -> FastString -> LHsExpr Id
trNameLit tr_name_dc fs
  = nlHsApps (dataConWrapId tr_name_dc) [nlHsLit (mkHsStringPrimLit fs)]

type TypeableStuff
  = ( DynFlags
    , LHsExpr Id  -- Of type GHC.Types.Module
    , String      -- Package name
    , String      -- Module name
    , DataCon     -- Data constructor GHC.Types.TyCon
    , DataCon )   -- Data constructor GHC.Types.TrNameS

mk_typeable_binds :: TypeableStuff -> TyCon -> LHsBinds Id
mk_typeable_binds stuff tycon
  = mkTyConRepBinds stuff tycon
    `unionBags`
    unionManyBags (map (mkTypeableDataConBinds stuff) (tyConDataCons tycon))

mkTyConRepBinds :: TypeableStuff -> TyCon -> LHsBinds Id
mkTyConRepBinds (dflags, mod_expr, pkg_str, mod_str, tr_datacon, trn_datacon) tycon
  = case tyConRepName_maybe tycon of
      Just rep_name -> unitBag (mkVarBind rep_id rep_rhs)
         where
           rep_id  = mkExportedLocalId ReflectionId rep_name (mkTyConApp tr_tycon [])
      _ -> emptyBag
  where
    tr_tycon = dataConTyCon tr_datacon
    rep_rhs = nlHsApps (dataConWrapId tr_datacon)
                       [ nlHsLit (word64 high), nlHsLit (word64 low)
                       , mod_expr
                       , trNameLit trn_datacon (mkFastString tycon_str) ]

    tycon_str = add_tick (occNameString (getOccName tycon))
    add_tick s | isPromotedDataCon tycon = '\'' : s
               | isPromotedTyCon   tycon = '\'' : s
               | otherwise               = s

    hashThis :: String
    hashThis = unwords [pkg_str, mod_str, tycon_str]

    Fingerprint high low
       | gopt Opt_SuppressUniques dflags = Fingerprint 0 0
       | otherwise                       = fingerprintString hashThis

    word64 :: Word64 -> HsLit
    word64 | wORD_SIZE dflags == 4 = \n -> HsWord64Prim (show n) (toInteger n)
           | otherwise             = \n -> HsWordPrim   (show n) (toInteger n)

mkTypeableDataConBinds :: TypeableStuff -> DataCon -> LHsBinds Id
mkTypeableDataConBinds stuff dc
  = case promoteDataCon_maybe dc of
      Promoted tc -> mkTyConRepBinds stuff tc
      NotPromoted -> emptyBag
