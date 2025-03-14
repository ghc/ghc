{-# LANGUAGE LambdaCase #-}

-- | A global typecheckable-thing, essentially anything that has a name.
module GHC.Types.TyThing
   ( TyThing (..)
   , MonadThings (..)
   , mkATyCon
   , mkAnId
   , pprShortTyThing
   , pprTyThingCategory
   , tyThingCategory
   , implicitTyThings
   , implicitConLikeThings
   , implicitClassThings
   , implicitTyConThings
   , implicitCoTyCon
   , isImplicitTyThing
   , tyThingParent_maybe
   , tyThingsTyCoVars
   , tyThingLocalGREs, tyThingGREInfo
   , tyThingTyCon
   , tyThingCoAxiom
   , tyThingDataCon
   , tyThingConLike
   , tyThingId
   )
where

import GHC.Prelude

import GHC.Types.GREInfo
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Unique.Set

import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.PatSyn
import GHC.Core.TyCo.FVs
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import Control.Monad ( liftM )
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE
import Data.List ( intersect )


{-
Note [ATyCon for classes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Both classes and type constructors are represented in the type environment
as ATyCon.  You can tell the difference, and get to the class, with
   isClassTyCon :: TyCon -> Bool
   tyConClass_maybe :: TyCon -> Maybe Class
The Class and its associated TyCon have the same Name.
-}

-- | A global typecheckable-thing, essentially anything that has a name.
-- Not to be confused with a 'TcTyThing', which is also a typecheckable
-- thing but in the *local* context.  See "GHC.Tc.Utils.Env" for how to retrieve
-- a 'TyThing' given a 'Name'.
data TyThing
  = AnId     Id
  | AConLike ConLike
  | ATyCon   TyCon       -- TyCons and classes; see Note [ATyCon for classes]
  | ACoAxiom (CoAxiom Branched)

instance Outputable TyThing where
  ppr = pprShortTyThing

instance NamedThing TyThing where       -- Can't put this with the type
  getName (AnId id)     = getName id    -- decl, because the DataCon instance
  getName (ATyCon tc)   = getName tc    -- isn't visible there
  getName (ACoAxiom cc) = getName cc
  getName (AConLike cl) = conLikeName cl

mkATyCon :: TyCon -> TyThing
mkATyCon = ATyCon

mkAnId :: Id -> TyThing
mkAnId = AnId

pprShortTyThing :: TyThing -> SDoc
-- c.f. GHC.Types.TyThing.Ppr.pprTyThing, which prints all the details
pprShortTyThing thing
  = pprTyThingCategory thing <+> quotes (ppr (getName thing))

pprTyThingCategory :: TyThing -> SDoc
pprTyThingCategory = text . capitalise . tyThingCategory

tyThingCategory :: TyThing -> String
tyThingCategory (ATyCon tc)
  | isClassTyCon tc = "class"
  | otherwise       = "type constructor"
tyThingCategory (ACoAxiom _) = "coercion axiom"
tyThingCategory (AnId   _)   = "identifier"
tyThingCategory (AConLike (RealDataCon _)) = "data constructor"
tyThingCategory (AConLike (PatSynCon _))  = "pattern synonym"


{-
Note [Implicit TyThings]
~~~~~~~~~~~~~~~~~~~~~~~~
  DEFINITION: An "implicit" TyThing is one that does not have its own
  IfaceDecl in an interface file.  Instead, its binding in the type
  environment is created as part of typechecking the IfaceDecl for
  some other thing.

Examples:
  * All DataCons are implicit, because they are generated from the
    IfaceDecl for the data/newtype.  Ditto class methods.

  * Record selectors are *not* implicit, because they get their own
    free-standing IfaceDecl.

  * Associated data/type families are implicit because they are
    included in the IfaceDecl of the parent class.  (NB: the
    IfaceClass decl happens to use IfaceDecl recursively for the
    associated types, but that's irrelevant here.)

  * Dictionary function Ids are not implicit.

  * Axioms for newtypes are implicit (same as above), but axioms
    for data/type family instances are *not* implicit (like DFunIds).
-}

-- | Determine the 'TyThing's brought into scope by another 'TyThing'
-- /other/ than itself. For example, Id's don't have any implicit TyThings
-- as they just bring themselves into scope, but classes bring their
-- dictionary datatype, type constructor and some selector functions into
-- scope, just for a start!

-- N.B. the set of TyThings returned here *must* match the set of
-- names returned by 'GHC.Iface.Load.ifaceDeclImplicitBndrs', in the sense that
-- TyThing.getOccName should define a bijection between the two lists.
-- This invariant is used in 'GHC.IfaceToCore.tc_iface_decl_fingerprint' (see
-- Note [Tricky iface loop])
-- The order of the list does not matter.
implicitTyThings :: TyThing -> [TyThing]
implicitTyThings (AnId _)       = []
implicitTyThings (ACoAxiom _cc) = []
implicitTyThings (ATyCon tc)    = implicitTyConThings tc
implicitTyThings (AConLike cl)  = implicitConLikeThings cl

implicitConLikeThings :: ConLike -> [TyThing]
implicitConLikeThings (RealDataCon dc)
  = dataConImplicitTyThings dc

implicitConLikeThings (PatSynCon {})
  = []  -- Pattern synonyms have no implicit Ids; the wrapper and matcher
        -- are not "implicit"; they are simply new top-level bindings,
        -- and they have their own declaration in an interface file
        -- Unless a record pat syn when there are implicit selectors
        -- They are still not included here as `implicitConLikeThings` is
        -- used by `tcTyClsDecls` whilst pattern synonyms are typed checked
        -- by `tcTopValBinds`.

implicitClassThings :: Class -> [TyThing]
implicitClassThings cl
  = -- Does not include default methods, because those Ids may have
    --    their own pragmas, unfoldings etc, not derived from the Class object

    -- associated types
    --    No recursive call for the classATs, because they
    --    are only the family decls; they have no implicit things
    map ATyCon (classATs cl) ++

    -- superclass and operation selectors
    map AnId (classAllSelIds cl)

implicitTyConThings :: TyCon -> [TyThing]
implicitTyConThings tc
  = class_stuff ++
      -- fields (names of selectors)

      -- (possibly) implicit newtype axioms
      -- or type family axioms
    implicitCoTyCon tc ++

      -- for each data constructor in order,
      --   the constructor and associated implicit 'Id's
    datacon_stuff
      -- NB. record selectors are *not* implicit, they have fully-fledged
      -- bindings that pass through the compilation pipeline as normal.
  where
    class_stuff = case tyConClass_maybe tc of
        Nothing -> []
        Just cl -> implicitClassThings cl

    -- For each data constructor in order,
    --   the constructor, worker, and (possibly) wrapper
    --
    -- If the data constructor is in a "type data" declaration,
    -- promote it to the type level now.
    -- See Note [Type data declarations] in GHC.Rename.Module.
    datacon_stuff :: [TyThing]
    datacon_stuff
      | isTypeDataTyCon tc = [ATyCon (promoteDataCon dc) | dc <- cons]
      | otherwise
      = [ty_thing | dc <- cons,
                    ty_thing <- AConLike (RealDataCon dc) :
                                dataConImplicitTyThings dc]

    cons :: [DataCon]
    cons = tyConDataCons tc

-- For newtypes and closed type families (only) add the implicit coercion tycon
implicitCoTyCon :: TyCon -> [TyThing]
implicitCoTyCon tc
  | Just co <- newTyConCo_maybe tc = [ACoAxiom $ toBranchedAxiom co]
  | Just co <- isClosedSynFamilyTyConWithAxiom_maybe tc
                                   = [ACoAxiom co]
  | otherwise                      = []

-- | Returns @True@ if there should be no interface-file declaration
-- for this thing on its own: either it is built-in, or it is part
-- of some other declaration, or it is generated implicitly by some
-- other declaration.
isImplicitTyThing :: TyThing -> Bool
isImplicitTyThing (AConLike cl) = case cl of
                                    RealDataCon {} -> True
                                    PatSynCon {}   -> False
isImplicitTyThing (AnId id)     = isImplicitId id
isImplicitTyThing (ATyCon tc)   = isImplicitTyCon tc
isImplicitTyThing (ACoAxiom ax) = isImplicitCoAxiom ax

-- | tyThingParent_maybe x returns (Just p)
-- when pprTyThingInContext should print a declaration for p
-- (albeit with some "..." in it) when asked to show x
-- It returns the *immediate* parent.  So a datacon returns its tycon
-- but the tycon could be the associated type of a class, so it in turn
-- might have a parent.
tyThingParent_maybe :: TyThing -> Maybe TyThing
tyThingParent_maybe (AConLike cl) = case cl of
    RealDataCon dc  -> Just (ATyCon (dataConTyCon dc))
    PatSynCon{}     -> Nothing
tyThingParent_maybe (ATyCon tc)
  | -- Special case for `type data` data constructors.  They appear as an
    -- ATyCon (not ADataCon) but we want to display them here as if they were
    -- a DataCon (i.e. with the parent declaration) (#22817).
    -- See Note [Type data declarations] in GHC.Rename.Module.
    Just dc <- isPromotedDataCon_maybe tc
  , let parent_tc = dataConTyCon dc
  , isTypeDataTyCon parent_tc
  = Just (ATyCon parent_tc)
  | Just tc <- tyConAssoc_maybe tc
  = Just (ATyCon tc)
  | otherwise
  = Nothing
tyThingParent_maybe (AnId id)     = case idDetails id of
                                      RecSelId { sel_tycon = RecSelData tc } ->
                                          Just (ATyCon tc)
                                      RecSelId { sel_tycon = RecSelPatSyn ps } ->
                                          Just (AConLike (PatSynCon ps))
                                      ClassOpId cls _             ->
                                          Just (ATyCon (classTyCon cls))
                                      _other                      -> Nothing
tyThingParent_maybe _other = Nothing

tyThingsTyCoVars :: [TyThing] -> TyCoVarSet
tyThingsTyCoVars tts =
    unionVarSets $ map ttToVarSet tts
    where
        ttToVarSet (AnId id)     = tyCoVarsOfType $ idType id
        ttToVarSet (AConLike cl) = case cl of
            RealDataCon dc  -> tyCoVarsOfType $ dataConRepType dc
            PatSynCon{}     -> emptyVarSet
        ttToVarSet (ATyCon tc)
          = case tyConClass_maybe tc of
              Just cls -> (mkVarSet . fst . classTvsFds) cls
              Nothing  -> tyCoVarsOfType $ tyConKind tc
        ttToVarSet (ACoAxiom _)  = emptyVarSet

-- | The 'GlobalRdrElt's that a 'TyThing' should bring into scope.
-- Used to build the 'GlobalRdrEnv' for the InteractiveContext.
tyThingLocalGREs :: TyThing -> [GlobalRdrElt]
tyThingLocalGREs ty_thing =
  case ty_thing of
    ATyCon t
      | Just c <- tyConClass_maybe t
      -> myself NoParent
       : (  map (mkLocalVanillaGRE (ParentIs $ className c) . getName) (classMethods c)
         ++ map tc_GRE (classATs c) )
      | otherwise
      -> let dcs = tyConDataCons t
             par = ParentIs $ tyConName t
             mk_nm = DataConName . dataConName
         in myself NoParent
          : map (dc_GRE par) dcs
            ++
            mkLocalFieldGREs par
               [ (mk_nm dc, con_info)
               | dc <- dcs
               , let con_info = conLikeConInfo (RealDataCon dc) ]
    AConLike con ->
      let (par, cons_flds) = case con of
            PatSynCon {} ->
              (NoParent, [(conLikeConLikeName con, conLikeConInfo con)])
              -- NB: NoParent for local pattern synonyms, as per
              -- Note [Parents] in GHC.Types.Name.Reader.
            RealDataCon dc1 ->
              (ParentIs $ tyConName $ dataConTyCon dc1
              , [ (DataConName $ dataConName $ dc, ConInfo conInfo (ConHasRecordFields (fld :| flds)))
                | dc <- tyConDataCons $ dataConTyCon dc1
                -- Go through all the data constructors of the parent TyCon,
                -- to ensure that all the record fields have the correct set
                -- of parent data constructors. See #23546.
                , let con_info = conLikeConInfo (RealDataCon dc)
                , ConInfo conInfo (ConHasRecordFields flds0) <- [con_info]
                , let flds1 = NE.toList flds0 `intersect` dataConFieldLabels dc
                , fld:flds <- [flds1]
                ])
      in myself par : mkLocalFieldGREs par cons_flds
    AnId id
      | RecSelId { sel_tycon = RecSelData tc } <- idDetails id
      -> [ myself (ParentIs $ tyConName tc) ]
      -- Fallback to NoParent for PatSyn record selectors,
      -- as per Note [Parents] in GHC.Types.Name.Reader.
    _ -> [ myself NoParent ]
  where
    tc_GRE :: TyCon -> GlobalRdrElt
    tc_GRE at = mkLocalTyConGRE
                     (fmap tyConName $ tyConFlavour at)
                     (tyConName at)
    dc_GRE :: Parent -> DataCon -> GlobalRdrElt
    dc_GRE par dc =
      let con_info = conLikeConInfo (RealDataCon dc)
      in mkLocalConLikeGRE par (DataConName $ dataConName dc, con_info)
    myself :: Parent -> GlobalRdrElt
    myself p = mkLocalGRE (tyThingGREInfo ty_thing) p (getName ty_thing)

-- | Obtain information pertinent to the renamer about a particular 'TyThing'.
--
-- This extracts out renamer information from typechecker information.
tyThingGREInfo :: TyThing -> GREInfo
tyThingGREInfo = \case
  AConLike con -> IAmConLike $ conLikeConInfo con
  AnId id -> case idDetails id of
    RecSelId { sel_tycon = parent, sel_fieldLabel = fl } ->
      let relevant_cons = case parent of
            RecSelPatSyn ps -> unitUniqSet $ PatSynName (patSynName ps)
            RecSelData   tc ->
              let dcs = map RealDataCon $ tyConDataCons tc in
              case rsi_def (conLikesRecSelInfo dcs [flLabel fl]) of
                []   -> pprPanic "tyThingGREInfo: no DataCons with this FieldLabel" $
                        vcat [ text "id:"  <+> ppr id
                             , text "fl:"  <+> ppr fl
                             , text "dcs:" <+> ppr dcs ]
                cons -> mkUniqSet $ map conLikeConLikeName cons
       in IAmRecField $
            RecFieldInfo
              { recFieldLabel = fl
              , recFieldCons  = relevant_cons }
    _ -> Vanilla
  ATyCon tc ->
    IAmTyCon (fmap tyConName $ tyConFlavour tc)
  _ -> Vanilla

-- | Get the 'TyCon' from a 'TyThing' if it is a type constructor thing. Panics otherwise
tyThingTyCon :: HasDebugCallStack => TyThing -> TyCon
tyThingTyCon (ATyCon tc) = tc
tyThingTyCon other       = pprPanic "tyThingTyCon" (ppr other)

-- | Get the 'CoAxiom' from a 'TyThing' if it is a coercion axiom thing. Panics otherwise
tyThingCoAxiom :: HasDebugCallStack => TyThing -> CoAxiom Branched
tyThingCoAxiom (ACoAxiom ax) = ax
tyThingCoAxiom other         = pprPanic "tyThingCoAxiom" (ppr other)

-- | Get the 'DataCon' from a 'TyThing' if it is a data constructor thing. Panics otherwise
tyThingDataCon :: HasDebugCallStack => TyThing -> DataCon
tyThingDataCon (AConLike (RealDataCon dc)) = dc
tyThingDataCon other                       = pprPanic "tyThingDataCon" (ppr other)

-- | Get the 'ConLike' from a 'TyThing' if it is a data constructor thing.
-- Panics otherwise
tyThingConLike :: HasDebugCallStack => TyThing -> ConLike
tyThingConLike (AConLike dc) = dc
tyThingConLike other         = pprPanic "tyThingConLike" (ppr other)

-- | Get the 'Id' from a 'TyThing' if it is a id *or* data constructor thing. Panics otherwise
tyThingId :: HasDebugCallStack => TyThing -> Id
tyThingId (AnId id)                   = id
tyThingId (AConLike (RealDataCon dc)) = dataConWrapId dc
tyThingId other                       = pprPanic "tyThingId" (ppr other)

-- | Class that abstracts out the common ability of the monads in GHC
-- to lookup a 'TyThing' in the monadic environment by 'Name'. Provides
-- a number of related convenience functions for accessing particular
-- kinds of 'TyThing'
class Monad m => MonadThings m where
        lookupThing :: Name -> m TyThing

        lookupId :: Name -> m Id
        lookupId = liftM tyThingId . lookupThing

        lookupDataCon :: Name -> m DataCon
        lookupDataCon = liftM tyThingDataCon . lookupThing

        lookupTyCon :: Name -> m TyCon
        lookupTyCon = liftM tyThingTyCon . lookupThing

-- Instance used in GHC.HsToCore.Quote
instance MonadThings m => MonadThings (ReaderT s m) where
  lookupThing = lift . lookupThing
