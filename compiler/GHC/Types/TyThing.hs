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
   , tyThingAvailInfo
   , tyThingTyCon
   , tyThingCoAxiom
   , tyThingDataCon
   , tyThingConLike
   , tyThingId
   )
where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Avail

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
      --   the constructor, worker, and (possibly) wrapper
    [ thing | dc    <- tyConDataCons tc
            , thing <- AConLike (RealDataCon dc) : dataConImplicitTyThings dc ]
      -- NB. record selectors are *not* implicit, they have fully-fledged
      -- bindings that pass through the compilation pipeline as normal.
  where
    class_stuff = case tyConClass_maybe tc of
        Nothing -> []
        Just cl -> implicitClassThings cl

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
tyThingParent_maybe (ATyCon tc)   = case tyConAssoc_maybe tc of
                                      Just tc -> Just (ATyCon tc)
                                      Nothing -> Nothing
tyThingParent_maybe (AnId id)     = case idDetails id of
                                      RecSelId { sel_tycon = RecSelData tc } ->
                                          Just (ATyCon tc)
                                      RecSelId { sel_tycon = RecSelPatSyn ps } ->
                                          Just (AConLike (PatSynCon ps))
                                      ClassOpId cls               ->
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

-- | The Names that a TyThing should bring into scope.  Used to build
-- the GlobalRdrEnv for the InteractiveContext.
tyThingAvailInfo :: TyThing -> [AvailInfo]
tyThingAvailInfo (ATyCon t)
   = case tyConClass_maybe t of
        Just c  -> [availTC n ((n : map getName (classMethods c)
                                 ++ map getName (classATs c))) [] ]
             where n = getName c
        Nothing -> [availTC n (n : map getName dcs) flds]
             where n    = getName t
                   dcs  = tyConDataCons t
                   flds = tyConFieldLabels t
tyThingAvailInfo (AConLike (PatSynCon p))
  = avail (getName p) : map availField (patSynFieldLabels p)
tyThingAvailInfo t
   = [avail (getName t)]

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
