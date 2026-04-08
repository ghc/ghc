{-# LANGUAGE CPP #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}



-- | The @GHC.Builtin.Utils@ interface to the compiler's prelude knowledge.
--
-- This module serves as the central gathering point for names which the
-- compiler knows something about. This includes functions for,
--
--  * discerning whether a 'Name' is known-key
--
--  * given a 'Unique', looking up its corresponding known-key 'Name'
--
-- See Note [Overview of known-key entities]
-- and Note [Overview of wired-in things] for information
-- about the types of "known" things in GHC.

module GHC.Builtin (
        -- * Main exports
        wiredInNames, wiredInIds, ghcPrimIds,
        knownKeyTable, knownKeyOccMap, knownKeyUniqMap,
        knownKeyOccName, knownKeyOccName_maybe,
        knownKeyRdrName, knownOccRdrName, knownVarOccRdrName,

        -- * Known-key names
        oldIsKnownKeyName,
        oldLookupKnownKeyName,
        oldLookupKnownNameInfo,


        ghcPrimExports,
        ghcPrimDeclDocs,
        ghcPrimWarns,
        ghcPrimFixities,

        -- * Random other things
        maybeCharLikeCon, maybeIntLikeCon,

        -- * Class categories
        isNumericClass, isStandardClass,
        numericClassKeys, fractionalClassKeys, standardClassKeys,
        derivableClassKeys, interactiveClassKeys

    ) where

import GHC.Prelude

import GHC.Builtin.Uniques
import GHC.Builtin.PrimOps
import GHC.Builtin.PrimOps.Ids
import GHC.Builtin.Types
import GHC.Builtin.Types.Literals ( typeNatTyCons )
import GHC.Builtin.Types.Prim
import GHC.Builtin.TH ( templateHaskellNames, thKnownKeyTable )
import GHC.Builtin.Names

import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Core.TyCon

import GHC.Types.Avail
import GHC.Types.Id
import GHC.Types.Fixity
import GHC.Types.Name
import GHC.Types.Name.Reader( RdrName, knownOccRdrName )
import GHC.Types.Name.Env
import GHC.Types.Id.Make
import GHC.Types.SourceText
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map
import GHC.Types.TyThing

import GHC.Utils.Outputable
import GHC.Utils.Misc as Utils
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Parser.Annotation
import GHC.Hs.Doc
import GHC.Hs.Extension (GhcPass)
import GHC.Unit.Module.ModIface (IfaceExport)
import GHC.Unit.Module.Warnings

import GHC.Data.List.SetOps
import GHC.Data.Maybe( orElse )

import Control.Applicative ((<|>))
import Data.Maybe



{- *********************************************************************
*                                                                      *
                     Known-key things
*                                                                      *
********************************************************************* -}

{- Note [Overview of known-key entities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A "wired-in" entity:
  * Its Unique, OccName
  * Its defining module
  * Its data constructors etc
  So GHC knows /everything/ about it.  See Note [Overview of wired-in things].

  We try hard to avoid wired-in things; it's tricky to ensure that GHC's static
  knowledge precisely reflects the code in the library.

A "known-key" entity:
  * Its Unique and OccName are baked into GHC
  * It is exported by base:GHC.KnownKeyNames
  * But that's all that GHC knows about it
  In particular, GHC does /not/ know in which module the entity is defined.

  Example: the `Eq` class has OccName "Eq" and unique `eqClassKey`.  It happens
  to be defined in ghc-internal:GHC.Internal.Classes, but GHC does not know that.

  See Note [Recipe for adding a known-key name] for how to add a known-key name
  to GHC. It's not hard.

A "known-occ" entity:
  * Its OccName is baked into GHC
  * It is exported by base:GHC.KnownKeyNames
  * But that's all that GHC knows about it
  In particular, GHC does /not/ know in which module the entity is defined,
  nor its Unique.

  Example: GHC.Internal.TH.Lib.varE is a known-occ name.  GHC knows its OccName,
  namely "varE", but that is all.

  It is significantly easier to add a known-occ entity to GHC than a known-key
  entity, so we use known-occ entities whenever we can.

When do we use each of these?

* We use a wired-in entity when we must.  E.g. `boolTy` uses the wired-in TyCon
  `boolTyCon`.  We want a static `boolTy` so we can use it in `mkIfThenElse`,
  which is a pure function with no monad in sight.

* We use a known-key entity when we want a fast test to say, for example,
  "are you /the/ Typeable class?", not some other class that happens to be called
  "Typeable".  It checks this using
       cls `hasKnownKey` typeableClassKey
  or equivalently
       getUnique cls == typeableClassKey
  where GHC.Builtin.Names.typeableClassKey is the statically chosen unique
  for `Typeable`.  See `GHC.Tc.Instance.Class.matchGlobalInst`

* We use a known-occ entity when we just want to refer to the thing in, say,
  the code generated for a `deriving` clause.




* Very similarly, see `GHC.Tc.Deriv.Utils.stockSideConditions`, which checks if a
  class is suitable for stock deriving.

Here is why GHC might want to refer to a known-occ entity:

* When desugaring a Template Haskell quotation, in GHC.HsToCore.Quote, GHC
  must generate Core that mentions a myriad of functions defined in
  ghc-internal:GHC.Internal.TH.Lib, such as `varE`, `conE`, `funD`, etc etc.
  They don't need a fixed /unique/, but we still need to find them, so we use
  their /OccName/.  They are "known-occ" entities.

  To do the lookup it uses
     dsLookupKnownOccId :: KnownOcc -> DsM TyThing

* When dealing with `deriving` clauses, GHC generates (LHsBinds GhcPs) bindings,
  and then renames and typechecks them.  These bindings refer to a myriad of
  identifiers, such as `(==)`, `(>)`, `inRange`, and so on.  Again GHC does not
  need to know a statically-known unique for them, but it does need to find them
  so it uses known

* When desugaring, the desugarer wants to refer to a particular
  class, type, or function.  It does this via (e.g.)
     dsLookupKnownOccTyCon :: KnownOcc -> DsM TyCon
  or
     dsLookupKnownKeyTyCon :: KnownKey -> DsM TyCon
  It doesn't really matter which we use.

* In a very similar way, for type-class defauting GHC has built-in defaulting behaviour
  for Num, IsString, etc.   It gets hold of these classes via their known key, via
     tcLookupKnownKeyClass :: KnownKey -> TcM Class
  See GHC.Tc.Gen.Default.tcDefaultDecls

To implement all this, here are the moving parts:

* Each known-key name has a /statically-chosen/ unique, fixed in GHC.Builtin.Names.
  e.g. eqClassKey :: KnownKey
       eqClassKey = mkPreludeClassUnique 3

* All the known-key names are gathered in one table:
      knownKeyTable :: [(OccName, KnownKey)]
      knownKeyTable
        = [ (mkTcOcc "Rational",     rationalTyConKey)
          , (mkTcOcc "Eq",           eqClassKey)
          ... etc ... ]

  INVARIANT (KnownKeyInvariant): It is a requirement that all known-key names
  have distinct OccNames. (We could have multiple name-spaces, but in practice
  this is not an onerous restriction.)

* Because of (KnownKeyInvariant) we can turn that table into two mappings:

      knownKeyOccMap :: OccEnv KnownKey
      knownKeyOccMap = mkOccEnv knownKeyTable

      knownKeyUniqMap :: UniqFM KnownKey OccName

* A new module `base:GHC.KnownKeyNames` exports all the known-key names.
  There is nothing special about this module except that GHC knows its
  name and can import it.

  In effect, the `mi_exports` of `GHC/KnownKeyNames.hi` tells GHC where each
  known-key name is defined.

  This is one reason for (KnownKeyInvariant): an export list cannot have two
  entities with the same OccName.

* There are three flags that control the treatment of known-key names:
    -frebindable-known-key-names
    -fdefines-known-key-names
    -fexclude-known-key-define=wombat   See wrinkle (KKN2)
  Details in the following bullets.

* Known-key name lookup (normal case: KKNS_FromModule)
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  In normal client code, suppose the desugarer calls `dsLookupKnownKeyTyCon`
  on `rationalTyConKey`.  Then, in `loadKnownKeyOccMap`
    * GHC imports GHC.KnownKeyNames, i.e. looks for `GHC/KnownKeyNames.hi`
    * Assuming this is successful, GHC usees its `mi_exports` to builds a mapping
      `KnownKeyNameMap` from each known-key unique to the Name of the entity.
    * It stashes this map in the `eps_known_keys` field of the ExternalPackageState
      so that it doesn't need to repeat the exercise.
  Now it can simplhy look up `rationalTyConKey` in the `eps_known_keys`.  Easy!
  See `dsLookupKnownKeyName`.

* Known-key name lookup (base case: KKNS_InScope)
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  We can't follow the above plan when compiling modules in `base` or `ghc-internal` because
  GHC.KnownKeyNames has not yet been compiled!  Instead, we use whatever is in scope with
  the desired `OccName`, rather like `-XRebindableSyntax`.

  See the `KnownKeyNameSource` argument to `lookupKnownKeyName`. When compiling modules
  in `ghc-internal` or `base`:
    * We switch on -frebindable-known-key-names
    * That ensures that we pass `KKNS_InScope` to `lookupKnownKeyName`
    * The latter now looks in the GlobalRdrEnv it is passed.

  This does mean that in `base` and `ghc-internal` we occasionally need an extra import
  to bring into scope some entities that are needed by `dsLookupKnownKeyTyCon` etc.
  See also wrinkle (KKN1)

* Defining known-key names
  ~~~~~~~~~~~~~~~~~~~~~~~~
  When we /define/ a known-key name, such as
      the `Num` class in ghc-internal:GHC.Internal.Num
  we must assign the correct Unique. So in GHC.Rename.Env.newTopVanillaSrcBinder
  if -fdefines-known-key-names is set (Opt_DefinesKnownKeyNames), we check the
  OccName against the list in `knownKeyTable`; if it appears there, we use the
  Unique from the table.

* Serialising known-key names
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  - When we serialise a known-key name into an interface file, we mark it as such.
    See `serialise_one` in GHC.Iface.Binary.putSymbolTable.
  - When deserialising a name from an interface file, we check the known-key bit,
    If it is set, we get the Unique from the `knownKeyTable`,
    and use `mkKnownKeyName` rather than `mkExternalName` to build the Name.

Wrinkles

(KKN1) We need some special treatment of unused-import warnings.
   See (UI1) in Note [Unused imports] in GHC.Rename.Names

(KKN2) The flag `-fdefines-known-key-names` is module-wide.  But what if that module
   happens to define an entity that /isn't/ a known-key entity, but /does/ share the
   same OccName.   For example:
          module GHC.Internal.Data.Foldable where
             class Foldable t where { ...; toList :: t a -> [a] }
          module GHC.Internal.IsList where
             class IsList l where { ...; toList :: l -> [Item l] }
   Foldable is a known-key entity, so GHC.Internal.Data.Foldable must be compiled
   with `-fdefines-known-key-names`.  But its `toList` method is /not/ known-key.
   Rather, the `toList` from GHC.Internal.IsList is teh known-key entity.

   So we compile GHC.Internal.Data.Foldable with
       -fexclude-known-key-define=toList

(KKN3) You don't need need to export the wired-in entities from GHC.KnownKeyNames
  because we (should) never look up a wired-in name via its key.  That is,
  `GHC.Iface.Load.lookupKnownKeyName` should never be called on the key of
  a wired-in name.

  Alternative: export all wired-in entities from GHC.KnownKeyNames.  But that
  would simply bloat the interface for no good reason.


Note [Recipe for adding a known-occ name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To make `wombat` into a known-occ name, you must ensure that:

* The module `GHC.KnownKeyNames` must export `wombat`.

* In any module in `base` or `ghc-internal` (which are compiled with
  -frebindable-known-key-names), in which `wombat` is needed, you must ensure
  that `wombat` is in scope by saying `import M( wombat )`.

  You do not need to import the module in which `wombat` is /defined/, although
  you may.  It is enough simply to bring `wombat` in scope by importing a
  module that re-exports. You can even import `GHC.KnownKeyNames`, if that does
  not create a module loop!

Note [Recipe for adding a known-key name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To make `wombat` into a known-key name, you must ensure that:

* The module M that defines `wombat` is compiled with `-fdefines-known-key-names`.

* If M.hs has an `M.hs-boot` file, it too must be compiled
  with `-fdefines-known-key-names`.

* The module `GHC.KnownKeyNames` must export `wombat`.

* In GHC.Builtin.Names you must define a static unique
     wombatKey :: KnownKey
     wombatKey = mkPreludeMiscIdUnique 892
  with an unused unique, here 892.

* The big list `GHC.Builtin.Names.knownKeyTable` must contain an
  entry for `wombat`
      (mkVarOcc "wombat", wombatKey)

* In any module in `base` or `ghc-internal` (which are compiled with
  -frebindable-known-key-names), you must ensure that `wombat` is in scope
  by saying `import M( wombat )`.

  If you just say `import M` you may get a "unused import" warning; that
  warning is suppressed for known-key names if you import `wombat` by name.

  You do not need to import the module in which `wombat` is /defined/, although
  you may.  It is enough simply to bring `wombat` in scope by importing a
  module that re-exports. You can even import `GHC.KnownKeyNames`, if that does
  not create a module loop!
-}

-- | `knownKeyOccMap` maps the OccName of a known-key to its Unique
knownKeyOccMap :: OccEnv KnownKey
knownKeyOccMap = mkOccEnv knownKeyTable

knownKeyUniqMap :: UniqFM KnownKey OccName
knownKeyUniqMap = listToUFM [ (uniq, occ) | (occ, uniq) <- knownKeyTable ]

knownKeyTable :: [(OccName, KnownKey)]
knownKeyTable = basicKnownKeyTable ++
                thKnownKeyTable

knownKeyOccName :: HasDebugCallStack => KnownKey -> OccName
-- Find the OccName from the KnownKey,
-- by looking in the knownKeyUniqMap
knownKeyOccName key
  = knownKeyOccName_maybe key `orElse`
    pprPanic "knownKeyOccName" (pprKnownKey key)

knownKeyOccName_maybe :: HasDebugCallStack
                      => KnownKey -> Maybe OccName
knownKeyOccName_maybe key
  = lookupUFM knownKeyUniqMap key

knownKeyRdrName :: KnownKey -> RdrName
knownKeyRdrName key = knownOccRdrName (knownKeyOccName key)

knownVarOccRdrName :: String -> RdrName
knownVarOccRdrName s = knownOccRdrName (mkVarOcc s)



{-
************************************************************************
*                                                                      *
              Groups of keys
*                                                                      *
************************************************************************

NOTE: @Eq@ and @Text@ do need to appear in @standardClasses@
even though every numeric class has these two as a superclass,
because the list of ambiguous dictionaries hasn't been simplified.
-}

numericClassKeys :: [KnownKey]
numericClassKeys
  = checkKnownKeys
      [ numClassKey
      , realClassKey
      , integralClassKey
      ]
    ++ fractionalClassKeys

fractionalClassKeys :: [KnownKey]
fractionalClassKeys
  = checkKnownKeys
        [ fractionalClassKey
        , floatingClassKey
        , realFracClassKey
        , realFloatClassKey
        ]

-- The "standard classes" are used in defaulting (Haskell 98 report 4.3.4),
-- and are: "classes defined in the Prelude or a standard library"
standardClassKeys :: [KnownKey]
standardClassKeys
  = derivableClassKeys
    ++ numericClassKeys
    ++ checkKnownKeys
          [ randomClassKey, randomGenClassKey
          , functorClassKey
          , monadClassKey, monadPlusClassKey, monadFailClassKey
          , semigroupClassKey, monoidClassKey
          , isStringClassKey
          , applicativeClassKey, foldableClassKey
          , traversableClassKey, alternativeClassKey
          ]

{-
@derivableClassKeys@ is also used in checking \tr{deriving} constructs
(@GHC.Tc.Deriv@).
-}

derivableClassKeys :: [KnownKey]
derivableClassKeys
  = checkKnownKeys [ eqClassKey, ordClassKey, enumClassKey, ixClassKey
                   , boundedClassKey, showClassKey, readClassKey ]

interactiveClassKeys :: [KnownKey]
-- These are the "interactive classes" that are consulted when doing
-- defaulting. Does not include Num or IsString, which have special
-- handling.
interactiveClassKeys
  = checkKnownKeys [ showClassKey, eqClassKey, ordClassKey
                   , foldableClassKey, traversableClassKey ]

checkKnownKeys :: [KnownKey] -> [KnownKey]
-- An assertion check, that checks that these alleged known-keys do
-- actually appear in the knownKeyTable.
#ifdef DEBUG
checkKnownKeys keys
  | null bad_keys = keys
  | otherwise     = pprPanic "checkKnownKeys" (vcat (map pprKnownKey bad_keys))
  where
    bad_keys = filter (not . (`elemUFM` knownKeyUniqMap)) keys
#else
checkKnownKeys keys = keys
#endif

isNumericClass, isStandardClass :: Class -> Bool
isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys

is_elem :: Eq a => a -> [a] -> Bool
is_elem = isIn "is_X_Class"


{- *********************************************************************
*                                                                      *
                     Wired-in things
*                                                                      *
************************************************************************

Note [Overview of wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Wired-in things are Ids/TyCons that are completely known to the compiler.
  They are global values in GHC, (e.g.  listTyCon :: TyCon).

* A wired-in Name contains the thing itself inside the Name:
        see Name.wiredInNameTyThing_maybe
  (E.g. listTyConName contains listTyCon.

* The name cache is initialised with (the names of) all wired-in things
  (except tuples and sums; see Note [Infinite families of known-key names])

* The type environment itself contains no wired in things. The type
  checker sees if the Name is wired in before looking up the name in
  the type environment.

* GHC.Iface.Make prunes out wired-in things before putting them in an interface file.
  So interface files never contain wired-in things.

See also
  - Note [Drop wired-in things] in GHC.Iface.Tidy
  - Note [Loading instances for wired-in things] in GHC.Iface.Load
  - Note [Related uniques for wired-in things] in GHC.Builtin.Uniques
  - Note [Declarations for wired-in things] in GHC.Tc.TyCl
-}

-- | This list is used to ensure that when you say "Prelude.map" in your source
-- code, or in an interface file, you get a Name with the correct known key (See
-- Note [Known-key names] in "GHC.Builtin.Names")
wiredInNames :: [Name]
wiredInNames
  | debugIsOn
  , Just badNamesDoc <- knownKeyNamesOkay all_names
  = pprPanic "badAllKnownKeyNames" badNamesDoc
  | otherwise
  = all_names
  where
    all_names =
      concat [ concatMap wired_tycon_kk_names primTyCons
             , concatMap wired_tycon_kk_names wiredInTyCons
             , concatMap wired_tycon_kk_names typeNatTyCons
             , map idName wiredInIds
             , map idName allThePrimOpIds
             , map (idName . primOpWrapperId) allThePrimOps
               -- Actually the primop wrapper Ids have External Names, not WiredIn,
               -- but we still want to populate the OrigNameCache with them

             -- ToDo: get rid of these
             , basicKnownKeyNames
             , templateHaskellNames
             ]
    -- All of the names associated with a wired-in TyCon.
    -- This includes the TyCon itself, its DataCons and promoted TyCons.
    wired_tycon_kk_names :: TyCon -> [Name]
    wired_tycon_kk_names tc =
        tyConName tc : (rep_names tc ++ implicits)
      where implicits = concatMap thing_kk_names (implicitTyConThings tc)

    wired_datacon_kk_names :: DataCon -> [Name]
    wired_datacon_kk_names dc =
      dataConName dc : rep_names (promoteDataCon dc)

    thing_kk_names :: TyThing -> [Name]
    thing_kk_names (ATyCon tc)                 = wired_tycon_kk_names tc
    thing_kk_names (AConLike (RealDataCon dc)) = wired_datacon_kk_names dc
    thing_kk_names thing                       = [getName thing]

    -- The TyConRepName for a known-key TyCon has a known key,
    -- but isn't itself an implicit thing.  Yurgh.
    -- NB: if any of the wired-in TyCons had record fields, the record
    --     field names would be in a similar situation.  Ditto class ops.
    --     But it happens that there aren't any
    rep_names tc = case tyConRepName_maybe tc of
                        Just n  -> [n]
                        Nothing -> []

-- | Check the known-key names list of consistency.
-- (a) Unique is in-range (ToDo: get rid of this)
-- (b) Distinct uniques
knownKeyNamesOkay :: [Name] -> Maybe SDoc
knownKeyNamesOkay all_names
  | ns@(_:_) <- filter (not . isValidKnownKeyUnique . getUnique) all_names
  = Just $ text "    Out-of-range known-key uniques: " <>
           brackets (pprWithCommas (ppr . nameOccName) ns)
  | null badNamesPairs
  = Nothing
  | otherwise
  = Just badNamesDoc
  where
    namesEnv      = foldl' (\m n -> extendNameEnv_Acc (:) Utils.singleton m n n)
                           emptyUFM all_names
    badNamesEnv   = filterNameEnv (\ns -> ns `lengthExceeds` 1) namesEnv
    badNamesPairs = nonDetUFMToList badNamesEnv
      -- It's OK to use nonDetUFMToList here because the ordering only affects
      -- the message when we get a panic
    badNamesDoc :: SDoc
    badNamesDoc  = vcat $ map pairToDoc badNamesPairs

    pairToDoc :: (Unique, [Name]) -> SDoc
    pairToDoc (uniq, ns) = text "        " <>
                           pprUniqueAlways uniq <>
                           text ": " <>
                           brackets (pprWithCommas (ppr . nameOccName) ns)

---------------  ToDo: get rid of these old-mechanism functions
---------------        when we complete the known-key tranitition
--------------   See #27013

-- | Given a 'Unique' lookup its associated 'Name' if it corresponds to a
-- known-key thing.
oldLookupKnownKeyName :: Unique -> Maybe Name
oldLookupKnownKeyName u =
    knownUniqueName u <|> lookupUFM_Directly oldKnownKeysMap u

-- TODO: remove this once all knownkey names come from providers
-- | Is a 'Name' known-key?
oldIsKnownKeyName :: Name -> Bool
oldIsKnownKeyName n =
    isJust (knownUniqueName $ nameUnique n) || elemUFM n oldKnownKeysMap

-- | Maps 'Unique's to known-key names.
--
-- The type is @UniqFM Name Name@ to denote that the 'Unique's used
-- in the domain are 'Unique's associated with 'Name's (as opposed
-- to some other namespace of 'Unique's).
oldKnownKeysMap :: UniqFM Name Name
oldKnownKeysMap = listToIdentityUFM wiredInNames

-- | Given a 'Unique' lookup any associated arbitrary SDoc's to be displayed by
-- GHCi's ':info' command.
oldLookupKnownNameInfo :: Name -> SDoc
oldLookupKnownNameInfo name = case lookupNameEnv knownNamesInfo name of
    -- If we do find a doc, we add comment delimiters to make the output
    -- of ':info' valid Haskell.
    Nothing  -> empty
    Just doc -> vcat [text "{-", doc, text "-}"]

-- A map from Uniques to SDocs, used in GHCi's ':info' command. (#12390)
knownNamesInfo :: NameEnv SDoc
knownNamesInfo = unitNameEnv coercibleTyConName $
    vcat [ text "Coercible is a special constraint with custom solving rules."
         , text "It is not a class."
         , text "Please see section `The Coercible constraint`"
         , text "of the user's guide for details." ]

{-
We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.
-}

{-
************************************************************************
*                                                                      *
            Export lists for pseudo-modules (GHC.Prim)
*                                                                      *
************************************************************************
-}

ghcPrimExports :: [IfaceExport]
ghcPrimExports
 = map (Avail . idName) ghcPrimIds ++
   map (Avail . idName) allThePrimOpIds ++
   [ AvailTC n [n]
   | tc <- exposedPrimTyCons, let n = tyConName tc ]

ghcPrimDeclDocs :: Docs
ghcPrimDeclDocs = emptyDocs
  { docs_decls = listToUniqMap $ mapMaybe declDoc primOpDocs
  , docs_structure = buildStructure primOpDocs
  }
  where
    declDoc (PrimOpDecl fs doc)
      | not (null doc)
      , Just name <- lookupFsEnv ghcPrimNames fs
      = Just (name, [mkHsDoc doc])
    declDoc _ = Nothing

    buildStructure [] = []
    buildStructure (PrimOpSection title desc : rest) =
        DsiSectionHeading 1 (mkHsDoc title)
      : [DsiDocChunk (mkHsDoc desc) | not (null desc)]
     ++ buildStructure rest
    buildStructure items =
      let (decls, rest) = span isDecl items
          avails = mapMaybe declAvail decls
      in  [DsiExports (DefinitelyDeterministicAvails avails) | not (null avails)]
       ++ buildStructure rest

    isDecl (PrimOpDecl {}) = True
    isDecl _               = False

    declAvail (PrimOpDecl fs _)
      | Just name <- lookupFsEnv ghcPrimNames fs
      = Just $ if isTyConName name
               then AvailTC name [name]
               else Avail name
    declAvail _ = Nothing

    mkHsDoc s = WithHsDocIdentifiers (mkGeneratedHsDocString s) []

ghcPrimNames :: FastStringEnv Name
ghcPrimNames
  = mkFsEnv
    [ (occNameFS $ nameOccName name, name)
    | name <-
        map idName ghcPrimIds ++
        map idName allThePrimOpIds ++
        map tyConName exposedPrimTyCons
    ]

-- See Note [GHC.Prim Deprecations]
ghcPrimWarns :: Warnings (GhcPass p)
ghcPrimWarns = WarnSome
  -- declaration warnings
  (map mk_decl_dep primOpDeprecations)
  -- export warnings
  []
  where
    mk_txt msg =
      DeprecatedTxt NoSourceText [noLocA $ WithHsDocIdentifiers (StringLiteral NoSourceText msg Nothing) []]
    mk_decl_dep (occ, msg) = (occ, mk_txt msg)

ghcPrimFixities :: [(OccName,Fixity)]
ghcPrimFixities = fixities
  where
    -- The fixity listed here for @`seq`@ should match
    -- those in primops.txt.pp (from which Haddock docs are generated).
    fixities = (getOccName seqId, Fixity 0 InfixR)
             : mapMaybe mkFixity allThePrimOps
    mkFixity op = (,) (primOpOcc op) <$> primOpFixity op

{-
Note [GHC.Prim Docs]
~~~~~~~~~~~~~~~~~~~~
GHCi's :doc command and Haddock read from ModIface's. GHC.Prim has a wired-in
iface whose docs are populated from primops.txt.

genprimopcode --wired-in-docs generates the primOpDocs list (included as
primop-docs.hs-incl), which contains section headers (PrimOpSection) and
per-declaration documentation (PrimOpDecl). We use stringy names because
mapping names to "Name"s is difficult for things like primtypes and pseudoops.

Note [GHC.Prim Deprecations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Like Haddock documentation, we must record deprecation pragmas in two places:
in the GHC.Prim source module consumed by Haddock, and in the
declarations wired-in to GHC. To do the following we generate
GHC.Builtin.PrimOps.primOpDeprecations, a list of (OccName, DeprecationMessage)
pairs. We insert these deprecations into the mi_warns field of GHC.Prim's ModIface,
as though they were written in a source module.
-}


{-
************************************************************************
*                                                                      *
            Built-in keys
*                                                                      *
************************************************************************

ToDo: make it do the ``like'' part properly (as in 0.26 and before).
-}

maybeCharLikeCon, maybeIntLikeCon :: DataCon -> Bool
maybeCharLikeCon con = con `hasKey` charDataConKey
maybeIntLikeCon  con = con `hasKey` intDataConKey

