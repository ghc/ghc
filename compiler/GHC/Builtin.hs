{-# LANGUAGE CPP #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}



-- | The @GHC.Builtin@ interface to the compiler's prelude knowledge.
--
-- This module serves as the central gathering point for names which the
-- compiler knows something about. This includes functions for,
--
--  * discerning whether a 'Name' is known-key
--
--  * given a 'Unique', looking up its corresponding known-key 'Name'
--
-- See Note [Overview of known entities]
-- and Note [Overview of wired-in things] for information
-- about the types of "known" things in GHC.

module GHC.Builtin (
        -- * Main exports
        wiredInNames, wiredInIds, ghcPrimIds, allKnownOccs,
        knownKeyTable, knownKeyOccMap, knownKeyUniqMap,
        knownKeyOccName, knownKeyOccName_maybe,

        -- * Known-key names
        oldIsKnownKeyName,
        oldLookupKnownKeyName,
        oldLookupKnownNameInfo,

        -- * Random other things
        maybeCharLikeCon, maybeIntLikeCon,
        allNameStrings, allNameStringList,
        itName, mkUnboundName, isUnboundName,

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
import GHC.Builtin.TH ( templateHaskellNames, thKnownOccs )
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs( knownOccs, knownOccRdrNames )

import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Core.TyCon

import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Reader( rdrNameOcc )
import GHC.Types.Name.Env
import GHC.Types.Id.Make
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.TyThing
import GHC.Types.SrcLoc

import GHC.Utils.Outputable
import GHC.Utils.Misc as Utils
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)

import GHC.Data.List.SetOps
import GHC.Data.FastString
import qualified GHC.Data.List.Infinite as Inf

import Control.Applicative ((<|>))
import Data.Maybe



{- *********************************************************************
*                                                                      *
                     Known-key things
*                                                                      *
********************************************************************* -}

{- Note [Overview of known entities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are three kinds "known entities", that is, entities that GHC knows
something about.  The three kinds are
  * known-occ entities
  * known-key entities
  * wired-in entities
A "known entity" has a "known name".

There is a spectrum here:
  - It is easy, cheap, and robust to add a new known-occ entity; but
    GHC does not know much about it.
  - In contrast, it is expensive and relatively fragile to add a new
    wired-in entity; but in exchange GHC knows a lot about it.
  - Known-key entities are in the middle.
Use the cheapest one that does what you need!

Here are more details.

A "wired-in" entity:
  * Its Unique, OccName
  * Its defining module
  * Its data constructors etc
  So GHC knows /everything/ about it.  See Note [Overview of wired-in things].

  We try hard to avoid wired-in things; it's tricky to ensure that GHC's static
  knowledge precisely reflects the code in the library.

A "known-key" entity:
  * Its Unique and OccName are baked into GHC. Its Unique is called a KnownKey.
  * It is exported by base:GHC.KnownKeyNames
  * But that's all that GHC knows about it
  In particular, GHC does /not/ know in which module the entity is defined.

  Example: the `Eq` class has OccName "Eq" and unique `eqClassKey`.  It happens
  to be defined in ghc-internal:GHC.Internal.Classes, but GHC does not know that.

  See Note [Recipe for adding a known-key name] for how to add a known-key name
  to GHC. It's not hard.

A "known-occ" entity:
  * Its OccName is baked into GHC -- we call it a KnownOcc
  * It is exported by base:GHC.KnownKeyNames
  * But that's all that GHC knows about it
  In particular, GHC does /not/ know in which module the entity is defined,
  nor its Unique.

  Example: GHC.Internal.TH.Lib.varE is a known-occ name.  GHC knows its OccName,
  namely "varE", but that is all.

  It is significantly easier to add a known-occ entity to GHC than a known-key
  entity, so we use known-occ entities whenever we can.

  Every known-key entity is also a known-occ entity, but not vice versa.

When do we use each of these?

* WIRED-IN.  We use a wired-in entity when we want a statically-defined Type or TyCon.
  E.g. `boolTy` uses the wired-in TyCon `boolTyCon`.  We want a static `boolTy` so
  we can use it in `mkIfThenElse`, which is a pure function with no monad in sight.

* KNOWN-KEY. We use a known-key entity when we want a fast test to say, for example,
  "are you /the/ Typeable class?", not some other class that happens to be called
  "Typeable".  It checks this using
       cls `hasKnownKey` typeableClassKey
  or equivalently
       getUnique cls == typeableClassKey
  where GHC.Builtin.KnownKeys.typeableClassKey is the statically chosen unique
  for `Typeable`.  See `GHC.Tc.Instance.Class.matchGlobalInst`

  Very similarly, see `GHC.Tc.Deriv.Utils.stockSideConditions`, which checks if a
  class is suitable for stock deriving.

  * For type-class defauting GHC has built-in defaulting behaviour
    for Num, IsString, etc.   It gets hold of these classes via their known key, via
       tcLookupKnownKeyClass :: KnownKey -> TcM Class
    See GHC.Tc.Gen.Default.tcDefaultDecls.

* KNOWN_OCC. We use a known-occ entity when we just want to /refer/ to the thing in,
  say, the code generated for a `deriving` clause.  Here is why GHC might want to
  refer to a known-occ entity:

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
    (It doesn't really matter which we use.)

To implement all this, here are the moving parts.

---------------------------
How known-occ entities work
---------------------------

* INVARIANT (KnownEntityInvariant): It is a requirement that all known-key and known-occ
  entities have distinct OccNames. We could have multiple name-spaces, but in practice
  this is not an onerous restriction.  But see Note [Tricky known-occ cases] in
  GHC.Builtin.KnownOccs for some awkward cases.

* A special module `base:GHC.KnownKeyNames` exports all the known-key and known-occ
  entities names. There is nothing special about this module except that GHC knows its
  name and can import it.

  In effect, the `mi_exports` of `GHC/KnownKeyNames.hi` tells GHC where each
  known-key name is defined.  It is /the/ place where we define canonically which
  particular "Eq" you mean when you want the known-occ "Eq".

  This is a big reason for (KnownEntityInvariant): an export list cannot have two
  entities with the same OccName.

* There are three flags that control the treatment of known entities:
    -frebindable-known-names
    -fdefines-known-names
    -fexclude-known-define=wombat   See wrinkle (KN2)
  Details in the following bullets.

* You initiate a known-occ lookup by calling
       tcLookupKnownOccTyCon :: KnownOcc -> TcM TyCon
       dsLookupKnownOccTyCon :: KnownOcc -> DsM TyCon

  The first thing we do is to get the `KnownNameSource`, via `getKnownKeySource`.
  There are then two cases, covered in the following sections.

* Known-occ lookup (normal case: KNS_FromModule)
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  In normal client code, suppose the desugarer calls
     dsLookupKnownKeyTyCon rationalTyConKey
  or
     dsLookupKnownOccTyCon rationalTyConOcc

  Then, in `GHC.Iface.Load.loadKnownKeyOccMaps`

    * GHC imports GHC.KnownKeyNames, i.e. looks for `GHC/KnownKeyNames.hi`

    * Assuming this is successful, GHC uses its `mi_exports` to build `KnownKeyNameMaps`,
      which has (a) a map from the KnownKey of each known-key entity to its Name
                (b) a map from the KnownOcc of each known-occ entity to its Name

    * It stashes these maps in the `eps_known_keys` field of the ExternalPackageState
      so that it doesn't need to repeat the exercise.

  Now it can simply look up `rationalTyConKey` in the `eps_known_keys`.  Easy!
  See `GHC.Iface.Load.lookupKnownKeyThing` and `lookupKnownOccThing`.

* Known-occ lookup (base case: KNS_InScope)
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  We can't follow the above plan when compiling modules in `base` or `ghc-internal` because
  GHC.KnownKeyNames has not yet been compiled!  Instead, we use (roughly) whatever is in
  scope with the desired `OccName`, rather like `-XRebindableSyntax`.

  More precisely, when compiling modules in `ghc-internal` or `base`:

    * We switch on -frebindable-known-names

    * That ensures that we pass `KNS_InScope gbl_rdr_env` to `lookupKnownKeyThing`

    * Suppose we are looking up the known-occ entity "wombat".   The key function is
      `lookupKnownGRE`:
         * First we look in the `gbl_rdr_env` for the /qualified/ name `Rebindable.wombat`.
           If we find a unique hit, choose it.
         * Otherwise we look in `gbl_rdr_env` for the /unqualified/ name `wombat`.
           If we find a unique hit, choose it.

      This plan means that we can have an unrelated local binding for `wombat` and still
      not get confused provided we import Rebindable.wombat.

  This does mean that in `base` and `ghc-internal` we need quite a few extra imports that
  look like    import GHC.InternalNum as Rebindable
          or   import qualified GHC.Internal.Num as Rebindable
  See also wrinkle (KN1)


---------------------------
How known-key entities work
---------------------------
Known-key entities are

* Each known-key entity has a /statically-chosen/ unique, fixed in GHC.Builtin.KnownKeys.
  e.g. eqClassKey :: KnownKey
       eqClassKey = mkPreludeClassUnique 3

* All the known-key names are gathered in one table:
      knownKeyTable :: [(KnownOcc, KnownKey)]
      knownKeyTable
        = [ (mkTcOcc "Rational",     rationalTyConKey)
          , (mkTcOcc "Eq",           eqClassKey)
          ... etc ... ]

* Because of (KnownEntityInvariant) we can turn that table into two mappings:

      knownKeyOccMap :: OccEnv KnownKey
      knownKeyOccMap = mkOccEnv knownKeyTable

      knownKeyUniqMap :: UniqFM KnownKey KnownOcc

* DEFINING.  In the module that /defines/ a known-key name, such as
      the `Num` class in ghc-internal:GHC.Internal.Num
  we must assign the correct Unique. So in GHC.Rename.Env.newTopVanillaSrcBinder
  if -fdefines-known-key-names is set (Opt_DefinesKnownKeyNames), we check the
  OccName against the list in `knownKeyTable`; if it appears there, we use the
  Unique from the table.

* SERIALISING.
  - When we serialise a known-key name into an interface file, we mark it as such.
    See `serialise_one` in GHC.Iface.Binary.putSymbolTable.
  - When deserialising a name from an interface file, we check the known-key bit,
    If it is set, we get the Unique from the `knownKeyTable`,
    and use `mkKnownKeyName` rather than `mkExternalName` to build the Name.

Wrinkles

(KN1) An import declaration may look entirely unused, if it is there solely to
   bring a known-occ name into scope for the desugarer. Why?  Becuase we only generate
   usage information, to drive unused-import warnings, in the renamer and typechecker.
   Not, currently, the desugarer.

   So we simply suppress an unused-import-decl warning if it has a "as Rebindable"
   qualifier.  See (UI1) in Note [Unused imports] in GHC.Rename.Names

(KN2) The flag `-fdefines-known-names` is module-wide.  But what if that module
   happens to define an entity that /isn't/ a known-key entity, but /does/ share the
   same OccName.   For example:
          module GHC.Internal.Data.Foldable where
             class Foldable t where { ...; toList :: t a -> [a] }
          module GHC.Internal.IsList where
             class IsList l where { ...; toList :: l -> [Item l] }
   Foldable is a known-key entity, so GHC.Internal.Data.Foldable must be compiled
   with `-fdefines-known-names`.  But its `toList` method is /not/ known-key.
   Rather, the `toList` from GHC.Internal.IsList is teh known-key entity.

   So we compile GHC.Internal.Data.Foldable with
       -fexclude-known-define=toList

(KN3) We don't need need to export wired-in entities from GHC.KnownKeyNames
  because we (should) never look up a wired-in name via its key.  That is,
  `GHC.Iface.Load.lookupKnownKeyName` should never be called on the key of
  a wired-in name.

  However, it's not wrong for GHC.KnownKeyNames to export more than necessary.

  Alternative: export all wired-in entities from GHC.KnownKeyNames.  But that
  would simply bloat the interface for no good reason.

Note [Recipe for adding a known-occ name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To make `wombat` into a known-occ name, you must ensure that:

* The module `GHC.KnownKeyNames` must export `wombat`.

* In any module in `base` or `ghc-internal` (which are compiled with
  -frebindable-known-names), in which `wombat` is needed, you must ensure
  that `wombat` is in scope by saying `import M( wombat )`, or
     import qualified M as Rebindable( wombat )

  Using the `as Rebindable` qualifier will suppress any unused-import-decl warnings.

  You do not need to import the precise module in which `wombat` is /defined/,
  although you may.  It is enough simply to bring `wombat` in scope by importing a
  module that re-exports it. You can even import `GHC.KnownKeyNames`, if doing so
  does not create a module loop!

Note [Recipe for adding a known-key name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To make `wombat` into a known-key name, you must ensure that:

* The module M that defines `wombat` is compiled with `-fdefines-known-names`.

* If M.hs has an `M.hs-boot` file, it too must be compiled
  with `-fdefines-known-names`.

* The module `GHC.KnownKeyNames` must export `wombat`.

* In GHC.Builtin.KnownKeys you must define a static unique
     wombatKey :: KnownKey
     wombatKey = mkPreludeMiscIdUnique 892
  with an unused unique, here 892.

* The big list `GHC.Builtin.KnownKeys.knownKeyTable` must contain an
  entry for `wombat`
      (mkVarOcc "wombat", wombatKey)

* Just like known-occ names, above in any module in `base` or `ghc-internal` (which
  are compiled with -frebindable-known-names), you must ensure that `wombat` is
  in scope by saying `import M( wombat )`.
-}

allKnownOccs :: OccSet
-- Used only for
--  (a) sanity checks
--  (b) suppressing unused-import warnings in `ghc-internal` and `base`
allKnownOccs
  = mkOccSet thKnownOccs `unionOccSets`
    mkOccSet knownOccs   `unionOccSets`
    mkOccSet (map rdrNameOcc knownOccRdrNames)

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
-- Note [Known-key names] in "GHC.Builtin.KnownKeys")
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
     allNameStrings
*                                                                      *
************************************************************************
-}

allNameStrings :: Inf.Infinite String
-- Infinite list of a,b,c...z, aa, ab, ac, ... etc
allNameStrings = Inf.allListsOf ['a'..'z']

allNameStringList :: [String]
-- Infinite list of a,b,c...z, aa, ab, ac, ... etc
allNameStringList = Inf.toList allNameStrings

{-
************************************************************************
*                                                                      *
\subsection{Local Names}
*                                                                      *
************************************************************************

This *local* name is used by the interactive stuff
-}

itName :: Unique -> SrcSpan -> Name
itName uniq loc = mkInternalName uniq (mkOccNameFS varName (fsLit "it")) loc

-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: OccName -> Name
mkUnboundName occ = mkInternalName unboundKey occ noSrcSpan

isUnboundName :: Name -> Bool
isUnboundName name = name `hasKey` unboundKey


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
