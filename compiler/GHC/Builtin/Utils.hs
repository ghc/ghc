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
-- See Note [Known-key names] and Note [About wired-in things] for information
-- about the two types of prelude things in GHC.
--
module GHC.Builtin.Utils (
        -- * Known-key names
        isKnownKeyName,
        lookupKnownKeyName,
        lookupKnownNameInfo,

        -- ** Internal use
        -- | 'knownKeyNames' is exported to seed the original name cache only;
        -- if you find yourself wanting to look at it you might consider using
        -- 'lookupKnownKeyName' or 'isKnownKeyName'.
        knownKeyNames,

        -- * Miscellaneous
        wiredInIds, ghcPrimIds,

        ghcPrimExports,
        ghcPrimDeclDocs,
        ghcPrimWarns,
        ghcPrimFixities,

        -- * Random other things
        maybeCharLikeCon, maybeIntLikeCon,

        -- * Class categories
        isNumericClass, isStandardClass

    ) where

import GHC.Prelude

import GHC.Builtin.Uniques
import GHC.Builtin.PrimOps
import GHC.Builtin.PrimOps.Ids
import GHC.Builtin.Types
import GHC.Builtin.Types.Literals ( typeNatTyCons )
import GHC.Builtin.Types.Prim
import GHC.Builtin.Names.TH ( templateHaskellNames )
import GHC.Builtin.Names

import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Core.TyCon

import GHC.Types.Avail
import GHC.Types.Id
import GHC.Types.Fixity
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Id.Make
import GHC.Types.SourceText
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map
import GHC.Types.TyThing
import GHC.Types.Unique ( isValidKnownKeyUnique, pprUniqueAlways )

import GHC.Utils.Outputable
import GHC.Utils.Misc as Utils
import GHC.Utils.Panic
import GHC.Utils.Constants (debugIsOn)
import GHC.Parser.Annotation
import GHC.Hs.Doc
import GHC.Unit.Module.ModIface (IfaceExport)
import GHC.Unit.Module.Warnings

import GHC.Data.List.SetOps

import Control.Applicative ((<|>))
import Data.Maybe

{-
************************************************************************
*                                                                      *
\subsection[builtinNameInfo]{Lookup built-in names}
*                                                                      *
************************************************************************

Note [About wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Wired-in things are Ids\/TyCons that are completely known to the compiler.
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
-}


-- | This list is used to ensure that when you say "Prelude.map" in your source
-- code, or in an interface file, you get a Name with the correct known key (See
-- Note [Known-key names] in "GHC.Builtin.Names")
knownKeyNames :: [Name]
knownKeyNames
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

-- | Given a 'Unique' lookup its associated 'Name' if it corresponds to a
-- known-key thing.
lookupKnownKeyName :: Unique -> Maybe Name
lookupKnownKeyName u =
    knownUniqueName u <|> lookupUFM_Directly knownKeysMap u

-- | Is a 'Name' known-key?
isKnownKeyName :: Name -> Bool
isKnownKeyName n =
    isJust (knownUniqueName $ nameUnique n) || elemUFM n knownKeysMap

-- | Maps 'Unique's to known-key names.
--
-- The type is @UniqFM Name Name@ to denote that the 'Unique's used
-- in the domain are 'Unique's associated with 'Name's (as opposed
-- to some other namespace of 'Unique's).
knownKeysMap :: UniqFM Name Name
knownKeysMap = listToIdentityUFM knownKeyNames

-- | Given a 'Unique' lookup any associated arbitrary SDoc's to be displayed by
-- GHCi's ':info' command.
lookupKnownNameInfo :: Name -> SDoc
lookupKnownNameInfo name = case lookupNameEnv knownNamesInfo name of
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
ghcPrimDeclDocs = emptyDocs { docs_decls = listToUniqMap $ mapMaybe findName primOpDocs }
  where
    findName (nameStr, doc)
      | Just name <- lookupFsEnv ghcPrimNames nameStr
      = Just (name, [WithHsDocIdentifiers (mkGeneratedHsDocString doc) []])
      | otherwise = Nothing

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
ghcPrimWarns :: Warnings a
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
For haddocks of GHC.Prim we generate a dummy haskell file (gen_hs_source) that
contains the type signatures and the comments (but no implementations)
specifically for consumption by haddock.

GHCi's :doc command reads directly from ModIface's though, and GHC.Prim has a
wired-in iface that has nothing to do with the above haskell file. The code
below converts primops.txt into an intermediate form that would later be turned
into a proper DeclDocMap.

We output the docs as a list of pairs (name, docs). We use stringy names here
because mapping names to "Name"s is difficult for things like primtypes and
pseudoops.

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

{-
************************************************************************
*                                                                      *
            Class predicates
*                                                                      *
************************************************************************
-}

isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys

is_elem :: Eq a => a -> [a] -> Bool
is_elem = isIn "is_X_Class"
