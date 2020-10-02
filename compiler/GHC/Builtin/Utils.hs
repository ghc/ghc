{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE CPP #-}

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
        primOpRules, builtinRules,

        ghcPrimExports,
        ghcPrimDeclDocs,
        primOpId,

        -- * Random other things
        maybeCharLikeCon, maybeIntLikeCon,

        -- * Class categories
        isNumericClass, isStandardClass

    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Builtin.Uniques
import GHC.Builtin.PrimOps
import GHC.Builtin.Types
import GHC.Builtin.Types.Literals ( typeNatTyCons )
import GHC.Builtin.Types.Prim
import GHC.Builtin.Names.TH ( templateHaskellNames )
import GHC.Builtin.Names

import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.Opt.ConstantFold
import GHC.Core.DataCon
import GHC.Core.Class
import GHC.Core.TyCon

import GHC.Types.Avail
import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Id.Make
import GHC.Types.Unique.FM
import GHC.Types.TyThing
import GHC.Types.Unique ( isValidKnownKeyUnique )

import GHC.Utils.Outputable
import GHC.Utils.Misc as Utils
import GHC.Utils.Panic
import GHC.Hs.Doc
import GHC.Unit.Module.ModIface (IfaceExport)

import Control.Applicative ((<|>))
import Data.List        ( intercalate , find )
import Data.Array
import Data.Maybe
import qualified Data.Map as Map

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
  , Just badNamesStr <- knownKeyNamesOkay all_names
  = panic ("badAllKnownKeyNames:\n" ++ badNamesStr)
       -- NB: We can't use ppr here, because this is sometimes evaluated in a
       -- context where there are no DynFlags available, leading to a cryptic
       -- "<<details unavailable>>" error. (This seems to happen only in the
       -- stage 2 compiler, for reasons I [Richard] have no clue of.)
  | otherwise
  = all_names
  where
    all_names =
      -- We exclude most tuples from this list—see
      -- Note [Infinite families of known-key names] in GHC.Builtin.Names.
      -- We make an exception for Solo (i.e., the boxed 1-tuple), since it does
      -- not use special syntax like other tuples.
      -- See Note [One-tuples] (Wrinkle: Make boxed one-tuple names have known keys)
      -- in GHC.Builtin.Types.
      tupleTyConName BoxedTuple 1 : tupleDataConName Boxed 1 :
      concat [ concatMap wired_tycon_kk_names primTyCons
             , concatMap wired_tycon_kk_names wiredInTyCons
             , concatMap wired_tycon_kk_names typeNatTyCons
             , map idName wiredInIds
             , map (idName . primOpId) allThePrimOps
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
knownKeyNamesOkay :: [Name] -> Maybe String
knownKeyNamesOkay all_names
  | ns@(_:_) <- filter (not . isValidKnownKeyUnique . getUnique) all_names
  = Just $ "    Out-of-range known-key uniques: ["
        ++ intercalate ", " (map (occNameString . nameOccName) ns) ++
         "]"
  | null badNamesPairs
  = Nothing
  | otherwise
  = Just badNamesStr
  where
    namesEnv      = foldl' (\m n -> extendNameEnv_Acc (:) Utils.singleton m n n)
                           emptyUFM all_names
    badNamesEnv   = filterNameEnv (\ns -> ns `lengthExceeds` 1) namesEnv
    badNamesPairs = nonDetUFMToList badNamesEnv
      -- It's OK to use nonDetUFMToList here because the ordering only affects
      -- the message when we get a panic
    badNamesStrs  = map pairToStr badNamesPairs
    badNamesStr   = unlines badNamesStrs

    pairToStr (uniq, ns) = "        " ++
                           show uniq ++
                           ": [" ++
                           intercalate ", " (map (occNameString . nameOccName) ns) ++
                           "]"

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

************************************************************************
*                                                                      *
                PrimOpIds
*                                                                      *
************************************************************************
-}

primOpIds :: Array Int Id
-- A cache of the PrimOp Ids, indexed by PrimOp tag
primOpIds = array (1,maxPrimOpTag) [ (primOpTag op, mkPrimOpId op)
                                   | op <- allThePrimOps ]

primOpId :: PrimOp -> Id
primOpId op = primOpIds ! primOpTag op

{-
************************************************************************
*                                                                      *
            Export lists for pseudo-modules (GHC.Prim)
*                                                                      *
************************************************************************
-}

ghcPrimExports :: [IfaceExport]
ghcPrimExports
 = map (avail . idName) ghcPrimIds ++
   map (avail . idName . primOpId) allThePrimOps ++
   [ availTC n [n] []
   | tc <- exposedPrimTyCons, let n = tyConName tc  ]

ghcPrimDeclDocs :: DeclDocMap
ghcPrimDeclDocs = DeclDocMap $ Map.fromList $ mapMaybe findName primOpDocs
  where
    names = map idName ghcPrimIds ++
            map (idName . primOpId) allThePrimOps ++
            map tyConName exposedPrimTyCons
    findName (nameStr, doc)
      | Just name <- find ((nameStr ==) . getOccString) names
      = Just (name, mkHsDocString doc)
      | otherwise = Nothing

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
