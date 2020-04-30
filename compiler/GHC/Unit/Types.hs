{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Unit & Module types
--
-- This module is used to resolve the loops between Unit and Module types
-- (Module references a Unit and vice-versa).
module GHC.Unit.Types
   ( -- * Modules
     GenModule (..)
   , Module
   , InstalledModule
   , InstantiatedModule
   , mkModule
   , pprModule
   , pprInstantiatedModule
   , moduleFreeHoles

     -- * Units
   , IsUnitId
   , GenUnit (..)
   , Unit
   , UnitId (..)
   , UnitKey (..)
   , GenInstantiatedUnit (..)
   , InstantiatedUnit
   , IndefUnitId
   , DefUnitId
   , Instantiations
   , GenInstantiations
   , mkInstantiatedUnit
   , mkInstantiatedUnitHash
   , mkVirtUnit
   , mapGenUnit
   , mapInstantiations
   , unitFreeModuleHoles
   , fsToUnit
   , unitFS
   , unitString
   , toUnitId
   , virtualUnitId
   , stringToUnit
   , stableUnitCmp
   , unitIsDefinite
   , isHoleUnit

     -- * Unit Ids
   , unitIdString
   , stringToUnitId

     -- * Utils
   , Definite (..)
   , Indefinite (..)

     -- * Wired-in units
   , primUnitId
   , bignumUnitId
   , baseUnitId
   , rtsUnitId
   , thUnitId
   , mainUnitId
   , thisGhcUnitId
   , interactiveUnitId

   , primUnit
   , bignumUnit
   , baseUnit
   , rtsUnit
   , thUnit
   , mainUnit
   , thisGhcUnit
   , interactiveUnit

   , isInteractiveModule
   , wiredInUnitIds

     -- * Boot modules
   , IsBootInterface (..)
   , GenWithIsBoot (..)
   , ModuleNameWithIsBoot
   , ModuleWithIsBoot
   )
where

import GHC.Prelude
import GHC.Types.Unique
import GHC.Types.Unique.DSet
import GHC.Unit.Module.Name
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Utils.Encoding
import GHC.Utils.Fingerprint
import GHC.Utils.Misc

import Control.DeepSeq
import Data.Data
import Data.List (sortBy )
import Data.Function
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8

---------------------------------------------------------------------
-- MODULES
---------------------------------------------------------------------

-- | A generic module is a pair of a unit identifier and a 'ModuleName'.
data GenModule unit = Module
   { moduleUnit :: !unit       -- ^ Unit the module belongs to
   , moduleName :: !ModuleName -- ^ Module name (e.g. A.B.C)
   }
   deriving (Eq,Ord,Data,Functor)

-- | A Module is a pair of a 'Unit' and a 'ModuleName'.
type Module = GenModule Unit

-- | A 'InstalledModule' is a 'Module' whose unit is identified with an
-- 'UnitId'.
type InstalledModule = GenModule UnitId

-- | An `InstantiatedModule` is a 'Module' whose unit is identified with an `InstantiatedUnit`.
type InstantiatedModule = GenModule InstantiatedUnit


mkModule :: u -> ModuleName -> GenModule u
mkModule = Module

instance Uniquable Module where
  getUnique (Module p n) = getUnique (unitFS p `appendFS` moduleNameFS n)

instance Binary a => Binary (GenModule a) where
  put_ bh (Module p n) = put_ bh p >> put_ bh n
  get bh = do p <- get bh; n <- get bh; return (Module p n)

instance NFData (GenModule a) where
  rnf (Module unit name) = unit `seq` name `seq` ()

instance Outputable Module where
  ppr = pprModule

instance Outputable InstalledModule where
  ppr (Module p n) =
    ppr p <> char ':' <> pprModuleName n

instance Outputable InstantiatedModule where
  ppr = pprInstantiatedModule

instance Outputable InstantiatedUnit where
    ppr uid =
      -- getPprStyle $ \sty ->
      ppr cid <>
        (if not (null insts) -- pprIf
          then
            brackets (hcat
                (punctuate comma $
                    [ ppr modname <> text "=" <> pprModule m
                    | (modname, m) <- insts]))
          else empty)
     where
      cid   = instUnitInstanceOf uid
      insts = instUnitInsts uid

-- | Class for types that are used as unit identifiers (UnitKey, UnitId, Unit)
--
-- We need this class because we create new unit ids for virtual units (see
-- VirtUnit) and they have to to be made from units with different kinds of
-- identifiers.
class IsUnitId u where
   unitFS :: u -> FastString

instance IsUnitId UnitKey where
   unitFS (UnitKey fs) = fs

instance IsUnitId UnitId where
   unitFS (UnitId fs) = fs

instance IsUnitId u => IsUnitId (GenUnit u) where
   unitFS (VirtUnit x)            = instUnitFS x
   unitFS (RealUnit (Definite x)) = unitFS x
   unitFS HoleUnit                = holeFS

pprModule :: Module -> SDoc
pprModule mod@(Module p n)  = getPprStyle doc
 where
  doc sty
    | codeStyle sty =
        (if p == mainUnit
                then empty -- never qualify the main package in code
                else ztext (zEncodeFS (unitFS p)) <> char '_')
            <> pprModuleName n
    | qualModule sty mod =
        case p of
          HoleUnit -> angleBrackets (pprModuleName n)
          _        -> ppr (moduleUnit mod) <> char ':' <> pprModuleName n
    | otherwise =
        pprModuleName n


pprInstantiatedModule :: InstantiatedModule -> SDoc
pprInstantiatedModule (Module uid m) =
    ppr uid <> char ':' <> ppr m

---------------------------------------------------------------------
-- UNITS
---------------------------------------------------------------------

-- | A unit key in the database
newtype UnitKey = UnitKey FastString

-- | A unit identifier identifies a (possibly partially) instantiated library.
-- It is primarily used as part of 'Module', which in turn is used in 'Name',
-- which is used to give names to entities when typechecking.
--
-- There are two possible forms for a 'Unit':
--
-- 1) It can be a 'RealUnit', in which case we just have a 'DefUnitId' that
-- uniquely identifies some fully compiled, installed library we have on disk.
--
-- 2) It can be an 'VirtUnit'. When we are typechecking a library with missing
-- holes, we may need to instantiate a library on the fly (in which case we
-- don't have any on-disk representation.)  In that case, you have an
-- 'InstantiatedUnit', which explicitly records the instantiation, so that we
-- can substitute over it.
data GenUnit uid
    = RealUnit !(Definite uid)
      -- ^ Installed definite unit (either a fully instantiated unit or a closed unit)

    | VirtUnit {-# UNPACK #-} !(GenInstantiatedUnit uid)
      -- ^ Virtual unit instantiated on-the-fly. It may be definite if all the
      -- holes are instantiated but we don't have code objects for it.

    | HoleUnit
      -- ^ Fake hole unit

-- | An instantiated unit.
--
-- It identifies an indefinite library (with holes) that has been instantiated.
--
-- This unit may be indefinite or not (i.e. with remaining holes or not). If it
-- is definite, we don't know if it has already been compiled and installed in a
-- database. Nevertheless, we have a mechanism called "improvement" to try to
-- match a fully instantiated unit with existing compiled and installed units:
-- see Note [VirtUnit to RealUnit improvement].
--
-- An indefinite unit identifier pretty-prints to something like
-- @p[H=<H>,A=aimpl:A>]@ (@p@ is the 'IndefUnitId', and the
-- brackets enclose the module substitution).
data GenInstantiatedUnit unit
    = InstantiatedUnit {
        -- | A private, uniquely identifying representation of
        -- an InstantiatedUnit. This string is completely private to GHC
        -- and is just used to get a unique.
        instUnitFS :: !FastString,
        -- | Cached unique of 'unitFS'.
        instUnitKey :: !Unique,
        -- | The indefinite unit being instantiated.
        instUnitInstanceOf :: !(Indefinite unit),
        -- | The sorted (by 'ModuleName') instantiations of this unit.
        instUnitInsts :: !(GenInstantiations unit),
        -- | A cache of the free module holes of 'instUnitInsts'.
        -- This lets us efficiently tell if a 'InstantiatedUnit' has been
        -- fully instantiated (empty set of free module holes)
        -- and whether or not a substitution can have any effect.
        instUnitHoles :: UniqDSet ModuleName
    }

type Unit             = GenUnit             UnitId
type InstantiatedUnit = GenInstantiatedUnit UnitId

type GenInstantiations unit = [(ModuleName,GenModule (GenUnit unit))]
type Instantiations         = GenInstantiations UnitId

holeUnique :: Unique
holeUnique = getUnique holeFS

holeFS :: FastString
holeFS = fsLit "<hole>"

isHoleUnit :: GenUnit u -> Bool
isHoleUnit HoleUnit = True
isHoleUnit _        = False


instance Eq (GenInstantiatedUnit unit) where
  u1 == u2 = instUnitKey u1 == instUnitKey u2

instance Ord (GenInstantiatedUnit unit) where
  u1 `compare` u2 = instUnitFS u1 `uniqCompareFS` instUnitFS u2

instance Binary InstantiatedUnit where
  put_ bh indef = do
    put_ bh (instUnitInstanceOf indef)
    put_ bh (instUnitInsts indef)
  get bh = do
    cid   <- get bh
    insts <- get bh
    let fs = mkInstantiatedUnitHash cid insts
    return InstantiatedUnit {
            instUnitInstanceOf = cid,
            instUnitInsts = insts,
            instUnitHoles = unionManyUniqDSets (map (moduleFreeHoles.snd) insts),
            instUnitFS = fs,
            instUnitKey = getUnique fs
           }

instance IsUnitId u => Eq (GenUnit u) where
  uid1 == uid2 = unitUnique uid1 == unitUnique uid2

instance IsUnitId u => Uniquable (GenUnit u) where
  getUnique = unitUnique

instance Ord Unit where
  nm1 `compare` nm2 = stableUnitCmp nm1 nm2

instance Data Unit where
  -- don't traverse?
  toConstr _   = abstractConstr "Unit"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Unit"

instance NFData Unit where
  rnf x = x `seq` ()

-- | Compares unit ids lexically, rather than by their 'Unique's
stableUnitCmp :: Unit -> Unit -> Ordering
stableUnitCmp p1 p2 = unitFS p1 `lexicalCompareFS` unitFS p2

instance Outputable Unit where
   ppr pk = pprUnit pk

pprUnit :: Unit -> SDoc
pprUnit (RealUnit uid) = ppr uid
pprUnit (VirtUnit uid) = ppr uid
pprUnit HoleUnit       = ftext holeFS

instance Show Unit where
    show = unitString

-- Performance: would prefer to have a NameCache like thing
instance Binary Unit where
  put_ bh (RealUnit def_uid) = do
    putByte bh 0
    put_ bh def_uid
  put_ bh (VirtUnit indef_uid) = do
    putByte bh 1
    put_ bh indef_uid
  put_ bh HoleUnit =
    putByte bh 2
  get bh = do b <- getByte bh
              case b of
                0 -> fmap RealUnit (get bh)
                1 -> fmap VirtUnit (get bh)
                _ -> pure HoleUnit

-- | Retrieve the set of free module holes of a 'Unit'.
unitFreeModuleHoles :: GenUnit u -> UniqDSet ModuleName
unitFreeModuleHoles (VirtUnit x) = instUnitHoles x
unitFreeModuleHoles (RealUnit _) = emptyUniqDSet
unitFreeModuleHoles HoleUnit     = emptyUniqDSet

-- | Calculate the free holes of a 'Module'.  If this set is non-empty,
-- this module was defined in an indefinite library that had required
-- signatures.
--
-- If a module has free holes, that means that substitutions can operate on it;
-- if it has no free holes, substituting over a module has no effect.
moduleFreeHoles :: GenModule (GenUnit u) -> UniqDSet ModuleName
moduleFreeHoles (Module HoleUnit name) = unitUniqDSet name
moduleFreeHoles (Module u        _   ) = unitFreeModuleHoles u


-- | Create a new 'GenInstantiatedUnit' given an explicit module substitution.
mkInstantiatedUnit :: IsUnitId u => Indefinite u -> GenInstantiations u -> GenInstantiatedUnit u
mkInstantiatedUnit cid insts =
    InstantiatedUnit {
        instUnitInstanceOf = cid,
        instUnitInsts = sorted_insts,
        instUnitHoles = unionManyUniqDSets (map (moduleFreeHoles.snd) insts),
        instUnitFS = fs,
        instUnitKey = getUnique fs
    }
  where
     fs           = mkInstantiatedUnitHash cid sorted_insts
     sorted_insts = sortBy (stableModuleNameCmp `on` fst) insts


-- | Smart constructor for instantiated GenUnit
mkVirtUnit :: IsUnitId u => Indefinite u -> [(ModuleName, GenModule (GenUnit u))] -> GenUnit u
mkVirtUnit uid []    = RealUnit $ Definite (indefUnit uid) -- huh? indefinite unit without any instantiation/hole?
mkVirtUnit uid insts = VirtUnit $ mkInstantiatedUnit uid insts

-- | Generate a uniquely identifying hash (internal unit-id) for an instantiated
-- unit.
--
-- This is a one-way function. If the indefinite unit has not been instantiated at all, we return its unit-id.
--
-- This hash is completely internal to GHC and is not used for symbol names or
-- file paths. It is different from the hash Cabal would produce for the same
-- instantiated unit.
mkInstantiatedUnitHash :: IsUnitId u => Indefinite u -> [(ModuleName, GenModule (GenUnit u))] -> FastString
mkInstantiatedUnitHash cid sorted_holes =
    mkFastStringByteString
  . fingerprintUnitId (bytesFS (unitFS cid))
  $ hashInstantiations sorted_holes

-- | Generate a hash for a sorted module instantiation.
hashInstantiations :: IsUnitId u => [(ModuleName, GenModule (GenUnit u))] -> Fingerprint
hashInstantiations sorted_holes =
    fingerprintByteString
  . BS.concat $ do
        (m, b) <- sorted_holes
        [ bytesFS (moduleNameFS m),              BS.Char8.singleton ' ',
          bytesFS (unitFS (moduleUnit b)),       BS.Char8.singleton ':',
          bytesFS (moduleNameFS (moduleName b)), BS.Char8.singleton '\n']

fingerprintUnitId :: BS.ByteString -> Fingerprint -> BS.ByteString
fingerprintUnitId prefix (Fingerprint a b)
    = BS.concat
    $ [ prefix
      , BS.Char8.singleton '-'
      , BS.Char8.pack (toBase62Padded a)
      , BS.Char8.pack (toBase62Padded b) ]

unitUnique :: IsUnitId u => GenUnit u -> Unique
unitUnique (VirtUnit x)            = instUnitKey x
unitUnique (RealUnit (Definite x)) = getUnique (unitFS x)
unitUnique HoleUnit                = holeUnique

-- | Create a new simple unit identifier from a 'FastString'.  Internally,
-- this is primarily used to specify wired-in unit identifiers.
fsToUnit :: FastString -> Unit
fsToUnit = RealUnit . Definite . UnitId

unitString :: IsUnitId u => u  -> String
unitString = unpackFS . unitFS

stringToUnit :: String -> Unit
stringToUnit = fsToUnit . mkFastString

-- | Map over the unit type of a 'GenUnit'
mapGenUnit :: IsUnitId v => (u -> v) -> GenUnit u -> GenUnit v
mapGenUnit f = go
   where
      go gu = case gu of
               HoleUnit   -> HoleUnit
               RealUnit d -> RealUnit (fmap f d)
               VirtUnit i ->
                  VirtUnit $ mkInstantiatedUnit
                     (fmap f (instUnitInstanceOf i))
                     (fmap (second (fmap go)) (instUnitInsts i))

-- | Map over the unit identifier of unit instantiations.
mapInstantiations :: IsUnitId v => (u -> v) -> GenInstantiations u -> GenInstantiations v
mapInstantiations f = map (second (fmap (mapGenUnit f)))

-- | Return the UnitId of the Unit. For on-the-fly instantiated units, return
-- the UnitId of the indefinite unit this unit is an instance of.
toUnitId :: Unit -> UnitId
toUnitId (RealUnit (Definite iuid)) = iuid
toUnitId (VirtUnit indef)           = indefUnit (instUnitInstanceOf indef)
toUnitId HoleUnit                   = error "Hole unit"

-- | Return the virtual UnitId of an on-the-fly instantiated unit.
virtualUnitId :: InstantiatedUnit -> UnitId
virtualUnitId i = UnitId (instUnitFS i)

-- | A 'Unit' is definite if it has no free holes.
unitIsDefinite :: Unit -> Bool
unitIsDefinite = isEmptyUniqDSet . unitFreeModuleHoles

---------------------------------------------------------------------
-- UNIT IDs
---------------------------------------------------------------------

-- | A UnitId identifies a built library in a database and is used to generate
-- unique symbols, etc. It's usually of the form:
--
--    pkgname-1.2:libname+hash
--
-- These UnitId are provided to us via the @-this-unit-id@ flag.
--
-- The library in question may be definite or indefinite; if it is indefinite,
-- none of the holes have been filled (we never install partially instantiated
-- libraries as we can cheaply instantiate them on-the-fly, cf VirtUnit).  Put
-- another way, an installed unit id is either fully instantiated, or not
-- instantiated at all.
newtype UnitId =
    UnitId {
      -- | The full hashed unit identifier, including the component id
      -- and the hash.
      unitIdFS :: FastString
    }

instance Binary UnitId where
  put_ bh (UnitId fs) = put_ bh fs
  get bh = do fs <- get bh; return (UnitId fs)

instance Eq UnitId where
    uid1 == uid2 = getUnique uid1 == getUnique uid2

instance Ord UnitId where
    -- we compare lexically to avoid non-deterministic output when sets of
    -- unit-ids are printed (dependencies, etc.)
    u1 `compare` u2 = unitIdFS u1 `lexicalCompareFS` unitIdFS u2

instance Uniquable UnitId where
    getUnique = getUnique . unitIdFS

instance Outputable UnitId where
    ppr (UnitId fs) = sdocOption sdocUnitIdForUser ($ fs) -- see Note [Pretty-printing UnitId]
                                                          -- in "GHC.Unit"

-- | A 'DefUnitId' is an 'UnitId' with the invariant that
-- it only refers to a definite library; i.e., one we have generated
-- code for.
type DefUnitId = Definite UnitId

unitIdString :: UnitId -> String
unitIdString = unpackFS . unitIdFS

stringToUnitId :: String -> UnitId
stringToUnitId = UnitId . mkFastString

---------------------------------------------------------------------
-- UTILS
---------------------------------------------------------------------

-- | A definite unit (i.e. without any free module hole)
newtype Definite unit = Definite { unDefinite :: unit }
   deriving (Functor)
   deriving newtype (Eq, Ord, Outputable, Binary, Uniquable, IsUnitId)

-- | An 'IndefUnitId' is an 'UnitId' with the invariant that it only
-- refers to an indefinite library; i.e., one that can be instantiated.
type IndefUnitId = Indefinite UnitId

newtype Indefinite unit = Indefinite { indefUnit :: unit }
   deriving (Functor)
   deriving newtype (Eq, Ord, Outputable, Binary, Uniquable, IsUnitId)

---------------------------------------------------------------------
-- WIRED-IN UNITS
---------------------------------------------------------------------

{-
Note [Wired-in units]
~~~~~~~~~~~~~~~~~~~~~

Certain packages are known to the compiler, in that we know about certain
entities that reside in these packages, and the compiler needs to
declare static Modules and Names that refer to these packages.  Hence
the wired-in packages can't include version numbers in their package UnitId,
since we don't want to bake the version numbers of these packages into GHC.

So here's the plan.  Wired-in units are still versioned as
normal in the packages database, and you can still have multiple
versions of them installed. To the user, everything looks normal.

However, for each invocation of GHC, only a single instance of each wired-in
package will be recognised (the desired one is selected via
@-package@\/@-hide-package@), and GHC will internally pretend that it has the
*unversioned* 'UnitId', including in .hi files and object file symbols.

Unselected versions of wired-in packages will be ignored, as will any other
package that depends directly or indirectly on it (much as if you
had used @-ignore-package@).

The affected packages are compiled with, e.g., @-this-unit-id base@, so that
the symbols in the object files have the unversioned unit id in their name.

Make sure you change 'GHC.Unit.State.findWiredInUnits' if you add an entry here.

-}

bignumUnitId, primUnitId, baseUnitId, rtsUnitId,
  thUnitId, mainUnitId, thisGhcUnitId, interactiveUnitId  :: UnitId

bignumUnit, primUnit, baseUnit, rtsUnit,
  thUnit, mainUnit, thisGhcUnit, interactiveUnit  :: Unit

primUnitId        = UnitId (fsLit "ghc-prim")
bignumUnitId      = UnitId (fsLit "ghc-bignum")
baseUnitId        = UnitId (fsLit "base")
rtsUnitId         = UnitId (fsLit "rts")
thisGhcUnitId     = UnitId (fsLit "ghc")
interactiveUnitId = UnitId (fsLit "interactive")
thUnitId          = UnitId (fsLit "template-haskell")

thUnit            = RealUnit (Definite thUnitId)
primUnit          = RealUnit (Definite primUnitId)
bignumUnit        = RealUnit (Definite bignumUnitId)
baseUnit          = RealUnit (Definite baseUnitId)
rtsUnit           = RealUnit (Definite rtsUnitId)
thisGhcUnit       = RealUnit (Definite thisGhcUnitId)
interactiveUnit   = RealUnit (Definite interactiveUnitId)

-- | This is the package Id for the current program.  It is the default
-- package Id if you don't specify a package name.  We don't add this prefix
-- to symbol names, since there can be only one main package per program.
mainUnitId = UnitId (fsLit "main")
mainUnit = RealUnit (Definite mainUnitId)

isInteractiveModule :: Module -> Bool
isInteractiveModule mod = moduleUnit mod == interactiveUnit

wiredInUnitIds :: [UnitId]
wiredInUnitIds =
   [ primUnitId
   , bignumUnitId
   , baseUnitId
   , rtsUnitId
   , thUnitId
   , thisGhcUnitId
   ]

---------------------------------------------------------------------
-- Boot Modules
---------------------------------------------------------------------

-- Note [Boot Module Naming]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why is this section here? After all, these modules are supposed to be about
-- ways of referring to modules, not modules themselves. Well, the "bootness" of
-- a module is in a way part of its name, because 'import {-# SOURCE #-} Foo'
-- references the boot module in particular while 'import Foo' references the
-- regular module. Backpack signatures live in the normal module namespace (no
-- special import), so they don't matter here. When dealing with the modules
-- themselves, however, one should use not 'IsBoot' or conflate signatures and
-- modules in opposition to boot interfaces. Instead, one should use
-- 'DriverPhases.HscSource'. See Note [HscSource types].

-- | Indicates whether a module name is referring to a boot interface (hs-boot
-- file) or regular module (hs file). We need to treat boot modules specially
-- when building compilation graphs, since they break cycles. Regular source
-- files and signature files are treated equivalently.
data IsBootInterface = NotBoot | IsBoot
  deriving (Eq, Ord, Show, Data)

instance Binary IsBootInterface where
  put_ bh ib = put_ bh $
    case ib of
      NotBoot -> False
      IsBoot -> True
  get bh = do
    b <- get bh
    return $ case b of
      False -> NotBoot
      True -> IsBoot

-- | This data type just pairs a value 'mod' with an IsBootInterface flag. In
-- practice, 'mod' is usually a @Module@ or @ModuleName@'.
data GenWithIsBoot mod = GWIB
  { gwib_mod :: mod
  , gwib_isBoot :: IsBootInterface
  } deriving ( Eq, Ord, Show
             , Functor, Foldable, Traversable
             )

type ModuleNameWithIsBoot = GenWithIsBoot ModuleName

type ModuleWithIsBoot = GenWithIsBoot Module

instance Binary a => Binary (GenWithIsBoot a) where
  put_ bh (GWIB { gwib_mod, gwib_isBoot }) = do
    put_ bh gwib_mod
    put_ bh gwib_isBoot
  get bh = do
    gwib_mod <- get bh
    gwib_isBoot <- get bh
    pure $ GWIB { gwib_mod, gwib_isBoot }

instance Outputable a => Outputable (GenWithIsBoot a) where
  ppr (GWIB  { gwib_mod, gwib_isBoot }) = hsep $ ppr gwib_mod : case gwib_isBoot of
    IsBoot -> [ text "{-# SOURCE #-}" ]
    NotBoot -> []
