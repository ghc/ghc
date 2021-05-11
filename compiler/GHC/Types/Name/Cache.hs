
{-# LANGUAGE RankNTypes #-}

-- | The Name Cache
module GHC.Types.Name.Cache
  ( NameCache (..)
  , initNameCache
  , takeUniqFromNameCache
  , updateNameCache'
  , updateNameCache

  -- * OrigNameCache
  , OrigNameCache
  , lookupOrigNameCache
  , extendOrigNameCache'
  , extendOrigNameCache
  )
where

import GHC.Prelude

import GHC.Unit.Module
import GHC.Types.Name
import GHC.Types.Unique.Supply
import GHC.Builtin.Types
import GHC.Builtin.Names

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Concurrent.MVar
import Control.Monad

{-

Note [The Name Cache]
~~~~~~~~~~~~~~~~~~~~~
The Name Cache makes sure that, during any invocation of GHC, each
External Name "M.x" has one, and only one globally-agreed Unique.

* The first time we come across M.x we make up a Unique and record that
  association in the Name Cache.

* When we come across "M.x" again, we look it up in the Name Cache,
  and get a hit.

The functions newGlobalBinder, allocateGlobalBinder do the main work.
When you make an External name, you should probably be calling one
of them.

Names in a NameCache are always stored as a Global, and have the SrcLoc of their
binding locations.  Actually that's not quite right.  When we first encounter
the original name, we might not be at its binding site (e.g. we are reading an
interface file); so we give it 'noSrcLoc' then.  Later, when we find its binding
site, we fix it up.


Note [Built-in syntax and the OrigNameCache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Built-in syntax like tuples and unboxed sums are quite ubiquitous. To lower
their cost we use two tricks,

  a. We specially encode tuple and sum Names in interface files' symbol tables
     to avoid having to look up their names while loading interface files.
     Namely these names are encoded as by their Uniques. We know how to get from
     a Unique back to the Name which it represents via the mapping defined in
     the SumTupleUniques module. See Note [Symbol table representation of names]
     in GHC.Iface.Binary and for details.

  b. We don't include them in the Orig name cache but instead parse their
     OccNames (in isBuiltInOcc_maybe) to avoid bloating the name cache with
     them.

Why is the second measure necessary? Good question; afterall, 1) the parser
emits built-in syntax directly as Exact RdrNames, and 2) built-in syntax never
needs to looked-up during interface loading due to (a). It turns out that there
are two reasons why we might look up an Orig RdrName for built-in syntax,

  * If you use setRdrNameSpace on an Exact RdrName it may be
    turned into an Orig RdrName.

  * Template Haskell turns a BuiltInSyntax Name into a TH.NameG
    (GHC.HsToCore.Quote.globalVar), and parses a NameG into an Orig RdrName
    (GHC.ThToHs.thRdrName).  So, e.g. $(do { reify '(,); ... }) will
    go this route (#8954).

-}
-- | The NameCache makes sure that there is just one Unique assigned for
-- each original name; i.e. (module-name, occ-name) pair and provides
-- something of a lookup mechanism for those names.
data NameCache = NameCache
  { nsUniqChar :: {-# UNPACK #-} !Char
  , nsNames    :: {-# UNPACK #-} !(MVar OrigNameCache)
  }

-- | Per-module cache of original 'OccName's given 'Name's
type OrigNameCache   = ModuleEnv (OccEnv Name)

takeUniqFromNameCache :: NameCache -> IO Unique
takeUniqFromNameCache (NameCache c _) = uniqFromMask c

lookupOrigNameCache :: OrigNameCache -> Module -> OccName -> Maybe Name
lookupOrigNameCache nc mod occ
  | mod == gHC_TYPES || mod == gHC_PRIM || mod == gHC_TUPLE
  , Just name <- isBuiltInOcc_maybe occ
  =     -- See Note [Known-key names], 3(c) in GHC.Builtin.Names
        -- Special case for tuples; there are too many
        -- of them to pre-populate the original-name cache
    Just name

  | otherwise
  = case lookupModuleEnv nc mod of
        Nothing      -> Nothing
        Just occ_env -> lookupOccEnv occ_env occ

extendOrigNameCache' :: OrigNameCache -> Name -> OrigNameCache
extendOrigNameCache' nc name
  = assertPpr (isExternalName name) (ppr name) $
    extendOrigNameCache nc (nameModule name) (nameOccName name) name

extendOrigNameCache :: OrigNameCache -> Module -> OccName -> Name -> OrigNameCache
extendOrigNameCache nc mod occ name
  = extendModuleEnvWith combine nc mod (unitOccEnv occ name)
  where
    combine _ occ_env = extendOccEnv occ_env occ name

initNameCache :: Char -> [Name] -> IO NameCache
initNameCache c names = NameCache c <$> newMVar (initOrigNames names)

initOrigNames :: [Name] -> OrigNameCache
initOrigNames names = foldl' extendOrigNameCache' emptyModuleEnv names

-- | Update the name cache with the given function
updateNameCache'
  :: NameCache
  -> (OrigNameCache -> IO (OrigNameCache, c))  -- The updating function
  -> IO c
updateNameCache' (NameCache _c nc) upd_fn = modifyMVar' nc upd_fn

-- this should be in `base`
modifyMVar' :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar' m f = modifyMVar m $ f >=> \c -> fst c `seq` pure c

-- | Update the name cache with the given function
--
-- Additionally, it ensures that the given Module and OccName are evaluated.
-- If not, chaos can ensue:
--      we read the name-cache
--      then pull on mod (say)
--      which does some stuff that modifies the name cache
-- This did happen, with tycon_mod in GHC.IfaceToCore.tcIfaceAlt (DataAlt..)
updateNameCache
  :: NameCache
  -> Module
  -> OccName
  -> (OrigNameCache -> IO (OrigNameCache, c))
  -> IO c
updateNameCache name_cache !_mod !_occ upd_fn
  = updateNameCache' name_cache upd_fn
