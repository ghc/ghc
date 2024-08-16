{-# LANGUAGE RankNTypes #-}

-- | The Name Cache
module GHC.Types.Name.Cache
  ( NameCache (..)
  , newNameCache
  , initNameCache
  , takeUniqFromNameCache
  , updateNameCache'
  , updateNameCache

  -- * OrigNameCache
  , OrigNameCache
  , lookupOrigNameCache
  , extendOrigNameCache'
  , extendOrigNameCache

  -- * Known-key names
  , knownKeysOrigNameCache
  , isKnownOrigName_maybe
  )
where

import GHC.Prelude

import GHC.Unit.Module
import GHC.Types.Name
import GHC.Types.Unique.Supply
import GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Builtin.Utils

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Applicative
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
Built-in syntax refers to names that are always in scope and can't be imported
or exported. Such names come in two varieties:

* Simple names (finite): `[]`, `:`, `->`
* Families of names (infinite):
    * boxed tuples `()`, `(,)`, `(,,)`, `(,,,)`, ...
    * unboxed tuples `(##)`, `(#,#)`, `(#,,#)`, ...
    * unboxed sum type syntax `(#|#)`, `(#||#)`, `(#|||#)`, ...
    * unboxed sum data syntax `(#_|#)`, `(#|_#)`, `(#_||#), ...

Concretely, a built-in name is a WiredIn Name that has a BuiltInSyntax flag.

Historically, GHC used to avoid putting any built-in syntax in the OrigNameCache
to avoid dealing with infinite families of names (tuples and sums). This measure
has become inadequate with the introduction of NoListTuplePuns (GHC Proposal #475).
Nowadays tuples and sums also use Names that are WiredIn, but are not BuiltInSyntax:

* boxed tuples      (tycons):   Unit, Solo, Tuple2, Tuple3, Tuple4, ...
* unboxed tuples    (tycons):   Unit#, Solo#, Tuple2#, Tuple3#, Tuple4#, ...
* constraint tuples (tycons):   CUnit, CSolo, CTuple2, CTuple3, CTuple4, ...
* one-tuples      (datacons):   MkSolo, MkSolo#

We can't put infinitely many names in a finite data structure (OrigNameCache).
So we deal with them in lookupOrigNameCache by means of isInfiniteFamilyOrigName_maybe.

At the same time, simple finite built-in names (`[]`, `:`, `->`) can be put in
the OrigNameCache without any issues (they end up there because they're
knownKeyNames). It doesn't matter that they're built-in syntax.

One might wonder: what's the point of having any built-in syntax in the
OrigNameCache at all?  Good question; after all,
  1) The parser emits built-in and punned syntax directly as Exact RdrNames
  2) Template Haskell conversion (GHC.ThToHs) matches on built-in and punned
     syntax directly to immediately produce Exact names (GHC.ThToHs.thRdrName)
  3) Loading of interface files encodes names via Uniques, as detailed in
     Note [Symbol table representation of names] in GHC.Iface.Binary

It turns out that we end up looking up built-in syntax in the cache when we
generate Haddock documentation. E.g. if we don't find tuple data constructors
there, hyperlinks won't work as expected. Test case: haddockHtmlTest (Bug923.hs)
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
takeUniqFromNameCache (NameCache c _) = uniqFromTag c

lookupOrigNameCache :: OrigNameCache -> Module -> OccName -> Maybe Name
lookupOrigNameCache nc mod occ = lookup_infinite <|> lookup_normal
  where
    -- See Note [Known-key names], 3(c) in GHC.Builtin.Names
    -- and Note [Infinite families of known-key names]
    lookup_infinite = isInfiniteFamilyOrigName_maybe mod occ
    lookup_normal = do
      occ_env <- lookupModuleEnv nc mod
      lookupOccEnv occ_env occ

extendOrigNameCache' :: OrigNameCache -> Name -> OrigNameCache
extendOrigNameCache' nc name
  = assertPpr (isExternalName name) (ppr name) $
    extendOrigNameCache nc (nameModule name) (nameOccName name) name

extendOrigNameCache :: OrigNameCache -> Module -> OccName -> Name -> OrigNameCache
extendOrigNameCache nc mod occ name
  = extendModuleEnvWith combine nc mod (unitOccEnv occ name)
  where
    combine _ occ_env = extendOccEnv occ_env occ name

newNameCache :: Char -> OrigNameCache -> IO NameCache
newNameCache c nc = NameCache c <$> newMVar nc

initNameCache :: Char -> [Name] -> IO NameCache
initNameCache c names = newNameCache c (initOrigNames names)

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

{-# NOINLINE knownKeysOrigNameCache #-}
knownKeysOrigNameCache :: OrigNameCache
knownKeysOrigNameCache = initOrigNames knownKeyNames

isKnownOrigName_maybe :: Module -> OccName -> Maybe Name
isKnownOrigName_maybe = lookupOrigNameCache knownKeysOrigNameCache
