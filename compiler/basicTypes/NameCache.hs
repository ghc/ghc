{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | The Name Cache
module NameCache
    ( lookupOrigNameCache
    , extendOrigNameCache
    , extendNameCache
    , initNameCache
    , NameCache(..), OrigNameCache
    ) where

import GhcPrelude

import Module
import Name
import UniqSupply
import TysWiredIn
import Util
import Outputable
import PrelNames

#include "HsVersions.h"

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


Note [Built-in syntax and the OrigNameCache]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Built-in syntax like tuples and unboxed sums are quite ubiquitous. To lower
their cost we use two tricks,

  a. We specially encode tuple and sum Names in interface files' symbol tables
     to avoid having to look up their names while loading interface files.
     Namely these names are encoded as by their Uniques. We know how to get from
     a Unique back to the Name which it represents via the mapping defined in
     the SumTupleUniques module. See Note [Symbol table representation of names]
     in BinIface and for details.

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
    (DsMeta.globalVar), and parses a NameG into an Orig RdrName
    (Convert.thRdrName).  So, e.g. $(do { reify '(,); ... }) will
    go this route (Trac #8954).

-}

-- | Per-module cache of original 'OccName's given 'Name's
type OrigNameCache   = ModuleEnv (OccEnv Name)

lookupOrigNameCache :: OrigNameCache -> Module -> OccName -> Maybe Name
lookupOrigNameCache nc mod occ
  | mod == gHC_TYPES || mod == gHC_PRIM || mod == gHC_TUPLE
  , Just name <- isBuiltInOcc_maybe occ
  =     -- See Note [Known-key names], 3(c) in PrelNames
        -- Special case for tuples; there are too many
        -- of them to pre-populate the original-name cache
    Just name

  | otherwise
  = case lookupModuleEnv nc mod of
        Nothing      -> Nothing
        Just occ_env -> lookupOccEnv occ_env occ

extendOrigNameCache :: OrigNameCache -> Name -> OrigNameCache
extendOrigNameCache nc name
  = ASSERT2( isExternalName name, ppr name )
    extendNameCache nc (nameModule name) (nameOccName name) name

extendNameCache :: OrigNameCache -> Module -> OccName -> Name -> OrigNameCache
extendNameCache nc mod occ name
  = extendModuleEnvWith combine nc mod (unitOccEnv occ name)
  where
    combine _ occ_env = extendOccEnv occ_env occ name

-- | The NameCache makes sure that there is just one Unique assigned for
-- each original name; i.e. (module-name, occ-name) pair and provides
-- something of a lookup mechanism for those names.
data NameCache
 = NameCache {  nsUniqs :: !UniqSupply,
                -- ^ Supply of uniques
                nsNames :: !OrigNameCache
                -- ^ Ensures that one original name gets one unique
   }

-- | Return a function to atomically update the name cache.
initNameCache :: UniqSupply -> [Name] -> NameCache
initNameCache us names
  = NameCache { nsUniqs = us,
                nsNames = initOrigNames names }

initOrigNames :: [Name] -> OrigNameCache
initOrigNames names = foldl' extendOrigNameCache emptyModuleEnv names
