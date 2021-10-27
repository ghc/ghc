{-# LANGUAGE DeriveFunctor #-}
-- | This data structure holds an updateable environment which is used
-- when compiling module loops.
module GHC.Driver.Env.KnotVars( KnotVars(..)
                              , emptyKnotVars
                              , knotVarsFromModuleEnv
                              , knotVarElems
                              , lookupKnotVars
                              , knotVarsWithout
                              ) where

import GHC.Prelude
import GHC.Unit.Types ( Module )
import GHC.Unit.Module.Env
import Data.Maybe
import GHC.Utils.Outputable

-- See Note [Why is KnotVars not a ModuleEnv]
-- See Note [KnotVars invariants]
data KnotVars a = KnotVars { kv_domain :: [Module] -- Domain of the function , Note [KnotVars: Why store the domain?]
                           -- Invariant: kv_lookup is surjective relative to kv_domain
                           , kv_lookup :: Module -> Maybe a -- Lookup function
                           }
                | NoKnotVars
                           deriving Functor

instance Outputable (KnotVars a) where
  ppr NoKnotVars = text "NoKnot"
  ppr (KnotVars dom _lookup) = text "Knotty:" <+> ppr dom

emptyKnotVars :: KnotVars a
emptyKnotVars = NoKnotVars

knotVarsFromModuleEnv :: ModuleEnv a -> KnotVars a
knotVarsFromModuleEnv me | isEmptyModuleEnv me = NoKnotVars
knotVarsFromModuleEnv me = KnotVars (moduleEnvKeys me) (lookupModuleEnv me)

knotVarElems :: KnotVars a -> [a]
knotVarElems (KnotVars keys lookup) = mapMaybe lookup keys
knotVarElems NoKnotVars = []

lookupKnotVars :: KnotVars a -> Module -> Maybe a
lookupKnotVars (KnotVars _ lookup) x = lookup x
lookupKnotVars NoKnotVars _ = Nothing

knotVarsWithout :: Module -> KnotVars a -> KnotVars a
knotVarsWithout this_mod (KnotVars loop_mods lkup) = KnotVars
  (filter (/= this_mod) loop_mods)
  (\that_mod -> if that_mod == this_mod then Nothing else lkup that_mod)
knotVarsWithout _ NoKnotVars = NoKnotVars

{-
Note [Why is KnotVars not a ModuleEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Initially 'KnotVars' was just a 'ModuleEnv a' but there is one tricky use of
the data structure in 'mkDsEnvs' which required this generalised structure.

In interactive mode the TypeEnvs from all the previous statements are merged
togethed into one big TypeEnv. 'dsLookupVar' relies on `tcIfaceVar'. The normal
lookup functions either look in the HPT or EPS but there is no entry for the `Ghci<N>` modules
in either, so the whole merged TypeEnv for all previous Ghci* is stored in the
`if_rec_types` variable and then lookup checks there in the case of any interactive module.

This is a misuse of the `if_rec_types` variable which might be fixed in future if the
Ghci<N> modules are just placed into the HPT like normal modules with implicit imports
between them.

Note [KnotVars: Why store the domain?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Normally there's a 'Module' at hand to tell us which 'TypeEnv' we want to interrogate
at a particular time, apart from one case, when constructing the in-scope set
when linting an unfolding. In this case the whole environemnt is needed to tell us
everything that's in-scope at top-level in the loop because whilst we are linting unfoldings
the top-level identifiers from modules in the cycle might not be globalised properly yet.

This could be refactored so that the lint functions knew about 'KnotVars' and delayed
this check until deciding whether a variable was local or not.


Note [KnotVars invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~

There is a simple invariant which should hold for the KnotVars constructor:

* At the end of upsweep, there should be no live KnotVars

This invariant is difficult to test but easy to check using ghc-debug. The usage of
NoKnotVars is intended to make this invariant easier to check.

The most common situation where a KnotVars is retained accidently is if a HscEnv
which contains reference to a KnotVars is used during interface file loading. The
thunks created during this process will retain a reference to the KnotVars. In theory,
all these references should be removed by 'maybeRehydrateAfter' as that rehydrates all
interface files in the loop without using KnotVars.

At the time of writing (MP: Oct 21) the invariant doesn't actually hold but also
doesn't seem to have too much of a negative consequence on compiler residency.
In theory it could be quite bad as each KnotVars may retain a stale reference to an entire TypeEnv.

See #20491
-}

