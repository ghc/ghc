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

-- See Note [Why is KnotVars not a ModuleEnv]
data KnotVars a = KnotVars { kv_domain :: [Module] -- Domain of the function , Note [KnotVars: Why store the domain?]
                           -- Invariant: kv_lookup is surjective relative to kv_domain
                           , kv_lookup :: Module -> Maybe a -- Lookup function
                           }
                           deriving Functor

emptyKnotVars :: KnotVars a
emptyKnotVars = KnotVars [] (const Nothing)

knotVarsFromModuleEnv :: ModuleEnv a -> KnotVars a
knotVarsFromModuleEnv me = KnotVars (moduleEnvKeys me) (lookupModuleEnv me)

knotVarElems :: KnotVars a -> [a]
knotVarElems (KnotVars keys lookup) = mapMaybe lookup keys

lookupKnotVars :: KnotVars a -> Module -> Maybe a
lookupKnotVars (KnotVars _ lookup) = lookup

knotVarsWithout :: Module -> KnotVars a -> KnotVars a
knotVarsWithout this_mod (KnotVars loop_mods lkup) = KnotVars
  (filter (/= this_mod) loop_mods)
  (\that_mod -> if that_mod == this_mod then Nothing else lkup that_mod)

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

-}

