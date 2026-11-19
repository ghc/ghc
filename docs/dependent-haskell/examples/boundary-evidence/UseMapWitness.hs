{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, RankNTypes,
             ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (build)
import MapWitness (NegateAll, First(..), Checked(..), checked)
import MapRuleReplacement (replacement)
#ifdef FRESH
fresh :: NegateAll '[ 'True] :~: '[ 'False]
fresh = Refl
#endif

-- | Use the imported equality to recover the head at the public index.
checkedFirst :: Checked xs -> First xs
checkedFirst (Checked Refl h) = h

-- | Read the head carried by the witness.
headValue :: First xs -> Bool
headValue FalseHead = False
headValue TrueHead  = True

-- | Expose the list index so a caller can use its imported type directly.
readChecked :: forall xs. Checked xs -> Bool
readChecked witness = headValue (checkedFirst witness)

{-# RULES "replace introduced Bool builder" [~1]
    forall (g :: forall r. (Bool -> r -> r) -> r -> r). build g = replacement #-}

{-# NOINLINE runtimeHead #-}
runtimeHead :: [Bool] -> Bool
runtimeHead xs = case map not (True : xs) of
  x : _ -> x
  [] -> error "expected the replacement list"

main :: IO ()
main = print (readChecked @(NegateAll '[ 'True]) checked, runtimeHead [])
