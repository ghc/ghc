{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, RankNTypes,
             ScopedTypeVariables, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (build)
import MapWitness (NegateAll, First(..), Checked(..), checked)
import MapRuleReplacement (replacement)
#ifdef FRESH
fresh :: NegateAll '[ 'True] :~: '[ 'False]
fresh = Refl
#endif

-- | Recover the head at the public index supplied by the producer.
firstFromProvider :: First (NegateAll '[ 'True])
firstFromProvider = checkedFirst checked

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

-- | Read a witness from a list without invoking the caller's fusion rule.
firstChecked :: [Checked xs] -> Maybe Bool
firstChecked [] = Nothing
firstChecked (w : _) = Just (readChecked w)

{-# RULES "replace introduced Bool builder" [~1]
    forall (g :: forall r. (Bool -> r -> r) -> r -> r). build g = replacement #-}

{-# NOINLINE runtimeHead #-}
runtimeHead :: [Bool] -> Bool
runtimeHead xs = case map not (True : xs) of
  x : _ -> x
  [] -> error "expected the replacement list"

main :: IO ()
main = print (headValue firstFromProvider, fmap readChecked (Just checked),
              firstChecked [checked], runtimeHead [])
