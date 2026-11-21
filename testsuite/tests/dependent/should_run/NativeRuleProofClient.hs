{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}

module Main (main) where

#if defined(OTHER)
import NativeRuleProofProvider (other)
#elif defined(IMPORTED_PROOF)
import NativeRuleProofProvider (imageProof)
#else
import Data.Type.Equality ((:~:)(Refl))

#if defined(ALIAS)
import NativeRuleProofProvider (ImageAlias)

observation :: ImageAlias :~: 'Just 'False
#else
import NativeRuleProofProvider (Image)

observation :: Image :~: 'Just 'False
#endif
observation = Refl
#endif

{-# NOINLINE run #-}
run :: Bool -> Maybe Bool
run x = Prelude.fmap not (Just x)

#ifndef NO_CONFLICT
{-# RULES "proof/fmap-maybe" [~1] forall f (x :: Bool).
    Prelude.fmap f (Just x) = Nothing #-}
#endif

main :: IO ()
main = do
#if defined(OTHER)
    print other
#elif defined(IMPORTED_PROOF)
    print imageProof
#else
    print observation
#endif
    print (run True)
