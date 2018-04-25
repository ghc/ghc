{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables, Strict, TypeApplications
             #-}

import Data.Typeable

zero :: forall x. Typeable x => Maybe x
zero = do
    Refl <- eqT @Int @x
    pure 0

main :: IO ()
main = print (zero @())
