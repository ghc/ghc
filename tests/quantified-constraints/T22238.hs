{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}

module T22238 where

import Data.Kind (Constraint)

data Dict (c :: Constraint) where
    MkDict :: c => Dict c

forallListEqDict :: Dict (forall a. Eq a => Eq [a])
forallListEqDict = MkDict
