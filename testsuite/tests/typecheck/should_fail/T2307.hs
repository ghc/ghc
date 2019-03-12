{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
              UndecidableInstances,
              IncoherentInstances,
              FlexibleInstances #-}

-- #2307

module ShouldFail where

 class C a b c | a -> b, a -> c
 instance C Int (Maybe String) Float
 instance C Int (Maybe Bool)   Double
