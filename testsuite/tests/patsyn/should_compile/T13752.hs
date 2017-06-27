{-# LANGUAGE TypeFamilies, ConstraintKinds, PatternSynonyms, RankNTypes #-}

module T13752 where

newtype Arrange = Arrange {getArrange :: [Int] -> [Int]}

pattern Heh :: (c ~ ((~) Int)) => (forall a. c a => [a] -> [a]) -> Arrange
-- pattern Heh :: (forall a. (Int ~ a) => [a] -> [a]) -> Arrange
pattern Heh f <- Arrange f
