module Good (Good(..)) where

class Good a where
  isGood :: a -> Bool
