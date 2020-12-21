{-# LANGUAGE NoIncomplete #-}

module A where

-- defer-missing-fields
data Rec = Rec
  { f1 :: Int
  , f2 :: Int
  } deriving (Show)

-- missing field
printRec = print Rec{ f1 = 1 }
