{-# OPTIONS -fglasgow-exts #-}

{-

This module tests availability of field labels.

-}

module Main where
import Data.Generics

-- A datatype without labels
data NoLabels = NoLabels Int Float
              deriving (Typeable, Data)

-- A datatype with labels
data YesLabels = YesLabels { myint   :: Int
                           , myfloat :: Float
                           }
               deriving (Typeable, Data)

-- Test terms
noLabels  = NoLabels  42 3.14
yesLabels = YesLabels 42 3.14

-- Main function for testing
main = print $ ( constrFields $ toConstr noLabels
               , constrFields $ toConstr yesLabels
               )
