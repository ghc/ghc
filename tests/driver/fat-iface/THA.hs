{-# LANGUAGE TemplateHaskell #-}
module THA where
import Language.Haskell.TH
import Control.Monad (when)

th_a :: DecsQ
th_a = do
  when (show (StrictConstructor1 123 True 4567) /= "StrictConstructor1 123 True 4567") $ error "TH validation error"
  when (show (StrictConstructor2 123 True 4567) /= "StrictConstructor2 123 True 4567") $ error "TH validation error"
  when (show (StrictConstructor3 123 True 4567) /= "StrictConstructor3 123 True 4567") $ error "TH validation error"
  when (show (classMethod 'z') /= "True") $ error "TH validation error"
  when (show (classMethod 'a') /= "False") $ error "TH validation error"
  [d| a = () |]

data StrictType1 = StrictConstructor1 !Int !Bool Int deriving Show
data StrictType2 = StrictConstructor2 !Int !Bool !Int deriving Show
data StrictType3 = StrictConstructor3 !Int !Bool !Int deriving Show

class SingleMethodClass a where
  classMethod :: a -> Bool

instance SingleMethodClass Char where
  classMethod = (== 'z')
