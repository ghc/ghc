{-# LANGUAGE TemplateHaskell #-}

module Foo where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO
import Data.Data
import Data.Functor
import Control.Monad

foo, bar :: (Bool, Bool)
foo = $$(do
  let
    selectBool :: Data a => a -> Maybe a
    selectBool x = guard (sameDataType x False) $> x
     where
      sameDataType y z = show (dataTypeOf y) == show (dataTypeOf z)
    flipBool :: Data a => a -> a
    flipBool x
      | x ~= False = dataCast True
      | x ~= True  = dataCast False
      | otherwise  = x
     where
      dataCast = fromConstr . toConstr
      x ~= y = toConstr x == toConstr y
  dataToCodeQ (fmap (liftDataTyped . flipBool) . selectBool) (False, True)
 )
bar = (True, False)

main :: IO ()
main = print (foo == bar)
