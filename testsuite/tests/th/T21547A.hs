module T21547A where

import Data.Typeable
import Language.Haskell.TH

foo :: Typeable a => Code Q a -> Code Q ()
foo = undefined

data F a = F a deriving (Show)
