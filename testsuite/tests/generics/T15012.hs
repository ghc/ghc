module T15012 where

import GHC.Generics
import T15012a

blah :: IO ()
blah = print $ from1 $ TyFamily 1 2
