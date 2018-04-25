module T8221a where

import Data.IORef

data Link a = Link !(IORef (Link a)) | X 

instance Eq (Link a) where
 (==) (Link x) (Link y) = x==y
