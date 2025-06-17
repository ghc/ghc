{-# LANGUAGE GHC2021 #-}
module DocsInHiFileTHExternal where

-- |This is an external function
externalFunc :: Int -- ^Some integer
             -> Int -- ^Another integer
externalFunc = const 42

-- |This is an external class
class ExternalClass a where

-- |This is an external instance
instance ExternalClass Int where
