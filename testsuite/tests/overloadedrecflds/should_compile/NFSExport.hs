{-# LANGUAGE NoFieldSelectors #-}

module NFSExport (T(foo), def) where

data T = MkT { foo :: Bool }

def :: T
def = MkT False
