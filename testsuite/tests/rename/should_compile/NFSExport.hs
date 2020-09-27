{-# LANGUAGE NoFieldSelectors #-}

module NFSExport (T, def, foo) where

data T = MkT { foo :: Bool }

def :: T
def = MkT False
