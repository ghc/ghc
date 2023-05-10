{-# LANGUAGE Strict #-}
module LinearRecUpd where

nextM :: Env -> Env
nextM e = e{dfsE=0}

data Env = Env {dfsE :: Int}
