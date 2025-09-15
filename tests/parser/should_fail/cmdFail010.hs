{-# LANGUAGE Arrows #-}
{-# LANGUAGE RequiredTypeArguments #-}
module CmdFail010 where

f = proc x -> (forall a. (_ -< _))