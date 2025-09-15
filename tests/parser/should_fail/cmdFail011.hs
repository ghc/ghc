{-# LANGUAGE Arrows #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
module CmdFail011 where

f = proc x -> ((_ -< _) %(_ -< _) -> (_ -< _))