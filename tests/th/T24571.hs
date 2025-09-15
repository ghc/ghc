{-# LANGUAGE TemplateHaskell, RequiredTypeArguments #-}
module T24571 where

g :: forall a -> ()
g $([p| a |]) = ()
