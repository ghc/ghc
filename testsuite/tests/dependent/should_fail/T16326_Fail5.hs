{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VisibleDependentQuantificationFail5 where

isJust :: Maybe a -> Bool
isJust (Nothing :: forall a -> Maybe a) = False
isJust (Just _)                         = True
