{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Bug where

f :: (forall a. Show a => a -> String) -> _
f s = s ()

