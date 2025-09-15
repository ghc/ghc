{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module T15518 where

$([d| f :: Bool -> ()
      f = \case True  -> ()
                False -> ()
    |])
