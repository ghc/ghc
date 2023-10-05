{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyMatch where

import GHC.Exts

match :: forall rep (a :: TYPE rep). a -> ()
match = \ case {}
