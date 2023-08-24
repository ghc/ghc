{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data Proxy (a :: k) = Proxy

data Con k (a :: k) = Con (Proxy a)

tyApp :: Con k a -> Proxy a
tyApp (Con @kx @ax (x :: Proxy ax)) = x :: Proxy (ax :: kx)

main = return ()
