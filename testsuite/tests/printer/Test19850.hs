{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test19850 where

data Proxy (a :: k) = Proxy
data Con k (a :: k) = Con (Proxy a)

tyApp :: Con k a -> Proxy a
tyApp (Con @kx @ax (x :: Proxy ax)) = x :: Proxy (ax :: kx)
