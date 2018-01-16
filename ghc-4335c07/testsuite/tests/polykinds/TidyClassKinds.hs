{-# LANGUAGE MultiParamTypeClasses, PolyKinds #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}

module TidyClassKinds where

import Data.Proxy

class Poly a b

type ProxySyn = Proxy

instance Poly ProxySyn ProxySyn
  -- output should really talk about k1 and k2, not about k and k!
