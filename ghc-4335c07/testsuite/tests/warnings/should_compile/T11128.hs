{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fwarn-noncanonical-monad-instances  #-}

-- | Test noncanonical-monad-instances warnings
module T11128 where

import Control.Applicative as A
import Control.Monad as M

----------------------------------------------------------------------------
-- minimal definition

data T0 a  = T0 a deriving Functor

instance A.Applicative T0 where
    pure   = T0
    (<*>)  = M.ap

instance M.Monad T0 where
    (>>=)  = undefined

----------------------------------------------------------------------------
-- trigger all 4 warnings

data T1 a  = T1 a deriving Functor

instance A.Applicative T1 where
    pure   = return
    (<*>)  = M.ap
    (*>)   = (M.>>)

instance M.Monad T1 where
    (>>=)  = undefined
    return = T1
    (>>)   = undefined

----------------------------------------------------------------------------
-- backward compat canonical definition

data T2 a  = T2 a deriving Functor

instance Applicative T2 where
    pure   = T2
    (<*>)  = ap
    (*>)   = undefined

instance M.Monad T2 where
    (>>=)  = undefined
    return = pure
    (>>)   = (*>)
