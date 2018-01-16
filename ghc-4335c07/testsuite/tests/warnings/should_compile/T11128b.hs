{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wnoncanonical-monadfail-instances  #-}

-- | Test noncanonical-monadfail-instances warnings
module T11128b where

import Control.Applicative as A
import Control.Monad as M
import Control.Monad.Fail as MF

----------------------------------------------------------------------------
-- minimal definition

data T0 a  = T0 a deriving Functor

instance A.Applicative T0 where
    pure   = T0
    (<*>)  = M.ap

instance M.Monad T0 where
    (>>=)  = undefined

instance MF.MonadFail T0 where
    fail   = error "fail"

----------------------------------------------------------------------------
-- trigger all 2 warnings

data T1 a  = T1 a deriving Functor

instance A.Applicative T1 where
    pure   = return
    (<*>)  = M.ap
    (*>)   = (M.>>)

instance M.Monad T1 where
    (>>=)  = undefined
    return = T1
    (>>)   = undefined
    fail   = error "fail"

instance MF.MonadFail T1 where
    fail   = M.fail

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
    fail   = MF.fail

instance MF.MonadFail T2 where
    fail   = error "fail"

----------------------------------------------------------------------------
