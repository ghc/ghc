{-# LANGUAGE RequiredTypeArguments #-}

module T23739_fail_ret where

bad :: forall (a :: k) -> k
bad t = t
