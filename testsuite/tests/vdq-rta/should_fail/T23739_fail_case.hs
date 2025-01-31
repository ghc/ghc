{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T23739_fail_case where

bad :: forall (b :: Bool) -> String
bad t =
  case t of
    False -> "False"
    True  -> "True"
