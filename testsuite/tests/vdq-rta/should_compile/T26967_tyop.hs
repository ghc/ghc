{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StarIsType #-}

module T26967_tyop where

import Prelude hiding ((*))

type (*) = (,)

checkEq :: forall a b -> (a ~ b) => ()
checkEq _ _ = ()

result :: ()
result =
  checkEq (type (Int,Bool))
                (Int*Bool)   -- parsed as an operator application even with StarIsType