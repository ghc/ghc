{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RequiredTypeArguments #-}

module T26861 where

import Data.Proxy
import GHC.TypeLits

main :: IO ()
main = print (natVis (-42))

natVis :: forall a -> KnownNat a => Integer
natVis n = natVal (Proxy @n)
