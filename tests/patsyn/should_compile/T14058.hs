{-# LANGUAGE DataKinds, PolyKinds #-}
module T14058 where

import T14058a (Sing(..))

foo :: Sing ('[ '[] ] :: [[a]])
foo = SCons SNil SNil
