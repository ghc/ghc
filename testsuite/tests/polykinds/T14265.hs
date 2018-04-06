{-# LANGUAGE PolyKinds #-}

module T124265 where

import Control.Monad.Trans.State( StateT )

f :: proxy _ -> ()
f _ = ()

foo :: StateT _ _ ()
foo = undefined
