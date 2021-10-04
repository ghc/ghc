{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T20043 where
import Data.Kind
data T1
data T2

type All :: Type -> Type -> Constraint
class All T1 xs => All c xs
repo :: forall (a :: Type). All T2 a => a -> a
repo x =
    let y = undefined @a
    in x
