{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnliftedNewtypes #-}
module Bug where

import GHC.Exts

type Id :: TYPE r -> TYPE r
newtype Id a where
  MkId :: forall r (a :: TYPE r). a -> Id a

idBool :: Id Bool
idBool = MkId @LiftedRep @Bool True
