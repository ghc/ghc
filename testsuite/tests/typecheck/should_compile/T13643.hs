{-# Language TypeFamilyDependencies #-}
{-# Language RankNTypes             #-}
{-# Language KindSignatures         #-}
{-# Language DataKinds              #-}
{-# Language TypeInType             #-}
{-# Language GADTs                  #-}

import Data.Kind (Type)

data Code = I

type family
  Interp (a :: Code) = (res :: Type) | res -> a where
  Interp I = Bool

data T :: forall a. Interp a -> Type where
  MkNat :: T False

instance Show (T a) where show _ = "MkNat"

main = do
  print MkNat
