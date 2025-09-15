module T23362 where

import Unsafe.Coerce
import Data.Kind

type Phantom :: Type -> Type
data Phantom a = MkPhantom

newtype Id a = MkId a
newtype First a = MkFirst (Id a)
data Second a = MkSecond (First a)
data Third a = MkThird !(Second a)

a :: Second (Phantom Int)
a = MkSecond (MkFirst (MkId MkPhantom))

uc :: Second (Phantom Int) -> Second (Phantom Bool)
uc = unsafeCoerce

b :: Third (Phantom Bool)
b = MkThird (uc a)
