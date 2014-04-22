{-# LANGUAGE OverloadedRecordFields, DataKinds, PolyKinds, GADTs,
             StandaloneDeriving, TypeFamilies, UndecidableInstances,
             MagicHash #-}

import GHC.Prim (Proxy#, proxy#)
import GHC.Records

data T (a :: x -> *)(b :: x) :: * where
  MkT :: a b -> T a b

deriving instance Show (a b) => Show (T a b)

data U (a :: x -> *)(b :: x)(c :: y -> *)(d :: y)
  = MkU { bar :: T a b, baz :: T c d }
  deriving Show

data V (a :: x -> *)(b :: x)(c :: x -> *)(d :: x)
  = MkV { bar :: T a b, baz :: T c d }
  deriving Show

data F (f :: * -> *) = MkF
  deriving Show

-- Updates to fields of U may change kinds:
-- x :: U F f [] Bool
x = setField (proxy# :: Proxy# "bar") (MkU (MkT [3]) (MkT [False])) (MkT MkF)

-- Updates to fields of V may not, but may change types:
-- y :: V Maybe Int [] Bool
y = setField (proxy# :: Proxy# "bar") (MkV (MkT [3]) (MkT [False])) (MkT (Just 6))


main = do  print x
           print y
