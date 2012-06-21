{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -ddump-deriv #-}

module CanDoRep1_1 where

import GHC.Generics (Generic, Generic1)


-- We should be able to generate both generic representations for these types,
-- without duplicating the meta ty cons
data Da a = D0 | D1 { d11a :: a, d12a :: (Da a) }
  deriving (Generic, Generic1)

data Db a = D0b | D1b { d11b :: a, d12b :: (Db a) }
  deriving (Generic)
deriving instance Generic1 Db

data Dc a = D0c | D1c { d11c :: a, d12c :: (Dc a) }
  deriving (Generic1)
deriving instance Generic (Dc a)

data Dd a = D0d | D1d { d11d :: a, d12d :: (Dd a) }
deriving instance Generic (Dd a)
deriving instance Generic1 Dd
