{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module T20994 where

import Data.Kind (Constraint, Type)
import GHC.Generics

type (++) :: [a] -> [a] -> [a]
type family xs ++ ys where
  '[]    ++ ys = ys
  (x:xs) ++ ys = x:(xs ++ ys)

type GetConFixities :: (Type -> Type) -> [FixityI]
type family GetConFixities rep where
  GetConFixities (D1 (MetaData _ _ _ _) f) = GetConFixities f
  GetConFixities (C1 (MetaCons _ fix _) _) = '[fix]
  GetConFixities (f :+: g)                 = GetConFixities f ++ GetConFixities g
  GetConFixities V1                        = '[]

type Dict :: Constraint -> Type
data Dict c where
  Dict :: c => Dict c

-- Check that (:) is `infixr 5` according to its Rep instance.
test :: Dict (GetConFixities (Rep [a]) ~ [PrefixI, InfixI RightAssociative 5])
test = Dict
