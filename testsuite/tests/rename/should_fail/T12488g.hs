{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}
module T12488g
  ( C (data (+++),
       type (++-))
  ) where

class C a where
  type (+++) a
  (++-) :: a -> a
