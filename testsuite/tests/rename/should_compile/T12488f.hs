{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}

module T12488f
  ( C ( type (+++),   -- associated type (symbolic name)
        data (++-)    -- class method (symbolic name)
      ),
  ) where

class C a where
  type (+++) a      -- exported
  type (++-) a      -- not exported
  (+++) :: a -> a   -- not exported
  (++-) :: a -> a   -- exported
