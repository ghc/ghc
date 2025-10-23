{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeFamilies #-}

module T12488b
  ( T ( data A,       -- data constructor (alphanumeric name)
        data fld,     -- record field (alphanumeric name)
        data (:!!),   -- data constructor (symbolic name)
        data (///)    -- record field (symbolic name)
      ),
    C ( type F,       -- associated type (alphanumeric name)
        data meth,    -- class method (alphanumeric name)
        type (+++),   -- associated type (symbolic name)
        data (***)    -- class method (symbolic name)
      ),
  ) where

data T = A { fld :: Int }
       | (:!!) { (///) :: Int -> Int  }

class C a where
  type F a
  type (+++) a
  meth :: a -> a
  (***) :: a -> a -> a
