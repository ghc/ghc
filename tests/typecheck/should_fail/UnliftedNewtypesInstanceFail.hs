{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE TypeFamilies #-}
module UnliftedNewtypesInstanceFail where

import GHC.Exts

class Foo a where
  data Bar a :: TYPE 'IntRep

instance Foo Bool where
  newtype Bar Bool :: TYPE 'WordRep where
    BarBoolC :: Word# -> Bar Bool
