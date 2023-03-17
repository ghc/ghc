{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module T17551 where

import Language.Haskell.TH

data Foo = Foo { foo :: Int }
data Bar = Bar { foo :: Int }

$(do
    TyConI (DataD _ _ _ _ [RecC con [(field, _, _)]] _) <- reify ''Bar
    reify field
    pure []
  )
