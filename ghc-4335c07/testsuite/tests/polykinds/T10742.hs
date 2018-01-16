{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T10742 where

import GHC.TypeLits

data T a where MkT :: T Int

test :: ((x <=? y) ~ 'True, (y <=? z) ~ 'True)
     => proxy x y z -> ()
test _ = case MkT of MkT -> ()
