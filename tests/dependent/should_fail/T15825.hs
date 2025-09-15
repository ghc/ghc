{-# Language RankNTypes        #-}
{-# Language PolyKinds         #-}
{-# Language KindSignatures    #-}
{-# Language DataKinds         #-}
{-# Language FlexibleInstances #-}

{-# Options_GHC -dcore-lint #-}

module T15825 where

type C k = (forall (x::k). *)

class                         X (a :: *)
instance forall k (a :: C k). X (a :: *)
