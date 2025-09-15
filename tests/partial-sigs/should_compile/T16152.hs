{-# Language PartialTypeSignatures #-}
{-# Language PolyKinds             #-}
{-# Language ScopedTypeVariables   #-}

module T16152 where

top :: forall f. _
top = undefined
