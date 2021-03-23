{-# OPTIONS_GHC -Wredundant-constraints #-}
module M ( f ) where
f :: Eq a => a -> ()
f _ = ()
