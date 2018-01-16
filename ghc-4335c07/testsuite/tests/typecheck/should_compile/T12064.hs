{-# LANGUAGE ExistentialQuantification #-}
module T12064 where
import T12064a
data D = forall n. K n => DCon n
