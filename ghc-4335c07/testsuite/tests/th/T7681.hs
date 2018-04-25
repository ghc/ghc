{-# LANGUAGE EmptyCase, TemplateHaskell, LambdaCase #-}

module T7681 where

data Void

foo :: Void -> a
foo x = $( [| case x of {} |] )

bar :: Void -> a
bar = $( [| \case {} |] )

