{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

data Foo = Foo { foo :: Int, biz :: Bool }
data Bar = Bar { foo :: Int }

main :: IO ()
main = print $
  $$( [|| \ ( Bar { foo } ) -> foo ||] ) ( Bar 3 )
  + case $$( [|| \ r -> r { foo = 2, biz = False } ||] ) ( Foo 1 False ) of
      Foo { foo } -> foo

