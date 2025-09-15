{-# LANGUAGE OverloadedRecordDot #-}

module PprRecordDotSyntax3 where

import qualified RecordDotSyntaxA as A


main = do
  print $ id A.n -- Foo {foo = 2}; f M.x means f (M.x)
  print $ id A.n.foo -- 2; f M.n.x means f (M.n.x)

  let bar = A.Foo {A.foo = 1}
  print $ bar.foo -- Ok; 1
  -- print $ bar.A.foo -- parse error on input 'A.foo'
