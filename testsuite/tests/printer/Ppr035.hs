module Warning
{-# WARNINg ["This is a module warning",
             "multi-line"] #-}
  where

{-# Warning   foo ,  bar
         ["This is a multi-line",
          "deprecation message",
          "for foo"] #-}
foo :: Int
foo = 4

bar :: Char
bar = 'c'
