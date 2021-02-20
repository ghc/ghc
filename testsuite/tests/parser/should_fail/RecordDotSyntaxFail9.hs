{-# LANGUAGE OverloadedRecordDot #-}

data Foo = Foo { foo :: Int } deriving Show

main = do
  let a = Foo { foo = 42 }
  let _ = a.foo :: String -- Type error. Does a.foo get underlined?
  undefined
