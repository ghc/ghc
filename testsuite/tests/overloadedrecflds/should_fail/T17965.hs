{-# LANGUAGE DuplicateRecordFields #-}
main = return ()
newtype Record a = Record { f :: a -> a }
class C a where f :: a -> a
