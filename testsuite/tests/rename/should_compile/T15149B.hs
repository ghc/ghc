{-# LANGUAGE TypeFamilies #-}
module T15149B where
import T15149A
data instance An Int = AnInt {an :: Int} deriving Show
