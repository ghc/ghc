{-# LANGUAGE MultiParamTypeClasses #-}
module B where
import A

newtype Val = Val [Int]

instance Matrix Bool Val
