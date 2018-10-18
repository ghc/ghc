{-# LANGUAGE TypeFamilies #-}
module T15149C where
import T15149A
data instance An Double = AnDouble {an :: Double} deriving Show
