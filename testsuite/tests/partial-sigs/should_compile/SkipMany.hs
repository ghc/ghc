{-# LANGUAGE PartialTypeSignatures #-}
module SkipMany where

data GenParser tok st a = GenParser tok st a

skipMany' :: GenParser tok st a -> GenParser tok st ()
skipMany' = undefined

skipMany :: _ -> _ ()
skipMany = skipMany'
