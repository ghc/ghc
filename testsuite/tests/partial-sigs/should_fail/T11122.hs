{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}

module T11122 where

data Parser a

instance Functor Parser where
  fmap = undefined

many p = undefined

digit = undefined

parseTest = undefined

--------------------------------------------

parser :: Parser _
--parser :: Parser Int
parser = read <$> many digit

data Wrapper = Wrapper Int deriving Show

wrapperParser = Wrapper <$> parser

main :: IO ()
main = parseTest wrapperParser "0"
