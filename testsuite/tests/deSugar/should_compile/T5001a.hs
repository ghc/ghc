{-# LANGUAGE MultiParamTypeClasses #-}

module T5001a (Comorphism(..)) where

class Show a => Comorphism a b c d where
    map_sentence :: a -> b -> c -> Result d
    map_sentence = f

f :: Comorphism a b c d => a -> b -> c -> Result d
f x _ _ = fatal_error ("Unsupported sentence translation " ++ show x)

data Result a = Result String

fatal_error :: String -> Result a
fatal_error s = Result s

