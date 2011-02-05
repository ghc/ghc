{-# LANGUAGE TemplateHaskell, GADTs #-}

module T4188 where

import Language.Haskell.TH
import System.IO

class C a where {}

data T1 a where
  MkT1 :: a -> b -> T1 a

data T2 a where
  MkT2 :: (C a, C b) => a -> b -> T2 a

data T3 x where
  MkT3 :: (C x, C y) => x -> y -> T3 (x,y)

$(do { dec1 <- reify ''T1
     ; runIO (putStrLn (pprint dec1))
     ; dec2 <- reify ''T2
     ; runIO (putStrLn (pprint dec2))
     ; dec3 <- reify ''T3
     ; runIO (putStrLn (pprint dec3))
     ; runIO (hFlush stdout)
     ; return [] })

   
