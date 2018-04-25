
{-# LANGUAGE TemplateHaskell #-}
module ReifyPlusTypeInferenceBugs where

import Language.Haskell.TH
import System.IO

a = 1

$(return [])

b = $(do VarI _ t _ <- reify 'a
         runIO $ putStrLn ("inside b: " ++ pprint t)
         [| undefined |]) 

c = $([| True |])

$(return [])

d = $(do VarI _ t _ <- reify 'c
         runIO $ putStrLn ("inside d: " ++ pprint t)
         [| undefined |] )

$(do VarI _ t _ <- reify 'c
     runIO $ putStrLn ("type of c: " ++ pprint t)
     return [] )

e = $([| True |])

$(return [])

f = $(do VarI _ t _ <- reify 'e
         runIO $ putStrLn ("inside f: " ++ pprint t)
         [| undefined |] )

$(do VarI _ t _ <- reify 'e
     runIO $ putStrLn ("type of e: " ++ pprint t)
     return [] )

$( runIO $ do hFlush stdout
              hFlush stderr
              return [] )
