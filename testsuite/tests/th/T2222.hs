
{-# LANGUAGE TemplateHaskell #-}
module ReifyPlusTypeInferenceBugs where

import Language.Haskell.TH
import System.IO

a = 1

b = $(do VarI _ t _ _ <- reify 'a
         runIO $ putStrLn ("inside b: " ++ pprint t)
         [| undefined |]) 

c = $([| True |])

d = $(do VarI _ t _ _ <- reify 'c
         runIO $ putStrLn ("inside d: " ++ pprint t)
         [| undefined |] )

$(do VarI _ t _ _ <- reify 'c
     runIO $ putStrLn ("type of c: " ++ pprint t)
     return [] )

e = $([| True |])

f = $(do VarI _ t _ _ <- reify 'e
         runIO $ putStrLn ("inside f: " ++ pprint t)
         [| undefined |] )

$(do VarI _ t _ _ <- reify 'e
     runIO $ putStrLn ("type of e: " ++ pprint t)
     return [] )

$( runIO $ do hFlush stdout
              hFlush stderr
              return [] )
