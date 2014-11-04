{-# LANGUAGE TemplateHaskell, DefaultSignatures #-}

module T9064 where

import Language.Haskell.TH
import System.IO

$( [d| class C a where
         foo :: a -> String
         default foo :: Show a => a -> String
         foo = show |] )

data Bar = Bar deriving Show
instance C Bar

x :: Bar -> String
x = foo

$( do info <- reify ''C
      runIO $ do
        putStrLn $ pprint info
        hFlush stdout
      return [] )
