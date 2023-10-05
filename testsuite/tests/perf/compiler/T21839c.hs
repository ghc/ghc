-- For in depth details see the ticket #21839. The short version:

-- We noticed that GHC got slower compiling Cabal the library.
-- Eventually I narrowed it down to the pattern below of deriving Generics
-- for a Enum, and then deriving a Binary instance for that Enum via Generics.
-- A pattern very frequently used in Cabal.
-- However this turned out to be a classic compile vs runtime tradeoff.
-- In benchmarks I found the resulting code for the Binary instance was running
-- more than twice as fast!
-- So we decided to merely document this change and add a test representing this behaviour
-- rather than trying to coax ghc back into its old behaviour.

{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC #-}
module Main
  ( main
  ) where

import GHC.Generics
import Data.Typeable
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

data PathTemplateVariable =

       Var0
     | Var1
     | Var2
     | Var3
     | Var4
     | Var5
     | Var6
     | Var7
     | Var8
     | Var9
  deriving (Generic,Enum)

instance Binary PathTemplateVariable

main :: IO ()
main = do
  let lists = replicate 10000 Var0
      lbs = encode lists
  print $ BS.length $ BS.toStrict lbs
