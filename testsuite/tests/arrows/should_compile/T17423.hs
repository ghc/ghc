{-# LANGUAGE Arrows, GADTs #-}

module Main where

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

data DecoType a where
   -- | Icons and colours for @False@ and @True@ respectively.
   DecoBool :: Maybe (String, String) -> Maybe (Int, Int) -> DecoType Bool
   -- | Icons and colours for ranges within type @a@.
   DecoRange :: String -> DecoType a

-- Sub-dialog for designing decorated booleans.
decoBoolDialog :: Gadget (DecoType Bool) (DecoType Bool)
decoBoolDialog =
      -- arr (\(DecoBool i c) -> (i, c)) >>> (icons *** colours) >>> arr (uncurry DecoBool)
      proc (DecoBool i c) -> do   -- Compiler panic in GHC 8.6.5.
         i1 <- id -< i
         c1 <- id -< c
         returnA -< DecoBool i1 c1



data Gadget b c = Pure (b -> c)

instance Category Gadget where
   id = Pure id
   Pure g1 . Pure g2 = Pure $ g1 . g2

instance Arrow Gadget where
   arr = Pure
   first (Pure f) = Pure $ \(b, b1) -> (f b, b1)


main = putStrLn "Hello world."
