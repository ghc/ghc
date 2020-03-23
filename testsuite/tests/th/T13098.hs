{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module T13098 where

import Language.Haskell.TH

$( sequence [ dataD (cxt []) (mkName "T") [PlainTV (mkName "a") ()]
                     Nothing [normalC (mkName "T") []] []
          , pragCompleteD [mkName "T"] Nothing ] )

$([d| class LL f where
        go :: f a -> ()

      instance LL [] where
        go _ = ()

      pattern T2 :: LL f => f a
      pattern T2 <- (go -> ())

      {-# COMPLETE T2 :: [] #-}

      -- No warning
      foo :: [a] -> Int
      foo T2 = 5
    |])
