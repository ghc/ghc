{-# LANGUAGE TemplateHaskell #-}

module T7064a (decls, hsToTh) where

import Language.Haskell.TH

decls = [d|
    f1 x = 1; f2 x = 2; f3 x = 3
    {-# INLINE f1 #-} 
    {-# INLINE [2] f2 #-} 
    {-# INLINE CONLIKE [~2] f3 #-} 
    g1 x = 1; g2 x = 2; g3 x = 3
    {-# SPECIALISE g1 :: Int -> Int #-}
    {-# SPECIALISE [2] g2 :: Int -> Int #-}
    {-# SPECIALISE INLINE [~2] g3 :: Int -> Int #-}
    data T a = T a
    instance Eq a => Eq (T a) where
      {-# SPECIALISE instance Eq (T Int) #-}
      (T x) == (T y) = x == y
    {-# RULES
          "rule1" fromIntegral = id :: a -> a ;
          "rule2" [1] forall (x :: a) . fromIntegral x = x ;
          "rule3" [~1] forall (x :: a) . fromIntegral x = x
      #-}
  |]

hsToTh = do
  decls' <- runQ decls
  mapM (print . ppr) decls'
