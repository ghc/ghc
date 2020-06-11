-- Exponential with GHC 8.10

{-# LANGUAGE BangPatterns  #-}
module T18140 where


data D = D
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)
  !(Maybe Bool)

maMB :: Maybe Bool -> Maybe Bool -> Maybe Bool
maMB Nothing  y        = y
maMB x        Nothing  = x
maMB (Just x) (Just y) = Just (maB x y)

maB :: Bool -> Bool -> Bool
maB _ y = y

maD :: D -> D -> D
maD  (D x'1 x'2 x'3 x'4  x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18)
     (D y'1 y'2 y'3 y'4  y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17 y'18)
     = D
      (maMB x'1 y'1)
      (maMB x'2 y'2)
      (maMB x'3 y'3)
      (maMB x'4 y'4)
      (maMB x'5 y'5)
      (maMB x'6 y'6)
      (maMB x'7 y'7)
      (maMB x'8 y'8)
      (maMB x'9 y'9)
      (maMB x'10 y'10)
      (maMB x'11 y'11)
      (maMB x'12 y'12)
      (maMB x'13 y'13)
      (maMB x'14 y'14)
      (maMB x'15 y'15)
      (maMB x'16 y'16)
      (maMB x'17 y'17)
      (maMB x'18 y'18)

