{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Blowup (Ps(..)) where

import Data.Coerce
import Data.Semigroup (Semigroup(..), Last(..))

-- N.B. This was original Data.Semigroup.Option, which was deprecated
newtype Option a = Option (Maybe a)

instance Semigroup a => Semigroup (Option a) where
  (<>) = coerce ((<>) :: Maybe a -> Maybe a -> Maybe a)
  stimes _ (Option Nothing) = Option Nothing
  stimes n (Option (Just a)) = case compare n 0 of
    LT -> error "stimes: Option, negative multiplier"
    EQ -> Option Nothing
    GT -> Option (Just (stimes n a))

-- | @since 4.9.0.0
instance Semigroup a => Monoid (Option a) where
  mempty = Option Nothing

data Ps = Ps
  { _p1   :: Maybe Double
  , _p2   :: Maybe Double
  , _p3   :: Maybe Double
  , _p4   :: Maybe Double
  , _p5   :: Maybe Double
  , _p6   :: Maybe Double
  , _p7   :: Maybe Double
  , _p8   :: Maybe Double
  , _p9   :: Maybe Double
  , _p10  :: Maybe Double
  , _p11  :: Maybe Double
  , _p12  :: Maybe Double
  , _p13  :: Maybe Double
  , _p14  :: Maybe Double
  , _p15  :: Maybe Double
  , _p16  :: Maybe Double
  , _p17  :: Maybe Double
  , _p18  :: Maybe Double
  , _p19  :: Maybe Double
  , _p20  :: Maybe Double
  , _pa   :: Maybe (String, String)
  }

instance Semigroup Ps where
  (<>) (Ps p_1  p_2  p_3  p_4  p_5  p_6  p_7  p_8 p_9
           p_10 p_11 p_12 p_13 p_14 p_15 p_16 p_17 p_18 p_19 p_20
           pa)
       (Ps p_1' p_2' p_3' p_4' p_5' p_6' p_7' p_8' p_9'
           p_10' p_11' p_12' p_13' p_14' p_15' p_16' p_17' p_18' p_19' p_20'
           pa')
    = Ps (f p_1   p_1')
         (f p_2   p_2')
         (f p_3   p_3')
         (f p_4   p_4')
         (f p_5   p_5')
         (f p_6   p_6')
         (f p_7   p_7')
         (f p_8   p_8')
         (f p_9   p_9')
         (f p_10  p_10')
         (f p_11  p_11')
         (f p_12  p_12')
         (f p_13  p_13')
         (f p_14  p_14')
         (f p_15  p_15')
         (f p_16  p_16')
         (f p_17  p_17')
         (f p_18  p_18')
         (f p_19  p_19')
         (f p_20  p_20')
         (f pa pa')

    where
      f :: forall a. Maybe a -> Maybe a -> Maybe a
#if defined(COERCE)
      f = coerce ((<>) :: Option (Last a) -> Option (Last a) -> Option (Last a))
#else
      f _ y@(Just _) = y
      f x _          = x
#endif
