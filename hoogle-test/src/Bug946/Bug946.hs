{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE PatternSynonyms #-}
module Bug946 (
  AnInt(AnInt, Zero),
  pattern TwoPointFive,
) where

-- | A wrapper around 'Int'
data AnInt = AnInt Int -- ^ some 'Int'

-- | The 'Int' 0
pattern Zero :: AnInt
pattern Zero = AnInt 0

-- | The double 2.5
pattern TwoPointFive :: Double
pattern TwoPointFive = 2.5
