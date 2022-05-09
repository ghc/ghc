{-# LANGUAGE PatternSynonyms #-}

module T21531 where

import Foreign.C( CChar )

newtype LGate = LGate CChar

{-# INLINE And #-}
pattern And :: LGate
pattern And <- LGate 0b00000000
  where
    And = LGate 0b00000000
