{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoLazyFieldAnnotations #-}

-- | A later NoLazyFieldAnnotations overrides the implication from StrictData,
-- so ~ is rejected.
module LazyFieldsDisabledStrictData where

data A = A ~Int
