
{-# LANGUAGE PatternSynonyms #-}

module T12178a where

data D a = MyConstructor a

pattern MyPattern = MyConstructor

-- INLINE for patterns is allowed
{-# INLINE MyPattern #-}

-- But not for data constructors
{-# INLINE MyConstructor #-}
