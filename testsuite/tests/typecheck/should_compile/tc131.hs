{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- !!! Typechecking of functional dependencies
-- Showed up (another) bug in the newtype-squashing machinery


module ShouldCompile where

class Split2 a b | a -> b, b -> a where
    combine2 :: (b,b) -> a

class Split4 a b | a -> b, b -> a where
    combine4 :: (b,b) -> a

newtype Word16 = Word16 Int
newtype Word32 = Word32 Int
newtype Word64 = Word64 Int

instance Split2 Word32 Word16 where
    combine2 = undefined

instance Split2 Word64 Word32 where
    combine2 a = undefined

instance Split4 Word64 Word16 where
    combine4 (a, b) = 
        combine2 ( (combine2 (a, b)), combine2 (a, b))



