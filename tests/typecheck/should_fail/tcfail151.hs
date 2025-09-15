{-# LANGUAGE DatatypeContexts, MultiParamTypeClasses #-}
module ShouldFail where

class (Show a, Eq a, Monad m) => Name m a where 
    hashName :: a -> m Int 
    newName :: m a 
 
data Name a => Exp a = MkExp a
-- The kind error should be reported here
-- GHC 6.4 reported an error with the class decl

