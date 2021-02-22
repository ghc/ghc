{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Functor.Identity

newtype App f x = App (f x)

class WrappedIn s a | s -> a where
  unwrap :: s -> a

instance WrappedIn (App f a) (f a) where
  unwrap (App fa) = fa

pattern Unwrapped :: WrappedIn s a => a -> s
pattern Unwrapped x <- (unwrap -> x)

{-# COMPLETE Unwrapped :: App t #-}

boom :: App Identity t -> Bool
boom (Unwrapped (Identity _)) = True

main :: IO ()
main = print ":)"
