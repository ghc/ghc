{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Functor.Identity

data HideArg f where
  HideArg :: f a -> HideArg f

data family App :: tF -> tF
data instance App f x = App1 (f x)

class WrappedIn s a | s -> a where
  unwrap :: s -> a

instance WrappedIn (App f a) (f a) where
  unwrap (App1 fa) = fa

pattern Unwrapped :: WrappedIn s a => a -> s
pattern Unwrapped x <- (unwrap -> x)
{-# COMPLETE Unwrapped :: App #-}

boom :: HideArg (App Identity) -> Bool
boom (HideArg (Unwrapped (Identity _))) = True

main :: IO ()
main = print ":("
