{-# LANGUAGE LinearTypes #-}
module LinearPatternGuardWildcard where

-- See #18439

unsafeConsume :: a %1 -> ()
unsafeConsume x | _ <- x = ()
