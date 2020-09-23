{-# LANGUAGE LinearTypes #-}
module LinearPatternGuardWildcard where

-- See #18439

unsafeConsume :: a #-> ()
unsafeConsume x | _ <- x = ()
