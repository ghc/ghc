{-#LANGUAGE TypeApplications#-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DefaultSignatures #-}
module T12220 where

-- | Type a is only used for
-- type application.
class ToUse a where
    toUse :: Int -> Int

-- | The type used for
-- type application
data Default


-- | The instance using Default as type application.
-- To call use:
-- > toUse @Default
instance ToUse Default where
    toUse a = 3*a

-- | Typeclass whose methods work
-- only with type application.
class Uses a b where
    uses :: b -> [b]
    -- | Default Signature, which generates the problem.
    -- It is the same as the normal one
    -- Comment it to 'fix' the bug.
    default uses :: b -> [b]
    uses v = [v]

-- | But this one doesn't.
-- Unless you comment the default signature.
instance (Uses t a, Uses t b, Uses t c) => Uses t (a,b,c)
