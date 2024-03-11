{-# LANGUAGE RebindableSyntax, ApplicativeDo #-}

module Bug where

_ >> _ = ()
return _ = ()
_ <*> _ = ()
fmap _ _ = ()

x = do x
       return x


-- xs :: [a]
-- xs = do
--   x <- []
--   pure x
