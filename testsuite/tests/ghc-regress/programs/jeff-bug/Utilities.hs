module Utilities where

import Maybe


-- Begin Signature -------------------------------------------------

infixr 1 `catchEx`

catchEx :: Maybe a -> a -> a
(><) :: (a -> b,c -> d) -> (a,c) -> (b,d)

-- End Signature -------------------------------------------------

catchEx = flip fromMaybe
(f,g) >< (x,y) = (f x, g y)


