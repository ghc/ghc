module Init where

import Signal

-- Begin Signature ------------------------------------------------------
{-

Very often, particularily when operating over pointed domains, each
type has a particular value that serves well as an inital state. 
The Init class picks that value out.  For example, the "def" value
for lists is "[]"

-}

class Init a where
   def :: a

{-instance Init [a]-}
{-instance Init (Maybe a)-}
{-instance Init Int-}
{-instance Init Bool-}


-- delay a signal using the type's default value as the initializer
del :: Init a => Signal a -> Signal a


-- End Signature ------------------------------------------------------
   
instance Init [a] where
   def = []

instance Init (Maybe a) where
   def = Nothing

instance Init Int where
   def = 0

instance Init Bool where
   def = False

del x = delay def x
