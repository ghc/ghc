-- #hide
module T25897c
  ( d
  , c
  )
  where

-- | This should appear in second, as per the explicit export list
c :: ()
c = ()

-- | This should appear in first, as per the explicit export list
d :: ()
d = ()
