-- #hide
module T25897b
  ( b
  , a
  )
  where

-- | This should appear in second, as per the explicit export list
a :: ()
a = ()

-- | This should appear in first, as per the explicit export list
b :: ()
b = ()
