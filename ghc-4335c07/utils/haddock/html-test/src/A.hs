module A where

data A = A

other :: Int
other = 2

-- | Doc for test2
test2 :: Bool
test2 = False

-- | Should show up on the page for both modules A and B
data X = X -- ^ Doc for consructor

-- | Should show up on the page for both modules A and B
reExport :: Int
reExport = 1
