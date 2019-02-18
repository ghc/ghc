-- http://trac.haskell.org/haddock/ticket/37
module HiddenInstances (VisibleClass, VisibleData) where

-- | Should be visible
class VisibleClass a

-- | Should *not* be visible
class HiddenClass a

-- | Should *not* be visible
data HiddenData = HiddenData

-- | Should be visible
data VisibleData = VisibleData

-- | Should be visible
instance VisibleClass Int

-- | Should be visible
instance VisibleClass VisibleData

-- | Should be visible
instance Num VisibleData

-- | Should *not* be visible
instance VisibleClass HiddenData

-- | Should *not* be visible
instance HiddenClass Int

-- | Should *not* be visible
instance HiddenClass VisibleData

-- | Should *not* be visible
instance HiddenClass HiddenData
