module A where

-- E source imports B
-- In interface file see source module dependencies: B {-# SOURCE #-}
import E
-- C imports B
-- In interface file see source module dependencies: B
import C

-- Instance for B only available from B.hi not B.hi-boot, so tests we load
-- that.
main = print B
