
-- trac #924: RnFail047_A.hs-boot exports more than RnFail047_A.hs

module RnFail047 where

import {-# SOURCE #-} RnFail047_A

v = x

