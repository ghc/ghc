module T19264 where

import T19264b -- needed (compiled before this module and triggering the failure)
import GHC.Num.BigNat (bigNatFromWordList)
