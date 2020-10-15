-- | Foreign export stubs
module GHC.Types.ForeignStubs
   ( ForeignStubs (..)
   , appendStubC
   )
where

import GHC.Utils.Outputable

-- | Foreign export stubs
data ForeignStubs
  = NoStubs
      -- ^ We don't have any stubs
  | ForeignStubs SDoc SDoc
      -- ^ There are some stubs. Parameters:
      --
      --  1) Header file prototypes for
      --     "foreign exported" functions
      --
      --  2) C stubs to use when calling
      --     "foreign exported" functions

appendStubC :: ForeignStubs -> SDoc -> ForeignStubs
appendStubC NoStubs            c_code = ForeignStubs empty c_code
appendStubC (ForeignStubs h c) c_code = ForeignStubs h (c $$ c_code)
