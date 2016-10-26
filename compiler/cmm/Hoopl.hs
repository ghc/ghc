{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Hoopl (
    module Compiler.Hoopl,
    module Hoopl.Dataflow,
  ) where

import Compiler.Hoopl hiding
  ( (<*>), mkLabel, mkBranch, mkMiddle, mkLast, -- clashes with our MkGraph
    Unique,
    FwdTransfer(..), FwdRewrite(..), FwdPass(..),
    BwdTransfer(..), BwdRewrite(..), BwdPass(..),
    mkFactBase, Fact,
    mkBRewrite3, mkBTransfer3,
    mkFRewrite3, mkFTransfer3,
  )

import Hoopl.Dataflow

-- Note [Deprecations in Hoopl]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- CmmLive and CmmBuildInfoTables modules enable -fno-warn-warnings-deprecations
-- flag because they import deprecated functions from Hoopl. I spent some time
-- trying to figure out what is going on, so here's a brief explanation. The
-- culprit is the joinOutFacts function, which should be replaced with
-- joinFacts. The difference between them is that the latter one needs extra
-- Label parameter. Labels identify blocks and are used in the fact base to
-- assign facts to a block (in case you're wondering, Label is an Int wrapped in
-- a newtype). Lattice join function is also required to accept a Label but the
-- only reason why it is so are the debugging purposes: see joinInFacts function
-- which is a no-op and is run only because join function might produce
-- debugging output. Now, going back to the Cmm modules. The "problem" with the
-- deprecated joinOutFacts function is that it passes wrong label when calling
-- lattice join function: instead of label of a block for which we are joining
-- facts it uses labels of successors of that block. So the joinFacts function
-- expects to be given a label of a block for which we are joining facts. I
-- don't see an obvious way of recovering that Label at the call sites of
-- joinOutFacts (if that was easily done then joinFacts function could do it
-- internally without requiring label as a parameter). A cheap way of
-- eliminating these warnings would be to create a bogus Label, since none of
-- our join functions is actually using the Label parameter. But that doesn't
-- feel right. I think the real solution here is to fix Hoopl API, which is
-- already broken in several ways. See Hoopl/Cleanup page on the wiki for more
-- notes on improving Hoopl.
