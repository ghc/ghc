{-# LANGUAGE CPP,
             FlexibleInstances,
             OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Rts.Apply
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
-- Types and utility functions used in the JS RTS.
-- FIXME: Jeff (2022,03): Add more details
-----------------------------------------------------------------------------

module GHC.StgToJS.Rts.Types where

import GHC.Prelude

import GHC.JS.Make
import GHC.JS.Syntax
import GHC.StgToJS.Regs
import GHC.StgToJS.Types

import GHC.Utils.Monad.State.Strict
import qualified GHC.Data.ShortText as T



traceRts :: StgToJSConfig -> JExpr -> JStat
traceRts s ex = jStatIf (csTraceRts s) (appS "h$log" [ex])

assertRts :: ToJExpr a => StgToJSConfig -> JExpr -> a -> JStat
assertRts s ex m = jStatIf (csAssertRts s)
  (jwhenS (UOpExpr NotOp ex) (appS "throw" [toJExpr m]))

jStatIf :: Bool -> JStat -> JStat
jStatIf True s = s
jStatIf _    _ = mempty

clName :: JExpr -> JExpr
clName c = c .^ "n"

clTypeName :: JExpr -> JExpr
clTypeName c = app "h$closureTypeName" [c .^ "t"]

type C = State GenState JStat

assertRtsStat :: C -> C
assertRtsStat stat = do
  s <- gets gsSettings
  if csAssertRts s then stat else return mempty

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
stackFrameSize :: JExpr -- ^ assign frame size to this
               -> JExpr -- ^ stack frame header function
               -> JStat -- ^ size of the frame, including header
stackFrameSize tgt f =
  ifS (f .===. var "h$ap_gen") -- h$ap_gen is special
      (tgt |= (stack .! (sp - 1) .>>. 8) + 2)  -- special case, FIXME (Jeff, 2022/03): what and why is
                                               -- it special and how does its
                                               -- special-ness change this code
      (jVar (\tag ->
               mconcat
               [tag |= f .^ "size"
               , ifS (tag .<. 0)              -- if tag is less than 0
                 (tgt |= stack .! (sp - 1))   -- set target to stack pointer - 1
                 (tgt |= mask8 tag + 1)       -- else set to mask'd tag + 1
               ]
        ))

-- some utilities do do something with a range of regs
-- start or end possibly supplied as javascript expr
withRegs :: StgReg -> StgReg -> (StgReg -> JStat) -> JStat
withRegs start end f = mconcat $ map f [start..end]

withRegs' :: StgReg -> StgReg -> (StgReg -> JStat) -> JStat
withRegs' start end = withRegs start end

-- start from js expr, start is guaranteed to be at least min
-- from low to high (fallthrough!)
withRegsS :: JExpr -> StgReg -> StgReg -> Bool -> (StgReg -> JStat) -> JStat
withRegsS start min end fallthrough f =
  SwitchStat start (map mkCase [min..end]) mempty
    where
      brk | fallthrough = mempty
          | otherwise   = BreakStat Nothing
      mkCase r = let stat = f r
                  in (toJExpr r, mconcat [stat , stat , brk])

-- end from js expr, from high to low
withRegsRE :: StgReg -> JExpr -> StgReg -> Bool -> (StgReg -> JStat) -> JStat
withRegsRE start end max fallthrough f =
  SwitchStat end (reverse $ map mkCase [start..max]) mempty
    where
      brk | fallthrough = mempty
          | otherwise   = BreakStat Nothing
      mkCase r = (toJExpr (fromEnum r), mconcat [f r , brk])

jsVar :: String -> JExpr
jsVar = ValExpr . JVar . TxtI . T.pack
