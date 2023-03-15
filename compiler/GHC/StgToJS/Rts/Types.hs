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
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- Types and utility functions used in the JS RTS.
-----------------------------------------------------------------------------

module GHC.StgToJS.Rts.Types where

import GHC.Prelude

import GHC.JS.Make
import GHC.JS.Unsat.Syntax
import GHC.StgToJS.Regs
import GHC.StgToJS.Types

--------------------------------------------------------------------------------
-- Syntactic Sugar for some Utilities we want in JS land
--------------------------------------------------------------------------------

-- | Syntactic sugar, i.e., a Haskell function which generates useful JS code.
-- Given a @JExpr@, 'ex', inject a trace statement on 'ex' in the compiled JS
-- program
traceRts :: StgToJSConfig -> JExpr -> JStat
traceRts s ex | (csTraceRts s)  = appS "h$log" [ex]
              | otherwise       = mempty

-- | Syntactic sugar. Given a @JExpr@, 'ex' which is assumed to be a predicate,
-- and a message 'm', assert that 'not ex' is True, if not throw an exception in
-- JS land with message 'm'.
assertRts :: ToJExpr a => StgToJSConfig -> JExpr -> a -> JStat
assertRts s ex m | csAssertRts s = jwhenS (UOpExpr NotOp ex) (appS "throw" [toJExpr m])
                 | otherwise     = mempty

-- | name of the closure 'c'
clName :: JExpr -> JExpr
clName c = c .^ "n"

-- | Type name of the closure 'c'
clTypeName :: JExpr -> JExpr
clTypeName c = app "h$closureTypeName" [c .^ "t"]

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
stackFrameSize :: JExpr -- ^ assign frame size to this
               -> JExpr -- ^ stack frame header function
               -> JStat -- ^ size of the frame, including header
stackFrameSize tgt f =
  ifS (f .===. var "h$ap_gen") -- h$ap_gen is special
      (tgt |= (stack .! (sp - 1) .>>. 8) + 2)
      (jVar (\tag ->
               mconcat
               [tag |= f .^ "size"
               , ifS (tag .<. 0)              -- if tag is less than 0
                 (tgt |= stack .! (sp - 1))   -- set target to stack pointer - 1
                 (tgt |= mask8 tag + 1)       -- else set to mask'd tag + 1
               ]
        ))

--------------------------------------------------------------------------------
-- Register utilities
--------------------------------------------------------------------------------

-- | Perform the computation 'f', on the range of registers bounded by 'start'
-- and 'end'.
withRegs :: StgReg -> StgReg -> (StgReg -> JStat) -> JStat
withRegs start end f = mconcat $ fmap f [start..end]
