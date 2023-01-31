{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.ExprCtx
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
-- TODO: Write my description!
-----------------------------------------------------------------------------

module GHC.StgToJS.ExprCtx
  ( ExprCtx
  , initExprCtx
  , ctxIsEvaluated
  , ctxSetSrcSpan
  , ctxSrcSpan
  , ctxSetTop
  , ctxTarget
  , ctxSetTarget
  -- * Let-no-escape
  , ctxClearLneFrame
  , ctxUpdateLneFrame
  , ctxLneFrameVars
  , ctxLneFrameSize
  , ctxIsLneBinding
  , ctxIsLneLiveVar
  , ctxLneBindingStackSize
  , ctxLneShrinkStack
  )
where

import GHC.Prelude

import GHC.StgToJS.Types

import GHC.Types.Unique.FM
import GHC.Types.Var
import GHC.Types.SrcLoc
import GHC.Types.Id
import GHC.Types.Id.Info

import GHC.Stg.EnforceEpt.TagSig

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Maybe


-- | Context into which an expression is evaluated
data ExprCtx = ExprCtx
  { ctxTop        :: Id
    -- ^ Top-level binding Id

  , ctxTarget     :: [TypedExpr]
    -- ^ Target variables for the evaluated expression

  , ctxSrcSpan    :: Maybe RealSrcSpan
    -- ^ Source location

  ----------------------------
  -- Handling of let-no-escape

  , ctxLneFrameBs :: UniqFM Id Int
    -- ^ LNE bindings with their expected stack size.
    --
    -- The Int is the size of the stack when the LNE binding was defined.
    -- We need to shrink the stack back to this size when we enter one of the
    -- associated binder rhs: it expects its free variables at certain offsets
    -- in the stack.

  , ctxLneFrameVars :: [(Id,Int)]
    -- ^ Contents of current LNE frame
    --
    -- Variables and their index on the stack

  , ctxLneFrameSize :: {-# UNPACK #-} !Int
    -- ^ Cache the length of `ctxLneFrameVars`

  }

-- | Initialize an expression context in the context of the given top-level
-- binding Id
initExprCtx :: Id -> ExprCtx
initExprCtx i = ExprCtx
  { ctxTop          = i
  , ctxTarget       = []
  , ctxLneFrameBs   = emptyUFM
  , ctxLneFrameVars = []
  , ctxLneFrameSize = 0
  , ctxSrcSpan      = Nothing
  }

-- | Set target
ctxSetTarget :: [TypedExpr] -> ExprCtx -> ExprCtx
ctxSetTarget t ctx = ctx { ctxTarget = t }

-- | Set top-level binding Id
ctxSetTop :: Id -> ExprCtx -> ExprCtx
ctxSetTop i ctx = ctx { ctxTop = i }

-- | Set source location
ctxSetSrcSpan :: RealSrcSpan -> ExprCtx -> ExprCtx
ctxSetSrcSpan span ctx = ctx { ctxSrcSpan = Just span }

-- | Update let-no-escape frame
ctxUpdateLneFrame :: [(Id,Int)] -> [Id] -> ExprCtx -> ExprCtx
ctxUpdateLneFrame new_spilled_vars new_lne_ids ctx =
  let old_frame_size = ctxLneFrameSize ctx
      new_frame_size = old_frame_size + length new_spilled_vars
  in ctx
    { ctxLneFrameBs   = addListToUFM (ctxLneFrameBs ctx) (map (,new_frame_size) new_lne_ids)
    , ctxLneFrameSize = new_frame_size
    , ctxLneFrameVars = ctxLneFrameVars ctx ++ new_spilled_vars
    }

-- | Remove information about the current LNE frame
ctxClearLneFrame :: ExprCtx -> ExprCtx
ctxClearLneFrame ctx =
  ctx
    { ctxLneFrameBs   = emptyUFM
    , ctxLneFrameVars = []
    , ctxLneFrameSize = 0
    }

-- | Predicate: do we know for sure that the given Id is evaluated?
ctxIsEvaluated :: Id -> Bool
ctxIsEvaluated i =
  maybe False isTaggedSig (idTagSig_maybe i)
  && go (idDetails i)
  where
    go JoinId{} = False
    go _        = True


      -- DFunId new_type -> not new_type
      --    -- DFuns terminate, unless the dict is implemented
      --    -- with a newtype in which case they may not

      -- DataConWorkId {} -> True

      -- ClassOpId {} -> False
      --   -- suppose an argument, and we don't have one

      -- PrimOpId op _ -> primop_ok op
      --   -- probably already handled by StgOpApp

      -- JoinId {} -> False
      --   -- Don't speculate join points

      -- TickBoxOpId {} -> False
      --   -- Don't speculate box ticking

      -- -- Tagged (evaluated) ids
      -- _ | Just sig <- idTagSig_maybe i
      --   , isTaggedSig sig
      --   -> True

      -- _ -> False

-- | Does the given Id correspond to a LNE binding
ctxIsLneBinding :: ExprCtx -> Id -> Bool
ctxIsLneBinding ctx i = isJust (ctxLneBindingStackSize ctx i)

-- | Does the given Id correspond to a LNE live var on the stack
ctxIsLneLiveVar :: ExprCtx -> Id -> Bool
ctxIsLneLiveVar ctx i = i `elem` map fst (ctxLneFrameVars ctx)

-- | Return the LNE stack size associated to the given Id.
-- Return Nothing when the Id doesn't correspond to a LNE binding.
ctxLneBindingStackSize :: ExprCtx -> Id -> Maybe Int
ctxLneBindingStackSize ctx i = lookupUFM (ctxLneFrameBs ctx) i

-- | Shrink the LNE stack to the given size
ctxLneShrinkStack :: ExprCtx -> Int -> ExprCtx
ctxLneShrinkStack ctx n =
  let l = ctxLneFrameSize ctx
  in assertPpr
      (l >= n)
      (vcat [ text "ctxLneShrinkStack: let-no-escape stack too short:"
            , ppr l
            , text " < "
            , ppr n
            ])
      (ctx { ctxLneFrameVars = take n (ctxLneFrameVars ctx)
           , ctxLneFrameSize = n
           }
      )
