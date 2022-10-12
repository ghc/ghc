{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

-- | JS codegen state monad
module GHC.StgToJS.Monad
  ( runG
  , emitGlobal
  , addDependency
  , emitToplevel
  , emitStatic
  , emitClosureInfo
  , emitForeign
  , assertRtsStat
  , getSettings
  , liftToGlobal
  , setGlobalIdCache
  , getGlobalIdCache
  -- * Group
  , modifyGroup
  , resetGroup
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Transform

import GHC.StgToJS.Types

import GHC.Unit.Module
import GHC.Stg.Syntax

import GHC.Types.SrcLoc
import GHC.Types.Id
import GHC.Types.Unique.FM
import GHC.Types.ForeignCall

import qualified Control.Monad.Trans.State.Strict as State
import GHC.Data.FastString
import GHC.Data.FastMutInt

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.List as L
import Data.Function

import GHC.Types.Unique.DSet

runG :: StgToJSConfig -> Module -> UniqFM Id CgStgExpr -> G a -> IO a
runG config m unfloat action = State.evalStateT action =<< initState config m unfloat

initState :: StgToJSConfig -> Module -> UniqFM Id CgStgExpr -> IO GenState
initState config m unfloat = do
  id_gen <- newFastMutInt 1
  pure $ GenState
    { gsSettings  = config
    , gsModule    = m
    , gsId        = id_gen
    , gsIdents    = emptyIdCache
    , gsUnfloated = unfloat
    , gsGroup     = defaultGenGroupState
    , gsGlobal    = []
    }


modifyGroup :: (GenGroupState -> GenGroupState) -> G ()
modifyGroup f = State.modify mod_state
  where
    mod_state s = s { gsGroup = f (gsGroup s) }

-- | emit a global (for the current module) toplevel statement
emitGlobal :: JStat -> G ()
emitGlobal stat = State.modify (\s -> s { gsGlobal = stat : gsGlobal s })

-- | add a dependency on a particular symbol to the current group
addDependency :: OtherSymb -> G ()
addDependency symbol = modifyGroup mod_group
  where
    mod_group g = g { ggsExtraDeps = S.insert symbol (ggsExtraDeps g) }

-- | emit a top-level statement for the current binding group
emitToplevel :: JStat -> G ()
emitToplevel s = modifyGroup mod_group
  where
    mod_group g = g { ggsToplevelStats = s : ggsToplevelStats g}

-- | emit static data for the binding group
emitStatic :: FastString -> StaticVal -> Maybe Ident -> G ()
emitStatic ident val cc = modifyGroup mod_group
  where
    mod_group  g = g { ggsStatic = mod_static (ggsStatic g) }
    mod_static s = StaticInfo ident val cc : s

-- | add closure info in our binding group. all heap objects must have closure info
emitClosureInfo :: ClosureInfo -> G ()
emitClosureInfo ci = modifyGroup mod_group
  where
    mod_group g = g { ggsClosureInfo = ci : ggsClosureInfo g}

emitForeign :: Maybe RealSrcSpan
            -> FastString
            -> Safety
            -> CCallConv
            -> [FastString]
            -> FastString
            -> G ()
emitForeign mbSpan pat safety cconv arg_tys res_ty = modifyGroup mod_group
  where
    mod_group g = g { ggsForeignRefs = new_ref : ggsForeignRefs g }
    new_ref = ForeignJSRef spanTxt pat safety cconv arg_tys res_ty
    spanTxt = case mbSpan of
                -- TODO: Is there a better way to concatenate FastStrings?
                Just sp -> mkFastString $
                  unpackFS (srcSpanFile sp) ++
                  " " ++
                  show (srcSpanStartLine sp, srcSpanStartCol sp) ++
                  "-" ++
                  show (srcSpanEndLine sp, srcSpanEndCol sp)
                Nothing -> "<unknown>"






-- | start with a new binding group
resetGroup :: G ()
resetGroup = State.modify (\s -> s { gsGroup = defaultGenGroupState })

defaultGenGroupState :: GenGroupState
defaultGenGroupState = GenGroupState [] [] [] [] 0 S.empty emptyGlobalIdCache []

emptyGlobalIdCache :: GlobalIdCache
emptyGlobalIdCache = GlobalIdCache M.empty

emptyIdCache :: IdCache
emptyIdCache = IdCache M.empty



assertRtsStat :: G JStat -> G JStat
assertRtsStat stat = do
  s <- State.gets gsSettings
  if csAssertRts s then stat else pure mempty

getSettings :: G StgToJSConfig
getSettings = State.gets gsSettings

getGlobalIdCache :: G GlobalIdCache
getGlobalIdCache = State.gets (ggsGlobalIdCache . gsGroup)

setGlobalIdCache :: GlobalIdCache -> G ()
setGlobalIdCache v = State.modify (\s -> s { gsGroup = (gsGroup s) { ggsGlobalIdCache = v}})


liftToGlobal :: JStat -> G [(Ident, Id)]
liftToGlobal jst = do
  GlobalIdCache gidc <- getGlobalIdCache
  let sids  = filterUniqDSet (`M.member` gidc) (identsS jst)
      cnt   = M.fromListWith (+) (map (,(1::Integer)) $ uniqDSetToList sids)
      sids' = L.sortBy (compare `on` (cnt M.!)) (nub' $ uniqDSetToList sids)
  pure $ map (\s -> (s, snd $ gidc M.! s)) sids'

nub' :: (Ord a, Eq a) => [a] -> [a]
nub' xs = go S.empty xs
  where
    go _ []     = []
    go s (x:xs) | S.member x s = go s xs
                | otherwise    = x : go (S.insert x s) xs
