{-# LANGUAGE GADTs, NoMonoLocalBinds #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- Norman likes local bindings
-- If this module lives on I'd like to get rid of the NoMonoLocalBinds
-- extension in due course

-- Todo: remove -fno-warn-warnings-deprecations
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module CmmBuildInfoTables
    ( CAFSet, CAFEnv, cafAnal, localCAFInfo, mkTopCAFInfo
    , setInfoTableSRT
    , TopSRT, emptySRT, srtToData
    , bundleCAFs
    , cafTransfers )
where

#include "HsVersions.h"

-- These should not be imported here!
import StgCmmUtils

import Digraph
import qualified Prelude as P
import Prelude hiding (succ)

import BlockId
import Bitmap
import CLabel
import Cmm
import CmmUtils
import IdInfo
import Data.List
import Maybes
import Name
import Outputable
import SMRep
import UniqSupply

import Hoopl

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

foldSet :: (a -> b -> b) -> b -> Set a -> b
#if __GLASGOW_HASKELL__ < 704
foldSet = Set.fold
#else
foldSet = Set.foldr
#endif

----------------------------------------------------------------
-- Building InfoTables


-----------------------------------------------------------------------
-- SRTs

-- WE NEED AN EXAMPLE HERE.
-- IN PARTICULAR, WE NEED TO POINT OUT THE DISTINCTION BETWEEN
-- FUNCTIONS WITH STATIC CLOSURES AND THOSE THAT MUST BE CONSTRUCTED
-- DYNAMICALLY (AND HENCE CAN'T BE REFERENCED IN AN SRT).
-- IN THE LATTER CASE, WE HAVE TO TAKE ALL THE CAFs REFERENCED BY
-- THE CLOSURE AND INLINE THEM INTO ANY SRT THAT MAY MENTION THE CLOSURE.
-- (I.E. TAKE THE TRANSITIVE CLOSURE, but only for non-static closures).


-----------------------------------------------------------------------
-- Finding the CAFs used by a procedure

type CAFSet = Set CLabel
type CAFEnv = BlockEnv CAFSet

-- First, an analysis to find live CAFs.
cafLattice :: DataflowLattice CAFSet
cafLattice = DataflowLattice "live cafs" Set.empty add
  where add _ (OldFact old) (NewFact new) = case old `Set.union` new of
                                              new' -> (changeIf $ Set.size new' > Set.size old, new')

cafTransfers :: BwdTransfer CmmNode CAFSet
cafTransfers = mkBTransfer3 first middle last
  where first  _ live = live
        middle m live = foldExpDeep addCaf m live
        last   l live = foldExpDeep addCaf l (joinOutFacts cafLattice l live)
        addCaf e set = case e of
               CmmLit (CmmLabel c)              -> add c set
               CmmLit (CmmLabelOff c _)         -> add c set
               CmmLit (CmmLabelDiffOff c1 c2 _) -> add c1 $ add c2 set
               _ -> set
        add l s = if hasCAF l then Set.insert (toClosureLbl l) s
                              else s

cafAnal :: CmmGraph -> CAFEnv
cafAnal g = dataflowAnalBwd g [] $ analBwd cafLattice cafTransfers

-----------------------------------------------------------------------
-- Building the SRTs

-- Description of the SRT for a given module.
-- Note that this SRT may grow as we greedily add new CAFs to it.
data TopSRT = TopSRT { lbl      :: CLabel
                     , next_elt :: Int -- the next entry in the table
                     , rev_elts :: [CLabel]
                     , elt_map  :: Map CLabel Int }
                        -- map: CLabel -> its last entry in the table
instance Outputable TopSRT where
  ppr (TopSRT lbl next elts eltmap) =
    text "TopSRT:" <+> ppr lbl
                   <+> ppr next
                   <+> ppr elts
                   <+> ppr eltmap

emptySRT :: MonadUnique m => m TopSRT
emptySRT =
  do top_lbl <- getUniqueM >>= \ u -> return $ mkSRTLabel (mkFCallName u "srt") NoCafRefs
     return TopSRT { lbl = top_lbl, next_elt = 0, rev_elts = [], elt_map = Map.empty }

cafMember :: TopSRT -> CLabel -> Bool
cafMember srt lbl = Map.member lbl (elt_map srt)

cafOffset :: TopSRT -> CLabel -> Maybe Int
cafOffset srt lbl = Map.lookup lbl (elt_map srt)

addCAF :: CLabel -> TopSRT -> TopSRT
addCAF caf srt =
  srt { next_elt = last + 1
      , rev_elts = caf : rev_elts srt
      , elt_map  = Map.insert caf last (elt_map srt) }
    where last  = next_elt srt

srtToData :: TopSRT -> CmmGroup
srtToData srt = [CmmData RelocatableReadOnlyData (Statics (lbl srt) tbl)]
    where tbl = map (CmmStaticLit . CmmLabel) (reverse (rev_elts srt))

-- Once we have found the CAFs, we need to do two things:
-- 1. Build a table of all the CAFs used in the procedure.
-- 2. Compute the C_SRT describing the subset of CAFs live at each procpoint.
--
-- When building the local view of the SRT, we first make sure that all the CAFs are 
-- in the SRT. Then, if the number of CAFs is small enough to fit in a bitmap,
-- we make sure they're all close enough to the bottom of the table that the
-- bitmap will be able to cover all of them.
buildSRTs :: TopSRT -> Map CLabel CAFSet -> CAFSet ->
             UniqSM (TopSRT, Maybe CmmDecl, C_SRT)
buildSRTs topSRT topCAFMap cafs =
  do let liftCAF lbl z = -- get CAFs for functions without static closures
           case Map.lookup lbl topCAFMap of Just cafs -> z `Set.union` cafs
                                            Nothing   -> Set.insert lbl z
         -- For each label referring to a function f without a static closure,
         -- replace it with the CAFs that are reachable from f.
         sub_srt topSRT localCafs =
           let cafs = Set.elems (foldSet liftCAF Set.empty localCafs)
               mkSRT topSRT =
                 do localSRTs <- procpointSRT (lbl topSRT) (elt_map topSRT) cafs
                    return (topSRT, localSRTs)
           in if length cafs > maxBmpSize then
                mkSRT (foldl add_if_missing topSRT cafs)
              else -- make sure all the cafs are near the bottom of the srt
                mkSRT (add_if_too_far topSRT cafs)
         add_if_missing srt caf =
           if cafMember srt caf then srt else addCAF caf srt
         -- If a CAF is more than maxBmpSize entries from the young end of the
         -- SRT, then we add it to the SRT again.
         -- (Note: Not in the SRT => infinitely far.)
         add_if_too_far srt@(TopSRT {elt_map = m}) cafs =
           add srt (sortBy farthestFst cafs)
             where
               farthestFst x y = case (Map.lookup x m, Map.lookup y m) of
                                   (Nothing, Nothing) -> EQ
                                   (Nothing, Just _)  -> LT
                                   (Just _,  Nothing) -> GT
                                   (Just d, Just d')  -> compare d' d
               add srt [] = srt
               add srt@(TopSRT {next_elt = next}) (caf : rst) =
                 case cafOffset srt caf of
                   Just ix -> if next - ix > maxBmpSize then
                                add (addCAF caf srt) rst
                              else srt
                   Nothing -> add (addCAF caf srt) rst
     (topSRT, subSRTs) <- sub_srt topSRT cafs
     let (sub_tbls, blockSRTs) = subSRTs
     return (topSRT, sub_tbls, blockSRTs)

-- Construct an SRT bitmap.
-- Adapted from simpleStg/SRT.lhs, which expects Id's.
procpointSRT :: CLabel -> Map CLabel Int -> [CLabel] ->
                UniqSM (Maybe CmmDecl, C_SRT)
procpointSRT _ _ [] =
 return (Nothing, NoC_SRT)
procpointSRT top_srt top_table entries =
 do (top, srt) <- bitmap `seq` to_SRT top_srt offset len bitmap
    return (top, srt)
  where
    ints = map (expectJust "constructSRT" . flip Map.lookup top_table) entries
    sorted_ints = sort ints
    offset = head sorted_ints
    bitmap_entries = map (subtract offset) sorted_ints
    len = P.last bitmap_entries + 1
    bitmap = intsToBitmap len bitmap_entries

maxBmpSize :: Int
maxBmpSize = widthInBits wordWidth `div` 2

-- Adapted from codeGen/StgCmmUtils, which converts from SRT to C_SRT.
to_SRT :: CLabel -> Int -> Int -> Bitmap -> UniqSM (Maybe CmmDecl, C_SRT)
to_SRT top_srt off len bmp
  | len > maxBmpSize || bmp == [fromIntegral srt_escape]
  = do id <- getUniqueM
       let srt_desc_lbl = mkLargeSRTLabel id
           tbl = CmmData RelocatableReadOnlyData $
                   Statics srt_desc_lbl $ map CmmStaticLit
                     ( cmmLabelOffW top_srt off
                     : mkWordCLit (fromIntegral len)
                     : map mkWordCLit bmp)
       return (Just tbl, C_SRT srt_desc_lbl 0 srt_escape)
  | otherwise
  = return (Nothing, C_SRT top_srt off (fromIntegral (head bmp)))
	-- The fromIntegral converts to StgHalfWord

-- Gather CAF info for a procedure, but only if the procedure
-- doesn't have a static closure.
-- (If it has a static closure, it will already have an SRT to
--  keep its CAFs live.)
-- Any procedure referring to a non-static CAF c must keep live
-- any CAF that is reachable from c.
localCAFInfo :: CAFEnv -> CmmDecl -> Maybe (CLabel, CAFSet)
localCAFInfo _      (CmmData _ _) = Nothing
localCAFInfo cafEnv (CmmProc top_info top_l (CmmGraph {g_entry=entry})) =
  case info_tbl top_info of
    CmmInfoTable { cit_rep = rep } 
      | not (isStaticRep rep) 
      -> Just (toClosureLbl top_l,
               expectJust "maybeBindCAFs" $ mapLookup entry cafEnv)
    _ -> Nothing

-- Once we have the local CAF sets for some (possibly) mutually
-- recursive functions, we can create an environment mapping
-- each function to its set of CAFs. Note that a CAF may
-- be a reference to a function. If that function f does not have
-- a static closure, then we need to refer specifically
-- to the set of CAFs used by f. Of course, the set of CAFs
-- used by f must be included in the local CAF sets that are input to
-- this function. To minimize lookup time later, we return
-- the environment with every reference to f replaced by its set of CAFs.
-- To do this replacement efficiently, we gather strongly connected
-- components, then we sort the components in topological order.
mkTopCAFInfo :: [(CLabel, CAFSet)] -> Map CLabel CAFSet
mkTopCAFInfo localCAFs = foldl addToTop Map.empty g
  where addToTop env (AcyclicSCC (l, cafset)) =
          Map.insert l (flatten env cafset) env
        addToTop env (CyclicSCC nodes) =
          let (lbls, cafsets) = unzip nodes
              cafset  = foldr Set.delete (foldl Set.union Set.empty cafsets) lbls
          in foldl (\env l -> Map.insert l (flatten env cafset) env) env lbls
        flatten env cafset = foldSet (lookup env) Set.empty cafset
        lookup env caf cafset' =
          case Map.lookup caf env of Just cafs -> foldSet add cafset' cafs
                                     Nothing -> add caf cafset'
        add caf cafset' = Set.insert caf cafset'
        g = stronglyConnCompFromEdgedVertices
              (map (\n@(l, cafs) -> (n, l, Set.elems cafs)) localCAFs)

-- Bundle the CAFs used at a procpoint.
bundleCAFs :: CAFEnv -> CmmDecl -> (CAFSet, CmmDecl)
bundleCAFs cafEnv t@(CmmProc _ _ (CmmGraph {g_entry=entry})) =
  (expectJust "bundleCAFs" (mapLookup entry cafEnv), t)
bundleCAFs _ t = (Set.empty, t)

-- Construct the SRTs for the given procedure.
setInfoTableSRT :: Map CLabel CAFSet -> TopSRT -> (CAFSet, CmmDecl) ->
                   UniqSM (TopSRT, [CmmDecl])
setInfoTableSRT topCAFMap topSRT (cafs, t) =
  setSRT cafs topCAFMap topSRT t

setSRT :: CAFSet -> Map CLabel CAFSet -> TopSRT ->
          CmmDecl -> UniqSM (TopSRT, [CmmDecl])
setSRT cafs topCAFMap topSRT t =
  do (topSRT, cafTable, srt) <- buildSRTs topSRT topCAFMap cafs
     let t' = updInfo id (const srt) t
     case cafTable of
       Just tbl -> return (topSRT, [t', tbl])
       Nothing  -> return (topSRT, [t'])

type StackLayout = Liveness

updInfo :: (StackLayout -> StackLayout) -> (C_SRT -> C_SRT) -> CmmDecl -> CmmDecl
updInfo toVars toSrt (CmmProc top_info top_l g) =
  CmmProc (top_info {info_tbl=updInfoTbl toVars toSrt (info_tbl top_info)}) top_l g
updInfo _ _ t = t

updInfoTbl :: (StackLayout -> StackLayout) -> (C_SRT -> C_SRT) -> CmmInfoTable -> CmmInfoTable
updInfoTbl toVars toSrt info_tbl@(CmmInfoTable {})
  = info_tbl { cit_srt = toSrt (cit_srt info_tbl)
             , cit_rep = case cit_rep info_tbl of
                           StackRep ls -> StackRep (toVars ls)
                           other       -> other }
updInfoTbl _ _ t@CmmNonInfoTable = t
