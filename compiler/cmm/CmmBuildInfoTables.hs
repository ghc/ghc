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
    ( CAFSet, CAFEnv, cafAnal
    , doSRTs, TopSRT, emptySRT, isEmptySRT, srtToData )
where

#include "HsVersions.h"

import Hoopl
import Digraph
import BlockId
import Bitmap
import CLabel
import PprCmmDecl ()
import Cmm
import CmmUtils
import CmmInfo
import Data.List
import DynFlags
import Maybes
import Outputable
import SMRep
import UniqSupply
import Util

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

import qualified Prelude as P
import Prelude hiding (succ)

foldSet :: (a -> b -> b) -> b -> Set a -> b
foldSet = Set.foldr

-----------------------------------------------------------------------
-- SRTs

{- EXAMPLE

f = \x. ... g ...
  where
    g = \y. ... h ... c1 ...
    h = \z. ... c2 ...

c1 & c2 are CAFs

g and h are local functions, but they have no static closures.  When
we generate code for f, we start with a CmmGroup of four CmmDecls:

   [ f_closure, f_entry, g_entry, h_entry ]

we process each CmmDecl separately in cpsTop, giving us a list of
CmmDecls. e.g. for f_entry, we might end up with

   [ f_entry, f1_ret, f2_proc ]

where f1_ret is a return point, and f2_proc is a proc-point.  We have
a CAFSet for each of these CmmDecls, let's suppose they are

   [ f_entry{g_closure}, f1_ret{g_closure}, f2_proc{} ]
   [ g_entry{h_closure, c1_closure} ]
   [ h_entry{c2_closure} ]

Now, note that we cannot use g_closure and h_closure in an SRT,
because there are no static closures corresponding to these functions.
So we have to flatten out the structure, replacing g_closure and
h_closure with their contents:

   [ f_entry{c2_closure, c1_closure}, f1_ret{c2_closure,c1_closure}, f2_proc{} ]
   [ g_entry{c2_closure, c1_closure} ]
   [ h_entry{c2_closure} ]

This is what flattenCAFSets is doing.

-}

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
  do top_lbl <- getUniqueM >>= \ u -> return $ mkTopSRTLabel u
     return TopSRT { lbl = top_lbl, next_elt = 0, rev_elts = [], elt_map = Map.empty }

isEmptySRT :: TopSRT -> Bool
isEmptySRT srt = null (rev_elts srt)

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
buildSRT :: DynFlags -> TopSRT -> CAFSet -> UniqSM (TopSRT, Maybe CmmDecl, C_SRT)
buildSRT dflags topSRT cafs =
  do let
         -- For each label referring to a function f without a static closure,
         -- replace it with the CAFs that are reachable from f.
         sub_srt topSRT localCafs =
           let cafs = Set.elems localCafs
               mkSRT topSRT =
                 do localSRTs <- procpointSRT dflags (lbl topSRT) (elt_map topSRT) cafs
                    return (topSRT, localSRTs)
           in if length cafs > maxBmpSize dflags then
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
                   Just ix -> if next - ix > maxBmpSize dflags then
                                add (addCAF caf srt) rst
                              else srt
                   Nothing -> add (addCAF caf srt) rst
     (topSRT, subSRTs) <- sub_srt topSRT cafs
     let (sub_tbls, blockSRTs) = subSRTs
     return (topSRT, sub_tbls, blockSRTs)

-- Construct an SRT bitmap.
-- Adapted from simpleStg/SRT.lhs, which expects Id's.
procpointSRT :: DynFlags -> CLabel -> Map CLabel Int -> [CLabel] ->
                UniqSM (Maybe CmmDecl, C_SRT)
procpointSRT _ _ _ [] =
 return (Nothing, NoC_SRT)
procpointSRT dflags top_srt top_table entries =
 do (top, srt) <- bitmap `seq` to_SRT dflags top_srt offset len bitmap
    return (top, srt)
  where
    ints = map (expectJust "constructSRT" . flip Map.lookup top_table) entries
    sorted_ints = sort ints
    offset = head sorted_ints
    bitmap_entries = map (subtract offset) sorted_ints
    len = P.last bitmap_entries + 1
    bitmap = intsToBitmap dflags len bitmap_entries

maxBmpSize :: DynFlags -> Int
maxBmpSize dflags = widthInBits (wordWidth dflags) `div` 2

-- Adapted from codeGen/StgCmmUtils, which converts from SRT to C_SRT.
to_SRT :: DynFlags -> CLabel -> Int -> Int -> Bitmap -> UniqSM (Maybe CmmDecl, C_SRT)
to_SRT dflags top_srt off len bmp
  | len > maxBmpSize dflags || bmp == [toStgWord dflags (fromStgHalfWord (srtEscape dflags))]
  = do id <- getUniqueM
       let srt_desc_lbl = mkLargeSRTLabel id
           tbl = CmmData RelocatableReadOnlyData $
                   Statics srt_desc_lbl $ map CmmStaticLit
                     ( cmmLabelOffW dflags top_srt off
                     : mkWordCLit dflags (fromIntegral len)
                     : map (mkStgWordCLit dflags) bmp)
       return (Just tbl, C_SRT srt_desc_lbl 0 (srtEscape dflags))
  | otherwise
  = return (Nothing, C_SRT top_srt off (toStgHalfWord dflags (fromStgWord (head bmp))))
	-- The fromIntegral converts to StgHalfWord

-- Gather CAF info for a procedure, but only if the procedure
-- doesn't have a static closure.
-- (If it has a static closure, it will already have an SRT to
--  keep its CAFs live.)
-- Any procedure referring to a non-static CAF c must keep live
-- any CAF that is reachable from c.
localCAFInfo :: CAFEnv -> CmmDecl -> (CAFSet, Maybe CLabel)
localCAFInfo _      (CmmData _ _) = (Set.empty, Nothing)
localCAFInfo cafEnv proc@(CmmProc _ top_l _ (CmmGraph {g_entry=entry})) =
  case topInfoTable proc of
    Just (CmmInfoTable { cit_rep = rep })
      | not (isStaticRep rep) && not (isStackRep rep)
      -> (cafs, Just (toClosureLbl top_l))
    _other -> (cafs, Nothing)
  where
    cafs = expectJust "maybeBindCAFs" $ mapLookup entry cafEnv

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
mkTopCAFInfo :: [(CAFSet, Maybe CLabel)] -> Map CLabel CAFSet
mkTopCAFInfo localCAFs = foldl addToTop Map.empty g
  where
        addToTop env (AcyclicSCC (l, cafset)) =
          Map.insert l (flatten env cafset) env
        addToTop env (CyclicSCC nodes) =
          let (lbls, cafsets) = unzip nodes
              cafset  = foldr Set.delete (foldl Set.union Set.empty cafsets) lbls
          in foldl (\env l -> Map.insert l (flatten env cafset) env) env lbls

        g = stronglyConnCompFromEdgedVertices
              [ ((l,cafs), l, Set.elems cafs) | (cafs, Just l) <- localCAFs ]

flatten :: Map CLabel CAFSet -> CAFSet -> CAFSet
flatten env cafset = foldSet (lookup env) Set.empty cafset
  where
      lookup env caf cafset' =
          case Map.lookup caf env of
             Just cafs -> foldSet Set.insert cafset' cafs
             Nothing   -> Set.insert caf cafset'

bundle :: Map CLabel CAFSet
       -> (CAFEnv, CmmDecl)
       -> (CAFSet, Maybe CLabel)
       -> (BlockEnv CAFSet, CmmDecl)
bundle flatmap (env, decl@(CmmProc infos lbl _ g)) (closure_cafs, mb_lbl)
  = ( mapMapWithKey get_cafs (info_tbls infos), decl )
 where
  entry = g_entry g

  entry_cafs
    | Just l <- mb_lbl = expectJust "bundle" $ Map.lookup l flatmap
    | otherwise        = flatten flatmap closure_cafs

  get_cafs l _
    | l == entry = entry_cafs
    | otherwise  = if not (mapMember l env)
                      then pprPanic "bundle" (ppr l <+> ppr lbl <+> ppr (info_tbls infos))
                      else flatten flatmap $ expectJust "bundle" $ mapLookup l env

bundle _flatmap (_, decl) _
  = ( mapEmpty, decl )


flattenCAFSets :: [(CAFEnv, [CmmDecl])] -> [(BlockEnv CAFSet, CmmDecl)]
flattenCAFSets cpsdecls = zipWith (bundle flatmap) zipped localCAFs
   where
     zipped    = [ (env,decl) | (env,decls) <- cpsdecls, decl <- decls ]
     localCAFs = unzipWith localCAFInfo zipped
     flatmap   = mkTopCAFInfo localCAFs -- transitive closure of localCAFs

doSRTs :: DynFlags
       -> TopSRT
       -> [(CAFEnv, [CmmDecl])]
       -> IO (TopSRT, [CmmDecl])

doSRTs dflags topSRT tops
  = do
     let caf_decls = flattenCAFSets tops
     us <- mkSplitUniqSupply 'u'
     let (topSRT', gs') = initUs_ us $ foldM setSRT (topSRT, []) caf_decls
     return (topSRT', reverse gs' {- Note [reverse gs] -})
  where
    setSRT (topSRT, rst) (caf_map, decl@(CmmProc{})) = do
       (topSRT, srt_tables, srt_env) <- buildSRTs dflags topSRT caf_map
       let decl' = updInfoSRTs srt_env decl
       return (topSRT, decl': srt_tables ++ rst)
    setSRT (topSRT, rst) (_, decl) =
      return (topSRT, decl : rst)

buildSRTs :: DynFlags -> TopSRT -> BlockEnv CAFSet
          -> UniqSM (TopSRT, [CmmDecl], BlockEnv C_SRT)
buildSRTs dflags top_srt caf_map
  = foldM doOne (top_srt, [], mapEmpty) (mapToList caf_map)
  where
  doOne (top_srt, decls, srt_env) (l, cafs)
    = do (top_srt, mb_decl, srt) <- buildSRT dflags top_srt cafs
         return ( top_srt, maybeToList mb_decl ++ decls
                , mapInsert l srt srt_env )

{-
- In each CmmDecl there is a mapping from BlockId -> CmmInfoTable
- The one corresponding to g_entry is the closure info table, the
  rest are continuations.
- Each one needs an SRT.
- We get the CAFSet for each one from the CAFEnv
- flatten gives us
    [(BlockEnv CAFSet, CmmDecl)]
-
-}


{- Note [reverse gs]

   It is important to keep the code blocks in the same order,
   otherwise binary sizes get slightly bigger.  I'm not completely
   sure why this is, perhaps the assembler generates bigger jump
   instructions for forward refs.  --SDM
-}

updInfoSRTs :: BlockEnv C_SRT -> CmmDecl -> CmmDecl
updInfoSRTs srt_env (CmmProc top_info top_l live g) =
  CmmProc (top_info {info_tbls = mapMapWithKey updInfoTbl (info_tbls top_info)}) top_l live g
  where updInfoTbl l info_tbl
             = info_tbl { cit_srt = expectJust "updInfo" $ mapLookup l srt_env }
updInfoSRTs _ t = t
