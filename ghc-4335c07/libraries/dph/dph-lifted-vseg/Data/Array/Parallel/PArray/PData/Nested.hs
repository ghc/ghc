{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP, UndecidableInstances, ParallelListComp #-}
{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

-- | PR instance for nested arrays.
module Data.Array.Parallel.PArray.PData.Nested 
        ( PData(..)
        , PDatas(..)
        , mkPNested
        , concatPR,     concatlPR
        , flattenPR,    takeSegdPD
        , unconcatPR
        , appendlPR
        , indexlPR
        , slicelPR
        , extractvs_delay)
where
import Data.Array.Parallel.Base
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Unit
import Data.Array.Parallel.PArray.PData.Base    as PA
import qualified Data.IntSet                    as IS
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Typeable                  as T
import GHC.Exts
import System.IO.Unsafe


-- Nested arrays --------------------------------------------------------------
data instance PData (PArray a)
        = PNested
        { pnested_uvsegd        :: U.VSegd
          -- ^ Virtual segmentation descriptor. 
          --   Defines a virtual nested array based on physical data.

        , pnested_psegdata      :: PDatas a
          -- ^ Chunks of array data, where each chunk has a linear index space. 

        , pnested_segd          :: U.Segd       -- LAZY FIELD
          -- ^ A demoted version of the VSegd.
          --   If the function that creates the array already has the plain Segd,
          --   then it should stash it here, otherwise build a thunk that makes it.

        , pnested_flat          :: PData a      -- LAZY FIELD
          -- ^ A pre-concatenated version of the array.
          --   If the function that creates the array already has a flat form,
          --   then it should stash it here, otherwise build a thunk that makes it.
        }

deriving instance T.Typeable PData


-- TODO: should we unpack the vsegd fields here?
data instance PDatas (PArray a)
        = PNesteds (V.Vector (PData (PArray a)))


-- | Construct a nested array.
mkPNested :: PR a
          => U.VSegd -> PDatas a
          -> U.Segd  -> PData  a
          -> PData (PArray a)
mkPNested = PNested
{-# INLINE_PDATA mkPNested #-}


-- Projections ----------------------------------------------------------------
-- These functions take concatenated forms of the vsegd and array data from
-- the representation of the nested array. Although the projections themselves
-- are O(1), they could return thunks.

-- | Concatenate a nested array.
concatPR :: PR a => PData (PArray a) -> PData a
concatPR (PNested _ _ _ flat)
        = flat
{-# INLINE concatPR #-}

-- | Take the segment descriptor from a nested array and demote it to a
--   plain Segd.
takeSegdPD :: PData (PArray a) -> U.Segd
takeSegdPD (PNested _ _ segd _) 
        = segd
{-# INLINE_PDATA takeSegdPD #-}

-- | Flatten a nested array, yielding a plain segment descriptor and 
--   concatenated data.
--
flattenPR :: PR a => PData (PArray a) -> (U.Segd, PData a)
flattenPR (PNested _ _ segd flat)
        = (segd, flat)
{-# INLINE_PDATA flattenPR #-}


-- PR Instances ---------------------------------------------------------------
instance U.Elt (Int, Int, Int)

instance PR a => PR (PArray a) where
  -- TODO: make this check all sub arrays as well
  -- TODO: ensure that all psegdata arrays are referenced from some psegsrc.
  -- TODO: shift segd checks into associated modules.
  {-# NOINLINE validPR #-}
  validPR (PNested vsegd pdatas _ _)
   = let vsegids        = U.takeVSegidsOfVSegd vsegd
         ssegd          = U.takeSSegdOfVSegd   vsegd
         pseglens       = U.lengthsOfSSegd     ssegd
         psegstarts     = U.startsOfSSegd      ssegd
         psegsrcs       = U.sourcesOfSSegd     ssegd

         -- The lengths of the pseglens, psegstarts and psegsrcs fields must all be the same
         fieldLensOK
                = validBool "nested array field lengths not identical"
                $ and 
                [ U.length psegstarts == U.length pseglens
                , U.length psegsrcs   == U.length pseglens ]

         -- Every vseg must reference a valid pseg.
         vsegsRefOK
                = validBool "nested array vseg doesn't ref pseg"
                $ U.and
                $ U.map (\vseg -> vseg < U.length pseglens) vsegids
                         
         -- Every pseg source id must point to a flat data array
         psegsrcsRefOK
                = validBool "nested array psegsrc doesn't ref flat array"
                $ U.and 
                $ U.map (\srcid -> srcid < lengthdPR pdatas) psegsrcs

         -- Every physical segment must be a valid slice of the corresponding flat array.
         -- 
         --   We allow psegs with len 0, start 0 even if the flat array is empty.
         --   This occurs with [ [] ]. 
         -- 
         --   As a generalistion of above, we allow segments with len 0, start <= srclen.
         --   This occurs when there is an empty array as the last segment
         --   For example:
         --        [ [5, 4, 3, 2] [ ] ].
         --        PNested  vsegids:    [0,1]
         --                 pseglens:   [4,0]
         --                 psegstarts: [0,4]  -- last '4' here is <= length of flat array
         --                 psegsrcs:   [0,0]
         --                 PInt        [5, 4, 3, 2]
         --
         psegSlicesOK 
                = validBool "nested array pseg slices are invalid"
                $ U.and 
                $ U.zipWith3 
                        (\len start srcid
                           -> let pdata = pdatas `indexdPR` srcid
                              in  and [ coversPR (len == 0) pdata start
                                      , coversPR True       pdata (start + len) ])
                        pseglens psegstarts psegsrcs

         -- Every pseg must be referenced by some vseg.
         vsegs   = IS.fromList $ U.toList vsegids
         psegsReffedOK
                =  validBool "nested array pseg not reffed by vseg"
                $  (U.length pseglens == 0) 
                || (U.and $ U.map (flip IS.member vsegs) 
                          $ U.enumFromTo 0 (U.length pseglens - 1))

     in unsafePerformIO
         $ do {-print fieldLensOK
              print vsegsRefOK
              print psegsrcsRefOK
              print psegSlicesOK
              print psegsReffedOK-}
              return $ 
               and [ fieldLensOK
                   , vsegsRefOK
                   , psegsrcsRefOK
                   , psegSlicesOK
                   , psegsReffedOK ]

  {-# NOINLINE nfPR #-}
  nfPR    = error "nfPR[PArray]: not defined yet"


  {-# NOINLINE similarPR #-}
  similarPR (PArray _ pdata1) (PArray _ pdata2)
        = V.and $ V.zipWith similarPR 
                        (toVectorPR pdata1)
                        (toVectorPR pdata2)


  {-# NOINLINE coversPR #-}
  coversPR weak (PNested vsegd _ _ _) ix
   | weak       = ix <= (U.length $ U.takeVSegidsOfVSegd vsegd)
   | otherwise  = ix <  (U.length $ U.takeVSegidsOfVSegd vsegd)

  {-# NOINLINE pprpPR #-}
  pprpPR (PArray n# pdata)
        =   (text "PArray " <+> int (I# n#))
        $+$ ( nest 4 
            $ pprpDataPR pdata)

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PNested vsegd pdatas _ _)
        =   text "PNested"
        $+$ ( nest 4
            $ pprp vsegd $$ pprp pdatas)

  {-# NOINLINE typeRepPR #-}
  typeRepPR arr@(PArray _ pdata)
        = T.typeOf1 arr
                `T.mkAppTy` typeRepDataPR pdata

  {-# NOINLINE typeRepDataPR #-}
  typeRepDataPR (PNested _ pdatas _ _)
        = T.typeOf1 (PArray 0# (emptyPR :: PData ()))
                `T.mkAppTy` typeRepDatasPR pdatas

  {-# NOINLINE typeRepDatasPR #-}
  -- CAREFUL: 
  --  Our generics setup turns implicitly recursive types in the source
  --  program (defined via data type declarations) into explicitly recursive
  --  ones in the Core program. If there is no data in the PNesteds then we
  --  can't continue printing the type. An empty vector may have an infinite
  --  type, and we could loop forever. This happens in the Barnes-Hut benchmark,
  --  due to the recursive tree structure.
  typeRepDatasPR (PNesteds vec)
        = T.typeOf1 (PArray 0# (emptyPR :: PData ()))
                `T.mkAppTy` (if V.length vec == 0 
                                then T.typeOf ()
                                else typeRepDataPR (vec V.! 0))


  -- Constructors -----------------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR = PNested U.emptyVSegd emptydPR U.emptySegd emptyPR


  -- When replicating an array we use the source as the single physical
  -- segment, then point all the virtual segments to it.
  {-# INLINE_PDATA replicatePR #-}
  replicatePR c (PArray n# pdata)
   = checkNotEmpty "replicatePR[PArray]" c
   $ let -- All virtual segments point to the same physical segment.
         vsegd   = U.replicatedVSegd (I# n#) c

         -- There is only one physical array.
         pdatas  = singletondPR pdata

         -- Pre-concatenated version.
         -- If the consumer pulls on this then the single segment gets physically copied.
         segd   = U.unsafeDemoteToSegdOfVSegd vsegd
         flat   = extractvs_delay pdatas vsegd

     in  PNested vsegd pdatas segd flat
                

  -- For segmented replicates, we just replicate the vsegids field.
  --
  -- TODO: Does replicate_s really need the whole segd,
  --       or could we get away without creating the indices field?
  --
  -- TODO: If we know the lens does not contain zeros, then we don't need
  --       to cull down the psegs.
  --
  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PNested uvsegd pdatas _ _)
   = let vsegd' = U.updateVSegsOfVSegd (U.replicate_s segd) uvsegd
         segd'  = U.unsafeDemoteToSegdOfVSegd vsegd'
         flat'  = extractvs_delay pdatas vsegd'
     in  PNested vsegd' pdatas segd' flat'


  -- Append nested arrays by appending the segment descriptors,
  -- and putting all physical arrays in the result.
  {-# NOINLINE appendPR #-}
  appendPR (PNested uvsegd1 pdatas1 _ _) (PNested uvsegd2 pdatas2 _ _)
   = let vsegd'  = U.appendVSegd
                        uvsegd1 (lengthdPR pdatas1) 
                        uvsegd2 (lengthdPR pdatas2)

         pdatas' = appenddPR pdatas1 pdatas2
         segd'   = U.unsafeDemoteToSegdOfVSegd vsegd'
         flat'   = extractvs_delay pdatas' vsegd'

     in  PNested vsegd' pdatas' segd' flat'
     

  -- Performing segmented append requires segments from the physical arrays to
  -- be interspersed, so we need to copy data from the second level of nesting.  
  --
  -- Each element of @xarr@ is a @PData (PArray a)@, and contains a vector of @PData a@.
  -- We collect all the @PData a@s in @xarr@ and @yarr@ into one vector,
  -- then do segmented append (@U.append_vs@) for the lengths and starts.
  --
  -- The sources are segmented append of the input sources summed with the
  -- scan of the length of each @PDatas a@ in @xarr@ and @yarr@,
  -- to find the index into the concatenated source arrays.
  {-# NOINLINE appendvsPR #-}
  appendvsPR rsegd segd1 (PNesteds xarr) segd2 (PNesteds yarr)
   = let 
         -- lengths of flattened segments
         flen           = U.lengthsSegd . takeSegdPD
         xlens          = U.fromVectors $ V.map flen xarr
         ylens          = U.fromVectors $ V.map flen yarr

         -- scattered segment starts
         fstart         = U.startsOfSSegd . U.takeSSegdOfVSegd . pnested_uvsegd
         xstarts        = U.fromVectors $ V.map fstart xarr
         ystarts        = U.fromVectors $ V.map fstart yarr

         -- input sources (without sum of scan)
         fsource        = U.sourcesOfSSegd . U.takeSSegdOfVSegd . pnested_uvsegd
         xsources       = U.fromVectors $ V.map fsource xarr
         ysources       = U.fromVectors $ V.map fsource yarr

         -- data arrays, the result will have these concatenated
         -- scan of lengths is used to generate new source indices
         fdata          = toVectordPR . pnested_psegdata
         xdata          = V.map fdata xarr
         ydata          = V.map fdata yarr

         -- why does V.concat take list, not vector?
         ccat   = V.concatMap id

         -- concatenate input data arrays
         datas' = fromVectordPR (ccat xdata V.++ ccat ydata)

         -- get data lengths to generate new source indices
         xdatalens = V.map V.length xdata
         ydatalens = V.map V.length ydata

         -- increase each source by scan so far
         getsrc srcs inc = U.map (+inc) srcs

         -- increase x's sources by scan of lengths
         xsrc'  = U.fromVectors $ V.zipWith getsrc (U.toVectors xsources)
                $ V.prescanl (+) 0 xdatalens

         -- increase y's sources, starting from sum of xdatalens
         ysrc'  = U.fromVectors $ V.zipWith getsrc (U.toVectors ysources)
                $ V.prescanl (+) (V.sum xdatalens) ydatalens


         -- segmented append to get new sources, lengths, and starts.
         -- TODO: would be nice if append_vs could do triples.
         src'   = U.append_vs rsegd segd1 xsrc'
                                    segd2 ysrc'

         segd'  = U.lengthsToSegd
                $ U.append_vs rsegd segd1 xlens
                                    segd2 ylens

         start' = U.append_vs rsegd segd1 xstarts
                                    segd2 ystarts

         -- generate vseg with new sources etc
         vsegd' = U.promoteSSegdToVSegd
                $ U.mkSSegd start' src' segd'

         -- lazy flattening of data
         flat'  = extractvs_delay datas' vsegd'

     in PNested vsegd' datas' segd' flat'



  -- Projections ------------------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PNested vsegd _ _ _)
        = U.lengthOfVSegd vsegd


  -- To index into a nested array, first determine what segment the index
  -- corresponds to, and extract that as a slice from that physical array.
  --
  -- IMPORTANT: 
  --   We need to go through the vsegd here, instead of demanding the
  --   flat version, because we don't want to force creation of the
  --   entire manifest array.
  {-# INLINE_PDATA indexPR #-}
  indexPR (PNested uvsegd pdatas _ _) ix
   | (pseglen@(I# pseglen#), psegstart, psegsrcid)    <- U.getSegOfVSegd uvsegd ix
   = let !psrc          = pdatas `indexdPR` psegsrcid
         !pdata'        = extractPR psrc psegstart pseglen
     in  PArray pseglen# pdata'


  {-# INLINE_PDATA indexsPR #-}
  indexsPR pdatas@(PNesteds arrs) srcixs
   = let (srcids, ixs)  = U.unzip srcixs
   
         -- See Note: psrcoffset
         !psrcoffset    = V.prescanl (+) 0
                        $ V.map (lengthdPR . pnested_psegdata) arrs

         -- length, start and srcid of the segments we're returning.
         --   Note that we need to offset the srcid 
         -- TODO: don't unbox the VSegd for every iteration.
         seginfo :: U.Array (Int, Int, Int)
         seginfo 
          = U.zipWith (\srcid ix -> 
                        let (PNested vsegd _ _ _)  = pdatas `indexdPR` srcid
                            (len, start, srcid')   = U.getSegOfVSegd vsegd ix
                        in  (len, start, srcid' + (psrcoffset `V.unsafeIndex` srcid)))
                srcids
                ixs

         (pseglens', psegstarts', psegsrcs')    
                        = U.unzip3 seginfo
                
         -- TODO: check that doing lengthsToSegd won't cause overflow
         segd'   = U.lengthsToSegd pseglens'
         vsegd'  = U.promoteSSegdToVSegd
                 $ U.mkSSegd psegstarts' psegsrcs' segd'
                                 
          -- All flat data arrays in the sources go into the result.
         pdatas' = fromVectordPR
                 $ V.concat $ V.toList 
                 $ V.map (toVectordPR . pnested_psegdata) arrs
   
         flat'  = extractvs_delay pdatas' vsegd'
   
     in  PNested vsegd' pdatas' segd' flat'


  {-# INLINE_PDATA indexvsPR #-}
  indexvsPR pdatas vsegd srcixs
   = let !vsegids         = U.takeVSegidsRedundantOfVSegd vsegd
         !ssegd           = U.takeSSegdRedundantOfVSegd   vsegd
         !sources         = U.sourcesOfSSegd   ssegd
         !starts          = U.startsOfSSegd    ssegd

         !srcixs' 
          = U.map (\(ix1, ix2)
                   -> let !psegid = U.index "indexvsPR/vsegids" vsegids ix1
                          !source = U.index "indexvsPR/sources" sources psegid
                          !start  = U.index "indexvsPR/starts"  starts  psegid
                      in  (source, start + ix2))
                   srcixs

     in  indexsPR pdatas srcixs'


  -- To extract a range of elements from a nested array, perform the extract
  -- on the vsegids field. The `updateVSegsOfUVSegd` function will then filter
  -- out all of the psegs that are no longer reachable from the new vsegids.
  --
  -- IMPORTANT: 
  --   We need to go through the vsegd here, instead of demanding the
  --   flat version, because we don't want to force creation of the
  --   entire manifest array.
  {-# INLINE_PDATA extractPR #-}
  extractPR (PNested uvsegd pdatas _ _) start len
   = let vsegd' = U.updateVSegsOfVSegd (\vsegids -> U.extract vsegids start len) uvsegd
         segd'  = U.unsafeDemoteToSegdOfVSegd vsegd'
         flat'  = extractvs_delay pdatas vsegd'
     in  PNested vsegd' pdatas segd' flat'


  -- [Note: psrcoffset]
  -- ~~~~~~~~~~~~~~~~~~
  -- As all the flat data arrays in the sources are present in the result array,
  -- we need to offset the psegsrcs field when combining multiple sources.
  -- 
  -- Exaple
  --  Source Arrays:
  --   arr0  ...
  --         psrcids  :  [0, 0, 0, 1, 1]
  --         psegdata :  [PInt xs1, PInt xs2]
  --
  --   arr1  ... 
  --         psrcids  :  [0, 0, 1, 1, 2, 2, 2]
  --         psegdata :  [PInt ys1, PInt ys2, PInt ys3]
  -- 
  --   Result Array:
  --         psrcids  :  [...]
  --         psegdata :  [PInt xs1, PInt xs2, PInt ys1, PInt ys2, PInt ys3] 
  --
  --  Note that references to flatdata arrays [0, 1, 2] in arr1 need to be offset
  --  by 2 (which is length arr0.psegdata) to refer to the same flat data arrays
  --  in the result.
  -- 
  --  We encode these offsets in the psrcoffset vector:
  --       psrcoffset :  [0, 2]
  --
  --   TODO: cleanup pnested projections
  --         use getSegOfUVSegd like in indexlPR
  --
  {-# NOINLINE extractssPR #-}
  extractssPR (PNesteds arrs) ussegd
   = let 
   
         segsrcs        = U.sourcesOfSSegd ussegd
         seglens        = U.lengthsOfSSegd ussegd

         vsegidss       = V.map (U.takeVSegidsOfVSegd . pnested_uvsegd) arrs
         vsegids_src    = U.extracts_nss ussegd vsegidss
         srcids'        = U.replicate_s (U.lengthsToSegd seglens) segsrcs

         -- See Note: psrcoffset
         psrcoffset     = V.prescanl (+) 0 
                        $ V.map (lengthdPR . pnested_psegdata) arrs

         -- Unpack the lens and srcids arrays so we don't need to 
         -- go though all the segment descriptors each time.
         !arrs_pseglens   = V.map (U.lengthsOfSSegd . U.takeSSegdOfVSegd . pnested_uvsegd) arrs
         !arrs_psegstarts = V.map (U.startsOfSSegd  . U.takeSSegdOfVSegd . pnested_uvsegd) arrs
         !arrs_psegsrcids = V.map (U.sourcesOfSSegd . U.takeSSegdOfVSegd . pnested_uvsegd) arrs

         !here'         = "extractssPR[Nested]"
         -- Function to get one element of the result.
         {-# INLINE get #-}
         get srcid vsegid
          = let !pseglen        =  U.index here' (arrs_pseglens   `V.unsafeIndex` srcid) vsegid
                !psegstart      =  U.index here' (arrs_psegstarts `V.unsafeIndex` srcid) vsegid
                !psegsrcid      = (U.index here' (arrs_psegsrcids `V.unsafeIndex` srcid) vsegid)
                                + (psrcoffset `V.unsafeIndex` srcid)
            in  (pseglen, psegstart, psegsrcid)
            
         (pseglens', psegstarts', psegsrcs')
                = U.unzip3 $ U.zipWith get srcids' vsegids_src

         -- All flat data arrays in the sources go into the result.
         pdatas'        = fromVectordPR
                        $ V.concat $ V.toList 
                        $ V.map (toVectordPR . pnested_psegdata) arrs
                   
         -- Build the result segment descriptor.
         segd'          = U.lengthsToSegd pseglens'
         vsegd'         = U.promoteSSegdToVSegd
                        $ U.mkSSegd psegstarts' psegsrcs' segd'
   
         flat'          = extractvs_delay pdatas' vsegd'
   
     in  PNested vsegd' pdatas' segd' flat'


  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR pdatas vsegd
   = extractssPR pdatas (U.unsafeDemoteToSSegdOfVSegd vsegd)

                
  -- Pack and Combine -------------------------------------
  -- Pack the vsegids to determine which of the vsegs are present in the result.
  --  eg  tags:           [0 1 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0 1 1]   tag = 1
  --      vsegids:        [0 0 1 1 2 2 2 2 3 3 4 4 4 5 5 5 5 6 6]
  --  =>  vsegids_packed: [  0 1 1         3         5   5   6 6]
  --       
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PNested vsegd pdatas _ _) tags tag
   = let vsegd' = U.updateVSegsOfVSegd (\vsegids -> U.packByTag vsegids tags tag) vsegd
         segd'  = U.unsafeDemoteToSegdOfVSegd vsegd'
         flat'  = extractvs_delay pdatas vsegd'
     in  PNested vsegd' pdatas segd' flat'


  -- Combine nested arrays by combining the segment descriptors, 
  -- and putting all physical arrays in the result.
  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel2 (PNested vsegd1 pdatas1 _ _) (PNested vsegd2 pdatas2 _ _)
   = let vsegd'  = U.combine2VSegd sel2 
                        vsegd1 (lengthdPR pdatas1)
                        vsegd2 (lengthdPR pdatas2)

         pdatas' = appenddPR pdatas1 pdatas2
         segd'   = U.unsafeDemoteToSegdOfVSegd vsegd'
         flat'   = extractvs_delay pdatas' vsegd'
     in  PNested vsegd' pdatas' segd' flat'


  -- Conversions ----------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR xx
   | V.length xx == 0 = emptyPR
   | otherwise
   = let segd   = U.lengthsToSegd $ U.fromList $ V.toList $ V.map PA.length xx
         vsegd  = U.promoteSegdToVSegd segd
         pdata  = V.foldl1 appendPR $ V.map takeData xx
         pdatas = singletondPR pdata
         flat   = extractvs_delay pdatas vsegd
     in  PNested vsegd pdatas segd flat


  {-# NOINLINE toVectorPR #-}
  toVectorPR arr@(PNested vsegd _ _ _)
   = let len    = U.length $ U.takeVSegidsOfVSegd vsegd
     in  V.generate len (indexPR arr)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PNesteds $ V.empty
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR pdata
        = PNesteds $ V.singleton pdata

  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PNesteds vec)
        = V.length vec
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PNesteds vec) ix
        = vec `V.unsafeIndex` ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PNesteds xs) (PNesteds ys)
        = PNesteds $ xs V.++ ys

  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
        = PNesteds vec
        
  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PNesteds vec)
        = vec


-------------------------------------------------------------------------------
-- | Wrapper for extracts that is NOT INLINED.
--
--   This is experimental, used to initialise the pnested_flat field
--   of a nested array. It's' marked at NOINLINE to avoid code explosion.
---
--   TODO: at a later fusion stage we could rewrite this to an INLINED
--         version to generate core for the occurrences we actually use.
extractvs_delay :: PR a => PDatas a -> U.VSegd -> PData a
extractvs_delay pdatas vsegd
        = extractvsPR pdatas vsegd
{-# NOINLINE extractvs_delay #-}
--  NOINLINE because we don't want a copy of the extracts loop to 
--           be generated at the use site.


------------------------------------------------------------------------------
-- | O(len result). Lifted indexing
indexlPR :: PR a => PData (PArray a) -> PData Int -> PData a
indexlPR (PNested vsegd pdatas _ _) (PInt ixs)
 = indexvsPR pdatas vsegd 
        (U.zip  (U.enumFromTo 0 (U.length ixs - 1))
                ixs)
{-# INLINE_PDATA indexlPR #-}


-- concatlPR ------------------------------------------------------------------
-- | Lifted concatenation.
-- 
--   Concatenate all the arrays in a triply nested array.
--
concatlPR :: PR a => PData (PArray (PArray a)) -> PData (PArray a)
concatlPR arr
 = let  (segd1, darr1)  = flattenPR arr
        (segd2, darr2)  = flattenPR darr1

        -- Generate indices for the result array
        -- See Note: Empty Arrays on End.
        ixs1            = U.indicesSegd segd1
        ixs2            = U.indicesSegd segd2
        len2            = U.length ixs2

        ixs'            = U.map (\ix -> if ix >= len2
                                                then 0
                                                else U.index "concatlPR" ixs2 ix)
                        $ ixs1

        segd'           = U.mkSegd (U.sum_s segd1 (U.lengthsSegd segd2))
                                   ixs'
                                   (U.elementsSegd segd2)

        vsegd'          = U.promoteSegdToVSegd segd'
        pdatas'         = singletondPR flat'
        flat'           = darr2

   in   PNested vsegd' pdatas' segd' flat'
{-# INLINE_PDATA concatlPR #-}

--  [Note: Empty Arrays on End]
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  There is a tedious edge case when the last segment in the nested
--  array has length 0. For example:
--
--    concatl [ [[1, 2, 3] [4, 5, 6]] [] ]
--  
--  After the calls to flattenPR we get:
--   segd1: lengths1 = [ 2 0 ]
--          indices1 = [ 0 2 ]

--   segd2: lengths2 = [ 3 3 ]
--          indices2 = [ 0 3 ]
-- 
--  The problem is that the last element of 'indices1' points off the end
--  of 'indices2' so we can't use use 'backpermute' as we'd like to:
--    ixs' = (U.bpermute (U.indicesSegd segd2) (U.indicesSegd segd1))        
--  Instead, we have to explicitly check for the out-of-bounds condition.
--
--  TODO: We want a faster way of doing this, that doesn't require the 
--        test for every element.



-- unconcatPR -----------------------------------------------------------------
-- | Build a nested array given a single flat data vector, 
--   and a template nested array that defines the segmentation.

--   Although the template nested array may be using vsegids to describe
--   internal sharing, the provided data array has manifest elements
--   for every segment. Because of this we need flatten out the virtual
--   segmentation of the template array.
--
unconcatPR :: PR b => PData (PArray a) -> PData b -> PData (PArray b)
unconcatPR (PNested _ _ segd _) pdata
 = {-# SCC "unconcatPD" #-}
   let  -- Demote the vsegd to a manifest vsegd so it contains all the segment
        -- lengths individually without going through the vsegids.
        -- Then Rebuild the vsegd based on the manifest vsegd. 
        -- The vsegids will be just [0..len-1], but this field is constructed
        -- lazilly and consumers aren't required to demand it.
        vsegd'         = U.promoteSegdToVSegd segd
        pdatas'         = singletondPR pdata
   in   PNested vsegd' pdatas' segd pdata
{-# INLINE_PDATA unconcatPR #-}


-- appendlPR ------------------------------------------------------------------
-- | Lifted append.
--   Both arrays must contain the same number of elements.
appendlPR :: PR a => PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendlPR  arr1@(PNested vsegd1 darr1 _ _) arr2@(PNested vsegd2 darr2 _ _)
 = let  segd1           = takeSegdPD arr1
        segd2           = takeSegdPD arr2
        segd'           = U.plusSegd segd1 segd2
        vsegd'          = U.promoteSegdToVSegd segd'

        flat'           = appendvsPR segd' vsegd1 darr1 vsegd2 darr2
        pdatas'         = singletondPR flat'
   in   PNested vsegd' pdatas' segd' flat'
{-# INLINE_PDATA appendlPR #-}


-- slicelPR -------------------------------------------------------------------
-- | Extract some slices from some arrays.
--
--   All three parameters must have the same length, and we take
--   one slice from each of the source arrays. 

--   TODO: cleanup pnested projections
slicelPR
        :: PR a
        => PData Int            -- ^ Starting indices of slices.
        -> PData Int            -- ^ Lengths of slices.
        -> PData (PArray a)     -- ^ Arrays to slice.
        -> PData (PArray a)

slicelPR (PInt sliceStarts) (PInt sliceLens)
         (PNested vsegd pdatas _segd _flat)
 = let  -- Build the new Segd
        segd'           = U.lengthsToSegd sliceLens

        -- Build the new SSegd
        vsegids         = U.takeVSegidsOfVSegd vsegd
        ssegd           = U.takeSSegdOfVSegd   vsegd
        psegstarts      = U.startsOfSSegd      ssegd
        psegsrcs        = U.sourcesOfSSegd     ssegd

        psegstarts'     = U.zipWith (+) (U.bpermute psegstarts vsegids) sliceStarts
        psegsources'    = U.bpermute psegsrcs vsegids
        ssegd'          = U.mkSSegd psegstarts' psegsources' segd'

        -- Promote SSegd to a VSegd
        vsegd'          = U.promoteSSegdToVSegd ssegd'
        flat'           = extractvs_delay pdatas vsegd'

   in   PNested vsegd' pdatas segd' flat'
{-# NOINLINE slicelPR #-}
--  NOINLINE because it won't fuse with anything.
--  The operation is also entierly on the segment descriptor, so we don't 
--  need to inline it to specialise it for the element type.


-- Testing --------------------------------------------------------------------
-- TODO: slurp debug flag from base 
validBool :: String -> Bool -> Bool
validBool str b
        = if b  then True 
                else error $ "validBool check failed -- " ++ str


-- Pretty ---------------------------------------------------------------------
deriving instance (Show (PDatas a), Show (PData a)) => Show (PDatas (PArray a))
deriving instance (Show (PDatas a), Show (PData a)) => Show (PData  (PArray a))


