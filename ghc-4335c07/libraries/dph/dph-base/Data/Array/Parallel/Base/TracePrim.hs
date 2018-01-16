-- | When `tracePrimEnabled`, DPH programs will print out what flat array
--   primitives they're using at runtime. See `tracePrim` for details.
module Data.Array.Parallel.Base.TracePrim
        ( tracePrim
        , TracePrim(..))
where
import Data.Array.Parallel.Base.Config
import qualified Debug.Trace


-- | Print tracing information to console.
--
--    This function is used to wrap the calls to DPH primitives defined
--    in @dph-prim-par@:"Data.Array.Parallel.Unlifted"
--
--    Tracing is only enabled when `tracePrimEnabled` is `True`.
--    otherwise it's a no-op.
--   
tracePrim :: TracePrim -> a -> a
tracePrim tr x
 | tracePrimEnabled     = Debug.Trace.trace (Prelude.show tr) x
 | otherwise            = x
 

-- | Records information about the use of a flat array primitive.
--
--    These are the operator names that the vectoriser introduces.
--
--    The actual implementation of each operator varies depending on what
--    DPH primitive library is being used.
--
--    We only trace operators that are at least O(n) in complexity. 
data TracePrim
        = TraceReplicate   { traceCount      :: Int}
        | TraceRepeat      { traceCount      :: Int, traceSrcLength   :: Int }
        | TraceIndex       { traceIndex      :: Int, traceSrcLength   :: Int }
        | TraceExtract     { traceStart      :: Int, traceSliceLength :: Int, traceSrcLength :: Int }
        | TraceDrop        { traceCount      :: Int, traceSrcLength   :: Int }
        | TracePermute     { traceSrcLength  :: Int }
        | TraceBPermuteDft { traceSrcLength  :: Int }
        | TraceBPermute    { traceSrcLength  :: Int }
        | TraceMBPermute   { traceSrcLength  :: Int }
        | TraceUpdate      { traceSrcLength  :: Int, traceModLength :: Int }
        | TraceAppend      { traceDstLength  :: Int }
        | TraceInterleave  { traceDstLength  :: Int }
        | TracePack        { traceSrcLength  :: Int }
        | TraceCombine     { traceSrcLength  :: Int }
        | TraceCombine2    { traceSrcLength  :: Int }
        | TraceMap         { traceSrcLength  :: Int }
        | TraceFilter      { traceSrcLength  :: Int, traceDstLength  :: Int }
        | TraceZipWith     { traceSrc1Length :: Int, traceSrc2Length :: Int }
        | TraceFold        { traceSrcLength  :: Int }
        | TraceFold1       { traceSrcLength  :: Int }
        | TraceAnd         { traceSrcLength  :: Int }
        | TraceSum         { traceSrcLength  :: Int }
        | TraceScan        { traceSrcLength  :: Int }
        | TraceIndexed     { traceSrcLength  :: Int }

        -- Enumerations.
        | TraceEnumFromTo          { traceDstLength :: Int }
        | TraceEnumFromThenTo      { traceDstLength :: Int }
        | TraceEnumFromStepLen     { traceDstLength :: Int }
        | TraceEnumFromStepLenEach { traceDstLength :: Int }

        -- Selectors.
        | TraceMkSel2              { traceSrcLength   :: Int }
        | TraceTagsSel2            { traceDstLength   :: Int }
        | TraceIndicesSel2         { traceDstLength   :: Int }
        | TraceElementsSel2_0      { traceSrcLength   :: Int }
        | TraceElementsSel2_1      { traceSrcLength   :: Int }

        | TraceMkSelRep2           { traceSrcLength   :: Int }
        | TraceIndicesSelRep2      { traceSrcLength   :: Int }
        | TraceElementsSelRep2_0   { traceSrcLength   :: Int }
        | TraceElementsSelRep2_1   { traceSrcLength   :: Int }
        
        -- Operations on segmented arrays.
        | TraceReplicate_s         { traceSrcLength   :: Int }
        | TraceReplicate_rs        { traceCount       :: Int, traceSrcLength   :: Int }
        | TraceAppend_s            { traceDstLength   :: Int }
        | TraceAppend_vs           { traceDstLength   :: Int }
        | TraceFold_s              { traceSrcLength   :: Int }
        | TraceFold1_s             { traceSrcLength   :: Int }
        | TraceFold_r              { traceSrcLength   :: Int }
        | TraceSum_r               { traceSrcLength   :: Int }
        | TraceIndices_s           { traceDstLength   :: Int }
        deriving Prelude.Show



