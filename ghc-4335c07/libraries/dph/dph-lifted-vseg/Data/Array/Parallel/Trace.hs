

module Data.Array.Parallel.Trace
        ( Trace (..)
        , Op    (..)
        , traceOp)
where
import Data.Typeable

traceOp :: Op -> a -> a
traceOp _op x = x
--        = Debug.Trace.trace (Prelude.show op) x

data Trace
        = Trace Op
        deriving Show

data Op
        -- Singleton
        = OpSingleton           { traceElemType         :: TypeRep }

        | OpSingletonL          { traceElemType         :: TypeRep
                                , traceContext          :: Int }

        | OpReplicate           { traceElemType         :: TypeRep
                                , traceCount            :: Int }

        -- Replicate
        | OpReplicateL          { traceElemType         :: TypeRep
                                , traceContext          :: Int }

        | OpReplicateS          { traceElemType         :: TypeRep
                                , traceCount            :: Int }

        -- Append
        | OpAppend              { traceElemType         :: TypeRep
                                , traceSrc1Length       :: Int
                                , traceSrc2Length       :: Int
                                , traceDstLength        :: Int }

        | OpAppendL             { traceElemType         :: TypeRep
                                , traceContext          :: Int }


        -- Concat
        | OpConcat              { traceElemType         :: TypeRep
                                , traceSrcLength        :: Int 
                                , traceDstLength        :: Int }

        | OpConcatL             { traceElemType         :: TypeRep
                                , traceContext          :: Int }

        -- Unconcat
        | OpUnconcat            { traceElemType         :: TypeRep
                                , traceCount            :: Int }

        -- Length
        | OpLength
        | OpLengthL             { traceContext          :: Int }

        -- Index
        | OpIndex       
        | OpIndexL              { traceContext          :: Int }

        -- Extract
        | OpExtract             { traceDstLength        :: Int }
        | OpExtractS            { traceDstLength        :: Int }

        -- Slice
        | OpSlice               { traceDstLength        :: Int }
        | OpSliceL              { traceContext          :: Int }

        -- Pack
        | OpPack                { traceDstLength        :: Int }
        | OpPackL               { traceContext          :: Int }
        | OpPackByTag           { traceDstLength        :: Int }

        -- Combine
        | OpCombine2            { traceDstLength        :: Int }

        -- Zip
        | OpZip                 { traceLength           :: Int }
        | OpZipL                { traceLength           :: Int }

        -- Unzip
        | OpUnzip               { traceLength           :: Int }
        | OpUnzipL              { traceLength           :: Int }
        deriving Show
