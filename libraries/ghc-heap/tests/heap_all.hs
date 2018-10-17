-- The simplifier changes the shapes of closures that we expect.
{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

import GHC.Exts.Heap

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Monad
import GHC.Exts
import GHC.Int
import GHC.IO
import GHC.IORef
import GHC.MVar
import GHC.Stack
import GHC.STRef
import GHC.Word
import System.Environment
import System.Mem

exData :: (Int,Int)
exData = (1,2)

exItbl :: StgInfoTable
exItbl = StgInfoTable
    { entry = Nothing
    , ptrs = 0
    , nptrs = 0
    , tipe = toEnum 0
    , srtlen = 0
    , code = Nothing
    }

exConstrClosure :: Closure
exConstrClosure = ConstrClosure
    { info = exItbl{tipe=CONSTR_1_0, ptrs=1, nptrs=0}
    , ptrArgs = []
    , dataArgs = []
    , pkg = "base"
    , modl = "GHC.Base"
    , name = "Just"
    }

exFunClosure :: Closure
exFunClosure = FunClosure
    { info = exItbl{tipe=FUN_0_1, ptrs=0, nptrs=1}
    , ptrArgs = []
    , dataArgs = [0]
    }

exThunkClosure :: Closure
exThunkClosure = ThunkClosure
    { info = exItbl{tipe=THUNK}
    , ptrArgs = []
    , dataArgs = []
    }

exSelectClosure :: Closure
exSelectClosure = SelectorClosure
    { info = exItbl
    , selectee = asBox exData
    }

exPAPClosure :: Closure
exPAPClosure = PAPClosure
    { info = exItbl{tipe=PAP}
    , arity = 1
    , n_args = 1
    , fun = asBox id
    , payload = []
    }

exAPClosure :: Closure
exAPClosure = APClosure
    { info = exItbl{tipe=AP}
    , arity = 0
    , n_args = 0
    , fun = asBox id
    , payload = []
    }

exAPStackClosure :: Closure
exAPStackClosure = APStackClosure
    { info = exItbl{tipe=AP_STACK}
    , fun = asBox id
    , payload = []
    }

exIndClosure :: Closure
exIndClosure = IndClosure
    { info = exItbl{tipe=IND}
    , indirectee = asBox []
    }

exBCOClosure :: Closure
exBCOClosure = BCOClosure
    { info = exItbl{tipe=BCO, ptrs=4}
    , instrs = asBox []
    , literals = asBox []
    , bcoptrs = asBox []
    , arity = 0
    , size = 5
    , bitmap = []
    }

exBlackholeClosure :: Closure
exBlackholeClosure = BlackholeClosure
    { info = exItbl{tipe=BLACKHOLE}
    , indirectee = asBox []
    }

exArrWordsClosure :: Closure
exArrWordsClosure = ArrWordsClosure
    { info = exItbl{tipe=ARR_WORDS}
    , bytes = 0
    , arrWords = []
    }

exMutArrClosure :: Closure
exMutArrClosure = MutArrClosure
    { info = exItbl{tipe=MUT_ARR_PTRS_DIRTY}
    , mccPtrs = 0
    , mccSize = 0
    , mccPayload = []
    }

exMVarClosure :: Closure
exMVarClosure = MVarClosure
    { info = exItbl{tipe=MVAR_DIRTY}
    , queueHead = asBox []
    , queueTail = asBox []
    , value = asBox 0
    }

exMutVarClosure :: Closure
exMutVarClosure = MutVarClosure
    { info = exItbl{tipe=MUT_VAR_DIRTY}
    , var = asBox []
    }

exBlockingQClosure :: Closure
exBlockingQClosure = BlockingQueueClosure
    { info = exItbl{tipe=BLOCKING_QUEUE}
    , link = asBox []
    , blackHole = asBox []
    , owner = asBox []
    , queue = asBox []
    }

exIntClosure :: Closure
exIntClosure = IntClosure
    { ptipe = PInt, intVal = 42 }

exWordClosure :: Closure
exWordClosure = WordClosure
    { ptipe = PWord, wordVal = 42 }

exInt64Closure :: Closure
exInt64Closure = Int64Closure
    { ptipe = PInt64, int64Val = 42 }

exWord64Closure :: Closure
exWord64Closure = Word64Closure
    { ptipe = PWord64, word64Val = 42 }

exAddrClosure :: Closure
exAddrClosure = AddrClosure
    { ptipe = PAddr, addrVal = 42 }

exFloatClosure :: Closure
exFloatClosure = FloatClosure
    { ptipe = PFloat, floatVal = 42.0 }

exDoubleClosure :: Closure
exDoubleClosure = DoubleClosure
    { ptipe = PDouble, doubleVal = 42.0 }

exOtherClosure :: Closure
exOtherClosure = OtherClosure
    { info = exItbl
    , hvalues = []
    , rawWords = []
    }

data A = A (Array# Int)
data MA = MA (MutableArray# RealWorld Int)
data BA = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)
data B = B BCO#
data APC a = APC a

main :: IO ()
main = do

    --------------------------------------------
    -- Objects to inspect

    MA ma <- IO $ \s ->
        case newArray# 0# 0 s of
            (# s1, x #) -> (# s1, MA x #)
    A a <- IO $ \s ->
        case freezeArray# ma 0# 0# s of
            (# s1, x #) -> (# s1, A x #)
    MBA mba <- IO $ \s ->
        case newByteArray# 0# s of
            (# s1, x #) -> (# s1, MBA x #)
    BA ba <- IO $ \s ->
        case newByteArray# 0# s of
            (# s1, x #) ->
                case unsafeFreezeByteArray# x s1 of
                    (# s2, y #) -> (# s2, BA y #)
    B bco <- IO $ \s ->
        case newBCO# ba ba a 0# ba s of
            (# s1, x #) -> (# s1, B x #)
    APC apc <- IO $ \s ->
        case mkApUpd0# bco of
            (# x #) -> (# s, APC x #)

    --------------------------------------------
    -- Closures

    -- Constructor
    let !con = Just 1
    getClosureData con >>=
        assertClosuresEq exConstrClosure

    -- Function
    let !fun = \x -> x + 1
    getClosureData fun >>=
        assertClosuresEq exFunClosure

    -- Thunk
    let thunk = map (+2) [1,2,3]
    getClosureData thunk >>=
        assertClosuresEq exThunkClosure

    -- Selector
    -- FAILING: Getting THUNK not THUNK_SELECTOR
    -- let sel = case exData of (a,_) -> a
    -- getClosureData sel >>=
    --     assertClosuresEq exSelectClosure

    -- Partial application
    let !f = map (+2)
    getClosureData f >>=
        assertClosuresEq exPAPClosure

    -- Applied function
    getClosureData apc >>=
        assertClosuresEq exAPClosure

    -- Suspended thunk evaluation
    -- getClosureData (Just 1) >>=
    --     assertClosuresEq exAPStackClosure

    -- Indirection
    -- getClosureData (Just 1) >>=
    --     assertClosuresEq exIndClosure

    -- ByteCode object
    getClosureData bco >>=
        assertClosuresEq exBCOClosure

    -- Blackhole
    -- getClosureData (Just 1) >>=
    --     assertClosuresEq exBlackholeClosure

    -- Byte array
    getClosureData ba >>=
        assertClosuresEq exArrWordsClosure

    -- Mutable pointer array
    getClosureData ma >>=
        assertClosuresEq exMutArrClosure

    -- MVar
    (MVar v) <- newMVar 1
    getClosureData (unsafeCoerce# v) >>=
        assertClosuresEq exMVarClosure

    -- MutVar
    (IORef (STRef v)) <- newIORef 1
    getClosureData v >>=
        assertClosuresEq exMutVarClosure

    -- Blocking queue
    -- getClosureData (Just 1) >>=
    --    assertClosuresEq exBlockingQClosure

    -----------------------------------------------------
    -- Unboxed unlifted types

    -- Primitive Int
    let (I# v) = 42
    getClosureData v >>=
        assertClosuresEq exIntClosure

    -- Primitive Word
    let (W# v) = 42
    getClosureData v >>=
        assertClosuresEq exWordClosure

    -- Primitive Int64
    -- FAILING: On 64-bit platforms, v is a regular Int
    -- let (I64# v) = 42
    -- getClosureData v >>=
    --     assertClosuresEq exInt64Closure

    -- Primitive Word64
    -- FAILING: On 64-bit platforms, v is a regular Word
    -- let (W64# v) = 42
    -- getClosureData v >>=
    --     assertClosuresEq exWord64Closure

    -- Primitive Addr
    let v = unsafeCoerce# 42# :: Addr#
    getClosureData v >>=
        assertClosuresEq exAddrClosure

    -- Primitive Float
    let (F# v) = 42.0
    getClosureData v >>=
        assertClosuresEq exFloatClosure

    -- Primitive Double
    let (D# v) = 42.0
    getClosureData v >>=
        assertClosuresEq exDoubleClosure

    ------------------------------------------------------
    -- Catch-all type

    -- Other
    -- getClosureData (Just 1) >>=
    --    assertClosuresEq exOtherClosure

    putStrLn "Done. No errors."


-- | Attempt to compare two closures
compareClosures :: Closure -> Closure -> Bool
compareClosures expected actual =
    -- Determine which fields to compare based
    -- upon expected closure type
    let funcs = case expected of
                    ConstrClosure{}         -> [ sEq (tipe . info)
                                               , sEq (ptrs . info)
                                               , sEq (nptrs . info)
                                               , sEq dataArgs
                                               , sEq name          ]
                    FunClosure{}            -> [ sEq (tipe . info)
                                               , sEq (ptrs . info)
                                               , sEq (nptrs . info)
                                               , sEq dataArgs      ]
                    ThunkClosure{}          -> [ sEq (tipe . info)
                                               , sEq (ptrs . info)
                                               , sEq (nptrs . info)
                                               , sEq dataArgs      ]
                    SelectorClosure{}       -> [ sEq (tipe . info) ]
                    PAPClosure{}            -> [ sEq (tipe . info)
                                               , sEq arity
                                               , sEq n_args        ]
                    APClosure{}             -> [ sEq (tipe . info)
                                               , sEq arity
                                               , sEq n_args        ]
                    APStackClosure{}        -> [ sEq (tipe . info) ]
                    IndClosure{}            -> [ sEq (tipe . info) ]
                    BCOClosure{}            -> [ sEq (tipe . info)
                                               , sEq arity
                                               , sEq bitmap        ]
                    BlackholeClosure{}      -> [ sEq (tipe . info) ]
                    ArrWordsClosure{}       -> [ sEq (tipe . info)
                                               , sEq bytes
                                               , sEq arrWords      ]
                    MutArrClosure{}         -> [ sEq (tipe . info)
                                               , sEq mccPtrs
                                               , sEq mccSize       ]
                    MVarClosure{}           -> [ sEq (tipe . info) ]
                    MutVarClosure{}         -> [ sEq (tipe . info) ]
                    BlockingQueueClosure{}  -> [ sEq (tipe . info) ]
                    IntClosure{}            -> [ sEq ptipe
                                               , sEq intVal    ]
                    WordClosure{}           -> [ sEq ptipe
                                               , sEq wordVal   ]
                    Int64Closure{}          -> [ sEq ptipe
                                               , sEq int64Val  ]
                    Word64Closure{}         -> [ sEq ptipe
                                               , sEq word64Val ]
                    AddrClosure{}           -> [ sEq ptipe
                                               , sEq addrVal   ]
                    FloatClosure{}          -> [ sEq ptipe
                                               , sEq floatVal  ]
                    DoubleClosure{}         -> [ sEq ptipe
                                               , sEq doubleVal ]
                    _ -> error $ "Don't know how to compare expected closure: "
                                 ++ show expected
    in compareWith funcs expected actual
  where
    -- Take a list of closure comparisons and check all
    compareWith :: [Closure -> Closure -> Bool] -> Closure -> Closure -> Bool
    compareWith funcs c1 c2 = all (\f -> f c1 c2) funcs

    -- Create a comparison function from a selector
    sEq :: Eq a => (Closure -> a) -> Closure -> Closure -> Bool
    sEq select c1 c2 = select c1 == select c2

-- | Assert two closures are equal, checking depending on closure type
assertClosuresEq :: HasCallStack => Closure -> Closure -> IO ()
assertClosuresEq _ c@UnsupportedClosure{} =
    fail $ unlines [ "Unsupported closure returned: " ++ show c
                   , ""
                   , prettyCallStack callStack
                   ]
assertClosuresEq expected actual =
    unless (compareClosures expected actual) $ fail $ unlines
        [ "assertClosuresEq: Closures do not match"
        , "Expected: " ++ show expected
        , "Actual:   " ++ show actual
        , ""
        , prettyCallStack callStack
        ]
