module Main where

import GHC.Driver.Session
import GHC
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

import GHC.ByteCode.Asm ( assembleBCO )
import GHC.ByteCode.Instr
import Control.Monad
import GHC.Builtin.Names

-- Testing the performance of the bytecode assembler

-- A nonsensical byte-code program
instrs = [ STKCHECK 1234
         , PUSH_L 1
         , PUSH_LL 1 2
         , PUSH_LLL 2 3 4
         , PUSH_LLL 2 3 4
         , PUSH_LLL 2 3 4
         , PUSH_LLL 2 3 4
         , PUSH_LLL 2 3 4
         , PUSH8 0
         , PUSH16 15
         , PUSH32 29
         , PUSH_PAD8
         , PUSH_APPLY_N
         , PUSH_APPLY_V
         , PUSH_APPLY_F
         , PUSH_APPLY_D
         , PUSH_APPLY_L
         , PUSH_APPLY_P
         , PUSH_APPLY_PP
         , PUSH_APPLY_PPP
         , PUSH_APPLY_PPPP
         , PUSH_APPLY_PPPPP
         , PUSH_APPLY_PPPPPP
         , TESTLT_I 100 (LocalLabel 0)
         , TESTEQ_I 100 (LocalLabel 0)
         ]
         ++ [ LABEL (LocalLabel n) | n <- [0..50] ]
         ++ [ TESTEQ_I64 n (LocalLabel 49) | n <- [1243 .. 1253 + 50 ]]
         ++ [ ENTER ]
         ++ [ SLIDE x n | x <- [0..5], n <- [0..10] ]
         ++ [ PUSH_G appAName | _ <- [0..100] ]
         ++ [ PUSH_BCO fake_proto2 ]

fake_proto = ProtoBCO appAName instrs [] 0 0 (Left [])

instrs2 = [ STKCHECK 77, UNPACK 4, SLIDE 0 4, ENTER ]

fake_proto2 = ProtoBCO appAName instrs2 [] 0 0 (Left [])

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let platform = targetPlatform dflags

    -- ~1s on my machine
    liftIO $ replicateM_ 100000 (assembleBCO platform fake_proto)
