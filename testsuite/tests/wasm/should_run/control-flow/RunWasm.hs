{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module RunWasm
  ( evalWasm
  )
where

-- Using a `ControlTestMonad` to provide observations,
-- simulate the execution of WebAssembly control flow.

import GHC.Wasm.ControlFlow

import ControlTestMonad

evalWasm :: ControlTestMonad s e m => WasmControl s e -> m ()

-- Evaluation uses a small-step semantics with a control stack.

type Stack s e = [Frame (WasmControl s e)]
data Frame s = EndLoop s | EndBlock | EndIf | Run s

evalWasm s = run [Run s]

run  :: forall s e m . ControlTestMonad s e m => Stack s e -> m ()
run [] = return ()
run (EndLoop s : stack) = run (Run s : EndLoop s : stack)
run (EndBlock : stack) = run stack
run (EndIf : stack) = run stack
run (Run s : stack) = step s
  where step :: WasmControl s e -> m ()
        step WasmNop = run stack
        step (WasmUnreachable) = fail "unreachable"
        step (WasmBlock s) = run (Run s : EndBlock : stack)
        step (WasmLoop s) = run (Run s : EndLoop s : stack)
        step (WasmBr k) = br k stack

        step (WasmIf e t f) = do
          b <- evalPredicate @s @e e
          run (Run (if b then t else f) : EndIf : stack)

        step (WasmBrTable e range targets default') = do
          n <- fromInteger <$>
               evalEnum @s @e e (bti_lo range, bti_lo range + bti_count range)
          if n >= 0 && n < length targets then br (targets !! n) stack
          else br default' stack

        step (WasmReturn) = return ()

        step (WasmSlc s) = takeAction @s @e s >> run stack
        step (WasmSeq s s') = run (Run s : Run s' : stack)

        br 0 (EndLoop s : stack) = run (EndLoop s : stack)
        br 0 (EndBlock : stack) = run stack
        br 0 (EndIf : stack) = run stack
        br k (Run _ : stack) = br k stack
        br k (_ : stack) = br (pred k) stack
        br _ [] = fail "br index too large"



instance Show (Frame s) where
  show (EndLoop _) = "end loop"
  show EndBlock    = "end block"
  show EndIf = "end if"
  show (Run _) = "run"
