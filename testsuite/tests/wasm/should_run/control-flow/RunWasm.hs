{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror #-}

module RunWasm
  ( evalWasm
  )
where

-- Using a `ControlTestMonad` to provide observations,
-- simulate the execution of WebAssembly control flow.

import GHC.Wasm.ControlFlow

import ControlTestMonad

evalWasm :: ControlTestMonad s e m => WasmControl s e pre post -> m ()

-- Evaluation uses a small-step semantics with a control stack.

type Stack s e = [Frame (UntypedControl s e) e]
data Frame s e = EndLoop s | EndBlock | EndIf | Run s | Pushed e

data UntypedControl s e =
  forall pre post . U (WasmControl s e pre post)

evalWasm s = run [Run (U s)]

withPushedValue :: Stack s e -> (e -> Stack s e -> answer) -> answer
withPushedValue (Pushed e : stack) k = k e stack
withPushedValue _ _ = error "looked for pushed value, but did not find one"


run  :: forall s e m . ControlTestMonad s e m => Stack s e -> m ()
run [] = return ()
run (EndLoop _ : stack) = run stack
run (EndBlock : stack) = run stack
run (EndIf : stack) = run stack
run (Pushed e : frame : stack) = run (frame : Pushed e : stack)
run (Pushed _ : []) = return ()
run (Run s : stack) = step s
  where step :: UntypedControl s e -> m ()
        step (U WasmFallthrough) = run stack
        step (U (WasmBlock _ s)) = run (Run (U s) : EndBlock : stack)
        step (U (WasmLoop _ s)) = run (Run (U s) : EndLoop (U s) : stack)
        step (U (WasmBr k)) = br k stack

        step (U (WasmPush _ e)) = run (Pushed e : stack)
        step (U (WasmIfTop _ t f)) = withPushedValue stack $ \ e stack -> do
                                  b <- evalPredicate @s @e e
                                  run (Run (U $ if b then t else f) : EndIf : stack)

        step (U (WasmBrTable e range targets default')) = do
          n <- fromInteger <$>
               evalEnum @s @e e (bti_lo range, bti_lo range + bti_count range)
          if n >= 0 && n < length targets then br (targets !! n) stack
          else br default' stack

        step (U (WasmTailCall e)) = withPushedValue (Pushed e : stack) $ \ _ _ -> return ()

        step (U (WasmActions s)) = takeAction @s @e s >> run stack
        step (U (WasmSeq s s')) = run (Run (U s) : Run (U s') : stack)
        br 0 (EndLoop us : stack) = run (Run us : EndLoop us : stack)
        br 0 (EndBlock : stack) = run stack
        br 0 (EndIf : stack) = run stack
        br k ((Run _) : stack) = br k stack
        br k ((Pushed _) : stack) = br k stack
        br k (_ : stack) = br (pred k) stack
        br _ [] = fail "br index too large"


instance Show (Frame s e) where
  show (EndLoop _) = "end loop"
  show EndBlock    = "end block"
  show EndIf = "end if"
  show (Pushed _) = "<pushed value>"
  show (Run _) = "run"
