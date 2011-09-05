-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Eval where

import Data.Array

import Geometry
import CSG
import Surface
import Data
import Parse (rayParse, rayParseF)

class Monad m => MonadEval m where
  doOp :: PrimOp -> GMLOp -> Stack -> m Stack
  tick :: m ()
  err  :: String -> m a

  tick = return ()

newtype Pure a = Pure a deriving Show

instance Monad Pure where
    Pure x >>= k = k x
    return       = Pure
    fail s       = error s

instance MonadEval Pure where
  doOp   = doPureOp
  err  s = error s

instance MonadEval IO where
  doOp prim op stk = do { -- putStrLn ("Calling " ++ show op
                          --           ++ " << " ++ show stk ++ " >>")
                          doAllOp  prim op stk
                        }
  err  s = error s

data State
	= State { env   :: Env
	        , stack :: Stack
	        , code  :: Code
	        } deriving Show

callback :: Env -> Code -> Stack -> Stack
callback env code stk
      = case eval (State { env = env, stack = stk, code = code}) of
             Pure stk -> stk

{-# SPECIALIZE eval ::  State -> Pure Stack #-}
{-# SPECIALIZE eval ::  State -> IO Stack #-}

eval :: MonadEval m => State -> m Stack
eval st =
  do { () <- return () -- $ unsafePerformIO (print st)   -- Functional debugger
     ; if moreCode st then
       do { tick             -- tick first, so as to catch loops on new eval.
            ; st' <- step st
            ; eval st'
            }
        else return (stack st)
     }

moreCode :: State -> Bool
moreCode (State {code = []}) = False
moreCode _                   = True

-- Step has a precondition that there *is* code to run
{-# SPECIALIZE step ::  State -> Pure State #-}
{-# SPECIALIZE step ::  State -> IO State #-}
step :: MonadEval m => State -> m State

-- Rule 1: Pushing BaseValues
step st@(State{ stack = stack, code = (TBool b):cs })
    = return (st { stack = (VBool b):stack,    code = cs })
step st@(State{ stack = stack, code = (TInt i):cs })
    = return (st { stack = (VInt i):stack,     code = cs })
step st@(State{ stack = stack, code = (TReal r):cs })
    = return (st { stack = (VReal r):stack,    code = cs })
step st@(State{ stack = stack, code = (TString s):cs })
    = return (st { stack = (VString s):stack,  code = cs })

-- Rule 2: Name binding
step st@(State{ env = env, stack = (v:stack), code = (TBind id):cs }) =
  return (State { env = extendEnv env id v, stack = stack,  code = cs })
step st@(State{ env = env, stack = [], code = (TBind id):cs }) =
  err "Attempt to bind the top of an empty stack"

-- Rule 3: Name lookup
step st@(State{ env = env, stack = stack, code = (TId id):cs }) =
  case (lookupEnv env id) of
  Just v -> return (st { stack = v:stack,  code = cs })
  Nothing -> err ("Cannot find value for identifier: " ++ id)

-- Rule 4: Closure creation
step st@(State{ env = env, stack = stack, code = (TBody body):cs }) =
  return (st { stack = (VClosure env body):stack, code = cs })

-- Rule 5: Application
step st@(State{ env = env, stack = (VClosure env' code'):stack, code = TApply:cs }) =
  do { stk <- eval (State {env = env', stack = stack, code = code'})
     ; return (st { stack = stk, code = cs })
     }
step st@(State{ env = env, stack = [], code = TApply:cs }) =
  err "Application with an empty stack"
step st@(State{ env = env, stack = _:_, code = TApply:cs }) =
  err "Application of a non-closure"

-- Rule 6: Arrays
step st@(State{ env = env, stack = stack, code = TArray code':cs }) =
  do { stk <- eval (State {env = env, stack = [], code = code'})
     ; let last = length stk-1
     ; let arr = array (0,last) (zip [last,last-1..] stk)
     ; return (st { stack = (VArray arr):stack, code = cs })
     }

-- Rule 7 & 8: If statement
step st@(State{ env = env, stack = (VClosure e2 c2):(VClosure e1 c1):(VBool True):stack, code = TIf:cs }) =
  do { stk <- eval (State {env = e1, stack = stack, code = c1})
     ; return (st { stack = stk, code = cs })
     }
step st@(State{ env = env, stack = (VClosure e2 c2):(VClosure e1 c1):(VBool False):stack, code = TIf:cs }) =
  do { stk <- eval (State {env = e2, stack = stack, code = c2})
     ; return (st { stack = stk, code = cs })
     }
step st@(State{ env = env, stack = _, code = TIf:cs }) =
  err "Incorrect use of if (bad and/or inappropriate values on the stack)"

-- Rule 9: Operators
step st@(State{ env = env, stack = stack, code = (TOp op):cs }) =
  do { stk <- doOp (opFnTable ! op) op stack
     ; return (st { stack = stk, code = cs })
     }

-- Rule Opps
step _ = err "Tripped on sidewalk while stepping."


--------------------------------------------------------------------------
-- Operator code

opFnTable :: Array GMLOp PrimOp
opFnTable = array (minBound,maxBound)
	          [ (op,prim) | (_,TOp op,prim) <- opcodes ]




doPureOp :: (MonadEval m) => PrimOp -> GMLOp -> Stack -> m Stack
doPureOp _ Op_render _ =
    err ("\nAttempting to call render from inside a purely functional callback.")
doPureOp primOp op stk = doPrimOp primOp op stk -- call the purely functional operators

{-# SPECIALIZE doPrimOp :: PrimOp -> GMLOp -> Stack -> Pure Stack #-}
{-# SPECIALIZE doPrimOp :: PrimOp -> GMLOp -> Stack -> IO Stack #-}
{-# SPECIALIZE doPrimOp :: PrimOp -> GMLOp -> Stack -> Abs Stack #-}

doPrimOp ::  (MonadEval m) => PrimOp -> GMLOp -> Stack -> m Stack

-- 1 argument.

doPrimOp (Int_Int fn) _ (VInt i1:stk)
  = return ((VInt (fn i1)) : stk)
doPrimOp (Real_Real fn) _ (VReal r1:stk)
  = return ((VReal (fn r1)) : stk)
doPrimOp (Point_Real fn) _ (VPoint x y z:stk)
  = return ((VReal (fn x y z)) : stk)

-- This is where the callbacks happen from...
doPrimOp (Surface_Obj fn) _ (VClosure env code:stk)
  = case absapply env code [VAbsObj AbsFACE,VAbsObj AbsU,VAbsObj AbsV] of
      Just [VReal r3,VReal r2,VReal r1,VPoint c1 c2 c3] ->
           let
	       res = prop (color c1 c2 c3) r1 r2 r3
           in
               return ((VObject (fn (SConst res))) : stk)
      _ -> return ((VObject (fn (SFun call))) : stk)
  where
        -- The most general case
        call i r1 r2 =
          case callback env code [VReal r2,VReal r1,VInt i] of
             [VReal r3,VReal r2,VReal r1,VPoint c1 c2 c3]
		 -> prop (color c1 c2 c3) r1 r2 r3
             stk -> error ("callback failed: incorrectly typed return arguments"
                         ++ show stk)

doPrimOp (Real_Int fn) _ (VReal r1:stk)
  = return ((VInt (fn r1)) : stk)
doPrimOp (Int_Real fn) _ (VInt r1:stk)
  = return ((VReal (fn r1)) : stk)
doPrimOp (Arr_Int fn) _ (VArray arr:stk)
  = return ((VInt (fn arr)) : stk)

-- 2 arguments.

doPrimOp (Int_Int_Int fn) _ (VInt i2:VInt i1:stk)
  = return ((VInt (fn i1 i2)) : stk)
doPrimOp (Int_Int_Bool fn) _ (VInt i2:VInt i1:stk)
  = return ((VBool (fn i1 i2)) : stk)
doPrimOp (Real_Real_Real fn) _ (VReal r2:VReal r1:stk)
  = return ((VReal (fn r1 r2)) : stk)
doPrimOp (Real_Real_Bool fn) _ (VReal r2:VReal r1:stk)
  = return ((VBool (fn r1 r2)) : stk)
doPrimOp (Arr_Int_Value fn) _ (VInt i:VArray arr:stk)
  = return ((fn arr i) : stk)


    -- Many arguments, typically image mangling

doPrimOp (Obj_Obj_Obj fn) _ (VObject o2:VObject o1:stk)
  = return ((VObject (fn o1 o2)) : stk)
doPrimOp (Point_Color_Light fn) _ (VPoint r g b:VPoint x y z : stk)
  = return (VLight (fn (x,y,z) (color r g b)) : stk)
doPrimOp (Point_Point_Color_Real_Real_Light fn) _
         (VReal r2:VReal r1:VPoint r g b:VPoint x2 y2 z2:VPoint x1 y1 z1 : stk)
  = return (VLight (fn (x1,y1,z1) (x2,y2,z2) (color r g b) r1 r2) : stk)
doPrimOp (Real_Real_Real_Point fn) _ (VReal r3:VReal r2:VReal r1:stk)
  = return ((fn r1 r2 r3) : stk)
doPrimOp (Obj_Real_Obj fn) _ (VReal r:VObject o:stk)
  = return (VObject (fn o r) : stk)
doPrimOp (Obj_Real_Real_Real_Obj fn) _ (VReal r3:VReal r2:VReal r1:VObject o:stk)
  = return (VObject (fn o r1 r2 r3) : stk)

-- This one is our testing harness
doPrimOp (Value_String_Value fn) _ (VString s:o:stk)
  = res `seq` return (res : stk)
  where
     res = fn o s

doPrimOp primOp op args
  = err ("\n\ntype error when attempting to execute builtin primitive \"" ++
          show op ++ "\"\n\n| " ++
          show op ++ " takes " ++ show (length types) ++ " argument" ++ s
	           ++ " with" ++ the ++ " type" ++ s ++ "\n|\n|" ++
          "      " ++ unwords [ show ty | ty <- types ]  ++ "\n|\n|" ++
          " currently, the relevent argument" ++ s ++ " on the stack " ++
	          are ++ "\n|\n| " ++
          unwords [ "(" ++ show arg ++ ")"
                  | arg <-  reverse (take (length types) args) ]  ++ "\n|\n| "
          ++ "    (top of stack is on the right hand side)\n\n")
  where
      len   = length types
      s =  (if len /= 1 then "s" else "")
      are =  (if len /= 1 then "are" else "is")
      the =  (if len /= 1 then "" else " the")
      types = getPrimOpType primOp


-- Render is somewhat funny, becauase it can only get called at top level.
-- All other operations are purely functional.

doAllOp :: PrimOp -> GMLOp -> Stack -> IO Stack
doAllOp (Render render) Op_render
			   (VString str:VInt ht:VInt wid:VReal fov
                           :VInt dep:VObject obj:VArray arr
                           :VPoint r g b : stk)
  = do { render (color r g b) lights obj dep (fov * (pi / 180.0)) wid ht str
       ; return stk
       }
  where
      lights = [ light | (VLight light) <- elems arr ]

doAllOp primOp op stk = doPrimOp primOp op stk -- call the purely functional operators

------------------------------------------------------------------------------
{-
 - Abstract evaluation.
 -
 - The idea is you check for constant code that
 - (1) does not look at its arguments
 - (2) gives a fixed result
 -
 - We run for 100 steps.
 -
 -}

absapply :: Env -> Code -> Stack -> Maybe Stack
absapply env code stk =
     case runAbs (eval (State env stk code)) 100 of
       AbsState stk _ -> Just stk
       AbsFail m      -> Nothing

newtype Abs a   = Abs { runAbs :: Int -> AbsState a }
data AbsState a = AbsState a !Int
                | AbsFail String

instance Monad Abs where
    (Abs fn) >>= k = Abs (\ s -> case fn s of
			           AbsState r s' -> runAbs (k r) s'
                                   AbsFail m     -> AbsFail m)
    return x     = Abs (\ n -> AbsState x n)
    fail s       = Abs (\ n -> AbsFail s)

instance MonadEval Abs where
  doOp = doAbsOp
  err  = fail
  tick = Abs (\ n -> if n <= 0
                     then AbsFail "run out of time"
                     else AbsState () (n-1))

doAbsOp :: PrimOp -> GMLOp -> Stack -> Abs Stack
doAbsOp _ Op_point (VReal r3:VReal r2:VReal r1:stk)
               = return ((VPoint r1 r2 r3) : stk)
 -- here, you could have an (AbsPoint :: AbsObj) which you put on the
 -- stack, with any object in the three fields.
doAbsOp _ op _ = err ("operator not understood (" ++ show op ++ ")")

------------------------------------------------------------------------------
-- Driver

mainEval :: Code -> IO ()
mainEval prog = do { stk <- eval (State emptyEnv [] prog)
                   ; return ()
                   }
{-
  * Oops, one of the example actually has something
  * on the stack at the end.
  * Oh well...
		   ; if null stk
                     then return ()
		     else do { putStrLn done
                             ; print stk
                             }
-}

done = "Items still on stack at (successfull) termination of program"

------------------------------------------------------------------------------
-- testing

test :: String -> Pure Stack
test is = eval (State emptyEnv [] (rayParse is))

testF :: String -> IO Stack
testF is = do prog <- rayParseF is
              eval (State emptyEnv [] prog)

testA :: String -> Either String (Stack,Int)
testA is = case runAbs (eval (State emptyEnv
                                    [VAbsObj AbsFACE,VAbsObj AbsU,VAbsObj AbsV]
                                    (rayParse is))) 100 of
             AbsState a n -> Right (a,n)
             AbsFail m -> Left m

abstest1 = "1.0 0.0 0.0 point /red { /v /u /face red 1.0 0.0 1.0 } apply"

-- should be [3:: Int]
et1 = test "1 /x { x } /f 2 /x f apply x addi"





