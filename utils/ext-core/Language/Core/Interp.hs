{-# OPTIONS -Wall -fno-warn-name-shadowing -XPatternGuards -fglasgow-exts #-}
{- 
Interprets the subset of well-typed Core programs for which
	(a) All constructor and primop applications are saturated
	(b) All non-trivial expressions of unlifted kind ('#') are
             scrutinized in a Case expression.

This is by no means a "minimal" interpreter, in the sense that considerably
simpler machinary could be used to run programs and get the right answers.
However, it attempts to mirror the intended use of various Core constructs,
particularly with respect to heap usage.  So considerations such as unboxed
tuples, sharing, trimming, black-holing, etc. are all covered.
The only major omission is garbage collection.

Just a sampling of primitive types and operators are included.
-}

module Language.Core.Interp ( evalProgram ) where

import Control.Monad.Error
import Control.Monad.State
import Data.Char
import Data.List

import GHC.Exts hiding (Ptr)
import System.IO

import Language.Core.Core
import Language.Core.Env
import Language.Core.Printer()

data HeapValue = 
    Hconstr Dcon [Value]       -- constructed value (note: no qualifier needed!)
  | Hclos Venv Var Exp         -- function closure
  | Hthunk Venv Exp            -- unevaluated thunk
  deriving (Show)

type Ptr = Int

data Value = 
    Vheap Ptr 		       -- heap pointer (boxed)
  | Vimm PrimValue     	       -- immediate primitive value (unboxed)
  | Vutuple [Value]            -- unboxed tuples
  deriving (Show)

instance Error Value where
  -- TODO: ??
  strMsg s = error s

type Venv = Env Var Value       -- values of vars

data PrimValue =                -- values of the (unboxed) primitive types
    PCharzh Integer		-- actually 31-bit unsigned
  | PIntzh Integer		-- actually WORD_SIZE_IN_BITS-bit signed
  | PWordzh Integer		-- actually WORD_SIZE_IN_BITS-bit unsigned
  | PAddrzh Integer		-- actually native pointer size
  | PFloatzh Rational		-- actually 32-bit 
  | PDoublezh Rational		-- actually 64-bit
--  etc., etc.
  | PString String
  deriving (Eq,Show)

type Menv = Env AnMname Venv	-- modules

initialGlobalEnv :: Menv
initialGlobalEnv =
    efromlist
	[(primMname,efromlist [("realWorldzh",Vimm (PIntzh 0))])]

{- Heap management. -}
{- Nothing is said about garbage collection. -}

data Heap = Heap Ptr (Env Ptr HeapValue) 
    -- last cell allocated; environment of allocated cells
  deriving Show

hallocate :: Heap -> HeapValue -> (Heap,Ptr)
hallocate (Heap last contents) v = 
   let next = last+1
   in (Heap next (eextend contents (next,v)),next)

hupdate :: Heap -> Ptr -> HeapValue -> Heap
hupdate (Heap last contents) p v =
   Heap last (eextend contents (p,v))

hlookup:: Heap -> Ptr -> HeapValue
hlookup (Heap _ contents) p =
   case elookup contents p of
     Just v -> v
     Nothing -> error "Missing heap entry (black hole?)"

hremove :: Heap -> Ptr -> Heap
hremove (Heap last contents) p = 
   Heap last (eremove contents p)

hempty :: Heap
hempty = Heap 0 eempty

{- The evaluation monad manages the heap and the possiblity 
   of exceptions. -}

type Exn = Value

type Eval a = ErrorT Exn (StateT Heap IO) a

hallocateE :: HeapValue -> Eval Ptr
hallocateE v = do
  h <- get
  let (h', p) = hallocate h v
  put h'
  return p

hupdateE :: Ptr -> HeapValue -> Eval ()
hupdateE p v = modify (\ h -> hupdate h p v)

hlookupE :: Ptr -> Eval HeapValue
hlookupE p =  get >>= (\h -> return (hlookup h p))

hremoveE :: Ptr -> Eval ()
hremoveE p = modify (\h -> hremove h p)

raiseE :: Exn -> Eval a
raiseE = throwError

catchE :: Show a => Eval a -> (Exn -> Eval a) -> Eval a
catchE = catchError

runE :: Eval a -> IO a
runE m = do
  resultOrError <- evalStateT (runErrorT m) hempty
  case resultOrError of
    Right v -> return v
    Left exn -> error
      ("evaluation failed with uncaught exception: " ++ show exn)

{- Main entry point -}
-- TODO: This is in the IO monad because primitive I/O ops
-- actually perform the IO. It might be better to model it
-- instead (by having the interpreter return a ([Char] -> (Value, [Char])))
evalProgram :: [Module] -> IO Value
evalProgram modules = runE $ do
     -- We do two passes: one to slurp in all the definitions *except*
     -- for :Main.main, and then one to look for the Main module
     -- and extract out just the :Main.main defn.
     -- It's kind of annoying.
     globalEnv' <- foldM evalModule initialGlobalEnv modules
     globalEnv  <- evalModule globalEnv' (rootModule modules)
     Vutuple [_,v] <- evalExp globalEnv eempty (App (Var wrapperMainVar)
                       stateToken)
     return v

rootModule :: [Module] -> Module
-- This looks for the Main module, and constructs
-- a fake module containing only the defn of
-- :Main.main.
rootModule ms =
  case find (\ (Module mn _ _) -> mn == mainMname) ms of
    Just (Module _ _ [Rec bs]) ->
        Module wrapperMainMname []
          [Rec (filter isWrapperMainVdef bs)]
    _ -> error "eval: missing main module"
  where isWrapperMainVdef (Vdef (v,_,_)) | v == wrapperMainVar = True
        isWrapperMainVdef _ = False

{- Environments:

Evaluating a module just fills an environment with suspensions for all
the external top-level values; it doesn't actually do any evaluation
or look anything up.

By the time we actually evaluate an expression, all external values from
all modules will be in globalEnv.  So evaluation just maintains an environment
of non-external values (top-level or local).  In particular, only non-external
values end up in closures (all other values are accessible from globalEnv.)

Throughout:

- globalEnv contains external values (all top-level) from all modules seen so far.

In evalModule:

- e_venv    contains external values (all top-level) seen so far in current module
- l_venv    contains non-external values (top-level or local)  
                 seen so far in current module.
In evalExp:

- env	    contains non-external values (top-level or local) seen so far
		in current expression.
-}


evalModule :: Menv -> Module -> Eval Menv
evalModule globalEnv (Module mn _ vdefgs) =
  do (e_venv,_) <- foldM evalVdef (eempty,eempty) vdefgs
     return (eextend globalEnv (mn,e_venv))
  where
    evalVdef :: (Venv,Venv) -> Vdefg -> Eval (Venv,Venv)
    evalVdef (e_env,l_env) (Nonrec(Vdef((m,x),_,e))) =
     do p <- hallocateE (suspendExp l_env e)
	let heaps =
               case m of
                 Nothing -> (e_env,eextend l_env (x,Vheap p))
	         _       -> (eextend e_env (x,Vheap p),l_env)
	return heaps
    evalVdef (e_env,l_env) (Rec vdefs) =
      do l_vs0 <- mapM preallocate l_xs
	 let l_env' = foldl eextend l_env (zip l_xs (map Vheap l_vs0))
	 let l_hs = map (suspendExp l_env') l_es
	 mapM_ reallocate (zip l_vs0 l_hs)
	 let e_hs = map (suspendExp l_env') e_es
	 e_vs <- (liftM (map Vheap)) $ mapM allocate e_hs
         let e_env' = foldl eextend e_env (zip e_xs e_vs)
	 return (e_env',l_env')            
      where 
	 (l_xs,l_es) = unzip [(x,e) | Vdef((Nothing,x),_,e) <- vdefs]
	 (e_xs,e_es) = unzip [(x,e) | Vdef ((Just _,x),_,e) <-
                         -- Do not dump the defn for :Main.main into
                         -- the environment for Main!
                                       filter inHomeModule vdefs]
         inHomeModule (Vdef ((Just m,_),_,_)) | m == mn = True
         inHomeModule _ = False
	 preallocate _ =
	   do p <- hallocateE undefined
	      return p
	 reallocate (p0,h) =
	   hupdateE p0 h
	 allocate h =
	   do p <- hallocateE h
	      return p

    suspendExp:: Venv -> Exp -> HeapValue
    suspendExp env (Lam (Vb(x,_)) e) = Hclos env' x e
       where env' = thin env (delete x (freevarsExp e))
    suspendExp env e = Hthunk env' e
       where env' = thin env (freevarsExp e)

evalExp :: Menv -> Venv -> Exp -> Eval Value
evalExp globalEnv env = eval
  where eval (Var qv) = 
          let v = qlookup globalEnv env qv
          in case v of 
               Vheap p -> do
	         z <- hlookupE p                    -- can fail due to black-holing
	         case z of
	           Hthunk env' e -> do
		     hremoveE p                     -- black-hole
                     w <- evalExp globalEnv env' e  -- result is guaranteed to be boxed!
                     case w of
                       Vheap p' -> do
   	                 h <- hlookupE p'
		         hupdateE p h
	                 return w
                       _ -> error ("eval: w was not boxed: " ++ show w)
	           _ -> return v -- return pointer to Hclos or Hconstr
               _ -> return v     -- return Vimm or Vutuple
        eval (Lit l) = return (Vimm (evalLit l))
        eval (Dcon (_,c)) = do
           p <- hallocateE (Hconstr c [])
           return (Vheap p)
        eval (App e1 e2) =
          evalApp env e1 [e2]
            where
              evalApp :: Venv -> Exp -> [Exp] -> Eval Value
              evalApp env (App e1 e2) es = evalApp env e1 (e2:es)
              evalApp env (Dcon (qdc@(_,c))) es = 
                  do vs <- suspendExps globalEnv env es
	             if isUtupleDc qdc
                       then
	                 return (Vutuple vs)
	               else
	                 {- allocate a thunk -}
     	                 do p <- hallocateE (Hconstr c vs)
  	                    return (Vheap p)
              evalApp env (Var(v@(_,p))) es | isPrimVar v =
                 do vs <- evalExps globalEnv env es
                    case (p,vs) of
   	              ("raisezh",[exn]) -> raiseE exn
	              ("catchzh",[body,handler,rws]) ->
	                        catchE (apply body [rws])
	                        (\exn -> apply handler [exn,rws])
  	              _ -> evalPrimop p vs
              evalApp env (External s _) es =
                  do vs <- evalExps globalEnv env es
	             evalExternal s vs
              evalApp env (Appt e _) es     = evalApp env e es
              evalApp env (Lam (Tb _) e) es = evalApp env e es
              evalApp env (Cast e _) es     = evalApp env e es
              evalApp env (Note _ e) es     = evalApp env e es
              evalApp env e es = 
          {- e must now evaluate to a closure -}
                  do vs <- suspendExps globalEnv env es
	             vop <- evalExp globalEnv env e
                     apply vop vs

              apply :: Value -> [Value] -> Eval Value
              apply vop [] = return vop
              apply (Vheap p) (v:vs) =
                  do Hclos env' x b <- hlookupE p
                     v' <- evalExp globalEnv (eextend env' (x,v)) b
                     apply v' vs
              apply _ _ = error ("apply: operator is not a closure")

        eval (Appt e _) = evalExp globalEnv env e
        eval (Lam (Vb(x,_)) e) = do
           p <- hallocateE (Hclos env' x e)
           return (Vheap p)
               where env' = thin env (delete x (freevarsExp e))
        eval (Lam _ e) = evalExp globalEnv env e
        eval (Let vdef e) =
          do env' <- evalVdef globalEnv env vdef
             evalExp globalEnv env' e
            where
              evalVdef :: Menv -> Venv -> Vdefg -> Eval Venv
              evalVdef globalEnv env (Nonrec(Vdef((_,x),_,e))) =
                  do v <- suspendExp globalEnv env e
	             return (eextend env (x,v))
              evalVdef globalEnv env (Rec vdefs) =
                  do vs0 <- mapM preallocate xs
	             let env' = foldl eextend env (zip xs (map Vheap vs0))
	             vs <- suspendExps globalEnv env' es
	             mapM_ reallocate (zip vs0 vs)
	             return env'
                  where 
	            (xs,es) = unzip [(x,e) | Vdef((_,x),_,e) <- vdefs]
	            preallocate _ = 
                        do p <- hallocateE (Hconstr "UGH" [])
	                   return p
	            reallocate (p0,Vheap p) =
	                do h <- hlookupE p
	                   hupdateE p0 h
                    reallocate (_,_) = error "reallocate: expected a heap value"
        eval (Case e (x,_) _ alts) =
            do z <- evalExp globalEnv env e
               let env' = eextend env (x,z)
               case z of
                 Vheap p -> do
	           h <- hlookupE p   -- can fail due to black-holing
	           case h of
	             Hconstr dcon vs -> evalDcAlt env' dcon vs (reverse alts)
  	             _ ->               evalDefaultAlt env' alts
                 Vutuple vs ->
	             evalUtupleAlt env' vs (reverse alts)
                 Vimm pv ->
	             evalLitAlt env' pv (reverse alts)
            where
              evalDcAlt :: Venv -> Dcon -> [Value] -> [Alt] -> Eval Value
              evalDcAlt env dcon vs = f
                where
	          f ((Acon (_,dcon') _ xs e):as) =
	             if dcon == dcon' then
	               evalExp globalEnv
                         (foldl eextend env (zip (map fst xs) vs)) e
	             else f as
	          f [Adefault e] =
	            evalExp globalEnv env e
	          f _ = error $ "impossible Case-evalDcAlt"

              evalUtupleAlt :: Venv -> [Value] -> [Alt] -> Eval Value
              evalUtupleAlt env vs [Acon _ _ xs e] =
                  evalExp globalEnv (foldl eextend env (zip (map fst xs) vs)) e
              evalUtupleAlt _ _ _ = error ("impossible Case: evalUtupleAlt")

              evalLitAlt :: Venv -> PrimValue -> [Alt] -> Eval Value
              evalLitAlt env pv alts =
                  f alts
                      where 
	                f ((Alit lit e):as) =
	                    let pv' = evalLit lit
	                    in if pv == pv' then
	                           evalExp globalEnv env e
                               else f as
                        f [Adefault e] =
	                    evalExp globalEnv env e
	                f _ = error "impossible Case-evalLitAlt"
    
              evalDefaultAlt :: Venv -> [Alt] -> Eval Value
              evalDefaultAlt env [Adefault e] = evalExp globalEnv env e
              evalDefaultAlt _ _ = error "evalDefaultAlt: impossible case"

        eval (Cast e _) = evalExp globalEnv env e
        eval (Note _ e) = evalExp globalEnv env e
        eval (External s _) = evalExternal s []

evalExps :: Menv -> Venv -> [Exp] -> Eval [Value]
evalExps globalEnv env = mapM (evalExp globalEnv env)

suspendExp:: Menv -> Venv -> Exp -> Eval Value
suspendExp globalEnv env (Var qv) = return (qlookup globalEnv env qv)
suspendExp _ _ (Lit l) = return (Vimm (evalLit l))
suspendExp _ env (Lam (Vb(x,_)) e) =
   do p <- hallocateE (Hclos env' x e)
      return (Vheap p)
   where env' = thin env (delete x (freevarsExp e))
suspendExp globalEnv env (Lam _ e) = suspendExp globalEnv env e
suspendExp globalEnv env (Appt e _) = suspendExp globalEnv env e
suspendExp globalEnv env (Cast e _) = suspendExp globalEnv env e
suspendExp globalEnv env (Note _ e) = suspendExp globalEnv env e
suspendExp _ _ (External s _) = evalExternal s []
suspendExp _ env e =
   do p <- hallocateE (Hthunk env' e)
      return (Vheap p)
   where env' = thin env (freevarsExp e)

suspendExps :: Menv -> Venv -> [Exp] -> Eval [Value]
suspendExps globalEnv env = mapM (suspendExp globalEnv env)

mlookup :: Menv -> Venv -> Mname -> Venv
mlookup _          env       Nothing  = env
mlookup globalEnv  _         (Just m) = 
    case elookup globalEnv m of
      Just env' -> env'
      Nothing -> error ("Interp: undefined module name: " ++ show m)

qlookup :: Menv -> Venv -> (Mname,Var) -> Value
qlookup globalEnv env (m,k) =
  case elookup (mlookup globalEnv env m) k of
    Just v -> v
    Nothing -> error ("undefined identifier: " ++ show m ++ "." ++ show k)

evalPrimop :: Var -> [Value] -> Eval Value
evalPrimop "zpzh"        = primIntBinop    (+)
evalPrimop "zpzhzh"      = primDoubleBinop (+)
evalPrimop "zmzh"        = primIntBinop    (-)
evalPrimop "zmzhzh"      = primDoubleBinop (-)
evalPrimop "ztzh"        = primIntBinop    (*)
evalPrimop "ztzhzh"      = primDoubleBinop (*)
evalPrimop "zgzh"        = primIntCmpOp    (>)
evalPrimop "zlzh"        = primIntCmpOp    (<)
evalPrimop "zlzhzh"      = primDoubleCmpOp (<)
evalPrimop "zezezh"      = primIntCmpOp    (==)
evalPrimop "zlzezh"      = primIntCmpOp    (<=)
evalPrimop "zlzezhzh"    = primDoubleCmpOp (<=)
evalPrimop "zgzezh"      = primIntCmpOp    (>=)
evalPrimop "zszezh"      = primIntCmpOp    (/=)
evalPrimop "zszhzh"      = primDoubleCmpOp (/=)
evalPrimop "negateIntzh" = primIntUnop     (\ i -> -i)
evalPrimop "quotIntzh"   = primIntBinop    quot
evalPrimop "remIntzh"    = primIntBinop    rem
evalPrimop "subIntCzh"   = primSubIntC
evalPrimop "addIntCzh"   = primAddIntC
evalPrimop "mulIntMayOflozh" = primIntBinop
  (\ i j ->
     case (fromIntegral i, fromIntegral j) of
       (I# x, I# y) -> 
         case x `mulIntMayOflo#` y of
           k -> fromIntegral (I# k))
evalPrimop "narrow32Intzh" = primIntUnop
  (\ i ->
     case fromIntegral i of
       (I# j) -> case narrow32Int# j of
                   k -> fromIntegral (I# k))
evalPrimop "int2Doublezh" = primInt2Double 
-- single-threaded, so, it's a no-op
--evalPrimop "noDuplicatezh" [state] = return state
evalPrimop "indexCharOffAddrzh" = primIndexChar
evalPrimop "eqCharzh"           = primCharCmpOp (==)
evalPrimop "leCharzh"           = primCharCmpOp (<) 
evalPrimop "ordzh"              = primOrd 
evalPrimop "chrzh"              = primChr
evalPrimop "isSpacezh"          = primCharUnop isSpace
evalPrimop "isAlphazh"          = primCharUnop isAlpha
evalPrimop "hPutCharzh"         = primHPutChar
-- etc.
evalPrimop p = error ("undefined primop: " ++ p)

primIntUnop :: (Integer -> Integer) -> [Value] -> Eval Value
primIntUnop op [Vimm (PIntzh i)] = return (Vimm (PIntzh (op i)))
primIntUnop _ _ = error "primIntUnop: wrong number of arguments"

primIntBinop :: (Integer -> Integer -> Integer) -> [Value] -> Eval Value
primIntBinop op [Vimm (PIntzh i), Vimm (PIntzh j)] = 
  return (Vimm (PIntzh (i `op` j)))
primIntBinop _ _ = error "primIntBinop: wrong number of arguments"

primDoubleBinop :: (Rational -> Rational -> Rational) -> [Value] -> Eval Value
primDoubleBinop op [Vimm (PDoublezh i), Vimm (PDoublezh j)] = 
  return (Vimm (PDoublezh (i `op` j)))
primDoubleBinop _ _ = error "primDoubleBinop: wrong number of arguments"

primIntCmpOp :: (Integer -> Integer -> Bool) -> [Value] -> Eval Value
primIntCmpOp op [Vimm (PIntzh i), Vimm (PIntzh j)] = mkBool (i `op` j)
primIntCmpOp _ _ = error "primIntCmpOp: wrong number of arguments"

primDoubleCmpOp :: (Rational -> Rational -> Bool) -> [Value] -> Eval Value
primDoubleCmpOp op [Vimm (PDoublezh i), Vimm (PDoublezh j)] = mkBool (i `op` j)
primDoubleCmpOp _ _ = error "primDoubleCmpOp: wrong number of arguments"

primCharCmpOp :: (Integer -> Integer -> Bool) -> [Value] -> Eval Value
primCharCmpOp op [Vimm (PCharzh c), Vimm (PCharzh d)] = mkBool (c `op` d)
primCharCmpOp _ _ = error "primCharCmpOp: wrong number of arguments"

primSubIntC :: [Value] -> Eval Value
primSubIntC vs = carryOp subIntC# vs

primAddIntC :: [Value] -> Eval Value
primAddIntC vs = carryOp addIntC# vs

carryOp :: (Int# -> Int# -> (# Int#, Int# #)) -> [Value] -> Eval Value
carryOp op [Vimm (PIntzh i1), Vimm (PIntzh i2)] =
  case (fromIntegral i1, fromIntegral i2) of
    (I# int1, I# int2) -> 
       case (int1 `op` int2) of
        (# res1, res2 #) -> 
           return $ Vutuple [Vimm (PIntzh (fromIntegral (I# res1))),
                             Vimm (PIntzh (fromIntegral (I# res2)))]
carryOp _ _ = error "carryOp: wrong number of arguments"

primInt2Double :: [Value] -> Eval Value
primInt2Double [Vimm (PIntzh i)] =
  return (Vimm (PDoublezh (fromIntegral i)))
primInt2Double _ = error "primInt2Double: wrong number of arguments"

primOrd :: [Value] -> Eval Value
primOrd [Vimm (PCharzh c)] = return $ Vimm (PIntzh c)
primOrd _ = error "primOrd: wrong number of arguments"

primChr :: [Value] -> Eval Value
primChr [Vimm (PIntzh c)] = return $ Vimm (PCharzh c)
primChr _ = error "primChr: wrong number of arguments"

primCharUnop :: (Char -> Bool) -> [Value] -> Eval Value
primCharUnop op [Vimm (PCharzh c)] = mkBool (op (chr (fromIntegral c)))
primCharUnop _ _ = error "primCharUnop: wrong number of arguments"

primIndexChar :: [Value] -> Eval Value
primIndexChar [(Vimm (PString s)), (Vimm (PIntzh i))] = 
  -- String is supposed to be null-terminated, so if i == length(s),
  -- we return null. (If i > length(s), emit nasal demons.)
  return $ let len = fromIntegral $ length s in
             if i < len 
               then Vimm (PCharzh (fromIntegral (ord (s !! fromIntegral i))))
               else if i == len
                      then Vimm (PCharzh 0)
                      else error "indexCharOffAddr#: index too large"
primIndexChar _ = error "primIndexChar: wrong number of arguments"

primHPutChar :: [Value] -> Eval Value
primHPutChar [Vimm (PIntzh hdl), Vimm (PCharzh c)] =
  liftIO (hPutChar 
     (if hdl == 0
        then stdin
        else if hdl == 1
               then stdout
               else -- lol
                 stderr) (chr (fromIntegral c))) >>
  returnUnit
primHPutChar _ = error "primHPutChar: wrong number of arguments"

evalExternal :: String -> [Value] -> Eval Value
-- etc.
evalExternal s _ = error $ "evalExternal undefined for now: " ++ show s  -- etc.,etc.

returnUnit :: Eval Value
returnUnit = do    
  p <- hallocateE (Hclos eempty "_"
         (App (App (Dcon (dcUtuple 2)) stateToken) unitCon))
  return $ Vheap p

evalLit :: Lit -> PrimValue
evalLit (Literal l t) = 
    case l of
      Lint i | (Tcon(_,"Intzh")) <- t -> PIntzh i
      Lint i | (Tcon(_,"Wordzh")) <- t -> PWordzh i
      Lint i | (Tcon(_,"Addrzh")) <- t -> PAddrzh i
      Lint i | (Tcon(_,"Charzh"))<- t -> PCharzh i
      Lrational r | (Tcon(_,"Floatzh"))  <- t -> PFloatzh r
      Lrational r | (Tcon(_,"Doublezh")) <- t -> PDoublezh r
      Lchar c | (Tcon(_,"Charzh")) <- t       -> PCharzh (toEnum (ord c))
      Lstring s | (Tcon(_,"Addrzh")) <- t     -> PString s
          -- should really be address of non-heap copy of C-format string s
          -- tjc: I am ignoring this comment
      _ -> error ("evalLit: strange combination of literal "
             ++ show l ++ " and type " ++ show t)

{- Utilities -}

mkBool :: Bool -> Eval Value
mkBool True = 
  do p <- hallocateE (Hconstr "True" [])
     return (Vheap p)
mkBool False = 
  do p <- hallocateE (Hconstr "False" [])
     return (Vheap p)

thin :: Ord a => Env a b -> [a] -> Env a b    
thin env vars = efilter env (`elem` vars)

{- Return the free non-external variables in an expression. -}

freevarsExp :: Exp -> [Var]
freevarsExp (Var (Nothing,v)) = [v]
freevarsExp (Var _) = []
freevarsExp (Dcon _) = []
freevarsExp (Lit _) = []
freevarsExp (App e1 e2) = freevarsExp e1 `union` freevarsExp e2
freevarsExp (Appt e _) = freevarsExp e
freevarsExp (Lam (Vb(v,_)) e) = delete v (freevarsExp e)
freevarsExp (Lam _ e) = freevarsExp e
freevarsExp (Let vdefg e) = freevarsVdefg vdefg `union` freevarsExp e
  where freevarsVdefg (Rec vdefs) = (foldl union [] (map freevarsExp es)) \\ vs
            where (vs,es) = unzip [(v,e) | Vdef((_,v),_,e) <- vdefs]	
        freevarsVdefg (Nonrec (Vdef (_,_,e))) = freevarsExp e
freevarsExp (Case e (v,_) _ as) = freevarsExp e `union` [v] `union` freevarsAlts as
  where freevarsAlts alts = foldl union [] (map freevarsAlt alts)
        freevarsAlt (Acon _ _ vbs e) = freevarsExp e \\ (map fst vbs) 
        freevarsAlt (Alit _ e) = freevarsExp e
        freevarsAlt (Adefault e) = freevarsExp e
freevarsExp (Cast e _) = freevarsExp e
freevarsExp (Note _ e) =  freevarsExp e
freevarsExp (External _ _) = []

stateToken :: Exp
stateToken = Var (qual primMname "realWorldzh")

unitCon :: Exp
unitCon = Dcon (qual baseMname "Z0T")
