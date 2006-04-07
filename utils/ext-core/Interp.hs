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

module Interp where

import Core
import Printer
import Monad
import Env
import List
import Char
import Prims

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

type Venv = Env Var Value       -- values of vars

data PrimValue =                -- values of the (unboxed) primitive types
    PCharzh Integer		-- actually 31-bit unsigned
  | PIntzh Integer		-- actually WORD_SIZE_IN_BITS-bit signed
  | PWordzh Integer		-- actually WORD_SIZE_IN_BITS-bit unsigned
  | PAddrzh Integer		-- actually native pointer size
  | PFloatzh Rational		-- actually 32-bit 
  | PDoublezh Rational		-- actually 64-bit
--  etc., etc.
  deriving (Eq,Show)

type Menv = Env Mname Venv	-- modules

initialGlobalEnv :: Menv
initialGlobalEnv =
    efromlist
	[(primMname,efromlist [("realWorldzh",Vimm (PIntzh 0))])]

{- Heap management. -}
{- Nothing is said about garbage collection. -}

data Heap = Heap Ptr (Env Ptr HeapValue) -- last cell allocated; environment of allocated cells
  deriving (Show)

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

newtype Eval a = Eval (Heap -> (Heap,Either a Exn))

instance Monad Eval where
  (Eval m) >>= k = Eval (
             \h -> case m h of
                    (h',Left x) -> case k x of
                                     Eval k' -> k' h'
                    (h',Right exn) -> (h',Right exn))
  return x = Eval (\h -> (h,Left x))

hallocateE :: HeapValue -> Eval Ptr
hallocateE v = Eval (\ h -> 
   let (h',p) = hallocate h v
   in (h', Left p))

hupdateE :: Ptr -> HeapValue -> Eval ()
hupdateE p v = Eval (\h -> (hupdate h p v,Left ()))

hlookupE :: Ptr -> Eval HeapValue
hlookupE p =  Eval (\h -> (h,Left (hlookup h p)))

hremoveE :: Ptr -> Eval ()
hremoveE p = Eval (\h -> (hremove h p, Left ()))

raiseE :: Exn -> Eval a
raiseE exn = Eval (\h -> (h,Right exn))

catchE :: Eval a -> (Exn -> Eval a) -> Eval a
catchE (Eval m) f = Eval 
                       (\h -> case m h of
                               (h',Left x) -> (h',Left x)
                               (h',Right exn) -> 
                                       case f exn of
                                         Eval f' -> f' h')

runE :: Eval a -> a
runE (Eval f) = 
  case f hempty of
    (_,Left v) -> v
    (_,Right exn) ->  error ("evaluation failed with uncaught exception: " ++ show exn)


{- Main entry point -}
evalProgram :: [Module] -> Value
evalProgram modules =
 runE(
  do globalEnv <- foldM evalModule initialGlobalEnv modules
     Vutuple [_,v] <- evalExp globalEnv eempty (App (Var ("Main","main")) (Var (primMname,"realWorldzh")))
     return v)

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
evalModule globalEnv (Module mn tdefs vdefgs) = 
  do (e_venv,l_venv) <- foldM evalVdef (eempty,eempty) vdefgs
     return (eextend globalEnv (mn,e_venv))
  where
    evalVdef :: (Venv,Venv) -> Vdefg -> Eval (Venv,Venv)
    evalVdef (e_env,l_env) (Nonrec(Vdef((m,x),t,e))) =
     do p <- hallocateE (suspendExp l_env e)
	let heaps = 
	       if m == "" then 
		 (e_env,eextend l_env (x,Vheap p))
	       else 
		 (eextend e_env (x,Vheap p),l_env)
	return heaps
    evalVdef (e_env,l_env) (Rec vdefs) =
      do l_vs0 <- mapM preallocate l_xs
	 let l_env' = foldl eextend l_env (zip l_xs l_vs0)
	 let l_hs = map (suspendExp l_env') l_es
	 mapM_ reallocate (zip l_vs0 l_hs)
	 let e_hs = map (suspendExp l_env') e_es
	 e_vs <- mapM allocate e_hs
	 let e_env' = foldl eextend e_env (zip e_xs e_vs)
	 return (e_env',l_env')            
      where 
	 (l_xs,l_es) = unzip [(x,e) | Vdef(("",x),_,e) <- vdefs]
	 (e_xs,e_es) = unzip [(x,e) | Vdef((m,x),_,e) <- vdefs, m /= ""]
	 preallocate _ =
	   do p <- hallocateE undefined
	      return (Vheap p)
	 reallocate (Vheap p0,h) =
	   hupdateE p0 h
	 allocate h =
	   do p <- hallocateE h
	      return (Vheap p)

    suspendExp:: Venv -> Exp -> HeapValue
    suspendExp env (Lam (Vb(x,_)) e) = Hclos env' x e
       where env' = thin env (delete x (freevarsExp e))
    suspendExp env e = Hthunk env' e
       where env' = thin env (freevarsExp e)
	

evalExp :: Menv -> Venv -> Exp -> Eval Value
evalExp globalEnv env (Var qv) =
  let v = qlookup globalEnv env qv
  in case v of 
       Vheap p ->
	  do z <- hlookupE p                                  -- can fail due to black-holing
	     case z of
	       Hthunk env' e -> 
		 do hremoveE p                                -- black-hole
		    w@(Vheap p') <- evalExp globalEnv env' e  -- result is guaranteed to be boxed!
	            h <- hlookupE p'        
		    hupdateE p h			
	            return w
	       _ -> return v                 -- return pointer to Hclos or Hconstr 
       _ -> return v                         -- return Vimm or Vutuple
evalExp globalEnv env (Lit l) = return (Vimm (evalLit l))
evalExp globalEnv env (Dcon (_,c)) = 
  do p <- hallocateE (Hconstr c [])
     return (Vheap p)

evalExp globalEnv env (App e1 e2) = evalApp env e1 [e2] 
  where
    evalApp :: Venv -> Exp -> [Exp] -> Eval Value
    evalApp env (App e1 e2) es = evalApp env e1 (e2:es)
    evalApp env (op @(Dcon (qdc@(m,c)))) es = 
      do vs <- suspendExps globalEnv env es
	 if isUtupleDc qdc then
	   return (Vutuple vs)
	  else
	    {- allocate a thunk -}
     	    do p <- hallocateE (Hconstr c vs)
  	       return (Vheap p)
    evalApp env (op @ (Var(m,p))) es | m == primMname =
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
    evalApp env (Appt e _) es = evalApp env e es
    evalApp env (Lam (Tb _) e) es = evalApp env e es
    evalApp env (Coerce _ e) es = evalApp env e es
    evalApp env (Note _ e) es = evalApp env e es
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


evalExp globalEnv env (Appt e _) = evalExp globalEnv env e
evalExp globalEnv env (Lam (Vb(x,_)) e) = 
  do p <- hallocateE (Hclos env' x e)
     return (Vheap p)
  where env' = thin env (delete x (freevarsExp e)) 
evalExp globalEnv env (Lam _ e) = evalExp globalEnv env e
evalExp globalEnv env (Let vdef e) =
  do env' <- evalVdef globalEnv env vdef
     evalExp globalEnv env' e
  where
    evalVdef :: Menv -> Venv -> Vdefg -> Eval Venv
    evalVdef globalEnv env (Nonrec(Vdef((m,x),t,e))) =
      do v <- suspendExp globalEnv env e
	 return (eextend env (x,v))
    evalVdef globalEnv env (Rec vdefs) =
      do vs0 <- mapM preallocate xs
	 let env' = foldl eextend env (zip xs vs0) 
	 vs <- suspendExps globalEnv env' es
	 mapM_ reallocate (zip vs0 vs)
	 return env'
      where 
	(xs,es) = unzip [(x,e) | Vdef((_,x),_,e) <- vdefs]
	preallocate _ = 
	  do p <- hallocateE (Hconstr "UGH" [])
	     return (Vheap p)
	reallocate (Vheap p0,Vheap p) =
	  do h <- hlookupE p
	     hupdateE p0 h
	
evalExp globalEnv env (Case e (x,_) alts) =  
  do z <- evalExp globalEnv env e
     let env' = eextend env (x,z)
     case z of
       Vheap p ->
	 do h <- hlookupE p   -- can fail due to black-holing
	    case h of
	      Hconstr dcon vs -> evalDcAlt env' dcon vs (reverse alts)
  	      _ -> evalDefaultAlt env' alts
       Vutuple vs ->
	 evalUtupleAlt env' vs (reverse alts)
       Vimm pv ->
	 evalLitAlt env' pv (reverse alts)
  where
    evalDcAlt :: Venv -> Dcon -> [Value] -> [Alt] -> Eval Value
    evalDcAlt env dcon vs alts = 
      f alts
      where 
	f ((Acon (_,dcon') _ xs e):as) =
	  if dcon == dcon' then
	    evalExp globalEnv (foldl eextend env (zip (map fst xs) vs)) e
	  else f as
	f [Adefault e] =
	  evalExp globalEnv env e
	f _ = error "impossible Case-evalDcAlt"
    
    evalUtupleAlt :: Venv -> [Value] -> [Alt] -> Eval Value
    evalUtupleAlt env vs [Acon _ _ xs e] = 
       evalExp globalEnv (foldl eextend env (zip (map fst xs) vs)) e

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

evalExp globalEnv env (Coerce _ e) = evalExp globalEnv env e
evalExp globalEnv env (Note _ e) = evalExp globalEnv env e
evalExp globalEnv env (External s t) = evalExternal s []

evalExps :: Menv -> Venv -> [Exp] -> Eval [Value]
evalExps globalEnv env = mapM (evalExp globalEnv env)

suspendExp:: Menv -> Venv -> Exp -> Eval Value
suspendExp globalEnv env (Var qv) = return (qlookup globalEnv env qv)
suspendExp globalEnv env (Lit l) = return (Vimm (evalLit l))
suspendExp globalEnv env (Lam (Vb(x,_)) e) = 
   do p <- hallocateE (Hclos env' x e)
      return (Vheap p)
   where env' = thin env (delete x (freevarsExp e))
suspendExp globalEnv env (Lam _ e) = suspendExp globalEnv env e
suspendExp globalEnv env (Appt e _) = suspendExp globalEnv env e
suspendExp globalEnv env (Coerce _ e) = suspendExp globalEnv env e
suspendExp globalEnv env (Note _ e) = suspendExp globalEnv env e
suspendExp globalEnv env (External s _) = evalExternal s []
suspendExp globalEnv env e = 
   do p <- hallocateE (Hthunk env' e)
      return (Vheap p)
   where env' = thin env (freevarsExp e)

suspendExps :: Menv -> Venv -> [Exp] -> Eval [Value]
suspendExps globalEnv env = mapM (suspendExp globalEnv env)

mlookup :: Menv -> Venv -> Mname -> Venv
mlookup _          env       "" = env
mlookup globalEnv  _         m  = 
    case elookup globalEnv m of
      Just env' -> env'
      Nothing -> error ("undefined module name: " ++ m)

qlookup :: Menv -> Venv -> (Mname,Var) -> Value
qlookup globalEnv env (m,k) =   
  case elookup (mlookup globalEnv env m) k of
    Just v -> v
    Nothing -> error ("undefined identifier: " ++ show m ++ "." ++ show k)

evalPrimop :: Var -> [Value] -> Eval Value
evalPrimop "zpzh" [Vimm (PIntzh i1),Vimm (PIntzh i2)] = return (Vimm (PIntzh (i1+i2)))
evalPrimop "zmzh" [Vimm (PIntzh i1),Vimm (PIntzh i2)] = return (Vimm (PIntzh (i1-i2)))
evalPrimop "ztzh" [Vimm (PIntzh i1),Vimm (PIntzh i2)] = return (Vimm (PIntzh (i1*i2)))
evalPrimop "zgzh" [Vimm (PIntzh i1),Vimm (PIntzh i2)] = mkBool (i1 > i2)
evalPrimop "remIntzh" [Vimm (PIntzh i1),Vimm (PIntzh i2)] = return (Vimm (PIntzh (i1 `rem` i2)))
-- etc.
evalPrimop p vs = error ("undefined primop: " ++ p)

evalExternal :: String -> [Value] -> Eval Value
-- etc.
evalExternal s vs = error "evalExternal undefined for now"  -- etc.,etc.
    
evalLit :: Lit -> PrimValue
evalLit l = 
    case l of
      Lint i (Tcon(_,"Intzh")) -> PIntzh i
      Lint i (Tcon(_,"Wordzh")) -> PWordzh i
      Lint i (Tcon(_,"Addrzh")) -> PAddrzh i
      Lint i (Tcon(_,"Charzh")) -> PCharzh i
      Lrational r (Tcon(_,"Floatzh")) -> PFloatzh r
      Lrational r (Tcon(_,"Doublezh")) -> PDoublezh r
      Lchar c (Tcon(_,"Charzh")) -> PCharzh (toEnum (ord c))
      Lstring s (Tcon(_,"Addrzh")) -> PAddrzh 0	 -- should really be address of non-heap copy of C-format string s

{- Utilities -}

mkBool True = 
  do p <- hallocateE (Hconstr "ZdwTrue" [])
     return (Vheap p)
mkBool False = 
  do p <- hallocateE (Hconstr "ZdwFalse" [])
     return (Vheap p)
    
thin env vars = efilter env (`elem` vars)

{- Return the free non-external variables in an expression. -}

freevarsExp :: Exp -> [Var]
freevarsExp (Var ("",v)) = [v]
freevarsExp (Var qv) = []
freevarsExp (Dcon _) = []
freevarsExp (Lit _) = []
freevarsExp (App e1 e2) = freevarsExp e1 `union` freevarsExp e2
freevarsExp (Appt e t) = freevarsExp e
freevarsExp (Lam (Vb(v,_)) e) = delete v (freevarsExp e)
freevarsExp (Lam _ e) = freevarsExp e
freevarsExp (Let vdefg e) = freevarsVdefg vdefg `union` freevarsExp e
  where freevarsVdefg (Rec vdefs) = (foldl union [] (map freevarsExp es)) \\ vs
            where (vs,es) = unzip [(v,e) | Vdef((_,v),_,e) <- vdefs]	
        freevarsVdefg (Nonrec (Vdef (_,_,e))) = freevarsExp e
freevarsExp (Case e (v,_) as) = freevarsExp e `union` [v] `union` freevarsAlts as
  where freevarsAlts alts = foldl union [] (map freevarsAlt alts)
        freevarsAlt (Acon _ _ vbs e) = freevarsExp e \\ (map fst vbs) 
        freevarsAlt (Alit _ e) = freevarsExp e
        freevarsAlt (Adefault e) = freevarsExp e
freevarsExp (Coerce _ e) = freevarsExp e
freevarsExp (Note _ e) =  freevarsExp e
freevarsExp (External _ _) = []




