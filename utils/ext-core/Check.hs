module Check where

import Monad
import Core
import Printer
import List
import Env

{- Checking is done in a simple error monad.  In addition to
   allowing errors to be captured, this makes it easy to guarantee
   that checking itself has been completed for an entire module. -}

data CheckResult a = OkC a | FailC String

instance Monad CheckResult where
  OkC a >>= k = k a
  FailC s >>= k = fail s
  return = OkC
  fail = FailC

require :: Bool -> String -> CheckResult ()
require False s = fail s
require True  _ = return ()

requireM :: CheckResult Bool -> String -> CheckResult ()
requireM cond s =
  do b <- cond
     require b s

{- Environments. -}
type Tvenv = Env Tvar Kind                    -- type variables  (local only)
type Tcenv = Env Tcon Kind                    -- type constructors
type Tsenv = Env Tcon ([Tvar],Ty)             -- type synonyms
type Cenv = Env Dcon Ty 		      -- data constructors
type Venv = Env Var Ty 			      -- values
type Menv = Env Mname Envs		      -- modules
data Envs = Envs {tcenv_::Tcenv,tsenv_::Tsenv,cenv_::Cenv,venv_::Venv} -- all the exportable envs

{- Extend an environment, checking for illegal shadowing of identifiers. -}
extendM :: (Ord a, Show a) => Env a b -> (a,b) -> CheckResult (Env a b)
extendM env (k,d) = 
   case elookup env k of
     Just _ -> fail ("multiply-defined identifier: " ++ show k)
     Nothing -> return (eextend env (k,d))

lookupM :: (Ord a, Show a) => Env a b -> a -> CheckResult b
lookupM env k =   
   case elookup env k of
     Just v -> return v
     Nothing -> fail ("undefined identifier: " ++ show k)
            
{- Main entry point. -}
checkModule :: Menv -> Module -> CheckResult Menv
checkModule globalEnv (Module mn tdefs vdefgs) = 
  do (tcenv,tsenv) <- foldM checkTdef0 (eempty,eempty) tdefs
     cenv <- foldM (checkTdef tcenv) eempty tdefs
     (e_venv,l_venv) <- foldM (checkVdefg True (tcenv,tsenv,eempty,cenv)) (eempty,eempty) vdefgs
     return (eextend globalEnv (mn,Envs{tcenv_=tcenv,tsenv_=tsenv,cenv_=cenv,venv_=e_venv}))
  where 

    checkTdef0 :: (Tcenv,Tsenv) -> Tdef -> CheckResult (Tcenv,Tsenv)
    checkTdef0 (tcenv,tsenv) tdef = ch tdef
      where 
	ch (Data (m,c) tbs _) = 
	    do require (m == mn) ("wrong module name in data type declaration:\n" ++ show tdef)
	       tcenv' <- extendM tcenv (c,k)
	       return (tcenv',tsenv)
	    where k = foldr Karrow Klifted (map snd tbs)
	ch (Newtype (m,c) tbs rhs) = 
	    do require (m == mn) ("wrong module name in newtype declaration:\n" ++ show tdef)
	       tcenv' <- extendM tcenv (c,k)
	       tsenv' <- case rhs of
			   Nothing -> return tsenv
			   Just rep -> extendM tsenv (c,(map fst tbs,rep))
	       return (tcenv', tsenv')
	    where k = foldr Karrow Klifted (map snd tbs)
    
    checkTdef :: Tcenv -> Cenv -> Tdef -> CheckResult Cenv
    checkTdef tcenv cenv = ch
       where 
	 ch (Data (_,c) utbs cdefs) = 
	    do cbinds <- mapM checkCdef cdefs
	       foldM extendM cenv cbinds
	    where checkCdef (cdef@(Constr (m,dcon) etbs ts)) =
		    do require (m == mn) ("wrong module name in constructor declaration:\n" ++ show cdef)
		       tvenv <- foldM extendM eempty tbs 
		       ks <- mapM (checkTy (tcenv,tvenv)) ts
		       mapM_ (\k -> require (baseKind k)
					    ("higher-order kind in:\n" ++ show cdef ++ "\n" ++
					     "kind: " ++ show k) ) ks
		       return (dcon,t) 
		    where tbs = utbs ++ etbs
			  t = foldr Tforall 
				  (foldr tArrow
					  (foldl Tapp (Tcon (mn,c))
						 (map (Tvar . fst) utbs)) ts) tbs
	 ch (tdef@(Newtype c tbs (Just t))) =  
	    do tvenv <- foldM extendM eempty tbs
	       k <- checkTy (tcenv,tvenv) t
	       require (k==Klifted) ("bad kind:\n" ++ show tdef) 
	       return cenv
	 ch (tdef@(Newtype c tbs Nothing)) =
	    {- should only occur for recursive Newtypes -}
	    return cenv
    

    checkVdefg :: Bool -> (Tcenv,Tsenv,Tvenv,Cenv) -> (Venv,Venv) -> Vdefg -> CheckResult (Venv,Venv)
    checkVdefg top_level (tcenv,tsenv,tvenv,cenv) (e_venv,l_venv) vdefg =
      case vdefg of
	Rec vdefs ->
	    do e_venv' <- foldM extendM e_venv e_vts
	       l_venv' <- foldM extendM l_venv l_vts
	       let env' = (tcenv,tsenv,tvenv,cenv,e_venv',l_venv')
	       mapM_ (\ (vdef@(Vdef ((m,v),t,e))) -> 
			    do require (m == "" || m == mn) ("wrong module name in value definition:\n" ++ show vdef)
			       k <- checkTy (tcenv,tvenv) t
			       require (k==Klifted) ("unlifted kind in:\n" ++ show vdef)
			       t' <- checkExp env' e
			       requireM (equalTy tsenv t t') 
					("declared type doesn't match expression type in:\n"  ++ show vdef ++ "\n" ++  
					 "declared type: " ++ show t ++ "\n" ++
					 "expression type: " ++ show t')) vdefs
	       return (e_venv',l_venv')
	    where e_vts  = [ (v,t) | Vdef ((m,v),t,_) <- vdefs, m /= "" ]
	          l_vts  = [ (v,t) | Vdef (("",v),t,_) <- vdefs]
	Nonrec (vdef@(Vdef ((m,v),t,e))) ->
	    do require (m == "" || m == mn) ("wrong module name in value definition:\n" ++ show vdef)
	       k <- checkTy (tcenv,tvenv) t 
	       require (k /= Kopen) ("open kind in:\n" ++ show vdef)
	       require ((not top_level) || (k /= Kunlifted)) ("top-level unlifted kind in:\n" ++ show vdef) 
	       t' <- checkExp (tcenv,tsenv,tvenv,cenv,e_venv,l_venv) e
	       requireM (equalTy tsenv t t') 
			("declared type doesn't match expression type in:\n" ++ show vdef  ++ "\n"  ++
			 "declared type: " ++ show t ++ "\n" ++
			 "expression type: " ++ show t') 
	       if m == "" then
                 do l_venv' <- extendM l_venv (v,t)
                    return (e_venv,l_venv')
	        else
		 do e_venv' <- extendM e_venv (v,t)
                    return (e_venv',l_venv)
    
    checkExp ::  (Tcenv,Tsenv,Tvenv,Cenv,Venv,Venv) -> Exp -> CheckResult Ty
    checkExp (tcenv,tsenv,tvenv,cenv,e_venv,l_venv) = ch 
      where 
	ch e0 = 
	  case e0 of
	    Var qv -> 
	      qlookupM venv_ e_venv l_venv qv
	    Dcon qc ->
	      qlookupM cenv_ cenv eempty qc
	    Lit l -> 
	      checkLit l
	    Appt e t -> 
	      do t' <- ch e
		 k' <- checkTy (tcenv,tvenv) t
		 case t' of
		   Tforall (tv,k) t0 ->
		     do require (k' <= k) 
				("kind doesn't match at type application in:\n" ++ show e0 ++ "\n" ++
				 "operator kind: " ++ show k ++ "\n" ++
				 "operand kind: " ++ show k') 
			return (substl [tv] [t] t0)
		   _ -> fail ("bad operator type in type application:\n" ++ show e0 ++ "\n" ++
			       "operator type: " ++ show t')
	    App e1 e2 -> 
	      do t1 <- ch e1
		 t2 <- ch e2
		 case t1 of
		   Tapp(Tapp(Tcon tc) t') t0 | tc == tcArrow ->
			do requireM (equalTy tsenv t2 t') 
				    ("type doesn't match at application in:\n" ++ show e0 ++ "\n" ++ 
				     "operator type: " ++ show t' ++ "\n" ++ 
				     "operand type: " ++ show t2) 
			   return t0
		   _ -> fail ("bad operator type at application in:\n" ++ show e0 ++ "\n" ++
			       "operator type: " ++ show t1)
	    Lam (Tb tb) e ->
	      do tvenv' <- extendM tvenv tb 
		 t <- checkExp (tcenv,tsenv,tvenv',cenv,e_venv,l_venv) e 
		 return (Tforall tb t)
	    Lam (Vb (vb@(_,vt))) e ->
	      do k <- checkTy (tcenv,tvenv) vt
		 require (baseKind k) 	
			 ("higher-order kind in:\n" ++ show e0 ++ "\n" ++
			  "kind: " ++ show k) 
		 l_venv' <- extendM l_venv vb
		 t <- checkExp (tcenv,tsenv,tvenv,cenv,e_venv,l_venv') e
		 require (not (isUtupleTy vt)) ("lambda-bound unboxed tuple in:\n" ++ show e0) 
		 return (tArrow vt t)
	    Let vdefg e ->
	      do (e_venv',l_venv') <- checkVdefg False (tcenv,tsenv,tvenv,cenv) (e_venv,l_venv) vdefg 
		 checkExp (tcenv,tsenv,tvenv,cenv,e_venv',l_venv') e
	    Case e (v,t) alts ->
	      do t' <- ch e 
		 checkTy (tcenv,tvenv) t
		 requireM (equalTy tsenv t t') 
			  ("scrutinee declared type doesn't match expression type in:\n" ++ show e0 ++ "\n" ++
			   "declared type: " ++ show t ++ "\n" ++
			   "expression type: " ++ show t') 
		 case (reverse alts) of
		   (Acon c _ _ _):as ->
		      let ok ((Acon c _ _ _):as) cs = do require (notElem c cs)
								 ("duplicate alternative in case:\n" ++ show e0) 
							 ok as (c:cs)
			  ok ((Alit _ _):_)      _  = fail ("invalid alternative in constructor case:\n" ++ show e0)
			  ok [Adefault _]        _  = return ()
			  ok (Adefault _:_)      _  = fail ("misplaced default alternative in case:\n" ++ show e0)
			  ok []                  _  = return () 
		      in ok as [c] 
		   (Alit l _):as -> 
		      let ok ((Acon _ _ _ _):_) _  = fail ("invalid alternative in literal case:\n" ++ show e0)
			  ok ((Alit l _):as)    ls = do require (notElem l ls)
								("duplicate alternative in case:\n" ++ show e0) 
							ok as (l:ls)
			  ok [Adefault _]       _  = return ()
			  ok (Adefault _:_)     _  = fail ("misplaced default alternative in case:\n" ++ show e0)
			  ok []                 _  = fail ("missing default alternative in literal case:\n" ++ show e0)
		      in ok as [l] 
		   [Adefault _] -> return ()
		   [] -> fail ("no alternatives in case:\n" ++ show e0) 
		 l_venv' <- extendM l_venv (v,t)
		 t:ts <- mapM (checkAlt (tcenv,tsenv,tvenv,cenv,e_venv,l_venv') t) alts  
		 bs <- mapM (equalTy tsenv t) ts
		 require (and bs)
			 ("alternative types don't match in:\n" ++ show e0 ++ "\n" ++
			  "types: " ++ show (t:ts))
		 return t
	    Coerce t e -> 
	      do ch e 
		 checkTy (tcenv,tvenv) t 
		 return t
	    Note s e -> 
	      ch e
	    External _ t -> 
	      do checkTy (tcenv,eempty) t {- external types must be closed -}
		 return t
    
    checkAlt :: (Tcenv,Tsenv,Tvenv,Cenv,Venv,Venv) -> Ty -> Alt -> CheckResult Ty 
    checkAlt (env@(tcenv,tsenv,tvenv,cenv,e_venv,l_venv)) t0 = ch
      where 
	ch a0 = 
	  case a0 of 
	    Acon qc etbs vbs e ->
	      do let uts = f t0                                      
		       where f (Tapp t0 t) = f t0 ++ [t]
			     f _ = []
		 ct <- qlookupM cenv_ cenv eempty qc
		 let (tbs,ct_args0,ct_res0) = splitTy ct
		 {- get universals -}
		 let (utbs,etbs') = splitAt (length uts) tbs
		 let utvs = map fst utbs
		 {- check existentials -}
		 let (etvs,eks) = unzip etbs
		 let (etvs',eks') = unzip etbs'
		 require (eks == eks')  
			 ("existential kinds don't match in:\n" ++ show a0 ++ "\n" ++
			  "kinds declared in data constructor: " ++ show eks ++
			  "kinds declared in case alternative: " ++ show eks') 
		 tvenv' <- foldM extendM tvenv etbs
		 {- check term variables -}
		 let vts = map snd vbs
		 mapM_ (\vt -> require ((not . isUtupleTy) vt)
				       ("pattern-bound unboxed tuple in:\n" ++ show a0 ++ "\n" ++
					"pattern type: " ++ show vt)) vts
		 vks <- mapM (checkTy (tcenv,tvenv')) vts
		 mapM_ (\vk -> require (baseKind vk)
				       ("higher-order kind in:\n" ++ show a0 ++ "\n" ++
					"kind: " ++ show vk)) vks 
		 let (ct_res:ct_args) = map (substl (utvs++etvs') (uts++(map Tvar etvs))) (ct_res0:ct_args0)
		 zipWithM_ 
		    (\ct_arg vt -> 
			requireM (equalTy tsenv ct_arg vt)
			         ("pattern variable type doesn't match constructor argument type in:\n" ++ show a0 ++ "\n" ++
				  "pattern variable type: " ++ show ct_arg ++ "\n" ++
				  "constructor argument type: " ++ show vt)) ct_args vts
		 requireM (equalTy tsenv ct_res t0)
			  ("pattern constructor type doesn't match scrutinee type in:\n" ++ show a0 ++ "\n" ++
			   "pattern constructor type: " ++ show ct_res ++ "\n" ++
			   "scrutinee type: " ++ show t0) 
		 l_venv' <- foldM extendM l_venv vbs
		 t <- checkExp (tcenv,tsenv,tvenv',cenv,e_venv,l_venv') e 
		 checkTy (tcenv,tvenv) t  {- check that existentials don't escape in result type -}
		 return t
	    Alit l e ->
	      do t <- checkLit l
		 requireM (equalTy tsenv t t0)
			 ("pattern type doesn't match scrutinee type in:\n" ++ show a0 ++ "\n" ++
			  "pattern type: " ++ show t ++ "\n" ++
			  "scrutinee type: " ++ show t0) 
		 checkExp env e
	    Adefault e ->
	      checkExp env e
    
    checkTy :: (Tcenv,Tvenv) -> Ty -> CheckResult Kind
    checkTy (tcenv,tvenv) = ch
     where
       ch (Tvar tv) = lookupM tvenv tv
       ch (Tcon qtc) = qlookupM tcenv_ tcenv eempty qtc
       ch (t@(Tapp t1 t2)) = 
	   do k1 <- ch t1
	      k2 <- ch t2
	      case k1 of
		 Karrow k11 k12 ->
		   do require (k2 <= k11) 
			     ("kinds don't match in type application: " ++ show t ++ "\n" ++
			      "operator kind: " ++ show k11 ++ "\n" ++
			      "operand kind: " ++ show k2) 		
		      return k12
		 _ -> fail ("applied type has non-arrow kind: " ++ show t)
       ch (Tforall tb t) = 
	    do tvenv' <- extendM tvenv tb 
	       checkTy (tcenv,tvenv') t
    
    {- Type equality modulo newtype synonyms. -}
    equalTy :: Tsenv -> Ty -> Ty -> CheckResult Bool
    equalTy tsenv t1 t2 = 
	    do t1' <- expand t1
	       t2' <- expand t2
	       return (t1' == t2')
      where expand (Tvar v) = return (Tvar v)
	    expand (Tcon qtc) = return (Tcon qtc)
	    expand (Tapp t1 t2) = 
	      do t2' <- expand t2
		 expapp t1 [t2']
	    expand (Tforall tb t) = 
	      do t' <- expand t
		 return (Tforall tb t')
	    expapp (t@(Tcon (m,tc))) ts = 
	      do env <- mlookupM tsenv_ tsenv eempty m
		 case elookup env tc of 
		    Just (formals,rhs) | (length formals) == (length ts) -> return (substl formals ts rhs)
		    _ -> return (foldl Tapp t ts)
	    expapp (Tapp t1 t2) ts = 
	      do t2' <- expand t2
		 expapp t1 (t2':ts)
	    expapp t ts = 
	      do t' <- expand t
		 return (foldl Tapp t' ts)
    

    mlookupM :: (Envs -> Env a b) -> Env a b -> Env a b -> Mname -> CheckResult (Env a b)
    mlookupM selector external_env local_env m =   
      if m == "" then
        return local_env
      else if m == mn then
        return external_env
      else 
        case elookup globalEnv m of
          Just env' -> return (selector env')
          Nothing -> fail ("undefined module name: " ++ show m)

    qlookupM :: (Ord a, Show a) => (Envs -> Env a b) -> Env a b -> Env a b -> (Mname,a) -> CheckResult b
    qlookupM selector external_env local_env (m,k) =   
      do env <- mlookupM selector external_env local_env m
	 lookupM env k


checkLit :: Lit -> CheckResult Ty
checkLit lit =
  case lit of
    Lint _ t -> 
	  do {- require (elem t [tIntzh, {- tInt32zh,tInt64zh, -} tWordzh, {- tWord32zh,tWord64zh, -} tAddrzh, tCharzh]) 
		     ("invalid int literal: " ++ show lit ++ "\n" ++ "type: " ++ show t) -}
	     return t
    Lrational _ t ->
	  do {- require (elem t [tFloatzh,tDoublezh]) 
		     ("invalid rational literal: " ++ show lit ++ "\n" ++ "type: " ++ show t) -}
	     return t
    Lchar _ t -> 
	  do {- require (t == tCharzh) 
		     ("invalid char literal: " ++ show lit ++ "\n" ++ "type: " ++ show t)  -}
	     return t	
    Lstring _ t ->
	  do {- require (t == tAddrzh) 
		     ("invalid string literal: " ++ show lit ++ "\n" ++ "type: " ++ show t)  -}
	     return t

{- Utilities -}

{- Split off tbs, arguments and result of a (possibly abstracted)  arrow type -}
splitTy :: Ty -> ([Tbind],[Ty],Ty)
splitTy (Tforall tb t) = (tb:tbs,ts,tr) 
		where (tbs,ts,tr) = splitTy t
splitTy (Tapp(Tapp(Tcon tc) t0) t) | tc == tcArrow = (tbs,t0:ts,tr)
		where (tbs,ts,tr) = splitTy t
splitTy t = ([],[],t)


{- Simultaneous substitution on types for type variables,
   renaming as neceessary to avoid capture.
   No checks for correct kindedness. -}
substl :: [Tvar] -> [Ty] -> Ty -> Ty
substl tvs ts t = f (zip tvs ts) t
  where 
    f env t0 =
     case t0 of
       Tcon _ -> t0
       Tvar v -> case lookup v env of
                   Just t1 -> t1
                   Nothing -> t0
       Tapp t1 t2 -> Tapp (f env t1) (f env t2)
       Tforall (t,k) t1 -> 
         if t `elem` free then
           Tforall (t',k) (f ((t,Tvar t'):env) t1)
         else 
	   Tforall (t,k) (f (filter ((/=t).fst) env) t1)
     where free = foldr union [] (map (freeTvars.snd) env)
           t' = freshTvar free 
   
{- Return free tvars in a type -}
freeTvars :: Ty -> [Tvar]
freeTvars (Tcon _) = []
freeTvars (Tvar v) = [v]
freeTvars (Tapp t1 t2) = (freeTvars t1) `union` (freeTvars t2)
freeTvars (Tforall (t,_) t1) = delete t (freeTvars t1) 

{- Return any tvar *not* in the argument list. -}
freshTvar :: [Tvar] -> Tvar
freshTvar tvs = maximum ("":tvs) ++ "x" -- one simple way!

