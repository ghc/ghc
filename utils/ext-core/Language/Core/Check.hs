{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
module Language.Core.Check(
  checkModule, envsModule,
  checkExpr, checkType, 
  primCoercionError, 
  Menv, Venv, Tvenv, Envs(..),
  CheckRes(..), splitTy, substl,
  mkTypeEnvsNoChecking, NtEnv, mkNtEnv) where

--import Debug.Trace

import Language.Core.Core
import Language.Core.CoreUtils
import Language.Core.Printer()
import Language.Core.PrimEnv
import Language.Core.Env
import Language.Core.Environments

import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Data.Maybe

{- Checking is done in a simple error monad.  In addition to
   allowing errors to be captured, this makes it easy to guarantee
   that checking itself has been completed for an entire module. -}

{- We use the Reader monad transformer in order to thread the 
   top-level module name throughout the computation simply.
   This is so that checkExp can also be an entry point (we call it
   from Prep.) -}
data CheckRes a = OkC a | FailC String
type CheckResult a = ReaderT (AnMname, Menv) CheckRes a
getMname :: CheckResult AnMname
getMname     = ask >>= (return . fst)
getGlobalEnv :: CheckResult Menv
getGlobalEnv = ask >>= (return . snd)

instance Monad CheckRes where
  OkC a >>= k = k a
  FailC s >>= _ = fail s
  return = OkC
  fail = FailC

require :: Bool -> String -> CheckResult ()
require False s = fail s
require True  _ = return ()


extendM :: (Ord a, Show a) => Bool -> EnvType -> Env a b -> (a,b) -> CheckResult (Env a b)
extendM checkNameShadowing envType env (k,d) = 
   case elookup env k of
     Just _ | envType == NotTv && checkNameShadowing -> fail ("multiply-defined identifier: " 
                                      ++ show k)
     _ -> return (eextend env (k,d))

extendVenv :: (Ord a, Show a) => Bool -> Env a b -> (a,b) -> CheckResult (Env a b)
extendVenv check = extendM check NotTv

extendTvenv :: (Ord a, Show a) => Env a b -> (a,b) -> CheckResult (Env a b)
extendTvenv = extendM True Tv

lookupM :: (Ord a, Show a) => Env a b -> a -> CheckResult (Maybe b)
lookupM env k = return $ elookup env k
          
{- Main entry point. -}
checkModule :: Menv -> Module -> CheckRes Menv
checkModule globalEnv (Module mn tdefs vdefgs) = 
  runReaderT 
    (do (tcenv, cenv) <- mkTypeEnvs tdefs
        (e_venv,_) <- foldM (checkVdefg True (tcenv,eempty,cenv))
                              (eempty,eempty) 
                              vdefgs
        return (eextend globalEnv 
            (mn,Envs{tcenv_=tcenv,cenv_=cenv,venv_=e_venv})))
    (mn, globalEnv)

-- Like checkModule, but doesn't typecheck the code, instead just
-- returning declared types for top-level defns.
-- This is necessary in order to handle circular dependencies, but it's sort
-- of unpleasant.
envsModule :: Menv -> Module -> Menv
envsModule globalEnv (Module mn tdefs vdefgs) = 
   let (tcenv, cenv) = mkTypeEnvsNoChecking tdefs
       e_venv               = foldr vdefgTypes eempty vdefgs in
     eextend globalEnv (mn, 
             (Envs{tcenv_=tcenv,cenv_=cenv,venv_=e_venv}))
        where vdefgTypes :: Vdefg -> Venv -> Venv
              vdefgTypes (Nonrec (Vdef (v,t,_))) e =
                             add [(v,t)] e
              vdefgTypes (Rec vds) e = 
                             add (map (\ (Vdef (v,t,_)) -> (v,t)) vds) e
              add :: [(Qual Var,Ty)] -> Venv -> Venv
              add pairs e = foldr addOne e pairs
              addOne :: (Qual Var, Ty) -> Venv -> Venv
              addOne ((_,v),t) e  = eextend e (v,t)

checkTdef0 :: Tcenv -> Tdef -> CheckResult Tcenv
checkTdef0 tcenv tdef = ch tdef
      where 
	ch (Data (m,c) tbs _) = 
	    do mn <- getMname
               requireModulesEq m mn "data type declaration" tdef False
	       extendM True NotTv tcenv (c, Kind k)
	    where k = foldr Karrow Klifted (map snd tbs)
	ch (Newtype (m,c) coVar tbs rhs) = 
	    do mn <- getMname
               requireModulesEq m mn "newtype declaration" tdef False
	       tcenv' <- extendM True NotTv tcenv (c, Kind k)
               -- add newtype axiom to env
               tcenv'' <- envPlusNewtype tcenv' (m,c) coVar tbs rhs
	       return tcenv''
	    where k = foldr Karrow Klifted (map snd tbs)

processTdef0NoChecking :: Tcenv -> Tdef -> Tcenv
processTdef0NoChecking tcenv tdef = ch tdef
      where 
	ch (Data (_,c) tbs _) = eextend tcenv (c, Kind k)
	    where k = foldr Karrow Klifted (map snd tbs)
	ch (Newtype tc@(_,c) coercion tbs rhs) = 
	    let tcenv' = eextend tcenv (c, Kind k) in
                -- add newtype axiom to env
                eextend tcenv'
                  (snd coercion, Coercion $ DefinedCoercion tbs
                    (foldl Tapp (Tcon tc) (map Tvar (fst (unzip tbs))), rhs))
	    where k = foldr Karrow Klifted (map snd tbs)

envPlusNewtype :: Tcenv -> Qual Tcon -> Qual Tcon -> [Tbind] -> Ty
  -> CheckResult Tcenv
envPlusNewtype tcenv tyCon coVar tbs rep = extendM True NotTv tcenv
                  (snd coVar, Coercion $ DefinedCoercion tbs
                            (foldl Tapp (Tcon tyCon) 
                                       (map Tvar (fst (unzip tbs))),
                                       rep))
    
checkTdef :: Tcenv -> Cenv -> Tdef -> CheckResult Cenv
checkTdef tcenv cenv = ch
       where 
	 ch (Data (_,c) utbs cdefs) = 
	    do cbinds <- mapM checkCdef cdefs
	       foldM (extendM True NotTv) cenv cbinds
	    where checkCdef (cdef@(Constr (m,dcon) etbs ts)) =
		    do mn <- getMname
                       requireModulesEq m mn "constructor declaration" cdef 
                         False 
		       tvenv <- foldM (extendM True Tv) eempty tbs 
		       ks <- mapM (checkTy (tcenv,tvenv)) ts
		       mapM_ (\k -> require (baseKind k)
					    ("higher-order kind in:\n" ++ show cdef ++ "\n" ++
					     "kind: " ++ show k) ) ks
		       return (dcon,t mn) 
		    where tbs = utbs ++ etbs
			  t mn = foldr Tforall 
				  (foldr tArrow
					  (foldl Tapp (Tcon (Just mn,c))
						 (map (Tvar . fst) utbs)) ts) tbs
         ch (tdef@(Newtype tc _ tbs t)) =  
	    do tvenv <- foldM (extendM True Tv) eempty tbs
	       kRhs <- checkTy (tcenv,tvenv) t
               require (kRhs `eqKind` Klifted) ("bad kind:\n" ++ show tdef)
               kLhs <- checkTy (tcenv,tvenv) 
                         (foldl Tapp (Tcon tc) (map Tvar (fst (unzip tbs))))
               require (kLhs `eqKind` kRhs) 
                  ("Kind mismatch in newtype axiom types: " ++ show tdef 
                    ++ " kinds: " ++
                   (show kLhs) ++ " and " ++ (show kRhs))
	       return cenv

processCdef :: Cenv -> Tdef -> Cenv
processCdef cenv = ch
  where
    ch (Data (_,c) utbs cdefs) = do 
       let cbinds = map checkCdef cdefs
       foldl eextend cenv cbinds
     where checkCdef (Constr (mn,dcon) etbs ts) =
             (dcon,t mn) 
            where tbs = utbs ++ etbs
                  t mn = foldr Tforall 
		          (foldr tArrow
			    (foldl Tapp (Tcon (mn,c))
                               (map (Tvar . fst) utbs)) ts) tbs
    ch _ = cenv

mkTypeEnvs :: [Tdef] -> CheckResult (Tcenv, Cenv)
mkTypeEnvs tdefs = do
  tcenv <- foldM checkTdef0 eempty tdefs
  cenv <- foldM (checkTdef tcenv) eempty tdefs
  return (tcenv, cenv)

mkTypeEnvsNoChecking :: [Tdef] -> (Tcenv, Cenv)
mkTypeEnvsNoChecking tdefs = 
  let tcenv = foldl processTdef0NoChecking eempty tdefs
      cenv  = foldl processCdef eempty tdefs in
    (tcenv, cenv)

requireModulesEq :: Show a => Mname -> AnMname -> String -> a 
                          -> Bool -> CheckResult ()
requireModulesEq (Just mn) m msg t _      = require (mn == m) (mkErrMsg msg t)
requireModulesEq Nothing _ msg t emptyOk  = require emptyOk (mkErrMsg msg t)

mkErrMsg :: Show a => String -> a -> String
mkErrMsg msg t = "wrong module name in " ++ msg ++ ":\n" ++ show t    

checkVdefg :: Bool -> (Tcenv,Tvenv,Cenv) -> (Venv,Venv)
               -> Vdefg -> CheckResult (Venv,Venv)
checkVdefg top_level (tcenv,tvenv,cenv) (e_venv,l_venv) vdefg = do
      mn <- getMname
      case vdefg of
	Rec vdefs ->
	    do (e_venv', l_venv') <- makeEnv mn vdefs
               let env' = (tcenv,tvenv,cenv,e_venv',l_venv')
               mapM_ (checkVdef (\ vdef k -> require (k `eqKind` Klifted) 
                        ("unlifted kind in:\n" ++ show vdef)) env') 
                     vdefs
               return (e_venv', l_venv')
	Nonrec vdef ->
	    do let env' = (tcenv, tvenv, cenv, e_venv, l_venv)
               checkVdef (\ vdef k -> do
                     require (not (k `eqKind` Kopen)) ("open kind in:\n" ++ show vdef)
	             require ((not top_level) || (not (k `eqKind` Kunlifted))) 
                       ("top-level unlifted kind in:\n" ++ show vdef)) env' vdef
               makeEnv mn [vdef]

  where makeEnv mn vdefs = do
             ev <- foldM (extendVenv False) e_venv e_vts
             lv <- foldM (extendVenv False) l_venv l_vts
             return (ev, lv)
           where e_vts = [ (v,t) | Vdef ((Just m,v),t,_) <- vdefs,
                                     not (vdefIsMainWrapper mn (Just m))]
                 l_vts = [ (v,t) | Vdef ((Nothing,v),t,_) <- vdefs]
        checkVdef checkKind env (vdef@(Vdef ((m,_),t,e))) = do
          mn <- getMname
          let isZcMain = vdefIsMainWrapper mn m
          unless isZcMain $
             requireModulesEq m mn "value definition" vdef True
	  k <- checkTy (tcenv,tvenv) t
	  checkKind vdef k
	  t' <- checkExp env e
	  require (t == t')
		   ("declared type doesn't match expression type in:\n"  
                    ++ show vdef ++ "\n" ++  
		    "declared type: " ++ show t ++ "\n" ++
		    "expression type: " ++ show t')
    
vdefIsMainWrapper :: AnMname -> Mname -> Bool
vdefIsMainWrapper enclosing defining = 
   enclosing == mainMname && defining == wrapperMainAnMname

checkExpr :: AnMname -> Menv -> Tcenv -> Cenv -> Venv -> Tvenv 
               -> Exp -> Ty
checkExpr mn menv _tcenv _cenv venv tvenv e = case runReaderT (do
  --(tcenv, cenv) <- mkTypeEnvs tdefs
  -- Since the preprocessor calls checkExpr after code has been
  -- typechecked, we expect to find the external env in the Menv.
  case (elookup menv mn) of
     Just thisEnv ->
       checkExp ({-tcenv-}tcenv_ thisEnv, tvenv, {-cenv-}cenv_ thisEnv, (venv_ thisEnv), venv) e
     Nothing -> reportError e ("checkExpr: Environment for " ++ 
                  show mn ++ " not found")) (mn,menv) of
         OkC t -> t
         FailC s -> reportError e s

checkType :: AnMname -> Menv -> Tcenv -> Tvenv -> Ty -> Kind
checkType mn menv _tcenv tvenv t = 
 case runReaderT (checkTy (tcenv_ (fromMaybe (error "checkType") (elookup menv mn)), tvenv) t) (mn, menv) of
      OkC k -> k
      FailC s -> reportError tvenv (s ++ "\n " ++ show menv ++ "\n mname =" ++ show mn)

checkExp :: (Tcenv,Tvenv,Cenv,Venv,Venv) -> Exp -> CheckResult Ty
checkExp (tcenv,tvenv,cenv,e_venv,l_venv) = ch
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
		     do require (k' `subKindOf` k) 
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
			do require (t2 == t')
				    ("type doesn't match at application in:\n" ++ show e0 ++ "\n" ++ 
				     "operator type: " ++ show t' ++ "\n" ++ 
				     "operand type: " ++ show t2) 
			   return t0
		   _ -> fail ("bad operator type at application in:\n" ++ show e0 ++ "\n" ++
			       "operator type: " ++ show t1)
	    Lam (Tb tb) e ->
	      do tvenv' <- extendTvenv tvenv tb 
		 t <- checkExp (tcenv,tvenv',cenv,e_venv,l_venv) e 
		 return (Tforall tb t)
	    Lam (Vb (vb@(_,vt))) e ->
	      do k <- checkTy (tcenv,tvenv) vt
		 require (baseKind k) 	
			 ("higher-order kind in:\n" ++ show e0 ++ "\n" ++
			  "kind: " ++ show k) 
		 l_venv' <- extendVenv True l_venv vb
		 t <- checkExp (tcenv,tvenv,cenv,e_venv,l_venv') e
		 require (not (isUtupleTy vt)) ("lambda-bound unboxed tuple in:\n" ++ show e0) 
		 return (tArrow vt t)
	    Let vdefg e ->
	      do (e_venv',l_venv') <- checkVdefg False (tcenv,tvenv,cenv)
                                        (e_venv,l_venv) vdefg
		 checkExp (tcenv,tvenv,cenv,e_venv',l_venv') e
	    Case e (v,t) resultTy alts ->
	      do t' <- ch e 
		 checkTy (tcenv,tvenv) t
		 require (t == t')
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
		   _ -> fail ("no alternatives in case:\n" ++ show e0) 
		 l_venv' <- extendVenv True l_venv (v,t)
		 t:ts <- mapM (checkAlt (tcenv,tvenv,cenv,e_venv,l_venv') t) alts
		 require (all (== t) ts)
			 ("alternative types don't match in:\n" ++ show e0 ++ "\n" ++
			  "types: " ++ show (t:ts))
                 checkTy (tcenv,tvenv) resultTy
                 require (t == resultTy) ("case alternative type doesn't " ++
                   " match case return type in:\n" ++ show e0 ++ "\n" ++
                   "alt type: " ++ show t ++ " return type: " ++ show resultTy)
		 return t
	    c@(Cast e t) -> 
	      do eTy <- ch e 
		 (fromTy, toTy) <- checkTyCo (tcenv,tvenv) t
                 require (eTy == fromTy) ("Type mismatch in cast: c = "
                             ++ show c ++ "\nand eTy = " ++ show eTy
                             ++ "\n and " ++ show fromTy)
                 return toTy
	    Note _ e -> 
	      ch e
	    External _ t -> 
	      do checkTy (tcenv,eempty) t {- external types must be closed -}
		 return t
    
checkAlt :: (Tcenv,Tvenv,Cenv,Venv,Venv) -> Ty -> Alt -> CheckResult Ty
checkAlt (env@(tcenv,tvenv,cenv,e_venv,l_venv)) t0 = ch
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
		 require (all (uncurry eqKind)
                            (zip eks eks'))  
			 ("existential kinds don't match in:\n" ++ show a0 ++ "\n" ++
			  "kinds declared in data constructor: " ++ show eks ++
			  "kinds declared in case alternative: " ++ show eks') 
		 tvenv' <- foldM extendTvenv tvenv etbs
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
			require (ct_arg == vt)
			         ("pattern variable type doesn't match constructor argument type in:\n" ++ show a0 ++ "\n" ++
				  "pattern variable type: " ++ show ct_arg ++ "\n" ++
				  "constructor argument type: " ++ show vt)) ct_args vts
		 require (ct_res == t0)
			  ("pattern constructor type doesn't match scrutinee type in:\n" ++ show a0 ++ "\n" ++
			   "pattern constructor type: " ++ show ct_res ++ "\n" ++
			   "scrutinee type: " ++ show t0) 
		 l_venv' <- foldM (extendVenv True) l_venv vbs
		 t <- checkExp (tcenv,tvenv',cenv,e_venv,l_venv') e
		 checkTy (tcenv,tvenv) t  {- check that existentials don't escape in result type -}
		 return t
	    Alit l e ->
	      do t <- checkLit l
		 require (t == t0)
			 ("pattern type doesn't match scrutinee type in:\n" ++ show a0 ++ "\n" ++
			  "pattern type: " ++ show t ++ "\n" ++
			  "scrutinee type: " ++ show t0) 
		 checkExp env e
	    Adefault e ->
	      checkExp env e
    
checkTy :: (Tcenv,Tvenv) -> Ty -> CheckResult Kind
checkTy es@(tcenv,tvenv) = ch
     where
       ch (Tvar tv) = do
          res <- lookupM tvenv tv
          case res of
            Just k  -> return k
            Nothing -> fail ("Undefined tvar: " ++ show tv)
       ch (Tcon qtc) = do
         kOrC <- qlookupM tcenv_ tcenv eempty qtc
         case kOrC of
            Kind k -> return k
            Coercion (DefinedCoercion [] (t1,t2)) -> return $ Keq t1 t2
            Coercion _ -> fail ("Unsaturated coercion app: " ++ show qtc)
       ch (t@(Tapp t1 t2)) = 
             case splitTyConApp_maybe t of
               Just (tc, tys) -> do
                 tcK <- qlookupM tcenv_ tcenv eempty tc 
                 case tcK of
                   Kind _ -> checkTapp t1 t2
                   Coercion co@(DefinedCoercion tbs _) -> do
                     -- makes sure coercion is fully applied
                     require (length tys == length tbs) $
                        ("Arity mismatch in coercion app: " ++ show t)
                     let (_, tks) = unzip tbs
                     argKs <- mapM (checkTy es) tys
                     let kPairs = zip argKs tks
                         -- Simon says it's okay for these to be
                         -- subkinds
                     let kindsOk = all (uncurry subKindOf) kPairs
                     require kindsOk
                        ("Kind mismatch in coercion app: " ++ show tks 
                         ++ " and " ++ show argKs ++ " t = " ++ show t)
                     return $ (uncurry Keq) (applyNewtype co tys)
               Nothing -> checkTapp t1 t2
            where checkTapp t1 t2 = do 
                    k1 <- ch t1
	            k2 <- ch t2
	            case k1 of
		      Karrow k11 k12 -> do
                         require (k2 `subKindOf` k11) kindError
                         return k12
                            where kindError = 
                                    "kinds don't match in type application: "
                                    ++ show t ++ "\n" ++
			            "operator kind: " ++ show k11 ++ "\n" ++
			            "operand kind: " ++ show k2 
		      _ -> fail ("applied type has non-arrow kind: " ++ show t)
                           
       ch (Tforall tb t) = 
	    do tvenv' <- extendTvenv tvenv tb 
               checkTy (tcenv,tvenv') t
       ch (TransCoercion t1 t2) = do
            (ty1,ty2) <- checkTyCo es t1
            (ty3,ty4) <- checkTyCo es t2
            require (ty2 == ty3) ("Types don't match in trans. coercion: " ++
                        show ty2 ++ " and " ++ show ty3)
            return $ Keq ty1 ty4
       ch (SymCoercion t1) = do
            (ty1,ty2) <- checkTyCo es t1
            return $ Keq ty2 ty1
       ch (UnsafeCoercion t1 t2) = do
            checkTy es t1
            checkTy es t2
            return $ Keq t1 t2
       ch (LeftCoercion t1) = do
            k <- checkTyCo es t1
            case k of
              ((Tapp u _), (Tapp w _)) -> return $ Keq u w
              _ -> fail ("Bad coercion kind in operand of left: " ++ show k)
       ch (RightCoercion t1) = do
            k <- checkTyCo es t1
            case k of
              ((Tapp _ v), (Tapp _ x)) -> return $ Keq v x
              _ -> fail ("Bad coercion kind in operand of left: " ++ show k)
       ch (InstCoercion ty arg) = do
            forallK <- checkTyCo es ty
            case forallK of
              ((Tforall (v1,k1) b1), (Tforall (v2,k2) b2)) -> do
                 require (k1 `eqKind` k2) ("Kind mismatch in argument of inst: "
                                            ++ show ty)
                 argK <- checkTy es arg
                 require (argK `eqKind` k1) ("Kind mismatch in type being "
                           ++ "instantiated: " ++ show arg)
                 let newLhs = substl [v1] [arg] b1
                 let newRhs = substl [v2] [arg] b2
                 return $ Keq newLhs newRhs
              _ -> fail ("Non-forall-ty in argument to inst: " ++ show ty)

checkTyCo :: (Tcenv, Tvenv) -> Ty -> CheckResult (Ty, Ty)
checkTyCo es@(tcenv,_) t@(Tapp t1 t2) = 
  (case splitTyConApp_maybe t of
    Just (tc, tys) -> do
       tcK <- qlookupM tcenv_ tcenv eempty tc
       case tcK of
 -- todo: avoid duplicating this code
 -- blah, this almost calls for a different syntactic form
 -- (for a defined-coercion app): (TCoercionApp Tcon [Ty])
         Coercion co@(DefinedCoercion tbs _) -> do
           require (length tys == length tbs) $ 
            ("Arity mismatch in coercion app: " ++ show t)
           let (_, tks) = unzip tbs
           argKs <- mapM (checkTy es) tys
           let kPairs = zip argKs tks
           let kindsOk = all (uncurry subKindOf) kPairs
           require kindsOk
              ("Kind mismatch in coercion app: " ++ show tks 
                 ++ " and " ++ show argKs ++ " t = " ++ show t)
           return (applyNewtype co tys)
         _ -> checkTapp t1 t2
    _ -> checkTapp t1 t2)
       where checkTapp t1 t2 = do
               (lhsRator, rhsRator) <- checkTyCo es t1
               (lhs, rhs) <- checkTyCo es t2
               -- Comp rule from paper
               checkTy es (Tapp lhsRator lhs)
               checkTy es (Tapp rhsRator rhs)
               return (Tapp lhsRator lhs, Tapp rhsRator rhs)
checkTyCo (tcenv, tvenv) (Tforall tb t) = do
  tvenv' <- extendTvenv tvenv tb
  (t1,t2) <- checkTyCo (tcenv, tvenv') t
  return (Tforall tb t1, Tforall tb t2)
checkTyCo es t = do
  k <- checkTy es t
  case k of
    Keq t1 t2 -> return (t1, t2)
    -- otherwise, expand by the "refl" rule
    _          -> return (t, t)

mlookupM :: (Eq a, Show a, Show b) => (Envs -> Env a b) -> Env a b -> Env a b -> Mname
          -> CheckResult (Env a b)
mlookupM _ _ local_env    Nothing            = -- (trace ("mlookupM_: returning " ++ show local_env)) $
  return local_env
mlookupM selector external_env local_env (Just m) = do
  mn <- getMname
  globalEnv <- getGlobalEnv
  if m == mn
     then -- trace ("global env would b e " ++ show (elookup globalEnv m)) $
            return external_env
     else
       case elookup globalEnv m of
         Just env' -> return (selector env')
         Nothing -> fail ("Check: undefined module name: "
                      ++ show m ++ show (edomain local_env))

qlookupM :: (Ord a, Show a,Show b) => (Envs -> Env a b) -> Env a b -> Env a b
                  -> Qual a -> CheckResult b
qlookupM selector external_env local_env v@(m,k) =
      do env <- -- trace ("qlookupM: " ++ show v) $
                  mlookupM selector external_env local_env m
         -- argh, hack for unqualified top-level names
         maybeRes <- lookupM env k
         case maybeRes of
           Just r -> return r
           Nothing -> do mn <- getMname
                         currentMenv <- --  trace ("qlookupM: trying module for " ++ show mn) $
                                         mlookupM selector external_env local_env (Just mn)
                         maybeRes1 <- -- trace ("qlookupM: trying in " ++ show currentMenv) $
                                        lookupM currentMenv k
                         case maybeRes1 of
                           Just r1 -> return r1
                           Nothing -> do
                             globalEnv <- getGlobalEnv
                             case elookup globalEnv mn of
                               Just e1 -> case elookup (selector e1) k of
                                            Just r2 -> return r2
                                            Nothing -> fail ("Undefined id " ++ show v)
                               Nothing -> fail ("Undefined id " ++ show v) 

checkLit :: Lit -> CheckResult Ty
checkLit (Literal lit t) =
  case lit of
    Lint _ -> 
	  do require (t `elem` intLitTypes)
		     ("invalid int literal: " ++ show lit ++ "\n" ++ "type: " ++ show t)
	     return t
    Lrational _ ->
	  do require (t `elem` ratLitTypes)
		     ("invalid rational literal: " ++ show lit ++ "\n" ++ "type: " ++ show t)
	     return t
    Lchar _ -> 
	  do require (t `elem` charLitTypes)
		     ("invalid char literal: " ++ show lit ++ "\n" ++ "type: " ++ show t)
	     return t	
    Lstring _ ->
	  do require (t `elem` stringLitTypes)
		     ("invalid string literal: " ++ show lit ++ "\n" ++ "type: " ++ show t)
	     return t

{- Utilities -}

{- Split off tbs, arguments and result of a (possibly abstracted)  arrow type -}
splitTy :: Ty -> ([Tbind],[Ty],Ty)
splitTy (Tforall tb t) = (tb:tbs,ts,tr) 
		where (tbs,ts,tr) = splitTy t
splitTy (Tapp(Tapp(Tcon tc) t0) t) | tc == tcArrow = (tbs,t0:ts,tr)
		where (tbs,ts,tr) = splitTy t
splitTy t = ([],[],t)


primCoercionError :: Show a => a -> b
primCoercionError s = error $ "Bad coercion application: " ++ show s

-- todo
reportError :: Show a => a -> String -> b
reportError e s = error $ ("Core type error: checkExpr failed with "
                   ++ s ++ " and " ++ show e)

type NtEnv  = M.Map Tcon CoercionKind

mkNtEnv :: Menv -> NtEnv
mkNtEnv menv = 
  foldl M.union M.empty $
        map (\ (_,e) ->
                 foldr (\ (_,thing) rest ->
                            case thing of
                              Kind _ -> rest
                              Coercion d@(DefinedCoercion _ (lhs,_)) -> 
                                  case splitTyConApp_maybe lhs of
                                    Just ((_,tc1),_) -> M.insert tc1 d rest
                                    _ -> rest) M.empty (etolist (tcenv_ e))) (etolist menv)
