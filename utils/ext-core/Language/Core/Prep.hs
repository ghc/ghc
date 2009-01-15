{-# OPTIONS -fno-warn-name-shadowing #-}
{-
Preprocess a module to normalize it in the following ways:
	(1) Saturate all constructor and primop applications. 
              (as well as external calls; this is probably already
               guaranteed, but paranoia is good)
	(2) Arrange that any non-trivial expression of unlifted kind ('#')
             is turned into the scrutinee of a Case.
After these preprocessing steps, Core can be interpreted (or given an operational semantics)
      ignoring type information almost completely.
-}


module Language.Core.Prep where

--import Debug.Trace

import Control.Monad.State
import Data.Either
import Data.List
import Data.Generics
import qualified Data.Map as M

import Language.Core.Core
import Language.Core.CoreUtils
import Language.Core.Env
import Language.Core.Check
import Language.Core.Environments
import Language.Core.Utils

prepModule :: Menv -> Module -> Module
prepModule globalEnv (Module mn tdefs vdefgs) = 
    Module mn tdefs (snd (evalState 
      (foldM prepTopVdefg (eempty,[]) vdefgs) initCounter))
  where
    (tcenv, cenv) = mkTypeEnvsNoChecking tdefs

    prepTopVdefg :: (Venv, [Vdefg]) -> Vdefg -> PrepM (Venv, [Vdefg])
    prepTopVdefg (venv,vdefgs) vdefg = do
         (venv',vdefg') <- prepVdefg (venv,eempty) vdefg
         return (venv',vdefgs ++ [vdefg'])
 
    prepVdefg (env@(venv,_)) (Nonrec(Vdef((Nothing,x),t,e))) = do
        e' <- prepExp env e
	return (eextend venv (x,t), Nonrec(Vdef((Nothing,x),t,e')))
    prepVdefg (env@(venv,_))  (Nonrec(Vdef(qx,t,e))) = do
        e' <- prepExp env e
     	return (venv, Nonrec(Vdef(qx,t,e')))
    prepVdefg (venv,tvenv) (Rec vdefs) = do
        vds' <- mapM (\ (Vdef (qx,t,e)) -> do
                         e' <- prepExp (venv',tvenv) e
                         return (Vdef (qx,t,e'))) vdefs
	return (venv', Rec vds')
	where venv' = foldl' eextend venv [(x,t) | Vdef((Nothing,x),t,_) <- vdefs]

    prepExp :: (Venv, Tvenv) -> Exp -> PrepM Exp
    prepExp _ (Var qv) = return $ Var qv
    prepExp _ (Dcon qdc) = return $ Dcon qdc
    prepExp _ (Lit l) = return $ Lit l
    prepExp env e@(App _ _) = unwindApp env e []
    prepExp env e@(Appt _ _) = unwindApp env e []
    prepExp (venv,tvenv) (Lam (Vb vb) e) = do
       e' <- prepExp (eextend venv vb,tvenv) e             
       return $ Lam (Vb vb) e' 
    prepExp (venv,tvenv) (Lam (Tb tb) e) = do
       e' <- prepExp (venv,eextend tvenv tb) e
       return $ Lam (Tb tb) e' 
    prepExp env@(venv,tvenv) (Let (Nonrec(Vdef((Nothing,x),t,b))) e) 
        | (kindOfTy tvenv t `eqKind` Kunlifted && suspends b) = do 
            -- There are two places where we call the typechecker, one of them
            -- here.
            -- We need to know the type of the let body in order to construct
            -- a case expression. 
                                -- need to extend the env with the let-bound var too!
            scrut' <- prepExp env b
            rhs' <- prepExp (eextend venv (x,t),tvenv) e
            return $
              let eTy = typeOfExp (eextend venv (x, t)) tvenv e in
                Case scrut' (x,t) eTy [Adefault rhs'] 
    prepExp (venv,tvenv) (Let vdefg e) =  do
      (venv',vdefg') <- prepVdefg (venv,tvenv) vdefg
      rhs' <- prepExp (venv',tvenv) e
      return $ Let vdefg' rhs'
    prepExp env@(venv,tvenv) (Case e vb t alts) = do
      e' <- prepExp env e
      alts' <- mapM (prepAlt (eextend venv vb,tvenv)) alts
      return $ Case e' vb t alts'
    prepExp env (Cast e t) = do
      e' <- prepExp env e
      return $ Cast e' t
    prepExp env (Note s e) = do
      e' <- prepExp env e
      return $ Note s e'
    prepExp _ (External s t) = return $ External s t

    prepAlt :: (Venv,Tvenv) -> Alt -> PrepM Alt
    prepAlt (venv,tvenv) (Acon qdc tbs vbs e) = do
      rhs' <- prepExp (foldl' eextend venv vbs,foldl' eextend tvenv tbs) e
      return $ Acon qdc tbs vbs rhs'
    prepAlt env (Alit l e) = (liftM (Alit l)) (prepExp env e)
    prepAlt env (Adefault e) = (liftM Adefault) (prepExp env e)

    unwindApp :: (Venv, Tvenv) -> Exp -> [Either Exp Ty] -> PrepM Exp
    unwindApp env (App e1 e2) as = unwindApp env e1 (Left e2:as)
    unwindApp env (Appt e t) as  = unwindApp env e (Right t:as)
    unwindApp env (op@(Dcon qdc)) as = do
        e' <- rewindApp env op as
        -- possibly dubious to assume no type args
        etaExpand [] (drop n atys) e'
        where (tbs,atys0,_) = splitTy (qlookup cenv_ eempty qdc)
	      atys = map (substl (map fst tbs) ts) atys0
	      ts = [t | Right t <- as]
              n = length [e | Left e <- as]
    unwindApp env (op@(Var qv)) as | isPrimVar qv = do
        e' <- rewindApp env op as
        etaExpand [] [] e'
    unwindApp env (op@(External _ t)) as = do
        e' <- rewindApp env op as
        etaExpand [] (drop n atys) e'
          where (_,atys,_) = splitTy t
                n = length as -- assumes all args are term args
    unwindApp env op as = rewindApp env op as


    etaExpand :: [Kind] -> [Ty] -> Exp -> PrepM Exp
    etaExpand ks ts e = do
         -- what a pain
         tyvs <- replicateM (length ks) freshVar
         termvs <- replicateM (length ts) freshVar
         let tyArgs   = zip tyvs ks
         let termArgs = zip termvs ts
         return $
          foldr (\ (t1,k1) e -> Lam (Tb (t1,k1)) e)
	   (foldr (\ (v,t) e -> Lam (Vb (v,t)) e)
              (foldl' (\ e (v,_) -> App e (Var (unqual v)))
                 (foldl' (\ e (tv,_) -> Appt e (Tvar tv))
                   e tyArgs)
              termArgs) termArgs)
           tyArgs

    rewindApp :: (Venv, Tvenv) -> Exp -> [Either Exp Ty] -> PrepM Exp
    rewindApp _ e [] = return e
    rewindApp env@(venv,tvenv) e1 (Left e2:as) | kindOfTy tvenv t `eqKind` Kunlifted && suspends e2 = do
        v <- freshVar
        let venv' = eextend venv (v,t)
        rhs <- rewindApp (venv', tvenv) (App e1 (Var (unqual v))) as
        newScrut <- prepExp env e2
       -- This is the other place where we call the typechecker.
        return $ Case newScrut (v,t) (typeOfExp venv' tvenv rhs) [Adefault rhs]
        where t = typeOfExp venv tvenv e2
    rewindApp env e1 (Left e2:as) = do
      e2' <- prepExp env e2
      rewindApp env (App e1 e2') as
    rewindApp env e (Right t:as) = rewindApp env (Appt e t) as

    typeOfExp :: Venv -> Tvenv -> Exp -> Ty
    typeOfExp = checkExpr mn globalEnv tcenv cenv

    kindOfTy :: Tvenv -> Ty -> Kind
    kindOfTy tvenv = checkType mn globalEnv tcenv tvenv

    {- Return false for those expressions for which Interp.suspendExp builds a thunk. -}
    suspends (Var _) = False
    suspends (Lit _) = False
    suspends (Lam (Vb _) _) = False
    suspends (Lam _ e) = suspends e
    suspends (Appt e _) = suspends e
    suspends (Cast e _) = suspends e
    suspends (Note _ e) = suspends e
    suspends (External _ _) = False
    suspends _ = True

    mlookup :: (Envs -> Env a b) -> Env a b -> Mname -> Env a b
    mlookup _ local_env Nothing = local_env
    mlookup selector _  (Just m) =   
      case elookup globalEnv m of
        Just env -> selector env
        Nothing -> error ("Prep: undefined module name: " ++ show m)

    qlookup ::  (Ord a, Show a) => (Envs -> Env a b) -> Env a b -> (Mname,a) -> b
    qlookup selector local_env (m,k) =   
      case elookup (mlookup selector local_env m) k of
        Just v -> v
        Nothing -> error ("undefined identifier: " ++ show k)

boundVars :: Exp -> [Id]
boundVars (Lam (Vb (v,_)) e) = [v] `union` boundVars e
boundVars (Lam _ e) = boundVars e
boundVars (Let vds e) = (boundVarsVdefs vds) `union` boundVars e
boundVars (Case scrut (v,_) _ alts) = 
   [v] `union` (boundVars scrut) `union` boundVarsAlts alts
boundVars (Cast e _) = boundVars e
boundVars (Note _ e) = boundVars e
boundVars (App e1 e2) = boundVars e1 `union` boundVars e2
boundVars (Appt e _) = boundVars e
boundVars _ = []

boundVarsVdefs :: Vdefg -> [Id]
boundVarsVdefs (Rec vds) = nub (concatMap boundVarsVdef vds)
boundVarsVdefs (Nonrec vd) = boundVarsVdef vd

boundVarsVdef :: Vdef -> [Id]
boundVarsVdef (Vdef ((_,v),_,e)) = [v] `union` boundVars e

boundVarsAlts :: [Alt] -> [Var]
boundVarsAlts as = nub (concatMap boundVarsAlt as)

boundVarsAlt :: Alt -> [Var]
boundVarsAlt (Acon _ _ vbs e) = (map fst vbs) `union` (boundVars e)
boundVarsAlt (Alit _ e) = boundVars e
boundVarsAlt (Adefault e) = boundVars e

substNewtys :: NtEnv -> Ty -> Ty
substNewtys ntEnv = everywhere'Except (mkT go)
                 where go t | Just ((_,tc),args) <- splitTyConApp_maybe t =
                         case M.lookup tc ntEnv of
                           Just d -> -- trace ("applying newtype: " ++ show t) $
                                       (snd (applyNewtype d args))
                           Nothing  -> t
                       go t = t

newtypeCoercion_maybe :: NtEnv -> Ty -> Maybe CoercionKind
newtypeCoercion_maybe ntEnv t | Just ((_,tc),_) <- splitTyConApp_maybe t =
  M.lookup tc ntEnv
newtypeCoercion_maybe _ _ = Nothing

mkTapp :: Ty -> [Ty] -> Ty
mkTapp = foldl Tapp

initCounter :: Int
initCounter = 0

type PrepM = State Int

freshVar :: PrepM String
freshVar = do
  i <- get
  put (i+1)
  return $ ("zd" ++ show i)
