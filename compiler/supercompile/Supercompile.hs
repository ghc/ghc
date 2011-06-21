module Supercompile (supercompileProgram) where

import Supercompile.Utilities
import qualified Supercompile.Core.Syntax as S
import qualified Supercompile.Evaluator.Syntax as S
import qualified Supercompile.Drive.Process as S

import CoreSyn
import CoreUtils  (exprType)
import DataCon    (DataCon, dataConWorkId, dataConName)
import Var        (isTyVar)
import Id         (mkSysLocalM, idType)
import MkId       (mkPrimOpId)
import MkCore     (mkBigCoreVarTup, mkTupleSelector, mkWildValBinder)
import FastString (mkFastString)
import PrimOp     (PrimOp, primOpType)
import Type       (Type)
import DynFlags   (DynFlags)

import Control.Monad
import qualified Data.Map as M


-- | Descriptions of terms: used for building readable names for ANF-introduced variables
data Description = Opaque String | ArgumentOf Description

descriptionString :: Description -> String
descriptionString = go (0 :: Int)
  where
    go n (Opaque s)     = s ++ (if n > 0 then show n else "")
    go n (ArgumentOf d) = go (n + 1) d

desc :: S.Term -> Description
desc = desc' . unI

desc' :: S.TermF Identity -> Description
desc' (S.Var x)         = Opaque (S.varString x)
desc' (S.Value _)       = Opaque "value"
desc' (S.App e1 _)      = argOf (desc e1)
desc' (S.TyApp e1 _)    = argOf (desc e1)
desc' (S.PrimOp pop es) = foldr (\_ d -> argOf d) (Opaque (show pop)) es
desc' (S.Case _ _ _ _)  = Opaque "case"
desc' (S.Cast _ _)      = Opaque "cast"
desc' (S.Let _ _ e)     = desc e
desc' (S.LetRec _ e)    = desc e

argOf :: Description -> Description
argOf = ArgumentOf


data ParseState = ParseState {
    uniqSupply :: UniqSupply,
    dcWrappers :: M.Map DataCon S.Var,
    primWrappers :: M.Map PrimOp S.Var
  }

initParseState :: ParseState
initParseState = ParseState {
    uniqSupply = anfUniqSupply,
    dcWrappers = M.empty,
    primWrappers = M.empty
  }

newtype ParseM a = ParseM { unParseM :: ParseState -> (ParseState, [(S.Var, S.Term)], a) }

instance Functor ParseM where
    fmap = liftM

instance Monad ParseM where
    return x = ParseM $ \s -> (s, [], x)
    mx >>= fxmy = ParseM $ \s -> case unParseM mx s of (s, floats1, x) -> case unParseM (fxmy x) s of (s, floats2, y) -> (s, floats1 ++ floats2, y)

instance MonadUnique ParseM where
    getUniqueSupplyM = ParseM $ \s -> case splitUniqSupply (uniqSupply s) of (us1, us2) -> (s { uniqSupply = us1 }, [], us2)

runParseM :: ParseM a -> ([(S.Var, S.Term)], a)
runParseM act = (buildWrappers s ++ floats, x)
  where
    (s, floats, x) = unParseM act initParseState

-- dataConWrapper :: DataCon -> ParseM S.Var
-- dataConWrapper = grabWrapper dcWrappers (\s x -> s { dcWrappers = x }) (S.nameString . dataConName) (idType . dataConWorkId)
-- 
-- primWrapper :: PrimOp -> ParseM S.Var
-- primWrapper = grabWrapper primWrappers (\s x -> s { primWrappers = x }) show primOpType
-- 
-- grabWrapper :: Ord a
--             => (ParseState -> M.Map a S.Var) -> (ParseState -> M.Map a S.Var -> ParseState)
--             -> (a -> String) -- For building human readable names for the wrapper invocations only
--             -> (a -> Type)
--             -> a -> ParseM S.Var
-- grabWrapper get set describe typ what = do
--     mb_x <- ParseM $ \s -> (s, [], M.lookup what (get s))
--     case mb_x of Just x -> return x
--                  Nothing -> mkSysLocalM (mkFastString ("w" ++ describe what)) (typ what) >>= \x -> ParseM $ \s -> (set s (M.insert what x (get s)), [], x)

buildWrappers :: ParseState -> [(S.Var, S.Term)]
buildWrappers _ = []
-- buildWrappers s = [(x, ) | (dc,  x) <- M.toList (dcWrappers s)] ++
--                   [(x, ) | (pop, x) <- M.toList (primWrappers s)]

freshFloatVar :: String -> S.Term -> ParseM (Maybe (S.Var, S.Term), S.Var)
freshFloatVar _ (I (S.Var x)) = return (Nothing, x)
freshFloatVar n e             = fmap (\x -> (Just (x, e), x)) $ mkSysLocalM (mkFastString n) (S.termType e)

floatIt :: [(S.Var, S.Term)] -> ParseM ()
floatIt floats = ParseM $ \s -> (s, floats, ())

nameIt :: Description -> S.Term -> ParseM S.Var
nameIt d e = freshFloatVar ("a" ++ descriptionString d) e >>= \(mb_float, x) -> floatIt (maybeToList mb_float) >> return x

-- bindFloats :: ParseM S.Term -> ParseM S.Term
-- bindFloats = bindFloatsWith . fmap ((,) [])
-- 
-- bindFloatsWith :: ParseM ([(S.Var, S.Term)], S.Term) -> ParseM S.Term
-- bindFloatsWith act = ParseM $ \s -> case unParseM act s of (s, floats, (xes, e)) -> (s, [], S.letRecSmart (xes ++ floats) e)

appE :: S.Term -> S.Term -> ParseM S.Term
appE e1 e2 = nameIt (argOf (desc e1)) e2 >>= \x2 -> return (e1 `S.app` x2)



coreExprToTerm :: CoreExpr -> S.Term
coreExprToTerm = uncurry S.letRecSmart . runParseM . term
  where
    term (Var x) -- | Just pop <- isPrimOpId_maybe x     = primWrapper pop
                 -- | Just dc <- isDataConWorkId_maybe x = dataConWrapper dc
                 | otherwise                          = return $ S.var x
    term (Lit l)                   = return $ S.value (S.Literal l)
    term (App e_fun (Type ty_arg)) = fmap (flip S.tyApp ty_arg) (term e_fun)
    term (App e_fun e_arg)         = join $ liftM2 appE (term e_fun) (term e_arg)
    term (Lam x e) | isTyVar x     = fmap (S.value . S.TyLambda x) (term e)
                   | otherwise     = fmap (S.value . S.Lambda x) (term e)
    term (Let (NonRec x e1) e2)    = liftM2 (S.let_ x) (term e1) (term e2)
    term (Let (Rec xes) e)         = liftM2 S.letRecSmart (mapM (secondM term) xes) (term e)
    term (Case e x ty alts)        = liftM2 (\e alts -> S.case_ e x ty alts) (term e) (mapM alt alts)
    term (Cast e co)               = fmap (flip S.cast co) (term e)
    term (Note _ e)                = term e -- FIXME: record notes
    term (Type ty)                 = pprPanic "termToCoreExpr" (ppr ty)
    term (Coercion co)             = return $ S.value (S.Coercion co)
    
    alt (DEFAULT,    [], e) = fmap ((,) S.DefaultAlt)      $ term e
    alt (LitAlt l,   [], e) = fmap ((,) (S.LiteralAlt l))  $ term e
    alt (DataAlt dc, xs, e) = fmap ((,) (S.DataAlt dc xs)) $ term e
    alt it                  = pprPanic "termToCoreExpr" (ppr it)

termToCoreExpr :: S.Term -> CoreExpr
termToCoreExpr = term
  where
    term e = case unI e of
        S.Var x            -> Var x
        S.Value v          -> value v
        S.App e x          -> term e `App` Var x
        S.TyApp e ty       -> term e `App` Type ty
        S.PrimOp pop es    -> Var (mkPrimOpId pop) `mkApps` map term es
        S.Case e x ty alts -> Case (term e) x ty (map alt alts)
        S.Let x e1 e2      -> Let (NonRec x (term e1)) (term e2)
        S.LetRec xes e     -> Let (Rec (map (second term) xes)) (term e)
        S.Cast e co        -> Cast (term e) co
    
    value (S.Indirect x)     = Var x
    value (S.Literal l)      = Lit l
    value (S.Coercion co)    = Coercion co
    value (S.TyLambda a e)   = Lam a (term e)
    value (S.Lambda x e)     = Lam x (term e)
    value (S.Data dc tys xs) = (Var (dataConWorkId dc) `mkTyApps` tys) `mkVarApps` xs
    
    alt (S.DataAlt dc xs, e) = (DataAlt dc, xs, term e)
    alt (S.LiteralAlt l,  e) = (LitAlt l,   [], term e)
    alt (S.DefaultAlt,    e) = (DEFAULT,    [], term e)

coreBindsToCoreTerm :: [CoreBind] -> (CoreExpr, CoreExpr -> [CoreBind])
coreBindsToCoreTerm binds
  = (mkLets binds (mkBigCoreVarTup xs),
     \e -> let wild_id = mkWildValBinder (exprType e) in [NonRec x (mkTupleSelector xs x wild_id e) | x <- xs])
  where xs = bindersOfBinds binds

supercompile :: CoreExpr -> CoreExpr
supercompile = termToCoreExpr . snd . S.supercompile . coreExprToTerm

supercompileProgram :: DynFlags -> [CoreBind] -> [CoreBind]
supercompileProgram _dflags binds = rebuild (supercompile e)
  where (e, rebuild) = coreBindsToCoreTerm binds
