{- 
   This module eliminates unused top-level bindings, under the
   assumption that all top-level bindings with qualified names
   should be retained.
-}
module Language.Core.ElimDeadCode(elimDeadCode) where

import Language.Core.Core
import Language.Core.Printer()
import Language.Core.CoreUtils
import Language.Core.Utils

import Control.Monad.Reader
import Data.Generics
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

elimDeadCode :: Bool -> Module -> Module
-- exports = true <=> it's assumed we want to keep exported functions;
-- otherwise, we assume the module is "closed" and eliminate everything
-- not reachable from Main
elimDeadCode exports (Module mn tdefs vdefgs) = runReader (do
  (usedVars, usedDcons, usedTcons) <- findUsed emptySet 
     (mkStartSet exports mn vdefgs) 
  let isUsed (Vdef (v,_,_)) = v `S.member` usedVars
  let newVdefgs = filterVdefgs isUsed vdefgs
  let newTdefs  = filter (tdefIsUsed usedTcons usedDcons) tdefs in
    return $ Module mn newTdefs newVdefgs) ((mkVarEnv vdefgs), mkTyEnv tdefs)

tdefIsUsed :: S.Set (Qual Tcon) -> S.Set (Qual Dcon) -> Tdef -> Bool
tdefIsUsed tcs dcs (Data qtc _ cdefs) = 
  (qtc `S.member` tcs || any (\ (Constr qdc _ _) -> qdc `S.member` dcs) cdefs)
tdefIsUsed tcs _ (Newtype qtc qtc_co _ _) = 
  qtc `S.member` tcs || qtc_co `S.member` tcs

mkVarEnv :: [Vdefg] -> M.Map (Qual Var) Exp
mkVarEnv vgs =
  let vdefs = flattenBinds vgs in
    M.fromList [(v, e) | (Vdef (v, _, e)) <- vdefs]

-- if there is a Newtype qtc qtc_co ty,
-- generate: qtc |-> ty and qtc_co |-> ty
-- roughly the same for rhs's of Data decls
mkTyEnv :: [Tdef] -> M.Map (Qual Tcon) [Ty]
mkTyEnv tdefs = 
  M.fromList ([(qtc, [ty]) | (Newtype qtc _ _ ty) <- tdefs]
         ++   [(qtc, [ty]) | (Newtype _ qtc _ ty) <- tdefs]
         ++   concatMap (\ td -> case td of
                                   Data qtc _ cdefs -> [(qtc, concatMap 
                                     (\ (Constr _ _ ts) -> ts) cdefs)]
                                   _ -> []) tdefs)

findUsed :: DeadSet -> DeadSet -> DeadM DeadSet
findUsed _old@(oldVars,oldDcs,oldTcs) _new@(newVars,newDcs,newTcs) = do
  let (todoVars, todoTcs) = ((S.\\) newVars oldVars, (S.\\) newTcs oldTcs)
  let nextOld = (oldVars `S.union` todoVars, oldDcs `S.union` newDcs,
                 oldTcs `S.union` todoTcs)
  nextStuff <- getVarsAndConsIn (todoVars, todoTcs)
  if (S.null todoVars && S.null todoTcs)
      then return nextOld
      else findUsed nextOld nextStuff

getVarsAndConsIn :: (S.Set (Qual Var), S.Set (Qual Tcon)) -> DeadM DeadSet
getVarsAndConsIn (vs, tcs) = do
  vs1 <- mapM varsAndConsInOne (S.toList vs)
  ts1 <- mapM varsAndConsInOne' (S.toList tcs)
  let (vs'::[S.Set (Qual Var)], dcs'::[S.Set (Qual Dcon)],
        ts'::[S.Set (Qual Tcon)]) = unzip3 (vs1 ++ ts1)
  return (foldl' S.union S.empty vs', foldl' S.union S.empty dcs',
          foldl' S.union S.empty ts')

varsAndConsInOne :: Qual Var -> DeadM DeadSet
varsAndConsInOne vr = do
  def <- findDefn vr
  return $ maybe emptySet 
    (noNames emptySet unionThree (mkQ emptySet usedNamesAll)) def

varsAndConsInOne' :: Qual Tcon -> DeadM DeadSet
varsAndConsInOne' tc = do
  ty <- findRepTy tc
  return $ maybe emptySet
    (noNames emptySet unionThree 
              (mkQ emptySet usedStuffTys)) ty

emptySet :: DeadSet
emptySet = (S.empty, S.empty, S.empty)
mkStartSet :: Bool -> AnMname -> [Vdefg] -> DeadSet
-- Initially, we assume the definitions of any exported functions are not
-- dead, and work backwards from there.
mkStartSet exports mn vds = 
  (S.fromList (filter ((== Just mn) . getModule) (if exports then exportedNames vds else [mainVar])), 
   S.empty, S.empty)

exportedNames :: [Vdefg] -> [Qual Var]
exportedNames vdefgs = 
  let vds = flattenBinds vdefgs in
    filter isQual (ns vds)
      where isQual    = isJust . fst
            ns = map (\ (Vdef (n,_,_)) -> n)

type DeadSet = (S.Set (Qual Var), S.Set (Qual Dcon), S.Set (Qual Tcon))
type DeadM = Reader (M.Map (Qual Var) Exp, M.Map (Qual Tcon) [Ty])

findDefn :: Qual Var -> DeadM (Maybe Exp)
findDefn vr = asks ((M.lookup vr) . fst)
findRepTy :: Qual Tcon -> DeadM (Maybe [Ty])
findRepTy tc = asks ((M.lookup tc) . snd)

unionThree :: DeadSet -> DeadSet -> DeadSet
unionThree (a,b,c) (d,e,f) = (a `S.union` d, b `S.union` e, c `S.union` f)

usedNamesAll :: Exp -> DeadSet
usedNamesAll = (noNames emptySet unionThree 
  ((mkQ emptySet usedStuff) `extQ` usedStuffTys `extQ` usedStuffAlts))
            
usedStuff :: Exp -> DeadSet
usedStuff (Var qv)  = (S.singleton qv, S.empty, S.empty)
usedStuff (Dcon dc) = (S.empty, S.singleton dc, S.empty)
usedStuff _         = emptySet

usedStuffAlts :: Alt -> DeadSet
usedStuffAlts (Acon qdc _ _ _) = (S.empty, S.singleton qdc, S.empty)
usedStuffAlts _ = emptySet

usedStuffTys :: Ty -> DeadSet
usedStuffTys (Tcon qtc) = (S.empty, S.empty, S.singleton qtc)
usedStuffTys _          = emptySet
