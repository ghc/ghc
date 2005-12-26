{-# OPTIONS -fglasgow-exts #-}
-- Generates soundness tests with respect to a reference implementation, mostly.
-- (Some glue must still be hand-coded.)

-- Quickly hacked: uglyness lies below.
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import System.Environment
import System.IO
import Data.Char
import Data.List
import Control.Monad.State
import Control.Monad.Writer


hsType name args = foldl HsTyApp (HsTyCon (UnQual name)) (map HsTyVar args)

hsTyId :: HsType
hsTyId = (HsTyCon (UnQual (HsIdent "Id")))

hsTyApp (HsTyCon (UnQual (HsIdent "Id"))) t = t
hsTyApp t u = HsTyApp t u

hsApp e1 e2 = HsApp e1 (hsParen e2)

hsParen (HsApp x y) = HsParen $ HsApp x y
hsParen e = e

hsName = UnQual . HsIdent

hsOp = HsQVarOp . hsName

hsVar' name = (HsVar (UnQual name))

hsVar = hsVar' . HsIdent

-- In the future we might want a 'real' unfication. 
-- Now it's lame stuff that works only for the input we feed it with.
unify (HsTyApp t1 u1) (HsTyApp t2 u2) = unify t1 t2  && unify u1 u2
unify (HsTyCon name1) (HsTyCon name2) = name1 == name2
unify (HsTyVar v1) (HsTyVar v2) = True
unify _ _ = False

hsId = hsVar "id"

hsMap = hsApp (hsVar "fmap")

postfixName (HsIdent s) p = (HsIdent (s++p))
prefixName  (HsIdent s) p = (HsIdent (p++s))

mkTester invalids loc pivot (HsQualType context typ0) (HsIdent name)
    = if invalid then [] else [HsTypeSig loc [propName] (HsQualType [] propType),
                               HsFunBind [HsMatch loc propName points (HsUnGuardedRhs rhs) []]]
        where rhs = hsVar "(==)" `hsApp` l `hsApp` r
              (l, vars)  = mkTesterSide loc "lIn" "lOut" pivot typ0 (HsIdent ("L."++ name))
              (r, vars') = mkTesterSide loc "rIn" "rOut" pivot typ0 (HsIdent ("R."++ name))
              points = map HsPVar $ map fst vars
              types = [hsType (HsIdent (map toUpper $ prettyPrint t)) [] | t <- map snd vars]
              propType = foldr HsTyFun (HsTyCon $ hsName "Bool") types
              propName = HsIdent ("prop_"++name)
              invalid = name `elem` invalids
              

type Allocater = StateT Int (Writer [(HsName, HsType)])

runAllocater :: Allocater a -> (a,[(HsName, HsType)])
runAllocater m = let ((a,s),w) = runWriter (runStateT m 0) in (a,w)

mkTesterSide :: SrcLoc -> String -> String -> HsType -> HsType -> HsName -> (HsExp, [(HsName, HsType)])
mkTesterSide loc cvtIn cvtOut  pivot typ0 name = runAllocater $ reduce (hsVar' name) typ0
    where reduce exp (HsTyFun l r) = do l' <- expand [hsTyId] (hsVar cvtIn) l
                                        reduce (hsApp exp l') r
          reduce exp t = expand' exp t

          expand' exp t | unify t pivot = return $ (hsVar cvtOut) `hsApp` exp 
          expand' exp (HsTyTuple ts) = do vs <- mapM (allocate [] False) ts
                                          let pat = HsPTuple $ map HsPVar vs
                                          vals <- sequence [expand' (hsVar' v) t | v <- vs | t <- ts]
                                          return $ HsParen $ HsLet 
                                                     [HsPatBind loc pat (HsUnGuardedRhs exp) []] (HsTuple vals)
          expand' exp t = return exp -- error $ "got: " ++ prettyPrint t

          expand fctr exp t | unify t pivot 
                           = return ((exp `hsApp`) . hsVar') `ap` allocate fctr True hsPivotType
          expand fctr exp t@(HsTyVar n) = return hsVar' `ap` allocate fctr True t
          expand fctr exp (HsTyCon n) = return $ hsVar (prettyPrint n)
          expand fctr exp (HsTyApp (HsTyCon n) t) = expand (HsTyCon n:fctr) (hsMap exp) t -- pray it's a functor :)
                                               --FIX: types of the inside allocated variables shall be adjusted.
          expand fctr exp t = return $ hsVar $ toFunName $ prettyPrint t

          allocate :: [HsType] -> Bool -> HsType -> Allocater HsName
          allocate fctr free typ = 
              do modify (+1)
                 idx <- get
                 let name = HsIdent $ toFunName $ prettyPrint typ
                     name' = postfixName name (show idx)
                 when free $ tell [(name', foldr1 (flip hsTyApp) (typ:fctr))]
                 return name'

          hsPivotType = hsType (HsIdent "REFCOLL") []

toFunName = map toLower . map head . group . map toUsc

toUsc c | isAlphaNum c = c
        | otherwise = '_'

processModule (HsModule loc moduleName exports@Nothing imports decls) invalids = result
    where (pivotType:_) = [hsType name args | HsTypeDecl srcloc name args def <- decls]
          testers = concat [concatMap (mkTester invalids loc pivotType qualType) names 
                                | HsTypeSig loc names qualType <- decls]
          result = map prettyPrint testers

delimiter = "-- !!! EVERYTHING BELOW THIS LINE WILL BE DELETED !!! --"

main = do [sourceName, targetName,invalids] <- getArgs
          sourceContents <- readFile sourceName
          targetContents <- readFile targetName
          print $ length targetContents -- lame way to force reading the whole file
          let ParseOk fileST = parseModuleWithMode (ParseMode sourceName) sourceContents
              result = processModule fileST (read invalids)
          writeFile targetName $ unlines $ takeWhile (/= delimiter) (lines targetContents) ++ delimiter : result
