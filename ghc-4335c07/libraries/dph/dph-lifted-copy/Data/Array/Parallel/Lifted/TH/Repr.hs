{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Data.Array.Parallel.Lifted.TH.Repr (
  scalarInstances, tupleInstances,
  voidPRInstance, unitPRInstance, wrapPRInstance
) where

import qualified Data.Array.Parallel.Unlifted   as U
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Base.DTrace          (traceFn)

import Language.Haskell.TH
import Data.List                                (intercalate)

tyBndrVar :: TyVarBndr -> Name
tyBndrVar (PlainTV  n)          = n
tyBndrVar (KindedTV n _)        = n

mkAppTs :: Type -> [Type] -> Type
mkAppTs = foldl AppT

varTs :: [Name] -> [TypeQ]
varTs = map varT

appTs :: TypeQ -> [TypeQ] -> TypeQ
appTs = foldl appT

varEs :: [Name] -> [ExpQ]
varEs = map varE

appEs :: ExpQ -> [ExpQ] -> ExpQ
appEs = foldl appE

normalMatch :: PatQ -> ExpQ -> MatchQ
normalMatch pat xx = match pat (normalB xx) []

varPs :: [Name] -> [PatQ]
varPs = map varP

vanillaC :: Name -> [TypeQ] -> ConQ
vanillaC con tys = normalC con (map (strictType notStrict) tys)


simpleFunD :: Name -> [PatQ] -> ExpQ -> DecQ
simpleFunD name pats xx
  = funD name [clause pats (normalB xx) []]


inlineD :: Name -> DecQ
inlineD name = pragInlD name Inline FunLike AllPhases


instance_PData :: TypeQ -> [Name] -> Name -> [TypeQ] -> DecQ
instance_PData tycon tyargs con tys
  = dataInstD (cxt []) ''PData [tycon `appTs` varTs tyargs]
                               [vanillaC con tys]
                               []


newtype_instance_PData :: Name -> [Name] -> Name -> TypeQ -> DecQ
newtype_instance_PData tycon tyargs con ty
  = newtypeInstD (cxt []) ''PData [conT tycon `appTs` varTs tyargs]
                                  (vanillaC con [ty])
                                  []


splitConAppTy :: Type -> Maybe (Type, [Type])
splitConAppTy ty = collect ty []
  where
    collect (ConT tycon)  args = Just (ConT tycon, args)
    collect (TupleT n)    args = Just (TupleT n,   args)
    collect ListT         args = Just (ListT,      args)
    collect ArrowT        args = Just (ArrowT,     args)
    collect (AppT t arg)  args = collect t (arg:args)
    collect _ _ = Nothing


normaliseTy :: Type -> Q Type
normaliseTy ty
  = case splitConAppTy ty of
      Just (ConT tycon, args)
        -> do
             info <- reify tycon
             case info of
               TyConI (TySynD _ bndrs t)
                 -> return $ substTy (zip (map tyBndrVar bndrs) args) t
               _ -> return ty
      _ -> return ty


substTy :: [(Name, Type)] -> Type -> Type
substTy _ (ForallT _ _ _)
        = error "DPH gen: can't substitute in forall ty"

substTy env (VarT v)    = case lookup v env of
                             Just ty -> ty
                             Nothing -> VarT v
substTy env (AppT t u)  = AppT (substTy env t) (substTy env u)
substTy env (SigT t k)  = SigT (substTy env t) k
substTy _   t           = t


splitFunTy :: Type -> ([Type], Type)
splitFunTy ty = case splitConAppTy ty of
                  Just (ArrowT, [arg, r]) -> let (args, res) = splitFunTy r
                                             in (arg:args, res)
                  _ -> ([], ty)

data Val = ScalarVal
         | PDataVal
         | ListVal
         | UnitVal
         | OtherVal
type NameGen = String -> String
type ArgVal = (Val, NameGen)

genPR_methods :: (Name -> [ArgVal] -> Val -> DecQ) -> Q [Dec]
genPR_methods mk_method
  = do
      ClassI (ClassD _ _ _ _ decs) _ <- reify ''PR
      inls <- sequence [inlineD $ mkName $ nameBase name | SigD name _ <- decs]
      defs <- mapM gen [(name, ty) | SigD name ty <- decs]
      return $ inls ++ defs
  where
    gen (name, ty)
      = case lookup name nameGens of
          Just gs -> do
                       (args, res) <- methodVals ty
                       mk_method name (zip args gs) res
          Nothing -> error $ "DPH gen: no name generator for " ++ show name


methodVals :: Type -> Q ([Val], Val)
methodVals (ForallT (PlainTV vv : _) _ ty)
  = do
      ty'               <- normaliseTy ty
      let (args, res)   = splitFunTy ty'

      return (map (val vv) args, val vv res)
  where
    val v (VarT n) | v == n             = ScalarVal

    val v (AppT (ConT c) (VarT n))
        | c == ''PData && v == n        = PDataVal
        | c == ''[]    && v == n        = ListVal

    val v (AppT ListT (VarT n)) | v==n  = ListVal
    val _ (ConT c) | c == ''()          = UnitVal
    val _ (TupleT 0)                    = UnitVal
    val _ _                             = OtherVal

methodVals tt
        = error $ "DPH gen: methodVals: no match for " ++ show tt


data Split = PatSplit  PatQ
           | CaseSplit PatQ ExpQ PatQ

data Arg = RecArg   [ExpQ] [ExpQ]
         | OtherArg ExpQ

data Gen = Gen {
             recursiveCalls :: Int
           , recursiveName  :: Name -> Name
           , split          :: ArgVal -> (Split, Arg)
           , joinG          :: Val -> [ExpQ] -> ExpQ
           , typeName       :: String
           }

recursiveMethod :: Gen -> Name -> [ArgVal] -> Val -> DecQ
recursiveMethod gen name avs res
  = simpleFunD (mkName $ nameBase name) (map pat splits)
  $ appE (varE 'traceFn `appEs` [stringE (nameBase name), stringE (typeName gen)])
  $ foldr mk_case
    (joinG gen res
     . recurse (recursiveCalls gen)
     . trans
     $ map expand args)
    splits
  where
    (splits, args)      = unzip (map split_arg avs)

    pat (PatSplit  p)     = p
    pat (CaseSplit p _ _) = p

    split_arg (OtherVal,  g)
     = let v = mkName (g "")
       in  (PatSplit (varP v), OtherArg (varE v))

    split_arg arg       = split gen arg

    mk_case (PatSplit  _) xx            = xx
    mk_case (CaseSplit _ scrut pat') xx = caseE scrut [normalMatch pat' xx]

    expand (RecArg _ es) = es
    expand (OtherArg  e) = repeat e

    trans []            = []
    trans [xs]          = [[x] | x <- xs]
    trans (xs : yss)    = zipWith (:) xs (trans yss)

    recurse 0 _         = []
    recurse n []        = replicate n (varE rec_name)
    recurse n args'     = [varE rec_name `appEs` es | es <- take n args']

    rec_name = recursiveName gen name


nameGens :: [(Name, [[Char] -> [Char]])]
nameGens =
  [
    ('emptyPR,          [])
  , ('replicatePR,      [const "n#", id])
  , ('replicatelPR,     [const "segd", id])
  , ('repeatPR,         [const "n#", const "len#", id])
  , ('indexPR,          [id, const "i#"])
  , ('extractPR,        [id, const "i#", const "n#"])
  , ('bpermutePR,       [id, const "n#", const "ixs"])
  , ('appPR,            [(++"1"), (++"2")])
  , ('applPR,           [const "segd", const "ixs", (++"1"), const "jxs", (++"2")])
  , ('packByTagPR,      [id, const "n#", const "tags", const "t#"])
  , ('combine2PR,       [const "n#", const "sel", (++"1"), (++"2")])
  , ('updatePR,         [(++"1"), const "ixs", (++"2")])
  , ('fromListPR,       [const "n#", id])
  , ('nfPR,             [id])
  ]

-- ---------------
-- Scalar types
-- ---------------

scalarInstances :: [Name] -> Q [Dec]
scalarInstances tys
  = do
      pdatas <- mapM instance_PData_scalar tys
      scalars  <- mapM instance_Scalar_scalar tys
      prs    <- mapM instance_PR_scalar tys
      return $ pdatas ++ scalars ++ prs

pdataScalarCon :: Name -> Name
pdataScalarCon n = mkName ("P" ++ nameBase n)

instance_PData_scalar :: Name -> DecQ
instance_PData_scalar tycon
  = newtype_instance_PData tycon [] (pdataScalarCon tycon)
                                    (conT ''U.Array `appT` conT tycon)

instance_Scalar_scalar :: Name -> DecQ
instance_Scalar_scalar ty
  = instanceD (cxt [])
              (conT ''Scalar `appT` conT ty)
              (map (inlineD . mkName . fst) methods ++ map snd methods)
  where
    pcon = pdataScalarCon ty
    xs   = mkName "xs"

    methods = [("fromScalarPData", mk_fromScalarPData),
               ("toScalarPData",   mk_toScalarPData)]

    mk_fromScalarPData = simpleFunD (mkName "fromScalarPData")
                                  [conP pcon [varP xs]]
                                  (varE xs)
    mk_toScalarPData = simpleFunD (mkName "toScalarPData") [] (conE pcon)

instance_PR_scalar :: Name -> DecQ
instance_PR_scalar ty
  = do
      methods <- genPR_methods (scalarMethod ty)
      return $ InstanceD []
                         (ConT ''PR `AppT` ConT ty)
                         methods

scalarMethod :: Name -> Name -> [ArgVal] -> Val -> DecQ
scalarMethod _ meth _ _
  = simpleFunD (mkName $ nameBase meth) []
  $ varE
  $ mkName (nameBase meth ++ "Scalar")

{-
  = simpleFunD (mkName $ nameBase meth) pats
  $ result res
  $ varE impl `appEs` vals
  where
    pcon = pdataPrimCon ty
    impl = mkName
         $ nameBase meth ++ "Prim"

    (pats, vals) = unzip [arg v g | (v,g) <- avs]

    arg ScalarVal g = var (g "x")
    arg PDataVal  g = let v = mkName (g "xs")
                      in (conP pcon [varP v], varE v)
    arg ListVal   g = var (g "xs")
    arg OtherVal  g = var (g "")

    var s = let v = mkName s in (varP v, varE v)

    result ScalarVal e = e
    result PDataVal  e = conE pcon `appE` e
    result UnitVal   e = varE 'seq `appEs` [e, varE '()]
    result OtherVal  e = e
-}

-- ----
-- Void
-- ----

voidPRInstance :: Name -> Name -> Name -> Q [Dec]
voidPRInstance ty void pvoid
  = do
      methods <- genPR_methods (voidMethod void pvoid)
      return [InstanceD []
                        (ConT ''PR `AppT` ConT ty)
                        methods]

voidMethod :: Name -> Name -> Name -> [ArgVal] -> Val -> DecQ
voidMethod void pvoid meth avs res
  = simpleFunD (mkName $ nameBase meth) (map (const wildP) avs)
  $ result res
  where
    result ScalarVal    = varE void
    result PDataVal     = varE pvoid
    result UnitVal      = conE '()
    result _            = error "DPH gen: voidMethod: no match"

-- --
-- ()
-- --

unitPRInstance :: Name -> Q [Dec]
unitPRInstance punit
  = do
      methods <- genPR_methods (unitMethod punit)
      return [InstanceD []
                        (ConT ''PR `AppT` ConT ''())
                        methods]

unitMethod :: Name -> Name -> [ArgVal] -> Val -> DecQ
unitMethod punit meth avs res
  = simpleFunD (mkName $ nameBase meth) pats
  $ foldr seq_val (result res) es
  where
    (pats, es)          = unzip [mkpat v g | (v,g) <- avs]

    mkpat ScalarVal _   = (conP '() [], Nothing)
    mkpat PDataVal  _   = (conP punit [], Nothing)

    mkpat ListVal   g
     = let xs = mkName (g "xs")
       in  (varP xs, Just $ \e -> varE 'foldr `appEs` [varE 'seq, e, varE xs])

    mkpat OtherVal  _   = (wildP, Nothing)
    mkpat _ _           = error "DPH gen: unitMethod/mkpat: no match"

    result ScalarVal    = conE '()
    result PDataVal     = conE punit
    result UnitVal      = conE '()
    result _            = error "DPH gen: unitMethod/result: no match"

    seq_val Nothing  e  = e
    seq_val (Just f) e  = f e

-- ----
-- Wrap
-- ----

wrapPRInstance :: Name -> Name -> Name -> Name -> Q [Dec]
wrapPRInstance ty wrap unwrap pwrap
  = do
      methods <- genPR_methods (recursiveMethod (wrapGen wrap unwrap pwrap))
      return [InstanceD [ConT ''PA `AppT` a]
                        (ConT ''PR `AppT` (ConT ty `AppT` a))
                        methods]
  where
    a = VarT (mkName "a")

wrapGen :: Name -> Name -> Name -> Gen
wrapGen wrap unwrap pwrap
 = Gen  { recursiveCalls = 1
        , recursiveName  = recursiveName'
        , split          = split'
        , joinG          = join'
        , typeName       = "Wrap a"
        }
  where
    recursiveName' = mkName . replace . nameBase
      where
        replace s = init s ++ "D"

    split' (ScalarVal, gen)
      = (PatSplit (conP wrap [varP x]), RecArg [] [varE x])
      where
        x = mkName (gen "x")

    split' (PDataVal, gen)
      = (PatSplit (conP pwrap [varP xs]), RecArg [] [varE xs])
      where
        xs = mkName (gen "xs")

    split' (ListVal, gen)
      = (PatSplit (varP xs),
         RecArg [] [varE 'map `appEs` [varE unwrap, varE xs]])
      where
        xs = mkName (gen "xs")

    split' _             = error "DPH gen: split': no match"


    join' ScalarVal [x]  = conE wrap `appE` x
    join' PDataVal  [xs] = conE pwrap `appE` xs
    join' UnitVal   [x]  = x
    join' _         _    = error "DPH gen: wrapGen: no match"


-- ------
-- Tuples
-- ------

tupleInstances :: [Int] -> Q [Dec]
tupleInstances ns
  = do
      pdatas <- mapM instance_PData_tup ns
      prs    <- mapM instance_PR_tup ns
      return $ pdatas ++ prs

pdataTupCon :: Int -> Name
pdataTupCon n = mkName ("P_" ++ show n)

instance_PData_tup :: Int -> DecQ
instance_PData_tup arity
  = instance_PData (tupleT arity) vars (pdataTupCon arity)
                [conT ''PData `appT` varT v | v <- vars]
  where
    vars = take arity $ [mkName [c] | c <- ['a' .. ]]


instance_PR_tup :: Int -> DecQ
instance_PR_tup arity
  = do
      methods <- genPR_methods (recursiveMethod (tupGen arity))
      return $ InstanceD [ConT ''PR `AppT` ty | ty <- tys]
                         (ConT ''PR `AppT` (TupleT arity `mkAppTs` tys))
                         methods
  where
    tys = take arity $ [VarT $ mkName [c] | c <- ['a' .. ]]

tupGen :: Int -> Gen
tupGen arity = Gen { recursiveCalls = arity
                   , recursiveName  = id
                   , split          = split'
                   , joinG          = join'
                   , typeName       = tyname
                   }
  where
    split' (ScalarVal, gen)
      = (PatSplit (tupP $ varPs names), RecArg [] (varEs names))
      where
        names = map (mkName . gen) vs

    split' (PDataVal, gen)
      = (PatSplit (conP (pdataTupCon arity) $ varPs names),
         RecArg [] (varEs names))
      where
        names = map (mkName . gen) pvs

    split' (ListVal, gen)
      = (CaseSplit (varP xs) (varE mkunzip `appE` varE xs)
                             (tupP $ varPs names),
         RecArg [] (varEs names))
      where
        xs = mkName (gen "xs")
        names = map (mkName . gen) pvs

        mkunzip | arity == 2 = mkName "unzip"
                | otherwise  = mkName ("unzip" ++ show arity)

    split' _            = error "DPH Gen: tupGen/split: no match"


    join' ScalarVal xs  = tupE xs
    join' PDataVal  xs  = conE (pdataTupCon arity) `appEs` xs
    join' UnitVal   xs  = foldl1 (\x y -> varE 'seq `appEs` [x,y]) xs
    join' _         _   = error "DPH Gen: tupGen/join: no match"

    vs          = take arity [[c] | c <- ['a' ..]]
    pvs         = take arity [c : "s" | c <- ['a' ..]]

    tyname      = "(" ++ intercalate "," vs ++ ")"
