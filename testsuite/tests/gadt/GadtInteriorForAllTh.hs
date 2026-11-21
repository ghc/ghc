{-# LANGUAGE GADTs, LinearTypes, RequiredTypeArguments, TemplateHaskell, TypeAbstractions #-}
module Main where
import GHC.Types (Multiplicity(Many))
import qualified Language.Haskell.TH as TH
import GadtInteriorForAllProvider

$(let rebuild ty local = do
        info <- TH.reify ty
        let rename (TH.AppT (TH.ConT parent) arg)
              | parent == ty = Just (TH.AppT (TH.ConT local) arg)
            rename (TH.ConT parent)
              | parent == ty = Just (TH.ConT local)
            rename _ = Nothing
        case info of
          TH.TyConI (TH.DataD [] _ vars kind
            [TH.RetainedGadtC [_]
              [first@(TH.GadtValueArg (TH.Bang _ TH.SourceStrict) _ (TH.ConT int))]
              (TH.ForallMatchT ma (TH.ForallT params@[TH.KindedTV a _ _] []
                (TH.AppT arrow@(TH.AppT (TH.AppT TH.MulArrowT _) (TH.VarT field)) result)))]
            [])
            | int == ''Int, a == field, Just result' <- rename result ->
                pure (TH.DataD [] local vars kind
                  [TH.RetainedGadtC [local] [first] (TH.ForallMatchT ma
                    (TH.ForallT params [] (TH.AppT arrow result')))] [])
          TH.TyConI (TH.DataD [] _ vars kind
            [TH.RetainedGadtC [_]
              [first@(TH.GadtValueArg _ _ (TH.ConT int))]
              (TH.ForallMatchT ma (TH.ForallVisT params@[TH.KindedTV a () _]
                (TH.AppT arrow@(TH.AppT (TH.AppT TH.MulArrowT _) (TH.VarT field)) result)))]
            [])
            | int == ''Int, a == field, Just result' <- rename result ->
                pure (TH.DataD [] local vars kind
                  [TH.RetainedGadtC [local] [first] (TH.ForallMatchT ma
                    (TH.ForallVisT params (TH.AppT arrow result')))] [])
          _ -> fail (show info)
  in sequence [rebuild ''Box (TH.mkName "Rebuilt"), rebuild ''Pack (TH.mkName "PackedAgain"),
               rebuild ''Required (TH.mkName "RebuiltRequired"),
               rebuild ''RequiredPack (TH.mkName "RebuiltRequiredPack")])

$([d| data Quoted where
        Quoted :: !Int %Many -> forall a. a %1 -> Quoted
      data QuotedRequired where
        QuotedRequired :: !Int %Many -> forall a -> a %1 -> QuotedRequired
   |])

alias :: $(TH.reifyType 'Box)
alias = Box

requiredAlias :: $(TH.reifyType 'Required)
requiredAlias = Required

rbox :: Rebuilt a -> (Int, a)
rbox (Rebuilt n @a x) = (n, x)

main :: IO ()
main = do
  print (case alias 7 True of Box n @a x -> (n,x),
         rbox (Rebuilt 11 'c'),
         case PackedAgain 13 True of PackedAgain n @a _ -> n,
         case Quoted 17 False of Quoted n @a _ -> n)
  print (case requiredAlias 19 (type Bool) True of Required n (type a) x -> (n,x),
         case RebuiltRequired 23 (type Char) 'd' of RebuiltRequired n (type a) x -> (n,x),
         case RebuiltRequiredPack 29 (type Bool) True of RebuiltRequiredPack n (type a) _ -> n,
         case QuotedRequired 31 (type Bool) False of QuotedRequired n (type a) _ -> n)
