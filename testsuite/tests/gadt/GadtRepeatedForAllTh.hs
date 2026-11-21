{-# LANGUAGE GADTs, LinearTypes, TemplateHaskell, TypeAbstractions, TypeApplications #-}
module Main where

import qualified Language.Haskell.TH as TH
import GadtRepeatedForAllProvider

$(do
    let rename = TH.mkName "Rebuilt"
        check (TH.ForallMatchT _ (TH.ForallT [TH.KindedTV b _ _] []
                (TH.AppT field@(TH.AppT (TH.AppT TH.MulArrowT _) (TH.VarT v)) rest)))
          | b == v = Just (b, field, rest)
        check _ = Nothing
        rebuild (TH.ForallMatchT ma (TH.ForallT vars [] (TH.AppT field _))) rest =
          TH.ForallMatchT ma (TH.ForallT vars [] (TH.AppT field rest))
        rebuild _ _ = error "expected a specified telescope"
    info <- TH.reify ''Parcel
    case info of
      TH.TyConI (TH.DataD [] _ [] kind [TH.RetainedGadtC [_]
        [prefix@(TH.GadtValueArg (TH.Bang _ TH.SourceStrict) _ _)] first] [])
        | Just (a, _, second) <- check first
        , Just (b, _, TH.ConT parent) <- check second
        , parent == ''Parcel, a /= b ->
            pure [TH.DataD [] rename [] kind [TH.RetainedGadtC [rename] [prefix]
              (rebuild first (rebuild second (TH.ConT rename)))] []]
      _ -> fail (show info))

$([d|
    data Quoted where
      Quoted :: !Int -> forall a. a %1 -> forall a. a %1 -> Quoted
  |])

alias :: $(TH.reifyType 'Indexed)
alias = Indexed

main :: IO ()
main = do
  print (case Rebuilt 5 @Bool False @Char 't' of Rebuilt n @a _ @b _ -> n,
         case Quoted 7 @Bool True @Char 'q' of Quoted n @a _ @b _ -> n,
         case alias 11 @Bool True @Char 'x' of Indexed n @a x @b y -> (n,x,y))
