{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Main where

import Data.Proxy (Proxy (..))
import GadtEmptyGroupsProvider
import qualified Language.Haskell.TH as TH

$( [d|
     data Quoted where
       Quote :: Bool -> forall. () => Bool -> Quoted
       QuoteRequired :: forall a. forall b -> a -> (Eq a) => Proxy b -> Quoted

     data QuotedAt b where
       QuoteTerminal :: forall a. (Eq a) => a -> () => forall b -> forall. QuotedAt b
     |]
 )

$( do
     trailing <- TH.reifyType 'Trailing
     between <- TH.reifyType 'Between
     ordered <- TH.reifyType 'Ordered
     canonical <- TH.reifyType 'Canonical
     let groups :: TH.Type -> String
         groups (TH.ForallMatchT _ body) = groups body
         groups (TH.ForallT binders ctx body)
           | not (null binders) || not (null ctx) =
               replicate (length binders) 'I' ++ replicate (length ctx) 'C' ++ groups body
         groups (TH.ForallVisT binders body)
           | not (null binders) = replicate (length binders) 'R' ++ groups body
         groups (TH.AppT (TH.AppT (TH.AppT (TH.AppKindT (TH.ConT arrow) _) _) _) body)
           | TH.nameBase arrow == "FUN" = 'V' : groups body
         groups (TH.AppT (TH.AppT (TH.AppT TH.MulArrowT _) _) body) = 'V' : groups body
         groups (TH.AppT (TH.AppT TH.ArrowT _) body) = 'V' : groups body
         groups (TH.ConT _) = []
         groups _ = "?"
     if map groups [trailing, between, ordered, canonical] == ["V", "VV", "IVRCV", "IICRVV"]
       then pure []
       else fail (show [trailing, between, ordered, canonical])
 )

$( do
     info <- TH.reify ''EmptyGroups
     let hasLeadingRequired
           ( TH.RetainedGadtC
               [constructor]
               [ TH.GadtInvisibleArgs _ [_, _]
                 , TH.GadtRequiredArgs _ [_]
                 , TH.GadtContextArgs [_]
                 , TH.GadtValueArg{}
                 , TH.GadtValueArg{}
                 ]
               _
             ) = constructor == 'LeadingRequired
         hasLeadingRequired _ = False
     case info of
       TH.TyConI (TH.DataD _ _ _ _ constructors _) | any hasLeadingRequired constructors -> pure []
       _ -> fail (show info)
 )

inspect :: EmptyGroups -> Bool
inspect (Trailing x) = x
inspect (Between x y) = x /= y
inspect (Constrained x) = x == x
inspect (Ordered x (type _) _) = x == x
inspect (LeadingRequired (type _) x _) = x == x
inspect (Canonical (type _) x _) = x == x

inspectQuote :: Quoted -> Bool
inspectQuote (Quote x y) = x /= y
inspectQuote (QuoteRequired (type _) x _) = x == x

inspectTerminal :: QuotedAt b -> Bool
inspectTerminal (QuoteTerminal x (type _)) = x == x

inspectImportedTerminal :: Terminal b -> Bool
inspectImportedTerminal (Terminal x (type _)) = x == x

main :: IO ()
main =
  print
    ( and
        [ inspectQuote (Quote False True)
        , inspectQuote (QuoteRequired @Int (type Bool) 1 Proxy)
        , inspectTerminal (QuoteTerminal (1 :: Int) (type Bool))
        , inspectImportedTerminal (Terminal (1 :: Int) (type Bool))
        , inspect (Trailing True)
        , inspect (Between True False)
        , inspect (Constrained False)
        , inspect (Ordered (1 :: Int) (type Bool) True)
        , inspect (LeadingRequired @Int @() (type Bool) 1 Proxy)
        , inspect (Canonical @Int @() (type Bool) 1 Proxy)
        ]
    )
