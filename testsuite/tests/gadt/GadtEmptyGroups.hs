{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeAbstractions #-}

module Main where

import GadtEmptyGroupsProvider
import qualified Language.Haskell.TH as TH

$( [d|
     data Quoted where
       Quote :: Bool -> forall. () => Bool -> Quoted
     |]
 )

$( do
     trailing <- TH.reifyType 'Trailing
     between <- TH.reifyType 'Between
     ordered <- TH.reifyType 'Ordered
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
     if map groups [trailing, between, ordered] == ["V", "VV", "IVRCV"]
       then pure []
       else fail (show [trailing, between, ordered])
 )

inspect :: EmptyGroups -> Bool
inspect (Trailing x) = x
inspect (Between x y) = x /= y
inspect (Constrained x) = x == x
inspect (Ordered x (type _) _) = x == x

main :: IO ()
main = case Quote False True of
  Quote x y ->
    print
      ( and
          [ x /= y
          , inspect (Trailing True)
          , inspect (Between True False)
          , inspect (Constrained False)
          , inspect (Ordered (1 :: Int) (type Bool) True)
          ]
      )
