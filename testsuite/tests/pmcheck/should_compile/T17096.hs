{-# language PatternSynonyms #-}
-- Taken from the Dhall library
module T17096 where

data Expr s a
    = Const String
    | Var Int
    | Lam String (Expr s a) (Expr s a)
    | Pi  String (Expr s a) (Expr s a)
    | App (Expr s a) (Expr s a)
    | Let String (Maybe (Expr s a)) (Expr s a) (Expr s a)
    | Annot (Expr s a) (Expr s a)
    | Bool
    | BoolLit Bool
    | BoolAnd (Expr s a) (Expr s a)
    | BoolOr  (Expr s a) (Expr s a)
    | BoolEQ  (Expr s a) (Expr s a)
    | BoolNE  (Expr s a) (Expr s a)
    | BoolIf (Expr s a) (Expr s a) (Expr s a)
    | Natural
    | NaturalLit Integer
    | NaturalFold
    | NaturalBuild
    | NaturalIsZero
    | NaturalEven
    | NaturalOdd
    | NaturalToInteger
    | NaturalShow
    | NaturalSubtract
    | NaturalPlus (Expr s a) (Expr s a)
    | NaturalTimes (Expr s a) (Expr s a)
    | Integer
    | IntegerLit Integer
    | IntegerShow
    | IntegerToDouble
    | Double
    | DoubleLit Double
    | DoubleShow
    | String
    | StringLit String
    | StringAppend (Expr s a) (Expr s a)
    | StringShow
    | List
    | ListLit (Maybe (Expr s a)) [Expr s a]
    | ListAppend (Expr s a) (Expr s a)
    | ListBuild
    | ListFold
    | ListLength
    | ListHead
    | ListLast
    | ListIndexed
    | ListReverse
    | Optional
    | Some (Expr s a)
    | None
    | OptionalFold
    | OptionalBuild
    | Record    [(String, Expr s a)]
    | RecordLit [(String, Expr s a)]
    | Union     [(String, Maybe (Expr s a))]
    | Combine (Expr s a) (Expr s a)
    | CombineTypes (Expr s a) (Expr s a)
    | Prefer (Expr s a) (Expr s a)
    | Merge (Expr s a) (Expr s a) (Maybe (Expr s a))
    | ToMap (Expr s a) (Maybe (Expr s a))
    | Field (Expr s a) String
    | Project (Expr s a) (Either [String] (Expr s a))
    | Assert (Expr s a)
    | Equivalent (Expr s a) (Expr s a)
    | Note s (Expr s a)
    | ImportAlt (Expr s a) (Expr s a)
    | Embed a

isNormalized :: Eq a => Expr s a -> Bool
isNormalized = loop
  where
    loop e = case e of
      Const _ -> True
      Var _ -> True
      Lam _ a b -> loop a && loop b
      Pi _ a b -> loop a && loop b
      App f a -> loop f && loop a && case App f a of
          App (Lam _ _ _) _ -> False
          App (App ListBuild _) (App (App ListFold _) _) -> False
          App NaturalBuild (App NaturalFold _) -> False
          App (App OptionalBuild _) (App (App OptionalFold _) _) -> False
          App (App (App (App NaturalFold (NaturalLit _)) _) _) _ -> False
          App NaturalFold (NaturalLit _) -> False
          App NaturalBuild _ -> False
          App NaturalIsZero (NaturalLit _) -> False
          App NaturalEven (NaturalLit _) -> False
          App NaturalOdd (NaturalLit _) -> False
          App NaturalShow (NaturalLit _) -> False
          App (App NaturalSubtract (NaturalLit _)) (NaturalLit _) -> False
          App (App NaturalSubtract (NaturalLit 0)) _ -> False
          App (App NaturalSubtract _) (NaturalLit 0) -> False
          App (App NaturalSubtract x) y -> not (undefined x y)
          App NaturalToInteger (NaturalLit _) -> False
          App IntegerShow (IntegerLit _) -> False
          App IntegerToDouble (IntegerLit _) -> False
          App DoubleShow (DoubleLit _) -> False
          App (App OptionalBuild _) _ -> False
          App (App ListBuild _) _ -> False
          App (App (App (App (App ListFold _) (ListLit _ _)) _) _) _ ->
              False
          App (App ListLength _) (ListLit _ _) -> False
          App (App ListHead _) (ListLit _ _) -> False
          App (App ListLast _) (ListLit _ _) -> False
          App (App ListIndexed _) (ListLit _ _) -> False
          App (App ListReverse _) (ListLit _ _) -> False
          App (App (App (App (App OptionalFold _) (Some _)) _) _) _ ->
              False
          App (App (App (App (App OptionalFold _) (App None _)) _) _) _ ->
              False
          App StringShow (StringLit _) ->
              False
          _ -> True
      Let _ _ _ _ -> False
      Annot _ _ -> False
      Bool -> True
      BoolLit _ -> True
      BoolAnd x y -> loop x && loop y && decide x y
        where
          decide (BoolLit _)  _          = False
          decide  _          (BoolLit _) = False
          decide  l           r          = not (undefined l r)
      BoolOr x y -> loop x && loop y && decide x y
        where
          decide (BoolLit _)  _          = False
          decide  _          (BoolLit _) = False
          decide  l           r          = not (undefined l r)
      BoolEQ x y -> loop x && loop y && decide x y
        where
          decide (BoolLit True)  _             = False
          decide  _             (BoolLit True) = False
          decide  l              r             = not (undefined l r)
      BoolNE x y -> loop x && loop y && decide x y
        where
          decide (BoolLit False)  _               = False
          decide  _              (BoolLit False ) = False
          decide  l               r               = not (undefined l r)
      BoolIf x y z ->
          loop x && loop y && loop z && decide x y z
        where
          decide (BoolLit _)  _              _              = False
          decide  _          (BoolLit True) (BoolLit False) = False
          decide  _           l              r              = not (undefined l r)
      Natural -> True
      NaturalLit _ -> True
      NaturalFold -> True
      NaturalBuild -> True
      NaturalIsZero -> True
      NaturalEven -> True
      NaturalOdd -> True
      NaturalShow -> True
      NaturalSubtract -> True
      NaturalToInteger -> True
      NaturalPlus x y -> loop x && loop y && decide x y
        where
          decide (NaturalLit 0)  _             = False
          decide  _             (NaturalLit 0) = False
          decide (NaturalLit _) (NaturalLit _) = False
          decide  _              _             = True
      NaturalTimes x y -> loop x && loop y && decide x y
        where
          decide (NaturalLit 0)  _             = False
          decide  _             (NaturalLit 0) = False
          decide (NaturalLit 1)  _             = False
          decide  _             (NaturalLit 1) = False
          decide (NaturalLit _) (NaturalLit _) = False
          decide  _              _             = True
      Integer -> True
      IntegerLit _ -> True
      IntegerShow -> True
      IntegerToDouble -> True
      Double -> True
      DoubleLit _ -> True
      DoubleShow -> True
      String -> True
      StringLit _ -> False
      StringAppend _ _ -> False
      StringShow -> True
      List -> True
      ListLit t es -> all loop t && all loop es
      ListAppend x y -> loop x && loop y && decide x y
        where
          decide (ListLit _ m)  _            | null m = False
          decide  _            (ListLit _ n) | null n = False
          decide (ListLit _ _) (ListLit _ _)          = False
          decide  _             _                     = True
      ListBuild -> True
      ListFold -> True
      ListLength -> True
      ListHead -> True
      ListLast -> True
      ListIndexed -> True
      ListReverse -> True
      Optional -> True
      Some a -> loop a
      None -> True
      OptionalFold -> True
      OptionalBuild -> True
      Record kts -> undefined kts && all loop (map snd kts)
      RecordLit kvs -> undefined kvs && all loop (map snd kvs)
      Union kts -> undefined kts && all (all loop) (map snd kts)
      Combine x y -> loop x && loop y && decide x y
        where
          decide (RecordLit m) _ | null m = False
          decide _ (RecordLit n) | null n = False
          decide (RecordLit _) (RecordLit _) = False
          decide  _ _ = True
      CombineTypes x y -> loop x && loop y && decide x y
        where
          decide (Record m) _ | null m = False
          decide _ (Record n) | null n = False
          decide (Record _) (Record _) = False
          decide  _ _ = True
      Prefer x y -> loop x && loop y && decide x y
        where
          decide (RecordLit m) _ | null m = False
          decide _ (RecordLit n) | null n = False
          decide (RecordLit _) (RecordLit _) = False
          decide l r = not (undefined l r)
      Merge x y t -> loop x && loop y && all loop t
      ToMap x t -> case x of
          RecordLit _ -> False
          _ -> loop x && all loop t
      Field r k -> case r of
          RecordLit _ -> False
          Project _ _ -> False
          Prefer (RecordLit m) _ -> map fst m == [k] && loop r
          Prefer _ (RecordLit _) -> False
          Combine (RecordLit m) _ -> map fst m == [k] && loop r
          Combine _ (RecordLit m) -> map fst m == [k] && loop r
          _ -> loop r
      Project r p -> loop r &&
          case p of
              Left s -> case r of
                  RecordLit _ -> False
                  _ -> not (null s) && undefined s
              Right e' -> case e' of
                  Record _ -> False
                  _ -> loop e'
      Assert t -> loop t
      Equivalent l r -> loop l && loop r
      Note _ e' -> loop e'
      ImportAlt l _r -> loop l
      Embed _ -> True

{-# COMPLETE
      Let'
    , Const
    , Var
    , Lam
    , Pi
    , App
    , Annot
    , Bool
    , BoolLit
    , BoolAnd
    , BoolOr
    , BoolEQ
    , BoolNE
    , BoolIf
    , Natural
    , NaturalLit
    , NaturalFold
    , NaturalBuild
    , NaturalIsZero
    , NaturalEven
    , NaturalOdd
    , NaturalToInteger
    , NaturalShow
    , NaturalSubtract
    , NaturalPlus
    , NaturalTimes
    , Integer
    , IntegerLit
    , IntegerShow
    , IntegerToDouble
    , Double
    , DoubleLit
    , DoubleShow
    , String
    , StringLit
    , StringAppend
    , StringShow
    , List
    , ListLit
    , ListAppend
    , ListBuild
    , ListFold
    , ListLength
    , ListHead
    , ListLast
    , ListIndexed
    , ListReverse
    , Optional
    , Some
    , None
    , OptionalFold
    , OptionalBuild
    , Record
    , RecordLit
    , Union
    , Combine
    , CombineTypes
    , Prefer
    , Merge
    , ToMap
    , Field
    , Project
    , Assert
    , Equivalent
    , Note
    , ImportAlt
    , Embed
 #-}
pattern Let' x mA a b = Let x mA a b
