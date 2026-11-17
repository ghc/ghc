{-# LANGUAGE ExplicitNamespaces, DataKinds, KindSignatures, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main where
import GHC.Exts (Matchability(..))
import GHC.Types.Name (nameSrcSpan, getOccString)
import TestUtils
import qualified Data.Map as M
import Data.Foldable
import Data.Either
import GHC.Iface.Type (ifaceTyConName)
f :: forall a -> a -> Maybe a
f (type t) (x :: t) = Just (x :: t)
--      ^p1      ^p2             ^p3

p1,p2 :: (Int,Int)
p1 = (13,9)
p2 = (13,18)
p3 = (13,34)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "HieVdq.hie"
  forM_ [p1,p2,p3] $ \point -> do
    putStr $ "At " ++ show point ++ ", got names: "
    let names =
          concatMap (rights . M.keys . nodeIdentifiers) $
          M.elems $ getSourcedNodeInfo $
          sourcedNodeInfo $ selectPoint' hf point
    forM_ names $ \name -> do
      putStrLn (render df (nameSrcSpan name, name))
  checkForallMetadata hf

keepM :: (forall a. @'Matchable a -> a) -> ()
keepM _ = ()

keepU :: (forall a. @'Unmatchable a -> a) -> ()
keepU _ = ()

keepLeft :: (forall (m :: Matchability) (n :: Matchability).
             @'Unmatchable forall m a. @m m -> a -> m) -> ()
keepLeft _ = ()

keepAlpha :: (forall (p :: Matchability) (q :: Matchability).
              @'Unmatchable forall b c. @p b -> c -> b) -> ()
keepAlpha _ = ()

keepRight :: (forall (p :: Matchability) (q :: Matchability).
              @'Unmatchable forall b c. @q b -> c -> b) -> ()
keepRight _ = ()

keepOuter :: forall a b. @'Unmatchable a -> b -> a
keepOuter x _ = x

-- | Check metadata and scopes after reading the generated HIE file.
checkForallMetadata :: HieFile -> IO ()
checkForallMetadata hf = do
  check "M and U quantifiers have distinct type-table keys"
    (typeIndex "keepM" /= typeIndex "keepU")
  check "M metadata survives the HIE codec"
    (concreteMetadata (argument "keepM") == "Matchable")
  check "U metadata survives the HIE codec"
    (concreteMetadata (argument "keepU") == "Unmatchable")
  check "alpha-renamed metadata shares a type-table key"
    (typeIndex "keepLeft" == typeIndex "keepAlpha")
  check "different outer metadata variables have distinct keys"
    (typeIndex "keepLeft" /= typeIndex "keepRight")
  checkSymbolic "keepLeft" False
  checkSymbolic "keepAlpha" False
  checkSymbolic "keepRight" True
  let outerPoint = (43,22)
      innerPoint = (44,35)
      otherPoint = (44,37)
      annotationPoint = (44,41)
      bodyPoint = (44,43)
      outer = nameAt outerPoint
      inner = nameAt innerPoint
  check "annotation resolves outside its whole forall group"
    (nameAt annotationPoint == outer && inner /= outer
      && nameAt bodyPoint == inner)
  check "outer scope contains the annotation"
    (inScopes (binderScopes outerPoint) annotationPoint)
  checkGroupScopes [innerPoint, otherPoint] annotationPoint bodyPoint
  checkGroupScopes [(55,21), (55,23)] (55,28) (55,40)
  check "outer signature body names refer to its binders"
    (nameAt (55,21) == nameAt (55,40)
      && nameAt (55,23) == nameAt (55,45))
  putStrLn "forall HIE metadata and scopes preserved"
  where
    refs = generateReferencesMap (getAsts (hie_asts hf))
    check label ok = if ok then pure () else fail label

    typeIndex label = case
      [ i | (Right name, uses) <- M.toList refs, getOccString name == label
          , (_, details) <- uses, Just i <- [identType details] ] of
      i : is | all (== i) is -> i
      _ -> error ("missing or inconsistent witness type: " ++ label)

    argument label = case recoverFullType (typeIndex label) (hie_types hf) of
      Roll (HFunTy _ _ arg _) -> arg
      _ -> error ("unexpected witness type: " ++ label)

    parts (Roll (HForAllTy ma ((name, _), _) body)) = (ma, name, body)
    parts _ = error "missing forall in decoded witness"

    concreteMetadata ty = case parts ty of
      (Roll (HTyConApp tc (HieArgs [])), _, _) -> getOccString (ifaceTyConName tc)
      _ -> error "non-concrete metadata in decoded witness"

    checkSymbolic label useSecond = do
      let (_, first, rest) = parts (argument label)
          (_, second, rest') = parts rest
          (ma, inner, rest'') = parts rest'
          (ma', _, _) = parts rest''
          selected = if useSecond then second else first
          isSelected (Roll (HTyVarTy name)) = name == selected && name /= inner
          isSelected _ = False
      check ("outer variable metadata survives: " ++ label)
        (isSelected ma && isSelected ma')

    identifiersAt = sourcedNodeIdents . sourcedNodeInfo . selectPoint' hf
    nameAt point = case rights (M.keys (identifiersAt point)) of
      [name] -> name
      _ -> error ("unexpected names at " ++ show point)

    binderScopes point = case
      [ scope : scopes | details <- M.elems (identifiersAt point)
                       , TyVarBind scope (ResolvedScopes scopes) <- toList (identInfo details) ] of
      scopes@(_ : _) -> concat scopes
      _ -> error ("missing resolved binder scope at " ++ show point)

    inScopes scopes point = any
      (\scope -> scopeContainsSpan scope (nodeSpan (selectPoint' hf point))) scopes

    checkGroupScopes points annotationPoint bodyPoint = forM_ points $ \point ->
      check "group binder scopes exclude their annotation"
        (not (inScopes (binderScopes point) annotationPoint)
          && inScopes (binderScopes point) bodyPoint)
