{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module GHC.Core.LateCC.OverloadedCalls
  ( overloadedCallsCC
  ) where

import GHC.Prelude

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified GHC.Data.Strict as Strict

import GHC.Data.FastString
import GHC.Core
import GHC.Core.LateCC.Utils
import GHC.Core.LateCC.Types
import GHC.Core.Make
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.Utils
import GHC.Tc.Utils.TcType
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Tickish
import GHC.Types.Var

type OverloadedCallsCCState = Strict.Maybe SrcSpan

-- | Insert cost centres on function applications with dictionary arguments. The
-- source locations attached to the cost centres is approximated based on the
-- "closest" source note encountered in the traversal.
overloadedCallsCC :: CoreBind -> LateCCM OverloadedCallsCCState CoreBind
overloadedCallsCC =
    processBind
  where
    processBind :: CoreBind -> LateCCM OverloadedCallsCCState CoreBind
    processBind core_bind =
        case core_bind of
          NonRec b e ->
            NonRec b <$> wrap_if_join b (processExpr e)
          Rec es ->
            Rec <$> mapM (\(b,e) -> (b,) <$> wrap_if_join b (processExpr e)) es
      where
        -- If an overloaded function is turned into a join point, we won't add
        -- SCCs directly to calls since it makes them non-tail calls. Instead,
        -- we look for join points here and add an SCC to their RHS if they are
        -- overloaded.
        wrap_if_join ::
             CoreBndr
          -> LateCCM OverloadedCallsCCState CoreExpr
          -> LateCCM OverloadedCallsCCState CoreExpr
        wrap_if_join b pexpr = do
            expr <- pexpr
            if isJoinId b && isOverloadedTy (exprType expr) then do
              let
                cc_name :: FastString
                cc_name = fsLit "join-rhs-" `appendFS` getOccFS b

              cc_srcspan <-
                fmap (Strict.fromMaybe (UnhelpfulSpan UnhelpfulNoLocationInfo)) $
                  lift $ gets lateCCState_extra

              insertCCRhs cc_name cc_srcspan expr
            else
              return expr


    processExpr :: CoreExpr -> LateCCM OverloadedCallsCCState CoreExpr
    processExpr expr =
      case expr of
        -- The case we care about: Application
        app@App{} -> do
          -- Here we have some application like `f v1 ... vN`, where v1 ... vN
          -- should be the function's type arguments followed by the value
          -- arguments. To determine if the `f` is an overloaded function, we
          -- check if any of the arguments v1 ... vN are dictionaries.
          let
            (f, xs) = collectArgs app
            resultTy = applyTypeToArgs (exprType f) xs

          -- Recursively process the arguments first for no particular reason
          args <- mapM processExpr xs
          let app' = mkCoreApps f args

          if
              -- Check if any of the arguments are dictionaries
              any isDictExpr args

              -- Avoid instrumenting dictionary functions, which may be
              -- overloaded if there are superclasses, by checking if the result
              -- type of the function is a dictionary type.
            && not (isDictTy resultTy)

              -- Avoid instrumenting constraint selectors like eq_sel
            && (typeTypeOrConstraint resultTy /= ConstraintLike)

              -- Avoid instrumenting join points.
              -- (See comment in processBind above)
            && not (isJoinVarExpr f)
          then do
            -- Extract a name and source location from the function being
            -- applied
            let
              cc_name :: FastString
              cc_name =
                maybe (fsLit "<no name available>") getOccFS (exprName app)

            cc_srcspan <-
              fmap (Strict.fromMaybe (UnhelpfulSpan UnhelpfulNoLocationInfo)) $
                lift $ gets lateCCState_extra

            insertCC cc_name cc_srcspan app'
          else
            return app'

        -- For recursive constructors of Expr, we traverse the nested Exprs
        Lam b e ->
          mkCoreLams [b] <$> processExpr e
        Let b e ->
          mkCoreLet <$> processBind b <*> processExpr e
        Case e b t alts ->
              Case
          <$> processExpr e
          <*> pure b
          <*> pure t
          <*> mapM processAlt alts
        Cast e co ->
          mkCast <$> processExpr e <*> pure co
        Tick t e -> do
          trackSourceNote t $
            mkTick t <$> processExpr e

        -- For non-recursive constructors of Expr, we do nothing
        x -> return x

    processAlt :: CoreAlt -> LateCCM OverloadedCallsCCState CoreAlt
    processAlt (Alt c bs e) = Alt c bs <$> processExpr e

    trackSourceNote :: CoreTickish -> LateCCM OverloadedCallsCCState a -> LateCCM OverloadedCallsCCState a
    trackSourceNote tick act =
      case tick of
        SourceNote rss _ -> do
          -- Prefer source notes from the current file
          in_current_file <-
            maybe False ((== EQ) . lexicalCompareFS (srcSpanFile rss)) <$>
              asks lateCCEnv_file
          if not in_current_file then
            act
          else do
            loc <- lift $ gets lateCCState_extra
            lift . modify $ \s ->
              s { lateCCState_extra =
                    Strict.Just $ RealSrcSpan rss mempty
                }
            x <- act
            lift . modify $ \s ->
              s { lateCCState_extra = loc
                }
            return x
        _ ->
          act

    -- Utility functions

    -- Extract a Name from an expression. If it is an application, attempt to
    -- extract a name from the applied function. If it is a variable, return the
    -- Name of the variable. If it is a tick/cast, attempt to extract a Name
    -- from the expression held in the tick/cast. Otherwise return Nothing.
    exprName :: CoreExpr -> Maybe Name
    exprName =
        \case
          App f _ ->
            exprName f
          Var f ->
            Just (idName f)
          Tick _ e ->
            exprName e
          Cast e _ ->
            exprName e
          _ ->
            Nothing

    -- Determine whether an expression is a dictionary
    isDictExpr :: CoreExpr -> Bool
    isDictExpr =
        maybe False isDictTy . exprType'
      where
        exprType' :: CoreExpr -> Maybe Type
        exprType' = \case
            Type{} -> Nothing
            expr -> Just $ exprType expr

    -- Determine whether an expression is a join variable
    isJoinVarExpr :: CoreExpr -> Bool
    isJoinVarExpr =
        \case
          Var var -> isJoinId var
          Tick _ e -> isJoinVarExpr e
          Cast e _ -> isJoinVarExpr e
          _ -> False
