
-- (those who have too heavy dependencies for TcEvidence)
module TcEvTerm
    ( evDelayedError, evCallStack )
where

import GhcPrelude

import FastString
import Type
import CoreSyn
import MkCore
import Literal ( Literal(..) )
import TcEvidence
import HscTypes
import DynFlags
import Name
import Module
import CoreUtils
import PrelNames
import SrcLoc

-- Used with Opt_DeferTypeErrors
-- See Note [Deferring coercion errors to runtime]
-- in TcSimplify
evDelayedError :: Type -> FastString -> EvTerm
evDelayedError ty msg
  = EvExpr $
    Var errorId `mkTyApps` [getRuntimeRep ty, ty] `mkApps` [litMsg]
  where
    errorId = tYPE_ERROR_ID
    litMsg  = Lit (LitString (fastStringToByteString msg))

-- Dictionary for CallStack implicit parameters
evCallStack :: (MonadThings m, HasModule m, HasDynFlags m) =>
    EvCallStack -> m EvExpr
-- See Note [Overview of implicit CallStacks] in TcEvidence.hs
evCallStack cs = do
  df            <- getDynFlags
  m             <- getModule
  srcLocDataCon <- lookupDataCon srcLocDataConName
  let mkSrcLoc l = mkCoreConApps srcLocDataCon <$>
               sequence [ mkStringExprFS (unitIdFS $ moduleUnitId m)
                        , mkStringExprFS (moduleNameFS $ moduleName m)
                        , mkStringExprFS (srcSpanFile l)
                        , return $ mkIntExprInt df (srcSpanStartLine l)
                        , return $ mkIntExprInt df (srcSpanStartCol l)
                        , return $ mkIntExprInt df (srcSpanEndLine l)
                        , return $ mkIntExprInt df (srcSpanEndCol l)
                        ]

  emptyCS <- Var <$> lookupId emptyCallStackName

  pushCSVar <- lookupId pushCallStackName
  let pushCS name loc rest =
        mkCoreApps (Var pushCSVar) [mkCoreTup [name, loc], rest]

  let mkPush name loc tm = do
        nameExpr <- mkStringExprFS name
        locExpr <- mkSrcLoc loc
        -- at this point tm :: IP sym CallStack
        -- but we need the actual CallStack to pass to pushCS,
        -- so we use unwrapIP to strip the dictionary wrapper
        -- See Note [Overview of implicit CallStacks]
        let ip_co = unwrapIP (exprType tm)
        return (pushCS nameExpr locExpr (Cast tm ip_co))

  case cs of
    EvCsPushCall name loc tm -> mkPush (occNameFS $ getOccName name) loc tm
    EvCsEmpty -> return emptyCS
