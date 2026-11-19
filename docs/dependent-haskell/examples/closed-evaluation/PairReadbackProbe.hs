-- | Compare a readback decision in two versions of the same executable.
module PairReadbackProbe (plugin) where

import GHC.Core.TyCo.Compare (eqType)
import GHC.Plugins
import GHC.Types.Demand (isDeadEndAppSig, splitDmdSig)

-- | Install the experiment after the ordinary Core simplifier.
plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = \_ passes ->
            pure (passes ++ [CoreDoPluginPass "pair readback experiment" addObserver])
        , pluginRecompile = purePlugin
        }

{- Note [A readback decision in the executable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The fixture exports a worker called pair and an observer with a fixed
ordinary type. Copy the final worker body into the observer. Return a
Boolean with the pair so the client can choose which fields to read.
The Boolean and the ordinary result come from the same linked module.
Only the test interprets this convention. A general compiler mechanism
would have to verify the producer identity and its dependencies.
-}

-- | Supply the fixture's observer from its actual pair constructor.
addObserver :: ModGuts -> CoreM ModGuts
addObserver guts = case [rhs | (v, rhs) <- bindings, getOccString v == "pair"] of
    [rhs] -> do
        let (params, result) = collectBinders rhs
            dead = case collectArgs result of
                (Var worker, [Type a, Type b, x, y])
                    | isDataConWorkId_maybe worker == Just (tupleDataCon Boxed 2)
                    , exprType x `eqType` a
                    , exprType y `eqType` b ->
                        cannotReturn (mkVarEnv bindings) emptyVarSet y
                _ -> False
            flag = mkCoreConApps (if dead then trueDataCon else falseDataCon) []
            entry = mkLams params (mkCoreTup [flag, result])
            replace (NonRec v _)
                | getOccString v == "observe"
                , idType v `eqType` exprType entry =
                    NonRec v entry
            replace other = other
        putMsgS ("Pair field classified as non-returning: " ++ show dead)
        pure guts{mg_binds = map replace (mg_binds guts)}
    _ -> pure guts
  where
    bindings = flattenBinds (mg_binds guts)

-- | Follow floated bindings to a saturated call whose demand signature excludes return.
cannotReturn :: VarEnv CoreExpr -> VarSet -> CoreExpr -> Bool
cannotReturn bodies seen expr = case collectArgs expr of
    (Var v, [])
        | Just rhs <- lookupVarEnv bodies v
        , idArity v == 0 ->
            not (v `elemVarSet` seen) && cannotReturn bodies (extendVarSet seen v) rhs
    (Var v, args)
        | isId v
        , isGlobalId v
        , not (v `elemVarEnv` bodies)
        , not (any isCoArg args) ->
            let n = valArgCount args
                sig = idDmdSig v
             in lengthIs (fst (splitDmdSig sig)) n && isDeadEndAppSig sig n
    _ -> False
