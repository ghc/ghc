{-|
  Prepare the STG for bytecode generation:

   - Ensure that all breakpoints are directly under
        a let-binding, introducing a new binding for
        those that aren't already.

   - Protect Not-necessarily lifted join points, see
        Note [Not-necessarily-lifted join points]

 -}

module GHC.Stg.BcPrep ( bcPrep ) where

import GHC.Prelude

import GHC.Types.Id.Make
import GHC.Types.Id
import GHC.Core.Type
import GHC.Builtin.Types ( unboxedUnitTy )
import GHC.Builtin.Types.Prim
import GHC.Types.Unique
import GHC.Data.FastString
import GHC.Utils.Panic.Plain
import GHC.Types.Tickish
import GHC.Types.Unique.Supply
import qualified GHC.Types.CostCentre as CC
import GHC.Stg.Syntax
import GHC.Utils.Monad.State.Strict

data BcPrepM_State
   = BcPrepM_State
        { prepUniqSupply :: !UniqSupply      -- for generating fresh variable names
        }

type BcPrepM a = State BcPrepM_State a

bcPrepRHS :: StgRhs -> BcPrepM StgRhs
-- explicitly match all constructors so we get a warning if we miss any
bcPrepRHS (StgRhsClosure fvs cc upd args (StgTick bp@Breakpoint{} expr) typ) = do
  {- If we have a breakpoint directly under an StgRhsClosure we don't
     need to introduce a new binding for it.
   -}
  expr' <- bcPrepExpr expr
  pure (StgRhsClosure fvs cc upd args (StgTick bp expr') typ)
bcPrepRHS (StgRhsClosure fvs cc upd args expr typ) =
  StgRhsClosure fvs cc upd args <$> bcPrepExpr expr <*> pure typ
bcPrepRHS con@StgRhsCon{} = pure con

bcPrepExpr :: StgExpr -> BcPrepM StgExpr
-- explicitly match all constructors so we get a warning if we miss any
bcPrepExpr (StgTick bp@(Breakpoint tick_ty _ _) rhs)
  | isLiftedTypeKind (typeKind tick_ty) = do
      id <- newId tick_ty
      rhs' <- bcPrepExpr rhs
      let expr' = StgTick bp rhs'
          bnd = StgNonRec id (StgRhsClosure noExtFieldSilent
                                            CC.dontCareCCS
                                            ReEntrant
                                            []
                                            expr'
                                            tick_ty
                             )
          letExp = StgLet noExtFieldSilent bnd (StgApp id [])
      pure letExp
  | otherwise = do
      id <- newId (mkVisFunTyMany realWorldStatePrimTy tick_ty)
      rhs' <- bcPrepExpr rhs
      let expr' = StgTick bp rhs'
          bnd = StgNonRec id (StgRhsClosure noExtFieldSilent
                                            CC.dontCareCCS
                                            ReEntrant
                                            [voidArgId]
                                            expr'
                                            tick_ty
                             )
      pure $ StgLet noExtFieldSilent bnd (StgApp id [StgVarArg realWorldPrimId])
bcPrepExpr (StgTick tick rhs) =
  StgTick tick <$> bcPrepExpr rhs
bcPrepExpr (StgLet xlet bnds expr) =
  StgLet xlet <$> bcPrepBind bnds
              <*> bcPrepExpr expr
bcPrepExpr (StgLetNoEscape xlne bnds expr) =
  StgLet xlne <$> bcPrepBind bnds
              <*> bcPrepExpr expr
bcPrepExpr (StgCase expr bndr alt_type alts) =
  StgCase <$> bcPrepExpr expr
          <*> pure bndr
          <*> pure alt_type
          <*> mapM bcPrepAlt alts
bcPrepExpr lit@StgLit{} = pure lit
-- See Note [Not-necessarily-lifted join points], step 3.
bcPrepExpr (StgApp x [])
  | isNNLJoinPoint x = pure $
      StgApp (protectNNLJoinPointId x) [StgVarArg voidPrimId]
bcPrepExpr app@StgApp{} = pure app
bcPrepExpr app@StgConApp{} = pure app
bcPrepExpr app@StgOpApp{} = pure app

bcPrepAlt :: StgAlt -> BcPrepM StgAlt
bcPrepAlt (GenStgAlt con bndrs rhs) = GenStgAlt con bndrs <$> bcPrepExpr rhs

bcPrepBind :: StgBinding -> BcPrepM StgBinding
-- explicitly match all constructors so we get a warning if we miss any
bcPrepBind (StgNonRec bndr rhs) =
  let (bndr', rhs') = bcPrepSingleBind (bndr, rhs)
  in  StgNonRec bndr' <$> bcPrepRHS rhs'
bcPrepBind (StgRec bnds) =
  StgRec <$> mapM ((\(b,r) -> (,) b <$> bcPrepRHS r) . bcPrepSingleBind)
                  bnds

bcPrepSingleBind :: (Id, StgRhs) -> (Id, StgRhs)
-- If necessary, modify this Id and body to protect not-necessarily-lifted join points.
-- See Note [Not-necessarily-lifted join points], step 2.
bcPrepSingleBind (x, StgRhsClosure ext cc upd_flag args body typ)
  | isNNLJoinPoint x
  = ( protectNNLJoinPointId x
    , StgRhsClosure ext cc upd_flag (args ++ [voidArgId]) body typ)
bcPrepSingleBind bnd = bnd

bcPrepTopLvl :: StgTopBinding -> BcPrepM StgTopBinding
bcPrepTopLvl lit@StgTopStringLit{} = pure lit
bcPrepTopLvl (StgTopLifted bnd) = StgTopLifted <$> bcPrepBind bnd

bcPrep :: UniqSupply -> [InStgTopBinding] -> [OutStgTopBinding]
bcPrep us bnds = evalState (mapM bcPrepTopLvl bnds) (BcPrepM_State us)

-- Is this Id a not-necessarily-lifted join point?
-- See Note [Not-necessarily-lifted join points], step 1
isNNLJoinPoint :: Id -> Bool
isNNLJoinPoint x = isJoinId x && mightBeUnliftedType (idType x)

-- Update an Id's type to take a (# #) argument.
-- Precondition: the Id is a not-necessarily-lifted join point.
-- See Note [Not-necessarily-lifted join points]
protectNNLJoinPointId :: Id -> Id
protectNNLJoinPointId x
  = assert (isNNLJoinPoint x )
    updateIdTypeButNotMult (unboxedUnitTy `mkVisFunTyMany`) x

newUnique :: BcPrepM Unique
newUnique = state $
  \st -> case takeUniqFromSupply (prepUniqSupply st) of
            (uniq, us) -> (uniq, st { prepUniqSupply = us })

newId :: Type -> BcPrepM Id
newId ty = do
    uniq <- newUnique
    return $ mkSysLocal prepFS uniq ManyTy ty

prepFS :: FastString
prepFS = fsLit "bcprep"

{-

Note [Not-necessarily-lifted join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A join point variable is essentially a goto-label: it is, for example,
never used as an argument to another function, and it is called only
in tail position. See Note [Join points] and Note [Invariants on join points],
both in GHC.Core. Because join points do not compile to true, red-blooded
variables (with, e.g., registers allocated to them), they are allowed
to be representation-polymorphic.
(See invariant #6 in Note [Invariants on join points] in GHC.Core.)

However, in this byte-code generator, join points *are* treated just as
ordinary variables. There is no check whether a binding is for a join point
or not; they are all treated uniformly. (Perhaps there is a missed optimization
opportunity here, but that is beyond the scope of my (Richard E's) Thursday.)

We thus must have *some* strategy for dealing with representation-polymorphic
and unlifted join points. Representation-polymorphic variables are generally
not allowed (though representation-polymorphic join points *are*; see
Note [Invariants on join points] in GHC.Core, point 6), and we don't wish to
evaluate unlifted join points eagerly.
The questionable join points are *not-necessarily-lifted join points*
(NNLJPs). (Not having such a strategy led to #16509, which panicked in the
isUnliftedType check in the AnnVar case of schemeE.) Here is the strategy:

1. Detect NNLJPs. This is done in isNNLJoinPoint.

2. When binding an NNLJP, add a `\ (_ :: (# #)) ->` to its RHS, and modify the
   type to tack on a `(# #) ->`.
   Note that functions are never representation-polymorphic, so this
   transformation changes an NNLJP to a non-representation-polymorphic
   join point. This is done in bcPrepSingleBind.

3. At an occurrence of an NNLJP, add an application to void# (called voidPrimId),
   being careful to note the new type of the NNLJP. This is done in the AnnVar
   case of schemeE, with help from protectNNLJoinPointId.

Here is an example. Suppose we have

  f = \(r :: RuntimeRep) (a :: TYPE r) (x :: T).
      join j :: a
           j = error @r @a "bloop"
      in case x of
           A -> j
           B -> j
           C -> error @r @a "blurp"

Our plan is to behave is if the code was

  f = \(r :: RuntimeRep) (a :: TYPE r) (x :: T).
      let j :: ((# #) -> a)
          j = \ _ -> error @r @a "bloop"
      in case x of
           A -> j void#
           B -> j void#
           C -> error @r @a "blurp"

It's a bit hacky, but it works well in practice and is local. I suspect the
Right Fix is to take advantage of join points as goto-labels.

-}

