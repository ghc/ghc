{-# LANGUAGE LambdaCase, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Optimizer
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--
-- * Domain and Purpose
--
--     GHC.JS.Optimizer is a shallow embedding of a peephole optimizer. That is,
--     this module defines transformations over the JavaScript IR in
--     'GHC.JS.Syntax', transforming the IR forms from inefficient, or
--     non-idiomatic, JavaScript to more efficient and idiomatic JavaScript. The
--     optimizer is written in continuation passing style so optimizations
--     compose.
--
-- * Architecture of the optimizer
--
--    The design is that each optimization pattern matches on the head of a
--    block by pattern matching onto the head of the stream of nodes in the
--    JavaScript IR. If an optimization gets a successful match then it performs
--    whatever rewrite is necessary and then calls the 'loop' continuation. This
--    ensures that the result of the optimization is subject to the same
--    optimization, /and/ the rest of the optimizations. If there is no match
--    then the optimization should call the 'next' continuation to pass the
--    stream to the next optimization in the optimization chain. We then define
--    the last "optimization" to be @tailLoop@ which selects the next block of
--    code to optimize and begin the optimization pipeline again.
-----------------------------------------------------------------------------
module GHC.JS.Optimizer
 ( jsOptimize
 ) where


import Prelude

import GHC.JS.Syntax

import Control.Arrow

import qualified GHC.JS.Opt.Simple as Simple

{-
Note [Unsafe JavaScript optimizations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a number of optimizations that the JavaScript Backend performs that
are not sound with respect to arbritrary JavaScript. We still perform these
optimizations because we are not optimizing arbritrary javascript and under the
assumption that the JavaScript backend will not generate code that violates the
soundness of the optimizer. For example, the @deadCodeElim@ optimization removes
all statements that occur after a 'return' in JavaScript, however this is not
always sound because of hoisting, consider this program:

  function foo() {
    var x = 2;
    bar();
    return x;

    function bar() {
      x = 10;
  }}

  which is transformed to:

  function foo() {
    var x = 2;
    bar();
    return x;
  }}

The optimized form is clearly a program that goes wrong because `bar()` is no
longer defined. But the JavaScript backend will never generate this code, so as
long as that assumption holds we are safe to perform optimizations that would
normally be unsafe.
-}


--------------------------------------------------------------------------------
--                        Top level Driver
--------------------------------------------------------------------------------
jsOptimize :: JStat -> JStat
jsOptimize s0 = jsOptimizeStat (Simple.simpleOpt s0)

jsOptimizeStat :: JStat -> JStat
jsOptimizeStat s0 = go s0
  where
    p_opt = jsOptimizeStat
    opt   = jsOptimize'
    e_opt = jExprOptimize
    -- base case
    go (BlockStat xs) = BlockStat (opt xs)
    -- recursive cases
    go (ForStat i p s body)   = ForStat (go i) (e_opt p) (go s) (p_opt body)
    go (ForInStat b i p body) = ForInStat b i p (p_opt body)
    go (WhileStat b c body)   = WhileStat b (e_opt c) (p_opt body)
    go (SwitchStat s ps body) = SwitchStat s (fmap (second go) ps) (p_opt body)
    go (FuncStat i args body) = FuncStat i args (p_opt body)
    go (IfStat c t e)         = IfStat (e_opt c) (p_opt t) (p_opt e)
    go (TryStat ths i c f)    = TryStat (p_opt ths) i (p_opt c) (p_opt f)
    go (LabelStat lbl s)      = LabelStat lbl (p_opt s)
    -- special case: drive the optimizer into expressions
    go (AssignStat id op rhs) = AssignStat (e_opt id) op (e_opt rhs)
    go (DeclStat i (Just e))  = DeclStat i (Just $ e_opt e)
    go (ReturnStat e)         = ReturnStat (e_opt e)
    go (UOpStat op e)         = UOpStat op (e_opt e)
    go (ApplStat f args)      = ApplStat   (e_opt f) (e_opt <$> args)
    -- all else is terminal, we match on these to force a warning in the event
    -- another constructor is added
    go x@BreakStat{}          = x
    go x@ContinueStat{}       = x
    go x@DeclStat{}           = x -- match on the nothing case

jsOptimize' :: [JStat] -> [JStat]
jsOptimize' = runBlockOpt opts . single_pass_opts
  where
    opts :: BlockOpt
    opts =  safe_opts
            <> unsafe_opts
            <> tailLoop  -- tailloop must be last, see module description

    unsafe_opts :: BlockOpt
    unsafe_opts = mconcat [ deadCodeElim ]

    safe_opts :: BlockOpt
    safe_opts = mconcat [ declareAssign, combineOps ]

    single_pass_opts :: BlockTrans
    single_pass_opts = runBlockTrans sp_opts

    sp_opts = [flattenBlocks]

-- | recur over a @JExpr@ and optimize the @JVal@s
jExprOptimize :: JExpr -> JExpr
-- the base case
jExprOptimize (ValExpr val)       = ValExpr (jValOptimize val)
-- recursive cases
jExprOptimize (SelExpr obj field) = SelExpr (jExprOptimize obj) field
jExprOptimize (IdxExpr obj ix)    = IdxExpr (jExprOptimize obj) (jExprOptimize ix)
jExprOptimize (UOpExpr op exp)    = UOpExpr op (jExprOptimize exp)
jExprOptimize (IfExpr c t e)      = IfExpr c (jExprOptimize t) (jExprOptimize e)
jExprOptimize (ApplExpr f args )  = ApplExpr (jExprOptimize f) (jExprOptimize <$> args)
jExprOptimize (InfixExpr op l r)  = InfixExpr op (jExprOptimize l) (jExprOptimize r)

-- | drive optimizations to anonymous functions and over expressions
jValOptimize ::  JVal -> JVal
-- base case
jValOptimize (JFunc args body) = JFunc args (jsOptimizeStat body)
-- recursive cases
jValOptimize (JList exprs)     = JList (jExprOptimize <$> exprs)
jValOptimize (JHash hash)      = JHash (jExprOptimize <$> hash)
-- all else is terminal
jValOptimize x@JVar{}          = x
jValOptimize x@JDouble{}       = x
jValOptimize x@JInt{}          = x
jValOptimize x@JStr{}          = x
jValOptimize x@JRegEx{}        = x
jValOptimize x@JBool{}         = x

-- | A block transformation is a function from a stream of syntax to another
-- stream
type BlockTrans = [JStat] -> [JStat]

-- | A BlockOpt is a function that alters the stream, and a continuation that
-- represents the rest of the stream. The first @BlockTrans@ represents
-- restarting the optimizer after a change has happened. The second @BlockTrans@
-- represents the rest of the continuation stream.
newtype BlockOpt = BlockOpt (BlockTrans -> BlockTrans -> BlockTrans)

-- | To merge two BlockOpt we first run the left-hand side optimization and
-- capture the right-hand side in the continuation
instance Semigroup BlockOpt where
  BlockOpt opt0 <> BlockOpt opt1 = BlockOpt
    $ \loop next -> opt0 loop (opt1 loop next)

instance Monoid BlockOpt where
  -- don't loop, just finalize
  mempty = BlockOpt $ \_loop next -> next

-- | loop until a fixpoint is reached
runBlockOpt :: BlockOpt -> [JStat] -> [JStat]
runBlockOpt (BlockOpt opt) xs = recur xs
  where recur = opt recur id

runBlockTrans :: [BlockTrans] -> [JStat] -> [JStat]
runBlockTrans opts = foldl (.) id opts

-- | Perform all the optimizations on the tail of a block.
tailLoop :: BlockOpt
tailLoop = BlockOpt $ \loop next -> \case
    []     -> next []
    -- this call to jsOptimize is required or else the optimizer will not
    -- properly recur down JStat. See the 'deadCodeElim' test for examples which
    -- were failing before this change
    (x:xs) -> next (jsOptimizeStat x : loop xs)

{- |
   Catch modify and assign operators:
      case 1:
        i = i + 1; ==> ++i;
      case 2:
        i = i - 1; ==> --i;
      case 3:
        i = i + n; ==> i += n;
      case 4:
        i = i - n; ==> i -= n;
-}
combineOps :: BlockOpt
combineOps = BlockOpt $ \loop next ->
    -- find an op pattern, and rerun the optimizer on its result unless there is
    -- nothing to optimize, in which case call the next optimization
  \case
    -- var x = expr; return x; ==> return expr;
    (DeclStat i (Just e) : ReturnStat (ValExpr (JVar i')) : xs)
      | i == i' -> loop $ ReturnStat e : xs

    -- x = expr; return x; ==> return expr;
    (AssignStat (ValExpr (JVar i)) AssignOp e : ReturnStat (ValExpr (JVar i')) : xs)
      | i == i' -> loop $ ReturnStat e : xs

    -- h$sp -= 2; h$sp += 5; ==> h$sp += 3;
    (op1 : op2 : xs)
      | Just s1 <- isStackAdjust op1
      , Just s2 <- isStackAdjust op2 -> loop $ mkStackAdjust (s1 + s2) ++ xs

    -- x = x + 1; ==> ++x;
    -- x = x - 1; ==> --x;
    -- x = x + n; ==> x += n;
    -- x = x - n; ==> x -= n;
    (unchanged@(AssignStat
                  ident@(ValExpr (JVar i))
                  AssignOp
                  (InfixExpr op (ValExpr (JVar i')) e)) : xs)
      | i == i' -> case (op, e) of
                     (AddOp, (ValExpr (JInt 1))) -> loop $ UOpStat PreIncOp ident          : xs
                     (SubOp, (ValExpr (JInt 1))) -> loop $ UOpStat PreDecOp ident          : xs
                     (AddOp, e')                 -> loop $ AssignStat ident AddAssignOp e' : xs
                     (SubOp, e')                 -> loop $ AssignStat ident SubAssignOp e' : xs
                     _                           -> next $ unchanged : xs
    -- commutative cases
    (unchanged@(AssignStat
                  ident@(ValExpr (JVar i))
                  AssignOp
                  (InfixExpr op e (ValExpr (JVar i')))) : xs)
      | i == i' -> case (op, e) of
                     (AddOp, (ValExpr (JInt 1))) -> loop $ UOpStat PreIncOp ident          : xs
                     (AddOp, e')                 -> loop $ AssignStat ident AddAssignOp e' : xs
                     _                           -> next $ unchanged : xs
    -- general case, we had nothing to optimize in this case so call the next
    -- optimization
    xs -> next xs


-- | Catch 'var i; i = q;' ==> 'var i = q;'
declareAssign :: BlockOpt
declareAssign = BlockOpt $
  \loop next -> \case
    ( (DeclStat i Nothing)
      : (AssignStat (ValExpr (JVar i')) AssignOp v)
      : xs
      )  | i == i' -> loop (DeclStat i (Just v) : xs)
    xs -> next xs

-- | Eliminate all code after a return statement. This is a special case
-- optimization that doesn't need to loop. See Note [Unsafe JavaScript
-- optimizations]
deadCodeElim :: BlockOpt
deadCodeElim = BlockOpt $
  \_loop next -> \case
    (x@ReturnStat{}:_) -> next [x]
    xs                 -> next xs

-- | remove nested blocks
flattenBlocks :: BlockTrans
flattenBlocks (BlockStat y : ys) = flattenBlocks y ++ flattenBlocks ys
flattenBlocks (x:xs)             = x : flattenBlocks xs
flattenBlocks []                 = []

-- | stack adjustments
sp :: JExpr
sp = ValExpr (JVar (TxtI "h$sp"))

isStackAdjust :: JStat -> Maybe Integer
isStackAdjust (UOpStat op (ValExpr (JVar (TxtI "h$sp"))))
  | op == PreIncOp || op == PostIncOp = Just 1
isStackAdjust (UOpStat op (ValExpr (JVar (TxtI "h$sp"))))
  | op == PreDecOp || op == PostDecOp = Just (-1)
isStackAdjust (AssignStat (ValExpr (JVar (TxtI "h$sp"))) op (ValExpr (JInt n)))
  | op == AddAssignOp = Just n
  | op == SubAssignOp = Just (-n)
isStackAdjust (AssignStat (ValExpr (JVar (TxtI "h$sp"))) AssignOp (InfixExpr op (ValExpr (JVar (TxtI "h$sp"))) (ValExpr (JInt n))))
  | op == AddOp = Just n
  | op == SubOp = Just (-n)
isStackAdjust (AssignStat (ValExpr (JVar (TxtI "h$sp"))) AssignOp (InfixExpr AddOp (ValExpr (JInt n)) (ValExpr (JVar (TxtI "h$sp")))))
  = Just n
isStackAdjust _ = Nothing

mkStackAdjust :: Integer -> [JStat]
mkStackAdjust 0 = []
mkStackAdjust 1 = [UOpStat PostIncOp sp]
mkStackAdjust (-1) = [UOpStat PostDecOp sp]
mkStackAdjust x
  | x < 0 = [AssignStat sp AssignOp (InfixExpr SubOp sp (ValExpr (JInt (-x))))]
  | otherwise = [AssignStat sp AssignOp (InfixExpr AddOp sp (ValExpr (JInt x)))]
