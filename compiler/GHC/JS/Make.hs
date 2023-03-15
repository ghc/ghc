{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- only for Num, Fractional on JExpr

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Make
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
--     GHC.JS.Make defines helper functions to ease the creation of JavaScript
--     ASTs as defined in 'GHC.JS.Syntax'. Its purpose is twofold: make the EDSL
--     more ergonomic to program in, and make errors in the EDSL /look/ obvious
--     because the EDSL is untyped. It is primarily concerned with injecting
--     terms into the domain of the EDSL to construct JS programs in Haskell.
--
-- * Strategy
--
--     The strategy for this module comes straight from gentzen; where we have
--     two types of helper functions. Functions which inject terms into the
--     EDSL, and combinator functions which operate on terms in the EDSL to
--     construct new terms in the EDSL. Crucially, missing from this module are
--     corresponding /elimination/ or /destructing/ functions which would
--     project information from the EDSL back to Haskell. See
--     'GHC.StgToJS.UnitUtils' and 'GHC.StgToJS.CoreUtils' for such functions.
--
--      * /Introduction/ functions
--
--           We define various primitive helpers which /introduce/ terms in the
--           EDSL, for example 'jVar', 'jLam', and 'var' and 'jString'. Notice
--           that the type of each of these functions have the domain @isSat a
--           => a -> ...@; indicating that they each take something that /can/
--           be injected into the EDSL domain, and the range 'JExpr' or 'JStat';
--           indicating the corresponding value in the EDSL domain. Similarly
--           this module exports two typeclasses 'ToExpr' and 'ToSat', 'ToExpr'
--           injects values as a JS expression into the EDSL. 'ToSat' ensures
--           that terms introduced into the EDSL carry identifier information so
--           terms in the EDSL must have meaning.
--
--      * /Combinator/ functions
--
--           The rest of the module defines combinators which create terms in
--           the EDSL from terms in the EDSL. Notable examples are '|=' and
--           '||=', '|=' is sugar for 'AssignStat', it is a binding form that
--           declares @foo = bar@ /assuming/ foo has been already declared.
--           '||=' is more sugar on top of '|=', it is also a binding form that
--           declares the LHS of '|=' before calling '|=' to bind a value, bar,
--           to a variable foo. Other common examples are the 'if_' and 'math_'
--           helpers such as 'math_cos'.
--
-- * Consumers
--
--     The entire JS backend consumes this module, e.g., the modules in
--     GHC.StgToJS.\*.
--
-- * Notation
--
--     In this module we use @==>@ in docstrings to show the translation from
--     the JS EDSL domain to JS code. For example, @foo ||= bar ==> var foo; foo
--     = bar;@ should be read as @foo ||= bar@ is in the EDSL domain and results
--     in the JS code @var foo; foo = bar;@ when compiled.
-----------------------------------------------------------------------------
module GHC.JS.Make
  ( -- * Injection Type classes
    -- $classes
    ToJExpr(..)
  , ToStat(..)
  -- * Introduction functions
  -- $intro_funcs
  , var
  , jString
  , jLam, jVar, jFor, jForIn, jForEachIn, jTryCatchFinally
  -- * Combinators
  -- $combinators
  , (||=), (|=), (.==.), (.===.), (.!=.), (.!==.), (.!)
  , (.>.), (.>=.), (.<.), (.<=.)
  , (.<<.), (.>>.), (.>>>.)
  , (.|.), (.||.), (.&&.)
  , if_, if10, if01, ifS, ifBlockS
  , jwhenS
  , app, appS, returnS
  , loop, loopBlockS
  , preIncrS, postIncrS
  , preDecrS, postDecrS
  , off8, off16, off32, off64
  , mask8, mask16
  , signExtend8, signExtend16
  , typeof
  , returnStack, assignAllEqual, assignAll, assignAllReverseOrder
  , declAssignAll
  , nullStat, (.^)
  , trace
  -- ** Hash combinators
  , jhEmpty
  , jhSingle
  , jhAdd
  , jhFromList
  -- * Literals
  -- $literals
  , null_
  , undefined_
  , false_
  , true_
  , zero_
  , one_
  , two_
  , three_
  -- ** Math functions
  -- $math
  , math_log, math_sin, math_cos, math_tan, math_exp, math_acos, math_asin,
    math_atan, math_abs, math_pow, math_sqrt, math_asinh, math_acosh, math_atanh,
    math_cosh, math_sinh, math_tanh, math_expm1, math_log1p, math_fround
  -- * Statement helpers
  , decl
  )
where

import GHC.Prelude hiding ((.|.))

import GHC.JS.Unsat.Syntax

import Control.Arrow ((***))

import qualified Data.Map as M

import GHC.Data.FastString
import GHC.Utils.Monad.State.Strict
import GHC.Utils.Misc
import GHC.Types.Unique.Map

--------------------------------------------------------------------------------
--                        Type Classes
--------------------------------------------------------------------------------
-- $classes
-- The 'ToJExpr' class handles injection of of things into the EDSL as a JS
-- expression

-- | Things that can be marshalled into javascript values.
-- Instantiate for any necessary data structures.
class ToJExpr a where
    toJExpr         :: a   -> JExpr
    toJExprFromList :: [a] -> JExpr
    toJExprFromList = ValExpr . JList . map toJExpr

instance ToJExpr a => ToJExpr [a] where
    toJExpr = toJExprFromList

instance ToJExpr JExpr where
    toJExpr = id

instance ToJExpr () where
    toJExpr _ = ValExpr $ JList []

instance ToJExpr Bool where
    toJExpr True  = var "true"
    toJExpr False = var "false"

instance ToJExpr JVal where
    toJExpr = ValExpr

instance ToJExpr a => ToJExpr (UniqMap FastString a) where
    toJExpr = ValExpr . JHash . mapUniqMap toJExpr

instance ToJExpr a => ToJExpr (M.Map String a) where
    toJExpr = ValExpr . JHash . listToUniqMap . map (mkFastString *** toJExpr) . M.toList

instance ToJExpr Double where
    toJExpr = ValExpr . JDouble . SaneDouble

instance ToJExpr Int where
    toJExpr = ValExpr . JInt . fromIntegral

instance ToJExpr Integer where
    toJExpr = ValExpr . JInt

instance ToJExpr Char where
    toJExpr = ValExpr . JStr . mkFastString . (:[])
    toJExprFromList = ValExpr . JStr . mkFastString
--        where escQuotes = tailDef "" . initDef "" . show

instance ToJExpr Ident where
    toJExpr = ValExpr . JVar

instance ToJExpr FastString where
    toJExpr = ValExpr . JStr

instance (ToJExpr a, ToJExpr b) => ToJExpr (a,b) where
    toJExpr (a,b) = ValExpr . JList $ [toJExpr a, toJExpr b]

instance (ToJExpr a, ToJExpr b, ToJExpr c) => ToJExpr (a,b,c) where
    toJExpr (a,b,c) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c]

instance (ToJExpr a, ToJExpr b, ToJExpr c, ToJExpr d) => ToJExpr (a,b,c,d) where
    toJExpr (a,b,c,d) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c, toJExpr d]
instance (ToJExpr a, ToJExpr b, ToJExpr c, ToJExpr d, ToJExpr e) => ToJExpr (a,b,c,d,e) where
    toJExpr (a,b,c,d,e) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c, toJExpr d, toJExpr e]
instance (ToJExpr a, ToJExpr b, ToJExpr c, ToJExpr d, ToJExpr e, ToJExpr f) => ToJExpr (a,b,c,d,e,f) where
    toJExpr (a,b,c,d,e,f) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c, toJExpr d, toJExpr e, toJExpr f]


-- | The 'ToStat' class handles injection of of things into the EDSL as a JS
-- statement. This ends up being polymorphic sugar for JS blocks, see helper
-- function 'GHC.JS.Make.expr2stat'. Instantiate for any necessary data
-- structures.
class ToStat a where
    toStat :: a -> JStat

instance ToStat JStat where
    toStat = id

instance ToStat [JStat] where
    toStat = BlockStat

instance ToStat JExpr where
    toStat = expr2stat

instance ToStat [JExpr] where
    toStat = BlockStat . map expr2stat

--------------------------------------------------------------------------------
--                        Introduction Functions
--------------------------------------------------------------------------------
-- $intro_functions
-- Introduction functions are functions that map values or terms in the Haskell
-- domain to the JS EDSL domain

-- | Create a new anonymous function. The result is a 'GHC.JS.Syntax.JExpr'
-- expression.
-- Usage:
--
-- > jLam $ \x -> jVar x + one_
-- > jLam $ \f -> (jLam $ \x -> (f `app` (x `app` x))) `app` (jLam $ \x -> (f `app` (x `app` x)))
jLam :: ToSat a => a -> JExpr
jLam f = ValExpr . UnsatVal . IS $ do
           (block,is) <- runIdentSupply $ toSat_ f []
           return $ JFunc is block

-- | Introduce a new variable into scope for the duration
-- of the enclosed expression. The result is a block statement.
-- Usage:
--
-- @jVar $ \x y -> mconcat [jVar x ||= one_, jVar y ||= two_, jVar x + jVar y]@
jVar :: ToSat a => a -> JStat
jVar f = UnsatBlock . IS $ do
           (block, is) <- runIdentSupply $ toSat_ f []
           let addDecls (BlockStat ss) =
                  BlockStat $ map decl is ++ ss
               addDecls x = x
           return $ addDecls block

-- | Create a 'for in' statement.
-- Usage:
--
-- @jForIn {expression} $ \x -> {block involving x}@
jForIn :: ToSat a => JExpr -> (JExpr -> a)  -> JStat
jForIn e f = UnsatBlock . IS $ do
               (block, is) <- runIdentSupply $ toSat_ f []
               let i = head is
               return $ decl i `mappend` ForInStat False i e block

-- | As with "jForIn" but creating a \"for each in\" statement.
jForEachIn :: ToSat a => JExpr -> (JExpr -> a) -> JStat
jForEachIn e f = UnsatBlock . IS $ do
               (block, is) <- runIdentSupply $ toSat_ f []
               let i = head is
               return $ decl i `mappend` ForInStat True i e block

-- | As with "jForIn" but creating a \"for each in\" statement.
jTryCatchFinally :: (ToSat a) => JStat -> a -> JStat -> JStat
jTryCatchFinally s f s2 = UnsatBlock . IS $ do
                     (block, is) <- runIdentSupply $ toSat_ f []
                     let i = head is
                     return $ TryStat s i block s2

-- | construct a JS variable reference
var :: FastString -> JExpr
var = ValExpr . JVar . TxtI

-- | Convert a ShortText to a Javascript String
jString :: FastString -> JExpr
jString = toJExpr

-- | Create a 'for' statement
jFor :: (ToJExpr a, ToStat b) => JStat -> a -> JStat -> b -> JStat
jFor before p after b = BlockStat [before, WhileStat False (toJExpr p) b']
    where b' = case toStat b of
                 BlockStat xs -> BlockStat $ xs ++ [after]
                 x -> BlockStat [x,after]

-- | construct a js declaration with the given identifier
decl :: Ident -> JStat
decl i = DeclStat i Nothing

-- | The empty JS HashMap
jhEmpty :: M.Map k JExpr
jhEmpty = M.empty

-- | A singleton JS HashMap
jhSingle :: (Ord k, ToJExpr a) => k -> a -> M.Map k JExpr
jhSingle k v = jhAdd k v jhEmpty

-- | insert a key-value pair into a JS HashMap
jhAdd :: (Ord k, ToJExpr a) => k -> a -> M.Map k JExpr -> M.Map k JExpr
jhAdd  k v m = M.insert k (toJExpr v) m

-- | Construct a JS HashMap from a list of key-value pairs
jhFromList :: [(FastString, JExpr)] -> JVal
jhFromList = JHash . listToUniqMap

-- | The empty JS statement
nullStat :: JStat
nullStat = BlockStat []


--------------------------------------------------------------------------------
--                             Combinators
--------------------------------------------------------------------------------
-- $combinators
-- Combinators operate on terms in the JS EDSL domain to create new terms in the
-- EDSL domain.

-- | JS infix Equality operators
(.==.), (.===.), (.!=.), (.!==.) :: JExpr -> JExpr -> JExpr
(.==.)  = InfixExpr EqOp
(.===.) = InfixExpr StrictEqOp
(.!=.)  = InfixExpr NeqOp
(.!==.) = InfixExpr StrictNeqOp

infixl 6 .==., .===., .!=., .!==.

-- | JS infix Ord operators
(.>.), (.>=.), (.<.), (.<=.) :: JExpr -> JExpr -> JExpr
(.>.)  = InfixExpr GtOp
(.>=.) = InfixExpr GeOp
(.<.)  = InfixExpr LtOp
(.<=.) = InfixExpr LeOp

infixl 7 .>., .>=., .<., .<=.

-- | JS infix bit operators
(.|.), (.||.), (.&&.)  :: JExpr -> JExpr -> JExpr
(.|.)   = InfixExpr BOrOp
(.||.)  = InfixExpr LOrOp
(.&&.)  = InfixExpr LAndOp

infixl 8 .||., .&&.

-- | JS infix bit shift operators
(.<<.), (.>>.), (.>>>.) :: JExpr -> JExpr -> JExpr
(.<<.)  = InfixExpr LeftShiftOp
(.>>.)  = InfixExpr RightShiftOp
(.>>>.) = InfixExpr ZRightShiftOp

infixl 9 .<<., .>>., .>>>.

-- | Given a 'JExpr', return the its type.
typeof :: JExpr -> JExpr
typeof = UOpExpr TypeofOp

-- | JS if-expression
--
-- > if_ e1 e2 e3 ==> e1 ? e2 : e3
if_ :: JExpr -> JExpr -> JExpr -> JExpr
if_ e1 e2 e3 = IfExpr e1 e2 e3

-- | If-expression which returns statements, see related 'ifBlockS'
--
-- > if e s1 s2 ==> if(e) { s1 } else { s2 }
ifS :: JExpr -> JStat -> JStat -> JStat
ifS e s1 s2 = IfStat e s1 s2

-- | A when-statement as syntactic sugar via `ifS`
--
-- > jwhenS cond block ==> if(cond) { block } else {  }
jwhenS :: JExpr -> JStat -> JStat
jwhenS cond block = ifS cond block mempty

-- | If-expression which returns blocks
--
-- > ifBlockS e s1 s2 ==> if(e) { s1 } else { s2 }
ifBlockS :: JExpr -> [JStat] -> [JStat] -> JStat
ifBlockS e s1 s2 = IfStat e (mconcat s1) (mconcat s2)

-- | if-expression that returns 1 if condition <=> true, 0 otherwise
--
-- > if10 e ==> e ? 1 : 0
if10 :: JExpr -> JExpr
if10 e = IfExpr e one_ zero_

-- | if-expression that returns 0 if condition <=> true, 1 otherwise
--
-- > if01 e ==> e ? 0 : 1
if01 :: JExpr -> JExpr
if01 e = IfExpr e zero_ one_

-- | an expression application, see related 'appS'
--
-- > app f xs ==> f(xs)
app :: FastString -> [JExpr] -> JExpr
app f xs = ApplExpr (var f) xs

-- | A statement application, see the expression form 'app'
appS :: FastString -> [JExpr] -> JStat
appS f xs = ApplStat (var f) xs

-- | Return a 'JExpr'
returnS :: JExpr -> JStat
returnS e = ReturnStat e

-- | "for" loop with increment at end of body
loop :: JExpr -> (JExpr -> JExpr) -> (JExpr -> JStat) -> JStat
loop initial test body = jVar $ \i ->
  mconcat [ i |= initial
          , WhileStat False (test i) (body i)
          ]

-- | "for" loop with increment at end of body
loopBlockS :: JExpr -> (JExpr -> JExpr) -> (JExpr -> [JStat]) -> JStat
loopBlockS initial test body = jVar $ \i ->
  mconcat [ i |= initial
          , WhileStat False (test i) (mconcat (body i))
          ]

-- | Prefix-increment a 'JExpr'
preIncrS :: JExpr -> JStat
preIncrS x = UOpStat PreIncOp x

-- | Postfix-increment a 'JExpr'
postIncrS :: JExpr -> JStat
postIncrS x = UOpStat PostIncOp x

-- | Prefix-decrement a 'JExpr'
preDecrS :: JExpr -> JStat
preDecrS x = UOpStat PreDecOp x

-- | Postfix-decrement a 'JExpr'
postDecrS :: JExpr -> JStat
postDecrS x = UOpStat PostDecOp x

-- | Byte indexing of o with a 64-bit offset
off64 :: JExpr -> JExpr -> JExpr
off64 o i = Add o (i .<<. three_)

-- | Byte indexing of o with a 32-bit offset
off32 :: JExpr -> JExpr -> JExpr
off32 o i = Add o (i .<<. two_)

-- | Byte indexing of o with a 16-bit offset
off16 :: JExpr -> JExpr -> JExpr
off16 o i = Add o (i .<<. one_)

-- | Byte indexing of o with a 8-bit offset
off8 :: JExpr -> JExpr -> JExpr
off8 o i = Add o i

-- | a bit mask to retrieve the lower 8-bits
mask8 :: JExpr -> JExpr
mask8 x = BAnd x (Int 0xFF)

-- | a bit mask to retrieve the lower 16-bits
mask16 :: JExpr -> JExpr
mask16 x = BAnd x (Int 0xFFFF)

-- | Sign-extend/narrow a 8-bit value
signExtend8 :: JExpr -> JExpr
signExtend8 x = (BAnd x (Int 0x7F  )) `Sub` (BAnd x (Int 0x80))

-- | Sign-extend/narrow a 16-bit value
signExtend16 :: JExpr -> JExpr
signExtend16 x = (BAnd x (Int 0x7FFF)) `Sub` (BAnd x (Int 0x8000))

-- | Select a property 'prop', from and object 'obj'
--
-- > obj .^ prop ==> obj.prop
(.^) :: JExpr -> FastString -> JExpr
obj .^ prop = SelExpr obj (TxtI prop)
infixl 8 .^

-- | Assign a variable to an expression
--
-- > foo |= expr ==> var foo = expr;
(|=) :: JExpr -> JExpr -> JStat
(|=) = AssignStat

-- | Declare a variable and then Assign the variable to an expression
--
-- > foo |= expr ==> var foo; foo = expr;
(||=) :: Ident -> JExpr -> JStat
i ||= ex = DeclStat i (Just ex)

infixl 2 ||=, |=

-- | return the expression at idx of obj
--
-- > obj .! idx ==> obj[idx]
(.!) :: JExpr -> JExpr -> JExpr
(.!) = IdxExpr

infixl 8 .!

assignAllEqual :: HasDebugCallStack => [JExpr] -> [JExpr] -> JStat
assignAllEqual xs ys = mconcat (zipWithEqual "assignAllEqual" (|=) xs ys)

assignAll :: [JExpr] -> [JExpr] -> JStat
assignAll xs ys = mconcat (zipWith (|=) xs ys)

assignAllReverseOrder :: [JExpr] -> [JExpr] -> JStat
assignAllReverseOrder xs ys = mconcat (reverse (zipWith (|=) xs ys))

declAssignAll :: [Ident] -> [JExpr] -> JStat
declAssignAll xs ys = mconcat (zipWith (||=) xs ys)

trace :: ToJExpr a => a -> JStat
trace ex = appS "h$log" [toJExpr ex]


--------------------------------------------------------------------------------
--                             Literals
--------------------------------------------------------------------------------
-- $literals
-- Literals in the JS EDSL are constants in the Haskell domain. These are useful
-- helper values and never change

-- | The JS literal 'null'
null_ :: JExpr
null_ = var "null"

-- | The JS literal 0
zero_ :: JExpr
zero_ = Int 0

-- | The JS literal 1
one_ :: JExpr
one_ = Int 1

-- | The JS literal 2
two_ :: JExpr
two_ = Int 2

-- | The JS literal 3
three_ :: JExpr
three_ = Int 3

-- | The JS literal 'undefined'
undefined_ :: JExpr
undefined_ = var "undefined"

-- | The JS literal 'true'
true_ :: JExpr
true_ = var "true"

-- | The JS literal 'false'
false_ :: JExpr
false_ = var "false"

returnStack :: JStat
returnStack = ReturnStat (ApplExpr (var "h$rs") [])


--------------------------------------------------------------------------------
--                             Math functions
--------------------------------------------------------------------------------
-- $math
-- Math functions in the EDSL are literals, with the exception of 'math_' which
-- is the sole math introduction function.

math :: JExpr
math = var "Math"

math_ :: FastString -> [JExpr] -> JExpr
math_ op args = ApplExpr (math .^ op) args

math_log, math_sin, math_cos, math_tan, math_exp, math_acos, math_asin, math_atan,
  math_abs, math_pow, math_sqrt, math_asinh, math_acosh, math_atanh, math_sign,
  math_sinh, math_cosh, math_tanh, math_expm1, math_log1p, math_fround
  :: [JExpr] -> JExpr
math_log   = math_ "log"
math_sin   = math_ "sin"
math_cos   = math_ "cos"
math_tan   = math_ "tan"
math_exp   = math_ "exp"
math_acos  = math_ "acos"
math_asin  = math_ "asin"
math_atan  = math_ "atan"
math_abs   = math_ "abs"
math_pow   = math_ "pow"
math_sign  = math_ "sign"
math_sqrt  = math_ "sqrt"
math_asinh = math_ "asinh"
math_acosh = math_ "acosh"
math_atanh = math_ "atanh"
math_sinh  = math_ "sinh"
math_cosh  = math_ "cosh"
math_tanh  = math_ "tanh"
math_expm1 = math_ "expm1"
math_log1p = math_ "log1p"
math_fround = math_ "fround"

instance Num JExpr where
    x + y = InfixExpr AddOp x y
    x - y = InfixExpr SubOp x y
    x * y = InfixExpr MulOp x y
    abs x    = math_abs [x]
    negate x = UOpExpr NegOp x
    signum x = math_sign [x]
    fromInteger x = ValExpr (JInt x)

instance Fractional JExpr where
    x / y = InfixExpr DivOp x y
    fromRational x = ValExpr (JDouble (realToFrac x))


--------------------------------------------------------------------------------
-- New Identifiers
--------------------------------------------------------------------------------

-- | The 'ToSat' class is heavily used in the Introduction function. It ensures
-- that all identifiers in the EDSL are tracked and named with an 'IdentSupply'.
class ToSat a where
    toSat_ :: a -> [Ident] -> IdentSupply (JStat, [Ident])

instance ToSat [JStat] where
    toSat_ f vs = IS $ return $ (BlockStat f, reverse vs)

instance ToSat JStat where
    toSat_ f vs = IS $ return $ (f, reverse vs)

instance ToSat JExpr where
    toSat_ f vs = IS $ return $ (toStat f, reverse vs)

instance ToSat [JExpr] where
    toSat_ f vs = IS $ return $ (BlockStat $ map expr2stat f, reverse vs)

instance (ToSat a, b ~ JExpr) => ToSat (b -> a) where
    toSat_ f vs = IS $ do
      x <- takeOneIdent
      runIdentSupply $ toSat_ (f (ValExpr $ JVar x)) (x:vs)

-- | Convert A JS expression to a JS statement where applicable. This only
-- affects applications; 'ApplExpr', If-expressions; 'IfExpr', and Unary
-- expression; 'UOpExpr'.
expr2stat :: JExpr -> JStat
expr2stat (ApplExpr x y) = (ApplStat x y)
expr2stat (IfExpr x y z) = IfStat x (expr2stat y) (expr2stat z)
expr2stat (UOpExpr o x) = UOpStat o x
expr2stat _ = nullStat

takeOneIdent :: State [Ident] Ident
takeOneIdent = do
  xxs <- get
  case xxs of
    (x:xs) -> do
      put xs
      return x
    _ -> error "takeOneIdent: empty list"

