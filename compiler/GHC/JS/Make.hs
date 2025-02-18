{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} -- only for Num, Fractional on JStgExpr

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
--     'GHC.StgToJS.Utils' for such functions.
--
--      * /Introduction/ functions
--
--           We define various primitive helpers which /introduce/ terms in the
--           EDSL, for example 'jVar', 'jLam', and 'var' and 'jString'.
--           Similarly this module exports four typeclasses 'ToExpr', 'ToStat',
--           'JVarMagic', 'JSArgument'. 'ToExpr' injects values as a JS
--           expression into the EDSL. 'ToStat' injects values as JS statements
--           into the EDSL. @JVarMagic@ provides a polymorphic way to introduce
--           a new name into the EDSL and @JSArgument@ provides a polymorphic
--           way to bind variable names for use in JS functions with different
--           arities.
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
--
--     In most cases functions prefixed with a 'j' are monadic because the
--     observably allocate. Notable exceptions are `jwhenS`, 'jString' and the
--     helpers for HashMaps.
-----------------------------------------------------------------------------
module GHC.JS.Make
  ( -- * Injection Type classes
    -- $classes
    ToJExpr(..)
  , ToStat(..)
  , JVarMagic(..)
  , JSArgument(..)
  -- * Introduction functions
  -- $intro_funcs
  , jString
  , jLam, jLam', jFunction, jFunctionSized, jFunction'
  , jVar, jVars, jFor, jForIn, jForEachIn, jTryCatchFinally
  -- * Combinators
  -- $combinators
  , (||=), (|=), (.==.), (.===.), (.!=.), (.!==.), (.!)
  , (.>.), (.>=.), (.<.), (.<=.)
  , (.<<.), (.>>.), (.>>>.)
  , (.|.), (.||.), (.&&.)
  , if_, if10, if01, ifS, ifBlockS, jBlock, jIf
  , jwhenS
  , app, appS, returnS
  , loop, loopBlockS
  , preIncrS, postIncrS
  , preDecrS, postDecrS
  , off8, off16, off32, off64
  , mask8, mask16
  , signExtend8, signExtend16
  , typeOf
  , returnStack, assignAllEqual, assignAll, assignAllReverseOrder
  , declAssignAll
  , nullStat, (.^)
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
    math_cosh, math_sinh, math_tanh, math_expm1, math_log1p, math_fround,
    math_min, math_max
  -- * Statement helpers
  , Solo(..)
  , decl
  )
where

import GHC.Prelude hiding ((.|.))

import GHC.JS.Ident
import GHC.JS.JStg.Syntax
import GHC.JS.JStg.Monad
import GHC.JS.Transform

import Control.Arrow ((***))
import Control.Monad (replicateM)
import Data.Tuple

import qualified Data.Map as M

import GHC.Data.FastString
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
    toJExpr         :: a   -> JStgExpr
    toJExprFromList :: [a] -> JStgExpr
    toJExprFromList = ValExpr . JList . map toJExpr

instance ToJExpr a => ToJExpr [a] where
    toJExpr = toJExprFromList

instance ToJExpr JStgExpr where
    toJExpr = id

instance ToJExpr () where
    toJExpr _ = ValExpr $ JList []

instance ToJExpr Bool where
    toJExpr True  = global "true"
    toJExpr False = global "false"

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
    toStat :: a -> JStgStat

instance ToStat JStgStat where
    toStat = id

instance ToStat [JStgStat] where
    toStat = BlockStat

instance ToStat JStgExpr where
    toStat = expr2stat

instance ToStat [JStgExpr] where
    toStat = BlockStat . map expr2stat

-- | Convert A JS expression to a JS statement where applicable. This only
-- affects applications; 'ApplExpr', If-expressions; 'IfExpr', and Unary
-- expression; 'UOpExpr'.
expr2stat :: JStgExpr -> JStgStat
expr2stat (ApplExpr x y) = (ApplStat x y)
expr2stat (IfExpr x y z) = IfStat x (expr2stat y) (expr2stat z)
expr2stat (UOpExpr o x) = UOpStat o x
expr2stat _ = nullStat

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
jLam :: JSArgument args => (args -> JSM JStgStat) -> JSM JStgExpr
jLam body = do xs <- args
               ValExpr . JFunc (argList xs) <$> body xs

-- | Special case of @jLam@ where the anonymous function requires no fresh
-- arguments.
jLam' :: JStgStat -> JStgExpr
jLam' body = ValExpr $ JFunc mempty body

-- | Introduce only one new variable into scope for the duration of the
-- enclosed expression. The result is a block statement. Usage:
--
-- 'jVar $ \x -> mconcat [jVar x ||= one_, ...'
jVar :: (JVarMagic t, ToJExpr t) => (t -> JSM JStgStat) -> JSM JStgStat
jVar f = jVars $ \(MkSolo only_one) -> f only_one

-- | Introduce one or many new variables into scope for the duration of the
-- enclosed expression. This function reifies the number of arguments based on
-- the container of the input function. We intentionally avoid lists and instead
-- opt for tuples because lists are not sized in general. The result is a block
-- statement. Usage:
--
-- @jVars $ \(x,y) -> mconcat [ x |= one_,  y |= two_,  x + y]@
jVars :: (JSArgument args) => (args -> JSM JStgStat) -> JSM JStgStat
jVars f = do as   <- args
             body <- f as
             return $ mconcat $ fmap decl (argList as) ++ [body]

-- | Construct a top-level function subject to JS hoisting. This combinator is
-- polymorphic over function arity so you can you use to define a JS syntax
-- object in Haskell, which is a function in JS that takes 2 or 4 or whatever
-- arguments. For a singleton function use the @Solo@ constructor @MkSolo@.
-- Usage:
--
-- an example from the Rts that defines a 1-arity JS function
-- > jFunction (global "h$getReg") (\(MkSolo n) -> return $ SwitchStat n getRegCases mempty)
--
-- an example of a two argument function from the Rts
-- > jFunction (global "h$bh_lne") (\(x, frameSize) -> bhLneStats s x frameSize)
jFunction
  :: (JSArgument args)
  => Ident                  -- ^ global name
  -> (args -> JSM JStgStat) -- ^ function body, input is locally unique generated variables
  -> JSM JStgStat
jFunction name body = do
  func_args <- args
  FuncStat name (argList func_args) <$> (body func_args)

-- | Construct a top-level function subject to JS hoisting. Special case where
-- the arity cannot be deduced from the 'args' parameter (atleast not without
-- dependent types).
jFunctionSized
  :: Ident                        -- ^ global name
  -> Int                          -- ^ Arity
  -> ([JStgExpr] -> JSM JStgStat) -- ^ function body, input is locally unique generated variables
  -> JSM JStgStat
jFunctionSized name arity body = do
  func_args <- replicateM arity newIdent
  FuncStat name func_args <$> (body $ toJExpr <$> func_args)

-- | Construct a top-level function subject to JS hoisting. Special case where
-- the function binds no parameters
jFunction'
  :: Ident        -- ^ global name
  -> JSM JStgStat -- ^ function body, input is locally unique generated variables
  -> JSM JStgStat
jFunction' name body = FuncStat name mempty <$> body

jBlock :: Monoid a => [JSM a] -> JSM a
jBlock =  fmap mconcat . sequence

-- | Create a 'for in' statement.
-- Usage:
--
-- @jForIn {expression} $ \x -> {block involving x}@
jForIn :: JStgExpr -> (JStgExpr -> JStgStat) -> JSM JStgStat
jForIn e f = do
  i <- newIdent
  return $ decl i `mappend` ForInStat False i e (f (ValExpr $! JVar i))

-- | As with "jForIn" but creating a \"for each in\" statement.
jForEachIn :: JStgExpr -> (JStgExpr -> JStgStat) -> JSM JStgStat
jForEachIn e f = do i     <- newIdent
                    return $ decl i `mappend` ForInStat True i e (f (ValExpr $! JVar i))

-- | Create a 'for' statement given a function for initialization, a predicate
-- to step to, a step and a body
-- Usage:
--
-- @ jFor (|= zero_) (.<. Int 65536) preIncrS
--        (\j -> ...something with the counter j...)@
--
jFor :: (JStgExpr -> JStgStat) -- ^ initialization function
     -> (JStgExpr -> JStgExpr) -- ^ predicate
     -> (JStgExpr -> JStgStat) -- ^ step function
     -> (JStgExpr -> JStgStat) -- ^ body
     -> JSM JStgStat
jFor init pred step body = do id <- newIdent
                              let i = ValExpr (JVar id)
                              return
                                $ decl id `mappend` ForStat (init i) (pred i) (step i) (body i)

-- | As with "jForIn" but creating a \"for each in\" statement.
jTryCatchFinally :: (Ident -> JStgStat) -> (Ident -> JStgStat) -> (Ident -> JStgStat) -> JSM JStgStat
jTryCatchFinally c f f2 = do i <- newIdent
                             return $ TryStat (c i) i (f i) (f2 i)

-- | Convert a FastString to a Javascript String
jString :: FastString -> JStgExpr
jString = toJExpr

-- | construct a js declaration with the given identifier
decl :: Ident -> JStgStat
decl i = DeclStat i Nothing

-- | The empty JS HashMap
jhEmpty :: M.Map k JStgExpr
jhEmpty = M.empty

-- | A singleton JS HashMap
jhSingle :: (Ord k, ToJExpr a) => k -> a -> M.Map k JStgExpr
jhSingle k v = jhAdd k v jhEmpty

-- | insert a key-value pair into a JS HashMap
jhAdd :: (Ord k, ToJExpr a) => k -> a -> M.Map k JStgExpr -> M.Map k JStgExpr
jhAdd  k v m = M.insert k (toJExpr v) m

-- | Construct a JS HashMap from a list of key-value pairs
jhFromList :: [(FastString, JStgExpr)] -> JVal
jhFromList = JHash . listToUniqMap

-- | The empty JS statement
nullStat :: JStgStat
nullStat = BlockStat []


--------------------------------------------------------------------------------
--                             Combinators
--------------------------------------------------------------------------------
-- $combinators
-- Combinators operate on terms in the JS EDSL domain to create new terms in the
-- EDSL domain.

-- | JS infix Equality operators
(.==.), (.===.), (.!=.), (.!==.) :: JStgExpr -> JStgExpr -> JStgExpr
(.==.)  = InfixExpr EqOp
(.===.) = InfixExpr StrictEqOp
(.!=.)  = InfixExpr NeqOp
(.!==.) = InfixExpr StrictNeqOp

infixl 6 .==., .===., .!=., .!==.

-- | JS infix Ord operators
(.>.), (.>=.), (.<.), (.<=.) :: JStgExpr -> JStgExpr -> JStgExpr
(.>.)  = InfixExpr GtOp
(.>=.) = InfixExpr GeOp
(.<.)  = InfixExpr LtOp
(.<=.) = InfixExpr LeOp

infixl 7 .>., .>=., .<., .<=.

-- | JS infix bit operators
(.|.), (.||.), (.&&.)  :: JStgExpr -> JStgExpr -> JStgExpr
(.|.)   = InfixExpr BOrOp
(.||.)  = InfixExpr LOrOp
(.&&.)  = InfixExpr LAndOp

infixl 8 .||., .&&.

-- | JS infix bit shift operators
(.<<.), (.>>.), (.>>>.) :: JStgExpr -> JStgExpr -> JStgExpr
(.<<.)  = InfixExpr LeftShiftOp
(.>>.)  = InfixExpr RightShiftOp
(.>>>.) = InfixExpr ZRightShiftOp

infixl 9 .<<., .>>., .>>>.

-- | Given a 'JStgExpr', return the its type.
typeOf :: JStgExpr -> JStgExpr
typeOf = UOpExpr TypeofOp

-- | JS if-expression
--
-- > if_ e1 e2 e3 ==> e1 ? e2 : e3
if_ :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr
if_ e1 e2 e3 = IfExpr e1 e2 e3

-- | If-expression which returns statements, see related 'ifBlockS'
--
-- > if e s1 s2 ==> if(e) { s1 } else { s2 }
ifS :: JStgExpr -> JStgStat -> JStgStat -> JStgStat
ifS e s1 s2 = IfStat e s1 s2


-- | Version of a JS if-expression which admits monadic actions in its branches
jIf :: JStgExpr -> JSM JStgStat -> JSM JStgStat -> JSM JStgStat
jIf e ma mb = do
  !a <- ma
  !b <- mb
  pure $ IfStat e a b

-- | A when-statement as syntactic sugar via `ifS`
--
-- > jwhenS cond block ==> if(cond) { block } else {  }
jwhenS :: JStgExpr -> JStgStat -> JStgStat
jwhenS cond block = IfStat cond block mempty

-- | If-expression which returns blocks
--
-- > ifBlockS e s1 s2 ==> if(e) { s1 } else { s2 }
ifBlockS :: JStgExpr -> [JStgStat] -> [JStgStat] -> JStgStat
ifBlockS e s1 s2 = IfStat e (mconcat s1) (mconcat s2)

-- | if-expression that returns 1 if condition <=> true, 0 otherwise
--
-- > if10 e ==> e ? 1 : 0
if10 :: JStgExpr -> JStgExpr
if10 e = IfExpr e one_ zero_

-- | if-expression that returns 0 if condition <=> true, 1 otherwise
--
-- > if01 e ==> e ? 0 : 1
if01 :: JStgExpr -> JStgExpr
if01 e = IfExpr e zero_ one_

-- | an application expression, see related 'appS'
--
-- > app f xs ==> f(xs)
app :: FastString -> [JStgExpr] -> JStgExpr
app f xs = ApplExpr (global f) xs

-- | A statement application, see the expression form 'app'
appS :: FastString -> [JStgExpr] -> JStgStat
appS f xs = ApplStat (global f) xs

-- | Return a 'JStgExpr'
returnS :: JStgExpr -> JStgStat
returnS e = ReturnStat e

-- | "for" loop with increment at end of body
loop :: JStgExpr -> (JStgExpr -> JStgExpr) -> (JStgExpr -> JSM JStgStat) -> JSM JStgStat
loop initial test body_ = jVar $ \i ->
  do body <- body_ i
     return $
       mconcat [ i |= initial
               , WhileStat False (test i) body
               ]

-- | "for" loop with increment at end of body
loopBlockS :: JStgExpr -> (JStgExpr -> JStgExpr) -> (JStgExpr -> [JStgStat]) -> JSM JStgStat
loopBlockS initial test body = jVar $ \i ->
  return $
  mconcat [ i |= initial
          , WhileStat False (test i) (mconcat (body i))
          ]

-- | Prefix-increment a 'JStgExpr'
preIncrS :: JStgExpr -> JStgStat
preIncrS x = UOpStat PreIncOp x

-- | Postfix-increment a 'JStgExpr'
postIncrS :: JStgExpr -> JStgStat
postIncrS x = UOpStat PostIncOp x

-- | Prefix-decrement a 'JStgExpr'
preDecrS :: JStgExpr -> JStgStat
preDecrS x = UOpStat PreDecOp x

-- | Postfix-decrement a 'JStgExpr'
postDecrS :: JStgExpr -> JStgStat
postDecrS x = UOpStat PostDecOp x

-- | Byte indexing of o with a 64-bit offset
off64 :: JStgExpr -> JStgExpr -> JStgExpr
off64 o i = Add o (i .<<. three_)

-- | Byte indexing of o with a 32-bit offset
off32 :: JStgExpr -> JStgExpr -> JStgExpr
off32 o i = Add o (i .<<. two_)

-- | Byte indexing of o with a 16-bit offset
off16 :: JStgExpr -> JStgExpr -> JStgExpr
off16 o i = Add o (i .<<. one_)

-- | Byte indexing of o with a 8-bit offset
off8 :: JStgExpr -> JStgExpr -> JStgExpr
off8 o i = Add o i

-- | a bit mask to retrieve the lower 8-bits
mask8 :: JStgExpr -> JStgExpr
mask8 x = BAnd x (Int 0xFF)

-- | a bit mask to retrieve the lower 16-bits
mask16 :: JStgExpr -> JStgExpr
mask16 x = BAnd x (Int 0xFFFF)

-- | Sign-extend/narrow a 8-bit value
signExtend8 :: JStgExpr -> JStgExpr
signExtend8 x = (BAnd x (Int 0x7F  )) `Sub` (BAnd x (Int 0x80))

-- | Sign-extend/narrow a 16-bit value
signExtend16 :: JStgExpr -> JStgExpr
signExtend16 x = (BAnd x (Int 0x7FFF)) `Sub` (BAnd x (Int 0x8000))

-- | Select a property 'prop', from and object 'obj'
--
-- > obj .^ prop ==> obj.prop
(.^) :: JStgExpr -> FastString -> JStgExpr
obj .^ prop = SelExpr obj (name prop)
infixl 8 .^

-- | Assign a variable to an expression
--
-- > foo |= expr ==> var foo = expr;
(|=) :: JStgExpr -> JStgExpr -> JStgStat
(|=) l r = AssignStat l AssignOp r

-- | Declare a variable and then Assign the variable to an expression
--
-- > foo |= expr ==> var foo; foo = expr;
(||=) :: Ident -> JStgExpr -> JStgStat
i ||= ex = DeclStat i (Just ex)

infixl 2 ||=, |=

-- | return the expression at idx of obj
--
-- > obj .! idx ==> obj[idx]
(.!) :: JStgExpr -> JStgExpr -> JStgExpr
(.!) = IdxExpr

infixl 8 .!

assignAllEqual :: HasDebugCallStack => [JStgExpr] -> [JStgExpr] -> JStgStat
assignAllEqual xs ys = mconcat (zipWithEqual (|=) xs ys)

assignAll :: [JStgExpr] -> [JStgExpr] -> JStgStat
assignAll xs ys = mconcat (zipWith (|=) xs ys)

assignAllReverseOrder :: [JStgExpr] -> [JStgExpr] -> JStgStat
assignAllReverseOrder xs ys = mconcat (reverse (zipWith (|=) xs ys))

declAssignAll :: [Ident] -> [JStgExpr] -> JStgStat
declAssignAll xs ys = mconcat (zipWith (||=) xs ys)


--------------------------------------------------------------------------------
--                             Literals
--------------------------------------------------------------------------------
-- $literals
-- Literals in the JS EDSL are constants in the Haskell domain. These are useful
-- helper values and never change

-- | The JS literal 'null'
null_ :: JStgExpr
null_ = global "null"

-- | The JS literal 0
zero_ :: JStgExpr
zero_ = Int 0

-- | The JS literal 1
one_ :: JStgExpr
one_ = Int 1

-- | The JS literal 2
two_ :: JStgExpr
two_ = Int 2

-- | The JS literal 3
three_ :: JStgExpr
three_ = Int 3

-- | The JS literal 'undefined'
undefined_ :: JStgExpr
undefined_ = global "undefined"

-- | The JS literal 'true'
true_ :: JStgExpr
true_ = ValExpr (JBool True)

-- | The JS literal 'false'
false_ :: JStgExpr
false_ = ValExpr (JBool False)

returnStack :: JStgStat
returnStack = ReturnStat (ApplExpr (global "h$rs") [])


--------------------------------------------------------------------------------
--                             Math functions
--------------------------------------------------------------------------------
-- $math
-- Math functions in the EDSL are literals, with the exception of 'math_' which
-- is the sole math introduction function.

math :: JStgExpr
math = global "Math"

math_ :: FastString -> [JStgExpr] -> JStgExpr
math_ op args = ApplExpr (math .^ op) args

math_log, math_sin, math_cos, math_tan, math_exp, math_acos, math_asin, math_atan,
  math_abs, math_pow, math_sqrt, math_asinh, math_acosh, math_atanh, math_sign,
  math_sinh, math_cosh, math_tanh, math_expm1, math_log1p, math_fround,
  math_min, math_max
  :: [JStgExpr] -> JStgExpr
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
math_min    = math_ "min"
math_max    = math_ "max"

instance Num JStgExpr where
    x + y = InfixExpr AddOp x y
    x - y = InfixExpr SubOp x y
    x * y = InfixExpr MulOp x y
    abs x    = math_abs [x]
    negate x = UOpExpr NegOp x
    signum x = math_sign [x]
    fromInteger x = ValExpr (JInt x)

instance Fractional JStgExpr where
    x / y = InfixExpr DivOp x y
    fromRational x = ValExpr (JDouble (realToFrac x))



--------------------------------------------------------------------------------
-- New Identifiers
--------------------------------------------------------------------------------

-- | Type class that generates fresh @a@'s for the JS backend. You should almost
-- never need to use this directly. Instead use @JSArgument@, for examples of
-- how to employ these classes please see @jVar@, @jFunction@ and call sites in
-- the Rts.
class JVarMagic a where
  fresh :: JSM a

-- | Type class that finds the form of arguments required for a JS syntax
-- object. This class gives us a single interface to generate variables for
-- functions that have different arities. Thus with it, we can have only one
-- @jFunction@ which is polymorphic over its arity, instead of 'jFunction2',
-- 'jFunction3' and so on.
class JSArgument args where
  argList :: args -> [Ident]
  args :: JSM args

instance JVarMagic Ident where
  fresh = newIdent

instance JVarMagic JVal where
  fresh = JVar <$> fresh

instance JVarMagic JStgExpr where
  fresh = do i <- fresh
             return $ ValExpr $ JVar i

instance (JVarMagic a, ToJExpr a) => JSArgument (Solo a) where
  argList (MkSolo a) = concatMap identsE [toJExpr a]
  args = do i <- fresh
            return $ MkSolo i

instance (JVarMagic a, JVarMagic b, ToJExpr a, ToJExpr b) => JSArgument (a,b) where
  argList (a,b) = concatMap identsE [toJExpr a , toJExpr b]
  args = (,) <$> fresh <*> fresh

instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         ) => JSArgument (a,b,c) where
  argList (a,b,c) = concatMap identsE [toJExpr a , toJExpr b, toJExpr c]
  args = (,,) <$> fresh <*> fresh <*> fresh

instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         , JVarMagic d, ToJExpr d
         ) => JSArgument (a,b,c,d) where
  argList (a,b,c,d) = concatMap identsE [toJExpr a , toJExpr b, toJExpr c, toJExpr d]
  args = (,,,) <$> fresh <*> fresh <*> fresh <*> fresh

instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         , JVarMagic d, ToJExpr d
         , JVarMagic e, ToJExpr e
         ) => JSArgument (a,b,c,d,e) where
  argList (a,b,c,d,e) = concatMap identsE [toJExpr a , toJExpr b, toJExpr c, toJExpr d, toJExpr e]
  args = (,,,,) <$> fresh <*> fresh <*> fresh <*> fresh <*> fresh

instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         , JVarMagic d, ToJExpr d
         , JVarMagic e, ToJExpr e
         , JVarMagic f, ToJExpr f
         ) => JSArgument (a,b,c,d,e,f) where
  argList (a,b,c,d,e,f) =  concatMap identsE [toJExpr a , toJExpr b, toJExpr c, toJExpr d, toJExpr e, toJExpr f]
  args = (,,,,,) <$> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh

instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         , JVarMagic d, ToJExpr d
         , JVarMagic e, ToJExpr e
         , JVarMagic f, ToJExpr f
         , JVarMagic g, ToJExpr g
         ) => JSArgument (a,b,c,d,e,f,g) where
  argList (a,b,c,d,e,f,g) = concatMap identsE [toJExpr a , toJExpr b, toJExpr c, toJExpr d, toJExpr e, toJExpr f, toJExpr g]
  args = (,,,,,,) <$> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh

instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         , JVarMagic d, ToJExpr d
         , JVarMagic e, ToJExpr e
         , JVarMagic f, ToJExpr f
         , JVarMagic g, ToJExpr g
         , JVarMagic h, ToJExpr h
         ) => JSArgument (a,b,c,d,e,f,g,h) where
  argList (a,b,c,d,e,f,g,h) =  concatMap identsE [toJExpr a , toJExpr b, toJExpr c, toJExpr d, toJExpr e, toJExpr f, toJExpr g, toJExpr h]
  args = (,,,,,,,) <$> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh

instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         , JVarMagic d, ToJExpr d
         , JVarMagic e, ToJExpr e
         , JVarMagic f, ToJExpr f
         , JVarMagic g, ToJExpr g
         , JVarMagic h, ToJExpr h
         , JVarMagic i, ToJExpr i
         ) => JSArgument (a,b,c,d,e,f,g,h,i) where
  argList (a,b,c,d,e,f,g,h,i) = concatMap identsE [toJExpr a , toJExpr b, toJExpr c, toJExpr d, toJExpr e, toJExpr f, toJExpr g, toJExpr h, toJExpr i]
  args = (,,,,,,,,) <$> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh


instance ( JVarMagic a, ToJExpr a
         , JVarMagic b, ToJExpr b
         , JVarMagic c, ToJExpr c
         , JVarMagic d, ToJExpr d
         , JVarMagic e, ToJExpr e
         , JVarMagic f, ToJExpr f
         , JVarMagic g, ToJExpr g
         , JVarMagic h, ToJExpr h
         , JVarMagic i, ToJExpr i
         , JVarMagic j, ToJExpr j
         ) => JSArgument (a,b,c,d,e,f,g,h,i,j) where
  argList (a,b,c,d,e,f,g,h,i,j) =  concatMap identsE [toJExpr a , toJExpr b, toJExpr c, toJExpr d, toJExpr e, toJExpr f, toJExpr g, toJExpr h, toJExpr i, toJExpr j]
  args = (,,,,,,,,,) <$> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh <*> fresh
