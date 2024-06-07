{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Syntax
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
--     GHC.JS.Syntax defines the Syntax for the JS backend in GHC. It comports
--     with the [ECMA-262](https://tc39.es/ecma262/) although not every
--     production rule of the standard is represented. Code in this module is a
--     fork of [JMacro](https://hackage.haskell.org/package/jmacro) (BSD 3
--     Clause) by Gershom Bazerman, heavily modified to accomodate GHC's
--     constraints.
--
--
-- * Strategy
--
--     Nothing fancy in this module, this is a classic deeply embedded AST for
--     JS. We define numerous ADTs and pattern synonyms to make pattern matching
--     and constructing ASTs easier.
--
--
-- * Consumers
--
--     The entire JS backend consumes this module, e.g., the modules in
--     GHC.StgToJS.\*. Please see 'GHC.JS.Make' for a module which provides
--     helper functions that use the deeply embedded DSL defined in this module
--     to provide some of the benefits of a shallow embedding.
--
-----------------------------------------------------------------------------

module GHC.JS.Syntax
  ( -- * Deeply embedded JS datatypes
    JStat(..)
  , JExpr(..)
  , JVal(..)
  , Op(..)
  , UOp(..)
  , AOp(..)
  , Ident(..)
  , JLabel
  -- * pattern synonyms over JS operators
  , pattern New
  , pattern Not
  , pattern Negate
  , pattern Add
  , pattern Sub
  , pattern Mul
  , pattern Div
  , pattern Mod
  , pattern BOr
  , pattern BAnd
  , pattern BXor
  , pattern BNot
  , pattern LOr
  , pattern LAnd
  , pattern Int
  , pattern String
  , pattern Var
  , pattern PreInc
  , pattern PostInc
  , pattern PreDec
  , pattern PostDec
  -- * Utility
  , SaneDouble(..)
  , var
  , true_
  , false_
  ) where

import GHC.Prelude

import GHC.JS.Ident

import GHC.Data.FastString
import GHC.Types.Unique.Map
import GHC.Types.SaneDouble

import Control.DeepSeq

import Data.Data
import qualified Data.Semigroup as Semigroup

import GHC.Generics


--------------------------------------------------------------------------------
--                            Statements
--------------------------------------------------------------------------------
-- | JavaScript statements, see the [ECMA262
-- Reference](https://tc39.es/ecma262/#sec-ecmascript-language-statements-and-declarations)
-- for details
data JStat
  = DeclStat   !Ident !(Maybe JExpr)  -- ^ Variable declarations: var foo [= e]
  | ReturnStat JExpr                  -- ^ Return
  | IfStat     JExpr JStat JStat      -- ^ If
  | WhileStat  Bool JExpr JStat       -- ^ While, bool is "do" when True
  | ForStat    JStat JExpr JStat JStat  -- ^ For
  | ForInStat  Bool Ident JExpr JStat -- ^ For-in, bool is "each' when True
  | SwitchStat JExpr [(JExpr, JStat)] JStat  -- ^ Switch
  | TryStat    JStat Ident JStat JStat -- ^ Try
  | BlockStat  [JStat]                 -- ^ Blocks
  | ApplStat   JExpr [JExpr]           -- ^ Application
  | UOpStat UOp JExpr                  -- ^ Unary operators
  | AssignStat JExpr AOp JExpr         -- ^ Binding form: @<foo> <op> <bar>@
  | LabelStat JLabel JStat             -- ^ Statement Labels, makes me nostalgic for qbasic
  | BreakStat (Maybe JLabel)           -- ^ Break
  | ContinueStat (Maybe JLabel)        -- ^ Continue
  | FuncStat   !Ident [Ident] JStat    -- ^ an explicit function definition
  deriving (Eq, Generic)

-- | A Label used for 'JStat', specifically 'BreakStat', 'ContinueStat' and of
-- course 'LabelStat'
type JLabel = LexicalFastString

instance Semigroup JStat where
  (<>) = appendJStat

instance Monoid JStat where
  mempty = BlockStat []

-- | Append a statement to another statement. 'appendJStat' only returns a
-- 'JStat' that is /not/ a 'BlockStat' when either @mx@ or @my is an empty
-- 'BlockStat'. That is:
-- > (BlockStat [] , y           ) = y
-- > (x            , BlockStat []) = x
appendJStat :: JStat -> JStat -> JStat
appendJStat mx my = case (mx,my) of
  (BlockStat [] , y           ) -> y
  (x            , BlockStat []) -> x
  (BlockStat xs , BlockStat ys) -> BlockStat $! xs ++ ys
  (BlockStat xs , ys          ) -> BlockStat $! xs ++ [ys]
  (xs           , BlockStat ys) -> BlockStat $! xs : ys
  (xs           , ys          ) -> BlockStat [xs,ys]


--------------------------------------------------------------------------------
--                            Expressions
--------------------------------------------------------------------------------
-- | JavaScript Expressions
data JExpr
  = ValExpr    JVal              -- ^ All values are trivially expressions
  | SelExpr    JExpr Ident       -- ^ Selection: Obj.foo, see 'GHC.JS.Make..^'
  | IdxExpr    JExpr JExpr       -- ^ Indexing:  Obj[foo], see 'GHC.JS.Make..!'
  | InfixExpr  Op JExpr JExpr    -- ^ Infix Expressions, see 'JExpr' pattern synonyms
  | UOpExpr    UOp JExpr         -- ^ Unary Expressions
  | IfExpr     JExpr JExpr JExpr -- ^ If-expression
  | ApplExpr   JExpr [JExpr]     -- ^ Application
  deriving (Eq, Generic)

-- * Useful pattern synonyms to ease programming with the deeply embedded JS
--   AST. Each pattern wraps @UOp@ and @Op@ into a @JExpr@s to save typing and
--   for convienience. In addition we include a string wrapper for JS string
--   and Integer literals.

-- | pattern synonym for a unary operator new
pattern New :: JExpr -> JExpr
pattern New x = UOpExpr NewOp x

-- | pattern synonym for prefix increment @++x@
pattern PreInc :: JExpr -> JExpr
pattern PreInc x = UOpExpr PreIncOp x

-- | pattern synonym for postfix increment @x++@
pattern PostInc :: JExpr -> JExpr
pattern PostInc x = UOpExpr PostIncOp x

-- | pattern synonym for prefix decrement @--x@
pattern PreDec :: JExpr -> JExpr
pattern PreDec x = UOpExpr PreDecOp x

-- | pattern synonym for postfix decrement @--x@
pattern PostDec :: JExpr -> JExpr
pattern PostDec x = UOpExpr PostDecOp x

-- | pattern synonym for logical not @!@
pattern Not :: JExpr -> JExpr
pattern Not x = UOpExpr NotOp x

-- | pattern synonym for unary negation @-@
pattern Negate :: JExpr -> JExpr
pattern Negate x = UOpExpr NegOp x

-- | pattern synonym for addition @+@
pattern Add :: JExpr -> JExpr -> JExpr
pattern Add x y = InfixExpr AddOp x y

-- | pattern synonym for subtraction @-@
pattern Sub :: JExpr -> JExpr -> JExpr
pattern Sub x y = InfixExpr SubOp x y

-- | pattern synonym for multiplication @*@
pattern Mul :: JExpr -> JExpr -> JExpr
pattern Mul x y = InfixExpr MulOp x y

-- | pattern synonym for division @*@
pattern Div :: JExpr -> JExpr -> JExpr
pattern Div x y = InfixExpr DivOp x y

-- | pattern synonym for remainder @%@
pattern Mod :: JExpr -> JExpr -> JExpr
pattern Mod x y = InfixExpr ModOp x y

-- | pattern synonym for Bitwise Or @|@
pattern BOr :: JExpr -> JExpr -> JExpr
pattern BOr x y = InfixExpr BOrOp x y

-- | pattern synonym for Bitwise And @&@
pattern BAnd :: JExpr -> JExpr -> JExpr
pattern BAnd x y = InfixExpr BAndOp x y

-- | pattern synonym for Bitwise XOr @^@
pattern BXor :: JExpr -> JExpr -> JExpr
pattern BXor x y = InfixExpr BXorOp x y

-- | pattern synonym for Bitwise Not @~@
pattern BNot :: JExpr -> JExpr
pattern BNot x = UOpExpr BNotOp x

-- | pattern synonym for logical Or @||@
pattern LOr :: JExpr -> JExpr -> JExpr
pattern LOr x y = InfixExpr LOrOp x y

-- | pattern synonym for logical And @&&@
pattern LAnd :: JExpr -> JExpr -> JExpr
pattern LAnd x y = InfixExpr LAndOp x y

-- | pattern synonym to create integer values
pattern Int :: Integer -> JExpr
pattern Int x = ValExpr (JInt x)

-- | pattern synonym to create string values
pattern String :: FastString -> JExpr
pattern String x = ValExpr (JStr x)

-- | pattern synonym to create a local variable reference
pattern Var :: Ident -> JExpr
pattern Var x = ValExpr (JVar x)

--------------------------------------------------------------------------------
--                            Values
--------------------------------------------------------------------------------

-- | JavaScript values
data JVal
  = JVar     Ident        -- ^ A variable reference
  | JList    [JExpr]      -- ^ A JavaScript list, or what JS calls an Array
  | JDouble  SaneDouble   -- ^ A Double
  | JInt     Integer      -- ^ A BigInt
  | JStr     FastString   -- ^ A String
  | JRegEx   FastString   -- ^ A Regex
  | JBool    Bool         -- ^ A Boolean
  | JHash    (UniqMap FastString JExpr) -- ^ A JS HashMap: @{"foo": 0}@
  | JFunc    [Ident] JStat             -- ^ A function
  deriving (Eq, Generic)


--------------------------------------------------------------------------------
--                            Operators
--------------------------------------------------------------------------------

-- | JS Binary Operators. We do not deeply embed the comma operator and the
-- assignment operators
data Op
  = EqOp            -- ^ Equality:              `==`
  | StrictEqOp      -- ^ Strict Equality:       `===`
  | NeqOp           -- ^ InEquality:            `!=`
  | StrictNeqOp     -- ^ Strict InEquality      `!==`
  | GtOp            -- ^ Greater Than:          `>`
  | GeOp            -- ^ Greater Than or Equal: `>=`
  | LtOp            -- ^ Less Than:              <
  | LeOp            -- ^ Less Than or Equal:     <=
  | AddOp           -- ^ Addition:               +
  | SubOp           -- ^ Subtraction:            -
  | MulOp           -- ^ Multiplication          \*
  | DivOp           -- ^ Division:               \/
  | ModOp           -- ^ Remainder:              %
  | LeftShiftOp     -- ^ Left Shift:             \<\<
  | RightShiftOp    -- ^ Right Shift:            \>\>
  | ZRightShiftOp   -- ^ Unsigned RightShift:    \>\>\>
  | BAndOp          -- ^ Bitwise And:            &
  | BOrOp           -- ^ Bitwise Or:             |
  | BXorOp          -- ^ Bitwise XOr:            ^
  | LAndOp          -- ^ Logical And:            &&
  | LOrOp           -- ^ Logical Or:             ||
  | InstanceofOp    -- ^ @instanceof@
  | InOp            -- ^ @in@
  deriving (Show, Eq, Ord, Enum, Data, Generic)

instance NFData Op

-- | JS Unary Operators
data UOp
  = NotOp           -- ^ Logical Not: @!@
  | BNotOp          -- ^ Bitwise Not: @~@
  | NegOp           -- ^ Negation:    @-@
  | PlusOp          -- ^ Unary Plus:  @+x@
  | NewOp           -- ^ new    x
  | TypeofOp        -- ^ typeof x
  | DeleteOp        -- ^ delete x
  | YieldOp         -- ^ yield  x
  | VoidOp          -- ^ void   x
  | PreIncOp        -- ^ Prefix Increment:  @++x@
  | PostIncOp       -- ^ Postfix Increment: @x++@
  | PreDecOp        -- ^ Prefix Decrement:  @--x@
  | PostDecOp       -- ^ Postfix Decrement: @x--@
  deriving (Show, Eq, Ord, Enum, Data, Generic)

instance NFData UOp

-- | JS Unary Operators
data AOp
  = AssignOp    -- ^ Vanilla  Assignment: =
  | AddAssignOp -- ^ Addition Assignment: +=
  | SubAssignOp -- ^ Subtraction Assignment: -=
  deriving (Show, Eq, Ord, Enum, Data, Generic)

instance NFData AOp

-- | construct a JS variable reference
var :: FastString -> JExpr
var = Var . name

-- | The JS literal 'true'
true_ :: JExpr
true_ = ValExpr (JBool True)

-- | The JS literal 'false'
false_ :: JExpr
false_ = ValExpr (JBool False)
