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
--     Nothing fancy in this module, this is a classic deeply embeded AST for
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
  , pattern JNew
  , pattern JNot
  , pattern JNegate
  , pattern JAdd
  , pattern JSub
  , pattern JMul
  , pattern JDiv
  , pattern JMod
  , pattern JBOr
  , pattern JBAnd
  , pattern JBXor
  , pattern JBNot
  , pattern JLOr
  , pattern JLAnd
  , pattern SatInt
  , pattern JString
  , pattern JPreInc
  , pattern JPostInc
  , pattern JPreDec
  , pattern JPostDec
  -- * Utility
  , SaneDouble(..)
  , jassignAll
  , jassignAllEqual
  , jvar
  ) where

import GHC.Prelude

import GHC.JS.Unsat.Syntax (Ident(..))
import GHC.Data.FastString
import GHC.Types.Unique.Map
import GHC.Types.SaneDouble
import GHC.Utils.Misc

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
  deriving (Eq, Typeable, Generic)

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
  deriving (Eq, Typeable, Generic)

-- * Useful pattern synonyms to ease programming with the deeply embedded JS
--   AST. Each pattern wraps @JUOp@ and @JOp@ into a @JExpr@s to save typing and
--   for convienience. In addition we include a string wrapper for JS string
--   and Integer literals.

-- | pattern synonym for a unary operator new
pattern JNew :: JExpr -> JExpr
pattern JNew x = UOpExpr NewOp x

-- | pattern synonym for prefix increment @++x@
pattern JPreInc :: JExpr -> JExpr
pattern JPreInc x = UOpExpr PreIncOp x

-- | pattern synonym for postfix increment @x++@
pattern JPostInc :: JExpr -> JExpr
pattern JPostInc x = UOpExpr PostIncOp x

-- | pattern synonym for prefix decrement @--x@
pattern JPreDec :: JExpr -> JExpr
pattern JPreDec x = UOpExpr PreDecOp x

-- | pattern synonym for postfix decrement @--x@
pattern JPostDec :: JExpr -> JExpr
pattern JPostDec x = UOpExpr PostDecOp x

-- | pattern synonym for logical not @!@
pattern JNot :: JExpr -> JExpr
pattern JNot x = UOpExpr NotOp x

-- | pattern synonym for unary negation @-@
pattern JNegate :: JExpr -> JExpr
pattern JNegate x = UOpExpr NegOp x

-- | pattern synonym for addition @+@
pattern JAdd :: JExpr -> JExpr -> JExpr
pattern JAdd x y = InfixExpr AddOp x y

-- | pattern synonym for subtraction @-@
pattern JSub :: JExpr -> JExpr -> JExpr
pattern JSub x y = InfixExpr SubOp x y

-- | pattern synonym for multiplication @*@
pattern JMul :: JExpr -> JExpr -> JExpr
pattern JMul x y = InfixExpr MulOp x y

-- | pattern synonym for division @*@
pattern JDiv :: JExpr -> JExpr -> JExpr
pattern JDiv x y = InfixExpr DivOp x y

-- | pattern synonym for remainder @%@
pattern JMod :: JExpr -> JExpr -> JExpr
pattern JMod x y = InfixExpr ModOp x y

-- | pattern synonym for Bitwise Or @|@
pattern JBOr :: JExpr -> JExpr -> JExpr
pattern JBOr x y = InfixExpr BOrOp x y

-- | pattern synonym for Bitwise And @&@
pattern JBAnd :: JExpr -> JExpr -> JExpr
pattern JBAnd x y = InfixExpr BAndOp x y

-- | pattern synonym for Bitwise XOr @^@
pattern JBXor :: JExpr -> JExpr -> JExpr
pattern JBXor x y = InfixExpr BXorOp x y

-- | pattern synonym for Bitwise Not @~@
pattern JBNot :: JExpr -> JExpr
pattern JBNot x = UOpExpr BNotOp x

-- | pattern synonym for logical Or @||@
pattern JLOr :: JExpr -> JExpr -> JExpr
pattern JLOr x y = InfixExpr LOrOp x y

-- | pattern synonym for logical And @&&@
pattern JLAnd :: JExpr -> JExpr -> JExpr
pattern JLAnd x y = InfixExpr LAndOp x y

-- | pattern synonym to create integer values
pattern SatInt :: Integer -> JExpr
pattern SatInt x = ValExpr (JInt x)

-- | pattern synonym to create string values
pattern JString :: FastString -> JExpr
pattern JString x = ValExpr (JStr x)


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
  | JHash    (UniqMap FastString JExpr) -- ^ A JS HashMap: @{"foo": 0}@
  | JFunc    [Ident] JStat             -- ^ A function
  deriving (Eq, Typeable, Generic)


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
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

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
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

instance NFData UOp

-- | JS Unary Operators
data AOp
  = AssignOp    -- ^ Vanilla  Assignment: =
  | AddAssignOp -- ^ Addition Assignment: +=
  | SubAssignOp -- ^ Subtraction Assignment: -=
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

instance NFData AOp

--------------------------------------------------------------------------------
--                            Helper Functions
--------------------------------------------------------------------------------

jassignAllEqual :: [JExpr] -> [JExpr] -> JStat
jassignAllEqual xs ys = mconcat (zipWithEqual "assignAllEqual" go xs ys)
  where go l r = AssignStat l AssignOp r

jassignAll :: [JExpr] -> [JExpr] -> JStat
jassignAll xs ys = mconcat $ zipWith go xs ys
  where go l r = AssignStat l AssignOp r

jvar :: FastString -> JExpr
jvar = ValExpr . JVar . TxtI

