{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.JStg.Syntax
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
--     GHC.JS.JStg.Syntax defines the eDSL that the JS backend's runtime system
--     is written in. Nothing fancy, its just a straightforward deeply embedded
--     DSL.
--
--     In general, one should not use these constructors explicitly in the JS
--     backend. Instead, prefer using the combinators in GHC.JS.Make, if those
--     are suitable then prefer using the patterns exported from this module

-----------------------------------------------------------------------------
module GHC.JS.JStg.Syntax
  ( -- * Deeply embedded JS datatypes
    JStgStat(..)
  , JStgExpr(..)
  , JVal(..)
  , Op(..)
  , AOp(..)
  , UOp(..)
  , JsLabel
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
  , pattern Func
  , global
  , local
  ) where

import GHC.Prelude
import GHC.Utils.Outputable

import GHC.JS.Ident

import Control.DeepSeq

import Data.Data
import qualified Data.Semigroup as Semigroup

import GHC.Generics

import GHC.Data.FastString
import GHC.Types.Unique.Map
import GHC.Types.SaneDouble

--------------------------------------------------------------------------------
--                            Statements
--------------------------------------------------------------------------------
-- | JavaScript statements, see the [ECMA262
-- Reference](https://tc39.es/ecma262/#sec-ecmascript-language-statements-and-declarations)
-- for details
data JStgStat
  = DeclStat   !Ident !(Maybe JStgExpr)         -- ^ Variable declarations: var foo [= e]
  | ReturnStat JStgExpr                         -- ^ Return
  | IfStat     JStgExpr JStgStat JStgStat       -- ^ If
  | WhileStat  Bool JStgExpr JStgStat           -- ^ While, bool is "do" when True
  | ForStat    JStgStat JStgExpr JStgStat JStgStat -- ^ For
  | ForInStat  Bool Ident JStgExpr JStgStat     -- ^ For-in, bool is "each' when True
  | SwitchStat JStgExpr [(JStgExpr, JStgStat)] JStgStat  -- ^ Switch
  | TryStat    JStgStat Ident JStgStat JStgStat          -- ^ Try
  | BlockStat  [JStgStat]                       -- ^ Blocks
  | ApplStat   JStgExpr [JStgExpr]              -- ^ Application
  | UOpStat UOp JStgExpr                        -- ^ Unary operators
  | AssignStat JStgExpr AOp JStgExpr            -- ^ Binding form: @foo = bar@
  | LabelStat JsLabel JStgStat                  -- ^ Statement Labels, makes me nostalgic for qbasic
  | BreakStat (Maybe JsLabel)                   -- ^ Break
  | ContinueStat (Maybe JsLabel)                -- ^ Continue
  | FuncStat   !Ident [Ident] JStgStat          -- ^ an explicit function definition
  deriving (Eq, Generic)

-- | A Label used for 'JStgStat', specifically 'BreakStat', 'ContinueStat' and of
-- course 'LabelStat'
type JsLabel = LexicalFastString

instance Semigroup JStgStat where
  (<>) = appendJStgStat

instance Monoid JStgStat where
  mempty = BlockStat []

-- | Append a statement to another statement. 'appendJStgStat' only returns a
-- 'JStgStat' that is /not/ a 'BlockStat' when either @mx@ or @my is an empty
-- 'BlockStat'. That is:
-- > (BlockStat [] , y           ) = y
-- > (x            , BlockStat []) = x
appendJStgStat :: JStgStat -> JStgStat -> JStgStat
appendJStgStat mx my = case (mx,my) of
  (BlockStat [] , y           ) -> y
  (x            , BlockStat []) -> x
  (BlockStat xs , BlockStat ys) -> BlockStat $ xs ++ ys
  (BlockStat xs , ys          ) -> BlockStat $ xs ++ [ys]
  (xs           , BlockStat ys) -> BlockStat $ xs : ys
  (xs           , ys          ) -> BlockStat [xs,ys]


--------------------------------------------------------------------------------
--                            Expressions
--------------------------------------------------------------------------------
-- | JavaScript Expressions
data JStgExpr
  = ValExpr    JVal                 -- ^ All values are trivially expressions
  | SelExpr    JStgExpr Ident       -- ^ Selection: Obj.foo, see 'GHC.JS.Make..^'
  | IdxExpr    JStgExpr JStgExpr    -- ^ Indexing:  Obj[foo], see 'GHC.JS.Make..!'
  | InfixExpr  Op JStgExpr JStgExpr -- ^ Infix Expressions, see 'JStgExpr' pattern synonyms
  | UOpExpr    UOp JStgExpr               -- ^ Unary Expressions
  | IfExpr     JStgExpr JStgExpr JStgExpr  -- ^ If-expression
  | ApplExpr   JStgExpr [JStgExpr]         -- ^ Application
  deriving (Eq, Generic)

instance Outputable JStgExpr where
  ppr x = case x of
    ValExpr _ -> text ("ValExpr" :: String)
    SelExpr x' _ -> text ("SelExpr" :: String) <+> ppr x'
    IdxExpr x' y' -> text ("IdxExpr" :: String) <+> ppr (x', y')
    InfixExpr _ x' y' -> text ("InfixExpr" :: String) <+> ppr (x', y')
    UOpExpr _ x' -> text ("UOpExpr" :: String) <+> ppr x'
    IfExpr p t e -> text ("IfExpr" :: String) <+> ppr (p, t, e)
    ApplExpr x' xs -> text ("ApplExpr" :: String) <+> ppr (x', xs)

-- * Useful pattern synonyms to ease programming with the deeply embedded JS
--   AST. Each pattern wraps @UOp@ and @Op@ into a @JStgExpr@s to save typing and
--   for convienience. In addition we include a string wrapper for JS string
--   and Integer literals.

-- | pattern synonym for a unary operator new
pattern New :: JStgExpr -> JStgExpr
pattern New x = UOpExpr NewOp x

-- | pattern synonym for prefix increment @++x@
pattern PreInc :: JStgExpr -> JStgExpr
pattern PreInc x = UOpExpr PreIncOp x

-- | pattern synonym for postfix increment @x++@
pattern PostInc :: JStgExpr -> JStgExpr
pattern PostInc x = UOpExpr PostIncOp x

-- | pattern synonym for prefix decrement @--x@
pattern PreDec :: JStgExpr -> JStgExpr
pattern PreDec x = UOpExpr PreDecOp x

-- | pattern synonym for postfix decrement @--x@
pattern PostDec :: JStgExpr -> JStgExpr
pattern PostDec x = UOpExpr PostDecOp x

-- | pattern synonym for logical not @!@
pattern Not :: JStgExpr -> JStgExpr
pattern Not x = UOpExpr NotOp x

-- | pattern synonym for unary negation @-@
pattern Negate :: JStgExpr -> JStgExpr
pattern Negate x = UOpExpr NegOp x

-- | pattern synonym for addition @+@
pattern Add :: JStgExpr -> JStgExpr -> JStgExpr
pattern Add x y = InfixExpr AddOp x y

-- | pattern synonym for subtraction @-@
pattern Sub :: JStgExpr -> JStgExpr -> JStgExpr
pattern Sub x y = InfixExpr SubOp x y

-- | pattern synonym for multiplication @*@
pattern Mul :: JStgExpr -> JStgExpr -> JStgExpr
pattern Mul x y = InfixExpr MulOp x y

-- | pattern synonym for division @*@
pattern Div :: JStgExpr -> JStgExpr -> JStgExpr
pattern Div x y = InfixExpr DivOp x y

-- | pattern synonym for remainder @%@
pattern Mod :: JStgExpr -> JStgExpr -> JStgExpr
pattern Mod x y = InfixExpr ModOp x y

-- | pattern synonym for Bitwise Or @|@
pattern BOr :: JStgExpr -> JStgExpr -> JStgExpr
pattern BOr x y = InfixExpr BOrOp x y

-- | pattern synonym for Bitwise And @&@
pattern BAnd :: JStgExpr -> JStgExpr -> JStgExpr
pattern BAnd x y = InfixExpr BAndOp x y

-- | pattern synonym for Bitwise XOr @^@
pattern BXor :: JStgExpr -> JStgExpr -> JStgExpr
pattern BXor x y = InfixExpr BXorOp x y

-- | pattern synonym for Bitwise Not @~@
pattern BNot :: JStgExpr -> JStgExpr
pattern BNot x = UOpExpr BNotOp x

-- | pattern synonym for logical Or @||@
pattern LOr :: JStgExpr -> JStgExpr -> JStgExpr
pattern LOr x y = InfixExpr LOrOp x y

-- | pattern synonym for logical And @&&@
pattern LAnd :: JStgExpr -> JStgExpr -> JStgExpr
pattern LAnd x y = InfixExpr LAndOp x y

-- | pattern synonym to create integer values
pattern Int :: Integer -> JStgExpr
pattern Int x = ValExpr (JInt x)

-- | pattern synonym to create string values
pattern String :: FastString -> JStgExpr
pattern String x = ValExpr (JStr x)

-- | pattern synonym to create a local variable reference
pattern Var :: Ident -> JStgExpr
pattern Var x = ValExpr (JVar x)

-- | pattern synonym to create an anonymous function
pattern Func :: [Ident] -> JStgStat -> JStgExpr
pattern Func args body = ValExpr (JFunc args body)

--------------------------------------------------------------------------------
--                            Values
--------------------------------------------------------------------------------
-- | JavaScript values
data JVal
  = JVar     Ident                      -- ^ A variable reference
  | JList    [JStgExpr]                 -- ^ A JavaScript list, or what JS
                                        --   calls an Array
  | JDouble  SaneDouble                 -- ^ A Double
  | JInt     Integer                    -- ^ A BigInt
  | JStr     FastString                 -- ^ A String
  | JRegEx   FastString                 -- ^ A Regex
  | JBool    Bool                       -- ^ A Boolean
  | JHash    (UniqMap FastString JStgExpr) -- ^ A JS HashMap: @{"foo": 0}@
  | JFunc    [Ident] JStgStat              -- ^ A function
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

-- | construct a JS reference, intended to refer to a global name
global :: FastString -> JStgExpr
global = Var . name

-- | construct a JS reference, intended to refer to a local name
local :: FastString -> JStgExpr
local = Var . name
