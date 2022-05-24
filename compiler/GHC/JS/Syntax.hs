{-# LANGUAGE UndecidableInstances #-}
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
--     Nothing fancy in this module, this is a classicdeeply embeded AST for JS.
--     We define numerous ADTs and pattern synonyms to make pattern matching and
--     constructing ASTs easier.
--
--
-- * Consumers
--
--     The entire JS backend consumes this module, e.g., the modules in
--     GHC.StgToJS.\*. Please see 'GHC.JS.Make' for a module which provides
--     helper functions that use the deeply embedded DSL defined in this module
--     to provide some of the benefits of a shallow embedding.
-----------------------------------------------------------------------------
module GHC.JS.Syntax
  ( -- * Deeply embedded JS datatypes
    JStat(..)
  , JExpr(..)
  , JVal(..)
  , JOp(..)
  , JUOp(..)
  , Ident(..)
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
  , pattern Int
  , pattern String
  , pattern PreInc
  , pattern PostInc
  , pattern PreDec
  , pattern PostDec
  -- * Ident supply
  , IdentSupply(..)
  , newIdentSupply
  , pseudoSaturate
  -- * Utility
  , SaneDouble(..)
  -- * Keywords
  , isJsKeyword
  ) where

import GHC.Prelude

import Control.DeepSeq

import Data.Function
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Data
import Data.Word
import qualified Data.Semigroup as Semigroup

import GHC.Generics

import Data.Binary
import GHC.Utils.Outputable (Outputable (..))
import qualified GHC.Utils.Outputable as O
import qualified GHC.Data.ShortText as ST
import GHC.Data.ShortText (ShortText())
import GHC.Utils.Monad.State.Strict

-- FIXME: Jeff (2022,03): This state monad is strict, but uses a lazy list as
-- the state, since the strict state monad evaluates to WHNF, this state monad
-- will only evaluate to the first cons cell, i.e., we will be spine strict but
-- store possible huge thunks. This isn't a problem as long as we use this list
-- as a stack, but if we don't then any kind of Functor or Traverse operation
-- over this state will yield a lot of thunks.
--
-- FIXME: Jeff (2022,05): IdentSupply is quite weird, it is used in
-- GHC.JS.Make.ToSat to record new identifiers but uses a list which could be
-- empty, even though the empty case has no denotation in the domain (i.e. it is
-- a meaningless case!) and sure enough newIdentSupply makes sure we can never
-- hit this case! But it is even /more/ weird because it is a wrapper around a
-- state monad /that doesn't/ itself instantiate a state monad! So we end up
-- with a lot of weird unboxing, boxing, and running of this "monad". It is
-- almost as if it wants to redefine 'MonadTransControl'! The situation gets
-- even /more/ weird when you look at the 'GHC.JS.Make.ToSat', which has
-- numerous problems: it isn't polymorphic over the "IdentSupply" monad, of the
-- instances it defines there is only one that is monadic, it has 7 call sites
-- in JS.Make and /each one/ is fed to 'runIdentSupply'. Basically we have a
-- monad that is never called a monad and so is run all over the place to get
-- non-monadic (although still pure) values back out. To make matters worse our
-- ASTs embed this monad statically! See the UnsatFoo constuctors in JExpr,
-- JStat, and JVal. Why do my ASTs know anything about the state of the
-- interpreter!? This is quite the confusion. It confuses the AST with the code
-- that interprets the AST. The fix is to just derive the state monad with
-- generalized newtype deriving and derivingStrategies, and swap this list out
-- for something that is NonEmpty and doesn't need to be reversed all the time!
-- And clean up the mess in the ASTs.

-- | A supply of identifiers, possibly empty
newtype IdentSupply a
  = IS {runIdentSupply :: State [Ident] a}
  deriving Typeable

instance NFData (IdentSupply a) where rnf IS{} = ()

inIdentSupply :: (State [Ident] a -> State [Ident] b) -> IdentSupply a -> IdentSupply b
inIdentSupply f x = IS $ f (runIdentSupply x)

instance Functor IdentSupply where
    fmap f x = inIdentSupply (fmap f) x

newIdentSupply :: Maybe ShortText -> [Ident]
newIdentSupply Nothing    = newIdentSupply (Just "jmId")
newIdentSupply (Just pfx) = [ TxtI (mconcat [pfx,"_",ST.pack (show x)])
                            | x <- [(0::Word64)..]
                            ]

-- FIXME: Jeff (2022,05): Create note for reason behind pseudoSaturate
-- FIXME: Jeff (2022,05): make "<<unsatId>>" a constant
-- | Given a Pseudo-saturate a value with garbage @<<unsatId>>@ identifiers.
pseudoSaturate :: IdentSupply a -> a
pseudoSaturate x = evalState (runIdentSupply x) $ newIdentSupply (Just "<<unsatId>>")

instance Eq a => Eq (IdentSupply a) where
    (==) = (==) `on` pseudoSaturate
instance Ord a => Ord (IdentSupply a) where
    compare = compare `on` pseudoSaturate
instance Show a => Show (IdentSupply a) where
    show x = "(" ++ show (pseudoSaturate x) ++ ")"


--------------------------------------------------------------------------------
--                            Statements
--------------------------------------------------------------------------------
-- FIXME: Jeff (2022,05): TryStat only conforms to the largest case of the
-- standard. See [try](https://tc39.es/ecma262/#sec-try-statement), notice that
-- we only encode the case where we have: try BLOCK IDENT BLOCK BLOCK, where the
-- inner IDENT BLOCK is actually the Catch production rule. Because we've opted
-- to deeply embed only a single case we are under-specifying the other cases
-- and probably have to check for empty JStats to know which case the TryStat
-- will be. We should partition this out into its own data type.

-- FIXME: Jeff (2022,05) Remove the Bools in For and While for real data types

-- FIXME: Jeff (2022,05): Why is Application a statement and not an expression?
-- Same for Unary Operators. I guess because these are side-effectual in JS?

-- | JavaScript statements, see the [ECMA262
-- Reference](https://tc39.es/ecma262/#sec-ecmascript-language-statements-and-declarations)
-- for details
data JStat
  = DeclStat   Ident                         -- ^ Variable declarations: var foo
  | ReturnStat JExpr                         -- ^ Return
  | IfStat     JExpr JStat JStat             -- ^ If
  | WhileStat  Bool JExpr JStat              -- ^ While, bool is "do" when True
  | ForInStat  Bool Ident JExpr JStat        -- ^ For-in, bool is "each' when True
  | SwitchStat JExpr [(JExpr, JStat)] JStat  -- ^ Switch
  | TryStat    JStat Ident JStat JStat       -- ^ Try
  | BlockStat  [JStat]                       -- ^ Blocks
  | ApplStat   JExpr [JExpr]                 -- ^ Application
  | UOpStat JUOp JExpr                       -- ^ Unary operators
  | AssignStat JExpr JExpr                   -- ^ Binding form: @foo = bar@
  | UnsatBlock (IdentSupply JStat)           -- ^ /Unsaturated/ blocks see 'pseudoSaturate'
  | LabelStat JsLabel JStat                  -- ^ Statement Labels, makes me nostalgic for qbasic
  | BreakStat (Maybe JsLabel)                -- ^ Break
  | ContinueStat (Maybe JsLabel)             -- ^ Continue
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData JStat

-- | A Label used for 'JStat', specifically 'BreakStat', 'ContinueStat' and of
-- course 'LabelStat'
type JsLabel = ShortText

instance Semigroup JStat where
  (<>) = appendJStat

-- FIXME (Sylvain, 2022/03): should we use OrdList instead of lists in
-- BlockStat?
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
  (BlockStat xs , BlockStat ys) -> BlockStat $ xs ++ ys
  (BlockStat xs , ys          ) -> BlockStat $ xs ++ [ys]
  (xs           , BlockStat ys) -> BlockStat $ xs : ys
  (xs           , ys          ) -> BlockStat [xs,ys]


--------------------------------------------------------------------------------
--                            Expressions
--------------------------------------------------------------------------------
-- FIXME: annotate expressions with type. This is an EDSL of JS ASTs in Haskell.
-- There are many approaches to leveraging the GHCs type system for correctness
-- guarentees in EDSLs and we should use them
-- | JavaScript Expressions
data JExpr
  = ValExpr    JVal                 -- ^ All values are trivially expressions
  | SelExpr    JExpr Ident          -- ^ Selection: Obj.foo, see 'GHC.JS.Make..^'
  | IdxExpr    JExpr JExpr          -- ^ Indexing:  Obj[foo], see 'GHC.JS.Make..!'
  | InfixExpr  JOp JExpr JExpr      -- ^ Infix Expressions, see 'JExpr'
                                    --   pattern synonyms
  | UOpExpr    JUOp JExpr           -- ^ Unary Expressions
  | IfExpr     JExpr JExpr JExpr    -- ^ If-expression
  | ApplExpr   JExpr [JExpr]        -- ^ Application
  | UnsatExpr  (IdentSupply JExpr)  -- ^ An /Unsaturated/ expression.
                                    --   See 'pseudoSaturate'
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Outputable JExpr where
  ppr x = O.text (show x)

instance NFData JExpr

-- * Useful pattern synonyms to ease programming with the deeply embedded JS
--   AST. Each pattern wraps @JUOp@ and @JOp@ into a @JExpr@s to save typing and
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

-- | pattern synonym to create integer values
pattern Int :: Integer -> JExpr
pattern Int x = ValExpr (JInt x)

-- | pattern synonym to create string values
pattern String :: ShortText -> JExpr
pattern String x = ValExpr (JStr x)


--------------------------------------------------------------------------------
--                            Values
--------------------------------------------------------------------------------
-- | JavaScript values
data JVal
  = JVar     Ident                           -- ^ A variable reference
  | JList    [JExpr]                         -- ^ A JavaScript list, or what JS
                                             --   calls an Array
  | JDouble  SaneDouble                      -- ^ A Double
  | JInt     Integer                         -- ^ A BigInt
  | JStr     ShortText                       -- ^ A String
  | JRegEx   ShortText                       -- ^ A Regex
  | JHash    (M.Map ShortText JExpr)         -- ^ A JS HashMap: @{"foo": 0}@
  | JFunc    [Ident] JStat                   -- ^ A function
  | UnsatVal (IdentSupply JVal)              -- ^ An /Unsaturated/ value, see 'pseudoSaturate'
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Outputable JVal where
  ppr x = O.text (show x)

instance NFData JVal


--------------------------------------------------------------------------------
--                            Operators
--------------------------------------------------------------------------------
-- | JS Binary Operators. We do not deeply embed the comma operator and the
-- assignment operators
data JOp
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

instance NFData JOp

-- | JS Unary Operators
data JUOp
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

instance NFData JUOp

-- | A newtype wrapper around 'Double' to ensure we never generate a 'Double'
-- that becomes a 'NaN', see 'Eq SaneDouble', 'Ord SaneDouble' for details on
-- Sane-ness
newtype SaneDouble = SaneDouble
  { unSaneDouble :: Double
  }
  deriving (Data, Typeable, Fractional, Num, Generic, NFData)

instance Eq SaneDouble where
    (SaneDouble x) == (SaneDouble y) = x == y || (isNaN x && isNaN y)

instance Ord SaneDouble where
    compare (SaneDouble x) (SaneDouble y) = compare (fromNaN x) (fromNaN y)
        where fromNaN z | isNaN z = Nothing
                        | otherwise = Just z

instance Show SaneDouble where
    show (SaneDouble x) = show x


--------------------------------------------------------------------------------
--                            Identifiers
--------------------------------------------------------------------------------
-- We use ShortText for identifiers in JS backend

-- | A newtype wrapper around 'ShortText' for JS identifiers.
newtype Ident = TxtI { itxt:: ShortText}
 deriving stock (Show, Typeable, Ord, Eq, Generic)
 deriving newtype (Binary, NFData) -- FIXME: Jeff (2022,03): ShortText uses Data.Binary
                                   -- rather than GHC.Utils.Binary. What is the
                                   -- difference? See related FIXME in StgToJS.Object


--------------------------------------------------------------------------------
--                            JS Keywords
--------------------------------------------------------------------------------
-- | The set of Javascript keywords
jsKeywords :: Set.Set Ident
jsKeywords = Set.fromList $ TxtI <$>
           [ "break", "case", "catch", "continue", "debugger"
           , "default", "delete", "do", "else", "finally", "for"
           , "function", "if", "in", "instanceof", "new", "return"
           , "switch", "this", "throw", "try", "typeof", "var", "void"
           , "while", "with"
           , "class", "enum", "export", "extends", "import", "super"
           , "const"
           , "implements", "interface", "let", "package", "private"
           , "protected"
           , "public", "static", "yield"
           , "null", "true", "false"
           ]

-- FIXME (Jeff, 2022/05): This predicate should be encoded in the type system as
-- a newtype over Ident. Basically we should be using nominal typing so that a
-- regular Ident can never be confused with a Keyword
-- | Predicate which checks if input 'Ident' is a JS keyword or not.
isJsKeyword :: Ident -> Bool
isJsKeyword = flip Set.member jsKeywords
