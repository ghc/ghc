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

-- | JavaScript syntax
--
-- Fork of JMacro (BSD 3 Clause) by Gershom Bazerman, heavily modified to
-- accomodate GHC's constraints.
module GHC.JS.Syntax
  ( JStat(..)
  , JExpr(..)
  , JVal(..)
  , JOp(..)
  , JUOp(..)
  , Ident(..)
  , JsLabel
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
-- over this state will become yield a lot of thunks.
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

-- | Pseudo-saturate a value with garbage "<<unsatId>>" identifiers
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
-- FIXME (Jeff, 2022/03): statements according to what version of the standard?
-- | JavaScript statements
data JStat
  = DeclStat   Ident
  | ReturnStat JExpr
  | IfStat     JExpr JStat JStat
  | WhileStat  Bool JExpr JStat -- bool is "do"
  | ForInStat  Bool Ident JExpr JStat -- bool is "each"
  | SwitchStat JExpr [(JExpr, JStat)] JStat
  | TryStat    JStat Ident JStat JStat
  | BlockStat  [JStat]
  | ApplStat   JExpr [JExpr]
  | UOpStat JUOp JExpr
  | AssignStat JExpr JExpr
  | UnsatBlock (IdentSupply JStat)
  | LabelStat JsLabel JStat
  | BreakStat (Maybe JsLabel)
  | ContinueStat (Maybe JsLabel)
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData JStat

type JsLabel = ShortText


instance Semigroup JStat where
  (<>) = appendJStat

-- FIXME (Sylvain, 2022/03): should we use OrdList instead of lists in
-- BlockStat?
instance Monoid JStat where
  mempty = BlockStat []

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
-- FIXME (Jeff, 2022/03): Expressions according to what version of the standard?
-- FIXME: annotate expressions with type. This is an EDSL of JS ASTs in Haskell.
-- There are many approaches to leveraging the GHCs type system for correctness
-- guarentees in EDSLs and we should use them
-- | Expressions
data JExpr
  = ValExpr    JVal
  | SelExpr    JExpr Ident
  | IdxExpr    JExpr JExpr
  | InfixExpr  JOp JExpr JExpr
  | UOpExpr    JUOp JExpr
  | IfExpr     JExpr JExpr JExpr
  | ApplExpr   JExpr [JExpr]
  | UnsatExpr  (IdentSupply JExpr)
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Outputable JExpr where
  ppr x = O.text (show x)

instance NFData JExpr

pattern New :: JExpr -> JExpr
pattern New x = UOpExpr NewOp x

pattern PreInc :: JExpr -> JExpr
pattern PreInc x = UOpExpr PreIncOp x

pattern PostInc :: JExpr -> JExpr
pattern PostInc x = UOpExpr PostIncOp x

pattern PreDec :: JExpr -> JExpr
pattern PreDec x = UOpExpr PreDecOp x

pattern PostDec :: JExpr -> JExpr
pattern PostDec x = UOpExpr PostDecOp x

pattern Not :: JExpr -> JExpr
pattern Not x = UOpExpr NotOp x

pattern Negate :: JExpr -> JExpr
pattern Negate x = UOpExpr NegOp x

pattern Add :: JExpr -> JExpr -> JExpr
pattern Add x y = InfixExpr AddOp x y

pattern Sub :: JExpr -> JExpr -> JExpr
pattern Sub x y = InfixExpr SubOp x y

pattern Mul :: JExpr -> JExpr -> JExpr
pattern Mul x y = InfixExpr MulOp x y

pattern Div :: JExpr -> JExpr -> JExpr
pattern Div x y = InfixExpr DivOp x y

pattern Mod :: JExpr -> JExpr -> JExpr
pattern Mod x y = InfixExpr ModOp x y

pattern BOr :: JExpr -> JExpr -> JExpr
pattern BOr x y = InfixExpr BOrOp x y

pattern BAnd :: JExpr -> JExpr -> JExpr
pattern BAnd x y = InfixExpr BAndOp x y

pattern BXor :: JExpr -> JExpr -> JExpr
pattern BXor x y = InfixExpr BXorOp x y

pattern BNot :: JExpr -> JExpr
pattern BNot x = UOpExpr BNotOp x

pattern Int :: Integer -> JExpr
pattern Int x = ValExpr (JInt x)

pattern String :: ShortText -> JExpr
pattern String x = ValExpr (JStr x)

--------------------------------------------------------------------------------
--                            Values
--------------------------------------------------------------------------------
-- | Values
data JVal
  = JVar     Ident
  | JList    [JExpr]
  | JDouble  SaneDouble
  | JInt     Integer
  | JStr     ShortText
  | JRegEx   ShortText
  | JHash    (M.Map ShortText JExpr)
  | JFunc    [Ident] JStat
  | UnsatVal (IdentSupply JVal)
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Outputable JVal where
  ppr x = O.text (show x)

instance NFData JVal

--------------------------------------------------------------------------------
--                            Operators
--------------------------------------------------------------------------------
data JOp
  = EqOp            -- ==
  | StrictEqOp      -- ===
  | NeqOp           -- !=
  | StrictNeqOp     -- !==
  | GtOp            -- >
  | GeOp            -- >=
  | LtOp            -- <
  | LeOp            -- <=
  | AddOp           -- +
  | SubOp           -- -
  | MulOp           -- "*"
  | DivOp           -- /
  | ModOp           -- %
  | LeftShiftOp     -- <<
  | RightShiftOp    -- >>
  | ZRightShiftOp   -- >>>
  | BAndOp          -- &
  | BOrOp           -- |
  | BXorOp          -- ^
  | LAndOp          -- &&
  | LOrOp           -- ||
  | InstanceofOp    -- instanceof
  | InOp            -- in
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

instance NFData JOp

data JUOp
  = NotOp           -- !
  | BNotOp          -- ~
  | NegOp           -- -
  | PlusOp          -- +x
  | NewOp           -- new x
  | TypeofOp        -- typeof x
  | DeleteOp        -- delete x
  | YieldOp         -- yield x
  | VoidOp          -- void x
  | PreIncOp        -- ++x
  | PostIncOp       -- x++
  | PreDecOp        -- --x
  | PostDecOp       -- x--
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

instance NFData JUOp


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

-- | Identifiers
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

-- | Check if provided Ident is a JS keyword
isJsKeyword :: Ident -> Bool
isJsKeyword = flip Set.member jsKeywords
