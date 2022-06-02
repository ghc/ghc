{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Helpers to create JS syntax values
module GHC.JS.Make
  ( ToJExpr (..)
  , ToStat (..)
  , var
  -- * Literals
  , null_
  , undefined_
  , false_
  , true_
  , zero_
  , one_
  , two_
  , three_
  -- * Hash combinators
  , jhEmpty
  , jhSingle
  , jhAdd
  , jhFromList
  -- * Combinators
  , (||=), (|=), (.==.), (.===.), (.!=.), (.!==.), (.!)
  , (.>.), (.>=.), (.<.), (.<=.)
  , (.<<.), (.>>.), (.>>>.)
  , (.||.), (.&&.)
  , if_, if10, if01, ifS, ifBlockS
  , app, appS, returnS
  , jLam, jVar, jFor, jForIn, jForEachIn, jTryCatchFinally
  , loop, loopBlockS
  , preIncrS, postIncrS
  , preDecrS, postDecrS
  , off8, off16, off32, off64
  , mask8, mask16
  , allocData, allocClsA
  , typeof
  , dataFieldName, dataFieldNames
  , returnStack, assignAllEqual, assignAll
  , declAssignAll
  , nullStat, (.^)
  )
where

import GHC.Prelude

import GHC.JS.Syntax

import Control.Arrow ((***))

import Data.Array
import qualified Data.Map as M
import qualified Data.List as List

import GHC.Utils.Outputable (Outputable (..))
import qualified GHC.Data.ShortText as ST
import GHC.Data.ShortText (ShortText)
import GHC.Utils.Monad.State.Strict
import GHC.Utils.Panic
import GHC.Utils.Misc

{--------------------------------------------------------------------
  ToJExpr Class
--------------------------------------------------------------------}


-- | Things that can be marshalled into javascript values.
-- Instantiate for any necessary data structures.
class ToJExpr a where
    toJExpr :: a -> JExpr
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

instance ToJExpr a => ToJExpr (M.Map ShortText a) where
    toJExpr = ValExpr . JHash . M.map toJExpr

instance ToJExpr a => ToJExpr (M.Map String a) where
    toJExpr = ValExpr . JHash . M.fromList . map (ST.pack *** toJExpr) . M.toList

instance ToJExpr Double where
    toJExpr = ValExpr . JDouble . SaneDouble

instance ToJExpr Int where
    toJExpr = ValExpr . JInt . fromIntegral

instance ToJExpr Integer where
    toJExpr = ValExpr . JInt

instance ToJExpr Char where
    toJExpr = ValExpr . JStr . ST.pack . (:[])
    toJExprFromList = ValExpr . JStr . ST.pack
--        where escQuotes = tailDef "" . initDef "" . show

instance ToJExpr Ident where
    toJExpr = ValExpr . JVar

instance ToJExpr ShortText where
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

{--------------------------------------------------------------------
  Block Sugar
--------------------------------------------------------------------}

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

{--------------------------------------------------------------------
  Combinators
--------------------------------------------------------------------}

-- | Create a new anonymous function. The result is an expression.
-- Usage:
-- @jLam $ \ x y -> {JExpr involving x and y}@
jLam :: ToSat a => a -> JExpr
jLam f = ValExpr . UnsatVal . IS $ do
           (block,is) <- runIdentSupply $ toSat_ f []
           return $ JFunc is block

-- | Introduce a new variable into scope for the duration
-- of the enclosed expression. The result is a block statement.
-- Usage:
-- @jVar $ \ x y -> {JExpr involving x and y}@
jVar :: ToSat a => a -> JStat
jVar f = UnsatBlock . IS $ do
           (block, is) <- runIdentSupply $ toSat_ f []
           let addDecls (BlockStat ss) =
                  BlockStat $ map DeclStat is ++ ss
               addDecls x = x
           return $ addDecls block

-- | Create a for in statement.
-- Usage:
-- @jForIn {expression} $ \x -> {block involving x}@
jForIn :: ToSat a => JExpr -> (JExpr -> a)  -> JStat
jForIn e f = UnsatBlock . IS $ do
               (block, is) <- runIdentSupply $ toSat_ f []
               let i = List.head is
               return $ DeclStat i `mappend` ForInStat False i e block

-- | As with "jForIn" but creating a \"for each in\" statement.
jForEachIn :: ToSat a => JExpr -> (JExpr -> a) -> JStat
jForEachIn e f = UnsatBlock . IS $ do
               (block, is) <- runIdentSupply $ toSat_ f []
               let i = List.head is
               return $ DeclStat i `mappend` ForInStat True i e block

jTryCatchFinally :: (ToSat a) => JStat -> a -> JStat -> JStat
jTryCatchFinally s f s2 = UnsatBlock . IS $ do
                     (block, is) <- runIdentSupply $ toSat_ f []
                     let i = List.head is
                     return $ TryStat s i block s2

var :: ShortText -> JExpr
var = ValExpr . JVar . TxtI

jFor :: (ToJExpr a, ToStat b) => JStat -> a -> JStat -> b -> JStat
jFor before p after b = BlockStat [before, WhileStat False (toJExpr p) b']
    where b' = case toStat b of
                 BlockStat xs -> BlockStat $ xs ++ [after]
                 x -> BlockStat [x,after]

jhEmpty :: M.Map k JExpr
jhEmpty = M.empty

jhSingle :: (Ord k, ToJExpr a) => k -> a -> M.Map k JExpr
jhSingle k v = jhAdd k v $ jhEmpty

jhAdd :: (Ord k, ToJExpr a) => k -> a -> M.Map k JExpr -> M.Map k JExpr
jhAdd  k v m = M.insert k (toJExpr v) m

jhFromList :: [(ShortText, JExpr)] -> JVal
jhFromList = JHash . M.fromList

nullStat :: JStat
nullStat = BlockStat []

expr2stat :: JExpr -> JStat
expr2stat (ApplExpr x y) = (ApplStat x y)
expr2stat (IfExpr x y z) = IfStat x (expr2stat y) (expr2stat z)
expr2stat (UOpExpr o x) = UOpStat o x
expr2stat _ = nullStat

(.==.), (.===.), (.!=.), (.!==.) :: JExpr -> JExpr -> JExpr
(.==.)  = InfixExpr EqOp
(.===.) = InfixExpr StrictEqOp
(.!=.)  = InfixExpr NeqOp
(.!==.) = InfixExpr StrictNeqOp

infixl 6 .==., .===., .!=., .!==.

(.>.), (.>=.), (.<.), (.<=.) :: JExpr -> JExpr -> JExpr
(.>.)  = InfixExpr GtOp
(.>=.) = InfixExpr GeOp
(.<.)  = InfixExpr LtOp
(.<=.) = InfixExpr LeOp

infixl 7 .>., .>=., .<., .<=.

(.||.), (.&&.)  :: JExpr -> JExpr -> JExpr
(.||.)  = InfixExpr LOrOp
(.&&.)  = InfixExpr LAndOp

infixl 8 .||., .&&.

(.<<.), (.>>.), (.>>>.) :: JExpr -> JExpr -> JExpr
(.<<.)  = InfixExpr LeftShiftOp
(.>>.)  = InfixExpr RightShiftOp
(.>>>.) = InfixExpr ZRightShiftOp

infixl 9 .<<., .>>., .>>>.

typeof :: JExpr -> JExpr
typeof = UOpExpr TypeofOp

-- e1 ? e2 : e3
if_ :: JExpr -> JExpr -> JExpr -> JExpr
if_ e1 e2 e3 = IfExpr e1 e2 e3

-- if(e) { s1 } else { s2 }
ifS :: JExpr -> JStat -> JStat -> JStat
ifS e s1 s2 = IfStat e s1 s2

-- if(e) { s1 } else { s2 }
ifBlockS :: JExpr -> [JStat] -> [JStat] -> JStat
ifBlockS e s1 s2 = IfStat e (mconcat s1) (mconcat s2)

-- e ? 1 : 0
if10 :: JExpr -> JExpr
if10 e = IfExpr e one_ zero_

-- e ? 0 : 1
if01 :: JExpr -> JExpr
if01 e = IfExpr e zero_ one_

app :: ShortText -> [JExpr] -> JExpr
app f xs = ApplExpr (var f) xs

appS :: ShortText -> [JExpr] -> JStat
appS f xs = ApplStat (var f) xs

returnS :: JExpr -> JStat
returnS e = ReturnStat e

-- "for" loop with increment at end of body
loop :: JExpr -> (JExpr -> JExpr) -> (JExpr -> JStat) -> JStat
loop initial test body = jVar \i -> mconcat
  [ i |= initial
  , WhileStat False (test i) (body i)
  ]

-- "for" loop with increment at end of body
loopBlockS :: JExpr -> (JExpr -> JExpr) -> (JExpr -> [JStat]) -> JStat
loopBlockS initial test body = jVar \i -> mconcat
  [ i |= initial
  , WhileStat False (test i) (mconcat (body i))
  ]

preIncrS :: JExpr -> JStat
preIncrS x = UOpStat PreIncOp x

postIncrS :: JExpr -> JStat
postIncrS x = UOpStat PostIncOp x

preDecrS :: JExpr -> JStat
preDecrS x = UOpStat PreDecOp x

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

mask8 :: JExpr -> JExpr
mask8 x = BAnd x (Int 0xFF)

mask16 :: JExpr -> JExpr
mask16 x = BAnd x (Int 0xFFFF)

null_ :: JExpr
null_ = var "null"

zero_ :: JExpr
zero_ = Int 0

one_ :: JExpr
one_ = Int 1

two_ :: JExpr
two_ = Int 2

three_ :: JExpr
three_ = Int 3

undefined_ :: JExpr
undefined_ = var "undefined"

true_ :: JExpr
true_ = var "true"

false_ :: JExpr
false_ = var "false"

(.^) :: JExpr -> ShortText -> JExpr
x .^ p = SelExpr x (TxtI p)
infixl 8 .^

returnStack :: JStat
returnStack = ReturnStat (ApplExpr (var "h$rs") [])

(|=) :: JExpr -> JExpr -> JStat
(|=) = AssignStat

(||=) :: Ident -> JExpr -> JStat
i ||= ex = DeclStat i `mappend` (toJExpr i |= ex)

infixl 2 ||=, |=

(.!) :: JExpr -> JExpr -> JExpr
(.!) = IdxExpr

infixl 8 .!

assignAllEqual :: HasDebugCallStack => [JExpr] -> [JExpr] -> JStat
assignAllEqual xs ys = mconcat (zipWithEqual "assignAllEqual" (|=) xs ys)

assignAll :: [JExpr] -> [JExpr] -> JStat
assignAll xs ys = mconcat (zipWith (|=) xs ys)

declAssignAll :: [Ident] -> [JExpr] -> JStat
declAssignAll xs ys = mconcat (zipWith (||=) xs ys)

-- | Cache "dXXX" field names
--
-- TODO: use FastString instead
dataFieldCache :: Array Int ShortText
dataFieldCache = listArray (1,nFieldCache) (map (ST.pack . ('d':) . show) [(1::Int)..nFieldCache])

nFieldCache :: Int
nFieldCache  = 16384

dataFieldName :: Int -> ShortText
dataFieldName i
  | i < 1 || i > nFieldCache = panic "dataFieldName" (ppr i)
  | otherwise                = dataFieldCache ! i

dataFieldNames :: [ShortText]
dataFieldNames = fmap dataFieldName [1..nFieldCache]


-- | Cache "h$dXXX" names
--
-- TODO: use FastString instead
dataCache :: Array Int ShortText
dataCache = listArray (1,1024) (map (ST.pack . ("h$d"++) . show) [(1::Int)..1024])

allocData :: Int -> JExpr
allocData i = toJExpr (TxtI (dataCache ! i))

-- | Cache "h$cXXX" names
--
-- TODO: use FastString instead
clsCache :: Array Int ShortText
clsCache = listArray (1,1024) (map (ST.pack . ("h$c"++) . show) [(1::Int)..1024])

allocClsA :: Int -> JExpr
allocClsA i = toJExpr (TxtI (clsCache ! i))

{--------------------------------------------------------------------
  New Identifiers
--------------------------------------------------------------------}

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

takeOneIdent :: State [Ident] Ident
takeOneIdent = do
  xxs <- get
  case xxs of
    (x:xs) -> do
      put xs
      return x
    _ -> error "takeOneIdent: empty list"


