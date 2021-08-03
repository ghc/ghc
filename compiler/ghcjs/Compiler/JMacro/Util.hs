{-# LANGUAGE OverloadedStrings #-}

module Compiler.JMacro.Util where

import Prelude hiding (tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read, (<), (&&))

import qualified Prelude as P
import Data.Text (Text)
import Compiler.JMacro.Base

(.) :: JExpr -> Text -> JExpr
x . y = SelExpr x (TxtI y)

(<>) :: (ToJExpr a) => JExpr -> a -> JExpr
x <> y = IdxExpr x (toJExpr y)

infixl 2 =:
(=:) :: ToJExpr a => JExpr -> a -> JStat
x =:  y = AssignStat x (toJExpr y)

($) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
x $  y = ApplExpr (toJExpr x) (toJExprList y)

($$) :: (ToJExpr a, ToJExpr b) => a -> b -> JStat
x $$  y = ApplStat (toJExpr x) (toJExprList y)

(==), (!=), (<), (&&) :: JExpr -> JExpr -> JExpr
x == y = InfixExpr EqOp x y
x != y = InfixExpr NeqOp x y

infix 4 <
x < y = InfixExpr LtOp x y
infixr 3 &&
x && y = InfixExpr LAndOp x y

null :: JExpr
null  = jsv "null" -- fixme

new :: ToJExpr a => a -> JExpr
new x = UOpExpr NewOp (toJExpr x)

if' :: (ToJExpr a, ToStat b) => a -> b -> JStat
if' x y       = IfStat (toJExpr x) (toStat y) (BlockStat [])

ifElse :: (ToJExpr a, ToStat b, ToStat c) => a -> b -> c -> JStat
ifElse x y z = IfStat (toJExpr x) (toStat y) (toStat z)

while :: ToJExpr a => a -> JStat -> JStat
while x y = WhileStat False (toJExpr x) y

return :: ToJExpr a => a -> JStat
return x = ReturnStat (toJExpr x)


toJExprList :: ToJExpr a => a -> [JExpr]
toJExprList x = case toJExpr x of
                  (ValExpr (JList l)) -> l
                  x' -> [x']


jstr :: Text -> JExpr
jstr = ValExpr P.. JStr

