{-# LANGUAGE OverloadedStrings #-}
module Compiler.JMacro.Combinators where

import Compiler.JMacro.Base
import Compiler.JMacro.Symbols

import Prelude
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

instance Num JExpr where
    x + y = InfixExpr AddOp x y
    x - y = InfixExpr SubOp x y
    abs x = math_abs [x]
    x * y = InfixExpr MulOp x y
    negate x = UOpExpr NegOp x
    signum x = math_sign [x]
    fromInteger x = ValExpr (JInt x)

instance Fractional JExpr where
    x / y = InfixExpr DivOp x y
    fromRational x = ValExpr (JDouble (realToFrac x))

(#) :: JStat -> JStat -> JStat
(#) = mappend

infixl 1 #

instance IsString Ident where
  fromString xs = TxtI (T.pack xs)

instance IsString JExpr where
  fromString xs = ValExpr (JStr (T.pack xs))

instance IsString JVal where
  fromString xs = JStr (T.pack xs)

math :: JExpr
math = var "Math"

math_ :: Text -> [JExpr] -> JExpr
math_ op args = math .^ op .$ args

math_log, math_sin, math_cos, math_tan, math_exp, math_acos, math_asin, math_atan,
  math_abs, math_pow, math_sign, math_sqrt, math_asinh, math_acosh, math_atanh
  :: [JExpr] -> JExpr
math_log  = math_ "log"
math_sin  = math_ "sin"
math_cos  = math_ "cos"
math_tan  = math_ "tan"
math_exp  = math_ "exp"
math_acos = math_ "acos"
math_asin = math_ "asin"
math_atan = math_ "atan"
math_abs  = math_ "abs"
math_pow  = math_ "pow"
math_sign = math_ "sign"
math_sqrt = math_ "sqrt"
math_asinh = math_ "asinh"
math_acosh = math_ "acosh"
math_atanh = math_ "atanh"

(.^) :: JExpr -> Text -> JExpr
x .^ p = SelExpr x (TxtI p)

infixl 8 .^

(.$) :: JExpr -> [JExpr] -> JExpr
(.$) = ApplExpr

infixl 8 .$

(|$) :: JExpr -> [JExpr] -> JStat
(|$) = ApplStat

infixl 7 |$

(.+) :: JExpr -> JExpr -> JExpr
(.+) e1 e2 = InfixExpr AddOp e1 e2

infixl 6 .+

app :: Text -> [JExpr] -> JExpr
app f xs = ApplExpr (var f) xs

appS :: Text -> [JExpr] -> JStat
appS f xs = ApplStat (var f) xs

-- tuple returns
appT :: [JExpr] -> Text -> [JExpr] -> JStat
appT [] f xs = appS f xs
appT (r:rs) f xs =
  r |= app f xs # mconcat (zipWith (\r ret -> r |= e ret) rs (enumFrom Ret1))


(|=) :: JExpr -> JExpr -> JStat
(|=) = AssignStat

(||=) :: Ident -> JExpr -> JStat
i ||= ex = decl i # e i |= ex

(.!) :: JExpr -> JExpr -> JExpr
(.!) = IdxExpr

infixl 2 ||=, |=

infixl 8 .!

(.|.), (.||.), (.&.), (.&&.), (.^.), (.%.) :: JExpr -> JExpr -> JExpr
(.|.)  = InfixExpr BOrOp
(.||.) = InfixExpr LOrOp 
(.&.)  = InfixExpr BAndOp
(.&&.) = InfixExpr LAndOp
(.^.)  = InfixExpr BXorOp
(.%.)  = InfixExpr ModOp

infixl 8 .||., .&&., .|., .&., .%., .^.

(.<<.), (.>>.), (.>>>.) :: JExpr -> JExpr -> JExpr
(.<<.)  = InfixExpr LeftShiftOp
(.>>.)  = InfixExpr RightShiftOp
(.>>>.) = InfixExpr ZRightShiftOp

infixl 9 .<<., .>>., .>>>.

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

-- e|0  (32 bit signed integer truncation)
trunc :: JExpr -> JExpr
trunc e = e .|. 0

-- shorthand
e :: (ToJExpr a) => a -> JExpr
e = toJExpr

var :: Text -> JExpr
var xs = ValExpr (JVar $ TxtI xs)

break :: JStat
break = BreakStat Nothing

decl :: Ident -> JStat
decl i = DeclStat i

decl' :: Ident -> JExpr -> JStat
decl' i ex = decl i # e i |= ex

typeof :: JExpr -> JExpr
typeof = UOpExpr TypeofOp

returnS :: JExpr -> JStat
returnS e = ReturnStat e

returnS' :: JStat
returnS' = ReturnStat undefined_

itxt :: Ident -> Text
itxt (TxtI i) = i

-- if(e) { s1 } else { s2 }
ifS :: JExpr -> JStat -> JStat -> JStat
ifS e s1 s2 = IfStat e s1 s2

-- if(e) { s }
ifS' :: JExpr -> JStat -> JStat
ifS' e s = ifS e s mempty

-- e1 ? e2 : e3
if_ :: JExpr -> JExpr -> JExpr -> JExpr
if_ e1 e2 e3 = IfExpr e1 e2 e3

-- e ? 1 : 0
if10 :: JExpr -> JExpr
if10 e = IfExpr e 1 0

-- "for" loop with increment at end of body
loop :: JExpr -> (JExpr -> JExpr) -> (JExpr -> JStat) -> JStat
loop initial test body =
  jVar (\i -> i |= initial # WhileStat False (test i) (body i))

preIncr :: JExpr -> JExpr
preIncr x = UOpExpr PreInc x

postIncr :: JExpr -> JExpr
postIncr x = UOpExpr PostInc x

preIncrS :: JExpr -> JStat
preIncrS x = UOpStat PreInc x

postIncrS :: JExpr -> JStat
postIncrS x = UOpStat PostInc x

preDecrS :: JExpr -> JStat
preDecrS x = UOpStat PreDec x

postDecrS :: JExpr -> JStat
postDecrS x = UOpStat PostDec x


-- these are keywords in JS, but JMacro doesn't know them
null_ :: JExpr
null_ = var "null"

undefined_ :: JExpr
undefined_ = var "undefined"

true_ :: JExpr
true_ = var "true"

false_ :: JExpr
false_ = var "false"

not_ :: JExpr -> JExpr
not_ = UOpExpr NotOp

-- lifted arrays
cloneArray :: JExpr -> JExpr -> Maybe JExpr -> JExpr -> JStat
cloneArray tgt src mb_offset len
           = tgt |= (src .^ "slice" .$ [start, end]) #
             tgt .^ "m" |= 0 #
             tgt .^ "__ghcjsArray" |= true_
  where
    start = fromMaybe 0 mb_offset
    end   = maybe len (+len) mb_offset

newArray :: JExpr -> JExpr -> JExpr -> JStat
newArray tgt len elem =
    tgt |= app "h$newArray" [len, elem]
    
-- byte arrays

newByteArray :: JExpr -> JExpr -> JStat
newByteArray tgt len =
  tgt |= app "h$newByteArray" [len]

i3_, u8_, f6_, f3_, u1_ :: JExpr -> JExpr -> JExpr
i3_ a i = a .^ "i3" .! i
u8_ a i = a .^ "u8" .! i
f6_ a i = a .^ "f6" .! i
f3_ a i = a .^ "f3" .! i
u1_ a i = a .^ "u1" .! i

dv_i8, dv_i16, dv_u16, dv_i32, dv_u32, dv_f32, dv_f64 :: JExpr -> JExpr -> JExpr
dv_i8  a i = a .^ "dv" .^ "getInt8"    .$ [i, true_]
dv_i16 a i = a .^ "dv" .^ "getInt16"   .$ [i, true_]
dv_u16 a i = a .^ "dv" .^ "getUint16"  .$ [i, true_]
dv_i32 a i = a .^ "dv" .^ "getInt32"   .$ [i, true_]
dv_u32 a i = a .^ "dv" .^ "getUint32"  .$ [i, true_]
dv_f32 a i = a .^ "dv" .^ "getFloat32" .$ [i, true_]
dv_f64 a i = a .^ "dv" .^ "getFloat64" .$ [i, true_]
 

dv_s_i8, dv_s_i16, dv_s_u16, dv_s_i32, dv_s_u32, dv_s_f32, dv_s_f64 :: JExpr -> JExpr -> JExpr -> JStat
dv_s_i8  a i v = a .^ "dv" .^ "setInt8"    |$ [i, v, true_]
dv_s_u16 a i v = a .^ "dv" .^ "setUint16"  |$ [i, v, true_]
dv_s_i16 a i v = a .^ "dv" .^ "setInt16"   |$ [i, v, true_]
dv_s_i32 a i v = a .^ "dv" .^ "setInt32"   |$ [i, v, true_]
dv_s_u32 a i v = a .^ "dv" .^ "setUint32"  |$ [i, v, true_]
dv_s_f32 a i v = a .^ "dv" .^ "setFloat32" |$ [i, v, true_]
dv_s_f64 a i v = a .^ "dv" .^ "setFloat64" |$ [i, v, true_]

fetchOpByteArray :: (JExpr -> JExpr -> JExpr) -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
fetchOpByteArray op tgt src i v =
        tgt |= i3_ src i #
        i3_ src i |= op tgt v
