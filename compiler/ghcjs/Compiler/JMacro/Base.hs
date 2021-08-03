{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverloadedStrings, TypeFamilies, RankNTypes, DeriveDataTypeable, FlexibleContexts, ScopedTypeVariables, GADTs, GeneralizedNewtypeDeriving, DeriveGeneric #-}

-----------------------------------------------------------------------------
{- |
Module      :  Language.Javascript.JMacro.Base
Copyright   :  (c) Gershom Bazerman, 2009
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

Simple DSL for lightweight (untyped) programmatic generation of Javascript.
-}
-----------------------------------------------------------------------------

module Compiler.JMacro.Base (
  -- * ADT
  JStat(..), JExpr(..), JVal(..), JOp(..), JUOp(..),
  Ident(..), IdentSupply(..), JsLabel,
  -- * Generic traversal (via compos)
  JMacro(..), JMGadt(..), Compos(..),
  composOp, composOpM, composOpM_, composOpFold,
  -- * Hygienic transformation
  withHygiene, scopify,
  -- * Display/Output
  renderJs, renderJs',
  renderPrefixJs, renderPrefixJs',
  JsToDoc(..), defaultRenderJs, RenderJs(..), jsToDoc,
  -- * Ad-hoc data marshalling
  ToJExpr(..),
  -- * Literals
  jsv,
  -- * Occasionally helpful combinators
  jLam, jVar, jFor, jForIn, jForEachIn, jTryCatchFinally,
  expr2stat, expr2ident, ToStat(..), nullStat,
  -- * Hash combinators
  jhEmpty, jhSingle, jhAdd, jhFromList,
  -- * Utility
  jsSaturate, SaneDouble(..),
  encodeJson
  ) where
import Prelude hiding (tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read)
-- import Control.Applicative hiding (empty)
import Control.Arrow (second, (***))
import Control.DeepSeq
import Control.Monad.State.Strict
import Control.Monad.Identity

import Data.Function
import Data.Bits ((.&.), shiftR)
import Data.Binary (Binary)
import Data.Char (toLower, isControl, ord)
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import           Data.Text (Text)
import qualified Data.Text as T
import Data.Data
import Data.Hashable (Hashable)

import GHC.Generics

import Numeric(showHex)
import Safe
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

import qualified Text.PrettyPrint.Leijen.Text as PP

-- wl-pprint-text compatibility with pretty
infixl 5 $$, $+$
($+$), ($$), ($$$) :: Doc -> Doc -> Doc
x $+$ y = x PP.<$> y
x $$ y  = align (x $+$ y)
x $$$ y = align (nest 2 $ x $+$ y)

{--------------------------------------------------------------------
  ADTs
--------------------------------------------------------------------}

newtype IdentSupply a = IS {runIdentSupply :: State [Ident] a} deriving Typeable

instance NFData (IdentSupply a) where rnf IS{} = ()

inIdentSupply :: (State [Ident] a -> State [Ident] b) -> IdentSupply a -> IdentSupply b
inIdentSupply f x = IS $ f (runIdentSupply x)

instance Data a => Data (IdentSupply a) where
    gunfold _ _ _ = error "gunfold IdentSupply"
    toConstr _ = error "toConstr IdentSupply"
    dataTypeOf _ = mkNoRepType "IdentSupply"

instance Functor IdentSupply where
    fmap f x = inIdentSupply (fmap f) x

takeOne :: State [Ident] Ident
takeOne = do
  xxs <- get
  case xxs of
    (x:xs) -> do
      put xs
      return x
    _ -> error "takeOne: empty list"

newIdentSupply :: Maybe Text -> [Ident]
newIdentSupply Nothing     = newIdentSupply (Just "jmId")
newIdentSupply (Just pfx') = [TxtI (pfx `mappend` T.pack (show x)) | x <- [(0::Integer)..]]
    where pfx = pfx' `mappend` "_"

sat_ :: IdentSupply a -> a
sat_ x = evalState (runIdentSupply x) $ newIdentSupply (Just "<<unsatId>>")

instance Eq a => Eq (IdentSupply a) where
    (==) = (==) `on` sat_
instance Ord a => Ord (IdentSupply a) where
    compare = compare `on` sat_
instance Show a => Show (IdentSupply a) where
    show x = "(" ++ show (sat_ x) ++ ")"


-- array comprehensions/generators?

-- | Statements
data JStat = DeclStat   Ident
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
             deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData JStat

type JsLabel = Text


instance Semigroup JStat where
     (BlockStat []) <> x              = x
     x <> (BlockStat [])              = x
     (BlockStat xs) <> (BlockStat ys) = BlockStat $ xs ++ ys
     (BlockStat xs) <> ys = BlockStat $ xs ++ [ys]
     xs <> (BlockStat ys) = BlockStat $ xs : ys
     xs <> ys = BlockStat [xs,ys]

instance Monoid JStat where
    mempty = BlockStat []
    mappend = (<>)



-- TODO: annotate expressions with type
-- | Expressions
data JExpr = ValExpr    JVal
           | SelExpr    JExpr Ident
           | IdxExpr    JExpr JExpr
           | InfixExpr  JOp JExpr JExpr
           | UOpExpr    JUOp JExpr
           | IfExpr     JExpr JExpr JExpr
           | ApplExpr   JExpr [JExpr]
           | UnsatExpr  (IdentSupply JExpr)
             deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData JExpr

-- | Values
data JVal = JVar     Ident
          | JList    [JExpr]
          | JDouble  SaneDouble
          | JInt     Integer
          | JStr     Text
          | JRegEx   Text
          | JHash    (M.Map Text JExpr)
          | JFunc    [Ident] JStat
          | UnsatVal (IdentSupply JVal)
            deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData JVal

data JOp =
        EqOp            -- ==
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

opText :: JOp -> TL.Text
opText EqOp          = "=="
opText StrictEqOp    = "==="
opText NeqOp         = "!="
opText StrictNeqOp   = "!=="
opText GtOp          = ">"
opText GeOp          = ">="
opText LtOp          = "<"
opText LeOp          = "<="
opText AddOp         = "+"
opText SubOp         = "-"
opText MulOp         = "*"
opText DivOp         = "/"
opText ModOp         = "%"
opText LeftShiftOp   = "<<"
opText RightShiftOp  = ">>"
opText ZRightShiftOp = ">>>"
opText BAndOp        = "&"
opText BOrOp         = "|"
opText BXorOp        = "^"
opText LAndOp        = "&&"
opText LOrOp         = "||"
opText InstanceofOp  = "instanceof"
opText InOp          = "in"

data JUOp =
        NotOp           -- !
      | BNotOp          -- ~
      | NegOp           -- -
      | PlusOp          -- +x
      | NewOp           -- new x
      | TypeofOp        -- typeof x
      | DeleteOp        -- delete x
      | YieldOp         -- yield x
      | VoidOp          -- void x
      | PreInc          -- ++x
      | PostInc         -- x++
      | PreDec          -- --x
      | PostDec         -- x--
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

instance NFData JUOp

isPre :: JUOp -> Bool
isPre PostInc = False
isPre PostDec = False
isPre _       = True

isAlphaOp :: JUOp -> Bool
isAlphaOp NewOp    = True
isAlphaOp TypeofOp = True
isAlphaOp DeleteOp = True
isAlphaOp YieldOp  = True
isAlphaOp VoidOp   = True
isAlphaOp _        = False

uOpText :: JUOp -> TL.Text
uOpText NotOp    = "!"
uOpText BNotOp   = "~"
uOpText NegOp    = "-"
uOpText PlusOp   = "+"
uOpText NewOp    = "new"
uOpText TypeofOp = "typeof"
uOpText DeleteOp = "delete"
uOpText YieldOp  = "yield"
uOpText VoidOp   = "void"
uOpText PreInc   = "++"
uOpText PostInc  = "++"
uOpText PreDec   = "--"
uOpText PostDec  = "--"

newtype SaneDouble = SaneDouble { unSaneDouble :: Double }
                   deriving (Data, Typeable, Fractional, Num, Generic, NFData)

instance Eq SaneDouble where
    (SaneDouble x) == (SaneDouble y) = x == y || (isNaN x && isNaN y)

instance Ord SaneDouble where
    compare (SaneDouble x) (SaneDouble y) = compare (fromNaN x) (fromNaN y)
        where fromNaN z | isNaN z = Nothing
                        | otherwise = Just z

instance Show SaneDouble where
    show (SaneDouble x) = show x

-- | Identifiers
newtype Ident = TxtI Text
 deriving (Show, Data, Typeable, Hashable, Eq, Ord, Binary, Generic, NFData)

expr2stat :: JExpr -> JStat
expr2stat (ApplExpr x y) = (ApplStat x y)
expr2stat (IfExpr x y z) = IfStat x (expr2stat y) (expr2stat z)
expr2stat (UOpExpr o x) = UOpStat o x
expr2stat _ = nullStat

{--------------------------------------------------------------------
  Compos
--------------------------------------------------------------------}
-- | Compos and ops for generic traversal as defined over
-- the JMacro ADT.

-- | Utility class to coerce the ADT into a regular structure.

class JMacro a where
    jtoGADT :: a -> JMGadt a
    jfromGADT :: JMGadt a -> a

instance JMacro Ident where
    jtoGADT = JMGId
    jfromGADT (JMGId x) = x

instance JMacro JStat where
    jtoGADT = JMGStat
    jfromGADT (JMGStat x) = x

instance JMacro JExpr where
    jtoGADT = JMGExpr
    jfromGADT (JMGExpr x) = x

instance JMacro JVal where
    jtoGADT = JMGVal
    jfromGADT (JMGVal x) = x

-- | Union type to allow regular traversal by compos.
data JMGadt a where
    JMGId   :: Ident -> JMGadt Ident
    JMGStat :: JStat -> JMGadt JStat
    JMGExpr :: JExpr -> JMGadt JExpr
    JMGVal  :: JVal  -> JMGadt JVal

composOp :: Compos t => (forall a. t a -> t a) -> t b -> t b
composOp f = runIdentity . composOpM (Identity . f)
composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t b -> m (t b)
composOpM = compos return ap
composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t b -> m ()
composOpM_ = composOpFold (return ()) (>>)
composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)
newtype C b a = C { unC :: b }

class Compos t where
    compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
           -> (forall a. t a -> m (t a)) -> t c -> m (t c)

instance Compos JMGadt where
    compos = jmcompos

jmcompos :: forall m c. (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b) -> (forall a. JMGadt a -> m (JMGadt a)) -> JMGadt c -> m (JMGadt c)
jmcompos ret app f' v =
    case v of
     JMGId _ -> ret v
     JMGStat v' -> ret JMGStat `app` case v' of
           DeclStat i -> ret DeclStat `app` f i
           ReturnStat i -> ret ReturnStat `app` f i
           IfStat e s s' -> ret IfStat `app` f e `app` f s `app` f s'
           WhileStat b e s -> ret (WhileStat b) `app` f e `app` f s
           ForInStat b i e s -> ret (ForInStat b) `app` f i `app` f e `app` f s
           SwitchStat e l d -> ret SwitchStat `app` f e `app` l' `app` f d
               where l' = mapM' (\(c,s) -> ret (,) `app` f c `app` f s) l
           BlockStat xs -> ret BlockStat `app` mapM' f xs
           ApplStat  e xs -> ret ApplStat `app` f e `app` mapM' f xs
           TryStat s i s1 s2 -> ret TryStat `app` f s `app` f i `app` f s1 `app` f s2
           UOpStat o e -> ret (UOpStat o) `app` f e
           AssignStat e e' -> ret AssignStat `app` f e `app` f e'
           UnsatBlock _ -> ret v'
           ContinueStat l -> ret (ContinueStat l)
           BreakStat l -> ret (BreakStat l)
           LabelStat l s -> ret (LabelStat l) `app` f s
     JMGExpr v' -> ret JMGExpr `app` case v' of
           ValExpr e -> ret ValExpr `app` f e
           SelExpr e e' -> ret SelExpr `app` f e `app` f e'
           IdxExpr e e' -> ret IdxExpr `app` f e `app` f e'
           InfixExpr o e e' -> ret (InfixExpr o) `app` f e `app` f e'
           UOpExpr o e -> ret (UOpExpr o) `app` f e
           IfExpr e e' e'' -> ret IfExpr `app` f e `app` f e' `app` f e''
           ApplExpr e xs -> ret ApplExpr `app` f e `app` mapM' f xs
           UnsatExpr _ -> ret v'
     JMGVal v' -> ret JMGVal `app` case v' of
           JVar i -> ret JVar `app` f i
           JList xs -> ret JList `app` mapM' f xs
           JDouble _ -> ret v'
           JInt    _ -> ret v'
           JStr    _ -> ret v'
           JRegEx  _ -> ret v'
           JHash   m -> ret JHash `app` m'
               where (ls, vs) = unzip (M.toList m)
                     m' = ret (M.fromAscList . zip ls) `app` mapM' f vs
           JFunc xs s -> ret JFunc `app` mapM' f xs `app` f s
           UnsatVal _ -> ret v'

  where
    mapM' :: forall a. (a -> m a) -> [a] -> m [a]
    mapM' g = foldr (app . app (ret (:)) . g) (ret [])
    f :: forall b. JMacro b => b -> m b
    f x = ret jfromGADT `app` f' (jtoGADT x)

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
    toSat_ f vs = IS $ return $ (expr2stat f, reverse vs)

instance ToSat [JExpr] where
    toSat_ f vs = IS $ return $ (BlockStat $ map expr2stat f, reverse vs)

instance (ToSat a, b ~ JExpr) => ToSat (b -> a) where
    toSat_ f vs = IS $ do
      x <- takeOne
      runIdentSupply $ toSat_ (f (ValExpr $ JVar x)) (x:vs)

{-
splitIdentSupply :: ([Ident] -> ([Ident], [Ident]))
splitIdentSupply is = (takeAlt is, takeAlt (drop 1 is))
    where takeAlt (x:_:xs) = x : takeAlt xs
          takeAlt _ = error "splitIdentSupply: stream is not infinite"
-}

{--------------------------------------------------------------------
  Saturation
--------------------------------------------------------------------}

-- | Given an optional prefix, fills in all free variable names with a supply
-- of names generated by the prefix.
jsSaturate :: (JMacro a) => Maybe Text -> a -> a
jsSaturate str x = evalState (runIdentSupply $ jsSaturate_ x) (newIdentSupply str)

jsSaturate_ :: (JMacro a) => a -> IdentSupply a
jsSaturate_ e = IS $ jfromGADT <$> go (jtoGADT e)
    where
      go :: forall a. JMGadt a -> State [Ident] (JMGadt a)
      go v = case v of
               JMGStat (UnsatBlock us) -> go =<< (JMGStat <$> runIdentSupply us)
               JMGExpr (UnsatExpr  us) -> go =<< (JMGExpr <$> runIdentSupply us)
               JMGVal  (UnsatVal   us) -> go =<< (JMGVal  <$> runIdentSupply us)
               _ -> composOpM go v

{--------------------------------------------------------------------
  Transformation
--------------------------------------------------------------------}

-- doesn't apply to unsaturated bits
jsReplace_ :: JMacro a => [(Ident, Ident)] -> a -> a
jsReplace_ xs e = jfromGADT $ go (jtoGADT e)
    where
      go :: forall a. JMGadt a -> JMGadt a
      go v = case v of
                   JMGId i -> maybe v JMGId (M.lookup i mp)
                   _ -> composOp go v
      mp = M.fromList xs

-- only works on fully saturated things
jsUnsat_ :: JMacro a => [Ident] -> a -> IdentSupply a
jsUnsat_ xs e = IS $ do
  (idents,is') <- splitAt (length xs) <$> get
  put is'
  return $ jsReplace_ (zip xs idents) e

-- | Apply a transformation to a fully saturated syntax tree,
-- taking care to return any free variables back to their free state
-- following the transformation. As the transformation preserves
-- free variables, it is hygienic.
withHygiene ::  JMacro a => (a -> a) -> a -> a
withHygiene f x = jfromGADT $ case jtoGADT x of
    JMGExpr z -> JMGExpr $ UnsatExpr $ inScope z
    JMGStat z -> JMGStat $ UnsatBlock $ inScope z
    JMGVal  z -> JMGVal $ UnsatVal $ inScope z
    JMGId _ -> jtoGADT $ f x
    where
        inScope z = IS $ do
            ti <- get
            case ti of
              ((TxtI a):b) -> do
                put b
                return $ withHygiene_ a f z
              _ -> error "withHygiene: empty list"

withHygiene_ :: JMacro a => Text -> (a -> a) -> a -> a
withHygiene_ un f x = jfromGADT $ case jtoGADT x of
    JMGStat _ -> jtoGADT $ UnsatBlock (jsUnsat_ is' x'')
    JMGExpr _ -> jtoGADT $ UnsatExpr (jsUnsat_ is' x'')
    JMGVal  _ -> jtoGADT $ UnsatVal (jsUnsat_ is' x'')
    JMGId _ -> jtoGADT $ f x
    where
        (x', (TxtI l : _)) = runState (runIdentSupply $ jsSaturate_ x) is
        is' = take lastVal is
        x'' = f x'
        lastVal = readNote ("inSat" ++ T.unpack un) (reverse . takeWhile (/= '_') . reverse $ T.unpack l) :: Int
        is = newIdentSupply $ Just ("inSat" `mappend` un)

-- | Takes a fully saturated expression and transforms it to use unique variables that respect scope.
scopify :: JStat -> JStat
scopify x = evalState (jfromGADT <$> go (jtoGADT x)) (newIdentSupply Nothing)
    where go :: forall a. JMGadt a -> State [Ident] (JMGadt a)
          go v = case v of
                   (JMGStat (BlockStat ss)) -> JMGStat . BlockStat <$>
                                             blocks ss
                       where blocks [] = return []
                             blocks (DeclStat (TxtI i) : xs)
                               | "!!" `T.isPrefixOf` i = (DeclStat (TxtI (T.drop 2 i)):) <$> blocks xs
                               | "!" `T.isPrefixOf` i  = (DeclStat (TxtI $ T.tail i):) <$> blocks xs
                               | otherwise = do
                                  xx <- get
                                  case xx of
                                    (newI:st) -> do
                                      put st
                                      rest <- blocks xs
                                      return $ [DeclStat newI `mappend` jsReplace_ [(TxtI i, newI)] (BlockStat rest)]
                                    _ -> error "scopify: empty list"
                             blocks (x':xs) = (jfromGADT <$> go (jtoGADT x')) <:> blocks xs
                             (<:>) = liftM2 (:)
                   (JMGStat (TryStat s (TxtI i) s1 s2)) -> do
                          xx <- get
                          case xx of
                            (newI:st) -> do
                              put st
                              t <- jfromGADT <$> go (jtoGADT s)
                              c <- jfromGADT <$> go (jtoGADT s1)
                              f <- jfromGADT <$> go (jtoGADT s2)
                              return . JMGStat . TryStat t newI (jsReplace_ [(TxtI i, newI)] c) $ f
                            _ -> error "scopify: empty list"
                   (JMGExpr (ValExpr (JFunc is s))) -> do
                            st <- get
                            let (newIs,newSt) = splitAt (length is) st
                            put newSt
                            rest <- jfromGADT <$> go (jtoGADT s)
                            return . JMGExpr . ValExpr $ JFunc newIs $ (jsReplace_ $ zip is newIs) rest
                   _ -> composOpM go v

{--------------------------------------------------------------------
  Pretty Printing
--------------------------------------------------------------------}

-- | Render a syntax tree as a pretty-printable document
-- (simply showing the resultant doc produces a nice,
-- well formatted String).
renderJs :: (JsToDoc a, JMacro a) => a -> Doc
renderJs = renderJs' defaultRenderJs

renderJs' :: (JsToDoc a, JMacro a) => RenderJs -> a -> Doc
renderJs' r = jsToDocR r . jsSaturate Nothing

data RenderJs = RenderJs { renderJsS :: RenderJs -> JStat -> Doc
                         , renderJsE :: RenderJs -> JExpr -> Doc
                         , renderJsV :: RenderJs -> JVal  -> Doc
                         , renderJsI :: RenderJs -> Ident -> Doc
                         }

defaultRenderJs :: RenderJs
defaultRenderJs = RenderJs defRenderJsS defRenderJsE defRenderJsV defRenderJsI

jsToDoc :: JsToDoc a => a -> Doc
jsToDoc = jsToDocR defaultRenderJs

-- | Render a syntax tree as a pretty-printable document, using a given prefix to all generated names. Use this with distinct prefixes to ensure distinct generated names between independent calls to render(Prefix)Js.
renderPrefixJs :: (JsToDoc a, JMacro a) => Text -> a -> Doc
renderPrefixJs pfx = renderPrefixJs' defaultRenderJs pfx

renderPrefixJs' :: (JsToDoc a, JMacro a) => RenderJs -> Text -> a -> Doc
renderPrefixJs' r pfx = jsToDocR r . jsSaturate (Just $ "jmId_" `mappend` pfx)

braceNest :: Doc -> Doc
braceNest x = char '{' <+> nest 2 x $$ char '}'

braceNest' :: Doc -> Doc
braceNest' x = nest 2 (char '{' $+$ x) $$ char '}'

class JsToDoc a where jsToDocR :: RenderJs -> a -> Doc
instance JsToDoc JStat where jsToDocR r = renderJsS r r
instance JsToDoc JExpr where jsToDocR r = renderJsE r r
instance JsToDoc JVal  where jsToDocR r = renderJsV r r
instance JsToDoc Ident where jsToDocR r = renderJsI r r
instance JsToDoc [JExpr] where
    jsToDocR r = vcat . map ((<> semi) . jsToDocR r)
instance JsToDoc [JStat] where
    jsToDocR r = vcat . map ((<> semi) . jsToDocR r)

defRenderJsS :: RenderJs -> JStat -> Doc
defRenderJsS r (IfStat cond x y) = text "if" <> parens (jsToDocR r cond) $$ braceNest' (jsToDocR r x) $$ mbElse
        where mbElse | y == BlockStat []  = PP.empty
                     | otherwise = text "else" $$ braceNest' (jsToDocR r y)
defRenderJsS r (DeclStat x) = text "var" <+> jsToDocR r x
defRenderJsS r (WhileStat False p b)  = text "while" <> parens (jsToDocR r p) $$ braceNest' (jsToDocR r b)
defRenderJsS r (WhileStat True  p b)  = (text "do" $$ braceNest' (jsToDocR r b)) $+$ text "while" <+> parens (jsToDocR r p)
defRenderJsS r (UnsatBlock e) = jsToDocR r $ sat_ e

defRenderJsS _ (BreakStat l) = maybe (text "break") (((<+>) `on` text) "break" . TL.fromStrict) l
defRenderJsS _ (ContinueStat l) = maybe (text "continue") (((<+>) `on` text) "continue" . TL.fromStrict) l
defRenderJsS r (LabelStat l s) = text (TL.fromStrict l) <> char ':' $$ printBS s
        where
          printBS (BlockStat ss) = vcat $ interSemi $ flattenBlocks ss
          printBS x = jsToDocR r x
          interSemi [x] = [jsToDocR r x]
          interSemi [] = []
          interSemi (x:xs) = (jsToDocR r x <> semi) : interSemi xs

defRenderJsS r (ForInStat each i e b) = text txt <> parens (jsToDocR r i <+> text "in" <+> jsToDocR r e) $$ braceNest' (jsToDocR r b)
        where txt | each = "for each"
                  | otherwise = "for"
defRenderJsS r (SwitchStat e l d) = text "switch" <+> parens (jsToDocR r e) $$ braceNest' cases
        where l' = map (\(c,s) -> (text "case" <+> parens (jsToDocR r c) <> char ':') $$$ (jsToDocR r s)) l ++ [text "default:" $$$ (jsToDocR r d)]
              cases = vcat l'
defRenderJsS r (ReturnStat e) = text "return" <+> jsToDocR r e
defRenderJsS r (ApplStat e es) = jsToDocR r e <> (parens . fillSep . punctuate comma $ map (jsToDocR r) es)
defRenderJsS r (TryStat s i s1 s2) = text "try" $$ braceNest' (jsToDocR r s) $$ mbCatch $$ mbFinally
        where mbCatch | s1 == BlockStat [] = PP.empty
                      | otherwise = text "catch" <> parens (jsToDocR r i) $$ braceNest' (jsToDocR r s1)
              mbFinally | s2 == BlockStat [] = PP.empty
                        | otherwise = text "finally" $$ braceNest' (jsToDocR r s2)
defRenderJsS r (AssignStat i x) = jsToDocR r i <+> char '=' <+> jsToDocR r x
defRenderJsS r (UOpStat op x)
        | isPre op && isAlphaOp op = text (uOpText op) <+> optParens r x
        | isPre op = text (uOpText op) <> optParens r x
        | otherwise = optParens r x <> text (uOpText op)
defRenderJsS r (BlockStat xs) = jsToDocR r (flattenBlocks xs)

flattenBlocks :: [JStat] -> [JStat]
flattenBlocks (BlockStat y:ys) = flattenBlocks y ++ flattenBlocks ys
flattenBlocks (y:ys) = y : flattenBlocks ys
flattenBlocks [] = []

optParens :: RenderJs -> JExpr -> Doc
optParens r x = case x of
                (UOpExpr _ _) -> parens (jsToDocR r x)
                _ -> jsToDocR r x

defRenderJsE :: RenderJs -> JExpr -> Doc
defRenderJsE r (ValExpr x) = jsToDocR r x
defRenderJsE r (SelExpr x y) = cat [jsToDocR r x <> char '.', jsToDocR r y]
defRenderJsE r (IdxExpr x y) = jsToDocR r x <> brackets (jsToDocR r y)
defRenderJsE r (IfExpr x y z) = parens (jsToDocR r x <+> char '?' <+> jsToDocR r y <+> char ':' <+> jsToDocR r z)
defRenderJsE r (InfixExpr op x y) = parens $ hsep [jsToDocR r x, text (opText op), jsToDocR r y]
defRenderJsE r (UOpExpr op x)
        | isPre op && isAlphaOp op = text (uOpText op) <+> optParens r x
        | isPre op = text (uOpText op) <> optParens r x
        | otherwise = optParens r x <> text (uOpText op)
defRenderJsE r (ApplExpr je xs) = jsToDocR r je <> (parens . fillSep . punctuate comma $ map (jsToDocR r) xs)
defRenderJsE r (UnsatExpr e) = jsToDocR r $ sat_ e

defRenderJsV :: RenderJs -> JVal -> Doc
defRenderJsV r (JVar i) = jsToDocR r i
defRenderJsV r (JList xs) = brackets . fillSep . punctuate comma $ map (jsToDocR r) xs
defRenderJsV _ (JDouble (SaneDouble d))
                     | d < 0 || isNegativeZero d = parens (double d)
                     | otherwise                 = double d
defRenderJsV _ (JInt i) | i < 0     = parens (integer i)
                     | otherwise = integer i
defRenderJsV _ (JStr s) = text . TL.fromChunks $ ["\"",encodeJson s,"\""]
defRenderJsV _ (JRegEx s) = text . TL.fromChunks $ ["/",s,"/"]
defRenderJsV r (JHash m)
            | M.null m = text "{}"
            | otherwise = braceNest . fillSep . punctuate comma .
                          map (\(x,y) -> squotes (text (TL.fromStrict x)) <> colon <+> jsToDocR r y) $ M.toList m
defRenderJsV r (JFunc is b) = parens $ text "function" <> parens (fillSep . punctuate comma . map (jsToDocR r) $ is) $$ braceNest' (jsToDocR r b)
defRenderJsV r (UnsatVal f) = jsToDocR r $ sat_ f

defRenderJsI :: RenderJs -> Ident -> Doc
defRenderJsI _ (TxtI t) = text (TL.fromStrict t)

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
    toJExpr True  = jsv "true"
    toJExpr False = jsv "false"

instance ToJExpr JVal where
    toJExpr = ValExpr

instance ToJExpr a => ToJExpr (M.Map Text a) where
    toJExpr = ValExpr . JHash . M.map toJExpr

instance ToJExpr a => ToJExpr (M.Map String a) where
    toJExpr = ValExpr . JHash . M.fromList . map (T.pack *** toJExpr) . M.toList

instance ToJExpr Double where
    toJExpr = ValExpr . JDouble . SaneDouble

instance ToJExpr Int where
    toJExpr = ValExpr . JInt . fromIntegral

instance ToJExpr Integer where
    toJExpr = ValExpr . JInt

instance ToJExpr Char where
    toJExpr = ValExpr . JStr . T.pack . (:[])
    toJExprFromList = ValExpr . JStr . T.pack
--        where escQuotes = tailDef "" . initDef "" . show

instance ToJExpr Ident where
    toJExpr = ValExpr . JVar

instance ToJExpr T.Text where
    toJExpr = ValExpr . JStr

instance ToJExpr TL.Text where
    toJExpr = ValExpr . JStr . TL.toStrict

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
               let i = headNote "jForIn" is
               return $ DeclStat i `mappend` ForInStat False i e block

-- | As with "jForIn" but creating a \"for each in\" statement.
jForEachIn :: ToSat a => JExpr -> (JExpr -> a) -> JStat
jForEachIn e f = UnsatBlock . IS $ do
               (block, is) <- runIdentSupply $ toSat_ f []
               let i = headNote "jForEachIn" is
               return $ DeclStat i `mappend` ForInStat True i e block

jTryCatchFinally :: (ToSat a) => JStat -> a -> JStat -> JStat
jTryCatchFinally s f s2 = UnsatBlock . IS $ do
                     (block, is) <- runIdentSupply $ toSat_ f []
                     let i = headNote "jTryCatchFinally" is
                     return $ TryStat s i block s2

jsv :: Text -> JExpr
jsv = ValExpr . JVar . TxtI

jFor :: (ToJExpr a, ToStat b) => JStat -> a -> JStat -> b -> JStat
jFor before p after b = BlockStat [before, WhileStat False (toJExpr p) b']
    where b' = case toStat b of
                 BlockStat xs -> BlockStat $ xs ++ [after]
                 x -> BlockStat [x,after]

jhEmpty :: M.Map Text JExpr
jhEmpty = M.empty

jhSingle :: ToJExpr a => Text -> a -> M.Map Text JExpr
jhSingle k v = jhAdd k v $ jhEmpty

jhAdd :: ToJExpr a => Text -> a -> M.Map Text JExpr -> M.Map Text JExpr
jhAdd  k v m = M.insert k (toJExpr v) m

jhFromList :: [(Text, JExpr)] -> JVal
jhFromList = JHash . M.fromList

nullStat :: JStat
nullStat = BlockStat []

expr2ident :: JExpr -> Ident
expr2ident (ValExpr (JVar i)) = i
expr2ident e                  = error ("expr2ident: expected (ValExpr (JVar _)), got: " ++ show e)

-- Aeson instance
instance ToJExpr Value where
    toJExpr Null             = ValExpr $ JVar $ TxtI "null"
    toJExpr (Bool b)         = ValExpr $ JVar $ TxtI $ T.pack $ map toLower (show b)
    toJExpr (Number n)       = ValExpr $ JDouble $ realToFrac n
    toJExpr (String s)       = ValExpr $ JStr $ s
    toJExpr (Array vs)       = ValExpr $ JList $ map toJExpr $ V.toList vs
    toJExpr (Object obj)     = ValExpr $ JHash $ M.fromList $ map (second toJExpr) $ HM.toList obj


encodeJson ::  Text -> Text
encodeJson = T.concatMap encodeJsonChar

encodeJsonChar :: Char -> Text
encodeJsonChar '/'  = "\\/"
encodeJsonChar '\b' = "\\b"
encodeJsonChar '\f' = "\\f"
encodeJsonChar '\n' = "\\n"
encodeJsonChar '\r' = "\\r"
encodeJsonChar '\t' = "\\t"
encodeJsonChar '"' = "\\\""
encodeJsonChar '\\' = "\\\\"
encodeJsonChar c
    | not (isControl c) && ord c <= 127 = T.singleton c
    | ord c <= 0xff   = hexxs "\\x" 2 (ord c)
    | ord c <= 0xffff = hexxs "\\u" 4 (ord c)
    | otherwise      = let cp0 = ord c - 0x10000 -- output surrogate pair
                       in hexxs "\\u" 4 ((cp0 `shiftR` 10) + 0xd800) `mappend`
                          hexxs "\\u" 4 ((cp0 .&. 0x3ff) + 0xdc00)
    where hexxs prefix pad cp =
            let h = showHex cp ""
            in  T.pack (prefix ++ replicate (pad - length h) '0' ++ h)
