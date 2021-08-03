{-# LANGUAGE
             TupleSections,
             DeriveDataTypeable,
             ScopedTypeVariables,
             GADTs,
             OverloadedStrings #-}

{-
   Dataflow analysis on the bastard child of a JavaScript AST and a control flow graph

   some valid JavaScript syntax may be unsupported here
-}

module Gen2.Dataflow ( Graph(..), nodes, arcsIn, arcsOut, entry, nodeid, labels, idents, identsR
                     , lookupId, lookupIdent, addIdents
                     , Node(..)
                     , Expr(..), eBool, AExpr(..), exprIdents, exprIdents'
                     , SimpleStat(..)
                     , Val(..)
                     , Id, IdSet
                     , isEqOp, isBoolOp
                     , NodeId
                     , Arc(..), arcFrom, arcTo, arcType
                     , ArcType(..)
                     , Facts(..)
                     , localVarsGraph
                     , noFacts, addFact, lookupFact
                     , cfg
                     , unCfg
                     , Forward(..), Backward(..)
                     , constForward, constBackward
                     , foldForward, foldBackward
                     , orderedNodes
                     , lookupNode
                     ) where

import           Control.Arrow (second)
-- import           Control.Lens hiding (op)
import           Compiler.JMacro.Lens
import           Control.Monad.State
import           Data.Bits (shiftL, shiftR, (.|.), (.&.))
-- import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.IntSet as IS
import           Data.IntSet (IntSet)
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Text (Text)
import           Compiler.JMacro
import           Data.Typeable
import           Data.Data
import Prelude
import Data.Function

type Id    = Int
type IdSet = IntSet
type IdS   = State (HashMap Ident Int)

-- | Annotated expression, store identifiers so that rewrites can be applied more quickly
data AExpr a = AExpr { getAnnot :: a
                     , fromA    :: Expr
                     }
  deriving (Show, Eq, Ord, Data, Typeable)

-- optimized way to get identifiers from a local function body
-- (does not include x1 for a.x1, even though it's stored as TxtI)
exprIdents :: Expr -> IntSet
exprIdents e = exprIdentsI IS.empty IS.insert e

exprIdents' :: Expr -> IntMap Int
exprIdents' e = exprIdentsI IM.empty (\x -> IM.insertWith (+) x 1) e

{-# INLINE exprIdentsI #-}
exprIdentsI :: a -> (Id -> a -> a) -> Expr -> a
exprIdentsI start c e = goE start e
  where
    goE s (ValE v)   = goV s v
    goE s (SelE e _) = goE s e -- ignore the selected thing
    goE s (IdxE e1 e2)     = go2 s e1 e2
    goE s (BOpE _ e1 e2)   = go2 s e1 e2
    goE s (UOpE _ e)       = goE s e
    goE s (CondE e1 e2 e3) = goE (go2 s e1 e2) e3
    goE s (ApplE e es)     = foldl' goE s (e:es)
    go2 s e1 e2 = let s' = goE s e1
                  in  s' `seq` goE s' e2
    goV s (Var i)   = i `c` s
    goV s (ListV es) = foldl' goE s es
    goV s (HashV m)  = foldl' goE s (M.elems m) -- ignore the keys
    goV s _         = s

data Expr =
        ValE  Val
      | SelE  Expr Id
      | IdxE  Expr Expr
      | BOpE  JOp Expr Expr
      | UOpE  JUOp Expr
      | CondE Expr Expr Expr
      | ApplE Expr [Expr]
  deriving (Show, Eq, Ord, Data, Typeable)

eBool :: Bool -> Expr
eBool = ValE . BoolV

fromJExpr :: JExpr -> IdS Expr
fromJExpr (ValExpr v)         = ValE  <$> fromJVal v
fromJExpr (IdxExpr e1 e2)     = IdxE  <$> fromJExpr e1 <*> fromJExpr e2
fromJExpr (InfixExpr o e1 e2) = BOpE  <$> pure o <*> fromJExpr e1 <*> fromJExpr e2
fromJExpr (UOpExpr o e)       = UOpE  <$> pure o <*> fromJExpr e
fromJExpr (IfExpr e1 e2 e3)   = CondE <$> fromJExpr e1 <*> fromJExpr e2 <*> fromJExpr e3
fromJExpr (ApplExpr e1 es)    = ApplE <$> fromJExpr e1 <*> mapM fromJExpr es
fromJExpr (SelExpr e i)       = SelE  <$> fromJExpr e <*> fromJIdent i
fromJExpr UnsatExpr{}         = error "fromJExpr: unsaturated expression"

toJExpr' :: IntMap Ident -> AExpr a -> JExpr
toJExpr' m (AExpr _ e) = exprToJExpr m e

exprToJExpr :: IntMap Ident -> Expr -> JExpr
exprToJExpr m (ValE v)   = ValExpr (toJVal m v)
exprToJExpr m (SelE e i) =
  case IM.lookup i m of
    Just i' -> SelExpr (exprToJExpr m e) i'
    Nothing -> error ("toJExpr: unknown identifier: " ++ show i)
exprToJExpr m (IdxE e1 e2) = IdxExpr (exprToJExpr m e1) (exprToJExpr m e2)
exprToJExpr m (BOpE op e1 e2) = InfixExpr op (exprToJExpr m e1) (exprToJExpr m e2)
exprToJExpr m (UOpE op e) = UOpExpr op (exprToJExpr m e)
exprToJExpr m (CondE e1 e2 e3) = IfExpr (exprToJExpr m e1) (exprToJExpr m e2) (exprToJExpr m e3)
exprToJExpr m (ApplE e es) = ApplExpr (exprToJExpr m e) (map (exprToJExpr m) es)

{-
data UnOp =
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
  deriving (Show, Eq, Ord, Enum, Data, Typeable)

fromJUOp :: Bool -> String -> UnOp
fromJUOp True  "!"      = NotOp
fromJUOp True  "~"      = BNotOp
fromJUOp True  "-"      = NegOp
fromJUOp True  "+"      = PlusOp
fromJUOp True  "new"    = NewOp
fromJUOp True  "typeof" = TypeofOp
fromJUOp True  "yield"  = YieldOp
fromJUOp True  "delete" = DeleteOp
fromJUOp True  "void"   = VoidOp
fromJUOp True  "++"     = PreInc
fromJUOp True  "--"     = PreDec
fromJUOp False "++"     = PostInc
fromJUOp False "--"     = PostDec
fromJUOp b     xs   = error ("fromJUop: unknown unary operator: " ++ show (b, xs))

toJUOp :: UnOp -> (Bool, String)
toJUOp NotOp    = (True, "!")
toJUOp BNotOp   = (True, "~")
toJUOp NegOp    = (True, "-")
toJUOp PlusOp   = (True, "+")
toJUOp NewOp    = (True, "new")
toJUOp TypeofOp = (True, "typeof")
toJUOp YieldOp  = (True, "yield")
toJUOp DeleteOp = (True, "delete")
toJUOp VoidOp   = (True, "void")
toJUOp PreInc   = (True, "++")
toJUOp PreDec   = (True, "--")
toJUOp PostInc  = (False, "++")
toJUOp PostDec  = (False, "--")

data BinOp =
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
      | MulOp           -- *
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
  deriving (Show, Eq, Ord, Enum, Data, Typeable)

fromJBOp :: String -> BinOp
fromJBOp "=="         = EqOp
fromJBOp "==="        = StrictEqOp
fromJBOp "!="         = NeqOp
fromJBOp "!=="        = StrictNeqOp
fromJBOp ">"          = GtOp
fromJBOp ">="         = GeOp
fromJBOp "<"          = LtOp
fromJBOp "<="         = LeOp
fromJBOp "+"          = AddOp
fromJBOp "-"          = SubOp
fromJBOp "*"          = MulOp
fromJBOp "/"          = DivOp
fromJBOp "%"          = ModOp
fromJBOp "<<"         = LeftShiftOp
fromJBOp ">>"         = RightShiftOp
fromJBOp ">>>"        = ZRightShiftOp
fromJBOp "&"          = BAndOp
fromJBOp "|"          = BOrOp
fromJBOp "^"          = BXorOp
fromJBOp "&&"         = LAndOp
fromJBOp "||"         = LOrOp
fromJBOp "instanceof" = InstanceofOp
fromJBOp "in"         = InOp
fromJBOp xs    = error ("fromJBOp: unknown binary operator: " ++ xs)

toJBOp :: BinOp -> String
toJBOp EqOp          = "=="
toJBOp StrictEqOp    = "==="
toJBOp NeqOp         = "!="
toJBOp StrictNeqOp   = "!=="
toJBOp GtOp          = ">"
toJBOp GeOp          = ">="
toJBOp LtOp          = "<"
toJBOp LeOp          = "<="
toJBOp AddOp         = "+"
toJBOp SubOp         = "-"
toJBOp MulOp         = "*"
toJBOp DivOp         = "/"
toJBOp ModOp         = "%"
toJBOp LeftShiftOp   = "<<"
toJBOp RightShiftOp  = ">>"
toJBOp ZRightShiftOp = ">>>"
toJBOp BAndOp        = "&"
toJBOp BOrOp         = "|"
toJBOp BXorOp        = "^"
toJBOp LAndOp        = "&&"
toJBOp LOrOp         = "||"
toJBOp InstanceofOp  = "instanceof"
toJBOp InOp          = "in"
-}

-- does this operator always give a bool result
isBoolOp :: JOp -> Bool
isBoolOp EqOp         = True
isBoolOp StrictEqOp   = True
isBoolOp NeqOp        = True
isBoolOp StrictNeqOp  = True
isBoolOp GtOp         = True
isBoolOp GeOp         = True
isBoolOp LtOp         = True
isBoolOp LeOp         = True
isBoolOp InstanceofOp = True
isBoolOp InOp         = True
isBoolOp _            = False

-- is this an equality operator
isEqOp :: JOp -> Bool
isEqOp EqOp        = True
isEqOp NeqOp       = True
isEqOp StrictEqOp  = True
isEqOp StrictNeqOp = True
isEqOp _           = False

data Val =
        UndefinedV
      | NullV
      | Var !Id
      | ListV [Expr]
      | BoolV Bool
      | DoubleV SaneDouble
      | IntV Integer
      | StrV Text
      | RegExV Text
      | HashV (Map Text Expr)
      | FuncV [Ident] JStat  -- function args/body not converted!
  deriving (Show, Eq, Ord, Data, Typeable)

toJVal :: IntMap Ident -> Val -> JVal
toJVal _ UndefinedV  = JVar (TxtI "undefined")
toJVal _ NullV       = JVar (TxtI "null")
toJVal _ (BoolV b)   = JVar (TxtI $ if b then "true" else "false")
toJVal m (Var i)        =
  case IM.lookup i m of
    Just i' -> JVar i'
    Nothing -> error ("toJVal: unknown identifier: " ++ show i)
toJVal _ (DoubleV d)  = JDouble d
toJVal _ (IntV i)     = JInt i
toJVal _ (StrV t)     = JStr t
toJVal _ (RegExV t)   = JRegEx t
toJVal _ (FuncV is s) = JFunc is s
toJVal m (ListV xs)   = JList (map (exprToJExpr m) xs)
toJVal m (HashV h)    = JHash . M.fromList . map (second (exprToJExpr m)) . M.toList $ h

fromJVal :: JVal -> IdS Val
fromJVal (JVar (TxtI x))
  | x == "undefined" = pure UndefinedV
  | x == "null"      = pure NullV
  | x == "false"     = pure (BoolV False)
  | x == "true"      = pure (BoolV True)
fromJVal (JVar i) = Var <$> fromJIdent i
fromJVal (JList xs)  = ListV <$> mapM fromJExpr xs
fromJVal (JDouble d) = pure (DoubleV d)
fromJVal (JInt i)    = pure (IntV i)
fromJVal (JStr s)    = pure (StrV s)
fromJVal (JRegEx r)  = pure (RegExV r)
fromJVal (JFunc is s) = pure (FuncV is s)
fromJVal UnsatVal{}  = error "fromJVal: unsaturated value"
fromJVal (JHash h)   = HashV . M.fromList <$> mapM f (M.toList h)
  where
   f :: (Text, JExpr) -> IdS (Text, Expr)
   f (k, e) = (k,) <$> fromJExpr e

fromJIdent :: Ident -> IdS Id
fromJIdent i = do
  m <- get
  case HM.lookup i m of
    Just n  -> return n
    Nothing ->
      let n = HM.size m
      in  put (HM.insert i n m) >> return n

toJIdent :: IntMap Ident -> Id -> Ident
toJIdent m i = fromMaybe (error $ "toJIdent: unknown identifier: " ++ show i) (IM.lookup i m)

type NodeId  = Int
data Node a where
  SimpleNode   :: SimpleStat a -> Node a
  SequenceNode :: [NodeId] -> Node a
  IfNode       :: AExpr a -> NodeId -> NodeId -> Node a
  WhileNode    :: AExpr a -> NodeId -> Node a
  DoWhileNode  :: AExpr a -> NodeId -> Node a
  ForInNode    :: Bool -> Id -> AExpr a -> NodeId -> Node a
  SwitchNode   :: AExpr a -> [(AExpr a,NodeId)] -> NodeId -> Node a
  ReturnNode   :: AExpr a -> Node a
  TryNode      :: NodeId -> Id -> NodeId -> NodeId -> Node a
  BreakNode    :: Maybe Text -> NodeId -> Node a
  ContinueNode :: Maybe Text -> NodeId -> Node a
  LabelNode    :: Text -> NodeId -> Node a
    deriving (Show, Data, Typeable, Eq, Ord)

-- a simple, non-control-flow statement
data SimpleStat a =
    DeclS !Id
  | ExprS (AExpr a) --  apply / unary op folded into expression
  | AssignS (AExpr a) (AExpr a)
  deriving (Show, Data, Typeable, Eq, Ord)

isLoop :: Node a -> Bool
isLoop WhileNode{} = True
isLoop DoWhileNode{} = True
isLoop ForInNode{} = True
isLoop _ = False

isSwitch :: Node a -> Bool
isSwitch SwitchNode{} = True
isSwitch _ = False

isContinue :: Node a -> Bool
isContinue ContinueNode{} = True
isContinue _ = False

isBreak :: Node a -> Bool
isBreak BreakNode{} = True
isBreak _ = False

-- invariant: idents and identsR are always filled
-- with Ids from 0..n so that we can use the
-- Map's size to get the next one
data Graph a = Graph { _nodes :: IntMap (Node a)
                     , _arcsIn :: IntMap (Set Arc)
                     , _arcsOut :: IntMap (Set Arc)
                     , _entry :: NodeId
                     , _nodeid :: NodeId
                     , _labels :: Map Text NodeId
                     , _idents :: IntMap Ident
                     , _identsR :: HashMap Ident Id
                     }
  deriving (Data, Typeable)

data Arc = Arc { _arcFrom :: NodeId
               , _arcTo   :: NodeId
               , _arcType :: ArcType
               }
  deriving (Eq, Ord, Data, Typeable)

data ArcType = BreakArc | ContinueArc deriving (Eq, Ord, Show, Data, Typeable)

{-
makeLenses ''Graph
-}
arcsIn :: forall a_a57Y1. Lens' (Graph a_a57Y1) (IntMap (Set Arc))
arcsIn
  f_a5bOD
  (Graph x1_a5bOE
         x2_a5bOF
         x3_a5bOG
         x4_a5bOH
         x5_a5bOI
         x6_a5bOJ
         x7_a5bOK
         x8_a5bOL)
  = (fmap
       (\ y1_a5bON
          -> (((((((Graph x1_a5bOE) y1_a5bON) x3_a5bOG) x4_a5bOH) x5_a5bOI)
                 x6_a5bOJ)
                x7_a5bOK)
               x8_a5bOL))
      (f_a5bOD x2_a5bOF)
{-# INLINE arcsIn #-}
arcsOut :: forall a_a57Y1. Lens' (Graph a_a57Y1) (IntMap (Set Arc))
arcsOut
  f_a5bOR
  (Graph x1_a5bOS
         x2_a5bOT
         x3_a5bOU
         x4_a5bOV
         x5_a5bOW
         x6_a5bOX
         x7_a5bOY
         x8_a5bOZ)
  = (fmap
       (\ y1_a5bP0
          -> (((((((Graph x1_a5bOS) x2_a5bOT) y1_a5bP0) x4_a5bOV) x5_a5bOW)
                 x6_a5bOX)
                x7_a5bOY)
               x8_a5bOZ))
      (f_a5bOR x3_a5bOU)
{-# INLINE arcsOut #-}
entry :: forall a_a57Y1. Lens' (Graph a_a57Y1) NodeId
entry
  f_a5bP2
  (Graph x1_a5bP3
         x2_a5bP4
         x3_a5bP5
         x4_a5bP6
         x5_a5bP7
         x6_a5bP8
         x7_a5bPa
         x8_a5bPb)
  = (fmap
       (\ y1_a5bPc
          -> (((((((Graph x1_a5bP3) x2_a5bP4) x3_a5bP5) y1_a5bPc) x5_a5bP7)
                 x6_a5bP8)
                x7_a5bPa)
               x8_a5bPb))
      (f_a5bP2 x4_a5bP6)
{-# INLINE entry #-}
idents :: forall a_a57Y1. Lens' (Graph a_a57Y1) (IntMap Ident)
idents
  f_a5bPd
  (Graph x1_a5bPe
         x2_a5bPf
         x3_a5bPg
         x4_a5bPh
         x5_a5bPi
         x6_a5bPj
         x7_a5bPk
         x8_a5bPl)
  = (fmap
       (\ y1_a5bPm
          -> (((((((Graph x1_a5bPe) x2_a5bPf) x3_a5bPg) x4_a5bPh) x5_a5bPi)
                 x6_a5bPj)
                y1_a5bPm)
               x8_a5bPl))
      (f_a5bPd x7_a5bPk)
{-# INLINE idents #-}
identsR :: forall a_a57Y1. Lens' (Graph a_a57Y1) (HashMap Ident Id)
identsR
  f_a5bPn
  (Graph x1_a5bPo
         x2_a5bPp
         x3_a5bPq
         x4_a5bPr
         x5_a5bPs
         x6_a5bPt
         x7_a5bPu
         x8_a5bPv)
  = (fmap
       (\ y1_a5bPw
          -> (((((((Graph x1_a5bPo) x2_a5bPp) x3_a5bPq) x4_a5bPr) x5_a5bPs)
                 x6_a5bPt)
                x7_a5bPu)
               y1_a5bPw))
      (f_a5bPn x8_a5bPv)
{-# INLINE identsR #-}
labels :: forall a_a57Y1. Lens' (Graph a_a57Y1) (Map Text NodeId)
labels
  f_a5bPy
  (Graph x1_a5bPz
         x2_a5bPA
         x3_a5bPB
         x4_a5bPC
         x5_a5bPD
         x6_a5bPE
         x7_a5bPF
         x8_a5bPH)
  = (fmap
       (\ y1_a5bPI
          -> (((((((Graph x1_a5bPz) x2_a5bPA) x3_a5bPB) x4_a5bPC) x5_a5bPD)
                 y1_a5bPI)
                x7_a5bPF)
               x8_a5bPH))
      (f_a5bPy x6_a5bPE)
{-# INLINE labels #-}
nodeid :: forall a_a57Y1. Lens' (Graph a_a57Y1) NodeId
nodeid
  f_a5bPK
  (Graph x1_a5bPL
         x2_a5bPM
         x3_a5bPN
         x4_a5bPO
         x5_a5bPP
         x6_a5bPR
         x7_a5bPS
         x8_a5bPT)
  = (fmap
       (\ y1_a5bPU
          -> (((((((Graph x1_a5bPL) x2_a5bPM) x3_a5bPN) x4_a5bPO) y1_a5bPU)
                 x6_a5bPR)
                x7_a5bPS)
               x8_a5bPT))
      (f_a5bPK x5_a5bPP)
{-# INLINE nodeid #-}
nodes ::
  forall a_a57Y1 a_a5bOA.
  Lens (Graph a_a57Y1) (Graph a_a5bOA) (IntMap (Node a_a57Y1)) (IntMap (Node a_a5bOA))
nodes
  f_a5bPW
  (Graph x1_a5bPX
         x2_a5bPY
         x3_a5bPZ
         x4_a5bQ0
         x5_a5bQ1
         x6_a5bQ2
         x7_a5bQ3
         x8_a5bQ4)
  = (fmap
       (\ y1_a5bQ6
          -> (((((((Graph y1_a5bQ6) x2_a5bPY) x3_a5bPZ) x4_a5bQ0) x5_a5bQ1)
                 x6_a5bQ2)
                x7_a5bQ3)
               x8_a5bQ4))
      (f_a5bPW x1_a5bPX)
{-# INLINE nodes #-}

{-
makeLenses ''Node
-}
{-
makeLenses ''Arc
-}

arcFrom :: Lens' Arc NodeId
arcFrom f_a5cjD (Arc x1_a5cjE x2_a5cjF x3_a5cjG)
  = (fmap (\ y1_a5cjH -> ((Arc y1_a5cjH) x2_a5cjF) x3_a5cjG))
      (f_a5cjD x1_a5cjE)
{-# INLINE arcFrom #-}
arcTo :: Lens' Arc NodeId
arcTo f_a5cjI (Arc x1_a5cjJ x2_a5cjK x3_a5cjL)
  = (fmap (\ y1_a5cjM -> ((Arc x1_a5cjJ) y1_a5cjM) x3_a5cjL))
      (f_a5cjI x2_a5cjK)
{-# INLINE arcTo #-}
arcType :: Lens' Arc ArcType
arcType f_a5cjN (Arc x1_a5cjO x2_a5cjP x3_a5cjQ)
  = (fmap (\ y1_a5cjR -> ((Arc x1_a5cjO) x2_a5cjP) y1_a5cjR))
      (f_a5cjN x3_a5cjQ)
{-# INLINE arcType #-}

-- makePrisms ''Node

lookupId :: Graph a -> Ident -> Maybe Id
lookupId g i = HM.lookup i (g ^. identsR)

lookupIdent :: Graph a -> Id -> Maybe Ident
lookupIdent g i = IM.lookup i (g ^. idents)

addIdents :: [Ident] -> Graph a -> ([Id], Graph a)
addIdents ids g =
  let xs = zip ids [IM.size (g ^. idents)..]
      xs' = map (\(x,y) -> (y,x)) xs
  in  (map snd xs, g & idents  %~ IM.union (IM.fromList xs')
                     & identsR %~ HM.union (HM.fromList xs))

-- local vars declared in the graph
localVarsGraph :: Graph a -> IntSet
localVarsGraph g = IS.fromList . concatMap getDecls . IM.elems $ g ^. nodes
  where
    getDecls (SimpleNode (DeclS i)) = [i]
    getDecls (TryNode _ _ _ _)      = [] -- fixme?
    getDecls _                      = []

instance Show a => Show (Graph a)
  where
    show g = unlines [ns, arcs, lbls, extra]
      where
        ns   = unlines $ g ^. nodes . to (map (\(i,n) -> show i ++ ": " ++ show n) . IM.toList)
        arcs   = unlines . map show . S.toList . S.unions $ g ^. arcsIn . to IM.elems
        lbls = unlines $ g ^. labels . to (map (\(l,i) -> "label: " ++ T.unpack l ++ ": " ++ show i) . M.toList)
        extra  = unlines [ "entry: " ++ (g ^. entry . to show) ]

instance Show Arc where
  show (Arc f t ty) = show f ++ " -> " ++ show t ++ " (" ++ show ty ++ ")"

emptyGraph :: Graph a
emptyGraph = Graph IM.empty IM.empty IM.empty 0 1 M.empty IM.empty HM.empty

addArc :: Arc -> State (Graph a) ()
addArc a = do
  let sa = S.singleton a
  arcsOut %= IM.insertWith S.union (a^.arcFrom) sa
  arcsIn  %= IM.insertWith S.union (a^.arcTo) sa

newNodeId :: State (Graph a) NodeId
newNodeId = do
  n <- use nodeid
  nodeid %= (+1)
  return n

newLabel :: Text -> NodeId -> State (Graph a) ()
newLabel lbl lid =
  labels %= M.insert lbl lid

lookupLabel :: Text -> State (Graph a) NodeId
lookupLabel lbl = do
  lbls <- use labels
  case M.lookup lbl lbls of
    Just nid -> return nid
    Nothing  -> error ("lookupLabel: unknown label: " ++ T.unpack lbl)

newNode :: Node a -> NodeId -> State (Graph a) NodeId
newNode s n = nodes %= IM.insert n s >> return n

continueTo :: Maybe Text -> NodeId -> NodeId -> State (Graph a) NodeId
continueTo lbl nid n
  | nid < 0 = error "continueTo: continuing to invalid node, not in a loop?"
  | otherwise = do
      _cnid <- newNode (ContinueNode lbl nid) n
      addArc (Arc n nid ContinueArc)
      return n

breakTo :: Maybe Text -> NodeId -> NodeId -> State (Graph a) NodeId
breakTo lbl nid n
  | nid < 0 = error "breakTo: breaking to invalid node, not in a loop?"
  | otherwise = do
      void (newNode (BreakNode lbl nid) n)
      addArc (Arc n nid BreakArc)
      return n

-- | after the initial conversion, labels may point to a SequenceNode or another label
--   fix them, and make sure that all labels point to a loop or switch now
fixLabels :: State (Graph a) ()
fixLabels = do
  lbls <- use labels
  ns   <- use nodes
  forM_ (M.toList lbls) (\(s,n) -> updLabel s ns n n)
  checkLabels
  checkJumps
    where
      updLabel :: Text -> IntMap (Node a) -> NodeId -> NodeId -> State (Graph a) ()
      updLabel s ns orig n =
        case IM.lookup n ns of
          (Just (SequenceNode (s0:_))) -> updLabel s ns orig s0
          (Just (LabelNode _ l0))      -> updLabel s ns orig l0
          _  -> when (n /= orig) $ do
                  allArcs <- S.toList . S.unions . IM.elems <$> use arcsOut
                  let f m      = if m == orig then n else m
                      f' (BreakNode l m)    | m == orig = BreakNode l n
                      f' (ContinueNode l m) | m == orig = ContinueNode l n
                      f' x = x
                      allArcs' = map ((arcTo %~ f) . (arcFrom %~ f)) allArcs
                  arcsOut .= IM.fromListWith S.union (map (\a -> (a ^. arcFrom, S.singleton a)) allArcs')
                  arcsIn  .= IM.fromListWith S.union (map (\a -> (a ^. arcTo,   S.singleton a)) allArcs')
                  labels  %= fmap f
                  nodes   %= fmap f'

-- | check that labels are only used for loops
checkLabels :: State (Graph a) ()
checkLabels = do
  lbls <- use labels
  ns   <- use nodes
  forM_ (M.toList lbls) (\(_,n) -> checkValidLabelTarget ns n)
    where
      checkValidLabelTarget ns l =
        case IM.lookup l ns of
          Just x | isLoop x -> return ()
          Just _            -> error ("invalid label target: " ++ show l)
          _                 -> error ("unknown label target: " ++ show l)

-- | check that continue only jumps to loops, break to switch or loop
checkJumps :: State (Graph a) ()
checkJumps = do
  out <- S.toList . S.unions . IM.elems <$> use arcsOut
  ns  <- use nodes
  forM_ out (checkJump ns)
    where
      checkJump ns (Arc fr to ContinueArc)
        | check isLoop ns to && check isContinue ns fr = return ()
      checkJump ns (Arc fr to BreakArc)
        | (check isLoop ns to || check isSwitch ns to) && check isBreak ns fr = return ()
      checkJump _ (Arc fr to _) = error ("invalid jump: " ++ show fr ++ " -> " ++ show to)
      check p ns n = maybe False p (IM.lookup n ns)

cfg :: forall a. (Graph a -> Expr -> AExpr a) -> JStat -> Graph a
cfg toAExpr stat = execState buildGraph emptyGraph
  where
    buildGraph = do
      start <- newNodeId
      _ <- go stat (-1) (-1) start
      entry .= start
      fixLabels
      ir <- use identsR
      idents  .= IM.fromList (map (\(x,y) -> (y,x)) $ HM.toList ir)
    loopOf s1 f n = do
      s1n <- go s1 n n =<< newNodeId
      newNode (f s1n) n
    newSimpleNode s n = newNode (SimpleNode s) n
    expr :: JExpr -> State (Graph a) (AExpr a)
    expr e = do
      g <- get
      runIdS (toAExpr g <$> fromJExpr e)
    ident :: Ident -> State (Graph a) Id
    ident i = runIdS (fromJIdent i)
    runIdS :: IdS b -> State (Graph a) b
    runIdS x = do
      (r', m') <- runState x <$> use identsR
      identsR .= m'
      return r'
    go :: JStat -> NodeId -> NodeId -> NodeId -> State (Graph a) NodeId
    go (DeclStat i) _lb _lc n = do
      i' <- ident i
      newSimpleNode (DeclS i') n
    go (ReturnStat e) _lb _lc n = do
      e' <- expr e
      _ <- newNode (ReturnNode e') n
      return n
    go (IfStat e s1 s2) lb lc n = do
      s1n <- go s1 lb lc =<< newNodeId
      s2n <- go s2 lb lc =<< newNodeId
      e' <- expr e
      newNode (IfNode e' s1n s2n) n
    go (WhileStat True e s1) _lb _lc n = do
      e' <- expr e
      loopOf s1 (DoWhileNode e') n
    go (WhileStat False e s1) _lb _lc n = do
      e' <- expr e
      loopOf s1 (WhileNode e') n
    go (ForInStat b i e s1) _lb _lc n = do
      e' <- expr e
      i' <- ident i
      loopOf s1 (ForInNode b i' e') n
    go (SwitchStat e es sd) _lb lc n = do
      ns <- mapM (\(e',s') -> (,) <$> expr e' <*> (go s' n lc =<< newNodeId)) es
      sd' <- go sd n lc =<< newNodeId
      e' <- expr e
      newNode (SwitchNode e' ns sd') n
    go (TryStat t i c f) lb lc n = do
      tn <- go t lb lc =<< newNodeId
      cn <- go c lb lc =<< newNodeId
      fn <- go f lb lc =<< newNodeId
      i' <- ident i
      newNode (TryNode tn i' cn fn) n
    go (BlockStat ss)                    lb lc n = do
      ss' <- (SequenceNode <$> mapM (\s' -> go s' lb lc =<< newNodeId) ss)
      newNode ss' n
    go (ApplStat e es)                   _lb _lc n = do
      e' <- expr (ApplExpr e es)
      _es' <- mapM expr es
      newSimpleNode (ExprS e') n
    go (UOpStat op e)                    _lb _lc n = do
       e' <- expr (UOpExpr op e)
       newSimpleNode (ExprS e') n
    go (AssignStat e1 e2)                _lb _lc n = do
       e1' <- expr e1
       e2' <- expr e2
       newSimpleNode (AssignS e1' e2') n
    go UnsatBlock{}                      _lb _lc _n = error "cfg: unsaturated block"
    go (LabelStat lbl s1)                lb lc n = do
      lid <- newNodeId
      newLabel lbl lid
      _ <- go s1                         lb lc lid
      newNode (LabelNode lbl lid) n
    go (BreakStat lbl@(Just lbl'))       _lb _lc n = do
      ll <- lookupLabel lbl'
      breakTo lbl ll n
    go (BreakStat lbl)                   lb _lc n = breakTo lbl lb n
    go (ContinueStat lbl@(Just lbl'))    _lb _lc n = do
      ll <- lookupLabel lbl'
      continueTo lbl ll n
    go (ContinueStat lbl)                _lb lc n = continueTo lbl lc n


unCfg :: forall a. Graph a -> JStat
unCfg g = go' (g ^. entry)
  where
    m = g ^. idents
    convertI :: Id -> Ident
    convertI i = fromMaybe (error $ "unCfg: unknown identifier: " ++ show i) (IM.lookup i m)
    go' :: Int -> JStat
    go' n = case IM.lookup n (g ^. nodes) of
              Just x  -> go x
              Nothing -> error ("unCfg: unknown node: " ++ show n)
    go :: Node a -> JStat
    go (SimpleNode s)        = fromSimpleStat m s
    go (SequenceNode ss)     = BlockStat (map go' ss)
    go (IfNode e n1 n2)      = IfStat (toJExpr' m e) (go' n1) (go' n2)
    go (WhileNode e s)       = WhileStat False (toJExpr' m e) (go' s)
    go (DoWhileNode e s)     = WhileStat True (toJExpr' m e) (go' s)
    go (ForInNode b i e s)   = ForInStat b (convertI i) (toJExpr' m e) (go' s)
    go (SwitchNode e ss s)   = SwitchStat (toJExpr' m e) (map (\(e',s') -> (toJExpr' m e', go' s')) ss) (go' s)
    go (ReturnNode e)        = ReturnStat (toJExpr' m e)
    go (TryNode t i c f)     = TryStat (go' t) (convertI i) (go' c) (go' f)
    go (BreakNode lbl _)     = BreakStat lbl
    go (ContinueNode lbl _)  = ContinueStat lbl
    go (LabelNode lbl s)     = LabelStat lbl (go' s)

fromSimpleStat :: IntMap Ident -> SimpleStat a -> JStat
fromSimpleStat m (DeclS i)       = DeclStat (toJIdent m i)
fromSimpleStat m (ExprS (AExpr _ (ApplE e es))) = ApplStat (exprToJExpr m e) (map (exprToJExpr m) es)
fromSimpleStat m (ExprS (AExpr _ (UOpE op e))) = UOpStat op (exprToJExpr m e)
fromSimpleStat _ (ExprS (AExpr _ e)) = error ("fromSimpleStat: ExprS, not a valid expression statement: " ++ show e)
fromSimpleStat m (AssignS e1 e2) = AssignStat (toJExpr' m e1) (toJExpr' m e2)

-- facts, forward flow:
-- index 0: combined fact when entering for the first time (continue, looping excluded)
-- index 1: combined fact leaving the node
-- index 2: facts just before the expressions in the node, if any
-- index n, n > 2, n <= 15: other interesting positions, depend on node
newtype Facts a = Facts (IntMap a)
  deriving (Eq)

instance Show a => Show (Facts a) where
  show (Facts xs) = unlines . map (\(k,v) -> show (unFactKey k) ++ " -> " ++ show v) . IM.toList $ xs

noFacts :: Facts a
noFacts = Facts IM.empty

addFact :: NodeId -> Int -> a -> Facts a -> Facts a
addFact node idx x (Facts m) = Facts $ IM.insert (factKey "addFact" node idx) x m

lookupFact :: a -> NodeId -> Int -> Facts a -> a
lookupFact z node idx (Facts m)
  = fromMaybe z (IM.lookup (factKey "lookupFact" node idx) m)

factKey :: String -> NodeId -> Int -> Int
factKey xs node idx
  | idx < 0 || idx > 15 = error (xs ++ ": invalid fact position")
  | otherwise           = shiftL node 4 .|. idx

unFactKey :: Int -> (NodeId, Int)
unFactKey k = (shiftR k 4, k .&. 15)

data Forward a b =
  Forward { fIf       :: NodeId -> AExpr a                   -> b -> (b, b)  -- condition true, false
          , fWhile    :: NodeId -> AExpr a                   -> b -> (b, b)  -- condition true, false
          , fDoWhile  :: NodeId -> AExpr a                   -> b -> (b, b)  -- condition true, false
          , fSimple   :: NodeId -> SimpleStat a              -> b -> b
          , fBreak    :: NodeId                              -> b -> b
          , fContinue :: NodeId                              -> b -> b
          , fReturn   :: NodeId -> AExpr a                   -> b -> b
          , fTry      :: NodeId                              -> b -> (b, b, b)   -- start of try, catch, finally
          , fForIn    :: NodeId -> Bool    -> Id  -> AExpr a -> b -> (b, b)   -- after condition: loop, no loop
          , fSwitch   :: NodeId -> AExpr a -> [AExpr a]      -> b -> ([b], b) -- start of every label, default
          }

instance Default (Forward a b) where
  def = Forward c2tup c2tup c2tup fconst2 fconst fconst fconst2 c1tup3 c4tup defSwitch

constForward :: b -> Forward a b
constForward z = Forward (const3 (z,z)) (const3 (z,z)) (const3 (z,z)) (const3 z)
                         (const2 z) (const2 z) (const3 z) (const2 (z,z,z)) (const5 (z,z)) (zSwitch z)

-- c1tup :: a -> b -> (b, b)
-- c1tup _ x = (x, x)

c1tup3 :: a -> b -> (b, b, b)
c1tup3 _ x = (x, x, x)

c2tup :: a -> b -> c -> (c, c)
c2tup _ _ x = (x, x)

c4tup :: a -> b -> c -> d -> e -> (e, e)
c4tup _ _ _ _ x = (x, x)

fconst :: a -> b -> b
fconst _ x = x

fconst2 :: a -> b -> c -> c
fconst2 _ _ x = x

const2 :: a -> b -> c -> a
const2 x _ _ = x

const3 :: a -> b -> c -> d -> a
const3 x _ _ _ = x

-- const4 :: a -> b -> c -> d -> e -> a
-- const4 x _ _ _ _ = x

const5 :: a -> b -> c -> d -> e -> f -> a
const5 x _ _ _ _ _ = x

-- fconst3 :: a -> b -> c -> d -> d
-- fconst3 _ _ _ x = x

-- fconst4 :: a -> b -> c -> d -> e -> e
-- fconst4 _ _ _ _ x = x

defSwitch :: a -> b -> [c] -> d -> ([d],d)
defSwitch _ _ xs y = (replicate (length xs) y, y)

zSwitch :: d -> a -> b -> [c] -> d -> ([d],d)
zSwitch z _ _ xs _ = (replicate (length xs) z, z)

foldForward :: forall a b. Eq b
            => (b -> b -> b)
            -> Forward a b
            -> b
            -> b
            -> Graph a
            -> Facts b
foldForward c f entr z g = fixed (goEntry $ g^.entry) noFacts
  where
    -- combine x with data from nodes, only if nodes have available data
    combineWith :: Int -> b -> [NodeId] -> State (Facts b) b
    combineWith factn x nids = do
      m <- get
      return $ foldl' c x (map (\n -> lookupFact z n factn m) nids)

    combineFrom :: Int -> [NodeId] -> State (Facts b) b
    combineFrom factn nids = do
      m <- get
      return $ foldl c z (map (\n -> lookupFact z n factn m) nids)

    upd :: NodeId -> Int -> b -> State (Facts b) ()
    upd nid i x = modify (addFact nid i x)

    upds :: NodeId -> [Int] -> b -> State (Facts b) ()
    upds nid is x = mapM_ (\i -> upd nid i x) is

    fact :: NodeId -> Int -> State (Facts b) b
    fact n i = gets (lookupFact z n i)

    goEntry :: NodeId -> Facts b -> Facts b
    goEntry nid = execState (go' nid entr)

    go' :: NodeId -> b -> State (Facts b) b
    go' nid x = go nid (lookupNode nid g) x

    go :: NodeId -> Node a -> b -> State (Facts b) b
    go nid (SimpleNode s) x = do
      upds nid [0,2] x
      let x' = fSimple f nid s x
      upd nid 1 x'
      return x'
    go nid (SequenceNode ss) x  = do
      upd nid 0 x
      go0 ss x
        where
          go0 (y:ys) x0 = do
            x1 <- go' y x0
            go0 ys x1
          go0 [] x0 = do
            upd nid 1 x0
            return x0
    go nid (IfNode e s1 s2) x = do
      upds nid [0,2] x
      let (xi,xe) = fIf f nid e x
      xi' <- go' s1 xi
      xe' <- go' s2 xe
      let x' = xi' `c` xe'
      upd nid 1 x'
      return x'
    go nid (WhileNode e s) x = do
      upd nid 0 x
      let (brks, conts) = getBreaksConts nid g
      x0 <- combineWith 0 x conts
      x1 <- (x0 `c`) <$> fact nid 2
      let (xt, xf) = fWhile f nid e x1
      s0 <- go' s xt
      upd nid 2 (x1 `c` s0)
      x3 <- combineWith 0 xf brks
      upd nid 1 x3
      return x3
    go nid (DoWhileNode e s) x = do
      upd nid 0 x
      let (brks, conts) = getBreaksConts nid g
      x0 <- (x `c`) <$> fact nid 3
      _s0 <- go' s x0
      x1 <- combineWith 0 x0 conts
      upd nid 2 x1
      let (xt, xf) = fDoWhile f nid e x1
      upd nid 3 xt
      x2 <- combineWith 0 xf brks
      upd nid 1 x2
      return x2
    go nid (ForInNode b i e s) x = do
      upds nid [0,2] x
      let (brks, conts) = getBreaksConts nid g
      x0 <- combineWith 0 x conts
      x1 <- (x0 `c`) <$> fact nid 3
      upd nid 2 x1
      let (xb, xf) = fForIn f nid b i e x1
      s0 <- go' s xb
      upd nid 3 s0
      x3 <- combineWith 0 xf brks
      upd nid 1 x3
      return x3
    go nid (SwitchNode e ss s) x = do
      let (brks, _) = getBreaksConts nid g
          (xes,xd)       = fSwitch f nid e (map fst ss) x
          go0 [] []              = return z
          go0 [a] [(_e,y)]       = go' y a
          go0 (a:as) ((_e,y):ys) = go' y a >> go0 as ys
          go0 _ _ = error "foldForward: unmatched list length for switch"
      upds nid [0,2] x
      s0 <- go0 xes ss
      s1 <- go' s (xd `c` s0)
      s2 <- (`c` s1) <$> combineFrom 0 brks
      upd nid 1 s2
      return s2
    go nid (ReturnNode e) x = do
      upds nid [0,2] x
      let x' = fReturn f nid e x
      upd nid 3 x'
      return z
    go nid BreakNode{} x = do
      upds nid [0,2] x
      let x'= fBreak f nid x
      upd nid 3 x'
      return z
    go nid ContinueNode{} x = do
      upds nid [0,2] x
      let x' = fContinue f nid x
      upd nid 3 x'
      return z
    go nid (LabelNode _ s) x = do
      upds nid [0,2] x
      x0 <- go' s x
      upd nid 1 x0
      return x0
    go nid (TryNode t _ ctch fin) x = do
      upd nid 0 x
      let (x', xc, xf) = fTry f nid x
      t' <- go' t x'
      upd nid 2 t'
      c' <- go' ctch xc
      upd nid 3 c'
      s <- go' fin xf -- (t' `c` c')
      upd nid 1 s
      return s

data Backward a b =
  Backward { bIf       :: NodeId -> AExpr a          -> (b, b) -> b
           , bWhile    :: NodeId -> AExpr a          -> (b, b) -> b
           , bDoWhile  :: NodeId -> AExpr a               -> b -> b
           , bSimple   :: NodeId -> SimpleStat a          -> b -> b
           , bBreak    :: NodeId                          -> b -> b
           , bContinue :: NodeId                          -> b -> b
           , bReturn   :: NodeId -> AExpr a                    -> b
           -- (beginning of try, beginning of catch, beginning of finally) -> (before try, end of try)
           , bTry      :: NodeId -> Id            -> (b, b, b) -> (b, b)
           , bForIn    :: NodeId -> Bool -> Id -> AExpr a -> (b, b) -> b
           , bSwitch   :: NodeId -> AExpr a               -> b -> b
           }

constBackward :: b -> Backward a b
constBackward z = Backward (const3 z) (const3 z) (const3 z) (const3 z) (const2 z)
                           (const2 z) (const2 z) (const3 (z, z)) (const5 z) (const3 z)

foldBackward :: forall a b. Eq b
             => (b -> b -> b)
             -> Backward a b
             -> b
             -> b
             -> Graph a
             -> Facts b
foldBackward c f exit z g = fixed (goEntry $ g^.entry) noFacts
  where
    upd :: NodeId -> Int -> b -> State (Facts b) ()
    upd nid i x = modify (addFact nid i x)

    fact :: NodeId -> Int -> State (Facts b) b
    fact n i = gets (lookupFact z n i)

    goEntry :: NodeId -> Facts b -> Facts b
    goEntry nid = execState (go' nid exit)

    go' :: NodeId -> b -> State (Facts b) b
    go' nid x = go nid (lookupNode nid g) x

    go :: NodeId -> Node a -> b -> State (Facts b) b
    go nid (SimpleNode s) x = do
      upd nid 0 x
      let x0 = bSimple f nid s x
      upd nid 1 x0
      return x0
    go nid (SequenceNode ss) x = do
      upd nid 0 x
      go0 (reverse ss) x
        where
          go0 (y:ys) x0 =
            go0 ys =<< go' y x0
          go0 [] x0 = do
            upd nid 1 x0
            return x0
    go nid (IfNode e s1 s2) x = do
      upd nid 0 x
      xi <- go' s1 x
      xe <- go' s2 x
      let x0 = bIf f nid e (xi,xe)
      upd nid 1 x0
      return x0
    go nid (WhileNode e s) x = do
      upd nid 0 x
      x0 <- go' s x
      let x1 = bWhile f nid e (x0,x)
      upd nid 1 x1
      return x1
    go nid (DoWhileNode e s) x = do
      upd nid 0 x
      let x0 = bDoWhile f nid e x
      upd nid 2 x0
      x1 <- go' s x0
      let x2 = x0 `c` x1 -- is this correct, x0 vs x?
      upd nid 1 x2
      return x2
    go nid (ForInNode b i e s) x = do
      upd nid 0 x
      x' <- go' s x
      let xb = bForIn f nid b i e (x', x)
      upd nid 1 xb
      return xb
    go nid (SwitchNode e ss s) x = do
      upd nid 0 x
      x0  <- go' s x
      xs1 <- go0 (reverse ss) x0
      let x2 = bSwitch f nid e (foldl' c x0 xs1)
      upd nid 1 x2
      return x2
        where
          go0 [] z = return [z]
          go0 ((_,y):ys) z = do
            z' <- go' y z
            (z':) <$> go0 ys z'
    go nid (ReturnNode e) _ = do
      upd nid 0 exit
      let x = bReturn f nid e
      upd nid 1 x
      return x
    go nid (BreakNode _ tgt) _ = do
      x <- fact tgt 0
      upd nid 0 x
      let x0 = bBreak f nid x
      upd nid 1 x0
      return x0
    go nid (ContinueNode _ tgt) _ = do
      x <- fact tgt 2
      upd nid 0 x
      let x0 = bContinue f nid x
      upd nid 1 x0
      return x0
    go nid (LabelNode _ s) x = do
      upd nid 0 x
      x0 <- go' s x
      upd nid 1 x0
      return x0
    go nid (TryNode t i ctch fin) x = do
      upd nid 0 x
      f' <- go' fin x
      c' <- go' ctch f'
      tb <- fact nid 3
      let (_b0, te0) = bTry f nid i (tb, c', f')
      tb' <- go' t te0
      let (b1, _te1) = bTry f nid i (tb', c', f')
      upd nid 3 tb'
      upd nid 1 b1
      return b1

lookupNode :: NodeId -> Graph a -> Node a
lookupNode nid g = fromMaybe
                     (error $ "lookupNode: unknown node " ++ show nid)
                     (IM.lookup nid $ g ^. nodes)

getBreaksConts :: NodeId -> Graph a -> ([NodeId], [NodeId])
getBreaksConts nid g = go (maybe [] S.toList . IM.lookup nid $ g ^. arcsIn) ([],[])
  where
    go [] x = x
    go ((Arc fr _ BreakArc):ss)    (xs,ys) = go ss (fr:xs,ys)
    go ((Arc fr _ ContinueArc):ss) (xs,ys) = go ss (fr:xs,ys)

-- iterate until fixed point
fixed :: Eq a => (a -> a) -> a -> a
fixed f a | fa == a   = a
          | otherwise = fixed f fa
  where fa = f a

-- non-SequenceNode nodes in the order in which
-- they can be encountered in the code
-- x before y means that (the expressions of) node y cannot
-- be encountered before node x for the first time
orderedNodes :: Graph a -> [NodeId]
orderedNodes g = go (g ^. entry)
  where
    go nid = go' nid (lookupNode nid g)
    go' :: NodeId -> Node a -> [NodeId]
    go' _   (SequenceNode xs)   = concatMap go xs
    go' nid (IfNode _ n1 n2)    = nid : go n1 ++ go n2
    go' nid (WhileNode _ n)     = nid : go n
    go' nid (DoWhileNode _ n)   = go n ++ [nid]
    go' nid (ForInNode _ _ _ n) = nid : go n
    go' nid (SwitchNode _ xs d) = nid : (concatMap (go . snd) xs) ++ go d
    go' nid (TryNode t _ c f)   = go t ++ [nid] ++  go c ++ go f
    go' nid _                   = [nid]
