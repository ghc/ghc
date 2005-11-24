{-# OPTIONS_GHC -fglasgow-exts #-}

-- This is a performance test.  In GHC 6.4, it simply wouldn't compile
-- because the types got exponentially large, due to poor handling of
-- type synonyms

module ShouldCompile where

import Data.Word
import Data.Int
import Data.Typeable

data HNil      = HNil      deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)

type e :*: l = HCons e l
	-- In GHC 6.4 the deeply-nested use of this
	-- synonym gave rise to exponential behaviour

--- list endian16
newtype Tables = Tables [TableInfo] deriving (Show, Typeable) 

type TableInfo = 
    AvgPot :*:
    NumPlayers :*:
    Waiting :*:
    PlayersFlop :*:
    TableName :*:
    TableID :*:
    GameType :*:
    InfoMaxPlayers :*:
    RealMoneyTable :*:
    LowBet :*:
    HighBet :*: 
    MinStartMoney :*:
    MaxStartMoney :*:
    GamesPerHour :*:
    TourType :*:
    TourID :*:
    BetType :*:
    CantReturnLess :*:
    AffiliateID :*:
    NIsResurrecting :*:
    MinutesForTimeout :*:
    SeatsToResurrect :*:
    LangID :*:
    HNil

newtype TourType = TourType TourType_ deriving (Show, Typeable) 
newtype AvgPot = AvgPot Word64 deriving (Show, Typeable)
newtype NumPlayers = NumPlayers Word16 deriving (Show, Typeable)
newtype Waiting = Waiting Word16 deriving (Show, Typeable)
newtype PlayersFlop = PlayersFlop Word8 deriving (Show, Typeable)
newtype TableName = TableName String deriving (Show, Typeable) 
newtype TableID = TableID Word32 deriving (Show, Typeable) 
newtype OldTableID = OldTableID Word32 deriving (Show, Typeable) 
newtype GameType = GameType GameType_ deriving (Show, Typeable) 
newtype InfoMaxPlayers = InfoMaxPlayers Word16 deriving (Show, Typeable) 
newtype RealMoneyTable = RealMoneyTable Bool deriving (Show, Typeable) 
newtype LowBet = LowBet RealMoney_ deriving (Show, Typeable) 
newtype HighBet = HighBet RealMoney_ deriving (Show, Typeable) 
newtype MinStartMoney = MinStartMoney RealMoney_ deriving (Show, Typeable) 
newtype MaxStartMoney = MaxStartMoney RealMoney_ deriving (Show, Typeable) 
newtype GamesPerHour = GamesPerHour Word16 deriving (Show, Typeable) 
newtype TourID = TourID Word32 deriving (Show, Typeable) 
newtype BetType = BetType BetType_ deriving (Show, Typeable) 
newtype CantReturnLess = CantReturnLess Word32 deriving (Show, Typeable) 
newtype AffiliateID = AffiliateID [Word8] deriving (Show, Typeable) 
newtype NIsResurrecting = NIsResurrecting Word32 deriving (Show, Typeable) 
newtype MinutesForTimeout = MinutesForTimeout Word32 deriving (Show, Typeable) 
newtype SeatsToResurrect = SeatsToResurrect Word32 deriving (Show, Typeable) 
newtype LangID = LangID Word32 deriving (Show, Typeable) 

data GameType_
    = EmptyGame
    | Holdem
    | OmahaHoldem
    | OmahaHiLo
    | SevenCardStud
    | SevenCardStudLoHi
    | OneToOne
    | OneToOneOmaha
    | OneToOne7CS
    | OneToOneOmahaHL
    | OneToOne7CSHL
    | TeenPatti
    | OneToOneTeenPatti
      deriving (Eq, Show, Typeable)

type RealMoney_ = Word64

data TourType_
    = TourNone
    | TourSingle
    | TourMulti
    | TourHeadsUpMulti
    deriving (Enum, Eq, Show, Typeable)

data BetType_
    = BetNone
    | BetFixed
    | BetPotLimit
    | BetNoLimit
    | BetBigRiver
    | BetTeenPatti
    | BetTeenPattiFixed
    deriving (Enum, Eq, Show, Typeable)
               
