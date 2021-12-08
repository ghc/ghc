-- Reduced from the xml-types and xml-conduit packages

{-# language DeriveGeneric, DeriveDataTypeable #-}

module Text.XML (Node(..), Element(..), Name(..)) where

import           Control.DeepSeq
import           Data.Function (on)
import qualified Data.Map as Map
import           Data.Text
import           Data.Typeable (Typeable)
import           Data.Data (Data)
import           GHC.Generics (Generic)

data Node
    = NodeElement Element
    | NodeInstruction Instruction
    | NodeContent Text
    | NodeComment Text
  deriving (Show, Eq, Ord, Typeable, Data)

instance NFData Node where
  rnf (NodeElement e)     = rnf e `seq` ()
  rnf (NodeInstruction i) = rnf i `seq` ()
  rnf (NodeContent t)     = rnf t `seq` ()
  rnf (NodeComment t)     = rnf t `seq` ()

data Element = Element
    { elementName       :: Name
    , elementAttributes :: Map.Map Name Text
    , elementNodes      :: [Node]
    }
  deriving (Show, Eq, Ord, Typeable, Data)

instance NFData Element where
  rnf (Element a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

data Name = Name
        { nameLocalName :: Text
        , nameNamespace :: Maybe Text
        , namePrefix :: Maybe Text
        }
        deriving (Show
        , Data, Typeable
        , Generic
        )

instance Eq Name where
        (==) = (==) `on` (\x -> (nameNamespace x, nameLocalName x))

instance Ord Name where
        compare = compare `on` (\x -> (nameNamespace x, nameLocalName x))

instance NFData Name where
        rnf (Name a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

data Instruction = Instruction
        { instructionTarget :: Text
        , instructionData :: Text
        }
        deriving (Eq, Ord, Show
        , Data, Typeable
        , Generic
        )

instance NFData Instruction where
        rnf (Instruction a b) = rnf a `seq` rnf b `seq` ()
