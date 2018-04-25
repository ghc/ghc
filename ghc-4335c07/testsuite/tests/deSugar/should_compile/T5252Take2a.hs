{-# OPTIONS_GHC -funbox-strict-fields #-}
 
module T5252Take2a ( WriteMessage(..) , WriteDevice ) where

import qualified Data.ByteString as ByteString

data WriteMessage = WriteMessage !WriteDevice
newtype WriteDevice = WriteDevice ByteString.ByteString
