-- From the GHC users mailing list, 3/9/14

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module BadSock where

import Data.Proxy
import GHC.Exts

data Message

data SocketType = Dealer | Push | Pull

data SocketOperation = Read | Write

data SockOp :: SocketType -> SocketOperation -> * where
    SRead :: Foo 'Read sock => SockOp sock 'Read
    SWrite :: Foo Write sock => SockOp sock Write

data Socket :: SocketType -> * where
    Socket :: proxy sock
           -> (forall op . Foo op sock => SockOp sock op -> Operation op)
           -> Socket sock

type family Foo (op :: SocketOperation) :: SocketType -> Constraint where
    Foo 'Read = Readable
    Foo Write = Writable

type family Operation (op :: SocketOperation) :: * where
    Operation 'Read = IO Message
    Operation Write = Message -> IO ()

type family Readable (t :: SocketType) :: Constraint where
    Readable Dealer = ()
    Readable Pull = ()

type family Writable (t :: SocketType) :: Constraint where
    Writable Dealer = ()
    Writable Push = ()

{-
dealer :: Socket Dealer
dealer = undefined

push :: Socket Push
push = undefined

pull :: Socket Pull
pull = undefined

readSocket :: forall sock . Readable sock => Socket sock -> IO Message
readSocket (Socket _ f) = f (SRead :: SockOp sock 'Read)
-}
