{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module T3927b where

import Data.Proxy
import GHC.Exts

data Message

data SocketType = Dealer | Push | Pull

data SocketOperation = Read | Write

type family Restrict (a :: SocketOperation) (as :: [SocketOperation]) :: Constraint where
    Restrict a (a ': as) = ()
    Restrict x (a ': as) = Restrict x as
    Restrict x '[] = ("Error!" ~ "Tried to apply a restricted type!")

type family Implements (t :: SocketType) :: [SocketOperation] where
    Implements Dealer = ['Read, Write]
    Implements Push = '[Write]
    Implements Pull = '[ 'Read]

data SockOp :: SocketType -> SocketOperation -> * where
    SRead :: SockOp sock 'Read
    SWrite :: SockOp sock Write

data Socket :: SocketType -> * where
    Socket :: proxy sock
           -> (forall op . Restrict op (Implements sock) => SockOp sock op -> Operation op)
           -> Socket sock

type family Operation (op :: SocketOperation) :: * where
    Operation 'Read = IO Message
    Operation Write = Message -> IO ()

class Restrict 'Read (Implements t) => Readable t where
    readSocket :: Socket t -> Operation 'Read
    readSocket (Socket _ f) = f (SRead :: SockOp t 'Read)

instance Readable Dealer

type family Writable (t :: SocketType) :: Constraint where
    Writable Dealer = ()
    Writable Push = ()

dealer :: Socket Dealer
dealer = Socket (Proxy :: Proxy Dealer) f
  where
    f :: Restrict op (Implements Dealer) => SockOp Dealer op -> Operation op
    f SRead = undefined
    f SWrite = undefined

push :: Socket Push
push = Socket (Proxy :: Proxy Push) f
  where
    f :: Restrict op (Implements Push) => SockOp Push op -> Operation op
    f SWrite = undefined

pull :: Socket Pull
pull = Socket (Proxy :: Proxy Pull) f
  where
    f :: Restrict op (Implements Pull) => SockOp Pull op -> Operation op
    f SRead = undefined

foo :: IO Message
foo = readSocket dealer
