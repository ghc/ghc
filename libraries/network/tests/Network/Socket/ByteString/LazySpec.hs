{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.ByteString.LazySpec (main, spec) where

import Prelude hiding (getContents)

import qualified Data.ByteString.Lazy as L
import Network.Socket
import Network.Socket.ByteString.Lazy
import Network.Test.Common
import Control.Monad

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "send" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock = do
                    close sock
                    send sock lazyTestMsg `shouldThrow` anyException
            tcpTest client server

    describe "sendAll" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = sendAll sock lazyTestMsg
            tcpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock = do
                    close sock
                    sendAll sock lazyTestMsg `shouldThrow` anyException
            tcpTest client server

    describe "getContents" $ do
        it "works well" $ do
            let server sock = getContents sock `shouldReturn` lazyTestMsg
                client sock = do
                    void $ send sock lazyTestMsg
                    shutdown sock ShutdownSend
            tcpTest client server

        it "returns empty string at EOF" $ do
            let client s = getContents s `shouldReturn` L.empty
                server s = shutdown s ShutdownSend
            tcpTest client server

    describe "recv" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "throws when closed" $ do
            let server sock = do
                    close sock
                    recv sock 1024 `shouldThrow` anyException
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "can treat overflow" $ do
            let server sock = do
                    seg1 <- recv sock (L.length lazyTestMsg - 3)
                    seg2 <- recv sock 1024
                    let msg = L.append seg1 seg2
                    msg `shouldBe` lazyTestMsg
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "returns empty string at EOF" $ do
            let client s = recv s 4096 `shouldReturn` L.empty
                server s = shutdown s ShutdownSend
            tcpTest client server
