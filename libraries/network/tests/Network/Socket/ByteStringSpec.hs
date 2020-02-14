{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.ByteStringSpec (main, spec) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString
import Network.Test.Common

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "send" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock = do
                    close sock
                    send sock testMsg `shouldThrow` anyException
            tcpTest client server

        it "checks -1 correctly on Windows" $ do
            sock <- socket AF_INET Stream defaultProtocol
            send sock "hello world" `shouldThrow` anyException

    describe "sendAll" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = sendAll sock testMsg
            tcpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock = do
                    close sock
                    sendAll sock testMsg `shouldThrow` anyException
            tcpTest client server

    describe "sendTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock addr = sendTo sock testMsg addr
            udpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock addr = do
                    close sock
                    sendTo sock testMsg addr `shouldThrow` anyException
            udpTest client server

    describe "sendAllTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock addr = sendAllTo sock testMsg addr
            udpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock addr = do
                    close sock
                    sendAllTo sock testMsg addr `shouldThrow` anyException
            udpTest client server

    describe "sendMany" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` S.append seg1 seg2
                client sock = sendMany sock [seg1, seg2]

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            tcpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock = do
                    close sock
                    sendMany sock [seg1, seg2] `shouldThrow` anyException

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            tcpTest client server

    describe "sendManyTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` S.append seg1 seg2
                client sock addr = sendManyTo sock [seg1, seg2] addr

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            udpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock addr = do
                    close sock
                    sendManyTo sock [seg1, seg2] addr `shouldThrow` anyException

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            udpTest client server

    describe "recv" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "throws when closed" $ do
            let server sock = do
                    close sock
                    recv sock 1024 `shouldThrow` anyException
                client sock = send sock testMsg
            tcpTest client server

        it "can treat overflow" $ do
            let server sock = do
                    seg1 <- recv sock (S.length testMsg - 3)
                    seg2 <- recv sock 1024
                    let msg = S.append seg1 seg2
                    msg `shouldBe` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "returns empty string at EOF" $ do
            let client s = recv s 4096 `shouldReturn` S.empty
                server s = shutdown s ShutdownSend
            tcpTest client server

        it "checks -1 correctly on Windows" $ do
            sock <- socket AF_INET Stream defaultProtocol
            recv sock 1024 `shouldThrow` anyException

    describe "recvFrom" $ do
        it "works well" $ do
            let server sock = do
                    (msg, _) <- recvFrom sock 1024
                    testMsg `shouldBe` msg
                client sock = do
                    addr <- getPeerName sock
                    sendTo sock testMsg addr
            tcpTest client server

        it "throws when closed" $ do
            let server sock = do
                    close sock
                    recvFrom sock 1024 `shouldThrow` anyException
                client sock = do
                    addr <- getPeerName sock
                    sendTo sock testMsg addr
            tcpTest client server

        it "can treat overflow" $ do
            let server sock = do
                    (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                    (seg2, _) <- recvFrom sock 1024
                    let msg = S.append seg1 seg2
                    testMsg `shouldBe` msg
                client sock = send sock testMsg
            tcpTest client server

        it "returns empty string at EOF" $ do
            let server sock = do
                    (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                    seg1 `shouldBe` S.empty
                client sock = shutdown sock ShutdownSend
            tcpTest client server
