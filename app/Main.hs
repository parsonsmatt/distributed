module Main where

import Control.Concurrent (threadDelay)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    _ <- runProcess node $ do
        echoPid <- spawnLocal $ forever $
            receiveWait
                [ match $ \(sender, msg) -> send sender (msg :: String)
                , match $ \msg -> say $ "handling " ++ msg
                ]

        say "sending messages"
        send echoPid "hello"
        self <- getSelfPid
        send echoPid (self, "hello world")

        m <- expectTimeout 10000000
        case m of 
            Nothing -> die "no words"
            Just s -> say $ "got " ++ s ++ " back!"

        liftIO $ threadDelay 20000000
    return ()
