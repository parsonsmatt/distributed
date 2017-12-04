{-# LANGUAGE NumDecimals #-}

module Distributed
    ( module Distributed
    , module Distributed.Types
    ) where

import           Control.Concurrent
import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   hiding (newLocalNode)
import           Control.Monad
import           Data.Foldable                                      (for_)
import           System.Random.MWC                                  (GenIO,
                                                                     uniformR)

import           Distributed.Types

remoteTable :: RemoteTable
remoteTable = initRemoteTable

run :: GenIO -> SendFor -> WaitFor -> IO ()
run gen sendFor waitFor = do
    t <- initializeBackend "127.0.0.1" "10501" remoteTable
    node <- newLocalNode t
    runProcess node $ do
        _ <- spawnLocal $ do
            receivingPid <- spawnLocal $ pure ()
            sendingPid <- spawnLocal $ sendingProcess gen t
            _ <- spawnLocal $ timerProcess sendFor waitFor sendingPid receivingPid
            pure ()
        pure ()

    threadDelay (calculateSendTime sendFor + calculateWaitTime waitFor)

sendingProcess :: GenIO -> Backend -> Process ()
sendingProcess gen backend = forever $ do
    peers <- liftIO $ findPeers backend 1e5

    n <- liftIO $ uniformR (0, 1) gen
    say $ "Sending a new number: " ++ show n
    say $ "Found peers: " ++ show peers
    for_ peers $ \peer ->
        nsendRemote peer "newnumber" (NewNumber n)

timerProcess :: SendFor -> WaitFor -> ProcessId -> ProcessId -> Process ()
timerProcess sendFor waitFor sendingPid receivingPid = do
    say "Waiting for send time to end..."
    liftIO (threadDelay (calculateSendTime sendFor))
    say "Sending Stop!"
    send sendingPid StopSending
    liftIO (threadDelay (calculateWaitTime waitFor))
    say "Sending Shutdown!"
    send receivingPid Shutdown
