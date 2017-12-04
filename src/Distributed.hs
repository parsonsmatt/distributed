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
    appState <- initialAppState
    runProcess node $ do
        _ <- spawnLocal $ do
            receivingPid <- spawnLocal $ receivingProcess appState
            sendingPid <- spawnLocal $ sendingProcess gen t
            _ <- spawnLocal $ timerProcess sendFor waitFor sendingPid receivingPid
            pure ()
        pure ()

    takeMVar (appCanStop appState)
    threadDelay (2*1e6)

sendingProcess :: GenIO -> Backend -> Process ()
sendingProcess gen backend = forever $ do
    peers <- liftIO $ findPeers backend 1e5

    mshouldStop <- receiveTimeout 0 
        [ match $ \StopSending -> pure ()
        ]

    for_ mshouldStop $ \_ ->
        die "I am done sending messages."

    n <- liftIO $ uniformR (0, 1) gen
    say $ "Sending a new number: " ++ show n
    say $ "Found peers: " ++ show peers
    for_ peers $ \peer ->
        nsendRemote peer "newnumber" (NewNumber n)

receivingProcess :: AppState -> Process ()
receivingProcess appState = do
    say $ "Current state: " ++ show (appReceivedMessages appState)
    md <- receiveWait 
        [ match $ \(NewNumber d) -> pure (Just d)
        , match $ \Shutdown ->     pure Nothing
        ]
    case md of
        Nothing -> do
            say "Received a Shutdown message, so stopping."
            say $ "Final Answer: " ++ show (finalAnswer appState)
            liftIO $ putMVar (appCanStop appState) ()
        Just d -> do
            say $ "Received: " ++ show d
            receivingProcess (addNumber d appState)

timerProcess :: SendFor -> WaitFor -> ProcessId -> ProcessId -> Process ()
timerProcess sendFor waitFor sendingPid receivingPid = do
    say "Waiting for send time to end..."
    liftIO (threadDelay (calculateSendTime sendFor))
    say "Sending Stop!"
    send sendingPid StopSending
    liftIO (threadDelay (calculateWaitTime waitFor))
    say "Sending Shutdown!"
    send receivingPid Shutdown
