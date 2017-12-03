{-# LANGUAGE NumDecimals #-}

module Distributed where

import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad                    (forever)
import           Data.Monoid
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)
import           System.Random.MWC                (GenIO)

newtype SendFor = SendFor { unSendFor :: Int }

newtype WaitFor = WaitFor { unWaitFor :: Int }

run :: GenIO -> SendFor -> WaitFor -> IO ()
run gen sendFor waitFor = do
    startMessages gen
    threadDelay (unSendFor sendFor * 10e6)
    stopMessages
    threadDelay (unWaitFor waitFor * 10e6)
    printResults

startMessages :: GenIO -> IO ()
startMessages = undefined

stopMessages :: IO ()
stopMessages = undefined

printResults :: IO ()
printResults = undefined
