{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumDecimals   #-}

module Distributed.Types where

import Control.Concurrent (MVar, newEmptyMVar)
import           Data.Binary   (Binary)
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)

-- | A newtype wrapper around an 'Int' number of seconds. Controls how long
-- the program will send messages for.
newtype SendFor = SendFor { unSendFor :: Int }

-- | A newtype wrapper around an 'Int' number of seconds. Controls how long
-- the program will wait after sending messages.
newtype WaitFor = WaitFor { unWaitFor :: Int }

-- | A message used to tell the processes to stop sending messages.
data StopSending = StopSending
    deriving (Show, Generic, Typeable)

instance Binary StopSending

-- | A message used to tell the receiver process to shutdown.
data Shutdown = Shutdown
    deriving (Show, Generic, Typeable)

instance Binary Shutdown

-- | A message that adds a new random number to the list of numbers
-- collected thus far.
newtype NewNumber = NewNumber Double
    deriving (Show, Generic, Typeable)

instance Binary NewNumber

type Messages = [Double]

data AppState = AppState
    { appReceivedMessages :: Messages
    , appCanStop :: MVar ()
    }

finalAnswer :: AppState -> ([Double], Double)
finalAnswer appState = (msgs, sumByIndex)
  where
    sumByIndex = sum $ zipWith (*) [1..] msgs
    msgs = appReceivedMessages appState

initialAppState :: IO AppState
initialAppState = do
    appStop <- newEmptyMVar
    pure AppState 
        { appReceivedMessages = []
        , appCanStop = appStop
        }

addNumber :: Double -> AppState -> AppState
addNumber d appState = appState
    { appReceivedMessages = d : appReceivedMessages appState
    }

-- | Calculate the time to spend waiting in microseconds after the sending
-- period has ended.
calculateWaitTime :: WaitFor -> Int
calculateWaitTime (WaitFor w) = w * 1e6

-- | Calculate the amount of time in microseconds to send messages for.
calculateSendTime :: SendFor -> Int
calculateSendTime (SendFor s) = s * 1e6
