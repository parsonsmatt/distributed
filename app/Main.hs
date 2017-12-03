{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad                    (forever)
import           Data.Monoid
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters)

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS8
import           Options.Applicative

data Args
    = Args
    { argsSendFor  :: Double
    , argsWaitFor  :: Double
    , argsWithSeed :: Maybe ByteString
    }

argsParser :: ParserInfo Args
argsParser = info (args <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Run the distributed example"
    , header "Funtimes with Cloud Haskell!"
    ]
  where
    sendForArg = 
        option auto $ mconcat
            [ long "send-for"
            , metavar "DOUBLE"
            , help "How long to send messagers to neighbor nodes."
            ]
    waitForArg = 
        option auto $ mconcat 
            [ long "wait-for"
            , metavar "DOUBLE"
            , help "How long to wait after sending messages." 
            ]
    withSeedArg = 
       optional . fmap BS8.pack . strOption . mconcat $ 
           [ long "with-seed"
           , metavar "SEED"
           , help "The seed for the randon number generator"
           ]
    args = Args
        <$> sendForArg
        <*> waitForArg
        <*> withSeedArg

main :: IO ()
main = do
    Args {..} <- execParser argsParser
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    _ <- runProcess node $ do
        echoPid <- spawnLocal $ forever $
            receiveWait
                [ match $ \(sender, msg) -> send sender (msg :: String)
                , match $ \msg -> say $ "handling " ++ msg
                ]

        say "sending messages"
        send echoPid ("hello" :: String)
        self <- getSelfPid
        send echoPid (self, "hello world" :: String)

        m <- expectTimeout 10000000
        case m of
            Nothing -> die ("no words" :: String)
            Just s  -> say $ "got " ++ s ++ " back!"

        liftIO $ threadDelay 20000000
    return ()
