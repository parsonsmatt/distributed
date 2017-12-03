{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Data.Vector                      (fromList)
import           Data.Word                        (Word32)
import           Options.Applicative
import           System.Random.MWC                (createSystemRandom,
                                                   initialize)

import qualified Distributed

main :: IO ()
main = do
    Args{..} <- execParser argsParser
    seed <- maybe createSystemRandom (initialize . fromList . (:[])) argsWithSeed
    Distributed.run seed (Distributed.SendFor argsSendFor) (Distributed.WaitFor argsWaitFor)

data Args
    = Args
    { argsSendFor  :: Int
    , argsWaitFor  :: Int
    , argsWithSeed :: Maybe Word32
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
       optional . option auto . mconcat $
           [ long "with-seed"
           , metavar "SEED"
           , help "The seed for the randon number generator"
           ]
    args = Args
        <$> sendForArg
        <*> waitForArg
        <*> withSeedArg
