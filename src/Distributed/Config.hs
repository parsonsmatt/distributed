{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distributed.Config where

import           Control.Distributed.Process (NodeId (..))
import           Control.Exception.Safe      (IOException, handle, throwIO)
import qualified Data.ByteString.Char8       as BS
import           Data.Text                   (unpack)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Yaml
import           Network.Transport           (EndPointAddress (..))

data Config = Config
    { configHost  :: String
    , configPort  :: String
    , configNodes :: [NodeId]
    } deriving Show

defaultConfig :: Config
defaultConfig = Config "127.0.0.1" "0" []

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        configHost <- unpack <$> o .: "host"
        configPort <- unpack <$> o .: "port"
        nodes <- o .: "nodes"
        let configNodes = fmap (NodeId . EndPointAddress . encodeUtf8) nodes
        pure Config{..}

loadConfigFile :: IO Config
loadConfigFile = handle handler $ do
    file <- BS.readFile "config.yaml"
    case decodeEither file of
        Left err ->
            throwIO . userError $ "There was an error parsing the configuration file: " ++ err
        Right config ->
            pure config
  where
    -- if the file does not exist, use a default configuration.
    handler :: IOException -> IO Config
    handler _ = pure defaultConfig
