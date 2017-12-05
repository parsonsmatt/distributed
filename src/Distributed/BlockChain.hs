{-# LANGUAGE DeriveGeneric     #-}

-- | This module contains a highly simplified blockchain, where each block
-- merely contains a single number. Much of the implementation is inspired
-- from
-- <http://www.michaelburge.us/2017/08/17/rolling-your-own-blockchain.html
-- this excellent post> by Michael Burge.
module Distributed.BlockChain where

import           Control.Distributed.Process
import           Crypto.Hash
import Control.Monad.IO.Class
import           Data.Binary
import           Data.ByteString             (ByteString)
-- import qualified Data.ByteString.Lazy        as LZ
import Data.Maybe (fromMaybe)
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           GHC.Generics
import Crypto.Number.Serialize (os2ip)
import Data.ByteArray (convert)
import qualified Control.Foldl as L

-- | A 'Transaction' represents a single number added to the chain.
newtype Transaction
    = Transaction
    { transactionNumber :: Double
    } deriving (Eq, Show, Generic, Typeable)

instance Binary Transaction

-- | To avoid orphan instances, we wrap the 'POSIXTime' type.
newtype Timestamp = Timestamp { unTimestamp :: POSIXTime }
    deriving (Eq, Show, Generic, Typeable)

getTimestamp :: MonadIO m => m Timestamp
getTimestamp = Timestamp <$> liftIO getPOSIXTime

instance Binary Timestamp where
    get = Timestamp . fromInteger <$> get
    put (Timestamp ts) = put (round ts :: Integer)

-- | A 'Header' contains the hash of the previous values in the chain,
-- along with the ProcessId that created this transaction.
data Header
    = Header
    { headerHash      :: ByteString
    , headerProcess   :: ProcessId
    , headerTimestamp :: Timestamp
    , headerNonce     :: Word32
    } deriving (Eq, Show, Generic, Typeable)

instance Binary Header

-- | A 'BlockChainF' is a 'BlockChain' that is generic in the sort of
-- transaction it contains.
type BlockChainF txn = [(Header, txn)]

-- | The 'BlockChain' that we care about in this application is only
-- capable of sending 'Double' number across the wire.
type BlockChain = BlockChainF Transaction

-- | Attempt to add the given 'Transaction' to the 'BlockChain' and credit
-- it to the 'ProcessId' provided.
mine :: Transaction -> ProcessId -> BlockChain -> IO BlockChain
mine txn pid chain = go 0
  where
    hashed = convert (hash256 chain)
    go nonce = do
        ts <- getTimestamp
        let newHeader = Header
                { headerHash = hashed
                , headerProcess = pid
                , headerTimestamp = ts
                , headerNonce = nonce
                }
            newChain = (newHeader, txn) : chain
            validChain bc = difficulty bc < desiredDifficulty chain
        putStrLn $ "Nonce: " ++ show nonce
        putStrLn $ "New:\t\t" ++ show (difficulty newChain) 
        putStrLn $ "Old:\t\t" ++ show (desiredDifficulty chain)
        if validChain newChain
           then pure newChain
           else go (nonce + 1)
           
hash256 :: Binary a => a -> Digest SHA256
hash256 = hashlazy . encode
    
difficulty :: BlockChain -> Integer
difficulty = os2ip . hash256

headers :: BlockChainF a -> [Header]
headers = fmap fst

blockTimeAverage :: BlockChain -> POSIXTime
blockTimeAverage chain = 
    L.fold L.mean (zipWith (-) times (drop 1 times))
  where
    times = take 10 
        . map (unTimestamp . headerTimestamp) 
        . headers 
        $ chain

desiredDifficulty :: BlockChain -> Integer
desiredDifficulty = round . loop
  where
    loop :: BlockChain -> POSIXTime
    loop [] = 
        fromInteger genesisBlockDifficulty / 4
    loop x@(_ : rest) = 
        loop rest / min 4.0 (fromMaybe 4 $ targetTime `safeDiv` blockTimeAverage x)

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv d n = Just (d / n)

-- | The 'genesisBlockDifficulty' is a value that we use to establish how
-- difficult it should be to mine the very first block.
genesisBlockDifficulty :: Integer
genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- | The 'targetTime' is the amount of time that we want mining a block to
-- take, on average, in seconds.
targetTime :: Num a => a
targetTime = 1
