{-# LANGUAGE BangPatterns #-}

module Profiler
    ( -- * Parsing program metdata
      parseBlocks, Block(..)
    , blocksToBlockMap, BlockMap
      -- * Parsing samples
    , getSamples, Sample(..), Address(..)
      -- * Histogramming samples
    , histogram, Histogram
    ) where

import Control.Applicative (many)
import Control.Monad (forever)
import Data.Bits
import Data.Word
import Data.Monoid
import Data.Foldable
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Numeric (showHex)

import Data.Binary.Get
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP

import EventLog
import qualified RangeMap as RM
import RangeMap (RangeMap, Range(..))

newtype Address = Addr Word64
                deriving (Ord, Eq)

instance Show Address where
  showsPrec _ (Addr n) = showHex n

type BlockMap = RM.RangeMap Address Block

data Block = Block { blkName :: !BS.ByteString
                   , blkChildren :: [Block]
                   , blkRegions :: [Range Address]
                   }

blocksToBlockMap :: [Block] -> BlockMap
blocksToBlockMap = foldMap go
  where
    go :: Block -> BlockMap
    go blk =
      foldMap (`RM.singleton` blk) (blkRegions blk) <> foldMap go (blkChildren blk)

parseBlocks :: Monad m => Producer Record (Producer Block m) () -> Producer Block m ()
parseBlocks = evalStateT go
  where
    go :: Monad m => Parser Record (Producer Block m) ()
    go = do
      mblk <- goBlock
      case mblk of
        Just blk -> lift (yield blk) >> go
        Nothing  -> return ()

    goBlock :: Monad m => Parser Record m (Maybe Block)
    goBlock = do
      res <- draw
      case res of
        Just r
          | r `isOfType` 200 -> do
              blk <- goBlockBody $ Block (BSL.toStrict $ recBody r) mempty mempty
              return $ Just blk
          | otherwise -> goBlock
        _ -> return Nothing

    goBlockBody blk = do
      res <- draw
      case res of
        Just r
          | r `isOfType` 200 -> do
              child <- goBlockBody $ Block (BSL.toStrict $ recBody r) mempty mempty
              goBlockBody $ blk { blkChildren = child : blkChildren blk }
          | r `isOfType` 201 ->
              return blk
          | r `isOfType` 202 -> do
              let Right (_, _, rng) = runGetOrFail getRange (recBody r)
              goBlockBody $ blk { blkRegions = rng : blkRegions blk }
          | otherwise        ->
              goBlockBody blk
        Nothing -> return blk

    getRange :: Get (Range Address)
    getRange = Rng <$> getAddress <*> getAddress

getAddress :: Get Address
getAddress = Addr <$> getWord64be

type Histogram = M.Map Address Int

histogram :: Monad m => Producer [Sample] m () -> m Histogram
histogram = PP.fold (foldr addSample) M.empty id
  where
    addSample (Sample addr weight) = M.insertWith (+) addr weight
{-# INLINE histogram #-}

getSamples :: Monad m => Pipe Record [Sample] m ()
getSamples = forever $ await >>= go
  where
    go r
      | r `isOfType` 210 = yield $ decodeSamples $ BSL.drop 4 $ recBody r
      | otherwise        = return ()
{-# INLINE getSamples #-}

data Sample = Sample !Address !Int
            deriving (Show)

decodeSamples :: BSL.ByteString -> [Sample]
decodeSamples = go (Addr 0)
  where
    go' _ bs = case runGetOrFail (many getAddress) bs of
      Right (bs', _, xs) -> map (\addr -> Sample addr 1) xs

    go lastAddr bs
      | BSL.null bs = []
      | otherwise = case runGetOrFail (getSample lastAddr) bs of
          Right (bs', _, s@(Sample addr _)) -> s : go addr bs'
          Left (bs', _, err) -> error err

    getSample (Addr lastAddr) = do
      header <- getWord8
      let sampleEncoding = header `shiftR` 4
          weightEncoding = header .&. 0xf

      addr <- Addr <$> case sampleEncoding of
        0 -> (lastAddr +) . fromIntegral <$> getWord8
        1 -> (lastAddr -) . fromIntegral <$> getWord8
        4 -> (lastAddr +) . fromIntegral <$> getWord32be
        5 -> (lastAddr -) . fromIntegral <$> getWord32be
        15 -> getWord64be
        _  -> fail "Failed to decode sample"

      weight <- case weightEncoding of
        0 -> pure 1
        1 -> fromIntegral <$> getWord8
        2 -> fromIntegral <$> getWord16be
        4 -> fromIntegral <$> getWord32be
        8 -> fromIntegral <$> getWord64be
        _  -> fail "Failed to decode weight"

      return $ Sample addr weight

isOfType :: Record -> Word16 -> Bool
isOfType r n = evtEventType (recEventType r) == EventType n
