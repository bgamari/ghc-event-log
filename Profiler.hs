{-# LANGUAGE BangPatterns #-}

module Profiler
    ( -- * Parsing program metdata
      parseBlocks, Block(..)
    , blockToBlockMap, BlockMap
      -- * Parsing samples
    , getSamples, Sample(..), Address(..), addrRange
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
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Numeric (showHex)

import Data.Binary.Get
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP

import EventLog
import qualified IntRangeMap as RM
import IntRangeMap (RangeMap, Range(..))

newtype Address = Addr Int
                deriving (Ord, Eq)

instance Show Address where
  showsPrec _ (Addr n) = showHex n

type BlockMap = RM.RangeMap Block

data LineCol = LineCol { lineNumber, columnNumber :: !Int }
data Span = Span { spanStart, spanEnd :: !LineCol }
data SourceNote = SourceNote { srcFile :: !BSS.ShortByteString
                             , srcSpan :: !Span
                             }

data Block = Block { blkName :: {-# UNPACK #-} !BSS.ShortByteString
                   , blkChildren :: [Block]
                   , blkRegions :: [Range]
                   , blkSrcNotes :: [SourceNote]
                   }

blockToBlockMap :: Block -> BlockMap
blockToBlockMap = go
  where
    go blk = foldMap (`RM.singleton` blk) (blkRegions blk) <> foldMap go (blkChildren blk)
{-# INLINE blockToBlockMap #-}

parseBlocks :: Monad m => Producer Record (Producer Block m) () -> Producer Block m ()
parseBlocks = evalStateT go
  where
    go :: Monad m => Parser Record (Producer Block m) ()
    go = do
      mblk <- goBlock
      case mblk of
        Just blk -> lift (yield blk) >> go
        Nothing  -> return ()

    emptyBlock name = Block name mempty mempty mempty

    goBlock :: Monad m => Parser Record m (Maybe Block)
    goBlock = do
      res <- draw
      case res of
        Just r
          | r `isOfType` 200 -> do
              blk <- goBlockBody $ emptyBlock (BSS.toShort $ BSL.toStrict $ recBody r)
              return $ Just blk
          | otherwise -> goBlock
        _ -> return Nothing

    goBlockBody !blk = do
      res <- draw
      case res of
        Just r
          | r `isOfType` 200 -> do
              child <- goBlockBody $ emptyBlock (BSS.toShort $ BSL.toStrict $ recBody r)
              goBlockBody $ blk { blkChildren = child : blkChildren blk }
          | r `isOfType` 201 ->
              return blk
          | r `isOfType` 202 -> do
              let Right (_, _, rng) = runGetOrFail getRange (recBody r)
              rng `seq` goBlockBody blk { blkRegions = rng : blkRegions blk }
          | r `isOfType` 203 -> do
              let Right (_, _, snote) = runGetOrFail getSourceNote (recBody r)
              snote `seq` goBlockBody blk { blkSrcNotes = snote : blkSrcNotes blk }
          | otherwise        ->
              goBlockBody blk
        Nothing -> return blk

    toBSS :: BSL.ByteString -> BSS.ShortByteString
    toBSS = BSS.toShort . BSL.toStrict

    getSourceNote :: Get SourceNote
    getSourceNote = SourceNote <$> fmap toBSS getLazyByteStringNul <*> getSpan

    getSpan :: Get Span
    getSpan = Span <$> getLineCol <*> getLineCol

    getLineCol :: Get LineCol
    getLineCol = LineCol <$> fmap fromIntegral getWord32be
                         <*> fmap fromIntegral getWord32be

    getRange :: Get Range
    getRange = addrRange <$> getAddress <*> getAddress
{-# INLINEABLE parseBlocks #-}

addrRange :: Address -> Address -> Range
addrRange (Addr a) (Addr b) = Rng a b

getAddress :: Get Address
getAddress = Addr . fromIntegral <$> getWord64be

type Histogram = M.Map Address Int

histogram :: Monad m => Producer [Sample] m () -> m Histogram
histogram = PP.fold (foldr addSample) M.empty id
  where
    addSample (Sample addr weight) = M.insertWith (+) addr weight
{-# INLINEABLE histogram #-}

getSamples :: Monad m => Pipe Record [Sample] m ()
getSamples = {-# SCC "getSamples" #-} forever $ await >>= go
  where
    go r
      | r `isOfType` 210 = yield $ decodeSamples $ BSL.drop 4 $ recBody r
      | otherwise        = return ()
{-# INLINEABLE getSamples #-}

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
        15 ->               fromIntegral <$> getWord64be
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
