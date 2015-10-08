{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Profiler
    ( -- * Parsing program metdata
      blockEvents, blockEventsAll, Block(..)
    , buildBlockMap, BlockMap
      -- ** Source notes
    , SourceNote(..)
    , LineCol(..)
    , Span(..)
      , showSourceNote
      -- * Parsing samples
    , getSamples, Sample(..), Address(..), addrRange
      -- * Histogramming samples
    , histogram, Histogram
    ) where

import Control.Applicative (many)
import Data.Bits
import Data.Maybe (isNothing)
import Data.Word
import Data.Char -- FIXME
import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Numeric (showHex)

import Data.Binary.Get
import Pipes
import Control.Monad.State
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
             deriving (Show)

data Span = Span { spanStart, spanEnd :: !LineCol }
          deriving (Show)

data SourceNote = SourceNote { srcFile :: !BSS.ShortByteString
                             , srcSpan :: !Span
                             }
                deriving (Show)

data Block = Block { blkName :: {-# UNPACK #-} !BSS.ShortByteString
                   , blkParents :: [Block]
                   , blkSrcNotes :: [SourceNote]
                   }
           deriving (Show)

showSourceNote :: SourceNote -> String
showSourceNote (SourceNote f (Span s e)) =
    map (chr . fromIntegral) (BSS.unpack f)++":"++showLC s++"-"++showLC e
  where
    showLC (LineCol l c) = show l++"."++show c

buildBlockMap :: Monad m => Producer BlockEvent m a -> m (BlockMap, a)
buildBlockMap prod =
    evalStateT (PP.foldM' (\accum blkev -> (accum <>) <$> buildBlockMap' blkev)
                          (pure mempty) pure
                          (hoist lift prod)
               ) []

buildBlockMap' :: Monad m => BlockEvent -> StateT [Block] m BlockMap
buildBlockMap' (StartBlockEv name) = do
    parents <- get
    push $ Block name parents mempty
    return mempty
buildBlockMap' EndBlockEv = pop >> return mempty
buildBlockMap' (AddressRangeEv rng) = do
    blk <- getHead
    return $ RM.singleton rng blk
buildBlockMap' (SourceNoteEv srcnote) = do
    modifyHead $ \blk -> blk {blkSrcNotes = srcnote : blkSrcNotes blk}
    return mempty

getHead :: Monad m => StateT [a] m a
getHead = do
    xs <- get
    case xs of
      []   -> error "getHead: Unexpected empty stack"
      x:xs -> return x

modifyHead :: Monad m => (a -> a) -> StateT [a] m ()
modifyHead f = do
    x <- pop
    push $! f x

push :: Monad m => a -> StateT [a] m ()
push !a = modify (a:)

pop :: Monad m => StateT [a] m a
pop = do
    xs <- get
    case xs of
      []   -> error "pop: Unexpected empty stack"
      x:xs -> put xs >> return x

parseFail :: String -> Get a -> BSL.ByteString -> a
parseFail thing get bs =
    case runGetOrFail get bs of
      Left (_, _, e) -> error $ "Failed to parse "++thing++" from "++show bs++": "++e
      Right (_, _, r) -> r

type Parser' a t m r = forall x. StateT (Producer a m x) (t m) r

undraw' :: (Monad m, Monad (t m), MonadTrans t)
        => a -> StateT (Producer a m x) (t m) ()
undraw' x = modify (yield x >>)

draw' :: (Monad m, Monad (t m), MonadTrans t)
      => StateT (Producer a m x) (t m) (Maybe a)
draw' = do
  p <- get
  x <- lift $ lift (next p)
  case x of
    Left r -> do
      put (return r)
      return Nothing
    Right (a, p') -> do
      put p'
      return (Just a)

skipWhile :: (Monad m, Monad (t m), MonadTrans t)
          => (a -> Bool) -> Parser' a t m ()
skipWhile pred = go
  where
    go = do
      x <- draw'
      case x of
        Just r
          | pred r -> go
          | otherwise -> undraw' r
        _ -> return ()

blockEvents :: Monad m
            => Producer Record m () -> Producer BlockEvent m (Producer Record m ())
blockEvents prod =
    mapWhileMaybe parseBlockEvent (prod >-> PP.dropWhile (isNothing . parseBlockEvent))

blockEventsAll :: Monad m
               => Producer Record m () -> Producer BlockEvent m (Producer Record m ())
blockEventsAll prod = do
    prod >-> PP.mapFoldable parseBlockEvent
    return prod

-- | A record describing the flattened basic block tree
data BlockEvent = StartBlockEv BSS.ShortByteString
                | EndBlockEv
                | AddressRangeEv Range
                | SourceNoteEv SourceNote

-- | Parses all of the program block records from an event log, ultimately
-- returning a producer consisting of all of the left-overs, starting with the first
-- non-program-block event.
parseBlockEvent :: Record -> Maybe BlockEvent
parseBlockEvent r
  | r `isOfType` 200 =
    Just $ StartBlockEv $ toBSS $ recBody r
  | r `isOfType` 201 =
    Just $ EndBlockEv
  | r `isOfType` 202 =
    Just $ AddressRangeEv $ parseFail "address range" getRange (recBody r)
  | r `isOfType` 203 =
    Just $ SourceNoteEv $ parseFail "source note" getSourceNote (recBody r)
  | otherwise =
    Nothing
  where
    toBSS :: BSL.ByteString -> BSS.ShortByteString
    toBSS = BSS.toShort . BSL.toStrict

    getRange = addrRange <$> getAddress <*> getAddress

    getSourceNote :: Get SourceNote
    getSourceNote = SourceNote <$> fmap toBSS getLazyByteStringNul <*> getSpan

    getSpan :: Get Span
    getSpan = Span <$> getLineCol <*> getLineCol

    getLineCol :: Get LineCol
    getLineCol = LineCol <$> fmap fromIntegral getWord32be
                         <*> fmap fromIntegral getWord32be

mapWhileMaybe :: Monad m
              => (a -> Maybe b) -> Producer a m r -> Producer b m (Producer a m r)
mapWhileMaybe f = go
  where
    go prod = do
      mb <- lift $ next prod
      case mb of
        Left r -> return $ return r
        Right (x, prod')
          | Just y <- f x -> yield y >> go prod'
          | otherwise     -> return (yield x >> prod')

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
