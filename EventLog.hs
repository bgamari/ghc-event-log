module EventLog
    ( -- * Event log
      EventLog
    , hdrEventTypes
    , fromFile
      -- * Event types
    , EventType
    , EventTypeDef
    , evtEventType
    , evtDescription
    , evtExtra
      -- * Events
    , Ref
    , Record(..)
      -- * Reading events
    , records
    , recordsFrom
    ) where

import Pipes
import Numeric (showHex)
import Data.Word
import Data.Int
import Data.Binary.Get
import Data.Binary
import Control.Applicative
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.IO.MMap
import qualified Data.Map.Strict as M
import Prelude hiding (log)

-- | A reference into a point in the event stream
newtype Ref a = Ref Word64
              deriving (Eq, Ord)

expectMagic :: (Binary a, Show a, Integral a) => a -> Get ()
expectMagic magic = do
    m <- get
    when (m /= magic) $
        fail $ "Invalid magic number: expected "++showHex magic ""++" saw "++showHex m ""

-- | A GHC event log
data EventLog = EventLog { logHeader :: Header
                         , logData   :: BSL.ByteString -- ^ starts at the first record
                         }

-- | Read an event log from a file
fromFile :: FilePath -> IO (Either String EventLog)
fromFile path = do
    -- we aren't really using laziness here, but binary expects its input as a
    -- lazy bytestring so this is the path of least resistance.
    bs <- mmapFileByteStringLazy path Nothing
    case runGetOrFail header bs of
      Left (_, _, err) -> return $ Left err
      Right (rest, _, hdr) -> return $ Right $ EventLog hdr rest

-- | An event type number
newtype EventType = EventType Word16
                  deriving (Show, Eq, Ord)

-- | An event log header
data Header = Header { hdrEventTypes :: !(M.Map EventType EventTypeDef) }
            deriving (Show)

magic_HEADER_BEGIN, magic_HEADER_END :: Word32
magic_HEADER_BEGIN = 0x68647262 -- hdrb
magic_HEADER_END   = 0x68647265 -- hdre

magic_HET_BEGIN, magic_HET_END :: Word32
magic_HET_BEGIN    = 0x68657462 -- hetb
magic_HET_END      = 0x68657465 -- hete

magic_ET_BEGIN, magic_ET_END :: Word32
magic_ET_BEGIN     = 0x65746200 -- etb.
magic_ET_END       = 0x65746500 -- ete.

magic_DATA_BEGIN :: Word32
magic_DATA_BEGIN   = 0x64617462 -- datb

-- | Decoder for an event log header
header :: Get Header
header = do
    expectMagic magic_HEADER_BEGIN
    expectMagic magic_HET_BEGIN
    ets <- many eventTypeDef
    expectMagic magic_HET_END
    expectMagic magic_HEADER_END
    expectMagic magic_DATA_BEGIN
    let etMap = M.fromList [ (evtEventType et, et) | et <- ets ]
    return $ Header etMap

-- | A definition of an event type.
data EventTypeDef = EventTypeDef { evtEventType   :: !EventType
                                 , evtSize        :: !Int16
                                 , evtDescription :: !String -- TODO What type should this be?
                                 , evtExtra       :: !BS.ByteString
                                 }
                  deriving (Show)

-- | Decoder for an event type definition.
eventTypeDef :: Get EventTypeDef
eventTypeDef = do
    expectMagic magic_ET_BEGIN
    et <- EventType <$> get
    size <- get
    descLen <- get :: Get Word32
    desc <- getByteString (fromIntegral descLen)
    extraLen <- get :: Get Word32
    extra <- getByteString (fromIntegral extraLen)
    expectMagic magic_ET_END
    return EventTypeDef { evtEventType = et
                        , evtSize = size
                        , evtDescription = BSC.unpack desc
                        , evtExtra = extra
                        }

-- | An event in a GHC event log
data Record = Record { recEventType :: EventTypeDef
                     , recTime      :: Word64
                     , recBody      :: BS.ByteString
                     }
            deriving (Show)

recordOrEnd :: Header -> Get (Maybe Record)
recordOrEnd hdr = end <|> fmap Just (record hdr)
  where end = expectMagic (0xffff :: Word16) *> pure Nothing

-- | Decoder for a 'Record'
record :: Header -> Get Record
record hdr = do
    etNum <- EventType <$> get
    time <- get
    et <- case etNum `M.lookup` hdrEventTypes hdr of
      Nothing -> fail $ "Unknown event type "++show etNum
      Just e -> return e
    len <- case evtSize et of
      -1 -> get :: Get Word16
      n  -> return $ fromIntegral n
    body <- getByteString (fromIntegral len)
    return $ Record et time body

-- | Produce all records in an 'EventLog'
records :: (MonadIO m)
        => EventLog -> Producer (Ref Record, Record) m ()
records log = recordsFrom log (Ref 0)

-- | Produce all records in an 'EventLog' starting with the record pointed
-- to by the given reference.
recordsFrom :: (MonadIO m)
            => EventLog -> Ref Record -> Producer (Ref Record, Record) m ()
recordsFrom log (Ref offset0) =
    go (BSL.drop (fromIntegral offset0) (logData log)) 0
  where
    go buf offset = do
        case runGetOrFail (recordOrEnd $ logHeader log) buf of
          Left (_,_,err)             -> fail err
          Right (rest, nBytes, Just r) -> do
              yield (Ref $ fromIntegral offset, r)
              go rest (offset + nBytes)
          Right (rest, nBytes, Nothing) -> return ()
