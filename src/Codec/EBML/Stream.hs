module Codec.EBML.Stream (StreamReader, newStreamReader, StreamFrame (..), feedReader) where

import Control.Monad (void, when)
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text

import Codec.EBML.Element
import Codec.EBML.Get
import Codec.EBML.Header
import Codec.EBML.Schema
import Codec.EBML.WebM qualified as WebM

-- | A valid frame that can be served.
data StreamFrame = StreamFrame
    { initialization :: BS.ByteString
    -- ^ The initialization segments, to be provided before the first media segment.
    , media :: BS.ByteString
    -- ^ The begining of the last media segment found in the input buffer.
    }

-- | Create a stream reader with 'newStreamReader', and decode media segments with 'feedReader'.
data StreamReader = StreamReader
    { acc :: [BS.ByteString]
    -- ^ Accumulate data in case the header is not completed in the first buffer.
    , consumed :: Int
    -- ^ Keep track of the decoder position accross multiple buffers.
    , header :: Maybe BS.ByteString
    -- ^ The stream initialization segments.
    , decoder :: Get.Decoder ()
    -- ^ The current decoder.
    }

streamSchema :: EBMLSchemas
streamSchema = compileSchemas schemaHeader

-- | Read the initialization frame.
getInitialization :: Get.Get ()
getInitialization = do
    -- Read the EBML header element
    elt <- getElement streamSchema
    when (elt.header.eid /= 0x1A45DFA3) do
        fail $ "Invalid magic: " <> show elt.header

    -- Read the begining of the first segment, until the first cluster
    segmentHead <- getElementHeader
    when (segmentHead.eid /= 0x18538067) do
        fail $ "Invalid segment: " <> show segmentHead
    elts <- getUntil streamSchema 0x1F43B675
    case WebM.decodeSegment elts of
        Right _webmDocument -> pure ()
        Left err -> fail (Text.unpack err)

-- | Read a cluster frame.
getCluster :: Get.Get ()
getCluster = do
    clusterHead <- getElementHeader
    when (clusterHead.eid /= 0x1F43B675) do
        fail $ "Invalid cluster: " <> show clusterHead
    getClusterBody

getClusterBody :: Get.Get ()
getClusterBody = do
    elts <- getUntil streamSchema 0x1F43B675
    case elts of
        (elt : _) | elt.header.eid == 0xE7 -> pure ()
        _ -> fail "Cluster first element is not a timestamp"

getClusterRemaining :: Get.Get ()
getClusterRemaining = do
    elth <- getElementHeader
    if elth.eid == 0x1F43B675
        then -- This is in fact a new cluster, get its body
            getClusterBody
        else -- This is a cluster left-over, let's keep on reading until a new start
            void (getUntil streamSchema 0x1F43B675)

-- | Initialize a stream reader.
newStreamReader :: StreamReader
newStreamReader = StreamReader [] 0 Nothing (Get.runGetIncremental getInitialization)

-- | Feed data into a stream reader. Returns either an error, or maybe a new 'StreamFrame' and an updated StreamReader.
feedReader :: BS.ByteString -> StreamReader -> Either Text (Maybe StreamFrame, StreamReader)
feedReader = go Nothing
  where
    -- This is the end
    go Nothing "" sr = case Get.pushEndOfInput sr.decoder of
        Get.Fail _ _ s -> Left (Text.pack s)
        Get.Partial _ -> Left "Missing data"
        Get.Done "" _ _ -> Right (Nothing, sr)
        Get.Done{} -> Left "Left-over data"
    -- Feed the decoder
    go mFrame bs sr =
        case Get.pushChunk sr.decoder bs of
            Get.Fail _ _ s -> Left (Text.pack s)
            newDecoder@(Get.Partial _) ->
                let newAcc = case sr.header of
                        Nothing -> bs : sr.acc
                        -- We don't need to accumulate data once the header is known.
                        Just _ -> []
                    newSR = StreamReader newAcc (sr.consumed + BS.length bs) sr.header newDecoder
                 in Right (mFrame, newSR)
            Get.Done leftover consumed _
                | BS.null leftover ->
                    -- We might have ended on a in-cluster element, use the remainingDecoder next time
                    Right (mFrame, newIR remainingDecoder)
                | otherwise ->
                    -- There might be a new frame after, keep on decoding
                    go newFrame leftover (newIR segmentDecoder)
              where
                -- The header is either the one already parsed, or the current complete decoded buffer.
                newHeader = case sr.header of
                    Just header -> header
                    Nothing ->
                        let currentPos = fromIntegral consumed - sr.consumed
                         in mconcat $ reverse (BS.take currentPos bs : sr.acc)
                -- The new frame starts after what was decoded.
                newFrame = Just (StreamFrame newHeader leftover)
                newIR = StreamReader [] 0 (Just newHeader)

    remainingDecoder = Get.runGetIncremental getClusterRemaining
    segmentDecoder = Get.runGetIncremental getCluster
