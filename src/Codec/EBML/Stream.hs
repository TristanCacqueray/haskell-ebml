{- | This module contains the incremental decoder logic to process a continuous stream.

Here is an example stream layout:

> | EBML | SIZE | ELT | ELTA | ELT | ... | SEGMENT | USIZE | ELT | ELTB | ... |
> | CLUSTER | USIZE | ELT | ELTC   | ... | CLUSTER | USIZE | ELT | ELT  | ... |

There are two difficulties:

- The element are not aligned, a segment id can start at position 15.
- Unknown sized element, such as segment and cluster, need to use a look-ahead to ensure it is completed.

Here are the main scenarios:

- The initial buffers does not contains the begining of a media segment.
  In that case we need to accumulate the data to provide the complete initialization segments.

- The buffer contains multiple segments. In that case we need to find the last one, e.g. the most recent.

- The buffer ends on the middle of the cluster id, e.g. "...\x1f\x43".
  In that case we need to wait for the next buffer to confirm a new media segment exists.
  We also need to returns the end of the previous buffer, so that the media segment does start with "\x1f\x43...".
  This is somehow already managed by the 'Data.Binary.Get.runGetIncremental'.

Checkout the 'testIncrementalLookahead' case in the test/Spec.hs module that validates these scenarios.
-}
module Codec.EBML.Stream (StreamReader, newStreamReader, StreamFrame (..), feedReader) where

import Control.Monad (when)
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text

import Codec.EBML.Decoder
import Codec.EBML.Element
import Codec.EBML.Matroska
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
    { header :: Either (Int, [BS.ByteString]) BS.ByteString
    -- ^ The stream initialization segments, either an accumulator (read bytes, list of buffer), either the full segments.
    , decoder :: Get.Decoder ()
    -- ^ The current decoder.
    }

streamSchema :: EBMLSchemas
streamSchema = compileSchemas schemaHeader

-- | Read elements until the first cluster eid.
getUntilNextCluster :: Get.Get [EBMLElement]
getUntilNextCluster =
    Get.lookAheadM getNonCluster >>= \case
        Just elt -> do
            elts <- getUntilNextCluster
            pure (elt : elts)
        Nothing -> pure []
  where
    getNonCluster = do
        eid <- getElementID
        if eid == 0x1F43B675
            then pure Nothing
            else do
                elth <- EBMLElementHeader eid <$> getMaybeDataSize
                Just <$> getElementValue streamSchema elth

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
    elts <- getUntilNextCluster
    case WebM.decodeSegment elts of
        Right _webmDocument -> pure ()
        Left err -> fail (Text.unpack err)

-- | Read a cluster frame.
getCluster :: Get.Get ()
getCluster = do
    clusterHead <- getElementHeader
    when (clusterHead.eid /= 0x1F43B675) do
        fail $ "Invalid cluster: " <> show clusterHead
    elts <- getUntilNextCluster
    case elts of
        (elt : _) | elt.header.eid == 0xE7 -> pure ()
        _ -> fail "Cluster first element is not a timestamp"

-- | Initialize a stream reader.
newStreamReader :: StreamReader
newStreamReader = StreamReader (Left (0, [])) (Get.runGetIncremental getInitialization)

-- | Feed data into a stream reader. Returns either an error, or maybe a new 'StreamFrame' and an updated StreamReader.
feedReader :: BS.ByteString -> StreamReader -> Either Text (Maybe StreamFrame, StreamReader)
feedReader = go Nothing
  where
    -- This is the end
    go Nothing "" _ = Left "empty buffer"
    -- Feed the decoder
    go mFrame bs sr =
        case Get.pushChunk sr.decoder bs of
            Get.Fail _ _ s -> Left (Text.pack s)
            -- More data is needed.
            newDecoder@(Get.Partial _) -> Right (mFrame, newSR)
              where
                -- Accumulate the buffer for the initialization segments if needed.
                newHeader = case sr.header of
                    Left (consumed, acc) -> Left (consumed + BS.length bs, bs : acc)
                    Right _ -> sr.header
                newSR = StreamReader newHeader newDecoder
            Get.Done leftover consumed _ -> go newFrame leftover newSR
              where
                -- The header is either the one already parsed, or the current complete decoded buffer.
                newHeader = case sr.header of
                    Left (prevConsumed, acc) ->
                        let currentPos = fromIntegral consumed - prevConsumed
                         in mconcat $ reverse (BS.take currentPos bs : acc)
                    Right header -> header
                -- The new frame starts after what was decoded.
                newFrame = Just (StreamFrame newHeader leftover)
                newSR = StreamReader (Right newHeader) (Get.runGetIncremental getCluster)
