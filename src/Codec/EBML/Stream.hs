module Codec.EBML.Stream (StreamReader, newStreamReader, StreamFrame (..), feedReader) where

import Control.Monad (when)
import Data.Binary.Get qualified as Get
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS

import Codec.EBML.Element
import Codec.EBML.Get
import Codec.EBML.Header
import Codec.EBML.Schema

data StreamFrame = StreamFrame
    { elements :: [EBMLElement]
    -- ^ Either the initial elements up to the first cluster, either the cluster child elements.
    , buffer :: BS.ByteString
    -- ^ The raw data.
    }

-- | A stream reader hold on the data previously read, until a new frame is completed.
data StreamReader = StreamReader
    { acc :: BS.Builder
    , consumed :: Int
    , decoder :: Get.Decoder [EBMLElement]
    }

streamSchema :: EBMLSchemas
streamSchema = compileSchemas schemaHeader

-- | Read the initialization frame.
getInitialization :: Get.Get [EBMLElement]
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
    pure [elt, EBMLElement segmentHead (EBMLRoot elts)]

-- | Read a cluster frame.
getCluster :: Get.Get [EBMLElement]
getCluster = do
    clusterHead <- getElementHeader
    when (clusterHead.eid /= 0x1F43B675) do
        fail $ "Invalid cluster: " <> show clusterHead
    getUntil streamSchema 0x1F43B675

-- | Initialize a stream reader
newStreamReader :: StreamReader
newStreamReader = StreamReader mempty 0 (Get.runGetIncremental getInitialization)

{- | Feed data into a stream reader, returns the list of completed frames, and either an error, or the updated stream reader.

The first frame contains the initialization data, then the next are the media segments.
-}
feedReader :: BS.ByteString -> StreamReader -> ([StreamFrame], Either String StreamReader)
feedReader = go []
  where
    segmentDecoder = Get.runGetIncremental getCluster
    strictBuilder = BS.toStrict . BS.toLazyByteString
    -- This is the end
    go [] "" ir = case Get.pushEndOfInput ir.decoder of
        Get.Fail _ _ s -> ([], Left s)
        Get.Partial _ -> ([], Left "Missing data")
        Get.Done "" _ xs -> ([StreamFrame xs (strictBuilder ir.acc)], Right ir)
        Get.Done{} -> ([], Left "Left-over data")
    -- Feed the decoder
    go chunks bs ir =
        case Get.pushChunk ir.decoder bs of
            Get.Fail _ _ s -> (reverse chunks, Left s)
            newDecoder@(Get.Partial _) ->
                let newAcc = ir.acc <> BS.byteString bs
                    newIR = StreamReader newAcc (ir.consumed + BS.length bs) newDecoder
                 in (reverse chunks, Right newIR)
            Get.Done leftover consumed xs -> do
                let doneChunk = BS.byteString (BS.take (fromIntegral consumed - ir.consumed) bs)
                    newChunk = StreamFrame xs (strictBuilder $ ir.acc <> doneChunk)
                    newIR = StreamReader mempty 0 segmentDecoder
                 in go (newChunk : chunks) leftover newIR
