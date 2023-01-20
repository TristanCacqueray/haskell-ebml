module Codec.EBML.Get where

import Data.Binary.Get (Get, bytesRead, getByteString, isEmpty)
import Data.Map.Strict qualified as Map

import Codec.EBML.Element
import Codec.EBML.Schema
import Data.Text.Encoding (decodeUtf8)

getElement :: EBMLSchemas -> Get EBMLElement
getElement schemas = do
    elth <- getElementHeader
    val <- case Map.lookup elth.eid schemas.getSchemas of
        Nothing -> getBinary elth
        Just schema -> schema.decode schemas elth
    pure $ EBMLElement elth val

getDocument :: EBMLSchemas -> Get EBMLDocument
getDocument schemas = EBMLDocument <$> go
  where
    go = do
        empty <- isEmpty
        if empty
            then pure []
            else do
                elt <- getElement schemas
                elts <- go
                pure (elt : elts)

getBinary :: EBMLElementHeader -> Get EBMLValue
getBinary elth = EBMLBinary <$> getByteString (fromIntegral elth.size)

getText :: EBMLElementHeader -> Get EBMLValue
getText elth = EBMLText . decodeUtf8 <$> getByteString (fromIntegral elth.size)

getUnsignedInteger :: EBMLElementHeader -> Get EBMLValue
getUnsignedInteger = getBinary

getRoot :: EBMLSchemas -> EBMLElementHeader -> Get EBMLValue
getRoot schemas elth = do
    startPosition <- bytesRead
    let maxPosition = startPosition + fromIntegral elth.size
        getChilds = do
            currentPosition <- bytesRead
            if
                    | currentPosition > maxPosition ->
                        fail $ "Element decode position " <> show currentPosition <> " exceed parent size " <> show elth.size
                    | currentPosition == maxPosition ->
                        pure []
                    | otherwise -> do
                        elt <- getElement schemas
                        elts <- getChilds
                        pure (elt : elts)
    EBMLRoot <$> getChilds
