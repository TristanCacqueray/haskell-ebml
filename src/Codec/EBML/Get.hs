module Codec.EBML.Get where

import Data.Binary.Get (Get, bytesRead, getByteString, isEmpty, lookAheadM)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word64)

import Codec.EBML.Element
import Codec.EBML.Schema

getElement :: EBMLSchemas -> Get EBMLElement
getElement schemas = do
    elth <- getElementHeader
    getElementValue schemas elth

getElementValue :: EBMLSchemas -> EBMLElementHeader -> Get EBMLElement
getElementValue schemas elth = do
    -- here is a good place to add traceM debug
    val <- case lookupSchema elth.eid schemas of
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
getBinary elth = case elth.size of
    Nothing -> fail "Invalid binary header size"
    Just sz -> EBMLBinary <$> getByteString (fromIntegral sz)

getText :: EBMLElementHeader -> Get EBMLValue
getText elth = case elth.size of
    Nothing -> fail "Invalid text header size"
    Just sz -> EBMLText . decodeUtf8 <$> getByteString (fromIntegral sz)

getUnsignedInteger :: EBMLElementHeader -> Get EBMLValue
getUnsignedInteger = getBinary

getRoot :: EBMLSchemas -> EBMLElementHeader -> Get EBMLValue
getRoot schemas elth = case elth.size of
    Nothing -> EBMLRoot <$> getUntil schemas elth.eid
    Just sz -> getRootFixed schemas sz

getUntil :: EBMLSchemas -> EBMLID -> Get [EBMLElement]
getUntil schemas eid = go
  where
    getChild :: Get (Maybe EBMLElement)
    getChild = do
        -- This is not exactly correct. The rfc-8794 spec (chapter 6.2) says we should decode until
        -- any valid parent or global element. Because the EBMLSchema doesn't yet contain this information,
        -- and because in practice such unknown-sized element are segment/cluster, we simply decode until
        -- we find a matching element.
        elth <- getElementHeader
        if elth.eid == eid
            then pure Nothing
            else Just <$> getElementValue schemas elth

    go = do
        empty <- isEmpty
        if empty
            then pure []
            else goGet

    goGet =
        lookAheadM getChild >>= \case
            Just elt -> do
                elts <- go
                pure (elt : elts)
            Nothing -> pure []

getRootFixed :: EBMLSchemas -> Word64 -> Get EBMLValue
getRootFixed schemas sz = do
    startPosition <- bytesRead
    let maxPosition = startPosition + fromIntegral sz
        getChilds = do
            currentPosition <- bytesRead
            if
                    | currentPosition > maxPosition ->
                        fail $ "Element decode position " <> show currentPosition <> " exceed parent size " <> show sz
                    | currentPosition == maxPosition ->
                        pure []
                    | otherwise -> do
                        elt <- getElement schemas
                        elts <- getChilds
                        pure (elt : elts)
    EBMLRoot <$> getChilds
