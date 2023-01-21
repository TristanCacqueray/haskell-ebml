{- | This module contains the logic to convert a raw EBMLDocument into a WebMDocument

See: https://www.matroska.org/technical/diagram.html
-}
module Codec.EBML.WebM where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)

import Codec.EBML.Element
import Data.Foldable (find)
import Data.Maybe (catMaybes)

data WebMDocument = WebMDocument
    { timestampScale :: Word64
    , clusters :: [WebMCluster]
    }

data WebMCluster = WebMCluster
    { timestamp :: Word64
    , content :: [EBMLElement]
    }

decodeWebMDocument :: EBMLDocument -> Either Text WebMDocument
decodeWebMDocument = \case
    (EBMLDocument [header, segment]) -> do
        headerElements <- getChilds header
        segmentElements <- getChilds segment
        docType <- getText =<< getElt headerElements 0x4282
        docVersion <- getUInt =<< getElt headerElements 0x4287
        if docType /= "webm" || docVersion /= 2
            then Left ("Invalid doctype: " <> Text.pack (show (docType, docVersion)))
            else decodeSegment segmentElements
    _ -> Left "Invalid EBML file structure"

decodeSegment :: [EBMLElement] -> Either Text WebMDocument
decodeSegment = go 0
  where
    go scale xs@(x : rest)
        | x.header.eid == 0x1F43B675 = WebMDocument scale . catMaybes <$> traverse decodeWebMCluster xs
        | x.header.eid == 0x1549A966 = do
            info <- getChilds x
            scaleValue <- getUInt =<< getElt info 0x2AD7B1
            go scaleValue rest
        | otherwise = go scale rest
    go scale [] = Right (WebMDocument scale [])

decodeWebMCluster :: EBMLElement -> Either Text (Maybe WebMCluster)
decodeWebMCluster elt
    | elt.header.eid == 0x1F43B675 =
        Just <$> do
            childs <- getChilds elt
            case childs of
                (tsElt : xs)
                    | tsElt.header.eid == 0xE7 -> do
                        timestamp <- getUInt tsElt
                        Right $ WebMCluster timestamp xs
                _ -> Left "Cluster first element is not a timestamp"
    | otherwise = Right Nothing

-- | Extract the document type, version and the segment elements.
documentSegment :: EBMLDocument -> Either Text (Text, Word64, [EBMLElement])
documentSegment (EBMLDocument [header, segment]) = do
    headerElements <- getChilds header
    segmentElements <- getChilds segment
    docType <- getText =<< getElt headerElements 0x4282
    docVersion <- getUInt =<< getElt headerElements 0x4287
    pure (docType, docVersion, segmentElements)
documentSegment _ = Left "Invalid EBML file structure"

getElt :: [EBMLElement] -> EBMLID -> Either Text EBMLElement
getElt xs eid = case find (\elt -> elt.header.eid == eid) xs of
    Just elt -> Right elt
    Nothing -> Left ("Element " <> Text.pack (show eid) <> " not found")

getText :: EBMLElement -> Either Text Text
getText elt = case elt.value of
    EBMLText txt -> Right txt
    _ -> Left "Invalid text value"

getUInt :: EBMLElement -> Either Text Word64
getUInt elt = case elt.value of
    EBMLUnsignedInteger x -> Right x
    _ -> Left ("Invalid uint value " <> Text.pack (show elt.value))

getChilds :: EBMLElement -> Either Text [EBMLElement]
getChilds elt = case elt.value of
    EBMLRoot xs -> Right xs
    _ -> Left "Element is not a root"
