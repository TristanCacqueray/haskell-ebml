{- | This module is intended to be imported qualified:

> import qualified Codec.EBML as EBML

Decode a webm file with:

> EBML.decodeFile EBML.webmSchemas "path/file.webm"

References:

 - The matroska schema: https://github.com/ietf-wg-cellar/matroska-specification/blob/master/ebml_matroska.xml
 - The matroska doc: https://www.matroska.org/technical/elements.html
 - The webm guidelines: https://www.webmproject.org/docs/container/
 - The MSE byte stream format spec: https://w3c.github.io/media-source/index.html#byte-stream-format-specs
 - The MSE byte stream format for webm: https://w3c.github.io/mse-byte-stream-format-webm/
-}
module Codec.EBML (
    -- * EBML schemas
    webmSchemas,

    -- * EBML decoder
    decodeEBMLDocument,

    -- * EBML stream reader
    module Codec.EBML.Stream,

    -- * EBML data types
    EBMLDocument (..),
    EBMLElement (..),
    EBMLValue (..),
    EBMLElementHeader (..),
    EBMLID (..),

    -- * EBML schema data types
    EBMLSchema (..),

    -- * Helpers
    decodeFile,
    prettyEBMLDocument,

    -- * Low-level API, mostly for testing
    EBMLSchemas,
    compileSchemas,
    getDocument,
    getElementHeader,
    getElementID,
    getDataSize,
    getElement,
) where

import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as LBS

import Codec.EBML.Element
import Codec.EBML.Get
import Codec.EBML.Header
import Codec.EBML.Pretty
import Codec.EBML.Schema
import Codec.EBML.Stream

-- | The webm document schemas.
webmSchemas :: [EBMLSchema]
webmSchemas = schemaHeader

-- | Lazy decode a 'EBMLDocument'.
decodeEBMLDocument :: [EBMLSchema] -> LBS.ByteString -> Either String EBMLDocument
decodeEBMLDocument schemas lbs = case runGetOrFail (getDocument (compileSchemas schemas)) lbs of
    Left (_, _, err) -> Left err
    Right ("", _, x) -> Right x
    Right (_rest, l, _) -> Left ("Left over data at " <> show l)

-- | Throw an error when the file is invalid.
decodeFile :: [EBMLSchema] -> FilePath -> IO EBMLDocument
decodeFile schemas fp = do
    bs <- LBS.readFile fp
    case decodeEBMLDocument schemas bs of
        Left e -> error e
        Right x -> pure x
