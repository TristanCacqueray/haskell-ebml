{- | This module is intended to be imported qualified:

> import qualified Codec.EBML as EBML

Decode a webm file with:

> EBML.decodeWebMFile "path/file.webm"

Split a webm stream segments with:

> let streamReader = EBML.newStreamReader
> buf <- acquire data
> EBML.feedReader buf streamReader

References:

 - EBML specification introduction: https://matroska-org.github.io/libebml/specs.html
 - Document layout: https://www.matroska.org/technical/diagram.html
 - The matroska schema: https://github.com/ietf-wg-cellar/matroska-specification/blob/master/ebml_matroska.xml
 - The matroska schema doc: https://www.matroska.org/technical/elements.html
 - The webm guidelines: https://www.webmproject.org/docs/container/
 - The MSE byte stream format spec: https://w3c.github.io/media-source/index.html#byte-stream-format-specs
 - The MSE byte stream format for webm: https://w3c.github.io/mse-byte-stream-format-webm/
-}
module Codec.EBML (
    -- * WebM decoder
    decodeWebM,
    WebMDocument (..),
    WebMCluster (..),

    -- * Raw EBML decoder
    decodeEBMLDocument,
    webmSchemas,

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
    decodeEBMLFile,
    decodeWebMFile,
    prettyEBMLDocument,

    -- * Low-level API, mostly for testing
    EBMLSchemas,
    compileSchemas,
    getDocument,
    getElementHeader,
    getElementID,
    getDataSize,
    putDataSize,
    getElement,
) where

import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text

import Codec.EBML.Decoder
import Codec.EBML.Element
import Codec.EBML.Matroska
import Codec.EBML.Pretty
import Codec.EBML.Schema
import Codec.EBML.Stream
import Codec.EBML.WebM

-- | Lazy decode a 'WebMDocument'.
decodeWebM :: LBS.ByteString -> Either Text WebMDocument
decodeWebM lbs = decodeWebMDocument =<< decodeEBMLDocument webmSchemas lbs

-- | The webm document schemas.
webmSchemas :: [EBMLSchema]
webmSchemas = schemaHeader

-- | Lazy decode a 'EBMLDocument'.
decodeEBMLDocument :: [EBMLSchema] -> LBS.ByteString -> Either Text EBMLDocument
decodeEBMLDocument schemas lbs = case runGetOrFail (getDocument (compileSchemas schemas)) lbs of
    Left (_, _, err) -> Left (Text.pack err)
    Right ("", _, x) -> Right x
    Right (_rest, l, _) -> Left ("Left over data at " <> Text.pack (show l))

-- | Decode a raw EBML file.
decodeEBMLFile :: [EBMLSchema] -> FilePath -> IO (Either Text EBMLDocument)
decodeEBMLFile schemas fp = decodeEBMLDocument schemas <$> LBS.readFile fp

-- | Decode a webm file.
decodeWebMFile :: FilePath -> IO (Either Text WebMDocument)
decodeWebMFile fp = do
    ebml <- decodeEBMLFile webmSchemas fp
    pure $ decodeWebMDocument =<< ebml
