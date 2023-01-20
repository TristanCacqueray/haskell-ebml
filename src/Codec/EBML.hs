module Codec.EBML (
    webmSchemas,
    decodeFile,
    prettyElements,
) where

import Data.Binary.Get (runGet)
import Data.ByteString qualified as BS

import Codec.EBML.Element
import Codec.EBML.Get
import Codec.EBML.Header
import Codec.EBML.Pretty
import Codec.EBML.Schema

webmSchemas :: [EBMLSchema]
webmSchemas = schemaHeader

decodeFile :: [EBMLSchema] -> FilePath -> IO [EBMLElement]
decodeFile schemas fp = do
    bs <- BS.readFile fp
    pure $ runGet (getElements (compileSchemas schemas)) (BS.fromStrict bs)
