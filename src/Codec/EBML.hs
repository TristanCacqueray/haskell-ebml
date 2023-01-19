module Codec.EBML where

import Data.Binary.Get (runGet)
import Data.ByteString qualified as BS

import Codec.EBML.Get
import Codec.EBML.Header
import Codec.EBML.Schema
import Codec.EBML.Value

webmSchemas :: [EBMLSchema]
webmSchemas = schemaHeader

decodeFile :: [EBMLSchema] -> FilePath -> IO [EBMLElement]
decodeFile schemas fp = do
    bs <- BS.readFile fp
    pure $ runGet (getElements (compileSchemas schemas)) (BS.fromStrict bs)
