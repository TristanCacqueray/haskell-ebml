module Codec.EBML.Schema where

import Data.Binary.Get (Get)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Codec.EBML.Element
import Codec.EBML.Value

data EBMLSchema = EBMLSchema
    { name :: Text
    , eid :: EBMLID
    , decode :: EBMLSchemas -> EBMLElementHeader -> Get EBMLValue
    }

type EBMLSchemas = Map EBMLID EBMLSchema

compileSchemas :: [EBMLSchema] -> EBMLSchemas
compileSchemas = Map.fromList . map toKV
  where
    toKV schema = (schema.eid, schema)
