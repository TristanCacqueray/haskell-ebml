module Codec.EBML.Schema where

import Data.Binary.Get (Get)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Codec.EBML.Element

data EBMLSchema = EBMLSchema
    { name :: Text
    , eid :: EBMLID
    , decode :: EBMLSchemas -> EBMLElementHeader -> Get EBMLValue
    }

newtype EBMLSchemas = EBMLSchemas (Map EBMLID EBMLSchema)

compileSchemas :: [EBMLSchema] -> EBMLSchemas
compileSchemas = EBMLSchemas . Map.fromList . map toKV
  where
    toKV schema = (schema.eid, schema)

lookupSchema :: EBMLID -> EBMLSchemas -> Maybe EBMLSchema
lookupSchema eid (EBMLSchemas schemas) = Map.lookup eid schemas
