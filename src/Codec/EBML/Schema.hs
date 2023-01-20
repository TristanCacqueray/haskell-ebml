module Codec.EBML.Schema where

import Data.Binary.Get (Get)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

import Codec.EBML.Element

{- | EBML schema definition.

Note that this is missing:

- min/max occurrence constraint.
- default value.
- element path.
-}
data EBMLSchema = EBMLSchema
    { name :: Text
    -- ^ The element name.
    , eid :: EBMLID
    -- ^ The element id.
    , decode :: EBMLSchemas -> EBMLElementHeader -> Get EBMLValue
    -- ^ How to decode the element value.
    }

newtype EBMLSchemas = EBMLSchemas (Map EBMLID EBMLSchema)

-- | Combine a list of schema for decoder.
compileSchemas :: [EBMLSchema] -> EBMLSchemas
compileSchemas = EBMLSchemas . Map.fromList . map toKV
  where
    toKV schema = (schema.eid, schema)

lookupSchema :: EBMLID -> EBMLSchemas -> Maybe EBMLSchema
lookupSchema eid (EBMLSchemas schemas) = Map.lookup eid schemas
