-- | Header schema definition, see: https://github.com/ietf-wg-cellar/ebml-specification/blob/master/specification.markdown#ebml-header-elements
module Codec.EBML.Header where

import Codec.EBML.Get
import Codec.EBML.Schema

schemaHeader :: [EBMLSchema]
schemaHeader =
    [ EBMLSchema "EBML" 0x1A45DFA3 getRoot
    , EBMLSchema "EBMLVersion" 0x4286 (const getUnsignedInteger)
    ]
