-- | Header schema definition, see: https://github.com/ietf-wg-cellar/ebml-specification/blob/master/specification.markdown#ebml-header-elements
module Codec.EBML.Header where

import Data.Text (Text)

import Codec.EBML.Element
import Codec.EBML.Get
import Codec.EBML.Schema

schemaHeader :: [EBMLSchema]
schemaHeader =
    [ EBMLSchema "EBML" 0x1A45DFA3 getRoot
    , EBMLSchema "DocType" 0x4282 (const getText)
    , EBMLSchema "Segment" 0x18538067 getRoot
    , EBMLSchema "Cluster" 0x1F43B675 getRoot
    ]
        <> map fromUints uints

fromUints :: (Text, EBMLID) -> EBMLSchema
fromUints (n, i) = EBMLSchema n i (const getUnsignedInteger)

uints :: [(Text, EBMLID)]
uints =
    [ ("EBMLVersion", 0x4286)
    , ("EBMLReadVersion", 0x42F7)
    , ("EBMLMaxIDLength", 0x42F2)
    , ("EBMLMaxSizeLength", 0x42F3)
    , ("DocTypeVersion", 0x4287)
    , ("DocTypeReadVersion", 0x4285)
    ]
