module Codec.EBML.Matroska where

import Data.Text (Text)

import Codec.EBML.Decoder
import Codec.EBML.Element
import Codec.EBML.Schema

schemaHeader :: [EBMLSchema]
schemaHeader =
    [ EBMLSchema "EBML" 0x1A45DFA3 getRoot
    , EBMLSchema "DocType" 0x4282 (const getText)
    , EBMLSchema "Segment" 0x18538067 getRoot
    , EBMLSchema "Info" 0x1549A966 getRoot
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
    , ("TimestampScale", 0x2AD7B1)
    , ("Timestamp", 0xE7)
    ]
