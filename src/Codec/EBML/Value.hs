module Codec.EBML.Value where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)

import Codec.EBML.Element

data EBMLElement = EBMLElement
    { header :: EBMLElementHeader
    , value :: EBMLValue
    }
    deriving (Show)

data EBMLValue
    = EBMLRoot [EBMLElement]
    | EBMLSignedInteger Int64
    | EBMLUnsignedInteger Word64
    | EBMLFloat Double
    | EBMLText Text
    | EBMLDate Text
    | EBMLBinary ByteString
    deriving (Show)
