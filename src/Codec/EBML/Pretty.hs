module Codec.EBML.Pretty where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Numeric.Natural

import Codec.EBML.Element

prettyElements :: [EBMLElement] -> Text
prettyElements = mconcat . map (prettyElement 0)

prettyElement :: Natural -> EBMLElement -> Text
prettyElement indent elt = indentTxt <> eltIDTxt <> ": " <> eltValueTxt <> "\n"
  where
    indentTxt = Text.replicate (fromIntegral indent) " "
    eltIDTxt = Text.pack (show elt.header.eid)
    eltValueTxt = case elt.value of
        EBMLRoot xs -> "\n" <> mconcat (map (prettyElement (indent + 2)) xs)
        EBMLText txt -> txt
        EBMLBinary bs -> "[raw:" <> Text.pack (show $ BS.length bs) <> " " <> bsTxt bs <> "]"
        _ -> "value"
    bsTxt bs = Text.replace "\n" "\\n" $ decodeUtf8With lenientDecode (BS.take 64 bs)
