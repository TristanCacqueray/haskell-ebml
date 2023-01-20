module Codec.EBML.Pretty where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural

import Codec.EBML.Element
import Codec.EBML.Schema

-- | Pretty-print a 'EBMLDocument'.
prettyEBMLDocument :: [EBMLSchema] -> EBMLDocument -> Text
prettyEBMLDocument schemas (EBMLDocument xs) = mconcat $ map (prettyElement (compileSchemas schemas) 0) xs

prettyElement :: EBMLSchemas -> Natural -> EBMLElement -> Text
prettyElement schemas indent elt = indentTxt <> eltIDTxt <> ": " <> eltValueTxt
  where
    indentTxt = Text.replicate (fromIntegral indent) " "
    eltIDTxt = case lookupSchema elt.header.eid schemas of
        Just schema -> schema.name
        Nothing -> Text.pack (show elt.header.eid)
    eltValueTxt = case elt.value of
        EBMLRoot xs -> "\n" <> mconcat (map (prettyElement schemas (indent + 2)) xs)
        EBMLText txt -> txt <> "\n"
        EBMLBinary bs -> "[raw:" <> Text.pack (show $ BS.length bs) <> " " <> bsTxt bs <> "]\n"
        _ -> "value\n"
    bsTxt = Text.pack . show . BS.take 32
