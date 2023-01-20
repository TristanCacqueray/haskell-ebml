module Main where

import Codec.EBML qualified as EBML
import Data.Text.IO qualified
import System.Environment (getArgs)

main :: IO ()
main =
    getArgs >>= \case
        [fp] -> do
            ebml <- EBML.decodeFile EBML.webmSchemas fp
            Data.Text.IO.putStrLn (EBML.prettyElements ebml)
        _ -> error "usage: haskell-ebml FILE"
