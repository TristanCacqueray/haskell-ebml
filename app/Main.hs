module Main where

import Codec.EBML qualified as EBML
import Data.Text.IO qualified
import System.Environment (getArgs)

main :: IO ()
main =
    getArgs >>= \case
        [fp] -> do
            let schemas = EBML.webmSchemas
            ebml <- EBML.decodeFile schemas fp
            Data.Text.IO.putStrLn (EBML.prettyEBMLDocument schemas ebml)
        _ -> error "usage: haskell-ebml FILE"
