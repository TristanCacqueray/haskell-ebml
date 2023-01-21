module Main where

import Codec.EBML qualified as EBML
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Text qualified as Text
import Data.Text.IO qualified
import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode), withBinaryFile)

main :: IO ()
main =
    getArgs >>= \case
        [fp] -> do
            let schemas = EBML.webmSchemas
            Right ebml <- EBML.decodeEBMLFile schemas fp
            Data.Text.IO.putStrLn (EBML.prettyEBMLDocument schemas ebml)
        ["split", fp] -> do
            let ir = EBML.newStreamReader
            withBinaryFile fp ReadMode (printSplit ir)
        _ -> error "usage: haskell-ebml FILE"

printSplit :: EBML.StreamReader -> Handle -> IO ()
printSplit ir handl = do
    putStrLn "Reading 2048 bytes"
    buf <- BS.hGet handl 2048
    let (chunks, result) = EBML.feedReader buf ir
    traverse_ printChunk chunks
    case result of
        Left e -> error (Text.unpack e)
        Right nextIR
            | buf == mempty -> putStrLn "Done."
            | otherwise -> printSplit nextIR handl
  where
    printChunk bs = putStrLn $ "Got chunk: " <> show (BS.length bs.buffer)
