module Main (main) where

import Codec.EBML qualified as EBML
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.IO qualified
import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode), stdin, withBinaryFile)

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
        [] -> printSplit EBML.newStreamReader stdin
        _ -> error "usage: haskell-ebml FILE"

printSplit :: EBML.StreamReader -> Handle -> IO ()
printSplit ir handl = do
    putStr "Reading 2048 bytes... "
    buf <- BS.hGet handl 2048
    case EBML.feedReader buf ir of
        Left e -> error (Text.unpack e)
        Right (Nothing, _) | buf == "" -> putStrLn "Done!"
        Right (mFrame, newIR) -> do
            case mFrame of
                Nothing -> putStrLn "Need more data"
                Just frame -> putStrLn $ "Got a new frame: " <> show (BS.length frame.media) <> " " <> show (BS.take 8 frame.media)
            printSplit newIR handl
