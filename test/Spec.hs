module Main (main) where

import Control.Exception (SomeException, try)
import Data.Binary.Get (runGet)
import Data.Binary.Put (putWord8, runPut)
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Text qualified as Text
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Codec.EBML qualified as EBML

main :: IO ()
main = do
    sampleFile <- readFileMaybe "./data/Volcano_Lava_Sample.webm"
    defaultMain $ testGroup "Codec.EBML" [unitTests, integrationTests sampleFile]
  where
    readFileMaybe fp = fromRight "" <$> try @SomeException (BS.readFile fp)

unitTests :: TestTree
unitTests =
    testGroup
        "Unit tests"
        [ goldenVsString "VarInt" "data/var-int.golden"
            . pure
            . BS.fromStrict
            . BS.unlines
            . map testVarInts
            $ [ "1000 0010"
              , "0100 0000 0000 0010"
              , "0010 0000 0000 0000 0000 0010"
              , "0001 0000 0000 0000 0000 0000 0000 0010"
              ]
        ]

testVarInts :: String -> BS.ByteString
testVarInts str = BS.pack $ padStr <> " => " <> padVal
  where
    padStr = replicate (42 - length str) ' ' <> str
    padVal = replicate (8 - length val) ' ' <> val
    val = show $ runGet EBML.getDataSize bs
    bs = runPut $ traverse_ (putWord8 . readOctet) $ chunksOf 8 $ filter (/= ' ') str

readOctet :: String -> Word8
readOctet s
    | length s /= 8 = error $ "Invalid length: " <> s
    | otherwise = fromIntegral $ foldl' (\acc x -> acc * 2 + digitToInt x) 0 s

integrationTests :: BS.ByteString -> TestTree
integrationTests sampleFile = testGroup "Integration tests" [testCase "sample" decodeSample]
  where
    decodeSample
        | BS.length sampleFile /= 53054906 = do
            -- the file was not checkout
            pure ()
        | otherwise =
            case EBML.decodeWebM (BS.fromStrict sampleFile) of
                Left e -> error (Text.unpack e)
                Right webM -> do
                    webM.timestampScale @?= 1000000
                    length webM.clusters @?= 49
                    (head webM.clusters).timestamp @?= 0
                    (webM.clusters !! 1).timestamp @?= 2794
