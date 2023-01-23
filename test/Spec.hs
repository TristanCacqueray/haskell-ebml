module Main (main) where

import Control.Exception (SomeException, try)
import Data.Binary.Get (runGet)
import Data.Binary.Put (putWord8, runPut)
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.Foldable (forM_, traverse_)
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
    streamFile <- readFileMaybe "./data/firefox-mrec-opus.webm"
    defaultMain $ testGroup "Codec.EBML" [unitTests, integrationTests sampleFile streamFile]
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

integrationTests :: BS.ByteString -> BS.ByteString -> TestTree
integrationTests sampleFile streamFile =
    testGroup
        "Integration tests"
        [ testGroup "sample" (decodeFile sampleFile 53054906 4458 49 0 2794)
        , testGroup "stream" (decodeFile streamFile 3499 260 6 6 1006)
        ]
  where
    decodeFile bs size headerSize clusterCount clusterTs1 clusterTs2
        | BS.length bs /= size =
            -- the file was not checkout
            [testCase "skip" (pure ())]
        | otherwise =
            [ testCase "decode" do
                -- lazy decode
                case EBML.decodeWebM (BS.fromStrict bs) of
                    Left e -> error (Text.unpack e)
                    Right webM -> do
                        webM.timestampScale @?= 1_000_000
                        length webM.clusters @?= clusterCount
                        (head webM.clusters).timestamp @?= clusterTs1
                        (webM.clusters !! 1).timestamp @?= clusterTs2
            , testCase "stream" do
                -- incremental decode
                let go buf sr acc =
                        let (cur, next) = BS.splitAt 256 buf
                         in case EBML.feedReader cur sr of
                                Left e -> error (Text.unpack e)
                                Right (mFrame, nextSR)
                                    | cur == "" -> newAcc
                                    | otherwise -> go next nextSR newAcc
                                  where
                                    newAcc = maybe acc (: acc) mFrame
                    frames = reverse $ go bs EBML.newStreamReader []
                -- this works because the chunk size is small enough to get every segment.
                length frames @?= clusterCount
                BS.length (head frames).initialization @?= headerSize
                BS.take 4 (head frames).initialization @?= "\x1A\x45\xdf\xa3"
                forM_ (take 5 frames) $ \frame ->
                    BS.take 4 frame.media @?= "\x1f\x43\xb6\x75"
            ]
