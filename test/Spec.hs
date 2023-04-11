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
import Data.Word (Word64, Word8)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

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
        , testCase "Incremental lookahead" (pure ())
        , testProperty "Put <-> Get" propPutGet
        ]

propPutGet :: Word64 -> Bool
propPutGet value
    | value > 72057594037927936 = True
    | otherwise =
        runGet EBML.getDataSize (runPut (EBML.putDataSize value)) == value

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
        , testCase "Incremental lookahead" testIncrementalLookahead
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
                                Left e
                                    | cur == "" -> reverse acc
                                    | otherwise -> error (Text.unpack e)
                                Right (mFrame, nextSR) -> go next nextSR newAcc
                                  where
                                    newAcc = maybe acc (: acc) mFrame
                    frames = go bs EBML.newStreamReader []
                -- this works because the chunk size is small enough to get every segment.
                length frames @?= clusterCount
                BS.length (head frames).initialization @?= headerSize
                BS.take 4 (head frames).initialization @?= "\x1A\x45\xdf\xa3"
                forM_ frames $ \frame -> do
                    BS.take 4 frame.media @?= "\x1f\x43\xb6\x75"
            ]

    testIncrementalLookahead = do
        let eltA = 0x10
            segment = 0x2b
            eltB = 0x43
            cluster1 = 0x104
            eltC = 0x113
            cluster2 = 0x36b
        -- validate the addresses
        BS.take 4 (BS.drop segment streamFile) @?= "\x18\x53\x80\x67"
        BS.take 4 (BS.drop cluster1 streamFile) @?= "\x1f\x43\xb6\x75"
        BS.take 4 (BS.drop cluster2 streamFile) @?= "\x1f\x43\xb6\x75"
        let getIDat pos =
                let elth = runGet EBML.getElementHeader (BS.fromStrict $ BS.drop pos streamFile)
                 in elth.eid
        getIDat eltA @?= EBML.EBMLID 0x42f7
        getIDat eltB @?= EBML.EBMLID 0x1549a966
        getIDat eltC @?= EBML.EBMLID 0xa3

        -- validate the first two media cluster
        let c1 = "\x1f\x43\xb6\x75\x01\xff\xff\xff\xff\xff\xff\xff\xe7\x81\x06\xa3"
            c2 = "\x1f\x43\xb6\x75\x01\xff\xff\xff\xff\xff\xff\xff\xe7\x82\x03\xee\xa3"
        BS.drop cluster1 streamFile `checkPrefix` c1
        BS.drop cluster2 streamFile `checkPrefix` c2

        -- Test stream reader with pathological buffer size
        let runIncrementalTest size = testIncremental size EBML.newStreamReader streamFile [c1, c2]
        -- Make sure the first two cluster are covered.
        forM_ [1, 2, 7] runIncrementalTest
        forM_ [eltA, segment, eltB, cluster1, eltC] $ \pos -> do
            runIncrementalTest pos
            forM_ [15, 8, 7, 4, 2, 1] $ \offset -> do
                runIncrementalTest (pos - offset)
                runIncrementalTest (pos + offset)

        -- Until c2 can be parsed, the latest media segment must be c1
        testIncremental cluster2 EBML.newStreamReader streamFile [c1]
        forM_ [1, 2, 3] $ \offset -> do
            testIncremental (cluster2 - offset) EBML.newStreamReader streamFile [c1]
            testIncremental (cluster2 + offset) EBML.newStreamReader streamFile [c1]

        -- The buffer contains both clusters, the latest media segment must be c2
        forM_ [4, 5, 8, 16, 24] $ \offset -> do
            testIncremental (cluster2 + offset) EBML.newStreamReader streamFile [c2]

checkPrefix :: HasCallStack => BS.ByteString -> BS.ByteString -> Assertion
checkPrefix b1 b2 = BS.take (BS.length b2) b1 @?= BS.take (BS.length b1) b2

testIncremental :: HasCallStack => Int -> EBML.StreamReader -> BS.ByteString -> [BS.ByteString] -> IO ()
testIncremental _ _ _ [] = pure ()
testIncremental _ _ "" _ = error "Reached the end of file"
testIncremental size sr buf clusters@(x : xs) = do
    let (chunk, nextBuf) = BS.splitAt size buf
    case EBML.feedReader chunk sr of
        Left e -> error (Text.unpack e)
        Right (mFrame, nextSR) -> do
            nextClusters <- case mFrame of
                Nothing -> pure clusters
                Just f -> do
                    (f.media <> nextBuf) `checkPrefix` x
                    pure xs
            testIncremental size nextSR nextBuf nextClusters
