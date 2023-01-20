module Main (main) where

import Codec.EBML.Element (getDataSize)
import Data.Binary.Get (runGet)
import Data.Binary.Put (putWord8, runPut)
import Data.ByteString.Char8 qualified as BS
import Data.Char (digitToInt)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Word (Word64, Word8)
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = defaultMain $ testGroup "Codec.EBML" [unitTests]

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
    val = show $ runGet getDataSize bs
    bs = runPut $ traverse_ (putWord8 . readOctet) $ chunksOf 8 $ filter (/= ' ') str

readOctet :: String -> Word8
readOctet s
    | length s /= 8 = error $ "Invalid length: " <> s
    | otherwise = fromIntegral $ foldl' (\acc x -> acc * 2 + digitToInt x) 0 s
