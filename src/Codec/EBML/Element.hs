-- | EBML core data decoder, see: https://matroska-org.github.io/libebml/specs.html
module Codec.EBML.Element where

import Data.Binary.Get (Get, getWord8)
import Data.Bits (Bits (shift, testBit, (.|.)), (.&.))
import Data.Word (Word32, Word64)

newtype EBMLID = EBMLID Word32
    deriving (Show)
    deriving newtype (Num, Eq, Ord)

data EBMLElementHeader = EBMLElementHeader
    { eid :: EBMLID
    , size :: Word64
    }
    deriving (Show)

getElementHeader :: Get EBMLElementHeader
getElementHeader = EBMLElementHeader <$> getElementID <*> getDataSize

getElementID :: Get EBMLID
getElementID =
    EBMLID <$> do
        b1 <- getWord8
        let w1 = fromIntegral b1
        if
                | b1 `testBit` 7 -> getVar 0 w1
                | b1 `testBit` 6 -> getVar 1 w1
                | b1 `testBit` 5 -> getVar 2 w1
                | b1 `testBit` 4 -> getVar 3 w1
                | otherwise -> fail ("Invalid width: " <> show b1)

getDataSize :: Get Word64
getDataSize = do
    b1 <- getWord8
    if
            | b1 `testBit` 7 -> getVar 0 (fromIntegral (b1 .&. 127))
            | b1 `testBit` 6 -> getVar 1 (fromIntegral (b1 .&. 63))
            | b1 `testBit` 5 -> getVar 2 (fromIntegral (b1 .&. 31))
            | b1 `testBit` 4 -> getVar 3 (fromIntegral (b1 .&. 15))
            | b1 `testBit` 3 -> getVar 4 (fromIntegral (b1 .&. 7))
            | b1 `testBit` 2 -> getVar 5 (fromIntegral (b1 .&. 3))
            | b1 `testBit` 1 -> getVar 6 (fromIntegral (b1 .&. 1))
            | b1 `testBit` 0 -> getVar 7 0
            | otherwise -> pure 0

getVar :: (Num a, Bits a) => Int -> a -> Get a
getVar 0 acc = pure acc
getVar n acc = do
    b <- getWord8
    getVar (n - 1) ((acc `shift` 8) .|. fromIntegral b)
