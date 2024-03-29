-- | EBML core data decoder, see: https://matroska-org.github.io/libebml/specs.html
module Codec.EBML.Element where

import Control.Monad (when)
import Data.Binary.Get (Get, getWord8)
import Data.Binary.Put (Put, putWord8)
import Data.Bits (Bits (shift, testBit, (.|.)), (.&.))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)

-- | EBML document structure, including the Header and Body Root.
newtype EBMLDocument = EBMLDocument [EBMLElement]

-- | EBML element id.
newtype EBMLID = EBMLID Word32
    deriving (Show)
    deriving newtype (Num, Eq, Ord)

-- | EBML element header.
data EBMLElementHeader = EBMLElementHeader
    { eid :: EBMLID
    , size :: Maybe Word64
    -- ^ size is Nothing for unknown-sized element.
    }
    deriving (Eq, Show)

-- | EBML element.
data EBMLElement = EBMLElement
    { header :: EBMLElementHeader
    , value :: EBMLValue
    }
    deriving (Eq, Show)

-- | EBML element value.
data EBMLValue
    = EBMLRoot [EBMLElement]
    | EBMLSignedInteger Int64
    | EBMLUnsignedInteger Word64
    | EBMLFloat Double
    | EBMLText Text
    | EBMLDate Text
    | EBMLBinary ByteString
    deriving (Eq, Show)

getElementHeader :: Get EBMLElementHeader
getElementHeader = EBMLElementHeader <$> getElementID <*> getMaybeDataSize

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

getMaybeDataSize :: Get (Maybe Word64)
getMaybeDataSize = do
    sz <- getDataSize
    pure $
        -- TODO: better check for unknown-sized for different VINT_DATA size.
        -- though, it seems like this is the common value.
        if sz == 0xFFFFFFFFFFFFFF
            then Nothing
            else Just sz

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

putDataSize :: Word64 -> Put
putDataSize v
    | v <= 126 = putVar 0 (v .|. 128)
    | v <= 16382 = putVar 1 (v .|. 16384)
    | v <= 2097150 = putVar 2 (v .|. 2097152)
    | v <= 268435454 = putVar 3 (v .|. 268435456)
    | v <= 34359738366 = putVar 4 (v .|. 34359738368)
    | v <= 4398046511102 = putVar 5 (v .|. 4398046511104)
    | v <= 562949953421310 = putVar 6 (v .|. 562949953421312)
    | v <= 72057594037927936 = putVar 7 (v .|. 72057594037927936)
    | otherwise = error "Invalid size"

putVar :: Int -> Word64 -> Put
putVar n acc = do
    putWord8 $ fromIntegral (part .&. 0xff)
    when (n > 0) $ putVar (n - 1) acc
  where
    part = acc `shift` ((-1) * 8 * n)
