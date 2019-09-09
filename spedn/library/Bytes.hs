module Bytes where

import           Data.Bits
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as L
import           Data.Word

serializeInt :: Int -> [Word8]
serializeInt n = serialize (abs n) (n < 0)
  where
    serialize m neg = if next == 0
                       then if neg then byteNeg else bytePos
                       else byte : serialize next neg
      where
        next     = m `shiftR` 8
        byte     = fromIntegral m .&. 0xff
        bytePos  = if overflow then [byte, 0x00] else [byte]
        byteNeg  = if overflow then [byte, 0x80] else [byte .|. 0x80]
        overflow = byte .&. 0x80 /= 0

serializeStr :: String -> [Word8]
serializeStr = L.unpack . toLazyByteString . stringUtf8

strlen :: String -> Int
strlen = length . serializeStr
