module Bytes where

import           Data.Bits
import           Data.Word

serialize :: Int -> [Word8]
serialize n = serialize' (abs n) (n < 0)
  where
    serialize' m neg = if next == 0
                       then if neg then byte' else [byte]
                       else byte : serialize' next neg
      where
        next     = m `shiftR` 8
        byte     = fromIntegral m .&. 0xff
        byte'    = if overflow then byte : [0x80] else [byte .|. 0x80]
        overflow = byte .&. 0x80 /= 0
