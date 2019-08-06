module EncodingProps where

import           Data.Bits

import           Bytes


prop_sign_bit :: Int -> Bool
prop_sign_bit n = sign == if n < 0 then 0x80 else 0x00
  where
    bytes = serialize n
    sign  = last bytes .&. 0x80
