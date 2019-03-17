{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Script where

import           Data.Data
import           Data.Persist
import           Data.Word
import           Data.ByteString (ByteString)
import           GHC.Generics

import           Bytes
import           IR
import           Syntax

data OP_CODE
      -- Constants
    = OP_FALSE
    | OP_PUSHDATA0 Word8 [Word8]
    | OP_PUSHDATA1 [Word8] [Word8]
    | OP_PUSHDATA2 [Word8] [Word8]
    | OP_PUSHDATA4 [Word8] [Word8]
    | OP_PUSH Name    -- Pseudo-opcode used in uninstantiated contract template
    | OP_1NEGATE
    | OP_RESERVED
    | OP_TRUE
    | OP_N Int
      -- Flow control
    | OP_NOP
    | OP_VER
    | OP_IF
    | OP_NOTIF
    | OP_VERIF
    | OP_VERNOTIF
    | OP_ELSE
    | OP_ENDIF
    | OP_VERIFY
    | OP_RETURN
      -- Stack operations
    | OP_TOALTSTACK
    | OP_FROMALTSTACK
    | OP_IFDUP
    | OP_DEPTH
    | OP_DROP
    | OP_DUP
    | OP_NIP
    | OP_OVER
    | OP_PICK
    | OP_ROLL
    | OP_ROT
    | OP_SWAP
    | OP_TUCK
    | OP_2DROP
    | OP_2DUP
    | OP_3DUP
    | OP_2OVER
    | OP_2ROT
    | OP_2SWAP
      -- Array operations
    | OP_CAT
    | OP_SPLIT
    | OP_NUM2BIN
    | OP_BIN2NUM
    | OP_SIZE
      -- Bitwise logic
    | OP_INVERT
    | OP_AND
    | OP_OR
    | OP_XOR
    | OP_EQUAL
    | OP_EQUALVERIFY
    | OP_RESERVED1
    | OP_RESERVED2
      -- Arithmetic
    | OP_1ADD
    | OP_1SUB
    | OP_2MUL
    | OP_2DIV
    | OP_NEGATE
    | OP_ABS
    | OP_NOT
    | OP_0NOTEQUAL
    | OP_ADD
    | OP_SUB
    | OP_MUL
    | OP_DIV
    | OP_MOD
    | OP_LSHIFT
    | OP_RSHIFT
    | OP_BOOLAND
    | OP_BOOLOR
    | OP_NUMEQUAL
    | OP_NUMEQUALVERIFY
    | OP_NUMNOTEQUAL
    | OP_LESSTHAN
    | OP_GREATERTHAN
    | OP_LESSTHANOREQUAL
    | OP_GREATERTHANOREQUAL
    | OP_MIN
    | OP_MAX
    | OP_WITHIN
      -- Crypto
    | OP_RIPEMD160
    | OP_SHA1
    | OP_SHA256
    | OP_HASH160
    | OP_HASH256
    | OP_CODESEPARATOR
    | OP_CHECKSIG
    | OP_CHECKSIGVERIFY
    | OP_CHECKMULTISIG
    | OP_CHECKMULTISIGVERIFY
      -- New operations
    | OP_NOP1
    | OP_CHECKLOCKTIMEVERIFY
    | OP_CHECKLOCKTIME  -- | Pseudo-opcode removed in postprocessing, simplifies compilation
    | OP_CHECKSEQUENCEVERIFY
    | OP_CHECKSEQUENCE  -- | Pseudo-opcode removed in postprocessing, simplifies compilation
    | OP_NOP4
    | OP_NOP5
    | OP_NOP6
    | OP_NOP7
    | OP_NOP8
    | OP_NOP9
    | OP_NOP10
    | OP_CHECKDATASIG
    | OP_CHECKDATASIGVERIFY
      -- Other
    | OP_PUBKEYHASH
    | OP_PUBKEY
    | OP_INVALIDOPCODE Word8
    deriving (Eq, Data, Generic)

instance Show OP_CODE where
    show (OP_PUSHDATA0 len payload) = "PUSH(" ++ show len ++ ")" ++ show payload
    show (OP_PUSHDATA1 len payload) = "PUSHDATA1" ++ show len ++ show payload
    show (OP_PUSHDATA2 len payload) = "PUSHDATA2" ++ show len ++ show payload
    show (OP_PUSHDATA4 len payload) = "PUSHDATA4" ++ show len ++ show payload
    show (OP_PUSH name)             = "<" ++ name ++ ">"
    show (OP_N n)                   = show n
    show op                         = drop 3 $ show $ toConstr op

putByte :: Word8 -> Put ()
putByte = put

putBytes :: [Word8] -> Put ()
putBytes = put

instance Persist OP_CODE where
    put op = case op of
        -- Push values
        OP_PUSHDATA0 len bytes -> putByte len >> putBytes bytes
        OP_PUSHDATA1 len bytes -> putByte 0x4c >> putBytes len >> putBytes bytes
        OP_PUSHDATA2 len bytes -> putByte 0x4d >> putBytes len >> putBytes bytes
        OP_PUSHDATA4 len bytes -> putByte 0x4e >> putBytes len >> putBytes bytes
        OP_FALSE               -> putByte 0x00
        OP_1NEGATE             -> putByte 0x4f
        OP_RESERVED            -> putByte 0x50
        OP_TRUE                -> putByte 0x51
        OP_N n                 -> putByte . fromIntegral $ 0x50 + n

        -- Flow Control
        OP_NOP                 -> putByte 0x61
        OP_VER                 -> putByte 0x62
        OP_IF                  -> putByte 0x63
        OP_NOTIF               -> putByte 0x64
        OP_VERIF               -> putByte 0x65
        OP_VERNOTIF            -> putByte 0x66
        OP_ELSE                -> putByte 0x67
        OP_ENDIF               -> putByte 0x68
        OP_VERIFY              -> putByte 0x69
        OP_RETURN              -> putByte 0x6a

        -- Stack Operations
        OP_TOALTSTACK          -> putByte 0x6b
        OP_FROMALTSTACK        -> putByte 0x6c
        OP_2DROP               -> putByte 0x6d
        OP_2DUP                -> putByte 0x6e
        OP_3DUP                -> putByte 0x6f
        OP_2OVER               -> putByte 0x70
        OP_2ROT                -> putByte 0x71
        OP_2SWAP               -> putByte 0x72
        OP_IFDUP               -> putByte 0x73
        OP_DEPTH               -> putByte 0x74
        OP_DROP                -> putByte 0x75
        OP_DUP                 -> putByte 0x76
        OP_NIP                 -> putByte 0x77
        OP_OVER                -> putByte 0x78
        OP_PICK                -> putByte 0x79
        OP_ROLL                -> putByte 0x7a
        OP_ROT                 -> putByte 0x7b
        OP_SWAP                -> putByte 0x7c
        OP_TUCK                -> putByte 0x7d

        -- Array operations
        OP_CAT                 -> putByte 0x7e
        OP_SPLIT               -> putByte 0x7f
        OP_NUM2BIN             -> putByte 0x80
        OP_BIN2NUM             -> putByte 0x81
        OP_SIZE                -> putByte 0x82

        -- Bitwise logic
        OP_INVERT              -> putByte 0x83
        OP_AND                 -> putByte 0x84
        OP_OR                  -> putByte 0x85
        OP_XOR                 -> putByte 0x86
        OP_EQUAL               -> putByte 0x87
        OP_EQUALVERIFY         -> putByte 0x88
        OP_RESERVED1           -> putByte 0x89
        OP_RESERVED2           -> putByte 0x8a

        -- Arithmetic
        OP_1ADD                -> putByte 0x8b
        OP_1SUB                -> putByte 0x8c
        OP_2MUL                -> putByte 0x8d
        OP_2DIV                -> putByte 0x8e
        OP_NEGATE              -> putByte 0x8f
        OP_ABS                 -> putByte 0x90
        OP_NOT                 -> putByte 0x91
        OP_0NOTEQUAL           -> putByte 0x92
        OP_ADD                 -> putByte 0x93
        OP_SUB                 -> putByte 0x94
        OP_MUL                 -> putByte 0x95
        OP_DIV                 -> putByte 0x96
        OP_MOD                 -> putByte 0x97
        OP_LSHIFT              -> putByte 0x98
        OP_RSHIFT              -> putByte 0x99
        OP_BOOLAND             -> putByte 0x9a
        OP_BOOLOR              -> putByte 0x9b
        OP_NUMEQUAL            -> putByte 0x9c
        OP_NUMEQUALVERIFY      -> putByte 0x9d
        OP_NUMNOTEQUAL         -> putByte 0x9e
        OP_LESSTHAN            -> putByte 0x9f
        OP_GREATERTHAN         -> putByte 0xa0
        OP_LESSTHANOREQUAL     -> putByte 0xa1
        OP_GREATERTHANOREQUAL  -> putByte 0xa2
        OP_MIN                 -> putByte 0xa3
        OP_MAX                 -> putByte 0xa4
        OP_WITHIN              -> putByte 0xa5

        -- Crypto
        OP_RIPEMD160           -> putByte 0xa6
        OP_SHA1                -> putByte 0xa7
        OP_SHA256              -> putByte 0xa8
        OP_HASH160             -> putByte 0xa9
        OP_HASH256             -> putByte 0xaa
        OP_CODESEPARATOR       -> putByte 0xab
        OP_CHECKSIG            -> putByte 0xac
        OP_CHECKSIGVERIFY      -> putByte 0xad
        OP_CHECKMULTISIG       -> putByte 0xae
        OP_CHECKMULTISIGVERIFY -> putByte 0xaf

        -- New operations
        OP_NOP1                -> putByte 0xb0
        OP_CHECKLOCKTIMEVERIFY -> putByte 0xb1
        OP_CHECKSEQUENCEVERIFY -> putByte 0xb2
        OP_NOP4                -> putByte 0xb3
        OP_NOP5                -> putByte 0xb4
        OP_NOP6                -> putByte 0xb5
        OP_NOP7                -> putByte 0xb6
        OP_NOP8                -> putByte 0xb7
        OP_NOP9                -> putByte 0xb8
        OP_NOP10               -> putByte 0xb9
        OP_CHECKDATASIG        -> putByte 0xba
        OP_CHECKDATASIGVERIFY  -> putByte 0xbb

        -- Other
        OP_PUBKEY              -> putByte 0xfe
        OP_PUBKEYHASH          -> putByte 0xfd
        (OP_INVALIDOPCODE x)   -> putByte x
        _                      -> error "Contract must be fully instantiated."

type Script = [OP_CODE]

instance {-# Overlaps #-} Persist Script where
    put = mapM_ put

toByteString :: Script -> ByteString
toByteString = encode

compileIR :: IR -> Script
compileIR = postprocess . concatMap compileOp

compileOp :: OpCode -> Script
compileOp (OpPushBool val)                = if val then [OP_TRUE] else [OP_FALSE]
compileOp (OpPushNum val) | val == -1     = [OP_1NEGATE]
                          | val == 0      = [OP_FALSE]
                          | val == 1      = [OP_TRUE]
                          | val <= 16     = [OP_N val]
                          | otherwise     = let payload = serialize val
                                            in [OP_PUSHDATA0 (head . serialize . length $ payload) payload]
compileOp (OpPushBin val) | length val <= 0x4b   = [OP_PUSHDATA0 (head . serialize . length $ val) val]
                          | length val <= 0xff   = [OP_PUSHDATA1 (serialize . length $ val) val]
                          | length val <= 0xffff = [OP_PUSHDATA2 (serialize . length $ val) val]
                          | otherwise            = [OP_PUSHDATA4 (serialize . length $ val) val]
compileOp (OpPush name) = [OP_PUSH name]
compileOp (OpPick i)  = compileOp (OpPushNum i) ++ [OP_PICK]
compileOp (OpRoll i)  = compileOp (OpPushNum i) ++ [OP_ROLL]
compileOp (OpCall op) = case op of
                          -- Operators
                          "Not"           -> [OP_NOT]
                          "Minus"         -> [OP_NEGATE]
                          "Add"           -> [OP_ADD]
                          "Sub"           -> [OP_SUB]
                          "Mul"           -> [OP_MUL]
                          "Div"           -> [OP_DIV]
                          "Mod"           -> [OP_MOD]
                          "And"           -> [OP_AND]
                          "Or"            -> [OP_OR]
                          "Xor"           -> [OP_XOR]
                          "BoolAnd"       -> [OP_BOOLAND]
                          "BoolOr"        -> [OP_BOOLOR]
                          "Eq"            -> [OP_EQUAL]
                          "Neq"           -> [OP_EQUAL, OP_NOT]
                          "NumEq"         -> [OP_NUMEQUAL]
                          "NumNeq"        -> [OP_NUMNOTEQUAL]
                          "Lt"            -> [OP_LESSTHAN]
                          "Lte"           -> [OP_LESSTHANOREQUAL]
                          "Gt"            -> [OP_GREATERTHAN]
                          "Gte"           -> [OP_GREATERTHANOREQUAL]
                          "Cat"           -> [OP_CAT]
                          "Split"         -> [OP_SPLIT]
                          -- Functions
                          "abs"           -> [OP_ABS]
                          "min"           -> [OP_MIN]
                          "max"           -> [OP_MAX]
                          "within"        -> [OP_WITHIN]
                          "size"          -> [OP_SIZE, OP_NIP]
                          "num2bin"       -> [OP_NUM2BIN]
                          "bin2num"       -> [OP_BIN2NUM]
                          "ripemd160"     -> [OP_RIPEMD160]
                          "sha1"          -> [OP_SHA1]
                          "sha256"        -> [OP_SHA256]
                          "hash160"       -> [OP_HASH160]
                          "hash256"       -> [OP_HASH256]
                          "checkSig"      -> [OP_CHECKSIG]
                          "checkMultiSig" -> [OP_CHECKMULTISIG]
                          "checkLockTime" -> [OP_CHECKLOCKTIME, OP_DROP, OP_TRUE]
                          "checkSequence" -> [OP_CHECKSEQUENCE, OP_DROP, OP_TRUE]
                          "checkDataSig"  -> [OP_CHECKDATASIG]
                          _               -> fail "Unknown function"
compileOp OpVerify = [OP_VERIFY]
compileOp OpIf     = [OP_IF]
compileOp OpElse   = [OP_ELSE]
compileOp OpEndIf  = [OP_ENDIF]
compileOp OpDrop   = [OP_DROP]
compileOp OpNip    = [OP_NIP]


-- Remove pseudo-opcode
postprocess :: Script -> Script
postprocess [] = []
postprocess (OP_CHECKLOCKTIME:OP_DROP:OP_TRUE:OP_VERIFY:ops)
               = OP_CHECKLOCKTIMEVERIFY : OP_DROP : postprocess ops
postprocess (OP_CHECKLOCKTIME:OP_DROP:OP_TRUE:ops)
               = OP_CHECKLOCKTIMEVERIFY : OP_DROP : OP_TRUE : postprocess ops
postprocess (OP_CHECKSEQUENCE:OP_DROP:OP_TRUE:OP_VERIFY:ops)
               = OP_CHECKSEQUENCEVERIFY : OP_DROP : postprocess ops
postprocess (OP_CHECKSEQUENCE:OP_DROP:OP_TRUE:ops)
               = OP_CHECKSEQUENCEVERIFY : OP_DROP : OP_TRUE : postprocess ops
postprocess (op:ops)
               = op : postprocess ops
