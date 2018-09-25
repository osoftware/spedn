{-# LANGUAGE DeriveDataTypeable #-}

module Script where

import           Data.Data
import           Data.Word

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
    deriving (Eq, Data)

instance Show OP_CODE where
  show (OP_PUSHDATA0 _ payload) = show payload
  show (OP_PUSHDATA1 _ payload) = show payload
  show (OP_PUSHDATA2 _ payload) = show payload
  show (OP_PUSHDATA4 _ payload) = show payload
  show (OP_PUSH name)           = "<" ++ name ++ ">"
  show (OP_N n)                 = show n
  show op                       = drop 3 $ show $ toConstr op

type Script = [OP_CODE]

compileIR :: IR -> Script
compileIR = concatMap compileOp

compileOp :: OpCode -> Script
compileOp (OpPushBool val)                = if val then [OP_TRUE] else [OP_FALSE]
compileOp (OpPushNum val) | val == -1     = [OP_1NEGATE]
                          | val == 0      = [OP_FALSE]
                          | val == 1      = [OP_TRUE]
                          | val <= 16     = [OP_N val]
                          | otherwise     = [OP_PUSHDATA0 (head . serialize $ 4) (serialize val)]
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
                          "num2bin"       -> [OP_NUM2BIN]
                          "bin2num"       -> [OP_BIN2NUM]
                          "ripemd160"     -> [OP_RIPEMD160]
                          "sha1"          -> [OP_SHA1]
                          "sha256"        -> [OP_SHA256]
                          "hash160"       -> [OP_HASH160]
                          "hash256"       -> [OP_HASH256]
                          "checkSig"      -> [OP_CHECKSIG]
                          "checkMultiSig" -> [OP_CHECKMULTISIG]
                          "checkLockTime" -> [OP_CHECKLOCKTIME]
                          "checkSequence" -> [OP_CHECKSEQUENCE]
                          "checkDataSig"  -> [OP_CHECKDATASIG]
                          _               -> fail "Unknown function"
compileOp OpVerify = [OP_VERIFY]
compileOp OpIf     = [OP_IF]
compileOp OpElse   = [OP_ELSE]
compileOp OpEndIf  = [OP_ENDIF]
compileOp OpDrop   = [OP_DROP]
