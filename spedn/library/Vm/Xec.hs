module Vm.Xec where

import qualified Data.Map.Lazy        as Map

import Env
import Vm
import Syntax

fixedSizeByteOp :: Type
fixedSizeByteOp = [Array Byte $ SizeParam "s", Array Byte $ SizeParam "s"] :-> Array Byte (SizeParam "s") :|: [List Byte, List Byte] :-> List Byte

globals :: SymbolTable
globals = Map.fromList
    [ (Operator "Not",     [Bool]      :-> Bool)
    , (Operator "Minus",   [Num]       :-> Num)
    , (Operator "Add",     [Num, Num]  :-> Num)
    , (Operator "Sub",     [Num, Num]  :-> Num)
    , (Operator "Div",     [Num, Num]  :-> Num)
    , (Operator "Mod",     [Num, Num]  :-> Num)
    , (Operator "And",     fixedSizeByteOp)
    , (Operator "Or",      fixedSizeByteOp)
    , (Operator "Xor",     fixedSizeByteOp)
    , (Operator "BoolAnd", [Bool, Bool] :-> Bool)
    , (Operator "BoolOr",  [Bool, Bool] :-> Bool)
    , (Operator "Eq",      [TypeParam "a", TypeParam "b"] :-> Bool)
    , (Operator "Neq",     [TypeParam "a", TypeParam "b"] :-> Bool)
    , (Operator "NumEq",   [Num, Num] :-> Bool)
    , (Operator "NumNeq",  [Num, Num] :-> Bool)
    , (Operator "Lt",      [Num, Num] :-> Bool)
    , (Operator "Lte",     [Num, Num] :-> Bool)
    , (Operator "Gt",      [Num, Num] :-> Bool)
    , (Operator "Gte",     [Num, Num] :-> Bool)
    , (Operator "Cat",     [List Byte, List Byte] :-> List Byte)  -- TODO: a way to express SizeParams relationships / dependent types
    , (Operator "Split",   [List Byte, Num]       :-> Tuple [List Byte, List Byte])

      -- Simple math
    , (Fun "abs",            [Num]           :-> Num)
    , (Fun "min",            [Num, Num]      :-> Num)
    , (Fun "max",            [Num, Num]      :-> Num)
    , (Fun "within",         [Num, Num, Num] :-> Bool)

      -- Hashing
    , (Fun "ripemd160",      [List Byte] :-> Alias "Ripemd160")
    , (Fun "sha1",           [List Byte] :-> Alias "Sha1")
    , (Fun "sha256",         [List Byte] :-> Alias "Sha256")
    , (Fun "hash160",        [List Byte] :-> Alias "Ripemd160")
    , (Fun "hash256",        [List Byte] :-> Alias "Sha256")

      -- Checking
    , (Fun "checkSig",       [Alias "Sig", Alias "PubKey"]                :-> Bool)
    , (Fun "checkMultiSig",  [Array Bit $ SizeParam "k",
                              Array (Alias "Sig") $ SizeParam "s", 
                              Array (Alias "PubKey") $ SizeParam "k"]     :-> Bool)
    , (Fun "checkDataSig",   [Alias "DataSig", List Byte, Alias "PubKey"] :-> Bool)
    , (Fun "checkLockTime",  [Alias "Time"]                               :-> Verification)
    , (Fun "checkSequence",  [Alias "TimeSpan"]                           :-> Verification)
    , (Fun "checkSize",      [Array Byte $ SizeParam "s"]                 :-> Bool)

      -- Array manipulation
    , (Fun "num2bin",        [Num, Num]  :-> List Byte)
    , (Fun "bin2num",        [List Byte] :-> Num)
    , (Fun "size",           [List Byte] :-> Num)
    , (Fun "reverseBytes",   [Array Byte $ SizeParam "s"] :-> Array Byte (SizeParam "s")
                         :|: [List Byte]                  :-> List Byte)

      -- Type aliases
    , (Type "PubKey",    Array Byte $ ConstSize 33)
    , (Type "Ripemd160", Array Byte $ ConstSize 20)
    , (Type "Sha1",      Array Byte $ ConstSize 16)
    , (Type "Sha256",    Array Byte $ ConstSize 32)
    , (Type "Sig",       Array Byte $ ConstSize 65)
    , (Type "DataSig",   Array Byte $ ConstSize 64)
    , (Type "TimeSpan",  Num)
    , (Type "Time",      Num)
    
    , (Type "Preimage",   List Byte)
    , (Type "NVersion",   Array Byte $ ConstSize 4)
    , (Type "Outpoint",   Array Byte $ ConstSize 36)
    , (Type "ScriptCode", List Byte)
    , (Type "Value",      Array Byte $ ConstSize 8)
    , (Type "NSequence",  Array Byte $ ConstSize 4)
    , (Type "NLocktime",  Array Byte $ ConstSize 4)
    , (Type "Sighash",    Array Byte $ ConstSize 4)
    , (Type "TxState",    Tuple [ Alias "NVersion"
                                , Alias "Sha256"
                                , Alias "Sha256"
                                , Alias "Outpoint"
                                , Alias "ScriptCode"
                                , Alias "Value"
                                , Alias "NSequence"
                                , Alias "Sha256"
                                , Alias "NLocktime"
                                , Alias "Sighash"
                                ])


      -- Type constructors
    , (Fun "PubKey",         [List Byte] :-> Alias "PubKey")
    , (Fun "Ripemd160",      [List Byte] :-> Alias "Ripemd160")
    , (Fun "Sha1",           [List Byte] :-> Alias "Sha1")
    , (Fun "Sha256",         [List Byte] :-> Alias "Sha256")
    , (Fun "Sig",            [List Byte] :-> Alias "Sig")
    , (Fun "DataSig",        [List Byte] :-> Alias "DataSig")
    , (Fun "Blocks",         [Num]       :-> Alias "TimeSpan")
    , (Fun "TimeStamp",      [Num]       :-> Alias "Time")
    , (Fun "Bytes",          [Num]       :-> List Byte)

    , (Fun "Preimage",       [List Byte] :-> Alias "Preimage")
    , (Fun "NVersion",       [List Byte] :-> Alias "NVersion")
    , (Fun "Sha256",         [List Byte] :-> Alias "Sha256")
    , (Fun "Sha256",         [List Byte] :-> Alias "Sha256")
    , (Fun "Outpoint",       [List Byte] :-> Alias "Outpoint")
    , (Fun "ScriptCode",     [List Byte] :-> Alias "ScriptCode")
    , (Fun "Value",          [List Byte] :-> Alias "Value")
    , (Fun "NSequence",      [List Byte] :-> Alias "NSequence")
    , (Fun "Sha256",         [List Byte] :-> Alias "Sha256")
    , (Fun "NLocktime",      [List Byte] :-> Alias "NLocktime")
    , (Fun "Sighash",        [List Byte] :-> Alias "Sighash")
    , (Fun "TxState",        [Alias "NVersion",
                              Alias "Sha256",
                              Alias "Sha256",
                              Alias "Outpoint",
                              Alias "ScriptCode",
                              Alias "Value",
                              Alias "NSequence",
                              Alias "Sha256",
                              Alias "NLocktime",
                              Alias "Sighash"] :-> Alias "TxState")

      -- Macros
    , (Fun "fst",            [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "a")
    , (Fun "snd",            [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "b")
    , (Fun "toDataSig",      [Alias "Sig"]                          :-> Alias "DataSig")
    , (Fun "parse",          [Alias "Preimage"]                     :-> Alias "TxState")
    , (Fun "nVersion",       [Alias "Preimage"]                     :-> Alias "NVersion")
    , (Fun "hashPrevouts",   [Alias "Preimage"]                     :-> Alias "Sha256")
    , (Fun "hashSequence",   [Alias "Preimage"]                     :-> Alias "Sha256")
    , (Fun "outpoint",       [Alias "Preimage"]                     :-> Alias "Outpoint")
    , (Fun "scriptCode",     [Alias "Preimage"]                     :-> Alias "ScriptCode")
    , (Fun "value",          [Alias "Preimage"]                     :-> Alias "Value")
    , (Fun "nSequence",      [Alias "Preimage"]                     :-> Alias "NSequence")
    , (Fun "hashOutputs",    [Alias "Preimage"]                     :-> Alias "Sha256")
    , (Fun "nLocktime",      [Alias "Preimage"]                     :-> Alias "NLocktime")
    , (Fun "sighash",        [Alias "Preimage"]                     :-> Alias "Sighash")
    ]


xec :: Vm
xec = Vm (-2147483647, 2147483647) [globals]