module Vm.Btc where

import qualified Data.Map.Lazy        as Map

import Env
import Vm
import Syntax

globals :: SymbolTable
globals = Map.fromList
    [ (Operator "Minus",   [Num]       :-> Num)
    , (Operator "Add",     [Num, Num]  :-> Num)
    , (Operator "Sub",     [Num, Num]  :-> Num)
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
    , (Fun "checkLockTime",  [Alias "Time"]                               :-> Verification)
    , (Fun "checkSequence",  [Alias "TimeSpan"]                           :-> Verification)
    , (Fun "checkSigAdd",    [Alias "Sig", Num, Alias "Pubkey"]           :-> Num)
    , (Fun "checkSize",      [Array Byte $ SizeParam "s"]                 :-> Bool)

      -- Array manipulation
    , (Fun "size",           [List Byte] :-> Num)

      -- Type aliases
    , (Type "PubKey",    Array Byte $ ConstSize 33)
    , (Type "Ripemd160", Array Byte $ ConstSize 20)
    , (Type "Sha1",      Array Byte $ ConstSize 16)
    , (Type "Sha256",    Array Byte $ ConstSize 32)
    , (Type "Sig",       Array Byte $ ConstSize 65)
    , (Type "TimeSpan",  Num)
    , (Type "Time",      Num)
    
      -- Type constructors
    , (Fun "PubKey",         [List Byte] :-> Alias "PubKey")
    , (Fun "Ripemd160",      [List Byte] :-> Alias "Ripemd160")
    , (Fun "Sha1",           [List Byte] :-> Alias "Sha1")
    , (Fun "Sha256",         [List Byte] :-> Alias "Sha256")
    , (Fun "Sig",            [List Byte] :-> Alias "Sig")
    , (Fun "Blocks",         [Num]       :-> Alias "TimeSpan")
    , (Fun "TimeStamp",      [Num]       :-> Alias "Time")
    , (Fun "Bytes",          [Num]       :-> List Byte)

      -- Macros
    , (Fun "fst",            [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "a")
    , (Fun "snd",            [Tuple [TypeParam "a", TypeParam "b"]] :-> TypeParam "b")
    ]


btc :: Vm
btc = Vm (-2147483647, 2147483647) [globals]