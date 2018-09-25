module Optimizer where

import           Script

optimize :: Script -> Script
optimize [] = []
optimize (OP_EQUAL:OP_VERIFY:ops)         = OP_EQUALVERIFY : optimize ops
optimize (OP_CHECKSIG:OP_VERIFY:ops)      = OP_CHECKSIGVERIFY : optimize ops
optimize (OP_CHECKMULTISIG:OP_VERIFY:ops) = OP_CHECKMULTISIGVERIFY : optimize ops
optimize (OP_CHECKDATASIG:OP_VERIFY:ops)  = OP_CHECKDATASIGVERIFY : optimize ops
optimize (OP_CHECKLOCKTIME:OP_VERIFY:ops) = OP_CHECKLOCKTIMEVERIFY : optimize ops
optimize (OP_CHECKLOCKTIME:ops)           = OP_CHECKLOCKTIMEVERIFY : OP_TRUE : optimize ops -- | Remove pseudo-opcode
optimize (OP_CHECKSEQUENCE:OP_VERIFY:ops) = OP_CHECKSEQUENCEVERIFY : optimize ops
optimize (OP_CHECKSEQUENCE:ops)           = OP_CHECKSEQUENCEVERIFY : OP_TRUE : optimize ops -- | Remove pseudo-opcode
optimize (OP_NUMEQUAL:OP_VERIFY:ops)      = OP_NUMEQUALVERIFY : optimize ops
optimize (OP_FALSE:OP_EQUAL:OP_NOT:ops)   = OP_0NOTEQUAL : optimize ops
optimize (OP_FALSE:OP_PICK:ops)           = OP_DUP : optimize ops
optimize (OP_DROP:OP_DROP:ops)            = OP_2DROP : optimize ops
optimize (op:ops)                         = op : optimize ops
