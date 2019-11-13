===============
Syntax overview
===============

Module
======

A single code file is called a *module*. It can contain any number of type definitions and contract templates.
Typed should be defined first, followed by contract templates.
Single and multiline comments are supported with ``//`` and ``/* */`` delimiters respectively.


Contract Templates
==================

A contract template in Spedn represents a template for generating a P2SH address and corresponding redeem script.
It can be parametrized. Contract parameters have to be specified to instantiate it, that is - to generate a particular contract with an address.
Contract name should start with a capital letter.

You can perceive a contract template as a specification of a pin tumbler lock mechanism while a contract is a particular lock and parameters are pin lengths in it.

Syntax:

    **contract** *ContractName* **(** [*type* *paramName* [**,** ...]] **) { }**

Example:

.. code-block:: c

    contract SomeContract(Ripemd160 pubKeyHash, int x) {
        // challenges
    }

Challenges
==========

A challenge is a set of conditions that have to be met to redeem a coin locked in a contract.
Challenges specify arguments that will be expected to be pushed in ``scriptSig`` when redeeming the coin.
A contract must contain at least one challenge and a challenge must define at least one argument.
Challenges must have unique names.

A challenge introduces a lexical scope so two different challenges can define an argument with the same name.

When redeeming a coin, a redeemer must choose one of the challenges and satisfy its conditions.
On the assemby level its done by pushing challenge's number (indexed from 1) after arguments.
If there is only one challenge, only arguments are pushed.

You can perceive a challenge as a keyhole in a lock and arguments as keys.

Syntax:

    **challenge** *name* **(** *type* *argName* [**,** ...] **) *statement* **

Example:

.. code-block:: c

    challenge someChallenge(PubKey pubKey, Sig signature) {
        // statements...
    }

Statements
==========

A challenge can contain any number of statements.
To be precise - it contains a single statement but this can be a block statement which can contain any number of statements.

There are the following kinds of statements:

Verification
------------

The most important statement and often the only one needed. It evaluates an expression and fails the script if the result is false.

Syntax:

    **verify** *expr* **;**

Example:

.. code-block:: c

    verify hash1 == hash2;

Variable binding
----------------

You can define a local variable that will be accessible down in the same lexical scope and nested scopes.

Syntax:

    *type* *name* **=** *expr* **;**

Example:

.. code-block:: c

    int a = b + c;

There is also a possibility to deconstruct a tuple into many variables (like in case of using the split operator).
If some of the results is unnecessary, you can ignore them with a low dash operator.

Syntax:

    **(** *type1* *name1* **,** *type2* *name2* **) =** *expr1* **@** *expr2* **;**

    **(_,** *type2* *name2* **) =** *expr1* **@** *expr2* **;**

    **(** *type* *name* **, _) =** *expr1* **@** *expr2* **;**


Example:

.. code-block:: guess

    ([byte;4] prefix, _) = secret @ 4;

Conditional
-----------

You can conditionaly execute a branch of code. A branch introduces a new lexical scope and it can be a verification, block or another conditional.

Syntax:

    **if (** *condition* **)** statement [ **else** *statement* ]

Example:

.. code-block:: c

    if (num % 2 == 1)
        verify checkSig(sig, alice);
    else
        verify checkSig(sig, bob);

Fail
----

To immediately fail the execution just type ``fail;`` - it will compile to ``OP_RETURN``.

.. code-block:: c

    if (num % 2 = 1)
        verify checkSig(sig, alice);
    else
        fail;

Separator
---------

The ``separator;`` statement compiles to ``OP_CODESEPARATOR``.
It affects the way the tx preimage used in ``checkSig`` is generated so that only the code *after* the separator
is included. Might by useful for reducing the size of a preimage used in covenant-style contracts.

Block
-----

A block is a statement that groups several statements for sequential execution.
A block introduces a lexical scope.
The last statement must be a verification or conditional.

Syntax:

    **{** [ *statements...* ] **}**

Example:

.. code-block:: c

    if (num % 2 = 1) {
        verify checkSig(sig, alice);
    } else {
        verify checkSig(sig, bob);
        verify checkSequence(5d);
    }


Loop
----

There are no loops, it's Bitcoin.


Type Definitions
================

You can defile a type alias. The name of the new type must start with a capital letter.

Syntax:

    **type** *Name* **=** *other type* **;**

Example:

.. code-block:: c

    type Message = [byte;7];

Once defined you can declare variables of the new type and use a type constructor for casting a raw type to an alias.

    .. code-block:: c

        Message msg = Message("abcdefg");

Lexical scopes
==============

Spedn creates common, nested lexical scopes for parameters, arguments, variables and functions.
There can be no 2 identical names within the same scope.
Also - name shadowing is prohibited so a nested scope cannot redefine a name present in its parent scope.

There are following scopes in the nesting order:

* **Module scope** - contains predefined functions and type definitions
* **Contract scope** - introduced by the contract, contains contract parameters
* **Challenge scope** - introduced by the challenge, contains challenge arguments and local variables
* **Local scope** - introduced by *if/else/block* statements, contains local variables

Exhaustive example:

.. code-block:: c

    // a global scope, names like checkSig, min, max are reserved.

    type Msg = [byte;15];

    // contract scope begins
    contract X(int a, int b) { // names a, b are defined

        // challenge scope begins
        challenge a( // it's OK for the challenge to be named a because challenge names don't occupy the name table.
            int c // name c is defined
            /* int a     // BAD - already defined in contract scope */)
        {
            verify a >= b;
            /* verify a == d // BAD - d is not yet defined */
            int d = a + b;   // name d is defined
            if (d > 0)
            // if scope begins
            {
                int e = d % c;
                verify e == 0;
            }
            // if scope ends; e is gone.
            else
            // else scope begins
                verify a == b;
            // else scope ends
            /* verify e == 1 // BAD - e is gone */
        }
        // challenge scope ends; c, d are gone

        // challenge scope begins
        challenge b(int c, int d) // names c, d are defined
        {
            verify c == d;
        }
        // challenge scope ends; c, d are gone

    }
    // contract scope ends; a, b are gone
