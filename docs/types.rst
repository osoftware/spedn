=====
Types
=====


Basic Types
===========

Basic types reflect types Script operates on.

* **bool** - a boolean value.
  Can be either ``true`` or ``false``.
  ``verify`` and ``if`` statements expect an expression returning this type.

* **bit** - a binary flag.
  Only arrays of this type can be created, see below. Array of bits is expected by ``checkMultiSig`` function.

* **int** - a 32-bit signed integer. Literals of this type can be specified in dec or hex.

    .. code-block:: c

        int a = -1234;
        int b = 0xff00i; // notice `i` suffix

* **byte** - a single byte.
  Literals of this type are specified in hex (with ``0x`` prefix) or as UTF-8 strings (in doubel quotes).

    .. code-block:: c

        byte x = 0x11;
        byte y = "a";

Arrays
======

Overall syntax of an array is:

    **[** *element_type* **;** *size* **]**

In case of ``bit`` and ``byte`` the array will mean a single byte vector on the stack in Bitcoin Virtual Machine terms.
In case of other types the array will mean a number of stack elements with element 0 on the bottom.
You can creaye bit array literals with ``0b`` prefix, byte arrays with ``0x`` prefix or double quotes
and any other arrays with comma-separated lists in brackets.

    .. code-block:: c

        [bit;4] checkbits = 0b1011;
        [byte;3] a = 0x11ab33;
        [byte; 3] b = "abc";
        [int;5] c = [1, 2, 3, 4, 5];
        [[byte;2];3] d = ["ab", "cd", "ef"];

It is also possible to define a byte list if the array size is unknown at the compile time.

    .. code-block:: c

        [byte] str = expr;

A byte array can be implicitly casted to a byte list but not the other way.
It is recommended though to use explicit sizes as much as possible to leverage the static type checking of the size.

    .. code-block:: quess

        ([byte] left, [byte] right) = expr @ 3;   // okay
        ([byte;3] left, [byte] right) = expr @ 3; // better
        ([byte;2] left, [byte] right) = expr @ 3; // type error


With an exception of bit arrays the array elements can be accessed by an index starting from 0.

    .. code-block:: c

        [byte] arr = "abcd"
        byte c = arr[2];
        byte x = arr[(i + 1) % 4];


Domain-Spcecific Types
======================

To increase safety, Spedn introduces meaningful types that help with catching semantic errors at compile time.

Numeric types
-------------

These types add meaning to a raw ``int``.
They must be explicitly casted from ``int`` with a type constructor.
They cannot be casted back to ``int``.

* **Time** - represents an absolute time.
    Can be expressed as a Unix Timestamp or a Block Height and variously defined.

    .. code-block:: guess

        Time x = `2018-10-13 21:37:00`; // defined with a time literal
        Time y = TimeStamp(1539466620); // conversion from `int` interpreted as Unix Timestamp
        Time z = TimeStamp(584834);     // conversion from `int` interpreted as Block Height

* **TimeSpan** - represents a relative time period. Can be expressed as a number of blocks or 512-seconds periods.

    .. code-block:: c

        TimeSpan x = 1d 2h 3m 4s; // Time units literal. Be awre that the number will be rounded down to full 512s periods
        TimeSpan y = 10b;         // Blocks literal.
        TimeSpan z = Blocks(10);  // Conversion from `int`


Binary types
------------

These types add meaning to a raw byte arrays.
They can be implicitly casted to ``[byte]``.
They must be explicitly casted from ``[byte]`` with a type constructor.

* **PubKey** - represents a public key.

    .. code-block:: c

        PubKey alice = PubKey(0x11223344556677889900aabbccddeeff);

* **Sig** - represents a tx signature (which can be checked with ``checkSig``).

    .. code-block:: c

        Sig alice = Sig(0x11223344556677889900aabbccddeeff);
        verify checkSig(alice, alicePubKey);

* **DataSig** - represents a data signature (which can be checked with ``checkDataSig``).

    .. code-block:: c

        DataSig alice = DataSig(0x11223344556677889900aabbccddee);
        verify checkDataSig(alice, preimageHash, alicePubKey);

* **Ripemd160** - represents a result of RIPEMD-160 hash.

    .. code-block:: c

        Ripemd160 h = hash160(pubKey);

* **Sha1** - represents a result of SHA-1 hash.

    .. code-block:: c

        Sha1 x = sha1(secret);

* **Sha256** - represents a result of SHA-256 hash.

    .. code-block:: c

        Sha256 x = hash256(secret);


Special types
-------------

These are types that can appear in expressions but you cannot define variables of them.

* **Verification** - almost like ``bool`` but the only thing you can do with it is to pass it to ``verify``.
    This is a return type of ``checkLockTime`` and ``checkSequence`` functions.:

    .. code-block:: c

        verify checkSequence(8b);

* **Tuple** - a series of values that can be of different types.
    Returned by `@` expressions, can be deconstructed or passed to ``fst`` or ``snd`` function.

    .. code-block:: guess

        (byte left; [byte] right) = str @ 1;
        [byte] right = snd(str @ n);


Custom types
============

You can defile a type alias. The name of the new type must start with a capital letter.
The types have to be defined before contracts in the code file. Syntax is:

    **type** *Name* **=** *other type* **;**

Once defined you can declare variables of the new type and use a type constructor for casting a raw type to an alias.
It behaves the same way as constructors of of domain-specific types described in the previous sections.
Actually, all of these are defined as type aliases internally.

    .. code-block:: c

        type Message = [byte;42];
        ...
        Message msg = Message(str);
