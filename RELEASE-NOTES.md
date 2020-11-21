# Release 0.3.1.1

* fix `Rts.utxo(addr)` implementaion in BCH-JS backen that was using old BITBOX REST endpoint.

# Release 0.3.1.0

This is a minor feature release focusing on SDK improvements. See migration guide in docs for details.

* Decoupling compiler from runtime. While the compiler stays in SDK package, the components for working with compiled contracts moved to RTS.
* Decoupling RTS from BITBOX, which is now just one of available backends for RTS.
* Introducing BCH-JS support as another (and recommended) backend for RTS.
* Introducing Portable format - compilation output that can be loaded by RTS without the need for compiler.
* All JS packages are moved to `@spedn` scope on npmjs: `@spedn/cli`, `@spedn/rts`, `@spedn/rts-bitbox`, `@spedn/rts-bchjs`, `@spedn/sdk`.
* For better npm tooling support, JS packages use SemVer, while the project as a whole stays with Haskell PVP. So for `MAJOR.major.minor.patch` release, the JS package will be `MAJORmajor.minor.patch`.

# Release 0.3.0

This is 15th May 2020 hard-fork compatibility update with some new features

* Introducing `reverseBytes` support.
* Introducing tuple literals, for example `(1, "abc")`, and ability to create type aliases for tuple types.
* One such alias is `TxState` which is a 10-tuple containing transaction preimage components.
* You can get a `TxState` by calling `parse` on a variable of `Preimage` type.
* Introducing functions extracting a single component of the preimage.

# Release 0.2.2

* fixed issues with compiling large source code files in JavaScript version [thanks to Tobias Ruck for reporting]
* improved accuracy of type error messages in concatenation (`a . b`) expressions [thanks to emergent_reasons for reporting]

# Release 0.2.1

* fix SDK bug in validation of Buffer passed as a ``[byte]`` argument [thanks to read.cash for noticing]
* allow to pass a string as a ``[byte]`` argument (it will by converted to a Buffer encoded in UTF-8) [thanks to read.cash for inspiration]

# Release 0.2.0

This is 15th Nov 2019 hard-fork compatibility update with a bunch of breaking changes and new features.

* Introducing an array type with syntax `[type; length]`, for example `[Sig; 3] signatures, [byte; 10] message`.
The compiler type-checks the lengths so for example a type of `message . message` expression will be inferred as `[byte; 20]`.
* Syntax for tuple assignment now allows its items to be of different type: `(int a, Sig b, PubKey c) = expr;`
* Array elements can be accessed with `x[i]` syntax.
* Introducing a `bit` type. In practice only arrays of bits are useful, as they represent a type of `checkbits` argument in the `checkMultiSig` function which was upgraded for Schnorr support. Bit array literal is also introduced, ex. `[bit; 5] checkbits = 0b00110`.
* As mentioned - `checkMultiSig` accepts an additional `checkbits` argument, as described in Nov 15 hard-fork spec.
* For a byte array of unknown size there is `[byte]` type which replaces the former `bin` type.
* Introducing (UTF-8) string literals, ex. `[byte] message = "Hello, World";`.
* Introducing custom type declarations (type aliases) which can be placed before contract declarations and then used as any other type in the contract. 
Ex. `type Message = [byte; 10];`. Actually, `Sig`, `DataSig`, `PubKey`, `Ripemd160`, `Sha1`, `Sha256`, `Time` and `TimeSpan` are defined internally as aliases.
* Introducing `separator;` statement that compiles to `OP_CODESEPARATOR`.
* Introducing `fail;` statement that compiles to `OP_RETURN`.
* Introducing `checkSize(x)` function that returns `true` if the runtime size of a byte array matches the declared type.
* Variable names can now contain underscores, ex. `[byte] my_string`.

# Release 0.1.5

* fix bug with final `else` statement generating unnecessary `OP_NIP`.

# Release 0.1.4

* fix bug with `fst` and `snd` functions dupicationg `OP_SPLIT`

# Release 0.1.3

* fix bug with stack tracking after `if` [thanks to Jonathan Silverblood for reporting]
* use Schnorr signatures in `SigningContext.signData()`
