Spedn Runtime
=================================

![logo](https://bytebucket.org/o-studio/spedn/raw/6c4c092ed4615a51b5e8a37cd68b2175e0ed826d/images/spedn-logo-cashwave-144.png "Spedn")

Spedn is a high level smart contracts language for Bitcoin Cash.
It is designed for explicitess and safety:

* It is statically typed - detects many errors at compile time
* It is explicitly typed - no guessing what the expression is supposed to return
* It is purely-functional - free of side effects, the common source of bugs
* It has a familiar C-like syntax

This package contains a lightweight, SDK-agnostic runtime for Bitcoin Cash applications using compiled Spedn contracts.
To integrate your contract into an app built on top of one of the SDKs, use the apropriate adapter:

* [@spedn/rts-bchjs](https://npmjs.com/packages/@spedn/bchjs-rts) - for apps on top of [BCH-JS](https://www.npmjs.com/package/@psf/bch-js)
* [@spedn/rts-bitbox](https://npmjs.com/packages/@spedn/bchjs-rts) - for apps on top of [BITBOX](https://www.npmjs.com/package/bitbox-sdk)

Install this package directly only if you want to develop your own adapter.

If you want to dynamically compile Spedn contracts in your app, also install [@spedn/sdk](https://www.npmjs.com/package/@spedn/sdk).

If you want to use Spedn compiler as a command-line tool, install [@spedn/cli](https://www.npmjs.com/package/@spedn/cli).

[Spedn Home Page ](https://spedn.pl)

[Full Docs](http://spedn.rtfd.io)

Donations: 🎺 Pein#7306 || bitcoincash:qrc2jhalczuka8q3dvk0g8mnkqx79wxp9gvvqvg7qt
[![Sponsors](https://rcimg.net/images/sponsors/svg/pein.svg)](https://read.cash/become-a-sponsor/pein)
