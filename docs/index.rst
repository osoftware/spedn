Welcome to Spedn's documentation!
=================================

.. image:: ../images/spedn-logo-cashwave-144.png

Spedn is a high level smart contracts language for Bitcoin Cash.
It is designed for explicitness and safety:

* It is statically typed - detects many errors at compile time
* It is explicitly typed - no guessing what the expression is supposed to return
* It is purely-functional - free of side effects, the common source of bugs
* It has a familiar syntax resembling C# or Rust

.. Warning:: Spedn is an experimental tool. It is not recommended to be used on mainnet yet.
   Or at least do a lot of tests on testnet first.

.. toctree::
   :maxdepth: 1
   :caption: Getting Started:

   quickstart
   script
   migration

.. toctree::
   :maxdepth: 1
   :caption: Language Specification:

   syntax
   types
   operators
   functions
   cli

.. toctree::
   :maxdepth: 2
   :caption: Integrations:

   bchjs
   bitbox

.. toctree::
   :maxdepth: 1
   :caption: Examples

   0conf
   chainbet

Contributing
------------

Every kind of contribution is appreciated, especially:

* Syntax ideas and other features suggestions
* Code review
* Unit tests
* Bug reports
* Usage examples and docs improvement


Sponsorship
-----------

Single donation:

.. raw :: html

   <script src="https://paybutton.cash/pre-release/v0.1/js/paybutton.min.js"></script>
   <button
      class="pay-button"
      amount="0.005"
      amount-type="BCH"
      address="bitcoincash:qrc2jhalczuka8q3dvk0g8mnkqx79wxp9gvvqvg7qt"
      button-text="Send Bitcoin Cash">
   </button>

Regular donations:

.. raw :: html

   <object type="image/svg+xml" data="https://rcimg.net/images/sponsors/svg/pein.svg" style="margin: auto;"></object>

Contract
--------

* `Telegram Channel <https://t.me/bch_compilers>`_
* `Issue tracker <https://bitbucket.org/o-studio/spedn/issues?status=new&status=open>`_
* `Twitter <http://twitter.com/tendo_pein_sama>`_
