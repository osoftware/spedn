Welcome to Spedn's documentation!
=================================

.. image:: ../images/spedn-logo-cashwave-144.png

Spedn is a high level smart contracts language for electronic cash.
It compiles to Script dialects in eCash, Bitcoin Cash, Lotus and Bitcoin.
It is designed for explicitness and safety:

* It is statically typed - detects many errors at compile time
* It is explicitly typed - no guessing what the expression is supposed to return
* It is purely-functional - free of side effects, the common source of bugs
* It has a familiar syntax resembling C# or Rust

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

   <script src="https://unpkg.com/@paybutton/paybutton/dist/paybutton.js"></script>
   <div style="display:flex">
      <div
      class="paybutton-widget"
      to="bitcoincash:qrc2jhalczuka8q3dvk0g8mnkqx79wxp9gvvqvg7qt"
      currency="BCH"
      theme='{"palette":{"primary":"#8e4d8a","secondary":"#FFFFFF","tertiary":"#231f20"}}'
      goal-amount=0.005
      ></div>
      <div
      class="paybutton-widget"
      to="ecash:qp8x5wakcts248drk5apn4k09a53qclcevjayr77xh"
      theme='{"palette":{"primary":"#8e4d8a","secondary":"#FFFFFF","tertiary":"#231f20"}}'
      goal-amount=0.005
      ></div>
   </div>

Regular donations:

.. raw :: html

   <object type="image/svg+xml" data="https://rcimg.net/images/sponsors/svg/pein.svg" style="margin: auto;"></object>

Contract
--------

* `Telegram Channel <https://t.me/bch_compilers>`_
* `Issue tracker <https://github.com/osoftware/spedn/issues>`_
