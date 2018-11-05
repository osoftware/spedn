Welcome to Spedn's documentation!
=================================

.. image:: ../images/spedn-logo-cashwave-144.png

Spedn is a high level smart contracts language for Bitcoin Cash.
It is designed for explicity and safety:

* It is statically typed - detects many errors at compile time
* It is explicitly typed - no guessing what the expression is supposed to return
* It is purely-functional - free of side effects, the common source of bugs
* It has a familiar C-like syntax

.. Warning:: Spedn is an experimental tool. It is not recommended to be used on mainnet yet.

.. toctree::
   :maxdepth: 1
   :caption: Getting Started:

   quickstart
   script

.. toctree::
   :maxdepth: 2
   :caption: Language Specification:

   syntax
   types
   operators
   functions
   cli

.. toctree::
   :maxdepth: 1
   :caption: Examples

   0conf
   chainbet

Roadmap
-------

Spedn is an early, experimental tool with a lot of plans:

* Making addresses and unsigned transaction templates
* JavaScript interoperability and libraries integration
* Compiled code optimizations
* IDE with a debugger
* ...and more

Check out the `Trello board <https://trello.com/b/u6vD1EWO/spedn>`_ 
to see what's currently going on.

Contributing
------------

Every kind of contribution is appreciated, especially:

* Syntax ideas and other features propositions
* Code review
* Unit tests
* Bug reports
* Usage examples and docs improvement
* `Tips <bitcoincash:qzu5e85zx770d82rv3u6fevr5s5ytl5kl5kzfp9tmc>`_

    .. raw:: html

        <div class="money-button"
             data-to="bitcoincash:qzu5e85zx770d82rv3u6fevr5s5ytl5kl5kzfp9tmc"
             data-amount="0.001"
             data-currency="BCH"
             data-label="Donate"
             data-client-identifier="b61564a61cfff8f88f51b757fd9c6deb"
             data-button-id="1540404534895"
             data-button-data="{}"
             data-type="tip">
        </div>
        <script src="https://www.moneybutton.com/moneybutton.js"></script>


Contract
--------

* `Issue tracker <https://bitbucket.org/o-studio/spedn/issues?status=new&status=open>`_
* `#spedn-lang channel on Electron Cash Slack <https://electroncash.slack.com/messages/CD81XT49X>`_
* `Twitter <http://twitter.com/tendo_pein_sama>`_
