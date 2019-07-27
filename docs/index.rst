Welcome to Spedn's documentation!
=================================

.. image:: ../images/spedn-logo-cashwave-144.png

Spedn is a high level smart contracts language for Bitcoin Cash.
It is designed for explicitness and safety:

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
   :maxdepth: 2
   :caption: Integrations:

   bitbox

.. toctree::
   :maxdepth: 1
   :caption: Examples

   0conf
   chainbet

Roadmap
-------

Spedn is an early, experimental tool with a lot of plans:

* Macros
* Extended support for covenants and tx preimage introspection
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
* `Tips <bitcoincash:qrc2jhalczuka8q3dvk0g8mnkqx79wxp9gvvqvg7qt>`_

Contract
--------

* `Issue tracker <https://bitbucket.org/o-studio/spedn/issues?status=new&status=open>`_
* `#spedn-lang channel on Electron Cash Slack <https://electroncash.slack.com/messages/CD81XT49X>`_
* `Twitter <http://twitter.com/tendo_pein_sama>`_
