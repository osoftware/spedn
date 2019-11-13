=================
Quick start guide
=================

Build from sources
==================

1. Intsall `Haskell Tool Stack <https://docs.haskellstack.org/en/stable/README/#how-to-install>`_.

2. Download Spedn `sources <https://bitbucket.org/o-studio/spedn/src>`_.

    .. code-block:: bash

        $ git clone https://bitbucket.org/o-studio/spedn.git

3. Build and install Spedn.

    .. code-block:: bash

        $ cd spedn/spedn
        $ stack install


Installation from npm
=====================

Alternatively, you can install a JavaScript version from npmjs repository:

    .. code-block:: bash

        $ npm i -g spedn-cli


Your first contract
===================

Create a file mycontract.spedn with a following content:

.. code-block:: c

    contract ExpiringTip(Ripemd160 alice, Ripemd160 bob) {

        challenge receive(Sig sig, PubKey pubKey) {
            verify hash160(pubKey) == bob;
            verify checkSig(sig, pubKey);
        }

        challenge revoke(Sig sig, PubKey pubKey) {
            verify checkSequence(7d);
            verify hash160(pubKey) == alice;
            verify checkSig(sig, pubKey);
        }
    }

Compile with command:

.. code-block:: bash

    $ spedn compile -c mycontract.spedn

You should get a compiled contract template similar to this:

.. code-block:: forth

    <alice> <bob> 2 PICK TRUE EQUAL IF 3 PICK HASH160 OVER EQUALVERIFY (...)
