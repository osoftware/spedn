=================
Quick start guide
=================

Installation
============

1. Intsall [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).::
2. Download Spedn [sources](https://bitbucket.org/o-studio/spedn/src).::

    $ git clone https://bitbucket.org/o-studio/spedn.git

3. Build and install Spedn.::

    $ cd spedn
    $ stack install spedn


Your first contract
===================

Create a file mycontract.spedn with a following content:::

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

Compile with command::

    $ spedn compile -c mycontract.spedn

You should get a following, compiled contract template:::

    <alice> <bob> 2 PICK TRUE EQUAL IF 3 PICK HASH160 OVER EQUALVERIFY (...)

Coming soon
===========

Instantiating the template. Address generation. Redeeming.
