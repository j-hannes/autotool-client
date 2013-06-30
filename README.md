Autotool Client
===============

A new user interface for the
[autotool](https://autotool.imn.htwk-leipzig.de/cgi-bin/Super.cgi).

See [http://dfa.imn.htwk-leipzig.de/auto/](http://dfa.imn.htwk-leipzig.de/auto/)
for more information.


Dependencies
------------

You need
[autotool-xmlrpc-client](https://github.com/J-Hannes/autotool-xmlrpc-client) to
run this client.

In order to run the mongoDB adapter you will need to install mongoDB on your
host machine too.


Installation
------------

    $ cabal install


Run
---

    $ autotool-client

(You may need to add you ~/.cabal/bin directory to your $PATH or run from
there...)
