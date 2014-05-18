This package contains code for an example Web application for a [class at
BayHac2014](http://alephcloud.github.io/bayhac2014/slides/index.html). The
application allows to create encrypted emails from within a browser.

**This is not production level code.**

The purpose of this package to demonstrate techniques for development of
portable Haskell libraries that can be used with native compilations as well as
cross-compilations to client-side javascript via the [Haste
compiler](https://github.com/valderman/haste-compiler).

Slides
======

[http://alephcloud.github.io/bayhac2014/slides/index.html](http://alephcloud.github.io/bayhac2014/slides/index.html)

Overview
========

The package includes a library component that can be compiled with native
GHC as well as with the Haste compiler. The library exposes two
interfaces:

1.  a Haskell API for encryption and decryption with a password and
2.  a JSON service API that wraps the Haskell API in JSON data types.

The package further contains an application that serves the JSON
API over HTTP using [scotty](http://hackage.haskell.org/package/scotty).

The package also contains an application that exports the JSON API as client
side javascript module.

Finally, there is a cabal test suite for unit testing the library with a native
compilation.

Setup of Haste with a Sandbox
=============================

The following instructions installs the haste-compiler in a cabal sandbox.
Note, however, that the resulting haste-compiler itself is **not** using a
sandbox for compilations.

Build Haste:

~~~{.bash}
cabal sandbox init
echo 'constraints: scientific<0.3' >> cabal.config
cabal install haste-compiler -j
export PATH=./.cabal-sandbox/bin:$PATH
~~~

Setup Haste:

~~~{.bash}
echo 'solver: topdown' >> ~/.cabal/config
haste-boot
sed -i '/solver: topdown/d' ~/.cabal/config
~~~

If `haste-boot` failes with an error message that inlcude the text `only already
installed instances can be used` you may try to temporarily add the
following to your `~/.cabal/config` while running `haste-boot`.

Installation
============

The native version of the library and the server application can be
installed via

~~~{.bash}
cabal install
~~~

The test suite can be run via

~~~{.bash}
cabal configure --enable-tests
cabal build
cabal test
~~~

The javascript module is build via

~~~{.bash}
mv cabal.sandbox.config cabal.sandbox.config.disabled
haste-inst install -fhaste
haste-inst configure -fhaste
haste-inst build
~~~

The resulting javascript code is in the file `src/Client.js`. It can be tested
by opening the file `Main.html` in a browser from the root directory of the
package.

Legal Notes
===========

This package includes a copy of a version of [SJCL](https://github.com/bitwiseshiftleft/sjcl)
in the file `./lib/sjcl.js`. SJCL is hosted at [https://github.com/bitwiseshiftleft/sjcl]() and
distributed under a BSD license as described at [http://bitwiseshiftleft.github.io/sjcl]().

The file `./src/BayHac2014/Cryptmail/Json.hs` and the files in the directory
`./src/BayHac2014/Cryptmail/Json` of this package in part contain code that was
copied, derived, or inspired by the [Aeson
package](http://hackage.haskell.org/package/aeson). The Aeson package is
distributed under a BSD3 License and copyright of *(c) 2011-2014 Bryan
O'Sullivan (c) 2011 MailRank, Inc.*. A copy of the LICENSE of the Aeson package
can be found
[here](http://hackage.haskell.org/package/aeson-0.7.0.4/src/LICENSE).

