---
title: Haskell in the Browser With Haste
author: Lars Kuhtz <lars@alephcloud.com>
date: 2014-05-17
...

Preparation
===========

Get the code:

~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
cd bayhac2014
~~~

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

Topics
======

<div class="notes">
A lot of the resources that come with Haste are about using Haste to develop monolithic
HTML5 web applications with a client and a server component.

This is different from how we use it. We use Haskell primarily as a programing
language for modeling backend logic and as a systems programing language on the server
side.

We don't (yet) use Haskell as a front end language or for GUI programing. GUIs included
web UIs are done by GUI developers who usually use the a framework specific language for
the respective platform. For the browser this is javascript.

However, for many components that tranditionally would be located on the server side our
product demands that they are executed on the client side. Specifically these components
include cryptographic modules and policy enforcement modules. Often these components are
not platform specific and represent core IP of our company. Therefor it is desireable
to develop and maintain them by the same developers accross all client platforms and
ideally in the same code base. Moreoever, these components are developed as services
that should provide the same API independent if they are used as a network service or
a library, on the client or on the server side.

This considerations lead to a design where, in the browser we host the same (or
similar) components that we also use on native clients and on the server side in the cloud.

*So our motivation for using Haste is development of javascript libraries that offer provide
a well-defined service API in the browser.*

In order to cope with the more standard usage of Haste we will split this class into
two main topcis:

1.  A general introduction to Haste that covers using Haste for simple examples of
    web-development, and

2.  a slightly more in depth part about usage of Haste for *backend* development
    in the browser.

We think that with browser becoming more and more the role of general systems programing
environments the second point may be of interest beyond our setting that, in our case
is a direct consequence of our product.
</div>

1.  Setup and usage of Haste

2.  Development of portable libraries for web applications

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Part 1: Setup and Usage
=======================

Outline 
========

1.  Installation and Setup
2.  Compiling and Deploying an Application
3.  FFI
4.  Marshaling of data and callbacks

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

<div class="notes">
What is not covered:

*   Concurrency ?
*   Reactive programing, browser events
*   Seamless client server app programing
*   DOM
*   Ajax calls, websockets
</div>

Setup Haste
===========

~~~{.bash}
cabal install haste-compiler
haste-boot
~~~

well ...

~~~{.bash}
echo 'constraints: scientific<0.3' >> cabal.config
cabal install haste-compiler
echo 'solver: topdown' >> ~/.cabal/config
haste-boot
sed -i '/solver: topdown/d' ~/.cabal/config
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Hello BayHac with Haste
=======================

`contrib/hello-bayhac.hs`:

~~~{.Haskell}
module Main
( main
) where

import Haste

main :: IO ()
main = alert "hello bayhac"
~~~

Compile: 

~~~{.bash}
hastec hello-bayhac.hs
~~~

`contrib/hello-bayhac.html`:

~~~{.html}
<!DOCTYPE html>
<html>
    <head> <script type="text/javascript" src="./hello-bayhac.js"></script></head>
    <body/>
</html>
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Haste Runtime
=============

Thunks:

~~~{.javascript}
function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}
~~~

Evaluate to head normal form:

~~~{.javascript}
function E(t) { /* ... */ }
~~~

Partial application:

~~~{.javascript}
function A(f, args) { /* ... */ }
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Haste Runtime
=============

*   Arithmetic
*   Strings: `toJSStr`, `fromJSStr`
*   `jsAlert`, `jsPrompt`, `jsLog`, `jsEval`, etc.
*   `jsSetTimeout`
*   DOM manipulation
*   Arrays and Pointers
*   etc.

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Compiled Code
=============

`contrib/hello-bayhac.hs`:

~~~{.haskell}
module Main
( main
) where

import Haste

main :: IO ()
main = alert "hello bayhac"
~~~

Result:

~~~{.javascript}
var _0=0,
    _1=unCStr("hello bayhac"),
    _2=function(_){var _3=jsAlert(toJSStr(E(_1)));return _0;},
    _4=function(_){return _2(_);};
var hasteMain = function() {A(_4, [0]);};
window.onload = hasteMain;
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Marshaling
==========

`[<Constructor ID>, <Constructor Args>, ...]`

Examples:

<table>
<tr><th>Haskell</th><th>Javascript</th></tr>
<tr><td>`5 :: Int`{.haskell}</td><td>`[0,5]`{.javascript}</td></tr>
<tr><td>`Nothing :: Maybe ()`{.haskell}</td><td>`[0,[0]]`{.javascript}</td></tr>
<tr><td>`Just () :: Maybe ()`{.haskell}</td><td>`[1,[0]]`{.javascript}</td></tr>
<tr><td>`[1,2] :: [Int]`{.haskell}</td><td>`[1,[0,1],[1,[0,2],[0]]]`{.javascript}</td></tr>
</table>

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Marshaling
==========

Example code from the Haste runtime:

~~~{.javascript}
function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

FFI
===

Static:

~~~{.haskell}
foreign import ccall "alert" alert2 :: JSString -> IO ()
~~~

Dynamic:

~~~{.haskell}
alert3 :: JSString -> IO ()
alert3 = ffi "(function(x) { alert(x); })"
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

FFI Example
===========

~~~{.haskell}
module Main
( main
) where

import Haste.Prim
import Haste.Foreign

foreign import ccall "alert" alert2 :: JSString -> IO ()

alert3 :: JSString -> IO ()
alert3 = ffi "(function (x) { alert(x); })"

main :: IO ()
main = do
    alert2 $ toJSStr "hello bayhac 2"
    alert3 $ toJSStr "hello bayhac 3"
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

<!-- --------------------------------------------------------------------------- -->

Part 2: Portable Libraries with Haste
=====================================

Outline
=======

1.  Example
2.  Sharing code with native builds
3.  Exporting an API

<!--
5.  Debugging and testing
    1. quick-check (for native and javascript builds)
    2. compatibility tests (between native and javascript code)

6.  Runtime model, performance, and concurrency
4.  Build system
-->

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Example Application
===================

*  A very simple encryption library.

*  A very simple web email encryption application.

Code:

~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~

Native build
============

~~~{.bash}
cabal install --enable-tests
cabal test
~~~

Result:

[bayhac2014-cryptmail](http://192.168.88.101:3535/package/bayhac2014-cryptmail)

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Haskell Library API
===================

~~~{.haskell}
newtype Password = Password { unPassword ∷ B.ByteString }
    deriving (Eq, Code64)

newtype CipherText = CipherText { unCipherText ∷ B.ByteString }
    deriving (Eq, Code64)

newtype PlainText = PlainText { unPlainText ∷ B.ByteString }
    deriving (Eq, Code64)

encryptWithPwd ∷ Password → PlainText → IO CipherText
decryptWithPwd ∷ Password → CipherText → Either String PlainText
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Service API
===========

~~~{.haskell}
class (FromJSON α, ToJSON (Response α)) ⇒ Request α where
    type Response α ∷ *
    answerRequest ∷ α → IO (Either String (Response α))

data EncryptWithPwd = EncryptWithPwd
    { ePassword ∷ !Password
    , ePlainText ∷ !PlainText
    }

instance FromJSON EncryptWithPwd where
    parseJSON = withObject "EncryptWithPwd" $ \o → EncryptWithPwd
        <$> o .: "password"
        <*> o .: "plain_text"

data EncryptWithPwdR = EncryptWithPwdR
    { erCipherText ∷ !CipherText
    }

instance ToJSON EncryptWithPwdR where
    toJSON EncryptWithPwdR{..} = object
        [ "cipher_text" .= erCipherText
        ]

instance Request EncryptWithPwd where
    type Response EncryptWithPwd = EncryptWithPwdR
    answerRequest EncryptWithPwd{..} =
        Right ∘ EncryptWithPwdR <$> encryptWithPwd ePassword ePlainText

~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Haste build
===========

~~~{.bash}
rm -rf dist
haste-inst install
~~~

well ...

~~~{.bash}
rm -rf dist
mv cabal.sandbox.config cabal.sandbox.config.disabled
haste-inst install -fhaste --dependencies-only
haste-inst configure -fhaste
haste-inst build
~~~


`bayhac2014-cryptmail.cabal`:

~~~{.cabal}
...
ghc-options: -Wall --start=asap --with-js=lib/sjcl.js,lib/bayhac2014-cryptmail.js
...
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Result:

[bayhac2014-cryptmail-app](file:///Users/lars/Devel/Code/bayhac2014/Main.html)

Javascript API
==============

~~~{.javascript}
var logg = function(str) { console.log(str); return; }
var api = new API();

var req = { "password" : btoa(pwd), "plain_text" : btoa(msg) };
api.encrypt_with_password(req, logg, logg, function (r) {
    console.log(r.cipher_text);
}

var req = { "password" : btoa(pwd), "cipher_text" : msg };
api.decrypt_with_password(, logg, logg, function (r) {
   console.log(r.plain_text);
}
~~~

The Javascript API behave like service API

*   Supports usage of web-workers
*   Switching between server and client API
*   Easy compatibility testing

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Sharing Code with Native Builds
===============================

Examples:

*   Aeson instances
*   Cryptographic protocols
*   Special arithmetic
*   Business logic

Abstract low-level dependencies:

*   Javascript big integers
*   ByteStrings
*   Text
*   Cryptographic primitives

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Sharing Code with Native Builds
===============================

~~~{.cabal}
Flag haste
    description: build with haste-compiler
    default: False

Library
    exposed-modules:
        BayHac2014.Cryptmail.Text
        BayHac2014.Cryptmail.ByteString
        BayHac2014.Cryptmail.PasswordEncryption
        BayHac2014.Cryptmail.ServiceApi
        BayHac2014.Cryptmail.Json
    if flag(haste)
        build-depends: ...
    else
        build-depends: ...

Executable cryptmail-client
    if ! flag(haste)
        buildable: False

Executable cryptmail-server
    if flag(haste)
        buildable: False
~~~

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Abstraction Low-Level Dependencies
==================================

Example ByteString:

~~~{.haskell}
newtype ByteString = ByteString JSAny

foreign import ccall "bsEmpty" j_bytesEmpty ∷ IO ByteString
foreign import ccall "bsConcat" j_bytesConcat ∷ ByteString → ByteString → IO ByteString
foreign import ccall "bsEqual" j_bytesEqual ∷ ByteString → ByteString → Bool
foreign import ccall "bsLength" j_bytesLength ∷ ByteString → Int

instance Monoid ByteString where
    mempty = unsafePerformIO $ j_bytesEmpty
    mappend a b = unsafePerformIO $ j_bytesConcat a b

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance Eq ByteString where
    (==) a b = j_bytesEqual a b

    {-# INLINE (==) #-}

length ∷ ByteString → Int
length = j_bytesLength
~~~

Exporting an API
================

~~~{.haskell}
main ∷ IO ()
main = do
    register "encrypt_with_password" (asyncRequest ∷ ApiMethod EncryptWithPwd)
    register "decrypt_with_password" (asyncRequest ∷ ApiMethod DecryptWithPwd)

type ApiMethod α
    = α                    -- ^ argument
    → (JSString → IO ())   -- ^ log callback
    → (JSString → IO ())   -- ^ failure callback
    → (Response α → IO ()) -- ^ success callback
    → IO ()

register ∷ (FromJSON α, ToJSON (Response α)) ⇒ JSString → ApiMethod α → IO ()

asyncRequest ∷ Request α ⇒ ApiMethod α

type FCallback
    = Ptr Value                -- ^ argument
    → Ptr (JSString → IO ())   -- ^ log method
    → Ptr (JSString → IO ())   -- ^ failure continuation
    → Ptr (Ptr Value → IO ())  -- ^ success continuation
    → IO ()

foreign import ccall "addApiMethod" js_add_api_method ∷ JSString → JSFun FCallback → IO ()
~~~

Issues
======

*   PBKDF2 is made to be expensive

    <div class="notes">
    In the attack model we must assume that the attacker
    uses the most capable implemenation (which certainly is not implemented in javascript).
    </div>

    PBKDF2 execution should be asynchronous or in a web-worker

*   A JSON API requires textual encoding for all values 

    <div class="notes">
    It would be nice
    to transparently pass the plaintext and ciphertext as references when possible.
    </div>

<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

Misc Topics
============

*   JSON serialization and marshaling of JSON `Value`
*   Calling Javascript callbacks from Haskell

<!-- *   Stateful Javascript APIs -->

<!-- ### Runtime and Performance -->

*   Asynchronous callback execution
*   Concurrency

<!-- ### Build System -->

*   Integration with *haste-ffi-parser*
*   Managing Javascript dependencies for linked libraries
<!-- *   building web-workers -->
*   QuickCheck testing of Javascript builds
*   Exception handling
*   Alternatives to usage of CPP

<div class="notes">
Finally we integrate the haste port of this library in the the web application.
We want to do the glueing in javascript (otherwise we would have to
statically link at compiletime, which we don't want). Therefor we
have to export the library API in javascript. We do this by mimicing
a service. This would also allow to run the expensive operations
of the library in a web-worker. It also allows seemlessly decide if
the server-side implemenation or client side implemenation is used.

We conclude with some performance considerations:

*   PBKDF2 is made to be expensive. In the attack model we must assume that the attacker
    uses the most capable implemenation (which certainly is no implemented in javascript).

*   The PBKDF2 implemenation should at least be asynchronous or in a web-worker

*   How to do asynchronous operations in Haste?

*   A JSON API requires call be value and a textual encoding! It would be nice
    to transparently pass the plaintext and ciphertext as references when possible.

</div>


<div class="footer">
~~~{.bash}
git clone https://github.com/alephcloud/bayhac2014
~~~
</div>

