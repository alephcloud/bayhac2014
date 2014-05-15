-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main
( main
) where

import Control.Monad

import Data.Monoid.Unicode

import Haste
import Haste.Prim

import BayHac2014.Cryptmail.Json
import BayHac2014.Cryptmail.ServiceApi

import Prelude.Unicode

main ∷ IO ()
main = do
    writeLog "Register API"
    register "encrypt_with_password" (asyncRequest ∷ RequestApiMethod EncryptWithPwd)
    register "decrypt_with_password" (asyncRequest ∷ RequestApiMethod DecryptWithPwd)

-- -------------------------------------------------------------------------- --
-- API methods

type RequestApiMethod α = ApiMethod α (Response α)

asyncRequest ∷ (Request α) ⇒ ApiMethod α (Response α)
asyncRequest req _pcb fsb scb = answerRequest req >>= \case
    Left e → fsb (toJSStr e)
    Right r → scb r

-- -------------------------------------------------------------------------- --
-- Utils

type FCallback
    = Ptr Value                -- ^ argument
    → Ptr (JSString → IO ())   -- ^ log method
    → Ptr (JSString → IO ())   -- ^ failure continuation
    → Ptr (Ptr Value → IO ())  -- ^ success continuation
    → IO ()

foreign import ccall "addApiMethod" js_add_api_method ∷ JSString → JSFun FCallback → IO ()

-- Call the Callbacks that we get from Javascript
foreign import ccall "calls" js_calls ∷ Ptr (JSString → IO ()) → Ptr JSString → IO ()
foreign import ccall "callv" js_callv ∷ Ptr (Ptr α → IO ()) → Ptr (Ptr α) → IO ()

type ApiMethod α β
    = α                   -- ^ argument
    → (JSString → IO ())  -- ^ log callback
    → (JSString → IO ())  -- ^ failure callback
    → (β → IO ())         -- ^ success callback
    → IO ()

register ∷ (FromJSON α, ToJSON β) ⇒ JSString → ApiMethod α β → IO ()
register call fun = do

    writeLog $ "Register API method: " ⊕ fromJSStr call

    -- register callback
    js_add_api_method call $! mkCallback $ \a lb fb sb → void $ do

        let successCb = js_callv sb ∘ toPtr
            failureCb = js_calls fb ∘ toPtr
            logCb = js_calls lb ∘ toPtr

        logCb $ "API call: " ⊕ call

        -- parse input argument
        case parseEither parseJSON ∘ fromPtr $ a of
            Left m → do
                let e' = "failed to parse input: " ⊕ toJSStr m
                logCb e'
                setTimeout 0 $ failureCb e'
            Right arg → do
                logCb $ "Argument: " ⊕ encode (fromPtr a)

                -- execute the method
                let sb' r = do
                        let jr = toJSON r
                        logCb $ "Result: " ⊕ encode jr
                        successCb (toPtr jr)
                    fb' e = do
                        let errMsg = "failed to run method " ⊕ call ⊕ ": " ⊕ e
                        logCb errMsg
                        failureCb errMsg

                setTimeout 0 $ fun arg logCb fb' sb'

