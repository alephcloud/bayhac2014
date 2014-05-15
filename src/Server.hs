-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main
( main
) where

import BayHac2014.Cryptmail.ServiceApi

import Control.Monad.IO.Class

import qualified Data.Text.Lazy as LT

import Prelude.Unicode

import Web.Scotty

main ∷ IO ()
main = server

servicePort ∷ Int
servicePort = 8080

server ∷ IO ()
server = scotty servicePort $ do

    post "/encrypt" $ do
        req ∷ EncryptWithPwd ← jsonData
        res req

    post "/decrypt" $ do
        req ∷ DecryptWithPwd ← jsonData
        res req

  where
    res ∷ Request α ⇒ α → ActionM ()
    res req = either (raise ∘ LT.pack) json =<< liftIO (answerRequest req)

#if 0

prop_service ∷ Password → PlainText → Property
prop_service0 pwd d = monadicIO $ do
    pid ← forkIO service
    run

#endif
