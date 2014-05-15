-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Test
( tests
) where

import BayHac2014.Cryptmail.PasswordEncryption
import Distribution.TestSuite.QuickCheck

tests ∷ IO [Test]
tests = return
    [ testGroup "password encryption tests" passwordEncryptionTests
    ]

