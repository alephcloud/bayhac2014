-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module BayHac2014.Cryptmail.Text
(

#ifdef __HASTE__

  Text
, pack
, unpack

#else
  module Data.Text
#endif

) where

#ifdef __HASTE__

import Data.Monoid
import Haste hiding (pack, unpack)
import Haste.Prim

type Text = JSString

instance Monoid JSString where
       mempty = ""
       a `mappend` b = toJSStr $ fromJSStr a `mappend` fromJSStr b

pack ∷ String → Text
pack = toJSStr

unpack ∷ Text → String
unpack = fromJSStr

#else

import Data.Text

#endif

