-- Copyright (c) 2014-2015 PivotCloud, Inc.
--
-- System.Logger.ColorOption
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.

-- |
-- Module: System.Logger.ColorOption
-- Copyright: Copyright (c) 2014-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- An option that indicates whether ANSI color escapes shall
-- be used in textual output.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Logger.ColorOption
( ColorOption(..)
, readColorOption
, colorOptionText
, defaultColorOption
, pColorOption
, useColor
) where

import Configuration.Utils

import Control.DeepSeq
import Control.Monad.Except

import qualified Data.CaseInsensitive as CI
import Data.Monoid
import Data.Monoid.Unicode
import Data.String
import Data.Typeable

import GHC.Generics

import qualified Options.Applicative as O

import Prelude.Unicode

import qualified System.Console.ANSI as A
import System.IO (stdout)

-- -------------------------------------------------------------------------- --
-- Color Option

-- | Color Option
--
data ColorOption
    = ColorAuto
    | ColorFalse
    | ColorTrue
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

instance NFData ColorOption

readColorOption
    ∷  (Monad m, Eq a, Show a, CI.FoldCase a, IsString a, IsString e, Monoid e, MonadError e m)
    ⇒ a
    → m ColorOption
readColorOption x = case CI.mk x of
    "auto" → return ColorAuto
    "false" → return ColorFalse
    "true" → return ColorTrue
    e → throwError $ "unexpected color option value: "
        ⊕ fromString (show e)
        ⊕ ", expected \"auto\", \"false\", or \"true\""

colorOptionText
    ∷ IsString a
    ⇒ ColorOption
    → a
colorOptionText ColorAuto = "auto"
colorOptionText ColorFalse = "false"
colorOptionText ColorTrue = "true"

defaultColorOption ∷ ColorOption
defaultColorOption = ColorAuto

instance ToJSON ColorOption where
    toJSON = String ∘ colorOptionText

instance FromJSON ColorOption where
    parseJSON = withText "ColorOption" $ either fail return ∘ readColorOption

pColorOption ∷ O.Parser ColorOption
pColorOption = option (eitherReader readColorOption)
   × long "color"
   ⊕ short 'c'
   ⊕ help "whether to use ANSI terminal colors in the output"

useColor
    ∷ ColorOption
    → IO Bool
useColor ColorFalse = return False
useColor ColorTrue = return True
useColor ColorAuto = A.hSupportsANSI stdout -- FIXME: what about stderr

