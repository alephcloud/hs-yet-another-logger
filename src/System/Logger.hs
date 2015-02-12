-- Copyright (c) 2014-2015 PivotCloud, Inc.
--
-- System.Logger
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: System.Logger
-- Description: Yet Another Logger
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: Apache-2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module re-exports the logger interface from "System.Logger.Types" and
-- the implementation of that interface from "System.Logger.Logger".
--
module System.Logger
( withConsoleLogger
, withFileLogger

-- * Logger Interface
, module System.Logger.Types

-- * Yet Another Logger
, module System.Logger.Logger
) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.Text as T

import Prelude.Unicode

import System.Logger.Types
import System.Logger.Logger
import System.Logger.ColorOption

-- | A simple console logger
--
-- > import System.Logger
-- >
-- > main ∷ IO ()
-- > main = withConsoleLogger Info $ do
-- >     logg Info "moin"
-- >     withLabel ("function", "f") f
-- >     logg Info "tschüss"
-- >   where
-- >     f = withLevel Debug $ do
-- >         logg Debug "debug f"
--
withConsoleLogger
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ LogLevel
    → LoggerT T.Text m α
    → m α
withConsoleLogger level inner =
    withHandleLoggerBackend (config ^. loggerConfigBackend) $ \backend →
        withLoggerCtx config backend $ \loggerCtx →
            runLoggerT loggerCtx inner
  where
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ level

-- | A simple file logger
--
withFileLogger
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ FilePath
    → LogLevel
    → LoggerT T.Text m α
    → m α
withFileLogger f level inner =
    withHandleLoggerBackend (config ^. loggerConfigBackend) $ \backend →
        withLoggerCtx config backend $ \loggerCtx →
            runLoggerT loggerCtx inner
  where
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ level
        & loggerConfigBackend ∘ loggerBackendConfigColor .~ ColorFalse
        & loggerConfigBackend ∘ loggerBackendConfigHandle .~ FileHandle f

