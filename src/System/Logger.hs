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

-- |
-- Module: System.Logger
-- Description: Yet Another Logger
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: Apache-2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module re-exports the logger interface from "System.Logger.Types" and
-- the implementation of that interface from "System.Logger.Logger" and
-- "System.Logger.Backend.Handle".
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Logger
( withConsoleLogger
, withFileLogger

-- * Logger Interface
, module System.Logger.Types

-- * Yet Another Logger
, module System.Logger.Logger

-- * Handle Backend
, module System.Logger.Backend.Handle

-- * Logging System Configuration
, LogConfig(..)
, logConfigLogger
, logConfigBackend
, defaultLogConfig
, validateLogConfig
, pLogConfig
) where

import Configuration.Utils hiding (Lens')

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.Text as T
import Data.Typeable

import GHC.Generics

import Prelude.Unicode

import System.Logger.Backend.ColorOption
import System.Logger.Backend.Handle
import System.Logger.Logger
import System.Logger.Types

-- | A simple console logger
--
-- > import System.Logger
-- >
-- > main ∷ IO ()
-- > main = withConsoleLogger Info $ do
-- >     logg Info "moin"
-- >     withLabel ("function", "f") f
-- >     logg Warn "tschüss"
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
    withHandleBackend (config ^. logConfigBackend) $ \backend →
        withLogger (config ^. logConfigLogger) backend $ runLoggerT inner
  where
    config = defaultLogConfig
        & logConfigLogger ∘ loggerConfigThreshold .~ level

-- | A simple file logger
--
withFileLogger
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ FilePath
    → LogLevel
    → LoggerT T.Text m α
    → m α
withFileLogger f level inner =
    withHandleBackend (config ^. logConfigBackend) $ \backend →
        withLogger (config ^. logConfigLogger) backend $ runLoggerT inner
  where
    config = defaultLogConfig
        & logConfigLogger ∘ loggerConfigThreshold .~ level
        & logConfigBackend ∘ handleBackendConfigColor .~ ColorFalse
        & logConfigBackend ∘ handleBackendConfigHandle .~ FileHandle f

-- -------------------------------------------------------------------------- --
-- Logging System Configuration

data LogConfig = LogConfig
    { _logConfigLogger ∷ !LoggerConfig
    , _logConfigBackend ∷ !HandleBackendConfig
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

logConfigLogger ∷ Lens' LogConfig LoggerConfig
logConfigLogger = lens _logConfigLogger $ \a b → a { _logConfigLogger = b }

logConfigBackend ∷ Lens' LogConfig HandleBackendConfig
logConfigBackend = lens _logConfigBackend $ \a b → a { _logConfigBackend = b }

defaultLogConfig ∷ LogConfig
defaultLogConfig = LogConfig
    { _logConfigLogger = defaultLoggerConfig
    , _logConfigBackend = defaultHandleBackendConfig
    }

validateLogConfig ∷ ConfigValidation LogConfig []
validateLogConfig LogConfig{..} = do
    validateLoggerConfig _logConfigLogger
    validateHandleBackendConfig _logConfigBackend

instance ToJSON LogConfig where
    toJSON LogConfig{..} = object
        [ "logger" .= _logConfigLogger
        , "backend" .= _logConfigBackend
        ]

instance FromJSON (LogConfig → LogConfig) where
    parseJSON = withObject "LogConfig" $ \o → id
        <$< logConfigLogger %.: "logger" × o
        <*< logConfigBackend %.: "backend" × o

pLogConfig ∷ MParser LogConfig
pLogConfig = id
    <$< logConfigLogger %:: pLoggerConfig
    <*< logConfigBackend %:: pHandleBackendConfig

