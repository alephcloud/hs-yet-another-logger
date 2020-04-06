-- Copyright (c) 2016-2020 Lars Kuhtz <lakuhtz@gmail.com>
-- Copyright (c) 2014-2015 PivotCloud, Inc.
--
-- System.Logger.Backend.Handle
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
-- Module: System.Logger.Backend.Handle
-- Description: Handle Backend for Yet Another Logger
-- Copyright:
--     Copyright (c) 2016-2020 Lars Kuhtz <lakuhtz@gmail.com>
--     Copyright (c) 2014-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Logger.Backend.Handle
(
-- * Handle Configuration
  LoggerHandleConfig(..)
, loggerHandleConfigText
, readLoggerHandleConfig
, validateLoggerHandleConfig
, pLoggerHandleConfig
, pLoggerHandleConfig_

-- * Backend Configuration
, HandleBackendConfig(..)
, handleBackendConfigHandle
, handleBackendConfigColor
, defaultHandleBackendConfig
, validateHandleBackendConfig
, pHandleBackendConfig
, pHandleBackendConfig_

-- * Backend Implementation
, withHandleBackend
, withHandleBackend_
, handleBackend
, handleBackend_
) where

import Configuration.Utils hiding (Error, Lens')
import Configuration.Utils.Validation

import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Writer

import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lens
import Data.Typeable

import GHC.Generics

import qualified Options.Applicative as O

import Prelude.Unicode

import qualified System.Console.ANSI as A
import System.IO

-- internal modules

import System.Logger.Backend.ColorOption
import System.Logger.Internal
import System.Logger.Types

-- -------------------------------------------------------------------------- --
-- Handle Logger Backend Configuration

data LoggerHandleConfig
    = StdOut
    | StdErr
    | FileHandle FilePath
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance NFData LoggerHandleConfig

readLoggerHandleConfig
    ∷ (MonadError e m, Eq a, Show a, CI.FoldCase a, IsText a, IsString e, Monoid e)
    ⇒ a
    → m LoggerHandleConfig
readLoggerHandleConfig x = case CI.mk tx of
    "stdout" → return StdOut
    "stderr" → return StdErr
    _ | CI.mk (L.take 5 tx) ≡ "file:" → return $ FileHandle (L.drop 5 tx)
    e → throwError $ "unexpected logger handle value: "
        ⊕ fromString (show e)
        ⊕ ", expected \"stdout\", \"stderr\", or \"file:<FILENAME>\""
  where
    tx = packed # x

loggerHandleConfigText
    ∷ (IsString a, Monoid a)
    ⇒ LoggerHandleConfig
    → a
loggerHandleConfigText StdOut = "stdout"
loggerHandleConfigText StdErr = "stderr"
loggerHandleConfigText (FileHandle f) = "file:" ⊕ fromString f

validateLoggerHandleConfig ∷ ConfigValidation LoggerHandleConfig λ
validateLoggerHandleConfig (FileHandle filepath) = validateFileWritable "file handle" filepath
validateLoggerHandleConfig _ = return ()

instance ToJSON LoggerHandleConfig where
    toJSON = String ∘ loggerHandleConfigText

instance FromJSON LoggerHandleConfig where
    parseJSON = withText "LoggerHandleConfig" $ either fail return ∘ readLoggerHandleConfig

pLoggerHandleConfig ∷ O.Parser LoggerHandleConfig
pLoggerHandleConfig = pLoggerHandleConfig_ ""

-- | A version of 'pLoggerHandleConfig' that takes a prefix for the
-- command line option.
--
-- @since 0.2
--
pLoggerHandleConfig_
    ∷ T.Text
        -- ^ prefix for the command line options.
    → O.Parser LoggerHandleConfig
pLoggerHandleConfig_ prefix = option (eitherReader readLoggerHandleConfig)
    % long (T.unpack prefix ⊕ "logger-backend-handle")
    ⊕ metavar "stdout|stderr|file:<FILENAME>"
    ⊕ help "handle where the logs are written"

-- -------------------------------------------------------------------------- --
-- Logger Backend Configuration

-- | HandleBackendConfig
--
data HandleBackendConfig = HandleBackendConfig
    { _handleBackendConfigColor ∷ !ColorOption
    , _handleBackendConfigHandle ∷ !LoggerHandleConfig
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

handleBackendConfigColor ∷ Lens' HandleBackendConfig ColorOption
handleBackendConfigColor = lens _handleBackendConfigColor $ \a b → a { _handleBackendConfigColor = b }

handleBackendConfigHandle ∷ Lens' HandleBackendConfig LoggerHandleConfig
handleBackendConfigHandle = lens _handleBackendConfigHandle $ \a b → a { _handleBackendConfigHandle = b }

instance NFData HandleBackendConfig

defaultHandleBackendConfig ∷ HandleBackendConfig
defaultHandleBackendConfig = HandleBackendConfig
    { _handleBackendConfigColor = defaultColorOption
    , _handleBackendConfigHandle = StdOut
    }

validateHandleBackendConfig ∷ ConfigValidation HandleBackendConfig []
validateHandleBackendConfig HandleBackendConfig{..} = do
        validateLoggerHandleConfig _handleBackendConfigHandle
        case (_handleBackendConfigHandle, _handleBackendConfigColor) of
            (FileHandle _, ColorTrue) →
                tell ["log messages are formatted using ANSI color escape codes but are written to a file"]
            _ → return ()

instance ToJSON HandleBackendConfig where
    toJSON HandleBackendConfig{..} = object
        [ "color" .= _handleBackendConfigColor
        , "handle" .= _handleBackendConfigHandle
        ]

instance FromJSON (HandleBackendConfig → HandleBackendConfig) where
    parseJSON = withObject "HandleBackendConfig" $ \o → id
        <$< handleBackendConfigColor ..: "color" % o
        <*< handleBackendConfigHandle ..: "handle" % o

pHandleBackendConfig ∷ MParser HandleBackendConfig
pHandleBackendConfig = pHandleBackendConfig_ ""

-- | A version of 'pLoggerHandleBackendConfig' that takes a prefix for the
-- command line option.
--
-- @since 0.2
--
pHandleBackendConfig_
    ∷ T.Text
        -- ^ prefix for this and all subordinate command line options.
    → MParser HandleBackendConfig
pHandleBackendConfig_ prefix = id
    <$< handleBackendConfigColor .:: pColorOption_ prefix
    <*< handleBackendConfigHandle .:: pLoggerHandleConfig_ prefix

-- -------------------------------------------------------------------------- --
-- Backend Implementation

withHandleBackend
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ HandleBackendConfig
    → (LoggerBackend T.Text → m α)
    → m α
withHandleBackend = withHandleBackend_ id
{-# INLINE withHandleBackend #-}

-- | A version of 'withHandleBackend' that is generic in the type
-- of the log message.
--
-- @since 0.2.2
--
withHandleBackend_
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ (msg → T.Text)
        -- ^ formatting function for the log message
    → HandleBackendConfig
    → (LoggerBackend msg → m α)
    → m α
withHandleBackend_ format conf inner =
    case conf ^. handleBackendConfigHandle of
        StdErr → run stderr
        StdOut → run stdout
        FileHandle f → liftBaseOp (withFile f AppendMode) run
  where
    run h = do
        colored ← liftIO $ useColor (conf ^. handleBackendConfigColor) h
        inner $ handleBackend_ format h colored

handleBackend
    ∷ Handle
    → Bool
        -- ^ whether to use ANSI color escape codes
    → LoggerBackend T.Text
handleBackend = handleBackend_ id
{-# INLINE handleBackend #-}

-- | A version of 'handleBackend' that is generic in the type of
-- the log message.
--
-- @since 0.2.2
--
handleBackend_
    ∷ (msg → T.Text)
        -- ^ formatting function for the log message
    → Handle
    → Bool
        -- ^ whether to use ANSI color escape codes
    → LoggerBackend msg
handleBackend_ format h colored eitherMsg = do
    T.hPutStrLn h
        $ formatIso8601Milli (msg ^. logMsgTime) ⊕ " "
        ⊕ inLevelColor colored ("[" ⊕ sshow level ⊕ "] ")
        ⊕ inScopeColor colored ("[" ⊕ formatedScope ⊕ "] ")
        ⊕ (msg ^. logMsg)
  where
    msg = either id (logMsg %~ format) eitherMsg
    level = msg ^. logMsgLevel

    formatedScope = T.intercalate "|" ∘ L.map formatLabel ∘ reverse $ msg ^. logMsgScope
    formatLabel (key, val) = key ⊕ "=" ⊕ val

    inScopeColor True = inBlue
    inScopeColor False = id

    inLevelColor True = case level of
        Error → inRed
        Warn → inOrange
        Info → inGreen
        _ → id
    inLevelColor False = id

    inColor ∷ A.ColorIntensity → A.Color → T.Text → T.Text
    inColor i c t = T.pack (A.setSGRCode [A.SetColor A.Foreground i c]) ⊕ t ⊕ T.pack (A.setSGRCode [A.Reset])

    inRed ∷ T.Text → T.Text
    inRed = inColor A.Vivid A.Red

    inOrange ∷ T.Text → T.Text
    inOrange = inColor A.Dull A.Red

    inGreen ∷ T.Text → T.Text
    inGreen = inColor A.Dull A.Green

    inBlue ∷ T.Text → T.Text
    inBlue = inColor A.Dull A.Blue
{-# INLINEABLE handleBackend_ #-}
