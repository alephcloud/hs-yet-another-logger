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
    → (LoggerT T.Text m α)
    → m α
withConsoleLogger level inner =
    withHandleLoggerBackend (config ^. loggerConfigBackend) $ \backend →
        withLogger config backend $ runLoggerT inner
  where
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ level

-- | A simple file logger
--
withFileLogger
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ FilePath
    → LogLevel
    → (LoggerT T.Text m α)
    → m α
withFileLogger f level inner =
    withHandleLoggerBackend (config ^. loggerConfigBackend) $ \backend →
        withLogger config backend $ runLoggerT inner
  where
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ level
        & loggerConfigBackend ∘ loggerBackendConfigColor .~ ColorFalse
        & loggerConfigBackend ∘ loggerBackendConfigHandle .~ FileHandle f

