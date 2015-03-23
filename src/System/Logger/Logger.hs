-- Copyright (c) 2014-2015 PivotCloud, Inc.
--
-- System.Logger.Logger
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
-- Module: System.Logger.Logger
-- Description: Yet Another Logger Implementation
-- Copyright: Copyright (c) 2014-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module provides a logger that implements the logger interface
-- that is defined in "System.Logger.Types".
--
-- All the code of this module is in "System.Logger.Logger.Internal".
--
-- The definitions in "System.Logger.Types" are re-exported by this module.
--

{-# LANGUAGE UnicodeSyntax #-}

module System.Logger.Logger
(
-- * Re-Export Logger Interface
  module System.Logger.Types

-- * Logger
, Logger
, withLogger
, withLogger_
, withLogFunction

-- * LoggerT Monad Transformer
, LoggerT
, runLoggerT
, runLogT

-- * Configuration Types

-- ** Logger Configuration
, LoggerConfig(..)
, loggerConfigQueueSize
, loggerConfigThreshold
, loggerConfigScope
, loggerConfigPolicy
, loggerConfigExceptionLimit
, loggerConfigExceptionWait
, loggerConfigExitTimeout
, defaultLoggerConfig
, validateLoggerConfig
, pLoggerConfig
, pLoggerConfig_

) where

import System.Logger.Types
import System.Logger.Logger.Internal

