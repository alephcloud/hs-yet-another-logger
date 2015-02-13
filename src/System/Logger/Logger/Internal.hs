-- Copyright (c) 2014-2015 PivotCloud, Inc.
--
-- System.Logger.Logger.Internal
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
-- Module: System.Logger.Logger.Internal
-- Description: Yet Another Logger Implementation
-- Copyright: Copyright (c) 2014-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
-- This module provides a logger that implements the logger interface
-- that is defined in "System.Logger.Types".
--
-- If you want to roll your own implementation you may use the code in this
-- module as an example and starting point.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Logger.Logger.Internal
(
-- * Logger Configuration
  LoggerConfig(..)
, loggerConfigQueueSize
, loggerConfigThreshold
, loggerConfigScope
, defaultLoggerConfig
, validateLoggerConfig
, pLoggerConfig

-- * Logger
, Logger
, loggerScope
, loggerThreshold
, createLogger
, releaseLogger
, withLogger
, loggCtx
, withLogFunction

-- * LoggerT Monad Transformer
, LoggerT
, runLoggerT
, runLogT
) where

import Configuration.Utils hiding (Lens', Error)

import Control.Concurrent.Async
-- FIXME: use a better data structure with non-amortized complexity bounds
import Control.Monad.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Exception.Lifted
import Control.Exception.Enclosed
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.Trans.Control
import Control.Monad.Unicode

import Data.Monoid.Unicode
import Data.Typeable

import GHC.Generics

import Prelude.Unicode

-- internal modules

import System.Logger.Internal
import System.Logger.Types

-- -------------------------------------------------------------------------- --
-- Logger Configuration

-- | Logger Configuration
--
data LoggerConfig = LoggerConfig
    { _loggerConfigQueueSize ∷ !Int
    , _loggerConfigThreshold ∷ !LogLevel
        -- ^ initial log threshold, can be changed later on
    , _loggerConfigScope ∷ !LogScope
        -- ^ initial stack of log labels, can be extended later on
    , _loggerConfigPolicy ∷ !LogPolicy
        -- ^ how to deal with a congested logging pipeline
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

loggerConfigQueueSize ∷ Lens' LoggerConfig Int
loggerConfigQueueSize = lens _loggerConfigQueueSize $ \a b → a { _loggerConfigQueueSize = b }

loggerConfigThreshold ∷ Lens' LoggerConfig LogLevel
loggerConfigThreshold = lens _loggerConfigThreshold $ \a b → a { _loggerConfigThreshold = b }

loggerConfigScope ∷ Lens' LoggerConfig LogScope
loggerConfigScope = lens _loggerConfigScope $ \a b → a { _loggerConfigScope = b }

loggerConfigPolicy ∷ Lens' LoggerConfig LogPolicy
loggerConfigPolicy = lens _loggerConfigPolicy $ \a b → a { _loggerConfigPolicy = b }

instance NFData LoggerConfig

defaultLoggerConfig ∷ LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _loggerConfigQueueSize = 1000
    , _loggerConfigThreshold = Warn
    , _loggerConfigScope = []
    , _loggerConfigPolicy = LogPolicyDiscard
    }

validateLoggerConfig ∷ ConfigValidation LoggerConfig λ
validateLoggerConfig _ = return ()

instance ToJSON LoggerConfig where
    toJSON LoggerConfig{..} = object
        [ "queue_size" .= _loggerConfigQueueSize
        , "log_level" .= _loggerConfigThreshold
        , "scope" .= _loggerConfigScope
        , "policy" .= _loggerConfigPolicy
        ]

instance FromJSON (LoggerConfig → LoggerConfig) where
    parseJSON = withObject "LoggerConfig" $ \o → id
        <$< loggerConfigQueueSize ..: "queue_size" × o
        <*< loggerConfigThreshold ..: "log_level" × o
        <*< loggerConfigScope ..: "scope" × o
        <*< loggerConfigPolicy ..: "policy" × o

pLoggerConfig ∷ MParser LoggerConfig
pLoggerConfig = id
    <$< loggerConfigQueueSize .:: option auto
        × long "queue-size"
        ⊕ metavar "INT"
        ⊕ help "size of the internal logger queue"
    <*< loggerConfigThreshold .:: pLogLevel
    <*< loggerConfigPolicy .:: pLogPolicy

-- -------------------------------------------------------------------------- --
-- Logger
--
-- The logger encapsulates a queue and a background worker that dequeues
-- log-messages and delivers them to a backend action. The opaque logger
-- context is thread safe. But it contains references to mutable state and
-- no copy or derivation of it must be used out-side of it's allocation scope.
--

-- | Interal log message queue.
--
-- The backend function formats and delivers log messages synchronously. In
-- order to not slow down the processing of the main program logic log messages
-- are enqueued and processed asynchronously by a background worker that takes
-- the message from queue and calls the backend function for each log message.
--
type LoggerQueue a = TBMQueue (LogMessage a)

data Logger a = Logger
    { _loggerQueue ∷ !(LoggerQueue a)
    , _loggerWorker ∷ !(Async ())
    , _loggerThreshold ∷ !LogLevel
    , _loggerScope ∷ !LogScope
    , _loggerPolicy ∷ !LogPolicy
    , _loggerMissed ∷ !(TVar Int)
    }
    deriving (Typeable, Generic)

loggerQueue ∷ Lens' (Logger a) (LoggerQueue a)
loggerQueue = lens _loggerQueue $ \a b → a { _loggerQueue = b }
{-# INLINE loggerQueue #-}

loggerWorker ∷ Lens' (Logger a) (Async ())
loggerWorker = lens _loggerWorker $ \a b → a { _loggerWorker = b }
{-# INLINE loggerWorker #-}

loggerThreshold ∷ Lens' (Logger a) LogLevel
loggerThreshold = lens _loggerThreshold $ \a b → a { _loggerThreshold = b }
{-# INLINE loggerThreshold #-}

loggerScope ∷ Lens' (Logger a) LogScope
loggerScope = lens _loggerScope $ \a b → a { _loggerScope = b }
{-# INLINE loggerScope #-}

loggerPolicy ∷ Lens' (Logger a) LogPolicy
loggerPolicy = lens _loggerPolicy $ \a b → a { _loggerPolicy = b }
{-# INLINE loggerPolicy #-}

loggerMissed ∷ Lens' (Logger a) (TVar Int)
loggerMissed = lens _loggerMissed $ \a b → a { _loggerMissed = b }
{-# INLINE loggerMissed #-}

createLogger
    ∷ MonadIO μ
    ⇒ LoggerConfig
    → LoggerBackend a
    → μ (Logger a)
createLogger LoggerConfig{..} backend = liftIO $ do
    queue ← newTBMQueueIO _loggerConfigQueueSize
    missed ← newTVarIO 0
    worker ← backendWorker backend queue missed
    return $ Logger
        { _loggerQueue = queue
        , _loggerWorker = worker
        , _loggerThreshold = _loggerConfigThreshold
        , _loggerScope = _loggerConfigScope
        , _loggerPolicy = _loggerConfigPolicy
        , _loggerMissed = missed
        }

-- FIXME: make this more reliable
--
-- For instance if 'readTBMQeue' (not sure if that can happen) throws an
-- exception 'releaseLogger' may not terminate.
--
-- We must deal better with exceptions thrown by the backend: we should
-- use some reasonable re-spawn logic. Right now there is the risk of a
-- busy loop.
--
backendWorker
    ∷ LoggerBackend a
    → LoggerQueue a
    → TVar Int
    → IO (Async ())
backendWorker backend queue missed = async $ go `catchAny` \e → do
    -- chances are that this fails, too...
    (backend ∘ Left $ backendErrorMsg (sshow e)) `catchAny` (const $ return ())
    go
  where
    go = atomically readMsg ≫= \case
        -- when the queue is closed and empty the backendWorker returns
        Nothing → return ()
        -- When there are still messages to process the backendWorker loops
        Just msg → backend msg ≫ go

    -- As long as the queue is not closed and empty this retries until
    -- a new message arrives
    --
    readMsg = do
        n ← swapTVar missed 0
        if n > 0
          then do
            return ∘ Just ∘ Left $ discardMsg n
          else
            fmap Right <$> readTBMQueue queue

    -- A log message that informs about discarded log messages
    discardMsg n = LogMessage
        { _logMsg = "discarded " ⊕ sshow n ⊕ " log messages"
        , _logMsgLevel = Warn
        , _logMsgScope = [("system", "logger")]
        }

    backendErrorMsg e = LogMessage
        { _logMsg = e
        , _logMsgLevel = Error
        , _logMsgScope = [("system", "logger"), ("component", "backend")]
        }

releaseLogger
    ∷ MonadIO μ
    ⇒ Logger a
    → μ ()
releaseLogger Logger{..} = liftIO $ do
    atomically $ closeTBMQueue _loggerQueue
    wait _loggerWorker

-- | Provide a computation with a 'Logger'.
--
-- Here is an example how this can be used to run a computation
-- with a 'MonadLog' constraint:
--
-- > withConsoleLogger
-- >     ∷ (MonadIO m, MonadBaseControl IO m)
-- >     ⇒ LogLevel
-- >     → LoggerT T.Text m α
-- >     → m α
-- > withConsoleLogger level inner = do
-- >    withHandleBackend (config ^. logConfigBackend) $ \backend →
-- >        withLogger (config ^. logConfigLogger) backend $ runLoggerT inner
-- >  where
-- >    config = defaultLogConfig
-- >        & logConfigLogger ∘ loggerConfigThreshold .~ level
--
withLogger
    ∷ (MonadIO μ, MonadBaseControl IO μ)
    ⇒ LoggerConfig
    → LoggerBackend a
    → (Logger a → μ α)
    → μ α
withLogger config backend =
        bracket (createLogger config backend) releaseLogger

-- | For simple cases, when the logger threshold and the logger scope is
-- constant this function can be used to directly initialize a log function.
--
withLogFunction
    ∷ (Show a, Typeable a, NFData a, MonadIO μ, MonadBaseControl IO μ)
    ⇒ LoggerConfig
    → LoggerBackend a
    → (LogFunctionIO a → μ α)
    → μ α
withLogFunction config backend f = withLogger config backend $ f ∘ loggCtx

-- -------------------------------------------------------------------------- --
-- Log Function

data LoggerException a
    = QueueFullException (LogMessage a)
    deriving (Show, Eq, Ord, Typeable, Generic)

instance (Typeable a, Show a) ⇒ Exception (LoggerException a)

-- Log a message with the given logger context
--
-- If the logger context has been released (by closing the queue)
-- this function has not effect.
--
loggCtx
    ∷ (Show a, Typeable a, NFData a)
    ⇒ Logger a
    → LogFunctionIO a
loggCtx Logger{..} level msg = do
    case _loggerThreshold of
        Quiet → return ()
        threshold
            | level ≤ threshold → liftIO ∘ atomically $
                writeWithLogPolicy $!! LogMessage
                    { _logMsg = msg
                    , _logMsgLevel = level
                    , _logMsgScope = _loggerScope
                    }
            | otherwise → return ()
  where
    writeWithLogPolicy lmsg
        | _loggerPolicy ≡ LogPolicyBlock = writeTBMQueue _loggerQueue lmsg
        | otherwise = tryWriteTBMQueue _loggerQueue lmsg ≫= \case
            Just False
                | _loggerPolicy ≡ LogPolicyDiscard → modifyTVar' _loggerMissed succ
                | _loggerPolicy ≡ LogPolicyRaise → throwSTM $ QueueFullException lmsg

            _ → return ()
{-# INLINEABLE loggCtx #-}

-- -------------------------------------------------------------------------- --
-- Logger Instance

instance LoggerCtx (Logger a) a where
    loggerFunIO = loggCtx
    setLoggerLevel = loggerThreshold
    setLoggerScope = loggerScope
    setLoggerPolicy = loggerPolicy

-- -------------------------------------------------------------------------- --
-- LoggerT

type LoggerT a = LoggerCtxT (Logger a)

runLoggerT ∷ LoggerT a m α → Logger a → m α
runLoggerT = runLoggerCtxT
{-# INLINE runLoggerT #-}

-- | Convenience function that unwraps a 'MonadLog' computation over
-- a newly created 'Logger'
--
runLogT
    ∷ (MonadBaseControl IO m, MonadIO m)
    ⇒ LoggerConfig
    → LoggerBackend msg
    → LoggerT msg m α
    → m α
runLogT config backend = withLogger config backend ∘ runLoggerT

-- -------------------------------------------------------------------------- --
-- Tools

{-
-- | Log all errors that are in current error trace and reset the trace
-- to a single short summary message.
--
logErrorsG
    ∷ MonadIO μ
    ⇒ LogLevel
    → T.Text
    → ExceptT [T.Text] μ α
    → ExceptT [T.Text] μ α
logErrorsG level label p = p `catchError` \e → do
    loggG level $ label ⊕ " failed: "  ⊕ T.intercalate " <|> " e
    throwError [label ⊕ " failed"]
-}


