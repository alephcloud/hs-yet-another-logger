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

{-# LANGUAGE GADTs #-}
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
, pLoggerConfig_

-- * Logger
, Logger
, loggerScope
, loggerThreshold
, createLogger
, createLogger_
, releaseLogger
, withLogger
, withLogger_
, loggCtx
, withLogFunction

-- * LoggerT Monad Transformer
, LoggerT
, runLoggerT
, runLogT
) where

import Configuration.Utils hiding (Lens', Error)

import Control.Concurrent (threadDelay)
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
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Text.IO as T (hPutStrLn)
import Data.Void

import GHC.Generics

import Prelude.Unicode

import System.IO (stderr)
import System.Timeout

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
    , _loggerConfigExceptionLimit ∷ !(Maybe Int)
        -- ^ number of consecutive backend exception that can occur before the logger
        -- raises an 'BackendToManyExceptions' exception. If this is 'Nothing'
        -- the logger will discard all exceptions. For instance a value of @1@
        -- means that an exception is raised when the second exception occurs.
        -- A value of @0@ means that an exception is raised for each exception.
    , _loggerConfigExceptionWait ∷ !(Maybe Int)
        -- ^ number of microseconds to wait after an exception from the backend.
        -- If this is 'Nothing' the logger won't wait at all after an exception.
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

loggerConfigExceptionLimit ∷ Lens' LoggerConfig (Maybe Int)
loggerConfigExceptionLimit = lens _loggerConfigExceptionLimit $ \a b → a { _loggerConfigExceptionLimit = b }

loggerConfigExceptionWait ∷ Lens' LoggerConfig (Maybe Int)
loggerConfigExceptionWait = lens _loggerConfigExceptionWait $ \a b → a { _loggerConfigExceptionWait = b }

instance NFData LoggerConfig

-- | Default Logger configuration
--
-- The exception limit for backend exceptions is 10 and the wait time between
-- exceptions is 1000. This means that in case of a defunctioned backend the
-- logger will exist by throwing an exception after at least one second.
--
defaultLoggerConfig ∷ LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _loggerConfigQueueSize = 1000
    , _loggerConfigThreshold = Warn
    , _loggerConfigScope = []
    , _loggerConfigPolicy = LogPolicyDiscard
    , _loggerConfigExceptionLimit = Just 10
    , _loggerConfigExceptionWait = Just 1000
    }

validateLoggerConfig ∷ ConfigValidation LoggerConfig λ
validateLoggerConfig _ = return ()

instance ToJSON LoggerConfig where
    toJSON LoggerConfig{..} = object
        [ "queue_size" .= _loggerConfigQueueSize
        , "log_level" .= _loggerConfigThreshold
        , "scope" .= _loggerConfigScope
        , "policy" .= _loggerConfigPolicy
        , "exception_limit" .= _loggerConfigExceptionLimit
        , "exception_wait" .= _loggerConfigExceptionWait
        ]

instance FromJSON (LoggerConfig → LoggerConfig) where
    parseJSON = withObject "LoggerConfig" $ \o → id
        <$< loggerConfigQueueSize ..: "queue_size" × o
        <*< loggerConfigThreshold ..: "log_level" × o
        <*< loggerConfigScope ..: "scope" × o
        <*< loggerConfigPolicy ..: "policy" × o
        <*< loggerConfigExceptionLimit ..: "exception_limit" × o
        <*< loggerConfigExceptionWait ..: "exception_wait" × o

pLoggerConfig ∷ MParser LoggerConfig
pLoggerConfig = pLoggerConfig_ ""

pLoggerConfig_
    ∷ T.Text
        -- ^ prefix for this and all subordinate command line options.
    → MParser LoggerConfig
pLoggerConfig_ prefix = id
    <$< loggerConfigQueueSize .:: option auto
        × long (T.unpack prefix ⊕ "queue-size")
        ⊕ metavar "INT"
        ⊕ help "size of the internal logger queue"
    <*< loggerConfigThreshold .:: pLogLevel_ prefix
    <*< loggerConfigPolicy .:: pLogPolicy_ prefix
    <*< loggerConfigExceptionLimit .:: fmap Just × option auto
        × long (T.unpack prefix ⊕ "exception-limit")
        ⊕ metavar "INT"
        ⊕ help "maximal number of backend failures before and exception is raised"
    <*< loggerConfigExceptionWait .:: fmap Just × option auto
        × long (T.unpack prefix ⊕ "exception-wait")
        ⊕ metavar "INT"
        ⊕ help "time to wait after an backend failure occured"

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

-- | Create a new logger. A logger created with this function must be released
-- with a call to 'releaseLogger' and must not be used after it is released.
--
-- The logger calls the backend function exactly once for each log message. If
-- the backend throws an exception, the message is discarded and the exception
-- is dealt with as follows:
--
-- 1. The exception is logged. First it is attempt to log to the backend itself.
--    If that fails, due to another exception, the incident is logged to an
--    alternate log sink, usually @T.putStrLn@ or just @const (return ())@.
--
-- 2. The message is discarded. If the backend exception is of type
--    'BackendTerminatedException' the exception is rethrown by the logger which
--    causes the logger to exit. Otherwise the exception is appended to the
--    exception list.
--
-- 3. If the length of the exception list exceeds a configurable threshold
--    a 'BackendToManyExceptions' exception is thrown (which causes the logger
--    to terminate).
--
-- 4. Otherwise the logger waits for a configurable amount of time before
--    proceeding.
--
-- 5. The next time the backend returns without throwing an exception the
--    exception list is reset to @[]@.
--
-- Backends are expected to implement there own retry logic if required.
-- Backends may base their behavoir on the 'LogPolicy' that is effective for a
-- given message. Please refer to the documentation of 'LoggerBackend' for
-- more details about how to implement and backend.
--
-- Backends are called synchronously. Backends authors must thus ensure that a
-- backend returns promptly in accordance with the 'LogPolicy' and the size of
-- the logger queue. For more elaborate failover strategies, such as batching
-- retried messages with the delivery of new messages, backends may implement
-- there only internal queue.
--
-- Exceptions of type 'BlockedIndefinitelyOnSTM' and 'NestedAtomically' are
-- rethrowin immediately. Those exceptions indicate a bug in the code due to
-- unsafe usage of 'createLogger'. This exceptions shouldn't be possible when
-- 'withLogger' is used to provide the logger and and the reference to the
-- logger isn't used outside the scope of the bracket.
--
createLogger
    ∷ MonadIO μ
    ⇒ LoggerConfig
    → LoggerBackend a
    → μ (Logger a)
createLogger = createLogger_ (T.hPutStrLn stderr)

-- | A version of 'createLogger' that takes as an extra argument
-- a function for logging errors in the logging system.
--
createLogger_
    ∷ MonadIO μ
    ⇒ (T.Text → IO ())
        -- ^ alternate sink for logging exceptions in the logger itself.
    → LoggerConfig
    → LoggerBackend a
    → μ (Logger a)
createLogger_ errLogFun LoggerConfig{..} backend = liftIO $ do
    queue ← newTBMQueueIO _loggerConfigQueueSize
    missed ← newTVarIO 0
    worker ← backendWorker errLogFun _loggerConfigExceptionLimit _loggerConfigExceptionWait backend queue missed
    link worker
    return $ Logger
        { _loggerQueue = queue
        , _loggerWorker = worker
        , _loggerThreshold = _loggerConfigThreshold
        , _loggerScope = _loggerConfigScope
        , _loggerPolicy = _loggerConfigPolicy
        , _loggerMissed = missed
        }

-- | A backend worker.
--
-- The only way for this function to exist without an exception is when
-- the interal logger queue is closed through a call to 'releaseLogger'.
--
backendWorker
    ∷ (T.Text → IO ())
        -- ^ alternate sink for logging exceptions in the logger itself.
    → Maybe Int
        -- ^ number of consecutive backend exception that can occur before the logger
        -- to raises an 'BackendToManyExceptions' exception. If this is 'Nothing'
        -- the logger will discard all exceptions. For instance a value of @1@
        -- means that an exception is raised when the second exception occurs.
        -- A value of @0@ means that an exception is raised for each exception.
    → Maybe Int
        -- ^ number of microseconds to wait after an exception from the backend.
        -- If this is 'Nothing' the logger won't wait at all after an exception.
    → LoggerBackend a
    → LoggerQueue a
    → TVar Int
    → IO (Async ())
backendWorker errLogFun errLimit errWait backend queue missed = async $ go []
  where

    -- we assume that 'BlockedIndefinitelyOnSTM' and 'NestedAtomically' are the
    -- only exceptions beside asynchronous exceptions that can be thrown by
    -- @atomically readMsg@.
    --
    go errList = atomically readMsg ≫= \case

        -- When the queue is closed and empty the backendWorker returns.
        -- This is the only way for backendWorker to exist without an exception.
        Nothing → return ()

        -- call backend for the message and loop
        Just msg → runBackend errList msg ≫= go

    runBackend errList msg = (backend msg ≫ return []) `catchAny` \e → do

        -- try to log exception to backend
        let errMsg = backendErrorMsg (sshow e)
        backend (Left errMsg) `catchAny` \_ →
            -- log exception to alternate sink
            errLogFun (errLogMsg errMsg) `catchAny` \_ →
                -- discard exception log
                return ()

        -- decide how to proceed in case of an error
        case fromException e of
            Just (BackendTerminatedException _ ∷ LoggerException Void) → throwIO e
            _ → do
                maybe (return ()) threadDelay errWait
                let errList' = e:errList
                case errLimit of
                    Nothing → return []
                    Just n
                        | length errList' > n → throwIO $ BackendToManyExceptions (reverse errList')
                        | otherwise → return errList'

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

    -- A log message that informs about an error in the backend
    backendErrorMsg e = LogMessage
        { _logMsg = e
        , _logMsgLevel = Error
        , _logMsgScope = [("system", "logger"), ("component", "backend")]
        }

    -- format a log message that is written to the error sink
    errLogMsg LogMessage{..} = T.unwords
        [ "[" ⊕ logLevelText _logMsgLevel ⊕ "]"
        , formatScope _logMsgScope
        , _logMsg
        ]
    formatScope scope = "[" ⊕ T.intercalate "," (map formatLabel scope) ⊕ "]"
    formatLabel (k,v) = "(" ⊕ k ⊕ "," ⊕ v ⊕ ")"

releaseLogger
    ∷ MonadIO μ
    ⇒ Maybe Int
        -- ^ Timeout to wait for the logger to finish (microseconds)
    → Logger a
    → μ ()
releaseLogger t Logger{..} = liftIO $ do
    atomically $ closeTBMQueue _loggerQueue
    maybe id (\x → void ∘ timeout x) t $ wait _loggerWorker
    cancel _loggerWorker

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
-- On termination the main threads waits up to 3 seconds for the the logger
-- to flush the log queue and deliver all log messages. You can adjust
-- this value by using 'createLogger' and 'releaseLogger' directly:
--
-- > withLogger config backend =
-- >     bracket (createLogger config backend) (releaseLogger waitTimeout)
-- >  where
-- >    waitTimeout = Just 3000000
--
-- For detailed information about how backends are executed refer
-- to the documentation of 'createLogger'.
--
withLogger
    ∷ (MonadIO μ, MonadBaseControl IO μ)
    ⇒ LoggerConfig
    → LoggerBackend a
    → (Logger a → μ α)
    → μ α
withLogger = withLogger_ (T.hPutStrLn stderr)

-- | A version of 'withLogger' that takes as an extra argument
-- a function for logging errors in the logging system.
--
withLogger_
    ∷ (MonadIO μ, MonadBaseControl IO μ)
    ⇒ (T.Text → IO ())
        -- ^ alternate sink for logging exceptions in the logger itself.
    → LoggerConfig
    → LoggerBackend a
    → (Logger a → μ α)
    → μ α
withLogger_ errLogFun config backend =
    bracket (createLogger_ errLogFun config backend) (releaseLogger waitTimeout)
  where
    waitTimeout = Just 3000000

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


