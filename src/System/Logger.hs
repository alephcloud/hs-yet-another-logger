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
-- Copyright: Copyright (c) 2014-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Logger
( withConsoleLogger

-- * Log-Level
, LogLevel(..)
, readLogLevel
, logLevelText
, pLogLevel

-- * Log-Scope
, LogLabel
, LogScope

-- * Logger Configuration
, QueuePolicy(..)
, queuePolicyText
, readQueuePolicy
, LoggerConfig(..)
, loggerConfigQueueSize
, loggerConfigBackend
, loggerConfigThreshold
, loggerConfigScope
, defaultLoggerConfig
, validateLoggerConfig
, pLoggerConfig

-- * Logger Handle Configuration

, LoggerHandleConfig(..)
, loggerHandleConfigText
, readLoggerHandleConfig
, validateLoggerHandleConfig
, pLoggerHandleConfig

-- * Logger Backend Configuration

, LoggerBackendConfig(..)
, loggerBackendConfigHandle
, loggerBackendConfigColor
, defaultLoggerBackendConfig
, validateLoggerBackendConfig
, pLoggerBackendConfig

-- * Logger

, LoggerBackend
, LogFunction
, LogFunctionIO

-- ** Logger Context
, LoggerCtx
, loggerScope
, loggerThreshold
, createLogger
, releaseLogger
, withLoggerCtx
, loggCtx
-- ** Running computations with modified logger context
, withLogLabel
, withLogLevel
, withLogQueuePolicy
-- ** Simple LogFunction on static logger context
, withLogFunction

-- * Handle Logger Backend Implementation

, withHandleLoggerBackend
, handleLoggerBackend

-- * MonadLog

, MonadLog(..)
, LoggerT
, runLoggerT

-- * Tools
-- , logErrorsG
) where

import Configuration.Utils hiding (Lens', Error)
import Configuration.Utils.Validation

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
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Monad.Trans.Trace

import qualified Data.CaseInsensitive as CI
import qualified Data.List as L
import Data.Monoid
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import Data.Text.Lens
import qualified Data.Text.IO as T
import Data.Typeable

import GHC.Generics

import qualified Options.Applicative as O

import Prelude.Unicode

import qualified System.Console.ANSI as A
import System.IO

-- internal modules

import System.Logger.ColorOption
import System.Logger.Utils

-- -------------------------------------------------------------------------- --
-- Convenient Logging to Console

-- | A simple console logger.
--
-- > import System.Logger
-- >
-- > main ∷ IO ()
-- > main = withConsoleLogger Info $ do
-- >     logg Info "moin"
-- >     withLabel ("function", "f") f
-- >     logg Info "tschüss"
-- >  where
-- >    f = withLevel Debug $ do
-- >        logg Debug "debug f"
--
withConsoleLogger
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ LogLevel
    → (LoggerT T.Text m α)
    → m α
withConsoleLogger level inner =
    withHandleLoggerBackend (config ^. loggerConfigBackend) $ \backend →
        withLoggerCtx config backend $ \loggerCtx →
            runLoggerT loggerCtx inner
  where
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ level

-- -------------------------------------------------------------------------- --
-- Log-Level

data LogLevel
    = Quiet
    | Error
    | Warn
    | Info
    | Debug
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

instance NFData LogLevel

readLogLevel
    ∷  (MonadError e m, Eq a, Show a, CI.FoldCase a, IsString a, IsString e, Monoid e)
    ⇒ a
    → m LogLevel
readLogLevel x = case CI.mk x of
    "quiet" → return Quiet
    "error" → return Error
    "warn" → return Warn
    "info" → return Info
    "debug" → return Debug
    e → throwError $ "unexpected log level value: "
        ⊕ fromString (show e)
        ⊕ ", expected \"quiet\", \"error\", \"warn\", \"info\", or \"debug\""

logLevelText
    ∷ IsString a
    ⇒ LogLevel
    → a
logLevelText Quiet = "quiet"
logLevelText Error = "error"
logLevelText Warn = "warn"
logLevelText Info = "info"
logLevelText Debug = "debug"

instance ToJSON LogLevel where
    toJSON = String ∘ logLevelText

instance FromJSON LogLevel where
    parseJSON = withText "LogLevel" $ either fail return ∘ readLogLevel

pLogLevel ∷ O.Parser LogLevel
pLogLevel = option (eitherReader readLogLevel)
    × long "loglevel"
    ⊕ metavar "quiet|error|warn|info|debug"
    ⊕ help "threshold for log messages"

-- -------------------------------------------------------------------------- --
-- Log-Label

type LogLabel = (T.Text, T.Text)
type LogScope = [LogLabel]

-- -------------------------------------------------------------------------- --
-- Logger Configuration

-- | Policy that determines how the case of a congested logging
-- pipeline is addressed.
--
data QueuePolicy
    = QueuePolicyDiscard
    | QueuePolicyRaise
    | QueuePolicyBlock
    deriving (Show, Read, Eq, Ord, Bounded, Enum, Typeable, Generic)

queuePolicyText ∷ IsString s ⇒ QueuePolicy → s
queuePolicyText QueuePolicyDiscard = "discard"
queuePolicyText QueuePolicyRaise = "raise"
queuePolicyText QueuePolicyBlock = "block"

readQueuePolicy
    ∷ (MonadError e m, Eq a, Show a, CI.FoldCase a, IsText a, IsString e, Monoid e)
    ⇒ a
    → m QueuePolicy
readQueuePolicy x = case CI.mk tx of
    "discard" → return QueuePolicyDiscard
    "raise" → return QueuePolicyRaise
    "block" → return QueuePolicyBlock
    e → throwError
        $ "invalid queue policy value " ⊕ fromString (show e) ⊕ ";"
        ⊕ " the queue policy value must be one of \"discard\", \"raise\", or \"block\""
  where
    tx = packed # x

instance ToJSON QueuePolicy where
    toJSON = toJSON ∘ (queuePolicyText ∷ QueuePolicy → T.Text)

instance FromJSON QueuePolicy where
    parseJSON = withText "QueuePolicy" $ either fail return ∘ readQueuePolicy

-- | Logger Configuration
--
data LoggerConfig = LoggerConfig
    { _loggerConfigQueueSize ∷ !Int
    , _loggerConfigBackend ∷ !LoggerBackendConfig
    , _loggerConfigThreshold ∷ !LogLevel
        -- ^ initial log threshold, can be changed later on
    , _loggerConfigScope ∷ !LogScope
        -- ^ initial stack of log labels, can be extended later on
    , _loggerConfigQueuePolicy ∷ !QueuePolicy
        -- ^ how to deal with a congested logging pipeline
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

loggerConfigQueueSize ∷ Lens' LoggerConfig Int
loggerConfigQueueSize = lens _loggerConfigQueueSize $ \a b → a { _loggerConfigQueueSize = b }

loggerConfigBackend ∷ Lens' LoggerConfig LoggerBackendConfig
loggerConfigBackend = lens _loggerConfigBackend $ \a b → a { _loggerConfigBackend = b }

loggerConfigThreshold ∷ Lens' LoggerConfig LogLevel
loggerConfigThreshold = lens _loggerConfigThreshold $ \a b → a { _loggerConfigThreshold = b }

loggerConfigScope ∷ Lens' LoggerConfig LogScope
loggerConfigScope = lens _loggerConfigScope $ \a b → a { _loggerConfigScope = b }

loggerConfigQueuePolicy ∷ Lens' LoggerConfig QueuePolicy
loggerConfigQueuePolicy = lens _loggerConfigQueuePolicy $ \a b → a { _loggerConfigQueuePolicy = b }

instance NFData LoggerConfig

defaultLoggerConfig ∷ LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _loggerConfigQueueSize = 1000
    , _loggerConfigBackend = defaultLoggerBackendConfig
    , _loggerConfigThreshold = Warn
    , _loggerConfigScope = []
    , _loggerConfigQueuePolicy = QueuePolicyDiscard
    }

validateLoggerConfig ∷ ConfigValidation LoggerConfig λ
validateLoggerConfig _ = return ()

instance ToJSON LoggerConfig where
    toJSON LoggerConfig{..} = object
        [ "queue_size" .= _loggerConfigQueueSize
        , "logger_backend" .= _loggerConfigBackend
        , "log_level" .= _loggerConfigThreshold
        , "scope" .= _loggerConfigScope
        , "queue_policy" .= _loggerConfigQueuePolicy
        ]

instance FromJSON (LoggerConfig → LoggerConfig) where
    parseJSON = withObject "LoggerConfig" $ \o → id
        <$< loggerConfigQueueSize ..: "queue_size" × o
        <*< loggerConfigBackend %.: "logger_backend" × o
        <*< loggerConfigThreshold ..: "log_level" × o
        <*< loggerConfigScope ..: "scope" × o
        <*< loggerConfigQueuePolicy ..: "queue_policy" × o

pLoggerConfig ∷ MParser LoggerConfig
pLoggerConfig = id
    <$< loggerConfigQueueSize .:: option auto
        × long "queue-size"
        ⊕ metavar "INT"
        ⊕ help "size of the internal logger queue"
    <*< loggerConfigBackend %:: pLoggerBackendConfig
    <*< loggerConfigThreshold .:: pLogLevel
    <*< loggerConfigQueuePolicy .:: option (eitherReader $ readQueuePolicy ∘ T.pack)
        × long "discard"
        ⊕ metavar "block|raise|discard"
        ⊕ help "how to deal with a congested logging pipeline"

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
pLoggerHandleConfig = option (eitherReader readLoggerHandleConfig)
    × long "logger-backend-handle"
    ⊕ metavar "stdout|stderr|file:<FILENAME>"
    ⊕ help "handle where the logs are written"

-- -------------------------------------------------------------------------- --
-- Logger Backend Configuration

-- | LoggerBackendConfig
--
data LoggerBackendConfig = LoggerBackendConfig
    { _loggerBackendConfigColor ∷ !ColorOption
    , _loggerBackendConfigHandle ∷ !LoggerHandleConfig
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

loggerBackendConfigColor ∷ Lens' LoggerBackendConfig ColorOption
loggerBackendConfigColor = lens _loggerBackendConfigColor $ \a b → a { _loggerBackendConfigColor = b }

loggerBackendConfigHandle ∷ Lens' LoggerBackendConfig LoggerHandleConfig
loggerBackendConfigHandle = lens _loggerBackendConfigHandle $ \a b → a { _loggerBackendConfigHandle = b }

instance NFData LoggerBackendConfig

defaultLoggerBackendConfig ∷ LoggerBackendConfig
defaultLoggerBackendConfig = LoggerBackendConfig
    { _loggerBackendConfigColor = defaultColorOption
    , _loggerBackendConfigHandle = StdOut
    }

-- TODO: warn when handle is a file and color is on
--
validateLoggerBackendConfig ∷ ConfigValidation LoggerBackendConfig λ
validateLoggerBackendConfig LoggerBackendConfig{..} =
        validateLoggerHandleConfig _loggerBackendConfigHandle

instance ToJSON LoggerBackendConfig where
    toJSON LoggerBackendConfig{..} = object
        [ "color" .= _loggerBackendConfigColor
        , "handle" .= _loggerBackendConfigHandle
        ]

instance FromJSON (LoggerBackendConfig → LoggerBackendConfig) where
    parseJSON = withObject "LoggerBackendConfig" $ \o → id
        <$< loggerBackendConfigColor ..: "color" × o
        <*< loggerBackendConfigHandle ..: "handle" × o

pLoggerBackendConfig ∷ MParser LoggerBackendConfig
pLoggerBackendConfig = id
    <$< loggerBackendConfigColor .:: pColorOption
    <*< loggerBackendConfigHandle .:: pLoggerHandleConfig

-- -------------------------------------------------------------------------- --
-- Internal Logger Message

-- | The type parameter 'a' is expected to provide intances
-- of 'Show', 'Typeable', and 'NFData'.
--
-- If we need to support different backends, we may consider
-- including the backend here...
--
data LogMessage a = LogMessage
    { _logMsg ∷ !a
    , _logMsgLevel ∷ !LogLevel
    , _logMsgScope ∷ !LogScope
        -- ^ efficiency of this depends on whether this is shared
        -- between log messsages. Usually this should be just a pointer to
        -- a shared list.
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

logMsg ∷ Lens' (LogMessage a) a
logMsg = lens _logMsg $ \a b → a { _logMsg = b }

logMsgLevel ∷ Lens' (LogMessage a) LogLevel
logMsgLevel = lens _logMsgLevel $ \a b → a { _logMsgLevel = b }

logMsgScope ∷ Lens' (LogMessage a) LogScope
logMsgScope = lens _logMsgScope $ \a b → a { _logMsgScope = b }

instance NFData a ⇒ NFData (LogMessage a)

-- -------------------------------------------------------------------------- --
-- Logger
--
-- A logger encapsulates a queue and a background worker that dequeues
-- log-messages and delivers them to a backend action. The opaque logger
-- context is thread safe. But it contains references to mutable state and
-- no copy or derivation of it must be used out-side of it's allocation scope.
--

-- | This is given to logger when it is created. It formats and delivers
-- individual log messages synchronously.
--
-- The type parameter 'a' is expected to provide instances for 'Show'
-- 'Typeable', and 'NFData'.
--
-- The 'Left' values of the argument allows the generation of log messages
-- that are independent of the parameter 'a'. The motivation for this is
-- reporting issues in Logging system itself, like a full logger queue
-- or providing statistics about the fill level of the queue. There may
-- be other uses of this, too.
--
-- TODO there may be scenarios where chunked processing is beneficial.
-- While this can be done in a closure of this function a more direct
-- support might be desirable.
--
type LoggerBackend a = Either (LogMessage T.Text) (LogMessage a) → IO ()

-- | This function is provided by the logger.
--
type LogFunctionIO a = LogLevel → a → IO ()
type LogFunction a m = LogLevel → a → m ()

-- internals

-- | Interal log message queue.
--
-- The backend function formats and delivers log messages synchronously. In
-- order to not slow down the processing of the main program logic log messages
-- are enqueued and processed asynchronously by a background worker that takes
-- the message from queue and calls the backend function for each log message.
--
type LoggerQueue a = TBMQueue (LogMessage a)

data LoggerCtx a = LoggerCtx
    { _loggerQueue ∷ !(LoggerQueue a)
    , _loggerWorker ∷ !(Async ())
    , _loggerThreshold ∷ !LogLevel
    , _loggerScope ∷ !LogScope
    , _loggerQueuePolicy ∷ !QueuePolicy
    , _loggerMissed ∷ !(TVar Int)
    }
    deriving (Typeable, Generic)

loggerQueue ∷ Lens' (LoggerCtx a) (LoggerQueue a)
loggerQueue = lens _loggerQueue $ \a b → a { _loggerQueue = b }
{-# INLINE loggerQueue #-}

loggerWorker ∷ Lens' (LoggerCtx a) (Async ())
loggerWorker = lens _loggerWorker $ \a b → a { _loggerWorker = b }
{-# INLINE loggerWorker #-}

loggerThreshold ∷ Lens' (LoggerCtx a) LogLevel
loggerThreshold = lens _loggerThreshold $ \a b → a { _loggerThreshold = b }
{-# INLINE loggerThreshold #-}

loggerScope ∷ Lens' (LoggerCtx a) LogScope
loggerScope = lens _loggerScope $ \a b → a { _loggerScope = b }
{-# INLINE loggerScope #-}

loggerQueuePolicy ∷ Lens' (LoggerCtx a) QueuePolicy
loggerQueuePolicy = lens _loggerQueuePolicy $ \a b → a { _loggerQueuePolicy = b }
{-# INLINE loggerQueuePolicy #-}

loggerMissed ∷ Lens' (LoggerCtx a) (TVar Int)
loggerMissed = lens _loggerMissed $ \a b → a { _loggerMissed = b }
{-# INLINE loggerMissed #-}

createLogger
    ∷ MonadIO μ
    ⇒ LoggerConfig
    → LoggerBackend a
    → μ (LoggerCtx a)
createLogger LoggerConfig{..} backend = liftIO $ do
    queue ← newTBMQueueIO _loggerConfigQueueSize
    missed ← newTVarIO 0
    worker ← backendWorker backend queue missed
    return $ LoggerCtx
        { _loggerQueue = queue
        , _loggerWorker = worker
        , _loggerThreshold = _loggerConfigThreshold
        , _loggerScope = _loggerConfigScope
        , _loggerQueuePolicy = _loggerConfigQueuePolicy
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
    go = atomically readMsg >>= \case
        -- when the queue is closed and empty the backendWorker returns
        Nothing → return ()
        -- When there are still messages to process the backendWorker loops
        Just msg → backend msg >> go

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
    ⇒ LoggerCtx a
    → μ ()
releaseLogger LoggerCtx{..} = liftIO $ do
    atomically $ closeTBMQueue _loggerQueue
    wait _loggerWorker

-- | Provide a computation with a 'LoggerContext'.
--
-- Here is an example how this can be used to run a computation
-- with a 'MonadLog' constraint:
--
-- > withConsoleLogger
-- >     ∷ (MonadIO m, MonadBaseControl IO m)
-- >     ⇒ LogLevel
-- >     → (LoggerT T.Text m α)
-- >     → m α
-- > withConsoleLogger level = do
-- >    backend ← mkHandleLoggerBackend $ config ^. loggerConfigBackend
-- >    withLoggerCtx config backend ∘ flip runLoggerT
-- >  where
-- >    config = defaultLoggerConfig
-- >        & loggerConfigThreshold .~ level
--
withLoggerCtx
    ∷ (MonadIO μ, MonadBaseControl IO μ)
    ⇒ LoggerConfig
    → LoggerBackend a
    → (LoggerCtx a → μ α)
    → μ α
withLoggerCtx config backend =
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
withLogFunction config backend f = withLoggerCtx config backend $ f ∘ loggCtx

-- -------------------------------------------------------------------------- --
-- Running computations with modified logger context

withLogLabel
    ∷ LogLabel
    → LoggerCtx a
    → (LoggerCtx a → α)
    → α
withLogLabel label ctx f = f $ loggerScope %~ (:) label $ ctx

withLogLevel
    ∷ LogLevel
    → LoggerCtx a
    → (LoggerCtx a → α)
    → α
withLogLevel level ctx f = f $ loggerThreshold .~ level $ ctx

withLogQueuePolicy
    ∷ QueuePolicy
    → LoggerCtx a
    → (LoggerCtx a → α)
    → α
withLogQueuePolicy policy ctx f = f $ loggerQueuePolicy .~ policy $ ctx

-- -------------------------------------------------------------------------- --
-- Logger Backend Implementation

withHandleLoggerBackend
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ LoggerBackendConfig
    → (LoggerBackend T.Text → m α)
    → m α
withHandleLoggerBackend conf inner =
    case conf ^. loggerBackendConfigHandle of
        StdErr → run stderr
        StdOut → run stdout
        FileHandle f → liftBaseOp (withFile f AppendMode) run
  where
    run h = do
        colored ← liftIO $ useColor (conf ^. loggerBackendConfigColor) h
        inner $ handleLoggerBackend h colored

handleLoggerBackend
    ∷ Handle
    → Bool
        -- ^ whether to use ANSI color escape codes
    → LoggerBackend T.Text
handleLoggerBackend h colored eitherMsg = do
    T.hPutStrLn h
        $ inLevelColor colored ("[" ⊕ sshow level ⊕ "] ")
        ⊕ inScopeColor colored ("[" ⊕ formatedScope ⊕ "] ")
        ⊕ (msg ^. logMsg)
  where
    msg = either id id eitherMsg
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
{-# INLINEABLE handleLoggerBackend #-}

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
    ⇒ LoggerCtx a
    → LogFunctionIO a
loggCtx LoggerCtx{..} level msg = do
    case _loggerThreshold of
        Quiet → return ()
        threshold
            | level ≤ threshold → liftIO ∘ atomically $
                writeWithQueuePolicy $!! LogMessage
                    { _logMsg = msg
                    , _logMsgLevel = level
                    , _logMsgScope = _loggerScope
                    }
            | otherwise → return ()
  where
    writeWithQueuePolicy lmsg
        | _loggerQueuePolicy ≡ QueuePolicyBlock = writeTBMQueue _loggerQueue lmsg
        | otherwise = tryWriteTBMQueue _loggerQueue lmsg >>= \case
            Just False
                | _loggerQueuePolicy ≡ QueuePolicyDiscard → modifyTVar' _loggerMissed succ
                | _loggerQueuePolicy ≡ QueuePolicyRaise → throwSTM $ QueueFullException lmsg

            _ → return ()
{-# INLINEABLE loggCtx #-}

-- -------------------------------------------------------------------------- --
-- MonadLog

class Monad m ⇒ MonadLog a m | m → a where
    logg ∷ LogFunction a m
    withLevel ∷ LogLevel → m α → m α
    withLabel ∷ LogLabel → m α → m α

instance (Show a, Typeable a, NFData a, MonadIO m, MonadReader (LoggerCtx a) m) ⇒ MonadLog a m where
    logg l m = ask >>= \ctx → liftIO (loggCtx ctx l m)
    withLevel level = local $ loggerThreshold .~ level
    withLabel label = local $ loggerScope %~ (:) label

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

instance (Show a, Typeable a, NFData a, MonadIO m) ⇒ MonadLog a (ReaderT (LoggerCtx a) m) where
    logg l m = ask >>= \ctx → liftIO (loggCtx ctx l m)
    withLevel level = local $ loggerThreshold .~ level
    withLabel label = local $ loggerScope %~ (:) label

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

instance MonadLog a m ⇒ MonadLog a (ReaderT σ m) where
    logg l = lift ∘ logg l
    withLevel level inner = liftWith (\run → withLevel level (run inner)) >>= restoreT ∘ return
    withLabel label inner = liftWith (\run → withLabel label (run inner)) >>= restoreT ∘ return

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

instance (MonadLog a m) ⇒ MonadLog a (ExceptT ε m) where
    logg l = lift ∘ logg l
    withLevel level inner = liftWith (\run → withLevel level (run inner)) >>= restoreT ∘ return
    withLabel label inner = liftWith (\run → withLabel label (run inner)) >>= restoreT ∘ return

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

instance (MonadLog a m) ⇒ MonadLog a (StateT σ m) where
    logg l = lift ∘ logg l
    withLevel level inner = liftWith (\run → withLevel level (run inner)) >>= restoreT ∘ return
    withLabel label inner = liftWith (\run → withLabel label (run inner)) >>= restoreT ∘ return

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

instance (MonadLog a m) ⇒ MonadLog a (TraceT t e m) where
    logg l = lift ∘ logg l
    withLevel level inner = liftWith (\run → withLevel level (run inner)) >>= restoreT ∘ return
    withLabel label inner = liftWith (\run → withLabel label (run inner)) >>= restoreT ∘ return

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

instance (MonadLog a m) ⇒ MonadLog a (EitherT σ m) where
    logg l = lift ∘ logg l
    withLevel level inner = liftWith (\run → withLevel level (run inner)) >>= restoreT ∘ return
    withLabel label inner = liftWith (\run → withLabel label (run inner)) >>= restoreT ∘ return

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

{-
-- Uses @OverlappingInstances@ to lift MonadLog in all transformers with an
-- instance for 'MonadTransControl'.
--
instance (MonadLog a m, MonadTransControl t, Monad n, n ~ (t m)) ⇒ MonadLog a n where
    logg l = lift ∘ logg l
    withLevel level inner = liftWith (\run → withLevel level (run inner)) >>= restoreT ∘ return
    withLabel label inner = liftWith (\run → withLabel label (run inner)) >>= restoreT ∘ return

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}
-}

-- -------------------------------------------------------------------------- --
-- LoggerT

type LoggerT a = ReaderT (LoggerCtx a)

runLoggerT ∷ LoggerCtx a → LoggerT a m α → m α
runLoggerT = flip runReaderT
{-# INLINE runLoggerT #-}

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

