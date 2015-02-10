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
-- ** Simple LogFunction on static logger context
, withLogFunction

-- * Handle Logger Backend Implementation

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
import Control.Concurrent.STM.TBQueue
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

-- internal modules

import System.Logger.ColorOption
import System.Logger.Utils

-- -------------------------------------------------------------------------- --
-- Convenient Logging to Console

-- | A simple console logger.
--
-- This function is a an example for a very basic usage of this library:
--
-- > withConsoleLogger
-- >     ∷ (MonadIO m, MonadBaseControl IO m)
-- >     ⇒ LogLevel
-- >     → (LoggerT T.Text m α)
-- >     → m α
-- > withConsoleLogger level = withLoggerCtx config backend ∘ flip runLoggerT
-- >   where
-- >     config = defaultLoggerConfig
-- >         & loggerConfigThreshold .~ level
-- >     backend = handleLoggerBackend $ config ^. loggerConfigBackend
--
withConsoleLogger
    ∷ (MonadIO m, MonadBaseControl IO m)
    ⇒ LogLevel
    → (LoggerT T.Text m α)
    → m α
withConsoleLogger level = withLoggerCtx config backend ∘ flip runLoggerT
  where
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ level
    backend = handleLoggerBackend $ config ^. loggerConfigBackend

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

-- | Logger Configuration
--
data LoggerConfig = LoggerConfig
    { _loggerConfigQueueSize ∷ !Int
    , _loggerConfigBackend ∷ !LoggerBackendConfig
    , _loggerConfigThreshold ∷ !LogLevel
        -- ^ initial log threshold, can be changed later on
    , _loggerConfigScope ∷ !LogScope
        -- ^ initial stack of log labels, can be extended later on
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

instance NFData LoggerConfig

defaultLoggerConfig ∷ LoggerConfig
defaultLoggerConfig = LoggerConfig
    { _loggerConfigQueueSize = 1000
    , _loggerConfigBackend = defaultLoggerBackendConfig
    , _loggerConfigThreshold = Warn
    , _loggerConfigScope = []
    }

validateLoggerConfig ∷ ConfigValidation LoggerConfig λ
validateLoggerConfig _ = return ()

instance ToJSON LoggerConfig where
    toJSON LoggerConfig{..} = object
        [ "queue_size" .= _loggerConfigQueueSize
        , "logger_backend" .= _loggerConfigBackend
        , "log_level" .= _loggerConfigThreshold
        , "scope" .= _loggerConfigScope
        ]

instance FromJSON (LoggerConfig → LoggerConfig) where
    parseJSON = withObject "LoggerConfig" $ \o → id
        <$< loggerConfigQueueSize ..: "queue_size" × o
        <*< loggerConfigBackend %.: "logger_backend" × o
        <*< loggerConfigThreshold ..: "log_level" × o
        <*< loggerConfigScope ..: "scope" × o

pLoggerConfig ∷ MParser LoggerConfig
pLoggerConfig = id
    <$< loggerConfigQueueSize .:: option auto
        × long "queue-size"
        ⊕ metavar "INT"
        ⊕ help "size of the internal logger queue"
    <*< loggerConfigBackend %:: pLoggerBackendConfig
    <*< loggerConfigThreshold .:: pLogLevel

-- -------------------------------------------------------------------------- --
-- Handle Logger Backend Configuration

data LoggerHandleConfig
    = StdOut
    | StdErr
    | FileHandle FilePath
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance NFData LoggerHandleConfig

readLoggerHandleConfig
    ∷  (MonadError e m, Eq a, Show a, CI.FoldCase a, IsText a, IsString e, Monoid e)
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

-- | If we need to support different backends, we may consider
-- including the backend here...
--
data LogMessage a = LogMessage
    { _logMsg ∷ !a
    , _logMsgLevel ∷ !LogLevel
    , _logMsgScope ∷ !LogScope
        -- ^ efficiency of this depends on wether this is shared
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
-- TODO there may be scenarios where chunked processing is beneficial.
-- While this can be done in a closure of this function a more direct
-- support might be desirable.
--
type LoggerBackend a = LogMessage a → IO ()

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
type LoggerQueue a = TBQueue (LogMessage a)

data LoggerCtx a = LoggerCtx
    { _loggerQueue ∷ !(LoggerQueue a)
    , _loggerWorker ∷ !(Async ())
    , _loggerThreshold ∷ !LogLevel
    , _loggerScope ∷ !LogScope
    }
    deriving (Eq, Typeable, Generic)

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

createLogger
    ∷ MonadIO μ
    ⇒ LoggerConfig
    → LoggerBackend a
    → μ (LoggerCtx a)
createLogger LoggerConfig{..} backend = liftIO $ do
    queue ← newTBQueueIO _loggerConfigQueueSize
    worker ← backendWorker backend queue
    return $ LoggerCtx
        { _loggerQueue = queue
        , _loggerWorker = worker
        , _loggerThreshold = _loggerConfigThreshold
        , _loggerScope = _loggerConfigScope
        }

-- FIXME: make this more reliable
backendWorker
    ∷ LoggerBackend a
    → LoggerQueue a
    → IO (Async ())
backendWorker backend queue = async ∘ forever ∘ flip catchAny (const $ return ()) $
    atomically (readTBQueue queue) >>= backend

releaseLogger
    ∷ MonadIO μ
    ⇒ LoggerCtx a
    → μ ()
releaseLogger LoggerCtx{..} = liftIO $ cancel _loggerWorker

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
    ∷ (NFData a, MonadIO μ, MonadBaseControl IO μ)
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

-- -------------------------------------------------------------------------- --
-- Logger Backend Implementation

handleLoggerBackend
    ∷ LoggerBackendConfig
    → LoggerBackend T.Text
handleLoggerBackend conf msg = do
    -- FIXME FIXME FIXME: don't do this for each message!
    colored ← useColor $ conf ^. loggerBackendConfigColor
    T.putStrLn
        $ inLevelColor colored ("[" ⊕ sshow level ⊕ "] ")
        ⊕ inScopeColor colored ("[" ⊕ formatedScope ⊕ "] ")
        ⊕ (msg ^. logMsg)
  where
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

-- Log a message with the given logger context
--
-- If the logger context has been released (by closing the queue)
-- this function has not effect.
--
loggCtx
    ∷ NFData a
    ⇒ LoggerCtx a
    → LogFunctionIO a
loggCtx LoggerCtx{..} level msg = do
    case _loggerThreshold of
        Quiet → return ()
        threshold
            | level ≤ threshold → liftIO ∘ atomically $
                writeTBQueue _loggerQueue $!! LogMessage
                    { _logMsg = msg
                    , _logMsgLevel = level
                    , _logMsgScope = _loggerScope
                    }
            | otherwise → return ()
{-# INLINEABLE loggCtx #-}

-- -------------------------------------------------------------------------- --
-- MonadLog

class Monad m ⇒ MonadLog a m | m → a where
    logg ∷ LogFunction a m
    withLevel ∷ LogLevel → m α → m α
    withLabel ∷ LogLabel → m α → m α

instance (NFData a, MonadIO m, MonadReader (LoggerCtx a) m) ⇒ MonadLog a m where
    logg l m = ask >>= \ctx → liftIO (loggCtx ctx l m)
    withLevel level = local $ loggerThreshold .~ level
    withLabel label = local $ loggerScope %~ (:) label

    {-# INLINE logg #-}
    {-# INLINE withLevel #-}
    {-# INLINE withLabel #-}

instance (NFData a, MonadIO m) ⇒ MonadLog a (ReaderT (LoggerCtx a) m) where
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

