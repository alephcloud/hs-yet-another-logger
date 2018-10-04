-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

-- |
-- Module: NoBackend
-- Copyright: Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
-- License: All Rights Reserved, see LICENSE file of the package
-- Maintainer: code@pivotmail.com
-- Stability: experimental
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module NoBackend
( tests
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Exception.Enclosed
import Control.Monad
import Control.Monad.Unicode
import Control.Lens

import Data.IORef
import Data.Monoid.Unicode
import Data.String
import qualified Data.Text as T
import Data.Typeable
import Data.Void

import Numeric.Natural

import GHC.Generics

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit hiding (testCaseSteps)

-- yet-another-logger
import System.Logger

-- internal
import TastyTools

-- -------------------------------------------------------------------------- --
-- TestParams

data TestParams = TestParams
    { queueSize ∷ !Natural
        -- ^ queue size
    , threadsN ∷ !Natural
        -- ^ number of threads
    , messageN ∷ !Natural
        -- ^ number of log message to write
    , messageSize ∷ !Natural
        -- ^ size of message
    , frontendDelay ∷ !Natural
        -- ^ delay between messages in microseconds
    , backendDelay ∷ !Natural
        -- ^ write delay in microseconds
    , exitDelay ∷ !(Maybe Natural)
        -- ^ exit timeout in microseconds
    }
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

-- -------------------------------------------------------------------------- --
-- Test Vectors

tests ∷ TestTree
tests = testGroup "trivial backend"
    [ noBackendTestsTimeout 1
    , noBackendTestsTimeout 100
    , noBackendTestsTimeout 100000000
    , buggyBackendTests 11 10
        [ TestParams 10 10 100 1000 25 1 (Just 10)
        , TestParams 1000 100 100 10000 25 1 (Just 10)
        , TestParams 10000000 100 100 10000 25 1 (Just 10)
        ]
    , buggyRecoverBackendTests 2 10
        [ TestParams 10 100 100 1000 100 1 (Just 10)
        , TestParams 1000 100 100 1000 10 1 (Just 10)
        , TestParams 10000000 100 100 1000 10 1 (Just 10)
        ]
    , buggyRecoverBackendTests 8 10
        [ TestParams 1000 10 100 100 100 1 (Just 10)
        ]
    , buggyNoRecoverBackendTests 10 15
        [ TestParams 1000 10 100 100 100 1 (Just 1000)
        -- we give these tests some more time at termination to ensure that
        -- the expected exceptions are actually thrown.
        ]
    ]
  where
    noBackendTestsTimeout t = noBackendTests
        [ TestParams 10 100 100 1000 25 1 (Just t)
        , TestParams 1000 100 100 10000 25 1 (Just t)
        , TestParams 10000000 100 100 10000 25 1 (Just t)

        , TestParams 10 100 100 1000 10 1 (Just t)
        , TestParams 1000 100 100 10000 10 1 (Just t)
        , TestParams 10000000 100 100 10000 10 1 (Just t)

        , TestParams 10 100 100 1000 1 5 (Just t)
        , TestParams 1000 100 100 10000 1 5 (Just t)
        , TestParams 10000000 100 100 10000 1 5 (Just t)
        ]

noBackendTests ∷ [TestParams] → TestTree
noBackendTests = testGroup "no backend" ∘ map tc
  where
    tc args = testCaseSteps (sshow args) $ \logLogStr →
        catchAny
            (noBackendLoggerTest logLogStr args)
            (\e → assertFailure $ "unexpected exception: " ⊕ show e)

-- Buggy Backend that calls 'BackendTerminatedException'.
--
buggyBackendTests ∷ Natural → Natural → [TestParams] → TestTree
buggyBackendTests m n =
    testGroup ("buggy backend " ⊕ sshow m ⊕ " " ⊕ sshow n) ∘ map tc
  where
    tc args = testCaseSteps (sshow args) $ \logLogStr →
        do
            buggyBackendLoggerTest exception (\x → x `mod` m <= n) logLogStr args
            assertFailure "Missing expected exception"
        `catch` \(e ∷ LoggerException Void) → case e of
            BackendTerminatedException e0 → case fromException e0 of
                Just BuggyBackendException → logLogStr $ "test: expected exception: " ⊕ sshow e
                _ → throwIO e
            _ → throwIO e
    exception = BackendTerminatedException $ toException BuggyBackendException

-- | Buggy Backend that calls some exception.
--
-- The logger is expected to recover.
--
buggyRecoverBackendTests ∷ Natural → Natural → [TestParams] → TestTree
buggyRecoverBackendTests m n =
    testGroup ("buggy recover backend " ⊕ sshow m ⊕ " " ⊕ sshow n) ∘ map tc
  where
    tc args = testCaseSteps (sshow args) $ \logLogStr →
        do
            buggyBackendLoggerTest exception (\x → x `mod` n <= m) logLogStr args
        `catchAny` \e →
            assertFailure $ "test: unexpected exception: " ⊕ show e
    exception = BuggyBackendException

-- | Buggy Backend that calls some exception.
--
-- The logger is expected to throw 'BackendTooManyExceptions'.
--
buggyNoRecoverBackendTests ∷ Natural → Natural → [TestParams] → TestTree
buggyNoRecoverBackendTests m n =
    testGroup ("buggy no recover backend " ⊕ sshow m ⊕ " " ⊕ sshow n) ∘ map tc
  where
    tc args = testCaseSteps (sshow args) $ \logLogStr → mask $ \umask → do
        do umask $ do
            buggyBackendLoggerTest exception (\x → x `mod` n <= m) logLogStr args
            -- Make sure to configure the exitWait and exceptionWait so that
            -- the backend has enough time to deliver enough messages to trigger
            -- and exception.
            assertFailure $ "Missing expected exception: " ⊕ sshow exception
        `catch` \(e ∷ LoggerException Void) → case e of
            BackendTooManyExceptions (e0:_) → case fromException e0 of
                Just BuggyBackendException → logLogStr $ "test: expected exception: " ⊕ sshow e
                _ → throwIO e
            _ → throwIO e
    exception = BuggyBackendException

-- -------------------------------------------------------------------------- --
-- Test Backend

-- | A thread that logs messages
--
testThread
    ∷ Natural
        -- ^ number of log message to write
    → Natural
        -- ^ size of message
    → Natural
        -- ^ delay between messages in microseconds
    → LogFunctionIO T.Text
    → IO ()
testThread n s delayMicro logFun = do
    void ∘ replicateM (fromIntegral n) $ do
        threadDelay (fromIntegral delayMicro)
        logFun Debug msg
  where
    msg = T.replicate (fromIntegral s) "a"

-- | A backend that logs all messages with level at least Warning
-- and discards all other messages.
--
-- Assuming that all test messages are of level lower than Warning
-- this will log only messages that are generated by the logging
-- framework itself.
--
testBackend
    ∷ Show msg
    ⇒ (T.Text → IO ())
    → Natural
        -- ^ minimal delay before returning in microseconds
    → LoggerBackend msg
testBackend _logLog delayMicro (Right LogMessage{..}) =
    -- simulate deliver by applying the delay
    threadDelay (fromIntegral delayMicro) ≫ return ()

testBackend logLog delayMicro (Left LogMessage{..}) = do
    -- assume that the message comes from the logging system itself.
    -- simulate delivery by applying the delay.
    logLog $ "[" ⊕ logLevelText _logMsgLevel ⊕ "] " ⊕ sshow _logMsg
    threadDelay (fromIntegral delayMicro)

-- -------------------------------------------------------------------------- --
-- No Backend Logger

noBackendLoggerTest
    ∷ (T.Text → IO ())
    → TestParams
    → IO ()
noBackendLoggerTest logLog TestParams{..} =
    nobackendLogger logLog config backendDelay $ \logFun →
        testClients logFun `catchAny` \e →
            logLog $ "unexpected exception in client: " ⊕ sshow e
  where
    testClients logFun = do
        s ← replicateM (fromIntegral threadsN) .
            async ∘ void $ testThread messageN messageSize frontendDelay logFun
        mapM_ wait s
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ Debug
        & loggerConfigQueueSize .~ queueSize
        & loggerConfigExitTimeout .~ exitDelay

-- | A logger with the testBackend.
--
nobackendLogger
    ∷ (T.Text → IO ())
    → LoggerConfig
    → Natural
        -- ^ write delay in microseconds
    → (LogFunctionIO T.Text → IO ())
    → IO ()
nobackendLogger logLog config delayMicro =
    withLogFunction_ logLog config (testBackend logLog delayMicro)

data BuggyBackendException = BuggyBackendException
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance Exception BuggyBackendException

-- -------------------------------------------------------------------------- --
-- Buggy backend

buggyBackendLoggerTest
    ∷ Exception e
    ⇒ e
    → (Natural → Bool)
        -- ^ exception predicate
    → (T.Text → IO ())
    → TestParams
    → IO ()
buggyBackendLoggerTest exception isException logLog TestParams{..} =
    buggyBackendLogger exception isException logLog config backendDelay $ \logFun →
        testClients logFun `catchAny` \e →
            logLog $ "unexpected exception in client: " ⊕ sshow e
  where
    testClients logFun = do
        s ← replicateM (fromIntegral threadsN) .
            async ∘ void $ testThread messageN messageSize frontendDelay logFun
        mapM_ wait s
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ Debug
        & loggerConfigQueueSize .~ queueSize
        & loggerConfigExitTimeout .~ exitDelay
        & loggerConfigExceptionLimit .~ Just 10
        & loggerConfigExceptionWait .~ Just 100

-- | A logger with the testBackend the throw exceptions.
--
buggyBackendLogger
    ∷ Exception e
    ⇒ e
    → (Natural → Bool)
        -- ^ exception predicate
    → (T.Text → IO ())
    → LoggerConfig
    → Natural
        -- ^ write delay in microseconds
    → (LogFunctionIO T.Text → IO ())
    → IO ()
buggyBackendLogger exception isException logLog config delayMicro f =
    withBackend $ \backend → withLogFunction_ logLog config backend f
  where
    withBackend inner = do
        counter ← newIORef 0
        result ← inner $ \msg → case msg of
            -- test log message
            Right{} → do
                modifyIORef' counter succ
                c ← readIORef counter
                if isException c
                    then throwIO exception
                    else testBackend logLog delayMicro msg
            -- internal log message (we don't count these)
            Left{} → testBackend logLog delayMicro msg
        n ← readIORef counter
        logLog $ "test: delivered " ⊕ sshow n ⊕ " log messages"
        return result

-- -------------------------------------------------------------------------- --
-- Utils

sshow ∷ (Show a, IsString b) ⇒ a → b
sshow = fromString ∘ show

