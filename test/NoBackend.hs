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

import GHC.Generics

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit

-- pc-directory
import System.Logger

-- -------------------------------------------------------------------------- --
-- TestParams

data TestParams = TestParams
    { queueSize ∷ !Int
        -- ^ queue size
    , threadsN ∷ !Int
        -- ^ number of threads
    , messageN ∷ !Int
        -- ^ number of log message to write
    , messageSize ∷ !Int
        -- ^ size of message
    , frontendDelay ∷ !Int
        -- ^ delay between messages in microseconds
    , backendDelay ∷ !Int
        -- ^ write delay in microseconds
    , exitDelay ∷ !(Maybe Int)
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
        [ TestParams 1000 10 100 100 100 1 (Just 10)
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
    tc args = testCaseSteps (show args) $ \logLogStr →
        catchAny
            (noBackendLoggerTest (logLogStr ∘ T.unpack) args)
            (\e → assertString $ "unexpected exception: " ⊕ show e)

-- Buggy Backend that calls 'BackendTerminatedException'.
--
buggyBackendTests ∷ Int → Int → [TestParams] → TestTree
buggyBackendTests m n =
    testGroup ("buggy backend " ⊕ sshow m ⊕ " " ⊕ sshow n) ∘ map tc
  where
    tc args = testCaseSteps (show args) $ \logLogStr →
        do
            buggyBackendLoggerTest exception (\x → x `mod` m <= n) (logLogStr ∘ T.unpack) args
            assertString $ "Missing expected exception"
        `catch` \(e ∷ LoggerException Void) → case e of
            BackendTerminatedException e0 → case fromException e0 of
                Just BuggyBackendException → logLogStr $ "test: expected exception: " ⊕ show e
                _ → throwIO e
            _ → throwIO e
    exception = BackendTerminatedException $ toException BuggyBackendException

-- | Buggy Backend that calls some exception.
--
-- The logger is expected to recover.
--
buggyRecoverBackendTests ∷ Int → Int → [TestParams] → TestTree
buggyRecoverBackendTests m n =
    testGroup ("buggy recover backend " ⊕ sshow m ⊕ " " ⊕ sshow n) ∘ map tc
  where
    tc args = testCaseSteps (show args) $ \logLogStr →
        do
            buggyBackendLoggerTest exception (\x → x `mod` n <= m) (logLogStr ∘ T.unpack) args
        `catchAny` \e →
            assertString $ "test: unexpected exception: " ⊕ show e
    exception = BuggyBackendException

-- | Buggy Backend that calls some exception.
--
-- The logger is expected to throw 'BackendToManyExceptions'.
--
buggyNoRecoverBackendTests ∷ Int → Int → [TestParams] → TestTree
buggyNoRecoverBackendTests m n =
    testGroup ("buggy no recover backend " ⊕ sshow m ⊕ " " ⊕ sshow n) ∘ map tc
  where
    tc args = testCaseSteps (show args) $ \logLogStr →
        do
            buggyBackendLoggerTest exception (\x → x `mod` n <= m) (logLogStr ∘ T.unpack) args
            assertString $ "Missing expected exception: " ⊕ sshow exception
        `catch` \(e ∷ LoggerException Void) → case e of
            BackendToManyExceptions (e0:_) → case fromException e0 of
                Just BuggyBackendException → logLogStr $ "test: expected exception: " ⊕ sshow e
                _ → throwIO e
            _ → throwIO e
    exception = BuggyBackendException

-- -------------------------------------------------------------------------- --
-- Test Backend

-- | A thread that logs messages
--
testThread
    ∷ Int
        -- ^ number of log message to write
    → Int
        -- ^ size of message
    → Int
        -- ^ delay between messages in microseconds
    → LogFunctionIO T.Text
    → IO ()
testThread n s delayMicro logFun = do
    void ∘ replicateM n $ do
        threadDelay delayMicro
        logFun Debug msg
  where
    msg = T.replicate s "a"

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
    → Int
        -- ^ minimal delay before returning in microseconds
    → LoggerBackend msg
testBackend _logLog delayMicro (Right LogMessage{..}) =
    -- simulate deliver by applying the delay
    threadDelay delayMicro ≫ return ()

testBackend logLog delayMicro (Left LogMessage{..}) = do
    -- assume that the message comes from the logging system itself.
    -- simulate delivery by applying the delay.
    logLog $ "[" ⊕ logLevelText _logMsgLevel ⊕ "] " ⊕ sshow _logMsg
    threadDelay delayMicro

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
        s ← replicateM threadsN .
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
    → Int
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
    → (Int → Bool)
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
        s ← replicateM threadsN .
            async ∘ void $ testThread messageN messageSize frontendDelay logFun
        mapM_ wait s
    config = defaultLoggerConfig
        & loggerConfigThreshold .~ Debug
        & loggerConfigQueueSize .~ queueSize
        & loggerConfigExitTimeout .~ exitDelay

-- | A logger with the testBackend the throw exceptions.
--
buggyBackendLogger
    ∷ Exception e
    ⇒ e
    → (Int → Bool)
        -- ^ exception predicate
    → (T.Text → IO ())
    → LoggerConfig
    → Int
        -- ^ write delay in microseconds
    → (LogFunctionIO T.Text → IO ())
    → IO ()
buggyBackendLogger exception isException logLog config delayMicro f =
    withBackend $ \backend → withLogFunction_ logLog config backend f
  where
    withBackend inner = do
        counter ← newIORef (0 ∷ Int)
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

