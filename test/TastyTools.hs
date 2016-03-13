-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

-- |
-- Module: TastyTools
-- Copyright: Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
-- License: All Rights Reserved, see LICENSE file of the package
-- Maintainer: code@pivotmail.com
-- Stability: experimental
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module TastyTools
( ProgressFunction
, TestCaseProgress
, testCaseProgress

, StepFunction
, testCaseSteps
) where

#ifndef MIN_VERSION_base
#define MIN_VESION_base(x,y,z) 1
#endif

import Configuration.Utils (boolReader)

#if ! MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception (try)
import Control.Monad
import Control.Monad.IO.Class

import Data.IORef.Lifted
import Data.Monoid.Unicode
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import Data.Typeable

import GHC.Generics

import Prelude.Unicode

import Test.Tasty
import Test.Tasty.HUnit hiding (testCaseSteps)
import Test.Tasty.Options
import Test.Tasty.Providers

newtype OptionVerbose = OptionVerbose Bool
    deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance IsOption OptionVerbose where
    defaultValue = OptionVerbose False
    parseValue s = OptionVerbose <$> either (\(_::T.Text) → Nothing) Just (boolReader s)
    optionName = Tagged "verbose"
    optionHelp = Tagged "verbosely log test progress messages to the console"

-- | Function to report progress
--
type ProgressFunction
    = ∀ m . MonadIO m
    ⇒ Float
        -- ^ progress measure
    → T.Text
        -- ^ progress message
    → m ()

newtype TestCaseProgress = TestCaseProgress (ProgressFunction → Assertion)
    deriving (Typeable)

instance IsTest TestCaseProgress where
    run opts (TestCaseProgress testAssertion) prog = do
        outRef ← newIORef ""
        try (testAssertion $ step outRef) >>= \case
            Left (HUnitFailure errMsg) → if verbose
              then do
                output ← readIORef outRef
                return ∘ testFailed ∘ LT.unpack ∘ LT.toLazyText $
                    output ⊕ "\n" ⊕ LT.fromString errMsg
              else
                return ∘ testFailed $ errMsg
            Right () → if verbose
                then
                    testPassed ∘ LT.unpack ∘ LT.toLazyText <$> readIORef outRef
                else
                    return $ testPassed ""
      where

        OptionVerbose verbose = lookupOption opts

        step ∷ MonadIO m ⇒ IORef LT.Builder → Float → T.Text → m ()
        step outRef p nm = liftIO $ do
            prog $ Progress (T.unpack nm) p
            when verbose $ atomicModifyIORef' outRef $ \l ->
                (l ⊕ "\n" ⊕ LT.fromText nm, ())

    testOptions = Tagged [Option (Proxy ∷ Proxy OptionVerbose)]

-- | Constructor for a 'TestTree' which can emit progress messages
--
testCaseProgress
    ∷ T.Text
        -- ^ Test name
    → (ProgressFunction → Assertion)
        -- ^ test method
    → TestTree
testCaseProgress testName test =
    singleTest (T.unpack testName) $ TestCaseProgress test

-- -------------------------------------------------------------------------- --
-- Step

-- | Function to report progress
--
type StepFunction
    = ∀ m . MonadIO m
    ⇒ T.Text
        -- ^ progress message
    → m ()

-- | Constructor for a 'TestTree' which can emit progress messages
--
testCaseSteps
    ∷ T.Text
        -- ^ Test name
    → (StepFunction → Assertion)
        -- ^ test method
    → TestTree
testCaseSteps testName inner = singleTest (T.unpack testName) $
    TestCaseProgress $ \f → inner (f 0)

