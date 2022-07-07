-- Copyright (c) 2016-2018 Lars Kuhtz <lakuhtz@gmail.com>
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
-- Module: System.Logger.Internal.Queue
-- Description: Queues for Usage with Yet Another Logger
-- Copyright:
--     Copyright © 2016-2022 Lars Kuhtz <lakuhtz@gmail.com>
--     Copyright © 2015 PivotCloud, Inc.
-- License: Apache-2.0
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Logger.Internal.Queue
( BoundedCloseableQueue(..)
, FairTBMQueue
, TBMQueue
, TBMChan
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.STM.TBMQueue
import Control.Monad.Unicode

import Numeric.Natural

import Prelude.Unicode


-- -------------------------------------------------------------------------- --
-- Queue Abstraction

class BoundedCloseableQueue q a | q → a where
    newQueue ∷ Natural → IO q
    closeQueue ∷ q → IO ()

    -- | Returns 'False' if and only if the queue
    -- is closed. If the queue is full this function blocks.
    --
    writeQueue ∷ q → a → IO Bool

    -- | Non-blocking version of 'writeQueue'. Returns 'Nothing' if the
    -- queue was full. Otherwise it returns 'Just True' if the value
    -- was successfully written and 'Just False' if the queue was closed.
    --
    tryWriteQueue ∷ q → a → IO (Maybe Bool)

    -- | Returns 'Nothing' if and only if the queue is
    -- closed. If this queue is empty this function blocks.
    --
    readQueue ∷ q → IO (Maybe a)

    {-
    -- | Non-blocking version of 'readQueue'. Returns 'Nothing' if the
    -- queue is empty. Returns 'Just Nothing' if the queue is closed
    -- and and 'Just (Just a)' otherwise.
    --
    tryReadQueue ∷ q → IO (Maybe (Maybe a))
    -}

-- -------------------------------------------------------------------------- --
-- TBMQueue

instance BoundedCloseableQueue (TBMQueue a) a where
    newQueue = newTBMQueueIO ∘ fromIntegral
    closeQueue = atomically ∘ closeTBMQueue
    writeQueue q a = atomically $ isClosedTBMQueue q ≫= \case
        True → return False
        False → do
            writeTBMQueue q a
            return True
    tryWriteQueue q a = atomically $ tryWriteTBMQueue q a ≫= \case
        Nothing → return $ Just False
        Just False → return Nothing
        Just True → return $ Just True
    readQueue q = atomically $ readTBMQueue q

-- -------------------------------------------------------------------------- --
-- TBMChan

instance BoundedCloseableQueue (TBMChan a) a where
    newQueue = newTBMChanIO ∘ fromIntegral
    closeQueue = atomically ∘ closeTBMChan
    writeQueue q a = atomically $ isClosedTBMChan q ≫= \case
        True → return False
        False → do
            writeTBMChan q a
            return True
    tryWriteQueue q a = atomically $ tryWriteTBMChan q a ≫= \case
        Nothing → return $ Just False
        Just False → return Nothing
        Just True → return $ Just True
    readQueue q = atomically $ readTBMChan q

-- -------------------------------------------------------------------------- --
-- FairTBMQueue

data FairTBMQueue α = FairTBMQueue
    { fairTBMQueueQueue ∷ !(TBMQueue α)
    , fairTBMQueueLock ∷ !(MVar ())
    }

instance BoundedCloseableQueue (FairTBMQueue a) a where
    newQueue i = FairTBMQueue <$> newTBMQueueIO (fromIntegral i) <*> newMVar ()
    closeQueue = closeQueue ∘ fairTBMQueueQueue
    readQueue = readQueue ∘ fairTBMQueueQueue
    writeQueue FairTBMQueue{..} a = do
        withMVar fairTBMQueueLock $ \_ → do
            writeQueue fairTBMQueueQueue a
    tryWriteQueue FairTBMQueue{..} a = do
        withMVar fairTBMQueueLock $ \_ → do
            tryWriteQueue fairTBMQueueQueue a

