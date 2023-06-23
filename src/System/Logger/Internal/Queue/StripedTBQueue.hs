{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: System.Logger.Internal.Queue.StripedTBQueue
-- Copyright: Copyright © 2022 Lars Kuhtz <lakuhtz@gmail.com>
-- License: MIT
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--
-- A striped TBQueue.
--
-- The implementation is based on "Control.Concurrent.STM.TBQueue" from the stm
-- package.
--
module System.Logger.Internal.Queue.StripedTBQueue
( Queue
, newQueueIO
, readQueue
, tryReadQueue
, writeQueue
, peekQueue
, tryPeekQueue
, lengthQueue
, isEmptyQueue
, isFullQueue
, flushQueue
) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM

import Data.Foldable
import Data.Function
import Data.Function.Unicode
import Data.Semigroup

import GHC.Arr

import Numeric.Natural

-- -------------------------------------------------------------------------- --
-- RTS Capabilities

newtype Capability = Capability Int
    deriving (Show)
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral, Ix)

myCapability ∷ IO Capability
myCapability = Capability ∘ fst <$> (myThreadId >>= threadCapability)

-- -------------------------------------------------------------------------- --
-- StripedTBQueue

data Stripe a = Stripe
    { _qOutC ∷ {-# UNPACK #-} !(TVar Natural)
    , _qInC ∷ {-# UNPACK #-} !(TVar Natural)
    , _qIn ∷ {-# UNPACK #-} !(TVar [(a, Capability)])
    }

data Queue a = Queue
    { _qOut ∷ {-# UNPACK #-} !(TVar [(a, Capability)])
    , _qStripes ∷ {-# UNPACK #-} !(Array Capability (Stripe a))
    , _qCapacity ∷ !Natural
    , _qPendingC ∷ {-# UNPACK #-} !(TVar (Maybe Capability))
    }

instance Eq (Queue a) where
    (==) = (==) `on` _qOut
    {-# INLINE (==) #-}

-- -------------------------------------------------------------------------- --
-- Queue Utils

getStripe ∷ Queue a → Capability → Stripe a
getStripe q c = _qStripes q `unsafeAt` fromIntegral c
{-# INLINE getStripe #-}

-- | Lazily awaits that input becomes available and returns the buffers of all
-- stripes merged into a single sorted list.
--
awaitReadInQueues ∷ Ord a ⇒ Queue a → STM [(a, Capability)]
awaitReadInQueues q = do
    ls ← forM (_qStripes q) $ \s → swapTVar_ (_qIn s) []
    check (not $ all null ls)
    return $ _sortedList $ mconcat2 $ SortedList ∘ reverse <$> toList ls
{-# INLINE awaitReadInQueues #-}

-- | Lazily returns the buffers of all stripes merged into a single sorted list.
--
readInQueues ∷ Ord a ⇒ Queue a → STM [(a, Capability)]
readInQueues q = do
    ls ← forM (_qStripes q) $ \s → swapTVar_ (_qIn s) []
    return $ _sortedList $ mconcat2 $ SortedList ∘ reverse <$> toList ls
{-# INLINE readInQueues #-}

-- | Check if there is a pending capacity update. Forcing the value of @c@ is
-- potentially expensive. However, no dependent variable is modified during that
-- time. Also, if we retry later, we don't have to do this work again.
--
creditPendingCapacity ∷ Queue a → STM ()
creditPendingCapacity q =
    swapTVar (_qPendingC q) Nothing >>= \case
        Nothing → return ()
        Just c → do
            let stripe = getStripe q c
            -- this can conflict with a writer
            modifyTVar' (_qOutC stripe) (+1)
{-# INLINE creditPendingCapacity #-}

-- -------------------------------------------------------------------------- --
-- API Functions

-- | Create a new Queue
--
newQueueIO ∷ Natural → IO (Queue a)
newQueueIO size = do
    n ← getNumCapabilities
    when (n < 1) $ error "System.Logger.Internal.Queue: number of RTS capabilities can not be smaller than one"
    let c = ceiling (fromIntegral size / fromIntegral n ∷ Double)
    stripes ← replicateM (fromIntegral n) (mkStripe c)
    Queue
        <$> newTVarIO [] -- _qOut
        <*> pure (listArray (0, fromIntegral n - 1) stripes) -- _qStripes
        <*> pure size -- _qCapacity
        <*> newTVarIO Nothing -- _qPendingC
  where
    mkStripe c = Stripe
        <$> newTVarIO 0 -- _qOutC
        <*> newTVarIO c -- _qInC
        <*> newTVarIO [] -- _qIn

-- | Add an item to the queue.
--
writeQueue ∷ Queue a → a → IO ()
writeQueue q a = do
    myCap ← myCapability
    atomically $ go myCap (getStripe q myCap)
  where
    go c stripe = do
        w ← readTVar $ _qInC stripe
        if w > 0
          then
            writeTVar (_qInC stripe) $! w - 1
          else do
            r ← readTVar (_qOutC stripe)
            if r > 0
              then do
                writeTVar (_qOutC stripe) 0
                writeTVar (_qInC stripe) $! r - 1
              else
                retry
        modifyTVar (_qIn stripe) ((a,c):)

-- | Get and remove a value from the queue.
--
readQueue ∷ Ord a ⇒ Queue a → IO a
readQueue = atomically . readQueueStm
{-# INLINE readQueue #-}

readQueueStm ∷ Ord a ⇒ Queue a → STM a
readQueueStm q = do
    creditPendingCapacity q
    readTVar (_qOut q) >>= \case
        ((h,c) : t) → do
            let stripe = getStripe q c
            modifyTVar' (_qOutC stripe) (+1)
            writeTVar (_qOut q) t
            return h
        [] → do
            -- lazily merge writer lists
            ~(h:t) ← awaitReadInQueues q

            -- t is committed as lazy value
            writeTVar (_qOut q) t

            -- We defer increasing @_qOutC@ in order to not force
            -- z (and zs) within the transaction.
            writeTVar (_qPendingC q) (Just $ snd h)

            -- returned as lazy value
            return (fst h)

-- | A version of 'readTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
--
tryReadQueue ∷ Ord a ⇒ Queue a → IO (Maybe a)
tryReadQueue q = atomically $
    (Just <$> readQueueStm q) `orElse` return Nothing
{-# INLINE tryReadQueue #-}

flushQueue ∷ Ord a ⇒ Queue a → IO [a]
flushQueue q = atomically $ do
    r ← readTVar (_qOut q)
    w ← readInQueues q
    forM_ (_qStripes q) $ \s → do
        writeTVar (_qOutC s) 0
        writeTVar (_qInC s) c
    return $ fst <$> r <> w
  where
    n = length (_qStripes q)
    c = ceiling (fromIntegral (_qCapacity q) / fromIntegral n ∷ Double)

peekQueueStm ∷ Ord a ⇒ Queue a → STM a
peekQueueStm q = do
    creditPendingCapacity q
    readTVar (_qOut q) >>= \case
        ((h,c) : t) → do
            let stripe = getStripe q c
            modifyTVar' (_qOutC stripe) (+1)
            writeTVar (_qOut q) t
            return h
        [] → do
            -- lazily merge writer lists
            ~(l@(h:_)) ← awaitReadInQueues q

            -- t is committed as lazy value
            writeTVar (_qOut q) l

            -- returned as lazy value
            return (fst h)

peekQueue ∷ Ord a ⇒ Queue a → IO a
peekQueue = atomically ∘ peekQueueStm
{-# INLINE peekQueue #-}

tryPeekQueue ∷ Ord a ⇒ Queue a → IO (Maybe a)
tryPeekQueue q = atomically $
    (Just <$> peekQueueStm q) `orElse` return Nothing
{-# INLINE tryPeekQueue #-}


lengthQueue ∷ Queue a → IO Natural
lengthQueue q = atomically $ do
    x ← fmap sum $ forM (_qStripes q) $ \s → (+)
        <$> (readTVar (_qInC s))
        <*> (readTVar (_qOutC s))
    readTVar (_qPendingC q) >>= \case
        Nothing → return $ (_qCapacity q) - x
        Just _ → return $ 1 + (_qCapacity q) - x

isEmptyQueue ∷ Queue a → IO Bool
isEmptyQueue q = atomically $ do
    r ← readTVar (_qOut q)
    case r of
        [] → fmap getAll $ msum $ flip fmap (_qStripes q) $ \s →
            All . null <$> readTVar (_qIn s)
        _ → return False

isFullQueue ∷ Queue a → IO Bool
isFullQueue q = atomically $ do
    fmap getAll $ asum $ concat $ flip fmap (_qStripes q) $ \s →
        [ All . (== 0) <$> (readTVar (_qInC s))
        , All . (== 0) <$> (readTVar (_qOutC s))
        ]

-- -------------------------------------------------------------------------- --
-- Monoid of Sorted Lists

newtype SortedList a = SortedList { _sortedList ∷ [a] }
    deriving (Show)
    deriving newtype (Eq, Ord, Foldable)

instance Ord a ⇒ Semigroup (SortedList a) where
    SortedList a <> SortedList b = SortedList $ a `merge` b
    {-# INLINE (<>) #-}

instance Ord a ⇒ Monoid (SortedList a) where
    mempty = SortedList []
    {-# INLINE mempty #-}

-- | Merge two ordered list
--
merge ∷ Ord a ⇒ [a] → [a] → [a]
merge l [] = l
merge [] l = l
merge l1@(h1:t1) l2@(h2:t2)
    | h1 < h2 = h1:merge t1 l2
    | otherwise = h2:merge l1 t2
{-# INLINABLE merge #-}

-- -------------------------------------------------------------------------- --
-- Utils

-- | A balanced version of mconcat. This is beneficial if the cost of computing
-- '<>' is a function of the arguments that increases monotonically with the
-- by the application of '<>'.
--
-- An example is merging of sorted lists, where the cost of '<>' depends on the
-- length of the inputs and repeated application of '<>' increases the cost.
--
-- (It could also be parallelized.)
--
mconcat2 ∷ Monoid a ⇒ [a] → a
mconcat2 l = case l of
    [] → mempty
    [x] → x
    _ → mconcat2 $ pairwise l
{-# INLINE mconcat2 #-}

pairwise ∷ Monoid a ⇒ [a] → [a]
pairwise [] = []
pairwise [x] = [x]
pairwise (a:b:t) = a <> b : pairwise t
{-# INLINABLE pairwise #-}

-- | Swap out the value of a TVar.
--
swapTVar_ ∷ Eq a ⇒ TVar a → a → STM a
swapTVar_ v new = do
    old ← readTVar v
    unless (old == new) $ writeTVar v new
    return old
{-# INLINE swapTVar_ #-}

