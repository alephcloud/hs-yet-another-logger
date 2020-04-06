-- |
-- Module: System.Logger.Internal
-- Copyright:
--     Copyright (c) 2016-2020 Lars Kuhtz <lakuhtz@gmail.com>
--     Copyright (c) 2014-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lakuhtz@gmail.com>
-- Stability: experimental
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.Logger.Internal
( sshow
, formatIso8601
, formatIso8601Milli
, formatIso8601Micro
, timeSpecToUtc
) where

import Data.Monoid.Unicode
import Data.String
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format

import Numeric.Natural

import Prelude.Unicode

import System.Clock

sshow
    ∷ (Show a, IsString b)
    ⇒ a
    → b
sshow = fromString ∘ show
{-# INLINE sshow #-}

-- | Format 'TimeSpec' as ISO8601 date-time string with
-- microseconds precision.
--
-- @since 0.2
--
formatIso8601Micro
    ∷ IsString a
    ⇒ TimeSpec
    → a
formatIso8601Micro = formatIso8601 6

-- | Format 'TimeSpec' as ISO8601 date-time string with
-- milliseconds precision.
--
-- @since 0.2
--
formatIso8601Milli
    ∷ IsString a
    ⇒ TimeSpec
    → a
formatIso8601Milli = formatIso8601 3

-- | Format 'TimeSpec' as ISO8601 date-time string with
-- the given sub-second precision.
--
-- @since 0.2
--
formatIso8601
    ∷ IsString a
    ⇒ Natural
        -- ^ precision, a value between 0 (seconds) and 6 (microseconds)
    → TimeSpec
    → a
formatIso8601 precision
    = fromString
    ∘ (⊕ "Z")
    ∘ take (fromIntegral $ 20 + precision)
    ∘ (⊕ replicate (fromIntegral precision) '0')
    ∘ formatTime defaultTimeLocale ("%Y-%m-%dT%H:%M:%S%Q")
    ∘ timeSpecToUtc

-- | Convert a 'TimeSpec' value into 'UTCTime'
--
-- @since 0.2
--
timeSpecToUtc
    ∷ TimeSpec
    → UTCTime
timeSpecToUtc (TimeSpec s ns) =
    posixSecondsToUTCTime (realToFrac s + realToFrac ns * 1e-9)
