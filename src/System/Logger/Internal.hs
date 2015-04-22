-- Copyright (c) 2014-2015 PivotCloud, Inc.
--
-- System.Logger.Internal
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
-- Module: System.Logger.Internal
-- Copyright: Copyright (c) 2014-2015 PivotCloud, Inc.
-- License: Apache License, Version 2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--

{-# LANGUAGE UnicodeSyntax #-}

module System.Logger.Internal
( sshow
, formatIso8601
, formatIso8601Milli
, formatIso8601Micro
, timeSpecToUtc
) where

import Data.Monoid.Unicode
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.String

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
formatIso8601Micro
    ∷ IsString a
    ⇒ TimeSpec
    → a
formatIso8601Micro = formatIso8601 6

-- | Format 'TimeSpec' as ISO8601 date-time string with
-- milliseconds precision.
--
formatIso8601Milli
    ∷ IsString a
    ⇒ TimeSpec
    → a
formatIso8601Milli = formatIso8601 3

-- | Format 'TimeSpec' as ISO8601 date-time string with
-- the given sub-second precision.
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
timeSpecToUtc
    ∷ TimeSpec
    → UTCTime
timeSpecToUtc (TimeSpec s ns) =
    posixSecondsToUTCTime (realToFrac s + realToFrac ns * 1e-9)

