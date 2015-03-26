-- Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
--
-- NOTICE: The dissemination, reproduction, or copying of this file and the
-- information contained herein, in any medium, is strictly forbidden.
--
-- The intellectual property and technical concepts contained herein are
-- proprietary to PivotCloud and are protected by U.S. and Foreign law.

-- |
-- Module: Main
-- Copyright: Copyright (c) 2013-2015 PivotCloud, Inc. All Rights Reserved.
-- License: All Rights Reserved, see LICENSE file of the package
-- Maintainer: code@pivotmail.com
-- Stability: experimental
--
-- Test suite for yet-another-logger
--

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main
(main
) where

import Test.Tasty

-- internal
import qualified NoBackend

main ∷ IO ()
main = defaultMain $
    NoBackend.tests

