{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Example
-- Copyright: Copyright © 2015 PivotCloud, Inc.
-- License: Apache-2.0
-- Maintainer: Lars Kuhtz <lkuhtz@pivotmail.com>
-- Stability: experimental
--
module Main
( main
) where

import Distribution.Simple.Utils (withTempFile)

import System.IO
import System.Logger

main ∷ IO ()
main = do

    -- log to console
    withConsoleLogger Info $ withLabel ("logger", "console") run

    -- log to a file
    withTempFile "." "logfile.log" $ \file h → do
        hClose h
        withFileLogger file Info $ withLabel ("logger", "file") run
        readFile file >>= putStrLn

  where
    f = withLevel Debug $ logg Debug "debug f"

    run = do
        logg Info "moin"
        withLabel ("function", "f") f
        logg Debug "don't show this"
        logg Info "tschüss"

