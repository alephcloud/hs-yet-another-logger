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

import System.Logger

main ∷ IO ()
main = withConsoleLogger Info $ do
    logg Info "moin"
    withLabel ("function", "f") f
    logg Debug "don't show this"
    logg Info "tschüss"
 where
   f = withLevel Debug $ do
       logg Debug "debug f"

