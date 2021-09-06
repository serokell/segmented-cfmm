-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.SegCFMM.Storage
  ( defaultStorage
  ) where

import Michelson.Typed

import SegCFMM.Types
import Util (fetchValue)

defaultStorage :: Storage
defaultStorage =
  fromVal ($(fetchValue @Storage "haskell/test/storage_default.tz" "STORAGE_PATH"))
                              -- ↑ This default path works on CI.
                              -- There it's relative to the repo root, apparently.
