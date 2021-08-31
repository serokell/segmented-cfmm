-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.SegCFMM.Types
  ( unit_ContractTypesMatch
  , unit_StorageTypesMatch
  ) where

import Universum

import Test.HUnit (Assertion)

import Michelson.Typed (Contract)

import SegCFMM.Types (Storage)
import Test.SegCFMM.Contract
import Test.SegCFMM.Storage

unit_ContractTypesMatch :: Assertion
unit_ContractTypesMatch = evaluateNF_ @(Contract _ _) segCFMMContract

unit_StorageTypesMatch :: Assertion
unit_StorageTypesMatch = do
  let !(_ :: Storage) = defaultStorage
  pure ()
