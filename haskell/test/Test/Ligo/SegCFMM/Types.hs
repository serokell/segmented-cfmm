-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.Ligo.SegCFMM.Types
  ( unit_TypesMatch
  ) where

import Universum

import Test.HUnit (Assertion)

import Michelson.Typed (Contract)

import Ligo.SegCFMM.Contract

unit_TypesMatch :: Assertion
unit_TypesMatch = evaluateNF_ @(Contract _ _) segCFMMContractLigo
