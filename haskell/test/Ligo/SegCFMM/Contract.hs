-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

-- | LIGO version of the contract.
module Ligo.SegCFMM.Contract
  ( segCFMMContractLigo
  ) where

import Michelson.Typed

import Ligo.SegCFMM.Types
import Ligo.Util

segCFMMContractLigo :: Contract (ToT Parameter) (ToT Storage)
segCFMMContractLigo =
 $(fetchContract @(ToT Parameter) @(ToT Storage) "SEGMENTED_CFMM_LIGO_PATH")
