-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.FA2.Common
  ( setSimplePosition
  ) where

import Prelude

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (assert, map, not, now, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest

import SegCFMM.Types
import Test.SegCFMM.Contract (TokenType (..))
import Test.SetPosition (mkStorage)
import Test.Util (mkDeadline, originateSegCFMM)

-- | Utility function to make a simple call to @Set_position@ between the two
-- given 'TickIndex'.
--
-- It should succeed and a position be created if the given address was also
-- given to 'prepareSomeSegCFMM'.
setSimplePosition
  :: MonadNettest caps base m
  => ContractHandler Parameter Storage
  -> Address
  -> TickIndex
  -> TickIndex
  -> m ()
setSimplePosition cfmm liquidityProvider lowerTickIndex upperTickIndex = do
  let liquidityDelta = 10000000

  deadline <- mkDeadline
  withSender liquidityProvider do
    call cfmm (Call @"Set_position")
      SetPositionParam
        { sppLowerTickIndex = lowerTickIndex
        , sppUpperTickIndex = upperTickIndex
        , sppLowerTickWitness = minTickIndex
        , sppUpperTickWitness = minTickIndex
        , sppLiquidityDelta = liquidityDelta
        , sppToX = liquidityProvider
        , sppToY = liquidityProvider
        , sppDeadline = deadline
        , sppMaximumTokensContributed = PerToken 1000000 1000000
        }
