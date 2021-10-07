-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

module Test.FA2.Common
  ( originateSegCFMMWithBalances
  , setSimplePosition
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


-- | Utility function to create the two FA2 tokens and then the CFMM that uses
-- them, without any position.
-- All the given addresses will be given initial funds in both tokens and will
-- set the CFMM as an operator.
--
-- Note: this produces always the same combination of X and Y tokens, but this
-- should not influence the FA2 positions tests.
originateSegCFMMWithBalances
  :: MonadNettest caps base m
  => [Address]
  -> m (ContractHandler Parameter Storage)
originateSegCFMMWithBalances addresses = do
  let xTokenId = FA2.TokenId 0
  let yTokenId = FA2.TokenId 1
  let xFa2storage = FA2.Storage
        { sLedger = mkBigMap $ map (\addr -> ((addr, xTokenId), 100000)) addresses
        , sOperators = mempty
        , sTokenMetadata = mempty
        }
  let yFa2storage = FA2.Storage
        { sLedger = mkBigMap $ map (\addr -> ((addr, yTokenId), 100000)) addresses
        , sOperators = mempty
        , sTokenMetadata = mempty
        }
  xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
  yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

  let initialSt = mkStorage xToken xTokenId yToken yTokenId
  cfmm <- originateSegCFMM FA2 FA2 initialSt

  forM_ addresses $ \liquidityProvider -> withSender liquidityProvider do
    call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) xTokenId]
    call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam liquidityProvider (toAddress cfmm) yTokenId]

  return cfmm

-- | Utility function to make a simple call to @Set_position@ between the two
-- given 'TickIndex'.
--
-- It should succeed and a position be created if the given address was also
-- given to 'originateSegCFMMWithBalances'.
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
