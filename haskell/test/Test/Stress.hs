-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman

{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ApplicativeDo #-}

-- Stress tests for the evaluation of reasonable gas costs
module Test.Stress where

import Prelude

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (assert, not, now, (>>), comment)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest
import Morley.Nettest.Tasty
import Tezos.Core (timestampPlusSeconds)
import Test.Tasty (TestTree)

import SegCFMM.Types
import Test.Position
import Test.SegCFMM.Contract (TokenType(..))
import Test.Util

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}


-- note: a slightly modified version of 'test_crossing_ticks'
-- Run with:
-- stack test segmented-cfmm --test-arguments "--color always --nettest-mode=only-network" --fast --ghc-options -Wwarn
test_large_tick_traversal :: TestTree
test_large_tick_traversal =
  nettestScenarioCaps "traversing many close positions fits into gas limits" do
    let liquidity = 1_e6
    let userFA2Balance = 1_e15
    let lowerTickIndex = -30 -- the lower the better
    let upperTickIndex = 0

    liquidityProvider <- newAddress auto
    swapper <- newAddress auto
    let accounts = [liquidityProvider, swapper]
    let xTokenId = FA2.TokenId 0
    let yTokenId = FA2.TokenId 1
    let xFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, xTokenId), userFA2Balance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    let yFa2storage = FA2.Storage
          { sLedger = mkBigMap $ accounts <&> \acct -> ((acct, yTokenId), userFA2Balance)
          , sOperators = mempty
          , sTokenMetadata = mempty
          }
    xToken <- originateSimple "fa2" xFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [xTokenId] })
    yToken <- originateSimple "fa2" yFa2storage (FA2.fa2Contract def { FA2.cAllowedTokenIds = [yTokenId] })

    cfmm <- originateSegCFMM FA2 FA2 $ mkStorage xToken xTokenId yToken yTokenId 0

    for_ accounts \account ->
      withSender account do
        call xToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) xTokenId]
        call yToken (Call @"Update_operators") [FA2.AddOperator $ FA2.OperatorParam account (toAddress cfmm) yTokenId]

    currentTime <- getNow
    let deadline = currentTime `timestampPlusSeconds` 86400 -- a day
    -- needs more money to do all the calls, maybe not as much tho
    transferMoney liquidityProvider $ toMutez 50_e6 -- 50 XTZ
    -- Place many small(est) positions with the same liquidity
    withSender liquidityProvider do
      for_ [lowerTickIndex, lowerTickIndex + 1 .. upperTickIndex - 1] \lowerTickIndex' -> do
        call cfmm (Call @"Set_position")
          SetPositionParam
            { sppLowerTickIndex = lowerTickIndex'
            , sppUpperTickIndex = lowerTickIndex' + 1
            , sppLowerTickWitness = minTickIndex
            , sppUpperTickWitness = minTickIndex
            , sppLiquidity = liquidity
            , sppDeadline = deadline
            , sppMaximumTokensContributed = PerToken userFA2Balance userFA2Balance
            }

    -- Place 1 big swap to push the tick all the way down to `lowerTickIndex`
    withSender swapper do
      call cfmm (Call @"X_to_y") XToYParam
        { xpDx = 50_000
        , xpDeadline = deadline
        , xpMinDy = 0
        , xpToDy = swapper
        }

