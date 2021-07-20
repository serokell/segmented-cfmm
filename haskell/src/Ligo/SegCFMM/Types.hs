-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Types mirrored from LIGO implementation.
module Ligo.SegCFMM.Types
  ( Storage (..)
  , Parameter (..)
  ) where

import Universum

import Fmt (Buildable, build, genericF)

import Lorentz hiding (now)

data Parameter
  = X_to_Y XToYParam
  | Y_to_X YToXParam
  | Set_position SetPositionParam
  | X_to_X_prime Address
  | Get_time_weighted_sum (ContractRef Views)

instance Buildable Parameter where
  build = genericF


data Views =
  IC_sum Integer

instance Buildable Views where
  build = genericF


data XToYParam = XToYParam
  { xpDx :: Natural
  , xpDeadline :: Timestamp
  , xpMinDy :: Natural
  , xpToDy :: Address -- Recipient of dy
  }

instance Buildable XToYParam where
  build = genericF


data YToXParam = YToXParam
  { ypDy :: Natural
  , ypDeadline :: Timestamp
  , ypMinDx :: Natural
  , ypToDx :: Address -- Recipient of dx
  }

instance Buildable YToXParam where
  build = genericF


data SetPositionParam = SetPositionParam
  { sppIL :: TickIndex
  , sppIU :: TickIndex
  , sppILL :: TickIndex
  , sppIUL :: TickIndex
  , sppDeltaLiquidity :: Integer
  , sppToX :: Address
  , sppToY :: Address
  }

instance Buildable SetPositionParam where
  build = genericF



-----------------------------------------------------------------
-- Storage
-----------------------------------------------------------------

data Storage = Storage
  { sLiquidity :: Natural
  , sSqrtPrice :: Natural
  , sIC :: Integer
  , sLo :: TickIndex
  , sFeeGrowth :: BalanceNat
  , sBalance :: BalanceNat
  , sTicks :: TickMap
  , sPositions :: PositionMap
  , sTimeWeightIcSum :: Integer
  , sLastIcSumUpdate :: Timestamp
  , sSecondsPerLiquidity :: Natural
  }

instance Buildable Storage where
  build = genericF



data BalanceNat = BalanceNat
  { bnX :: Natural
  , bnY :: Natural
  }

instance Buildable BalanceNat where
  build = genericF


newtype TickIndex = TickIndex Integer
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue

instance Buildable TickIndex where
  build = genericF

instance HasAnnotation TickIndex where
  annOptions = baseDaoAnnOptions


data TickState = TickState
  { tsPrev :: TickIndex
  , tsNext :: TickIndex
  , tsDeltaLiquidity :: Integer
  , tsNPosition :: Natural
  , tsFeeGrowthOutside :: BalanceNat
  , tsSecondsOutside :: Natural
  , tsSecondsPerLiquidityOutside :: Natural
  , tsSqrtPrice :: Natural
  }

instance Buildable TickState where
  build = genericF


type TickMap = BigMap TickIndex TickState


data PositionIndex = PositionIndex
  { piOwner :: Address
  , piLO :: TickIndex
  , piHI :: TickIndex
  } deriving stock (Ord, Eq)

instance Buildable PositionIndex where
  build = genericF


data PositionState = PositionState
  { psLiquidity :: Natural
  , psFeeGrowthInsideLast :: BalanceNat
  , psSecondsPerLiquidityInside :: Natural
  }

instance Buildable PositionState where
  build = genericF

type PositionMap = BigMap PositionIndex PositionState

-----------------------------------------------------------------
-- Helper
-----------------------------------------------------------------

baseDaoAnnOptions :: AnnOptions
baseDaoAnnOptions = defaultAnnOptions
  { fieldAnnModifier = dropPrefixThen toSnake }

-----------------------------------------------------------------
-- TH
-----------------------------------------------------------------

customGeneric "Views" ligoLayout
deriving anyclass instance IsoValue Views
instance HasAnnotation Views where
  annOptions = baseDaoAnnOptions

customGeneric "XToYParam" ligoLayout
deriving anyclass instance IsoValue XToYParam
instance HasAnnotation XToYParam where
  annOptions = baseDaoAnnOptions

customGeneric "YToXParam" ligoLayout
deriving anyclass instance IsoValue YToXParam
instance HasAnnotation YToXParam where
  annOptions = baseDaoAnnOptions

customGeneric "SetPositionParam" ligoLayout
deriving anyclass instance IsoValue SetPositionParam
instance HasAnnotation SetPositionParam where
  annOptions = baseDaoAnnOptions

customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter
instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate


customGeneric "BalanceNat" ligoLayout
deriving anyclass instance IsoValue BalanceNat
instance HasAnnotation BalanceNat where
  annOptions = baseDaoAnnOptions

customGeneric "TickState" ligoLayout
deriving anyclass instance IsoValue TickState
instance HasAnnotation TickState where
  annOptions = baseDaoAnnOptions


customGeneric "PositionIndex" ligoLayout
deriving anyclass instance IsoValue PositionIndex
instance HasAnnotation PositionIndex where
  annOptions = baseDaoAnnOptions

customGeneric "PositionState" ligoLayout
deriving anyclass instance IsoValue PositionState
instance HasAnnotation PositionState where
  annOptions = baseDaoAnnOptions


customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
instance HasAnnotation Storage where
  annOptions = baseDaoAnnOptions
