-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Types mirrored from LIGO implementation.
module SegCFMM.Types
  ( Storage (..)
  , Parameter (..)
  ) where

import Universum

import Fmt (Buildable, build, genericF)

import Lorentz hiding (now)
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16

data Parameter
  = X_to_Y XToYParam
    -- ^ Trade up to a quantity dx of asset x, receives dy
  | Y_to_X YToXParam
    -- ^ Trade up to a quantity dy of asset y, receives dx
  | Set_position SetPositionParam
    -- ^ TODO: Add deadline, maximum tokens contributed, and maximum liquidity present
  | X_to_X_prime Address
    -- ^ Equivalent to token_to_token
  | Get_time_weighted_sum (ContractRef Views)

instance Buildable Parameter where
  build = genericF


data Views =
  IC_sum Integer

instance Buildable Views where
  build = genericF

-- | Parameter of @X_to_Y@ entrypoints
data XToYParam = XToYParam
  { xpDx :: Natural
    -- ^ Sold tokens amount.
  , xpDeadline :: Timestamp
    -- ^ Deadline for the exchange.
  , xpMinDy :: Natural
    -- ^ Minimal expected number of tokens bought.
  , xpToDy :: Address
    -- ^ Recipient of Y tokens.
  }

instance Buildable XToYParam where
  build = genericF

-- | Parameter of @Y_to_X@ entrypoints
data YToXParam = YToXParam
  { ypDy :: Natural
    -- ^ Sold tokens amount.
  , ypDeadline :: Timestamp
    -- ^ Deadline for the exchange.
  , ypMinDx :: Natural
    -- ^ Minimal expected number of tokens bought.
  , ypToDx :: Address
    -- ^ Recipient of X tokens.
  }

instance Buildable YToXParam where
  build = genericF


data SetPositionParam = SetPositionParam
  { sppIL :: TickIndex
    -- ^ Lower tick
  , sppIU :: TickIndex
    -- ^ Upper tick
  , sppILL :: TickIndex
    -- ^ Index of an initialized lower tick lower than `sppIL` (to find it easily in the linked list).
  , sppIUL :: TickIndex
    -- ^ Index of an initialized upper tick lower than `sppIU` (to find it easily in the linked list).
  , sppLiquidityDelta :: Integer
    -- ^ How to change liquidity of the position (if not yet exists, assumed to have 0 liquidity).
  , sppToX :: Address
    -- ^ Where to send freed X tokens, if any.
  , sppToY :: Address
    -- ^ Where to send freed Y tokens, if any.
  }

instance Buildable SetPositionParam where
  build = genericF



-----------------------------------------------------------------
-- Storage
-----------------------------------------------------------------

data Storage = Storage
  { sLiquidity :: Natural
    -- ^ Virtual liquidity, the value L for which the curve locally looks like x * y = L^2
  , sSqrtPrice :: Natural
    -- ^ Square root of the virtual price, the value P for which P = x / y
  , sIC :: Integer
    -- ^ Current tick index: The highest tick corresponding to a price less than or
    -- equal to sqrt_price^2, does not necessarily corresponds to a boundary.
  , sLo :: TickIndex
    -- ^ The highest initialized tick lower than or equal to i_c
  , sFeeGrowth :: BalanceNat
    -- ^ Represent the total amount of fees that have been earned per unit of
    -- virtual liquidity, over the entire history of the contract.
  , sBalance :: BalanceNat
    -- ^ Tokens' balances.
  , sTicks :: TickMap
    -- ^ Ticks' states.
  , sPositions :: PositionMap
    -- ^ Positions' states.
  , sTimeWeightIcSum :: Integer
    -- ^ Cumulative time-weighted sum of the 'sIC'.
  , sLastIcSumUpdate :: Timestamp
    -- ^ Last time 'sLastIcSumUpdate' was updated.
  , sSecondsPerLiquidity :: Natural

  , sMetadata :: TZIP16.MetadataMap BigMap
    -- ^ TZIP-16 metadata.
  }

instance Buildable Storage where
  build = genericF

-- Needed by `sMetadata`
instance Buildable (ByteString) where
  build = build . show @Text

instance HasFieldOfType Storage name field => StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT


data BalanceNat = BalanceNat
  { bnX :: Natural
  , bnY :: Natural
  }

instance Buildable BalanceNat where
  build = genericF

-- | Tick types, representing pieces of the curve offered between different tick segments.
newtype TickIndex = TickIndex Integer
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue

instance Buildable TickIndex where
  build = genericF

instance HasAnnotation TickIndex where
  annOptions = segCfmmAnnOptions

-- | Information stored for every initialized tick.
data TickState = TickState
  { tsPrev :: TickIndex
    -- ^ Index of the previous initialized tick.
  , tsNext :: TickIndex
    -- ^ Index of the next initialized tick.
  , tsLiquidityNet :: Integer
    -- ^ Track total amount of liquidity that is added/removed when
    -- this tick is crossed.
  , tsNPosition :: Natural
    -- ^ Number of positions that cover this tick.
  , tsSecondsOutside :: Natural
    -- ^ Overall number of seconds spent below or above this tick
    --   (below or above - depends on whether the current tick
    --    is below or above this tick).
  , tsFeeGrowthOutside :: BalanceNat
    -- ^ Track fees accumulated below or above this tick.
  , tsSecondsPerLiquidityOutside :: Natural
  , tsSqrtPrice :: Natural
    -- ^ Square root of the price associated with this tick.
  }

instance Buildable TickState where
  build = genericF


type TickMap = BigMap TickIndex TickState

-- | Position types, representing LP positions.
data PositionIndex = PositionIndex
  { piOwner :: Address
  , piLO :: TickIndex
    -- ^ Lower bound.
  , piHI :: TickIndex
    -- ^ Upper bound.
  } deriving stock (Ord, Eq)

instance Buildable PositionIndex where
  build = genericF


data PositionState = PositionState
  { psLiquidity :: Natural
    -- ^ Amount of virtual liquidity that the position represented the last
    -- time it was touched. This amount does not reflect the fees that have
    -- been accumulated since the contract was last touched.
  , psFeeGrowthInsideLast :: BalanceNat
    -- ^ Used to calculate uncollected fees.
  , psSecondsPerLiquidityInside :: Natural
  }

instance Buildable PositionState where
  build = genericF

-- | Map containing Liquidity providers.
type PositionMap = BigMap PositionIndex PositionState

-----------------------------------------------------------------
-- Helper
-----------------------------------------------------------------

segCfmmAnnOptions :: AnnOptions
segCfmmAnnOptions = defaultAnnOptions
  { fieldAnnModifier = dropPrefixThen toSnake }

-----------------------------------------------------------------
-- TH
-----------------------------------------------------------------

customGeneric "Views" ligoLayout
deriving anyclass instance IsoValue Views
instance HasAnnotation Views where
  annOptions = segCfmmAnnOptions

customGeneric "XToYParam" ligoLayout
deriving anyclass instance IsoValue XToYParam
instance HasAnnotation XToYParam where
  annOptions = segCfmmAnnOptions

customGeneric "YToXParam" ligoLayout
deriving anyclass instance IsoValue YToXParam
instance HasAnnotation YToXParam where
  annOptions = segCfmmAnnOptions

customGeneric "SetPositionParam" ligoLayout
deriving anyclass instance IsoValue SetPositionParam
instance HasAnnotation SetPositionParam where
  annOptions = segCfmmAnnOptions

customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter
instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate


customGeneric "BalanceNat" ligoLayout
deriving anyclass instance IsoValue BalanceNat
instance HasAnnotation BalanceNat where
  annOptions = segCfmmAnnOptions

customGeneric "TickState" ligoLayout
deriving anyclass instance IsoValue TickState
instance HasAnnotation TickState where
  annOptions = segCfmmAnnOptions


customGeneric "PositionIndex" ligoLayout
deriving anyclass instance IsoValue PositionIndex
instance HasAnnotation PositionIndex where
  annOptions = segCfmmAnnOptions

customGeneric "PositionState" ligoLayout
deriving anyclass instance IsoValue PositionState
instance HasAnnotation PositionState where
  annOptions = segCfmmAnnOptions


customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
instance HasAnnotation Storage where
  annOptions = segCfmmAnnOptions
