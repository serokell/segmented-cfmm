-- SPDX-FileCopyrightText: 2021 Arthur Breitman
-- SPDX-License-Identifier: LicenseRef-MIT-Arthur-Breitman
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Types mirrored from LIGO implementation.
module SegCFMM.Types
  ( -- * Parameter
    X(..)
  , powerOfX
  , mkX
  , mkX'
  , adjustScale
  , Parameter(..)
  , XToYParam(..)
  , YToXParam(..)
  , XToXPrimeParam(..)
  , SetPositionParam(..)
  , UpdatePositionParam(..)
  , ObserveParam
  , PositionInfo(..)
  , CumulativesInsideSnapshot(..)
  , subCumulativesInsideSnapshot
  , SnapshotCumulativesInsideParam(..)
  , GetPositionInfoParam
  -- * Storage
  , Storage(..)
  , sLiquidityL
  , sSqrtPriceL
  , sCurTickIndexL
  , sCurTickWitnessL
  , sFeeGrowthL
  , sTicksL
  , sPositionsL
  , sCumulativesBufferL
  , sMetadataL
  , sNewPositionIdL
  , sOperatorsL
  , sConstantsL
  , sLadderL
  , StorageRPC(..)
  , PerToken(..)
  , TickIndex(..)
  , minTickIndex
  , maxTickIndex
  , TickState(..)
  , TickMap
  , PositionState(..)
  , PositionId(..)
  , PositionMap
  , CumulativesValue(..)
  , TickCumulative(..)
  , SplCumulative(..)
  , TimedCumulatives(..)
  , initTimedCumulatives
  , CumulativesBuffer(..)
  , CumulativesBufferRPC(..)
  , cbActualLength
  , cbEntries
  , cbAllEntries
  , initCumulativesBuffer
  , Constants(..)
  , cFeeBpsL
  , cCtezBurnFeeBpsL
  , cXTokenIdL
  , cYTokenIdL
  , cXTokenAddressL
  , cYTokenAddressL
  , cTickSpacingL

  -- * Operators
  , Operators

  -- * helpers
  , segCfmmAnnOptions
  ) where

import Universum

import Data.Ix (Ix)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Fmt (Buildable, GenericBuildable(..), build, pretty, (+|), (|+))
import qualified Text.Show
import Util.Lens (makeLensesWith, postfixLFields)

import qualified Control.Exception as E
import Lorentz hiding (abs, now)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface.ParameterInstances ()
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZIP16
import Morley.Client (AsRPC, deriveRPCWithStrategy)

-- | A value with @2^-n@ precision.
newtype X (n :: Nat) a = X
  { pickX :: a
    -- ^ Get the value multiplied by @2^n@.
  } deriving stock (Show, Eq, Generic, Functor)
    deriving newtype (IsoValue, HasAnnotation, Integral, Enum, Ord, Real, Ix)

instance (Num a, KnownNat n) => Num (X n a) where
  X a + X b = X (a + b)
  X a - X b = X (a - b)
  X _ * X _ = error "multiplication on X type is not defined"
  fromInteger i = X $ fromIntegral $ i * 2 ^ (natVal (Proxy @n))
  abs (X a) = X (abs a)
  signum (X a) = X (signum a)

instance (Buildable a, KnownNat n, Integral a) => Buildable (X n a) where
  build x =
    pickX x |+ " X 2^-" +| build (powerOfX x) |+ " \
    \(≈" +| fromIntegral @Integer @Double (round (pickX x * 1000 % (2 ^ powerOfX x))) / 1000 |+ ")"

type instance AsRPC (X n a) = X n a

powerOfX :: KnownNat n => X n a -> Natural
powerOfX (X{} :: X n a) = natVal (Proxy @n)

-- | Stores the number is a @2^n@ scale.
mkX :: forall a n. (KnownNat n, Integral a) => a -> X n a
mkX a = X (a * 2 ^ natVal (Proxy @n))

-- | Stores the number is a @2^n@ scale.
mkX' :: forall a n b. (KnownNat n, RealFrac a, Integral b) => a -> X n b
mkX' = X . round . (* 2 ^ natVal (Proxy @n))

adjustScale :: forall n2 n1 i. (Integral i, KnownNat n1, KnownNat n2) => X n1 i -> X n2 i
adjustScale (X i) =
  let scaleAdjustment = toInteger (natVal (Proxy @n2)) - toInteger (natVal (Proxy @n1))
  in  if scaleAdjustment >= 0
        then X (i * 2^scaleAdjustment)
        else X (i `div` 2^(-scaleAdjustment))

data Parameter
  = X_to_y XToYParam
    -- ^ Trade up to a quantity dx of asset x, receives dy
  | Y_to_x YToXParam
    -- ^ Trade up to a quantity dy of asset y, receives dx
  | X_to_x_prime XToXPrimeParam
    -- ^ Equivalent to token_to_token
  | Set_position SetPositionParam
    -- ^ Creates a new position in the given range.
  | Update_position UpdatePositionParam
    -- ^ Updates an existing position.
  | Get_position_info GetPositionInfoParam
    -- ^ Get information about position
  | Call_fa2 FA2.Parameter
    -- ^ Call FA2 interface
  | Snapshot_cumulatives_inside SnapshotCumulativesInsideParam
    -- ^ Get oracle values at given range.
  | Observe ObserveParam
    -- ^ Get oracle values at certain points of time.
  | Increase_observation_count Natural
    -- ^ Set the number of stored accumulators for geometric mean price oracle.

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

-- | Parameter of @X_to_X_prime@ entrypoints
data XToXPrimeParam = XToXPrimeParam
  { xppDx :: Natural
    -- ^ Sold tokens amount.
  , xppX_PrimeContract :: Address
    -- ^ Address of another segmented-cfmm contract.
  , xppDeadline :: Timestamp
    -- ^ Deadline for the exchange.
  , xppMinDxPrime :: Natural
    -- ^ Minimal expected number of tokens bought.
  , xppToDxPrime :: Address
    -- ^ Recipient of X tokens.
  }

data SetPositionParam = SetPositionParam
  { sppLowerTickIndex :: TickIndex
    -- ^ Lower tick
  , sppUpperTickIndex :: TickIndex
    -- ^ Upper tick
  , sppLowerTickWitness :: TickIndex
    -- ^ Index of an initialized lower tick lower than `sppIL` (to find it easily in the linked list).
  , sppUpperTickWitness :: TickIndex
    -- ^ Index of an initialized upper tick lower than `sppIU` (to find it easily in the linked list).
  , sppLiquidity :: Natural
    -- ^ Liquidity of the position.
  , sppDeadline :: Timestamp
    -- ^ The deadline for the request to be executed.
  , sppMaximumTokensContributed :: PerToken Natural
    -- ^ The maximum number of tokens to contribute.
    -- If a higher amount is required, the entrypoint fails.
  }

data UpdatePositionParam = UpdatePositionParam
  { uppPositionId :: PositionId
    -- ^ Position identifier
  , uppLiquidityDelta :: Integer
    -- ^ How to change liquidity of the position.
  , uppToX :: Address
    -- ^ Where to send freed X tokens, if any.
  , uppToY :: Address
    -- ^ Where to send freed Y tokens, if any.
  , uppDeadline :: Timestamp
    -- ^ The deadline for the request to be executed.
  , uppMaximumTokensContributed :: PerToken Natural
    -- ^ The maximum number of tokens to contribute.
    -- If a higher amount is required, the entrypoint fails.
  }

type ObserveParam = View [Timestamp] [CumulativesValue]

data PositionInfo = PositionInfo
  { piLiquidity :: Natural
  , piOwner :: Address
  , piLowerTickIndex :: TickIndex
  , piUpperTickIndex :: TickIndex
  }

data CumulativesInsideSnapshot = CumulativesInsideSnapshot
  { cisTickCumulativeInside :: Integer
  , cisSecondsPerLiquidityInside :: X 128 Integer
  , cisSecondsInside :: Integer
  } deriving stock (Eq)

type instance AsRPC CumulativesInsideSnapshot = CumulativesInsideSnapshot

subCumulativesInsideSnapshot
  :: CumulativesInsideSnapshot
  -> CumulativesInsideSnapshot
  -> CumulativesInsideSnapshot
subCumulativesInsideSnapshot cs1 cs2 =
  let subOn :: Num a => (CumulativesInsideSnapshot -> a) -> a
      subOn getter = getter cs1 - getter cs2 in
  CumulativesInsideSnapshot
  { cisTickCumulativeInside = subOn cisTickCumulativeInside
  , cisSecondsPerLiquidityInside = subOn cisSecondsPerLiquidityInside
  , cisSecondsInside = subOn cisSecondsInside
  }

data SnapshotCumulativesInsideParam = SnapshotCumulativesInsideParam
  { sciLowerTickIndex :: TickIndex
  , sciUpperTickIndex :: TickIndex
  , sciCallback :: ContractRef CumulativesInsideSnapshot
  }

type GetPositionInfoParam = View PositionId PositionInfo

-----------------------------------------------------------------
-- Storage
-----------------------------------------------------------------

data Storage = Storage
  { sLiquidity :: Natural
    -- ^ Virtual liquidity, the value L for which the curve locally looks like x * y = L^2
  , sSqrtPrice :: X 80 Natural
    -- ^ Square root of the virtual price, the value P for which P = x / y
  , sCurTickIndex :: TickIndex
    -- ^ Current tick index: The highest tick corresponding to a price less than or
    -- equal to sqrt_price^2, does not necessarily corresponds to a boundary.
  , sCurTickWitness :: TickIndex
    -- ^ The highest initialized tick lower than or equal to cur_tick_index
  , sFeeGrowth :: PerToken (X 128 Natural)
    -- ^ Represent the total amount of fees that have been earned per unit of
    -- virtual liquidity, over the entire history of the contract.
  , sTicks :: TickMap
    -- ^ Ticks' states.
  , sPositions :: PositionMap
    -- ^ Positions' states.
  , sCumulativesBuffer :: CumulativesBuffer
    -- ^ Stored cumulative time-weighted values.

  , sMetadata :: TZIP16.MetadataMap BigMap
    -- ^ TZIP-16 metadata.
  , sNewPositionId :: PositionId
    -- ^ Incremental position id to be assigned to new position.
  , sOperators :: Operators
    -- ^ FA2 operators
  , sConstants :: Constants
    -- ^ Settable constants of the contract
  , sLadder :: Ladder
    -- ^ Exponents ladder for power calculations
  }

-- Needed by `sMetadata`
instance Buildable (ByteString) where
  build = build . show @Text

instance HasFieldOfType Storage name field => StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT


data PerToken a = PerToken
  { ptX :: a
  , ptY :: a
  }
  deriving stock (Eq, Show, Functor)

instance Num a => Num (PerToken a) where
  PerToken x1 y1 + PerToken x2 y2 = PerToken (x1 + x2) (y1 + y2)
  PerToken x1 y1 * PerToken x2 y2 = PerToken (x1 * x2) (y1 * y2)
  PerToken x1 y1 - PerToken x2 y2 = PerToken (x1 - x2) (y1 - y2)
  abs (PerToken x y) = PerToken (abs x) (abs y)
  signum (PerToken x y) = PerToken (signum x) (signum y)
  fromInteger i = join PerToken (fromInteger i)

type instance AsRPC (PerToken a) = PerToken a

-- | Tick types, representing pieces of the curve offered between different tick segments.
newtype TickIndex = TickIndex { unTickIndex :: Integer }
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral, Buildable, Ix)
  deriving anyclass IsoValue

instance Bounded TickIndex where
  minBound = -1048575
  maxBound = 1048575

minTickIndex, maxTickIndex :: TickIndex
minTickIndex = minBound
maxTickIndex = maxBound

instance HasAnnotation TickIndex where
  annOptions = segCfmmAnnOptions

type instance AsRPC TickIndex = TickIndex


-- | Information stored for every initialized tick.
data TickState = TickState
  { tsPrev :: TickIndex
    -- ^ Index of the previous initialized tick.
  , tsNext :: TickIndex
    -- ^ Index of the next initialized tick.
  , tsLiquidityNet :: Integer
    -- ^ Track total amount of liquidity that is added/removed when
    -- this tick is crossed.
  , tsNPositions :: Natural
    -- ^ Number of positions that cover this tick.
  , tsSecondsOutside :: Natural
    -- ^ Overall number of seconds spent below or above this tick
    --   (below or above - depends on whether the current tick
    --    is below or above this tick).
  , tsTickCumulativeOutside :: Integer
    -- ^ Track tick index accumulated below or above this tick.
  , tsFeeGrowthOutside :: PerToken (X 128 Natural)
    -- ^ Track fees accumulated below or above this tick.
  , tsSecondsPerLiquidityOutside :: X 128 Natural
    -- ^ Track seconds-weighted 1/L value below or above this tick.
  , tsSqrtPrice :: X 80 Natural
    -- ^ Square root of the price associated with this tick.
  }
  deriving stock (Eq, Show)

type TickMap = BigMap TickIndex TickState

data PositionState = PositionState
  { psLowerTickIndex :: TickIndex
    -- ^ Lower bound tick index of this position.
  , psUpperTickIndex :: TickIndex
    -- ^ Upper bound tick index of this position.
  , psOwner :: Address
    -- ^ Position's current owner.
  , psLiquidity :: Natural
    -- ^ Amount of virtual liquidity that the position represented the last
    -- time it was touched. This amount does not reflect the fees that have
    -- been accumulated since the contract was last touched.
  , psFeeGrowthInsideLast :: PerToken (X 128 Integer)
    -- ^ Used to calculate uncollected fees.
  }
  deriving stock (Eq, Show)

newtype PositionId = PositionId Natural
  deriving stock (Generic, Show)
  deriving newtype (Enum, Ord, Eq, Num, Real, Integral)
  deriving anyclass IsoValue
  deriving Buildable via GenericBuildable PositionId

instance HasAnnotation PositionId where
  annOptions = segCfmmAnnOptions

type instance AsRPC PositionId = PositionId

instance PositionId `CanCastTo` FA2.TokenId
instance FA2.TokenId `CanCastTo` PositionId

-- | Map containing Liquidity providers.
type PositionMap = BigMap PositionId PositionState

-- | Return value of observation entrypoint.
data CumulativesValue = CumulativesValue
  { cvTickCumulative :: Integer
  , cvSecondsPerLiquidityCumulative :: X 128 Natural
  }
  deriving stock (Eq, Show)

instance Num CumulativesValue where
  CumulativesValue t1 spl1 + CumulativesValue t2 spl2 =
    CumulativesValue (t1 + t2) (spl1 + spl2)
  CumulativesValue t1 spl1 - CumulativesValue t2 spl2 =
    CumulativesValue (t1 - t2) (spl1 - spl2)
  _ * _ = error "`*` on CumulativesValue is not defined"
  abs (CumulativesValue t spl) = CumulativesValue (abs t) (abs spl)
  signum _ = error "signum on CumulativesValue is not defined"
  fromInteger _ = error "fromInteger on CumulativesValue is not defined"

type instance AsRPC CumulativesValue = CumulativesValue

data TickCumulative = TickCumulative
  { tcSum :: Integer
  , tcBlockStartValue :: TickIndex
  }
  deriving stock Eq

data SplCumulative = SplCumulative
  { scSum :: X 128 Natural
  , scBlockStartLiquidityValue :: Natural
  }
  deriving stock Eq

data TimedCumulatives = TimedCumulatives
  { tcTime :: Timestamp
  , tcTick :: TickCumulative
  , tcSpl :: SplCumulative
  }
  deriving stock Eq

initTimedCumulatives :: TimedCumulatives
initTimedCumulatives = TimedCumulatives
  { tcTime = timestampFromSeconds 0
  , tcTick = TickCumulative 0 (TickIndex 0)
  , tcSpl = SplCumulative (X 0) 0
  }

data CumulativesBuffer = CumulativesBuffer
  { cbMap :: BigMap Natural TimedCumulatives
  , cbFirst :: Natural
  , cbLast :: Natural
  , cbReservedLength :: Natural
  }

-- | All entries, including the pre-allocated ones.
cbAllEntries :: CumulativesBuffer -> Map Natural TimedCumulatives
cbAllEntries = bmMap . cbMap

-- | All records.
cbEntries :: CumulativesBuffer -> Map Natural TimedCumulatives
cbEntries b =
  Map.filterWithKey (\k _ -> cbFirst b <= k && k <= cbLast b) (cbAllEntries b)

initCumulativesBuffer :: Natural -> CumulativesBuffer
initCumulativesBuffer extraReservedSlots = CumulativesBuffer
  { cbMap = mkBigMap $ foldMap (one . (, initTimedCumulatives)) [0 .. extraReservedSlots]
  , cbFirst = 0
  , cbLast = 0
  , cbReservedLength = extraReservedSlots + 1
  }

cbActualLength :: CumulativesBuffer -> Natural
cbActualLength CumulativesBuffer{..} =
  E.assert (cbLast >= cbFirst) $
    cbLast - cbFirst + 1

-- | Stored constants picked at origination
data Constants = Constants
  { cFeeBps :: Natural
  , cCtezBurnFeeBps :: Natural
  , cXTokenId :: FA2.TokenId
  , cYTokenId :: FA2.TokenId
  , cXTokenAddress :: Address
  , cYTokenAddress :: Address
  , cTickSpacing :: Natural
  }
  deriving stock Eq


type Ladder = BigMap LadderKey FixedPoint

data FixedPoint = FixedPoint
  { fpV :: Natural
  , fpOffset :: Integer
  } deriving stock (Ord, Eq)

data LadderKey = LadderKey
  { lkExp :: Natural
  , lkPositive :: Bool
  } deriving stock (Ord, Eq)

------------------------------------------------------------------------
-- Operators
------------------------------------------------------------------------

deriving stock instance Ord FA2.OperatorParam

type Operators = BigMap FA2.OperatorParam ()

instance Buildable () where
  build _ = "()"

-----------------------------------------------------------------
-- Helper
-----------------------------------------------------------------

segCfmmAnnOptions :: AnnOptions
segCfmmAnnOptions = defaultAnnOptions
  { fieldAnnModifier = dropPrefixThen toSnake }

-----------------------------------------------------------------
-- TH
-----------------------------------------------------------------

customGeneric "XToYParam" ligoLayout
deriving via (GenericBuildable XToYParam) instance Buildable XToYParam
deriving anyclass instance IsoValue XToYParam
instance HasAnnotation XToYParam where
  annOptions = segCfmmAnnOptions

customGeneric "YToXParam" ligoLayout
deriving via (GenericBuildable YToXParam) instance Buildable YToXParam
deriving anyclass instance IsoValue YToXParam
instance HasAnnotation YToXParam where
  annOptions = segCfmmAnnOptions

customGeneric "XToXPrimeParam" ligoLayout
deriving via (GenericBuildable XToXPrimeParam) instance Buildable XToXPrimeParam
deriving anyclass instance IsoValue XToXPrimeParam
instance HasAnnotation XToXPrimeParam where
  annOptions = segCfmmAnnOptions

customGeneric "SetPositionParam" ligoLayout
deriving via (GenericBuildable SetPositionParam) instance Buildable SetPositionParam
instance Show SetPositionParam where show = pretty
deriving anyclass instance IsoValue SetPositionParam
instance HasAnnotation SetPositionParam where
  annOptions = segCfmmAnnOptions

customGeneric "UpdatePositionParam" ligoLayout
deriving via (GenericBuildable UpdatePositionParam) instance Buildable UpdatePositionParam
deriving anyclass instance IsoValue UpdatePositionParam
instance HasAnnotation UpdatePositionParam where
  annOptions = segCfmmAnnOptions

customGeneric "PositionInfo" ligoLayout
deriving via (GenericBuildable PositionInfo) instance Buildable PositionInfo
deriving anyclass instance IsoValue PositionInfo
instance HasAnnotation PositionInfo where
  annOptions = segCfmmAnnOptions

customGeneric "CumulativesInsideSnapshot" ligoLayout
deriving via (GenericBuildable CumulativesInsideSnapshot) instance Buildable CumulativesInsideSnapshot
deriving anyclass instance IsoValue CumulativesInsideSnapshot
instance HasAnnotation CumulativesInsideSnapshot where
  annOptions = segCfmmAnnOptions

customGeneric "SnapshotCumulativesInsideParam" ligoLayout
deriving via (GenericBuildable SnapshotCumulativesInsideParam) instance Buildable SnapshotCumulativesInsideParam
deriving anyclass instance IsoValue SnapshotCumulativesInsideParam
instance HasAnnotation SnapshotCumulativesInsideParam where
  annOptions = segCfmmAnnOptions

customGeneric "Parameter" ligoLayout
deriving via (GenericBuildable Parameter) instance Buildable Parameter
deriving anyclass instance IsoValue Parameter

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate


customGeneric "PerToken" ligoLayout
deriving via (GenericBuildable (PerToken a)) instance Buildable a => Buildable (PerToken a)
deriving anyclass instance IsoValue a => IsoValue (PerToken a)
instance HasAnnotation a => HasAnnotation (PerToken a) where
  annOptions = segCfmmAnnOptions

customGeneric "TickState" ligoLayout
deriving via (GenericBuildable TickState) instance Buildable TickState
deriving anyclass instance IsoValue TickState
instance HasAnnotation TickState where
  annOptions = segCfmmAnnOptions

customGeneric "PositionState" ligoLayout
deriving via (GenericBuildable PositionState) instance Buildable PositionState
deriving anyclass instance IsoValue PositionState
instance HasAnnotation PositionState where
  annOptions = segCfmmAnnOptions

customGeneric "CumulativesValue" ligoLayout
deriving via (GenericBuildable CumulativesValue) instance Buildable CumulativesValue
deriving anyclass instance IsoValue CumulativesValue
instance HasAnnotation CumulativesValue where
  annOptions = segCfmmAnnOptions

customGeneric "TickCumulative" ligoLayout
deriving via (GenericBuildable TickCumulative) instance Buildable TickCumulative
deriving anyclass instance IsoValue TickCumulative
instance HasAnnotation TickCumulative where
  annOptions = segCfmmAnnOptions

customGeneric "SplCumulative" ligoLayout
deriving via (GenericBuildable SplCumulative) instance Buildable SplCumulative
deriving anyclass instance IsoValue SplCumulative
instance HasAnnotation SplCumulative where
  annOptions = segCfmmAnnOptions

customGeneric "TimedCumulatives" ligoLayout
deriving via (GenericBuildable TimedCumulatives) instance Buildable TimedCumulatives
deriving anyclass instance IsoValue TimedCumulatives
instance HasAnnotation TimedCumulatives where
  annOptions = segCfmmAnnOptions

customGeneric "CumulativesBuffer" ligoLayout
deriving via (GenericBuildable CumulativesBuffer) instance Buildable CumulativesBuffer
deriving anyclass instance IsoValue CumulativesBuffer
instance HasAnnotation CumulativesBuffer where
  annOptions = segCfmmAnnOptions

deriveRPCWithStrategy "CumulativesBuffer" ligoLayout
deriving via (GenericBuildable CumulativesBufferRPC) instance Buildable CumulativesBufferRPC

customGeneric "Constants" ligoLayout
deriving via (GenericBuildable Constants) instance Buildable Constants
deriving anyclass instance IsoValue Constants
instance HasAnnotation Constants where
  annOptions = segCfmmAnnOptions

deriveRPCWithStrategy "Constants" ligoLayout
deriving via (GenericBuildable ConstantsRPC) instance Buildable ConstantsRPC

customGeneric "FixedPoint" ligoLayout
deriving via (GenericBuildable FixedPoint) instance Buildable FixedPoint
deriving anyclass instance IsoValue FixedPoint
instance HasAnnotation FixedPoint where
  annOptions = segCfmmAnnOptions

deriveRPCWithStrategy "FixedPoint" ligoLayout
deriving via (GenericBuildable FixedPointRPC) instance Buildable FixedPointRPC

customGeneric "LadderKey" ligoLayout
deriving via (GenericBuildable LadderKey) instance Buildable LadderKey
deriving anyclass instance IsoValue LadderKey
instance HasAnnotation LadderKey where
  annOptions = segCfmmAnnOptions

deriveRPCWithStrategy "LadderKey" ligoLayout
deriving via (GenericBuildable LadderKeyRPC) instance Buildable LadderKeyRPC

customGeneric "Storage" ligoLayout
deriving via (GenericBuildable Storage) instance Buildable Storage
deriving anyclass instance IsoValue Storage
instance HasAnnotation Storage where
  annOptions = segCfmmAnnOptions

type instance AsRPC FA2.TokenId = FA2.TokenId

deriveRPCWithStrategy "Storage" ligoLayout
deriving via (GenericBuildable StorageRPC) instance Buildable StorageRPC

makeLensesWith postfixLFields ''Storage
makeLensesWith postfixLFields ''Constants
