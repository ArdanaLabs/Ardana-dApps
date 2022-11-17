module Types
  (ProtocolParams (..)
  ,PriceEntry(..)
  ,PriceData
  ) where

import Plutarch.Prelude
import Plutarch.Api.V1

newtype ProtocolParams (s :: S)
  = ProtocolParams
      ( Term
          s
          ( PDataRecord
              '[ "debtFloor" ':= PInteger
               , "liquidationDiscount" ':= PRational
               , "liquidationFee" ':= PInteger
               , "liquidationRatio" ':= PRational
               -- TODO Are these types correct
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType ProtocolParams where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData ProtocolParams)


type PriceData :: PType
type PriceData = PBuiltinList  (PAsData PriceEntry)

newtype PriceEntry (s :: S)
  = PriceEntry
      ( Term
          s
          ( PDataRecord
              '[ "time" ':= PPOSIXTime
               , "price" ':= PRational
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PriceEntry where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData PriceEntry)
