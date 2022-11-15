module Types (ProtocolParams (ProtocolParams),ConfigDatum(ConfigDatum)) where

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

newtype ConfigDatum (s :: S)
  = ConfigDatum
      ( Term
          s
          ( PDataRecord
              '[ "authPolicy" ':= PCurrencySymbol
               , "versions" ':= PBuiltinList PData
               -- TODO elaborate the pdata
               -- once the spec is more complete
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType ConfigDatum where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData ConfigDatum)
