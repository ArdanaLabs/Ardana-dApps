module Types (ProtocolParams (..)) where

import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

newtype ProtocolParams (s :: S)
  = ProtocolParams
      ( Term
          s
          ( PDataRecord
              '[ "debtFloor" ':= PInteger
               , "liquidationDiscount" ':= PRational
               , "liquidationFee" ':= PInteger
               , "liquidationRatio" ':=  PRational
              -- TODO Are these types correct
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType ProtocolParams where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData ProtocolParams)


