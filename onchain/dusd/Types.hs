module Types (ProtocolParams(..)) where

import Plutarch.Prelude
import Plutarch.DataRepr (PDataFields)

newtype ProtocolParams (s :: S)
  = ProtocolParams
      ( Term
          s
          ( PDataRecord
              '[ "debtCeil" ':= PInteger
               , "debtFloor" ':= PInteger
               -- TODO other params
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType ProtocolParams where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData ProtocolParams)
