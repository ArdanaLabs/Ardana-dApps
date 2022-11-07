module Dusd (configCBOR) where

import Plutarch.Prelude

import Plutarch (Config)
import Plutarch.Api.V1 (PDatum (PDatum), PMap, PValue (PValue))
import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName)
import Plutarch.Api.V2 (POutputDatum (POutputDatum), PTxOut (PTxOut), PValidator)
import Plutarch.Api.V2.Contexts (PTxInfo (..))
import Plutarch.Extensions.Data (ptryFromData)
import Plutarch.Extensions.Monad (pletFieldC)
import Plutarch.Extra.TermCont (pguardC, pletC, pmatchC)

import Utils (closedTermToHexString)

import Plutarch.Api.V1 qualified as Value
import Plutarch.Api.V1.AssocMap qualified as AssocMap

configCBOR :: Config -> Maybe String
configCBOR = closedTermToHexString configValidator

configValidator :: ClosedTerm (PData :--> PValidator)
configValidator = phoistAcyclic $
  plam $ \nftCs' inDatum' _ sc -> unTermCont $ do
    PTxInfo info <- pmatchC $ pfield @"txInfo" # sc
    outputs <- pletFieldC @"outputs" info
    nftCs <- pletC $ pfromData $ ptryFromData nftCs'
    PJust continuing <-
      pmatchC $
        pfind
          # plam
            ( \output -> unTermCont $ do
                PTxOut out <- pmatchC output
                let val = pfield @"value" # out
                pure $ isJustTn # (atCS # val # nftCs) # pconstant ""
            )
          # outputs
    PTxOut outRec <- pmatchC continuing
    outDatum1 <- pletFieldC @"datum" outRec
    POutputDatum outDatum2 <- pmatchC outDatum1
    outDatum3 <- pletFieldC @"outputDatum" outDatum2
    PDatum outDatum4 <- pmatchC outDatum3
    outDatum5 :: Term _ (PBuiltinList PData) <- pletC $ pfromData $ ptryFromData outDatum4
    PCons _ outDatumTail <- pmatchC outDatum5
    inDatum <- pletC $ pfromData $ ptryFromData inDatum'
    pguardC "no edit" $ outDatumTail #== inDatum
    pure $ popaque $ pcon PUnit

-- TODO check signature

-- TODO I think this is repeated put them somewhere better
isJustTn :: ClosedTerm (PMap 'Value.Sorted PTokenName PInteger :--> PTokenName :--> PBool)
isJustTn = phoistAcyclic $ plam $ \m tn -> isJustTn' # m # tn # 1

isJustTn' :: ClosedTerm (PMap 'Value.Sorted PTokenName PInteger :--> PTokenName :--> PInteger :--> PBool)
isJustTn' = phoistAcyclic $ plam $ \m tn n -> m #== AssocMap.psingleton # tn # n

atCS :: ClosedTerm (PValue s a :--> PCurrencySymbol :--> PMap s PTokenName PInteger)
atCS = phoistAcyclic $
  plam $ \val cs -> unTermCont $ do
    PValue valMap <- pmatchC val
    PJust subMap <- pmatchC $ AssocMap.plookup # cs # valMap
    pure subMap
