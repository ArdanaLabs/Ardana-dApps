module Dusd (configWithUpdatesCBOR, paramCbor) where

import Plutarch.Api.V2
import Plutarch.Prelude

import Lib (checkAdminSig, getNextOutputByNft)
import Plutarch (Config)
import Plutarch.Extensions.Data (ptryFromData)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Types (ProtocolParams (..))

import Utils (closedTermToHexString)

configWithUpdatesCBOR :: Config -> Maybe String
configWithUpdatesCBOR = closedTermToHexString configWithUpdatesValidator

{- | The Protocol config utxo address validator
 parametized by the admin pubKeyHash and
 the CS of its identifying utxo
-}
configWithUpdatesValidator :: ClosedTerm (PData :--> PData :--> PValidator)
configWithUpdatesValidator = ptrace "config validator" $
  phoistAcyclic $
    plam $ \adminPKHdata nftCsData inDatum' _ sc -> unTermCont $ do
      nftCs <- pletC $ pfromData $ ptryFromData nftCsData
      inDatum <- pletC $ pfromData $ ptryFromData inDatum'
      scRec <- pletFieldsC @'["txInfo", "purpose"] sc
      PTxInfo info <- pmatchC $ getField @"txInfo" scRec
      infoRec <- pletFieldsC @'["outputs", "inputs", "signatories"] info
      checkAdminSig adminPKHdata infoRec
      outDatum :: Term _ (PBuiltinList PData) <-
        getNextOutputByNft
          nftCs
          (pconstant "")
          infoRec
          scRec
      PCons _ outDatumTail <- pmatchC outDatum
      pguardC "no edit" $ outDatumTail #== inDatum
      pure $ popaque $ pcon PUnit

paramCbor :: Config -> Maybe String
paramCbor = closedTermToHexString paramModuleAdr

{- | The Protocol parameter utxo address validator
 parametized by the admin pubKeyHash and
 the CS of its identifying utxo
-}
paramModuleAdr :: ClosedTerm (PData :--> PData :--> PValidator)
paramModuleAdr = ptrace "param module" $
  phoistAcyclic $
    plam $ \adminPKHdata nftCsData inDatum' _ sc -> unTermCont $ do
      nftCs <- pletC $ pfromData $ ptryFromData nftCsData
      _inDatum :: Term _ PData <- pletC $ pfromData $ ptryFromData inDatum'
      scRec <- pletFieldsC @'["txInfo", "purpose"] sc
      PTxInfo info <- pmatchC $ getField @"txInfo" scRec
      infoRec <- pletFieldsC @'["outputs", "inputs", "signatories"] info
      checkAdminSig adminPKHdata infoRec
      outDatum :: Term _ ProtocolParams <- getNextOutputByNft nftCs (pconstant "") infoRec scRec
      ProtocolParams outRec' <- pmatchC outDatum
      outRec <-
        pletFieldsC
          @'["liquidationFee", "debtFloor", "liquidationDiscount", "liquidationRatio"]
          outRec'
      pguardC "liquidationFee >= 0" $ 0 #<= pfromData (getField @"liquidationFee" outRec)
      pguardC "liquidationDiscount >= 0" $ 0 #<= pfromData (getField @"liquidationDiscount" outRec)
      pguardC "debtFloor >= 0" $ 0 #<= pfromData (getField @"debtFloor" outRec)
      pguardC "liquidationRatio > 1" $ 1 #< pfromData (getField @"liquidationRatio" outRec)
      pure $ popaque $ pcon PUnit
