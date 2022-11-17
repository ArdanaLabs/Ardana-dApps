module Dusd (configWithUpdatesCBOR, paramCbor,priceOracleCBOR) where

import Plutarch.Api.V2
import Plutarch.Prelude

import Lib (checkAdminSig, getNextOutputByNft)
import Plutarch (Config)
import Plutarch.Extensions.Data (ptryFromData)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Types (ProtocolParams (..), PriceData)

import Utils (closedTermToHexString)
import Plutarch.Api.V1 (PCurrencySymbol)
import Plutarch.Api.V1.Value (padaToken)
import Plutarch.Extensions.List (ptake)

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
          padaToken
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
    plam $ \adminPKHdata nftCsData _inDatum _red sc -> unTermCont $ do
      nftCs <- pletC $ pfromData $ ptryFromData nftCsData
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

priceOracleCBOR :: Config -> Maybe String
priceOracleCBOR = closedTermToHexString priceOracleValidator

priceOracleValidator :: ClosedTerm (PData :--> PData :--> PData :--> PValidator)
priceOracleValidator = ptrace "price oracle" $
  phoistAcyclic $
    plam $ \_time adminPKHdata nftCsData inDatum _red sc -> unTermCont $ do
      nftCs :: Term _ PCurrencySymbol  <- pletC $ pfromData $ ptryFromData nftCsData
      scRec <- pletFieldsC @'["txInfo","purpose"] sc
      PTxInfo info <- pmatchC $ getField @"txInfo" scRec
      infoRec <- pletFieldsC @'["outputs", "inputs", "signatories"] info
      checkAdminSig adminPKHdata infoRec
      oldList :: Term _ PriceData <- pletC $ pfromData $ ptryFromData inDatum
      newList :: Term _ PriceData <- getNextOutputByNft nftCs padaToken infoRec scRec
      pguardC "no history edit" $ ptail # newList #== ptake # 47 # oldList
        -- TODO this is probably ineficent
      -- TODO check time interval
      pure $ popaque $ pcon PUnit
