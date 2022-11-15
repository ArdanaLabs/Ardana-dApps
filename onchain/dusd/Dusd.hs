module Dusd
  (configWithUpdatesCBOR
  ,paramCbor
  ,adminUpdatePolicyCBOR
  ) where

import Plutarch.Api.V2
import Plutarch.Prelude

import Lib (checkAdminSig, getNextOutputByNft, checkMintsNft)
import Plutarch (Config)
import Plutarch.Extensions.Data (ptryFromData)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Types (ProtocolParams (..), ConfigDatum (ConfigDatum))

import Utils (closedTermToHexString)
import Plutarch.Api.V1.Value (padaToken)

configWithUpdatesCBOR :: Config -> Maybe String
configWithUpdatesCBOR = closedTermToHexString configWithUpdatesValidator

{- | The Protocol config utxo address validator
 the CS of its identifying utxo
-}
configWithUpdatesValidator :: ClosedTerm (PData :--> PValidator)
configWithUpdatesValidator = ptrace "config validator" $
  phoistAcyclic $
    plam $ \nftCsData inDatum _ sc -> unTermCont $ do
      nftCs <- pletC $ pfromData $ ptryFromData nftCsData
      ConfigDatum oldConf <- pmatchC $ pfromData $ ptryFromData inDatum
      oldConfRec <- pletFieldsC @'[ "authPolicy" , "versions" ] oldConf
      scRec <- pletFieldsC @'["txInfo", "purpose"] sc
      PTxInfo info <- pmatchC $ getField @"txInfo" scRec
      infoRec <- pletFieldsC @'["outputs", "inputs", "signatories" , "mint"] info
      -- technically not an NFT but the logic is the same
      checkMintsNft infoRec (getField @"authPolicy" oldConfRec) padaToken
      ConfigDatum newConf <- pmatchC =<<
        getNextOutputByNft
          nftCs
          padaToken
          infoRec
          scRec
      newConfRec <- pletFieldsC @'[ "versions" ] newConf
      let outVersions = pfromData $ getField @"versions" newConfRec
      let inVersions = pfromData $ getField @"versions" oldConfRec
      pguardC "no edit maybe append" $
        outVersions #== inVersions
          -- we allow no change to allow updating
          -- the control policy withouot a new version
        #|| ptail # outVersions #== inVersions
      pure $ popaque $ pcon PUnit

adminUpdatePolicyCBOR :: Config -> Maybe String
adminUpdatePolicyCBOR = closedTermToHexString adminUpdatePolicy

-- | Minting policy that lets the pubkey passed
-- as a parameter mint tokens whenever they want
-- intended for use as the initial control policy in the config utxo
adminUpdatePolicy :: ClosedTerm (PData :--> PMintingPolicy)
adminUpdatePolicy = ptrace "admin update policy" $
  phoistAcyclic $
    plam $ \adminKeyData _ sc -> unTermCont $ do
      scRec <- pletFieldsC @'["txInfo"] sc
      PTxInfo info <- pmatchC $ getField @"txInfo" scRec
      infoRec <- pletFieldsC @'["signatories"] info
      checkAdminSig adminKeyData infoRec
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
