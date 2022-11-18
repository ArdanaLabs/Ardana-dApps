module Dusd (configWithUpdatesCBOR, paramCbor,priceOracleCBOR) where

import Plutarch.Api.V2
import Plutarch.Prelude

import Lib (checkAdminSig, getNextOutputByNft)
import Plutarch (Config)
import Plutarch.Extensions.Data (ptryFromData)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC, ptraceC)
import Types (ProtocolParams (..), PriceData, PriceEntry (PriceEntry))

import Utils (closedTermToHexString)
import Plutarch.Api.V1.Value (padaToken, PCurrencySymbol)
import Plutarch.Extensions.List (ptake)
import Plutarch.Api.V1.Interval (PInterval(PInterval))
import Plutarch.Api.V1.Time (PPOSIXTime (PPOSIXTime))
import Plutarch.Api.V1 (PLowerBound(PLowerBound),PUpperBound(PUpperBound),PExtended(PFinite))
import Plutarch.Extensions.Monad (pletFieldC)

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

priceOracleValidator :: ClosedTerm (PData :--> PData :--> PData :--> PData :--> PValidator)
priceOracleValidator = ptrace "price oracle" $
  phoistAcyclic $
    plam $ \interval' margin' adminPKHdata nftCsData inDatum _red sc -> unTermCont $ do
      nftCs :: Term _ PCurrencySymbol  <- pletC $ pfromData $ ptryFromData nftCsData
      interval :: Term _ PPOSIXTime <- pletC $ pfromData $ ptryFromData interval'
      margin :: Term _ PPOSIXTime <- pletC $ pfromData $ ptryFromData margin'
      scRec <- pletFieldsC @'["txInfo","purpose"] sc

      PTxInfo info <- pmatchC $ getField @"txInfo" scRec
      infoRec <- pletFieldsC @'["outputs", "inputs", "signatories", "validRange" ] info
      checkAdminSig adminPKHdata infoRec
      oldList :: Term _ PriceData <- pletC $ pfromData $ ptryFromData inDatum
      newList' :: Term _ PData <- getNextOutputByNft nftCs padaToken infoRec scRec
      newList <- pletC $ pfromData $ ptryFromData newList'
      pguardC "no history edit" $ ptail # newList #== ptake # 47 # oldList

      -- last time
      PriceEntry lastRec' <- pmatchC $ pfromData $ phead # oldList
      lastTime :: Term _ PPOSIXTime <- pletFieldC @"time" lastRec'

      -- next time
      PriceEntry nextRec' <- pmatchC $ pfromData $ phead # newList
      nextTime :: Term _ PPOSIXTime <- pletFieldC @"time" nextRec'

        -- TODO this is probably ineficent
      PInterval timeRec' <- pmatchC $ getField @"validRange" infoRec
      timeRec <- pletFieldsC @'["from","to"] timeRec'

      -- Get start time
      PLowerBound start1 <- pmatchC $ getField @"from" timeRec
      PFinite start2 <- pmatchC $ pfield @"_0" # start1
      start <- pletFieldC @"_0" start2

      -- Get end time
      PUpperBound end1 <- pmatchC $ getField @"to" timeRec
      PFinite end2 <- pmatchC $ pfield @"_0" # end1
      end <- pletFieldC @"_0" end2

      ptraceC $ "start " <> timeShow start
      ptraceC $ "end " <> timeShow end
      ptraceC $ "margin " <> timeShow margin
      ptraceC $ "diff " <> timeShow (end - start)

      pguardC "time is in interval" $ start #<= nextTime #&& nextTime #<= end
      pguardC "interval isn't too long" $ end - start #<= margin
      pguardC "waited enough" $ lastTime + interval #<= start

      pure $ popaque $ pcon PUnit

-- TODO upstream this as an instance
timeShow :: Term s PPOSIXTime -> Term s PString
timeShow t = pmatch t $ \(PPOSIXTime t') -> "PPosixTime " <> pshow t'
