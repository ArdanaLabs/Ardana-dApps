module Lib (getNextOutputByNft,checkAdminSig) where

import Plutarch.Prelude
import Plutarch.Api.V2
    ( PScriptPurpose(PSpending),
      PPubKeyHash,
      PDatum(PDatum),
      POutputDatum(POutputDatum),
      PTxInInfo(..),
      PTxOut(..) )

import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, PValue)

import Plutarch.Extensions.Data (ptryFromData)
import Plutarch.Extensions.Monad (pletFieldC)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)

import Plutarch.Api.V1 qualified as Value
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import GHC.Records (HasField)
import Plutarch.Api.V1 (PMap, PValue (PValue))

getNextOutputByNft
  :: (HasField "outputs" infoRec (Term s (PBuiltinList PTxOut))
     ,HasField "inputs" infoRec (Term s (PBuiltinList PTxInInfo))
     ,HasField "purpose" scRec (Term s PScriptPurpose)
     ,PIsData datumType
     ,PTryFrom PData (PAsData datumType)
     )
  => Term s PCurrencySymbol
  -> Term s PTokenName
  -> infoRec
  -> scRec
  -> TermCont s (Term s datumType)
getNextOutputByNft cs tn infoRec scRec = do
    outputs <- pletC $ getField @"outputs" infoRec
    PSpending outRef' <- pmatchC $ getField @"purpose" scRec
    outRef <- pletFieldC @"_0" outRef'
    PJust ownInput' <- pmatchC $ pfind
      # plam (\txininfo -> outRef #== (pfield @"outRef" # txininfo))
      # getField @"inputs" infoRec
    PTxInInfo ownInput <- pmatchC ownInput'
    PTxOut ownInputOutRec <- pmatchC $ pfield @"resolved" # ownInput
    ownAdr <- pletFieldC @"address" ownInputOutRec
    PJust continuing <-
      pmatchC $
        pfind
          # plam
            ( \output -> unTermCont $ do
                PTxOut out <- pmatchC output
                outRec <- pletFieldsC @'["value","address"] out
                let val = getField @"value" outRec
                let adr = getField @"address" outRec
                pure $ (adr #== ownAdr) #&& isJustTn # (atCS # val # cs) # tn
            )
          # outputs
    PTxOut outRec <- pmatchC continuing
    outDatum1 <- pletFieldC @"datum" outRec
    POutputDatum outDatum2 <- pmatchC outDatum1
    outDatum3 <- pletFieldC @"outputDatum" outDatum2
    PDatum outDatum4 <- pmatchC outDatum3
    pletC $ pfromData $ ptryFromData outDatum4


checkAdminSig
  :: HasField "signatories" a
  (Term s (PBuiltinList (PAsData PPubKeyHash)))
  => Term s PData
  -> a
  -> TermCont s ()
checkAdminSig adminKeyData infoRec = do
    pguardC "admin signed" $
      pelem
      # ptryFromData adminKeyData
      # getField @"signatories" infoRec


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
