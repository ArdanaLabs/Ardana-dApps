module Lib (getNextOutputByNft, checkAdminSig) where

import Plutarch.Api.V2 (
  PDatum (PDatum),
  POutputDatum (POutputDatum),
  PPubKeyHash,
  PScriptPurpose (PSpending),
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Prelude

import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, PValue)

import Plutarch.Extensions.Data (ptryFromData)
import Plutarch.Extensions.Monad (pletFieldC)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)

import GHC.Records (HasField)
import Plutarch.Api.V1 (PMap, PValue (PValue))
import Plutarch.Api.V1 qualified as Value
import Plutarch.Api.V1.AssocMap qualified as AssocMap

{- | a common pattern is to have a state represented by
 a utxo with a consistant NFT and address
 to help with that this function finds the output that
 represents the same state by checking
 that the address and NFT have remained the same
 and then gets and decodes its datum as this is
 usually also needed
-}
getNextOutputByNft ::
  forall
    (s :: S)
    infoRec -- should be an HRec type returned by pletFieldsC on a txinfo
    scRec -- should be an HRec type returned by pletFieldsC on a script context
    datumType. -- the type being represented by the datum
  ( HasField "outputs" infoRec (Term s (PBuiltinList PTxOut))
  , HasField "inputs" infoRec (Term s (PBuiltinList PTxInInfo))
  , HasField "purpose" scRec (Term s PScriptPurpose)
  , PIsData datumType
  , PTryFrom PData (PAsData datumType)
  ) =>
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  infoRec ->
  scRec ->
  TermCont s (Term s datumType)
getNextOutputByNft cs tn infoRec scRec = do
  outputs <- pletC $ getField @"outputs" infoRec
  PSpending outRef' <- pmatchC $ getField @"purpose" scRec
  outRef <- pletFieldC @"_0" outRef'
  PJust ownInput' <-
    pmatchC $
      pfind
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
              outRec <- pletFieldsC @'["value", "address"] out
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

{- | A common pattern is to take a
 pubKeyHash as a parameter and check for
 the signature this function does that
-}
checkAdminSig ::
  forall
    infoRec -- should be an HRec returned by pletFieldsC on a txinfo
    s.
  HasField
    "signatories"
    infoRec
    (Term s (PBuiltinList (PAsData PPubKeyHash))) =>
  Term s PData -> -- The admin key still as Data
  infoRec ->
  TermCont s ()
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
