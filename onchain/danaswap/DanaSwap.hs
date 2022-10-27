{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DanaSwap (
  trivialCbor,
  trivialFailCbor,
  configScriptCbor,
  nftCbor,
  liqudityTokenCbor,
  poolIdTokenMPCbor,
  -- testing
  validOpenAmts,
) where

import Plutarch.Prelude

import Utils (closedTermToHexString, globalConfig, validatorToHexString)

import GHC.Records (HasField)
import Plutarch.Api.V1 (AmountGuarantees (..), KeyGuarantees (..), PTokenName (PTokenName), PValue (PValue))
import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.AssocMap qualified as PMap
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (
  PAddress (..),
  PDatum (..),
  PMintingPolicy,
  POutputDatum (POutputDatum),
  PScriptPurpose (PMinting),
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
  PTxOutRef,
  PValidator,
  mkValidator,
 )
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Extensions.Api (passert, passert_)
import Plutarch.Extensions.Data (parseData, ptryFromData)
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)
import Plutarch.Maybe (pfromJust)

data LiquidityAction (s :: S)
  = Open (Term s (PDataRecord '[]))
  | Spend (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType LiquidityAction where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData LiquidityAction
instance PShow LiquidityAction

newtype LiquidityRedeemer (s :: S)
  = LiquidityRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "poolId" ':= PTokenName
               , "action" ':= LiquidityAction
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType LiquidityRedeemer where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData LiquidityRedeemer)
instance PShow LiquidityRedeemer

newtype ConfigData (s :: S)
  = ConfigData
      ( Term
          s
          ( PDataRecord
              '[ "poolAdr" ':= PAddress
               , "liquidityCS" ':= PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType ConfigData where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData ConfigData)

type PAssetClass = PBuiltinPair (PAsData PCurrencySymbol) (PAsData PTokenName)

newtype PoolData (s :: S)
  = PoolData
      ( Term
          s
          ( PDataRecord
              '[ "ac1" ':= PAssetClass
               , "ac2" ':= PAssetClass
               , "bal1" ':= PInteger
               , "bal2" ':= PInteger
               , "adminBal1" ':= PInteger
               , "adminBal2" ':= PInteger
               , "issuedLiquidity" ':= PInteger
               , "isLive" ':= PBool
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PoolData where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData PoolData)

-- TODO should this be upstreamed?
-- or is there a reason I shouldn't need this?
instance PTryFrom PData (PAsData PBool)

trivialCbor :: Maybe String
trivialCbor = closedTermToHexString trivial

trivial :: ClosedTerm (PData :--> PValidator)
trivial = plam $ \_ _ _ _ -> popaque $ pcon PUnit

trivialFailCbor :: Maybe String
trivialFailCbor = closedTermToHexString trivialFail

trivialFail :: ClosedTerm PValidator
trivialFail = perror

configScriptCbor :: String
configScriptCbor = validatorToHexString $ mkValidator globalConfig configScript

configScript :: ClosedTerm PValidator
configScript = perror

poolIdTokenMPCbor :: Maybe String
poolIdTokenMPCbor = closedTermToHexString poolIdTokenMP

liqudityTokenCbor :: Maybe String
liqudityTokenCbor = closedTermToHexString liqudityTokenMP

liqudityTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
liqudityTokenMP = phoistAcyclic $
  plam $
    \poolIdCSData redeemerData scriptContextData -> unTermCont $ do
      -- Parse various information
      LiquidityRedeemer redeemerRec' <- pmatchC $ pfromData $ ptryFromData redeemerData
      redeemerRec <- pletFieldsC @'["poolId", "action"] redeemerRec'
      scriptContextRec <- pletFieldsC @'["txInfo", "purpose"] scriptContextData
      poolIdCS <- pletC $ pfromData (ptryFromData poolIdCSData)
      poolIdTokenName <- pletC $ getField @"poolId" redeemerRec
      -- Get own CS from script purpose
      PMinting liquidityCSRec <- pmatchC $ getField @"purpose" scriptContextRec
      infoRec <- pletFieldsC @'["mint", "inputs"] (getField @"txInfo" scriptContextRec)
      let minting = getField @"mint" infoRec
      PValue mintingMap <- pmatchC minting
      PJust liquidity <- pmatchC $ PMap.plookup # (pfield @"_0" # liquidityCSRec) # mintingMap
      PMap.PMap liquidityAsList <- pmatchC liquidity
      -- Common assertions for both actions
      passert_ "minted exactly one token name of liquidity tokens" $
        plength # liquidityAsList #== 1
      passert_ "token name matched redeemer" $
        pfromData (pfstBuiltin #$ phead # liquidityAsList) #== poolIdTokenName
      -- Case over the action field of the redeemer
      pmatchC (pfromData $ getField @"action" redeemerRec) >>= \case
        Open _ -> do
          -- look up the pool id from the mint field of the script context
          -- and check that it matches the redeemer
          PJust idTokens <- pmatchC $ PMap.plookup # poolIdCS # mintingMap
          PJust _shouldBe1 <- pmatchC $ PMap.plookup # getField @"poolId" redeemerRec # idTokens
          -- TODO should we check that it is just 1? It should be redundant so for now I'm not checking
          pure $ popaque $ pcon PUnit
        Spend _ -> do
          let inputs = pfromData $ getField @"inputs" infoRec
          passert "token name matched redeemer" $
            pany # isRightPool # inputs
          where
            isRightPool = plam $ \input -> unTermCont $ do
              -- Check that the value of the field contains the right poolID
              -- This can't error when it doesn't (not all inputs are the pool) hence the cases
              PValue val <- pmatchC $ pfield @"value" # (pfield @"resolved" # input)
              pmatchC (PMap.plookup # poolIdCS # val) >>= \case
                PNothing -> pure $ pcon PFalse
                PJust poolIdToken ->
                  pmatchC (PMap.plookup # poolIdTokenName # poolIdToken) >>= \case
                    PNothing -> pure $ pcon PFalse
                    PJust _ -> pure $ pcon PTrue

nftCbor :: Maybe String
nftCbor = closedTermToHexString standardNft

standardNft :: ClosedTerm (PData :--> PMintingPolicy)
standardNft = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    passert "didn't spend out ref" $ pelem # outRef # inputs

poolIdTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
poolIdTokenMP = phoistAcyclic $
  plam $
    \configUtxoNftCs poolIdRedData sc -> ptrace "pool id mp ran" $
      unTermCont $ do
        seedInput :: Term _ PTxOutRef <- pletC $ pfromData $ ptryFromData poolIdRedData
        poolId <-
          pletC $
            pcon $
              PTokenName $
                pblake2b_256 #$ pserialiseData #$ pforgetData (pdata seedInput)
        scRec <- pletFieldsC @'["txInfo", "purpose"] sc
        let txInfo = getField @"txInfo" scRec
        let purpouse = getField @"purpose" scRec
        PMinting cs' <- pmatchC purpouse
        cs :: Term _ PCurrencySymbol <- pletC $ pfield @"_0" # cs'
        infoRec <- pletFieldsC @'["mint", "referenceInputs", "outputs"] txInfo
        let minting = getField @"mint" infoRec
        let referenceInputs = pfromData $ getField @"referenceInputs" infoRec
        let outputs = getField @"outputs" infoRec
        poolIds <- pletC $ atCS # minting # cs
        passert_ "Only mints one correct pool id" $
          isJustTn # poolIds # poolId
        configUtxo <-
          pletC $
            unsingleton $
              pfilter
                # (isConfigUtxo #$ pfromData $ ptryFromData configUtxoNftCs)
                # referenceInputs
        POutputDatum configDatum' <- pmatchC $ pfield @"datum" #$ pfield @"resolved" # configUtxo
        PDatum configDatum <- pmatchC $ pfield @"outputDatum" # configDatum'
        ConfigData configData <- pmatchC $ pfromData $ ptryFromData configDatum
        configRec <- pletFieldsC @'["poolAdr", "liquidityCS"] configData
        let poolAdr = getField @"poolAdr" configRec
        let liquidityCS = getField @"liquidityCS" configRec
        outPool <-
          pletC $
            unsingleton $
              pfilter
                # (isAtAdrWithVal # poolAdr #$ Value.psingleton # cs # poolId # 1)
                # outputs
        PTxOut outPoolRec' <- pmatchC outPool
        outPoolRec <- pletFieldsC @'["datum", "value"] outPoolRec'
        POutputDatum outPoolDatum' <- pmatchC $ getField @"datum" outPoolRec
        PDatum outPoolDatumrec <- pmatchC $ pfield @"outputDatum" # outPoolDatum'
        PoolData poolDataRec' <- pmatchC $ pfromData $ ptryFromData outPoolDatumrec
        poolDataRec <-
          pletFieldsC
            @'[ "issuedLiquidity"
              , "ac1"
              , "ac2"
              , "bal1"
              , "bal2"
              , "adminBal1"
              , "adminBal2"
              ]
            poolDataRec'
        let issuedLiquidity = pfromData $ getField @"issuedLiquidity" poolDataRec
        liquidity <- pletC $ atCS # minting # liquidityCS
        passert_ "actaully minted same amount reported in datum" $
          isJustTn' # liquidity # poolId # issuedLiquidity
        valueMatchesDatum poolDataRec (getField @"value" outPoolRec)
        passert "liquidity is correct" $
          validOpenAmts
            # getField @"bal1" poolDataRec
            # getField @"bal2" poolDataRec
            # issuedLiquidity

-- This part of the logic is seperated mainly for ease of testing
validOpenAmts :: ClosedTerm (PInteger :--> PInteger :--> PInteger :--> PBool)
validOpenAmts = phoistAcyclic $
  plam $
    \bal1 bal2 liq -> unTermCont $ do
      k2 <- pletC $ bal1 * bal2
      liq1 <- pletC $ liq + 1
      pure $
        liq * liq #<= k2
          #&& k2 #< liq1 * liq1

valueMatchesDatum ::
  ( HasField "ac1" a (Term s PAssetClass)
  , HasField "bal1" a (Term s PInteger)
  , HasField "adminBal1" a (Term s PInteger)
  , HasField "ac2" a (Term s PAssetClass)
  , HasField "bal2" a (Term s PInteger)
  , HasField "adminBal2" a (Term s PInteger)
  ) =>
  a ->
  Term s (PValue 'Sorted 'Positive) ->
  TermCont s ()
valueMatchesDatum rec val = do
  PValue valMap <- pmatchC val
  --PBuiltinPair ac1cs ac1tn <- pmatchC $ getField @"ac1" rec
  let ac1cs = pfromData $ pfstBuiltin # getField @"ac1" rec
  let ac1tn = pfromData $ psndBuiltin # getField @"ac1" rec
  bal1Val <- pletC $ pfromJust #$ PMap.plookup # ac1tn #$ pfromJust #$ PMap.plookup # ac1cs # valMap
  passert_ "bal1 is wrong" $ bal1Val #== getField @"bal1" rec + getField @"adminBal1" rec
  let ac2cs = pfromData $ pfstBuiltin # getField @"ac2" rec
  let ac2tn = pfromData $ psndBuiltin # getField @"ac2" rec
  bal2Val <- pletC $ pfromJust #$ PMap.plookup # ac2tn #$ pfromJust #$ PMap.plookup # ac2cs # valMap
  passert_ "bal2 is wrong" $ bal2Val #== getField @"bal2" rec + getField @"adminBal2" rec

isAtAdrWithVal :: ClosedTerm (PAddress :--> PValue 'Sorted 'NonZero :--> PTxOut :--> PBool)
isAtAdrWithVal = phoistAcyclic $
  plam $
    \adr val txOut -> unTermCont $ do
      PTxOut rec' <- pmatchC txOut
      rec <- pletFieldsC @'["address", "value"] rec'
      pure $
        getField @"address" rec #== adr
          #&& val #<= Value.pforgetPositive (getField @"value" rec)

isConfigUtxo :: ClosedTerm (PCurrencySymbol :--> PTxInInfo :--> PBool)
isConfigUtxo = phoistAcyclic $
  plam $ \cs ->
    plet (Value.psingleton # cs # pconstant "" # 1) $
      \shouldBe -> plam $ \txInInfo -> unTermCont $ do
        PTxInInfo rec <- pmatchC txInInfo
        PTxOut res <- pmatchC $ pfield @"resolved" # rec
        pure $ shouldBe #<= Value.pforgetPositive (pfield @"value" # res)

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
