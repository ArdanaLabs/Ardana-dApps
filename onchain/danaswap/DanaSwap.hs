{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DanaSwap (
  trivialCbor,
  trivialFailCbor,
  configScriptCbor,
  nftCbor,
  liqudityTokenCbor,
  poolIdTokenMPCbor,
  poolAdrValidatorCbor,
  -- testing
  validOpenAmts,
) where

import Plutarch.Prelude

import Utils (closedTermToHexString, validatorToHexString)

import GHC.Records (HasField)
import Plutarch.Api.V1 (AmountGuarantees (..), KeyGuarantees (..), PTokenName (PTokenName), PValue (PValue))
import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as AssocMap

import Data.Default (def)
import Plutarch (Config (tracingMode), TracingMode (..))
import Plutarch.Api.V1.AssocMap qualified as PMap
import Plutarch.Api.V1.Value (PCurrencySymbol)
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (
  PAddress (..),
  PDatum (..),
  PMintingPolicy,
  POutputDatum (POutputDatum),
  PScriptPurpose (..),
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
  PTxOutRef,
  PValidator,
  mkValidator,
 )
import Plutarch.Builtin (pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.Extensions.Data (parseData, ptryFromData)
import Plutarch.Extensions.List (unsingleton)
import Plutarch.Extensions.Monad (pletFieldC)
import Plutarch.Extra.TermCont (
  pguardC,
  pletC,
  pletFieldsC,
  pmatchC,
  ptraceC,
 )
import Plutarch.Maybe (pfromJust)

newtype PoolRed (s :: S)
  = PoolRed
      ( Term
          s
          ( PDataRecord
              '[ "id" ':= PTokenName
               , "action" ':= PoolAction
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PoolRed where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData PoolRed)

data PoolAction (s :: S)
  = Swap (Term s (PDataRecord '["fee" ':= PInteger]))
  | Liq (Term s (PDataRecord '[]))
  | Kill (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PoolAction where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData PoolAction

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

type AllPoolFields =
  '[ "ac1"
   , "ac2"
   , "bal1"
   , "bal2"
   , "adminBal1"
   , "adminBal2"
   , "issuedLiquidity"
   , "isLive"
   ]

trivialCbor :: Config -> Maybe String
trivialCbor = closedTermToHexString trivial

trivial :: ClosedTerm (PData :--> PValidator)
trivial = plam $ \_ _ _ _ -> popaque $ pcon PUnit

trivialFailCbor :: Config -> Maybe String
trivialFailCbor = closedTermToHexString trivialFail

trivialFail :: ClosedTerm PValidator
trivialFail = perror

configScriptCbor :: String
configScriptCbor = validatorToHexString $ mkValidator def {tracingMode = NoTracing} configScript

configScript :: ClosedTerm PValidator
configScript = perror

poolIdTokenMPCbor :: Config -> Maybe String
poolIdTokenMPCbor = closedTermToHexString poolIdTokenMP

liqudityTokenCbor :: Config -> Maybe String
liqudityTokenCbor = closedTermToHexString liqudityTokenMP

liqudityTokenMP :: ClosedTerm (PData :--> PMintingPolicy)
liqudityTokenMP = phoistAcyclic $
  ptrace "liquidity minting policy" $
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
        pguardC "minted exactly one token name of liquidity tokens" $
          plength # liquidityAsList #== 1
        pguardC "token name matched redeemer" $
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
            pguardC "token name matched redeemer" $
              pany # isRightPool # inputs
            pure $ popaque $ pcon PUnit
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

nftCbor :: Config -> Maybe String
nftCbor = closedTermToHexString standardNft

standardNft :: ClosedTerm (PData :--> PMintingPolicy)
standardNft = phoistAcyclic $
  plam $ \outRefData _ sc -> unTermCont $ do
    outRef :: Term _ PTxOutRef <- parseData outRefData
    let (inputs :: Term _ (PBuiltinList PTxOutRef)) =
          pmap # pfield @"outRef"
            #$ pfield @"inputs"
            #$ pfield @"txInfo" # sc
    pguardC "didn't spend out ref" $ pelem # outRef # inputs
    pure $ popaque $ pcon PUnit

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
        pguardC "Only mints one correct pool id" $
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
        poolDataRec <- pletFieldsC @AllPoolFields poolDataRec'
        let issuedLiquidity = pfromData $ getField @"issuedLiquidity" poolDataRec
        liquidity <- pletC $ atCS # minting # liquidityCS
        pguardC "actaully minted same amount reported in datum" $
          isJustTn' # liquidity # poolId # issuedLiquidity
        valueMatchesDatum poolDataRec (getField @"value" outPoolRec)
        pguardC "liquidity is correct" $
          validOpenAmts
            # getField @"bal1" poolDataRec
            # getField @"bal2" poolDataRec
            # issuedLiquidity
        pure $ popaque $ pcon PUnit

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
  pguardC "bal1 is wrong" $ bal1Val #== getField @"bal1" rec + getField @"adminBal1" rec
  let ac2cs = pfromData $ pfstBuiltin # getField @"ac2" rec
  let ac2tn = pfromData $ psndBuiltin # getField @"ac2" rec
  bal2Val <- pletC $ pfromJust #$ PMap.plookup # ac2tn #$ pfromJust #$ PMap.plookup # ac2cs # valMap
  pguardC "bal2 is wrong" $ bal2Val #== getField @"bal2" rec + getField @"adminBal2" rec

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

poolAdrValidatorCbor :: Config -> Maybe String
poolAdrValidatorCbor = closedTermToHexString poolAdrValidator

poolAdrValidator :: ClosedTerm (PData :--> PData :--> PValidator)
poolAdrValidator = phoistAcyclic $
  ptrace "poolAdrValidator" $
    plam $ \poolIdCsData liquidityCsData datum redeemer sc ->
      unTermCont $ do
        -- Get old pool
        PoolData oldPool <- pmatchC $ pfromData $ ptryFromData datum
        oldPoolRec <- pletFieldsC @AllPoolFields oldPool

        -- get poolId Currency Symbol from data
        poolIdCs <- pletC $ pfromData $ ptryFromData poolIdCsData

        scRec <- pletFieldsC @'["txInfo", "purpose"] sc
        PSpending outRef' <- pmatchC (getField @"purpose" scRec)
        outRef <- pletC $ pfield @"_0" # outRef'
        let txInfo = getField @"txInfo" scRec
        infoRec <- pletFieldsC @'["inputs", "outputs", "mint"] txInfo
        let inputs :: Term _ (PBuiltinList PTxInInfo) = getField @"inputs" infoRec

        -- find old pool in inputs
        PJust poolInput <-
          pmatchC $
            pfind
              # plam
                ( \input -> unTermCont $ do
                    PTxInInfo rec <- pmatchC input
                    pure $ pfield @"outRef" # rec #== outRef
                )
              # inputs
        PTxInInfo input' <- pmatchC poolInput
        PTxOut resolved <- pmatchC $ pfield @"resolved" # input'
        resolvedRec <- pletFieldsC @'["address", "value"] resolved
        ownAdr <- pletC $ getField @"address" resolvedRec

        -- parse redeemer
        PoolRed poolRed <- pmatchC $ pfromData $ ptryFromData redeemer
        redRec <- pletFieldsC @'["id", "action"] poolRed
        let poolId = getField @"id" redRec
        let action = getField @"action" redRec

        -- get the old pool's value and check pool was valid
        inputValue <- pletC $ getField @"value" resolvedRec
        idTokens <- pletC $ atCS # inputValue # poolIdCs
        pguardC "in pool was valid" $ isJustTn # idTokens # poolId

        -- find pool output
        let outputs :: Term _ (PBuiltinList PTxOut) = getField @"outputs" infoRec
        PJust poolOut <-
          pmatchC $
            pfind
              # plam
                ( \output -> unTermCont $ do
                    PTxOut out <- pmatchC output
                    let val = pfield @"value" # out
                    pure $ isJustTn # (atCS # val # poolIdCs) # poolId
                )
              # outputs

        PTxOut outPool <- pmatchC poolOut
        outPoolRec <- pletFieldsC @'["address", "datum", "value"] outPool
        let outPoolDatumOut = getField @"datum" outPoolRec
        POutputDatum outDatum' <- pmatchC outPoolDatumOut
        PDatum outDatum <- pmatchC $ pfield @"outputDatum" # outDatum'
        PoolData outPoolData <- pmatchC $ pfromData $ ptryFromData outDatum
        outPoolDataRec <- pletFieldsC @AllPoolFields outPoolData

        -- check that datum is accurate in the output pool
        valueMatchesDatum outPoolDataRec (getField @"value" outPoolRec)

        -- get liquidity minted so it can be used in multiple branches
        pguardC "out pool is at the right address" $ getField @"address" outPoolRec #== ownAdr
        let minting = getField @"mint" infoRec
        liquidityCS <- pletC $ pfromData $ ptryFromData liquidityCsData
        PValue mintingMap <- pmatchC minting
        liquidityMinted <- pletC $ AssocMap.plookup # liquidityCS # mintingMap

        pguardC "no asset switches" $
          getField @"ac1" outPoolDataRec #== getField @"ac1" oldPoolRec
            #&& getField @"ac2" outPoolDataRec #== getField @"ac2" oldPoolRec

        pmatchC action >>= \case
          Swap swap -> do
            PNothing <- pmatchC liquidityMinted
            -- check no liqudity is minted
            -- compute old invariant
            ptraceC "swap branch"
            fee <- pletFieldC @"fee" swap
            let oldk2 :: Term _ PInteger = getField @"bal1" oldPoolRec * getField @"bal2" oldPoolRec
            d1 :: Term _ PInteger <- pletC $ getField @"bal1" outPoolDataRec - getField @"bal1" oldPoolRec
            d2 :: Term _ PInteger <- pletC $ getField @"bal2" outPoolDataRec - getField @"bal2" oldPoolRec
            fee1 <- pletC $ pif (d1 #< 0) fee 0
            fee2 <- pletC $ pif (d2 #< 0) fee 0
            ptraceC $ "fee 1:" <> pshow fee1
            ptraceC $ "fee 2:" <> pshow fee2
            ptraceC $ "d 1:" <> pshow d1
            ptraceC $ "d 2:" <> pshow d2
            pguardC "fee is enough" $ d1 * (-3) #<= fee1 * 1_000 #&& d2 * (-3) #<= fee2 * 1_000
            let newk2 :: Term _ PInteger = (getField @"bal1" outPoolDataRec - fee1) * (getField @"bal2" outPoolDataRec - fee2)
            pguardC "invariant is non-decreasing" $ oldk2 #<= newk2
            pguardC "admin fee paid" $
              getField @"adminBal1" oldPoolRec + fee1 #<= getField @"adminBal1" outPoolDataRec
                #&& getField @"adminBal2" oldPoolRec + fee2 #<= getField @"adminBal2" outPoolDataRec
            -- TODO add stuff for fees
            pguardC "in and out pools are live" $
              getField @"isLive" oldPoolRec
                #&& getField @"isLive" outPoolDataRec
            pure $ popaque $ pcon PUnit
          _ -> do
            ptraceC "not yet implemented"
            pure perror
