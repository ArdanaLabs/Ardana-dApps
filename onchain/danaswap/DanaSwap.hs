{-# LANGUAGE UndecidableInstances #-}

module DanaSwap (
  trivialCbor,
  trivialFailCbor,
  configScriptCbor,
  nftCbor,
  liqudityTokenCBor,
) where

import Plutarch.Prelude

import Utils (closedTermToHexString, globalConfig, validatorToHexString)

import Plutarch.Api.V2 (PMintingPolicy, PScriptPurpose (PMinting), PValidator, mkValidator, PTxOut (PTxOut), POutputDatum (POutputDatum))

import Plutarch.Api.V1.AssocMap qualified as PMap
import Plutarch.Api.V2.Tx (PTxOutRef)
import Plutarch.Extensions.Data (parseData, ptryFromData)
import Plutarch.Extra.TermCont (pguardC, pletC, pletFieldsC, pmatchC)
import Plutarch.Api.V1.Value
import Plutarch.Extensions.Monad (pletFieldC)
import Plutarch.Api.V2.Contexts (PTxInfo(..))
import Plutarch.Api.V1 (PMap, PDatum (PDatum))
import qualified Plutarch.Api.V1 as Value
import qualified Plutarch.Api.V1.AssocMap as AssocMap

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

liqudityTokenCBor :: Maybe String
liqudityTokenCBor = closedTermToHexString liqudityTokenMP

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
    pguardC "didn't spend out ref" $ pelem # outRef # inputs
    pure $ popaque $ pcon PUnit

configValidator :: ClosedTerm (PData :--> PValidator)
configValidator = phoistAcyclic $
  plam $ \nftCs' inDatum' _ sc -> unTermCont $ do
    PTxInfo info <- pmatchC $ pfield @"txInfo" # sc
    outputs <- pletFieldC @"outputs" info
    nftCs <- pletC $ pfromData $ ptryFromData nftCs'
    PJust continuing <- pmatchC $
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
    outDatum5 :: Term _ (PBuiltinList PData)  <- pletC $ pfromData $ ptryFromData outDatum4
    PCons _ outDatumTail <- pmatchC outDatum5
    inDatum <- pletC $ pfromData $ ptryFromData inDatum'
    pguardC "no edit" $ outDatumTail #== inDatum
    pure $ popaque $ pcon PUnit
    -- TODO check signature


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

